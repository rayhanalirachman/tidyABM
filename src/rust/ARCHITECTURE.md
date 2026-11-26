# tidyABM Rust Engine Architecture

This document describes how `tidyABM`'s Rust engine fits under the unchanged R
grammar. For build setup, toolchain versions, and everyday commands, see
`dev/CONTRIBUTING-rust.md`.

## Crate Split

| Path | Role |
|---|---|
| `src/rust/tidyabm-core/` | Pure Rust engine logic with **no R dependency** (`cargo test -p tidyabm-core`): column store (`store.rs`), expression IR & evaluator (`expr.rs`), exact integer/logical/double reductions (`reduce.rs`), `sample.int` / Bernoulli algorithms (`sample.rs`), neighbour folds (`neighbours.rs`), panic guard (`lib.rs`). |
| `src/rust/src/` | `savvy` FFI bridge crate (`tidyabm`, staticlib): `lib.rs` (`#[savvy]` entry points), `rapi.rs` (hand-written R C API declarations + `protect()`), `rng.rs` (`with_rng()`/`RRng`), `reduce.rs` (C `long double` wrapper), `store.rs` (`AgentStore` + mirror cache), `expr.rs` (`RxProgram` decoder + `RHost`), `runloop.rs` (`RunLoop` tick loop + 19 native step executors). |
| `src/reduce.c` | Small C helper for `long double` sums, means, cumsums, and group sums (matches R's 80-bit `long double` accumulator on x86_64; collapses to `double` where `sizeof(long double) == 8`). |

## Core Invariants

1. **MIT Licence / No R GPL Code (D13)**: Nothing is copied or ported from R's
   C sources. Random draws and powers call R's public C API (`unif_rand`,
   `norm_rand`, `exp_rand`, `R_unif_index`, `Rf_r*`, `R_pow`) declared by hand
   in `rapi.rs`.
2. **Main-Thread RNG & Unwind Protection**: `rng::with_rng()` pairs
   `GetRNGstate()`/`PutRNGstate()` with a `!Send`/`!Sync` `RRng` token and
   restores state on error or panic. Any R API call that can allocate, warn, or
   longjmp (including `R_CheckUserInterrupt()`) runs inside `rapi::protect()`
   (`R_UnwindProtect`), so an R longjmp never unwinds across Rust frames.
3. **Silent Panic Guard**: Every `#[savvy]` entry point runs inside `run(|| ...)`,
   which installs a silent panic hook and converts panics into
   `tidyABM-rust-panic: ...` errors mapped to `tidyABM_rust_panic` in R
   (`R/engine-switch.R`). Rust never formats user-facing CLI errors.
4. **GC Safety & Copy-on-Write**:
   - Kept R objects live in savvy's preserve list (`Kept` / `H`).
   - Existing R vectors/lists/tibbles are **never** mutated in place; updates
     use shallow-copy helpers (`set_named`, `with_elt`) and store freshly
     allocated `expr::to_r()` vectors immediately into a protected
     `OwnedListSexp` before the next allocation.
   - `gctorture(TRUE)` tests (`tests/testthat/test-rust-loop.R`) exercise every
     native step kind.
5. **R Engine Untouched as Reference (D12)**: The R engine's step functions
   (`R/engine-*.R`, `R/network-steps.R`, `R/tell.R`, `R/relation.R`) never call
   Rust. `"both"` mode (`R/run-rust.R`) runs both engines side by side from the
   same state, caches, and `.Random.seed` and aborts on any diff.

## Execution Pipeline

1. **Dispatch (`R/run-rust.R`)**: `engine_run_once()` inspects flattened steps
   (including steps inside `abm_repeat()`) via each kernel's `can()` predicate
   in `R/rust-kernels.R`. Under `"auto"`, if any step has an `auto = TRUE`
   native kernel, `run_once_loop()` runs; otherwise `run_once()` runs directly
   in pure R.
2. **Expression Compilation (`R/rust-expr.R` → `src/rust/src/expr.rs`)**:
   - `rx_compile()` translates supported R formulas into a list-based IR,
     fast-rejecting unknown call heads before entering `tryCatch()`.
   - Compiled expressions and `RxProgram` external pointers are memoised in a
     bounded per-session cache (`.rx_cache`, capped at 4,096 entries).
   - `rx_program()` resolves symbols into 4 ordered input banks: group columns,
     globals, formula-environment values (re-read per step via `scope_fn()`),
     and standing-match / relation columns.
3. **Tick Loop (`R/run-loop.R` → `src/rust/src/runloop.rs`)**:
   - `RunLoop` holds the run's `state` in a single-reference `ctx` environment
     so R fallback steps still mutate unshared tibbles in place.
   - Each native step is compiled per schema (`schema_key`: global count, group
     column counts, and relation/edge column counts) via `prep(i)`.
   - Consecutive R steps are batched into one `seg(i, j, restore)` callback.
   - Any runtime decline leaves `state` unchanged, restores `.Random.seed` if
     the step could draw, and replays the step on R; 8 consecutive declines for
     one schema pin the step to R until the schema changes
     (`DECLINES_BEFORE_SKIP`).
   - `R_CheckUserInterrupt()` is polled at every tick (and every 64 passes of
     an `abm_repeat()` block) inside `rapi::protect()`.
   - Recorded ticks log `state$groups` zero-copy into `slog` and stack once at
     run end via `rs_stack_ticks()`.

### Native Step Kinds (`rl_kind()` / `RunLoop`)

| Kind | Step | Executor |
|---:|---|---|
| 1 | `abm_rules()` (incl. mixed native/R, match scope) | `exec_rules` |
| 2 | `abm_global()` (also tick measures/checks & `abm_repeat(until =)`) | `exec_global` |
| 3 | Network `abm_neighbours()` (`sum`/`mean` fast index) | `exec_nb` |
| 4 | `abm_rules(.by =)` | `exec_by` |
| 5 | `abm_neighbours(within = <col> == own_<col>)` (unique key) | `exec_nb1` |
| 6 | Lattice `abm_neighbours(.where =)` | `exec_nbw` |
| 7 | `abm_match()` (`random`, `one_of`, `opposite_group`, `network`, `nearest` + `cost`, `role`, `.by`) | `exec_match` |
| 8 | `abm_death()` (declines with relations for pruning) | `exec_death` |
| 9 | `abm_birth()` (`when =` or `n =`, `attach_via =`) | `exec_birth` / `exec_birth_count` |
| 10 | Lattice `abm_move()` (random, empty, `direction`, `range`, `axes_only`, `uphill`/`downhill`) | `exec_move` |
| 11 | `abm_tell(to = <id>)` (`last`, `first`, `sum`, `mean`, `error`, `collect`) | `exec_tell` |
| 12 | `abm_sequential()` (incl. standing match & relation reads/writes) | `exec_seq` |
| 13 | `abm_global(.by = <name>)` | `exec_global_by` |
| 14 | `abm_neighbours(within =)` (general cross product) | `exec_cross` |
| 15 | Network `abm_neighbours()` (`any`/`all`) | `exec_nb_anyall` |
| 16 | `abm_draw()` (per edge or per endpoint) | `exec_draw` |
| 17 | Network `abm_neighbours()` pair view (`own_`, edge draws, `na.rm`, mixes) | `exec_nb_view` |
| 18 | Network `abm_link()` / `abm_unlink()` (no `via =`, pairs & cliques) | `exec_link` |
| 19 | `abm_pairs(via =)` (`.when`, `from_`/`to_`) | `exec_pairs` |
| < 0 | Flattened `abm_repeat()` begin (`-1`) and end (`-(2 + 2*max + has_until)`) markers | `RunLoop::run` |

## Adding a Native Step Kind

1. Add `rl_<name>_ok()` and `rl_prep_<name>()` in `R/run-loop.R`, wire `rl_kind()` and `rl_prep()`, and register in `R/rust-kernels.R`.
2. Add plan fields, `parse_plan()`, `exec_<name>()`, and `exec()` dispatch in `src/rust/src/runloop.rs`.
3. Add `"both"`-mode parity and error-parity tests in `tests/testthat/test-rust-grammar.R` plus a `gctorture` case in `tests/testthat/test-rust-loop.R`.
4. Verify `cargo fmt`, `cargo clippy --workspace --all-targets -- -D warnings`, `cargo test -p tidyabm-core`, MSRV 1.81 (`cargo +1.81 check --workspace --lib`), full `testthat` (`r`/`rust`/`both`), oracle `verify.sh`, and `R CMD check --as-cran --no-manual` with an empty `CARGO_HOME`.
