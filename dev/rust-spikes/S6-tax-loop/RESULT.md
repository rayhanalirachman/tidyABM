# S6: the tax model's year loop in Rust, against the engine

Date 2026-11-17. macOS arm64, R 4.6.1, tidyABM `77ae511` (round 10), rustc/cargo
1.100.0, savvy 0.11.0 (crate, pinned `=0.11.0`; savvy-cli 0.11.0 for the wrappers),
release profile `panic = "unwind"`, lto, codegen-units 1. Package: `../pkg/rspike`
(built by `../pkg/build.sh` into `../lib`). Load average during timing ~3.7-4.5
(other work on the machine).

## What was built

* `pkg/rspike/src/rust/src/tax.rs`: the whole tick loop (neighbour two-pass mean over
  a CSR index, the nested `case_when` decision, `runif(n()) < audit_rate`, the three
  dependent rules, the global) with the uniform generator passed in as a closure. No R
  dependency.
* `rs_tax_run()` (one run, R's own generator via `GetRNGstate`/`unif_rand`/
  `PutRNGstate` through a `!Send` guard) and `rs_tax_sweep()` (all runs, pure-Rust
  RNG port from S1, `std::thread::scope` over runs, no R API inside the threads).
* `s6.R`: builds the world with `abm_setup()` exactly as `oracle/bench/tax.R`, takes the
  run seeds from `tidyABM:::run_seeds(1, 150)`, compares with the engine's golden
  output (`oracle/bench/golden/tax.rds`) using `identical()`.

## Result

| check | result |
|---|---|
| Rust loop + R's C RNG, 150 runs x 40 ticks, vs engine | **identical** (6000 values; the full globals table rebuilt with Rust values is `identical()` to the golden) |
| `.Random.seed` after a Rust run vs after `runif(441 * 40)` | identical (one uniform per agent per tick, as `runif(n())`) |
| pure-Rust RNG port, 1 / 2 / 8 threads | **identical** in all three |
| time, 150 runs, R `lapply` + `set.seed` + Rust year loop | real 0.041 s, CPU 0.040 s (median of 7) |
| time, 150 runs entirely in Rust, 1 thread | real 0.037 s, CPU 0.036 s |
| time, 150 runs entirely in Rust, 8 threads | real 0.009 s, CPU 0.045 s |
| engine today (oracle pairs) | CPU 3.2 s |

So the compiled tax loop is ~80x faster than the engine on one core (0.040 vs 3.2 s),
in line with the C prototype `rustq/tax.c` (0.03 s), and `cores` via threads is
reproducible.

## The one failure on the way (and what it teaches)

The first run was **not** identical: 347 of 6000 values differed by 1 ulp from tick 17
of run 1 on. Cause (found in `diag.R`, `results/diag.txt`): the order in which each
agent's neighbours are summed. The engine folds neighbours in **edge-table order (each
edge seen from both ends, stable by focal agent)**, not in `model$lattice$nb` order
(which the C prototype and the first Rust version used). With edge order: identical;
with `lattice$nb`, sorted or reversed order: 347-489 values differ. The INVENTORY's
"bit-identical" claim for `rustq/tax.c` was therefore based on a script that also tried
the edge order. Lesson for P4: the CSR index must be built from the edge table in the
engine's order, and summation order is part of the specification of every kernel.

## Verdict against PLAN.md

**PASS** (kill criterion "S6 not identical after investigation" not hit). The P5/P6
targets are confirmed for tax: compiled work 0.036-0.040 s per sweep, so P6's
0.06-0.2 s single-core target is about what the remaining per-run R work allows (see
S3 for that part). The pure RNG port running on threads is identical here, which is
the D3 evidence on macOS arm64.
