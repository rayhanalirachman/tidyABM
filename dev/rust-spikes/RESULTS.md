# Phase 0 spike results (2026-11-17)

Platform for everything below: macOS 26 arm64, R 4.6.1 (CRAN binary), dplyr 1.2.1,
vctrs 0.7.3, rlang 1.3.0, rustc/cargo 1.100.0 (stable), savvy 0.11.0 / savvy-cli 0.11.0,
extendr-api 0.9.0 / rextendr 0.5.0, tidyABM `77ae511`. Load average 3.6-4.7 during
timings (other work on the machine). Details, numbers and commands in each
`Sn-*/RESULT.md`. Oracle: `../oracle/README.md` (green).

| spike | verdict | headline |
|---|---|---|
| S1 RNG port | **PASS on macOS arm64**; other platforms not run | 105/105 cases identical (draws + `.Random.seed`): runif, rnorm, rexp 10^7 x 5 seeds; sample.int all paths incl. sample2, prob (ProbSampleReplace, Walker, no-replace) with heavy ties. Needed: qnorm/exp_rand FMA via `mul_add` on aarch64-apple, Walker via `R_unif_index`. Port is 2x faster than R's own generator. |
| S2 reductions / libm | **PASS on macOS arm64**; x86_64/Windows not run | sum/mean/cumsum identical (128/128, NA/NaN order, overflow). Math identical only after porting R >= 4.5's wrapper layer (`Rexp`, `Rsqrt`, `Rlog1p`, `Rexpm1`, `math1_ari`): plain `std` exp/log1p differ. Windows R uses `powl` for `^`. Rust never emits FMA unless asked. |
| S3 boundary costs | **PASS** | R side of a step in an R-driven loop 1.8-2.5 us (budget 3, kill 10); Get/PutRNGstate 0.1 us; column copy at n = 441 0.8 out / 0.35 in us. Per-run fixed cost in today's R code 0.82 ms (0.12 s per 150-run sweep). Character materialisation via savvy 220 ns/element: needs the mirror cache + CHARSXP reuse. extendr call costs not measured. |
| S4 fallback / errors | **PASS** (14/14) | classed R conditions (rlang fields, base errors, engine's `rethrow_step_error` full message) survive Rust frames bit for bit; warnings/restarts work; panics on main and worker threads become R errors; Ctrl-C answered in 0.01 s; gctorture clean. savvy's panic hook calls R from worker threads: replace it. |
| S5 CRAN build | **partial PASS (macOS only)**; Windows x86_64 kill criterion **not tested** | savvy and extendr hello packages: vendored, offline, `-j 2`, rustc reported, 0.59 MB tarballs, `--as-cran` 0 errors (1 warning: checkbashisms missing locally). savvy's stock Makevars breaks on paths with spaces (this repo); fixed by a relative target dir. MSRV not tested. |
| S6 tax loop in Rust | **PASS** | 150 runs x 40 ticks identical to the engine, with R's C RNG and with the pure port on 1/2/8 threads. 0.040 s CPU vs 3.2 s (80x). First attempt differed by 1 ulp: neighbour sums must follow the edge table's order, not `lattice$nb`. |
| S7 expression compiler | **PASS** | R AST -> IR (nested lists) -> Rust evaluator: tax rules identical over the full sweep; 5,000 random expressions with NA/NaN/Inf: 3,630 identical, 1,329 both fall back/error, 0 mismatches (after copying dplyr's size rules). Spike evaluator 4x faster on `case_when` rules, slower on one-liners because it converts the R environment per call. |
| S8 Wolf-Sheep kernels | **not run** (optional; skipped to stay within the session) | P4 estimate unconfirmed; risk R4 unchanged. |

## Blockers and commands for the user

1. Other platforms for S1, S2, S5 (Linux x86_64, macOS x86_64, Windows x86_64 and
   arm64, R devel): put `sandbox/rust-migration/spikes/` (without `lib/` and `target/`
   folders) in a scratch GitHub repository with `ci/spikes.yaml` as
   `.github/workflows/spikes.yaml`, push, and read the job logs/artifacts.
2. MSRV: `rustup toolchain install 1.81.0` (or the `msrv` job in `ci/spikes.yaml`).
3. R-hub sanitizers and webR: r-hub/actions on `S5-cran-build/hellosavvy`; r-universe
   for the wasm build.

## How to rerun locally

```sh
cd sandbox/rust-migration/spikes
pkg/build.sh                                            # builds rspike into lib/
(cd S1-rng/rrng && cargo build --release) && Rscript S1-rng/tests/compare.R "$PWD/S1-rng/rrng/target/release/dump" /tmp/s1 1e7
(cd S2-reductions/rreduce && cargo build --release)     # then see S2 RESULT.md for ldsum.so
export TIDYABM_PKG="$PWD/../../../tidyABM"
Rscript S6-tax-loop/s6.R lib ../oracle/bench/golden/tax.rds
ORACLE="$PWD/../oracle" Rscript S3-boundary/s3.R lib S3-boundary/results
Rscript S4-fallback-errors/s4.R lib
Rscript S7-expr-compiler/s7.R lib ../oracle/bench/golden/tax.rds 5000
```

Code quality: `cargo fmt` applied and `cargo clippy` clean (0 warnings) for `rrng`,
`rreduce` and `rspike`. `unsafe` is confined to `rspike/src/rust/src/rapi.rs` (four
R C API declarations, each call with a SAFETY comment: main thread only, the one
longjmp-capable call wrapped in `savvy::unwind_protect`).
