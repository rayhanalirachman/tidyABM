# S3: boundary costs of the R-driven loop (savvy)

Date 2026-11-17. macOS arm64, R 4.6.1, rustc/cargo 1.100.0, savvy 0.11.0 (crate) /
savvy-cli 0.11.0, package `../pkg/rspike` (`Store` = 10 double columns + a
pooled character column behind an external pointer). Script `s3.R`, log
`results/s3.txt`, table `results/s3.rds`. CPU (user) time, best of 5, empty loop
subtracted. Load average 4.2-4.7 during the run (other work on the machine), so
treat +-20 % as noise. extendr side by side: **not measured** (see below).

## Numbers

| measurement | n | us |
|---|---|---|
| R closure call (baseline) | | 0.07 |
| savvy no-op, `.Call(<native symbol>)` | | 0.07 |
| savvy no-op through its generated R wrapper | | 0.16 |
| copy-out 1 double column, `.Call` | 10 / 441 / 1e4 / 1e5 | 0.5 / 0.8 / 3.9 / 42 |
| copy-out via the savvy method wrapper `st$col()` | 10 / 441 / 1e4 / 1e5 | 1.1 / 1.4 / 4.7 / 42.5 |
| copy-in 1 double column | 10 / 441 / 1e4 / 1e5 | 0.3 / 0.35 / 1.6 / 15 |
| character column materialised (savvy `set_elt` per element) | 10 / 441 / 1e4 / 1e5 | 2.6 / 97 / 2,182 / 21,862 |
| character column from an R-side mirror cache (version hit) | any | 0.15-0.2 |
| full tibble round trip, 10 columns out + `new_tibble` + 10 in | 10 / 1e4 | 51 / 132 |
| R closure called from Rust (`FunctionSexp::call`) | | 0.32 |
| **R side of one step** in a minimal R-driven loop (switch on class, `.Call` into the store, one `withCallingHandlers` per run, per-tick `global_cells()` and bookkeeping), kernel work excluded | 10 / 441 | **1.75-1.8 / 2.0-2.5** |
| `GetRNGstate()` + `PutRNGstate()` per step | | 0.11 |
| GC in that loop (5 x 80,000 steps) | | ~0.26-0.33 s in total, i.e. ~0.7-0.8 us per step, already inside the per-step figure |
| per-run fixed cost of today's engine, tax sweep `ticks = 0` (setup, tick-0 snapshot and globals, plan, final binding) / 150 runs | 441 | **820** |
| engine today, tax sweep `ticks = 40`, per run | 441 | 31,660 (771 us per tick) |

## Verdict against PLAN.md

* **R-driven loop: PASS, well inside budget.** R side per step 1.8-2.5 us (budget
  <= 3 us; kill threshold 10 us). `PutRNGstate`'s 626-int allocation costs ~0.1 us per
  step; not a concern. Copy per column at n = 441: 0.8 us out, 0.35 us in (budget
  <= 1 us). Copy (not ALTREP) stays the default.
* **Per-run fixed cost is the bigger number**: ~0.82 ms per run in today's R code,
  i.e. 0.12 s for the 150-run tax sweep, three times the compiled loop itself (S6:
  0.04 s). P6's tax target (0.06-0.2 s single core) is reachable only if
  `build_run`/`run_once` setup/tick-0 snapshot/final `bind_rows` either shrink or move
  to Rust for fully compiled plans; with today's R per-run code the floor is ~0.16 s.
  Re-estimate: P6 tax 0.10-0.20 s (upper half of the range), unless the run assembly
  in P3 also covers the tick-0 work.
* **Character columns need care**: savvy's `OwnedStringSexp::set_elt` costs ~220 ns
  per element (a `mkCharLenCE` + unwind-protect each time), 2.2 ms for 10,000 agents.
  The R-mirror cache (design already in RESEARCH.md) makes the steady state free;
  on a miss, P3 must build the STRSXP by reusing one cached CHARSXP per pool entry
  (`SET_STRING_ELT`, needs a small reviewed `unsafe` block or a savvy helper).
* **The step adapter is not cheap**: a 10-column tibble round trip is 51 us at n = 10,
  132 us at n = 10,000. That matches P3's "slower while steps go through the adapter".
* Use `.Call(<native symbol>)` directly from the engine's R code, not savvy's
  generated environment wrappers (they add 0.1-0.6 us per call).
* **extendr comparison: not run.** The R-driven loop is far inside budget with savvy,
  so the kill criterion that would have weighted the bridge choice did not trigger.
  S5 compares the two templates for the build. Measuring extendr's call cost would need
  a second spike package (rextendr 0.5.0 is now installed, so it is feasible later).
