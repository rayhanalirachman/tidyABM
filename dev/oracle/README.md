# tidyABM oracle (frozen 2026-11-17, Phase 0a of the Rust migration)

Everything the migration is verified against, copied out of the session scratchpad
so it survives a reboot. Reference implementation = tidyABM at git commit `77ae511`
(round 10 of the speed campaign), snapshot in `ref/tidyABM/` (R/, tests, vignettes,
man, DESCRIPTION, NAMESPACE; `R/` diff-verified equal to the working tree).

Recorded on: macOS 26 (Darwin 25.6.0) arm64, R 4.6.1, dplyr 1.2.1, vctrs 0.7.3,
rlang 1.3.0, igraph 2.3.3. Goldens are bit-level on **this platform and these
versions**; elsewhere they are a regression alarm only (PLAN.md: the pass rule is a
live `identical()` comparison of the R engine and the Rust engine on the same machine).

All scripts take paths with spaces. Run them from anywhere.

**Location.** Frozen in `sandbox/rust-migration/oracle/` in Phase 0 and copied here,
into the package (`dev/` is in `.Rbuildignore`), when Phase 1 started (2026-11-17).
The scripts default to the package this folder lives in (`../..`). Run outputs go to
`results/`, which git ignores here; the Phase 0 freeze logs stay in the sandbox copy
(`sandbox/rust-migration/oracle/results/`). The package now has compiled code, so
the first `pkgload::load_all()` builds the Rust library into `src/` (about a minute;
needs cargo); run one `load_all()` before timing anything.

## Layout

| path | what |
|---|---|
| `ref/tidyABM/` | reference package (git archive of `77ae511`); `load_all()`-able |
| `harness/cases/cases{,2..8}.R` | 291 old-vs-new cases (85+27+5+14+20+101+24+15) |
| `harness/driver.R`, `compare.R` | run cases under `set.seed(424242)` capturing value, warnings, messages, errors (class, parent) and `.Random.seed`; compare with `identical()` |
| `harness/golden/*-old.rds` | case results from pkg-r6 (round 6), the campaign's baseline; round 10 is identical to them |
| `models/scripts/` + `models/golden/` | the 68 models/ scripts and their printed output (pkg-r6) |
| `models/rprofile.R` | `R_PROFILE_USER` that makes `library(tidyABM)` load `$PKG` |
| `bench/tax.R`, `ws.R`, `gross.R` (+ `gross/`) | the three benchmarks (Andrei tax 6x25x40 runs, Wolf-Sheep 40x40 400 ticks x 5 seeds, Gross 2022 SFC 1000 ticks) |
| `bench/golden/{tax,ws,gross}.rds` | their outputs (were `res-tidyABM-v3.rds`, `ws-cur.rds`, `gross-new.rds`) |
| `bench/gross/gross-2022-check.R` | Gross vs the base-R MATLAB transcription (11 series must be identical) |
| `bench/vn-cpu.R` | tax with a `cores` argument |
| `vignettes/src/*.R`, `golden-strict/`, `golden-tolerant/` | vignette code and the outputs of the original package |
| `rustq/` | the C prototype of the tax loop (`tax.c`), floor/classify scripts, profiling drivers, Julia steady-state script |
| `scripts/verify.sh` | tests + harness + vignettes + benchmarks identity + Gross SFC check |
| `scripts/models.sh` | the 68 models/ scripts vs goldens |
| `scripts/pairs.sh`, `median.R` | interleaved A/B timing of the three benchmarks |
| `legacy/` | the scratchpad originals (verify4.sh, pairs*.sh, harness.sh, campaign.log, last logs) for provenance; they hard-code scratchpad paths and are not meant to be run |
| `results/` | logs of runs of the scripts above (git-ignored in the package copy) |

Changes made to the frozen copies (path robustness only, no logic change):
`bench/ws.R`, `bench/vn-cpu.R`, `bench/gross/gross-2022.R` load `$TIDYABM_PKG`
instead of `./tidyABM`; `bench/tax.R` takes the output file as its 2nd argument;
`bench/gross.R` and `gross-2022-check.R` decode Rscript's `~+~` for spaces in
`--file=`.

## How to run and what to expect

```sh
cd tidyABM/dev/oracle                 # the package's own copy
./scripts/verify.sh                   # ~3 min; the package defaults to ../..
./scripts/models.sh                   # ~17 min CPU
./scripts/pairs.sh ../.. ref/tidyABM 3   # ~2.5 min; A/B timing vs the reference
```

| check | expected (round 10, this Mac) |
|---|---|
| testthat, `NOT_CRAN=true` | 379 blocks, 2020 expectations, 0 failed, 0 errors, 0 warnings, 0 skipped (round 10; Phase 1 adds the `test-rust-plumbing.R` blocks) |
| harness | 291 IDENTICAL, 0 DIFFER |
| vignettes | 0 diff lines (corrections, grammar, models, tidyABM; tolerant grammar, tidyABM) |
| benchmarks | tax, ws, gross outputs `identical()` to `bench/golden` |
| Gross SFC check | "paths identical in 11 of 11 series" for seeds 1-3 |
| models/ | 68 scripts, exit 0, 0 diff lines; ~960 s user in total |
| speed (CPU s, median of 3 interleaved pairs) | tax ~3.2, Wolf-Sheep ~4.3-4.4, Gross ~10.9-11.1 |

Single timing runs inside `verify.sh` are not the speed number; use `pairs.sh`
(one Rscript at a time, interleaved, real and CPU, load average logged).

## Status of the freeze run (2026-11-17, against `tidyABM/` = `77ae511`)

* `results/verify-p0/verify.log`: tests 379/2020 green; harness 291/291 IDENTICAL;
  vignettes 0 diff (all six); tax / ws / gross identical to goldens; Gross SFC check
  11/11 for seeds 1-3 (the Gross steps were rerun after fixing the `~+~` path bug,
  noted in the log).
* `results/models-p0/` + `results/models-p0-rerun7/`: 68/68 exit 0, 0 diff (7 scripts
  that write HTML viewers failed in the first run only because the viewer folder was
  missing; fixed in `models.sh`, rerun green).
* `results/pairs-p0/pairs.log` (A = tidyABM/, B = ref/tidyABM, load average ~4):
  median CPU tax 3.2 / 3.2, Wolf-Sheep 4.4 / 4.4, Gross 11.0 / 11.1 s; A == B ==
  golden for all three.

Green: the oracle reproduces the numbers the campaign ended with.
