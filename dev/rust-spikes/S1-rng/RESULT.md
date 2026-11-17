# S1: R's RNG ported to pure Rust: bit-identity

Date 2026-11-17. macOS 26 (Darwin 25.6.0) arm64, R 4.6.1 (CRAN binary, Apple clang 17,
`RNGkind()` = Mersenne-Twister / Inversion / Rejection), rustc 1.100.0 / cargo 1.100.0
(aarch64-apple-darwin), crate `rrng` 0.0.1 with **no dependencies** (Cargo.lock has only
itself). Release profile: lto, codegen-units = 1.

## What was built

`rrng/src/lib.rs` (~450 lines, no `unsafe`, no R dependency, so it can run on worker
threads): `set.seed` scrambling (69069 LCG, 50 + 625 steps, `mti = 624`), MT19937
genrand, `fixup`, `unif_rand`, `rbits` / `R_unif_index` (Rejection), `norm_rand`
(Inversion, 2 uniforms + AS241 `qnorm5`), `exp_rand` (Ahrens-Dieter 1972),
`sample.int` without replacement (partial Fisher-Yates), with replacement,
`sample2` (hash path used for n > 1e7, i.e. tidyABM's `run_seeds()`), `prob =` with
`FixupProb`, `revsort` heapsort ported line for line, `ProbSampleReplace`,
`ProbSampleNoReplace` and Walker alias; `.Random.seed` import/export.

`tests/compare.R` runs the `dump` binary and the same R expression and compares the
draws **and** the 626-integer `.Random.seed` afterwards with `identical()`.

Run: `cd rrng && cargo build --release && cd .. && Rscript tests/compare.R "$PWD/rrng/target/release/dump" <tmpdir> 1e7`

## Result (macOS arm64): 105 / 105 identical

| generator | cases (n x 5 seeds: 1, 2, 42, 2026, -7) | draws per case | identical | Rust ns/draw | R ns/draw |
|---|---|---|---|---|---|
| `runif` | 5 | 10^7 | 5/5 | 3.7 | 7.5 |
| `rnorm` | 5 | 10^7 | 5/5 | 17.8 | 28.3 |
| `rexp` | 5 | 10^7 | 5/5 | 23.2 | |
| `sample.int(n, k, TRUE)`, n = 1, 7, 65536, 65537, 1e6, 2^31-1 | 30 | 10^6 | 30/30 | 16 | |
| `sample.int(n, 10)` x 10^5 calls, n = 10, 441, 1e5 | 15 | 10^6 | 15/15 | | |
| `sample.int(.Machine$integer.max, k)` (sample2) | 5 | 10^4 | 5/5 | 17 | |
| `sample.int(n, k, TRUE, prob)` heavy ties, n = 5, 50, 199 (ProbSampleReplace), 250, 1000 (Walker) | 25 | 10^5 | 25/25 | 28 | |
| `sample.int(n, 10, prob)` x 10^4 calls, n = 10, 50, 300 | 15 | 10^5 | 15/15 | | |

Full log: `results/compare-1e7.txt`; table: `results/s1-results.rds`.

## What it took (honest record of the failures on the way)

The first run was 85/105. Three causes, all found by differential testing:
1. **`qnorm5` coefficient typo** (one digit string of one denominator coefficient,
   3.3 % of normals differed). Found by reading R's own `qnorm.c` (copies of the R C
   sources used are kept in `../r-src/`). Every literal is now cross-checked against
   the R source by script.
2. **`exp_rand` FMA**: CRAN's arm64 libR contracts `a + umin * q[0]` to `fmadd`
   (1.3 % of exponentials differed). Now `mul_add` on aarch64-apple.
3. **Walker alias draw**: R >= 3.6 draws the column with `R_unif_index(n)` and then
   one more `unif_rand()`, not `(int)(unif_rand() * n)` (99 % differed).

The FMA placement in `qnorm5` was confirmed from the disassembly of
`libR.dylib`'s `_Rf_qnorm5` (`otool -tV`): `r = .180625 - q*q` is one `fmsub`, each
Horner step one `fmadd`, then a plain `fmul` (q x numerator) and `fdiv`.

## Verdict against PLAN.md

* **macOS arm64: PASS.** A pure-Rust RNG is bit-identical to R for every generator the
  engine uses today (`runif`, `rnorm`, `sample`/`sample.int` all paths, run seeds) plus
  `rexp`. Platform-specific code: one `fma()` helper that is fused only on
  aarch64-apple; `qnorm5` and `exp_rand` use it.
* **Linux x86_64, Windows x86_64, macOS x86_64: NOT RUN** (no CI runner reachable from
  this session). The non-fused branch of `fma()` is the hypothesis for x86_64 builds of
  R (compiled without FMA by default) and must be confirmed per platform. Command for the
  user is in `../RESULTS.md` (CI workflow `../ci/spikes.yaml`).
* Not ported in S1 (by plan): `rbinom`, `rpois`, `rlnorm` (needs `exp`), other
  `RNGkind()`s. Design rule 9 keeps non-default kinds on R's C RNG.
* Decision D3 (port the RNG for threaded runs): **keep "port it"**, enabled per platform
  by these goldens. macOS arm64 is cleared.
* Speed: the port is faster than R's own generator (3.7 vs 7.5 ns per uniform), so
  using it on worker threads costs nothing.
