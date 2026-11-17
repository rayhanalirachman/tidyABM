# S2: reductions, libm and `^` in Rust vs R

Date 2026-11-17. macOS arm64, R 4.6.1 (`sizeof(long double) = 8`,
`capabilities("long.double") = FALSE`), rustc/cargo 1.100.0, crate `rreduce` 0.0.1 (no
dependencies), C helper `ldsum.c` built with `R CMD SHLIB` (Apple clang 17).

## What was measured

`compare.R` writes inputs to a file, runs `rreduce/target/release/apply <op>` and
compares with R using `identical()`:

* `sum`, `sum(na.rm = TRUE)`, `mean`, `cumsum` on 32 vectors (n = 1e2, 1e4, 1e6, 1e7;
  uniform, +-1e300 wide range, catastrophic cancellation, NA/NaN mixed, NaN-before-NA,
  NA-before-NaN, +-Inf, overflow to Inf, a mix of specials incl. -0 and subnormals);
  ports of `rsum`, `real_mean` (incl. the overflow retry branch) and `cumsum` +
  `handleNaN` from R's summary.c / cum.c.
* `exp`, `log`, `sqrt`, `log2`, `log10`, `log1p`, `expm1` over ~1e7 inputs each, and
  `x ^ y` over 1e7 pairs plus 1e5 special pairs (port of `R_pow`).
* the same reductions through `ldsum.c` (long double accumulator) called with `.Call`.
* Rust code generation: `--emit asm` of the crate.

## Results (macOS arm64)

* **Reductions: 128 / 128 identical** (incl. which of NA / NaN wins, overflow, -0). On
  this platform R's accumulator is a plain double, so a left-to-right f64 loop in R's
  order is exact. `ldsum.c` is also identical here (it degenerates to double).
* **Math: identical for all 7 functions and `^`, but only after porting R's own
  wrappers.** R >= 4.5 does not call libm directly for several functions
  (arithmetic.c): `Rexp` returns `1 + x` for `|x| <= sqrt(eps)`, `Rexpm1`/`Rlog1p`
  return `x` for `|x| <= eps`, `Rsqrt` returns exact integers for 0 and 1..121,
  `math1_ari` maps `x == 0` to a fixed result (`log1p(-0)` is `+0` in R, `-0` in libm)
  and keeps an incoming NaN as is. Plain Rust `f64::exp` / `f64::ln_1p` (same Apple
  libm) **differ** from R (`log1p`: 1 ulp for 1.6e-16 < |x| < 2.2e-16, and the sign of
  zero). So the expression engine must port R's wrapper layer function by function,
  not call `std` directly; the wrapper list is version-dependent (it changed in R 4.5),
  which is one more reason to gate the compiler on the R version (as on dplyr's).
* `R_pow` on non-Windows calls `pow()`; **on 64-bit Windows it calls `powl()`**
  (`USE_POWL_IN_R_POW`), i.e. 80-bit long double. Rust has no `powl`: on Windows `^`
  needs a C helper or the R path.
* **No FMA contraction in Rust**: `a * b + c` compiles to `fmul` + `fadd`; the only
  `fmadd` in the crate is LLVM's exact lowering of `y % 2.0` inside `R_pow`
  (fmod-style, exact result). Rust never contracts unless `mul_add` is written, so FMA
  placement is fully under our control (as S1 needs).
* NaN payloads: R returns the input NaN itself (`math1_ari`), so NA stays NA bit for bit;
  the port does the same. `identical()` does not compare payload bits, so the store's
  "R bit patterns" rule (design rule 5) needs its own bit-level test in P2.

## Not run (needs other platforms)

* x86_64 Linux/macOS/Windows: R accumulates sums in 80-bit x87 long double; the Rust
  f64 loop will differ there, `ldsum.c` is the planned fix and is written but only
  checked here where long double == double. Windows libm (UCRT/mingw) vs Rust `std`
  math and `powl`: not checked. CI workflow: `../ci/spikes.yaml`.

## Verdict against PLAN.md

**PASS on macOS arm64** for D2's default (identical, no tolerance): reductions with
f64, math via ported R wrappers. **Plan changes**: (1) P2/P5 scope adds "port R's math
wrapper layer (`Rexp`, `Rsqrt`, `Rlog1p`, `Rexpm1`, `math1_ari`, `R_log`, `logbase`,
`R_pow`/`powl` on Windows)" with an R-version gate; (2) Windows `^` needs `powl` via C
or the R path (decision D2 (a) applies until CI proves otherwise). x86_64 bit-identity
of `ldsum.c` remains open until the CI job runs (kill criterion not hit; it is a
per-platform switch, not a design change).
