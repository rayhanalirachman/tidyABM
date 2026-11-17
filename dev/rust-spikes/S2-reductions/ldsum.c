/* Spike S2: long-double reductions in R's order, for platforms where R's LDOUBLE is
 * wider than double (x86_64 Linux/macOS/Windows). Called from R with .Call; in the
 * package it would be linked into the Rust ffi crate. Mirrors summary.c rsum /
 * real_mean and cum.c cumsum (handleNaN is done on the Rust side, see rreduce). */
#include <R.h>
#include <Rinternals.h>
#include <float.h>
SEXP ld_sum(SEXP x_) {
  const double *x = REAL(x_); R_xlen_t n = XLENGTH(x_); long double s = 0.0;
  for (R_xlen_t i = 0; i < n; i++) s += x[i];
  return ScalarReal(s > DBL_MAX ? R_PosInf : (s < -DBL_MAX ? R_NegInf : (double) s));
}
SEXP ld_mean(SEXP x_) {
  const double *x = REAL(x_); R_xlen_t n = XLENGTH(x_); long double s = 0.0;
  for (R_xlen_t i = 0; i < n; i++) s += x[i];
  int finite_s = R_FINITE((double) s);
  if (finite_s) s /= n; else { s = 0.; for (R_xlen_t i = 0; i < n; i++) s += x[i] / n; }
  if (finite_s && R_FINITE((double) s)) { long double t = 0.0; for (R_xlen_t i = 0; i < n; i++) t += (x[i] - s); s += t / n; }
  else if (R_FINITE((double) s)) { long double t = 0.0; for (R_xlen_t i = 0; i < n; i++) t += (x[i] - s) / n; s += t; }
  return ScalarReal((double) s);
}
SEXP ld_cumsum(SEXP x_) {
  const double *x = REAL(x_); R_xlen_t n = XLENGTH(x_); long double s = 0.0;
  SEXP out = PROTECT(allocVector(REALSXP, n)); double *o = REAL(out);
  for (R_xlen_t i = 0; i < n; i++) { s += x[i]; o[i] = (double) s; }
  UNPROTECT(1); return out;
}
SEXP ld_size(void) { return ScalarInteger((int) sizeof(long double)); }
