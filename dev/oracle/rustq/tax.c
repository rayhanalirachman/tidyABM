// Whole tax model tick loop in native code, RNG from R's generator (unif_rand).
// Stand-in for a Rust engine: same LLVM backend, same R C API a Rust crate would call.
#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <math.h>
SEXP tax_run(SEXP type_, SEXP income_, SEXP risk_, SEXP nb_, SEXP rate_, SEXP ticks_) {
  int n = LENGTH(income_), T = asInteger(ticks_);
  int *type = INTEGER(type_), *nb = INTEGER(nb_);          // type: 1 honest 2 dishonest 3 imitating; nb: 4 x n, 0-based
  double *w = REAL(income_), *k = REAL(risk_), rate = asReal(rate_);
  const double tax = 0.30, pen = 0.45;
  double *q = (double*) R_alloc(n, sizeof(double)), *share = (double*) R_alloc(n, sizeof(double)),
         *x = (double*) R_alloc(n, sizeof(double)), *owed = (double*) R_alloc(n, sizeof(double)),
         *ns = (double*) R_alloc(n, sizeof(double));
  for (int i = 0; i < n; i++) { q[i] = 0.02; share[i] = type[i] == 1 ? 1.0 : 0.0; x[i] = 0; owed[i] = 0; }
  SEXP out = PROTECT(allocVector(REALSXP, T)); double *o = REAL(out);
  GetRNGstate();
  for (int t = 0; t < T; t++) {
    for (int i = 0; i < n; i++) {                           // two-pass mean, as base R / the engine does
      const int *b = nb + 4 * i; double s = 0, r = 0;
      for (int j = 0; j < 4; j++) s += share[b[j]];
      s /= 4; for (int j = 0; j < 4; j++) r += share[b[j]] - s;
      ns[i] = s + r / 4;
    }
    double sx = 0, sw = 0;
    for (int i = 0; i < n; i++) {
      double d;
      if (owed[i] > 0) d = owed[i];
      else if (type[i] == 1) d = w[i];
      else if (type[i] == 2) {
        if (q[i] > tax / pen) d = w[i];
        else if (q[i] < tax / (tax + (pen - tax) * exp(k[i] * pen * w[i]))) d = 0;
        else d = fmin(w[i], fmax(0, w[i] - log((1 - q[i]) * tax / (q[i] * (pen - tax))) / (k[i] * pen)));
      } else d = ns[i] * w[i];
      x[i] = d;
    }
    for (int i = 0; i < n; i++) {
      double u = unif_rand();                               // runif(n()) draws for every agent, in order
      int aud = x[i] < w[i] - 1e-9 && u < rate;
      owed[i] = aud ? tax * (w[i] - x[i]) * (1 + pen * w[i]) : 0;
      if (aud) q[i] = 1; else if (q[i] > rate) q[i] = (q[i] - 0.2 < 0) ? rate : q[i] - 0.2;
      share[i] = x[i] / w[i];
    }
    for (int i = 0; i < n; i++) { sx += x[i]; sw += w[i]; }
    o[t] = tax * sx / sw;
  }
  PutRNGstate(); UNPROTECT(1); return out;
}
// neighbour mean kernel alone (design i): CSR-free fixed degree
SEXP nb_mean(SEXP v_, SEXP nb_, SEXP deg_) {
  int n = LENGTH(v_), d = asInteger(deg_); double *v = REAL(v_); int *nb = INTEGER(nb_);
  SEXP out = PROTECT(allocVector(REALSXP, n)); double *o = REAL(out);
  for (int i = 0; i < n; i++) { double s = 0; for (int j = 0; j < d; j++) s += v[nb[d * i + j]]; o[i] = s / d; }
  UNPROTECT(1); return out;
}
