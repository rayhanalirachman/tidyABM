/*
 * Long-double reductions for tidyABM's Rust engine (migration phase 2).
 *
 * R documents that sum() accumulates in a long double where the platform has a
 * wider one and that mean() refines its first estimate with a second pass. Rust
 * has no long double, so these few loops are written in C and called from Rust.
 * Where long double is a plain double (arm64 macOS and Windows) they reduce to
 * ordinary double loops. Written for tidyABM from that documented behaviour and
 * checked against R by differential tests (tests/testthat/test-rust-reduce.R); no
 * code here comes from R's sources.
 *
 * Every loop adds left to right, the order the result depends on. No R headers:
 * NA handling that needs R's NA bit patterns stays on the Rust side.
 */
#include <stddef.h>
#include <stdlib.h>

typedef long double ldouble;

/* sizeof(long double) of the compiler that built the package (8 or 16 bytes). */
int tidyabm_ld_bytes(void) { return (int) sizeof(ldouble); }

/* x != x is true only for NaN, which includes R's NA_real_. */
static int is_nan(double v) { return v != v; }

static int is_finite(ldouble v) { return v == v && v - v == 0; }

/* The mean of nothing: 0/0 computed at run time, so its NaN has the hardware's
   sign bit (set on x86_64, clear on arm64), as R's mean() of an empty vector.
   volatile stops the compiler folding it into a NaN constant of its own choice. */
static double empty_mean(void) {
    volatile ldouble zero = 0;
    return (double) (zero / zero);
}

/* sum(x, na.rm = narm) of a double vector. */
double tidyabm_ld_sum(const double *x, ptrdiff_t n, int narm) {
    ldouble s = 0;
    for (ptrdiff_t i = 0; i < n; i++)
        if (!narm || !is_nan(x[i])) s += x[i];
    return (double) s;
}

/*
 * mean(x) of a double vector without NA removal (mean.default removes them first).
 * n may be 0.
 * The first pass is sum / n, corrected by the mean of the residuals when finite.
 * When the sum overflows although the mean would not, both passes are redone on
 * values divided by n first. (The scaled variant was singled out by differential
 * tests against R: 300 of 300 overflowing vectors identical.)
 */
double tidyabm_ld_mean(const double *x, ptrdiff_t n) {
    ldouble s = 0, t = 0;
    if (n == 0) return empty_mean();
    for (ptrdiff_t i = 0; i < n; i++) s += x[i];
    s /= n;
    if (is_finite(s)) {
        for (ptrdiff_t i = 0; i < n; i++) t += x[i] - s;
        return (double) (s + t / n);
    }
    s = 0;
    for (ptrdiff_t i = 0; i < n; i++) s += x[i] / n;
    if (is_finite(s)) {
        for (ptrdiff_t i = 0; i < n; i++) t += (x[i] - s) / n;
        s += t;
    }
    return (double) s;
}

/* mean(x) of an integer vector known to hold no NA (n may be 0). */
double tidyabm_ld_mean_int(const int *x, ptrdiff_t n) {
    if (n == 0) return empty_mean();
    ldouble s = 0;
    for (ptrdiff_t i = 0; i < n; i++) s += x[i];
    return (double) (s / n);
}

/* cumsum(x) of a double vector into out. */
void tidyabm_ld_cumsum(const double *x, double *out, ptrdiff_t n) {
    ldouble s = 0;
    for (ptrdiff_t i = 0; i < n; i++) {
        s += x[i];
        out[i] = (double) s;
    }
}

/*
 * Per-group sums, each group summed like sum(x[g == k]) (input order, long double):
 * g holds 1-based group codes in 1..ngroups, out has ngroups slots. Returns 0, or
 * -1 when out of memory, -2 for a code outside 1..ngroups.
 */
int tidyabm_ld_group_sum(const double *x, const int *g, ptrdiff_t n, double *out,
                         int ngroups, int narm) {
    if (ngroups < 1) return 0;
    ldouble *acc = calloc((size_t) ngroups, sizeof(ldouble));
    if (acc == NULL) return -1;
    for (ptrdiff_t i = 0; i < n; i++) {
        if (g[i] < 1 || g[i] > ngroups) {
            free(acc);
            return -2;
        }
        if (!narm || !is_nan(x[i])) acc[g[i] - 1] += x[i];
    }
    for (int k = 0; k < ngroups; k++) out[k] = (double) acc[k];
    free(acc);
    return 0;
}
