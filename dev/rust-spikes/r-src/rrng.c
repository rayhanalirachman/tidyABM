// Standalone re-implementation of R's set.seed + MT + runif/rnorm/sample.int,
// checked against R. qnorm is either libR's Rf_qnorm5 or a local transcription.
#include <stdio.h>
#include <stdint.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
extern double Rf_qnorm5(double, double, double, int, int);

static uint32_t dummy[625];
static uint32_t *mt = dummy + 1;
#define N 624
#define M 397
static void set_seed(int32_t s) {
  uint32_t seed = (uint32_t)s;
  for (int j = 0; j < 50; j++) seed = 69069 * seed + 1;
  for (int j = 0; j < 625; j++) { seed = 69069 * seed + 1; dummy[j] = seed; }
  dummy[0] = 624; // FixupSeeds(initial=1): mti = N
}
static double MT_genrand(void) {
  static const uint32_t mag01[2] = {0x0, 0x9908b0df};
  int mti = (int)dummy[0]; uint32_t y;
  if (mti >= N) {
    int kk;
    for (kk = 0; kk < N - M; kk++) { y = (mt[kk] & 0x80000000) | (mt[kk+1] & 0x7fffffff); mt[kk] = mt[kk+M] ^ (y >> 1) ^ mag01[y & 1]; }
    for (; kk < N - 1; kk++) { y = (mt[kk] & 0x80000000) | (mt[kk+1] & 0x7fffffff); mt[kk] = mt[kk+(M-N)] ^ (y >> 1) ^ mag01[y & 1]; }
    y = (mt[N-1] & 0x80000000) | (mt[0] & 0x7fffffff); mt[N-1] = mt[M-1] ^ (y >> 1) ^ mag01[y & 1];
    mti = 0;
  }
  y = mt[mti++];
  y ^= (y >> 11); y ^= (y << 7) & 0x9d2c5680; y ^= (y << 15) & 0xefc60000; y ^= (y >> 18);
  dummy[0] = mti;
  return (double)y * 2.3283064365386963e-10;
}
#define i2_32m1 2.328306437080797e-10
static double unif_rand(void) {
  double x = MT_genrand();
  if (x <= 0.0) return 0.5 * i2_32m1;
  if ((1.0 - x) <= 0.0) return 1.0 - 0.5 * i2_32m1;
  return x;
}
static double rbits(int bits) {
  uint64_t v = 0;
  for (int n = 0; n <= bits; n += 16) { int v1 = (int)floor(unif_rand() * 65536); v = 65536 * v + v1; }
  return (double)(v & ((((uint64_t)1) << bits) - 1));
}
static double unif_index(double dn) {
  if (dn <= 0) return 0.0;
  int bits = (int)ceil(log2(dn)); double dv;
  do { dv = rbits(bits); } while (dn <= dv);
  return dv;
}
// transcription of qnorm5 central + r<=5 tail, lower_tail=1, log_p=0
static double my_qnorm(double p) {
  double q = p - 0.5, r, val;
  if (fabs(q) <= .425) {
    r = .180625 - q * q;
    val = q * (((((((r * 2509.0809287301226727 + 33430.575583588128105) * r + 67265.770927008700853) * r + 45921.953931549871457) * r + 13731.693765509461125) * r + 1971.5909503065514427) * r + 133.14166789178437745) * r + 3.387132872796366608)
        / (((((((r * 5226.495278852854561 + 28729.085735721942674) * r + 39307.89580009271061) * r + 21213.794301586595867) * r + 5394.1960214247511077) * r + 687.1870074920579083) * r + 42.313330701600911252) * r + 1.);
    return val;
  }
  r = sqrt(-log(q > 0 ? 1 - p : p));
  if (r > 5.) return NAN; // not transcribed
  r += -1.6;
  val = (((((((r * 7.7454501427834140764e-4 + .0227238449892691845833) * r + .24178072517745061177) * r + 1.27045825245236838258) * r + 3.64784832476320460504) * r + 5.7694972214606914055) * r + 4.6303378461565452959) * r + 1.42343711074968357734)
      / (((((((r * 1.05075007164441684324e-9 + 5.475938084995344946e-4) * r + .0151986665636164571966) * r + .14810397642748007459) * r + .68976733498510000455) * r + 1.6763848301838038494) * r + 2.05319162663775882187) * r + 1.);
  return q < 0 ? -val : val;
}
static double norm_from(double (*qn)(double)) {
  double u1 = unif_rand();
  u1 = (int)(134217728 * u1) + unif_rand();
  return qn(u1 / 134217728);
}
static double libq(double p) { return Rf_qnorm5(p, 0, 1, 1, 0); }

int main(int argc, char **argv) {
  set_seed(42);
  for (int i = 0; i < 3; i++) printf("runif %.17g\n", unif_rand());
  set_seed(42);
  for (int i = 0; i < 2; i++) printf("rnorm %.17g\n", norm_from(libq));
  set_seed(1);
  printf("sample.int(100,5):"); { int n = 100, x[100]; for (int i = 0; i < n; i++) x[i] = i;
    for (int i = 0; i < 5; i++) { int j = (int)unif_index(n); printf(" %d", x[j] + 1); x[j] = x[--n]; } } printf("\n");
  // compare local qnorm transcription (contract mode set at compile time) with libR on 1e7 draws
  set_seed(2024); long diff = 0, n = 10000000, nan = 0;
  for (long i = 0; i < n; i++) {
    double u1 = unif_rand(); u1 = (int)(134217728 * u1) + unif_rand(); double p = u1 / 134217728;
    double a = my_qnorm(p), b = libq(p);
    if (isnan(a)) { nan++; continue; }
    if (a != b) diff++;
  }
  printf("qnorm transcription vs libR: %ld of %ld differ (%ld skipped)\n", diff, n, nan);
  return 0;
}
