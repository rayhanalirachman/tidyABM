//! Spike S1: a pure-Rust port of R's default random number generation
//! (`RNGkind() == c("Mersenne-Twister", "Inversion", "Rejection")`), written from
//! R's C sources (src/main/RNG.c, src/main/random.c, src/main/sort.c,
//! src/nmath/{snorm,sexp,qnorm}.c). No R dependency: usable on worker threads.
//!
//! Bit-identity with R is checked by `tests/compare.R` against the R binary on the
//! running platform. `qnorm5` uses `f64::mul_add` exactly where CRAN's arm64 macOS
//! libR has fused multiply-adds (clang `-ffp-contract=on`); other platforms use
//! plain arithmetic until a platform's goldens say otherwise.

// Literals are copied digit for digit from R's C sources (incl. its ln 2 table entry).
#![allow(clippy::excessive_precision, clippy::approx_constant)]

const N: usize = 624;
const M: usize = 397;
const MATRIX_A: u32 = 0x9908_b0df;
const UPPER_MASK: u32 = 0x8000_0000;
const LOWER_MASK: u32 = 0x7fff_ffff;
const I2_32M1: f64 = 2.328_306_437_080_797e-10; // 1/(2^32 - 1)

/// R's Mersenne-Twister state: `dummy[0]` is `mti`, `dummy[1..=624]` is `mt`.
/// `.Random.seed` is `c(10403L, dummy)`.
#[derive(Clone)]
pub struct RRng {
    dummy: [u32; N + 1],
}

impl RRng {
    /// `set.seed(seed)` for the default kinds (RNG_Init + FixupSeeds).
    pub fn set_seed(seed: i32) -> Self {
        let mut s = seed as u32;
        for _ in 0..50 {
            s = s.wrapping_mul(69069).wrapping_add(1);
        }
        let mut dummy = [0u32; N + 1];
        for d in dummy.iter_mut() {
            s = s.wrapping_mul(69069).wrapping_add(1);
            *d = s;
        }
        dummy[0] = N as u32; // FixupSeeds(initial = TRUE): mti = N
        RRng { dummy }
    }

    /// Restore from an R `.Random.seed` (626 integers, first = 10403).
    pub fn from_random_seed(seed: &[i32]) -> Option<Self> {
        if seed.len() != N + 2 || seed[0] != 10403 {
            return None;
        }
        let mut dummy = [0u32; N + 1];
        for (d, s) in dummy.iter_mut().zip(&seed[1..]) {
            *d = *s as u32;
        }
        Some(RRng { dummy })
    }

    /// The equivalent of R's `.Random.seed` after these draws.
    pub fn random_seed(&self) -> Vec<i32> {
        let mut v = Vec::with_capacity(N + 2);
        v.push(10403);
        v.extend(self.dummy.iter().map(|&d| d as i32));
        v
    }

    fn mt_genrand(&mut self) -> f64 {
        const MAG01: [u32; 2] = [0, MATRIX_A];
        let mut mti = self.dummy[0] as usize;
        let mt = &mut self.dummy[1..];
        if mti >= N {
            // mti == N + 1 (never seeded) cannot happen after set_seed / from_random_seed
            for kk in 0..N - M {
                let y = (mt[kk] & UPPER_MASK) | (mt[kk + 1] & LOWER_MASK);
                mt[kk] = mt[kk + M] ^ (y >> 1) ^ MAG01[(y & 1) as usize];
            }
            for kk in N - M..N - 1 {
                let y = (mt[kk] & UPPER_MASK) | (mt[kk + 1] & LOWER_MASK);
                mt[kk] = mt[kk + M - N] ^ (y >> 1) ^ MAG01[(y & 1) as usize];
            }
            let y = (mt[N - 1] & UPPER_MASK) | (mt[0] & LOWER_MASK);
            mt[N - 1] = mt[M - 1] ^ (y >> 1) ^ MAG01[(y & 1) as usize];
            mti = 0;
        }
        let mut y = mt[mti];
        mti += 1;
        y ^= y >> 11;
        y ^= (y << 7) & 0x9d2c_5680;
        y ^= (y << 15) & 0xefc6_0000;
        y ^= y >> 18;
        self.dummy[0] = mti as u32;
        (y as f64) * 2.328_306_436_538_696_3e-10 // 2^-32
    }

    /// `unif_rand()`.
    pub fn unif_rand(&mut self) -> f64 {
        let x = self.mt_genrand();
        if x <= 0.0 {
            0.5 * I2_32M1
        } else if 1.0 - x <= 0.0 {
            1.0 - 0.5 * I2_32M1
        } else {
            x
        }
    }

    fn rbits(&mut self, bits: i32) -> f64 {
        let mut v: i64 = 0;
        let mut n = 0;
        while n <= bits {
            let v1 = (self.unif_rand() * 65536.0).floor() as i32;
            v = 65536 * v + v1 as i64;
            n += 16;
        }
        (v & ((1i64 << bits) - 1)) as f64
    }

    /// `R_unif_index(dn)` with sample.kind = "Rejection".
    pub fn unif_index(&mut self, dn: f64) -> f64 {
        if dn <= 0.0 {
            return 0.0;
        }
        let bits = dn.log2().ceil() as i32;
        loop {
            let dv = self.rbits(bits);
            if dn > dv {
                return dv;
            }
        }
    }

    /// `norm_rand()` with kind "Inversion".
    pub fn norm_rand(&mut self) -> f64 {
        const BIG: f64 = 134_217_728.0; // 2^27
        let u = self.unif_rand();
        let u = ((BIG * u) as i32) as f64 + self.unif_rand();
        qnorm_std(u / BIG)
    }

    /// `exp_rand()` (Ahrens & Dieter 1972).
    pub fn exp_rand(&mut self) -> f64 {
        const Q: [f64; 16] = [
            0.693_147_180_559_945_3,
            0.933_373_687_519_045_9,
            0.988_877_796_183_867_5,
            0.998_495_925_291_496_004_0,
            0.999_829_281_106_138_9,
            0.999_983_316_410_072_7,
            0.999_998_569_143_876_7,
            0.999_999_890_692_555_8,
            0.999_999_992_473_415_9,
            0.999_999_999_528_327_5,
            0.999_999_999_972_881_4,
            0.999_999_999_998_559_8,
            0.999_999_999_999_928_9,
            0.999_999_999_999_996_8,
            0.999_999_999_999_999_9,
            1.0,
        ];
        let mut a = 0.0;
        let mut u = self.unif_rand();
        while u <= 0.0 || u >= 1.0 {
            u = self.unif_rand();
        }
        loop {
            u += u;
            if u > 1.0 {
                break;
            }
            a += Q[0];
        }
        u -= 1.0;
        if u <= Q[0] {
            return a + u;
        }
        let mut i = 0;
        let mut ustar = self.unif_rand();
        let mut umin = ustar;
        loop {
            ustar = self.unif_rand();
            if umin > ustar {
                umin = ustar;
            }
            i += 1;
            if u <= Q[i] {
                break;
            }
        }
        fma(umin, Q[0], a) // contracted to fmadd in CRAN's arm64 libR
    }

    /// `sample.int(n, k, replace = TRUE)` without prob (1-based results).
    pub fn sample_replace(&mut self, n: usize, k: usize) -> Vec<i32> {
        (0..k)
            .map(|_| self.unif_index(n as f64) as i32 + 1)
            .collect()
    }

    /// `sample.int(n, k)` without replacement, n <= 1e7 (partial Fisher-Yates).
    pub fn sample_noreplace(&mut self, n: usize, k: usize) -> Vec<i32> {
        let mut x: Vec<i32> = (0..n as i32).collect();
        let mut n = n;
        let mut y = Vec::with_capacity(k);
        for _ in 0..k {
            let j = self.unif_index(n as f64) as usize;
            y.push(x[j] + 1);
            n -= 1;
            x[j] = x[n];
        }
        y
    }

    /// `sample.int(n, k)` via `.Internal(sample2())`: used by R when n > 1e7, no
    /// replace, k <= n/2 (tidyABM's `run_seeds()` hits this with n = 2^31 - 1).
    pub fn sample2(&mut self, n: f64, k: usize) -> Vec<i32> {
        let mut seen = std::collections::HashSet::with_capacity(k);
        let mut y = Vec::with_capacity(k);
        for _ in 0..k {
            let mut v = 0;
            for _ in 0..100 {
                v = self.unif_index(n) as i32 + 1;
                if seen.insert(v) {
                    break;
                }
            }
            y.push(v);
        }
        y
    }

    /// `sample.int(n, k, replace, prob)`: FixupProb then the algorithm R picks.
    /// Returns None where R would raise an error.
    pub fn sample_prob(&mut self, prob: &[f64], k: usize, replace: bool) -> Option<Vec<i32>> {
        let n = prob.len();
        let mut p = prob.to_vec();
        let mut sum = 0.0;
        let mut npos = 0;
        for &pi in &p {
            if !pi.is_finite() || pi < 0.0 {
                return None;
            }
            if pi > 0.0 {
                npos += 1;
                sum += pi;
            }
        }
        if npos == 0 || (!replace && k > npos) {
            return None;
        }
        for pi in p.iter_mut() {
            *pi /= sum;
        }
        let mut perm: Vec<i32> = (1..=n as i32).collect();
        let mut ans = vec![0i32; k];
        if replace {
            let nc = p.iter().filter(|&&pi| n as f64 * pi > 0.1).count();
            if nc > 200 {
                self.walker(&p, &mut ans);
            } else {
                // ProbSampleReplace
                revsort(&mut p, &mut perm);
                for i in 1..n {
                    p[i] += p[i - 1];
                }
                for a in ans.iter_mut() {
                    let ru = self.unif_rand();
                    let mut j = 0;
                    while j < n - 1 {
                        if ru <= p[j] {
                            break;
                        }
                        j += 1;
                    }
                    *a = perm[j];
                }
            }
        } else {
            // ProbSampleNoReplace
            revsort(&mut p, &mut perm);
            let mut totalmass = 1.0;
            let mut n1 = n - 1;
            for a in ans.iter_mut() {
                let rt = totalmass * self.unif_rand();
                let mut mass = 0.0;
                let mut j = 0;
                while j < n1 {
                    mass += p[j];
                    if rt <= mass {
                        break;
                    }
                    j += 1;
                }
                *a = perm[j];
                totalmass -= p[j];
                for kk in j..n1 {
                    p[kk] = p[kk + 1];
                    perm[kk] = perm[kk + 1];
                }
                n1 = n1.saturating_sub(1);
            }
        }
        Some(ans)
    }

    fn walker(&mut self, p: &[f64], ans: &mut [i32]) {
        let n = p.len();
        let mut q = vec![0.0; n];
        let mut a = vec![0usize; n];
        // HL holds the "small" indices growing from the front (H) and the "large"
        // ones growing from the back (L), as in R's walker_ProbSampleReplace.
        let mut hl = vec![0usize; n];
        let mut h = 0usize; // next free slot from the front
        let mut l = n; // first used slot from the back
        for i in 0..n {
            q[i] = p[i] * n as f64;
            if q[i] < 1.0 {
                hl[h] = i;
                h += 1;
            } else {
                l -= 1;
                hl[l] = i;
            }
        }
        if h > 0 && l < n {
            for k in 0..n - 1 {
                let i = hl[k];
                let j = hl[l];
                a[i] = j;
                q[j] += q[i] - 1.0;
                if q[j] < 1.0 {
                    l += 1;
                }
                if l >= n {
                    break;
                }
            }
        }
        for (i, qi) in q.iter_mut().enumerate() {
            *qi += i as f64;
        }
        for out in ans.iter_mut() {
            // R >= 3.6: index by R_unif_index, then one more uniform (research/rng-fp.md 3)
            let k = self.unif_index(n as f64) as usize;
            let ru = k as f64 + self.unif_rand();
            *out = if ru < q[k] {
                k as i32 + 1
            } else {
                a[k] as i32 + 1
            };
        }
    }
}

/// R's `revsort` (src/main/sort.c): heapsort into descending order, carrying `ib`.
/// Not stable; ported line for line because tie order is part of the result.
pub fn revsort(a: &mut [f64], ib: &mut [i32]) {
    let n = a.len();
    if n <= 1 {
        return;
    }
    // 1-based indexing as in the C code
    let (mut l, mut ir) = ((n >> 1) + 1, n);
    let (mut ra, mut ii);
    loop {
        if l > 1 {
            l -= 1;
            ra = a[l - 1];
            ii = ib[l - 1];
        } else {
            ra = a[ir - 1];
            ii = ib[ir - 1];
            a[ir - 1] = a[0];
            ib[ir - 1] = ib[0];
            ir -= 1;
            if ir == 1 {
                a[0] = ra;
                ib[0] = ii;
                return;
            }
        }
        let mut i = l;
        let mut j = l << 1;
        while j <= ir {
            if j < ir && a[j - 1] > a[j] {
                j += 1;
            }
            if ra > a[j - 1] {
                a[i - 1] = a[j - 1];
                ib[i - 1] = ib[j - 1];
                i = j;
                j += i;
            } else {
                j = ir + 1;
            }
        }
        a[i - 1] = ra;
        ib[i - 1] = ii;
    }
}

/// `a * b + c`, fused where CRAN's libR is fused on this platform.
#[inline(always)]
fn fma(a: f64, b: f64, c: f64) -> f64 {
    if cfg!(all(target_arch = "aarch64", target_os = "macos")) {
        a.mul_add(b, c)
    } else {
        a * b + c
    }
}

/// `qnorm5(p, 0, 1, lower_tail = TRUE, log_p = FALSE)` for p in (0, 1), Wichura AS241,
/// as reached from `norm_rand()` (the r > 27 branch is unreachable there).
pub fn qnorm_std(p: f64) -> f64 {
    let q = p - 0.5;
    if q.abs() <= 0.425 {
        let r = fma(-q, q, 0.180625);
        let num = fma(
            fma(
                fma(
                    fma(
                        fma(
                            fma(
                                fma(r, 2509.0809287301226727, 33430.575583588128105),
                                r,
                                67265.770927008700853,
                            ),
                            r,
                            45921.953931549871457,
                        ),
                        r,
                        13731.693765509461125,
                    ),
                    r,
                    1971.5909503065514427,
                ),
                r,
                133.14166789178437745,
            ),
            r,
            3.387132872796366608,
        );
        let den = fma(
            fma(
                fma(
                    fma(
                        fma(
                            fma(
                                fma(r, 5226.495278852854561, 28729.085735721942674),
                                r,
                                39307.89580009271061,
                            ),
                            r,
                            21213.794301586595867,
                        ),
                        r,
                        5394.1960214247511077,
                    ),
                    r,
                    687.1870074920579083,
                ),
                r,
                42.313330701600911252,
            ),
            r,
            1.0,
        );
        return q * num / den;
    }
    let lp = if q > 0.0 { (1.0 - p).ln() } else { p.ln() };
    let mut r = (-lp).sqrt();
    let val = if r <= 5.0 {
        r += -1.6;
        let num = fma(
            fma(
                fma(
                    fma(
                        fma(
                            fma(
                                fma(r, 7.7454501427834140764e-4, 0.0227238449892691845833),
                                r,
                                0.24178072517745061177,
                            ),
                            r,
                            1.27045825245236838258,
                        ),
                        r,
                        3.64784832476320460504,
                    ),
                    r,
                    5.7694972214606914055,
                ),
                r,
                4.6303378461565452959,
            ),
            r,
            1.42343711074968357734,
        );
        let den = fma(
            fma(
                fma(
                    fma(
                        fma(
                            fma(
                                fma(r, 1.05075007164441684324e-9, 5.475938084995344946e-4),
                                r,
                                0.0151986665636164571966,
                            ),
                            r,
                            0.14810397642748007459,
                        ),
                        r,
                        0.68976733498510000455,
                    ),
                    r,
                    1.6763848301838038494,
                ),
                r,
                2.05319162663775882187,
            ),
            r,
            1.0,
        );
        num / den
    } else {
        r += -5.0;
        let num = fma(
            fma(
                fma(
                    fma(
                        fma(
                            fma(
                                fma(r, 2.01033439929228813265e-7, 2.71155556874348757815e-5),
                                r,
                                0.0012426609473880784386,
                            ),
                            r,
                            0.026532189526576123093,
                        ),
                        r,
                        0.29656057182850489123,
                    ),
                    r,
                    1.7848265399172913358,
                ),
                r,
                5.4637849111641143699,
            ),
            r,
            6.6579046435011037772,
        );
        let den = fma(
            fma(
                fma(
                    fma(
                        fma(
                            fma(
                                fma(r, 2.04426310338993978564e-15, 1.4215117583164458887e-7),
                                r,
                                1.8463183175100546818e-5,
                            ),
                            r,
                            7.868691311456132591e-4,
                        ),
                        r,
                        0.0148753612908506148525,
                    ),
                    r,
                    0.13692988092273580531,
                ),
                r,
                0.59983220655588793769,
            ),
            r,
            1.0,
        );
        num / den
    };
    if q < 0.0 {
        -val
    } else {
        val
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Values printed by R 4.6.1 on this Mac (also quoted in research/rng-fp.md).
    #[test]
    fn runif_seed42() {
        let mut g = RRng::set_seed(42);
        let v: Vec<f64> = (0..3).map(|_| g.unif_rand()).collect();
        assert_eq!(
            v,
            [
                0.914_806_043_496_355_41,
                0.937_075_413_297_861_81,
                0.286_139_534_786_343_57
            ]
        );
    }

    #[test]
    fn sample_seed1() {
        let mut g = RRng::set_seed(1);
        assert_eq!(g.sample_noreplace(100, 5), [68, 39, 1, 34, 87]);
    }

    #[test]
    fn revsort_descending_with_perm() {
        let mut a = [0.1, 0.4, 0.2, 0.4, 0.3];
        let mut ib = [1, 2, 3, 4, 5];
        revsort(&mut a, &mut ib);
        assert_eq!(a, [0.4, 0.4, 0.3, 0.2, 0.1]);
        let mut s = ib;
        s.sort();
        assert_eq!(s, [1, 2, 3, 4, 5]);
    }

    #[test]
    fn seed_roundtrip() {
        let mut g = RRng::set_seed(7);
        g.unif_rand();
        let h = RRng::from_random_seed(&g.random_seed()).unwrap();
        assert_eq!(g.random_seed(), h.random_seed());
    }
}
