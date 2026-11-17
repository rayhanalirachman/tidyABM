//! Spike S2: R's double reductions and `^` in Rust, in R's order and accumulator
//! width. On platforms where R's LDOUBLE is 8 bytes (macOS arm64) f64 is R's
//! accumulator; on x86_64 R accumulates in 80-bit long double, which Rust cannot
//! express: there the C helper `../ldsum.c` must be used instead.

/// R's NA_real_: a NaN whose low word is 1954.
pub fn is_r_na(x: f64) -> bool {
    x.is_nan() && (x.to_bits() & 0xffff_ffff) == 1954
}

/// `sum(x, na.rm)` for doubles (summary.c `rsum`).
pub fn sum(x: &[f64], na_rm: bool) -> f64 {
    let mut s = 0.0f64;
    for &v in x {
        if !na_rm || !v.is_nan() {
            s += v;
        }
    }
    if s > f64::MAX {
        f64::INFINITY
    } else if s < -f64::MAX {
        f64::NEG_INFINITY
    } else {
        s
    }
}

/// `mean(x)` for doubles without na.rm (summary.c `real_mean`).
pub fn mean(x: &[f64]) -> f64 {
    let n = x.len() as f64;
    let mut s: f64 = x.iter().fold(0.0, |s, &v| s + v);
    let finite_s = s.is_finite();
    if finite_s {
        s /= n;
    } else {
        s = x.iter().fold(0.0, |s, &v| s + v / n);
    }
    if finite_s && s.is_finite() {
        let t: f64 = x.iter().fold(0.0, |t, &v| t + (v - s));
        s += t / n;
    } else if s.is_finite() {
        let t: f64 = x.iter().fold(0.0, |t, &v| t + (v - s) / n);
        s += t;
    }
    s
}

/// `cumsum(x)` for doubles (cum.c `cumsum` + `handleNaN`).
pub fn cumsum(x: &[f64]) -> Vec<f64> {
    let mut sum = 0.0f64;
    let mut out: Vec<f64> = x
        .iter()
        .map(|&v| {
            sum += v;
            sum
        })
        .collect();
    if sum.is_nan() {
        let (mut has_nan, mut has_na) = (false, false);
        for (o, &v) in out.iter_mut().zip(x) {
            has_nan = has_nan || v.is_nan();
            has_na = has_na || (has_nan && is_r_na(v));
            if has_na {
                *o = R_NA;
            } else if has_nan {
                *o = R_NAN;
            }
        }
    }
    out
}

/// R's NA_real_ and R_NaN bit patterns (const `from_bits` needs rustc >= 1.83).
pub const R_NA: f64 = f64::from_bits(0x7ff0_0000_0000_07a2);
pub const R_NAN: f64 = f64::from_bits(0x7ff8_0000_0000_0000);

/// `x ^ y` for doubles (arithmetic.c `R_pow`, non-Windows branch: `pow()`).
pub fn r_pow(x: f64, y: f64) -> f64 {
    if y == 2.0 {
        return x * x;
    }
    if x == 1.0 || y == 0.0 {
        return 1.0;
    }
    if x == 0.0 {
        return if y > 0.0 {
            0.0
        } else if y < 0.0 {
            f64::INFINITY
        } else {
            y
        };
    }
    if (-11.0..=11.0).contains(&x) {
        if y == 4.0 {
            return x * x * x * x;
        }
        if y == 3.0 {
            return x * x * x;
        }
    }
    if x.is_finite() && y.is_finite() {
        return x.powf(y);
    }
    if x.is_nan() || y.is_nan() {
        return x + y;
    }
    if !x.is_finite() {
        if x > 0.0 {
            return if y < 0.0 { 0.0 } else { f64::INFINITY };
        } else if y.is_finite() && y == y.floor() {
            // myfmod(y, 2) != 0 <=> y is odd
            return if y < 0.0 {
                0.0
            } else if y % 2.0 != 0.0 {
                x
            } else {
                -x
            };
        }
    }
    if !y.is_finite() && x >= 0.0 {
        return if y > 0.0 {
            if x >= 1.0 {
                f64::INFINITY
            } else {
                0.0
            }
        } else if x < 1.0 {
            f64::INFINITY
        } else {
            0.0
        };
    }
    R_NAN
}

/// `math1_ari` (arithmetic.c): `x == arg` gives `res`; a NaN input is returned as is
/// (keeps NA vs NaN and the payload bits).
#[inline]
fn math1_ari(x: f64, f: fn(f64) -> f64, arg: f64, res: f64) -> f64 {
    let y = if x == arg { res } else { f(x) };
    if y.is_nan() && x.is_nan() {
        x
    } else {
        y
    }
}
/// `math1` (no special argument).
#[inline]
fn math1(x: f64, f: fn(f64) -> f64) -> f64 {
    let y = f(x);
    if y.is_nan() && x.is_nan() {
        x
    } else {
        y
    }
}
const DBL_EPSILON: f64 = f64::EPSILON;
/// R's `exp()` (R >= 4.5 `Rexp`: 1 + x for |x| <= sqrt(eps)).
pub fn r_exp(x: f64) -> f64 {
    math1_ari(
        x,
        |x| {
            if x.abs() <= DBL_EPSILON.sqrt() {
                1.0 + x
            } else {
                x.exp()
            }
        },
        0.0,
        1.0,
    )
}
/// R's `expm1()` (`Rexpm1`: x for |x| <= eps).
pub fn r_expm1(x: f64) -> f64 {
    math1_ari(
        x,
        |x| {
            if x.abs() <= DBL_EPSILON {
                x
            } else {
                x.exp_m1()
            }
        },
        0.0,
        0.0,
    )
}
/// R's `log1p()` (`Rlog1p`: x for |x| <= eps).
pub fn r_log1p(x: f64) -> f64 {
    math1_ari(
        x,
        |x| if x.abs() <= DBL_EPSILON { x } else { x.ln_1p() },
        0.0,
        0.0,
    )
}
/// R's `sqrt()` (`Rsqrt`: exact for 0 and the squares 1..121).
pub fn r_sqrt(x: f64) -> f64 {
    math1(x, |x| {
        if x == 0.0 {
            return x;
        }
        for i in 1..12 {
            if x == (i * i) as f64 {
                return i as f64;
            }
        }
        x.sqrt()
    })
}
/// R's one-argument `log()` (`R_log`: x > 0 ? log(x) : x == 0 ? -Inf : NaN).
pub fn r_log(x: f64) -> f64 {
    math1(x, |x| {
        if x > 0.0 {
            x.ln()
        } else if x == 0.0 {
            f64::NEG_INFINITY
        } else {
            R_NAN
        }
    })
}

/// Not R code: a plain `a * b + c`, compiled to check Rust never contracts it to FMA.
#[inline(never)]
pub fn axpb(a: f64, b: f64, c: f64) -> f64 {
    a * b + c
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn na_bits() {
        assert!(is_r_na(R_NA));
        assert!(!is_r_na(R_NAN));
    }
    #[test]
    fn pow_specials() {
        assert_eq!(r_pow(0.0, -1.0), f64::INFINITY);
        assert_eq!(r_pow(f64::NEG_INFINITY, 3.0), f64::NEG_INFINITY);
        assert_eq!(r_pow(f64::NEG_INFINITY, 2.0), f64::INFINITY);
        assert!(r_pow(-2.0, 0.5).is_nan());
    }
}
