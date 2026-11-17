//! Reductions over R vectors that need no long double (migration phase 2): integer
//! and logical sums, integer cumulative sums, extremes and `any`/`all`, with R's NA
//! rules as documented in `?sum`, `?cumsum`, `?Extremes` and `?any`. Double sums and
//! means need R's long double accumulator and live in the C helper `src/reduce.c`.
//!
//! `None` means "R would warn or return a different type here": the caller takes the
//! R path so that the user sees R's own result and warning (integer overflow, the
//! extremes of an empty vector).

use crate::sample::NA_INT;

/// `sum()` of an integer or logical vector: `Some(NA)` for an NA without `na_rm`,
/// `None` when the sum leaves the integer range (R warns and returns NA).
pub fn sum_int(x: &[i32], na_rm: bool) -> Option<i32> {
    let mut s: i64 = 0;
    for &v in x {
        if v == NA_INT {
            if na_rm {
                continue;
            }
            return Some(NA_INT);
        }
        s += v as i64;
    }
    i32::try_from(s).ok().filter(|&s| s != NA_INT)
}

/// `cumsum()` of an integer vector: NA from the first NA on; `None` on overflow.
pub fn cumsum_int(x: &[i32]) -> Option<Vec<i32>> {
    let mut s: i64 = 0;
    let mut out = Vec::with_capacity(x.len());
    for &v in x {
        if v == NA_INT {
            out.resize(x.len(), NA_INT);
            return Some(out);
        }
        s += v as i64;
        out.push(i32::try_from(s).ok().filter(|&s| s != NA_INT)?);
    }
    Some(out)
}

/// `max()` (`want_max`) or `min()` of a double vector. An NA anywhere gives NA, else
/// a NaN gives NaN (`?Extremes`); with `na_rm` both are dropped. `None` when no value
/// is left (R warns and returns `-Inf` / `Inf`). The first of equal values wins.
pub fn extreme_f64(
    x: &[f64],
    want_max: bool,
    na_rm: bool,
    is_na: impl Fn(f64) -> bool,
) -> Option<f64> {
    let mut best: Option<f64> = None;
    let mut nan: Option<f64> = None;
    for &v in x {
        if v.is_nan() {
            if na_rm {
                continue;
            }
            if is_na(v) {
                return Some(v);
            }
            nan.get_or_insert(v);
            continue;
        }
        best = match best {
            Some(b) if (want_max && v > b) || (!want_max && v < b) => Some(v),
            Some(b) => Some(b),
            None => Some(v),
        };
    }
    nan.or(best)
}

/// `max()` / `min()` of an integer vector: NA without `na_rm` if any; `None` if empty.
pub fn extreme_int(x: &[i32], want_max: bool, na_rm: bool) -> Option<i32> {
    if !na_rm && x.contains(&NA_INT) {
        return Some(NA_INT);
    }
    let it = x.iter().copied().filter(|&v| v != NA_INT);
    if want_max {
        it.max()
    } else {
        it.min()
    }
}

/// `any()` (`want_any`) or `all()` of a logical vector, with R's three-valued logic.
pub fn any_all(x: &[i32], want_any: bool, na_rm: bool) -> i32 {
    let decisive = want_any as i32; // TRUE decides any(), FALSE decides all()
    let mut saw_na = false;
    for &v in x {
        if v == NA_INT {
            saw_na = true;
        } else if v == decisive {
            return decisive;
        }
    }
    if saw_na && !na_rm {
        NA_INT
    } else {
        1 - decisive
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const NA: i32 = NA_INT;

    #[test]
    fn integer_sums() {
        assert_eq!(sum_int(&[], false), Some(0));
        assert_eq!(sum_int(&[1, 2, -4], false), Some(-1));
        assert_eq!(sum_int(&[1, NA, 2], false), Some(NA));
        assert_eq!(sum_int(&[1, NA, 2], true), Some(3));
        assert_eq!(sum_int(&[i32::MAX, 1], false), None);
        assert_eq!(sum_int(&[-i32::MAX, -1], false), None, "would be NA's bits");
        assert_eq!(sum_int(&[i32::MAX, 1, -1], false), Some(i32::MAX));
    }

    #[test]
    fn integer_cumsums() {
        assert_eq!(cumsum_int(&[1, 2, 3]), Some(vec![1, 3, 6]));
        assert_eq!(cumsum_int(&[1, NA, 3]), Some(vec![1, NA, NA]));
        assert_eq!(cumsum_int(&[i32::MAX, 1]), None);
        assert_eq!(cumsum_int(&[]), Some(vec![]));
    }

    #[test]
    fn double_extremes_follow_the_na_rules() {
        // R's NA_real_ is a NaN with low word 1954; any marker works for the test
        let na = f64::from_bits(0x7FF0_0000_0000_07A2);
        let is_na = |v: f64| v.to_bits() & 0xFFFF_FFFF == 1954;
        let nan = f64::NAN;
        assert_eq!(extreme_f64(&[1.0, 3.0, 2.0], true, false, is_na), Some(3.0));
        assert_eq!(
            extreme_f64(&[1.0, 3.0, 2.0], false, false, is_na),
            Some(1.0)
        );
        assert!(extreme_f64(&[1.0, nan, 2.0], true, false, is_na)
            .unwrap()
            .is_nan());
        let v = extreme_f64(&[nan, 1.0, na], true, false, is_na).unwrap();
        assert!(is_na(v), "NA wins over NaN wherever it is");
        assert_eq!(extreme_f64(&[nan, 1.0, na], true, true, is_na), Some(1.0));
        assert_eq!(extreme_f64(&[], true, false, is_na), None);
        assert_eq!(extreme_f64(&[na], true, true, is_na), None);
        assert_eq!(
            extreme_f64(&[f64::NEG_INFINITY], true, false, is_na),
            Some(f64::NEG_INFINITY)
        );
    }

    #[test]
    fn integer_extremes() {
        assert_eq!(extreme_int(&[3, 1, 2], true, false), Some(3));
        assert_eq!(extreme_int(&[3, NA, 2], false, false), Some(NA));
        assert_eq!(extreme_int(&[3, NA, 2], false, true), Some(2));
        assert_eq!(extreme_int(&[NA], true, true), None);
    }

    #[test]
    fn any_and_all() {
        assert_eq!(any_all(&[0, NA, 1], true, false), 1);
        assert_eq!(any_all(&[0, NA], true, false), NA);
        assert_eq!(any_all(&[0, NA], true, true), 0);
        assert_eq!(any_all(&[], true, false), 0);
        assert_eq!(any_all(&[1, NA, 0], false, false), 0);
        assert_eq!(any_all(&[1, NA], false, false), NA);
        assert_eq!(any_all(&[1, NA], false, true), 1);
        assert_eq!(any_all(&[], false, false), 1);
    }
}
