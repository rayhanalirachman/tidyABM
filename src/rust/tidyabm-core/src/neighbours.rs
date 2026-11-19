//! `abm_neighbours()` kernels (migration phase 4): the network fast path's folds.
//!
//! [`nb_fold`] is the R engine's `nb_fold()` (R/network-steps.R) over the same
//! padded neighbour index: column `j` of the index holds each agent's `j`-th
//! neighbour as a 1-based row, `n + 1` where the agent has fewer. The R function
//! adds `c(x, 0)[col]` to a running sum that starts at `0`, one column at a time,
//! in plain double arithmetic (R's vectorised `+`, no long double); this does the
//! same additions in the same order, so the bits agree on every platform. A row
//! whose sum is `NA`/`NaN` makes it decline: which of the two a sum that met both
//! comes out as is down to `rowsum()`, which the R engine calls for those rows.

/// `sum()` (`mean = false`) or `mean()` of `x` over each agent's neighbours, or
/// `None` where the R engine's `nb_fold()` has to answer (some agent with
/// neighbours sums to `NA`/`NaN`, or the index is malformed). `cols` are the
/// index columns, `cnt` the neighbour counts, `na` R's `NA_real_` (read from R at
/// run time, never a Rust constant).
pub fn nb_fold(mean: bool, x: &[f64], cols: &[&[i32]], cnt: &[i32], na: f64) -> Option<Vec<f64>> {
    let n = x.len();
    if cnt.len() != n || cols.iter().any(|c| c.len() != n) {
        return None;
    }
    // c(x, 0)[k], with `k` 1-based and `n + 1` the zero pad
    let at = |k: i32| -> Option<f64> {
        let k = usize::try_from(k).ok()?;
        match k {
            0 => None,
            _ if k <= n => Some(x[k - 1]),
            _ if k == n + 1 => Some(0.0),
            _ => None,
        }
    };
    let mut s = vec![0.0f64; n];
    for col in cols {
        for (si, &k) in s.iter_mut().zip(col.iter()) {
            *si += at(k)?;
        }
    }
    if s.iter().zip(cnt).any(|(v, &c)| v.is_nan() && c != 0) {
        return None;
    }
    if !mean {
        for (v, &c) in s.iter_mut().zip(cnt) {
            if c == 0 {
                *v = na;
            }
        }
        return Some(s);
    }
    let mut out: Vec<f64> = s.iter().zip(cnt).map(|(v, &c)| v / f64::from(c)).collect();
    // base's second pass: the mean of the residuals, where the first was finite;
    // a pad's residual is set to 0 after the subtraction, as the R code does
    let mut r = vec![0.0f64; n];
    for col in cols {
        for i in 0..n {
            let k = col[i];
            let d = if k as usize == n + 1 {
                0.0
            } else {
                at(k)? - out[i]
            };
            r[i] += d;
        }
    }
    for i in 0..n {
        if out[i].is_finite() {
            out[i] += r[i] / f64::from(cnt[i]);
        }
        if cnt[i] == 0 {
            out[i] = na;
        }
    }
    Some(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn na() -> f64 {
        f64::from_bits(0x7ff0_0000_0000_07a2)
    }

    #[test]
    fn sums_and_means_in_index_order() {
        // agents 1..3; 1 has neighbours 2, 3; 2 has 1; 3 has none (pad = 4)
        let x = [1.0, 2.0, 4.0];
        let c1 = [2, 1, 4];
        let c2 = [3, 4, 4];
        let cols: [&[i32]; 2] = [&c1, &c2];
        let cnt = [2, 1, 0];
        let s = nb_fold(false, &x, &cols, &cnt, na()).unwrap();
        assert_eq!(s[0], 6.0);
        assert_eq!(s[1], 1.0);
        assert_eq!(s[2].to_bits(), na().to_bits());
        let m = nb_fold(true, &x, &cols, &cnt, na()).unwrap();
        assert_eq!(m[0], 3.0);
        assert_eq!(m[1], 1.0);
        assert_eq!(m[2].to_bits(), na().to_bits());
    }

    #[test]
    fn declines_nan_sums_and_bad_indices() {
        let x = [f64::NAN, 2.0];
        let c1 = [2, 1];
        let cols: [&[i32]; 1] = [&c1];
        assert!(nb_fold(false, &x, &cols, &[1, 1], na()).is_none());
        let bad = [5, 1];
        let cols: [&[i32]; 1] = [&bad];
        assert!(nb_fold(false, &[1.0, 2.0], &cols, &[1, 1], na()).is_none());
    }
}
