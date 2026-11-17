//! Sampling algorithms on top of R's own generator (migration phase 2).
//!
//! The draws themselves come from R through its public C API (`unif_rand`,
//! `R_unif_index`), reached by the bridge crate on the R main thread; this module only
//! decides which draws to make and how to turn them into indices, so it is testable
//! without R. The algorithms are textbook ones whose results were checked against
//! R's `sample.int()` by differential tests (`tests/testthat/test-rust-rng.R`); no code
//! comes from R's sources (decision D13).

use std::collections::HashSet;

/// R's integer and logical NA (`NA_integer_`, `NA`): the most negative `i32`.
pub const NA_INT: i32 = i32::MIN;

/// A source of R's uniform draws. The bridge crate implements it with R's generator;
/// tests use scripted sources.
pub trait RDraws {
    /// One uniform draw in (0, 1), R's `unif_rand()`.
    fn unif(&mut self) -> f64;
    /// One index draw in `0..dn` (as a double), R's `R_unif_index(dn)`.
    fn unif_index(&mut self, dn: f64) -> f64;
}

/// `sample.int(n, size, replace)`: `size` 1-based indices into `1..=n`.
///
/// * with replacement: one index draw over `n` per value;
/// * without replacement, for `n > 1e7` and `size <= n / 2` (the condition under
///   which `sample.int()` documents its hash version, `useHash`): draw over `n` and
///   draw again whenever the value was already taken;
/// * otherwise: draw over the values still left, take that value and move the last
///   remaining value into its slot (a partial Fisher-Yates selection).
///
/// Errors (before any draw) when `size > n` without replacement, or when values are
/// asked from an empty population.
pub fn sample_int(
    src: &mut impl RDraws,
    n: usize,
    size: usize,
    replace: bool,
) -> Result<Vec<i32>, String> {
    check_sample(n, size, replace)?;
    let dn = n as f64;
    if replace {
        return Ok((0..size).map(|_| src.unif_index(dn) as i32 + 1).collect());
    }
    if dn > 1e7 && size as f64 <= dn / 2.0 {
        let mut seen = HashSet::with_capacity(size);
        let mut out = Vec::with_capacity(size);
        while out.len() < size {
            let v = src.unif_index(dn) as i32 + 1;
            if seen.insert(v) {
                out.push(v);
            }
        }
        return Ok(out);
    }
    let mut left: Vec<i32> = (1..=n as i32).collect();
    let mut out = Vec::with_capacity(size);
    for _ in 0..size {
        let j = src.unif_index(left.len() as f64) as usize;
        out.push(left.swap_remove(j));
    }
    Ok(out)
}

/// The argument checks of [`sample_int`], callable before any RNG state is loaded.
pub fn check_sample(n: usize, size: usize, replace: bool) -> Result<(), String> {
    if n > i32::MAX as usize {
        return Err("`n` is larger than the largest integer".into());
    }
    if size > 0 && n == 0 {
        return Err("cannot sample from an empty population".into());
    }
    if !replace && size > n {
        return Err("cannot take a sample larger than the population without replacement".into());
    }
    Ok(())
}

/// `runif(n) < p` as an R logical vector: `p` has length 1 or `n`; a `NaN` (or NA)
/// probability gives NA. One uniform draw per value, NA or not, as R draws all `n`
/// uniforms before comparing.
pub fn bernoulli(src: &mut impl RDraws, n: usize, p: &[f64]) -> Result<Vec<i32>, String> {
    if p.len() != 1 && p.len() != n {
        return Err(format!("`p` must have length 1 or {n}, not {}", p.len()));
    }
    Ok((0..n)
        .map(|i| {
            let u = src.unif();
            let pi = p[if p.len() == 1 { 0 } else { i }];
            if pi.is_nan() {
                NA_INT
            } else {
                (u < pi) as i32
            }
        })
        .collect())
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Plays back index draws as fractions of `dn` and uniforms from a list.
    struct Scripted {
        fracs: Vec<f64>,
        at: usize,
        calls: Vec<f64>,
    }

    impl Scripted {
        fn new(fracs: &[f64]) -> Self {
            Scripted {
                fracs: fracs.to_vec(),
                at: 0,
                calls: vec![],
            }
        }
        fn next(&mut self) -> f64 {
            let f = self.fracs[self.at % self.fracs.len()];
            self.at += 1;
            f
        }
    }

    impl RDraws for Scripted {
        fn unif(&mut self) -> f64 {
            self.next()
        }
        fn unif_index(&mut self, dn: f64) -> f64 {
            self.calls.push(dn);
            (self.next() * dn).floor()
        }
    }

    #[test]
    fn with_replacement_draws_over_n_each_time() {
        let mut s = Scripted::new(&[0.0, 0.999, 0.5]);
        assert_eq!(sample_int(&mut s, 4, 5, true).unwrap(), vec![1, 4, 3, 1, 4]);
        assert_eq!(s.calls, vec![4.0; 5]);
    }

    #[test]
    fn without_replacement_moves_the_last_value_into_the_gap() {
        // always the first slot: 1, then the value moved there from the end, ...
        let mut s = Scripted::new(&[0.0]);
        assert_eq!(
            sample_int(&mut s, 5, 5, false).unwrap(),
            vec![1, 5, 4, 3, 2]
        );
        assert_eq!(s.calls, vec![5.0, 4.0, 3.0, 2.0, 1.0]);
        // always the last slot: n, n-1, ...
        let mut s = Scripted::new(&[0.9999]);
        assert_eq!(sample_int(&mut s, 4, 3, false).unwrap(), vec![4, 3, 2]);
    }

    #[test]
    fn without_replacement_is_a_permutation() {
        let mut s = Scripted::new(&[0.3, 0.7, 0.1, 0.9, 0.5]);
        let mut v = sample_int(&mut s, 50, 50, false).unwrap();
        v.sort_unstable();
        assert_eq!(v, (1..=50).collect::<Vec<_>>());
    }

    #[test]
    fn large_n_redraws_duplicates_over_the_whole_range() {
        // the same fraction twice gives the same value, which is drawn again
        let mut s = Scripted::new(&[0.25, 0.25, 0.5]);
        let n = i32::MAX as usize;
        let v = sample_int(&mut s, n, 2, false).unwrap();
        assert_eq!(v.len(), 2);
        assert_ne!(v[0], v[1]);
        assert_eq!(s.calls, vec![n as f64; 3]);
        // at size > n / 2 the selection algorithm is used even for large n: not
        // tested here (it would allocate n values)
    }

    #[test]
    fn errors_come_before_any_draw() {
        let mut s = Scripted::new(&[0.5]);
        assert!(sample_int(&mut s, 3, 4, false).is_err());
        assert!(sample_int(&mut s, 0, 1, true).is_err());
        assert!(s.calls.is_empty());
        assert_eq!(sample_int(&mut s, 0, 0, false).unwrap(), Vec::<i32>::new());
        assert_eq!(sample_int(&mut s, 3, 0, true).unwrap(), Vec::<i32>::new());
    }

    #[test]
    fn bernoulli_compares_one_uniform_per_value() {
        let mut s = Scripted::new(&[0.1, 0.6, 0.4, 0.2]);
        assert_eq!(bernoulli(&mut s, 4, &[0.5]).unwrap(), vec![1, 0, 1, 1]);
        let mut s = Scripted::new(&[0.1, 0.6, 0.4]);
        assert_eq!(
            bernoulli(&mut s, 3, &[0.0, f64::NAN, 1.0]).unwrap(),
            vec![0, NA_INT, 1]
        );
        assert_eq!(s.at, 3, "a NaN probability still consumes its draw");
        assert!(bernoulli(&mut s, 3, &[0.1, 0.2]).is_err());
    }
}
