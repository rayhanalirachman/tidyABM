//! Safe wrappers over tidyABM's long-double C helper `src/reduce.c` (migration
//! phase 2): double sums, means and cumulative sums accumulated as R documents for
//! `sum()` and `mean()`. The C code is pure computation (no R API), so these may run
//! on any thread.

use crate::rapi;

/// `sizeof(long double)` of the C compiler that built the package.
pub fn ld_bytes() -> i32 {
    // SAFETY: pure function without arguments.
    unsafe { rapi::tidyabm_ld_bytes() }
}

/// `sum(x, na.rm = na_rm)` for a double vector.
pub fn sum(x: &[f64], na_rm: bool) -> f64 {
    // SAFETY: the pointer and length describe `x`, which the C code only reads.
    unsafe { rapi::tidyabm_ld_sum(x.as_ptr(), x.len() as isize, na_rm as i32) }
}

/// `mean(x)` for a double vector (NA removal, if wanted, is done by the caller as
/// `mean.default()` does). `NaN` for an empty vector.
pub fn mean(x: &[f64]) -> f64 {
    if x.is_empty() {
        return f64::NAN;
    }
    // SAFETY: as for `sum`.
    unsafe { rapi::tidyabm_ld_mean(x.as_ptr(), x.len() as isize) }
}

/// `mean(x)` for an integer vector without NA. `NaN` for an empty vector.
pub fn mean_int(x: &[i32]) -> f64 {
    if x.is_empty() {
        return f64::NAN;
    }
    // SAFETY: as for `sum`.
    unsafe { rapi::tidyabm_ld_mean_int(x.as_ptr(), x.len() as isize) }
}

/// `cumsum(x)` for a double vector.
pub fn cumsum(x: &[f64]) -> Vec<f64> {
    let mut out = vec![0.0; x.len()];
    // SAFETY: `x` is read and `out` written, both of length `x.len()`.
    unsafe { rapi::tidyabm_ld_cumsum(x.as_ptr(), out.as_mut_ptr(), x.len() as isize) };
    out
}

/// Per-group sums, group `k` summed like `sum(x[g == k])`; `g` holds codes in
/// `1..=ngroups`.
pub fn group_sum(x: &[f64], g: &[i32], ngroups: usize, na_rm: bool) -> Result<Vec<f64>, String> {
    if x.len() != g.len() {
        return Err(format!("`x` has length {} but `g` {}", x.len(), g.len()));
    }
    let k = i32::try_from(ngroups).map_err(|_| "too many groups".to_string())?;
    let mut out = vec![0.0; ngroups];
    // SAFETY: `x` and `g` are read for `x.len()` elements, `out` written for
    // `ngroups`; the C code checks every code against `ngroups` before using it.
    let rc = unsafe {
        rapi::tidyabm_ld_group_sum(
            x.as_ptr(),
            g.as_ptr(),
            x.len() as isize,
            out.as_mut_ptr(),
            k,
            na_rm as i32,
        )
    };
    match rc {
        0 => Ok(out),
        -1 => Err("out of memory".into()),
        _ => Err(format!("group codes must be in 1..{ngroups}")),
    }
}
