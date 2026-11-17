//! Hand-written declarations of the C functions the bridge calls (migration phase 2):
//! R's public API for its random number generator and math (`R_ext/Random.h`,
//! `Rmath.h`, as documented in "Writing R Extensions", 6.3 and 6.7) and tidyABM's own
//! long-double helper `src/reduce.c`. Nothing here is ported from R's sources
//! (decision D13): every random draw and every power is made by R's own code.
//!
//! Every function in the first block uses R's global state, so it may only be called
//! on the R main thread, and the RNG functions only between `GetRNGstate()` and
//! `PutRNGstate()` (`rng.rs`). Several of them can raise an R warning or error
//! (`GetRNGstate()` on a broken `.Random.seed`, `R_pow()` warns "probable complete
//! loss of accuracy in modulus" for huge exponents), and a warning becomes a longjmp
//! under `options(warn = 2)` or an exiting `tryCatch(warning = )`. So they are called
//! only inside [`protect`].

use savvy::ffi::SEXP;
use std::os::raw::c_int;

extern "C" {
    pub static R_NilValue: SEXP;

    // R_ext/Random.h
    pub fn GetRNGstate();
    pub fn PutRNGstate();
    pub fn unif_rand() -> f64;
    pub fn norm_rand() -> f64;
    pub fn exp_rand() -> f64;
    pub fn R_unif_index(dn: f64) -> f64;

    // Rmath.h (the header maps rnorm to Rf_rnorm etc.; these are the exported names)
    pub fn Rf_runif(a: f64, b: f64) -> f64;
    pub fn Rf_rnorm(mu: f64, sigma: f64) -> f64;
    pub fn Rf_rlnorm(meanlog: f64, sdlog: f64) -> f64;
    pub fn Rf_rexp(scale: f64) -> f64;
    pub fn Rf_rpois(mu: f64) -> f64;
    pub fn Rf_rbinom(size: f64, prob: f64) -> f64;
    pub fn R_pow(x: f64, y: f64) -> f64;
}

/// Run `f` inside `R_UnwindProtect` (through `savvy::unwind_protect`): an R error or
/// a warning turned into one does not unwind through Rust frames but comes back as
/// `savvy::Error::Aborted`, which R re-raises once the entry point returns.
///
/// `f` runs inside a C callback, so it must not panic (a panic there aborts the
/// process); being `Copy`, it can capture only plain data such as numbers and raw
/// pointers.
///
/// # Safety
///
/// Call it on the R main thread only (it enters R).
pub unsafe fn protect<F: FnOnce() + Copy>(f: F) -> savvy::Result<()> {
    // SAFETY: main thread (caller's contract); `unwind_protect` catches R's longjmp;
    // `R_NilValue` is a valid SEXP to return.
    unsafe {
        savvy::unwind_protect(move || {
            f();
            R_NilValue
        })
    }
    .map(|_| ())
}

// src/reduce.c (tidyABM's own C; pure computation, safe on any thread)
extern "C" {
    pub fn tidyabm_ld_bytes() -> c_int;
    pub fn tidyabm_ld_sum(x: *const f64, n: isize, narm: c_int) -> f64;
    pub fn tidyabm_ld_mean(x: *const f64, n: isize) -> f64;
    pub fn tidyabm_ld_mean_int(x: *const i32, n: isize) -> f64;
    pub fn tidyabm_ld_cumsum(x: *const f64, out: *mut f64, n: isize);
    pub fn tidyabm_ld_group_sum(
        x: *const f64,
        g: *const i32,
        n: isize,
        out: *mut f64,
        ngroups: c_int,
        narm: c_int,
    ) -> c_int;
}
