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

/// [`protect`] for a closure that returns a plain value (a number, a raw pointer, a
/// `SEXP`); `init` is returned only if R raised, and is then discarded.
///
/// # Safety
///
/// As [`protect`].
pub unsafe fn protect_value<T: Copy, F: FnOnce() -> T + Copy>(init: T, f: F) -> savvy::Result<T> {
    let mut out = init;
    let p: *mut T = &mut out;
    // SAFETY: caller's contract; `p` points at `out`, alive for the whole call.
    unsafe { protect(move || *p = f()) }?;
    Ok(out)
}

// The column store (`store.rs`, migration phase 3): R's public API for vectors,
// attributes and identity ("Writing R Extensions", 5.9 and 6.21). Main thread only.
// `TYPEOF`, `Rf_xlength`, `SET_STRING_ELT` and `VECTOR_ELT` neither allocate nor
// raise; the rest may
// allocate (ALTREP data pointers materialise, attribute copies allocate) and so may
// raise an R error, and are called only inside `protect`.
pub type SexpType = std::os::raw::c_uint;
pub const LGLSXP: SexpType = 10;
pub const INTSXP: SexpType = 13;
pub const REALSXP: SexpType = 14;
pub const STRSXP: SexpType = 16;
pub const VECSXP: SexpType = 19;

extern "C" {
    pub static R_NaString: SEXP;
    pub static R_NaReal: f64;
    pub static R_NamesSymbol: SEXP;

    pub fn TYPEOF(x: SEXP) -> SexpType;
    pub fn Rf_xlength(x: SEXP) -> isize;
    pub fn Rf_allocVector(t: SexpType, n: isize) -> SEXP;
    pub fn Rf_protect(x: SEXP) -> SEXP;
    pub fn Rf_unprotect(n: c_int);
    pub fn REAL_RO(x: SEXP) -> *const f64;
    pub fn INTEGER_RO(x: SEXP) -> *const c_int;
    // never allocate or raise: NULL for an ALTREP vector without data in memory
    pub fn REAL_OR_NULL(x: SEXP) -> *const f64;
    pub fn INTEGER_OR_NULL(x: SEXP) -> *const c_int;
    pub fn DATAPTR_OR_NULL(x: SEXP) -> *const std::ffi::c_void;
    pub fn LOGICAL(x: SEXP) -> *mut c_int;
    pub fn STRING_PTR_RO(x: SEXP) -> *const SEXP;
    pub fn SET_STRING_ELT(x: SEXP, i: isize, v: SEXP);
    pub fn VECTOR_ELT(x: SEXP, i: isize) -> SEXP;
    pub fn Rf_getAttrib(x: SEXP, name: SEXP) -> SEXP;
    pub fn Rf_setAttrib(x: SEXP, name: SEXP, value: SEXP) -> SEXP;
    pub fn SHALLOW_DUPLICATE_ATTRIB(to: SEXP, from: SEXP);
    pub fn R_compute_identical(x: SEXP, y: SEXP, flags: c_int) -> c_int;

    // the expression evaluator (phase 5): R's own `exp`/`log`/`sqrt` called back
    pub static R_BaseEnv: SEXP;
    pub fn Rf_install(name: *const std::os::raw::c_char) -> SEXP;
    pub fn Rf_lang2(f: SEXP, x: SEXP) -> SEXP;
    pub fn Rf_eval(e: SEXP, env: SEXP) -> SEXP;
    pub fn R_CHAR(x: SEXP) -> *const std::os::raw::c_char;
    pub fn LOGICAL_RO(x: SEXP) -> *const c_int;
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
