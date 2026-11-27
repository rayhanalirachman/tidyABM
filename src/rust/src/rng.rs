//! Access to R's random number generator from Rust (migration phase 2).
//!
//! Every draw is made by R's own generator through its public C API, so the stream
//! is R's by construction, for whatever `RNGkind()` is set; Rust only decides how
//! many draws to make and what to do with them (`tidyabm_core::sample`).
//!
//! Rules (PLAN.md, phase 2):
//! * R's RNG is touched only on the R main thread, between `GetRNGstate()` and
//!   `PutRNGstate()`. [`with_rng`] is the only way to get an [`RRng`]; the token is
//!   neither `Send` nor `Sync`, so it cannot reach a worker thread.
//! * `PutRNGstate()` runs on every exit path: normal return, a returned error and a
//!   panic, so `.Random.seed` always holds the state after the draws actually made
//!   and R code that runs next continues the same stream.
//! * Calls that may raise an R error or warning (`GetRNGstate()`, `PutRNGstate()`,
//!   the Rmath variate functions) run under [`rapi::protect`], so an R condition
//!   never unwinds through Rust frames.

use crate::rapi;
use std::marker::PhantomData;
use std::panic::{catch_unwind, resume_unwind, AssertUnwindSafe};
use tidyabm_core::sample::RDraws;

/// Proof that R's RNG state is loaded on the R main thread. Created only by
/// [`with_rng`]; the raw-pointer marker makes it `!Send` and `!Sync`.
pub struct RRng {
    _main_thread_only: PhantomData<*const ()>,
}

/// Run `body` with R's RNG state loaded; `.Random.seed` is written back whatever
/// way `body` ends (its panic is re-raised after `PutRNGstate()`).
pub fn with_rng<T>(body: impl FnOnce(&mut RRng) -> savvy::Result<T>) -> savvy::Result<T> {
    // SAFETY (both calls): R main thread, as every entry point; no panic inside.
    unsafe { rapi::protect(|| rapi::GetRNGstate()) }?;
    let mut rng = RRng {
        _main_thread_only: PhantomData,
    };
    let out = catch_unwind(AssertUnwindSafe(|| body(&mut rng)));
    // SAFETY: as above; runs whether `body` returned or panicked.
    unsafe { rapi::protect(|| rapi::PutRNGstate()) }?;
    out.unwrap_or_else(|panic| resume_unwind(panic))
}

type Rmath2 = unsafe extern "C" fn(f64, f64) -> f64;
type Rmath1 = unsafe extern "C" fn(f64) -> f64;

/// One kind of draw: R's `unif_rand`, `norm_rand`, `exp_rand`, `R_unif_index(dn)`,
/// or an Rmath variate function (`Rf_runif`, `Rf_rnorm`, ..., `Rf_rexp`, `Rf_rpois`)
/// with its parameters.
#[derive(Clone, Copy)]
pub enum Draw {
    Unif,
    Norm,
    Exp,
    Index(f64),
    Rmath2(Rmath2, f64, f64),
    Rmath1(Rmath1, f64),
}

impl RRng {
    /// Fill `out` with draws of one kind, in order.
    pub fn fill(&mut self, out: &mut [f64], d: Draw) -> savvy::Result<()> {
        let (p, n) = (out.as_mut_ptr(), out.len());
        // SAFETY: main thread, RNG state loaded (the token exists); the loop cannot
        // panic; `i < n` stays inside `out`, which is borrowed mutably for this call.
        unsafe {
            rapi::protect(move || {
                for i in 0..n {
                    *p.add(i) = match d {
                        Draw::Unif => rapi::unif_rand(),
                        Draw::Norm => rapi::norm_rand(),
                        Draw::Exp => rapi::exp_rand(),
                        Draw::Index(dn) => rapi::R_unif_index(dn),
                        Draw::Rmath2(f, a, b) => f(a, b),
                        Draw::Rmath1(f, a) => f(a),
                    }
                }
            })
        }
    }
}

/// For the sampling algorithms: `unif_rand` and `R_unif_index` raise no R condition
/// under the verified `RNGkind()`s (no user-supplied generator), so they are called
/// directly.
impl RDraws for RRng {
    fn unif(&mut self) -> f64 {
        // SAFETY: main thread, RNG state loaded (the token exists).
        unsafe { rapi::unif_rand() }
    }

    fn unif_index(&mut self, dn: f64) -> f64 {
        // SAFETY: as for `unif`; callers pass dn >= 1.
        unsafe { rapi::R_unif_index(dn) }
    }

    fn revsort(&mut self, a: &mut [f64], ib: &mut [i32]) {
        assert_eq!(a.len(), ib.len());
        let n = std::os::raw::c_int::try_from(a.len()).expect("revsort length fits c_int");
        // SAFETY: pure C sort over live slices of length `n` (never allocates or raises).
        unsafe { rapi::Rf_revsort(a.as_mut_ptr(), ib.as_mut_ptr(), n) };
    }

    fn sum_f64(&self, x: &[f64]) -> f64 {
        crate::reduce::sum(x, false)
    }
}
