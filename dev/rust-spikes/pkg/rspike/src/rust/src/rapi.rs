//! The few R C API entry points savvy does not wrap. Main thread only.
//!
//! SAFETY (for every function here): R's API is single-threaded. These are called
//! only from `#[savvy]` entry points, i.e. on R's main thread inside a `.Call`.
//! `GetRNGstate`/`unif_rand`/`PutRNGstate` cannot longjmp for the default RNG kind
//! (GetRNGstate can error on a corrupt `.Random.seed`; we accept that in a spike and
//! note it). `R_CheckUserInterrupt` CAN longjmp, so it is only called through
//! `savvy::unwind_protect`, which turns the jump into an `Err` that savvy's C
//! wrapper re-raises with `R_ContinueUnwind` after Rust frames are dropped.

use std::marker::PhantomData;

extern "C" {
    fn GetRNGstate();
    fn PutRNGstate();
    fn unif_rand() -> f64;
    fn R_CheckUserInterrupt();
}

/// A token proving `GetRNGstate()` was called; `PutRNGstate()` runs on drop (also on
/// `?` early returns). `!Send`: it cannot cross to a worker thread.
pub struct RRngGuard {
    _not_send: PhantomData<*const ()>,
}

impl RRngGuard {
    pub fn get() -> Self {
        // SAFETY: see module docs (main thread, inside .Call).
        unsafe { GetRNGstate() };
        RRngGuard {
            _not_send: PhantomData,
        }
    }
    #[inline]
    pub fn unif(&self) -> f64 {
        // SAFETY: state was loaded by GetRNGstate in `get()`.
        unsafe { unif_rand() }
    }
}

impl Drop for RRngGuard {
    fn drop(&mut self) {
        // SAFETY: paired with the GetRNGstate in `get()`.
        unsafe { PutRNGstate() };
    }
}

/// Process a pending Ctrl-C. Returns Err(Aborted) when R wants to unwind.
pub fn check_interrupt() -> savvy::Result<()> {
    // SAFETY: the possible longjmp is caught by R_UnwindProtect inside unwind_protect.
    unsafe {
        savvy::unwind_protect(|| {
            R_CheckUserInterrupt();
            savvy_ffi::R_NilValue
        })?;
    }
    Ok(())
}
