//! R entry points of tidyABM's Rust engine (savvy bridge).
//!
//! Phase 1: plumbing only. None of these functions is on a model's run path; they
//! prove the build, the calls and the error and panic transport. Every entry point
//! runs its body under `run()`, so a panic becomes an R error and never reaches R's
//! C frames, and no thread calls R while panicking.
//!
//! After editing this file run `savvy-cli update .` from the package root to
//! regenerate `R/000-wrappers.R`, `src/init.c` and `src/rust/api.h`.

use savvy::{savvy, OwnedRealSexp, OwnedStringSexp, RealSexp, Sexp};
use tidyabm_core::{guard, on_worker, ERROR_PREFIX, VERSION};

/// Run an entry point's body under tidyABM's panic guard; a panic becomes an error
/// whose message starts with `tidyabm_core::PANIC_PREFIX`.
fn run<T>(body: impl FnOnce() -> savvy::Result<T>) -> savvy::Result<T> {
    guard(body).unwrap_or_else(|msg| Err(savvy::Error::new(msg)))
}

/// Facts about the compiled Rust engine.
/// @noRd
#[savvy]
fn rs_info() -> savvy::Result<Sexp> {
    run(|| {
        let panic = if cfg!(panic = "unwind") {
            "unwind"
        } else {
            "abort"
        };
        let fields = [
            ("version", VERSION),
            ("target_arch", std::env::consts::ARCH),
            ("target_os", std::env::consts::OS),
            ("panic", panic),
        ];
        let mut out = OwnedStringSexp::try_from_slice(fields.map(|f| f.1))?;
        out.set_names(fields.map(|f| f.0))?;
        Ok(out.into())
    })
}

/// Do nothing (measures the cost of a call).
/// @noRd
#[savvy]
fn rs_noop() -> savvy::Result<()> {
    run(|| Ok(()))
}

/// Copy a double vector through Rust; the bits come back unchanged (NA and NaN
/// payloads included).
/// @noRd
#[savvy]
fn rs_copy_doubles(x: RealSexp) -> savvy::Result<Sexp> {
    run(|| OwnedRealSexp::try_from_slice(x.as_slice()).map(Into::into))
}

/// Return a typed Rust error (test helper for the error path).
/// @noRd
#[savvy]
fn rs_test_error(msg: &str) -> savvy::Result<()> {
    run(|| Err(savvy::Error::new(format!("{ERROR_PREFIX}{msg}"))))
}

/// Panic on the main thread or on a worker thread (test helper for the panic path).
/// @noRd
#[savvy]
fn rs_test_panic(msg: &str, worker: bool) -> savvy::Result<()> {
    run(|| {
        let msg = msg.to_string();
        if worker {
            on_worker::<()>(move || panic!("{msg}"));
            Ok(())
        } else {
            panic!("{msg}")
        }
    })
}
