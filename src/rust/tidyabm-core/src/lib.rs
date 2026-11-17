//! tidyABM's engine code that does not touch R. Phase 1 added the panic guard the R
//! entry points run under; phase 2 the sampling algorithms over R's generator
//! (`sample`) and the reductions that need no long double (`reduce`). The column
//! store and kernels arrive in later phases.

pub mod reduce;
pub mod sample;

use std::any::Any;
use std::panic::{self, AssertUnwindSafe};

/// Version of the Rust engine (the crate version).
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

/// Prefix of the error message a caught panic turns into. The R side matches it to
/// raise a `tidyABM_rust_panic` condition (Rust never formats user-facing messages).
pub const PANIC_PREFIX: &str = "tidyABM-rust-panic: ";

/// Prefix of a typed error returned by Rust code, raised in R as `tidyABM_rust_error`.
pub const ERROR_PREFIX: &str = "tidyABM-rust-error: ";

/// Run `f` and turn a panic into `Err(message)`.
///
/// While `f` runs, the process-wide panic hook is a silent one, so a panicking thread
/// (the main thread or a worker spawned inside `f`) never calls R; savvy's own hook
/// prints through R's console API from whichever thread panicked (spike S4). The
/// message travels in the panic payload instead: a worker's panic comes back through
/// `JoinHandle::join()` and is re-raised on the calling thread with `resume_unwind`.
/// The previous hook is restored before returning.
///
/// Call it only from the R main thread; the hook is global state.
pub fn guard<T>(f: impl FnOnce() -> T) -> Result<T, String> {
    let previous = panic::take_hook();
    panic::set_hook(Box::new(|_| {}));
    // AssertUnwindSafe: on a panic every value `f` touched is dropped or discarded and
    // only the message is kept, so no broken invariant can be observed afterwards.
    let result = panic::catch_unwind(AssertUnwindSafe(f));
    panic::set_hook(previous);
    result.map_err(|payload| format!("{PANIC_PREFIX}{}", panic_message(payload.as_ref())))
}

/// The message of a panic payload (`panic!` with a literal gives `&str`, with
/// formatting `String`; anything else has no message).
pub fn panic_message(payload: &(dyn Any + Send)) -> String {
    if let Some(s) = payload.downcast_ref::<&str>() {
        (*s).to_string()
    } else if let Some(s) = payload.downcast_ref::<String>() {
        s.clone()
    } else {
        "a panic without a message".to_string()
    }
}

/// Run `f` on a new thread and join it, re-raising a panic on the calling thread.
/// Used by the R-side tests of the worker-thread panic path.
pub fn on_worker<T: Send + 'static>(f: impl FnOnce() -> T + Send + 'static) -> T {
    match std::thread::spawn(f).join() {
        Ok(v) => v,
        Err(payload) => panic::resume_unwind(payload),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::Mutex;

    // The panic hook is process-wide and cargo runs tests on parallel threads.
    static HOOK_LOCK: Mutex<()> = Mutex::new(());
    static HOOK_CALLS: AtomicUsize = AtomicUsize::new(0);

    fn lock() -> std::sync::MutexGuard<'static, ()> {
        HOOK_LOCK.lock().unwrap_or_else(|e| e.into_inner())
    }

    #[test]
    fn guard_passes_values_through() {
        let _l = lock();
        assert_eq!(guard(|| 41 + 1), Ok(42));
    }

    #[test]
    fn guard_catches_main_thread_panics() {
        let _l = lock();
        assert_eq!(
            guard(|| -> i32 { panic!("boom") }),
            Err(format!("{PANIC_PREFIX}boom"))
        );
        let n = 3;
        assert_eq!(
            guard(|| -> i32 { panic!("boom {n}") }),
            Err(format!("{PANIC_PREFIX}boom 3"))
        );
        assert_eq!(
            guard(|| -> i32 { std::panic::panic_any(7_u8) }),
            Err(format!("{PANIC_PREFIX}a panic without a message"))
        );
    }

    #[test]
    fn guard_catches_worker_panics() {
        let _l = lock();
        assert_eq!(
            guard(|| on_worker(|| -> i32 { panic!("worker boom") })),
            Err(format!("{PANIC_PREFIX}worker boom"))
        );
        assert_eq!(guard(|| on_worker(|| 5)), Ok(5));
    }

    #[test]
    fn guard_silences_and_restores_the_hook() {
        let _l = lock();
        let before = panic::take_hook();
        panic::set_hook(Box::new(|_| {
            HOOK_CALLS.fetch_add(1, Ordering::SeqCst);
        }));
        let start = HOOK_CALLS.load(Ordering::SeqCst);
        let _ = guard(|| -> i32 { panic!("silent") });
        let _ = guard(|| on_worker(|| -> i32 { panic!("silent worker") }));
        assert_eq!(
            HOOK_CALLS.load(Ordering::SeqCst),
            start,
            "hook ran inside guard"
        );
        let _ = panic::catch_unwind(|| panic!("outside"));
        assert_eq!(
            HOOK_CALLS.load(Ordering::SeqCst),
            start + 1,
            "hook not restored"
        );
        panic::set_hook(before);
    }
}
