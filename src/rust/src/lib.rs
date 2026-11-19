//! R entry points of tidyABM's Rust engine (savvy bridge).
//!
//! None of these functions is on a model's run path yet. Phase 1 proved the build,
//! the calls and the error and panic transport; phase 2 adds the building blocks of
//! later kernels, reached from R only by tests: draws from R's own generator
//! (`rng.rs`), sampling built on them (`tidyabm_core::sample`), and reductions that
//! give R's bits (`reduce.rs`, `tidyabm_core::reduce`). Every entry point runs its
//! body under `run()`, so a panic becomes an R error and never reaches R's C frames,
//! and no thread calls R while panicking.
//!
//! After editing this file run `savvy-cli update .` from the package root to
//! regenerate `R/000-wrappers.R`, `src/init.c` and `src/rust/api.h`.

mod rapi;
mod reduce;
mod rng;
mod store;

use savvy::{
    savvy, IntegerSexp, NotAvailableValue, NullSexp, OwnedIntegerSexp, OwnedLogicalSexp,
    OwnedRealSexp, OwnedStringSexp, RealSexp, Sexp, TypedSexp,
};
use tidyabm_core::sample::{self, RDraws, NA_INT};
use tidyabm_core::{guard, on_worker, ERROR_PREFIX, VERSION};

/// Run an entry point's body under tidyABM's panic guard; a panic becomes an error
/// whose message starts with `tidyabm_core::PANIC_PREFIX`.
pub(crate) fn run<T>(body: impl FnOnce() -> savvy::Result<T>) -> savvy::Result<T> {
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
        let ld = reduce::ld_bytes().to_string();
        let fields = [
            ("version", VERSION),
            ("target_arch", std::env::consts::ARCH),
            ("target_os", std::env::consts::OS),
            ("panic", panic),
            ("long_double_bytes", ld.as_str()),
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

// ---- Phase 2: R's random number generator and R-order reductions ---------------

/// A typed error (prefix `ERROR_PREFIX`) raised in R as `tidyABM_rust_error`.
pub(crate) fn typed_error(msg: impl std::fmt::Display) -> savvy::Error {
    savvy::Error::new(format!("{ERROR_PREFIX}{msg}"))
}

fn count(n: i32, what: &str) -> savvy::Result<usize> {
    usize::try_from(n).map_err(|_| typed_error(format!("`{what}` must be >= 0, not {n}")))
}

/// R's `NULL`: the Rust path declines and the caller uses R's own function.
fn decline() -> savvy::Result<Sexp> {
    Ok(NullSexp.into())
}

/// Doubles to an R vector; to an integer vector instead when `as_int` and every
/// value fits (what `rpois()` and `rbinom()` return).
fn doubles_out(v: Vec<f64>, as_int: bool) -> savvy::Result<Sexp> {
    if as_int && v.iter().all(|&x| (0.0..=i32::MAX as f64).contains(&x)) {
        let iv: Vec<i32> = v.iter().map(|&x| x as i32).collect();
        return OwnedIntegerSexp::try_from_slice(iv).map(Into::into);
    }
    OwnedRealSexp::try_from_slice(v).map(Into::into)
}

fn logicals_out(v: &[i32]) -> savvy::Result<Sexp> {
    let mut out = OwnedLogicalSexp::new(v.len())?;
    for (i, &x) in v.iter().enumerate() {
        if x == NA_INT {
            out.set_na(i)?;
        } else {
            out.set_elt(i, x != 0)?;
        }
    }
    Ok(out.into())
}

/// `n` draws from R's generator, through R's C API.
///
/// `kind`: `"unif"`, `"norm"`, `"exp"` (`unif_rand`, `norm_rand`, `exp_rand`: what
/// `runif(n)`, `rnorm(n)`, `rexp(n)` draw); `"index"` (`R_unif_index(a)`, values in
/// `0..a`); `"runif"` (min `a`, max `b`), `"rnorm"`, `"rlnorm"` (mean / sd `a`, `b`),
/// `"rexp"` (rate `a`), `"rpois"` (lambda `a`), `"rbinom"` (size `a`, prob `b`), each
/// R's own Rmath function. Returns `NULL` (decline) for parameters outside the
/// finite, valid range, where R would warn or return `NaN`: the caller then uses R.
/// @noRd
#[savvy]
fn rs_draw(kind: &str, n: i32, a: f64, b: f64) -> savvy::Result<Sexp> {
    run(|| {
        use rng::Draw;
        let n = count(n, "n")?;
        let fin = a.is_finite() && b.is_finite();
        let (d, ok, as_int) = match kind {
            "unif" => (Draw::Unif, true, false),
            "norm" => (Draw::Norm, true, false),
            "exp" => (Draw::Exp, true, false),
            "index" => (
                Draw::Index(a),
                fin && a >= 1.0 && a <= i32::MAX as f64,
                false,
            ),
            "runif" => (Draw::Rmath2(rapi::Rf_runif, a, b), fin && a <= b, false),
            "rnorm" => (Draw::Rmath2(rapi::Rf_rnorm, a, b), fin && b >= 0.0, false),
            "rlnorm" => (Draw::Rmath2(rapi::Rf_rlnorm, a, b), fin && b >= 0.0, false),
            "rexp" => (
                Draw::Rmath1(rapi::Rf_rexp, 1.0 / a),
                fin && (1.0 / a).is_finite() && a > 0.0,
                false,
            ),
            "rpois" => (Draw::Rmath1(rapi::Rf_rpois, a), fin && a >= 0.0, true),
            "rbinom" => {
                let size_ok = a >= 0.0 && a <= i32::MAX as f64 && a.fract() == 0.0;
                let ok = fin && size_ok && (0.0..=1.0).contains(&b);
                (Draw::Rmath2(rapi::Rf_rbinom, a, b), ok, true)
            }
            _ => return Err(typed_error(format!("unknown draw kind \"{kind}\""))),
        };
        if !ok {
            return decline();
        }
        let mut v = vec![0.0; n];
        rng::with_rng(|r| r.fill(&mut v, d))?;
        doubles_out(v, as_int)
    })
}

/// `sample.int(n, size, replace)` from R's generator (`R_unif_index`).
/// @noRd
#[savvy]
fn rs_sample_int(n: i32, size: i32, replace: bool) -> savvy::Result<Sexp> {
    run(|| {
        let (n, size) = (count(n, "n")?, count(size, "size")?);
        // validate before touching the RNG state: an error must not draw
        sample::check_sample(n, size, replace).map_err(typed_error)?;
        let v = rng::with_rng(|r| sample::sample_int(r, n, size, replace).map_err(typed_error))?;
        OwnedIntegerSexp::try_from_slice(v).map(Into::into)
    })
}

/// `runif(n) < p` from R's generator; `p` has length 1 or `n`.
/// @noRd
#[savvy]
fn rs_bernoulli(n: i32, p: RealSexp) -> savvy::Result<Sexp> {
    run(|| {
        let n = count(n, "n")?;
        let p = p.as_slice();
        if p.len() != 1 && p.len() != n {
            return Err(typed_error(format!(
                "`p` must have length 1 or {n}, not {}",
                p.len()
            )));
        }
        let v = rng::with_rng(|r| sample::bernoulli(r, n, p).map_err(typed_error))?;
        logicals_out(&v)
    })
}

/// Draw `n` uniforms from R's generator, then fail with a typed error (`how =
/// "error"`) or a panic (`"panic"`): tests that `.Random.seed` is written back.
/// @noRd
#[savvy]
fn rs_test_rng_fail(n: i32, how: &str) -> savvy::Result<()> {
    run(|| {
        let n = count(n, "n")?;
        rng::with_rng(|r| {
            for _ in 0..n {
                r.unif();
            }
            match how {
                "panic" => panic!("failed after {n} draws"),
                _ => Err(typed_error(format!("failed after {n} draws"))),
            }
        })
    })
}

/// A reduction with R's order, accumulator and NA rules: `op` is one of `"sum"`,
/// `"mean"`, `"cumsum"`, `"max"`, `"min"`, `"any"`, `"all"`, `x` a double, integer
/// or logical vector without attributes. Returns `NULL` (decline) where R would
/// warn or the type is not covered (integer overflow, extremes of an empty
/// vector, `any()` of a non-logical vector, ...): the caller then uses R.
/// @noRd
#[savvy]
fn rs_reduce(op: &str, x: Sexp, na_rm: bool) -> savvy::Result<Sexp> {
    run(|| {
        use tidyabm_core::reduce as cr;
        let int_out = |v: Option<i32>| match v {
            Some(v) => OwnedIntegerSexp::try_from_scalar(v).map(Into::into),
            None => decline(),
        };
        let real_out = |v: f64| OwnedRealSexp::try_from_scalar(v).map(Into::into);
        match (op, x.into_typed()) {
            (_, TypedSexp::Real(x)) => {
                let x = x.as_slice();
                match op {
                    "sum" => real_out(reduce::sum(x, na_rm)),
                    "mean" if na_rm => {
                        let kept: Vec<f64> = x.iter().copied().filter(|v| !v.is_nan()).collect();
                        real_out(reduce::mean(&kept))
                    }
                    "mean" => real_out(reduce::mean(x)),
                    "cumsum" => {
                        // from the first NA on, R's result is its NA_real_ itself
                        // (found by differential tests: whatever the hardware makes of
                        // NaN + NA, and in NA_real_'s bits, not a quieted NaN)
                        let mut v = reduce::cumsum(x);
                        if let Some(i) = x.iter().position(|s| s.is_na()) {
                            v[i..].fill(f64::na());
                        }
                        OwnedRealSexp::try_from_slice(v).map(Into::into)
                    }
                    "max" | "min" => match cr::extreme_f64(x, op == "max", na_rm, |v| v.is_na()) {
                        Some(v) => real_out(v),
                        None => decline(),
                    },
                    _ => decline(),
                }
            }
            (_, x @ (TypedSexp::Integer(_) | TypedSexp::Logical(_))) => {
                let logical = matches!(x, TypedSexp::Logical(_));
                let x: &[i32] = match &x {
                    TypedSexp::Integer(v) => v.as_slice(),
                    TypedSexp::Logical(v) => v.as_slice_raw(),
                    _ => unreachable!(),
                };
                match op {
                    "sum" => int_out(cr::sum_int(x, na_rm)),
                    "mean" => {
                        let has_na = x.contains(&NA_INT);
                        if has_na && !na_rm {
                            return real_out(f64::na());
                        }
                        let kept: Vec<i32> = x.iter().copied().filter(|&v| v != NA_INT).collect();
                        real_out(reduce::mean_int(&kept))
                    }
                    "cumsum" => match cr::cumsum_int(x) {
                        Some(v) => OwnedIntegerSexp::try_from_slice(v).map(Into::into),
                        None => decline(),
                    },
                    "max" | "min" => int_out(cr::extreme_int(x, op == "max", na_rm)),
                    "any" | "all" if logical => logicals_out(&[cr::any_all(x, op == "any", na_rm)]),
                    _ => decline(),
                }
            }
            _ => decline(),
        }
    })
}

/// Per-group sums, group `k` summed like `sum(x[g == k], na.rm = na_rm)`.
/// @noRd
#[savvy]
fn rs_group_sum(x: RealSexp, g: IntegerSexp, ngroups: i32, na_rm: bool) -> savvy::Result<Sexp> {
    run(|| {
        let k = count(ngroups, "ngroups")?;
        let v = reduce::group_sum(x.as_slice(), g.as_slice(), k, na_rm).map_err(typed_error)?;
        OwnedRealSexp::try_from_slice(v).map(Into::into)
    })
}

/// R's exported `R_pow(x, y)` element by element (what R's `^` computes, also for
/// integer exponents; `R_pow_di()` is not, so it is not used); `y` has length 1 or
/// `length(x)`. R's own warnings (huge exponents) are raised as R raises them.
/// @noRd
#[savvy]
fn rs_pow(x: RealSexp, y: RealSexp) -> savvy::Result<Sexp> {
    run(|| {
        let (x, y) = (x.as_slice(), y.as_slice());
        if y.len() != 1 && y.len() != x.len() {
            return Err(typed_error("`y` must have length 1 or length(x)"));
        }
        let mut out = vec![0.0; x.len()];
        let (xp, yp, op, n, ny) = (x.as_ptr(), y.as_ptr(), out.as_mut_ptr(), x.len(), y.len());
        // SAFETY: main thread; R_pow may raise R's "loss of accuracy" warning, hence
        // `protect`; the loop cannot panic and reads/writes within the three slices.
        unsafe {
            rapi::protect(move || {
                for i in 0..n {
                    *op.add(i) = rapi::R_pow(*xp.add(i), *yp.add(if ny == 1 { 0 } else { i }));
                }
            })
        }?;
        OwnedRealSexp::try_from_slice(out).map(Into::into)
    })
}

/// `abm_neighbours()`'s network fast path: `sum()` (`mean = FALSE`) or `mean()` of
/// `x` over each agent's neighbours through the R engine's padded index (`cols`,
/// `cnt` from `nb_index()`), in `nb_fold()`'s order; `NULL` where `nb_fold()` must
/// answer (an `NA`/`NaN` sum, a malformed index).
/// @noRd
#[savvy]
fn rs_nb_fold(mean: bool, x: Sexp, cols: savvy::ListSexp, cnt: Sexp) -> savvy::Result<Sexp> {
    run(|| {
        let (Some(x), Some(cnt)) = (
            store::numbers_of::<f64>(x.0, rapi::REALSXP)?,
            store::numbers_of::<i32>(cnt.0, rapi::INTSXP)?,
        ) else {
            return decline();
        };
        let mut slices: Vec<&[i32]> = Vec::with_capacity(cols.len());
        for (_, c) in cols.iter() {
            match store::numbers_of::<i32>(c.0, rapi::INTSXP)? {
                Some(v) => slices.push(v),
                None => return decline(),
            }
        }
        // SAFETY: reading a static of R's (R's NA_real_, not a Rust constant)
        let na = unsafe { rapi::R_NaReal };
        match tidyabm_core::neighbours::nb_fold(mean, x, &slices, cnt, na) {
            Some(v) => OwnedRealSexp::try_from_slice(v).map(Into::into),
            None => decline(),
        }
    })
}
