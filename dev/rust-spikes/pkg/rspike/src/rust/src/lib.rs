//! Throwaway spike package for tidyABM's Rust migration, Phase 0 (S3, S4, S6, S7).
//! Nothing here is meant to survive into the package as is.

use savvy::{
    savvy, savvy_err, FunctionArgs, FunctionSexp, IntegerSexp, OwnedIntegerSexp, OwnedRealSexp,
    OwnedStringSexp, RealSexp, Sexp,
};

mod expr;
mod rapi;
mod tax;

use rapi::{check_interrupt, RRngGuard};

// ---------------------------------------------------------------- S6: tax loop

fn csr<'a>(off: &'a IntegerSexp, idx: &'a IntegerSexp) -> tax::Csr<'a> {
    tax::Csr {
        off: off.as_slice(),
        idx: idx.as_slice(),
    }
}

/// One run of the tax model in Rust, drawing from R's own generator (C API), so the
/// caller's `set.seed()` / `.Random.seed` drive it and are advanced by it.
#[savvy]
fn rs_tax_run(
    typ: IntegerSexp,
    income: RealSexp,
    risk: RealSexp,
    nb_off: IntegerSexp,
    nb_idx: IntegerSexp,
    rate: f64,
    ticks: i32,
) -> savvy::Result<Sexp> {
    let nb = csr(&nb_off, &nb_idx);
    let rng = RRngGuard::get();
    let out = tax::tax_run(
        typ.as_slice(),
        income.as_slice(),
        risk.as_slice(),
        &nb,
        rate,
        ticks as usize,
        || rng.unif(),
    );
    drop(rng);
    OwnedRealSexp::try_from_slice(out).map(Into::into)
}

/// The whole 150-run sweep with the pure-Rust RNG port, each run seeded like
/// `set.seed(seeds[r])`, runs spread over `threads` OS threads (no R API inside).
#[savvy]
#[allow(clippy::too_many_arguments)]
fn rs_tax_sweep(
    typ: IntegerSexp,
    income: RealSexp,
    risk: RealSexp,
    nb_off: IntegerSexp,
    nb_idx: IntegerSexp,
    rates: RealSexp,
    seeds: IntegerSexp,
    ticks: i32,
    threads: i32,
) -> savvy::Result<Sexp> {
    let (typ, w, k) = (typ.as_slice(), income.as_slice(), risk.as_slice());
    let nb = csr(&nb_off, &nb_idx);
    let (rates, seeds) = (rates.as_slice(), seeds.as_slice());
    let t = ticks as usize;
    let runs = rates.len();
    let mut out = vec![0.0; runs * t];
    let per = runs.div_ceil(threads.max(1) as usize);
    // Plain slices and f64s only cross into the threads; no SEXP, no R call.
    std::thread::scope(|s| {
        for (c, chunk) in out.chunks_mut(per * t).enumerate() {
            let nb = &nb;
            s.spawn(move || {
                for (j, o) in chunk.chunks_mut(t).enumerate() {
                    let r = c * per + j;
                    let mut g = rrng::RRng::set_seed(seeds[r]);
                    let v = tax::tax_run(typ, w, k, nb, rates[r], t, || g.unif_rand());
                    o.copy_from_slice(&v);
                }
            });
        }
    });
    OwnedRealSexp::try_from_slice(out).map(Into::into)
}

// ---------------------------------------------------------------- S3: boundary costs

/// Does nothing: the fixed cost of a `.Call` through savvy.
#[savvy]
fn rs_noop() -> savvy::Result<()> {
    Ok(())
}

/// A stand-in for the run state held in Rust behind an external pointer.
#[savvy]
struct Store {
    cols: Vec<Vec<f64>>,
    chr: Vec<u32>,
    pool: Vec<String>,
}

#[savvy]
impl Store {
    fn new(n: i32, k: i32) -> savvy::Result<Self> {
        let n = n as usize;
        let mut g = rrng::RRng::set_seed(1);
        let cols = (0..k)
            .map(|_| (0..n).map(|_| g.unif_rand()).collect())
            .collect();
        let pool = vec!["patches".into(), "sheep".into(), "wolves".into()];
        let chr = (0..n).map(|i| (i % 3) as u32).collect();
        Ok(Store { cols, chr, pool })
    }

    /// Copy column j (1-based) out to a fresh R double vector.
    fn col(&self, j: i32) -> savvy::Result<Sexp> {
        let c = self
            .cols
            .get(j as usize - 1)
            .ok_or(savvy_err!("no column {j}"))?;
        OwnedRealSexp::try_from_slice(c).map(Into::into)
    }

    /// Copy an R double vector into column j (1-based).
    fn set_col(&mut self, j: i32, x: RealSexp) -> savvy::Result<()> {
        let c = self
            .cols
            .get_mut(j as usize - 1)
            .ok_or(savvy_err!("no column {j}"))?;
        if c.len() != x.len() {
            return Err(savvy_err!("size {} != {}", x.len(), c.len()));
        }
        c.copy_from_slice(x.as_slice());
        Ok(())
    }

    /// Materialise the character column as a new R character vector.
    fn chr_col(&self) -> savvy::Result<Sexp> {
        let mut out = OwnedStringSexp::new(self.chr.len())?;
        for (i, &c) in self.chr.iter().enumerate() {
            out.set_elt(i, &self.pool[c as usize])?;
        }
        Ok(out.into())
    }

    /// A trivial kernel over column 1; `rng` brackets it with Get/PutRNGstate and
    /// draws one uniform per agent from R's generator.
    fn step(&mut self, rng: bool) -> savvy::Result<()> {
        let c = &mut self.cols[0];
        if rng {
            let g = RRngGuard::get();
            c.iter_mut().for_each(|v| *v = 0.5 * *v + g.unif());
        } else {
            c.iter_mut().for_each(|v| *v *= 0.5);
        }
        Ok(())
    }

    fn nrow(&self) -> savvy::Result<Sexp> {
        OwnedIntegerSexp::try_from_scalar(self.chr.len() as i32).map(Into::into)
    }
}

/// Call an R closure `times` times from Rust (no arguments), return the last value.
#[savvy]
fn rs_call_closure(f: FunctionSexp, times: i32) -> savvy::Result<Sexp> {
    let mut last = None;
    for _ in 0..times {
        last = Some(f.call(FunctionArgs::new())?);
    }
    match last {
        Some(v) => Ok(v.into()),
        None => Ok(savvy::NullSexp.into()),
    }
}

// ---------------------------------------------------------------- S4: errors

/// Panics. With panic = "unwind" savvy's wrapper catches it and raises an R error.
#[savvy]
fn rs_panic(msg: &str) -> savvy::Result<()> {
    panic!("{}", msg);
}

/// A typed Rust error, returned (not panicked): becomes an R error with this message.
#[savvy]
fn rs_error(msg: &str) -> savvy::Result<()> {
    Err(savvy_err!("{}", msg))
}

/// Call an R function with one argument from Rust; R errors inside it must reach R
/// unchanged (class, message, fields).
#[savvy]
fn rs_call_r(f: FunctionSexp, x: Sexp) -> savvy::Result<Sexp> {
    let mut a = FunctionArgs::new();
    a.add("x", x)?;
    Ok(f.call(a)?.into())
}

/// Spin for `seconds`, checking for Ctrl-C every ~1 ms; returns iterations done.
#[savvy]
fn rs_spin(seconds: f64) -> savvy::Result<Sexp> {
    let t0 = std::time::Instant::now();
    let mut it: i64 = 0;
    let mut acc = 0.0f64;
    while t0.elapsed().as_secs_f64() < seconds {
        for i in 0..10_000 {
            acc += (i as f64).sqrt();
        }
        it += 1;
        if it % 16 == 0 {
            check_interrupt()?;
        }
    }
    std::hint::black_box(acc);
    OwnedRealSexp::try_from_scalar(it as f64).map(Into::into)
}

// ---------------------------------------------------------------- S7: expressions

/// Compile an IR (nested R list from `ir.R`) into a Rust program.
#[savvy]
struct Program {
    node: expr::Node,
}

#[savvy]
impl Program {
    fn new(ir: Sexp) -> savvy::Result<Self> {
        Ok(Program {
            node: expr::compile(ir)?,
        })
    }

    /// Evaluate over named columns / globals (a named list), `n` rows. Draws use R's
    /// generator (Get/PutRNGstate around the whole evaluation).
    fn eval(&self, env: savvy::ListSexp, n: i32) -> savvy::Result<Sexp> {
        let env = expr::Env::from_list(&env, n as usize)?;
        let rng = RRngGuard::get();
        let v = expr::eval(&self.node, &env, &mut || rng.unif())?;
        drop(rng);
        expr::to_sexp(v, n as usize)
    }
}
