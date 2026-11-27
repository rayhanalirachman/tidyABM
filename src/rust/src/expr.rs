//! The R side of the expression evaluator (migration phase 5): `RxProgram`, a set
//! of rules compiled by `R/rust-expr.R` and decoded once per run into
//! `tidyabm_core::expr` trees, evaluated over R's own column vectors (read in
//! place, never copied in) with results handed back as new R vectors.
//!
//! R is reached only through its public API: draws through `rng::with_rng` (R's
//! generator, main thread), `exp`/`log`/`sqrt` by calling R's own functions back
//! (so their bits are R's on every platform, D13), sums and means through the
//! long-double helper. Anything the evaluator cannot do exactly makes `eval`
//! return `NULL` and the R side runs the step on the R path.

use crate::rapi::{self, LGLSXP, REALSXP, STRSXP, VECSXP};
use crate::rng::{self, Draw, RRng};
use crate::store::{self, H};
use crate::{reduce, run, typed_error};
use savvy::ffi::SEXP;
use savvy::{savvy, ListSexp, NullSexp, OwnedListSexp, Sexp};
use std::borrow::Cow;
use std::cell::Cell;
use tidyabm_core::expr::{self as ce, Bin, Decline, Host, Konst, MathFn, Node, Red, Un, Vals};

thread_local! {
    /// Why the last `eval()` declined, for coverage reports (main thread only).
    static LAST_DECLINE: Cell<&'static str> = const { Cell::new("") };
}

/// Compiled rules: one tree per rule, and for each rule the input it overwrites
/// once evaluated (a global the later rules of an `abm_global()` step read), or
/// none.
#[savvy]
pub struct RxProgram {
    rules: Vec<Node>,
    writes: Vec<Option<usize>>,
    draws: bool,
    /// the R functions `Node::Call` names (kept alive by `_fns`)
    fns: Vec<SEXP>,
    // the IR list: keeps the literal strings' CHARSXPs alive
    _ir: H,
    _fns: H,
}

fn bad(what: &str) -> savvy::Error {
    typed_error(format!("malformed expression IR: {what}"))
}

fn decode(x: SEXP) -> savvy::Result<Node> {
    let l = ListSexp::try_from(Sexp(x)).map_err(|_| bad("node is not a list"))?;
    let op = match l.get_by_index(0).map(|s| s.into_typed()) {
        Some(savvy::TypedSexp::String(s)) => s.iter().next().unwrap_or("").to_string(),
        _ => return Err(bad("node without an operator")),
    };
    let arg = |i: usize| -> savvy::Result<Node> {
        decode(l.get_by_index(i).ok_or_else(|| bad("missing argument"))?.0)
    };
    let boxed = |i: usize| arg(i).map(Box::new);
    let bin = |b: Bin| -> savvy::Result<Node> { Ok(Node::Bin(b, boxed(1)?, boxed(2)?)) };
    Ok(match op.as_str() {
        "const" => {
            let v = l
                .get_by_index(1)
                .ok_or_else(|| bad("const without a value"))?
                .0;
            // SAFETY: main thread; `v` is an element of the protected IR list.
            let (t, n) = unsafe { (rapi::TYPEOF(v), rapi::Rf_xlength(v)) };
            if n != 1 {
                return Err(bad("const of length != 1"));
            }
            Node::Const(match t {
                REALSXP => {
                    Konst::Dbl(store::numbers_of::<f64>(v, REALSXP)?.ok_or_else(|| bad("real"))?[0])
                }
                rapi::INTSXP => Konst::Int(
                    store::numbers_of::<i32>(v, rapi::INTSXP)?.ok_or_else(|| bad("int"))?[0],
                ),
                LGLSXP => Konst::Lgl(logicals_of(v)?[0]),
                STRSXP => Konst::Str(store::chars_of(v)?[0] as usize),
                _ => return Err(bad("const of an unsupported type")),
            })
        }
        "in" => {
            let k = l.get_by_index(1).ok_or_else(|| bad("input index"))?.0;
            let k = store::numbers_of::<i32>(k, rapi::INTSXP)?.ok_or_else(|| bad("input index"))?;
            Node::Input(usize::try_from(k[0]).map_err(|_| bad("negative input"))?)
        }
        "n" => Node::N,
        "runif" => Node::Runif,
        "neg" => Node::Unary(Un::Neg, boxed(1)?),
        "!" => Node::Unary(Un::Not, boxed(1)?),
        "is.na" => Node::Unary(Un::IsNa, boxed(1)?),
        "is.finite" => Node::Unary(Un::IsFinite, boxed(1)?),
        "is.infinite" => Node::Unary(Un::IsInfinite, boxed(1)?),
        "is.nan" => Node::Unary(Un::IsNaN, boxed(1)?),
        "+" => bin(Bin::Add)?,
        "-" => bin(Bin::Sub)?,
        "*" => bin(Bin::Mul)?,
        "/" => bin(Bin::Div)?,
        "<" => bin(Bin::Lt)?,
        ">" => bin(Bin::Gt)?,
        "<=" => bin(Bin::Le)?,
        ">=" => bin(Bin::Ge)?,
        "==" => bin(Bin::Eq)?,
        "!=" => bin(Bin::Ne)?,
        "&" => bin(Bin::And)?,
        "|" => bin(Bin::Or)?,
        "if_else" => Node::IfElse(boxed(1)?, boxed(2)?, boxed(3)?),
        "case_when" => {
            let pairs = l.get_by_index(1).ok_or_else(|| bad("case_when pairs"))?;
            let pairs = ListSexp::try_from(pairs).map_err(|_| bad("case_when pairs"))?;
            let mut out = Vec::with_capacity(pairs.len());
            for p in pairs.values_iter() {
                let p = ListSexp::try_from(p).map_err(|_| bad("case_when pair"))?;
                let get = |i| p.get_by_index(i).ok_or_else(|| bad("case_when pair"));
                out.push((decode(get(0)?.0)?, decode(get(1)?.0)?));
            }
            let def = match l.get_by_index(2) {
                Some(d) if !d.is_null() => Some(Box::new(decode(d.0)?)),
                _ => None,
            };
            Node::CaseWhen(out, def)
        }
        "pmin" | "pmax" => {
            let args = (1..l.len()).map(arg).collect::<savvy::Result<Vec<_>>>()?;
            Node::PMinMax(op == "pmax", args)
        }
        "exp" => Node::Math(MathFn::Exp, boxed(1)?),
        "log" => Node::Math(MathFn::Log, boxed(1)?),
        "sqrt" => Node::Math(MathFn::Sqrt, boxed(1)?),
        "sum" => Node::Reduce(Red::Sum, boxed(1)?),
        "mean" => Node::Reduce(Red::Mean, boxed(1)?),
        "sum_narm" => Node::ReduceNaRm(Red::Sum, boxed(1)?),
        "mean_narm" => Node::ReduceNaRm(Red::Mean, boxed(1)?),
        "constv" => {
            let v = l.get_by_index(1).ok_or_else(|| bad("constv"))?.0;
            match vals_of(v, false)? {
                // the IR list keeps `v` (and its CHARSXPs) alive
                Some(v) => Node::ConstVec(owned(v)),
                None => return Err(bad("constv of an unsupported type")),
            }
        }
        "call" => {
            let k = l.get_by_index(1).ok_or_else(|| bad("call index"))?.0;
            let k = store::numbers_of::<i32>(k, rapi::INTSXP)?.ok_or_else(|| bad("call index"))?;
            let k = usize::try_from(k[0]).map_err(|_| bad("call index"))?;
            Node::Call(k, (2..l.len()).map(arg).collect::<savvy::Result<Vec<_>>>()?)
        }
        "coalesce" => Node::Coalesce((1..l.len()).map(arg).collect::<savvy::Result<Vec<_>>>()?),
        "%in%" => Node::In(boxed(1)?, boxed(2)?),
        "sample" => {
            let sz = match l.get_by_index(2) {
                Some(s) if !s.is_null() => Some(Box::new(decode(s.0)?)),
                _ => None,
            };
            let rep = logicals_of(l.get_by_index(3).ok_or_else(|| bad("sample replace"))?.0)?
                .first()
                .copied()
                == Some(1);
            let prob = match l.get_by_index(4) {
                Some(s) if !s.is_null() => Some(Box::new(decode(s.0)?)),
                _ => None,
            };
            Node::Sample(boxed(1)?, sz, rep, prob)
        }
        "runif_ab" => Node::Rand(false, boxed(1)?, boxed(2)?),
        "rnorm_ab" => Node::Rand(true, boxed(1)?, boxed(2)?),
        "[" => Node::Index(boxed(1)?, boxed(2)?),
        "if" => Node::If(boxed(1)?, boxed(2)?, boxed(3)?),
        "runif_ab1" => Node::Draw1(false, boxed(1)?, boxed(2)?),
        "rnorm_ab1" => Node::Draw1(true, boxed(1)?, boxed(2)?),
        "as_int" => Node::Cast(true, boxed(1)?),
        "as_dbl" => Node::Cast(false, boxed(1)?),
        "abs" => Node::Abs(boxed(1)?),
        "min" => Node::Extreme(
            false,
            (1..l.len()).map(arg).collect::<savvy::Result<Vec<_>>>()?,
        ),
        "max" => Node::Extreme(
            true,
            (1..l.len()).map(arg).collect::<savvy::Result<Vec<_>>>()?,
        ),
        "any" => Node::AnyAll(true, false, boxed(1)?),
        "any_narm" => Node::AnyAll(true, true, boxed(1)?),
        "all" => Node::AnyAll(false, false, boxed(1)?),
        "all_narm" => Node::AnyAll(false, true, boxed(1)?),
        "between" => Node::Between(boxed(1)?, boxed(2)?, boxed(3)?),
        "ifelse" => Node::BaseIfElse(boxed(1)?, boxed(2)?, boxed(3)?),
        "seq_len" => Node::SeqLen(boxed(1)?),
        "seq_along" => Node::SeqAlong(boxed(1)?),
        "sample_int" => {
            let sz = match l.get_by_index(2) {
                Some(s) if !s.is_null() => Some(Box::new(decode(s.0)?)),
                _ => None,
            };
            let rep = logicals_of(
                l.get_by_index(3)
                    .ok_or_else(|| bad("sample_int replace"))?
                    .0,
            )?
            .first()
            .copied()
                == Some(1);
            let prob = match l.get_by_index(4) {
                Some(s) if !s.is_null() => Some(Box::new(decode(s.0)?)),
                _ => None,
            };
            Node::SampleInt(boxed(1)?, sz, rep, prob)
        }
        "rdist" => {
            let k = l.get_by_index(1).ok_or_else(|| bad("rdist kind"))?.0;
            let k = store::numbers_of::<i32>(k, rapi::INTSXP)?.ok_or_else(|| bad("rdist kind"))?;
            let kind = u8::try_from(k[0]).map_err(|_| bad("rdist kind"))?;
            let one = logicals_of(l.get_by_index(2).ok_or_else(|| bad("rdist one"))?.0)?
                .first()
                .copied()
                == Some(1);
            let b = match l.get_by_index(4) {
                Some(s) if !s.is_null() => Some(Box::new(decode(s.0)?)),
                _ => None,
            };
            Node::RDist(kind, one, boxed(3)?, b)
        }
        other => return Err(bad(&format!("unknown operator `{other}`"))),
    })
}

fn draws(n: &Node) -> bool {
    match n {
        Node::Runif
        | Node::Sample(..)
        | Node::Rand(..)
        | Node::Draw1(..)
        | Node::SampleInt(..)
        | Node::RDist(..) => true,
        Node::Const(_) | Node::Input(_) | Node::N | Node::ConstVec(_) => false,
        Node::Unary(_, x)
        | Node::Math(_, x)
        | Node::Reduce(_, x)
        | Node::ReduceNaRm(_, x)
        | Node::Cast(_, x)
        | Node::Abs(x)
        | Node::AnyAll(_, _, x)
        | Node::SeqLen(x)
        | Node::SeqAlong(x) => draws(x),
        Node::Bin(_, a, b) | Node::In(a, b) | Node::Index(a, b) => draws(a) || draws(b),
        Node::Call(_, a) | Node::Coalesce(a) | Node::Extreme(_, a) => a.iter().any(draws),
        Node::IfElse(a, b, c)
        | Node::If(a, b, c)
        | Node::Between(a, b, c)
        | Node::BaseIfElse(a, b, c) => draws(a) || draws(b) || draws(c),
        Node::CaseWhen(p, d) => {
            p.iter().any(|(c, v)| draws(c) || draws(v)) || d.as_deref().is_some_and(draws)
        }
        Node::PMinMax(_, a) => a.iter().any(draws),
    }
}

/// The data of a protected logical vector, in place (ALTREP materialised in `protect`).
fn logicals_of<'a>(x: SEXP) -> savvy::Result<&'a [i32]> {
    // SAFETY: main thread; `x` is a protected LGLSXP (checked by every caller).
    let n = unsafe { rapi::Rf_xlength(x) } as usize;
    if n == 0 {
        return Ok(&[]);
    }
    // SAFETY: as above; DATAPTR_OR_NULL never allocates or raises.
    let direct = unsafe { rapi::DATAPTR_OR_NULL(x) } as *const i32;
    if !direct.is_null() {
        // SAFETY: a logical vector's data is `n` ints, fixed while `x` is protected.
        return Ok(unsafe { std::slice::from_raw_parts(direct, n) });
    }
    // SAFETY: as above; LOGICAL_RO may materialise (allocate), hence `protect`.
    unsafe {
        let p = rapi::protect_value(std::ptr::null(), move || rapi::LOGICAL_RO(x))?;
        Ok(std::slice::from_raw_parts(p, n))
    }
}

/// One input as evaluator values, borrowing R's memory; `None` for a type the
/// evaluator does not take.
pub(crate) fn vals_of<'a>(x: SEXP, stack: bool) -> savvy::Result<Option<Vals<'a>>> {
    // SAFETY: main thread; `x` is an element of a protected argument list.
    let t = unsafe { rapi::TYPEOF(x) };
    Ok(Some(match t {
        REALSXP => Vals::Dbl(Cow::Borrowed(store::numbers_of(x, REALSXP)?.unwrap_or(&[]))),
        rapi::INTSXP => Vals::Int(Cow::Borrowed(
            store::numbers_of(x, rapi::INTSXP)?.unwrap_or(&[]),
        )),
        LGLSXP => Vals::Lgl(Cow::Borrowed(logicals_of(x)?)),
        STRSXP => {
            let c = store::chars_of(x)?;
            // SAFETY: a SEXP is a pointer, the size and alignment of a usize; the
            // slice is only read, while `x` keeps its CHARSXPs alive.
            Vals::Str(Cow::Borrowed(unsafe {
                std::slice::from_raw_parts(c.as_ptr() as *const usize, c.len())
            }))
        }
        VECSXP if stack => return stacked(x),
        _ => return Ok(None),
    }))
}

/// A list of vectors of one type, stacked into one (an `abm_global()` input read
/// across groups).
pub(crate) fn stacked<'a>(x: SEXP) -> savvy::Result<Option<Vals<'a>>> {
    let l = ListSexp::try_from(Sexp(x)).map_err(|_| bad("stacked input"))?;
    let mut out: Option<Vals<'a>> = None;
    for part in l.values_iter() {
        // SAFETY: main thread; `part` is an element of a protected list.
        if unsafe { rapi::TYPEOF(part.0) } == VECSXP {
            return Ok(None);
        }
        let Some(v) = vals_of(part.0, false)? else {
            return Ok(None);
        };
        out = Some(match (out, v) {
            (None, v) => owned(v),
            (Some(Vals::Dbl(mut a)), Vals::Dbl(b)) => {
                a.to_mut().extend_from_slice(&b);
                Vals::Dbl(a)
            }
            (Some(Vals::Int(mut a)), Vals::Int(b)) => {
                a.to_mut().extend_from_slice(&b);
                Vals::Int(a)
            }
            (Some(Vals::Lgl(mut a)), Vals::Lgl(b)) => {
                a.to_mut().extend_from_slice(&b);
                Vals::Lgl(a)
            }
            (Some(Vals::Str(mut a)), Vals::Str(b)) => {
                a.to_mut().extend_from_slice(&b);
                Vals::Str(a)
            }
            _ => return Ok(None),
        });
    }
    Ok(out)
}

/// `v` with its data owned, so further parts can be appended.
pub(crate) fn owned(v: Vals<'_>) -> Vals<'static> {
    match v {
        Vals::Dbl(b) => Vals::Dbl(Cow::Owned(b.into_owned())),
        Vals::Int(b) => Vals::Int(Cow::Owned(b.into_owned())),
        Vals::Lgl(b) => Vals::Lgl(Cow::Owned(b.into_owned())),
        Vals::Str(b) => Vals::Str(Cow::Owned(b.into_owned())),
    }
}

struct RHost<'r> {
    rng: Option<&'r mut RRng>,
    fns: &'r [SEXP],
    na_real: f64,
    na_str: usize,
    /// an R condition caught while calling R back, re-raised after `eval()`
    err: Option<savvy::Error>,
}

impl Host for RHost<'_> {
    fn na_real(&self) -> f64 {
        self.na_real
    }
    fn na_str(&self) -> usize {
        self.na_str
    }
    fn str_ascii(&self, s: usize) -> bool {
        // SAFETY: `s` is the address of a live CHARSXP (from an input or the IR);
        // R_CHAR gives its NUL-terminated bytes without allocating.
        let p = unsafe { rapi::R_CHAR(s as SEXP) };
        // SAFETY: NUL-terminated, valid while the CHARSXP lives.
        unsafe { std::ffi::CStr::from_ptr(p) }.to_bytes().is_ascii()
    }
    fn runif(&mut self, n: usize) -> Result<Vec<f64>, Decline> {
        let Some(r) = self.rng.as_deref_mut() else {
            return Err(Decline("a draw outside the RNG scope"));
        };
        let mut v = vec![0.0; n];
        // R's runif(n) is Rf_runif(0, 1) per element (P2's differential tests)
        if let Err(e) = r.fill(&mut v, Draw::Rmath2(rapi::Rf_runif, 0.0, 1.0)) {
            self.err = Some(e);
            return Err(Decline("an R condition while drawing"));
        }
        Ok(v)
    }
    fn math(&mut self, f: MathFn, x: &[f64]) -> Result<Vec<f64>, Decline> {
        let name: &[u8] = match f {
            MathFn::Exp => b"exp\0",
            MathFn::Log => b"log\0",
            MathFn::Sqrt => b"sqrt\0",
        };
        match call_math(name, x) {
            Ok(v) => Ok(v),
            Err(e) => {
                self.err = Some(e);
                Err(Decline("an R condition in exp()/log()/sqrt()"))
            }
        }
    }
    fn sum(&self, x: &[f64]) -> f64 {
        reduce::sum(x, false)
    }
    fn mean(&self, x: &[f64]) -> f64 {
        reduce::mean(x)
    }
    fn mean_int(&self, x: &[i32]) -> f64 {
        reduce::mean_int(x)
    }
    fn call(&mut self, f: usize, args: &[Vals]) -> Result<Vals<'static>, Decline> {
        let Some(&fun) = self.fns.get(f) else {
            return Err(Decline("a function out of range"));
        };
        // numbers only: with them the functions called this way cannot err
        if args.iter().any(|a| matches!(a, Vals::Str(_))) {
            return Err(Decline("a string handed to an R function"));
        }
        if args.len() > 1 {
            common_len(args)?;
        }
        match call_fn(fun, args) {
            Ok(Some(v)) => Ok(v),
            Ok(None) => Err(Decline(
                "R's function warned, erred or gave a classed value",
            )),
            Err(e) => {
                self.err = Some(e);
                Err(Decline("an R condition in a function call"))
            }
        }
    }
    fn rand2(&mut self, norm: bool, a: f64, b: f64, n: usize) -> Result<Vec<f64>, Decline> {
        let Some(r) = self.rng.as_deref_mut() else {
            return Err(Decline("a draw outside the RNG scope"));
        };
        let mut v = vec![0.0; n];
        let f = if norm { rapi::Rf_rnorm } else { rapi::Rf_runif };
        if let Err(e) = r.fill(&mut v, Draw::Rmath2(f, a, b)) {
            self.err = Some(e);
            return Err(Decline("an R condition while drawing"));
        }
        Ok(v)
    }
    fn unif_index(&mut self, dn: f64, n: usize) -> Result<Vec<i32>, Decline> {
        let Some(r) = self.rng.as_deref_mut() else {
            return Err(Decline("a draw outside the RNG scope"));
        };
        let mut v = vec![0.0; n];
        if let Err(e) = r.fill(&mut v, Draw::Index(dn)) {
            self.err = Some(e);
            return Err(Decline("an R condition while drawing"));
        }
        Ok(v.iter().map(|&x| x as i32 + 1).collect())
    }
    fn sample_int(&mut self, n: usize, size: usize, replace: bool) -> Result<Vec<i32>, Decline> {
        if tidyabm_core::sample::check_sample(n, size, replace).is_err() {
            return Err(Decline("sample.int() invalid n or size"));
        }
        let Some(r) = self.rng.as_deref_mut() else {
            return Err(Decline("a draw outside the RNG scope"));
        };
        tidyabm_core::sample::sample_int(r, n, size, replace)
            .map_err(|_| Decline("sample.int() failed"))
    }
    fn sample_int_prob(
        &mut self,
        n: usize,
        size: usize,
        replace: bool,
        prob: &[f64],
    ) -> Result<Vec<i32>, Decline> {
        if tidyabm_core::sample::check_sample_prob(n, size, replace, prob, |x| {
            reduce::sum(x, false)
        })
        .is_err()
        {
            return Err(Decline("sample() invalid prob or size"));
        }
        let Some(r) = self.rng.as_deref_mut() else {
            return Err(Decline("a draw outside the RNG scope"));
        };
        tidyabm_core::sample::sample_int_prob(r, n, size, replace, prob)
            .map_err(|_| Decline("sample() with prob failed"))
    }
    fn rdist(&mut self, kind: u8, a: f64, b: f64, n: usize) -> Result<Vals<'static>, Decline> {
        let Some(r) = self.rng.as_deref_mut() else {
            return Err(Decline("a draw outside the RNG scope"));
        };
        let fin = a.is_finite() && b.is_finite();
        let (d, ok, as_int) = match kind {
            0 => {
                let sz_ok = a >= 0.0 && a <= i32::MAX as f64 && a.fract() == 0.0;
                (
                    Draw::Rmath2(rapi::Rf_rbinom, a, b),
                    fin && sz_ok && (0.0..=1.0).contains(&b),
                    true,
                )
            }
            1 => (Draw::Rmath1(rapi::Rf_rpois, a), fin && a >= 0.0, true),
            2 => (
                Draw::Rmath1(rapi::Rf_rexp, 1.0 / a),
                fin && a > 0.0 && (1.0 / a).is_finite(),
                false,
            ),
            3 => (Draw::Rmath2(rapi::Rf_rlnorm, a, b), fin && b >= 0.0, false),
            _ => return Err(Decline("unknown rdist kind")),
        };
        if !ok {
            return Err(Decline("rdist() parameter out of range (R warns)"));
        }
        let mut v = vec![0.0; n];
        if let Err(e) = r.fill(&mut v, d) {
            self.err = Some(e);
            return Err(Decline("an R condition while drawing"));
        }
        if v.iter().any(|x| x.is_nan()) {
            return Err(Decline("rdist() made NaN (R warns)"));
        }
        if as_int
            && v.iter()
                .all(|&x| x.is_finite() && (0.0..=i32::MAX as f64).contains(&x))
        {
            Ok(Vals::Int(Cow::Owned(v.iter().map(|&x| x as i32).collect())))
        } else {
            Ok(Vals::Dbl(Cow::Owned(v)))
        }
    }
}

/// The common length of arguments of length 1 or `n` (else a decline: R would
/// recycle, maybe with a warning).
fn common_len(args: &[Vals]) -> Result<usize, Decline> {
    let mut n = 1;
    for a in args {
        match a.len() {
            0 => return Err(Decline("zero-length argument")),
            1 => {}
            l if n == 1 || n == l => n = l,
            _ => return Err(Decline("arguments of different lengths")),
        }
    }
    Ok(n)
}

/// R function `fun` called with `args` (as new R vectors); `None` when it
/// returned `NULL` (a guarded function caught a condition) or anything else than
/// a bare logical, integer or double vector.
fn call_fn(fun: SEXP, args: &[Vals]) -> savvy::Result<Option<Vals<'static>>> {
    // the arguments as R vectors, kept in a list while the call is built
    let mut held = OwnedListSexp::new(args.len(), false)?;
    for (i, a) in args.iter().enumerate() {
        held.set_value(i, to_r(a, a.len())?)?;
    }
    let hp = held.inner();
    let k = args.len();
    // SAFETY: main thread; `fun` is kept by the program, the arguments by `held`;
    // every object allocated is protected until the result is kept; an R error
    // longjmps to `protect` and comes back as `Err`.
    let (x, tok) = unsafe {
        rapi::protect_value((rapi::R_NilValue, rapi::R_NilValue), move || {
            let mut a = rapi::R_NilValue;
            for i in (0..k).rev() {
                a = rapi::Rf_protect(rapi::Rf_cons(rapi::VECTOR_ELT(hp, i as isize), a));
            }
            let call = rapi::Rf_protect(rapi::Rf_lcons(fun, a));
            let res = rapi::Rf_protect(rapi::Rf_eval(call, rapi::R_BaseEnv));
            let tok = savvy::protect::insert_to_preserved_list(res);
            rapi::Rf_unprotect(k as i32 + 2);
            (res, tok)
        })
    }?;
    let res = store::kept_from(x, tok);
    let x = res.get();
    // SAFETY: reading a type tag.
    let t = unsafe { rapi::TYPEOF(x) };
    if !matches!(t, REALSXP | rapi::INTSXP | LGLSXP) || !crate::runloop::all_bare(&[x])? {
        return Ok(None);
    }
    Ok(vals_of(x, false)?.map(owned))
}

/// R's own `exp(x)` (`log`, `sqrt`) over a copy of `x`, its result copied back.
fn call_math(name: &'static [u8], x: &[f64]) -> savvy::Result<Vec<f64>> {
    let n = x.len();
    let mut out = vec![0.0; n];
    let (xp, op) = (x.as_ptr(), out.as_mut_ptr());
    // SAFETY: main thread; everything allocated is protected until the result is
    // copied out; `xp`/`op` describe `x`/`out` (length n); R's exp/log/sqrt of a
    // double vector returns a double vector of the same length; no panic inside.
    unsafe {
        rapi::protect(move || {
            let v = rapi::Rf_protect(rapi::Rf_allocVector(REALSXP, n as isize));
            let dst = rapi::REAL_OR_NULL(v) as *mut f64;
            std::ptr::copy_nonoverlapping(xp, dst, n);
            let call = rapi::Rf_protect(rapi::Rf_lang2(rapi::Rf_install(name.as_ptr().cast()), v));
            let res = rapi::Rf_protect(rapi::Rf_eval(call, rapi::R_BaseEnv));
            if rapi::TYPEOF(res) == REALSXP && rapi::Rf_xlength(res) as usize == n {
                std::ptr::copy_nonoverlapping(rapi::REAL_RO(res), op, n);
            }
            rapi::Rf_unprotect(3);
        })
    }?;
    Ok(out)
}

/// A result as an R vector of `n` elements (a length-1 value recycled).
pub(crate) fn to_r(v: &Vals, n: usize) -> savvy::Result<Sexp> {
    if v.len() != n && v.len() != 1 {
        return Err(bad("result of the wrong length"));
    }
    let pick = |i: usize| if v.len() == 1 { 0 } else { i };
    let ty = match v {
        Vals::Dbl(_) => REALSXP,
        Vals::Int(_) => rapi::INTSXP,
        Vals::Lgl(_) => LGLSXP,
        Vals::Str(_) => STRSXP,
    };
    // SAFETY: main thread; the new vector is protected while filled and returned
    // at once (savvy protects what an entry point returns); writes stay in 0..n;
    // the CHARSXPs set are live (inputs or IR literals); no panic inside.
    let x = unsafe {
        let v: *const Vals = v;
        rapi::protect_value(rapi::R_NilValue, move || {
            let x = rapi::Rf_protect(rapi::Rf_allocVector(ty, n as isize));
            match &*v {
                Vals::Dbl(d) => {
                    let p = rapi::REAL_OR_NULL(x) as *mut f64;
                    for i in 0..n {
                        *p.add(i) = d[pick(i)];
                    }
                }
                Vals::Int(d) | Vals::Lgl(d) => {
                    let p = rapi::DATAPTR_OR_NULL(x) as *mut i32;
                    for i in 0..n {
                        *p.add(i) = d[pick(i)];
                    }
                }
                Vals::Str(d) => {
                    for i in 0..n {
                        rapi::SET_STRING_ELT(x, i as isize, d[pick(i)] as SEXP);
                    }
                }
            }
            rapi::Rf_unprotect(1);
            x
        })
    }?;
    Ok(Sexp(x))
}

#[savvy]
impl RxProgram {
    /// Decode the IR: `rules` a list of node trees, `writes` (integer, 0-based, -1
    /// for none) the input each rule's result replaces for the rules after it.
    fn new(rules: ListSexp, writes: savvy::IntegerSexp, fns: ListSexp) -> savvy::Result<Self> {
        run(|| {
            let ir = store::keep(rules.inner())?;
            let kept_fns = store::keep(fns.inner())?;
            let fn_list: Vec<SEXP> = fns.values_iter().map(|f| f.0).collect();
            let mut out = Vec::with_capacity(rules.len());
            for r in rules.values_iter() {
                out.push(decode(r.0)?);
            }
            let writes: Vec<Option<usize>> = writes
                .as_slice()
                .iter()
                .map(|&k| usize::try_from(k).ok())
                .collect();
            if writes.len() != out.len() {
                return Err(bad("writes and rules differ in length"));
            }
            let draws = out.iter().any(draws);
            Ok(RxProgram {
                rules: out,
                writes,
                draws,
                fns: fn_list,
                _ir: ir,
                _fns: kept_fns,
            })
        })
    }

    /// Does any rule draw?
    fn draws(&self) -> savvy::Result<Sexp> {
        run(|| savvy::OwnedLogicalSexp::try_from_scalar(self.draws).map(Into::into))
    }

    /// Evaluate every rule, in order, over `n` rows of `inputs`; each result as an
    /// R vector of `out_n` elements. With `stacked`, an input that is a list of
    /// vectors is their concatenation (a column read across groups); otherwise a
    /// list input declines. `NULL` (and the reason kept for `rs_rx_reason()`)
    /// where R must evaluate the step instead.
    fn eval(&self, inputs: ListSexp, n: i32, out_n: i32, stacked: bool) -> savvy::Result<Sexp> {
        run(|| {
            let n = usize::try_from(n).map_err(|_| bad("negative n"))?;
            let out_n = usize::try_from(out_n).map_err(|_| bad("negative out_n"))?;
            let mut ins = Vec::with_capacity(inputs.len());
            for x in inputs.values_iter() {
                match vals_of(x.0, stacked)? {
                    Some(v) => ins.push(v),
                    None => return declined(Decline("an input of an unsupported type")),
                }
            }
            let res = match self.compute(ins, n, out_n)? {
                Ok(r) => r,
                Err(d) => return declined(d),
            };
            let mut out = OwnedListSexp::new(res.len(), false)?;
            for (i, v) in res.iter().enumerate() {
                out.set_value(i, to_r(v, out_n)?)?;
            }
            Ok(out.into())
        })
    }
}

impl RxProgram {
    /// Does any rule draw?
    pub(crate) fn draws_any(&self) -> bool {
        self.draws
    }

    /// The input rule `k` overwrites for the rules after it.
    pub(crate) fn write_of(&self, k: usize) -> Option<usize> {
        self.writes.get(k).copied().flatten()
    }

    /// Does rule `k` draw?
    pub(crate) fn draws_rule(&self, k: usize) -> bool {
        self.rules.get(k).is_some_and(draws)
    }

    /// Rule `k` alone over `n` rows of `ins` (a step whose rules R and Rust
    /// share: each in its turn); as [`RxProgram::compute`].
    pub(crate) fn compute_one<'a>(
        &self,
        k: usize,
        ins: &[Vals<'a>],
        n: usize,
    ) -> savvy::Result<Result<Vals<'a>, Decline>> {
        let rule = self.rules.get(k).ok_or_else(|| bad("rule out of range"))?;
        // SAFETY: reading statics of R's (its NA_real_ and NA_character_).
        let (na_real, na_str) = unsafe { (rapi::R_NaReal, rapi::R_NaString as usize) };
        let body = |rng: Option<&mut RRng>| -> savvy::Result<Result<Vals<'a>, Decline>> {
            let mut h = RHost {
                rng,
                fns: &self.fns,
                na_real,
                na_str,
                err: None,
            };
            match ce::eval(rule, ins, n, &mut h) {
                Ok(v) if v.len() == n || v.len() == 1 => Ok(Ok(v)),
                Ok(_) => Ok(Err(Decline("a result of the wrong length"))),
                Err(d) => match h.err.take() {
                    Some(e) => Err(e),
                    None => Ok(Err(d)),
                },
            }
        };
        if draws(rule) {
            rng::with_rng(|r| body(Some(r)))
        } else {
            body(None)
        }
    }

    /// Every rule, in order, over `n` rows of `ins` (the later rules seeing the
    /// inputs earlier ones overwrite); `Err(Decline)` where R must evaluate the
    /// step, and also for a result whose length is neither `out_n` nor 1. An R
    /// condition raised while calling R back is the outer `Err`.
    pub(crate) fn compute<'a>(
        &self,
        mut ins: Vec<Vals<'a>>,
        n: usize,
        out_n: usize,
    ) -> savvy::Result<Result<Vec<Vals<'a>>, Decline>> {
        // SAFETY: reading statics of R's (its NA_real_ and NA_character_).
        let (na_real, na_str) = unsafe { (rapi::R_NaReal, rapi::R_NaString as usize) };
        let mut body = |rng: Option<&mut RRng>| -> savvy::Result<Result<Vec<Vals<'a>>, Decline>> {
            let mut h = RHost {
                rng,
                fns: &self.fns,
                na_real,
                na_str,
                err: None,
            };
            let mut res = Vec::with_capacity(self.rules.len());
            for (rule, w) in self.rules.iter().zip(&self.writes) {
                match ce::eval(rule, &ins, n, &mut h) {
                    Ok(v) => {
                        if let Some(k) = *w {
                            if k >= ins.len() {
                                return Err(bad("write target out of range"));
                            }
                            ins[k] = v.clone();
                        }
                        res.push(v);
                    }
                    Err(d) => {
                        if let Some(e) = h.err.take() {
                            return Err(e);
                        }
                        return Ok(Err(d));
                    }
                }
            }
            Ok(Ok(res))
        };
        let res = if self.draws {
            rng::with_rng(|r| body(Some(r)))?
        } else {
            body(None)?
        };
        Ok(match res {
            Ok(r) if r.iter().any(|v| v.len() != out_n && v.len() != 1) => {
                Err(Decline("a result of the wrong length"))
            }
            other => other,
        })
    }
}

/// One host held across many evaluations of several programs (FR8: a sequential
/// step's agents, one at a time), inside one RNG scope: R's generator is loaded
/// once for the whole step rather than per evaluation.
pub(crate) struct Evaluator<'r> {
    h: RHost<'r>,
}

impl<'r> Evaluator<'r> {
    pub(crate) fn new(rng: Option<&'r mut RRng>) -> Self {
        // SAFETY: reading statics of R's (its NA_real_ and NA_character_).
        let (na_real, na_str) = unsafe { (rapi::R_NaReal, rapi::R_NaString as usize) };
        Evaluator {
            h: RHost {
                rng,
                fns: &[],
                na_real,
                na_str,
                err: None,
            },
        }
    }

    /// The generator, for draws made outside a program (a shuffle).
    pub(crate) fn rng(&mut self) -> Option<&mut RRng> {
        self.h.rng.as_deref_mut()
    }

    /// Rule `k` of `prog` over `n` rows of `ins`: as [`RxProgram::compute_one`]
    /// (a result of the wrong length declines; an R condition is the outer `Err`).
    pub(crate) fn eval<'a>(
        &mut self,
        prog: &'r RxProgram,
        k: usize,
        ins: &[Vals<'a>],
        n: usize,
    ) -> savvy::Result<Result<Vals<'a>, Decline>> {
        let rule = prog.rules.get(k).ok_or_else(|| bad("rule out of range"))?;
        self.h.fns = &prog.fns;
        match ce::eval(rule, ins, n, &mut self.h) {
            Ok(v) if v.len() == n || v.len() == 1 => Ok(Ok(v)),
            Ok(_) => Ok(Err(Decline("a result of the wrong length"))),
            Err(d) => match self.h.err.take() {
                Some(e) => Err(e),
                None => Ok(Err(d)),
            },
        }
    }
}

/// `assign_rule()`'s write of `value` (length 1 or `rows.len()`) into the rows
/// `rows` of column `old` (`None`: a new column, `NA` elsewhere), in the vctrs
/// common type of the two.
pub(crate) fn assign_rows(
    old: Option<Vals>,
    value: &Vals,
    rows: &[bool],
) -> savvy::Result<Result<Vals<'static>, Decline>> {
    // SAFETY: reading statics of R's (its NA_real_ and NA_character_).
    let (na_real, na_str) = unsafe { (rapi::R_NaReal, rapi::R_NaString as usize) };
    let h = RHost {
        rng: None,
        fns: &[],
        na_real,
        na_str,
        err: None,
    };
    Ok(ce::assign_rows(old, value.clone(), rows, &h))
}

/// `f` with an evaluator host that has no generator and no functions (R's NA
/// values only), for the typing helpers of `tidyabm_core::expr`.
pub(crate) fn with_host<T>(f: impl FnOnce(&dyn Host) -> T) -> T {
    // SAFETY: reading statics of R's (its NA_real_ and NA_character_).
    let (na_real, na_str) = unsafe { (rapi::R_NaReal, rapi::R_NaString as usize) };
    let h = RHost {
        rng: None,
        fns: &[],
        na_real,
        na_str,
        err: None,
    };
    f(&h)
}

/// Keep `d` as the reason the last evaluation declined (for `rs_rx_reason()`).
pub(crate) fn note_decline(d: &Decline) {
    LAST_DECLINE.with(|c| c.set(d.0));
}

fn declined(d: Decline) -> savvy::Result<Sexp> {
    note_decline(&d);
    Ok(NullSexp.into())
}

/// Why the last `RxProgram$eval()` returned `NULL`.
/// @noRd
#[savvy]
fn rs_rx_reason() -> savvy::Result<Sexp> {
    run(|| {
        let r = LAST_DECLINE.with(|c| c.get());
        savvy::OwnedStringSexp::try_from_scalar(r).map(Into::into)
    })
}
