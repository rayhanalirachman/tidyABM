//! The expression evaluator of the Rust engine (migration phase 5): a rule's
//! formula, compiled by R (`R/rust-expr.R`) into a tree of [`Node`]s, evaluated a
//! column at a time with R's and dplyr's semantics.
//!
//! The evaluator never guesses. Wherever R would warn, error, or produce bits it
//! cannot reproduce exactly (an integer overflow, `log()` of a negative number, a
//! NaN reaching `pmin()`, a non-ASCII string comparison, a size rule dplyr rejects),
//! it returns [`Decline`] and the caller runs the whole step on the R path from the
//! state and random stream it started from, so R raises its own condition.
//!
//! What touches R (the random stream, R's own `exp`/`log`/`sqrt`, the long-double
//! sums, R's `NA_real_` and `NA_character_`) goes through [`Host`], implemented by
//! the bridge crate; tests use a plain implementation.

use crate::sample::NA_INT;
use std::borrow::Cow;

/// Why a step leaves the Rust path (kept for coverage reports).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Decline(pub &'static str);

/// A column, a scalar or an intermediate result, in R's representation: logicals
/// and integers as `i32` with `NA_INT`, doubles with their bits, strings as the
/// address of R's `CHARSXP` (equal address, equal string).
#[derive(Debug, Clone)]
pub enum Vals<'a> {
    Lgl(Cow<'a, [i32]>),
    Int(Cow<'a, [i32]>),
    Dbl(Cow<'a, [f64]>),
    Str(Cow<'a, [usize]>),
}

impl Vals<'_> {
    pub fn len(&self) -> usize {
        match self {
            Vals::Lgl(v) | Vals::Int(v) => v.len(),
            Vals::Dbl(v) => v.len(),
            Vals::Str(v) => v.len(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    fn rank(&self) -> u8 {
        match self {
            Vals::Lgl(_) => 0,
            Vals::Int(_) => 1,
            Vals::Dbl(_) => 2,
            Vals::Str(_) => 3,
        }
    }

    /// vctrs' "unspecified": a bare logical vector of only `NA`, which takes the
    /// type of whatever it is combined with.
    fn unspecified(&self) -> bool {
        matches!(self, Vals::Lgl(v) if !v.is_empty() && v.iter().all(|&x| x == NA_INT))
    }
}

/// A literal.
#[derive(Debug, Clone, Copy)]
pub enum Konst {
    Lgl(i32),
    Int(i32),
    Dbl(f64),
    Str(usize),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Un {
    Neg,
    Not,
    IsNa,
    IsFinite,
    IsInfinite,
    IsNaN,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Bin {
    Add,
    Sub,
    Mul,
    Div,
    Lt,
    Gt,
    Le,
    Ge,
    Eq,
    Ne,
    And,
    Or,
}

/// Functions R computes for us (its own code, called back through the host).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MathFn {
    Exp,
    Log,
    Sqrt,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Red {
    Sum,
    Mean,
}

/// One node of a compiled formula.
#[derive(Debug, Clone)]
pub enum Node {
    Const(Konst),
    /// the k-th input (a column, a global or a value from the formula's scope)
    Input(usize),
    /// `n()`: the number of rows evaluated over
    N,
    Unary(Un, Box<Node>),
    Bin(Bin, Box<Node>, Box<Node>),
    /// dplyr's `if_else(condition, true, false)`
    IfElse(Box<Node>, Box<Node>, Box<Node>),
    /// dplyr's `case_when(c1 ~ v1, ..., .default = d)`
    CaseWhen(Vec<(Node, Node)>, Option<Box<Node>>),
    /// base `pmin()` (`false`) / `pmax()` (`true`)
    PMinMax(bool, Vec<Node>),
    Math(MathFn, Box<Node>),
    /// `runif(n())`
    Runif,
    Reduce(Red, Box<Node>),
    /// `sum(x, na.rm = TRUE)`, `mean(x, na.rm = TRUE)`
    ReduceNaRm(Red, Box<Node>),
    /// a literal vector (`c(1, 2)`, `c("a", "b")`), evaluated by R at compile time
    ConstVec(Vals<'static>),
    /// a base function R computes over the evaluated arguments (`^`, `abs()`,
    /// `cumsum()`, ...): the host's function table entry `.0`
    Call(usize, Vec<Node>),
    /// dplyr's `coalesce(x, y, ...)`
    Coalesce(Vec<Node>),
    /// `x %in% table` (the engine's `in_rowwise()`, base `match()` for an atomic
    /// table)
    In(Box<Node>, Box<Node>),
    /// `sample(x, size, replace, prob)`
    Sample(Box<Node>, Option<Box<Node>>, bool, Option<Box<Node>>),
    /// `runif(n(), a, b)` (`false`) / `rnorm(n(), a, b)` (`true`)
    Rand(bool, Box<Node>, Box<Node>),
    /// `x[i]`, `i` positive whole numbers or `NA`
    Index(Box<Node>, Box<Node>),
    /// base `if (c) t else f`: one value of `c`, then only the branch taken (FR8)
    If(Box<Node>, Box<Node>, Box<Node>),
    /// `runif(1, a, b)` (`false`) / `rnorm(1, a, b)` (`true`): one draw whatever
    /// the number of rows (FR8)
    Draw1(bool, Box<Node>, Box<Node>),
    /// `as.integer(x)` (`true`) / `as.numeric(x)`, `as.double(x)` of numbers (FR8)
    Cast(bool, Box<Node>),
    /// base `abs(x)` of numbers: logicals and integers as integers (FR12)
    Abs(Box<Node>),
    /// base `min(...)` (`false`) / `max(...)` (`true`) without `na.rm` (numeric/int/lgl)
    Extreme(bool, Vec<Node>),
    /// base `any(x, na.rm =)` (`true`) / `all(x, na.rm =)` (`false`) on logicals
    AnyAll(bool, bool, Box<Node>),
    /// dplyr `between(x, left, right)`
    Between(Box<Node>, Box<Node>, Box<Node>),
    /// base `seq_len(k)` / `seq_along(x)`
    SeqLen(Box<Node>),
    SeqAlong(Box<Node>),
    /// base `rank(x, ties.method = "random")` of numbers without `NA`
    RankRandom(Box<Node>),
    /// base lazy `ifelse(test, yes, no)`
    BaseIfElse(Box<Node>, Box<Node>, Box<Node>),
    /// base `sample.int(n, size, replace, prob)`
    SampleInt(Box<Node>, Option<Box<Node>>, bool, Option<Box<Node>>),
    /// `rbinom` (0), `rpois` (1), `rexp` (2), `rlnorm` (3); `one` = scalar draw (1 vs n())
    RDist(u8, bool, Box<Node>, Option<Box<Node>>),
}

/// What the evaluator needs from R.
pub trait Host {
    /// R's `NA_real_`, read at run time.
    fn na_real(&self) -> f64;
    /// R's `NA_character_`.
    fn na_str(&self) -> usize;
    /// Is the string all ASCII? (ASCII strings are never marked with an encoding,
    /// so two ASCII strings at different addresses differ.)
    fn str_ascii(&self, s: usize) -> bool;
    /// `runif(n)` from R's generator.
    fn runif(&mut self, n: usize) -> Result<Vec<f64>, Decline>;
    /// R's own `exp(x)`, `log(x)`, `sqrt(x)`.
    fn math(&mut self, f: MathFn, x: &[f64]) -> Result<Vec<f64>, Decline>;
    /// `sum(x)` and `mean(x)` of doubles, accumulated as R does (long double).
    fn sum(&self, x: &[f64]) -> f64;
    fn mean(&self, x: &[f64]) -> f64;
    /// `mean(x)` of integers without `NA`.
    fn mean_int(&self, x: &[i32]) -> f64;
    /// R's function `f` (the program's table) over `args`: its value, or a
    /// decline where it warned, erred or returned something else than a bare
    /// vector.
    fn call(&mut self, f: usize, args: &[Vals]) -> Result<Vals<'static>, Decline>;
    /// `n` draws of `Rf_runif(a, b)` (`norm = false`) or `Rf_rnorm(a, b)`.
    fn rand2(&mut self, norm: bool, a: f64, b: f64, n: usize) -> Result<Vec<f64>, Decline>;
    /// `n` index draws `R_unif_index(dn)`, as 1-based values.
    fn unif_index(&mut self, dn: f64, n: usize) -> Result<Vec<i32>, Decline>;
    /// `sample.int(n, size, replace)` from R's generator.
    fn sample_int(&mut self, n: usize, size: usize, replace: bool) -> Result<Vec<i32>, Decline>;
    /// `sample.int(n, size, replace, prob)` from R's generator.
    fn sample_int_prob(
        &mut self,
        n: usize,
        size: usize,
        replace: bool,
        prob: &[f64],
    ) -> Result<Vec<i32>, Decline>;
    /// Rmath variate draws: 0 `rbinom(a, b)`, 1 `rpois(a)`, 2 `rexp(a)`, 3 `rlnorm(a, b)`.
    fn rdist(&mut self, kind: u8, a: f64, b: f64, n: usize) -> Result<Vals<'static>, Decline>;
}

type R<T> = Result<T, Decline>;

/// Typed slices of values already cast to one type, for the selections below.
macro_rules! typed {
    ($vals:expr, $var:ident) => {
        $vals
            .iter()
            .map(|v| match v {
                Vals::$var(x) => &x[..],
                _ => unreachable!("values are cast to the result type first"),
            })
            .collect::<Vec<_>>()
    };
}

/// The common length of operands of length 1 or `n` (R recycles others too, with or
/// without a warning; the engine never builds them, so they decline).
fn common_len(lens: &[usize]) -> R<usize> {
    let mut n = 1usize;
    for &l in lens {
        if l == 0 {
            return Err(Decline("zero-length operand"));
        }
        if l != 1 {
            if n != 1 && n != l {
                return Err(Decline("operands of different lengths"));
            }
            n = l;
        }
    }
    Ok(n)
}

#[inline]
fn at<T: Copy>(v: &[T], i: usize) -> T {
    if v.len() == 1 {
        v[0]
    } else {
        v[i]
    }
}

/// `f` over operands of length 1 or `n` (checked by the caller), specialised so the
/// loops vectorise.
#[inline(always)]
fn map2<A: Copy, B: Copy, O>(x: &[A], y: &[B], f: impl Fn(A, B) -> O) -> Vec<O> {
    match (x.len(), y.len()) {
        (1, _) => {
            let a = x[0];
            y.iter().map(|&b| f(a, b)).collect()
        }
        (_, 1) => {
            let b = y[0];
            x.iter().map(|&a| f(a, b)).collect()
        }
        _ => x.iter().zip(y).map(|(&a, &b)| f(a, b)).collect(),
    }
}

/// Integers (or logicals) as doubles, R's way: `NA` becomes R's `NA_real_`.
fn int_to_dbl(v: &[i32], na: f64) -> Vec<f64> {
    v.iter()
        .map(|&x| if x == NA_INT { na } else { x as f64 })
        .collect()
}

fn as_dbl<'a>(v: Vals<'a>, na: f64) -> R<Cow<'a, [f64]>> {
    match v {
        Vals::Dbl(d) => Ok(d),
        Vals::Int(i) | Vals::Lgl(i) => Ok(Cow::Owned(int_to_dbl(&i, na))),
        Vals::Str(_) => Err(Decline("a string where a number is needed")),
    }
}

/// `v` cast to type rank `to` (0 logical, 1 integer, 2 double, 3 string) as vctrs
/// casts within a common type; an unspecified `NA` vector becomes `NA` of `to`.
fn cast<'a>(v: Vals<'a>, to: u8, h: &dyn Host) -> R<Vals<'a>> {
    if v.rank() == to {
        return Ok(v);
    }
    if v.unspecified() {
        let n = v.len();
        return Ok(match to {
            1 => Vals::Int(Cow::Owned(vec![NA_INT; n])),
            2 => Vals::Dbl(Cow::Owned(vec![h.na_real(); n])),
            3 => Vals::Str(Cow::Owned(vec![h.na_str(); n])),
            _ => v,
        });
    }
    match (v, to) {
        (Vals::Lgl(x), 1) => Ok(Vals::Int(x)),
        (Vals::Lgl(x) | Vals::Int(x), 2) => Ok(Vals::Dbl(Cow::Owned(int_to_dbl(&x, h.na_real())))),
        _ => Err(Decline("values of incompatible types")),
    }
}

/// vctrs' common type of `vals` (unspecified values do not count), as a rank.
fn common_type(vals: &[&Vals]) -> R<u8> {
    let mut to: Option<u8> = None;
    for v in vals {
        if v.unspecified() {
            continue;
        }
        let r = v.rank();
        to = Some(match to {
            None => r,
            Some(t) if (t == 3) != (r == 3) => return Err(Decline("strings mixed with numbers")),
            Some(t) => t.max(r),
        });
    }
    Ok(to.unwrap_or(0))
}

/// Evaluate `node` over `n` rows with `inputs`.
pub fn eval<'a>(node: &Node, inputs: &[Vals<'a>], n: usize, h: &mut dyn Host) -> R<Vals<'a>> {
    match node {
        Node::Const(k) => Ok(match *k {
            Konst::Lgl(x) => Vals::Lgl(Cow::Owned(vec![x])),
            Konst::Int(x) => Vals::Int(Cow::Owned(vec![x])),
            Konst::Dbl(x) => Vals::Dbl(Cow::Owned(vec![x])),
            Konst::Str(x) => Vals::Str(Cow::Owned(vec![x])),
        }),
        Node::Input(k) => inputs.get(*k).cloned().ok_or(Decline("input out of range")),
        Node::N => {
            let n = i32::try_from(n).map_err(|_| Decline("n() too large"))?;
            Ok(Vals::Int(Cow::Owned(vec![n])))
        }
        Node::Unary(op, x) => unary(*op, eval(x, inputs, n, h)?, h),
        Node::Bin(op, a, b) => {
            // R evaluates both sides, left first (also for `&` and `|`)
            let a = eval(a, inputs, n, h)?;
            let b = eval(b, inputs, n, h)?;
            binary(*op, a, b, h)
        }
        Node::IfElse(c, t, f) => {
            let c = eval(c, inputs, n, h)?;
            let t = eval(t, inputs, n, h)?;
            let f = eval(f, inputs, n, h)?;
            if_else(c, t, f, h)
        }
        Node::CaseWhen(pairs, default) => {
            // dplyr evaluates each formula's left side, then its right side, in
            // written order, and `.default` last
            let mut conds = Vec::with_capacity(pairs.len());
            let mut vals = Vec::with_capacity(pairs.len());
            for (c, v) in pairs {
                conds.push(eval(c, inputs, n, h)?);
                vals.push(eval(v, inputs, n, h)?);
            }
            let def = match default {
                Some(d) => Some(eval(d, inputs, n, h)?),
                None => None,
            };
            case_when(conds, vals, def, h)
        }
        Node::PMinMax(max, args) => {
            let mut vals = Vec::with_capacity(args.len());
            for a in args {
                vals.push(eval(a, inputs, n, h)?);
            }
            pminmax(*max, vals, h)
        }
        Node::Math(f, x) => {
            let x = as_dbl(eval(x, inputs, n, h)?, h.na_real())?;
            if matches!(f, MathFn::Log | MathFn::Sqrt) && x.iter().any(|&v| v < 0.0) {
                return Err(Decline("log() or sqrt() of a negative number warns"));
            }
            Ok(Vals::Dbl(Cow::Owned(h.math(*f, &x)?)))
        }
        Node::Runif => Ok(Vals::Dbl(Cow::Owned(h.runif(n)?))),
        Node::Reduce(r, x) => reduce(*r, eval(x, inputs, n, h)?, h),
        Node::ReduceNaRm(r, x) => reduce_na_rm(*r, eval(x, inputs, n, h)?, h),
        Node::ConstVec(v) => Ok(v.clone()),
        Node::Call(f, args) => {
            let mut vals = Vec::with_capacity(args.len());
            for a in args {
                vals.push(eval(a, inputs, n, h)?);
            }
            h.call(*f, &vals)
        }
        Node::Coalesce(args) => {
            let mut vals = Vec::with_capacity(args.len());
            for a in args {
                vals.push(eval(a, inputs, n, h)?);
            }
            coalesce(vals, h)
        }
        Node::In(x, t) => {
            let x = eval(x, inputs, n, h)?;
            let t = eval(t, inputs, n, h)?;
            is_in(&x, &t, h)
        }
        Node::Sample(x, size, replace, prob) => {
            let x = eval(x, inputs, n, h)?;
            let sz = match size {
                Some(s) => Some(whole(&eval(s, inputs, n, h)?, 0.0)?),
                None => None,
            };
            let pr = match prob {
                Some(p) => Some(prob_vec(&eval(p, inputs, n, h)?)?),
                None => None,
            };
            sample(x, sz, *replace, pr.as_deref(), h)
        }
        Node::Rand(norm, a, b) => {
            let a = scalar_dbl(eval(a, inputs, n, h)?, h)?;
            let b = scalar_dbl(eval(b, inputs, n, h)?, h)?;
            let v = h.rand2(*norm, a, b, n)?;
            if v.iter().any(|x| x.is_nan()) {
                return Err(Decline("runif()/rnorm() made NaN (R warns)"));
            }
            Ok(Vals::Dbl(Cow::Owned(v)))
        }
        Node::Index(x, i) => {
            let x = eval(x, inputs, n, h)?;
            let i = eval(i, inputs, n, h)?;
            index(x, &i, h)
        }
        Node::If(..)
        | Node::Draw1(..)
        | Node::Cast(..)
        | Node::Abs(..)
        | Node::Extreme(..)
        | Node::AnyAll(..)
        | Node::Between(..)
        | Node::SeqLen(..)
        | Node::SeqAlong(..)
        | Node::RankRandom(..)
        | Node::BaseIfElse(..)
        | Node::SampleInt(..)
        | Node::RDist(..) => eval_fr8(node, inputs, n, h),
    }
}

/// FR8's nodes, out of line: `eval()` is the hot recursion of every compiled
/// rule, and its frame and inlining stay as they were without them.
#[inline(never)]
fn eval_fr8<'a>(node: &Node, inputs: &[Vals<'a>], n: usize, h: &mut dyn Host) -> R<Vals<'a>> {
    match node {
        Node::If(c, t, f) => {
            if if_cond(&eval(c, inputs, n, h)?)? {
                eval(t, inputs, n, h)
            } else {
                eval(f, inputs, n, h)
            }
        }
        Node::Draw1(norm, a, b) => {
            let a = scalar_dbl(eval(a, inputs, n, h)?, h)?;
            let b = scalar_dbl(eval(b, inputs, n, h)?, h)?;
            let v = h.rand2(*norm, a, b, 1)?;
            if v.iter().any(|x| x.is_nan()) {
                return Err(Decline("runif()/rnorm() made NaN (R warns)"));
            }
            Ok(Vals::Dbl(Cow::Owned(v)))
        }
        Node::Cast(to_int, x) => cast_num(*to_int, eval(x, inputs, n, h)?, h),
        Node::Abs(x) => Ok(match eval(x, inputs, n, h)? {
            // NA_integer_ is i32::MIN, so no integer overflows
            Vals::Int(v) | Vals::Lgl(v) => Vals::Int(Cow::Owned(
                v.iter()
                    .map(|&e| if e == NA_INT { e } else { e.abs() })
                    .collect(),
            )),
            Vals::Dbl(v) => Vals::Dbl(Cow::Owned(v.iter().map(|e| e.abs()).collect())),
            Vals::Str(_) => return Err(Decline("abs() of strings")),
        }),
        Node::Extreme(want_max, args) => {
            let mut vals = Vec::with_capacity(args.len());
            let mut is_dbl = false;
            for a in args {
                let v = eval(a, inputs, n, h)?;
                match &v {
                    Vals::Str(_) => return Err(Decline("min()/max() of strings")),
                    Vals::Dbl(_) => is_dbl = true,
                    Vals::Int(_) | Vals::Lgl(_) => {}
                }
                vals.push(v);
            }
            if is_dbl {
                let na = h.na_real();
                let na_low = na.to_bits() & 0xFFFF_FFFF;
                let mut buf = Vec::new();
                for v in &vals {
                    match v {
                        Vals::Dbl(x) => buf.extend_from_slice(x),
                        Vals::Int(x) | Vals::Lgl(x) => {
                            buf.extend(
                                x.iter()
                                    .map(|&e| if e == NA_INT { na } else { f64::from(e) }),
                            );
                        }
                        Vals::Str(_) => unreachable!(),
                    }
                }
                let Some(out) = crate::reduce::extreme_f64(&buf, *want_max, false, |v| {
                    v.is_nan() && (v.to_bits() & 0xFFFF_FFFF) == na_low
                }) else {
                    return Err(Decline("min()/max() of empty inputs (R warns)"));
                };
                Ok(Vals::Dbl(Cow::Owned(vec![out])))
            } else {
                let mut buf = Vec::new();
                for v in &vals {
                    match v {
                        Vals::Int(x) | Vals::Lgl(x) => buf.extend_from_slice(x),
                        _ => unreachable!(),
                    }
                }
                let Some(out) = crate::reduce::extreme_int(&buf, *want_max, false) else {
                    return Err(Decline("min()/max() of empty inputs (R warns)"));
                };
                Ok(Vals::Int(Cow::Owned(vec![out])))
            }
        }
        Node::AnyAll(want_any, na_rm, x) => {
            let Vals::Lgl(v) = eval(x, inputs, n, h)? else {
                return Err(Decline("any()/all() of non-logical (R warns)"));
            };
            Ok(Vals::Lgl(Cow::Owned(vec![crate::reduce::any_all(
                &v, *want_any, *na_rm,
            )])))
        }
        Node::Between(x, lo, hi) => {
            let x = eval(x, inputs, n, h)?;
            let lo = eval(lo, inputs, n, h)?;
            let hi = eval(hi, inputs, n, h)?;
            if x.rank() == 3 || lo.rank() == 3 || hi.rank() == 3 {
                return Err(Decline("between() of strings"));
            }
            let m = common_len(&[x.len(), lo.len(), hi.len()])?;
            let ge = compare(Bin::Ge, x.clone(), lo, m, h)?;
            let le = compare(Bin::Le, x, hi, m, h)?;
            binary(Bin::And, ge, le, h)
        }
        Node::SeqLen(k) => {
            let k = whole(&eval(k, inputs, n, h)?, 0.0)?;
            Ok(Vals::Int(Cow::Owned((1..=k as i32).collect())))
        }
        Node::RankRandom(x) => {
            let x = eval(x, inputs, n, h)?;
            rank_random(x, h)
        }
        Node::SeqAlong(x) => {
            let len = eval(x, inputs, n, h)?.len();
            if len > i32::MAX as usize {
                return Err(Decline("seq_along() length overflows i32"));
            }
            Ok(Vals::Int(Cow::Owned((1..=len as i32).collect())))
        }
        Node::BaseIfElse(test, yes, no) => {
            let Vals::Lgl(cv) = eval(test, inputs, n, h)? else {
                return Err(Decline("ifelse() test is not logical"));
            };
            let m = cv.len();
            if m == 0 {
                return Ok(Vals::Lgl(Cow::Owned(Vec::new())));
            }
            let has_t = cv.contains(&1);
            let has_f = cv.contains(&0);
            let tv = if has_t {
                Some(eval(yes, inputs, n, h)?)
            } else {
                None
            };
            let fv = if has_f {
                Some(eval(no, inputs, n, h)?)
            } else {
                None
            };
            for v in tv.iter().chain(fv.iter()) {
                if v.len() != 1 && v.len() != m {
                    return Err(Decline("ifelse() branch of non-matching length"));
                }
            }
            match (tv, fv) {
                (None, None) => Ok(Vals::Lgl(Cow::Owned(vec![NA_INT; m]))),
                (Some(t), None) => Ok(match t {
                    Vals::Lgl(v) => Vals::Lgl(Cow::Owned(pick2(&cv, &v, &[], NA_INT))),
                    Vals::Int(v) => Vals::Int(Cow::Owned(pick2(&cv, &v, &[], NA_INT))),
                    Vals::Dbl(v) => Vals::Dbl(Cow::Owned(pick2(&cv, &v, &[], h.na_real()))),
                    Vals::Str(v) => Vals::Str(Cow::Owned(pick2(&cv, &v, &[], h.na_str()))),
                }),
                (None, Some(f)) => Ok(match f {
                    Vals::Lgl(v) => Vals::Lgl(Cow::Owned(pick2(&cv, &[], &v, NA_INT))),
                    Vals::Int(v) => Vals::Int(Cow::Owned(pick2(&cv, &[], &v, NA_INT))),
                    Vals::Dbl(v) => Vals::Dbl(Cow::Owned(pick2(&cv, &[], &v, h.na_real()))),
                    Vals::Str(v) => Vals::Str(Cow::Owned(pick2(&cv, &[], &v, h.na_str()))),
                }),
                (Some(t), Some(f)) => {
                    let to = common_type(&[&t, &f])?;
                    let tf = [cast(t, to, h)?, cast(f, to, h)?];
                    Ok(match to {
                        0 => {
                            let v = typed!(tf, Lgl);
                            Vals::Lgl(Cow::Owned(pick2(&cv, v[0], v[1], NA_INT)))
                        }
                        1 => {
                            let v = typed!(tf, Int);
                            Vals::Int(Cow::Owned(pick2(&cv, v[0], v[1], NA_INT)))
                        }
                        2 => {
                            let v = typed!(tf, Dbl);
                            Vals::Dbl(Cow::Owned(pick2(&cv, v[0], v[1], h.na_real())))
                        }
                        _ => {
                            let v = typed!(tf, Str);
                            Vals::Str(Cow::Owned(pick2(&cv, v[0], v[1], h.na_str())))
                        }
                    })
                }
            }
        }
        Node::SampleInt(nv, size, replace, prob) => {
            let nv = whole(&eval(nv, inputs, n, h)?, 0.0)?;
            let sz = match size {
                Some(s) => whole(&eval(s, inputs, n, h)?, 0.0)?,
                None => nv,
            };
            if let Some(p) = prob {
                let pr = prob_vec(&eval(p, inputs, n, h)?)?;
                return Ok(Vals::Int(Cow::Owned(
                    h.sample_int_prob(nv, sz, *replace, &pr)?,
                )));
            }
            if !*replace && sz > nv {
                return Err(Decline("sample.int() size > n without replace"));
            }
            Ok(Vals::Int(Cow::Owned(h.sample_int(nv, sz, *replace)?)))
        }
        Node::RDist(kind, one, a, b) => {
            let a = scalar_dbl(eval(a, inputs, n, h)?, h)?;
            let b = match b {
                Some(bn) => scalar_dbl(eval(bn, inputs, n, h)?, h)?,
                None => 0.0,
            };
            h.rdist(*kind, a, b, if *one { 1 } else { n })
        }
        _ => Err(Decline("not an FR8 node")),
    }
}

/// The condition of base `if`: one logical or number, not `NA` (R errs on
/// anything else, or reads strings such as `"TRUE"`: both left to R).
fn if_cond(c: &Vals) -> R<bool> {
    let bad = Err(Decline("an if() condition that is not one TRUE or FALSE"));
    if c.len() != 1 {
        return bad;
    }
    match c {
        Vals::Lgl(x) | Vals::Int(x) if x[0] != NA_INT => Ok(x[0] != 0),
        Vals::Dbl(x) if !x[0].is_nan() => Ok(x[0] != 0.0),
        _ => bad,
    }
}

/// `as.integer(x)` / `as.numeric(x)` of logicals, integers or doubles, as R
/// coerces them (`NaN` and `NA` become `NA`; a double outside the integer range,
/// where R warns, declines); strings decline.
fn cast_num<'a>(to_int: bool, x: Vals<'a>, h: &dyn Host) -> R<Vals<'a>> {
    match (to_int, x) {
        (false, Vals::Dbl(v)) => Ok(Vals::Dbl(v)),
        (false, Vals::Int(v) | Vals::Lgl(v)) => {
            Ok(Vals::Dbl(Cow::Owned(int_to_dbl(&v, h.na_real()))))
        }
        (true, Vals::Int(v) | Vals::Lgl(v)) => Ok(Vals::Int(v)),
        (true, Vals::Dbl(v)) => {
            let mut out = Vec::with_capacity(v.len());
            for &d in v.iter() {
                if d.is_nan() {
                    out.push(NA_INT);
                    continue;
                }
                let t = d.trunc();
                if !(-2147483647.0..=2147483647.0).contains(&t) {
                    return Err(Decline("as.integer() out of range (R warns)"));
                }
                out.push(t as i32);
            }
            Ok(Vals::Int(Cow::Owned(out)))
        }
        (_, Vals::Str(_)) => Err(Decline("as.integer()/as.numeric() of strings")),
    }
}

/// The engine's `assign_rule()` for some rows: `value` (length 1 or `rows.len()`)
/// written into the rows `rows` of column `old`, both cast to their vctrs common
/// type first (`vec_cast_common()`); a new column (`old` `None`) is `NA` of the
/// value's type elsewhere (`vec_init()`).
pub fn assign_rows(
    old: Option<Vals>,
    value: Vals,
    rows: &[bool],
    h: &dyn Host,
) -> R<Vals<'static>> {
    let n = rows.len();
    if value.len() != 1 && value.len() != n {
        return Err(Decline("a value of the wrong length"));
    }
    let old = match old {
        Some(o) if o.len() == n => o,
        Some(_) => return Err(Decline("a column of the wrong length")),
        None => {
            // vec_init(value, n): NA of the value's own type
            let k = match value {
                Vals::Lgl(_) => Konst::Lgl(NA_INT),
                Vals::Int(_) => Konst::Int(NA_INT),
                Vals::Dbl(_) => Konst::Dbl(h.na_real()),
                Vals::Str(_) => Konst::Str(h.na_str()),
            };
            match k {
                Konst::Lgl(x) => Vals::Lgl(Cow::Owned(vec![x; n])),
                Konst::Int(x) => Vals::Int(Cow::Owned(vec![x; n])),
                Konst::Dbl(x) => Vals::Dbl(Cow::Owned(vec![x; n])),
                Konst::Str(x) => Vals::Str(Cow::Owned(vec![x; n])),
            }
        }
    };
    let to = common_type(&[&old, &value])?;
    let old = cast(old, to, h)?;
    let value = cast(value, to, h)?;
    fn put<T: Copy>(o: &[T], v: &[T], rows: &[bool]) -> Vec<T> {
        o.iter()
            .enumerate()
            .map(|(i, &x)| if rows[i] { at(v, i) } else { x })
            .collect()
    }
    Ok(match (&old, &value) {
        (Vals::Lgl(o), Vals::Lgl(v)) => Vals::Lgl(Cow::Owned(put(o, v, rows))),
        (Vals::Int(o), Vals::Int(v)) => Vals::Int(Cow::Owned(put(o, v, rows))),
        (Vals::Dbl(o), Vals::Dbl(v)) => Vals::Dbl(Cow::Owned(put(o, v, rows))),
        (Vals::Str(o), Vals::Str(v)) => Vals::Str(Cow::Owned(put(o, v, rows))),
        _ => return Err(Decline("values of incompatible types")),
    })
}

/// The rows of each value of a `.by` key, in the order `group_by()` and
/// `vec_order()` visit them: ascending, `NA` last (logicals `FALSE`, `TRUE`,
/// `NA`), each group's rows in row order; `-0` and `0` one value. `None` for a
/// key the engine does not group natively (strings, a double `NaN`/`NA`).
pub fn group_rows(key: &Vals) -> Option<Vec<Vec<usize>>> {
    use std::collections::HashMap;
    match key {
        Vals::Int(v) | Vals::Lgl(v) => {
            let mut at: HashMap<i32, usize> = HashMap::new();
            let mut out: Vec<(i32, Vec<usize>)> = Vec::new();
            for (i, &x) in v.iter().enumerate() {
                let g = *at.entry(x).or_insert_with(|| {
                    out.push((x, Vec::new()));
                    out.len() - 1
                });
                out[g].1.push(i);
            }
            out.sort_by_key(|(x, _)| (*x == NA_INT, *x));
            Some(out.into_iter().map(|(_, r)| r).collect())
        }
        Vals::Dbl(v) => {
            if v.iter().any(|x| x.is_nan()) {
                return None;
            }
            let mut at: HashMap<u64, usize> = HashMap::new();
            let mut out: Vec<(f64, Vec<usize>)> = Vec::new();
            for (i, &x) in v.iter().enumerate() {
                let bits = if x == 0.0 { 0 } else { x.to_bits() };
                let g = *at.entry(bits).or_insert_with(|| {
                    out.push((x, Vec::new()));
                    out.len() - 1
                });
                out[g].1.push(i);
            }
            out.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap_or(std::cmp::Ordering::Equal));
            Some(out.into_iter().map(|(_, r)| r).collect())
        }
        Vals::Str(_) => None,
    }
}

/// `v`'s elements at `rows`.
pub fn take<'a>(v: &Vals, rows: &[usize]) -> Vals<'a> {
    match v {
        Vals::Lgl(x) => Vals::Lgl(Cow::Owned(rows.iter().map(|&i| at(x, i)).collect())),
        Vals::Int(x) => Vals::Int(Cow::Owned(rows.iter().map(|&i| at(x, i)).collect())),
        Vals::Dbl(x) => Vals::Dbl(Cow::Owned(rows.iter().map(|&i| at(x, i)).collect())),
        Vals::Str(x) => Vals::Str(Cow::Owned(rows.iter().map(|&i| at(x, i)).collect())),
    }
}

/// Per-group values (each of length 1 or its group's size) put back at their
/// rows of an `n`-row result, in vctrs' common type of all of them
/// (`list_unchop(indices =)`).
pub fn unchop(parts: Vec<Vals>, groups: &[Vec<usize>], n: usize, h: &dyn Host) -> R<Vals<'static>> {
    let to = common_type(&parts.iter().collect::<Vec<_>>())?;
    let mut cast_parts = Vec::with_capacity(parts.len());
    for p in parts {
        cast_parts.push(cast(p, to, h)?);
    }
    fn fill<T: Copy>(parts: &[&[T]], groups: &[Vec<usize>], n: usize, init: T) -> Vec<T> {
        let mut out = vec![init; n];
        for (p, rows) in parts.iter().zip(groups) {
            for (j, &i) in rows.iter().enumerate() {
                out[i] = at(p, j);
            }
        }
        out
    }
    Ok(match to {
        0 => Vals::Lgl(Cow::Owned(fill(
            &typed!(cast_parts, Lgl),
            groups,
            n,
            NA_INT,
        ))),
        1 => Vals::Int(Cow::Owned(fill(
            &typed!(cast_parts, Int),
            groups,
            n,
            NA_INT,
        ))),
        2 => Vals::Dbl(Cow::Owned(fill(
            &typed!(cast_parts, Dbl),
            groups,
            n,
            h.na_real(),
        ))),
        _ => Vals::Str(Cow::Owned(fill(
            &typed!(cast_parts, Str),
            groups,
            n,
            h.na_str(),
        ))),
    })
}

/// `value[!keep] <- old[!keep]` (base R's `[<-`: the higher of the two types;
/// strings with numbers decline); `old` `None` writes `NA`.
pub fn keep_old<'a>(
    value: Vals<'a>,
    old: Option<&Vals>,
    keep: &[bool],
    h: &dyn Host,
) -> R<Vals<'a>> {
    let to = match old {
        Some(o) => {
            if (o.rank() == 3) != (value.rank() == 3) {
                return Err(Decline("strings replaced by numbers"));
            }
            o.rank().max(value.rank())
        }
        None => value.rank(),
    };
    let v = cast(value, to, h)?;
    let o = match old {
        Some(o) => Some(cast(o.clone(), to, h)?),
        None => None,
    };
    fn put<T: Copy>(v: &[T], o: Option<&[T]>, keep: &[bool], na: T) -> Vec<T> {
        (0..keep.len())
            .map(|i| {
                if keep[i] {
                    at(v, i)
                } else {
                    o.map_or(na, |o| at(o, i))
                }
            })
            .collect()
    }
    Ok(match (&v, &o) {
        (Vals::Lgl(x), Some(Vals::Lgl(y))) => Vals::Lgl(Cow::Owned(put(x, Some(y), keep, NA_INT))),
        (Vals::Lgl(x), None) => Vals::Lgl(Cow::Owned(put(x, None, keep, NA_INT))),
        (Vals::Int(x), Some(Vals::Int(y))) => Vals::Int(Cow::Owned(put(x, Some(y), keep, NA_INT))),
        (Vals::Int(x), None) => Vals::Int(Cow::Owned(put(x, None, keep, NA_INT))),
        (Vals::Dbl(x), Some(Vals::Dbl(y))) => {
            Vals::Dbl(Cow::Owned(put(x, Some(y), keep, h.na_real())))
        }
        (Vals::Dbl(x), None) => Vals::Dbl(Cow::Owned(put(x, None, keep, h.na_real()))),
        (Vals::Str(x), Some(Vals::Str(y))) => {
            Vals::Str(Cow::Owned(put(x, Some(y), keep, h.na_str())))
        }
        (Vals::Str(x), None) => Vals::Str(Cow::Owned(put(x, None, keep, h.na_str()))),
        _ => return Err(Decline("values of incompatible types")),
    })
}

/// A length-1 number as a double.
fn scalar_dbl(v: Vals, h: &dyn Host) -> R<f64> {
    if v.len() != 1 {
        return Err(Decline("a parameter that is not one value"));
    }
    Ok(as_dbl(v, h.na_real())?[0])
}

/// A length-1 whole number `>= lo` (not `NA`) as a `usize`.
fn whole(v: &Vals, lo: f64) -> R<usize> {
    let x = match v {
        Vals::Int(x) if x.len() == 1 && x[0] != NA_INT => f64::from(x[0]),
        Vals::Dbl(x) if x.len() == 1 => x[0],
        _ => return Err(Decline("a size that is not one whole number")),
    };
    if !(x.is_finite() && x >= lo && x.fract() == 0.0 && x <= i32::MAX as f64) {
        return Err(Decline("a size that is not one whole number"));
    }
    Ok(x as usize)
}

fn reduce_na_rm<'a>(r: Red, x: Vals<'a>, h: &dyn Host) -> R<Vals<'a>> {
    Ok(match x {
        Vals::Dbl(v) => {
            let kept: Vec<f64> = v.iter().copied().filter(|x| !x.is_nan()).collect();
            Vals::Dbl(Cow::Owned(vec![match r {
                Red::Sum => h.sum(&kept),
                Red::Mean => h.mean(&kept),
            }]))
        }
        Vals::Int(v) | Vals::Lgl(v) => match r {
            Red::Sum => match crate::reduce::sum_int(&v, true) {
                Some(s) => Vals::Int(Cow::Owned(vec![s])),
                None => return Err(Decline("integer sum overflows")),
            },
            Red::Mean => {
                let kept: Vec<i32> = v.iter().copied().filter(|&x| x != NA_INT).collect();
                Vals::Dbl(Cow::Owned(vec![h.mean_int(&kept)]))
            }
        },
        Vals::Str(_) => return Err(Decline("sum()/mean() of strings")),
    })
}

/// Is element `i` of `v` missing as vctrs sees it (`NA`, and `NaN` for doubles)?
fn missing(v: &Vals, i: usize, na_str: usize) -> bool {
    match v {
        Vals::Dbl(x) => at(x, i).is_nan(),
        Vals::Int(x) | Vals::Lgl(x) => at(x, i) == NA_INT,
        Vals::Str(x) => at(x, i) == na_str,
    }
}

/// dplyr's `coalesce()`: the common type and size of the arguments, each
/// missing element filled from the next argument.
fn coalesce<'a>(vals: Vec<Vals<'a>>, h: &dyn Host) -> R<Vals<'a>> {
    if vals.is_empty() || vals.iter().any(Vals::is_empty) {
        return Err(Decline("coalesce() of nothing"));
    }
    let n = size_common(&vals.iter().map(Vals::len).collect::<Vec<_>>())?;
    let to = common_type(&vals.iter().collect::<Vec<_>>())?;
    let mut cast_vals = Vec::with_capacity(vals.len());
    for v in vals {
        cast_vals.push(cast(v, to, h)?);
    }
    let na_str = h.na_str();
    // the row's value: the first argument not missing there, else the last's
    let pick_row = |i: usize| {
        cast_vals
            .iter()
            .position(|v| !missing(v, i, na_str))
            .unwrap_or(cast_vals.len() - 1)
    };
    let rows: Vec<usize> = (0..n).map(pick_row).collect();
    Ok(match to {
        0 => {
            let v = typed!(cast_vals, Lgl);
            Vals::Lgl(Cow::Owned((0..n).map(|i| at(v[rows[i]], i)).collect()))
        }
        1 => {
            let v = typed!(cast_vals, Int);
            Vals::Int(Cow::Owned((0..n).map(|i| at(v[rows[i]], i)).collect()))
        }
        2 => {
            let v = typed!(cast_vals, Dbl);
            Vals::Dbl(Cow::Owned((0..n).map(|i| at(v[rows[i]], i)).collect()))
        }
        _ => {
            let v = typed!(cast_vals, Str);
            Vals::Str(Cow::Owned((0..n).map(|i| at(v[rows[i]], i)).collect()))
        }
    })
}

/// `match(x, table, nomatch = 0) > 0` for atomic `x` and `table`.
fn is_in<'a>(x: &Vals, t: &Vals, h: &dyn Host) -> R<Vals<'a>> {
    let out: Vec<i32> = match (x, t) {
        (Vals::Str(x), Vals::Str(t)) => {
            let ascii = |s: &usize| *s == h.na_str() || h.str_ascii(*s);
            if !x.iter().all(ascii) || !t.iter().all(ascii) {
                return Err(Decline("%in% of non-ASCII strings"));
            }
            x.iter().map(|s| t.contains(s) as i32).collect()
        }
        (Vals::Str(_), _) | (_, Vals::Str(_)) => {
            return Err(Decline("%in% of strings and numbers"));
        }
        (Vals::Int(x) | Vals::Lgl(x), Vals::Int(t) | Vals::Lgl(t)) => {
            x.iter().map(|v| t.contains(v) as i32).collect()
        }
        _ => {
            let na = h.na_real();
            let x = as_dbl(x.clone(), na)?;
            let t = as_dbl(t.clone(), na)?;
            // R tells NA from NaN in match(): left to R
            if x.iter().chain(t.iter()).any(|v| v.is_nan()) {
                return Err(Decline("%in% meets NA or NaN doubles"));
            }
            x.iter().map(|v| t.contains(v) as i32).collect()
        }
    };
    Ok(Vals::Lgl(Cow::Owned(out)))
}

/// Coerce a `prob` vector (double, integer, or logical) to `Vec<f64>` (`NA_INT` -> `NaN`).
fn prob_vec(v: &Vals) -> R<Vec<f64>> {
    match v {
        Vals::Dbl(x) => Ok(x.to_vec()),
        Vals::Int(x) | Vals::Lgl(x) => Ok(x
            .iter()
            .map(|&k| if k == NA_INT { f64::NAN } else { f64::from(k) })
            .collect()),
        Vals::Str(_) => Err(Decline("sample() prob of strings")),
    }
}

/// `sample(x, size, replace, prob)`: `sample.int(x, ...)` for one whole number
/// `x >= 1`, else `x[sample.int(length(x), ...)]`.
fn sample<'a>(
    x: Vals<'a>,
    size: Option<usize>,
    replace: bool,
    prob: Option<&[f64]>,
    h: &mut dyn Host,
) -> R<Vals<'a>> {
    let one_num = x.len() == 1 && matches!(x, Vals::Int(_) | Vals::Dbl(_));
    if one_num {
        let v = match &x {
            Vals::Int(v) if v[0] != NA_INT => Some(f64::from(v[0])),
            Vals::Dbl(v) => Some(v[0]),
            _ => None,
        };
        if let Some(v) = v.filter(|v| v.is_finite() && *v >= 1.0) {
            if v.fract() != 0.0 || v > i32::MAX as f64 {
                return Err(Decline("sample() of a number that is not whole"));
            }
            let nv = v as usize;
            let sz = size.unwrap_or(nv);
            let idx = match prob {
                Some(p) => h.sample_int_prob(nv, sz, replace, p)?,
                None if replace => h.unif_index(v, sz)?,
                None => h.sample_int(nv, sz, false)?,
            };
            return Ok(Vals::Int(Cow::Owned(idx)));
        }
    }
    if x.is_empty() || x.len() > i32::MAX as usize {
        return Err(Decline("sample() of an empty vector"));
    }
    let m = x.len();
    let sz = size.unwrap_or(m);
    let idx = match prob {
        Some(p) => h.sample_int_prob(m, sz, replace, p)?,
        None if replace => h.unif_index(m as f64, sz)?,
        None => h.sample_int(m, sz, false)?,
    };
    let ix = Vals::Int(Cow::Owned(idx));
    index(x, &ix, h)
}

/// The elements of `v` where `c` is `TRUE`, and `na` where it is `NA`.
fn keep_where<T: Copy>(v: &[T], c: &[i32], na: T) -> Vec<T> {
    let mut out = Vec::with_capacity(c.iter().filter(|&&k| k != 0).count());
    out.extend(
        v.iter()
            .zip(c)
            .filter(|&(_, &k)| k != 0)
            .map(|(&x, &k)| if k == NA_INT { na } else { x }),
    );
    out
}

/// R's `sort.list(order(x, runif(length(x))))`: ties broken by one draw each.
fn rank_random<'a>(x: Vals<'a>, h: &mut dyn Host) -> R<Vals<'a>> {
    let key: Vec<f64> = match &x {
        Vals::Dbl(v) if !v.iter().any(|x| x.is_nan()) => v.to_vec(),
        Vals::Int(v) | Vals::Lgl(v) if !v.contains(&NA_INT) => {
            v.iter().map(|&x| f64::from(x)).collect()
        }
        _ => return Err(Decline("rank() of strings or missing values")),
    };
    let u = h.runif(key.len())?;
    let mut order: Vec<usize> = (0..key.len()).collect();
    // `partial_cmp`, so that -0 and 0 tie as they do in R
    order.sort_by(|&a, &b| {
        let by_key = key[a].partial_cmp(&key[b]);
        by_key
            .unwrap_or(std::cmp::Ordering::Equal)
            .then_with(|| u[a].total_cmp(&u[b]))
    });
    let mut rank = vec![0; key.len()];
    for (r, &i) in order.iter().enumerate() {
        rank[i] = r as i32 + 1;
    }
    Ok(Vals::Int(Cow::Owned(rank)))
}

/// `x[i]` for positive whole indices (`NA` or past the end gives `NA`) or a
/// logical `i`, recycled over `x` (`NA`, or `TRUE` past the end, gives `NA`).
fn index<'a>(x: Vals<'a>, i: &Vals, h: &dyn Host) -> R<Vals<'a>> {
    let m = x.len();
    // `x[cond]`, one condition per element: kept without a list of positions
    if let Vals::Lgl(c) = i {
        if c.len() == m {
            return Ok(match x {
                Vals::Dbl(v) => Vals::Dbl(Cow::Owned(keep_where(&v, c, h.na_real()))),
                Vals::Int(v) => Vals::Int(Cow::Owned(keep_where(&v, c, NA_INT))),
                Vals::Lgl(v) => Vals::Lgl(Cow::Owned(keep_where(&v, c, NA_INT))),
                Vals::Str(v) => Vals::Str(Cow::Owned(keep_where(&v, c, h.na_str()))),
            });
        }
    }
    let pos: Vec<Option<usize>> = match i {
        Vals::Int(v) => v
            .iter()
            .map(|&k| match k {
                NA_INT => Ok(None),
                k if k >= 1 => Ok(Some(k as usize - 1).filter(|&j| j < m)),
                _ => Err(Decline("an index that is not positive")),
            })
            .collect::<R<_>>()?,
        Vals::Dbl(v) => v
            .iter()
            .map(|&k| {
                if !(k.is_finite() && k >= 1.0) {
                    return Err(Decline("an index that is not a positive number"));
                }
                Ok(Some(k.trunc() as usize - 1).filter(|&j| j < m))
            })
            .collect::<R<_>>()?,
        Vals::Lgl(v) => {
            let len = if v.is_empty() { 0 } else { m.max(v.len()) };
            (0..len)
                .filter_map(|j| match v[j % v.len()] {
                    0 => None,
                    NA_INT => Some(None),
                    _ => Some(Some(j).filter(|&j| j < m)),
                })
                .collect()
        }
        _ => return Err(Decline("an index that is not a number")),
    };
    Ok(match x {
        Vals::Dbl(v) => {
            let na = h.na_real();
            Vals::Dbl(Cow::Owned(
                pos.iter().map(|p| p.map_or(na, |j| v[j])).collect(),
            ))
        }
        Vals::Int(v) => Vals::Int(Cow::Owned(
            pos.iter().map(|p| p.map_or(NA_INT, |j| v[j])).collect(),
        )),
        Vals::Lgl(v) => Vals::Lgl(Cow::Owned(
            pos.iter().map(|p| p.map_or(NA_INT, |j| v[j])).collect(),
        )),
        Vals::Str(v) => {
            let na = h.na_str();
            Vals::Str(Cow::Owned(
                pos.iter().map(|p| p.map_or(na, |j| v[j])).collect(),
            ))
        }
    })
}

fn unary<'a>(op: Un, x: Vals<'a>, h: &dyn Host) -> R<Vals<'a>> {
    Ok(match (op, x) {
        (Un::Neg, Vals::Dbl(v)) => Vals::Dbl(Cow::Owned(v.iter().map(|&x| -x).collect())),
        (Un::Neg, Vals::Int(v) | Vals::Lgl(v)) => Vals::Int(Cow::Owned(
            v.iter()
                .map(|&x| if x == NA_INT { NA_INT } else { -x })
                .collect(),
        )),
        (Un::Not, Vals::Lgl(v)) => Vals::Lgl(Cow::Owned(
            v.iter()
                .map(|&x| if x == NA_INT { NA_INT } else { (x == 0) as i32 })
                .collect(),
        )),
        (Un::IsNa, v) => {
            let out: Vec<i32> = match &v {
                Vals::Dbl(d) => d.iter().map(|x| x.is_nan() as i32).collect(),
                Vals::Int(i) | Vals::Lgl(i) => i.iter().map(|&x| (x == NA_INT) as i32).collect(),
                Vals::Str(s) => {
                    let na = h.na_str();
                    s.iter().map(|&x| (x == na) as i32).collect()
                }
            };
            Vals::Lgl(Cow::Owned(out))
        }
        (Un::IsFinite, v) => {
            let out: Vec<i32> = match &v {
                Vals::Dbl(d) => d.iter().map(|x| x.is_finite() as i32).collect(),
                Vals::Int(i) | Vals::Lgl(i) => i.iter().map(|&x| (x != NA_INT) as i32).collect(),
                Vals::Str(s) => vec![0; s.len()],
            };
            Vals::Lgl(Cow::Owned(out))
        }
        (Un::IsInfinite, v) => {
            let out: Vec<i32> = match &v {
                Vals::Dbl(d) => d.iter().map(|x| x.is_infinite() as i32).collect(),
                Vals::Int(i) | Vals::Lgl(i) => vec![0; i.len()],
                Vals::Str(s) => vec![0; s.len()],
            };
            Vals::Lgl(Cow::Owned(out))
        }
        (Un::IsNaN, v) => {
            let out: Vec<i32> = match &v {
                Vals::Dbl(d) => {
                    let na_low = h.na_real().to_bits() & 0xFFFF_FFFF;
                    d.iter()
                        .map(|x| (x.is_nan() && (x.to_bits() & 0xFFFF_FFFF) != na_low) as i32)
                        .collect()
                }
                Vals::Int(i) | Vals::Lgl(i) => vec![0; i.len()],
                Vals::Str(s) => vec![0; s.len()],
            };
            Vals::Lgl(Cow::Owned(out))
        }
        _ => return Err(Decline("unary operator on an unsupported type")),
    })
}

fn binary<'a>(op: Bin, a: Vals<'a>, b: Vals<'a>, h: &dyn Host) -> R<Vals<'a>> {
    // an empty operand (an emptied population) gives an empty result, as in R
    let n = if a.is_empty() || b.is_empty() {
        0
    } else {
        common_len(&[a.len(), b.len()])?
    };
    match op {
        Bin::And | Bin::Or => {
            let (Vals::Lgl(x), Vals::Lgl(y)) = (&a, &b) else {
                return Err(Decline("`&`/`|` of non-logical values"));
            };
            let out = if op == Bin::And {
                map2(x, y, |p, q| {
                    if p == 0 || q == 0 {
                        0
                    } else if p == NA_INT || q == NA_INT {
                        NA_INT
                    } else {
                        1
                    }
                })
            } else {
                map2(x, y, |p, q| {
                    if p == 1 || q == 1 {
                        1
                    } else if p == NA_INT || q == NA_INT {
                        NA_INT
                    } else {
                        0
                    }
                })
            };
            Ok(Vals::Lgl(Cow::Owned(out)))
        }
        Bin::Eq | Bin::Ne | Bin::Lt | Bin::Gt | Bin::Le | Bin::Ge => compare(op, a, b, n, h),
        Bin::Add | Bin::Sub | Bin::Mul if a.rank() <= 1 && b.rank() <= 1 => {
            let (Vals::Lgl(x) | Vals::Int(x), Vals::Lgl(y) | Vals::Int(y)) = (&a, &b) else {
                unreachable!()
            };
            let mut out = Vec::with_capacity(n);
            for i in 0..n {
                let (p, q) = (at(x, i), at(y, i));
                if p == NA_INT || q == NA_INT {
                    out.push(NA_INT);
                    continue;
                }
                let r = match op {
                    Bin::Add => p as i64 + q as i64,
                    Bin::Sub => p as i64 - q as i64,
                    _ => p as i64 * q as i64,
                };
                match i32::try_from(r) {
                    Ok(r) if r != NA_INT => out.push(r),
                    _ => return Err(Decline("integer overflow warns")),
                }
            }
            Ok(Vals::Int(Cow::Owned(out)))
        }
        Bin::Div if a.rank() <= 1 && b.rank() <= 1 => {
            // integer / integer: R gives its NA_real_ itself for an NA operand
            // (differential test), not the NaN the hardware makes of it
            let (Vals::Lgl(x) | Vals::Int(x), Vals::Lgl(y) | Vals::Int(y)) = (&a, &b) else {
                unreachable!()
            };
            let na = h.na_real();
            Ok(Vals::Dbl(Cow::Owned(
                (0..n)
                    .map(|i| {
                        let (p, q) = (at(x, i), at(y, i));
                        if p == NA_INT || q == NA_INT {
                            na
                        } else {
                            p as f64 / q as f64
                        }
                    })
                    .collect(),
            )))
        }
        Bin::Add | Bin::Sub | Bin::Mul | Bin::Div => {
            let na = h.na_real();
            let x = as_dbl(a, na)?;
            let y = as_dbl(b, na)?;
            let out = match op {
                Bin::Add => map2(&x, &y, |p, q| p + q),
                Bin::Sub => map2(&x, &y, |p, q| p - q),
                Bin::Mul => map2(&x, &y, |p, q| p * q),
                _ => map2(&x, &y, |p, q| p / q),
            };
            Ok(Vals::Dbl(Cow::Owned(out)))
        }
    }
}

fn compare<'a>(op: Bin, a: Vals<'a>, b: Vals<'a>, n: usize, h: &dyn Host) -> R<Vals<'a>> {
    if let Some(out) = compare_one(op, &a, &b, h) {
        return Ok(Vals::Lgl(Cow::Owned(out)));
    }
    let out: Vec<i32> = match (&a, &b) {
        (Vals::Str(x), Vals::Str(y)) => {
            if !matches!(op, Bin::Eq | Bin::Ne) {
                return Err(Decline("string ordering depends on the locale"));
            }
            let na = h.na_str();
            let mut out = Vec::with_capacity(n);
            // a column repeats a few strings: each is checked for ASCII once
            let mut seen: Vec<(usize, bool)> = Vec::new();
            let mut ascii = |s: usize| match seen.iter().find(|e| e.0 == s) {
                Some(e) => e.1,
                None => {
                    let a = h.str_ascii(s);
                    seen.push((s, a));
                    a
                }
            };
            for i in 0..n {
                let (p, q) = (at(x, i), at(y, i));
                let v = if p == na || q == na {
                    NA_INT
                } else if p == q {
                    (op == Bin::Eq) as i32
                } else if ascii(p) && ascii(q) {
                    (op == Bin::Ne) as i32
                } else {
                    return Err(Decline("comparison of non-ASCII strings"));
                };
                out.push(v);
            }
            out
        }
        (Vals::Str(_), _) | (_, Vals::Str(_)) => {
            return Err(Decline("string compared with a number"));
        }
        (Vals::Int(x) | Vals::Lgl(x), Vals::Int(y) | Vals::Lgl(y)) => {
            cmp_vec(op, x, y, |v: i32| v == NA_INT)
        }
        _ => {
            let na = h.na_real();
            let x = as_dbl(a, na)?;
            let y = as_dbl(b, na)?;
            cmp_vec(op, &x, &y, |v: f64| v.is_nan())
        }
    };
    Ok(Vals::Lgl(Cow::Owned(out)))
}

/// `==`/`!=` against one ASCII string (`.group == "firm"`), a pointer test per
/// element: R keeps one CHARSXP per ASCII string, and no other string equals it.
fn compare_one(op: Bin, a: &Vals, b: &Vals, h: &dyn Host) -> Option<Vec<i32>> {
    let (Vals::Str(x), Vals::Str(y)) = (a, b) else {
        return None;
    };
    let (many, s) = match (x.len(), y.len()) {
        (_, 1) => (x, y[0]),
        (1, _) => (y, x[0]),
        _ => return None,
    };
    let na = h.na_str();
    if !matches!(op, Bin::Eq | Bin::Ne) || s == na || !h.str_ascii(s) {
        return None;
    }
    let eq = op == Bin::Eq;
    Some(
        many.iter()
            .map(|&p| {
                if p == na {
                    NA_INT
                } else {
                    i32::from((p == s) == eq)
                }
            })
            .collect(),
    )
}

/// A comparison over operands of length 1 or `n`: `NA` where either is missing.
fn cmp_vec<T: PartialOrd + Copy>(op: Bin, x: &[T], y: &[T], na: impl Fn(T) -> bool) -> Vec<i32> {
    let g = |p: T, q: T, r: bool| if na(p) || na(q) { NA_INT } else { r as i32 };
    match op {
        Bin::Lt => map2(x, y, |p, q| g(p, q, p < q)),
        Bin::Gt => map2(x, y, |p, q| g(p, q, p > q)),
        Bin::Le => map2(x, y, |p, q| g(p, q, p <= q)),
        Bin::Ge => map2(x, y, |p, q| g(p, q, p >= q)),
        Bin::Eq => map2(x, y, |p, q| g(p, q, p == q)),
        _ => map2(x, y, |p, q| g(p, q, p != q)),
    }
}

/// `case_when()`'s pick per row: the first value whose condition is `TRUE`, else
/// the default, else `na`.
fn pick<T: Copy>(n: usize, conds: &[&[i32]], vals: &[&[T]], def: Option<&[T]>, na: T) -> Vec<T> {
    (0..n)
        .map(|i| {
            for (c, v) in conds.iter().zip(vals) {
                if at(c, i) == 1 {
                    return at(v, i);
                }
            }
            def.map_or(na, |d| at(d, i))
        })
        .collect()
}

/// `if_else()`'s pick per row.
fn pick2<T: Copy>(c: &[i32], t: &[T], f: &[T], na: T) -> Vec<T> {
    (0..c.len())
        .map(|i| match c[i] {
            NA_INT => na,
            0 => at(f, i),
            _ => at(t, i),
        })
        .collect()
}

fn if_else<'a>(c: Vals<'a>, t: Vals<'a>, f: Vals<'a>, h: &dyn Host) -> R<Vals<'a>> {
    let Vals::Lgl(cv) = &c else {
        return Err(Decline("if_else() condition is not logical"));
    };
    let n = cv.len();
    if [t.len(), f.len()].iter().any(|&l| l != 1 && l != n) {
        return Err(Decline("if_else() branch of the wrong size"));
    }
    let to = common_type(&[&t, &f])?;
    let tf = [cast(t, to, h)?, cast(f, to, h)?];
    Ok(match to {
        0 => {
            let v = typed!(tf, Lgl);
            Vals::Lgl(Cow::Owned(pick2(cv, v[0], v[1], NA_INT)))
        }
        1 => {
            let v = typed!(tf, Int);
            Vals::Int(Cow::Owned(pick2(cv, v[0], v[1], NA_INT)))
        }
        2 => {
            let v = typed!(tf, Dbl);
            Vals::Dbl(Cow::Owned(pick2(cv, v[0], v[1], h.na_real())))
        }
        _ => {
            let v = typed!(tf, Str);
            Vals::Str(Cow::Owned(pick2(cv, v[0], v[1], h.na_str())))
        }
    })
}

/// The common size of `lens` as `vec_size_common()` finds it (1 if all are 1).
fn size_common(lens: &[usize]) -> R<usize> {
    let mut s = 1usize;
    for &l in lens {
        if l != 1 {
            if s != 1 && s != l {
                return Err(Decline("case_when() inputs of different sizes"));
            }
            s = l;
        }
    }
    Ok(s)
}

fn case_when<'a>(
    conds: Vec<Vals<'a>>,
    vals: Vec<Vals<'a>>,
    def: Option<Vals<'a>>,
    h: &dyn Host,
) -> R<Vals<'a>> {
    if conds.iter().chain(vals.iter()).any(|v| v.is_empty()) {
        return Err(Decline("zero-length case_when() input"));
    }
    // dplyr 1.2's case_when_size_common(): conditions and values sized apart
    let sc = size_common(&conds.iter().map(Vals::len).collect::<Vec<_>>())?;
    let sv = size_common(&vals.iter().map(Vals::len).collect::<Vec<_>>())?;
    let n = match (sc, sv) {
        (1, 1) => 1,
        (1, _) => return Err(Decline("case_when() scalar condition, vector value warns")),
        (c, 1) => c,
        (c, v) if c == v => c,
        _ => return Err(Decline("case_when() sizes differ")),
    };
    if let Some(d) = &def {
        if d.len() != 1 && d.len() != n {
            return Err(Decline("case_when() .default of the wrong size"));
        }
    }
    let mut cs = Vec::with_capacity(conds.len());
    for c in &conds {
        match c {
            Vals::Lgl(v) => cs.push(v),
            _ => return Err(Decline("case_when() condition is not logical")),
        }
    }
    let mut all: Vec<&Vals> = vals.iter().collect();
    if let Some(d) = &def {
        all.push(d);
    }
    let to = common_type(&all)?;
    let mut vs = Vec::with_capacity(vals.len());
    for v in vals {
        vs.push(cast(v, to, h)?);
    }
    let def = match def {
        Some(d) => Some(cast(d, to, h)?),
        None => None,
    };
    // an NA condition is no match
    let cs: Vec<&[i32]> = cs.iter().map(|c| &c[..]).collect();
    let def: Vec<Vals> = def.into_iter().collect();
    Ok(match to {
        0 => Vals::Lgl(Cow::Owned(pick(
            n,
            &cs,
            &typed!(vs, Lgl),
            typed!(def, Lgl).first().copied(),
            NA_INT,
        ))),
        1 => Vals::Int(Cow::Owned(pick(
            n,
            &cs,
            &typed!(vs, Int),
            typed!(def, Int).first().copied(),
            NA_INT,
        ))),
        2 => Vals::Dbl(Cow::Owned(pick(
            n,
            &cs,
            &typed!(vs, Dbl),
            typed!(def, Dbl).first().copied(),
            h.na_real(),
        ))),
        _ => Vals::Str(Cow::Owned(pick(
            n,
            &cs,
            &typed!(vs, Str),
            typed!(def, Str).first().copied(),
            h.na_str(),
        ))),
    })
}

fn pminmax<'a>(max: bool, vals: Vec<Vals<'a>>, h: &dyn Host) -> R<Vals<'a>> {
    if vals.len() < 2 {
        return Err(Decline("pmin()/pmax() with one argument"));
    }
    let n = common_len(&vals.iter().map(Vals::len).collect::<Vec<_>>())?;
    let to = vals.iter().map(Vals::rank).max().unwrap_or(0);
    if to == 3 {
        return Err(Decline("pmin()/pmax() of strings"));
    }
    if to <= 1 {
        let ints: Vec<&[i32]> = vals
            .iter()
            .map(|v| match v {
                Vals::Lgl(x) | Vals::Int(x) => &x[..],
                _ => unreachable!(),
            })
            .collect();
        let out = (0..n)
            .map(|i| {
                let mut r = at(ints[0], i);
                for x in &ints[1..] {
                    let q = at(x, i);
                    if r == NA_INT || q == NA_INT {
                        return NA_INT;
                    }
                    if (max && q > r) || (!max && q < r) {
                        r = q;
                    }
                }
                r
            })
            .collect();
        return Ok(Vals::Int(Cow::Owned(out)));
    }
    let na = h.na_real();
    let mut ds = Vec::with_capacity(vals.len());
    for v in vals {
        let d = as_dbl(v, na)?;
        // which NaN R returns depends on the argument order and payloads
        if d.iter().any(|x| x.is_nan()) {
            return Err(Decline("NA or NaN in pmin()/pmax()"));
        }
        ds.push(d);
    }
    // the first of equal values is kept (pmin(0, -0) is 0, pmin(-0, 0) is -0)
    let out = (0..n)
        .map(|i| {
            let mut r = at(&ds[0], i);
            for d in &ds[1..] {
                let q = at(d, i);
                if (max && q > r) || (!max && q < r) {
                    r = q;
                }
            }
            r
        })
        .collect();
    Ok(Vals::Dbl(Cow::Owned(out)))
}

fn reduce<'a>(r: Red, x: Vals<'a>, h: &dyn Host) -> R<Vals<'a>> {
    Ok(match (r, x) {
        (Red::Sum, Vals::Dbl(v)) => Vals::Dbl(Cow::Owned(vec![h.sum(&v)])),
        (Red::Mean, Vals::Dbl(v)) => Vals::Dbl(Cow::Owned(vec![h.mean(&v)])),
        (Red::Sum, Vals::Int(v) | Vals::Lgl(v)) => match crate::reduce::sum_int(&v, false) {
            Some(s) => Vals::Int(Cow::Owned(vec![s])),
            None => return Err(Decline("integer sum overflows")),
        },
        (Red::Mean, Vals::Int(v) | Vals::Lgl(v)) => {
            let m = if v.contains(&NA_INT) {
                h.na_real()
            } else {
                h.mean_int(&v)
            };
            Vals::Dbl(Cow::Owned(vec![m]))
        }
        _ => return Err(Decline("sum()/mean() of strings")),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    // not a const: const f64::from_bits needs Rust 1.83, the MSRV is 1.81
    fn na_real() -> f64 {
        f64::from_bits(0x7ff0_0000_0000_07a2)
    }

    struct Plain {
        draws: Vec<f64>,
    }

    impl Host for Plain {
        fn na_real(&self) -> f64 {
            na_real()
        }
        fn na_str(&self) -> usize {
            0
        }
        fn str_ascii(&self, _s: usize) -> bool {
            true
        }
        fn runif(&mut self, n: usize) -> R<Vec<f64>> {
            Ok(self.draws.drain(..n).collect())
        }
        fn math(&mut self, f: MathFn, x: &[f64]) -> R<Vec<f64>> {
            Ok(x.iter()
                .map(|&v| match f {
                    MathFn::Exp => v.exp(),
                    MathFn::Log => v.ln(),
                    MathFn::Sqrt => v.sqrt(),
                })
                .collect())
        }
        fn sum(&self, x: &[f64]) -> f64 {
            x.iter().sum()
        }
        fn mean(&self, x: &[f64]) -> f64 {
            self.sum(x) / x.len() as f64
        }
        fn mean_int(&self, x: &[i32]) -> f64 {
            x.iter().map(|&v| v as f64).sum::<f64>() / x.len() as f64
        }
        fn call(&mut self, _f: usize, _args: &[Vals]) -> R<Vals<'static>> {
            Err(Decline("no R here"))
        }
        fn rand2(&mut self, _norm: bool, a: f64, b: f64, n: usize) -> R<Vec<f64>> {
            Ok(self.draws.drain(..n).map(|u| a + (b - a) * u).collect())
        }
        fn unif_index(&mut self, dn: f64, n: usize) -> R<Vec<i32>> {
            Ok(self
                .draws
                .drain(..n)
                .map(|u| (u * dn).floor() as i32 + 1)
                .collect())
        }
        fn sample_int(&mut self, n: usize, size: usize, _replace: bool) -> R<Vec<i32>> {
            self.unif_index(n as f64, size)
        }
        fn sample_int_prob(
            &mut self,
            n: usize,
            size: usize,
            _replace: bool,
            _prob: &[f64],
        ) -> R<Vec<i32>> {
            self.unif_index(n as f64, size)
        }
        fn rdist(&mut self, kind: u8, a: f64, _b: f64, n: usize) -> R<Vals<'static>> {
            let v: Vec<f64> = self.draws.drain(..n).map(|u| a + u).collect();
            Ok(if kind <= 1 {
                Vals::Int(Cow::Owned(v.iter().map(|&x| x as i32).collect()))
            } else {
                Vals::Dbl(Cow::Owned(v))
            })
        }
    }

    #[test]
    fn if_draw1_cast() {
        let mut h = Plain {
            draws: vec![0.25, 0.75],
        };
        let k = |x: Konst| Box::new(Node::Const(x));
        // if (FALSE) runif(1) else 2: the branch not taken does not draw
        let lazy = Node::If(
            k(Konst::Lgl(0)),
            Box::new(Node::Draw1(false, k(Konst::Dbl(0.0)), k(Konst::Dbl(1.0)))),
            k(Konst::Int(2)),
        );
        assert!(matches!(eval(&lazy, &[], 5, &mut h).unwrap(), Vals::Int(v) if v[..] == [2]));
        assert_eq!(
            h.draws.len(),
            2,
            "if() drew from the branch it did not take"
        );
        // one draw whatever n is
        let d = Node::Draw1(false, k(Konst::Dbl(0.0)), k(Konst::Dbl(4.0)));
        assert!(matches!(eval(&d, &[], 5, &mut h).unwrap(), Vals::Dbl(v) if v[..] == [1.0]));
        // NA condition, two-value condition: R errs
        assert!(eval(
            &Node::If(k(Konst::Lgl(NA_INT)), k(Konst::Int(1)), k(Konst::Int(2))),
            &[],
            1,
            &mut h
        )
        .is_err());
        let two = Vals::Lgl(Cow::Owned(vec![1, 0]));
        assert!(eval(
            &Node::If(Box::new(Node::Input(0)), k(Konst::Int(1)), k(Konst::Int(2))),
            &[two],
            2,
            &mut h
        )
        .is_err());
        // casts
        let x = Vals::Dbl(Cow::Owned(vec![-1.7, 2.9, f64::NAN, na_real()]));
        let got = eval(&Node::Cast(true, Box::new(Node::Input(0))), &[x], 4, &mut h).unwrap();
        assert!(matches!(got, Vals::Int(v) if v[..] == [-1, 2, NA_INT, NA_INT]));
        let big = Vals::Dbl(Cow::Owned(vec![3e9]));
        assert!(eval(
            &Node::Cast(true, Box::new(Node::Input(0))),
            &[big],
            1,
            &mut h
        )
        .is_err());
        let l = Vals::Lgl(Cow::Owned(vec![1, NA_INT]));
        let got = eval(
            &Node::Cast(false, Box::new(Node::Input(0))),
            &[l],
            2,
            &mut h,
        )
        .unwrap();
        assert!(
            matches!(got, Vals::Dbl(v) if v[0] == 1.0 && v[1].to_bits() == na_real().to_bits())
        );
    }

    #[test]
    fn coalesce_in_sample_index() {
        let mut h = Plain {
            draws: vec![0.1, 0.9, 0.5],
        };
        // coalesce(x, 0L): double; NaN is missing too
        let ins = [dbl(&[1.0, na_real(), f64::NAN])];
        let c = Node::Coalesce(vec![Node::Input(0), Node::Const(Konst::Int(0))]);
        let Vals::Dbl(v) = eval(&c, &ins, 3, &mut h).unwrap() else {
            panic!()
        };
        assert_eq!(*v, [1.0, 0.0, 0.0]);
        // 2 %in% c(1, 2) on integers; doubles with NA decline
        let t = Vals::Int(Cow::Owned(vec![1, 2]));
        let x = Vals::Int(Cow::Owned(vec![2, 3, NA_INT]));
        let Vals::Lgl(v) = is_in(&x, &t, &h).unwrap() else {
            panic!()
        };
        assert_eq!(*v, [1, 0, 0]);
        assert!(is_in(&dbl(&[na_real()]), &dbl(&[1.0]), &h).is_err());
        // sample(c(10, 20), 3, TRUE) and sample(5, 0, TRUE)
        let s = sample(dbl(&[10.0, 20.0]), Some(3), true, None, &mut h).unwrap();
        assert!(matches!(s, Vals::Dbl(v) if *v == [10.0, 20.0, 20.0]));
        assert!(
            matches!(sample(dbl(&[5.0]), Some(0), true, None, &mut h).unwrap(), Vals::Int(v) if v.is_empty())
        );
        assert!(sample(dbl(&[2.5]), Some(1), true, None, &mut h).is_err());
        // x[i]: NA and past the end give NA; 0 declines
        let i = Vals::Int(Cow::Owned(vec![2, NA_INT, 9]));
        let Vals::Dbl(v) = index(dbl(&[1.0, 2.0]), &i, &h).unwrap() else {
            panic!()
        };
        assert_eq!(
            (v[0], v[1].to_bits(), v[2].to_bits()),
            (2.0, na_real().to_bits(), na_real().to_bits())
        );
        assert!(index(dbl(&[1.0]), &Vals::Int(Cow::Owned(vec![0])), &h).is_err());
        // sum(x, na.rm = TRUE) skips NA
        let Vals::Dbl(v) = reduce_na_rm(Red::Sum, dbl(&[1.0, na_real(), 2.0]), &h).unwrap() else {
            panic!()
        };
        assert_eq!(v[0], 3.0);
    }

    fn dbl(v: &[f64]) -> Vals<'static> {
        Vals::Dbl(Cow::Owned(v.to_vec()))
    }
    fn lgl(v: &[i32]) -> Vals<'static> {
        Vals::Lgl(Cow::Owned(v.to_vec()))
    }
    fn input(k: usize) -> Box<Node> {
        Box::new(Node::Input(k))
    }
    fn num(x: f64) -> Box<Node> {
        Box::new(Node::Const(Konst::Dbl(x)))
    }

    #[test]
    fn three_valued_logic_and_comparisons() {
        let mut h = Plain { draws: vec![] };
        let ins = [lgl(&[1, 0, NA_INT, NA_INT]), lgl(&[NA_INT, NA_INT, 0, 1])];
        let and = eval(&Node::Bin(Bin::And, input(0), input(1)), &ins, 4, &mut h).unwrap();
        let or = eval(&Node::Bin(Bin::Or, input(0), input(1)), &ins, 4, &mut h).unwrap();
        assert!(matches!(and, Vals::Lgl(v) if *v == [NA_INT, 0, 0, NA_INT]));
        assert!(matches!(or, Vals::Lgl(v) if *v == [1, NA_INT, NA_INT, 1]));
        let ins = [dbl(&[1.0, f64::NAN, 3.0])];
        let lt = eval(&Node::Bin(Bin::Lt, input(0), num(2.0)), &ins, 3, &mut h).unwrap();
        assert!(matches!(lt, Vals::Lgl(v) if *v == [1, NA_INT, 0]));
    }

    #[test]
    fn integer_overflow_declines_and_division_is_double() {
        let mut h = Plain { draws: vec![] };
        let ins = [Vals::Int(Cow::Owned(vec![i32::MAX, NA_INT]))];
        let one = Box::new(Node::Const(Konst::Int(1)));
        let add = Node::Bin(Bin::Add, input(0), one.clone());
        assert_eq!(
            eval(&add, &ins, 2, &mut h).unwrap_err().0,
            "integer overflow warns"
        );
        let div = eval(&Node::Bin(Bin::Div, input(0), one), &ins, 2, &mut h).unwrap();
        let Vals::Dbl(d) = div else { panic!() };
        assert_eq!(d[0], i32::MAX as f64);
        assert_eq!(d[1].to_bits(), na_real().to_bits());
    }

    #[test]
    fn case_when_and_if_else_follow_dplyr() {
        let mut h = Plain { draws: vec![] };
        let ins = [dbl(&[1.0, 5.0, f64::NAN]), lgl(&[NA_INT])];
        // case_when(x > 2 ~ x, x > 0 ~ 0L, .default = NA): common type double
        let cw = Node::CaseWhen(
            vec![
                (Node::Bin(Bin::Gt, input(0), num(2.0)), Node::Input(0)),
                (
                    Node::Bin(Bin::Gt, input(0), num(0.0)),
                    Node::Const(Konst::Int(0)),
                ),
            ],
            Some(input(1)),
        );
        let Vals::Dbl(v) = eval(&cw, &ins, 3, &mut h).unwrap() else {
            panic!()
        };
        assert_eq!(v[0], 0.0);
        assert_eq!(v[1], 5.0);
        assert_eq!(v[2].to_bits(), na_real().to_bits());
        // a scalar condition with a vector value warns in dplyr: decline
        let bad = Node::CaseWhen(vec![(Node::Const(Konst::Lgl(1)), Node::Input(0))], None);
        assert!(eval(&bad, &ins, 3, &mut h).is_err());
        // if_else(): unspecified NA takes the other branch's type
        let ie = Node::IfElse(
            Box::new(Node::Bin(Bin::Gt, input(0), num(2.0))),
            Box::new(Node::Const(Konst::Str(7))),
            input(1),
        );
        let Vals::Str(s) = eval(&ie, &ins, 3, &mut h).unwrap() else {
            panic!()
        };
        assert_eq!(*s, [0, 7, 0]);
    }

    #[test]
    fn pmin_keeps_the_first_of_ties_and_declines_on_nan() {
        let mut h = Plain { draws: vec![] };
        let ins = [dbl(&[0.0, 2.0]), dbl(&[-0.0, 1.0])];
        let Vals::Dbl(v) = eval(
            &Node::PMinMax(false, vec![Node::Input(0), Node::Input(1)]),
            &ins,
            2,
            &mut h,
        )
        .unwrap() else {
            panic!()
        };
        assert!(v[0].is_sign_positive() && v[1] == 1.0);
        let ins = [dbl(&[f64::NAN]), dbl(&[1.0])];
        assert!(eval(
            &Node::PMinMax(true, vec![Node::Input(0), Node::Input(1)]),
            &ins,
            1,
            &mut h
        )
        .is_err());
    }

    #[test]
    fn logical_index() {
        let h = Plain { draws: vec![] };
        let x = Vals::Int(Cow::Borrowed(&[10, 20, 30][..]));
        let run = |i: &[i32]| match index(x.clone(), &Vals::Lgl(Cow::Borrowed(i)), &h) {
            Ok(Vals::Int(v)) => v.into_owned(),
            _ => panic!("not an integer vector"),
        };
        assert_eq!(run(&[1, 0, 1]), [10, 30]);
        assert_eq!(run(&[1, 0]), [10, 30]); // recycled
        assert_eq!(run(&[NA_INT, 0, 1]), [NA_INT, 30]);
        assert_eq!(run(&[0, 0, 0, 1]), [NA_INT]); // past the end
        assert_eq!(run(&[]), [0; 0]);
        assert_eq!(run(&[0, NA_INT, 1]), [NA_INT, 30]);
    }

    #[test]
    fn random_rank() {
        let mut h = Plain {
            draws: vec![0.9, 0.5, 0.1, 0.2],
        };
        let x = Vals::Dbl(Cow::Borrowed(&[2.0, 0.0, -0.0, 2.0][..]));
        match rank_random(x, &mut h) {
            Ok(Vals::Int(v)) => assert_eq!(v.into_owned(), [4, 2, 1, 3]),
            _ => panic!("not an integer vector"),
        }
    }

    #[test]
    fn draws_come_in_evaluation_order() {
        let mut h = Plain {
            draws: vec![0.1, 0.9, 0.2, 0.3],
        };
        // runif(n()) < 0.5 & runif(n()) < 0.5 draws the left n first
        let r = || Box::new(Node::Bin(Bin::Lt, Box::new(Node::Runif), num(0.5)));
        let e = Node::Bin(Bin::And, r(), r());
        let Vals::Lgl(v) = eval(&e, &[], 2, &mut h).unwrap() else {
            panic!()
        };
        assert_eq!(*v, [1, 0]);
    }
}
