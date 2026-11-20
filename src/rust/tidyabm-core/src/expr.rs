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
}

type R<T> = Result<T, Decline>;

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
    }
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
        _ => return Err(Decline("unary operator on an unsupported type")),
    })
}

fn binary<'a>(op: Bin, a: Vals<'a>, b: Vals<'a>, h: &dyn Host) -> R<Vals<'a>> {
    let n = common_len(&[a.len(), b.len()])?;
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
