//! Spike S7: a closure-free tree evaluator for the IR that `R/ir.R` builds from a
//! rule's quosure. Column-at-a-time; R / dplyr semantics for the covered functions:
//! NA propagation, R's relational NA rule, three-valued `&`/`|`, pmin/pmax's NaN
//! rule (summary.c do_pmin), R's exp/log wrappers (arithmetic.c), `if_else` and
//! `case_when` (dplyr >= 1.1: conditions and values evaluated in written order,
//! NA condition = no match). Anything that would make R warn ("NaNs produced")
//! returns `Err(FALLBACK)` so the caller re-evaluates the rule in R (decision D9).

use savvy::{
    savvy_err, ListSexp, NotAvailableValue, OwnedLogicalSexp, OwnedRealSexp, Sexp, TypedSexp,
};

pub const NA_LGL: i32 = i32::MIN;
pub const FALLBACK: &str = "tidyABM_fallback";

#[derive(Debug, Clone)]
pub enum Node {
    Num(f64),
    Lgl(i32),
    Str(String),
    Var(String),
    Call(Fun, Vec<Node>),
    /// case_when: (condition, value) pairs and an optional .default
    CaseWhen(Vec<(Node, Node)>, Option<Box<Node>>),
}

#[derive(Debug, Clone, Copy)]
pub enum Fun {
    Add,
    Sub,
    Mul,
    Div,
    Neg,
    Lt,
    Gt,
    Le,
    Ge,
    Eq,
    Ne,
    And,
    Or,
    Not,
    Exp,
    Log,
    Pmin,
    Pmax,
    IfElse,
    Runif,
    N,
}

/// A value: length 1 recycles.
#[derive(Debug, Clone)]
pub enum V<'a> {
    Dbl(Vec<f64>),
    Lgl(Vec<i32>),
    Str(Vec<&'a str>),
}

pub struct Env {
    // &'static str: R's CHARSXP cache entries, alive while the .Call's arguments are
    vars: Vec<(String, V<'static>)>,
    pub n: usize,
}

impl Env {
    pub fn from_list(l: &ListSexp, n: usize) -> savvy::Result<Self> {
        let mut vars = Vec::with_capacity(l.len());
        for (name, v) in l.iter() {
            let v = match v.into_typed() {
                TypedSexp::Real(x) => V::Dbl(x.to_vec()),
                TypedSexp::Logical(x) => V::Lgl(x.as_slice_raw().to_vec()),
                TypedSexp::String(x) => V::Str(x.to_vec()),
                _ => return Err(savvy_err!("{FALLBACK}: unsupported column type for {name}")),
            };
            vars.push((name.to_string(), v));
        }
        Ok(Env { vars, n })
    }
    fn get(&self, name: &str) -> savvy::Result<&V<'static>> {
        self.vars
            .iter()
            .find(|(k, _)| k == name)
            .map(|(_, v)| v)
            .ok_or_else(|| savvy_err!("{FALLBACK}: unknown name {name}"))
    }
}

// ------------------------------------------------------------------ compile

fn field(l: &ListSexp, k: &str) -> savvy::Result<Sexp> {
    l.get(k).ok_or_else(|| savvy_err!("IR node without `{k}`"))
}

fn str_of(s: Sexp) -> savvy::Result<String> {
    match s.into_typed() {
        TypedSexp::String(x) => Ok(x.iter().next().unwrap_or("").to_string()),
        _ => Err(savvy_err!("IR: expected a string")),
    }
}

pub fn compile(ir: Sexp) -> savvy::Result<Node> {
    let l = match ir.into_typed() {
        TypedSexp::List(l) => l,
        _ => return Err(savvy_err!("IR node must be a list")),
    };
    let op = str_of(field(&l, "op")?)?;
    Ok(match op.as_str() {
        "num" => match field(&l, "value")?.into_typed() {
            TypedSexp::Real(x) => Node::Num(x.as_slice()[0]),
            TypedSexp::Integer(x) => Node::Num(x.as_slice()[0] as f64),
            _ => return Err(savvy_err!("IR num")),
        },
        "lgl" => match field(&l, "value")?.into_typed() {
            TypedSexp::Logical(x) => Node::Lgl(x.as_slice_raw()[0]),
            _ => return Err(savvy_err!("IR lgl")),
        },
        "str" => Node::Str(str_of(field(&l, "value")?)?),
        "var" => Node::Var(str_of(field(&l, "name")?)?),
        "case_when" => {
            let pairs = match field(&l, "pairs")?.into_typed() {
                TypedSexp::List(p) => p,
                _ => return Err(savvy_err!("IR case_when")),
            };
            let mut out = Vec::new();
            for (_, p) in pairs.iter() {
                let p = match p.into_typed() {
                    TypedSexp::List(p) => p,
                    _ => return Err(savvy_err!("IR case_when pair")),
                };
                out.push((compile(field(&p, "cond")?)?, compile(field(&p, "value")?)?));
            }
            let default = match l.get("default") {
                Some(d) if !d.is_null() => Some(Box::new(compile(d)?)),
                _ => None,
            };
            Node::CaseWhen(out, default)
        }
        "call" => {
            let f = match str_of(field(&l, "fn")?)?.as_str() {
                "+" => Fun::Add,
                "-" => Fun::Sub,
                "*" => Fun::Mul,
                "/" => Fun::Div,
                "neg" => Fun::Neg,
                "<" => Fun::Lt,
                ">" => Fun::Gt,
                "<=" => Fun::Le,
                ">=" => Fun::Ge,
                "==" => Fun::Eq,
                "!=" => Fun::Ne,
                "&" => Fun::And,
                "|" => Fun::Or,
                "!" => Fun::Not,
                "exp" => Fun::Exp,
                "log" => Fun::Log,
                "pmin" => Fun::Pmin,
                "pmax" => Fun::Pmax,
                "if_else" => Fun::IfElse,
                "runif" => Fun::Runif,
                "n" => Fun::N,
                other => return Err(savvy_err!("IR: function {other} not covered")),
            };
            let args = match field(&l, "args")?.into_typed() {
                TypedSexp::List(a) => a
                    .values_iter()
                    .map(compile)
                    .collect::<savvy::Result<Vec<_>>>()?,
                TypedSexp::Null(_) => vec![],
                _ => return Err(savvy_err!("IR args")),
            };
            Node::Call(f, args)
        }
        other => return Err(savvy_err!("IR: unknown op {other}")),
    })
}

// ------------------------------------------------------------------ evaluate

fn len(v: &V<'_>) -> usize {
    match v {
        V::Dbl(x) => x.len(),
        V::Lgl(x) => x.len(),
        V::Str(x) => x.len(),
    }
}

/// Coerce to double as R does (logical NA -> NA_real_).
fn dbl(v: V<'_>) -> savvy::Result<Vec<f64>> {
    match v {
        V::Dbl(x) => Ok(x),
        V::Lgl(x) => Ok(x
            .into_iter()
            .map(|b| if b == NA_LGL { f64::na() } else { b as f64 })
            .collect()),
        V::Str(_) => Err(savvy_err!("{FALLBACK}: arithmetic on character")),
    }
}

/// Coerce to logical as R's `&`/`!` do (double: NaN -> NA, 0 -> FALSE, else TRUE).
fn lgl(v: V<'_>) -> savvy::Result<Vec<i32>> {
    match v {
        V::Lgl(x) => Ok(x),
        V::Dbl(x) => Ok(x
            .into_iter()
            .map(|d| {
                if d.is_nan() {
                    NA_LGL
                } else {
                    (d != 0.0) as i32
                }
            })
            .collect()),
        V::Str(_) => Err(savvy_err!("{FALLBACK}: logical op on character")),
    }
}

fn size2(a: usize, b: usize, n: usize) -> savvy::Result<usize> {
    let m = a.max(b);
    if a == 0 || b == 0 {
        return Err(savvy_err!("{FALLBACK}: zero-length operand"));
    }
    if !m.is_multiple_of(a) || !m.is_multiple_of(b) || (m != 1 && m != n) {
        return Err(savvy_err!("{FALLBACK}: recycling"));
    }
    Ok(m)
}

fn zip_d(a: &[f64], b: &[f64], m: usize, f: impl Fn(f64, f64) -> f64) -> Vec<f64> {
    (0..m).map(|i| f(a[i % a.len()], b[i % b.len()])).collect()
}

fn rel<'a>(
    a: V<'a>,
    b: V<'a>,
    m: usize,
    f: impl Fn(std::cmp::Ordering) -> bool,
) -> savvy::Result<V<'a>> {
    Ok(V::Lgl(match (a, b) {
        (V::Str(a), V::Str(b)) => (0..m)
            .map(|i| {
                let (x, y) = (a[i % a.len()], b[i % b.len()]);
                if x.is_na() || y.is_na() {
                    NA_LGL
                } else if x == y {
                    f(std::cmp::Ordering::Equal) as i32
                } else {
                    // only == / != are compiled for strings (collation is locale-dependent)
                    f(std::cmp::Ordering::Less) as i32 & f(std::cmp::Ordering::Greater) as i32
                }
            })
            .collect(),
        (V::Str(_), _) | (_, V::Str(_)) => {
            return Err(savvy_err!("{FALLBACK}: mixed string comparison"))
        }
        (a, b) => {
            let (a, b) = (dbl(a)?, dbl(b)?);
            (0..m)
                .map(|i| {
                    let (x, y) = (a[i % a.len()], b[i % b.len()]);
                    if x.is_nan() || y.is_nan() {
                        NA_LGL
                    } else {
                        f(x.partial_cmp(&y).unwrap()) as i32
                    }
                })
                .collect()
        }
    }))
}

/// R's `exp` (Rexp + math1_ari) and `log` (R_log + math1); Err on "NaNs produced".
fn math1(x: Vec<f64>, f: fn(f64) -> f64) -> savvy::Result<Vec<f64>> {
    let mut out = Vec::with_capacity(x.len());
    for v in x {
        let y = f(v);
        if y.is_nan() {
            if v.is_nan() {
                out.push(v);
                continue;
            }
            return Err(savvy_err!("{FALLBACK}: NaNs produced"));
        }
        out.push(y);
    }
    Ok(out)
}
fn r_exp(x: f64) -> f64 {
    if x == 0.0 {
        1.0
    } else if x.abs() <= f64::EPSILON.sqrt() {
        1.0 + x
    } else {
        x.exp()
    }
}
fn r_log(x: f64) -> f64 {
    if x > 0.0 {
        x.ln()
    } else if x == 0.0 {
        f64::NEG_INFINITY
    } else {
        f64::NAN
    }
}

pub fn eval<'a>(
    node: &'a Node,
    env: &'a Env,
    unif: &mut dyn FnMut() -> f64,
) -> savvy::Result<V<'a>> {
    let n = env.n;
    Ok(match node {
        Node::Num(x) => V::Dbl(vec![*x]),
        Node::Lgl(x) => V::Lgl(vec![*x]),
        Node::Str(s) => V::Str(vec![s.as_str()]),
        Node::Var(name) => env.get(name)?.clone(),
        Node::CaseWhen(pairs, default) => {
            // dplyr::case_when: each condition then its value, in written order
            let mut conds = Vec::with_capacity(pairs.len());
            let mut vals = Vec::with_capacity(pairs.len());
            let mut all_lgl = true;
            for (c, v) in pairs {
                let c = eval(c, env, unif)?;
                let c = match c {
                    V::Lgl(x) => x,
                    _ => return Err(savvy_err!("{FALLBACK}: case_when condition not logical")),
                };
                let v = eval(v, env, unif)?;
                if !matches!(v, V::Lgl(_)) {
                    all_lgl = false;
                }
                for (len_, what) in [(c.len(), "condition"), (len(&v), "value")] {
                    if len_ != 1 && len_ != n {
                        return Err(savvy_err!("{FALLBACK}: case_when {what} size"));
                    }
                }
                conds.push(c);
                vals.push(v);
            }
            let def = match default {
                Some(d) => Some(eval(d, env, unif)?),
                None => None,
            };
            if all_lgl && def.as_ref().is_none_or(|d| matches!(d, V::Lgl(_))) {
                return Err(savvy_err!(
                    "{FALLBACK}: logical case_when not compiled in the spike"
                ));
            }
            let vals: Vec<Vec<f64>> = vals.into_iter().map(dbl).collect::<savvy::Result<_>>()?;
            let def = def.map(dbl).transpose()?;
            // dplyr::case_when: the common size of the conditions; values and
            // .default must be size 1 or that size
            let size = conds.iter().map(|c| c.len()).max().unwrap_or(1);
            if conds.iter().any(|c| c.len() != 1 && c.len() != size)
                || vals.iter().any(|v| v.len() != 1 && v.len() != size)
                || def
                    .as_ref()
                    .is_some_and(|d| d.len() != 1 && d.len() != size)
            {
                return Err(savvy_err!("{FALLBACK}: case_when sizes"));
            }
            let n = size;
            let mut out = vec![f64::na(); n];
            let mut done = vec![false; n];
            for (c, v) in conds.iter().zip(&vals) {
                for i in 0..n {
                    if !done[i] && c[i % c.len()] == 1 {
                        out[i] = v[i % v.len()];
                        done[i] = true;
                    }
                }
            }
            if let Some(d) = def {
                for i in 0..n {
                    if !done[i] {
                        out[i] = d[i % d.len()];
                    }
                }
            }
            V::Dbl(out)
        }
        Node::Call(f, args) => {
            let mut ev = |k: usize| eval(&args[k], env, unif);
            match f {
                Fun::N => V::Dbl(vec![n as f64]),
                Fun::Runif => {
                    // runif(n()) only: one draw per row from R's generator, in order
                    let k = dbl(ev(0)?)?;
                    if args.len() != 1 || k.len() != 1 || k[0] != n as f64 {
                        return Err(savvy_err!("{FALLBACK}: runif form"));
                    }
                    V::Dbl((0..n).map(|_| unif()).collect())
                }
                Fun::Neg => V::Dbl(dbl(ev(0)?)?.into_iter().map(|x| -x).collect()),
                Fun::Not => V::Lgl(
                    lgl(ev(0)?)?
                        .into_iter()
                        .map(|b| if b == NA_LGL { NA_LGL } else { 1 - b })
                        .collect(),
                ),
                Fun::Exp => V::Dbl(math1(dbl(ev(0)?)?, r_exp)?),
                Fun::Log => {
                    if args.len() != 1 {
                        return Err(savvy_err!("{FALLBACK}: log with base"));
                    }
                    V::Dbl(math1(dbl(ev(0)?)?, r_log)?)
                }
                Fun::Add | Fun::Sub | Fun::Mul | Fun::Div => {
                    let (a, b) = (dbl(ev(0)?)?, dbl(ev(1)?)?);
                    let m = size2(a.len(), b.len(), n)?;
                    V::Dbl(match f {
                        Fun::Add => zip_d(&a, &b, m, |x, y| x + y),
                        Fun::Sub => zip_d(&a, &b, m, |x, y| x - y),
                        Fun::Mul => zip_d(&a, &b, m, |x, y| x * y),
                        _ => zip_d(&a, &b, m, |x, y| x / y),
                    })
                }
                Fun::Lt | Fun::Gt | Fun::Le | Fun::Ge | Fun::Eq | Fun::Ne => {
                    let (a, b) = (ev(0)?, ev(1)?);
                    let m = size2(len(&a), len(&b), n)?;
                    use std::cmp::Ordering::*;
                    match f {
                        Fun::Lt => rel(a, b, m, |o| o == Less)?,
                        Fun::Gt => rel(a, b, m, |o| o == Greater)?,
                        Fun::Le => rel(a, b, m, |o| o != Greater)?,
                        Fun::Ge => rel(a, b, m, |o| o != Less)?,
                        Fun::Eq => rel(a, b, m, |o| o == Equal)?,
                        _ => rel(a, b, m, |o| o != Equal)?,
                    }
                }
                Fun::And | Fun::Or => {
                    // vectorised: both sides always evaluated (draws on the right happen)
                    let (a, b) = (lgl(ev(0)?)?, lgl(ev(1)?)?);
                    let m = size2(a.len(), b.len(), n)?;
                    V::Lgl(
                        (0..m)
                            .map(|i| {
                                let (x, y) = (a[i % a.len()], b[i % b.len()]);
                                if matches!(f, Fun::And) {
                                    if x == 0 || y == 0 {
                                        0
                                    } else if x == NA_LGL || y == NA_LGL {
                                        NA_LGL
                                    } else {
                                        1
                                    }
                                } else if x == 1 || y == 1 {
                                    1
                                } else if x == NA_LGL || y == NA_LGL {
                                    NA_LGL
                                } else {
                                    0
                                }
                            })
                            .collect(),
                    )
                }
                Fun::Pmin | Fun::Pmax => {
                    let mut acc = dbl(ev(0)?)?;
                    for k in 1..args.len() {
                        let b = dbl(ev(k)?)?;
                        let m = size2(acc.len(), b.len(), n)?;
                        let mut r: Vec<f64> = (0..m).map(|i| acc[i % acc.len()]).collect();
                        for (i, ri) in r.iter_mut().enumerate() {
                            let t = b[i % b.len()];
                            let better = if matches!(f, Fun::Pmax) {
                                t > *ri
                            } else {
                                t < *ri
                            };
                            if (!ri.is_nan() && !t.is_nan() && better) || t.is_nan() {
                                *ri = t;
                            }
                        }
                        acc = r;
                    }
                    V::Dbl(acc)
                }
                Fun::IfElse => {
                    let c = match ev(0)? {
                        V::Lgl(x) => x,
                        _ => return Err(savvy_err!("{FALLBACK}: if_else condition not logical")),
                    };
                    let (t, e) = (ev(1)?, ev(2)?);
                    if matches!((&t, &e), (V::Lgl(_), V::Lgl(_))) {
                        return Err(savvy_err!(
                            "{FALLBACK}: logical if_else not compiled in the spike"
                        ));
                    }
                    let (t, e) = (dbl(t)?, dbl(e)?);
                    // dplyr::if_else: `true`/`false` recycle to the size of `condition`
                    // only (a scalar condition with vector branches is an error in R)
                    let m = c.len();
                    if [t.len(), e.len()].iter().any(|&l| l != 1 && l != m) || (m != 1 && m != n) {
                        return Err(savvy_err!("{FALLBACK}: if_else sizes"));
                    }
                    V::Dbl(
                        (0..m)
                            .map(|i| match c[i % c.len()] {
                                1 => t[i % t.len()],
                                0 => e[i % e.len()],
                                _ => f64::na(),
                            })
                            .collect(),
                    )
                }
            }
        }
    })
}

pub fn to_sexp(v: V<'_>, n: usize) -> savvy::Result<Sexp> {
    match v {
        V::Dbl(x) => {
            let x = if x.len() == 1 && n != 1 {
                vec![x[0]; n]
            } else {
                x
            };
            OwnedRealSexp::try_from_slice(x).map(Into::into)
        }
        V::Lgl(x) => {
            let x = if x.len() == 1 && n != 1 {
                vec![x[0]; n]
            } else {
                x
            };
            let mut out = OwnedLogicalSexp::new(x.len())?;
            for (i, b) in x.into_iter().enumerate() {
                if b == NA_LGL {
                    out.set_na(i)?;
                } else {
                    out.set_elt(i, b != 0)?;
                }
            }
            Ok(out.into())
        }
        V::Str(_) => Err(savvy_err!("{FALLBACK}: character result")),
    }
}
