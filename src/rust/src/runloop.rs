//! The Rust-driven run (full-Rust stages 1-2, PLAN.md "Full-Rust plan").
//!
//! `RunLoop` owns the run's `state` (the R engine's own state list: groups,
//! globals, edges, ...) for the whole run and drives the tick loop. A step with a
//! native implementation runs here without evaluating any R code: it reads the
//! state's column vectors in place and commits new vectors into shallow copies of
//! the containers (never modifying an R object in place, so value semantics hold
//! for any R variable that still sees the old state). Every other step, and every
//! run of consecutive such steps, is one call back into R (`seg`), which runs the
//! R engine's `run_step()` on the same state object (zero copy). The tick's
//! book-keeping (`tick`: `state$match <- NULL`, `state$tick <- t`, snapshots,
//! globals log, measures, checks) is an R callback too.
//!
//! A native step is compiled by an R callback (`prep`) for the state's schema
//! (`c(length(globals), lengths(groups))`, as `rx_key()`), kept until the schema
//! changes. It is transactional: everything is evaluated first and committed only
//! if nothing declined, so a declined step leaves the state as it was; the R side
//! then replays it on the R path, after restoring `.Random.seed` (saved through
//! `rng` before any drawing native step), so R's own conditions and draws result.
//!
//! R errors raised inside a callback unwind through `R_UnwindProtect` only
//! (`rapi::protect`), come back as `savvy::Error::Aborted` and are re-raised by
//! savvy once `run()` has dropped (released) everything it kept.

use crate::expr::{self, RxProgram};
use crate::rapi::{self, INTSXP, STRSXP, VECSXP};
use crate::store::{self, keep, kept_from, H};
use crate::{run, typed_error};
use savvy::ffi::SEXP;
use savvy::protect::insert_to_preserved_list;
use savvy::{savvy, IntegerSexp, LogicalSexp, NullSexp, OwnedListSexp, Sexp};
use tidyabm_core::expr::{Decline, Vals};

/// Build a new R object with `f` inside `protect` and keep it before anything
/// else can allocate.
///
/// # Safety
///
/// Main thread; `f` must not panic and may use only objects protected by the
/// caller.
unsafe fn build<F: FnOnce() -> SEXP + Copy>(f: F) -> savvy::Result<H> {
    // SAFETY: caller's contract; the result is protected while it is inserted
    // into the preserved list.
    let (x, tok) = unsafe {
        rapi::protect_value((rapi::R_NilValue, rapi::R_NilValue), move || {
            let x = rapi::Rf_protect(f());
            let tok = insert_to_preserved_list(x);
            rapi::Rf_unprotect(1);
            (x, tok)
        })
    }?;
    Ok(kept_from(x, tok))
}

/// `f(ints...)` evaluated in R.
///
/// # Safety
///
/// Main thread; `fun` is kept by the caller. The value is returned unprotected:
/// the caller keeps it at once, or it is already bound in the loop's context
/// environment (the state callbacks bind what they return).
unsafe fn eval_r(fun: SEXP, ints: &[i32]) -> savvy::Result<SEXP> {
    let (p, n) = (ints.as_ptr(), ints.len());
    // SAFETY: caller's contract; every object allocated here is protected until
    // the call has been evaluated; `p` points at `n` ints alive for this call. An
    // R error in the callback longjmps to `protect` and comes back as `Err`.
    unsafe {
        rapi::protect_value(rapi::R_NilValue, move || {
            let mut args = rapi::R_NilValue;
            for k in (0..n).rev() {
                let v = rapi::Rf_protect(rapi::Rf_ScalarInteger(*p.add(k)));
                args = rapi::Rf_protect(rapi::Rf_cons(v, args));
            }
            let call = rapi::Rf_protect(rapi::Rf_lcons(fun, args));
            let res = rapi::Rf_eval(call, rapi::R_GlobalEnv);
            rapi::Rf_unprotect(2 * n as i32 + 1);
            res
        })
    }
}

/// `f(ints...)` evaluated in R, its value kept.
fn call_r(f: &H, ints: &[i32]) -> savvy::Result<H> {
    let fun = f.get();
    let ints = ints.to_vec();
    let p = ints.as_ptr();
    let n = ints.len();
    // SAFETY: main thread; `fun` kept; the value is kept inside the same protected
    // call (`build`), before anything else allocates.
    unsafe {
        build(move || {
            let mut args = rapi::R_NilValue;
            for k in (0..n).rev() {
                let v = rapi::Rf_protect(rapi::Rf_ScalarInteger(*p.add(k)));
                args = rapi::Rf_protect(rapi::Rf_cons(v, args));
            }
            let call = rapi::Rf_protect(rapi::Rf_lcons(fun, args));
            let res = rapi::Rf_eval(call, rapi::R_GlobalEnv);
            rapi::Rf_unprotect(2 * n as i32 + 1);
            res
        })
    }
}

fn rapi_nil() -> SEXP {
    // SAFETY: reading a static of R's.
    unsafe { rapi::R_NilValue }
}

/// The length of an R vector (0 for `NULL`).
fn len(x: SEXP) -> usize {
    // SAFETY: main thread; `x` protected by the caller; neither allocates.
    unsafe { rapi::Rf_xlength(x) as usize }
}

fn elt(x: SEXP, i: usize) -> SEXP {
    // SAFETY: main thread; `x` is a protected list with more than `i` elements.
    unsafe { rapi::VECTOR_ELT(x, i as isize) }
}

/// The names of list `x` as `CHARSXP`s (empty when it has none).
fn names_of<'a>(x: SEXP) -> savvy::Result<&'a [SEXP]> {
    // SAFETY: main thread; `x` protected by the caller. On a list (`VECSXP`)
    // getAttrib of `names` returns the attribute itself, kept alive by `x`,
    // without allocating or raising (only pairlists and language objects build
    // their names), so it needs no `protect`; anything else goes through it.
    let nm = unsafe {
        if rapi::TYPEOF(x) == VECSXP {
            rapi::Rf_getAttrib(x, rapi::R_NamesSymbol)
        } else {
            rapi::protect_value(rapi::R_NilValue, move || {
                rapi::Rf_getAttrib(x, rapi::R_NamesSymbol)
            })?
        }
    };
    // SAFETY: reading a type tag.
    if unsafe { rapi::TYPEOF(nm) } != STRSXP {
        return Ok(&[]);
    }
    store::chars_of(nm)
}

fn bytes<'a>(c: SEXP) -> &'a [u8] {
    // SAFETY: `c` is a live CHARSXP; R_CHAR gives its NUL-terminated bytes.
    unsafe { std::ffi::CStr::from_ptr(rapi::R_CHAR(c)) }.to_bytes()
}

/// Where name `want` is among `names`: the same `CHARSXP`, or the same bytes when
/// both are ASCII (other encodings: not found, and the step goes to R).
fn find(names: &[SEXP], want: SEXP) -> Option<usize> {
    if let Some(i) = names.iter().position(|&c| c == want) {
        return Some(i);
    }
    let w = bytes(want);
    if !w.is_ascii() {
        return None;
    }
    names.iter().position(|&c| bytes(c) == w)
}

/// Where the element named `want` (ASCII) is in list `x`.
fn find_str(x: SEXP, want: &[u8]) -> savvy::Result<Option<usize>> {
    Ok(names_of(x)?.iter().position(|&c| bytes(c) == want))
}

/// Is every one of `xs` free of attributes? (`ATTRIB()` is not API: each one's
/// attributes are copied onto a new empty vector, which is compared with a bare
/// one; all in one protected call.)
pub(crate) fn all_bare(xs: &[SEXP]) -> savvy::Result<bool> {
    if xs.is_empty() {
        return Ok(true);
    }
    let (p, n) = (xs.as_ptr(), xs.len());
    // SAFETY: main thread; the `xs` are protected by the caller; each pair of new
    // vectors is protected while compared; `p` points at `n` SEXPs.
    let bare = unsafe {
        rapi::protect_value(0, move || {
            for k in 0..n {
                let x = *p.add(k);
                let t = rapi::TYPEOF(x);
                let a = rapi::Rf_protect(rapi::Rf_allocVector(t, 0));
                rapi::SHALLOW_DUPLICATE_ATTRIB(a, x);
                let b = rapi::Rf_protect(rapi::Rf_allocVector(t, 0));
                // flags 16: identical()'s defaults
                let same = rapi::R_compute_identical(a, b, 16);
                rapi::Rf_unprotect(2);
                if same == 0 {
                    return 0;
                }
            }
            1
        })
    }?;
    Ok(bare != 0)
}

/// A shallow copy of list `x` with element `i` set to `v`; `i == length(x)`
/// appends `v` named `nm` (a `CHARSXP`), as `x[[nm]] <- v` does.
fn with_elt(x: SEXP, i: usize, v: SEXP, nm: SEXP) -> savvy::Result<H> {
    // SAFETY: main thread; `x`, `v`, `nm` protected by the caller; the new list
    // and names are protected while filled; indices stay within the lengths.
    unsafe {
        build(move || {
            let n = rapi::Rf_xlength(x) as usize;
            let m = if i == n { n + 1 } else { n };
            let y = rapi::Rf_protect(rapi::Rf_allocVector(VECSXP, m as isize));
            for k in 0..n {
                rapi::SET_VECTOR_ELT(y, k as isize, rapi::VECTOR_ELT(x, k as isize));
            }
            rapi::SET_VECTOR_ELT(y, i as isize, v);
            rapi::SHALLOW_DUPLICATE_ATTRIB(y, x);
            if i == n {
                let old = rapi::Rf_getAttrib(x, rapi::R_NamesSymbol);
                let nn = rapi::Rf_protect(rapi::Rf_allocVector(STRSXP, m as isize));
                let named = rapi::TYPEOF(old) == STRSXP;
                for k in 0..n {
                    let s = if named {
                        rapi::STRING_ELT(old, k as isize)
                    } else {
                        rapi::R_BlankString
                    };
                    rapi::SET_STRING_ELT(nn, k as isize, s);
                }
                rapi::SET_STRING_ELT(nn, n as isize, nm);
                rapi::Rf_setAttrib(y, rapi::R_NamesSymbol, nn);
                rapi::Rf_unprotect(1);
            }
            rapi::Rf_unprotect(1);
            y
        })
    }
}

/// `x[[nm]] <- v` on a shallow copy: replaced when the name is there, else
/// appended.
fn set_named(x: SEXP, nm: SEXP, v: SEXP) -> savvy::Result<H> {
    let i = find(names_of(x)?, nm).unwrap_or(len(x));
    with_elt(x, i, v, nm)
}

/// `x[[name]] <- NULL` on a shallow copy (the element dropped when present).
fn without(x: SEXP, name: &[u8]) -> savvy::Result<H> {
    let Some(i) = find_str(x, name)? else {
        return keep(x);
    };
    // SAFETY: main thread; `x` protected by the caller; the new list and names
    // are protected while filled; indices stay within the lengths.
    unsafe {
        build(move || {
            let n = rapi::Rf_xlength(x) as usize;
            let y = rapi::Rf_protect(rapi::Rf_allocVector(VECSXP, n as isize - 1));
            let old = rapi::Rf_getAttrib(x, rapi::R_NamesSymbol);
            let nn = rapi::Rf_protect(rapi::Rf_allocVector(STRSXP, n as isize - 1));
            let mut j = 0;
            for k in 0..n {
                if k == i {
                    continue;
                }
                rapi::SET_VECTOR_ELT(y, j, rapi::VECTOR_ELT(x, k as isize));
                rapi::SET_STRING_ELT(nn, j, rapi::STRING_ELT(old, k as isize));
                j += 1;
            }
            rapi::SHALLOW_DUPLICATE_ATTRIB(y, x);
            rapi::Rf_setAttrib(y, rapi::R_NamesSymbol, nn);
            rapi::Rf_unprotect(2);
            y
        })
    }
}

/// `x$tick <- t` on a shallow copy.
fn set_tick(x: SEXP, t: i32) -> savvy::Result<H> {
    // SAFETY: main thread; the scalar is kept before anything else allocates.
    let v = unsafe { build(move || rapi::Rf_ScalarInteger(t)) }?;
    // SAFETY: as above (installing/making the CHARSXP may allocate).
    let nm = unsafe { build(move || rapi::Rf_mkChar(c"tick".as_ptr())) }?;
    set_named(x, nm.get(), v.get())
}

/// The rows of an agent group (a data frame whose first column is `.id`).
fn rows(g: SEXP) -> Option<usize> {
    // SAFETY: reading a type tag.
    if unsafe { rapi::TYPEOF(g) } != VECSXP || len(g) == 0 {
        return None;
    }
    Some(len(elt(g, 0)))
}

/// The schema key: `c(length(globals), lengths(groups))`.
fn schema_key(st: SEXP) -> savvy::Result<Option<Vec<usize>>> {
    let (Some(gi), Some(li)) = (find_str(st, b"groups")?, find_str(st, b"globals")?) else {
        return Ok(None);
    };
    let groups = elt(st, gi);
    let mut key = vec![len(elt(st, li))];
    for k in 0..len(groups) {
        key.push(len(elt(groups, k)));
    }
    Ok(Some(key))
}

// ---- native steps ------------------------------------------------------------

/// One group's compiled rules (or, for a global step, the one program over all
/// groups): the program and the `CHARSXP`s of the names it reads and writes.
struct GroupPlan {
    name: SEXP,
    prog: *const RxProgram,
    cols: Vec<SEXP>,
    globs: Vec<SEXP>,
    targets: Vec<SEXP>,
    /// the formula-environment values it reads: positions in `scope_fn()`'s list
    scope: Vec<usize>,
    /// every rule elementwise (may run under a standing match)
    ew: bool,
    /// each rule in order: its place in `prog`, or -1 for a rule R evaluates
    /// (empty: every rule is in `prog`); `ridx` its index in `step$rules`
    order: Vec<i32>,
    ridx: Vec<i32>,
    /// the targets of every rule in `order`
    all_targets: Vec<SEXP>,
}

/// The R closures of a step whose rules R and Rust share (`rl_rule_fns()`).
struct Mixed {
    rule_fn: SEXP,
    rng_save: SEXP,
    rng_restore: SEXP,
}

/// A native step compiled by `prep` (`R/run-loop.R`, `rl_prep()`).
struct Plan {
    global: bool,
    /// `within` neighbourhoods of one candidate each (kind 5)
    nb1: Option<Nb1Plan>,
    /// `.where` lattice neighbourhoods (kind 6)
    nbw: Option<NbwPlan>,
    /// `.by` rules: the key column's name
    by: Option<SEXP>,
    nb: Option<NbPlan>,
    scope_match: bool,
    draws: bool,
    groups: Vec<GroupPlan>,
    /// R closure returning the formula-environment values (kept by `_desc`)
    scope_fn: Option<SEXP>,
    mixed: Option<Mixed>,
    // the descriptor: keeps the names' CHARSXPs and the programs alive
    _desc: H,
}

/// A `within = <col> == own_<col> [& rest]` step whose candidates are distinct
/// ([nb_one()]): the program evaluates the rest (if any) then each rule's argument.
struct Nb1Plan {
    focal: SEXP,
    cand: SEXP,
    cand_is_id: bool,
    rest: bool,
    /// per rule: 0 `any`, 1 `all`, 2 `sum`, 3 `n`
    fns: Vec<u8>,
    /// per rule: a bare `n()` (an agent without a candidate counts 0)
    counts: Vec<bool>,
}

/// A `.where = <direction>` step: the lattice's wired group, its neighbour
/// table for the direction (cell id -> cell id, `NA` at an edge), the lattice
/// it came from, and per rule 0 `any`, 1 `all`, 2 `sum`, 3 `n`, 4 `mean`.
struct NbwPlan {
    on: SEXP,
    dir: SEXP,
    lattice: SEXP,
    fns: Vec<u8>,
}

/// A network `abm_neighbours()` step whose rules are `sum()`/`mean()` of bare
/// double columns (the shapes P4's `rk_neighbours()` folded in Rust), with the
/// neighbour index and `state$nb_cache` that R builds for these edges and this
/// many agents.
struct NbPlan {
    mean: Vec<bool>,
    targets: Vec<SEXP>,
    cols: Vec<SEXP>,
    cnt: SEXP,
    cache: SEXP,
    cache_name: SEXP,
    edges: SEXP,
    n: usize,
}

fn bad(what: &str) -> savvy::Error {
    typed_error(format!("malformed native step: {what}"))
}

fn parse_nb(nb: SEXP) -> savvy::Result<NbPlan> {
    let mean = field(nb, b"mean")?;
    // SAFETY: reading a type tag.
    if unsafe { rapi::TYPEOF(mean) } != rapi::LGLSXP {
        return Err(bad("nb mean"));
    }
    let k = len(mean);
    // SAFETY: a protected logical vector of length `k`.
    let mean: Vec<bool> = (0..k)
        .map(|i| unsafe { *rapi::LOGICAL_RO(mean).add(i) } == 1)
        .collect();
    let cl = field(nb, b"cols")?;
    let cols = (0..len(cl)).map(|j| elt(cl, j)).collect();
    let n = store::numbers_of::<i32>(field(nb, b"n")?, INTSXP)?
        .and_then(|v| v.first().copied())
        .ok_or_else(|| bad("nb n"))?;
    let plan = NbPlan {
        mean,
        targets: chars(field(nb, b"targets")?)?,
        cols,
        cnt: field(nb, b"cnt")?,
        cache: field(nb, b"cache")?,
        cache_name: chars(field(nb, b"cache_name")?)?
            .first()
            .copied()
            .ok_or_else(|| bad("nb cache name"))?,
        edges: field(nb, b"edges")?,
        n: usize::try_from(n).map_err(|_| bad("nb n"))?,
    };
    if plan.targets.len() != k {
        return Err(bad("nb rules"));
    }
    Ok(plan)
}

fn field(x: SEXP, name: &[u8]) -> savvy::Result<SEXP> {
    let i = find_str(x, name)?.ok_or_else(|| bad("missing field"))?;
    Ok(elt(x, i))
}

fn chars(x: SEXP) -> savvy::Result<Vec<SEXP>> {
    // SAFETY: reading a type tag.
    if unsafe { rapi::TYPEOF(x) } != STRSXP {
        return Err(bad("names that are not a character vector"));
    }
    Ok(store::chars_of(x)?.to_vec())
}

/// Decode `prep`'s descriptor: `list(kind, scope_match, groups = list(list(name,
/// prog, cols, globs, targets), ...))`, `prog` an `RxProgram`'s external pointer.
fn parse_plan(desc: H) -> savvy::Result<Plan> {
    let d = desc.get();
    let kind = store::numbers_of::<i32>(field(d, b"kind")?, INTSXP)?
        .and_then(|k| k.first().copied())
        .ok_or_else(|| bad("kind"))?;
    let sm = field(d, b"scope_match")?;
    // SAFETY: reading a type tag.
    let scope_match = unsafe { rapi::TYPEOF(sm) } == rapi::LGLSXP
        && len(sm) == 1
        // SAFETY: a logical vector of length 1.
        && unsafe { *rapi::LOGICAL_RO(sm) } == 1;
    let gs = field(d, b"groups")?;
    let mut groups = Vec::with_capacity(len(gs));
    let mut draws = false;
    for k in 0..len(gs) {
        let g = elt(gs, k);
        let ptr = field(g, b"prog")?;
        // SAFETY: an external pointer made by savvy for an `RxProgram`, kept alive
        // by the descriptor (and `R/rust-expr.R`'s memo); savvy's own wrappers
        // read it the same way.
        let prog = unsafe { savvy::get_external_pointer_addr(ptr) }? as *const RxProgram;
        // SAFETY: as just above; the program is not mutated while borrowed.
        draws |= unsafe { &*prog }.draws_any();
        let name = field(g, b"name")?;
        let scope = store::numbers_of::<i32>(field(g, b"scope")?, INTSXP)?
            .unwrap_or(&[])
            .iter()
            .map(|&k| usize::try_from(k).map_err(|_| bad("scope index")))
            .collect::<savvy::Result<Vec<_>>>()?;
        let ew = field(g, b"ew")?;
        let ints = |nm: &[u8]| -> savvy::Result<Vec<i32>> {
            Ok(match find_str(g, nm)? {
                Some(i) => store::numbers_of::<i32>(elt(g, i), INTSXP)?
                    .unwrap_or(&[])
                    .to_vec(),
                None => Vec::new(),
            })
        };
        let all_targets = match find_str(g, b"all_targets")? {
            Some(i) => chars(elt(g, i))?,
            None => Vec::new(),
        };
        groups.push(GroupPlan {
            order: ints(b"order")?,
            ridx: ints(b"ridx")?,
            all_targets,
            name: chars(name)?.first().copied().ok_or_else(|| bad("name"))?,
            prog,
            cols: chars(field(g, b"cols")?)?,
            globs: chars(field(g, b"globs")?)?,
            targets: chars(field(g, b"targets")?)?,
            scope,
            // SAFETY: reading a type tag, then a logical of length 1.
            ew: unsafe { rapi::TYPEOF(ew) } == rapi::LGLSXP
                && len(ew) == 1
                && unsafe { *rapi::LOGICAL_RO(ew) } == 1,
        });
    }
    let nb = if kind == 3 {
        Some(parse_nb(field(d, b"nb")?)?)
    } else {
        None
    };
    let scope_fn = find_str(d, b"scope_fn")?.map(|i| elt(d, i));
    let mixed = match find_str(d, b"rule_fn")? {
        Some(i) => Some(Mixed {
            rule_fn: elt(d, i),
            rng_save: field(d, b"rng_save")?,
            rng_restore: field(d, b"rng_restore")?,
        }),
        None => None,
    };
    // an R-evaluated rule may draw
    let draws = draws || mixed.is_some();
    let nb1 = if kind == 5 {
        let x = field(d, b"nb1")?;
        let one = |nm: &[u8]| -> savvy::Result<SEXP> {
            chars(field(x, nm)?)?
                .first()
                .copied()
                .ok_or_else(|| bad("nb1"))
        };
        let lgl = |nm: &[u8]| -> savvy::Result<Vec<bool>> {
            let v = field(x, nm)?;
            // SAFETY: reading a type tag.
            if unsafe { rapi::TYPEOF(v) } != rapi::LGLSXP {
                return Err(bad("nb1 logical"));
            }
            // SAFETY: a protected logical vector of `len(v)` elements.
            Ok((0..len(v))
                .map(|i| unsafe { *rapi::LOGICAL_RO(v).add(i) } == 1)
                .collect())
        };
        let cand = one(b"cand")?;
        let fns = chars(field(x, b"fns")?)?
            .iter()
            .map(|&c| match bytes(c) {
                b"any" => Ok(0),
                b"all" => Ok(1),
                b"sum" => Ok(2),
                b"n" => Ok(3),
                _ => Err(bad("nb1 fn")),
            })
            .collect::<savvy::Result<Vec<u8>>>()?;
        let counts = lgl(b"counts")?;
        if counts.len() != fns.len() {
            return Err(bad("nb1 rules"));
        }
        Some(Nb1Plan {
            focal: one(b"focal")?,
            cand,
            cand_is_id: bytes(cand) == b".id",
            rest: lgl(b"rest")?.first().copied().unwrap_or(false),
            fns,
            counts,
        })
    } else {
        None
    };
    let nbw = if kind == 6 {
        let x = field(d, b"nbw")?;
        let fns = chars(field(x, b"fns")?)?
            .iter()
            .map(|&c| match bytes(c) {
                b"any" => Ok(0),
                b"all" => Ok(1),
                b"sum" => Ok(2),
                b"n" => Ok(3),
                b"mean" => Ok(4),
                _ => Err(bad("nbw fn")),
            })
            .collect::<savvy::Result<Vec<u8>>>()?;
        Some(NbwPlan {
            on: chars(field(x, b"on")?)?
                .first()
                .copied()
                .ok_or_else(|| bad("nbw on"))?,
            dir: field(x, b"dir")?,
            lattice: field(x, b"lattice")?,
            fns,
        })
    } else {
        None
    };
    let by = if kind == 4 {
        Some(
            chars(field(d, b"by")?)?
                .first()
                .copied()
                .ok_or_else(|| bad("by"))?,
        )
    } else {
        None
    };
    Ok(Plan {
        mixed,
        nb1,
        nbw,
        by,
        global: kind == 2,
        nb,
        scope_match,
        draws,
        groups,
        scope_fn,
        _desc: desc,
    })
}

/// Why the step declined (kept for `rs_rx_reason()`).
fn decline(why: &'static str) -> savvy::Result<Option<H>> {
    expr::note_decline(&Decline(why));
    Ok(None)
}

/// Bare input vectors (checked together) as evaluator values, or `None`.
fn inputs<'a>(xs: &[SEXP]) -> savvy::Result<Option<Vec<Vals<'a>>>> {
    if !all_bare(xs)? {
        return Ok(None);
    }
    let mut out = Vec::with_capacity(xs.len());
    for &x in xs {
        match expr::vals_of(x, false)? {
            Some(v) => out.push(v),
            None => return Ok(None),
        }
    }
    Ok(Some(out))
}

/// The globals a plan reads, appended to `ins`; `false` when one is missing.
fn read_globals(st: SEXP, globs: &[SEXP], ins: &mut Vec<SEXP>) -> savvy::Result<bool> {
    if globs.is_empty() {
        return Ok(true);
    }
    let Some(li) = find_str(st, b"globals")? else {
        return Ok(false);
    };
    let gl = elt(st, li);
    let names = names_of(gl)?;
    for &g in globs {
        let Some(j) = find(names, g) else {
            return Ok(false);
        };
        ins.push(elt(gl, j));
    }
    Ok(true)
}

/// The formula-environment values of `plan` (an R list kept by the returned
/// handle), `Ok(None)` when they cannot be read (the step declines); an empty
/// handle-less `Ok(Some(None))` when the plan reads none.
fn scope_values(plan: &Plan) -> savvy::Result<Option<Option<H>>> {
    let Some(f) = plan.scope_fn else {
        return Ok(Some(None));
    };
    // SAFETY: main thread; `f` is kept by the plan's descriptor.
    let v = unsafe {
        build(move || {
            let call = rapi::Rf_protect(rapi::Rf_lcons(f, rapi::R_NilValue));
            let res = rapi::Rf_eval(call, rapi::R_GlobalEnv);
            rapi::Rf_unprotect(1);
            res
        })
    }?;
    // SAFETY: reading a type tag.
    if unsafe { rapi::TYPEOF(v.get()) } != VECSXP {
        return Ok(None);
    }
    Ok(Some(Some(v)))
}

/// Append the scope values `idx` of `vals` to `xs`; `false` when one is missing.
fn read_scope(vals: &Option<H>, idx: &[usize], xs: &mut Vec<SEXP>) -> bool {
    if idx.is_empty() {
        return true;
    }
    let Some(v) = vals else { return false };
    let v = v.get();
    for &k in idx {
        if k >= len(v) {
            return false;
        }
        xs.push(elt(v, k));
    }
    true
}

/// R closure `f()` called for its effect.
fn call0(f: SEXP) -> savvy::Result<()> {
    // SAFETY: main thread; `f` is kept by the plan's descriptor; the value is
    // not used.
    unsafe { eval_r(f, &[]) }.map(|_| ())
}

/// `rule_fn(g, globals, i, multi)` (`rl_rule_fns()`): one rule's value from R.
fn call_rule(f: SEXP, g: SEXP, gl: SEXP, i: i32, multi: i32) -> savvy::Result<H> {
    // SAFETY: main thread; `f` kept by the descriptor, `g` and `gl` by the state;
    // everything allocated is protected until the call has run; the value is
    // kept inside the same protected call.
    unsafe {
        build(move || {
            let m = rapi::Rf_protect(rapi::Rf_ScalarInteger(multi));
            let iv = rapi::Rf_protect(rapi::Rf_ScalarInteger(i));
            let a = rapi::Rf_protect(rapi::Rf_cons(m, rapi::R_NilValue));
            let a = rapi::Rf_protect(rapi::Rf_cons(iv, a));
            let a = rapi::Rf_protect(rapi::Rf_cons(gl, a));
            let a = rapi::Rf_protect(rapi::Rf_cons(g, a));
            let call = rapi::Rf_protect(rapi::Rf_lcons(f, a));
            let res = rapi::Rf_eval(call, rapi::R_GlobalEnv);
            rapi::Rf_unprotect(7);
            res
        })
    }
}

/// An R rule's value as `assign_rule()` takes it: an atomic vector of length 1
/// or `n` with no attribute but names (which it drops); `None` otherwise.
fn r_value<'a>(x: SEXP, n: usize) -> savvy::Result<Option<Vals<'a>>> {
    // SAFETY: reading a type tag.
    let t = unsafe { rapi::TYPEOF(x) };
    if !matches!(t, rapi::LGLSXP | INTSXP | rapi::REALSXP | STRSXP) || !(len(x) == n || len(x) == 1)
    {
        return Ok(None);
    }
    // SAFETY: main thread; `x` is kept by the caller; the two new vectors are
    // protected while compared.
    let bare = unsafe {
        rapi::protect_value(0, move || {
            let a = rapi::Rf_protect(rapi::Rf_allocVector(t, 0));
            rapi::SHALLOW_DUPLICATE_ATTRIB(a, x);
            rapi::Rf_setAttrib(a, rapi::R_NamesSymbol, rapi::R_NilValue);
            let b = rapi::Rf_protect(rapi::Rf_allocVector(t, 0));
            let same = rapi::R_compute_identical(a, b, 16);
            rapi::Rf_unprotect(2);
            same
        })
    }?;
    if bare == 0 {
        return Ok(None);
    }
    expr::vals_of(x, false)
}

/// `abm_rules()` natively (the shapes of slice 1's `rk_rules()`): every group's
/// rules from the start-of-step columns, then the writes. `None` = declined.
fn exec_rules(plan: &Plan, st: SEXP) -> savvy::Result<Option<H>> {
    if let Some(ri) = find_str(st, b"relations")? {
        if len(elt(st, ri)) > 0 {
            return decline("a relation");
        }
    }
    // a standing match with pairs narrows a match-scope step to the agents it
    // placed (`run_rules()`: rows `!is.na(.group_id)`); elementwise rules give
    // the same values over the whole group, others go to R
    let mut placed: Option<std::collections::HashMap<i32, bool>> = None;
    if plan.scope_match {
        if let Some(mi) = find_str(st, b"match")? {
            let m = elt(st, mi);
            if m != rapi_nil() {
                let Some(mm) = find_str(m, b"match")?.map(|i| elt(m, i)) else {
                    return decline("a match without its table");
                };
                if rows(mm).unwrap_or(0) > 0 {
                    if plan.groups.iter().any(|g| !g.ew) {
                        return decline("a rule that is not elementwise under a standing match");
                    }
                    let col = |nm: &[u8]| -> savvy::Result<Option<&[i32]>> {
                        match find_str(mm, nm)? {
                            Some(j) => store::numbers_of::<i32>(elt(mm, j), INTSXP),
                            None => Ok(None),
                        }
                    };
                    let (Some(ids), Some(gid)) = (col(b".id")?, col(b".group_id")?) else {
                        return decline("a match table of another shape");
                    };
                    let mut map = std::collections::HashMap::with_capacity(ids.len());
                    for (&id, &g) in ids.iter().zip(gid) {
                        // match() takes the first row of an id
                        map.entry(id).or_insert(g != tidyabm_core::sample::NA_INT);
                    }
                    placed = Some(map);
                }
            }
        }
    }
    let Some(svals) = scope_values(plan)? else {
        return decline("a formula-environment value that cannot be read");
    };
    let gi = find_str(st, b"groups")?.ok_or_else(|| bad("no groups"))?;
    let groups = elt(st, gi);
    let gnames = names_of(groups)?;
    let mut results: Vec<(usize, OwnedListSexp, &[SEXP])> = Vec::new();
    let mut applied = false;
    // a step mixing R and Rust rules: an R rule has run / the rest is R's
    let (mut r_ran, mut r_mode) = (false, false);
    for gp in &plan.groups {
        let Some(k) = find(gnames, gp.name) else {
            return decline("a group not found");
        };
        let g = elt(groups, k);
        let Some(n) = rows(g) else {
            return decline("a group without columns");
        };
        if n == 0 {
            continue;
        }
        applied = true;
        let cn = names_of(g)?;
        let mut xs = Vec::with_capacity(gp.cols.len() + gp.globs.len());
        for &c in &gp.cols {
            let Some(j) = find(cn, c) else {
                return decline("a column not found");
            };
            xs.push(elt(g, j));
        }
        if !read_globals(st, &gp.globs, &mut xs)? {
            return decline("a global not found");
        }
        if !read_scope(&svals, &gp.scope, &mut xs) {
            return decline("a formula-environment value not found");
        }
        let Some(ins) = inputs(&xs)? else {
            return decline("an input that is not a bare vector");
        };
        // SAFETY: the program is kept alive by the plan's descriptor.
        let prog = unsafe { &*gp.prog };
        // R values of R-evaluated rules, kept while they are read
        let mut held: Vec<H> = Vec::new();
        let (res, targets): (Vec<Vals>, &[SEXP]) = match &plan.mixed {
            Some(mx) if gp.order.iter().any(|&o| o < 0) || r_ran => {
                let multi = i32::from(gp.order.len() > 1);
                let mut vals = Vec::with_capacity(gp.order.len());
                for (pos, &o) in gp.order.iter().enumerate() {
                    if o >= 0 && !r_mode {
                        let k = o as usize;
                        let save = r_ran && prog.draws_rule(k);
                        if save {
                            call0(mx.rng_save)?;
                        }
                        match prog.compute_one(k, &ins, n)? {
                            Ok(v) => {
                                vals.push(v);
                                continue;
                            }
                            Err(d) if !r_ran => {
                                expr::note_decline(&d);
                                return Ok(None);
                            }
                            Err(_) => {
                                // an R rule has run (its effects stay): this rule
                                // and the rest are R's, from the stream before it
                                if save {
                                    call0(mx.rng_restore)?;
                                }
                                r_mode = true;
                            }
                        }
                    }
                    let gl = find_str(st, b"globals")?.map_or_else(rapi_nil, |i| elt(st, i));
                    let v = call_rule(mx.rule_fn, g, gl, gp.ridx[pos], multi)?;
                    r_ran = true;
                    match r_value(v.get(), n)? {
                        Some(x) => {
                            vals.push(x);
                            held.push(v);
                        }
                        None => return decline("an R-evaluated rule gave another kind of value"),
                    }
                }
                (vals, &gp.all_targets)
            }
            _ => match prog.compute(ins, n, n)? {
                Ok(r) => (r, &gp.targets),
                Err(d) => {
                    expr::note_decline(&d);
                    return Ok(None);
                }
            },
        };
        let mut out = OwnedListSexp::new(res.len(), false)?;
        // the rows the step writes, when a match narrows it
        let mask: Option<Vec<bool>> = match &placed {
            None => None,
            Some(map) => {
                let Some(j) = find_str(g, b".id")? else {
                    return decline("a group without ids");
                };
                let Some(ids) = store::numbers_of::<i32>(elt(g, j), INTSXP)? else {
                    return decline("ids that are not integers");
                };
                let m: Vec<bool> = ids
                    .iter()
                    .map(|id| *map.get(id).unwrap_or(&false))
                    .collect();
                if m.iter().all(|&b| b) {
                    None
                } else {
                    Some(m)
                }
            }
        };
        for (i, v) in res.iter().enumerate() {
            match &mask {
                None => out.set_value(i, expr::to_r(v, n)?)?,
                Some(m) => {
                    let old = match find(cn, targets[i]) {
                        Some(j) => {
                            let c = elt(g, j);
                            if !all_bare(&[c])? {
                                return decline("a target column with attributes");
                            }
                            match expr::vals_of(c, false)? {
                                Some(v) => Some(v),
                                None => return decline("a target column of another type"),
                            }
                        }
                        None => None,
                    };
                    match expr::assign_rows(old, v, m)? {
                        Ok(col) => out.set_value(i, expr::to_r(&col, n)?)?,
                        Err(d) => {
                            expr::note_decline(&d);
                            return Ok(None);
                        }
                    }
                }
            }
        }
        drop(held);
        results.push((k, out, targets));
    }
    if !applied && (0..len(groups)).any(|k| rows(elt(groups, k)).unwrap_or(0) > 0) {
        // nobody to apply the rules to: the R engine's error to raise
        return decline("no group to apply the rules to");
    }
    // commit
    let mut cur: Option<H> = None;
    for (k, out, targets) in &results {
        let gs = cur.as_ref().map_or(groups, |h| h.get());
        let mut g: Option<H> = None;
        for (i, &t) in targets.iter().enumerate() {
            let base = g.as_ref().map_or_else(|| elt(gs, *k), |h| h.get());
            g = Some(set_named(base, t, elt(out.inner(), i))?);
        }
        if let Some(g) = g {
            // SAFETY: reading a static of R's (unused name: `k` is in range).
            cur = Some(with_elt(gs, *k, g.get(), unsafe { rapi::R_BlankString })?);
        }
    }
    match cur {
        // SAFETY: as above.
        Some(gs) => Ok(Some(with_elt(st, gi, gs.get(), unsafe {
            rapi::R_BlankString
        })?)),
        None => Ok(Some(keep(st)?)),
    }
}

/// `abm_global()` natively (slice 1's `rk_global()`, also for one group): each
/// rule over all groups stacked, the later rules seeing the earlier writes.
fn exec_global(plan: &Plan, st: SEXP, values_only: bool) -> savvy::Result<Option<H>> {
    let gp = plan.groups.first().ok_or_else(|| bad("no program"))?;
    let gi = find_str(st, b"groups")?.ok_or_else(|| bad("no groups"))?;
    let li = find_str(st, b"globals")?.ok_or_else(|| bad("no globals"))?;
    let groups = elt(st, gi);
    let gl = elt(st, li);
    // SAFETY: reading a type tag.
    if unsafe { rapi::TYPEOF(gl) } != VECSXP {
        return decline("globals that are not a list");
    }
    let ng = len(groups);
    let mut n = 0;
    for k in 0..ng {
        match rows(elt(groups, k)) {
            Some(r) => n += r,
            None => return decline("a group without columns"),
        }
    }
    if n == 0 {
        return decline("no agents");
    }
    // every column of every group the step reads, then the globals: checked bare
    let mut xs = Vec::with_capacity(gp.cols.len() * ng + gp.globs.len());
    for &c in &gp.cols {
        for k in 0..ng {
            let g = elt(groups, k);
            let Some(j) = find(names_of(g)?, c) else {
                return decline("a column some group lacks");
            };
            xs.push(elt(g, j));
        }
    }
    let n_cols = xs.len();
    if !read_globals(st, &gp.globs, &mut xs)? {
        return decline("a global not found");
    }
    let Some(svals) = scope_values(plan)? else {
        return decline("a formula-environment value that cannot be read");
    };
    if !read_scope(&svals, &gp.scope, &mut xs) {
        return decline("a formula-environment value not found");
    }
    if !all_bare(&xs)? {
        return decline("an input that is not a bare vector");
    }
    let mut ins = Vec::with_capacity(gp.cols.len() + gp.globs.len());
    for parts in xs[..n_cols].chunks(ng.max(1)) {
        let v = if ng == 1 {
            expr::vals_of(parts[0], false)?
        } else {
            let mut l = OwnedListSexp::new(ng, false)?;
            for (k, &x) in parts.iter().enumerate() {
                l.set_value(k, Sexp(x))?;
            }
            expr::stacked(l.inner())?
        };
        match v {
            Some(v) => ins.push(v),
            None => return decline("an input of an unsupported type"),
        }
    }
    for &x in &xs[n_cols..] {
        match expr::vals_of(x, false)? {
            Some(v) => ins.push(v),
            None => return decline("a global or scope value of an unsupported type"),
        }
    }
    // SAFETY: the program is kept alive by the plan's descriptor.
    let prog = unsafe { &*gp.prog };
    let res = match prog.compute(ins, n, 1)? {
        Ok(r) => r,
        Err(d) => {
            expr::note_decline(&d);
            return Ok(None);
        }
    };
    let mut out = OwnedListSexp::new(res.len(), false)?;
    for (i, v) in res.iter().enumerate() {
        out.set_value(i, expr::to_r(v, 1)?)?;
    }
    if values_only {
        // measures and checks (FR9b): the values, nothing committed
        return Ok(Some(keep(out.inner())?));
    }
    let mut cur: Option<H> = None;
    for (i, &t) in gp.targets.iter().enumerate() {
        let base = cur.as_ref().map_or(gl, |h| h.get());
        cur = Some(set_named(base, t, elt(out.inner(), i))?);
    }
    match cur {
        // SAFETY: reading a static of R's (unused name: `li` is in range).
        Some(g) => Ok(Some(with_elt(st, li, g.get(), unsafe {
            rapi::R_BlankString
        })?)),
        None => Ok(Some(keep(st)?)),
    }
}

/// Column `name` of every group stacked (`None` when a group lacks it, a column
/// has attributes or an unsupported type).
fn stacked_col<'a>(groups: SEXP, name: SEXP) -> savvy::Result<Option<Vals<'a>>> {
    let ng = len(groups);
    let mut parts = Vec::with_capacity(ng);
    for k in 0..ng {
        let g = elt(groups, k);
        let Some(j) = find(names_of(g)?, name) else {
            return Ok(None);
        };
        parts.push(elt(g, j));
    }
    if !all_bare(&parts)? {
        return Ok(None);
    }
    if ng == 1 {
        return expr::vals_of(parts[0], false);
    }
    let mut l = OwnedListSexp::new(ng, false)?;
    for (k, &p) in parts.iter().enumerate() {
        l.set_value(k, Sexp(p))?;
    }
    expr::stacked(l.inner())
}

/// Column `name` of every group stacked as `vctrs::vec_rbind()` stacks it: `NA`
/// for a group that lacks it. `None` (decline) when no group has it, the groups
/// hold it in different types, or a column has attributes.
fn stacked_pad(groups: SEXP, name: SEXP, sizes: &[usize]) -> savvy::Result<Option<Vals<'static>>> {
    use std::borrow::Cow;
    let ng = len(groups);
    let mut parts: Vec<Option<SEXP>> = Vec::with_capacity(ng);
    for k in 0..ng {
        let g = elt(groups, k);
        parts.push(find(names_of(g)?, name).map(|j| elt(g, j)));
    }
    let present: Vec<SEXP> = parts.iter().flatten().copied().collect();
    if present.is_empty() || !all_bare(&present)? {
        return Ok(None);
    }
    // SAFETY: reading type tags.
    let t = unsafe { rapi::TYPEOF(present[0]) };
    // SAFETY: as above.
    if present.iter().any(|&p| unsafe { rapi::TYPEOF(p) } != t) {
        return Ok(None);
    }
    // SAFETY: reading statics of R's (its NA_real_ and NA_character_).
    let (na_real, na_str) = unsafe { (rapi::R_NaReal, rapi::R_NaString as usize) };
    let mut out: Option<Vals<'static>> = None;
    for (k, p) in parts.iter().enumerate() {
        let v: Vals<'static> = match p {
            Some(x) => match expr::vals_of(*x, false)? {
                Some(v) => expr::owned(v),
                None => return Ok(None),
            },
            None => match t {
                rapi::LGLSXP => Vals::Lgl(Cow::Owned(vec![tidyabm_core::sample::NA_INT; sizes[k]])),
                INTSXP => Vals::Int(Cow::Owned(vec![tidyabm_core::sample::NA_INT; sizes[k]])),
                rapi::REALSXP => Vals::Dbl(Cow::Owned(vec![na_real; sizes[k]])),
                STRSXP => Vals::Str(Cow::Owned(vec![na_str; sizes[k]])),
                _ => return Ok(None),
            },
        };
        out = Some(match (out, v) {
            (None, v) => v,
            (Some(Vals::Lgl(mut a)), Vals::Lgl(b)) => {
                a.to_mut().extend_from_slice(&b);
                Vals::Lgl(a)
            }
            (Some(Vals::Int(mut a)), Vals::Int(b)) => {
                a.to_mut().extend_from_slice(&b);
                Vals::Int(a)
            }
            (Some(Vals::Dbl(mut a)), Vals::Dbl(b)) => {
                a.to_mut().extend_from_slice(&b);
                Vals::Dbl(a)
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

/// `abm_neighbours(within = <col> == own_<col> [& rest])` natively, as
/// [nb_one()] answers it: each focal agent's one candidate (its `<col>` matched
/// against the candidates' distinct values), kept where the rest holds there,
/// and each rule's argument read off at that candidate; written to every group,
/// `NA` (`0` for a bare `n()`) for an agent without one.
fn exec_nb1(plan: &Plan, st: SEXP) -> savvy::Result<Option<H>> {
    use std::borrow::Cow;
    use tidyabm_core::sample::NA_INT;
    let nb = plan.nb1.as_ref().ok_or_else(|| bad("no nb1"))?;
    let gp = plan.groups.first().ok_or_else(|| bad("no program"))?;
    if let Some(ri) = find_str(st, b"relations")? {
        if len(elt(st, ri)) > 0 {
            return decline("a relation");
        }
    }
    let gi = find_str(st, b"groups")?.ok_or_else(|| bad("no groups"))?;
    let groups = elt(st, gi);
    let ng = len(groups);
    let mut sizes = Vec::with_capacity(ng);
    for k in 0..ng {
        match rows(elt(groups, k)) {
            Some(r) => sizes.push(r),
            None => return decline("a group without columns"),
        }
    }
    let n: usize = sizes.iter().sum();
    if n == 0 {
        return Ok(Some(keep(st)?));
    }
    let (Some(Vals::Int(focal)), Some(Vals::Int(cand))) = (
        stacked_pad(groups, nb.focal, &sizes)?,
        stacked_pad(groups, nb.cand, &sizes)?,
    ) else {
        return decline("join columns that are not integers");
    };
    // match(focal, cand): the first candidate of a value (NA matches NA, as in
    // match()); candidates other than `.id` must be distinct
    let mut at: std::collections::HashMap<i32, usize> = std::collections::HashMap::with_capacity(n);
    for (j, &c) in cand.iter().enumerate() {
        if at.insert(c, j).is_some() && !nb.cand_is_id && c != NA_INT {
            return decline("candidates that are not distinct");
        }
    }
    // keep the first of a repeated value (match() semantics)
    for (j, &c) in cand.iter().enumerate().rev() {
        at.insert(c, j);
    }
    let mut ins: Vec<Vals> = Vec::with_capacity(gp.cols.len());
    for &c in &gp.cols {
        match stacked_pad(groups, c, &sizes)? {
            Some(v) => ins.push(v),
            None => return decline("a column of another kind"),
        }
    }
    // SAFETY: the program is kept alive by the plan's descriptor.
    let prog = unsafe { &*gp.prog };
    let res = match prog.compute(ins, n, n)? {
        Ok(r) => r,
        Err(d) => {
            expr::note_decline(&d);
            return Ok(None);
        }
    };
    let pick = |v: &Vals, i: usize| -> usize {
        if v.len() == 1 {
            0
        } else {
            i
        }
    };
    // the focal rows with a candidate (where the rest holds there), in row order
    let mut pairs: Vec<(usize, usize)> = Vec::new();
    for (i, f) in focal.iter().enumerate() {
        if let Some(&j) = at.get(f) {
            pairs.push((i, j));
        }
    }
    let mut next = 0;
    if nb.rest {
        let Vals::Lgl(k) = &res[0] else {
            return decline("a `within` condition that is not logical");
        };
        pairs.retain(|&(_, j)| k[pick(&res[0], j)] == 1);
        next = 1;
    }
    if pairs.is_empty() {
        return decline("nobody with a candidate");
    }
    // per rule: the value at each focal row (`None` = no candidate)
    let mut slot = vec![usize::MAX; n];
    for (p, &(i, _)) in pairs.iter().enumerate() {
        slot[i] = p;
    }
    let mut outs: Vec<Vals> = Vec::with_capacity(nb.fns.len());
    for (r, &f) in nb.fns.iter().enumerate() {
        let v = match f {
            3 => Vals::Int(Cow::Owned(vec![1; pairs.len()])),
            _ => {
                let x = &res[next];
                next += 1;
                match (f, x) {
                    (0 | 1, Vals::Lgl(a)) => Vals::Lgl(Cow::Owned(
                        pairs.iter().map(|&(_, j)| a[pick(x, j)]).collect(),
                    )),
                    (0 | 1, _) => return decline("any()/all() of a value that is not logical"),
                    (_, Vals::Dbl(a)) => Vals::Dbl(Cow::Owned(
                        pairs.iter().map(|&(_, j)| 0.0 + a[pick(x, j)]).collect(),
                    )),
                    (_, Vals::Int(a) | Vals::Lgl(a)) => Vals::Int(Cow::Owned(
                        pairs.iter().map(|&(_, j)| a[pick(x, j)]).collect(),
                    )),
                    _ => return decline("sum() of strings"),
                }
            }
        };
        let _ = r;
        outs.push(v);
    }
    // SAFETY: reading a static of R's (its NA_real_).
    let na_real = unsafe { rapi::R_NaReal };
    let mut cur: Option<H> = None;
    for (r, v) in outs.iter().enumerate() {
        let t = gp.targets[r];
        let miss_zero = nb.counts[r];
        let mut start = 0;
        for (g, &sz) in sizes.iter().enumerate() {
            let rows_g = start..start + sz;
            start += sz;
            if sz == 0 {
                continue;
            }
            let col = match v {
                Vals::Lgl(a) => Vals::Lgl(Cow::Owned(
                    rows_g
                        .map(|i| {
                            if slot[i] == usize::MAX {
                                NA_INT
                            } else {
                                a[slot[i]]
                            }
                        })
                        .collect(),
                )),
                Vals::Int(a) => Vals::Int(Cow::Owned(
                    rows_g
                        .map(|i| match slot[i] {
                            usize::MAX if miss_zero => 0,
                            usize::MAX => NA_INT,
                            p => a[p],
                        })
                        .collect(),
                )),
                Vals::Dbl(a) => Vals::Dbl(Cow::Owned(
                    rows_g
                        .map(|i| {
                            if slot[i] == usize::MAX {
                                na_real
                            } else {
                                a[slot[i]]
                            }
                        })
                        .collect(),
                )),
                Vals::Str(_) => return Err(bad("a string neighbour value")),
            };
            let gs = cur.as_ref().map_or(groups, |h| h.get());
            let base = elt(gs, g);
            // the holder exists before the new column (`to_r()` returns it
            // unprotected)
            let mut hold = OwnedListSexp::new(1, false)?;
            hold.set_value(0, expr::to_r(&col, sz)?)?;
            let g2 = set_named(base, t, elt(hold.inner(), 0))?;
            // SAFETY: reading a static of R's (unused name: `g` is in range).
            cur = Some(with_elt(gs, g, g2.get(), unsafe { rapi::R_BlankString })?);
        }
    }
    match cur {
        // SAFETY: as above.
        Some(gs) => Ok(Some(with_elt(st, gi, gs.get(), unsafe {
            rapi::R_BlankString
        })?)),
        None => Ok(Some(keep(st)?)),
    }
}

/// `abm_neighbours(.where = )` natively ([directional_view()] then
/// [neighbour_aggregate()] over neighbourhoods of one): a wired cell's own id,
/// else a mover's `.cell`, looked up in the direction's table, the candidate
/// the agent with that id; each rule's argument evaluated over the (focal,
/// candidate) pairs, `own_<col>` from the focal agent; `NA` for an agent
/// without a neighbour there.
fn exec_nbw(plan: &Plan, st: SEXP) -> savvy::Result<Option<H>> {
    use std::borrow::Cow;
    use tidyabm_core::sample::NA_INT;
    let nb = plan.nbw.as_ref().ok_or_else(|| bad("no nbw"))?;
    let gp = plan.groups.first().ok_or_else(|| bad("no program"))?;
    if let Some(ri) = find_str(st, b"relations")? {
        if len(elt(st, ri)) > 0 {
            return decline("a relation");
        }
    }
    match find_str(st, b"lattice")? {
        Some(li) if elt(st, li) == nb.lattice => {}
        _ => return decline("the lattice changed"),
    }
    let gi = find_str(st, b"groups")?.ok_or_else(|| bad("no groups"))?;
    let groups = elt(st, gi);
    let ng = len(groups);
    let mut sizes = Vec::with_capacity(ng);
    for k in 0..ng {
        match rows(elt(groups, k)) {
            Some(r) => sizes.push(r),
            None => return decline("a group without columns"),
        }
    }
    let n: usize = sizes.iter().sum();
    if n == 0 {
        return Ok(Some(keep(st)?));
    }
    let id_name = names_of(elt(groups, 0))?
        .first()
        .copied()
        .ok_or_else(|| bad("no .id"))?;
    let (Some(Vals::Int(ids)), Some(Vals::Str(grp))) = (
        stacked_pad(groups, id_name, &sizes)?,
        stacked_col_named(groups, b".group", &sizes)?,
    ) else {
        return decline("ids or groups of another kind");
    };
    if bytes(id_name) != b".id" {
        return decline("a group whose first column is not .id");
    }
    let cell = stacked_col_named(groups, b".cell", &sizes)?;
    let cell: Option<Cow<[i32]>> = match cell {
        None => None,
        Some(Vals::Int(c)) => Some(c),
        Some(_) => return decline("a .cell that is not integer"),
    };
    let dir = store::numbers_of::<i32>(nb.dir, INTSXP)?.ok_or_else(|| bad("nbw dir"))?;
    let on = bytes(nb.on);
    let mut at: std::collections::HashMap<i32, usize> = std::collections::HashMap::with_capacity(n);
    for (j, &id) in ids.iter().enumerate().rev() {
        at.insert(id, j);
    }
    let mut pairs: Vec<(usize, usize)> = Vec::new();
    for i in 0..n {
        let wired = grp[i] as SEXP == nb.on || bytes(grp[i] as SEXP) == on;
        let fc = if wired {
            ids[i]
        } else {
            cell.as_ref().map_or(NA_INT, |c| c[i])
        };
        if fc == NA_INT || fc < 1 || fc as usize > dir.len() {
            if fc != NA_INT && (fc < 1 || fc as usize > dir.len()) {
                return decline("a cell outside the lattice");
            }
            continue;
        }
        let nc = dir[fc as usize - 1];
        if nc == NA_INT {
            continue;
        }
        if let Some(&j) = at.get(&nc) {
            pairs.push((i, j));
        }
    }
    let m = pairs.len();
    if m == 0 {
        return decline("nobody with a neighbour there");
    }
    // inputs over the pairs: a column from the candidate, `own_<col>` from the
    // focal agent; then globals and formula-environment values (one value each)
    let mut ins: Vec<Vals> = Vec::with_capacity(gp.cols.len() + gp.globs.len());
    for &c in &gp.cols {
        let b = bytes(c);
        let (name, own) = match b.strip_prefix(b"own_") {
            Some(rest) => (rest, true),
            None => (b, false),
        };
        let Some(v) = stacked_col_named(groups, name, &sizes)? else {
            return decline("a column of another kind");
        };
        let rows: Vec<usize> = pairs
            .iter()
            .map(|&(i, j)| if own { i } else { j })
            .collect();
        ins.push(tidyabm_core::expr::take(&v, &rows));
    }
    let mut xs = Vec::new();
    if !read_globals(st, &gp.globs, &mut xs)? {
        return decline("a global not found");
    }
    let Some(svals) = scope_values(plan)? else {
        return decline("a formula-environment value that cannot be read");
    };
    if !read_scope(&svals, &gp.scope, &mut xs) {
        return decline("a formula-environment value not found");
    }
    let Some(more) = inputs(&xs)? else {
        return decline("a global or scope value of an unsupported type");
    };
    if more.iter().any(|v| v.len() != 1) {
        return decline("a global or scope value of more than one element");
    }
    ins.extend(more);
    // SAFETY: the program is kept alive by the plan's descriptor.
    let prog = unsafe { &*gp.prog };
    let res = match prog.compute(ins, m, m)? {
        Ok(r) => r,
        Err(d) => {
            expr::note_decline(&d);
            return Ok(None);
        }
    };
    // SAFETY: reading a static of R's (its NA_real_).
    let na_real = unsafe { rapi::R_NaReal };
    let at_ = |v: &Vals, p: usize| if v.len() == 1 { 0 } else { p };
    let mut next = 0;
    let mut outs: Vec<Vals> = Vec::with_capacity(nb.fns.len());
    for &f in &nb.fns {
        if f == 3 {
            outs.push(Vals::Int(Cow::Owned(vec![1; m])));
            continue;
        }
        let x = &res[next];
        next += 1;
        outs.push(match (f, x) {
            (0 | 1, Vals::Lgl(a)) => Vals::Lgl(Cow::Owned((0..m).map(|p| a[at_(x, p)]).collect())),
            (0 | 1, _) => return decline("any()/all() of a value that is not logical"),
            (_, Vals::Dbl(a)) => {
                Vals::Dbl(Cow::Owned((0..m).map(|p| 0.0 + a[at_(x, p)]).collect()))
            }
            (2, Vals::Int(a) | Vals::Lgl(a)) => {
                Vals::Int(Cow::Owned((0..m).map(|p| a[at_(x, p)]).collect()))
            }
            (_, Vals::Int(a) | Vals::Lgl(a)) => Vals::Dbl(Cow::Owned(
                (0..m)
                    .map(|p| {
                        let v = a[at_(x, p)];
                        if v == NA_INT {
                            na_real
                        } else {
                            f64::from(v)
                        }
                    })
                    .collect(),
            )),
            _ => return decline("sum()/mean() of strings"),
        });
    }
    let mut slot = vec![usize::MAX; n];
    for (p, &(i, _)) in pairs.iter().enumerate() {
        slot[i] = p;
    }
    let mut cur: Option<H> = None;
    for (r, v) in outs.iter().enumerate() {
        let t = gp.targets[r];
        let mut start = 0;
        for (g, &sz) in sizes.iter().enumerate() {
            let rows_g = start..start + sz;
            start += sz;
            if sz == 0 {
                continue;
            }
            let col = match v {
                Vals::Lgl(a) => Vals::Lgl(Cow::Owned(
                    rows_g
                        .map(|i| {
                            if slot[i] == usize::MAX {
                                NA_INT
                            } else {
                                a[slot[i]]
                            }
                        })
                        .collect(),
                )),
                Vals::Int(a) => Vals::Int(Cow::Owned(
                    rows_g
                        .map(|i| {
                            if slot[i] == usize::MAX {
                                NA_INT
                            } else {
                                a[slot[i]]
                            }
                        })
                        .collect(),
                )),
                Vals::Dbl(a) => Vals::Dbl(Cow::Owned(
                    rows_g
                        .map(|i| {
                            if slot[i] == usize::MAX {
                                na_real
                            } else {
                                a[slot[i]]
                            }
                        })
                        .collect(),
                )),
                Vals::Str(_) => return Err(bad("a string neighbour value")),
            };
            let gs = cur.as_ref().map_or(groups, |h| h.get());
            let base = elt(gs, g);
            // the holder exists before the new column (`to_r()` returns it
            // unprotected)
            let mut hold = OwnedListSexp::new(1, false)?;
            hold.set_value(0, expr::to_r(&col, sz)?)?;
            let g2 = set_named(base, t, elt(hold.inner(), 0))?;
            // SAFETY: reading a static of R's (unused name: `g` is in range).
            cur = Some(with_elt(gs, g, g2.get(), unsafe { rapi::R_BlankString })?);
        }
    }
    match cur {
        // SAFETY: as above.
        Some(gs) => Ok(Some(with_elt(st, gi, gs.get(), unsafe {
            rapi::R_BlankString
        })?)),
        None => Ok(Some(keep(st)?)),
    }
}

/// [`stacked_pad`] of the column named `name` (ASCII); `None` when no group
/// has it (or as `stacked_pad` declines).
fn stacked_col_named(
    groups: SEXP,
    name: &[u8],
    sizes: &[usize],
) -> savvy::Result<Option<Vals<'static>>> {
    for k in 0..len(groups) {
        let g = elt(groups, k);
        if let Some(c) = names_of(g)?.iter().copied().find(|&c| bytes(c) == name) {
            return stacked_pad(groups, c, sizes);
        }
    }
    Ok(None)
}

/// `abm_rules(.by = )` natively ([run_rules_by()]): the groups stacked, each rule
/// evaluated once per value of the key in `group_by()`'s order (`NA` too, its
/// rows then keeping their old value), the later rules reading what the earlier
/// wrote, then each value written back to every group.
fn exec_by(plan: &Plan, st: SEXP) -> savvy::Result<Option<H>> {
    use tidyabm_core::expr as ce;
    let gp = plan.groups.first().ok_or_else(|| bad("no program"))?;
    let by = plan.by.ok_or_else(|| bad("no key"))?;
    if let Some(ri) = find_str(st, b"relations")? {
        if len(elt(st, ri)) > 0 {
            return decline("a relation");
        }
    }
    let gi = find_str(st, b"groups")?.ok_or_else(|| bad("no groups"))?;
    let groups = elt(st, gi);
    let ng = len(groups);
    let mut sizes = Vec::with_capacity(ng);
    for k in 0..ng {
        match rows(elt(groups, k)) {
            Some(r) => sizes.push(r),
            None => return decline("a group without columns"),
        }
    }
    let n: usize = sizes.iter().sum();
    if n == 0 {
        return Ok(Some(keep(st)?));
    }
    let mut ins: Vec<Vals> = Vec::with_capacity(gp.cols.len() + gp.globs.len());
    for &c in &gp.cols {
        match stacked_col(groups, c)? {
            Some(v) => ins.push(v),
            None => return decline("a column some group lacks or of another kind"),
        }
    }
    let n_cols = ins.len();
    let mut xs = Vec::new();
    if !read_globals(st, &gp.globs, &mut xs)? {
        return decline("a global not found");
    }
    let Some(svals) = scope_values(plan)? else {
        return decline("a formula-environment value that cannot be read");
    };
    if !read_scope(&svals, &gp.scope, &mut xs) {
        return decline("a formula-environment value not found");
    }
    let Some(more) = inputs(&xs)? else {
        return decline("a global or scope value of an unsupported type");
    };
    ins.extend(more);
    let Some(key0) = stacked_col(groups, by)? else {
        return decline("a key of another kind");
    };
    let taking: Vec<bool> = match &key0 {
        Vals::Int(v) | Vals::Lgl(v) => v
            .iter()
            .map(|&x| x != tidyabm_core::sample::NA_INT)
            .collect(),
        Vals::Dbl(v) => v.iter().map(|x| !x.is_nan()).collect(),
        Vals::Str(_) => return decline("a string key"),
    };
    let all_taking = taking.iter().all(|&t| t);
    // SAFETY: the program is kept alive by the plan's descriptor.
    let prog = unsafe { &*gp.prog };
    // what this step has written, by target, for later rules' keys and old values
    let mut written: Vec<(SEXP, Vals)> = Vec::new();
    let mut values: Vec<Vals> = Vec::with_capacity(gp.targets.len());
    for (k, &t) in gp.targets.iter().enumerate() {
        let key = written
            .iter()
            .rev()
            .find(|(w, _)| *w == by)
            .map_or(&key0, |(_, v)| v);
        let Some(loc) = ce::group_rows(key) else {
            return decline("a key of another kind");
        };
        let mut parts = Vec::with_capacity(loc.len());
        for rows_g in &loc {
            let sliced: Vec<Vals> = ins
                .iter()
                .enumerate()
                .map(|(j, v)| {
                    if j < n_cols {
                        ce::take(v, rows_g)
                    } else {
                        v.clone()
                    }
                })
                .collect();
            match prog.compute_one(k, &sliced, rows_g.len())? {
                Ok(v) => parts.push(expr::owned(v)),
                Err(d) => {
                    expr::note_decline(&d);
                    return Ok(None);
                }
            }
        }
        let value = match expr::with_host(|h| ce::unchop(parts, &loc, n, h)) {
            Ok(v) => v,
            Err(d) => {
                expr::note_decline(&d);
                return Ok(None);
            }
        };
        let value = if all_taking {
            value
        } else {
            let old_w = written
                .iter()
                .rev()
                .find(|(w, _)| *w == t)
                .map(|(_, v)| v.clone());
            let old = match old_w {
                Some(v) => Some(v),
                None => {
                    let present = (0..ng)
                        .filter(|&g| find(names_of(elt(groups, g)).unwrap_or(&[]), t).is_some())
                        .count();
                    if present == 0 {
                        None
                    } else {
                        match stacked_col(groups, t)? {
                            Some(v) => Some(v),
                            None => return decline("an old target of another kind"),
                        }
                    }
                }
            };
            match expr::with_host(|h| ce::keep_old(value, old.as_ref(), &taking, h)) {
                Ok(v) => expr::owned(v),
                Err(d) => {
                    expr::note_decline(&d);
                    return Ok(None);
                }
            }
        };
        if let Some(w) = prog.write_of(k) {
            if w >= ins.len() {
                return Err(bad("write target out of range"));
            }
            ins[w] = value.clone();
        }
        written.push((t, value.clone()));
        values.push(value);
    }
    // commit: rule by rule, group by group (a group's rows are a run of the stack)
    let mut cur: Option<H> = None;
    for (k, &t) in gp.targets.iter().enumerate() {
        let mut start = 0;
        for (g, &sz) in sizes.iter().enumerate() {
            let rows_g: Vec<usize> = (start..start + sz).collect();
            start += sz;
            if sz == 0 {
                continue;
            }
            let gs = cur.as_ref().map_or(groups, |h| h.get());
            let base = elt(gs, g);
            let v = ce::take(&values[k], &rows_g);
            let tp = &taking[rows_g[0]..rows_g[0] + sz];
            // the new column is held here from the moment it exists (`to_r()`
            // returns it unprotected; anything allocating after it could free it)
            let mut hold = OwnedListSexp::new(1, false)?;
            let col = if tp.iter().all(|&b| b) {
                expr::to_r(&v, sz)?
            } else {
                let old = match find(names_of(base)?, t) {
                    Some(j) => {
                        let c = elt(base, j);
                        if !all_bare(&[c])? {
                            return decline("a target column with attributes");
                        }
                        match expr::vals_of(c, false)? {
                            Some(v) => Some(v),
                            None => return decline("a target column of another type"),
                        }
                    }
                    None => None,
                };
                match expr::assign_rows(old, &v, tp)? {
                    Ok(c) => expr::to_r(&c, sz)?,
                    Err(d) => {
                        expr::note_decline(&d);
                        return Ok(None);
                    }
                }
            };
            hold.set_value(0, col)?;
            let g2 = set_named(base, t, elt(hold.inner(), 0))?;
            // SAFETY: reading a static of R's (unused name: `g` is in range).
            cur = Some(with_elt(gs, g, g2.get(), unsafe { rapi::R_BlankString })?);
        }
    }
    match cur {
        // SAFETY: as above.
        Some(gs) => Ok(Some(with_elt(st, gi, gs.get(), unsafe {
            rapi::R_BlankString
        })?)),
        None => Ok(Some(keep(st)?)),
    }
}

/// The agent count and each group's id column, when the ids stacked are `1..n`.
type RowIds<'a> = Option<(usize, Vec<&'a [i32]>)>;

/// [`RowIds`] of `groups` (protected by the caller, which the slices borrow).
fn ids_are_rows<'a>(groups: SEXP) -> savvy::Result<RowIds<'a>> {
    let mut n = 0;
    let mut out = Vec::with_capacity(len(groups));
    for k in 0..len(groups) {
        let g = elt(groups, k);
        let Some(j) = find_str(g, b".id")? else {
            return Ok(None);
        };
        let Some(ids) = store::numbers_of::<i32>(elt(g, j), INTSXP)? else {
            return Ok(None);
        };
        for &id in ids {
            n += 1;
            if usize::try_from(id).ok() != Some(n) {
                return Ok(None);
            }
        }
        out.push(ids);
    }
    Ok(Some((n, out)))
}

/// `abm_neighbours()` over a network natively (P4's `rk_neighbours()` shapes):
/// each rule's column stacked over the groups, folded over each agent's
/// neighbours in `nb_fold()`'s order, written to every group (by `.id` when
/// there are several), and `state$nb_cache` set as R sets it.
fn exec_nb(plan: &Plan, nb: &NbPlan, st: SEXP) -> savvy::Result<Option<H>> {
    // SAFETY: reading a static of R's.
    let nil = unsafe { rapi::R_NilValue };
    if let Some(ri) = find_str(st, b"relations")? {
        if len(elt(st, ri)) > 0 {
            return decline("a relation");
        }
    }
    if let Some(si) = find_str(st, b"stale_draws")? {
        if elt(st, si) != nil {
            return decline("stale draws");
        }
    }
    match find_str(st, b"edges")? {
        Some(ei) if elt(st, ei) == nb.edges => {}
        _ => return decline("edges changed"),
    }
    let gi = find_str(st, b"groups")?.ok_or_else(|| bad("no groups"))?;
    let groups = elt(st, gi);
    let ng = len(groups);
    let Some((n, ids)) = ids_are_rows(groups)? else {
        return decline("ids that are not the row numbers");
    };
    if n == 0 || n != nb.n {
        return decline("a population of another size");
    }
    let mut cols: Vec<&[i32]> = Vec::with_capacity(nb.cols.len());
    for &c in &nb.cols {
        cols.push(store::numbers_of::<i32>(c, INTSXP)?.ok_or_else(|| bad("nb index"))?);
    }
    let cnt = store::numbers_of::<i32>(nb.cnt, INTSXP)?.ok_or_else(|| bad("nb counts"))?;
    // SAFETY: reading a static of R's (its NA_real_, not a Rust constant).
    let na = unsafe { rapi::R_NaReal };
    // each rule's argument over the agents (columns stacked, one-value globals
    // and formula-environment values), as `neighbour_fast()` evaluates it
    let gp = plan.groups.first().ok_or_else(|| bad("no program"))?;
    let mut ins: Vec<Vals> = Vec::with_capacity(gp.cols.len() + gp.globs.len());
    for &c in &gp.cols {
        match stacked_col(groups, c)? {
            Some(v) => ins.push(v),
            None => return decline("a column of another kind"),
        }
    }
    let mut xs = Vec::new();
    if !read_globals(st, &gp.globs, &mut xs)? {
        return decline("a global not found");
    }
    let Some(svals) = scope_values(plan)? else {
        return decline("a formula-environment value that cannot be read");
    };
    if !read_scope(&svals, &gp.scope, &mut xs) {
        return decline("a formula-environment value not found");
    }
    let Some(more) = inputs(&xs)? else {
        return decline("a global or scope value of an unsupported type");
    };
    if more.iter().any(|v| v.len() != 1) {
        return decline("a global or scope value of more than one element");
    }
    ins.extend(more);
    // SAFETY: the program is kept alive by the plan's descriptor.
    let prog = unsafe { &*gp.prog };
    let res = match prog.compute(ins, n, n)? {
        Ok(r) => r,
        Err(d) => {
            expr::note_decline(&d);
            return Ok(None);
        }
    };
    let _ = ng;
    let mut vals: Vec<Vals> = Vec::with_capacity(res.len());
    for (r, x) in res.into_iter().enumerate() {
        // a one-value argument stands for every agent (`rep(x, n)`)
        let x = if x.len() == 1 && n != 1 {
            tidyabm_core::expr::take(&x, &vec![0; n])
        } else {
            x
        };
        let x = Some(x);
        let folded = match x {
            Some(Vals::Dbl(x)) => tidyabm_core::neighbours::nb_fold(nb.mean[r], &x, &cols, cnt, na)
                .map(|v| Vals::Dbl(std::borrow::Cow::Owned(v))),
            Some(Vals::Int(x) | Vals::Lgl(x)) => {
                tidyabm_core::neighbours::nb_fold_int(nb.mean[r], &x, &cols, cnt, na).map(|v| {
                    match v {
                        Ok(s) => Vals::Int(std::borrow::Cow::Owned(s)),
                        Err(m) => Vals::Dbl(std::borrow::Cow::Owned(m)),
                    }
                })
            }
            _ => return decline("a column that is not a number"),
        };
        match folded {
            Some(v) => vals.push(v),
            None => return decline("a neighbourhood that meets NA or NaN"),
        }
    }
    // commit
    let mut cur: Option<H> = None;
    for (k, gids) in ids.iter().enumerate() {
        if gids.is_empty() {
            continue;
        }
        let gs = cur.as_ref().map_or(groups, |h| h.get());
        let mut g: Option<H> = None;
        for (r, &t) in nb.targets.iter().enumerate() {
            let pick = |id: &i32| *id as usize - 1;
            let v = match &vals[r] {
                _ if ng == 1 => vals[r].clone(),
                Vals::Dbl(d) => Vals::Dbl(std::borrow::Cow::Owned(
                    gids.iter().map(|id| d[pick(id)]).collect(),
                )),
                Vals::Int(d) | Vals::Lgl(d) => Vals::Int(std::borrow::Cow::Owned(
                    gids.iter().map(|id| d[pick(id)]).collect(),
                )),
                Vals::Str(_) => return Err(bad("a string fold")),
            };
            // the list that holds the new column exists before it (`to_r()`
            // returns it unprotected; an allocation after it could free it)
            let mut hold = OwnedListSexp::new(1, false)?;
            hold.set_value(0, expr::to_r(&v, gids.len())?)?;
            let base = g.as_ref().map_or_else(|| elt(gs, k), |h| h.get());
            g = Some(set_named(base, t, elt(hold.inner(), 0))?);
        }
        if let Some(g) = g {
            // SAFETY: reading a static of R's (unused name: `k` is in range).
            cur = Some(with_elt(gs, k, g.get(), unsafe { rapi::R_BlankString })?);
        }
    }
    let st2 = match &cur {
        // SAFETY: as above.
        Some(gs) => with_elt(st, gi, gs.get(), unsafe { rapi::R_BlankString })?,
        None => keep(st)?,
    };
    Ok(Some(set_named(st2.get(), nb.cache_name, nb.cache)?))
}

fn exec(plan: &Plan, st: SEXP) -> savvy::Result<Option<H>> {
    if let Some(nb) = &plan.nb {
        return exec_nb(plan, nb, st);
    }
    if plan.nbw.is_some() {
        exec_nbw(plan, st)
    } else if plan.nb1.is_some() {
        exec_nb1(plan, st)
    } else if plan.by.is_some() {
        exec_by(plan, st)
    } else if plan.global {
        exec_global(plan, st, false)
    } else {
        exec_rules(plan, st)
    }
}

/// The recorded ticks stacked (FR9b): `pieces[[k]]` is the `state$groups` of tick
/// `ticks[k]`; the result is the columns `rl_population()` builds from R's own
/// snapshots (`tick`, `.id`, `.group`, then every other column in order of first
/// appearance; a column a group lacks is `NA` there), as a named list, or `NULL`
/// when a column is not a bare logical/integer/double/character vector of one type
/// throughout (R then stacks the snapshots itself).
/// @noRd
#[savvy]
fn rs_stack_ticks(pieces: Sexp, ticks: IntegerSexp) -> savvy::Result<Sexp> {
    run(|| match stack_ticks(pieces.0, ticks.as_slice())? {
        Some(h) => Ok(Sexp(h.get())),
        None => Ok(NullSexp.into()),
    })
}

fn stack_ticks(pieces: SEXP, ticks: &[i32]) -> savvy::Result<Option<H>> {
    use tidyabm_core::sample::NA_INT;
    // SAFETY: reading a type tag.
    if unsafe { rapi::TYPEOF(pieces) } != VECSXP || len(pieces) != ticks.len() {
        return Ok(None);
    }
    // pass 1: the columns (first appearance), their types, the row count
    let mut names: Vec<SEXP> = Vec::new();
    let mut types: Vec<rapi::SexpType> = Vec::new();
    let mut total = 0usize;
    for p in 0..len(pieces) {
        let gs = elt(pieces, p);
        // SAFETY: reading a type tag.
        if unsafe { rapi::TYPEOF(gs) } != VECSXP {
            return Ok(None);
        }
        for k in 0..len(gs) {
            let g = elt(gs, k);
            let Some(r) = rows(g) else { return Ok(None) };
            total += r;
            let cols: Vec<SEXP> = (0..len(g)).map(|j| elt(g, j)).collect();
            if !all_bare(&cols)? {
                return Ok(None);
            }
            for (j, &nm) in names_of(g)?.iter().enumerate() {
                // SAFETY: reading a type tag.
                let t = unsafe { rapi::TYPEOF(cols[j]) };
                if !matches!(t, rapi::LGLSXP | INTSXP | rapi::REALSXP | STRSXP) || len(cols[j]) != r
                {
                    return Ok(None);
                }
                match find(&names, nm) {
                    Some(c) if types[c] != t => return Ok(None),
                    Some(_) => {}
                    None => {
                        if !bytes(nm).is_ascii() || bytes(nm) == b"tick" {
                            return Ok(None);
                        }
                        names.push(nm);
                        types.push(t);
                    }
                }
            }
        }
    }
    let pos = |b: &[u8]| names.iter().position(|&c| bytes(c) == b);
    let (Some(id), Some(grp)) = (pos(b".id"), pos(b".group")) else {
        return Ok(None);
    };
    let mut order: Vec<usize> = vec![id, grp];
    order.extend((0..names.len()).filter(|&c| c != id && c != grp));
    let k = order.len() + 1;
    let mut out = OwnedListSexp::new(k, true)?;
    // SAFETY: reading statics of R's.
    let (na_real, na_str) = unsafe { (rapi::R_NaReal, rapi::R_NaString) };
    // the tick column
    let tcol = {
        // SAFETY: main thread; the new vector is stored into `out` (protected)
        // before anything else allocates.
        let x = unsafe {
            rapi::protect_value(rapi::R_NilValue, move || {
                rapi::Rf_allocVector(INTSXP, total as isize)
            })
        }?;
        out.set_value(0, Sexp(x))?;
        x
    };
    out.set_name(0, "tick")?;
    {
        // SAFETY: an integer vector of `total` elements, protected by `out`.
        let dst = unsafe { rapi::DATAPTR_OR_NULL(tcol) } as *mut i32;
        let mut at = 0usize;
        for (p, &tk) in ticks.iter().enumerate() {
            let gs = elt(pieces, p);
            for g in 0..len(gs) {
                let r = rows(elt(gs, g)).unwrap_or(0);
                for _ in 0..r {
                    // SAFETY: `at < total` (pass 1 counted these rows).
                    unsafe { *dst.add(at) = tk };
                    at += 1;
                }
            }
        }
    }
    for (slot, &c) in order.iter().enumerate() {
        let (nm, t) = (names[c], types[c]);
        // SAFETY: main thread; stored into `out` before anything else allocates.
        let x = unsafe {
            rapi::protect_value(rapi::R_NilValue, move || {
                rapi::Rf_allocVector(t, total as isize)
            })
        }?;
        out.set_value(slot + 1, Sexp(x))?;
        let name = std::str::from_utf8(bytes(nm)).map_err(|_| bad("column name"))?;
        out.set_name(slot + 1, name)?;
        let mut at = 0usize;
        for p in 0..len(pieces) {
            let gs = elt(pieces, p);
            for gk in 0..len(gs) {
                let g = elt(gs, gk);
                let r = rows(g).unwrap_or(0);
                let src = find(names_of(g)?, nm).map(|j| elt(g, j));
                // SAFETY (all arms): `x` has `total` elements of type `t`, `src`
                // (when present) `r` of the same type; `at + r <= total`.
                unsafe {
                    match t {
                        rapi::REALSXP => {
                            let d = rapi::DATAPTR_OR_NULL(x) as *mut f64;
                            match src.and_then(|s| {
                                store::numbers_of::<f64>(s, rapi::REALSXP).ok().flatten()
                            }) {
                                Some(v) => std::ptr::copy_nonoverlapping(v.as_ptr(), d.add(at), r),
                                None => (0..r).for_each(|i| *d.add(at + i) = na_real),
                            }
                        }
                        rapi::LGLSXP | INTSXP => {
                            let d = rapi::DATAPTR_OR_NULL(x) as *mut i32;
                            let v =
                                match src {
                                    Some(s) if t == INTSXP => {
                                        store::numbers_of::<i32>(s, INTSXP).ok().flatten()
                                    }
                                    Some(s) => {
                                        expr::vals_of(s, false).ok().flatten().and_then(|v| match v
                                        {
                                            Vals::Lgl(l) => match l {
                                                std::borrow::Cow::Borrowed(b) => Some(b),
                                                std::borrow::Cow::Owned(_) => None,
                                            },
                                            _ => None,
                                        })
                                    }
                                    None => None,
                                };
                            match v {
                                Some(v) => std::ptr::copy_nonoverlapping(v.as_ptr(), d.add(at), r),
                                None if src.is_none() => {
                                    (0..r).for_each(|i| *d.add(at + i) = NA_INT)
                                }
                                None => return Err(bad("unreadable logical column")),
                            }
                        }
                        _ => match src {
                            Some(s) => {
                                for (i, &ch) in store::chars_of(s)?.iter().enumerate() {
                                    rapi::SET_STRING_ELT(x, (at + i) as isize, ch);
                                }
                            }
                            None => (0..r)
                                .for_each(|i| rapi::SET_STRING_ELT(x, (at + i) as isize, na_str)),
                        },
                    }
                }
                at += r;
            }
        }
    }
    Ok(Some(keep(out.inner())?))
}

/// Run one native step from `prep`'s descriptor `desc` over `state`: the new
/// state, or `NULL` where the R path must run it ("both" mode's kernel; the loop
/// runs the same code).
/// @noRd
#[savvy]
fn rs_native_exec(desc: Sexp, state: Sexp) -> savvy::Result<Sexp> {
    run(|| {
        let plan = parse_plan(keep(desc.0)?)?;
        match exec(&plan, state.0)? {
            Some(h) => Ok(Sexp(h.get())),
            None => Ok(NullSexp.into()),
        }
    })
}

// ---- the loop ----------------------------------------------------------------

/// Consecutive run-time declines after which a step stays on R for its schema.
const DECLINES_BEFORE_SKIP: u32 = 8;

enum Slot {
    /// not compiled for the current schema yet
    Fresh,
    /// never native (static shape)
    Never,
    /// R for the schema `key`
    Skip(Vec<usize>),
    Ready(Vec<usize>, Box<Plan>),
}

/// A run driven from Rust. `kinds[i]`: 0 an R step, 1 native `abm_rules`, 2
/// native `abm_global`, 3 native network `abm_neighbours`. The state is bound as
/// `st` in `ctx`; callbacks (R closures) read it there: `prep(i)` -> descriptor,
/// `NULL` or `FALSE`; `seg(i, j, restore)` runs steps `i..j` (1-based) on the R
/// path, after restoring the saved `.Random.seed` when `restore`, and `tick(done,
/// next)` the book-keeping after tick `done` (0: none) and before tick `next` (0:
/// none) -- both bind the new state in `ctx` and return it; `rng()` saves
/// `.Random.seed`.
#[savvy]
pub struct RunLoop {
    kinds: Vec<i32>,
    slots: Vec<Slot>,
    prep: H,
    seg: H,
    tick: H,
    rng: H,
    /// the environment whose `st` binds the state (its only reference, so the R
    /// engine's steps change it in place as under [run_once()])
    ctx: H,
    st_sym: SEXP,
    /// native steps run, declined, compiled; R callbacks for steps
    stats: [i32; 4],
    /// `.Random.seed` was saved for the current native attempt
    saved: bool,
    /// consecutive run-time declines per step (reset by a native success)
    declines: Vec<u32>,
    /// `mprep(k)`: the measures (1) / checks (2) program for the schema
    mprep: H,
    mslots: Vec<Slot>,
}

impl RunLoop {
    /// Step `i` natively, or `None` (declined or not native for this schema).
    /// Bind `x` as the state in the context environment.
    fn put(&self, x: SEXP) -> savvy::Result<()> {
        let (sym, env) = (self.st_sym, self.ctx.get());
        // SAFETY: main thread; `x` protected by the caller, `env` kept; defineVar
        // may allocate a binding, hence `protect`.
        unsafe { rapi::protect(move || rapi::Rf_defineVar(sym, x, env)) }
    }

    /// Run R callback `f(ints...)` that reads `ctx$st` and binds the new state
    /// there, returning it.
    fn call_state(&self, f: SEXP, ints: &[i32]) -> savvy::Result<SEXP> {
        // SAFETY: main thread; `f` is kept by `self`; the value is bound in `ctx`.
        unsafe { eval_r(f, ints) }
    }

    fn try_native(&mut self, i: usize, st: SEXP) -> savvy::Result<Option<H>> {
        self.saved = false;
        let Some(mut key) = schema_key(st)? else {
            return Ok(None);
        };
        if self.kinds[i] == 3 {
            // a neighbour index is built for these edges and this many agents
            let s = st;
            let edges = find_str(s, b"edges")?.map_or(0, |e| elt(s, e) as usize);
            let gs = find_str(s, b"groups")?.map_or(rapi_nil(), |g| elt(s, g));
            let n: usize = (0..len(gs)).map(|k| rows(elt(gs, k)).unwrap_or(0)).sum();
            key.extend([edges, n]);
        }
        let stale = match &self.slots[i] {
            Slot::Never => return Ok(None),
            Slot::Fresh => true,
            Slot::Skip(k) | Slot::Ready(k, _) => *k != key,
        };
        if stale {
            self.stats[2] += 1;
            let d = call_r(&self.prep, &[i as i32 + 1])?;
            let x = d.get();
            // SAFETY: reading a type tag.
            let t = unsafe { rapi::TYPEOF(x) };
            self.slots[i] = if t == rapi::LGLSXP {
                Slot::Never
            } else if t == VECSXP {
                Slot::Ready(key, Box::new(parse_plan(d)?))
            } else {
                Slot::Skip(key)
            };
        }
        let Slot::Ready(_, plan) = &self.slots[i] else {
            return Ok(None);
        };
        if plan.draws {
            call_r(&self.rng, &[])?;
            self.saved = true;
        }
        let out = exec(plan, st)?;
        self.stats[if out.is_some() { 0 } else { 1 }] += 1;
        if out.is_some() {
            self.declines[i] = 0;
        } else {
            // a step that keeps declining for this schema is left to R until the
            // schema changes: each attempt costs its inputs' checks for nothing
            self.declines[i] += 1;
            if self.declines[i] >= DECLINES_BEFORE_SKIP {
                self.declines[i] = 0;
                if let Slot::Ready(k, _) = &self.slots[i] {
                    self.slots[i] = Slot::Skip(k.clone());
                }
            }
        }
        Ok(out)
    }

    /// The measures (`k = 0`) or checks (`k = 1`) of the state natively: a list
    /// of their values, or `None` (not native for this schema, or declined).
    fn try_tick(&mut self, k: usize, st: SEXP) -> savvy::Result<Option<H>> {
        let Some(key) = schema_key(st)? else {
            return Ok(None);
        };
        let stale = match &self.mslots[k] {
            Slot::Never => return Ok(None),
            Slot::Fresh => true,
            Slot::Skip(kk) | Slot::Ready(kk, _) => *kk != key,
        };
        if stale {
            let d = call_r(&self.mprep, &[k as i32 + 1])?;
            // SAFETY: reading a type tag.
            let t = unsafe { rapi::TYPEOF(d.get()) };
            self.mslots[k] = if t == rapi::LGLSXP {
                Slot::Never
            } else if t == VECSXP {
                Slot::Ready(key, Box::new(parse_plan(d)?))
            } else {
                Slot::Skip(key)
            };
        }
        let Slot::Ready(_, plan) = &self.mslots[k] else {
            return Ok(None);
        };
        exec_global(plan, st, true)
    }

    /// A recorded tick's groups into `slog` (the run's own list; stacked after
    /// the run). The R tick callback leaves them to this, so the list is only
    /// ever written here.
    fn log_groups(&self, keep_t: &[bool], slog: &Sexp, t: i32, st: SEXP) -> savvy::Result<()> {
        let t = t as usize;
        // SAFETY: reading a type tag.
        if keep_t.get(t) != Some(&true)
            || unsafe { rapi::TYPEOF(slog.0) } != VECSXP
            || t >= len(slog.0)
        {
            return Ok(());
        }
        let gs = find_str(st, b"groups")?.map_or_else(rapi_nil, |i| elt(st, i));
        // SAFETY: main thread; `slog` is the run's own log list (no other R value
        // aliases it), `t` is in range, `gs` is protected by the state.
        unsafe { rapi::SET_VECTOR_ELT(slog.0, t as isize, gs) };
        Ok(())
    }

    fn native_now(&self, i: usize) -> bool {
        self.kinds[i] != 0 && !matches!(self.slots[i], Slot::Never)
    }
}

#[savvy]
impl RunLoop {
    fn new(
        kinds: IntegerSexp,
        prep: Sexp,
        seg: Sexp,
        tick: Sexp,
        rng: Sexp,
        ctx: Sexp,
        mprep: Sexp,
    ) -> savvy::Result<Self> {
        run(|| {
            // SAFETY: main thread; installing a symbol may allocate, hence protect.
            let st_sym = unsafe {
                rapi::protect_value(rapi::R_NilValue, || rapi::Rf_install(c"st".as_ptr()))
            }?;
            let kinds = kinds.as_slice().to_vec();
            let n_steps = kinds.len();
            Ok(RunLoop {
                slots: kinds.iter().map(|_| Slot::Fresh).collect(),
                kinds,
                prep: keep(prep.0)?,
                seg: keep(seg.0)?,
                tick: keep(tick.0)?,
                rng: keep(rng.0)?,
                ctx: keep(ctx.0)?,
                st_sym,
                stats: [0; 4],
                saved: false,
                declines: vec![0; n_steps],
                mprep: keep(mprep.0)?,
                mslots: vec![Slot::Fresh, Slot::Fresh],
            })
        })
    }

    /// Run `ticks` ticks from `state` (tick 0 already recorded); the final state.
    fn run(
        &mut self,
        state: Sexp,
        ticks: i32,
        need: LogicalSexp,
        has: LogicalSexp,
        keep_t: LogicalSexp,
        logs: Sexp,
    ) -> savvy::Result<Sexp> {
        run(|| {
            // `logs`: the run's globals, measures and recorded-groups lists
            let lg = |k: usize| -> SEXP {
                // SAFETY: reading a type tag; `logs` is protected by savvy.
                if unsafe { rapi::TYPEOF(logs.0) } == VECSXP && len(logs.0) == 3 {
                    elt(logs.0, k)
                } else {
                    rapi_nil()
                }
            };
            let (glog, mlog, slog) = (Sexp(lg(0)), Sexp(lg(1)), Sexp(lg(2)));
            let n = self.kinds.len();
            // the ticks whose book-keeping needs R (a snapshot, measures, checks,
            // progress); the others log their globals into `glog` here
            let need: Vec<bool> = need.iter().collect();
            // SAFETY: reading a type tag.
            let glog_ok = unsafe { rapi::TYPEOF(glog.0) } == VECSXP
                && len(glog.0) == ticks as usize + 1
                // SAFETY: reading a type tag.
                && unsafe { rapi::TYPEOF(mlog.0) } == VECSXP
                && len(mlog.0) == ticks as usize + 1
                && need.len() == ticks as usize + 1;
            let has: Vec<bool> = has.iter().collect();
            // the recorded ticks: their `state$groups` go into `slog` (stacked
            // after the run, `rs_stack_ticks()`)
            let keep_t: Vec<bool> = keep_t.iter().collect();
            // SAFETY: reading a type tag.
            let glog_ok = glog_ok
                && keep_t.len() == ticks as usize + 1
                && unsafe { rapi::TYPEOF(slog.0) } == VECSXP
                && len(slog.0) == ticks as usize + 1;
            let (has_m, has_c) = (has.first() == Some(&true), has.get(1) == Some(&true));
            self.put(state.0)?;
            let (seg, tick) = (self.seg.get(), self.tick.get());
            let mut st = self.call_state(tick, &[0, i32::from(ticks > 0)])?;
            for t in 1..=ticks {
                let mut i = 0;
                while i < n {
                    if self.native_now(i) {
                        if let Some(next) = self.try_native(i, st)? {
                            self.put(next.get())?;
                            st = next.get();
                            i += 1;
                            continue;
                        }
                        // declined (or R for this schema): the R path, from the
                        // saved `.Random.seed` if the native attempt could draw
                        let restore = self.saved;
                        self.stats[3] += 1;
                        st = self
                            .call_state(seg, &[i as i32 + 1, i as i32 + 1, i32::from(restore)])?;
                        i += 1;
                        continue;
                    }
                    let mut j = i;
                    while j + 1 < n && !self.native_now(j + 1) {
                        j += 1;
                    }
                    self.stats[3] += 1;
                    st = self.call_state(seg, &[i as i32 + 1, j as i32 + 1, 0])?;
                    i = j + 1;
                }
                let next = if t < ticks { t + 1 } else { 0 };
                if !glog_ok || need[t as usize] {
                    st = self.call_state(tick, &[t, next])?;
                    self.log_groups(&keep_t, &slog, t, st)?;
                    continue;
                }
                // FR9b: measures and checks natively; anything else (a decline,
                // a check that is not TRUE) closes the tick through R, which
                // computes them itself and raises what it raises
                let mvals = if has_m { self.try_tick(0, st)? } else { None };
                let mut native = !has_m || mvals.is_some();
                if native && has_c {
                    native = match self.try_tick(1, st)? {
                        Some(c) => (0..len(c.get())).all(|i| {
                            let v = elt(c.get(), i);
                            // SAFETY: reading a type tag, then a logical of length 1.
                            let lgl = unsafe { rapi::TYPEOF(v) } == rapi::LGLSXP;
                            // SAFETY: a logical vector of length 1.
                            lgl && len(v) == 1 && unsafe { *rapi::LOGICAL_RO(v) } == 1
                        }),
                        None => false,
                    };
                }
                if !native {
                    st = self.call_state(tick, &[t, next])?;
                    self.log_groups(&keep_t, &slog, t, st)?;
                    continue;
                }
                self.log_groups(&keep_t, &slog, t, st)?;
                if let Some(m) = &mvals {
                    // SAFETY: main thread; `mlog` is the run's own log list (no
                    // other R value aliases it), `t <= ticks` is in range, `m` is
                    // kept.
                    unsafe { rapi::SET_VECTOR_ELT(mlog.0, t as isize, m.get()) };
                }
                // FR9: `global_log[[t + 1]]`'s globals, then `st$match <- NULL;
                // st$tick <- next` (as the R callback does)
                let gl = find_str(st, b"globals")?.map_or_else(rapi_nil, |i| elt(st, i));
                // SAFETY: main thread; `glog` is the run's own log list (no other R
                // value aliases it), `t <= ticks` is in range, `gl` is protected
                // by the state.
                unsafe { rapi::SET_VECTOR_ELT(glog.0, t as isize, gl) };
                if next > 0 {
                    let s1 = without(st, b"match")?;
                    let s2 = set_tick(s1.get(), next)?;
                    self.put(s2.get())?;
                    st = s2.get();
                }
            }
            Ok(Sexp(st))
        })
    }

    /// Native steps run, declined, compiled, and R callbacks for steps.
    fn stats(&self) -> savvy::Result<Sexp> {
        run(|| savvy::OwnedIntegerSexp::try_from_slice(self.stats).map(Into::into))
    }
}
