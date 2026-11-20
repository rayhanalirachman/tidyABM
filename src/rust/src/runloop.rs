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
use savvy::{savvy, IntegerSexp, NullSexp, OwnedListSexp, Sexp};
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
fn all_bare(xs: &[SEXP]) -> savvy::Result<bool> {
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
}

/// A native step compiled by `prep` (`R/run-loop.R`, `rl_prep()`).
struct Plan {
    global: bool,
    nb: Option<NbPlan>,
    scope_match: bool,
    draws: bool,
    groups: Vec<GroupPlan>,
    // the descriptor: keeps the names' CHARSXPs and the programs alive
    _desc: H,
}

/// A network `abm_neighbours()` step whose rules are `sum()`/`mean()` of bare
/// double columns (the shapes P4's `rk_neighbours()` folded in Rust), with the
/// neighbour index and `state$nb_cache` that R builds for these edges and this
/// many agents.
struct NbPlan {
    mean: Vec<bool>,
    args: Vec<SEXP>,
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
        args: chars(field(nb, b"args")?)?,
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
    if plan.args.len() != k || plan.targets.len() != k {
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
        groups.push(GroupPlan {
            name: chars(name)?.first().copied().ok_or_else(|| bad("name"))?,
            prog,
            cols: chars(field(g, b"cols")?)?,
            globs: chars(field(g, b"globs")?)?,
            targets: chars(field(g, b"targets")?)?,
        });
    }
    let nb = if kind == 3 {
        Some(parse_nb(field(d, b"nb")?)?)
    } else {
        None
    };
    Ok(Plan {
        global: kind == 2,
        nb,
        scope_match,
        draws,
        groups,
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

/// `abm_rules()` natively (the shapes of slice 1's `rk_rules()`): every group's
/// rules from the start-of-step columns, then the writes. `None` = declined.
fn exec_rules(plan: &Plan, st: SEXP) -> savvy::Result<Option<H>> {
    if let Some(ri) = find_str(st, b"relations")? {
        if len(elt(st, ri)) > 0 {
            return decline("a relation");
        }
    }
    if plan.scope_match {
        if let Some(mi) = find_str(st, b"match")? {
            // SAFETY: reading a static of R's.
            if elt(st, mi) != unsafe { rapi::R_NilValue } {
                return decline("a standing match");
            }
        }
    }
    let gi = find_str(st, b"groups")?.ok_or_else(|| bad("no groups"))?;
    let groups = elt(st, gi);
    let gnames = names_of(groups)?;
    let mut results: Vec<(usize, OwnedListSexp, &GroupPlan)> = Vec::new();
    let mut applied = false;
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
        let Some(ins) = inputs(&xs)? else {
            return decline("an input that is not a bare vector");
        };
        // SAFETY: the program is kept alive by the plan's descriptor.
        let prog = unsafe { &*gp.prog };
        let res = match prog.compute(ins, n, n)? {
            Ok(r) => r,
            Err(d) => {
                expr::note_decline(&d);
                return Ok(None);
            }
        };
        let mut out = OwnedListSexp::new(res.len(), false)?;
        for (i, v) in res.iter().enumerate() {
            out.set_value(i, expr::to_r(v, n)?)?;
        }
        results.push((k, out, gp));
    }
    if !applied && (0..len(groups)).any(|k| rows(elt(groups, k)).unwrap_or(0) > 0) {
        // nobody to apply the rules to: the R engine's error to raise
        return decline("no group to apply the rules to");
    }
    // commit
    let mut cur: Option<H> = None;
    for (k, out, gp) in &results {
        let gs = cur.as_ref().map_or(groups, |h| h.get());
        let mut g: Option<H> = None;
        for (i, &t) in gp.targets.iter().enumerate() {
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
fn exec_global(plan: &Plan, st: SEXP) -> savvy::Result<Option<H>> {
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
            None => return decline("a global of an unsupported type"),
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
fn exec_nb(nb: &NbPlan, st: SEXP) -> savvy::Result<Option<H>> {
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
    let mut vals: Vec<Vec<f64>> = Vec::with_capacity(nb.args.len());
    for (r, &a) in nb.args.iter().enumerate() {
        let mut x: Vec<f64> = Vec::with_capacity(n);
        for k in 0..ng {
            let g = elt(groups, k);
            let Some(j) = find(names_of(g)?, a) else {
                return decline("a column some group lacks");
            };
            let col = elt(g, j);
            if !all_bare(&[col])? {
                return decline("an input that is not a bare vector");
            }
            match store::numbers_of::<f64>(col, rapi::REALSXP)? {
                Some(v) => x.extend_from_slice(v),
                None => return decline("a column that is not double"),
            }
        }
        match tidyabm_core::neighbours::nb_fold(nb.mean[r], &x, &cols, cnt, na) {
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
            let v: Vec<f64> = if ng == 1 {
                vals[r].clone()
            } else {
                gids.iter().map(|&id| vals[r][id as usize - 1]).collect()
            };
            let col = expr::to_r(&Vals::Dbl(std::borrow::Cow::Owned(v)), gids.len())?;
            let base = g.as_ref().map_or_else(|| elt(gs, k), |h| h.get());
            // `col` is unprotected: kept in a list while the group is copied
            let mut hold = OwnedListSexp::new(1, false)?;
            hold.set_value(0, col)?;
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
        return exec_nb(nb, st);
    }
    if plan.global {
        exec_global(plan, st)
    } else {
        exec_rules(plan, st)
    }
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

enum Slot {
    /// not compiled for the current schema yet
    Fresh,
    /// never native (static shape)
    Never,
    /// R for the schema `key`
    Skip(Vec<usize>),
    Ready(Vec<usize>, Plan),
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
                Slot::Ready(key, parse_plan(d)?)
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
        Ok(out)
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
    ) -> savvy::Result<Self> {
        run(|| {
            // SAFETY: main thread; installing a symbol may allocate, hence protect.
            let st_sym = unsafe {
                rapi::protect_value(rapi::R_NilValue, || rapi::Rf_install(c"st".as_ptr()))
            }?;
            let kinds = kinds.as_slice().to_vec();
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
            })
        })
    }

    /// Run `ticks` ticks from `state` (tick 0 already recorded); the final state.
    fn run(&mut self, state: Sexp, ticks: i32) -> savvy::Result<Sexp> {
        run(|| {
            let n = self.kinds.len();
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
                st = self.call_state(tick, &[t, next])?;
            }
            Ok(Sexp(st))
        })
    }

    /// Native steps run, declined, compiled, and R callbacks for steps.
    fn stats(&self) -> savvy::Result<Sexp> {
        run(|| savvy::OwnedIntegerSexp::try_from_slice(self.stats).map(Into::into))
    }
}
