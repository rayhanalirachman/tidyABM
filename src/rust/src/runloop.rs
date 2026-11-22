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
use crate::{reduce, run, typed_error};
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
    names.iter().position(|&c| same_bytes(c, w))
}

/// Where the element named `want` (ASCII) is in list `x`.
fn find_str(x: SEXP, want: &[u8]) -> savvy::Result<Option<usize>> {
    Ok(names_of(x)?.iter().position(|&c| same_bytes(c, want)))
}

/// Does `CHARSXP` `c` hold exactly `want`? Byte by byte against R's buffer, no
/// `strlen()` first: most names differ in their first byte (this runs for
/// every state lookup of every step).
fn same_bytes(c: SEXP, want: &[u8]) -> bool {
    // SAFETY: `c` is a live CHARSXP; R_CHAR gives its NUL-terminated bytes,
    // read only up to the first difference or `want.len()` (a NUL in R's
    // buffer stops the comparison before reading past it).
    unsafe {
        let p = rapi::R_CHAR(c) as *const u8;
        for (k, &w) in want.iter().enumerate() {
            if *p.add(k) != w {
                return false;
            }
        }
        *p.add(want.len()) == 0
    }
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
    /// the standing match's columns the program reads (after the scope values)
    mcols: Vec<SEXP>,
    /// per rule in `order`: elementwise (else, under a standing match of a
    /// match-scope step, evaluated once per pair or group, `.group_id`)
    rew: Vec<bool>,
    /// a birth step's program: 0 `when`, 1 `times`, 2 `cost`, 3 `inherit`; 4 a
    /// sequential step's `.order`
    part: u8,
    /// a sequential step (FR8): per rule, the input its value replaces for the
    /// rules after it (-1 none)
    wslot: Vec<i32>,
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
    /// `abm_match()` (kind 7)
    mt: Option<MatchPlan>,
    /// `abm_death()` (kind 8): `prune_edges`
    death: Option<bool>,
    /// `abm_birth()` (kind 9): the `as.integer(round(.t))` program
    birth: Option<*const RxProgram>,
    /// `abm_birth(n =)`: the count (`None`: `when =`)
    count: Option<usize>,
    /// `abm_birth(attach_via =)`: 1 `from = "parent"`, 2 a random agent; `links`
    attach: Option<(u8, usize)>,
    /// `abm_move()` (kind 10)
    mv: Option<MovePlan>,
    /// `abm_tell()` (kind 11): `when` given, and `.resolve` (0 last, 1 first,
    /// 2 sum, 3 mean, 4 error)
    tell: Option<(bool, u8)>,
    /// `abm_sequential()` (kind 12): `.order` given
    seq: Option<bool>,
    /// the cross-product `within` (kind 14): per rule 0 `n()`, 1 `sum`, 2
    /// `mean`, 3 `any`, 4 `all`, 5 any other formula; `na.rm`
    cross: Option<(Vec<u8>, Vec<bool>)>,
    /// `abm_global(.by = <name>)` (kind 13): the keys' name, from a global (2)
    /// or the formula's environment (3)
    gby: Option<(SEXP, i32)>,
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

/// An `abm_match()` step: 1 random, 2 one_of, 3 opposite_group; the step's
/// `size` (as R holds it, for `state$match`), `by` and `.by` columns (names or
/// none), and whether the program evaluates `eligible` and then `among`.
struct MatchPlan {
    pair: i32,
    size: SEXP,
    size_n: usize,
    by: Option<SEXP>,
    dot_by: Option<SEXP>,
    eligible: bool,
    among: bool,
    /// the two role names (`role = list(a = <cond>, b = <cond>)`), or none
    roles: Vec<SEXP>,
}

/// An `abm_move()` step to a random (empty) adjacent cell: the moving groups,
/// whether occupied cells are avoided, and the lattice's adjacency and coordinates.
struct MovePlan {
    who: Vec<SEXP>,
    avoid: bool,
    lattice: SEXP,
    nb: SEXP,
    x: SEXP,
    y: SEXP,
    ncell: i32,
    /// `direction =` (FR13): the lattice's `dir_nb` (or `NULL`)
    dir: SEXP,
}

/// A network `abm_neighbours()` step whose rules are `sum()`/`mean()` of bare
/// double columns (the shapes P4's `rk_neighbours()` folded in Rust), with the
/// neighbour index and `state$nb_cache` that R builds for these edges and this
/// many agents.
struct NbPlan {
    mean: Vec<bool>,
    /// per rule 0 `sum`/`mean`, 1 `any`, 2 `all` (FR13), and `na.rm`
    anyall: Vec<i32>,
    narm: Vec<bool>,
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
    let anyall = match find_str(nb, b"anyall")? {
        Some(i) => store::numbers_of::<i32>(elt(nb, i), INTSXP)?
            .unwrap_or(&[])
            .to_vec(),
        None => vec![0; k],
    };
    let narm = match find_str(nb, b"narm")? {
        Some(i) => {
            let v = elt(nb, i);
            // SAFETY: reading a type tag, then a logical vector of its length.
            if unsafe { rapi::TYPEOF(v) } != rapi::LGLSXP {
                return Err(bad("nb narm"));
            }
            // SAFETY: as above.
            (0..len(v))
                .map(|j| unsafe { *rapi::LOGICAL_RO(v).add(j) } == 1)
                .collect()
        }
        None => vec![false; k],
    };
    if anyall.len() != k || narm.len() != k {
        return Err(bad("nb any/all"));
    }
    let plan = NbPlan {
        anyall,
        narm,
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
        // (`NULL`: a step that compiles nothing, e.g. a match without conditions)
        let prog = if ptr == rapi_nil() {
            std::ptr::null()
        } else {
            // SAFETY: as just above.
            unsafe { savvy::get_external_pointer_addr(ptr) }? as *const RxProgram
        };
        if !prog.is_null() {
            // SAFETY: as just above; the program is not mutated while borrowed.
            draws |= unsafe { &*prog }.draws_any();
        }
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
        let mcols = match find_str(g, b"mcols")? {
            Some(i) => chars(elt(g, i))?,
            None => Vec::new(),
        };
        let rew = match find_str(g, b"rew")? {
            Some(i) => {
                let v = elt(g, i);
                // SAFETY: reading a type tag, then a logical vector of its length.
                if unsafe { rapi::TYPEOF(v) } == rapi::LGLSXP {
                    (0..len(v))
                        .map(|k| unsafe { *rapi::LOGICAL_RO(v).add(k) } == 1)
                        .collect()
                } else {
                    Vec::new()
                }
            }
            None => Vec::new(),
        };
        let part = match find_str(g, b"part")? {
            Some(i) => match chars(elt(g, i))?.first().map(|&c| bytes(c)) {
                Some(b"times") => 1,
                Some(b"cost") => 2,
                Some(b"inherit") => 3,
                Some(b"order") => 4,
                _ => 0,
            },
            None => 0,
        };
        groups.push(GroupPlan {
            wslot: ints(b"wslot")?,
            part,
            rew,
            mcols,
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
    let mt = if kind == 7 {
        let x = field(d, b"mt")?;
        let int1 = |nm: &[u8]| -> savvy::Result<i32> {
            store::numbers_of::<i32>(field(x, nm)?, INTSXP)?
                .and_then(|v| v.first().copied())
                .ok_or_else(|| bad("match int"))
        };
        let lgl1 = |nm: &[u8]| -> savvy::Result<bool> {
            let v = field(x, nm)?;
            // SAFETY: reading a type tag, then a logical of length 1.
            let ok = unsafe { rapi::TYPEOF(v) } == rapi::LGLSXP && len(v) == 1;
            // SAFETY: as above.
            Ok(ok && unsafe { *rapi::LOGICAL_RO(v) } == 1)
        };
        let name = |nm: &[u8]| -> savvy::Result<Option<SEXP>> {
            Ok(chars(field(x, nm)?)?.first().copied())
        };
        let size_n = int1(b"size")?;
        Some(MatchPlan {
            pair: int1(b"pair")?,
            size: field(x, b"size")?,
            size_n: usize::try_from(size_n).map_err(|_| bad("match size"))?,
            by: name(b"by")?,
            dot_by: name(b"dot_by")?,
            eligible: lgl1(b"eligible")?,
            among: lgl1(b"among")?,
            roles: chars(field(x, b"roles")?)?,
        })
    } else {
        None
    };
    // a match draws
    let draws = draws || mt.is_some();
    let mv = if kind == 10 {
        let x = field(d, b"mv")?;
        let av = field(x, b"avoid")?;
        // SAFETY: reading a type tag, then a logical of length 1.
        let lg = unsafe { rapi::TYPEOF(av) } == rapi::LGLSXP && len(av) == 1;
        // SAFETY: as above.
        let avoid = lg && unsafe { *rapi::LOGICAL_RO(av) } == 1;
        let ncell = store::numbers_of::<i32>(field(x, b"ncell")?, INTSXP)?
            .and_then(|v| v.first().copied())
            .ok_or_else(|| bad("move ncell"))?;
        let y = find_str(x, b"y")?.map_or_else(rapi_nil, |i| elt(x, i));
        Some(MovePlan {
            who: chars(field(x, b"who")?)?,
            avoid,
            lattice: field(x, b"lattice")?,
            nb: field(x, b"nb")?,
            x: field(x, b"x")?,
            y,
            ncell,
            dir: find_str(x, b"dir")?.map_or_else(rapi_nil, |i| elt(x, i)),
        })
    } else {
        None
    };
    let draws = draws || mv.is_some();
    let tell = if kind == 11 {
        let x = field(d, b"tell")?;
        let w = field(x, b"when")?;
        // SAFETY: reading a type tag, then a logical of length 1.
        let lg = unsafe { rapi::TYPEOF(w) } == rapi::LGLSXP && len(w) == 1;
        // SAFETY: as above.
        let when = lg && unsafe { *rapi::LOGICAL_RO(w) } == 1;
        let res = match chars(field(x, b"resolve")?)?.first().map(|&c| bytes(c)) {
            Some(b"last") => 0,
            Some(b"first") => 1,
            Some(b"sum") => 2,
            Some(b"mean") => 3,
            Some(b"error") => 4,
            _ => return Err(bad("tell resolve")),
        };
        Some((when, res))
    } else {
        None
    };
    let seq = if kind == 12 {
        let x = field(d, b"seq")?;
        let o = field(x, b"order")?;
        // SAFETY: reading a type tag, then a logical of length 1.
        let lg = unsafe { rapi::TYPEOF(o) } == rapi::LGLSXP && len(o) == 1;
        // SAFETY: as above.
        Some(lg && unsafe { *rapi::LOGICAL_RO(o) } == 1)
    } else {
        None
    };
    let cross = if kind == 14 {
        let x = field(d, b"cross")?;
        let fns = chars(field(x, b"fns")?)?
            .iter()
            .map(|&c| match bytes(c) {
                b"n" => Ok(0),
                b"sum" => Ok(1),
                b"mean" => Ok(2),
                b"any" => Ok(3),
                b"all" => Ok(4),
                b"expr" => Ok(5),
                _ => Err(bad("cross fn")),
            })
            .collect::<savvy::Result<Vec<u8>>>()?;
        let v = field(x, b"narm")?;
        // SAFETY: reading a type tag, then a logical vector of its length.
        if unsafe { rapi::TYPEOF(v) } != rapi::LGLSXP || len(v) != fns.len() {
            return Err(bad("cross narm"));
        }
        // SAFETY: as above.
        let narm = (0..len(v))
            .map(|i| unsafe { *rapi::LOGICAL_RO(v).add(i) } == 1)
            .collect();
        Some((fns, narm))
    } else {
        None
    };
    let gby = if kind == 13 {
        let x = field(d, b"gby")?;
        let by = chars(field(x, b"by")?)?
            .first()
            .copied()
            .ok_or_else(|| bad("gby by"))?;
        let src = store::numbers_of::<i32>(field(x, b"src")?, INTSXP)?
            .and_then(|v| v.first().copied())
            .ok_or_else(|| bad("gby src"))?;
        Some((by, src))
    } else {
        None
    };
    // a sequential step without `.order` shuffles its agents
    let draws = draws || seq == Some(false);
    let birth = if kind == 9 {
        let x = field(d, b"birth")?;
        let ptr = field(x, b"round")?;
        // SAFETY: an external pointer made by savvy for an `RxProgram`, kept
        // alive by the descriptor.
        Some(unsafe { savvy::get_external_pointer_addr(ptr) }? as *const RxProgram)
    } else {
        None
    };
    let attach = if kind == 9 {
        let x = field(d, b"birth")?;
        let int1 = |nm: &[u8]| -> savvy::Result<i32> {
            store::numbers_of::<i32>(field(x, nm)?, INTSXP)?
                .and_then(|v| v.first().copied())
                .ok_or_else(|| bad("birth attach"))
        };
        match int1(b"attach")? {
            0 => None,
            m => Some((
                m as u8,
                usize::try_from(int1(b"links")?).map_err(|_| bad("links"))?,
            )),
        }
    } else {
        None
    };
    let count = if kind == 9 {
        let x = field(d, b"birth")?;
        store::numbers_of::<i32>(field(x, b"count")?, INTSXP)?
            .and_then(|v| v.first().copied())
            .and_then(|c| usize::try_from(c).ok())
    } else {
        None
    };
    // attaching newborns draws, and so does drawing templates
    let draws = draws || attach.is_some() || count.is_some();
    let death = if kind == 8 {
        let x = field(d, b"death")?;
        let v = field(x, b"prune")?;
        // SAFETY: reading a type tag, then a logical of length 1.
        let ok = unsafe { rapi::TYPEOF(v) } == rapi::LGLSXP && len(v) == 1;
        // SAFETY: as above.
        Some(ok && unsafe { *rapi::LOGICAL_RO(v) } == 1)
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
        cross,
        count,
        attach,
        gby,
        seq,
        tell,
        mv,
        mixed,
        nb1,
        nbw,
        mt,
        death,
        birth,
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

/// The columns `augment_group()` adds to group `g` under the standing match, in
/// the order of `names` (`.partner`, `.role`, `.group_id`, `partner_<col>`):
/// each agent's first row of the match table, the partner's columns read from
/// the groups stacked (`bind_groups()`). `None` (decline) without a standing
/// match, or for `partner_<col>` of a match that is not of pairs.
fn match_inputs(
    st: SEXP,
    groups: SEXP,
    g: SEXP,
    names: &[&[u8]],
) -> savvy::Result<Option<Vec<Vals<'static>>>> {
    let Some(gi) = find_str(g, b".id")? else {
        return Ok(None);
    };
    let Some(gids) = store::numbers_of::<i32>(elt(g, gi), INTSXP)? else {
        return Ok(None);
    };
    match_inputs_ids(st, groups, gids, names)
}

/// [`match_inputs`] for the agents with ids `gids`.
fn match_inputs_ids(
    st: SEXP,
    groups: SEXP,
    gids: &[i32],
    names: &[&[u8]],
) -> savvy::Result<Option<Vec<Vals<'static>>>> {
    use std::borrow::Cow;
    use tidyabm_core::sample::NA_INT;
    let Some(mi) = find_str(st, b"match")? else {
        return Ok(None);
    };
    let ms = elt(st, mi);
    if ms == rapi_nil() {
        return Ok(None);
    }
    let (Some(ti), Some(si)) = (find_str(ms, b"match")?, find_str(ms, b"size")?) else {
        return Ok(None);
    };
    let (m, size) = (elt(ms, ti), elt(ms, si));
    let size = match store::numbers_of::<i32>(size, INTSXP)? {
        Some(v) if v.len() == 1 => v[0],
        _ => match store::numbers_of::<f64>(size, rapi::REALSXP)? {
            Some(v) if v.len() == 1 => v[0] as i32,
            _ => return Ok(None),
        },
    };
    let col =
        |nm: &[u8]| -> savvy::Result<Option<SEXP>> { Ok(find_str(m, nm)?.map(|j| elt(m, j))) };
    let (Some(mid), Some(mpart), Some(mrole), Some(mgid)) = (
        col(b".id")?,
        col(b".partner")?,
        col(b".role")?,
        col(b".group_id")?,
    ) else {
        return Ok(None);
    };
    let (Some(mid), Some(mpart), Some(mgid)) = (
        store::numbers_of::<i32>(mid, INTSXP)?,
        store::numbers_of::<i32>(mpart, INTSXP)?,
        store::numbers_of::<i32>(mgid, INTSXP)?,
    ) else {
        return Ok(None);
    };
    // SAFETY: reading a type tag.
    if unsafe { rapi::TYPEOF(mrole) } != STRSXP {
        return Ok(None);
    }
    let mrole = store::chars_of(mrole)?;
    let first = IdIndex::new(mid);
    let idx: Vec<Option<usize>> = gids.iter().map(|&id| first.get(id)).collect();
    // SAFETY: reading a static of R's.
    let na_str = unsafe { rapi::R_NaString } as usize;
    let partner: Vec<i32> = idx.iter().map(|r| r.map_or(NA_INT, |r| mpart[r])).collect();
    let mut out = Vec::with_capacity(names.len());
    // the stacked population, built once if a partner column is read
    let mut stack: Option<(Vec<usize>, IdIndex)> = None;
    for &b in names {
        out.push(match b {
            b".partner" => Vals::Int(Cow::Owned(partner.clone())),
            b".group_id" => Vals::Int(Cow::Owned(
                idx.iter().map(|r| r.map_or(NA_INT, |r| mgid[r])).collect(),
            )),
            b".role" => Vals::Str(Cow::Owned(
                idx.iter()
                    .map(|r| r.map_or(na_str, |r| mrole[r] as usize))
                    .collect(),
            )),
            _ => {
                let Some(c) = b.strip_prefix(b"partner_") else {
                    return Ok(None);
                };
                if size != 2 {
                    return Ok(None);
                }
                if stack.is_none() {
                    let sizes: Vec<usize> = (0..len(groups))
                        .map(|k| rows(elt(groups, k)).unwrap_or(0))
                        .collect();
                    let Some(Vals::Int(all)) = stacked_col_named(groups, b".id", &sizes)? else {
                        return Ok(None);
                    };
                    stack = Some((sizes, IdIndex::new(&all)));
                }
                let (sizes, at) = stack.as_ref().ok_or_else(|| bad("stack"))?;
                let Some(v) = stacked_col_named(groups, c, sizes)? else {
                    return Ok(None);
                };
                // combined[[col]][match(partner, combined$.id)]
                let rows_p: Vec<Option<usize>> = partner
                    .iter()
                    .map(|p| if *p == NA_INT { None } else { at.get(*p) })
                    .collect();
                // SAFETY: reading a static of R's.
                let na_real = unsafe { rapi::R_NaReal };
                match &v {
                    Vals::Dbl(x) => Vals::Dbl(Cow::Owned(
                        rows_p.iter().map(|r| r.map_or(na_real, |r| x[r])).collect(),
                    )),
                    Vals::Int(x) => Vals::Int(Cow::Owned(
                        rows_p.iter().map(|r| r.map_or(NA_INT, |r| x[r])).collect(),
                    )),
                    Vals::Lgl(x) => Vals::Lgl(Cow::Owned(
                        rows_p.iter().map(|r| r.map_or(NA_INT, |r| x[r])).collect(),
                    )),
                    Vals::Str(x) => Vals::Str(Cow::Owned(
                        rows_p.iter().map(|r| r.map_or(na_str, |r| x[r])).collect(),
                    )),
                }
            }
        });
    }
    Ok(Some(out))
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
    let mut placed: Option<(IdIndex, Vec<bool>)> = None;
    if plan.scope_match {
        if let Some(mi) = find_str(st, b"match")? {
            let m = elt(st, mi);
            if m != rapi_nil() {
                let Some(mm) = find_str(m, b"match")?.map(|i| elt(m, i)) else {
                    return decline("a match without its table");
                };
                if rows(mm).unwrap_or(0) > 0 {
                    // a rule that is not elementwise runs once per pair or group
                    // (below); one R evaluates would need R's grouping: declined
                    let r_grouped = plan
                        .groups
                        .iter()
                        .any(|g| g.order.iter().zip(&g.rew).any(|(&o, &e)| o < 0 && !e));
                    if r_grouped
                        || plan
                            .groups
                            .iter()
                            .any(|g| !g.ew && g.rew.len() != g.order.len())
                    {
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
                    // match() takes the first row of an id
                    let placed_ok: Vec<bool> = gid
                        .iter()
                        .map(|&g| g != tidyabm_core::sample::NA_INT)
                        .collect();
                    placed = Some((IdIndex::new(ids), placed_ok));
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
        let Some(mut ins) = inputs(&xs)? else {
            return decline("an input that is not a bare vector");
        };
        if !gp.mcols.is_empty() {
            let mnames: Vec<&[u8]> = gp.mcols.iter().map(|&c| bytes(c)).collect();
            match match_inputs(st, groups, g, &mnames)? {
                Some(v) => ins.extend(v),
                None => return decline("match columns that cannot be built natively"),
            }
        }
        // SAFETY: the program is kept alive by the plan's descriptor.
        let prog = unsafe { &*gp.prog };
        // R values of R-evaluated rules, kept while they are read
        let mut held: Vec<H> = Vec::new();
        // under a standing match, a rule that is not elementwise runs once per
        // `.group_id` (`run_rules()`'s grouped evaluation, as `eval_by_key()`)
        let pairs_mode = placed.is_some() && gp.rew.iter().any(|&e| !e);
        if pairs_mode && plan.mixed.is_some() {
            return decline("a mixed step under a standing match");
        }
        let (res, targets): (Vec<Vals>, &[SEXP]) = if pairs_mode {
            let Some(gid) = match_inputs(st, groups, g, &[b".group_id"])?.and_then(|mut v| v.pop())
            else {
                return decline("no .group_id for a grouped rule");
            };
            let Some(loc) = tidyabm_core::expr::group_rows(&gid) else {
                return decline("a .group_id of another kind");
            };
            let (nc, nm, ni) = (gp.cols.len(), gp.mcols.len(), ins.len());
            let mut vals = Vec::with_capacity(gp.order.len());
            for (pos, &o) in gp.order.iter().enumerate() {
                let k = o as usize;
                if gp.rew[pos] {
                    match prog.compute_one(k, &ins, n)? {
                        Ok(v) => vals.push(v),
                        Err(d) => {
                            expr::note_decline(&d);
                            return Ok(None);
                        }
                    }
                    continue;
                }
                let mut parts = Vec::with_capacity(loc.len());
                for rows_g in &loc {
                    let sliced: Vec<Vals> = ins
                        .iter()
                        .enumerate()
                        .map(|(j, v)| {
                            if j < nc || j >= ni - nm {
                                tidyabm_core::expr::take(v, rows_g)
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
                match expr::with_host(|h| tidyabm_core::expr::unchop(parts, &loc, n, h)) {
                    Ok(v) => vals.push(v),
                    Err(d) => {
                        expr::note_decline(&d);
                        return Ok(None);
                    }
                }
            }
            (vals, &gp.targets)
        } else {
            match &plan.mixed {
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
                            None => {
                                return decline("an R-evaluated rule gave another kind of value")
                            }
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
            }
        };
        let mut out = OwnedListSexp::new(res.len(), false)?;
        // the rows the step writes, when a match narrows it
        let mask: Option<Vec<bool>> = match &placed {
            None => None,
            Some((map, ok)) => {
                let Some(j) = find_str(g, b".id")? else {
                    return decline("a group without ids");
                };
                let Some(ids) = store::numbers_of::<i32>(elt(g, j), INTSXP)? else {
                    return decline("ids that are not integers");
                };
                let m: Vec<bool> = ids
                    .iter()
                    .map(|&id| map.get(id).is_some_and(|r| ok[r]))
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
    let at = IdIndex::new(&ids);
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
        if let Some(j) = at.get(nc) {
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

/// `x` reordered by `sample(x)` (R's `shuffle()`: a vector of one or none is
/// left alone): `sample.int(n)` without replacement drawn from R's generator.
fn shuffle(r: &mut crate::rng::RRng, x: &mut Vec<i32>) -> savvy::Result<()> {
    if x.len() <= 1 {
        return Ok(());
    }
    let idx = tidyabm_core::sample::sample_int(r, x.len(), x.len(), false)
        .map_err(|e| bad(&format!("shuffle: {e}")))?;
    *x = idx.iter().map(|&k| x[k as usize - 1]).collect();
    Ok(())
}

/// `new_match(id, partner, role = NA, group_id)`: the tibble `.id`, `.partner`,
/// `.role`, `.group_id` (`vctrs::new_data_frame(class = c("tbl_df", "tbl"))`).
fn match_table(id: &[i32], partner: &[i32], role: &[SEXP], gid: &[i32]) -> savvy::Result<H> {
    let n = id.len();
    let (ip, pp, gp) = (id.as_ptr(), partner.as_ptr(), gid.as_ptr());
    let rp = role.as_ptr();
    let has_role = role.len() == n;
    // SAFETY: main thread; every vector is protected from its allocation until
    // it is stored in the protected list; `ip`/`pp`/`gp` point at `n` ints each,
    // alive for this call; the list is kept before anything else allocates.
    unsafe {
        build(move || {
            let x = rapi::Rf_protect(rapi::Rf_allocVector(VECSXP, 4));
            for (k, src) in [ip, pp].iter().enumerate() {
                let v = rapi::Rf_allocVector(INTSXP, n as isize);
                rapi::SET_VECTOR_ELT(x, k as isize, v);
                std::ptr::copy_nonoverlapping(*src, rapi::DATAPTR_OR_NULL(v) as *mut i32, n);
            }
            let role = rapi::Rf_allocVector(STRSXP, n as isize);
            rapi::SET_VECTOR_ELT(x, 2, role);
            for i in 0..n {
                let r = if has_role {
                    *rp.add(i)
                } else {
                    rapi::R_NaString
                };
                rapi::SET_STRING_ELT(role, i as isize, r);
            }
            let g = rapi::Rf_allocVector(INTSXP, n as isize);
            rapi::SET_VECTOR_ELT(x, 3, g);
            std::ptr::copy_nonoverlapping(gp, rapi::DATAPTR_OR_NULL(g) as *mut i32, n);
            let nm = rapi::Rf_protect(rapi::Rf_allocVector(STRSXP, 4));
            for (k, s) in [c".id", c".partner", c".role", c".group_id"]
                .iter()
                .enumerate()
            {
                rapi::SET_STRING_ELT(nm, k as isize, rapi::Rf_mkChar(s.as_ptr()));
            }
            rapi::Rf_setAttrib(x, rapi::R_NamesSymbol, nm);
            let rn = rapi::Rf_protect(rapi::Rf_allocVector(INTSXP, 2));
            let rp = rapi::DATAPTR_OR_NULL(rn) as *mut i32;
            *rp = tidyabm_core::sample::NA_INT;
            *rp.add(1) = -(n as i32);
            rapi::Rf_setAttrib(x, rapi::Rf_install(c"row.names".as_ptr()), rn);
            let cl = rapi::Rf_protect(rapi::Rf_allocVector(STRSXP, 3));
            for (k, s) in [c"tbl_df", c"tbl", c"data.frame"].iter().enumerate() {
                rapi::SET_STRING_ELT(cl, k as isize, rapi::Rf_mkChar(s.as_ptr()));
            }
            rapi::Rf_setAttrib(x, rapi::Rf_install(c"class".as_ptr()), cl);
            rapi::Rf_unprotect(4);
            x
        })
    }
}

/// A logical program result as `eval_condition()` makes it: recycled, `NA`
/// (and only `NA`) false. `None` for anything not logical.
fn condition(v: &Vals, n: usize) -> Option<Vec<bool>> {
    let Vals::Lgl(x) = v else { return None };
    if x.len() != 1 && x.len() != n {
        return None;
    }
    Some(
        (0..n)
            .map(|i| x[if x.len() == 1 { 0 } else { i }] == 1)
            .collect(),
    )
}

/// The sorted values of a side column that `sort(unique())` orders the same in
/// every locale: numbers, logicals, and strings of lowercase ASCII letters only.
#[derive(Clone, Copy, PartialEq)]
enum Side {
    I(i32),
    D(f64),
    S(usize),
}

fn side_at(v: &Vals, i: usize) -> Option<Side> {
    use tidyabm_core::sample::NA_INT;
    match v {
        Vals::Int(x) | Vals::Lgl(x) => (x[i] != NA_INT).then_some(Side::I(x[i])),
        Vals::Dbl(x) => (!x[i].is_nan()).then_some(Side::D(x[i])),
        Vals::Str(x) => {
            // SAFETY: reading a static of R's.
            (x[i] != unsafe { rapi::R_NaString } as usize).then_some(Side::S(x[i]))
        }
    }
}

/// Two distinct side values in `sort()` order, or `None` (decline) where the
/// order could depend on the locale or the values are not two.
fn two_sides(vals: &[Side]) -> Option<Option<(Side, Side)>> {
    let mut u: Vec<Side> = Vec::new();
    for &v in vals {
        let same = |a: &Side| match (a, v) {
            (Side::D(p), Side::D(q)) => *p == q,
            (p, q) => *p == q,
        };
        if !u.iter().any(same) {
            u.push(v);
        }
    }
    if u.len() > 2 {
        return None;
    }
    if u.len() < 2 {
        return Some(None);
    }
    let key = |s: &Side| -> Option<Vec<u8>> {
        match s {
            Side::S(c) => {
                let b = bytes(*c as SEXP);
                b.iter().all(|c| c.is_ascii_lowercase()).then(|| b.to_vec())
            }
            _ => Some(Vec::new()),
        }
    };
    let (a, b) = (u[0], u[1]);
    let a_first = match (a, b) {
        (Side::I(p), Side::I(q)) => p < q,
        (Side::D(p), Side::D(q)) => p < q,
        (Side::S(_), Side::S(_)) => key(&a)? < key(&b)?,
        _ => return None,
    };
    Some(Some(if a_first { (a, b) } else { (b, a) }))
}

/// `abm_match()` natively (`run_match()`'s random, one_of and opposite_group
/// modes without roles, and [match_by_opposite()]): `eligible`/`among` over the
/// groups stacked, then the same draws in the same order from R's generator;
/// `state$match <- list(match = <table>, size = <size>)`.
fn exec_match(plan: &Plan, st: SEXP) -> savvy::Result<Option<H>> {
    use tidyabm_core::sample::NA_INT;
    let mt = plan.mt.as_ref().ok_or_else(|| bad("no match"))?;
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
    let Some(Vals::Int(ids)) = stacked_col_named(groups, b".id", &sizes)? else {
        return decline("ids that are not integers");
    };
    // the conditions over the agents
    let mut elig = vec![true; n];
    let mut among = vec![true; n];
    let mut role_c: Option<(Vec<bool>, Vec<bool>)> = None;
    if (mt.eligible || mt.among || !mt.roles.is_empty()) && n > 0 {
        if gp.prog.is_null() {
            return Err(bad("conditions without a program"));
        }
        let mut ins: Vec<Vals> = Vec::with_capacity(gp.cols.len() + gp.globs.len());
        for &c in &gp.cols {
            match stacked_pad(groups, c, &sizes)? {
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
        let mut k = 0;
        if mt.eligible {
            let Some(c) = condition(&res[k], n) else {
                return decline("an eligible condition that is not logical");
            };
            elig = c;
            k += 1;
        }
        if mt.among {
            let Some(c) = condition(&res[k], n) else {
                return decline("an among condition that is not logical");
            };
            among = c;
            k += 1;
        }
        if mt.roles.len() == 2 {
            let (Some(a), Some(b)) = (condition(&res[k], n), condition(&res[k + 1], n)) else {
                return decline("a role condition that is not logical");
            };
            role_c = Some((a, b));
        }
    }
    let col = |nm: Option<SEXP>| -> savvy::Result<Option<Vals<'static>>> {
        match nm {
            Some(c) => stacked_pad(groups, c, &sizes),
            None => Ok(None),
        }
    };
    let by = col(mt.by)?;
    let dot_by = col(mt.dot_by)?;
    if (mt.by.is_some() && by.is_none()) || (mt.dot_by.is_some() && dot_by.is_none()) {
        return decline("a by column of another kind");
    }
    let pool: Vec<i32> = (0..n).filter(|&i| elig[i]).map(|i| ids[i]).collect();
    let mut out_id: Vec<i32> = Vec::new();
    let mut out_partner: Vec<i32> = Vec::new();
    let mut out_gid: Vec<i32> = Vec::new();
    let mut out_role: Vec<SEXP> = Vec::new();
    // agent row by id (`match(ids, agents$.id)`), for roles and the network mode
    let row_of = if !mt.roles.is_empty() || mt.pair == 4 {
        IdIndex::new(&ids)
    } else {
        IdIndex::new(&[])
    };
    let pair2 =
        |a: &[i32], b: &[i32], off: i32, id: &mut Vec<i32>, pt: &mut Vec<i32>, g: &mut Vec<i32>| {
            id.extend_from_slice(a);
            id.extend_from_slice(b);
            pt.extend_from_slice(b);
            pt.extend_from_slice(a);
            for half in 0..2 {
                let _ = half;
                g.extend((1..=a.len() as i32).map(|k| k + off));
            }
        };
    let res: savvy::Result<Option<()>> = crate::rng::with_rng(|r| {
        match mt.pair {
            1 => {
                let mut p = pool.clone();
                shuffle(r, &mut p)?;
                let size = mt.size_n;
                let full = p.len() / size;
                p.truncate(full * size);
                if size == 2 {
                    let a: Vec<i32> = p.iter().step_by(2).copied().collect();
                    let b: Vec<i32> = p.iter().skip(1).step_by(2).copied().collect();
                    match &role_c {
                        None => pair2(&a, &b, 0, &mut out_id, &mut out_partner, &mut out_gid),
                        Some(rc) => {
                            let (ka, kb, ra, rb) = assign_roles(r, rc, &mt.roles, &a, &b, &row_of)?;
                            pair2(&ka, &kb, 0, &mut out_id, &mut out_partner, &mut out_gid);
                            out_role.extend(ra);
                            out_role.extend(rb);
                        }
                    }
                } else {
                    out_partner = vec![NA_INT; p.len()];
                    out_gid = (0..p.len()).map(|i| (i / size) as i32 + 1).collect();
                    out_id = p;
                }
            }
            2 => {
                let cands: Vec<i32> = (0..n).filter(|&i| among[i]).map(|i| ids[i]).collect();
                if pool.is_empty() || cands.is_empty() {
                    return Ok(Some(()));
                }
                let mut p = pool.clone();
                if cands.len() <= 1 {
                    p.retain(|x| !cands.contains(x));
                }
                if p.is_empty() {
                    return Ok(Some(()));
                }
                let dn = cands.len() as f64;
                let draw_k = |r: &mut crate::rng::RRng, k: usize| -> Vec<i32> {
                    (0..k)
                        .map(|_| cands[tidyabm_core::sample::RDraws::unif_index(r, dn) as usize])
                        .collect()
                };
                let mut draw = draw_k(r, p.len());
                loop {
                    let clash: Vec<usize> = (0..p.len()).filter(|&i| draw[i] == p[i]).collect();
                    if clash.is_empty() {
                        break;
                    }
                    let re = draw_k(r, clash.len());
                    for (j, &i) in clash.iter().enumerate() {
                        draw[i] = re[j];
                    }
                }
                match &role_c {
                    None => {
                        out_gid = (1..=p.len() as i32).collect();
                        out_partner = draw;
                        out_id = p;
                    }
                    Some(rc) => {
                        let (ka, kb, ra, _) = assign_roles(r, rc, &mt.roles, &p, &draw, &row_of)?;
                        out_gid = (1..=ka.len() as i32).collect();
                        out_partner = kb;
                        out_id = ka;
                        out_role = ra;
                    }
                }
            }
            5 => {
                // match_nearest() on one coordinate: per chooser, the candidate
                // with the least `(a^2 + b^2) - 2 * (a * b)` (itself excluded),
                // the first of ties (`max.col(-d, "first")`); a chooser whose
                // distances are none finite sits out
                let Some(xy) = by.as_ref() else {
                    return Ok(Some(()));
                };
                // SAFETY: reading a static of R's.
                let na = unsafe { rapi::R_NaReal };
                let x: Vec<f64> = match xy {
                    Vals::Dbl(v) => v.to_vec(),
                    Vals::Int(v) | Vals::Lgl(v) => v
                        .iter()
                        .map(|&e| if e == NA_INT { na } else { f64::from(e) })
                        .collect(),
                    Vals::Str(_) => return Ok(None),
                };
                let subs: Vec<usize> = (0..n).filter(|&i| elig[i]).collect();
                let cands: Vec<usize> = (0..n).filter(|&i| among[i]).collect();
                if subs.is_empty() || cands.is_empty() {
                    return Ok(Some(()));
                }
                // a missing coordinate makes max.col() answer NA: R's path
                if subs.iter().chain(&cands).any(|&i| x[i].is_nan()) {
                    return Ok(None);
                }
                for &s in &subs {
                    let a = x[s];
                    let mut best: Option<(f64, usize)> = None;
                    for &c in &cands {
                        let b = x[c];
                        let d = if ids[s] == ids[c] {
                            f64::INFINITY
                        } else {
                            (a * a + b * b) - 2.0 * (a * b)
                        };
                        if best.map_or(true, |(m, _)| d < m) {
                            best = Some((d, c));
                        }
                    }
                    if let Some((m, c)) = best {
                        if m.is_finite() {
                            out_id.push(ids[s]);
                            out_partner.push(ids[c]);
                        }
                    }
                }
                out_gid = (1..=out_id.len() as i32).collect();
            }
            4 => {
                // match_network(): each eligible agent's edge partners, the edge
                // list read both ways round and shuffled, the first per agent
                let Some(ei) = find_str(st, b"edges")? else {
                    return Ok(Some(()));
                };
                let e = elt(st, ei);
                if e == rapi_nil() || rows(e).unwrap_or(0) == 0 {
                    return Ok(Some(()));
                }
                let ecol = |nm: &[u8]| -> savvy::Result<Option<&[i32]>> {
                    match find_str(e, nm)? {
                        Some(j) => store::numbers_of::<i32>(elt(e, j), INTSXP),
                        None => Ok(None),
                    }
                };
                let (Some(from), Some(to)) = (ecol(b"from")?, ecol(b"to")?) else {
                    return Ok(None);
                };
                let inpool = IdIndex::new(&pool);
                let mut id: Vec<i32> = from.iter().chain(to.iter()).copied().collect();
                let mut nbr: Vec<i32> = to.iter().chain(from.iter()).copied().collect();
                let keep: Vec<bool> = (0..id.len())
                    .map(|i| inpool.get(id[i]).is_some() && row_of.get(nbr[i]).is_some())
                    .collect();
                id = id
                    .iter()
                    .zip(&keep)
                    .filter(|(_, &k)| k)
                    .map(|(&v, _)| v)
                    .collect();
                nbr = nbr
                    .iter()
                    .zip(&keep)
                    .filter(|(_, &k)| k)
                    .map(|(&v, _)| v)
                    .collect();
                if id.is_empty() {
                    return Ok(Some(()));
                }
                let o = tidyabm_core::sample::sample_int(r, id.len(), id.len(), false)
                    .map_err(|e| bad(&format!("network match: {e}")))?;
                let mut seen = std::collections::HashSet::new();
                for &k in &o {
                    let (a, b) = (id[k as usize - 1], nbr[k as usize - 1]);
                    if seen.insert(a) {
                        out_id.push(a);
                        out_partner.push(b);
                    }
                }
                out_gid = (1..=out_id.len() as i32).collect();
            }
            _ => {
                let side = by.as_ref().ok_or_else(|| bad("no side"))?;
                match &dot_by {
                    None => {
                        // match_opposite(): the sides among the agents taking part
                        let take: Vec<usize> = (0..n).filter(|&i| elig[i]).collect();
                        let vals: Vec<Side> =
                            take.iter().filter_map(|&i| side_at(side, i)).collect();
                        let Some(two) = two_sides(&vals) else {
                            return Ok(None);
                        };
                        let Some((v1, v2)) = two else {
                            return Ok(Some(()));
                        };
                        let pick = |v: Side| -> Vec<i32> {
                            take.iter()
                                .filter(|&&i| side_at(side, i).is_some_and(|s| same_side(s, v)))
                                .map(|&i| ids[i])
                                .collect()
                        };
                        let (mut g1, mut g2) = (pick(v1), pick(v2));
                        shuffle(r, &mut g1)?;
                        shuffle(r, &mut g2)?;
                        let k = g1.len().min(g2.len());
                        match &role_c {
                            None => pair2(
                                &g1[..k],
                                &g2[..k],
                                0,
                                &mut out_id,
                                &mut out_partner,
                                &mut out_gid,
                            ),
                            Some(rc) => {
                                let (ka, kb, ra, rb) =
                                    assign_roles(r, rc, &mt.roles, &g1[..k], &g2[..k], &row_of)?;
                                pair2(&ka, &kb, 0, &mut out_id, &mut out_partner, &mut out_gid);
                                out_role.extend(ra);
                                out_role.extend(rb);
                            }
                        }
                    }
                    Some(key) => {
                        // match_by_opposite(): partitions by a number key, in key order
                        let keys: Vec<Option<f64>> = (0..n)
                            .map(|i| match key {
                                Vals::Int(x) => (x[i] != NA_INT).then_some(f64::from(x[i])),
                                Vals::Dbl(x) => (!x[i].is_nan()).then_some(x[i]),
                                _ => None,
                            })
                            .collect();
                        if !matches!(key, Vals::Int(_) | Vals::Dbl(_)) {
                            return Ok(None);
                        }
                        let rows: Vec<usize> = (0..n)
                            .filter(|&i| elig[i] && keys[i].is_some() && side_at(side, i).is_some())
                            .collect();
                        let mut parts: Vec<(f64, Vec<usize>)> = Vec::new();
                        let mut at: std::collections::HashMap<u64, usize> =
                            std::collections::HashMap::new();
                        for &i in &rows {
                            let kv = keys[i].unwrap_or(0.0);
                            let b = if kv == 0.0 { 0 } else { kv.to_bits() };
                            let p = *at.entry(b).or_insert_with(|| {
                                parts.push((kv, Vec::new()));
                                parts.len() - 1
                            });
                            parts[p].1.push(i);
                        }
                        let mut todo: Vec<(f64, Vec<usize>, Side, Side)> = Vec::new();
                        for (kv, loc) in parts {
                            let vals: Vec<Side> =
                                loc.iter().filter_map(|&i| side_at(side, i)).collect();
                            let Some(two) = two_sides(&vals) else {
                                return Ok(None);
                            };
                            if let Some((v1, v2)) = two {
                                todo.push((kv, loc, v1, v2));
                            }
                        }
                        todo.sort_by(|a, b| {
                            a.0.partial_cmp(&b.0).unwrap_or(std::cmp::Ordering::Equal)
                        });
                        let mut off = 0i32;
                        for (_, loc, v1, v2) in todo {
                            let pick = |v: Side| -> Vec<i32> {
                                loc.iter()
                                    .filter(|&&i| side_at(side, i).is_some_and(|s| same_side(s, v)))
                                    .map(|&i| ids[i])
                                    .collect()
                            };
                            let (mut g1, mut g2) = (pick(v1), pick(v2));
                            shuffle(r, &mut g1)?;
                            shuffle(r, &mut g2)?;
                            let k = g1.len().min(g2.len());
                            pair2(
                                &g1[..k],
                                &g2[..k],
                                off,
                                &mut out_id,
                                &mut out_partner,
                                &mut out_gid,
                            );
                            off += k as i32;
                        }
                    }
                }
            }
        }
        Ok(Some(()))
    });
    if res?.is_none() {
        return decline("sides that are not two plain values");
    }
    let tbl = match_table(&out_id, &out_partner, &out_role, &out_gid)?;
    // list(match = <table>, size = <size>)
    let tp = tbl.get();
    let size = mt.size;
    // SAFETY: main thread; `tp` is kept by `tbl`, `size` by the descriptor; the
    // list and its names are protected while built, then kept.
    let m = unsafe {
        build(move || {
            let l = rapi::Rf_protect(rapi::Rf_allocVector(VECSXP, 2));
            rapi::SET_VECTOR_ELT(l, 0, tp);
            rapi::SET_VECTOR_ELT(l, 1, size);
            let nm = rapi::Rf_protect(rapi::Rf_allocVector(STRSXP, 2));
            rapi::SET_STRING_ELT(nm, 0, rapi::Rf_mkChar(c"match".as_ptr()));
            rapi::SET_STRING_ELT(nm, 1, rapi::Rf_mkChar(c"size".as_ptr()));
            rapi::Rf_setAttrib(l, rapi::R_NamesSymbol, nm);
            rapi::Rf_unprotect(2);
            l
        })
    }?;
    // SAFETY: as above (the name's CHARSXP is kept while used).
    let key = unsafe { build(|| rapi::Rf_mkChar(c"match".as_ptr())) }?;
    Ok(Some(set_named(st, key.get(), m.get())?))
}

/// [assign_roles()] for pairs `(a[i], b[i])`: `a` takes the first role where
/// its first condition and `b`'s second hold, else the second role where the
/// reverse holds (a pair where both do flips a fair coin, `runif(n) < 0.5`,
/// drawn for those pairs in order); pairs where neither holds are dropped.
/// Returns the kept `a`, `b` and their roles.
#[allow(clippy::type_complexity)]
fn assign_roles(
    r: &mut crate::rng::RRng,
    rc: &(Vec<bool>, Vec<bool>),
    names: &[SEXP],
    a: &[i32],
    b: &[i32],
    row_of: &IdIndex,
) -> savvy::Result<(Vec<i32>, Vec<i32>, Vec<SEXP>, Vec<SEXP>)> {
    let at = |id: &i32| row_of.get(*id).ok_or_else(|| bad("role: unknown id"));
    let (c1, c2) = rc;
    let mut ab = Vec::with_capacity(a.len());
    let mut ba = Vec::with_capacity(a.len());
    for (x, y) in a.iter().zip(b) {
        let (i, j) = (at(x)?, at(y)?);
        ab.push(c1[i] && c2[j]);
        ba.push(c1[j] && c2[i]);
    }
    let both: Vec<usize> = (0..a.len()).filter(|&k| ab[k] && ba[k]).collect();
    if !both.is_empty() {
        let mut u = vec![0.0; both.len()];
        r.fill(&mut u, crate::rng::Draw::Rmath2(rapi::Rf_runif, 0.0, 1.0))?;
        for (q, &k) in both.iter().enumerate() {
            if u[q] < 0.5 {
                ab[k] = false;
            }
        }
    }
    let (mut ka, mut kb, mut ra, mut rb) = (Vec::new(), Vec::new(), Vec::new(), Vec::new());
    for k in 0..a.len() {
        let (x, y) = if ab[k] {
            (names[0], names[1])
        } else if ba[k] {
            (names[1], names[0])
        } else {
            continue;
        };
        ka.push(a[k]);
        kb.push(b[k]);
        ra.push(x);
        rb.push(y);
    }
    Ok((ka, kb, ra, rb))
}

/// Group `g` without the rows where `drop` holds (`vctrs::vec_slice(g, !drop)`):
/// each column sliced, the frame's attributes kept with compact row names.
/// `None` (decline) for a column that is not a bare atomic or list vector.
fn slice_group(g: SEXP, drop: &[bool]) -> savvy::Result<Option<H>> {
    let k = len(g);
    let cols: Vec<SEXP> = (0..k).map(|j| elt(g, j)).collect();
    if !all_bare(&cols)? {
        return Ok(None);
    }
    for &c in &cols {
        // SAFETY: reading a type tag.
        let t = unsafe { rapi::TYPEOF(c) };
        if !matches!(t, rapi::LGLSXP | INTSXP | rapi::REALSXP | STRSXP | VECSXP)
            || len(c) != drop.len()
        {
            return Ok(None);
        }
    }
    let keep: Vec<usize> = (0..drop.len()).filter(|&i| !drop[i]).collect();
    let m = keep.len();
    let (kp, cp) = (keep.as_ptr(), cols.as_ptr());
    // SAFETY: main thread; the new frame is protected while filled, each new
    // column stored into it as soon as it is allocated; `kp` (m indices, each
    // below the column length) and `cp` (k columns, protected by `g`) are alive
    // for this call; the frame is kept before anything else allocates.
    unsafe {
        build(move || {
            let y = rapi::Rf_protect(rapi::Rf_allocVector(VECSXP, k as isize));
            for j in 0..k {
                let c = *cp.add(j);
                let t = rapi::TYPEOF(c);
                let v = rapi::Rf_allocVector(t, m as isize);
                rapi::SET_VECTOR_ELT(y, j as isize, v);
                match t {
                    rapi::REALSXP => {
                        let (src, dst) = (rapi::REAL_RO(c), rapi::DATAPTR_OR_NULL(v) as *mut f64);
                        for i in 0..m {
                            *dst.add(i) = *src.add(*kp.add(i));
                        }
                    }
                    rapi::LGLSXP | INTSXP => {
                        let src = if t == INTSXP {
                            rapi::INTEGER_RO(c)
                        } else {
                            rapi::LOGICAL_RO(c)
                        };
                        let dst = rapi::DATAPTR_OR_NULL(v) as *mut i32;
                        for i in 0..m {
                            *dst.add(i) = *src.add(*kp.add(i));
                        }
                    }
                    STRSXP => {
                        for i in 0..m {
                            rapi::SET_STRING_ELT(
                                v,
                                i as isize,
                                rapi::STRING_ELT(c, *kp.add(i) as isize),
                            );
                        }
                    }
                    _ => {
                        for i in 0..m {
                            rapi::SET_VECTOR_ELT(
                                v,
                                i as isize,
                                rapi::VECTOR_ELT(c, *kp.add(i) as isize),
                            );
                        }
                    }
                }
            }
            rapi::SHALLOW_DUPLICATE_ATTRIB(y, g);
            let rn = rapi::Rf_protect(rapi::Rf_allocVector(INTSXP, 2));
            let rp = rapi::DATAPTR_OR_NULL(rn) as *mut i32;
            *rp = tidyabm_core::sample::NA_INT;
            *rp.add(1) = -(m as i32);
            rapi::Rf_setAttrib(y, rapi::Rf_install(c"row.names".as_ptr()), rn);
            rapi::Rf_unprotect(2);
            y
        })
    }
    .map(Some)
}

/// `abm_death(when =)` natively ([run_death()]): the condition over each group it
/// applies to (with the standing match's columns where read), the rows where it
/// holds removed. Edges that would need pruning, and relations, go to R.
fn exec_death(plan: &Plan, st: SEXP) -> savvy::Result<Option<H>> {
    let prune = plan.death.unwrap_or(true);
    if let Some(ri) = find_str(st, b"relations")? {
        if len(elt(st, ri)) > 0 {
            return decline("a relation");
        }
    }
    let Some(svals) = scope_values(plan)? else {
        return decline("a formula-environment value that cannot be read");
    };
    let gi = find_str(st, b"groups")?.ok_or_else(|| bad("no groups"))?;
    let groups = elt(st, gi);
    let gnames = names_of(groups)?;
    let mut new_groups: Vec<(usize, H)> = Vec::new();
    let mut removed: Vec<i32> = Vec::new();
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
        let Some(mut ins) = inputs(&xs)? else {
            return decline("an input that is not a bare vector");
        };
        if !gp.mcols.is_empty() {
            let mnames: Vec<&[u8]> = gp.mcols.iter().map(|&c| bytes(c)).collect();
            match match_inputs(st, groups, g, &mnames)? {
                Some(v) => ins.extend(v),
                None => return decline("match columns that cannot be built natively"),
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
        let Some(dead) = condition(&res[0], n) else {
            return decline("a condition that is not logical");
        };
        if !dead.iter().any(|&d| d) {
            continue;
        }
        let Some(ii) = find_str(g, b".id")? else {
            return decline("a group without ids");
        };
        let Some(ids) = store::numbers_of::<i32>(elt(g, ii), INTSXP)? else {
            return decline("ids that are not integers");
        };
        removed.extend(ids.iter().zip(&dead).filter(|(_, &d)| d).map(|(&id, _)| id));
        match slice_group(g, &dead)? {
            Some(h) => new_groups.push((k, h)),
            None => return decline("a column that cannot be sliced natively"),
        }
    }
    let mut cur_st: Option<H> = None;
    // edges: `edge_max` kept as R keeps it; the dead's edges dropped (FR12)
    let mut pruned: Option<H> = None;
    if !removed.is_empty() && prune {
        if let Some(ei) = find_str(st, b"edges")? {
            let e = elt(st, ei);
            let from = find_str(e, b"from")?.map(|j| elt(e, j));
            if let Some(fr) = from.filter(|&f| len(f) > 0) {
                let to = find_str(e, b"to")?
                    .map(|j| elt(e, j))
                    .ok_or_else(|| bad("edges without to"))?;
                let (Some(fv), Some(tv)) = (
                    store::numbers_of::<i32>(fr, INTSXP)?,
                    store::numbers_of::<i32>(to, INTSXP)?,
                ) else {
                    return decline("edges that are not integers");
                };
                if fv.contains(&tidyabm_core::sample::NA_INT)
                    || tv.contains(&tidyabm_core::sample::NA_INT)
                {
                    return decline("edges with NA");
                }
                let reuse = match find_str(st, b"edge_max")? {
                    Some(mi) => {
                        let em = elt(st, mi);
                        find_str(em, b"edges")?.map(|j| elt(em, j)) == Some(e)
                    }
                    None => false,
                };
                let mx = if reuse {
                    let em = elt(
                        st,
                        find_str(st, b"edge_max")?.ok_or_else(|| bad("edge_max"))?,
                    );
                    match find_str(em, b"max")?.map(|j| elt(em, j)) {
                        Some(v) => match store::numbers_of::<i32>(v, INTSXP)? {
                            Some(x) if x.len() == 1 => x[0],
                            _ => return decline("an edge_max of another kind"),
                        },
                        None => return decline("an edge_max without max"),
                    }
                } else {
                    let mx = fv
                        .iter()
                        .chain(tv.iter())
                        .copied()
                        .max()
                        .unwrap_or(i32::MIN);
                    // state$edge_max <- list(edges = <edges>, max = <max>)
                    // SAFETY: main thread; `e` is protected by the state; the
                    // list, its names and the scalar are protected while built.
                    let em = unsafe {
                        build(move || {
                            let l = rapi::Rf_protect(rapi::Rf_allocVector(VECSXP, 2));
                            rapi::SET_VECTOR_ELT(l, 0, e);
                            rapi::SET_VECTOR_ELT(l, 1, rapi::Rf_ScalarInteger(mx));
                            let nm = rapi::Rf_protect(rapi::Rf_allocVector(STRSXP, 2));
                            rapi::SET_STRING_ELT(nm, 0, rapi::Rf_mkChar(c"edges".as_ptr()));
                            rapi::SET_STRING_ELT(nm, 1, rapi::Rf_mkChar(c"max".as_ptr()));
                            rapi::Rf_setAttrib(l, rapi::R_NamesSymbol, nm);
                            rapi::Rf_unprotect(2);
                            l
                        })
                    }?;
                    // SAFETY: as above.
                    let key = unsafe { build(|| rapi::Rf_mkChar(c"edge_max".as_ptr())) }?;
                    cur_st = Some(set_named(st, key.get(), em.get())?);
                    mx
                };
                if removed.iter().copied().min().unwrap_or(i32::MAX) <= mx {
                    // edges[!(from %in% removed | to %in% removed), , drop = FALSE]
                    let dead: std::collections::HashSet<i32> = removed.iter().copied().collect();
                    let keep: Vec<bool> = fv
                        .iter()
                        .zip(tv)
                        .map(|(f, t)| !(dead.contains(f) || dead.contains(t)))
                        .collect();
                    if keep.iter().any(|&k| !k) {
                        match slice_tibble(e, &keep)? {
                            Some(h) => pruned = Some(h),
                            None => return decline("edges that cannot be sliced natively"),
                        }
                    }
                }
            }
        }
    }
    // commit
    let base_st = cur_st.as_ref().map_or(st, |h| h.get());
    let mut gs: Option<H> = None;
    for (k, h) in &new_groups {
        let cur = gs.as_ref().map_or(groups, |x| x.get());
        // SAFETY: reading a static of R's (unused name: `k` is in range).
        gs = Some(with_elt(cur, *k, h.get(), unsafe { rapi::R_BlankString })?);
    }
    let mut out = match &gs {
        // SAFETY: as above.
        Some(g) => with_elt(base_st, gi, g.get(), unsafe { rapi::R_BlankString })?,
        None => keep(base_st)?,
    };
    if let Some(pe) = &pruned {
        let ei = find_str(out.get(), b"edges")?.ok_or_else(|| bad("edges"))?;
        // SAFETY: reading a static of R's (unused name: `ei` is in range).
        out = with_elt(out.get(), ei, pe.get(), unsafe { rapi::R_BlankString })?;
    }
    // `state$relations <- prune_relations(NULL, ...)`: a NULL element is dropped
    if let Some(ri) = find_str(out.get(), b"relations")? {
        if elt(out.get(), ri) == rapi_nil() {
            out = without(out.get(), b"relations")?;
        }
    }
    Ok(Some(out))
}

/// A group program's inputs over rows `rows` of group `g` (all rows: `None`):
/// its columns (and the standing match's) sliced, then globals and scope values
/// whole. `None` (decline) where R would see something else.
fn entry_inputs<'a>(
    st: SEXP,
    groups: SEXP,
    g: SEXP,
    gp: &GroupPlan,
    svals: &Option<H>,
    rows: Option<&[usize]>,
) -> savvy::Result<Option<Vec<Vals<'a>>>> {
    let cn = names_of(g)?;
    let mut xs = Vec::with_capacity(gp.cols.len() + gp.globs.len());
    for &c in &gp.cols {
        let Some(j) = find(cn, c) else {
            return Ok(None);
        };
        xs.push(elt(g, j));
    }
    let nc = xs.len();
    if !read_globals(st, &gp.globs, &mut xs)? || !read_scope(svals, &gp.scope, &mut xs) {
        return Ok(None);
    }
    let Some(mut ins) = inputs(&xs)? else {
        return Ok(None);
    };
    if !gp.mcols.is_empty() {
        let mnames: Vec<&[u8]> = gp.mcols.iter().map(|&c| bytes(c)).collect();
        match match_inputs(st, groups, g, &mnames)? {
            Some(v) => ins.extend(v),
            None => return Ok(None),
        }
    }
    if let Some(r) = rows {
        let nm = gp.mcols.len();
        let ni = ins.len();
        ins = ins
            .into_iter()
            .enumerate()
            .map(|(j, v)| {
                if j < nc || j >= ni - nm {
                    tidyabm_core::expr::take(&v, r)
                } else {
                    v
                }
            })
            .collect();
    }
    Ok(Some(ins))
}

/// `x` (length 1 or `n`) as `n` values.
fn recycle<'a>(x: Vals<'a>, n: usize) -> Vals<'a> {
    if x.len() == 1 && n != 1 {
        tidyabm_core::expr::take(&x, &vec![0; n])
    } else {
        x
    }
}

fn rank(v: &Vals) -> u8 {
    match v {
        Vals::Lgl(_) => 0,
        Vals::Int(_) => 1,
        Vals::Dbl(_) => 2,
        Vals::Str(_) => 3,
    }
}

/// `abm_birth(when =)` natively ([birth_by_condition()], no `attach_via`): per
/// group the parents (`when`), their offspring counts (`times`, through R's own
/// `as.integer(round())`), the parents' `cost` and the offspring's `inherit`,
/// each evaluated in R's order over the rows R evaluates it over; offspring
/// appended with new ids from `state$next_id`.
fn exec_birth(plan: &Plan, st: SEXP) -> savvy::Result<Option<H>> {
    use std::borrow::Cow;
    use tidyabm_core::sample::NA_INT;
    let round_prog = plan.birth.ok_or_else(|| bad("no birth"))?;
    if let Some(c) = plan.count {
        return exec_birth_count(plan, st, c);
    }
    if let Some(ri) = find_str(st, b"relations")? {
        if len(elt(st, ri)) > 0 {
            return decline("a relation");
        }
    }
    let Some(svals) = scope_values(plan)? else {
        return decline("a formula-environment value that cannot be read");
    };
    let gi = find_str(st, b"groups")?.ok_or_else(|| bad("no groups"))?;
    let groups = elt(st, gi);
    let gnames = names_of(groups)?;
    let Some(ni) = find_str(st, b"next_id")? else {
        return decline("no next_id");
    };
    let mut next_id = match store::numbers_of::<i32>(elt(st, ni), INTSXP)? {
        Some(v) if v.len() == 1 && v[0] != NA_INT => v[0],
        _ => return decline("a next_id that is not one integer"),
    };
    let start_id = next_id;
    // each newborn's id and its parent's, in the order R makes them
    let mut born: Vec<(i32, i32)> = Vec::new();
    // the agents before the births, stacked (`attach_newborns()`'s `existing`)
    let existing: Vec<i32> = if plan.attach.is_some() {
        let sizes: Vec<usize> = (0..len(groups))
            .map(|k| rows(elt(groups, k)).unwrap_or(0))
            .collect();
        match stacked_col_named(groups, b".id", &sizes)? {
            Some(Vals::Int(v)) => v.to_vec(),
            _ => return decline("ids that are not integers"),
        }
    } else {
        Vec::new()
    };
    let mut new_groups: Vec<(usize, H)> = Vec::new();
    let mut k0 = 0;
    while k0 < plan.groups.len() {
        // this group's programs: when, then times / cost / inherit
        let name = plan.groups[k0].name;
        let mut k1 = k0;
        while k1 < plan.groups.len() && plan.groups[k1].name == name {
            k1 += 1;
        }
        let entries = &plan.groups[k0..k1];
        k0 = k1;
        let part = |p: u8| entries.iter().find(|e| e.part == p);
        let Some(k) = find(gnames, name) else {
            return decline("a group not found");
        };
        let g = elt(groups, k);
        let Some(n) = rows(g) else {
            return decline("a group without columns");
        };
        if n == 0 {
            continue;
        }
        let eval = |gp: &GroupPlan,
                    rows: Option<&[usize]>,
                    m: usize|
         -> savvy::Result<Option<Vec<Vals<'static>>>> {
            if gp.prog.is_null() {
                // a part that does not fit this group, reached: R's error
                expr::note_decline(&Decline("a birth formula that does not fit the group"));
                return Ok(None);
            }
            let Some(ins) = entry_inputs(st, groups, g, gp, &svals, rows)? else {
                return Ok(None);
            };
            // SAFETY: the program is kept alive by the plan's descriptor.
            let prog = unsafe { &*gp.prog };
            match prog.compute(ins, m, m)? {
                Ok(r) => Ok(Some(
                    r.into_iter().map(|v| expr::owned(recycle(v, m))).collect(),
                )),
                Err(d) => {
                    expr::note_decline(&d);
                    Ok(None)
                }
            }
        };
        let w = part(0).ok_or_else(|| bad("birth without when"))?;
        let Some(wv) = eval(w, None, n)? else {
            return Ok(None);
        };
        let Some(parents) = condition(&wv[0], n) else {
            return decline("a condition that is not logical");
        };
        let prows: Vec<usize> = (0..n).filter(|&i| parents[i]).collect();
        if prows.is_empty() {
            continue;
        }
        let m = prows.len();
        let reps: Vec<usize> = match part(1) {
            None => vec![1; m],
            Some(t) => {
                let Some(tv) = eval(t, Some(&prows), m)? else {
                    return Ok(None);
                };
                let v = tv.into_iter().next().ok_or_else(|| bad("times"))?;
                if !matches!(v, Vals::Int(_) | Vals::Dbl(_)) {
                    return decline("times that are not numbers");
                }
                // SAFETY: kept alive by the descriptor.
                let rp = unsafe { &*round_prog };
                let r = match rp.compute(vec![v], m, m)? {
                    Ok(mut r) => r.remove(0),
                    Err(d) => {
                        expr::note_decline(&d);
                        return Ok(None);
                    }
                };
                let Vals::Int(r) = recycle(r, m) else {
                    return decline("counts that are not integers");
                };
                r.iter()
                    .map(|&c| if c == NA_INT || c < 0 { 0 } else { c as usize })
                    .collect()
            }
        };
        let total: usize = reps.iter().sum();
        if total == 0 {
            continue;
        }
        let kid_rows: Vec<usize> = prows
            .iter()
            .zip(&reps)
            .flat_map(|(&r, &c)| std::iter::repeat(r).take(c))
            .collect();
        // the group's columns (bare atomic only: R appends them with `c()`)
        let cn: Vec<SEXP> = names_of(g)?.to_vec();
        let cols: Vec<SEXP> = (0..len(g)).map(|j| elt(g, j)).collect();
        if !all_bare(&cols)? {
            return decline("a column with attributes");
        }
        let mut gcols: Vec<Vals> = Vec::with_capacity(cols.len());
        for &c in &cols {
            match expr::vals_of(c, false)? {
                Some(v) if v.len() == n => gcols.push(expr::owned(v)),
                _ => return decline("a column that is not a bare atomic vector"),
            }
        }
        let mut kcols: Vec<Vals> = gcols
            .iter()
            .map(|v| tidyabm_core::expr::take(v, &kid_rows))
            .collect();
        let idj = cn
            .iter()
            .position(|&c| bytes(c) == b".id")
            .ok_or_else(|| bad("no .id"))?;
        if !matches!(gcols[idj], Vals::Int(_)) {
            return decline("ids that are not integers");
        }
        let Some(last) = next_id.checked_add(total as i32) else {
            return decline("ids past the integer range");
        };
        kcols[idj] = Vals::Int(Cow::Owned((next_id..last).collect()));
        if let Vals::Int(pid) = &gcols[idj] {
            for (q, &r) in kid_rows.iter().enumerate() {
                born.push((next_id + q as i32, pid[r]));
            }
        }
        next_id = last;
        // cost: over the parents, written to the parents and given to each offspring
        if let Some(c) = part(2) {
            let Some(vals) = eval(c, Some(&prows), m)? else {
                return Ok(None);
            };
            for (r, v) in vals.into_iter().enumerate() {
                let Some(j) = find(&cn, c.targets[r]) else {
                    return decline("a cost target that is not a column");
                };
                if rank(&v) < rank(&gcols[j]) || (rank(&v) == 3) != (rank(&gcols[j]) == 3) {
                    return decline("a cost of another type than its column");
                }
                let mut mask = vec![false; n];
                for &p in &prows {
                    mask[p] = true;
                }
                // replace(g[[target]], parents, val): the parents' rows in order
                let mut full_rows = vec![0usize; n];
                for (q, &p) in prows.iter().enumerate() {
                    full_rows[p] = q;
                }
                let spread = tidyabm_core::expr::take(&v, &full_rows);
                let newcol = match expr::with_host(|h| {
                    tidyabm_core::expr::keep_old(spread, Some(&gcols[j]), &mask, h)
                }) {
                    Ok(x) => expr::owned(x),
                    Err(d) => {
                        expr::note_decline(&d);
                        return Ok(None);
                    }
                };
                gcols[j] = newcol;
                let per_kid: Vec<usize> = (0..m)
                    .flat_map(|q| std::iter::repeat(q).take(reps[q]))
                    .collect();
                kcols[j] = tidyabm_core::expr::take(&v, &per_kid);
            }
        }
        // inherit: over one row per offspring (the group as it was), to the offspring
        if let Some(ih) = part(3) {
            let Some(vals) = eval(ih, Some(&kid_rows), total)? else {
                return Ok(None);
            };
            for (r, v) in vals.into_iter().enumerate() {
                let Some(j) = find(&cn, ih.targets[r]) else {
                    return decline("an inherit target that is not a column");
                };
                kcols[j] = v;
            }
        }
        // appended column by column (`append_rows()`'s `c()`), same types only
        if gcols.iter().zip(&kcols).any(|(a, b)| rank(a) != rank(b)) {
            return decline("offspring columns of another type");
        }
        let all_n = n + total;
        let mut out = OwnedListSexp::new(cols.len(), false)?;
        for (j, (a, b)) in gcols.iter().zip(&kcols).enumerate() {
            let both: Vals = match (a, b) {
                (Vals::Lgl(x), Vals::Lgl(y)) => Vals::Lgl(Cow::Owned([&x[..], &y[..]].concat())),
                (Vals::Int(x), Vals::Int(y)) => Vals::Int(Cow::Owned([&x[..], &y[..]].concat())),
                (Vals::Dbl(x), Vals::Dbl(y)) => Vals::Dbl(Cow::Owned([&x[..], &y[..]].concat())),
                (Vals::Str(x), Vals::Str(y)) => Vals::Str(Cow::Owned([&x[..], &y[..]].concat())),
                _ => return Err(bad("append types")),
            };
            out.set_value(j, expr::to_r(&both, all_n)?)?;
        }
        let frame = tbl_frame(out.inner(), &cn, all_n)?;
        new_groups.push((k, frame));
    }
    if next_id == start_id {
        return Ok(Some(keep(st)?));
    }
    // `attach_newborns()`: the new edges, drawn after every group's births
    let new_edges = match plan.attach {
        Some((mode, links)) => match attach_draws(st, mode, links, &born, &existing)? {
            Some(e) => e,
            None => return Ok(None),
        },
        None => None,
    };
    // commit: the groups, then `state$next_id`
    let mut gs: Option<H> = None;
    for (k, h) in &new_groups {
        let cur = gs.as_ref().map_or(groups, |x| x.get());
        // SAFETY: reading a static of R's (unused name: `k` is in range).
        gs = Some(with_elt(cur, *k, h.get(), unsafe { rapi::R_BlankString })?);
    }
    let gsv = gs.ok_or_else(|| bad("births without groups"))?;
    // SAFETY: as above.
    let s1 = with_elt(st, gi, gsv.get(), unsafe { rapi::R_BlankString })?;
    // SAFETY: main thread; the scalar is kept before anything else allocates.
    let nid = unsafe { build(move || rapi::Rf_ScalarInteger(next_id)) }?;
    let s1g = s1.get();
    let ni2 = find_str(s1g, b"next_id")?.ok_or_else(|| bad("next_id"))?;
    // SAFETY: as above (unused name: `ni2` is in range).
    let s2 = with_elt(s1g, ni2, nid.get(), unsafe { rapi::R_BlankString })?;
    match new_edges {
        None => Ok(Some(s2)),
        Some(e) => {
            let ei = find_str(s2.get(), b"edges")?.ok_or_else(|| bad("edges"))?;
            // SAFETY: as above (unused name: `ei` is in range).
            Ok(Some(with_elt(s2.get(), ei, e.get(), unsafe {
                rapi::R_BlankString
            })?))
        }
    }
}

/// The first position of each id (`match(id, ids)`): a direct table when the
/// ids are small positive numbers (agent ids are), else a hash map.
enum IdIndex {
    Dense(Vec<u32>),
    Map(std::collections::HashMap<i32, usize>),
}

impl IdIndex {
    fn new(ids: &[i32]) -> IdIndex {
        let max = ids.iter().copied().max().unwrap_or(0);
        let min = ids.iter().copied().min().unwrap_or(1);
        if min >= 1 && (max as usize) <= 4 * ids.len() + 1024 {
            let mut t = vec![u32::MAX; max as usize + 1];
            for (i, &id) in ids.iter().enumerate().rev() {
                t[id as usize] = i as u32;
            }
            IdIndex::Dense(t)
        } else {
            let mut m = std::collections::HashMap::with_capacity(ids.len());
            for (i, &id) in ids.iter().enumerate().rev() {
                m.insert(id, i);
            }
            IdIndex::Map(m)
        }
    }
    fn get(&self, id: i32) -> Option<usize> {
        match self {
            IdIndex::Dense(t) => usize::try_from(id)
                .ok()
                .and_then(|k| t.get(k))
                .filter(|&&v| v != u32::MAX)
                .map(|&v| v as usize),
            IdIndex::Map(m) => m.get(&id).copied(),
        }
    }
}

/// `lat$x[cell]` (or `lat$y`) as a new R vector of `xy`'s type (integer or double).
fn coords(xy: SEXP, cell: &[i32]) -> savvy::Result<Option<Vals<'static>>> {
    use std::borrow::Cow;
    let rows: Vec<usize> = cell.iter().map(|&c| c as usize - 1).collect();
    Ok(match expr::vals_of(xy, false)? {
        Some(Vals::Int(v)) => Some(Vals::Int(Cow::Owned(rows.iter().map(|&r| v[r]).collect()))),
        Some(Vals::Dbl(v)) => Some(Vals::Dbl(Cow::Owned(rows.iter().map(|&r| v[r]).collect()))),
        _ => None,
    })
}

/// `abm_move(to = "random_neighbour" / "random_empty_neighbour")` natively
/// ([run_move()], `range = 1`): the movers of every moving group in one random
/// order (`sample.int(n)`), each drawing one of its cell's neighbours (the batch
/// draw `sample.int(k, n, TRUE)` when nothing can stop it and every cell has `k`
/// neighbours, else one `sample.int(length(cand), 1)` per mover, skipping occupied
/// cells when asked); then `.cell` written and `sync_cell_coords()`'s `.x`/`.y`.
fn exec_move(plan: &Plan, st: SEXP) -> savvy::Result<Option<H>> {
    use tidyabm_core::sample::NA_INT;
    let mv = plan.mv.as_ref().ok_or_else(|| bad("no move"))?;
    match find_str(st, b"lattice")? {
        Some(li) if elt(st, li) == mv.lattice => {}
        _ => return decline("the lattice changed"),
    }
    let gi = find_str(st, b"groups")?.ok_or_else(|| bad("no groups"))?;
    let groups = elt(st, gi);
    let gnames = names_of(groups)?;
    // the moving groups' cells
    let mut ks = Vec::with_capacity(mv.who.len());
    let mut cells: Vec<Vec<i32>> = Vec::with_capacity(mv.who.len());
    for &w in &mv.who {
        let Some(k) = find(gnames, w) else {
            return decline("a moving group not found");
        };
        let g = elt(groups, k);
        let Some(j) = find_str(g, b".cell")? else {
            return decline("a moving group without .cell");
        };
        match store::numbers_of::<i32>(elt(g, j), INTSXP)? {
            Some(v) => cells.push(v.to_vec()),
            None => return decline("a .cell that is not integer"),
        }
        ks.push(k);
    }
    let sizes: Vec<usize> = cells.iter().map(Vec::len).collect();
    let n_movers: usize = sizes.iter().sum();
    if n_movers == 0 {
        return Ok(Some(keep(st)?));
    }
    // the adjacency: one integer vector (or NULL) per cell
    let n_nb = len(mv.nb);
    let nb_at = |c: i32| -> savvy::Result<Option<&[i32]>> {
        if c < 1 || c as usize > n_nb {
            return Ok(Some(&[]));
        }
        let v = elt(mv.nb, c as usize - 1);
        if v == rapi_nil() {
            return Ok(Some(&[]));
        }
        store::numbers_of::<i32>(v, INTSXP)
    };
    // `direction =`: each mover's side of the lattice (`heading_names()`), as an
    // index into `dir_nb`, `None` for `NA`
    let mut heading: Vec<Vec<Option<usize>>> = Vec::new();
    let mut dir_vecs: Vec<&[i32]> = Vec::new();
    if mv.dir != rapi_nil() {
        let dn: Vec<&[u8]> = names_of(mv.dir)?.iter().map(|&c| bytes(c)).collect();
        for k in 0..len(mv.dir) {
            match store::numbers_of::<i32>(elt(mv.dir, k), INTSXP)? {
                Some(v) => dir_vecs.push(v),
                None => return decline("a direction table that is not integer"),
            }
        }
        let Some(svals) = scope_values(plan)? else {
            return decline("a formula-environment value that cannot be read");
        };
        let compass: [&[u8]; 4] = [b"north", b"east", b"south", b"west"];
        for (p, &k) in ks.iter().enumerate() {
            let g = elt(groups, k);
            let gp = plan
                .groups
                .iter()
                .find(|e| e.name == mv.who[p])
                .ok_or_else(|| bad("no heading program"))?;
            let ng = sizes[p];
            let Some(ins) = entry_inputs(st, groups, g, gp, &svals, None)? else {
                return Ok(None);
            };
            // SAFETY: the program is kept alive by the plan's descriptor.
            let prog = unsafe { &*gp.prog };
            let v = match prog.compute(ins, ng, ng)? {
                Ok(mut r) => recycle(r.remove(0), ng),
                Err(d) => {
                    expr::note_decline(&d);
                    return Ok(None);
                }
            };
            let mut hs: Vec<Option<usize>> = Vec::with_capacity(ng);
            for i in 0..ng {
                let name: Option<Vec<u8>> = match &v {
                    Vals::Str(x) => {
                        // SAFETY: reading a static of R's.
                        if x[i] == unsafe { rapi::R_NaString } as usize {
                            None
                        } else {
                            let b = bytes(x[i] as SEXP);
                            if !b.is_ascii() {
                                return decline("a non-ASCII heading");
                            }
                            Some(b.to_ascii_lowercase())
                        }
                    }
                    _ => {
                        // as.integer(round(as.numeric(v))) %% 4
                        let d = match &v {
                            Vals::Dbl(x) => x[i],
                            Vals::Int(x) | Vals::Lgl(x) => {
                                if x[i] == NA_INT {
                                    f64::NAN
                                } else {
                                    f64::from(x[i])
                                }
                            }
                            Vals::Str(_) => unreachable!("strings above"),
                        };
                        let r = d.round_ties_even();
                        if r.is_nan() || !(-2147483647.0..=2147483647.0).contains(&r) {
                            None
                        } else {
                            Some(compass[(r as i64).rem_euclid(4) as usize].to_vec())
                        }
                    }
                };
                hs.push(match name {
                    None => None,
                    Some(nm) => match dn.iter().position(|&d| d == &nm[..]) {
                        Some(q) => Some(q),
                        None => {
                            return decline("a heading the lattice has no side for (R's error)")
                        }
                    },
                });
            }
            heading.push(hs);
        }
    }
    // (group, row) of every mover, the groups stacked
    let mut owner: Vec<(usize, usize)> = Vec::with_capacity(n_movers);
    for (gk, &sz) in sizes.iter().enumerate() {
        owner.extend((0..sz).map(|r| (gk, r)));
    }
    let res: savvy::Result<Option<()>> = crate::rng::with_rng(|r| {
        let ord: Vec<usize> = if n_movers > 1 {
            tidyabm_core::sample::sample_int(r, n_movers, n_movers, false)
                .map_err(|e| bad(&format!("move order: {e}")))?
                .iter()
                .map(|&k| k as usize - 1)
                .collect()
        } else {
            vec![0]
        };
        if !heading.is_empty() {
            let mut occupied: Vec<i32> = Vec::new();
            if mv.avoid {
                let mx = cells
                    .iter()
                    .flatten()
                    .filter(|&&c| c != NA_INT)
                    .copied()
                    .max()
                    .unwrap_or(0)
                    .max(0);
                occupied = vec![0; n_nb.max(mx as usize)];
                for &c in cells.iter().flatten() {
                    if c != NA_INT {
                        if c < 1 {
                            return Ok(None);
                        }
                        occupied[c as usize - 1] += 1;
                    }
                }
            }
            for &m in &ord {
                let (gk, rw) = owner[m];
                let here = cells[gk][rw];
                if here == NA_INT {
                    continue;
                }
                let Some(d) = heading[gk][rw] else { continue };
                let tbl = dir_vecs[d];
                if here < 1 {
                    return Ok(None);
                }
                // dir_nb[[d]][here] past its end is NA: the mover stays
                if here as usize > tbl.len() {
                    continue;
                }
                let target = tbl[here as usize - 1];
                if target == NA_INT {
                    continue;
                }
                if mv.avoid {
                    if target < 1
                        || target as usize > occupied.len()
                        || here as usize > occupied.len()
                    {
                        return Ok(None);
                    }
                    if target != here && occupied[target as usize - 1] > 0 {
                        continue;
                    }
                    occupied[here as usize - 1] -= 1;
                    occupied[target as usize - 1] += 1;
                }
                cells[gk][rw] = target;
            }
            return Ok(Some(()));
        }
        if !mv.avoid {
            // the batch draw, when every mover stands on a cell with k > 1 neighbours
            let here: Vec<i32> = ord.iter().map(|&m| cells[owner[m].0][owner[m].1]).collect();
            let ok_here = here
                .iter()
                .all(|&c| c != NA_INT && c >= 1 && c as usize <= n_nb);
            if ok_here {
                let mut cand: Vec<&[i32]> = Vec::with_capacity(n_movers);
                for &c in &here {
                    match nb_at(c)? {
                        Some(v) => cand.push(v),
                        None => return Ok(None),
                    }
                }
                let k = cand[0].len();
                if k > 1 && cand.iter().all(|c| c.len() == k && !c.contains(&NA_INT)) {
                    for (q, &m) in ord.iter().enumerate() {
                        let j = tidyabm_core::sample::RDraws::unif_index(r, k as f64) as usize;
                        cells[owner[m].0][owner[m].1] = cand[q][j];
                    }
                    return Ok(Some(()));
                }
            }
        }
        // one mover at a time
        let mut occupied: Vec<i32> = Vec::new();
        if mv.avoid {
            let mx = cells
                .iter()
                .flatten()
                .filter(|&&c| c != NA_INT)
                .copied()
                .max()
                .unwrap_or(0)
                .max(0);
            occupied = vec![0; n_nb.max(mx as usize)];
            for &c in cells.iter().flatten() {
                if c != NA_INT {
                    if c < 1 {
                        return Ok(None);
                    }
                    occupied[c as usize - 1] += 1;
                }
            }
        }
        let taken =
            |occ: &Vec<i32>, i: i32| i >= 1 && (i as usize) <= occ.len() && occ[i as usize - 1] > 0;
        for &m in &ord {
            let (gk, rw) = owner[m];
            let here = cells[gk][rw];
            if here == NA_INT {
                continue;
            }
            let Some(all) = nb_at(here)? else {
                return Ok(None);
            };
            let mut cand: Vec<i32> = all.iter().copied().filter(|&c| c != NA_INT).collect();
            if mv.avoid {
                cand.retain(|&c| c == here || !taken(&occupied, c));
            }
            if cand.is_empty() {
                continue;
            }
            let target = if cand.len() == 1 {
                cand[0]
            } else {
                cand[tidyabm_core::sample::RDraws::unif_index(r, cand.len() as f64) as usize]
            };
            if mv.avoid && target != here {
                if here < 1
                    || target < 1
                    || here as usize > occupied.len()
                    || target as usize > occupied.len()
                {
                    return Ok(None);
                }
                occupied[here as usize - 1] -= 1;
                occupied[target as usize - 1] += 1;
            }
            cells[gk][rw] = target;
        }
        Ok(Some(()))
    });
    if res?.is_none() {
        return decline("an adjacency of another kind");
    }
    // `.cell` of the movers, then `sync_cell_coords()` over every group on the
    // lattice: whole cells in 1..ncell, `.x`/`.y` from the lattice
    let mut cur: Option<H> = None;
    for k in 0..len(groups) {
        let gs = cur.as_ref().map_or(groups, |h| h.get());
        let g = elt(gs, k);
        let moved = ks.iter().position(|&q| q == k);
        let Some(j) = find_str(g, b".cell")? else {
            continue;
        };
        if rows(g).unwrap_or(0) == 0 {
            if let Some(p) = moved {
                // an empty mover still gets its (empty) integer .cell
                let _ = p;
            }
            continue;
        }
        let cell: Vec<i32> = match moved {
            Some(p) => cells[p].clone(),
            None => match store::numbers_of::<i32>(elt(g, j), INTSXP)? {
                Some(v) => v.to_vec(),
                None => return decline("a .cell that is not integer"),
            },
        };
        if cell.iter().any(|&c| c == NA_INT || c < 1 || c > mv.ncell) {
            return decline("a .cell that is not a cell (R's error)");
        }
        let (Some(x), y) = (
            coords(mv.x, &cell)?,
            if mv.y == rapi_nil() {
                None
            } else {
                coords(mv.y, &cell)?
            },
        ) else {
            return decline("coordinates of another kind");
        };
        if mv.y != rapi_nil() && y.is_none() {
            return decline("coordinates of another kind");
        }
        let n = cell.len();
        let mut hold = OwnedListSexp::new(3, false)?;
        let mut base = g;
        let mut g_h: Option<H> = None;
        let cname = names_of(g)?[j];
        if moved.is_some() {
            hold.set_value(
                0,
                expr::to_r(&Vals::Int(std::borrow::Cow::Owned(cell.clone())), n)?,
            )?;
            g_h = Some(set_named(base, cname, elt(hold.inner(), 0))?);
            base = g_h.as_ref().map_or(g, |h| h.get());
        }
        // SAFETY: as above (the names' CHARSXPs are kept while used).
        let xn = unsafe { build(|| rapi::Rf_mkChar(c".x".as_ptr())) }?;
        hold.set_value(1, expr::to_r(&x, n)?)?;
        let g2 = set_named(base, xn.get(), elt(hold.inner(), 1))?;
        let g3 = match &y {
            Some(yv) => {
                // SAFETY: as above.
                let yn = unsafe { build(|| rapi::Rf_mkChar(c".y".as_ptr())) }?;
                hold.set_value(2, expr::to_r(yv, n)?)?;
                set_named(g2.get(), yn.get(), elt(hold.inner(), 2))?
            }
            None => g2,
        };
        drop(g_h);
        // SAFETY: reading a static of R's (unused name: `k` is in range).
        cur = Some(with_elt(gs, k, g3.get(), unsafe { rapi::R_BlankString })?);
    }
    match cur {
        // SAFETY: as above.
        Some(gs) => Ok(Some(with_elt(st, gi, gs.get(), unsafe {
            rapi::R_BlankString
        })?)),
        None => Ok(Some(keep(st)?)),
    }
}

/// `abm_tell(to = <id>)` natively ([run_tell()]): `when`, `to` and the messages
/// over the groups stacked; each sender's message to its recipient, several to
/// one resolved (`last`, `first`, `sum`, `mean`; `error` is R's), written into
/// the recipients' existing columns in vctrs' common type (`replace_at()`).
fn exec_tell(plan: &Plan, st: SEXP) -> savvy::Result<Option<H>> {
    use std::borrow::Cow;
    use tidyabm_core::sample::NA_INT;
    let (has_when, resolve) = plan.tell.ok_or_else(|| bad("no tell"))?;
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
    let Some(Vals::Int(ids)) = stacked_col_named(groups, b".id", &sizes)? else {
        return decline("ids that are not integers");
    };
    let mut ins: Vec<Vals> = Vec::with_capacity(gp.cols.len() + gp.globs.len());
    for &c in &gp.cols {
        match stacked_pad(groups, c, &sizes)? {
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
    ins.extend(more);
    if !gp.mcols.is_empty() {
        let mnames: Vec<&[u8]> = gp.mcols.iter().map(|&c| bytes(c)).collect();
        match match_inputs_ids(st, groups, &ids, &mnames)? {
            Some(v) => ins.extend(v),
            None => return decline("match columns that cannot be built natively"),
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
    let mut k = 0;
    let speaks: Vec<bool> = if has_when {
        k = 1;
        match condition(&res[0], n) {
            Some(c) => c,
            None => return decline("a when that is not logical"),
        }
    } else {
        vec![true; n]
    };
    if !speaks.iter().any(|&b| b) {
        return Ok(Some(keep(st)?));
    }
    let tgt = recycle(res[k].clone(), n);
    k += 1;
    let Vals::Int(tgt) = tgt else {
        return decline("recipients that are not integer ids");
    };
    let senders: Vec<usize> = (0..n).filter(|&i| speaks[i] && tgt[i] != NA_INT).collect();
    let recipient: Vec<i32> = senders.iter().map(|&i| tgt[i]).collect();
    let at_id = IdIndex::new(&ids);
    let mut recipient_row = Vec::with_capacity(recipient.len());
    for &r in &recipient {
        match at_id.get(r) {
            Some(row) => recipient_row.push(row),
            None => return decline("a recipient that does not exist (R's error)"),
        }
    }
    if senders.is_empty() {
        return Ok(Some(keep(st)?));
    }
    // recipients in order of first mention, and each one's messages
    let rmax = recipient.iter().copied().max().unwrap_or(0).max(0) as usize;
    let mut slot = vec![u32::MAX; rmax + 1];
    let mut first: Vec<usize> = Vec::new();
    let mut members: Vec<Vec<usize>> = Vec::new();
    for (q, &r) in recipient.iter().enumerate() {
        if r < 0 {
            return decline("a negative recipient");
        }
        let s = &mut slot[r as usize];
        if *s == u32::MAX {
            *s = first.len() as u32;
            first.push(q);
            members.push(Vec::new());
        }
        members[*s as usize].push(q);
    }
    let dup = first.len() < recipient.len();
    if dup && resolve == 4 {
        return decline("several messages to one agent (R's error)");
    }
    let at: Vec<usize> = if dup {
        first.iter().map(|&q| recipient_row[q]).collect()
    } else {
        recipient_row.clone()
    };
    // SAFETY: reading a static of R's.
    let na_real = unsafe { rapi::R_NaReal };
    let mut msgs: Vec<Vals> = Vec::with_capacity(gp.targets.len());
    for v in res.into_iter().skip(k) {
        let v = recycle(v, n);
        let sent = tidyabm_core::expr::take(&v, &senders);
        let msg = if !dup {
            sent
        } else {
            match resolve {
                1 => tidyabm_core::expr::take(&sent, &first),
                0 => {
                    let last: Vec<usize> =
                        members.iter().map(|m| *m.last().unwrap_or(&0)).collect();
                    tidyabm_core::expr::take(&sent, &last)
                }
                2 | 3 => {
                    let out: Vec<f64> = match &sent {
                        Vals::Dbl(x) => members
                            .iter()
                            .map(|m| {
                                let part: Vec<f64> = m.iter().map(|&q| x[q]).collect();
                                if resolve == 2 {
                                    reduce::sum(&part, false)
                                } else {
                                    reduce::mean(&part)
                                }
                            })
                            .collect(),
                        Vals::Int(x) | Vals::Lgl(x) => {
                            let mut o = Vec::with_capacity(members.len());
                            for m in &members {
                                let part: Vec<i32> = m.iter().map(|&q| x[q]).collect();
                                o.push(if resolve == 2 {
                                    match tidyabm_core::reduce::sum_int(&part, false) {
                                        Some(NA_INT) => na_real,
                                        Some(s) => f64::from(s),
                                        None => {
                                            return decline(
                                                "an integer sum that overflows (R warns)",
                                            )
                                        }
                                    }
                                } else if part.contains(&NA_INT) {
                                    na_real
                                } else {
                                    reduce::mean_int(&part)
                                });
                            }
                            o
                        }
                        Vals::Str(_) => return decline("sum()/mean() of strings"),
                    };
                    Vals::Dbl(Cow::Owned(out))
                }
                _ => return Err(bad("tell resolve")),
            }
        };
        msgs.push(msg);
    }
    // write: rule by rule, group by group
    let ends: Vec<usize> = sizes
        .iter()
        .scan(0, |a, &x| {
            *a += x;
            Some(*a)
        })
        .collect();
    let mut cur: Option<H> = None;
    for (r, msg) in msgs.iter().enumerate() {
        let t = gp.targets[r];
        for (j, &end) in ends.iter().enumerate() {
            let start = if j == 0 { 0 } else { ends[j - 1] };
            let sel: Vec<usize> = (0..at.len())
                .filter(|&q| at[q] >= start && at[q] < end)
                .collect();
            if sel.is_empty() {
                continue;
            }
            let gs = cur.as_ref().map_or(groups, |h| h.get());
            let g = elt(gs, j);
            let Some(cj) = find(names_of(g)?, t) else {
                return decline("a recipient group without the column (R's error)");
            };
            let col = elt(g, cj);
            if !all_bare(&[col])? {
                return decline("a target column with attributes");
            }
            let Some(old) = expr::vals_of(col, false)? else {
                return decline("a target column of another type");
            };
            let sz = sizes[j];
            let mut mask = vec![false; sz];
            let mut spread_rows = vec![0usize; sz];
            for &q in &sel {
                mask[at[q] - start] = true;
                spread_rows[at[q] - start] = q;
            }
            let spread = tidyabm_core::expr::take(msg, &spread_rows);
            // replace_at(): vctrs' common type, then the rows written
            let new = match expr::assign_rows(Some(old), &spread, &mask)? {
                Ok(v) => v,
                Err(d) => {
                    expr::note_decline(&d);
                    return Ok(None);
                }
            };
            let mut hold = OwnedListSexp::new(1, false)?;
            hold.set_value(0, expr::to_r(&new, sz)?)?;
            let g2 = set_named(g, t, elt(hold.inner(), 0))?;
            // SAFETY: reading a static of R's (unused name: `j` is in range).
            cur = Some(with_elt(gs, j, g2.get(), unsafe { rapi::R_BlankString })?);
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

// ---- abm_sequential (FR8) ----------------------------------------------------

/// A column of a sequential step's working store: loaded once per step, written
/// in place agent by agent, and written back when `dirty`.
struct SeqCol {
    name: SEXP,
    vals: Vals<'static>,
    dirty: bool,
}

/// Where a sequential rule writes ([run_sequential()]'s order of cases): the
/// partner's row (a standing match), a global, else a column of the agent's own
/// group (an index into its store).
#[derive(Clone, Copy)]
enum SeqTarget {
    Partner(usize),
    Global(usize),
    Own(usize),
}

/// A match column a sequential rule reads, per agent.
#[derive(Clone, Copy)]
enum SeqMatchIn {
    Partner,
    Role,
    GroupId,
    /// `partner_<col>`: an index into the partner columns
    Col(usize),
}

/// One group's rules, resolved against the step's stores.
struct SeqGroup<'s> {
    q: usize,
    cols: Vec<usize>,
    globs: Vec<usize>,
    scope: Vec<Vals<'s>>,
    mcols: Vec<SeqMatchIn>,
    targets: Vec<SeqTarget>,
    wslot: Vec<Option<usize>>,
}

/// Column `name` of group `j` in the store (loaded on first use): `Ok(Some)` its
/// index, `Ok(None)` when the group lacks it, `Err(())` (decline) when it is not
/// a bare vector of a type the evaluator takes.
fn seq_col(
    groups: SEXP,
    store: &mut [Vec<SeqCol>],
    j: usize,
    name: &[u8],
) -> savvy::Result<Result<Option<usize>, ()>> {
    if let Some(k) = store[j].iter().position(|c| bytes(c.name) == name) {
        return Ok(Ok(Some(k)));
    }
    let g = elt(groups, j);
    let names = names_of(g)?;
    let Some(ci) = names.iter().position(|&c| same_bytes(c, name)) else {
        return Ok(Ok(None));
    };
    let col = elt(g, ci);
    if !all_bare(&[col])? {
        return Ok(Err(()));
    }
    let Some(v) = expr::vals_of(col, false)? else {
        return Ok(Err(()));
    };
    store[j].push(SeqCol {
        name: names[ci],
        vals: expr::owned(v),
        dirty: false,
    });
    Ok(Ok(Some(store[j].len() - 1)))
}

/// Element `i` of `v`, borrowed.
fn seq_at<'a>(v: &'a Vals<'static>, i: usize) -> Vals<'a> {
    use std::borrow::Cow;
    match v {
        Vals::Lgl(x) => Vals::Lgl(Cow::Borrowed(&x[i..=i])),
        Vals::Int(x) => Vals::Int(Cow::Borrowed(&x[i..=i])),
        Vals::Dbl(x) => Vals::Dbl(Cow::Borrowed(&x[i..=i])),
        Vals::Str(x) => Vals::Str(Cow::Borrowed(&x[i..=i])),
    }
}

/// [write_at()]'s value (one) for a column of `col`'s type: itself when of that
/// type, cast when vctrs' common type of the two is the column's own; `None`
/// (decline) when the write would change the column's type.
fn seq_cast(col: &Vals, v: &Vals) -> Option<Vals<'static>> {
    use std::borrow::Cow;
    use tidyabm_core::sample::NA_INT;
    // SAFETY: reading statics of R's.
    let (na_real, na_str) = unsafe { (rapi::R_NaReal, rapi::R_NaString as usize) };
    Some(match (col, v) {
        (Vals::Dbl(_), Vals::Dbl(x)) => Vals::Dbl(Cow::Owned(vec![x[0]])),
        (Vals::Dbl(_), Vals::Int(x) | Vals::Lgl(x)) => {
            Vals::Dbl(Cow::Owned(vec![if x[0] == NA_INT {
                na_real
            } else {
                f64::from(x[0])
            }]))
        }
        (Vals::Int(_), Vals::Int(x) | Vals::Lgl(x)) => Vals::Int(Cow::Owned(vec![x[0]])),
        (Vals::Lgl(_), Vals::Lgl(x)) => Vals::Lgl(Cow::Owned(vec![x[0]])),
        (Vals::Str(_), Vals::Str(x)) => Vals::Str(Cow::Owned(vec![x[0]])),
        (Vals::Str(_), Vals::Lgl(x)) if x[0] == NA_INT => Vals::Str(Cow::Owned(vec![na_str])),
        _ => return None,
    })
}

/// `col[[i]] <- v[[1]]`, `v` of `col`'s type ([`seq_cast`]).
fn seq_set(col: &mut Vals<'static>, i: usize, v: &Vals) {
    match (col, v) {
        (Vals::Dbl(c), Vals::Dbl(x)) => c.to_mut()[i] = x[0],
        (Vals::Int(c), Vals::Int(x)) | (Vals::Lgl(c), Vals::Lgl(x)) => c.to_mut()[i] = x[0],
        (Vals::Str(c), Vals::Str(x)) => c.to_mut()[i] = x[0],
        _ => {}
    }
}

/// The standing match's columns `.id`, `.partner`, `.role`, `.group_id`:
/// `Ok(None)` without one, `Ok(Some(None))` for one the step cannot read.
#[allow(clippy::type_complexity)]
fn seq_match<'a>(
    st: SEXP,
) -> savvy::Result<Option<Option<(&'a [i32], &'a [i32], &'a [SEXP], &'a [i32])>>> {
    let Some(mi) = find_str(st, b"match")? else {
        return Ok(None);
    };
    let ms = elt(st, mi);
    if ms == rapi_nil() {
        return Ok(None);
    }
    let Some(m) = find_str(ms, b"match")?.map(|j| elt(ms, j)) else {
        return Ok(Some(None));
    };
    let col =
        |nm: &[u8]| -> savvy::Result<Option<SEXP>> { Ok(find_str(m, nm)?.map(|j| elt(m, j))) };
    let (Some(id), Some(pt), Some(role), Some(gid)) = (
        col(b".id")?,
        col(b".partner")?,
        col(b".role")?,
        col(b".group_id")?,
    ) else {
        return Ok(Some(None));
    };
    // SAFETY: reading a type tag.
    if unsafe { rapi::TYPEOF(role) } != STRSXP {
        return Ok(Some(None));
    }
    let (Some(id), Some(pt), Some(gid)) = (
        store::numbers_of::<i32>(id, INTSXP)?,
        store::numbers_of::<i32>(pt, INTSXP)?,
        store::numbers_of::<i32>(gid, INTSXP)?,
    ) else {
        return Ok(Some(None));
    };
    let role = store::chars_of(role)?;
    if pt.len() != id.len() || role.len() != id.len() || gid.len() != id.len() {
        return Ok(Some(None));
    }
    Ok(Some(Some((id, pt, role, gid))))
}

/// `abm_sequential()` natively ([run_sequential()]): the agents in a shuffled
/// order (or by `.order`), each evaluating its group's rules on its own row, a
/// later rule reading what an earlier one wrote, and the next agent reading the
/// columns and globals as the agents before it left them. Every write is checked
/// as [write_at()] makes it; anything else declines and the step is replayed on
/// the R path.
fn exec_seq(plan: &Plan, st: SEXP) -> savvy::Result<Option<H>> {
    use std::borrow::Cow;
    use tidyabm_core::sample::NA_INT;
    let has_order = plan.seq.ok_or_else(|| bad("no seq"))?;
    if let Some(ri) = find_str(st, b"relations")? {
        if len(elt(st, ri)) > 0 {
            return decline("a relation");
        }
    }
    let gi = find_str(st, b"groups")?.ok_or_else(|| bad("no groups"))?;
    let li = find_str(st, b"globals")?.ok_or_else(|| bad("no globals"))?;
    let groups = elt(st, gi);
    let gl = elt(st, li);
    // SAFETY: reading a type tag.
    if unsafe { rapi::TYPEOF(gl) } != VECSXP {
        return decline("globals that are not a list");
    }
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
    let Some(Vals::Int(all_ids)) = stacked_col_named(groups, b".id", &sizes)? else {
        return decline("ids that are not integers");
    };
    let mut group_of = Vec::with_capacity(n);
    let mut starts = Vec::with_capacity(ng);
    for (k, &s) in sizes.iter().enumerate() {
        starts.push(group_of.len());
        group_of.extend(std::iter::repeat(k).take(s));
    }
    let gnames = names_of(groups)?;
    let mut gplan: Vec<Option<usize>> = vec![None; ng];
    let mut key_q = None;
    for (q, gp) in plan.groups.iter().enumerate() {
        if gp.part == 4 {
            key_q = Some(q);
            continue;
        }
        let Some(j) = find(gnames, gp.name) else {
            return decline("a group not found");
        };
        gplan[j] = Some(q);
    }
    // the standing match narrows the step to the agents it placed
    let mt = match seq_match(st)? {
        None => None,
        Some(Some(m)) => Some(m),
        Some(None) => return decline("a match the step cannot read"),
    };
    let uses_match = plan.groups.iter().any(|g| {
        !g.mcols.is_empty() || g.targets.iter().any(|&t| bytes(t).starts_with(b"partner_"))
    });
    let mut order_ids: Vec<i32> = all_ids.to_vec();
    let midx = match mt {
        None if uses_match => return decline("match columns without a standing match"),
        None => None,
        Some((mid, ..)) => {
            if mid.is_empty() {
                return Ok(Some(keep(st)?));
            }
            let ix = IdIndex::new(mid);
            order_ids.retain(|&id| ix.get(id).is_some());
            if order_ids.is_empty() {
                return Ok(Some(keep(st)?));
            }
            Some(ix)
        }
    };
    let Some(svals) = scope_values(plan)? else {
        return decline("a formula-environment value that cannot be read");
    };
    // the stores: columns, globals, partner columns
    let mut store: Vec<Vec<SeqCol>> = (0..ng).map(|_| Vec::new()).collect();
    let gl_names = names_of(gl)?;
    let mut gstore: Vec<(SEXP, Vals)> = Vec::new();
    let mut gwritten: Vec<bool> = Vec::new();
    let mut pcols: Vec<(Vec<u8>, Vec<Option<usize>>)> = Vec::new();
    let mut seqs: Vec<Option<SeqGroup>> = (0..ng).map(|_| None).collect();
    for j in 0..ng {
        let Some(q) = gplan[j] else { continue };
        let gp = &plan.groups[q];
        let mut cols = Vec::with_capacity(gp.cols.len());
        for &c in &gp.cols {
            match seq_col(groups, &mut store, j, bytes(c))? {
                Ok(Some(k)) => cols.push(k),
                _ => return decline("a column the step reads that cannot be read natively"),
            }
        }
        let glob = |name: SEXP, gstore: &mut Vec<(SEXP, Vals)>| -> savvy::Result<Option<usize>> {
            let Some(gj) = find(gl_names, name) else {
                return Ok(None);
            };
            if let Some(x) = gstore.iter().position(|(nm, _)| *nm == gl_names[gj]) {
                return Ok(Some(x));
            }
            let x = elt(gl, gj);
            if !all_bare(&[x])? {
                return Ok(None);
            }
            match expr::vals_of(x, false)? {
                Some(v) => {
                    gstore.push((gl_names[gj], v));
                    Ok(Some(gstore.len() - 1))
                }
                None => Ok(None),
            }
        };
        let mut globs = Vec::with_capacity(gp.globs.len());
        for &g in &gp.globs {
            match glob(g, &mut gstore)? {
                Some(x) => globs.push(x),
                None => return decline("a global the step cannot read natively"),
            }
        }
        let mut xs = Vec::new();
        if !read_scope(&svals, &gp.scope, &mut xs) {
            return decline("a formula-environment value not found");
        }
        let Some(scope) = inputs(&xs)? else {
            return decline("a scope value of an unsupported type");
        };
        let mut pcol = |x: &[u8], store: &mut Vec<Vec<SeqCol>>| -> savvy::Result<Option<usize>> {
            if let Some(p) = pcols.iter().position(|(nm, _)| nm == x) {
                return Ok(Some(p));
            }
            let mut at = Vec::with_capacity(ng);
            for j2 in 0..ng {
                match seq_col(groups, store, j2, x)? {
                    Ok(k) => at.push(k),
                    Err(()) => return Ok(None),
                }
            }
            pcols.push((x.to_vec(), at));
            Ok(Some(pcols.len() - 1))
        };
        let mut mcols = Vec::with_capacity(gp.mcols.len());
        for &c in &gp.mcols {
            mcols.push(match bytes(c) {
                b".partner" => SeqMatchIn::Partner,
                b".role" => SeqMatchIn::Role,
                b".group_id" => SeqMatchIn::GroupId,
                b => match b.strip_prefix(b"partner_") {
                    Some(x) => match pcol(x, &mut store)? {
                        Some(p) => SeqMatchIn::Col(p),
                        None => return decline("a partner column of another kind"),
                    },
                    None => return decline("a match column the step cannot read"),
                },
            });
        }
        let mut targets = Vec::with_capacity(gp.targets.len());
        for &t in &gp.targets {
            let b = bytes(t);
            targets.push(if let Some(x) = b.strip_prefix(b"partner_") {
                match pcol(x, &mut store)? {
                    Some(p) => SeqTarget::Partner(p),
                    None => return decline("a partner column of another kind"),
                }
            } else if find(gl_names, t).is_some() {
                match glob(t, &mut gstore)? {
                    Some(x) => SeqTarget::Global(x),
                    None => return decline("a global the step cannot write natively"),
                }
            } else {
                match seq_col(groups, &mut store, j, b)? {
                    Ok(Some(k)) => SeqTarget::Own(k),
                    _ => return decline("a column the step writes that is new or of another kind"),
                }
            });
        }
        if gp.wslot.len() != targets.len() {
            return Err(bad("seq write slots"));
        }
        let wslot = gp.wslot.iter().map(|&w| usize::try_from(w).ok()).collect();
        seqs[j] = Some(SeqGroup {
            q,
            cols,
            globs,
            scope,
            mcols,
            targets,
            wslot,
        });
    }
    gwritten.resize(gstore.len(), false);
    // the `.order` key's inputs: the groups stacked as bind_groups() stacks them
    let mut key_ins: Vec<Vals> = Vec::new();
    if has_order {
        let gp = &plan.groups[key_q.ok_or_else(|| bad("no order program"))?];
        for &c in &gp.cols {
            match stacked_pad(groups, c, &sizes)? {
                Some(v) => key_ins.push(v),
                None => return decline("an order column of another kind"),
            }
        }
        let mut xs = Vec::new();
        if !read_globals(st, &gp.globs, &mut xs)? {
            return decline("a global not found");
        }
        if !read_scope(&svals, &gp.scope, &mut xs) {
            return decline("a formula-environment value not found");
        }
        let Some(more) = inputs(&xs)? else {
            return decline("a global or scope value of an unsupported type");
        };
        key_ins.extend(more);
    }
    let at_id = IdIndex::new(&all_ids);
    // SAFETY: reading a static of R's.
    let na_str = unsafe { rapi::R_NaString } as usize;
    let mut body = |rng: Option<&mut crate::rng::RRng>| -> savvy::Result<Result<(), Decline>> {
        let mut ev = expr::Evaluator::new(rng);
        if has_order {
            let gp = &plan.groups[key_q.ok_or_else(|| bad("no order program"))?];
            // SAFETY: the program is kept alive by the plan's descriptor.
            let prog = unsafe { &*gp.prog };
            let key = match ev.eval(prog, 0, &key_ins, n)? {
                Ok(v) => recycle(v, n),
                Err(d) => return Ok(Err(d)),
            };
            let wanted = IdIndex::new(&order_ids);
            let keep_at: Vec<usize> = (0..n)
                .filter(|&i| wanted.get(all_ids[i]).is_some())
                .collect();
            // `combined$.id[keep][order(key[keep])]`: a stable sort, `NA` dropped
            let mut kept: Vec<usize> = match &key {
                Vals::Dbl(x) => keep_at.into_iter().filter(|&i| !x[i].is_nan()).collect(),
                Vals::Int(x) | Vals::Lgl(x) => {
                    keep_at.into_iter().filter(|&i| x[i] != NA_INT).collect()
                }
                Vals::Str(_) => return Ok(Err(Decline("an order of strings (collation)"))),
            };
            match &key {
                Vals::Dbl(x) => kept
                    .sort_by(|&a, &b| x[a].partial_cmp(&x[b]).unwrap_or(std::cmp::Ordering::Equal)),
                Vals::Int(x) | Vals::Lgl(x) => kept.sort_by_key(|&i| x[i]),
                Vals::Str(_) => {}
            }
            order_ids = kept.iter().map(|&i| all_ids[i]).collect();
        } else if let Some(r) = ev.rng() {
            shuffle(r, &mut order_ids)?;
        } else if order_ids.len() > 1 {
            return Err(bad("a shuffle outside the RNG scope"));
        }
        let mut pending: Vec<(usize, usize, usize, Vals<'static>)> = Vec::new();
        let mut gpending: Vec<(usize, Vals<'static>)> = Vec::new();
        for &id in &order_ids {
            let pos = at_id.get(id).ok_or_else(|| bad("an agent not found"))?;
            let g = group_of[pos];
            let i = pos - starts[g];
            let Some(sg) = &seqs[g] else { continue };
            let (mut pid, mut role, mut gid) = (NA_INT, na_str, NA_INT);
            if let (Some((_, mpt, mrole, mgid)), Some(ix)) = (mt, &midx) {
                let r = ix.get(id).ok_or_else(|| bad("an agent not in the match"))?;
                pid = mpt[r];
                role = mrole[r] as usize;
                gid = mgid[r];
            }
            let ploc = if pid == NA_INT {
                None
            } else {
                match at_id.get(pid) {
                    Some(pp) => Some((group_of[pp], pp - starts[group_of[pp]])),
                    None => return Ok(Err(Decline("a partner not in the population (R's error)"))),
                }
            };
            {
                let mut ins: Vec<Vals> = Vec::with_capacity(
                    sg.cols.len() + sg.globs.len() + sg.scope.len() + sg.mcols.len(),
                );
                for &ci in &sg.cols {
                    ins.push(seq_at(&store[g][ci].vals, i));
                }
                for &x in &sg.globs {
                    ins.push(gstore[x].1.clone());
                }
                for s in &sg.scope {
                    ins.push(s.clone());
                }
                for m in &sg.mcols {
                    ins.push(match *m {
                        SeqMatchIn::Partner => Vals::Int(Cow::Owned(vec![pid])),
                        SeqMatchIn::Role => Vals::Str(Cow::Owned(vec![role])),
                        SeqMatchIn::GroupId => Vals::Int(Cow::Owned(vec![gid])),
                        SeqMatchIn::Col(p) => match ploc.and_then(|(pg, pj)| {
                            pcols[p].1[pg].map(|ci| seq_at(&store[pg][ci].vals, pj))
                        }) {
                            Some(v) => v,
                            // no partner, or one whose group lacks the column
                            None => Vals::Lgl(Cow::Owned(vec![NA_INT])),
                        },
                    });
                }
                // SAFETY: the program is kept alive by the plan's descriptor.
                let prog = unsafe { &*plan.groups[sg.q].prog };
                for k in 0..sg.targets.len() {
                    let v = match ev.eval(prog, k, &ins, 1)? {
                        Ok(v) => v,
                        Err(d) => return Ok(Err(d)),
                    };
                    if v.len() != 1 {
                        return Ok(Err(Decline("a sequential value that is not one value")));
                    }
                    let raw = expr::owned(v);
                    match sg.targets[k] {
                        SeqTarget::Own(ci) => {
                            let Some(c) = seq_cast(&store[g][ci].vals, &raw) else {
                                return Ok(Err(Decline("a write that changes a column's type")));
                            };
                            pending.push((g, ci, i, c));
                        }
                        SeqTarget::Global(x) => gpending.push((x, raw.clone())),
                        SeqTarget::Partner(p) => {
                            let Some((pg, pj)) = ploc else { continue };
                            if let Some(ci) = pcols[p].1[pg] {
                                let Some(c) = seq_cast(&store[pg][ci].vals, &raw) else {
                                    return Ok(Err(Decline(
                                        "a write that changes a column's type",
                                    )));
                                };
                                pending.push((pg, ci, pj, c));
                            }
                        }
                    }
                    if let Some(w) = sg.wslot[k] {
                        if w >= ins.len() {
                            return Err(bad("seq write slot"));
                        }
                        ins[w] = raw;
                    }
                }
            }
            for (g2, ci, row, c) in pending.drain(..) {
                let col = &mut store[g2][ci];
                seq_set(&mut col.vals, row, &c);
                col.dirty = true;
            }
            for (x, v) in gpending.drain(..) {
                gstore[x].1 = v;
                gwritten[x] = true;
            }
        }
        Ok(Ok(()))
    };
    let res = if plan.draws {
        crate::rng::with_rng(|r| body(Some(r)))?
    } else {
        body(None)?
    };
    if let Err(d) = res {
        expr::note_decline(&d);
        return Ok(None);
    }
    // write back: the columns written, then the globals
    // SAFETY: reading a static of R's.
    let blank = unsafe { rapi::R_BlankString };
    let mut st_cur: Option<H> = None;
    if store.iter().flatten().any(|c| c.dirty) {
        let mut cur: Option<H> = None;
        for (j, cols) in store.iter().enumerate() {
            let mut g2: Option<H> = None;
            for c in cols.iter().filter(|c| c.dirty) {
                let gs = cur.as_ref().map_or(groups, |h| h.get());
                let base = g2.as_ref().map_or_else(|| elt(gs, j), |h| h.get());
                let mut hold = OwnedListSexp::new(1, false)?;
                hold.set_value(0, expr::to_r(&c.vals, sizes[j])?)?;
                g2 = Some(set_named(base, c.name, elt(hold.inner(), 0))?);
            }
            if let Some(g2) = g2 {
                let gs = cur.as_ref().map_or(groups, |h| h.get());
                cur = Some(with_elt(gs, j, g2.get(), blank)?);
            }
        }
        if let Some(gs) = cur {
            st_cur = Some(with_elt(st, gi, gs.get(), blank)?);
        }
    }
    if gwritten.iter().any(|&w| w) {
        let mut gcur: Option<H> = None;
        for (x, (name, v)) in gstore.iter().enumerate() {
            if !gwritten[x] {
                continue;
            }
            let base = gcur.as_ref().map_or(gl, |h| h.get());
            let mut hold = OwnedListSexp::new(1, false)?;
            hold.set_value(0, expr::to_r(v, 1)?)?;
            gcur = Some(set_named(base, *name, elt(hold.inner(), 0))?);
        }
        if let Some(g) = gcur {
            let base = st_cur.as_ref().map_or(st, |h| h.get());
            st_cur = Some(with_elt(base, li, g.get(), blank)?);
        }
    }
    match st_cur {
        Some(h) => Ok(Some(h)),
        None => Ok(Some(keep(st)?)),
    }
}

/// Does `x` carry no attribute but (possibly) names? (`all_bare()` on a copy of
/// its attributes without the names.)
fn names_only(x: SEXP) -> savvy::Result<bool> {
    // SAFETY: main thread; `x` is protected by the caller; the new vectors are
    // protected while compared.
    let bare = unsafe {
        rapi::protect_value(0, move || {
            let t = rapi::TYPEOF(x);
            let a = rapi::Rf_protect(rapi::Rf_allocVector(t, 0));
            rapi::SHALLOW_DUPLICATE_ATTRIB(a, x);
            rapi::Rf_setAttrib(a, rapi::R_NamesSymbol, rapi::R_NilValue);
            let b = rapi::Rf_protect(rapi::Rf_allocVector(t, 0));
            let same = rapi::R_compute_identical(a, b, 16);
            rapi::Rf_unprotect(2);
            same
        })
    }?;
    Ok(bare != 0)
}

/// The value at `i` of `v`, as a vector of one.
fn one_at(v: &Vals, i: usize) -> Vals<'static> {
    tidyabm_core::expr::take(v, &[i])
}

/// `abm_global(.by = <name>)` natively ([run_global_by()]): rule by rule, key
/// by key, the rule's program over the groups stacked with the target bound to
/// the key's current value (`NA` for a key the global does not name) and
/// `.key` to the key; the values gathered into a named vector as `out[[key]] <-
/// val` gathers them (types promoted as `[[<-` promotes them). Keys must be
/// integers or strings (`as.character()` of a double is R's formatting).
fn exec_global_by(plan: &Plan, st: SEXP) -> savvy::Result<Option<H>> {
    use std::borrow::Cow;
    use tidyabm_core::sample::NA_INT;
    let (by, by_src) = plan.gby.ok_or_else(|| bad("no gby"))?;
    if let Some(ri) = find_str(st, b"relations")? {
        if len(elt(st, ri)) > 0 {
            return decline("a relation");
        }
    }
    let gi = find_str(st, b"groups")?.ok_or_else(|| bad("no groups"))?;
    let li = find_str(st, b"globals")?.ok_or_else(|| bad("no globals"))?;
    let groups = elt(st, gi);
    let gl0 = elt(st, li);
    // SAFETY: reading a type tag.
    if unsafe { rapi::TYPEOF(gl0) } != VECSXP {
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
    let Some(svals) = scope_values(plan)? else {
        return decline("a formula-environment value that cannot be read");
    };
    // the keys
    let keys_sexp = if by_src == 2 {
        let Some(j) = find(names_of(gl0)?, by) else {
            return decline("the keys' global not found");
        };
        elt(gl0, j)
    } else {
        let holder = plan
            .groups
            .iter()
            .find(|g| g.part == 4)
            .ok_or_else(|| bad("gby scope"))?;
        let mut xs = Vec::new();
        if !read_scope(&svals, &holder.scope, &mut xs) || xs.len() != 1 {
            return decline("the keys' value not found");
        }
        xs[0]
    };
    if !all_bare(&[keys_sexp])? {
        return decline("keys with attributes");
    }
    let keys = match expr::vals_of(keys_sexp, false)? {
        Some(v @ Vals::Int(_)) | Some(v @ Vals::Str(_)) => expr::owned(v),
        _ => return decline("keys that are not integers or strings"),
    };
    // SAFETY: reading a static of R's.
    let na_str = unsafe { rapi::R_NaString } as usize;
    let n_keys = keys.len();
    if n_keys == 0 {
        return decline("no keys (R's error)");
    }
    // each key as a CHARSXP (`as.character(k)`), held in a list
    let mut key_chr = OwnedListSexp::new(1, false)?;
    {
        // `as.character()` of each integer key, made before entering R
        let digits: Vec<std::ffi::CString> = match &keys {
            Vals::Int(x) => x
                .iter()
                .map(|v| std::ffi::CString::new(v.to_string()).unwrap_or_default())
                .collect(),
            _ => Vec::new(),
        };
        let strs: Vec<usize> = match &keys {
            Vals::Str(x) => x.to_vec(),
            _ => Vec::new(),
        };
        let ints: Vec<i32> = match &keys {
            Vals::Int(x) => x.to_vec(),
            _ => Vec::new(),
        };
        let (dp, sp, ip) = (digits.as_ptr(), strs.as_ptr(), ints.as_ptr());
        let is_str = !strs.is_empty();
        // SAFETY: main thread; the new string vector is protected while filled
        // and stored at once into `key_chr`; `dp`/`sp`/`ip` point at `n_keys`
        // elements of vectors alive across the call; mkChar of ASCII digits.
        let v = unsafe {
            rapi::protect_value(rapi::R_NilValue, move || {
                let out = rapi::Rf_protect(rapi::Rf_allocVector(STRSXP, n_keys as isize));
                for i in 0..n_keys {
                    let c = if is_str {
                        *sp.add(i) as SEXP
                    } else if *ip.add(i) != NA_INT {
                        rapi::Rf_mkChar((*dp.add(i)).as_ptr())
                    } else {
                        rapi::R_NaString
                    };
                    rapi::SET_STRING_ELT(out, i as isize, c);
                }
                rapi::Rf_unprotect(1);
                out
            })
        }?;
        key_chr.set_value(0, Sexp(v))?;
    }
    let kc: Vec<SEXP> = store::chars_of(elt(key_chr.inner(), 0))?.to_vec();
    if kc.iter().any(|&c| c as usize == na_str) {
        return decline("an NA key");
    }
    let mut gl: Option<H> = None;
    for gp in plan.groups.iter().filter(|g| g.part != 4) {
        let glv = gl.as_ref().map_or(gl0, |h| h.get());
        let target = *gp.targets.first().ok_or_else(|| bad("gby target"))?;
        let gnames = names_of(glv)?;
        let current = find(gnames, target).map(|j| elt(glv, j));
        // the current value: its values and names
        let mut cur_vals: Option<Vals<'static>> = None;
        let mut cur_names: Option<Vec<SEXP>> = None;
        if let Some(c) = current {
            if c != rapi_nil() {
                if !names_only(c)? {
                    return decline("a target with attributes");
                }
                let Some(v) = expr::vals_of(c, false)? else {
                    return decline("a target of another type");
                };
                // SAFETY: reading an attribute of a protected vector.
                let nm = unsafe { rapi::Rf_getAttrib(c, rapi::R_NamesSymbol) };
                // SAFETY: reading a type tag.
                if unsafe { rapi::TYPEOF(nm) } == STRSXP {
                    cur_names = Some(store::chars_of(nm)?.to_vec());
                } else if v.len() > 1 {
                    return decline("a target of several unnamed values (R's error)");
                }
                cur_vals = Some(expr::owned(v));
            }
        }
        // the inputs that do not depend on the key
        let mut base: Vec<Vals> = Vec::new();
        for &c in &gp.cols {
            match stacked_col(groups, c)? {
                Some(v) => base.push(v),
                None => return decline("a column of another kind"),
            }
        }
        let mut xs = Vec::new();
        if !read_globals_in(glv, &gp.globs, &mut xs)? {
            return decline("a global not found");
        }
        if !read_scope(&svals, &gp.scope, &mut xs) {
            return decline("a formula-environment value not found");
        }
        let Some(more) = inputs(&xs)? else {
            return decline("a global or scope value of an unsupported type");
        };
        base.extend(more);
        // SAFETY: the program is kept alive by the plan's descriptor.
        let prog = unsafe { &*gp.prog };
        let same = |a: SEXP, b: SEXP| a == b || bytes(a) == bytes(b);
        let mut out: Option<(Vals<'static>, Vec<SEXP>)> = match (&cur_vals, &cur_names) {
            (Some(v), Some(nm)) => Some((v.clone(), nm.clone())),
            _ => None,
        };
        for (q, &kq) in kc.iter().enumerate() {
            let here: Vals = match (&cur_vals, &cur_names) {
                (Some(v), Some(nm)) => match nm.iter().position(|&c| same(c, kq)) {
                    Some(p) => one_at(v, p),
                    None => Vals::Lgl(Cow::Owned(vec![NA_INT])),
                },
                (Some(v), None) if v.len() == 1 => v.clone(),
                _ => Vals::Lgl(Cow::Owned(vec![NA_INT])),
            };
            let mut ins = base.clone();
            for &m in &gp.mcols {
                ins.push(if bytes(m) == b".key" {
                    one_at(&keys, q)
                } else {
                    here.clone()
                });
            }
            let val = match prog.compute(ins, n, 1)? {
                Ok(mut r) if r.len() == 1 && r[0].len() == 1 => expr::owned(r.remove(0)),
                Ok(_) => return decline("a value of other than one element (R's error)"),
                Err(d) => {
                    expr::note_decline(&d);
                    return Ok(None);
                }
            };
            out = Some(match out {
                None => (val, vec![kq]),
                Some((v, mut nm)) => {
                    let Some(v) = gby_assign(v, &val, nm.iter().position(|&c| same(c, kq))) else {
                        return decline("values of types `[[<-` cannot join natively");
                    };
                    if v.len() > nm.len() {
                        nm.push(kq);
                    }
                    (v, nm)
                }
            });
        }
        let (vals, nm) = out.ok_or_else(|| bad("gby no keys"))?;
        let k = vals.len();
        let mut hold = OwnedListSexp::new(2, false)?;
        hold.set_value(0, expr::to_r(&vals, k)?)?;
        {
            let (np, nl) = (nm.as_ptr(), nm.len());
            // SAFETY: main thread; the new string vector is protected while
            // filled and stored at once in `hold`; `np` points at `nl` live
            // CHARSXPs (the keys' list or the global's names).
            let names = unsafe {
                rapi::protect_value(rapi::R_NilValue, move || {
                    let out = rapi::Rf_protect(rapi::Rf_allocVector(STRSXP, nl as isize));
                    for i in 0..nl {
                        rapi::SET_STRING_ELT(out, i as isize, *np.add(i));
                    }
                    rapi::Rf_unprotect(1);
                    out
                })
            }?;
            hold.set_value(1, Sexp(names))?;
        }
        let (v, names) = (elt(hold.inner(), 0), elt(hold.inner(), 1));
        // SAFETY: main thread; both are protected by `hold`; setting the names
        // attribute of a fresh vector of the same length.
        unsafe {
            rapi::protect(move || {
                rapi::Rf_setAttrib(v, rapi::R_NamesSymbol, names);
            })
        }?;
        gl = Some(set_named(glv, target, v)?);
    }
    match gl {
        // SAFETY: reading a static of R's.
        Some(g) => Ok(Some(with_elt(st, li, g.get(), unsafe {
            rapi::R_BlankString
        })?)),
        None => Ok(Some(keep(st)?)),
    }
}

/// `out[[i]] <- val` (`i` `None`: appended) on a vector of one type: both
/// promoted to the higher of logical < integer < double; strings only with
/// strings. `None` where that is not how `[[<-` would combine them natively.
fn gby_assign(out: Vals<'static>, val: &Vals, i: Option<usize>) -> Option<Vals<'static>> {
    use std::borrow::Cow;
    use tidyabm_core::sample::NA_INT;
    // SAFETY: reading a static of R's.
    let na = unsafe { rapi::R_NaReal };
    let rank = |v: &Vals| match v {
        Vals::Lgl(_) => 0,
        Vals::Int(_) => 1,
        Vals::Dbl(_) => 2,
        Vals::Str(_) => 3,
    };
    let (ro, rv) = (rank(&out), rank(val));
    if (ro == 3) != (rv == 3) {
        return None;
    }
    let to_dbl = |v: &[i32]| -> Vec<f64> {
        v.iter()
            .map(|&x| if x == NA_INT { na } else { f64::from(x) })
            .collect()
    };
    let r = ro.max(rv);
    let mut o: Vals<'static> = match (&out, r) {
        (Vals::Lgl(x), 1) => Vals::Int(Cow::Owned(x.to_vec())),
        (Vals::Lgl(x) | Vals::Int(x), 2) => Vals::Dbl(Cow::Owned(to_dbl(x))),
        _ => out,
    };
    let v: Vals = match (val, r) {
        (Vals::Lgl(x), 1) => Vals::Int(Cow::Owned(x.to_vec())),
        (Vals::Lgl(x) | Vals::Int(x), 2) => Vals::Dbl(Cow::Owned(to_dbl(x))),
        _ => val.clone(),
    };
    match (&mut o, &v) {
        (Vals::Lgl(a), Vals::Lgl(b)) | (Vals::Int(a), Vals::Int(b)) => match i {
            Some(i) => a.to_mut()[i] = b[0],
            None => a.to_mut().push(b[0]),
        },
        (Vals::Dbl(a), Vals::Dbl(b)) => match i {
            Some(i) => a.to_mut()[i] = b[0],
            None => a.to_mut().push(b[0]),
        },
        (Vals::Str(a), Vals::Str(b)) => match i {
            Some(i) => a.to_mut()[i] = b[0],
            None => a.to_mut().push(b[0]),
        },
        _ => return None,
    }
    Some(o)
}

/// [`read_globals`] from a globals list itself.
fn read_globals_in(gl: SEXP, globs: &[SEXP], ins: &mut Vec<SEXP>) -> savvy::Result<bool> {
    if globs.is_empty() {
        return Ok(true);
    }
    let names = names_of(gl)?;
    for &g in globs {
        let Some(j) = find(names, g) else {
            return Ok(false);
        };
        ins.push(elt(gl, j));
    }
    Ok(true)
}

/// `attach_newborns()` natively (no `random_edge`): per newborn, its parent and
/// up to `links - 1` of the parent's neighbours (mode 1), or `links` distinct
/// agents drawn from those there before the births (mode 2); the edges the
/// state then holds (`vec_rbind(edges, edge_tbl(from, to))`), `Ok(Some(None))`
/// when none was added, `Ok(None)` (decline) for edges the step cannot extend.
#[allow(clippy::type_complexity)]
fn attach_draws(
    st: SEXP,
    mode: u8,
    links: usize,
    born: &[(i32, i32)],
    existing: &[i32],
) -> savvy::Result<Option<Option<H>>> {
    use tidyabm_core::sample::NA_INT;
    if born.is_empty() {
        return Ok(Some(None));
    }
    let Some(ei) = find_str(st, b"edges")? else {
        return Ok(None);
    };
    let edges = elt(st, ei);
    // SAFETY: reading a type tag.
    if edges == rapi_nil() || unsafe { rapi::TYPEOF(edges) } != VECSXP || len(edges) != 2 {
        expr::note_decline(&Decline("edges the step cannot extend"));
        return Ok(None);
    }
    let en = names_of(edges)?;
    if bytes(en[0]) != b"from"
        || bytes(en[1]) != b"to"
        || !all_bare(&[elt(edges, 0), elt(edges, 1)])?
    {
        expr::note_decline(&Decline("edges the step cannot extend"));
        return Ok(None);
    }
    let (Some(ef), Some(et)) = (
        store::numbers_of::<i32>(elt(edges, 0), INTSXP)?,
        store::numbers_of::<i32>(elt(edges, 1), INTSXP)?,
    ) else {
        expr::note_decline(&Decline("edges that are not integers"));
        return Ok(None);
    };
    let newborn: std::collections::HashSet<i32> = born.iter().map(|b| b.0).collect();
    let mut nf: Vec<i32> = Vec::new();
    let mut nt: Vec<i32> = Vec::new();
    let res = crate::rng::with_rng(|r| -> savvy::Result<()> {
        let draw = |r: &mut crate::rng::RRng, n: usize, k: usize| -> savvy::Result<Vec<i32>> {
            tidyabm_core::sample::sample_int(r, n, k, false)
                .map_err(|e| bad(&format!("attach: {e}")))
        };
        for &(id, parent) in born {
            let mut targets: Vec<i32> = Vec::new();
            if mode == 1 {
                targets.push(parent);
                let k = links.saturating_sub(1);
                if k > 0 && parent != NA_INT {
                    // c(to[from == parent], from[to == parent]), unique, without
                    // the parent, the newborn and its cohort
                    let mut nb: Vec<i32> = Vec::new();
                    for (q, &f) in ef.iter().enumerate() {
                        if f == parent {
                            nb.push(et[q]);
                        }
                    }
                    for (q, &t) in et.iter().enumerate() {
                        if t == parent {
                            nb.push(ef[q]);
                        }
                    }
                    let mut seen = std::collections::HashSet::new();
                    nb.retain(|&v| {
                        seen.insert(v) && v != parent && v != id && !newborn.contains(&v)
                    });
                    if !nb.is_empty() {
                        let m = k.min(nb.len());
                        for i in draw(r, nb.len(), m)? {
                            targets.push(nb[i as usize - 1]);
                        }
                    }
                }
            } else {
                // attach_targets(): one draw per target from those not chosen yet
                let mut attempts = 0;
                while targets.len() < links && attempts < 10 * links {
                    attempts += 1;
                    let pool: Vec<i32> = existing
                        .iter()
                        .copied()
                        .filter(|v| !targets.contains(v))
                        .collect();
                    if pool.is_empty() {
                        continue;
                    }
                    let hit = pool[draw(r, pool.len(), 1)?[0] as usize - 1];
                    if hit == NA_INT || targets.contains(&hit) {
                        continue;
                    }
                    targets.push(hit);
                }
            }
            // unique(targets[!is.na(targets)])
            let mut seen = std::collections::HashSet::new();
            targets.retain(|&v| v != NA_INT && seen.insert(v));
            for t in targets {
                nf.push(id);
                nt.push(t);
            }
        }
        Ok(())
    });
    res?;
    if nf.is_empty() {
        return Ok(Some(None));
    }
    let from: Vec<i32> = [ef, &nf[..]].concat();
    let to: Vec<i32> = [et, &nt[..]].concat();
    let n = from.len();
    let mut hold = OwnedListSexp::new(2, false)?;
    hold.set_value(0, expr::to_r(&Vals::Int(std::borrow::Cow::Owned(from)), n)?)?;
    hold.set_value(1, expr::to_r(&Vals::Int(std::borrow::Cow::Owned(to)), n)?)?;
    let hp = hold.inner();
    // SAFETY: main thread; `edges` (protected by the state) donates its
    // attributes, then the row names are set to the compact `c(NA, -n)`; the new
    // list and row names are protected while built; the columns are kept by
    // `hold`.
    let out = unsafe {
        build(move || {
            let y = rapi::Rf_protect(rapi::Rf_allocVector(VECSXP, 2));
            rapi::SET_VECTOR_ELT(y, 0, rapi::VECTOR_ELT(hp, 0));
            rapi::SET_VECTOR_ELT(y, 1, rapi::VECTOR_ELT(hp, 1));
            rapi::SHALLOW_DUPLICATE_ATTRIB(y, edges);
            let rn = rapi::Rf_protect(rapi::Rf_allocVector(INTSXP, 2));
            let rp = rapi::DATAPTR_OR_NULL(rn) as *mut i32;
            *rp = NA_INT;
            *rp.add(1) = -(n as i32);
            rapi::Rf_setAttrib(y, rapi::Rf_install(c"row.names".as_ptr()), rn);
            rapi::Rf_unprotect(2);
            y
        })
    }?;
    Ok(Some(Some(out)))
}

/// `x[keep, , drop = FALSE]` of a tibble (class `tbl_df`, `tbl`, `data.frame`)
/// of bare atomic columns: the rows kept, its attributes, compact row names;
/// `None` for anything else.
fn slice_tibble(x: SEXP, keep: &[bool]) -> savvy::Result<Option<H>> {
    use tidyabm_core::sample::NA_INT;
    // SAFETY: main thread; installing the symbol may allocate (protected call);
    // the attribute is kept by `x`, which the caller protects.
    let cl = unsafe {
        rapi::protect_value(rapi::R_NilValue, move || {
            rapi::Rf_getAttrib(x, rapi::Rf_install(c"class".as_ptr()))
        })
    }?;
    // SAFETY: reading a type tag.
    if unsafe { rapi::TYPEOF(cl) } != STRSXP {
        return Ok(None);
    }
    let cls: Vec<&[u8]> = store::chars_of(cl)?.iter().map(|&c| bytes(c)).collect();
    if cls != [&b"tbl_df"[..], b"tbl", b"data.frame"] {
        return Ok(None);
    }
    let ncol = len(x);
    let cols: Vec<SEXP> = (0..ncol).map(|j| elt(x, j)).collect();
    if !all_bare(&cols)? {
        return Ok(None);
    }
    let rows: Vec<usize> = (0..keep.len()).filter(|&i| keep[i]).collect();
    let n = rows.len();
    let mut hold = OwnedListSexp::new(ncol, false)?;
    for (j, &c) in cols.iter().enumerate() {
        let Some(v) = expr::vals_of(c, false)? else {
            return Ok(None);
        };
        if v.len() != keep.len() {
            return Ok(None);
        }
        hold.set_value(j, expr::to_r(&tidyabm_core::expr::take(&v, &rows), n)?)?;
    }
    let hp = hold.inner();
    // SAFETY: main thread; `x` (protected by the caller) donates its attributes,
    // the row names are then set to `c(NA, -n)`; the new list and row names are
    // protected while built; the columns are kept by `hold`.
    let out = unsafe {
        build(move || {
            let y = rapi::Rf_protect(rapi::Rf_allocVector(VECSXP, ncol as isize));
            for j in 0..ncol {
                rapi::SET_VECTOR_ELT(y, j as isize, rapi::VECTOR_ELT(hp, j as isize));
            }
            rapi::SHALLOW_DUPLICATE_ATTRIB(y, x);
            let rn = rapi::Rf_protect(rapi::Rf_allocVector(INTSXP, 2));
            let rp = rapi::DATAPTR_OR_NULL(rn) as *mut i32;
            *rp = NA_INT;
            *rp.add(1) = -(n as i32);
            rapi::Rf_setAttrib(y, rapi::Rf_install(c"row.names".as_ptr()), rn);
            rapi::Rf_unprotect(2);
            y
        })
    }?;
    Ok(Some(out))
}

/// A tibble (`tbl_df`, `tbl`, `data.frame`, compact row names) of the columns
/// held in list `o`, named `cn`, of `all_n` rows (`append_rows()`'s result).
fn tbl_frame(o: SEXP, cn: &[SEXP], all_n: usize) -> savvy::Result<H> {
    use tidyabm_core::sample::NA_INT;
    let cnp = cn.as_ptr();
    let ncol = cn.len();
    // SAFETY: main thread; `o` is protected by the caller, `cnp` points at the
    // group's names (CHARSXPs kept by the group); the frame, its names, row
    // names and class are protected while built; the frame is kept before
    // anything else allocates.
    unsafe {
        build(move || {
            let y = rapi::Rf_protect(rapi::Rf_allocVector(VECSXP, ncol as isize));
            for j in 0..ncol {
                rapi::SET_VECTOR_ELT(y, j as isize, rapi::VECTOR_ELT(o, j as isize));
            }
            let nm = rapi::Rf_protect(rapi::Rf_allocVector(STRSXP, ncol as isize));
            for j in 0..ncol {
                rapi::SET_STRING_ELT(nm, j as isize, *cnp.add(j));
            }
            rapi::Rf_setAttrib(y, rapi::R_NamesSymbol, nm);
            let rn = rapi::Rf_protect(rapi::Rf_allocVector(INTSXP, 2));
            let rp = rapi::DATAPTR_OR_NULL(rn) as *mut i32;
            *rp = NA_INT;
            *rp.add(1) = -(all_n as i32);
            rapi::Rf_setAttrib(y, rapi::Rf_install(c"row.names".as_ptr()), rn);
            let cl = rapi::Rf_protect(rapi::Rf_allocVector(STRSXP, 3));
            for (q, s) in [c"tbl_df", c"tbl", c"data.frame"].iter().enumerate() {
                rapi::SET_STRING_ELT(cl, q as isize, rapi::Rf_mkChar(s.as_ptr()));
            }
            rapi::Rf_setAttrib(y, rapi::Rf_install(c"class".as_ptr()), cl);
            rapi::Rf_unprotect(4);
            y
        })
    }
}

/// `abm_birth(n =)` natively ([birth_by_count()]): `n` rows of the first group
/// drawn with replacement as templates, new ids, each `cost`/`inherit` rule
/// over the template rows (no rule reads what an earlier one wrote, an id or a
/// group: checked by `rl_birth_ok()`), appended; then `attach_via`.
fn exec_birth_count(plan: &Plan, st: SEXP, count: usize) -> savvy::Result<Option<H>> {
    use std::borrow::Cow;
    use tidyabm_core::sample::NA_INT;
    if count == 0 {
        return Ok(Some(keep(st)?));
    }
    if let Some(ri) = find_str(st, b"relations")? {
        if len(elt(st, ri)) > 0 {
            return decline("a relation");
        }
    }
    let Some(svals) = scope_values(plan)? else {
        return decline("a formula-environment value that cannot be read");
    };
    let gi = find_str(st, b"groups")?.ok_or_else(|| bad("no groups"))?;
    let groups = elt(st, gi);
    if len(groups) == 0 {
        return decline("no groups");
    }
    let g = elt(groups, 0);
    let Some(n) = rows(g) else {
        return decline("a group without columns");
    };
    if n == 0 {
        return decline("an empty group (R's error)");
    }
    let Some(ni) = find_str(st, b"next_id")? else {
        return decline("no next_id");
    };
    let next_id = match store::numbers_of::<i32>(elt(st, ni), INTSXP)? {
        Some(v) if v.len() == 1 && v[0] != NA_INT => v[0],
        _ => return decline("a next_id that is not one integer"),
    };
    let Some(last) = next_id.checked_add(count as i32) else {
        return decline("ids past the integer range");
    };
    let existing: Vec<i32> = if plan.attach.is_some() {
        let sizes: Vec<usize> = (0..len(groups))
            .map(|k| rows(elt(groups, k)).unwrap_or(0))
            .collect();
        match stacked_col_named(groups, b".id", &sizes)? {
            Some(Vals::Int(v)) => v.to_vec(),
            _ => return decline("ids that are not integers"),
        }
    } else {
        Vec::new()
    };
    let cn: Vec<SEXP> = names_of(g)?.to_vec();
    let cols: Vec<SEXP> = (0..len(g)).map(|j| elt(g, j)).collect();
    if !all_bare(&cols)? {
        return decline("a column with attributes");
    }
    let mut gcols: Vec<Vals> = Vec::with_capacity(cols.len());
    for &c in &cols {
        match expr::vals_of(c, false)? {
            Some(v) if v.len() == n => gcols.push(expr::owned(v)),
            _ => return decline("a column that is not a bare atomic vector"),
        }
    }
    let idj = cn
        .iter()
        .position(|&c| bytes(c) == b".id")
        .ok_or_else(|| bad("no .id"))?;
    if !matches!(gcols[idj], Vals::Int(_)) {
        return decline("ids that are not integers");
    }
    // g[sample.int(nrow(g), n, replace = TRUE), ]
    let picks = crate::rng::with_rng(|r| {
        tidyabm_core::sample::sample_int(r, n, count, true).map_err(|e| bad(&format!("birth: {e}")))
    })?;
    let trows: Vec<usize> = picks.iter().map(|&i| i as usize - 1).collect();
    let mut kcols: Vec<Vals> = gcols
        .iter()
        .map(|v| tidyabm_core::expr::take(v, &trows))
        .collect();
    kcols[idj] = Vals::Int(Cow::Owned((next_id..last).collect()));
    let g0 = plan.groups.iter().filter(|e| e.part == 2 || e.part == 3);
    for gp in g0 {
        if gp.prog.is_null() {
            return decline("a birth formula that does not fit the group");
        }
        let Some(ins) = entry_inputs(st, groups, g, gp, &svals, Some(&trows))? else {
            return Ok(None);
        };
        // SAFETY: the program is kept alive by the plan's descriptor.
        let prog = unsafe { &*gp.prog };
        let vals = match prog.compute(ins, count, count)? {
            Ok(r) => r,
            Err(d) => {
                expr::note_decline(&d);
                return Ok(None);
            }
        };
        for (r, v) in vals.into_iter().enumerate() {
            let Some(j) = find(&cn, gp.targets[r]) else {
                return decline("a target that is not a column");
            };
            kcols[j] = expr::owned(recycle(v, count));
        }
    }
    if gcols.iter().zip(&kcols).any(|(a, b)| rank(a) != rank(b)) {
        return decline("offspring columns of another type");
    }
    let all_n = n + count;
    let mut out = OwnedListSexp::new(cols.len(), false)?;
    for (j, (a, b)) in gcols.iter().zip(&kcols).enumerate() {
        let both: Vals = match (a, b) {
            (Vals::Lgl(x), Vals::Lgl(y)) => Vals::Lgl(Cow::Owned([&x[..], &y[..]].concat())),
            (Vals::Int(x), Vals::Int(y)) => Vals::Int(Cow::Owned([&x[..], &y[..]].concat())),
            (Vals::Dbl(x), Vals::Dbl(y)) => Vals::Dbl(Cow::Owned([&x[..], &y[..]].concat())),
            (Vals::Str(x), Vals::Str(y)) => Vals::Str(Cow::Owned([&x[..], &y[..]].concat())),
            _ => return Err(bad("append types")),
        };
        out.set_value(j, expr::to_r(&both, all_n)?)?;
    }
    let born: Vec<(i32, i32)> = (next_id..last).map(|id| (id, NA_INT)).collect();
    let new_edges = match plan.attach {
        Some((mode, links)) => match attach_draws(st, mode, links, &born, &existing)? {
            Some(e) => e,
            None => return Ok(None),
        },
        None => None,
    };
    let frame = tbl_frame(out.inner(), &cn, all_n)?;
    // SAFETY: reading a static of R's (unused names: the indices are in range).
    let blank = unsafe { rapi::R_BlankString };
    let gs = with_elt(groups, 0, frame.get(), blank)?;
    let s1 = with_elt(st, gi, gs.get(), blank)?;
    // SAFETY: main thread; the scalar is kept before anything else allocates.
    let nid = unsafe { build(move || rapi::Rf_ScalarInteger(last)) }?;
    let ni2 = find_str(s1.get(), b"next_id")?.ok_or_else(|| bad("next_id"))?;
    let s2 = with_elt(s1.get(), ni2, nid.get(), blank)?;
    match new_edges {
        None => Ok(Some(s2)),
        Some(e) => {
            let ei = find_str(s2.get(), b"edges")?.ok_or_else(|| bad("edges"))?;
            Ok(Some(with_elt(s2.get(), ei, e.get(), blank)?))
        }
    }
}

/// The cross-product `within` natively ([attribute_view()] then
/// [neighbour_aggregate()] or `summarise()`): for each focal agent, `within`
/// over every candidate (its `own_` columns as values), then each rule over the
/// candidates kept. Aggregates of doubles go to R (`rowsum()`'s sums), and so
/// does a neighbourhood that meets `NA` in a sum.
fn exec_cross(plan: &Plan, st: SEXP) -> savvy::Result<Option<H>> {
    use std::borrow::Cow;
    use tidyabm_core::sample::NA_INT;
    let (fns, narm) = plan.cross.as_ref().ok_or_else(|| bad("no cross"))?;
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
    // candidate columns, and the columns read as `own_<col>`
    let mut cand: Vec<Option<Vals>> = Vec::with_capacity(gp.cols.len());
    let mut own: Vec<Option<Vals>> = Vec::with_capacity(gp.cols.len());
    for &c in &gp.cols {
        let b = bytes(c);
        let (name, is_own) = match b.strip_prefix(b"own_") {
            Some(x) => (x, true),
            None => (b, false),
        };
        let Some(v) = stacked_col_named(groups, name, &sizes)? else {
            return decline("a column of another kind");
        };
        if is_own {
            own.push(Some(v));
            cand.push(None);
        } else {
            cand.push(Some(v));
            own.push(None);
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
    let Some(rest) = inputs(&xs)? else {
        return decline("a global or scope value of an unsupported type");
    };
    // SAFETY: the program is kept alive by the plan's descriptor.
    let prog = unsafe { &*gp.prog };
    // SAFETY: reading statics of R's.
    let (na_real, na_str) = unsafe { (rapi::R_NaReal, rapi::R_NaString as usize) };
    let nr = fns.len();
    let mut vals: Vec<Vec<Option<Vals<'static>>>> = vec![Vec::with_capacity(n); nr];
    let mut any_rows = false;
    for i in 0..n {
        let build = |rows: Option<&[usize]>| -> Vec<Vals> {
            let mut ins = Vec::with_capacity(cand.len() + rest.len());
            for (c, o) in cand.iter().zip(&own) {
                ins.push(match (c, o) {
                    (Some(v), _) => match rows {
                        None => match v {
                            Vals::Lgl(x) => Vals::Lgl(Cow::Borrowed(&x[..])),
                            Vals::Int(x) => Vals::Int(Cow::Borrowed(&x[..])),
                            Vals::Dbl(x) => Vals::Dbl(Cow::Borrowed(&x[..])),
                            Vals::Str(x) => Vals::Str(Cow::Borrowed(&x[..])),
                        },
                        Some(r) => tidyabm_core::expr::take(v, r),
                    },
                    (None, Some(v)) => tidyabm_core::expr::take(v, &[i]),
                    (None, None) => unreachable!("a column is a candidate or an own_ column"),
                });
            }
            ins.extend(rest.iter().cloned());
            ins
        };
        let keep = match prog.compute_one(0, &build(None), n)? {
            Ok(v) => v,
            Err(d) => {
                expr::note_decline(&d);
                return Ok(None);
            }
        };
        let kept: Vec<usize> = match &keep {
            Vals::Lgl(x) if x.len() == n => (0..n).filter(|&j| x[j] == 1).collect(),
            Vals::Lgl(x) if x.len() == 1 => {
                if x[0] == 1 {
                    (0..n).collect()
                } else {
                    Vec::new()
                }
            }
            _ => return decline("a within that is not a condition (R's error)"),
        };
        if kept.is_empty() {
            for v in vals.iter_mut() {
                v.push(None);
            }
            continue;
        }
        any_rows = true;
        let ins = build(Some(&kept));
        let m = kept.len();
        let mut p = 1;
        for (r, &f) in fns.iter().enumerate() {
            if f == 0 {
                vals[r].push(Some(Vals::Int(Cow::Owned(vec![m as i32]))));
                continue;
            }
            let v = match prog.compute_one(p, &ins, m)? {
                // an aggregate's argument is elementwise (a summary: as it is)
                Ok(v) if f < 5 => recycle(v, m),
                Ok(v) => v,
                Err(d) => {
                    expr::note_decline(&d);
                    return Ok(None);
                }
            };
            p += 1;
            let out: Vals<'static> = match f {
                1 | 2 => {
                    let x = match &v {
                        Vals::Lgl(x) | Vals::Int(x) => x,
                        _ => return decline("an aggregate of doubles (rowsum())"),
                    };
                    let mut s: i64 = 0;
                    let mut cnt: i64 = 0;
                    for &e in x.iter() {
                        if e == NA_INT {
                            if narm[r] {
                                continue;
                            }
                            return decline("a neighbourhood sum that meets NA");
                        }
                        s += i64::from(e);
                        cnt += 1;
                    }
                    if f == 1 {
                        if s.abs() > i64::from(i32::MAX) {
                            return decline("an integer sum outside the range");
                        }
                        Vals::Int(Cow::Owned(vec![s as i32]))
                    } else {
                        Vals::Dbl(Cow::Owned(vec![s as f64 / cnt as f64]))
                    }
                }
                3 | 4 => {
                    let Vals::Lgl(x) = &v else {
                        return decline("any()/all() of other than logicals");
                    };
                    let want = if f == 3 { 1 } else { 0 };
                    let hit = x.contains(&want);
                    let na = x.contains(&NA_INT);
                    let o = if f == 3 { hit } else { !hit };
                    let val = if !narm[r] && !hit && na {
                        NA_INT
                    } else {
                        o as i32
                    };
                    Vals::Lgl(Cow::Owned(vec![val]))
                }
                _ => {
                    if v.len() != 1 {
                        return decline("a summary of other than one value");
                    }
                    expr::owned(v)
                }
            };
            vals[r].push(Some(out));
        }
    }
    if !any_rows {
        return decline("nobody has a neighbour");
    }
    // each rule's values, `NA` where nobody was kept (0 for a bare `n()`)
    let mut cols_out: Vec<Vals<'static>> = Vec::with_capacity(nr);
    for (r, vs) in vals.iter().enumerate() {
        let first = vs
            .iter()
            .flatten()
            .next()
            .ok_or_else(|| bad("cross values"))?;
        let rk = rank(first);
        if vs.iter().flatten().any(|v| rank(v) != rk) {
            return decline("summaries of different types");
        }
        let col = match first {
            Vals::Lgl(_) | Vals::Int(_) => {
                let fill = if fns[r] == 0 { 0 } else { NA_INT };
                let d: Vec<i32> = vs
                    .iter()
                    .map(|v| match v {
                        Some(Vals::Lgl(x) | Vals::Int(x)) => x[0],
                        _ => fill,
                    })
                    .collect();
                if matches!(first, Vals::Lgl(_)) {
                    Vals::Lgl(Cow::Owned(d))
                } else {
                    Vals::Int(Cow::Owned(d))
                }
            }
            Vals::Dbl(_) => Vals::Dbl(Cow::Owned(
                vs.iter()
                    .map(|v| match v {
                        Some(Vals::Dbl(x)) => x[0],
                        _ => na_real,
                    })
                    .collect(),
            )),
            Vals::Str(_) => Vals::Str(Cow::Owned(
                vs.iter()
                    .map(|v| match v {
                        Some(Vals::Str(x)) => x[0],
                        _ => na_str,
                    })
                    .collect(),
            )),
        };
        cols_out.push(col);
    }
    let mut cur: Option<H> = None;
    for (r, v) in cols_out.iter().enumerate() {
        let t = gp.targets[r];
        let mut start = 0;
        for (g, &sz) in sizes.iter().enumerate() {
            let rows_g: Vec<usize> = (start..start + sz).collect();
            start += sz;
            if sz == 0 {
                continue;
            }
            let col = tidyabm_core::expr::take(v, &rows_g);
            let gs = cur.as_ref().map_or(groups, |h| h.get());
            let base = elt(gs, g);
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

/// Network `any()`/`all()` natively ([network_view()] then
/// [neighbour_aggregate()]): each agent's neighbours read from
/// `state$nb_cache$nb` when the cache is the current edges' (else R builds
/// it); `NA` for an agent with none. The cache is left as it is, as R leaves a
/// current one.
fn exec_nb_anyall(plan: &Plan, nb: &NbPlan, st: SEXP) -> savvy::Result<Option<H>> {
    use tidyabm_core::sample::NA_INT;
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
    let Some(e) = find_str(st, b"edges")?.map(|i| elt(st, i)) else {
        return decline("no network (R's error)");
    };
    if e == nil || len(e) != 2 {
        return decline("edges with columns besides from and to");
    }
    let en = names_of(e)?;
    if bytes(en[0]) != b"from" || bytes(en[1]) != b"to" {
        return decline("edges with columns besides from and to");
    }
    // `if (!identical(state$nb_cache$edges, state$edges))` the cache is rebuilt:
    // `list(edges = <edges>, nb = neighbour_table(<edges>))`
    let mut cached: Option<SEXP> = None;
    if let Some(i) = find_str(st, b"nb_cache")? {
        let c = elt(st, i);
        if c != nil {
            cached = find_str(c, b"edges")?.map(|j| elt(c, j));
        }
    }
    let current = match cached {
        Some(ce) if ce == e => true,
        Some(ce) => {
            // SAFETY: main thread; both are protected by the state.
            let same =
                unsafe { rapi::protect_value(0, move || rapi::R_compute_identical(ce, e, 16)) }?;
            same != 0
        }
        None => false,
    };
    let (Some(ef), Some(et)) = (
        store::numbers_of::<i32>(elt(e, 0), INTSXP)?,
        store::numbers_of::<i32>(elt(e, 1), INTSXP)?,
    ) else {
        return decline("edges that are not integers");
    };
    if !all_bare(&[elt(e, 0), elt(e, 1)])? {
        return decline("edges with attributes");
    }
    // neighbour_table(): every edge read both ways round
    let focal: Vec<i32> = [ef, et].concat();
    let nbr: Vec<i32> = [et, ef].concat();
    let gi = find_str(st, b"groups")?.ok_or_else(|| bad("no groups"))?;
    let groups = elt(st, gi);
    let Some((n, ids)) = ids_are_rows(groups)? else {
        return decline("ids that are not the row numbers");
    };
    if n == 0 {
        return Ok(Some(keep(st)?));
    }
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
    // the view's rows: both endpoints among the agents (ids are the rows)
    let rows: Vec<(usize, usize)> = focal
        .iter()
        .zip(&nbr)
        .filter(|(&f, &b)| f >= 1 && (f as usize) <= n && b >= 1 && (b as usize) <= n)
        .map(|(&f, &b)| (f as usize - 1, b as usize - 1))
        .collect();
    let mut vals: Vec<Vals> = Vec::with_capacity(res.len());
    for (r, x) in res.into_iter().enumerate() {
        let x = recycle(x, n);
        let Vals::Lgl(x) = &x else {
            return decline("any()/all() of other than logicals");
        };
        let want = if nb.anyall[r] == 1 { 1 } else { 0 };
        let mut seen = vec![false; n];
        let mut hit = vec![false; n];
        let mut na = vec![false; n];
        for &(f, b) in &rows {
            seen[f] = true;
            hit[f] |= x[b] == want;
            na[f] |= x[b] == NA_INT;
        }
        let out: Vec<i32> = (0..n)
            .map(|i| {
                if !seen[i] || (!nb.narm[r] && !hit[i] && na[i]) {
                    NA_INT
                } else {
                    i32::from(if want == 1 { hit[i] } else { !hit[i] })
                }
            })
            .collect();
        vals.push(Vals::Lgl(std::borrow::Cow::Owned(out)));
    }
    let mut cur: Option<H> = None;
    for (k, gids) in ids.iter().enumerate() {
        if gids.is_empty() {
            continue;
        }
        let gs = cur.as_ref().map_or(groups, |h| h.get());
        let mut g: Option<H> = None;
        for (r, &t) in nb.targets.iter().enumerate() {
            let Vals::Lgl(d) = &vals[r] else {
                return Err(bad("an any/all value"));
            };
            let v = Vals::Lgl(std::borrow::Cow::Owned(
                gids.iter().map(|&id| d[id as usize - 1]).collect(),
            ));
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
    if current {
        return Ok(Some(st2));
    }
    let cache = nb_cache_of(e, &focal, &nbr, ef.len())?;
    // SAFETY: making the CHARSXP may allocate; kept at once.
    let key = unsafe { build(|| rapi::Rf_mkChar(c"nb_cache".as_ptr())) }?;
    Ok(Some(set_named(st2.get(), key.get(), cache.get())?))
}

/// `list(edges = e, nb = neighbour_table(e))`: the table `.id`, `.neighbour`
/// (every edge both ways round), `.edge` and `.forward`, or just the first two
/// for no edges, as `tibble::new_tibble()` makes it.
fn nb_cache_of(e: SEXP, focal: &[i32], nbr: &[i32], m: usize) -> savvy::Result<H> {
    use std::borrow::Cow;
    let n2 = focal.len();
    let ncol = if m == 0 { 2 } else { 4 };
    let mut hold = OwnedListSexp::new(ncol, false)?;
    hold.set_value(0, expr::to_r(&Vals::Int(Cow::Borrowed(focal)), n2)?)?;
    hold.set_value(1, expr::to_r(&Vals::Int(Cow::Borrowed(nbr)), n2)?)?;
    if m > 0 {
        let edge: Vec<i32> = (1..=m as i32).chain(1..=m as i32).collect();
        let fwd: Vec<i32> = std::iter::repeat(1)
            .take(m)
            .chain(std::iter::repeat(0).take(m))
            .collect();
        hold.set_value(2, expr::to_r(&Vals::Int(Cow::Owned(edge)), n2)?)?;
        hold.set_value(3, expr::to_r(&Vals::Lgl(Cow::Owned(fwd)), n2)?)?;
    }
    let names: [&std::ffi::CStr; 4] = [c".id", c".neighbour", c".edge", c".forward"];
    let tbl = tbl_frame_named(hold.inner(), &names[..ncol], n2)?;
    let tp = tbl.get();
    if n2 == 0 {
        // a tibble of no rows has the row names `integer(0)`
        // SAFETY: main thread; `tp` is kept by `tbl`; the new vector is
        // protected until set.
        unsafe {
            rapi::protect(move || {
                let rn = rapi::Rf_protect(rapi::Rf_allocVector(INTSXP, 0));
                rapi::Rf_setAttrib(tp, rapi::Rf_install(c"row.names".as_ptr()), rn);
                rapi::Rf_unprotect(1);
            })
        }?;
    }
    // SAFETY: main thread; `e` is protected by the state, `tp` by `tbl`; the
    // list and its names are protected while built.
    unsafe {
        build(move || {
            let l = rapi::Rf_protect(rapi::Rf_allocVector(VECSXP, 2));
            rapi::SET_VECTOR_ELT(l, 0, e);
            rapi::SET_VECTOR_ELT(l, 1, tp);
            let nm = rapi::Rf_protect(rapi::Rf_allocVector(STRSXP, 2));
            rapi::SET_STRING_ELT(nm, 0, rapi::Rf_mkChar(c"edges".as_ptr()));
            rapi::SET_STRING_ELT(nm, 1, rapi::Rf_mkChar(c"nb".as_ptr()));
            rapi::Rf_setAttrib(l, rapi::R_NamesSymbol, nm);
            rapi::Rf_unprotect(2);
            l
        })
    }
}

/// [`tbl_frame`] with the names given as C strings.
fn tbl_frame_named(o: SEXP, names: &[&std::ffi::CStr], n: usize) -> savvy::Result<H> {
    let ncol = names.len();
    let np: Vec<*const std::ffi::c_char> = names.iter().map(|c| c.as_ptr()).collect();
    let npp = np.as_ptr();
    // SAFETY: main thread; the names vector is protected while filled and kept
    // before the frame is built; `npp` points at `ncol` NUL-terminated strings.
    let nm = unsafe {
        build(move || {
            let v = rapi::Rf_protect(rapi::Rf_allocVector(STRSXP, ncol as isize));
            for j in 0..ncol {
                rapi::SET_STRING_ELT(v, j as isize, rapi::Rf_mkChar(*npp.add(j)));
            }
            rapi::Rf_unprotect(1);
            v
        })
    }?;
    let cn: Vec<SEXP> = store::chars_of(nm.get())?.to_vec();
    tbl_frame(o, &cn, n)
}

fn same_side(a: Side, b: Side) -> bool {
    match (a, b) {
        (Side::D(p), Side::D(q)) => p == q,
        (p, q) => p == q,
    }
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
    if nb.anyall.iter().any(|&a| a != 0) {
        return exec_nb_anyall(plan, nb, st);
    }
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
                Vals::Int(d) => Vals::Int(std::borrow::Cow::Owned(
                    gids.iter().map(|id| d[pick(id)]).collect(),
                )),
                Vals::Lgl(d) => Vals::Lgl(std::borrow::Cow::Owned(
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
    if plan.cross.is_some() {
        exec_cross(plan, st)
    } else if plan.gby.is_some() {
        exec_global_by(plan, st)
    } else if plan.seq.is_some() {
        exec_seq(plan, st)
    } else if plan.tell.is_some() {
        exec_tell(plan, st)
    } else if plan.mv.is_some() {
        exec_move(plan, st)
    } else if plan.birth.is_some() {
        exec_birth(plan, st)
    } else if plan.death.is_some() {
        exec_death(plan, st)
    } else if plan.mt.is_some() {
        exec_match(plan, st)
    } else if plan.nbw.is_some() {
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
    /// time spent per step (native attempt or its R callback), in microseconds,
    /// for profiling (`RunLoop$times()`)
    times: Vec<f64>,
    /// record `times` (environment variable `TIDYABM_RUST_TIMING` set): the
    /// clock reads cost ~0.5 us per tick, measurable on short models
    timing: bool,
    /// `abm_repeat()` blocks (kind -1 begin, `-(2 + 2 max + has_until)` end):
    /// `until(k)` the block's condition after a pass, each end's begin, the
    /// passes made and the match standing when the block began
    until: H,
    begin_of: Vec<usize>,
    passes: Vec<i64>,
    stash: Vec<Option<H>>,
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

    /// Block end `i`'s `until` on `st`: natively (a value-only program
    /// compiled per schema in `slots[i]`) when it gives one logical, else from
    /// the R callback ([repeat_until()], which raises R's errors).
    fn until_met(&mut self, i: usize, st: SEXP) -> savvy::Result<bool> {
        let one_true = |x: SEXP| -> Option<bool> {
            // SAFETY: reading a type tag, then a logical of length 1.
            (unsafe { rapi::TYPEOF(x) } == rapi::LGLSXP && len(x) == 1)
                .then(|| unsafe { *rapi::LOGICAL_RO(x) } == 1)
        };
        if let Some(key) = schema_key(st)? {
            let stale = match &self.slots[i] {
                Slot::Never => false,
                Slot::Fresh => true,
                Slot::Skip(k) | Slot::Ready(k, _) => *k != key,
            };
            if stale {
                let d = call_r(&self.prep, &[i as i32 + 1])?;
                // SAFETY: reading a type tag.
                let t = unsafe { rapi::TYPEOF(d.get()) };
                self.slots[i] = if t == rapi::LGLSXP {
                    Slot::Never
                } else if t == VECSXP {
                    Slot::Ready(key, Box::new(parse_plan(d)?))
                } else {
                    Slot::Skip(key)
                };
            }
            if let Slot::Ready(_, plan) = &self.slots[i] {
                if let Some(v) = exec_global(plan, st, true)? {
                    if len(v.get()) == 1 {
                        // (`NA` is not met: `isTRUE()`)
                        if let Some(b) = one_true(elt(v.get(), 0)) {
                            return Ok(b);
                        }
                    }
                }
            }
        }
        let r = call_r(&self.until, &[i as i32 + 1])?;
        Ok(one_true(r.get()) == Some(true))
    }

    fn native_now(&self, i: usize) -> bool {
        self.kinds[i] != 0 && !matches!(self.slots[i], Slot::Never)
    }
}

#[savvy]
impl RunLoop {
    // the R callbacks of a run, one argument each
    #[allow(clippy::too_many_arguments)]
    fn new(
        kinds: IntegerSexp,
        prep: Sexp,
        seg: Sexp,
        tick: Sexp,
        rng: Sexp,
        ctx: Sexp,
        mprep: Sexp,
        until: Sexp,
    ) -> savvy::Result<Self> {
        run(|| {
            // SAFETY: main thread; installing a symbol may allocate, hence protect.
            let st_sym = unsafe {
                rapi::protect_value(rapi::R_NilValue, || rapi::Rf_install(c"st".as_ptr()))
            }?;
            let kinds = kinds.as_slice().to_vec();
            let n_steps = kinds.len();
            // each block end's begin (blocks nest)
            let mut begin_of = vec![0; n_steps];
            let mut open = Vec::new();
            for (i, &k) in kinds.iter().enumerate() {
                if k == -1 {
                    open.push(i);
                } else if k < -1 {
                    begin_of[i] = open
                        .pop()
                        .ok_or_else(|| bad("a block end without a begin"))?;
                }
            }
            if !open.is_empty() {
                return Err(bad("a block begin without an end"));
            }
            Ok(RunLoop {
                until: keep(until.0)?,
                begin_of,
                passes: vec![0; n_steps],
                stash: (0..n_steps).map(|_| None).collect(),
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
                times: vec![0.0; n_steps],
                timing: std::env::var_os("TIDYABM_RUST_TIMING").is_some(),
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
                    let k = self.kinds[i];
                    if k == -1 {
                        // [run_repeat()]: the match standing when the block begins
                        self.stash[i] = match find_str(st, b"match")? {
                            Some(mi) if elt(st, mi) != rapi_nil() => Some(keep(elt(st, mi))?),
                            _ => None,
                        };
                        self.passes[i] = 0;
                        i += 1;
                        continue;
                    }
                    if k < -1 {
                        let b = self.begin_of[i];
                        let v = -i64::from(k) - 2;
                        self.passes[b] += 1;
                        // `until` is asked after every pass, the last one too
                        let mut stop = self.passes[b] >= v / 2;
                        if v % 2 == 1 && self.until_met(i, st)? {
                            stop = true;
                        }
                        if !stop {
                            i = b + 1;
                            continue;
                        }
                        // `state$match <- outer_match`
                        let s2 = match self.stash[b].take() {
                            None => without(st, b"match")?,
                            Some(m) => {
                                // SAFETY: making the CHARSXP may allocate; kept at once.
                                let nm =
                                    unsafe { build(move || rapi::Rf_mkChar(c"match".as_ptr())) }?;
                                set_named(st, nm.get(), m.get())?
                            }
                        };
                        self.put(s2.get())?;
                        st = s2.get();
                        i += 1;
                        continue;
                    }
                    let t0 = self.timing.then(std::time::Instant::now);
                    if self.native_now(i) {
                        if let Some(next) = self.try_native(i, st)? {
                            self.put(next.get())?;
                            st = next.get();
                            if let Some(t0) = t0 {
                                self.times[i] += t0.elapsed().as_secs_f64() * 1e6;
                            }
                            i += 1;
                            continue;
                        }
                        // declined (or R for this schema): the R path, from the
                        // saved `.Random.seed` if the native attempt could draw
                        let restore = self.saved;
                        self.stats[3] += 1;
                        st = self
                            .call_state(seg, &[i as i32 + 1, i as i32 + 1, i32::from(restore)])?;
                        if let Some(t0) = t0 {
                            self.times[i] += t0.elapsed().as_secs_f64() * 1e6;
                        }
                        i += 1;
                        continue;
                    }
                    let mut j = i;
                    while j + 1 < n && !self.native_now(j + 1) {
                        j += 1;
                    }
                    self.stats[3] += 1;
                    st = self.call_state(seg, &[i as i32 + 1, j as i32 + 1, 0])?;
                    // a run of R steps is booked on its first step
                    if let Some(t0) = t0 {
                        self.times[i] += t0.elapsed().as_secs_f64() * 1e6;
                    }
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

    /// Microseconds spent per step (a run of R steps booked on its first).
    fn times(&self) -> savvy::Result<Sexp> {
        run(|| savvy::OwnedRealSexp::try_from_slice(&self.times).map(Into::into))
    }

    /// Native steps run, declined, compiled, and R callbacks for steps.
    fn stats(&self) -> savvy::Result<Sexp> {
        run(|| savvy::OwnedIntegerSexp::try_from_slice(self.stats).map(Into::into))
    }
}
