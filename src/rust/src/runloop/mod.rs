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

mod match_step;
mod move_step;
mod neighbours;
mod population;
mod rules;
mod seq;
mod tell_link;

use match_step::exec_match;
use move_step::exec_move;
use neighbours::{exec_cross, exec_nb, exec_nb1, exec_nb_view, exec_nbw};
use population::{exec_birth, exec_death};
use rules::{exec_by, exec_global, exec_global_by, exec_rules};
use seq::exec_seq;
use tell_link::{exec_draw, exec_link, exec_pairs, exec_tell};

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

/// Replace `miss` entries of `val` with `def` (casting to vctrs common type),
/// matching `run_neighbours()`'s `.default =`.
fn apply_nb_default(
    val: Vals<'static>,
    miss: &[bool],
    def: Option<SEXP>,
) -> savvy::Result<Option<Vals<'static>>> {
    let Some(d) = def else {
        return Ok(Some(val));
    };
    if !miss.iter().any(|&b| b) {
        return Ok(Some(val));
    }
    if !all_bare(&[d])? {
        return Ok(None);
    }
    let Some(dv) = expr::vals_of(d, false)? else {
        return Ok(None);
    };
    if dv.len() != 1 {
        return Ok(None);
    }
    match expr::assign_rows(Some(val), &dv, miss)? {
        Ok(out) => Ok(Some(out)),
        Err(dec) => {
            expr::note_decline(&dec);
            Ok(None)
        }
    }
}

/// The rows of an agent group (a data frame whose first column is `.id`).
fn rows(g: SEXP) -> Option<usize> {
    // SAFETY: reading a type tag.
    if unsafe { rapi::TYPEOF(g) } != VECSXP || len(g) == 0 {
        return None;
    }
    Some(len(elt(g, 0)))
}

/// The schema key: `c(length(globals), lengths(groups))`, plus relation count
/// and each relation's edge column count when `state$relations` is non-NULL.
fn schema_key(st: SEXP) -> savvy::Result<Option<Vec<usize>>> {
    let (Some(gi), Some(li)) = (find_str(st, b"groups")?, find_str(st, b"globals")?) else {
        return Ok(None);
    };
    let groups = elt(st, gi);
    let mut key = vec![len(elt(st, li))];
    for k in 0..len(groups) {
        key.push(len(elt(groups, k)));
    }
    if let Some(ri) = find_str(st, b"relations")? {
        let rels = elt(st, ri);
        if rels != rapi_nil() {
            let nr = len(rels);
            key.push(nr);
            for k in 0..nr {
                let r = elt(rels, k);
                let nc = find_str(r, b"edges")?.map_or(0, |ei| len(elt(r, ei)));
                key.push(nc);
            }
        }
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
    /// `abm_death()` (kind 8): `(prune_edges, wired_on)`
    death: Option<(bool, Option<SEXP>)>,
    /// `abm_birth()` (kind 9): the `as.integer(round(.t))` program
    birth: Option<*const RxProgram>,
    /// `abm_birth(n =)` target group name (`None`: first group)
    birth_group: Option<SEXP>,
    /// `abm_birth(when =)` wired patch group name (decline only if it has parents)
    birth_wired: Option<SEXP>,
    /// `abm_birth(n =)`: the count (`None`: `when =`)
    count: Option<usize>,
    /// `abm_birth(attach_via =)`: 1 `from = "parent"`, 2 a random agent, 3 `from = "random_edge"`; `links`
    attach: Option<(u8, usize)>,
    /// `abm_move()` (kind 10)
    mv: Option<MovePlan>,
    /// `abm_tell()` (kind 11): `when` given, `.resolve` (0 last, 1 first,
    /// 2 sum, 3 mean, 4 error, 5 collect, 6 min, 7 max, 8 add), `to = "neighbours"`,
    /// `.order` given, and edge field name (`edges` / `space_edges`)
    tell: Option<(bool, u8, bool, bool, SEXP)>,
    /// `abm_sequential()` (kind 12): `.order` given, plus any relation reads/writes
    seq: Option<SeqPlan>,
    /// the cross-product `within` (kind 14)
    cross: Option<CrossPlan>,
    /// `abm_global(.by = <name>)` (kind 13): the keys' name, from a global (2)
    /// or the formula's environment (3)
    gby: Option<(SEXP, i32)>,
    /// `abm_draw()` (kind 16): `(.each == "endpoint", edge field name)`
    draw: Option<(bool, SEXP)>,
    /// network `abm_neighbours()` over the pair view (kind 17: `own_`, edge draws, `na.rm`, mixes)
    nbv: Option<NbvPlan>,
    /// network or relation `abm_link()` / `abm_unlink()` (kind 18)
    link: Option<LinkPlan>,
    /// `abm_rules()` relation reads and writes (kind 1)
    rules_rels: Vec<SeqRel>,
    /// `abm_pairs()` (kind 19): `(via, has_when)`
    pairs: Option<(SEXP, bool)>,
    /// `.by` rules: the key column's name
    by: Option<SEXP>,
    nb: Option<NbPlan>,
    /// optional `.default` scalar for `abm_neighbours()` (kept by `_desc`)
    nb_default: Option<SEXP>,
    scope_match: bool,
    has_when: bool,
    draws: bool,
    groups: Vec<GroupPlan>,
    /// R closure returning the formula-environment values (kept by `_desc`)
    scope_fn: Option<SEXP>,
    mixed: Option<Mixed>,
    // the descriptor: keeps the names' CHARSXPs and the programs alive
    _desc: H,
}

/// A relation read or write referenced by an `abm_sequential()` step (FR15b).
struct SeqRel {
    name: SEXP,
    rel: SEXP,
    /// column name on `rel$edges` (`""` for existence `.R` / `.R_back`)
    col: SEXP,
    back: bool,
}

/// An `abm_sequential()` step: `.order` given, `.scope = "population"`,
/// `when =` given, plus resolved relation names.
struct SeqPlan {
    order: bool,
    scope_pop: bool,
    has_when: bool,
    rels: Vec<SeqRel>,
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
    by: Vec<SEXP>,
    dot_by: Option<SEXP>,
    eligible: bool,
    among: bool,
    /// the two role names (`role = list(a = <cond>, b = <cond>)`), or none
    roles: Vec<SEXP>,
    /// `pair = "one_of"` with bare `among = .R` / `.R_back`: `(rel_name, back)`
    among_rel: Option<(SEXP, bool)>,
    /// `pair = "one_of"` with candidate-only `weight =`
    has_weight: bool,
    /// `pair = "nearest"` with continuous-space torus distance
    use_torus: bool,
    /// spatial dims / torus (for `.dist` in `cost =` and torus `nearest`)
    dims: Vec<f64>,
    torus: Vec<bool>,
}

/// An `abm_link()` / `abm_unlink()` step on the network or a relation (kind 18).
struct LinkPlan {
    drop: bool,
    has_when: bool,
    via: Option<SEXP>,
    has_to: bool,
    rels: Vec<SeqRel>,
}

/// A cross-product `abm_neighbours(within = ...)` step (kind 14), optionally
/// with `.dist` and torus-aware spatial cell bucketing.
struct CrossPlan {
    /// per rule: 0 `n()`, 1 `sum`, 2 `mean`, 3 `any`, 4 `all`, 5 other formula
    fns: Vec<u8>,
    narm: Vec<bool>,
    /// per rule: candidate-only aggregate (`n()`, or no `own_`/`.dist`/`n()`)
    cand_only: Vec<bool>,
    has_dist: bool,
    dims: Vec<f64>,
    torus: Vec<bool>,
    shift: f64,
    dist_bucket: bool,
    dist_strict: bool,
    dist_excl_self: bool,
    dist_cand_rest: bool,
    dist_has_rest: bool,
}

/// An `abm_move()` step on a lattice or continuous space:
/// `mode`: 0 random_neighbour, 1 stay, 2 random_empty/random_cell, 3 direction,
/// 4 uphill/downhill, 5 offset, 6 forward.
struct MovePlan {
    mode: u8,
    who: Vec<SEXP>,
    avoid: bool,
    lattice: SEXP,
    nb: SEXP,
    x: SEXP,
    y: SEXP,
    ncell: i32,
    /// `direction =` (FR13) or `axes_only` (FR14): the lattice's `dir_nb` (or `NULL`)
    dir: SEXP,
    /// `range` (>= 1) and `axes_only` (FR14)
    range: usize,
    axes: bool,
    /// `uphill` (1), `downhill` (-1), or none (0) (FR14)
    grad: i32,
    /// the lattice's wired group name (for gradient moves)
    on: Option<SEXP>,
    has_when: bool,
    has_range: bool,
    /// continuous space (`offset()` / `forward()`)
    space: SEXP,
    dims: Vec<f64>,
    torus: Vec<bool>,
}

/// A network `abm_neighbours()` step evaluated over the `(focal, neighbour)`
/// view (FR14: `own_<col>`, edge-draw columns, `na.rm`, mixed aggregates).
struct NbvPlan {
    /// per rule: 0 `n()`, 1 `sum`, 2 `mean`, 3 `any`, 4 `all`
    fns: Vec<u8>,
    narm: Vec<bool>,
    /// per view input (`gp.cols`): 0 cand, 1 focal, 2 draw fwd, 3 draw back,
    /// 4 `.of`, 5 relation col, 6 relation existence (`.R`/`.R_back`)
    vkind: Vec<i32>,
    /// `.back_<name>` CHARSXP for draw inputs
    vback: Vec<SEXP>,
    efield: SEXP,
    cache_name: SEXP,
    /// `within = .R` / `.R_back`: relation name and direction
    rel_name: Option<SEXP>,
    rel_back: bool,
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
    efield: SEXP,
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
    let efield = match find_str(nb, b"efield")? {
        Some(i) => chars(elt(nb, i))?
            .first()
            .copied()
            .ok_or_else(|| bad("nb efield"))?,
        // SAFETY: reading a static of R's (sentinel meaning `"edges"`).
        None => rapi_nil(),
    };
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
        efield,
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

fn parse_rels(r: SEXP) -> savvy::Result<Vec<SeqRel>> {
    let mut rels = Vec::new();
    if r == rapi_nil() {
        return Ok(rels);
    }
    let rnames = chars(field(r, b"names")?)?;
    let rrel = chars(field(r, b"rel")?)?;
    let rcol = chars(field(r, b"col")?)?;
    let rback_v = field(r, b"back")?;
    // SAFETY: reading a type tag, then a logical vector of its length.
    if unsafe { rapi::TYPEOF(rback_v) } != rapi::LGLSXP
        || len(rback_v) != rnames.len()
        || rrel.len() != rnames.len()
        || rcol.len() != rnames.len()
    {
        return Err(bad("rel spec"));
    }
    for j in 0..rnames.len() {
        // SAFETY: as above.
        let back = unsafe { *rapi::LOGICAL_RO(rback_v).add(j) } == 1;
        rels.push(SeqRel {
            name: rnames[j],
            rel: rrel[j],
            col: rcol[j],
            back,
        });
    }
    Ok(rels)
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
    let has_when = match find_str(d, b"has_when")? {
        Some(i) => {
            let v = elt(d, i);
            // SAFETY: reading a type tag, then a logical of length 1.
            let ok = unsafe { rapi::TYPEOF(v) } == rapi::LGLSXP && len(v) == 1;
            // SAFETY: as above.
            ok && unsafe { *rapi::LOGICAL_RO(v) } == 1
        }
        None => false,
    };
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
                Some(b"when") if kind != 9 => 5,
                Some(b"range") => 6,
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
        let dims = match find_str(x, b"dims")? {
            Some(i) => store::numbers_of::<f64>(elt(x, i), rapi::REALSXP)?
                .unwrap_or(&[])
                .to_vec(),
            None => Vec::new(),
        };
        let torus = match find_str(x, b"torus")? {
            Some(i) => {
                let v = elt(x, i);
                // SAFETY: reading a type tag, then a logical vector of its length.
                if unsafe { rapi::TYPEOF(v) } == rapi::LGLSXP {
                    (0..len(v))
                        .map(|j| unsafe { *rapi::LOGICAL_RO(v).add(j) } == 1)
                        .collect()
                } else {
                    Vec::new()
                }
            }
            None => Vec::new(),
        };
        let among_rel = match find_str(x, b"among_rel")? {
            Some(i) => chars(elt(x, i))?
                .first()
                .copied()
                .filter(|&c| !bytes(c).is_empty())
                .map(|rn| {
                    let back = find_str(x, b"among_back").ok().flatten().is_some_and(|bi| {
                        let bv = elt(x, bi);
                        // SAFETY: reading a type tag, then a logical of length 1.
                        unsafe {
                            rapi::TYPEOF(bv) == rapi::LGLSXP
                                && len(bv) == 1
                                && *rapi::LOGICAL_RO(bv) == 1
                        }
                    });
                    (rn, back)
                }),
            None => None,
        };
        let flag_opt = |nm: &[u8]| -> savvy::Result<bool> {
            Ok(match find_str(x, nm)? {
                Some(i) => {
                    let v = elt(x, i);
                    // SAFETY: reading a type tag, then a logical of length 1.
                    let ok = unsafe { rapi::TYPEOF(v) } == rapi::LGLSXP && len(v) == 1;
                    // SAFETY: as above.
                    ok && unsafe { *rapi::LOGICAL_RO(v) } == 1
                }
                None => false,
            })
        };
        Some(MatchPlan {
            pair: int1(b"pair")?,
            size: field(x, b"size")?,
            size_n: usize::try_from(size_n).map_err(|_| bad("match size"))?,
            by: chars(field(x, b"by")?)?,
            dot_by: name(b"dot_by")?,
            eligible: lgl1(b"eligible")?,
            among: lgl1(b"among")?,
            roles: chars(field(x, b"roles")?)?,
            among_rel,
            has_weight: flag_opt(b"has_weight")?,
            use_torus: flag_opt(b"use_torus")?,
            dims,
            torus,
        })
    } else {
        None
    };
    // a drawing match (non-nearest, or nearest with drawing cost) needs RNG save
    let draws = draws || mt.as_ref().is_some_and(|m| m.pair != 5);
    let mv = if kind == 10 {
        let x = field(d, b"mv")?;
        let flag = |nm: &[u8]| -> savvy::Result<bool> {
            Ok(match find_str(x, nm)? {
                Some(i) => {
                    let v = elt(x, i);
                    // SAFETY: reading a type tag, then a logical of length 1.
                    let lg = unsafe { rapi::TYPEOF(v) } == rapi::LGLSXP && len(v) == 1;
                    // SAFETY: as above.
                    lg && unsafe { *rapi::LOGICAL_RO(v) } == 1
                }
                None => false,
            })
        };
        let mode = match find_str(x, b"mode")? {
            Some(i) => store::numbers_of::<i32>(elt(x, i), INTSXP)?
                .and_then(|v| v.first().copied())
                .and_then(|m| u8::try_from(m).ok())
                .ok_or_else(|| bad("move mode"))?,
            None => 0,
        };
        let avoid = flag(b"avoid")?;
        let ncell = store::numbers_of::<i32>(field(x, b"ncell")?, INTSXP)?
            .and_then(|v| v.first().copied())
            .ok_or_else(|| bad("move ncell"))?;
        let y = find_str(x, b"y")?.map_or_else(rapi_nil, |i| elt(x, i));
        let range = match find_str(x, b"range")? {
            Some(i) => store::numbers_of::<i32>(elt(x, i), INTSXP)?
                .and_then(|v| v.first().copied())
                .and_then(|r| usize::try_from(r).ok())
                .filter(|&r| r >= 1)
                .ok_or_else(|| bad("move range"))?,
            None => 1,
        };
        let axes = flag(b"axes")?;
        let grad = match find_str(x, b"grad")? {
            Some(i) => store::numbers_of::<i32>(elt(x, i), INTSXP)?
                .and_then(|v| v.first().copied())
                .ok_or_else(|| bad("move grad"))?,
            None => 0,
        };
        let on = match find_str(x, b"on")? {
            Some(i) => chars(elt(x, i))?
                .first()
                .copied()
                .filter(|&c| !bytes(c).is_empty()),
            None => None,
        };
        let has_when = flag(b"has_when")?;
        let has_range = flag(b"has_range")?;
        let space = find_str(x, b"space")?.map_or_else(rapi_nil, |i| elt(x, i));
        let dims = match find_str(x, b"dims")? {
            Some(i) => store::numbers_of::<f64>(elt(x, i), rapi::REALSXP)?
                .unwrap_or(&[])
                .to_vec(),
            None => Vec::new(),
        };
        let torus = match find_str(x, b"torus")? {
            Some(i) => {
                let v = elt(x, i);
                // SAFETY: reading a type tag, then a logical vector of its length.
                if unsafe { rapi::TYPEOF(v) } == rapi::LGLSXP {
                    (0..len(v))
                        .map(|j| unsafe { *rapi::LOGICAL_RO(v).add(j) } == 1)
                        .collect()
                } else {
                    Vec::new()
                }
            }
            None => Vec::new(),
        };
        Some(MovePlan {
            mode,
            who: chars(field(x, b"who")?)?,
            avoid,
            lattice: field(x, b"lattice")?,
            nb: field(x, b"nb")?,
            x: field(x, b"x")?,
            y,
            ncell,
            dir: find_str(x, b"dir")?.map_or_else(rapi_nil, |i| elt(x, i)),
            range,
            axes,
            grad,
            on,
            has_when,
            has_range,
            space,
            dims,
            torus,
        })
    } else {
        None
    };
    let draws = draws || mv.as_ref().is_some_and(|m| !matches!(m.mode, 1 | 5 | 6));
    let tell = if kind == 11 {
        let x = field(d, b"tell")?;
        let flag = |nm: &[u8]| -> savvy::Result<bool> {
            Ok(match find_str(x, nm)? {
                Some(i) => {
                    let v = elt(x, i);
                    // SAFETY: reading a type tag, then a logical of length 1.
                    let lg = unsafe { rapi::TYPEOF(v) } == rapi::LGLSXP && len(v) == 1;
                    // SAFETY: as above.
                    lg && unsafe { *rapi::LOGICAL_RO(v) } == 1
                }
                None => false,
            })
        };
        let when = flag(b"when")?;
        let res = match chars(field(x, b"resolve")?)?.first().map(|&c| bytes(c)) {
            Some(b"last") => 0,
            Some(b"first") => 1,
            Some(b"sum") => 2,
            Some(b"mean") => 3,
            Some(b"error") => 4,
            Some(b"collect") => 5,
            Some(b"min") => 6,
            Some(b"max") => 7,
            Some(b"add") => 8,
            _ => return Err(bad("tell resolve")),
        };
        let to_nb = flag(b"nb")?;
        let has_order = flag(b"order")?;
        let efield = match find_str(x, b"efield")? {
            Some(i) => chars(elt(x, i))?
                .first()
                .copied()
                .ok_or_else(|| bad("tell efield"))?,
            None => rapi_nil(),
        };
        Some((when, res, to_nb, has_order, efield))
    } else {
        None
    };
    let seq = if kind == 12 {
        let x = field(d, b"seq")?;
        let flag = |nm: &[u8]| -> savvy::Result<bool> {
            Ok(match find_str(x, nm)? {
                Some(i) => {
                    let v = elt(x, i);
                    // SAFETY: reading a type tag, then a logical of length 1.
                    let lg = unsafe { rapi::TYPEOF(v) } == rapi::LGLSXP && len(v) == 1;
                    // SAFETY: as above.
                    lg && unsafe { *rapi::LOGICAL_RO(v) } == 1
                }
                None => false,
            })
        };
        let order = flag(b"order")?;
        let scope_pop = flag(b"scope_pop")?;
        let seq_when = flag(b"when")?;
        let rels = match find_str(x, b"rel")? {
            Some(ri) => parse_rels(elt(x, ri))?,
            None => Vec::new(),
        };
        Some(SeqPlan {
            order,
            scope_pop,
            has_when: seq_when,
            rels,
        })
    } else {
        None
    };
    let cross = if kind == 14 {
        let x = field(d, b"cross")?;
        let flag = |nm: &[u8]| -> savvy::Result<bool> {
            Ok(match find_str(x, nm)? {
                Some(i) => {
                    let v = elt(x, i);
                    // SAFETY: reading a type tag, then a logical of length 1.
                    let lg = unsafe { rapi::TYPEOF(v) } == rapi::LGLSXP && len(v) == 1;
                    // SAFETY: as above.
                    lg && unsafe { *rapi::LOGICAL_RO(v) } == 1
                }
                None => false,
            })
        };
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
        let dims = match find_str(x, b"dims")? {
            Some(i) => store::numbers_of::<f64>(elt(x, i), rapi::REALSXP)?
                .unwrap_or(&[])
                .to_vec(),
            None => Vec::new(),
        };
        let torus = match find_str(x, b"torus")? {
            Some(i) => {
                let tv = elt(x, i);
                // SAFETY: reading a type tag, then a logical vector of its length.
                if unsafe { rapi::TYPEOF(tv) } == rapi::LGLSXP {
                    (0..len(tv))
                        .map(|j| unsafe { *rapi::LOGICAL_RO(tv).add(j) } == 1)
                        .collect()
                } else {
                    Vec::new()
                }
            }
            None => Vec::new(),
        };
        let shift = match find_str(x, b"shift")? {
            Some(i) => store::numbers_of::<f64>(elt(x, i), rapi::REALSXP)?
                .and_then(|s| s.first().copied())
                .unwrap_or(0.0),
            None => 0.0,
        };
        let cand_only = match find_str(x, b"cand_only")? {
            Some(i) => {
                let cv = elt(x, i);
                // SAFETY: reading a type tag, then a logical vector of its length.
                if unsafe { rapi::TYPEOF(cv) } == rapi::LGLSXP && len(cv) == fns.len() {
                    (0..len(cv))
                        .map(|j| unsafe { *rapi::LOGICAL_RO(cv).add(j) } == 1)
                        .collect()
                } else {
                    vec![false; fns.len()]
                }
            }
            None => vec![false; fns.len()],
        };
        Some(CrossPlan {
            fns,
            narm,
            cand_only,
            has_dist: flag(b"has_dist")?,
            dims,
            torus,
            shift,
            dist_bucket: flag(b"dist_bucket")?,
            dist_strict: flag(b"dist_strict")?,
            dist_excl_self: flag(b"dist_excl_self")?,
            dist_cand_rest: flag(b"dist_cand_rest")?,
            dist_has_rest: flag(b"dist_has_rest")?,
        })
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
    let draws = draws || seq.as_ref().is_some_and(|s| !s.order);
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
    let birth_group = if kind == 9 {
        let x = field(d, b"birth")?;
        match find_str(x, b"group")? {
            Some(i) => chars(elt(x, i))?
                .first()
                .copied()
                .filter(|&c| !bytes(c).is_empty()),
            None => None,
        }
    } else {
        None
    };
    let birth_wired = if kind == 9 {
        let x = field(d, b"birth")?;
        match find_str(x, b"wired")? {
            Some(i) => chars(elt(x, i))?
                .first()
                .copied()
                .filter(|&c| !bytes(c).is_empty()),
            None => None,
        }
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
        let wired = match find_str(x, b"wired")? {
            Some(i) => chars(elt(x, i))?
                .first()
                .copied()
                .filter(|&c| !bytes(c).is_empty()),
            None => None,
        };
        // SAFETY: as above.
        Some((ok && unsafe { *rapi::LOGICAL_RO(v) } == 1, wired))
    } else {
        None
    };
    let draw = if kind == 16 {
        let x = field(d, b"draw")?;
        let ep = field(x, b"endpoint")?;
        // SAFETY: reading a type tag, then a logical of length 1.
        let lg = unsafe { rapi::TYPEOF(ep) } == rapi::LGLSXP && len(ep) == 1;
        let efield = match find_str(x, b"efield")? {
            Some(i) => chars(elt(x, i))?
                .first()
                .copied()
                .ok_or_else(|| bad("draw efield"))?,
            None => rapi_nil(),
        };
        // SAFETY: as above.
        Some((lg && unsafe { *rapi::LOGICAL_RO(ep) } == 1, efield))
    } else {
        None
    };
    let nbv = if kind == 17 {
        let x = field(d, b"nbv")?;
        let fns: Vec<u8> = store::numbers_of::<i32>(field(x, b"fns")?, INTSXP)?
            .ok_or_else(|| bad("nbv fns"))?
            .iter()
            .map(|&f| u8::try_from(f).map_err(|_| bad("nbv fn")))
            .collect::<savvy::Result<Vec<_>>>()?;
        let v = field(x, b"narm")?;
        // SAFETY: reading a type tag, then a logical vector of its length.
        if unsafe { rapi::TYPEOF(v) } != rapi::LGLSXP || len(v) != fns.len() {
            return Err(bad("nbv narm"));
        }
        // SAFETY: as above.
        let narm = (0..len(v))
            .map(|i| unsafe { *rapi::LOGICAL_RO(v).add(i) } == 1)
            .collect();
        let vkind = store::numbers_of::<i32>(field(x, b"vkind")?, INTSXP)?
            .unwrap_or(&[])
            .to_vec();
        let vback = chars(field(x, b"vback")?)?;
        if vkind.len() != vback.len() {
            return Err(bad("nbv inputs"));
        }
        let efield = match find_str(x, b"efield")? {
            Some(i) => chars(elt(x, i))?
                .first()
                .copied()
                .ok_or_else(|| bad("nbv efield"))?,
            None => rapi_nil(),
        };
        let cache_name = match find_str(x, b"cache_name")? {
            Some(i) => chars(elt(x, i))?
                .first()
                .copied()
                .ok_or_else(|| bad("nbv cache_name"))?,
            None => rapi_nil(),
        };
        let rel_name = match find_str(x, b"rel_name")? {
            Some(i) => chars(elt(x, i))?
                .first()
                .copied()
                .filter(|&c| !bytes(c).is_empty()),
            None => None,
        };
        let rel_back = match find_str(x, b"rel_back")? {
            Some(i) => {
                let bv = elt(x, i);
                // SAFETY: reading a type tag, then a logical of length 1.
                unsafe {
                    rapi::TYPEOF(bv) == rapi::LGLSXP && len(bv) == 1 && *rapi::LOGICAL_RO(bv) == 1
                }
            }
            None => false,
        };
        Some(NbvPlan {
            fns,
            narm,
            vkind,
            vback,
            efield,
            cache_name,
            rel_name,
            rel_back,
        })
    } else {
        None
    };
    let link = if kind == 18 {
        let x = field(d, b"link")?;
        let flag = |nm: &[u8]| -> savvy::Result<bool> {
            Ok(match find_str(x, nm)? {
                Some(i) => {
                    let v = elt(x, i);
                    // SAFETY: reading a type tag, then a logical of length 1.
                    let lg = unsafe { rapi::TYPEOF(v) } == rapi::LGLSXP && len(v) == 1;
                    // SAFETY: as above.
                    lg && unsafe { *rapi::LOGICAL_RO(v) } == 1
                }
                None => false,
            })
        };
        let via = match find_str(x, b"via")? {
            Some(i) => chars(elt(x, i))?
                .first()
                .copied()
                .filter(|&c| !bytes(c).is_empty()),
            None => None,
        };
        let rels = match find_str(x, b"rel")? {
            Some(ri) => parse_rels(elt(x, ri))?,
            None => Vec::new(),
        };
        Some(LinkPlan {
            drop: flag(b"drop")?,
            has_when: flag(b"when")?,
            via,
            has_to: flag(b"has_to")?,
            rels,
        })
    } else {
        None
    };
    let rules_rels = if kind == 1 {
        match find_str(d, b"rel")? {
            Some(ri) => parse_rels(elt(d, ri))?,
            None => Vec::new(),
        }
    } else {
        Vec::new()
    };
    let pairs = if kind == 19 {
        let x = field(d, b"pairs")?;
        let via = chars(field(x, b"via")?)?
            .first()
            .copied()
            .ok_or_else(|| bad("pairs via"))?;
        let w = field(x, b"when")?;
        // SAFETY: reading a type tag, then a logical of length 1.
        let lg = unsafe { rapi::TYPEOF(w) } == rapi::LGLSXP && len(w) == 1;
        // SAFETY: as above.
        Some((via, lg && unsafe { *rapi::LOGICAL_RO(w) } == 1))
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
    let nb_default = find_str(d, b"nb_default")?
        .map(|i| elt(d, i))
        .filter(|&v| v != rapi_nil());
    Ok(Plan {
        pairs,
        link,
        rules_rels,
        nbv,
        draw,
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
        birth_group,
        birth_wired,
        by,
        global: kind == 2,
        nb,
        nb_default,
        scope_match,
        has_when,
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
/// `.tick` reads `state$tick`.
fn read_globals(st: SEXP, globs: &[SEXP], ins: &mut Vec<SEXP>) -> savvy::Result<bool> {
    if globs.is_empty() {
        return Ok(true);
    }
    let li = find_str(st, b"globals")?;
    let gl = li.map(|i| elt(st, i));
    let names = match gl {
        Some(g) => names_of(g)?,
        None => &[],
    };
    let mut tick_val: Option<SEXP> = None;
    for &g in globs {
        if bytes(g) == b".tick" {
            let tv = match tick_val {
                Some(v) => v,
                None => {
                    let Some(ti) = find_str(st, b"tick")? else {
                        return Ok(false);
                    };
                    let v = elt(st, ti);
                    tick_val = Some(v);
                    v
                }
            };
            ins.push(tv);
            continue;
        }
        let Some(gl_s) = gl else {
            return Ok(false);
        };
        let Some(j) = find(names, g) else {
            return Ok(false);
        };
        ins.push(elt(gl_s, j));
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

/// Gather column `col` from the lattice's wired group (`state$lattice$on`) at
/// cell ids `cells` (`attach_cell_columns()`). `None` if unavailable or unsupported.
fn cell_col_for_cells(
    st: SEXP,
    groups: SEXP,
    cells: &[i32],
    col: &[u8],
) -> savvy::Result<Option<Vals<'static>>> {
    use std::borrow::Cow;
    use tidyabm_core::sample::NA_INT;
    let Some(lati) = find_str(st, b"lattice")? else {
        return Ok(None);
    };
    let lat = elt(st, lati);
    if lat == rapi_nil() {
        return Ok(None);
    }
    let Some(oni) = find_str(lat, b"on")? else {
        return Ok(None);
    };
    let on_s = elt(lat, oni);
    if on_s == rapi_nil() {
        return Ok(None);
    }
    let on_chars = chars(on_s)?;
    let Some(&on_name) = on_chars.first() else {
        return Ok(None);
    };
    let Some(wi) = find(names_of(groups)?, on_name) else {
        return Ok(None);
    };
    let wired = elt(groups, wi);
    let Some(wn) = rows(wired) else {
        return Ok(None);
    };
    let Some(ci) = find_str(wired, col)? else {
        return Ok(None);
    };
    let csexp = elt(wired, ci);
    if !all_bare(&[csexp])? {
        return Ok(None);
    }
    let Some(wval) = expr::vals_of(csexp, false)? else {
        return Ok(None);
    };
    let Some(widi) = find_str(wired, b".id")? else {
        return Ok(None);
    };
    let Some(wids) = store::numbers_of::<i32>(elt(wired, widi), INTSXP)? else {
        return Ok(None);
    };
    let dense = wids.len() == wn && wids.iter().enumerate().all(|(i, &id)| id == (i as i32 + 1));
    let widx = if dense {
        None
    } else {
        Some(IdIndex::new(wids))
    };
    let rows_c: Vec<Option<usize>> = cells
        .iter()
        .map(|&c| {
            if c == NA_INT {
                None
            } else if let Some(ref ix) = widx {
                ix.get(c)
            } else if c >= 1 && (c as usize) <= wn {
                Some(c as usize - 1)
            } else {
                None
            }
        })
        .collect();
    // SAFETY: reading statics of R's.
    let (na_real, na_str) = unsafe { (rapi::R_NaReal, rapi::R_NaString as usize) };
    Ok(Some(match &wval {
        Vals::Dbl(x) => Vals::Dbl(Cow::Owned(
            rows_c.iter().map(|r| r.map_or(na_real, |r| x[r])).collect(),
        )),
        Vals::Int(x) => Vals::Int(Cow::Owned(
            rows_c.iter().map(|r| r.map_or(NA_INT, |r| x[r])).collect(),
        )),
        Vals::Lgl(x) => Vals::Lgl(Cow::Owned(
            rows_c.iter().map(|r| r.map_or(NA_INT, |r| x[r])).collect(),
        )),
        Vals::Str(x) => Vals::Str(Cow::Owned(
            rows_c.iter().map(|r| r.map_or(na_str, |r| x[r])).collect(),
        )),
    }))
}

/// The columns `augment_group()` / `attach_cell_columns()` add to group `g`, in
/// the order of `names` (`.partner`, `.role`, `.group_id`, `.cost`,
/// `partner_<col>`, `cell_<col>`).
fn match_inputs(
    st: SEXP,
    groups: SEXP,
    g: SEXP,
    names: &[&[u8]],
) -> savvy::Result<Option<Vec<Vals<'static>>>> {
    match_inputs_rel(st, groups, g, names, &[], None)
}

fn match_inputs_rel(
    st: SEXP,
    groups: SEXP,
    g: SEXP,
    names: &[&[u8]],
    rels: &[SeqRel],
    rels_ov: Option<SEXP>,
) -> savvy::Result<Option<Vec<Vals<'static>>>> {
    let Some(gi) = find_str(g, b".id")? else {
        return Ok(None);
    };
    let Some(gids) = store::numbers_of::<i32>(elt(g, gi), INTSXP)? else {
        return Ok(None);
    };
    let cells = if names.iter().any(|b| b.starts_with(b"cell_")) {
        let Some(ci) = find_str(g, b".cell")? else {
            return Ok(None);
        };
        let Some(cv) = store::numbers_of::<i32>(elt(g, ci), INTSXP)? else {
            return Ok(None);
        };
        Some(cv)
    } else {
        None
    };
    match_inputs_with_cells(st, groups, gids, cells, names, rels, rels_ov)
}

/// [`match_inputs`] for the stacked population with ids `gids`.
fn match_inputs_ids(
    st: SEXP,
    groups: SEXP,
    gids: &[i32],
    names: &[&[u8]],
) -> savvy::Result<Option<Vec<Vals<'static>>>> {
    match_inputs_ids_rel(st, groups, gids, names, &[], None)
}

fn match_inputs_ids_rel(
    st: SEXP,
    groups: SEXP,
    gids: &[i32],
    names: &[&[u8]],
    rels: &[SeqRel],
    rels_ov: Option<SEXP>,
) -> savvy::Result<Option<Vec<Vals<'static>>>> {
    let owned_cells: Option<Vec<i32>> = if names.iter().any(|b| b.starts_with(b"cell_")) {
        let sizes: Vec<usize> = (0..len(groups))
            .map(|k| rows(elt(groups, k)).unwrap_or(0))
            .collect();
        match stacked_pad_named(groups, b".cell", &sizes)? {
            Some(Vals::Int(v)) => Some(v.into_owned()),
            _ => return Ok(None),
        }
    } else {
        None
    };
    match_inputs_with_cells(
        st,
        groups,
        gids,
        owned_cells.as_deref(),
        names,
        rels,
        rels_ov,
    )
}

fn stacked_pad_named(
    groups: SEXP,
    name: &[u8],
    sizes: &[usize],
) -> savvy::Result<Option<Vals<'static>>> {
    // SAFETY: main thread; mkChar may allocate, keep handle alive.
    let cname = unsafe {
        let mut buf = Vec::with_capacity(name.len() + 1);
        buf.extend_from_slice(name);
        buf.push(0);
        let ptr = buf.as_ptr() as *const std::ffi::c_char;
        build(move || rapi::Rf_mkChar(ptr))
    }?;
    stacked_pad(groups, cname.get(), sizes)
}

fn match_inputs_with_cells(
    st: SEXP,
    groups: SEXP,
    gids: &[i32],
    cells: Option<&[i32]>,
    names: &[&[u8]],
    rels: &[SeqRel],
    rels_ov: Option<SEXP>,
) -> savvy::Result<Option<Vec<Vals<'static>>>> {
    use std::borrow::Cow;
    use tidyabm_core::sample::NA_INT;
    let needs_match = names.iter().any(|b| !b.starts_with(b"cell_"));
    // SAFETY: reading statics of R's.
    let (na_real, na_str) = unsafe { (rapi::R_NaReal, rapi::R_NaString as usize) };
    let match_data = if needs_match {
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
        let mcost: Option<&[f64]> = match col(b".cost")? {
            Some(c) => store::numbers_of::<f64>(c, rapi::REALSXP)?,
            None => None,
        };
        let first = IdIndex::new(mid);
        let idx: Vec<Option<usize>> = gids.iter().map(|&id| first.get(id)).collect();
        let partner: Vec<i32> = idx.iter().map(|r| r.map_or(NA_INT, |r| mpart[r])).collect();
        Some((size, mgid, mrole, mcost, idx, partner))
    } else {
        None
    };
    let mut out = Vec::with_capacity(names.len());
    // the stacked population, built once if a partner column is read
    let mut stack: Option<(Vec<usize>, IdIndex)> = None;
    for &b in names {
        if let Some(ccol) = b.strip_prefix(b"cell_") {
            let Some(cs) = cells else {
                return Ok(None);
            };
            let Some(v) = cell_col_for_cells(st, groups, cs, ccol)? else {
                return Ok(None);
            };
            out.push(v);
            continue;
        }
        let (size, mgid, mrole, mcost, ref idx, ref partner) =
            *match_data.as_ref().ok_or_else(|| bad("match data"))?;
        if let Some(sr) = rels.iter().find(|r| bytes(r.name) == b) {
            if size != 2 {
                return Ok(None);
            }
            let rels_s = match rels_ov {
                Some(rs) => rs,
                None => match find_str(st, b"relations")? {
                    Some(ri) => elt(st, ri),
                    None => return Ok(None),
                },
            };
            if rels_s == rapi_nil() {
                return Ok(None);
            }
            let Some(rk) = find(names_of(rels_s)?, sr.rel) else {
                return Ok(None);
            };
            let rel = elt(rels_s, rk);
            let Some(ei) = find_str(rel, b"edges")? else {
                return Ok(None);
            };
            let edges = elt(rel, ei);
            let (Some(fi_c), Some(ti_c)) = (find_str(edges, b"from")?, find_str(edges, b"to")?)
            else {
                return Ok(None);
            };
            let (Some(ef), Some(et)) = (
                store::numbers_of::<i32>(elt(edges, fi_c), INTSXP)?,
                store::numbers_of::<i32>(elt(edges, ti_c), INTSXP)?,
            ) else {
                return Ok(None);
            };
            let mut rmap: std::collections::HashMap<(i32, i32), usize> =
                std::collections::HashMap::with_capacity(ef.len());
            for (r, (&f, &t)) in ef.iter().zip(et).enumerate() {
                rmap.entry((f, t)).or_insert(r);
            }
            let rows_r: Vec<Option<usize>> = gids
                .iter()
                .zip(partner)
                .map(|(&me, &pt)| {
                    if pt == NA_INT {
                        None
                    } else {
                        let key = if sr.back { (pt, me) } else { (me, pt) };
                        rmap.get(&key).copied()
                    }
                })
                .collect();
            if bytes(sr.col).is_empty() {
                out.push(Vals::Lgl(Cow::Owned(
                    rows_r.iter().map(|r| i32::from(r.is_some())).collect(),
                )));
            } else {
                let Some(ci) = find(names_of(edges)?, sr.col) else {
                    return Ok(None);
                };
                let csexp = elt(edges, ci);
                if !all_bare(&[csexp])? {
                    return Ok(None);
                }
                let Some(ev) = expr::vals_of(csexp, false)? else {
                    return Ok(None);
                };
                out.push(match &ev {
                    Vals::Dbl(x) => Vals::Dbl(Cow::Owned(
                        rows_r.iter().map(|r| r.map_or(na_real, |r| x[r])).collect(),
                    )),
                    Vals::Int(x) => Vals::Int(Cow::Owned(
                        rows_r.iter().map(|r| r.map_or(NA_INT, |r| x[r])).collect(),
                    )),
                    Vals::Lgl(x) => Vals::Lgl(Cow::Owned(
                        rows_r.iter().map(|r| r.map_or(NA_INT, |r| x[r])).collect(),
                    )),
                    Vals::Str(x) => Vals::Str(Cow::Owned(
                        rows_r.iter().map(|r| r.map_or(na_str, |r| x[r])).collect(),
                    )),
                });
            }
            continue;
        }
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
            b".cost" => Vals::Dbl(Cow::Owned(
                idx.iter()
                    .map(|r| match (r, mcost) {
                        (Some(r), Some(mc)) => mc[*r],
                        _ => na_real,
                    })
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

/// Convert numeric/logical `Vals` to `Vec<f64>` (`NA_INT -> na_real`), matching
/// R's `as.double()`. `None` for strings.
fn vals_as_f64(v: &Vals, na_real: f64) -> Option<Vec<f64>> {
    use tidyabm_core::sample::NA_INT;
    match v {
        Vals::Dbl(x) => Some(x.to_vec()),
        Vals::Int(x) | Vals::Lgl(x) => Some(
            x.iter()
                .map(|&e| if e == NA_INT { na_real } else { f64::from(e) })
                .collect(),
        ),
        Vals::Str(_) => None,
    }
}

/// Torus-aware Euclidean distance between `(x1, y1)` and `(x2, y2)`, matching
/// `space_distance()` bit-for-bit.
fn space_dist_pair(
    x1: f64,
    y1: Option<f64>,
    x2: f64,
    y2: Option<f64>,
    dims: &[f64],
    torus: &[bool],
) -> f64 {
    let mut dx = (x1 - x2).abs();
    if torus.first().copied().unwrap_or(false) && !dx.is_nan() {
        dx = dx.min(dims[0] - dx);
    }
    let (Some(y1), Some(y2)) = (y1, y2) else {
        return dx;
    };
    if dims.len() < 2 {
        return dx;
    }
    let mut dy = (y1 - y2).abs();
    if torus.get(1).copied().unwrap_or(false) && !dy.is_nan() {
        dy = dy.min(dims[1] - dy);
    }
    (dx * dx + dy * dy).sqrt()
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

fn exec(plan: &Plan, st: SEXP) -> savvy::Result<Option<H>> {
    if let Some(nb) = &plan.nb {
        return exec_nb(plan, nb, st);
    }
    if plan.pairs.is_some() {
        exec_pairs(plan, st)
    } else if plan.link.is_some() {
        exec_link(plan, st)
    } else if plan.nbv.is_some() {
        exec_nb_view(plan, st)
    } else if plan.draw.is_some() {
        exec_draw(plan, st)
    } else if plan.cross.is_some() {
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
        let Some(base_key) = schema_key(st)? else {
            return Ok(None);
        };
        let mut key = base_key.clone();
        let nb_by_edge =
            self.kinds[i] == 3 && !matches!(&self.slots[i], Slot::Ready(_, p) if p.nbv.is_some());
        if nb_by_edge {
            // a neighbour index is built for these edges and this many agents
            let s = st;
            let edges = find_str(s, b"edges")?.map_or(0, |e| elt(s, e) as usize);
            let sp_edges = find_str(s, b"space_edges")?.map_or(0, |e| elt(s, e) as usize);
            let gs = find_str(s, b"groups")?.map_or(rapi_nil(), |g| elt(s, g));
            let n: usize = (0..len(gs)).map(|k| rows(elt(gs, k)).unwrap_or(0)).sum();
            key.extend([edges, sp_edges, n]);
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
                let p = parse_plan(d)?;
                let store_key = if p.nbv.is_some() { base_key } else { key };
                Slot::Ready(store_key, Box::new(p))
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
    fn log_groups(&self, keep_now: bool, slog: &Sexp, t: i32, st: SEXP) -> savvy::Result<()> {
        let t = t as usize;
        // SAFETY: reading a type tag.
        if !keep_now || unsafe { rapi::TYPEOF(slog.0) } != VECSXP || t >= len(slog.0) {
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
                mslots: vec![Slot::Fresh, Slot::Fresh, Slot::Fresh],
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
            let has_u = has.get(2) == Some(&true);
            let record_globals = has.get(3) == Some(&true);
            self.put(state.0)?;
            let (seg, tick) = (self.seg.get(), self.tick.get());
            let mut st = self.call_state(tick, &[0, i32::from(ticks > 0)])?;
            for t in 1..=ticks {
                // SAFETY: main thread; `R_CheckUserInterrupt` longjmps on Ctrl-C,
                // caught by `protect` so Rust frames drop cleanly.
                unsafe { rapi::protect(|| rapi::R_CheckUserInterrupt()) }?;
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
                        if self.passes[b] % 64 == 0 {
                            // SAFETY: main thread; catches Ctrl-C inside long repeat blocks.
                            unsafe { rapi::protect(|| rapi::R_CheckUserInterrupt()) }?;
                        }
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
                    while j + 1 < n && self.kinds[j + 1] >= 0 && !self.native_now(j + 1) {
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
                let stop_now = if has_u {
                    let nat = match self.try_tick(2, st)? {
                        Some(v) if len(v.get()) == 1 => {
                            let x = elt(v.get(), 0);
                            // SAFETY: reading a type tag, then a logical of length 1.
                            if unsafe { rapi::TYPEOF(x) } == rapi::LGLSXP && len(x) == 1 {
                                // SAFETY: as above.
                                Some(unsafe { *rapi::LOGICAL_RO(x) } == 1)
                            } else {
                                None
                            }
                        }
                        _ => None,
                    };
                    match nat {
                        Some(b) => b,
                        None => {
                            let r = call_r(&self.until, &[0])?;
                            let rv = r.get();
                            // SAFETY: reading a type tag, then a logical of length 1.
                            let lgl = unsafe { rapi::TYPEOF(rv) } == rapi::LGLSXP && len(rv) == 1;
                            // SAFETY: as above.
                            lgl && unsafe { *rapi::LOGICAL_RO(rv) } == 1
                        }
                    }
                } else {
                    false
                };
                let next = if !stop_now && t < ticks { t + 1 } else { 0 };
                let keep_now =
                    keep_t.get(t as usize) == Some(&true) || (stop_now && !record_globals);
                if !glog_ok || need[t as usize] {
                    st = self.call_state(tick, &[t, next])?;
                    self.log_groups(keep_now, &slog, t, st)?;
                    if stop_now {
                        break;
                    }
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
                    self.log_groups(keep_now, &slog, t, st)?;
                    if stop_now {
                        break;
                    }
                    continue;
                }
                self.log_groups(keep_now, &slog, t, st)?;
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
                if stop_now {
                    break;
                }
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
