//! The R side of the column store (migration phase 3): loading a run's groups
//! (a named list of tibbles) into `tidyabm_core::store`, handing them back as R
//! objects, recording snapshots and stacking them into the run's result.
//!
//! Formats (PLAN.md design rule 5): doubles, integers and logicals are copied
//! bit for bit (NA payloads included); a character column becomes codes into a
//! pool of R's own `CHARSXP`s, so exporting it allocates no string and gives the
//! very `CHARSXP`s R had (S3: savvy's per-element `set_elt` costs 220 ns, a pool
//! lookup a few); a typed column with attributes (factor, Date, names) keeps them
//! on a zero-length donor vector and gets them back with
//! `SHALLOW_DUPLICATE_ATTRIB()`; anything else (list columns, complex, raw) is an
//! opaque, preserved R object.
//!
//! The mirror cache: a column, a group and the list of groups may keep an R
//! object that holds exactly their Rust values. While Rust has not changed them,
//! `export()` returns that object (no copy) and `load()` recognises it by address
//! and copies nothing. An export that has to build a vector from the Rust values
//! keeps it as the new mirror. With `use_mirror = TRUE` (the "rust" engine) the R
//! object a column is loaded from is its mirror too; with `use_mirror = FALSE`
//! (the "both" engine) it is not, so every value R reads back was converted from
//! Rust, once per change, which is what the differential harness checks.
//!
//! Lazy copy-in (phase 4): under `use_mirror` a typed column R hands over is not
//! copied but kept as the R vector (`ColData::Opaque` of a typed vector, which is
//! its own mirror), and a group's frame attributes are read off its mirror only
//! when the mirror has to go. Values are copied into Rust when a snapshot that
//! may reach the result is taken ([`AgentStore::materialise`]). Copy-in with the
//! attribute check cost ~9 us per changed column at n = 441 (P3), more than most
//! steps' own work; keeping the vector costs one preserved-list entry. Under
//! "both" (`use_mirror = FALSE`) every column is still copied in at once, so the
//! differential harness keeps checking the conversions.
//!
//! The write API ([`AgentStore::set_col`]) replaces or appends one column and
//! drops that column's, its group's and the list's mirrors in one place, so
//! `export()` can never hand R a stale object.
//!
//! Every R object the store holds is kept alive through savvy's preserved list
//! ([`Kept`]); every R call that may allocate or raise runs inside
//! `rapi::protect`, and nothing here runs off the main thread.

use crate::rapi::{self, SexpType, INTSXP, LGLSXP, REALSXP, STRSXP, VECSXP};
use crate::{run, typed_error};
use savvy::ffi::SEXP;
use savvy::protect::{insert_to_preserved_list, release_from_preserved_list};
use savvy::{
    savvy, ListSexp, NullSexp, OwnedIntegerSexp, OwnedListSexp, OwnedLogicalSexp, OwnedRealSexp,
    OwnedStringSexp, Sexp, StringSexp,
};
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;
use tidyabm_core::store::{self as cs, ColData, Column, Group, Values, NA_CODE};

/// An R object kept alive while Rust holds it (an entry in savvy's preserved list,
/// released on drop; dropping never allocates, so it is safe in a finalizer).
pub struct Kept {
    sexp: SEXP,
    token: SEXP,
}

impl Drop for Kept {
    fn drop(&mut self) {
        release_from_preserved_list(self.token);
    }
}

type H = Rc<Kept>;

/// Keep `x` alive (main thread; the list insertion allocates, hence `protect`).
fn keep(x: SEXP) -> savvy::Result<H> {
    // SAFETY: main thread (every caller is an entry point); `x` is reachable from
    // an argument of the current `.Call` or already kept, so it is protected.
    let token =
        unsafe { rapi::protect_value(rapi::R_NilValue, move || insert_to_preserved_list(x)) }?;
    Ok(Rc::new(Kept { sexp: x, token }))
}

/// `x`'s attributes on a new zero-length vector of type `ty`, kept; `None` when
/// `x` has none (the copy is then `identical()` to the bare `empty`).
fn donor(x: SEXP, ty: SexpType, empty: SEXP) -> savvy::Result<Option<H>> {
    // SAFETY: main thread; `x` and `empty` are protected (argument / kept). The new
    // vector is protected while the attributes are copied and compared, and is
    // inserted into the preserved list inside the same protected call, so it is
    // never reachable by the collector unprotected. No panic inside.
    let (t, token) = unsafe {
        rapi::protect_value((rapi::R_NilValue, rapi::R_NilValue), move || {
            let t = rapi::Rf_protect(rapi::Rf_allocVector(ty, 0));
            rapi::SHALLOW_DUPLICATE_ATTRIB(t, x);
            // flags 16: identical()'s defaults
            let bare = rapi::R_compute_identical(t, empty, 16) != 0;
            let token = if bare {
                rapi::R_NilValue
            } else {
                insert_to_preserved_list(t)
            };
            rapi::Rf_unprotect(1);
            (t, token)
        })
    }?;
    // SAFETY: reading a static of R's.
    if token == unsafe { rapi::R_NilValue } {
        return Ok(None);
    }
    Ok(Some(Rc::new(Kept { sexp: t, token })))
}

/// The `CHARSXP`s of character vector `x` (protected by the caller), as a slice
/// valid while `x` is: an ALTREP vector is materialised, which may allocate.
fn chars_of<'a>(x: SEXP) -> savvy::Result<&'a [SEXP]> {
    // SAFETY: main thread; `x` is a protected STRSXP (checked by every caller).
    let n = unsafe { rapi::Rf_xlength(x) } as usize;
    if n == 0 {
        return Ok(&[]);
    }
    // SAFETY: main thread; DATAPTR_OR_NULL neither allocates nor raises (NULL for
    // an ALTREP vector without its data in memory); the data of a STRSXP is its
    // `n` CHARSXPs, which stay put while `x` is protected.
    let direct = unsafe { rapi::DATAPTR_OR_NULL(x) } as *const SEXP;
    if !direct.is_null() {
        // SAFETY: as just above.
        return Ok(unsafe { std::slice::from_raw_parts(direct, n) });
    }
    // SAFETY: as above; R's collector does not move objects, so the data stays put
    // while `x` is protected; it holds `n` CHARSXPs.
    unsafe {
        let p = rapi::protect_value(std::ptr::null(), move || rapi::STRING_PTR_RO(x))?;
        Ok(std::slice::from_raw_parts(p, n))
    }
}

/// The data of a protected double (`REALSXP`) or integer (`INTSXP`) vector `x` as a
/// slice valid while `x` is protected; an ALTREP vector is materialised inside
/// `protect`. `None` for any other type.
pub(crate) fn numbers_of<'a, T>(x: SEXP, ty: SexpType) -> savvy::Result<Option<&'a [T]>> {
    // SAFETY: main thread; `x` protected by the caller.
    let (t, n) = unsafe { (rapi::TYPEOF(x), rapi::Rf_xlength(x) as usize) };
    let size_ok = match ty {
        REALSXP => std::mem::size_of::<T>() == 8,
        INTSXP => std::mem::size_of::<T>() == 4,
        _ => false,
    };
    if t != ty || !size_ok {
        return Ok(None);
    }
    if n == 0 {
        return Ok(Some(&[]));
    }
    // SAFETY: main thread; `*_OR_NULL` neither allocate nor raise: they return the
    // data of a vector that has it in memory and NULL for an ALTREP one without.
    let direct = unsafe {
        match ty {
            REALSXP => rapi::REAL_OR_NULL(x) as *const u8,
            _ => rapi::INTEGER_OR_NULL(x) as *const u8,
        }
    };
    if !direct.is_null() {
        // SAFETY: as below.
        return Ok(Some(unsafe {
            std::slice::from_raw_parts(direct as *const T, n)
        }));
    }
    // SAFETY: as above; R's collector does not move objects, so the data stays put
    // while `x` is protected; it holds `n` elements of `T`'s size (checked above;
    // the callers use f64 for REALSXP and i32 for INTSXP).
    unsafe {
        let p = rapi::protect_value(std::ptr::null::<u8>(), move || match ty {
            REALSXP => rapi::REAL_RO(x) as *const u8,
            _ => rapi::INTEGER_RO(x) as *const u8,
        })?;
        Ok(Some(std::slice::from_raw_parts(p as *const T, n)))
    }
}

/// Copy `donor`'s attributes onto `to` (both protected by the caller).
fn put_attrs(to: SEXP, donor: Option<&H>) -> savvy::Result<()> {
    if let Some(d) = donor {
        let from = d.sexp;
        // SAFETY: main thread; `to` is owned (preserved) by the caller, `from` kept.
        unsafe { rapi::protect(move || rapi::SHALLOW_DUPLICATE_ATTRIB(to, from)) }?;
    }
    Ok(())
}

/// The string pool: one code per distinct `CHARSXP`, which R's global string cache
/// makes one per distinct string and encoding. The `CHARSXP`s stay alive in a
/// preserved character vector, so the addresses used as keys stay valid.
struct Pool {
    chars: Vec<SEXP>,
    index: HashMap<usize, u32>,
    keep: Option<OwnedStringSexp>,
    last: (SEXP, u32),
}

impl Pool {
    fn new() -> Self {
        Pool {
            chars: Vec::new(),
            index: HashMap::new(),
            keep: None,
            last: (std::ptr::null_mut(), NA_CODE),
        }
    }

    fn code(&mut self, ch: SEXP) -> savvy::Result<u32> {
        // SAFETY: reading a static of R's.
        if ch == unsafe { rapi::R_NaString } {
            return Ok(NA_CODE);
        }
        if ch == self.last.0 {
            return Ok(self.last.1);
        }
        let code = match self.index.get(&(ch as usize)) {
            Some(&c) => c,
            None => self.push(ch)?,
        };
        self.last = (ch, code);
        Ok(code)
    }

    fn push(&mut self, ch: SEXP) -> savvy::Result<u32> {
        let code = self.chars.len();
        if code >= NA_CODE as usize {
            return Err(typed_error("the string pool is full"));
        }
        let cap = self.keep.as_ref().map_or(0, |k| k.len());
        if code == cap {
            let bigger = OwnedStringSexp::new((2 * cap).max(64))?;
            for (i, &c) in self.chars.iter().enumerate() {
                // SAFETY: main thread; `bigger` is preserved; `c` is a live CHARSXP
                // (held by the old pool vector until `bigger` replaces it below).
                unsafe { rapi::SET_STRING_ELT(bigger.inner(), i as isize, c) };
            }
            self.keep = Some(bigger);
        }
        if let Some(k) = &self.keep {
            // SAFETY: as above; `code` < the vector's length.
            unsafe { rapi::SET_STRING_ELT(k.inner(), code as isize, ch) };
        }
        self.chars.push(ch);
        self.index.insert(ch as usize, code as u32);
        Ok(code as u32)
    }

    fn charsxp(&self, code: u32) -> SEXP {
        if code == NA_CODE {
            // SAFETY: reading a static of R's.
            unsafe { rapi::R_NaString }
        } else {
            self.chars[code as usize]
        }
    }

    /// A character vector of the strings with these codes.
    fn strings(&self, codes: impl ExactSizeIterator<Item = u32>) -> savvy::Result<OwnedStringSexp> {
        let out = OwnedStringSexp::new(codes.len())?;
        for (i, c) in codes.enumerate() {
            // SAFETY: main thread; `out` preserved; the CHARSXP is held by the pool.
            unsafe { rapi::SET_STRING_ELT(out.inner(), i as isize, self.charsxp(c)) };
        }
        Ok(out)
    }
}

/// Counters for the tests and the boundary-cost measurements.
#[derive(Default)]
struct Stats {
    cols_copied: i32,
    cols_reused: i32,
    groups_reused: i32,
    lists_reused: i32,
}

/// Bare zero-length vectors, to tell a column without attributes (see [`donor`]).
struct Empties {
    real: OwnedRealSexp,
    int: OwnedIntegerSexp,
    lgl: OwnedLogicalSexp,
    str: OwnedStringSexp,
}

impl Empties {
    fn of(&self, ty: SexpType) -> SEXP {
        match ty {
            REALSXP => self.real.inner(),
            INTSXP => self.int.inner(),
            LGLSXP => self.lgl.inner(),
            _ => self.str.inner(),
        }
    }
}

/// Pool codes of the names the stacking treats specially.
fn names_codes(pool: &mut Pool, special: &StringSexp) -> savvy::Result<cs::Names> {
    if special.len() != 4 {
        return Err(typed_error(
            "`special` must hold .id, .group, tick and \"\"",
        ));
    }
    let at = chars_of(special.inner())?;
    Ok(cs::Names {
        id: pool.code(at[0])?,
        group: pool.code(at[1])?,
        tick: pool.code(at[2])?,
        blank: pool.code(at[3])?,
    })
}

/// A run's agent groups in Rust (an external pointer in R; see R/run-rust.R).
/// @noRd
#[savvy]
pub struct AgentStore {
    core: cs::Store<H>,
    pool: Pool,
    names: cs::Names,
    empties: Empties,
    list_mirror: Option<H>,
    use_mirror: bool,
    stats: Stats,
}

impl AgentStore {
    fn import_col(
        &mut self,
        x: SEXP,
        name: u32,
        prev: Option<&Group<H>>,
    ) -> savvy::Result<Column<H>> {
        if let Some(c) = prev.and_then(|g| g.col(name)) {
            if c.mirror.as_ref().is_some_and(|m| m.sexp == x) {
                self.stats.cols_reused += 1;
                return Ok(c.clone());
            }
        }
        self.stats.cols_copied += 1;
        // SAFETY: main thread; `x` is protected (reachable from the argument).
        let ty = unsafe { rapi::TYPEOF(x) };
        if self.use_mirror || !matches!(ty, REALSXP | INTSXP | LGLSXP | STRSXP) {
            // opaque, or typed and kept lazily (copied in by `materialise`)
            let k = keep(x)?;
            return Ok(Column {
                name,
                data: ColData::Opaque(k.clone()),
                attrs: None,
                mirror: Some(k),
            });
        }
        let mirror = self.import_mirror(x)?;
        self.copy_col(x, name, mirror)
    }

    /// Typed R vector `x` copied into Rust values, with its attributes on a donor.
    fn copy_col(&mut self, x: SEXP, name: u32, mirror: Option<H>) -> savvy::Result<Column<H>> {
        // SAFETY: main thread; `x` is protected by the caller (argument or kept).
        let (ty, n) = unsafe { (rapi::TYPEOF(x), rapi::Rf_xlength(x) as usize) };
        // SAFETY: main thread; the data pointer of an ALTREP vector may be made by
        // allocating, hence `protect`. R's collector does not move objects and `x`
        // stays protected, so the pointer is valid for the rest of this call.
        let p = if ty == STRSXP || n == 0 {
            std::ptr::null()
        } else {
            unsafe {
                rapi::protect_value(std::ptr::null::<u8>(), move || match ty {
                    REALSXP => rapi::REAL_RO(x) as *const u8,
                    INTSXP => rapi::INTEGER_RO(x) as *const u8,
                    _ => rapi::LOGICAL_RO(x) as *const u8,
                })
            }?
        };
        // SAFETY (all three): `p` points at `n` elements of the type matching `ty`
        // (R's vector layout); empty vectors are not read through it.
        let values = match ty {
            _ if n == 0 && ty != STRSXP => match ty {
                REALSXP => Values::Real(Vec::new()),
                INTSXP => Values::Int(Vec::new()),
                _ => Values::Lgl(Vec::new()),
            },
            REALSXP => {
                Values::Real(unsafe { std::slice::from_raw_parts(p as *const f64, n) }.to_vec())
            }
            INTSXP => {
                Values::Int(unsafe { std::slice::from_raw_parts(p as *const i32, n) }.to_vec())
            }
            LGLSXP => {
                Values::Lgl(unsafe { std::slice::from_raw_parts(p as *const i32, n) }.to_vec())
            }
            _ => {
                let mut v = Vec::with_capacity(n);
                for &ch in chars_of(x)? {
                    v.push(self.pool.code(ch)?);
                }
                Values::Str(v)
            }
        };
        Ok(Column {
            name,
            data: ColData::Typed(Arc::new(values)),
            attrs: donor(x, ty, self.empties.of(ty))?,
            mirror,
        })
    }

    /// Copy every lazily kept typed column of the current groups into Rust and read
    /// every frame's attributes off its mirror, so a snapshot holds values (the
    /// stacking in `assemble` needs them) and no R object a later step replaces.
    fn materialise(&mut self) -> savvy::Result<()> {
        let mut groups = std::mem::take(&mut self.core.groups);
        let res = (|| {
            for g in groups.iter_mut() {
                self.ensure_frame(g)?;
                for c in g.cols.iter_mut() {
                    let ColData::Opaque(k) = &c.data else {
                        continue;
                    };
                    let x = k.sexp;
                    // SAFETY: main thread; a kept object.
                    if matches!(
                        unsafe { rapi::TYPEOF(x) },
                        REALSXP | INTSXP | LGLSXP | STRSXP
                    ) {
                        let mirror = c.mirror.take();
                        *c = self.copy_col(x, c.name, mirror)?;
                    }
                }
            }
            Ok(())
        })();
        self.core.groups = groups;
        res
    }

    /// The group's frame attributes, read off its mirror if they were not yet.
    fn ensure_frame(&self, g: &mut Group<H>) -> savvy::Result<()> {
        if g.frame.is_none() {
            if let Some(m) = &g.mirror {
                g.frame = donor(m.sexp, VECSXP, self.empties.of(STRSXP))?;
            }
        }
        Ok(())
    }

    fn import_group(
        &mut self,
        x: SEXP,
        name: u32,
        prev: Option<&Group<H>>,
    ) -> savvy::Result<Group<H>> {
        // SAFETY: main thread; `x` protected; `Rf_getAttrib(, R_NamesSymbol)` of a
        // list returns the attribute itself (no allocation).
        let (ty, ncol, cn) = unsafe {
            (
                rapi::TYPEOF(x),
                rapi::Rf_xlength(x),
                rapi::Rf_getAttrib(x, rapi::R_NamesSymbol),
            )
        };
        // SAFETY: as above.
        if ty != VECSXP || unsafe { rapi::TYPEOF(cn) } != STRSXP {
            return Err(typed_error(
                "every group must be a data frame with column names",
            ));
        }
        let cnames = chars_of(cn)?;
        if cnames.len() != ncol as usize {
            return Err(typed_error("every column of a group must have a name"));
        }
        let mut cols = Vec::with_capacity(ncol as usize);
        for (j, &cname) in cnames.iter().enumerate() {
            // SAFETY: `j` < length of the list `x`; VECTOR_ELT neither allocates nor
            // raises.
            let col = unsafe { rapi::VECTOR_ELT(x, j as isize) };
            let cname = self.pool.code(cname)?;
            cols.push(self.import_col(col, cname, prev)?);
        }
        // the row count from `.id` (always an integer column of the group), or from
        // the first column; R's row names would have to be expanded to be read
        let nrow = cols
            .iter()
            .find(|c| c.name == self.names.id)
            .or(cols.first())
            .map_or(0, |c| match &c.data {
                ColData::Typed(v) => v.len(),
                // SAFETY: main thread; a kept object.
                ColData::Opaque(k) => (unsafe { rapi::Rf_xlength(k.sexp) }) as usize,
            });
        Ok(Group {
            name,
            nrow,
            cols,
            // a list is never identical() to the empty character vector, so the
            // frame's attributes are always kept; under `use_mirror` they are read
            // off the mirror when it goes (`ensure_frame`)
            frame: if self.use_mirror {
                None
            } else {
                donor(x, VECSXP, self.empties.of(STRSXP))?
            },
            mirror: self.import_mirror(x)?,
        })
    }

    fn load_list(&mut self, list: SEXP) -> savvy::Result<()> {
        if self.list_mirror.as_ref().is_some_and(|m| m.sexp == list) {
            self.stats.lists_reused += 1;
            return Ok(());
        }
        // SAFETY: main thread; `list` is a `.Call` argument.
        let (ty, n, names) = unsafe {
            (
                rapi::TYPEOF(list),
                rapi::Rf_xlength(list),
                rapi::Rf_getAttrib(list, rapi::R_NamesSymbol),
            )
        };
        // SAFETY: as above.
        if ty != VECSXP || unsafe { rapi::TYPEOF(names) } != STRSXP {
            return Err(typed_error("the groups must be a named list"));
        }
        let gnames = chars_of(names)?;
        if gnames.len() != n as usize {
            return Err(typed_error("the groups must be a named list"));
        }
        let old = std::mem::take(&mut self.core.groups);
        let mut groups = Vec::with_capacity(n as usize);
        for (i, &gname) in gnames.iter().enumerate() {
            // SAFETY: `i` < length of the list; VECTOR_ELT neither allocates nor raises.
            let x = unsafe { rapi::VECTOR_ELT(list, i as isize) };
            let gname = self.pool.code(gname)?;
            let prev = old.iter().find(|g| g.name == gname);
            match prev {
                Some(p) if p.mirror.as_ref().is_some_and(|m| m.sexp == x) => {
                    self.stats.groups_reused += 1;
                    groups.push(p.clone());
                }
                _ => groups.push(self.import_group(x, gname, prev)?),
            }
        }
        self.core.groups = groups;
        self.list_mirror = self.import_mirror(list)?;
        Ok(())
    }

    /// The mirror of an object just loaded: the object itself under `use_mirror`.
    fn import_mirror(&self, x: SEXP) -> savvy::Result<Option<H>> {
        if self.use_mirror {
            keep(x).map(Some)
        } else {
            Ok(None)
        }
    }
}

/// One typed column as a new R vector, with its attributes, set as element `i` of
/// `parent`.
fn put_values(
    pool: &Pool,
    v: &Values,
    attrs: Option<&H>,
    parent: &mut OwnedListSexp,
    i: usize,
) -> savvy::Result<()> {
    match v {
        Values::Real(x) => {
            let o = OwnedRealSexp::try_from_slice(x.as_slice())?;
            put_attrs(o.inner(), attrs)?;
            parent.set_value(i, o)
        }
        Values::Int(x) => {
            let o = OwnedIntegerSexp::try_from_slice(x.as_slice())?;
            put_attrs(o.inner(), attrs)?;
            parent.set_value(i, o)
        }
        Values::Lgl(x) => {
            // SAFETY: every element is written just below.
            let o = unsafe { OwnedLogicalSexp::new_without_init(x.len())? };
            if !x.is_empty() {
                // SAFETY: main thread; `o` is a fresh, preserved, non-ALTREP logical
                // vector of `x.len()` elements.
                let dst =
                    unsafe { std::slice::from_raw_parts_mut(rapi::LOGICAL(o.inner()), x.len()) };
                dst.copy_from_slice(x);
            }
            put_attrs(o.inner(), attrs)?;
            parent.set_value(i, o)
        }
        Values::Str(x) => {
            let o = pool.strings(x.iter().copied())?;
            put_attrs(o.inner(), attrs)?;
            parent.set_value(i, o)
        }
    }
}

/// Keep element `i` of `parent` (just set) as a mirror.
fn keep_elt(parent: &OwnedListSexp, i: usize) -> savvy::Result<H> {
    // SAFETY: main thread; `i` < length; VECTOR_ELT neither allocates nor raises.
    keep(unsafe { rapi::VECTOR_ELT(parent.inner(), i as isize) })
}

/// Column `c` as element `i` of `parent`: its mirror, or a vector built from its
/// values, which becomes its mirror when `cache`.
fn put_col(
    pool: &Pool,
    c: &mut Column<H>,
    parent: &mut OwnedListSexp,
    i: usize,
    cache: bool,
) -> savvy::Result<()> {
    if let Some(m) = &c.mirror {
        return parent.set_value(i, Sexp(m.sexp));
    }
    match &c.data {
        ColData::Opaque(k) => parent.set_value(i, Sexp(k.sexp)),
        ColData::Typed(v) => {
            put_values(pool, v, c.attrs.as_ref(), parent, i)?;
            if cache {
                c.mirror = Some(keep_elt(parent, i)?);
            }
            Ok(())
        }
    }
}

/// Groups as a named list of tibbles (the run's `state$groups`), from the mirrors
/// where there are any; what is built is kept as mirrors when `cache`.
fn groups_out(
    pool: &Pool,
    groups: &mut [Group<H>],
    mirror: &mut Option<H>,
    cache: bool,
) -> savvy::Result<Sexp> {
    if let Some(m) = mirror {
        return Ok(Sexp(m.sexp));
    }
    let mut out = OwnedListSexp::new(groups.len(), false)?;
    for (i, g) in groups.iter_mut().enumerate() {
        if let Some(m) = &g.mirror {
            out.set_value(i, Sexp(m.sexp))?;
            continue;
        }
        let mut gl = OwnedListSexp::new(g.cols.len(), false)?;
        for (j, c) in g.cols.iter_mut().enumerate() {
            put_col(pool, c, &mut gl, j, cache)?;
        }
        // the frame's row names and class as loaded (no kernel changes a group's
        // rows yet), and the names of the columns it has now
        put_attrs(gl.inner(), g.frame.as_ref())?;
        let cn = pool.strings(g.cols.iter().map(|c| c.name))?;
        let (o, n) = (gl.inner(), cn.inner());
        // SAFETY: main thread; both preserved; setting names may allocate.
        unsafe {
            rapi::protect(move || {
                rapi::Rf_setAttrib(o, rapi::R_NamesSymbol, n);
            })
        }?;
        out.set_value(i, gl)?;
        if cache {
            g.mirror = Some(keep_elt(&out, i)?);
        }
    }
    let nm = pool.strings(groups.iter().map(|g| g.name))?;
    let (o, n) = (out.inner(), nm.inner());
    // SAFETY: main thread; both preserved; setting names may allocate.
    unsafe {
        rapi::protect(move || {
            rapi::Rf_setAttrib(o, rapi::R_NamesSymbol, n);
        })
    }?;
    if cache {
        *mirror = Some(keep(o)?);
    }
    Ok(out.into())
}

#[savvy]
impl AgentStore {
    /// A store holding `groups` (a named list of data frames). `special` is
    /// `c(".id", ".group", "tick", "")`; `use_mirror = FALSE`: what R gets back is
    /// always built from the Rust values (see the module notes).
    fn new(groups: ListSexp, special: StringSexp, use_mirror: bool) -> savvy::Result<Self> {
        run(|| {
            let mut pool = Pool::new();
            let names = names_codes(&mut pool, &special)?;
            let mut s = AgentStore {
                core: cs::Store::default(),
                pool,
                names,
                empties: Empties {
                    real: OwnedRealSexp::new(0)?,
                    int: OwnedIntegerSexp::new(0)?,
                    lgl: OwnedLogicalSexp::new(0)?,
                    str: OwnedStringSexp::new(0)?,
                },
                list_mirror: None,
                use_mirror,
                stats: Stats::default(),
            };
            s.load_list(groups.inner())?;
            Ok(s)
        })
    }

    /// Load the groups back after an R step: what R left untouched (same object)
    /// is kept as it is, anything else is copied in.
    fn load(&mut self, groups: ListSexp) -> savvy::Result<()> {
        run(|| self.load_list(groups.inner()))
    }

    /// The groups as R objects.
    fn export(&mut self) -> savvy::Result<Sexp> {
        run(|| {
            groups_out(
                &self.pool,
                &mut self.core.groups,
                &mut self.list_mirror,
                true,
            )
        })
    }

    /// Replace (or append) column `name` of group `group` with the R vector
    /// `values`, of the group's row count; the column's, the group's and the
    /// list's mirrors go, so the next `export()` builds them from the store.
    fn set_col(&mut self, group: StringSexp, name: StringSexp, values: Sexp) -> savvy::Result<()> {
        run(|| {
            if group.len() != 1 || name.len() != 1 {
                return Err(typed_error("`group` and `name` must be single strings"));
            }
            let g = self.pool.code(chars_of(group.inner())?[0])?;
            let nm = self.pool.code(chars_of(name.inner())?[0])?;
            let Some(gi) = self.core.groups.iter().position(|x| x.name == g) else {
                return Err(typed_error("no such group"));
            };
            let x = values.0;
            // SAFETY: main thread; `x` is an argument.
            let n = unsafe { rapi::Rf_xlength(x) } as usize;
            if n != self.core.groups[gi].nrow {
                return Err(typed_error("a column must have one value per row"));
            }
            let col = self.import_col(x, nm, None)?;
            let mut grp = std::mem::take(&mut self.core.groups[gi]);
            let res = self.ensure_frame(&mut grp);
            if res.is_ok() {
                grp.mirror = None;
                match grp.cols.iter().position(|c| c.name == nm) {
                    Some(j) => grp.cols[j] = col,
                    None => grp.cols.push(col),
                }
                self.list_mirror = None;
            }
            self.core.groups[gi] = grp;
            res
        })
    }

    /// Record the current groups as tick `tick`; `keep`: in the result.
    fn snapshot(&mut self, tick: i32, keep: bool) -> savvy::Result<()> {
        run(|| {
            if keep || (tick == 0 && self.core.first.is_none()) {
                self.materialise()?;
            }
            self.core.snapshot(tick, keep);
            Ok(())
        })
    }

    /// The recorded ticks stacked into the result's columns (`tick`, `.id`,
    /// `.group`, then the rest), or `NULL` where R's own stacking is needed.
    fn assemble(&self) -> savvy::Result<Sexp> {
        run(|| {
            // SAFETY: reading a static of R's (R's NA_real_, not a Rust constant)
            let na = unsafe { rapi::R_NaReal };
            let Some(cols) = cs::assemble(&self.core, self.names, na) else {
                return Ok(NullSexp.into());
            };
            let mut out = OwnedListSexp::new(cols.len(), false)?;
            for (i, (_, v)) in cols.iter().enumerate() {
                put_values(&self.pool, v, None, &mut out, i)?;
            }
            let nm = self.pool.strings(cols.iter().map(|c| c.0))?;
            let (o, n) = (out.inner(), nm.inner());
            // SAFETY: main thread; both preserved.
            unsafe {
                rapi::protect(move || {
                    rapi::Rf_setAttrib(o, rapi::R_NamesSymbol, n);
                })
            }?;
            Ok(out.into())
        })
    }

    /// The recorded ticks.
    fn ticks(&self) -> savvy::Result<Sexp> {
        run(|| {
            let t: Vec<i32> = self.core.snaps.iter().map(|s| s.tick).collect();
            OwnedIntegerSexp::try_from_slice(t).map(Into::into)
        })
    }

    /// The groups of recorded tick number `k` (from 0), or of tick 0 when `k` is -1.
    fn recorded_groups(&self, k: i32) -> savvy::Result<Sexp> {
        run(|| {
            let snap = if k < 0 {
                self.core.first.as_ref()
            } else {
                self.core.snaps.get(k as usize)
            };
            match snap {
                // a recorded tick has no mirrors and keeps none
                Some(s) => groups_out(&self.pool, &mut s.groups.clone(), &mut None, false),
                None => Err(typed_error(format!("no recorded tick number {k}"))),
            }
        })
    }

    /// Counters: columns copied in, columns / groups / lists recognised unchanged,
    /// strings in the pool, typed / opaque / attributed columns now held.
    fn stats(&self) -> savvy::Result<Sexp> {
        run(|| {
            let cols = self.core.groups.iter().flat_map(|g| g.cols.iter());
            let typed = cols
                .clone()
                .filter(|c| matches!(c.data, ColData::Typed(_)))
                .count();
            let opaque = cols
                .clone()
                .filter(|c| matches!(c.data, ColData::Opaque(_)))
                .count();
            let attrs = cols.filter(|c| c.attrs.is_some()).count();
            let s = &self.stats;
            let fields = [
                ("cols_copied", s.cols_copied),
                ("cols_reused", s.cols_reused),
                ("groups_reused", s.groups_reused),
                ("lists_reused", s.lists_reused),
                ("pool", self.pool.chars.len() as i32),
                ("typed", typed as i32),
                ("opaque", opaque as i32),
                ("attrs", attrs as i32),
                ("recorded", self.core.snaps.len() as i32),
            ];
            let mut out = OwnedIntegerSexp::try_from_slice(fields.map(|f| f.1))?;
            out.set_names(fields.map(|f| f.0))?;
            Ok(out.into())
        })
    }
}
