use super::match_step::shuffle;
use super::*;

/// A column of a sequential step's working store: loaded once per step, written
/// in place agent by agent, and written back when `dirty`.
struct SeqCol {
    name: SEXP,
    vals: Vals<'static>,
    dirty: bool,
}

/// A relation's working store inside `exec_seq()` (FR15b).
struct SeqRelStore {
    rel_name: SEXP,
    rel_i: usize,
    edges_i: usize,
    nrows: usize,
    map: std::collections::HashMap<(i32, i32), usize>,
    cols: Vec<SeqCol>,
}

/// Where a sequential rule writes ([run_sequential()]'s order of cases): a
/// relation value on `(me, partner)`, the partner's row (a standing match), a
/// global, else a column of the agent's own group (an index into its store).
#[derive(Clone, Copy)]
enum SeqTarget {
    Rel(usize, usize, bool),
    Partner(usize),
    Global(usize),
    Own(usize),
}

/// A match, cell or relation column a sequential rule reads, per agent.
#[derive(Clone, Copy)]
enum SeqMatchIn {
    Partner,
    Role,
    GroupId,
    Cost,
    /// `partner_<col>`: an index into the partner columns
    Col(usize),
    /// `cell_<col>`: `(wired_col_idx, agent_cell_col_idx_in_store)`
    Cell(usize, usize),
    /// `.R` / `.R_back`: `(rstore_idx, back)`
    RelExist(usize, bool),
    /// `R_<col>` / `R_<col>_back`: `(rstore_idx, col_idx_opt, back)`
    RelCol(usize, Option<usize>, bool),
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

/// The standing match's columns `.id`, `.partner`, `.role`, `.group_id`, optional
/// `.cost` and its `size`: `Ok(None)` without one, `Ok(Some(None))` for one the step cannot read.
#[allow(clippy::type_complexity)]
fn seq_match<'a>(
    st: SEXP,
) -> savvy::Result<
    Option<
        Option<(
            &'a [i32],
            &'a [i32],
            &'a [SEXP],
            &'a [i32],
            Option<&'a [f64]>,
            i32,
        )>,
    >,
> {
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
    let msz = match find_str(ms, b"size")? {
        Some(si) => {
            let s = elt(ms, si);
            store::numbers_of::<i32>(s, INTSXP)?
                .and_then(|v| v.first().copied())
                .or_else(|| {
                    store::numbers_of::<f64>(s, rapi::REALSXP)
                        .ok()
                        .flatten()
                        .and_then(|v| v.first().copied())
                        .map(|x| x as i32)
                })
                .unwrap_or(0)
        }
        None => 0,
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
    let cost: Option<&[f64]> = match col(b".cost")? {
        Some(c) => store::numbers_of::<f64>(c, rapi::REALSXP)?,
        None => None,
    };
    if pt.len() != id.len()
        || role.len() != id.len()
        || gid.len() != id.len()
        || cost.is_some_and(|c| c.len() != id.len())
    {
        return Ok(Some(None));
    }
    Ok(Some(Some((id, pt, role, gid, cost, msz))))
}

/// `abm_sequential()` natively ([run_sequential()]): the agents in a shuffled
/// order (or by `.order`), each evaluating its group's rules on its own row, a
/// later rule reading what an earlier one wrote, and the next agent reading the
/// columns, globals and relations as the agents before it left them. Every write
/// is checked as [write_at()] makes it; anything else declines and the step is
/// replayed on the R path.
pub(super) fn exec_seq(plan: &Plan, st: SEXP) -> savvy::Result<Option<H>> {
    use std::borrow::Cow;
    use std::collections::HashMap;
    use tidyabm_core::sample::NA_INT;
    let seq_plan = plan.seq.as_ref().ok_or_else(|| bad("no seq"))?;
    let has_order = seq_plan.order;
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
    let mut when_qs: Vec<(usize, usize)> = Vec::new();
    let mut key_q = None;
    for (q, gp) in plan.groups.iter().enumerate() {
        if gp.part == 4 {
            key_q = Some(q);
            continue;
        }
        let Some(j) = find(gnames, gp.name) else {
            return decline("a group not found");
        };
        if gp.part == 5 {
            when_qs.push((j, q));
            continue;
        }
        gplan[j] = Some(q);
    }
    // the standing match narrows the step to the agents it placed unless `.scope = "population"`
    let mt = if seq_plan.scope_pop {
        None
    } else {
        match seq_match(st)? {
            None => None,
            Some(Some(m)) => Some(m),
            Some(None) => return decline("a match the step cannot read"),
        }
    };
    let has_rel_target = !seq_plan.rels.is_empty()
        && plan.groups.iter().filter(|g| g.part != 5).any(|g| {
            g.targets
                .iter()
                .any(|&t| seq_plan.rels.iter().any(|r| r.name == t))
        });
    let uses_match = has_rel_target
        || plan.groups.iter().filter(|g| g.part != 5).any(|g| {
            g.mcols.iter().any(|&c| !bytes(c).starts_with(b"cell_"))
                || g.targets.iter().any(|&t| bytes(t).starts_with(b"partner_"))
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
    if has_rel_target && !matches!(mt, Some((_, _, _, _, _, 2))) {
        return decline("relation write without a pairing match (R's error)");
    }
    let Some(svals) = scope_values(plan)? else {
        return decline("a formula-environment value that cannot be read");
    };
    if seq_plan.has_when {
        let mut keep_ids: std::collections::HashSet<i32> = std::collections::HashSet::new();
        for &(j, q) in &when_qs {
            if sizes[j] == 0 {
                continue;
            }
            let g = elt(groups, j);
            let wgp = &plan.groups[q];
            let Some(wins) = entry_inputs(st, groups, g, wgp, &svals, None)? else {
                return decline("seq when inputs that cannot be built natively");
            };
            // SAFETY: the program is kept alive by the plan's descriptor.
            let wprog = unsafe { &*wgp.prog };
            let wres = match wprog.compute(wins, sizes[j], sizes[j])? {
                Ok(r) => r,
                Err(d) => {
                    expr::note_decline(&d);
                    return Ok(None);
                }
            };
            let Some(wmask) = condition(&wres[0], sizes[j]) else {
                return decline("a seq when condition that is not logical");
            };
            for (row_i, &ok) in wmask.iter().enumerate() {
                if ok {
                    keep_ids.insert(all_ids[starts[j] + row_i]);
                }
            }
        }
        order_ids.retain(|id| keep_ids.contains(id));
        if order_ids.is_empty() {
            return Ok(Some(keep(st)?));
        }
    }
    // relation stores (FR15b): loaded once per referenced relation
    let mut rel_root: Option<(usize, SEXP)> = None;
    let mut rstore: Vec<SeqRelStore> = Vec::new();
    if !seq_plan.rels.is_empty() {
        let Some(ri) = find_str(st, b"relations")? else {
            return decline("no relations");
        };
        let rels_sexp = elt(st, ri);
        // SAFETY: reading a type tag.
        if rels_sexp == rapi_nil() || unsafe { rapi::TYPEOF(rels_sexp) } != VECSXP {
            return decline("relations that are not a list");
        }
        rel_root = Some((ri, rels_sexp));
        let rnames = names_of(rels_sexp)?;
        for sr in &seq_plan.rels {
            if rstore.iter().any(|rs| rs.rel_name == sr.rel) {
                continue;
            }
            let Some(rel_i) = find(rnames, sr.rel) else {
                return decline("a relation not found");
            };
            let rel_obj = elt(rels_sexp, rel_i);
            let Some(edges_i) = find_str(rel_obj, b"edges")? else {
                return decline("a relation without edges");
            };
            let edges = elt(rel_obj, edges_i);
            let Some(nrows) = rows(edges) else {
                return decline("malformed relation edges");
            };
            let (Some(fi), Some(ti)) = (find_str(edges, b"from")?, find_str(edges, b"to")?) else {
                return decline("relation edges without from/to");
            };
            let (cf, ct) = (elt(edges, fi), elt(edges, ti));
            if !all_bare(&[cf, ct])? {
                return decline("relation edges with attributes");
            }
            let (Some(ef), Some(et)) = (
                store::numbers_of::<i32>(cf, INTSXP)?,
                store::numbers_of::<i32>(ct, INTSXP)?,
            ) else {
                return decline("relation edges that are not integers");
            };
            if ef.len() != nrows || et.len() != nrows {
                return decline("malformed relation edges");
            }
            let mut map: HashMap<(i32, i32), usize> = HashMap::with_capacity(nrows);
            for r in 0..nrows {
                map.entry((ef[r], et[r])).or_insert(r);
            }
            rstore.push(SeqRelStore {
                rel_name: sr.rel,
                rel_i,
                edges_i,
                nrows,
                map,
                cols: Vec::new(),
            });
        }
    }
    let load_rel_col = |rstore: &mut [SeqRelStore],
                        rs_i: usize,
                        col_nm: SEXP|
     -> savvy::Result<Result<Option<usize>, ()>> {
        let cname = bytes(col_nm);
        if let Some(ci) = rstore[rs_i]
            .cols
            .iter()
            .position(|c| bytes(c.name) == cname)
        {
            return Ok(Ok(Some(ci)));
        }
        let (_, rels_sexp) = rel_root.ok_or_else(|| bad("no rel root"))?;
        let rel_obj = elt(rels_sexp, rstore[rs_i].rel_i);
        let edges = elt(rel_obj, rstore[rs_i].edges_i);
        let enames = names_of(edges)?;
        let Some(ej) = enames.iter().position(|&c| same_bytes(c, cname)) else {
            return Ok(Ok(None));
        };
        let col = elt(edges, ej);
        if !all_bare(&[col])? {
            return Ok(Err(()));
        }
        let Some(v) = expr::vals_of(col, false)? else {
            return Ok(Err(()));
        };
        rstore[rs_i].cols.push(SeqCol {
            name: enames[ej],
            vals: expr::owned(v),
            dirty: false,
        });
        Ok(Ok(Some(rstore[rs_i].cols.len() - 1)))
    };
    // the stores: columns, globals, partner columns, start-of-step wired cell columns
    let mut store: Vec<Vec<SeqCol>> = (0..ng).map(|_| Vec::new()).collect();
    let gl_names = names_of(gl)?;
    let mut gstore: Vec<(SEXP, Vals)> = Vec::new();
    let mut gwritten: Vec<bool> = Vec::new();
    let mut pcols: Vec<(Vec<u8>, Vec<Option<usize>>)> = Vec::new();
    let mut wired_snap: Vec<(Vec<u8>, Vals<'static>)> = Vec::new();
    let mut wired_meta: Option<(usize, Option<IdIndex>, SEXP)> = None;
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
            if bytes(name) == b".tick" {
                if let Some(x) = gstore.iter().position(|(nm, _)| bytes(*nm) == b".tick") {
                    return Ok(Some(x));
                }
                let Some(ti) = find_str(st, b"tick")? else {
                    return Ok(None);
                };
                let x = elt(st, ti);
                if !all_bare(&[x])? {
                    return Ok(None);
                }
                return match expr::vals_of(x, false)? {
                    Some(v) => {
                        gstore.push((name, v));
                        Ok(Some(gstore.len() - 1))
                    }
                    None => Ok(None),
                };
            }
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
            if let Some(sr) = seq_plan.rels.iter().find(|r| r.name == c) {
                let rs_i = rstore
                    .iter()
                    .position(|rs| rs.rel_name == sr.rel)
                    .ok_or_else(|| bad("seq rel store"))?;
                if bytes(sr.col).is_empty() {
                    mcols.push(SeqMatchIn::RelExist(rs_i, sr.back));
                } else {
                    match load_rel_col(&mut rstore, rs_i, sr.col)? {
                        Ok(ci) => mcols.push(SeqMatchIn::RelCol(rs_i, ci, sr.back)),
                        Err(()) => return decline("a relation column of another kind"),
                    }
                }
                continue;
            }
            mcols.push(match bytes(c) {
                b".partner" => SeqMatchIn::Partner,
                b".role" => SeqMatchIn::Role,
                b".group_id" => SeqMatchIn::GroupId,
                b".cost" => SeqMatchIn::Cost,
                b => {
                    if let Some(x) = b.strip_prefix(b"partner_") {
                        match pcol(x, &mut store)? {
                            Some(p) => SeqMatchIn::Col(p),
                            None => return decline("a partner column of another kind"),
                        }
                    } else if let Some(x) = b.strip_prefix(b"cell_") {
                        let Ok(Some(cell_ci)) = seq_col(groups, &mut store, j, b".cell")? else {
                            return decline("a group without .cell for cell_ column");
                        };
                        if wired_meta.is_none() {
                            let Some(lati) = find_str(st, b"lattice")? else {
                                return decline("no lattice for cell_ column");
                            };
                            let lat = elt(st, lati);
                            let Some(oni) = find_str(lat, b"on")? else {
                                return decline("no wired group for cell_ column");
                            };
                            let on_chars = chars(elt(lat, oni))?;
                            let Some(&on_nm) = on_chars.first() else {
                                return decline("no wired group for cell_ column");
                            };
                            let Some(wi) = find(gnames, on_nm) else {
                                return decline("wired group not found");
                            };
                            let wg = elt(groups, wi);
                            let Some(wn) = rows(wg) else {
                                return decline("wired group has no rows");
                            };
                            let Some(widi) = find_str(wg, b".id")? else {
                                return decline("wired group without .id");
                            };
                            let Some(wids) = store::numbers_of::<i32>(elt(wg, widi), INTSXP)?
                            else {
                                return decline("wired ids not integer");
                            };
                            let dense = wids.len() == wn
                                && wids.iter().enumerate().all(|(i, &id)| id == (i as i32 + 1));
                            let widx = if dense {
                                None
                            } else {
                                Some(IdIndex::new(wids))
                            };
                            wired_meta = Some((wn, widx, wg));
                        }
                        let (_, _, wg) = wired_meta.as_ref().ok_or_else(|| bad("wired meta"))?;
                        let wci = if let Some(pos) = wired_snap.iter().position(|(nm, _)| nm == x) {
                            pos
                        } else {
                            let Some(ci) = find_str(*wg, x)? else {
                                return decline("wired column not found");
                            };
                            let csexp = elt(*wg, ci);
                            if !all_bare(&[csexp])? {
                                return decline("wired column with attributes");
                            }
                            let Some(v) = expr::vals_of(csexp, false)? else {
                                return decline("wired column of unsupported type");
                            };
                            wired_snap.push((x.to_vec(), expr::owned(v)));
                            wired_snap.len() - 1
                        };
                        SeqMatchIn::Cell(wci, cell_ci)
                    } else {
                        return decline("a match column the step cannot read");
                    }
                }
            });
        }
        let mut targets = Vec::with_capacity(gp.targets.len());
        for &t in &gp.targets {
            if let Some(sr) = seq_plan.rels.iter().find(|r| r.name == t) {
                if bytes(sr.col).is_empty() {
                    return decline("cannot write relation existence (R's error)");
                }
                let rs_i = rstore
                    .iter()
                    .position(|rs| rs.rel_name == sr.rel)
                    .ok_or_else(|| bad("seq rel store"))?;
                match load_rel_col(&mut rstore, rs_i, sr.col)? {
                    Ok(Some(ci)) => targets.push(SeqTarget::Rel(rs_i, ci, sr.back)),
                    _ => return decline("a relation column that is new or of another kind"),
                }
                continue;
            }
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
        let mut rpending: Vec<(usize, usize, usize, Vals<'static>)> = Vec::new();
        let mut gpending: Vec<(usize, Vals<'static>)> = Vec::new();
        for &id in &order_ids {
            let pos = at_id.get(id).ok_or_else(|| bad("an agent not found"))?;
            let g = group_of[pos];
            let i = pos - starts[g];
            let Some(sg) = &seqs[g] else { continue };
            let (mut pid, mut role, mut gid, mut cost) =
                (NA_INT, na_str, NA_INT, unsafe { rapi::R_NaReal });
            if let (Some((_, mpt, mrole, mgid, mcost, _)), Some(ix)) = (mt, &midx) {
                let r = ix.get(id).ok_or_else(|| bad("an agent not in the match"))?;
                pid = mpt[r];
                role = mrole[r] as usize;
                gid = mgid[r];
                if let Some(mc) = mcost {
                    cost = mc[r];
                }
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
                        SeqMatchIn::Cost => Vals::Dbl(Cow::Owned(vec![cost])),
                        SeqMatchIn::Col(p) => match ploc.and_then(|(pg, pj)| {
                            pcols[p].1[pg].map(|ci| seq_at(&store[pg][ci].vals, pj))
                        }) {
                            Some(v) => v,
                            // no partner, or one whose group lacks the column
                            None => Vals::Lgl(Cow::Owned(vec![NA_INT])),
                        },
                        SeqMatchIn::Cell(wci, cell_ci) => {
                            let c = match &store[g][cell_ci].vals {
                                Vals::Int(v) => v[i],
                                _ => NA_INT,
                            };
                            let wrow = if c == NA_INT {
                                None
                            } else if let Some((wn, ref widx, _)) = wired_meta {
                                if let Some(ref ix) = widx {
                                    ix.get(c)
                                } else if c >= 1 && (c as usize) <= wn {
                                    Some(c as usize - 1)
                                } else {
                                    None
                                }
                            } else {
                                None
                            };
                            match wrow {
                                Some(wr) => seq_at(&wired_snap[wci].1, wr),
                                None => match &wired_snap[wci].1 {
                                    Vals::Dbl(_) => {
                                        // SAFETY: reading a static of R's.
                                        Vals::Dbl(Cow::Owned(vec![unsafe { rapi::R_NaReal }]))
                                    }
                                    Vals::Int(_) => Vals::Int(Cow::Owned(vec![NA_INT])),
                                    Vals::Lgl(_) => Vals::Lgl(Cow::Owned(vec![NA_INT])),
                                    Vals::Str(_) => Vals::Str(Cow::Owned(vec![na_str])),
                                },
                            }
                        }
                        SeqMatchIn::RelExist(rs_i, back) => {
                            let found = pid != NA_INT
                                && rstore[rs_i].map.contains_key(&if back {
                                    (pid, id)
                                } else {
                                    (id, pid)
                                });
                            Vals::Lgl(Cow::Owned(vec![i32::from(found)]))
                        }
                        SeqMatchIn::RelCol(rs_i, ci_opt, back) => {
                            let key = if back { (pid, id) } else { (id, pid) };
                            match (pid != NA_INT)
                                .then(|| rstore[rs_i].map.get(&key).copied())
                                .flatten()
                                .zip(ci_opt)
                            {
                                Some((erow, ci)) => seq_at(&rstore[rs_i].cols[ci].vals, erow),
                                None => Vals::Lgl(Cow::Owned(vec![NA_INT])),
                            }
                        }
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
                        SeqTarget::Rel(rs_i, ci, back) => {
                            if pid == NA_INT {
                                continue;
                            }
                            let key = if back { (pid, id) } else { (id, pid) };
                            let Some(&erow) = rstore[rs_i].map.get(&key) else {
                                return Ok(Err(Decline("not related (R's error)")));
                            };
                            let Some(c) = seq_cast(&rstore[rs_i].cols[ci].vals, &raw) else {
                                return Ok(Err(Decline(
                                    "a write that changes a relation column's type",
                                )));
                            };
                            rpending.push((rs_i, ci, erow, c));
                        }
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
            for (rs_i, ci, row, c) in rpending.drain(..) {
                let col = &mut rstore[rs_i].cols[ci];
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
    // write back: the columns written, the globals, then any dirty relation columns
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
    if let Some((ri, rels_sexp)) = rel_root {
        if rstore.iter().any(|r| r.cols.iter().any(|c| c.dirty)) {
            let mut rcur: Option<H> = None;
            for rs in &rstore {
                if !rs.cols.iter().any(|c| c.dirty) {
                    continue;
                }
                let cur_rels = rcur.as_ref().map_or(rels_sexp, |h| h.get());
                let rel_obj = elt(cur_rels, rs.rel_i);
                let edges0 = elt(rel_obj, rs.edges_i);
                let mut e2: Option<H> = None;
                for c in rs.cols.iter().filter(|c| c.dirty) {
                    let base = e2.as_ref().map_or(edges0, |h| h.get());
                    let mut hold = OwnedListSexp::new(1, false)?;
                    hold.set_value(0, expr::to_r(&c.vals, rs.nrows)?)?;
                    e2 = Some(set_named(base, c.name, elt(hold.inner(), 0))?);
                }
                if let Some(e2) = e2 {
                    let rel2 = with_elt(rel_obj, rs.edges_i, e2.get(), blank)?;
                    rcur = Some(with_elt(cur_rels, rs.rel_i, rel2.get(), blank)?);
                }
            }
            if let Some(rcur) = rcur {
                let base = st_cur.as_ref().map_or(st, |h| h.get());
                st_cur = Some(with_elt(base, ri, rcur.get(), blank)?);
            }
        }
    }
    match st_cur {
        Some(h) => Ok(Some(h)),
        None => Ok(Some(keep(st)?)),
    }
}
