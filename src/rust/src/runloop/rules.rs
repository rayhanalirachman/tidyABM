use super::*;

/// Write one relation target column from `exec_rules()` into `rels_s`, matching
/// `write_relation_rule()` + `write_relation()`. Returns `Ok(None)` to decline.
#[allow(clippy::too_many_arguments)]
fn write_rel_rule_col(
    st: SEXP,
    rels_s: SEXP,
    g: SEXP,
    sr: &SeqRel,
    val: &Vals,
    mask: Option<&[bool]>,
    spread_rows: Option<&[usize]>,
) -> savvy::Result<Option<H>> {
    use std::borrow::Cow;
    use tidyabm_core::sample::NA_INT;
    if bytes(sr.col).is_empty() || rels_s == rapi_nil() {
        return Ok(None);
    }
    let Some(mi) = find_str(st, b"match")? else {
        return Ok(None);
    };
    let ms = elt(st, mi);
    if ms == rapi_nil() {
        return Ok(None);
    }
    let (Some(mti), Some(msi)) = (find_str(ms, b"match")?, find_str(ms, b"size")?) else {
        return Ok(None);
    };
    let size = match store::numbers_of::<i32>(elt(ms, msi), INTSXP)? {
        Some(v) if v.len() == 1 => v[0],
        _ => match store::numbers_of::<f64>(elt(ms, msi), rapi::REALSXP)? {
            Some(v) if v.len() == 1 => v[0] as i32,
            _ => return Ok(None),
        },
    };
    if size != 2 {
        return Ok(None);
    }
    let m = elt(ms, mti);
    let (Some(midi), Some(mpi)) = (find_str(m, b".id")?, find_str(m, b".partner")?) else {
        return Ok(None);
    };
    let (Some(mid), Some(mpart)) = (
        store::numbers_of::<i32>(elt(m, midi), INTSXP)?,
        store::numbers_of::<i32>(elt(m, mpi), INTSXP)?,
    ) else {
        return Ok(None);
    };
    let Some(gidi) = find_str(g, b".id")? else {
        return Ok(None);
    };
    let Some(gids) = store::numbers_of::<i32>(elt(g, gidi), INTSXP)? else {
        return Ok(None);
    };
    let first = IdIndex::new(mid);
    let mut writes: Vec<(i32, i32, usize)> = Vec::new();
    for (i, &id) in gids.iter().enumerate() {
        if let Some(mk) = mask {
            if !mk[i] {
                continue;
            }
        }
        let pt = first.get(id).map_or(NA_INT, |r| mpart[r]);
        if pt == NA_INT {
            continue;
        }
        let (fr, to) = if sr.back { (pt, id) } else { (id, pt) };
        let vi = if val.len() == 1 {
            0
        } else if let Some(srows) = spread_rows {
            srows[i]
        } else {
            i
        };
        writes.push((fr, to, vi));
    }
    if writes.is_empty() {
        return Ok(Some(keep(rels_s)?));
    }
    let Some(rk) = find(names_of(rels_s)?, sr.rel) else {
        return Ok(None);
    };
    let rel = elt(rels_s, rk);
    let Some(ei) = find_str(rel, b"edges")? else {
        return Ok(None);
    };
    let edges = elt(rel, ei);
    let (Some(fi_c), Some(ti_c), Some(ci)) = (
        find_str(edges, b"from")?,
        find_str(edges, b"to")?,
        find(names_of(edges)?, sr.col),
    ) else {
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
    let mut erows = Vec::with_capacity(writes.len());
    for &(fr, to, _) in &writes {
        let Some(&er) = rmap.get(&(fr, to)) else {
            return Ok(None);
        };
        erows.push(er);
    }
    let csexp = elt(edges, ci);
    if !all_bare(&[csexp])? {
        return Ok(None);
    }
    let Some(old) = expr::vals_of(csexp, false)? else {
        return Ok(None);
    };
    // Promote `old` and `val` to their vctrs common type, then apply writes in order
    // SAFETY: reading statics of R's.
    let (na_real, na_str) = unsafe { (rapi::R_NaReal, rapi::R_NaString as usize) };
    let promote_dbl = |v: &Vals| -> Option<Vec<f64>> { vals_as_f64(v, na_real) };
    let promote_int = |v: &Vals| -> Option<Vec<i32>> {
        match v {
            Vals::Int(x) | Vals::Lgl(x) => Some(x.to_vec()),
            _ => None,
        }
    };
    let new_col: Vals<'static> = match (&old, val) {
        (Vals::Dbl(_), _) | (_, Vals::Dbl(_)) => {
            let (Some(mut a), Some(b)) = (promote_dbl(&old), promote_dbl(val)) else {
                return Ok(None);
            };
            for (er, &(_, _, vi)) in erows.iter().zip(&writes) {
                a[*er] = b[vi];
            }
            Vals::Dbl(Cow::Owned(a))
        }
        (Vals::Int(_), Vals::Int(_) | Vals::Lgl(_)) | (Vals::Lgl(_), Vals::Int(_)) => {
            let (Some(mut a), Some(b)) = (promote_int(&old), promote_int(val)) else {
                return Ok(None);
            };
            for (er, &(_, _, vi)) in erows.iter().zip(&writes) {
                a[*er] = b[vi];
            }
            Vals::Int(Cow::Owned(a))
        }
        (Vals::Lgl(x), Vals::Lgl(b)) => {
            let mut a = x.to_vec();
            for (er, &(_, _, vi)) in erows.iter().zip(&writes) {
                a[*er] = b[vi];
            }
            Vals::Lgl(Cow::Owned(a))
        }
        (Vals::Str(x), Vals::Str(b)) => {
            let mut a = x.to_vec();
            for (er, &(_, _, vi)) in erows.iter().zip(&writes) {
                a[*er] = b[vi];
            }
            Vals::Str(Cow::Owned(a))
        }
        (Vals::Str(x), Vals::Lgl(b)) if b.iter().all(|&e| e == NA_INT) => {
            let mut a = x.to_vec();
            for &er in &erows {
                a[er] = na_str;
            }
            Vals::Str(Cow::Owned(a))
        }
        _ => return Ok(None),
    };
    let mut hold = OwnedListSexp::new(1, false)?;
    hold.set_value(0, expr::to_r(&new_col, ef.len())?)?;
    // SAFETY: reading statics of R's (indices are in range).
    let blank = unsafe { rapi::R_BlankString };
    let e2 = with_elt(edges, ci, elt(hold.inner(), 0), blank)?;
    let r2 = with_elt(rel, ei, e2.get(), blank)?;
    Ok(Some(with_elt(rels_s, rk, r2.get(), blank)?))
}

/// `abm_rules()` natively (the shapes of slice 1's `rk_rules()`): every group's
/// rules from the start-of-step columns, then the writes. `None` = declined.
pub(super) fn exec_rules(plan: &Plan, st: SEXP) -> savvy::Result<Option<H>> {
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
                if rows(mm).unwrap_or(0) == 0 {
                    // Empty standing match with match scope writes nobody.
                    return Ok(Some(keep(st)?));
                }
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
    let mut results: Vec<(usize, OwnedListSexp, Vec<SEXP>)> = Vec::new();
    let mut cur_rels: Option<H> = None;
    let mut applied = false;
    // a step mixing R and Rust rules: an R rule has run / the rest is R's
    let (mut r_ran, mut r_mode) = (false, false);
    for gp in &plan.groups {
        if gp.part == 5 {
            continue;
        }
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
        // the rows placed by a standing match (if any)
        let placed_mask: Option<Vec<bool>> = match &placed {
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
        // optional row-level `when =`: evaluate over the full group, intersect
        // with `placed_mask`, skip if empty, narrow inputs when partial
        let when_gp = plan
            .groups
            .iter()
            .find(|w| w.part == 5 && w.name == gp.name);
        let narrowed: Option<(Vec<bool>, Vec<usize>)> = if let Some(wgp) = when_gp {
            let wins = if !plan.rules_rels.is_empty() && !wgp.mcols.is_empty() {
                let cn_w = names_of(g)?;
                let mut wxs = Vec::with_capacity(wgp.cols.len() + wgp.globs.len());
                for &c in &wgp.cols {
                    let Some(j) = find(cn_w, c) else {
                        return decline("a when column not found");
                    };
                    wxs.push(elt(g, j));
                }
                if !read_globals(st, &wgp.globs, &mut wxs)? {
                    return decline("a when global not found");
                }
                if !read_scope(&svals, &wgp.scope, &mut wxs) {
                    return decline("a when scope value not found");
                }
                let Some(mut ins_w) = inputs(&wxs)? else {
                    return decline("when input not a bare vector");
                };
                let mnames: Vec<&[u8]> = wgp.mcols.iter().map(|&c| bytes(c)).collect();
                match match_inputs_rel(
                    st,
                    groups,
                    g,
                    &mnames,
                    &plan.rules_rels,
                    cur_rels.as_ref().map(|h| h.get()),
                )? {
                    Some(v) => ins_w.extend(v),
                    None => return decline("when relation inputs that cannot be built natively"),
                }
                ins_w
            } else {
                let Some(wins) = entry_inputs(st, groups, g, wgp, &svals, None)? else {
                    return decline("when inputs that cannot be built natively");
                };
                wins
            };
            // SAFETY: the program is kept alive by the plan's descriptor.
            let wprog = unsafe { &*wgp.prog };
            let wres = match wprog.compute(wins, n, n)? {
                Ok(r) => r,
                Err(d) => {
                    expr::note_decline(&d);
                    return Ok(None);
                }
            };
            let Some(wmask) = condition(&wres[0], n) else {
                return decline("a when condition that is not logical");
            };
            let comb: Vec<bool> = match &placed_mask {
                Some(pm) => pm.iter().zip(&wmask).map(|(&a, &b)| a && b).collect(),
                None => wmask,
            };
            let active: Vec<usize> = (0..n).filter(|&i| comb[i]).collect();
            if active.is_empty() {
                continue;
            }
            if active.len() < n {
                Some((comb, active))
            } else {
                None
            }
        } else {
            None
        };
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
            match match_inputs_rel(
                st,
                groups,
                g,
                &mnames,
                &plan.rules_rels,
                cur_rels.as_ref().map(|h| h.get()),
            )? {
                Some(v) => ins.extend(v),
                None => return decline("match columns that cannot be built natively"),
            }
        }
        let n_eval = if let Some((_, ref active)) = narrowed {
            if plan.mixed.is_some() {
                return decline("a mixed step with when");
            }
            let (nc, nm, ni) = (gp.cols.len(), gp.mcols.len(), ins.len());
            ins = ins
                .into_iter()
                .enumerate()
                .map(|(j, v)| {
                    if j < nc || j >= ni - nm {
                        tidyabm_core::expr::take(&v, active)
                    } else {
                        v
                    }
                })
                .collect();
            active.len()
        } else {
            n
        };
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
            let Some(mut gid) =
                match_inputs(st, groups, g, &[b".group_id"])?.and_then(|mut v| v.pop())
            else {
                return decline("no .group_id for a grouped rule");
            };
            if let Some((_, ref active)) = narrowed {
                gid = tidyabm_core::expr::take(&gid, active);
            }
            let Some(loc) = tidyabm_core::expr::group_rows(&gid) else {
                return decline("a .group_id of another kind");
            };
            let (nc, nm, ni) = (gp.cols.len(), gp.mcols.len(), ins.len());
            let mut vals = Vec::with_capacity(gp.order.len());
            for (pos, &o) in gp.order.iter().enumerate() {
                let k = o as usize;
                if gp.rew[pos] {
                    match prog.compute_one(k, &ins, n_eval)? {
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
                match expr::with_host(|h| tidyabm_core::expr::unchop(parts, &loc, n_eval, h)) {
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
                            match prog.compute_one(k, &ins, n_eval)? {
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
                _ => match prog.compute(ins, n_eval, n_eval)? {
                    Ok(r) => (r, &gp.targets),
                    Err(d) => {
                        expr::note_decline(&d);
                        return Ok(None);
                    }
                },
            }
        };
        let spread_rows: Option<Vec<usize>> = narrowed.as_ref().map(|(_, active)| {
            let mut sr = vec![0usize; n];
            for (q, &row) in active.iter().enumerate() {
                sr[row] = q;
            }
            sr
        });
        let mask: Option<&[bool]> = match &narrowed {
            Some((comb, _)) => Some(comb.as_slice()),
            None => placed_mask.as_deref(),
        };
        let mut group_targets: Vec<SEXP> = Vec::with_capacity(res.len());
        let mut group_vals: Vec<Vals> = Vec::with_capacity(res.len());
        for (i, v) in res.into_iter().enumerate() {
            if let Some(sr) = plan.rules_rels.iter().find(|r| r.name == targets[i]) {
                let rels_s = match &cur_rels {
                    Some(h) => h.get(),
                    None => match find_str(st, b"relations")? {
                        Some(ri) => elt(st, ri),
                        None => return decline("no relations"),
                    },
                };
                match write_rel_rule_col(st, rels_s, g, sr, &v, mask, spread_rows.as_deref())? {
                    Some(h) => cur_rels = Some(h),
                    None => return decline("relation rule write declined"),
                }
            } else {
                group_targets.push(targets[i]);
                group_vals.push(v);
            }
        }
        let mut out = OwnedListSexp::new(group_vals.len(), false)?;
        for (i, v) in group_vals.iter().enumerate() {
            match mask {
                None => out.set_value(i, expr::to_r(v, n)?)?,
                Some(m) => {
                    let old = match find(cn, group_targets[i]) {
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
                    let spread;
                    let val = match &spread_rows {
                        Some(sr) if v.len() != 1 => {
                            spread = tidyabm_core::expr::take(v, sr);
                            &spread
                        }
                        _ => v,
                    };
                    match expr::assign_rows(old, val, m)? {
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
        if !group_targets.is_empty() {
            results.push((k, out, group_targets));
        }
    }
    if !applied && !plan.has_when && (0..len(groups)).any(|k| rows(elt(groups, k)).unwrap_or(0) > 0)
    {
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
    let mut out_st = match cur {
        // SAFETY: as above.
        Some(gs) => with_elt(st, gi, gs.get(), unsafe { rapi::R_BlankString })?,
        None => keep(st)?,
    };
    if let Some(cr) = &cur_rels {
        let ri = find_str(out_st.get(), b"relations")?.ok_or_else(|| bad("relations"))?;
        // SAFETY: reading a static of R's (unused name: `ri` is in range).
        out_st = with_elt(out_st.get(), ri, cr.get(), unsafe { rapi::R_BlankString })?;
    }
    Ok(Some(out_st))
}

/// `abm_global()` natively (slice 1's `rk_global()`, also for one group): each
/// rule over all groups stacked, the later rules seeing the earlier writes.
pub(super) fn exec_global(plan: &Plan, st: SEXP, values_only: bool) -> savvy::Result<Option<H>> {
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
    let mut sizes = Vec::with_capacity(ng);
    let mut n = 0;
    for k in 0..ng {
        match rows(elt(groups, k)) {
            Some(r) => {
                sizes.push(r);
                n += r;
            }
            None => return decline("a group without columns"),
        }
    }
    if n == 0 {
        return decline("no agents");
    }
    let mut ins: Vec<Vals> = Vec::with_capacity(gp.cols.len() + gp.globs.len() + gp.scope.len());
    let mut xs = Vec::with_capacity(
        (if ng == 1 { gp.cols.len() } else { 0 }) + gp.globs.len() + gp.scope.len(),
    );
    let n_cols = if ng == 1 {
        let g = elt(groups, 0);
        let gnms = names_of(g)?;
        for &c in &gp.cols {
            let Some(j) = find(gnms, c) else {
                return decline("a column some group lacks");
            };
            xs.push(elt(g, j));
        }
        xs.len()
    } else {
        for &c in &gp.cols {
            match stacked_pad(groups, c, &sizes)? {
                Some(v) => ins.push(v),
                None => return decline("a column of another kind"),
            }
        }
        0
    };
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
    for &x in &xs[..n_cols] {
        match expr::vals_of(x, false)? {
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

/// `abm_global(.by = <name>)` natively ([run_global_by()]): rule by rule, key
/// by key, the rule's program over the groups stacked with the target bound to
/// the key's current value (`NA` for a key the global does not name) and
/// `.key` to the key; the values gathered into a named vector as `out[[key]] <-
/// val` gathers them (types promoted as `[[<-` promotes them). Keys must be
/// integers or strings (`as.character()` of a double is R's formatting).
pub(super) fn exec_global_by(plan: &Plan, st: SEXP) -> savvy::Result<Option<H>> {
    use std::borrow::Cow;
    use tidyabm_core::sample::NA_INT;
    let (by, by_src) = plan.gby.ok_or_else(|| bad("no gby"))?;
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

/// `abm_rules(.by = )` natively ([run_rules_by()]): the groups stacked, each rule
/// evaluated once per value of the key in `group_by()`'s order (`NA` too, its
/// rows then keeping their old value), the later rules reading what the earlier
/// wrote, then each value written back to every group.
pub(super) fn exec_by(plan: &Plan, st: SEXP) -> savvy::Result<Option<H>> {
    use tidyabm_core::expr as ce;
    let gp = plan.groups.first().ok_or_else(|| bad("no program"))?;
    let by = plan.by.ok_or_else(|| bad("no key"))?;
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
