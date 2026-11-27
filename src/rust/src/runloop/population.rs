use super::*;

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
/// holds removed, pruning dead endpoints from `state$edges` and `state$relations`.
pub(super) fn exec_death(plan: &Plan, st: SEXP) -> savvy::Result<Option<H>> {
    let (prune, wired) = plan.death.unwrap_or((true, None));
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
        if wired.is_some_and(|w| w == gp.name || bytes(w) == bytes(gp.name)) {
            return decline("cannot kill wired patches (R's error)");
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
    // relations: drop every row touching a removed agent (`prune_relations()`)
    let mut pruned_rels: Option<H> = None;
    if !removed.is_empty() {
        if let Some(ri) = find_str(st, b"relations")? {
            let rels = elt(st, ri);
            if rels != rapi_nil() && len(rels) > 0 {
                let dead: std::collections::HashSet<i32> = removed.iter().copied().collect();
                let mut cur_rels: Option<H> = None;
                for k in 0..len(rels) {
                    let r = elt(rels, k);
                    let Some(ei) = find_str(r, b"edges")? else {
                        return decline("a relation without edges");
                    };
                    let e = elt(r, ei);
                    let (Some(fi), Some(ti)) = (find_str(e, b"from")?, find_str(e, b"to")?) else {
                        return decline("relation edges without from/to");
                    };
                    let (fr, to) = (elt(e, fi), elt(e, ti));
                    if len(fr) == 0 {
                        continue;
                    }
                    let (Some(fv), Some(tv)) = (
                        store::numbers_of::<i32>(fr, INTSXP)?,
                        store::numbers_of::<i32>(to, INTSXP)?,
                    ) else {
                        return decline("relation edges that are not integers");
                    };
                    let keep_r: Vec<bool> = fv
                        .iter()
                        .zip(tv)
                        .map(|(f, t)| !(dead.contains(f) || dead.contains(t)))
                        .collect();
                    if keep_r.iter().any(|&b| !b) {
                        let Some(sliced) = slice_tibble(e, &keep_r)? else {
                            return decline("relation edges that cannot be sliced natively");
                        };
                        // SAFETY: reading a static of R's (unused name: `ei` is in range).
                        let r2 = with_elt(r, ei, sliced.get(), unsafe { rapi::R_BlankString })?;
                        let base_r = cur_rels.as_ref().map_or(rels, |h| h.get());
                        // SAFETY: as above (`k` is in range).
                        cur_rels = Some(with_elt(base_r, k, r2.get(), unsafe {
                            rapi::R_BlankString
                        })?);
                    }
                }
                pruned_rels = cur_rels;
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
    if let Some(pr) = &pruned_rels {
        let ri = find_str(out.get(), b"relations")?.ok_or_else(|| bad("relations"))?;
        // SAFETY: reading a static of R's (unused name: `ri` is in range).
        out = with_elt(out.get(), ri, pr.get(), unsafe { rapi::R_BlankString })?;
    }
    // `state$relations <- prune_relations(NULL, ...)`: a NULL element is dropped
    if let Some(ri) = find_str(out.get(), b"relations")? {
        if elt(out.get(), ri) == rapi_nil() {
            out = without(out.get(), b"relations")?;
        }
    }
    Ok(Some(out))
}

/// `abm_birth(when =)` natively ([birth_by_condition()], no `attach_via`): per
/// group the parents (`when`), their offspring counts (`times`, through R's own
/// `as.integer(round())`), the parents' `cost` and the offspring's `inherit`,
/// each evaluated in R's order over the rows R evaluates it over; offspring
/// appended with new ids from `state$next_id`.
pub(super) fn exec_birth(plan: &Plan, st: SEXP) -> savvy::Result<Option<H>> {
    use std::borrow::Cow;
    use tidyabm_core::sample::NA_INT;
    let round_prog = plan.birth.ok_or_else(|| bad("no birth"))?;
    if let Some(c) = plan.count {
        return exec_birth_count(plan, st, c);
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
        if plan
            .birth_wired
            .is_some_and(|w| w == name || bytes(w) == bytes(name))
        {
            return decline("cannot clone wired patches (R's error)");
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
    let mut cur_f: Vec<i32> = if mode == 3 { ef.to_vec() } else { Vec::new() };
    let mut cur_t: Vec<i32> = if mode == 3 { et.to_vec() } else { Vec::new() };
    let res = crate::rng::with_rng(|r| -> savvy::Result<()> {
        use tidyabm_core::sample::RDraws;
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
            } else if mode == 3 {
                // `from = "random_edge"`: preferential attachment over live-growing edges
                let mut attempts = 0;
                while targets.len() < links && attempts < 10 * links {
                    attempts += 1;
                    let hit = if !cur_f.is_empty() {
                        let e = draw(r, cur_f.len(), 1)?[0] as usize - 1;
                        if r.unif() < 0.5 {
                            cur_f[e]
                        } else {
                            cur_t[e]
                        }
                    } else {
                        let pool: Vec<i32> = existing
                            .iter()
                            .copied()
                            .filter(|v| !targets.contains(v))
                            .collect();
                        if pool.is_empty() {
                            NA_INT
                        } else {
                            pool[draw(r, pool.len(), 1)?[0] as usize - 1]
                        }
                    };
                    if hit == NA_INT || targets.contains(&hit) {
                        continue;
                    }
                    targets.push(hit);
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
                if mode == 3 {
                    cur_f.push(id);
                    cur_t.push(t);
                }
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
    let Some(svals) = scope_values(plan)? else {
        return decline("a formula-environment value that cannot be read");
    };
    let gi = find_str(st, b"groups")?.ok_or_else(|| bad("no groups"))?;
    let groups = elt(st, gi);
    if len(groups) == 0 {
        return decline("no groups");
    }
    let tg_idx = match plan.birth_group {
        Some(bg) => match find(names_of(groups)?, bg) {
            Some(idx) => idx,
            None => return decline("birth target group not found"),
        },
        None => 0,
    };
    let g = elt(groups, tg_idx);
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
    let gs = with_elt(groups, tg_idx, frame.get(), blank)?;
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
