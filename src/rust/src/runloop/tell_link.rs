use super::*;

/// `abm_tell(to = <id>)` natively ([run_tell()]): `when`, `to` and the messages
/// over the groups stacked; each sender's message to its recipient, several to
/// one resolved (`last`, `first`, `sum`, `mean`; `error` is R's), written into
/// the recipients' existing columns in vctrs' common type (`replace_at()`).
pub(super) fn exec_tell(plan: &Plan, st: SEXP) -> savvy::Result<Option<H>> {
    use std::borrow::Cow;
    use tidyabm_core::sample::NA_INT;
    let (has_when, resolve, to_nb, has_order, efield) = plan.tell.ok_or_else(|| bad("no tell"))?;
    let gp = plan.groups.first().ok_or_else(|| bad("no program"))?;
    let edges_nb: Option<(&[i32], &[i32])> = if to_nb {
        let ef_bytes = if efield == rapi_nil() {
            b"edges".as_slice()
        } else {
            bytes(efield)
        };
        let Some(e) = find_str(st, ef_bytes)?.map(|i| elt(st, i)) else {
            return decline("no network (R's error)");
        };
        if e == rapi_nil() {
            return decline("no network (R's error)");
        }
        let (Some(fi), Some(ti)) = (find_str(e, b"from")?, find_str(e, b"to")?) else {
            return decline("edges without from/to");
        };
        let (ec_f, ec_t) = (elt(e, fi), elt(e, ti));
        if !all_bare(&[ec_f, ec_t])? {
            return decline("edges with attributes");
        }
        let (Some(ef), Some(et)) = (
            store::numbers_of::<i32>(ec_f, INTSXP)?,
            store::numbers_of::<i32>(ec_t, INTSXP)?,
        ) else {
            return decline("edges that are not integers");
        };
        if ef.len() != et.len() {
            return decline("malformed edges");
        }
        Some((ef, et))
    } else {
        None
    };
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
    let eval_one = |idx: usize| -> savvy::Result<Option<Vals>> {
        match prog.compute_one(idx, &ins, n)? {
            Ok(v) => Ok(Some(v)),
            Err(d) => {
                expr::note_decline(&d);
                Ok(None)
            }
        }
    };
    let mut k = 0;
    let speaks: Vec<bool> = if has_when {
        k = 1;
        let Some(w) = eval_one(0)? else {
            return Ok(None);
        };
        match condition(&w, n) {
            Some(c) => c,
            None => return decline("a when that is not logical"),
        }
    } else {
        vec![true; n]
    };
    if !speaks.iter().any(|&b| b) {
        return Ok(Some(keep(st)?));
    }
    let at_id = IdIndex::new(&ids);
    let (mut senders, mut recipient, mut recipient_row): (Vec<usize>, Vec<i32>, Vec<usize>) =
        if let Some((ef, et)) = edges_nb {
            let mut s = Vec::new();
            let mut r = Vec::new();
            let mut rr = Vec::new();
            // exact neighbour_table(state$edges) order: (from, to) then (to, from)
            for (&f_id, &t_id) in ef.iter().zip(et.iter()).chain(et.iter().zip(ef.iter())) {
                if let Some(sr) = at_id.get(f_id) {
                    if speaks[sr] {
                        let Some(rrow) = at_id.get(t_id) else {
                            return decline("a recipient that does not exist (R's error)");
                        };
                        s.push(sr);
                        r.push(t_id);
                        rr.push(rrow);
                    }
                }
            }
            (s, r, rr)
        } else {
            let Some(tv) = eval_one(k)? else {
                return Ok(None);
            };
            k += 1;
            let tgt = recycle(tv, n);
            let Vals::Int(tgt) = tgt else {
                return decline("recipients that are not integer ids");
            };
            let s: Vec<usize> = (0..n).filter(|&i| speaks[i] && tgt[i] != NA_INT).collect();
            let r: Vec<i32> = s.iter().map(|&i| tgt[i]).collect();
            let mut rr = Vec::with_capacity(r.len());
            for &id in &r {
                match at_id.get(id) {
                    Some(row) => rr.push(row),
                    None => return decline("a recipient that does not exist (R's error)"),
                }
            }
            (s, r, rr)
        };
    if senders.is_empty() {
        return Ok(Some(keep(st)?));
    }
    if has_order {
        let Some(ov) = eval_one(k)? else {
            return Ok(None);
        };
        k += 1;
        let key_all = recycle(ov, n);
        let key = tidyabm_core::expr::take(&key_all, &senders);
        let m_len = senders.len();
        let mut kept: Vec<usize> = match &key {
            Vals::Dbl(x) => (0..m_len).filter(|&i| !x[i].is_nan()).collect(),
            Vals::Int(x) | Vals::Lgl(x) => (0..m_len).filter(|&i| x[i] != NA_INT).collect(),
            Vals::Str(_) => return decline("tell .order of strings (collation)"),
        };
        if kept.is_empty() {
            return Ok(Some(keep(st)?));
        }
        match &key {
            Vals::Dbl(x) => {
                kept.sort_by(|&a, &b| x[a].partial_cmp(&x[b]).unwrap_or(std::cmp::Ordering::Equal))
            }
            Vals::Int(x) | Vals::Lgl(x) => kept.sort_by_key(|&i| x[i]),
            Vals::Str(_) => {}
        }
        senders = kept.iter().map(|&i| senders[i]).collect();
        recipient = kept.iter().map(|&i| recipient[i]).collect();
        recipient_row = kept.iter().map(|&i| recipient_row[i]).collect();
    }
    let mut raw_msgs: Vec<Vals> = Vec::with_capacity(gp.targets.len());
    for r_idx in k..k + gp.targets.len() {
        let Some(v) = eval_one(r_idx)? else {
            return Ok(None);
        };
        raw_msgs.push(v);
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
    let at: Vec<usize> = if dup || resolve == 5 {
        first.iter().map(|&q| recipient_row[q]).collect()
    } else {
        recipient_row.clone()
    };
    // SAFETY: reading a static of R's.
    let na_real = unsafe { rapi::R_NaReal };
    let mut msgs: Vec<Vals> = Vec::with_capacity(gp.targets.len());
    for v in raw_msgs {
        let v = recycle(v, n);
        let sent = tidyabm_core::expr::take(&v, &senders);
        let msg = if !dup || resolve == 5 {
            sent
        } else {
            match resolve {
                1 => tidyabm_core::expr::take(&sent, &first),
                0 => {
                    let last: Vec<usize> =
                        members.iter().map(|m| *m.last().unwrap_or(&0)).collect();
                    tidyabm_core::expr::take(&sent, &last)
                }
                2 | 3 | 8 => {
                    let is_sum = resolve == 2 || resolve == 8;
                    let out: Vec<f64> = match &sent {
                        Vals::Dbl(x) => members
                            .iter()
                            .map(|m| {
                                let part: Vec<f64> = m.iter().map(|&q| x[q]).collect();
                                if is_sum {
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
                                o.push(if is_sum {
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
                6 | 7 => {
                    let want_max = resolve == 7;
                    let na_low = na_real.to_bits() & 0xFFFF_FFFF;
                    let out: Vec<f64> = match &sent {
                        Vals::Dbl(x) => {
                            let mut o = Vec::with_capacity(members.len());
                            for m in &members {
                                let part: Vec<f64> = m.iter().map(|&q| x[q]).collect();
                                let Some(v) = tidyabm_core::reduce::extreme_f64(
                                    &part,
                                    want_max,
                                    false,
                                    |v| v.is_nan() && (v.to_bits() & 0xFFFF_FFFF) == na_low,
                                ) else {
                                    return decline("min()/max() of empty messages");
                                };
                                o.push(v);
                            }
                            o
                        }
                        Vals::Int(x) | Vals::Lgl(x) => {
                            let mut o = Vec::with_capacity(members.len());
                            for m in &members {
                                let part: Vec<i32> = m.iter().map(|&q| x[q]).collect();
                                let Some(v) =
                                    tidyabm_core::reduce::extreme_int(&part, want_max, false)
                                else {
                                    return decline("min()/max() of empty messages");
                                };
                                o.push(if v == NA_INT { na_real } else { f64::from(v) });
                            }
                            o
                        }
                        Vals::Str(_) => return decline("min()/max() of strings"),
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
            let sz = sizes[j];
            if resolve == 5 {
                // SAFETY: reading a type tag.
                if unsafe { rapi::TYPEOF(col) } != VECSXP || len(col) != sz {
                    return decline("collect target that is not a list column");
                }
                // accept bare list or `AsIs`-only list (`vctrs::vec_cast_common` sets class = "AsIs")
                let ok_col = if all_bare(&[col])? {
                    true
                } else {
                    // SAFETY: main thread; `col` is protected by `g`; temporary empty lists
                    // are protected while compared with `R_compute_identical`.
                    unsafe {
                        rapi::protect_value(0, move || {
                            let a = rapi::Rf_protect(rapi::Rf_allocVector(VECSXP, 0));
                            rapi::SHALLOW_DUPLICATE_ATTRIB(a, col);
                            let b = rapi::Rf_protect(rapi::Rf_allocVector(VECSXP, 0));
                            let cls = rapi::Rf_protect(rapi::Rf_allocVector(STRSXP, 1));
                            rapi::SET_STRING_ELT(cls, 0, rapi::Rf_mkChar(c"AsIs".as_ptr()));
                            rapi::Rf_setAttrib(b, rapi::Rf_install(c"class".as_ptr()), cls);
                            let eq = rapi::R_compute_identical(a, b, 16);
                            rapi::Rf_unprotect(3);
                            eq
                        })
                    }? != 0
                };
                if !ok_col {
                    return decline("collect target with unsupported attributes");
                }
                let mut new_col = OwnedListSexp::new(sz, false)?;
                for i in 0..sz {
                    new_col.set_value(i, Sexp(elt(col, i)))?;
                }
                for &q in &sel {
                    let part = tidyabm_core::expr::take(msg, &members[q]);
                    new_col.set_value(at[q] - start, expr::to_r(&part, part.len())?)?;
                }
                let nc_p = new_col.inner();
                // SAFETY: main thread; `new_col` is protected by `OwnedListSexp`; setting
                // `class = "AsIs"` matches `vctrs::vec_cast_common(list, I(list))`.
                unsafe {
                    rapi::protect(move || {
                        let cls = rapi::Rf_protect(rapi::Rf_allocVector(STRSXP, 1));
                        rapi::SET_STRING_ELT(cls, 0, rapi::Rf_mkChar(c"AsIs".as_ptr()));
                        rapi::Rf_setAttrib(nc_p, rapi::Rf_install(c"class".as_ptr()), cls);
                        rapi::Rf_unprotect(1);
                    })
                }?;
                let g2 = set_named(g, t, nc_p)?;
                // SAFETY: reading a static of R's (unused name: `j` is in range).
                cur = Some(with_elt(gs, j, g2.get(), unsafe { rapi::R_BlankString })?);
                continue;
            }
            if !all_bare(&[col])? {
                return decline("a target column with attributes");
            }
            let Some(old) = expr::vals_of(col, false)? else {
                return decline("a target column of another type");
            };
            let mut mask = vec![false; sz];
            let mut spread_rows = vec![0usize; sz];
            for &q in &sel {
                mask[at[q] - start] = true;
                spread_rows[at[q] - start] = q;
            }
            let spread = if resolve == 8 {
                // `.resolve = "add"`: `old[rows_j] + msg[sel]`
                let m_spread = tidyabm_core::expr::take(msg, &spread_rows);
                match (&old, &m_spread) {
                    (Vals::Int(o) | Vals::Lgl(o), Vals::Int(m) | Vals::Lgl(m)) => {
                        let mut out = vec![0i32; sz];
                        for i in 0..sz {
                            if !mask[i] {
                                continue;
                            }
                            if o[i] == NA_INT || m[i] == NA_INT {
                                out[i] = NA_INT;
                            } else {
                                let Some(s) = o[i].checked_add(m[i]) else {
                                    return decline("integer overflow in add (R warns)");
                                };
                                out[i] = s;
                            }
                        }
                        Vals::Int(Cow::Owned(out))
                    }
                    (Vals::Dbl(o), Vals::Dbl(m)) => {
                        let mut out = vec![0.0; sz];
                        for i in 0..sz {
                            if mask[i] {
                                out[i] = o[i] + m[i];
                            }
                        }
                        Vals::Dbl(Cow::Owned(out))
                    }
                    (Vals::Dbl(o), Vals::Int(m) | Vals::Lgl(m)) => {
                        let mut out = vec![0.0; sz];
                        for i in 0..sz {
                            if mask[i] {
                                out[i] = if m[i] == NA_INT {
                                    na_real
                                } else {
                                    o[i] + f64::from(m[i])
                                };
                            }
                        }
                        Vals::Dbl(Cow::Owned(out))
                    }
                    (Vals::Int(o) | Vals::Lgl(o), Vals::Dbl(m)) => {
                        let mut out = vec![0.0; sz];
                        for i in 0..sz {
                            if mask[i] {
                                out[i] = if o[i] == NA_INT {
                                    na_real
                                } else {
                                    f64::from(o[i]) + m[i]
                                };
                            }
                        }
                        Vals::Dbl(Cow::Owned(out))
                    }
                    _ => return decline("add on non-numeric values"),
                }
            } else {
                tidyabm_core::expr::take(msg, &spread_rows)
            };
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

/// `abm_draw()` natively (FR14, [run_draw()]): rules evaluated over the edge
/// table (one per edge, or twice for `.each = "endpoint"`), written into
/// `.draw_<target>` and `.back_<target>` on shallow copies of `state$edges`,
/// clearing `state$stale_draws`.
pub(super) fn exec_draw(plan: &Plan, st: SEXP) -> savvy::Result<Option<H>> {
    let (endpoint, efield) = plan.draw.ok_or_else(|| bad("no draw"))?;
    let gp = plan.groups.first().ok_or_else(|| bad("no program"))?;
    if gp.all_targets.len() != gp.targets.len() {
        return Err(bad("draw targets"));
    }
    let ef_bytes = if efield == rapi_nil() {
        b"edges".as_slice()
    } else {
        bytes(efield)
    };
    let Some(ei) = find_str(st, ef_bytes)? else {
        return decline("no network (R's error)");
    };
    let edges = elt(st, ei);
    if edges == rapi_nil() {
        return decline("no network (R's error)");
    }
    let Some(m) = rows(edges) else {
        return decline("edges without columns");
    };
    let enms = names_of(edges)?;
    let mut ins: Vec<Vals> = Vec::with_capacity(gp.cols.len() + gp.globs.len() + gp.scope.len());
    for &c in &gp.cols {
        let Some(j) = find(enms, c) else {
            return decline("an edge column not found");
        };
        let col = elt(edges, j);
        if !all_bare(&[col])? {
            return decline("an edge column with attributes");
        }
        let Some(v) = expr::vals_of(col, false)? else {
            return decline("an edge column of another type");
        };
        ins.push(v);
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
    let nr = gp.targets.len();
    type DrawPair = (Vals<'static>, Option<Vals<'static>>);
    let eval_all = |rng: Option<&mut crate::rng::RRng>| -> savvy::Result<Option<Vec<DrawPair>>> {
        let mut ev = expr::Evaluator::new(rng);
        let mut out = Vec::with_capacity(nr);
        for k in 0..nr {
            let fwd = match ev.eval(prog, k, &ins, m)? {
                Ok(v) => expr::owned(recycle(v, m)),
                Err(d) => {
                    expr::note_decline(&d);
                    return Ok(None);
                }
            };
            let back = if endpoint {
                match ev.eval(prog, k, &ins, m)? {
                    Ok(v) => Some(expr::owned(recycle(v, m))),
                    Err(d) => {
                        expr::note_decline(&d);
                        return Ok(None);
                    }
                }
            } else {
                None
            };
            out.push((fwd, back));
        }
        Ok(Some(out))
    };
    let drawn = if plan.draws {
        crate::rng::with_rng(|r| eval_all(Some(r)))?
    } else {
        eval_all(None)?
    };
    let Some(drawn) = drawn else {
        return Ok(None);
    };
    let mut e_cur: Option<H> = None;
    for (k, (fwd, back)) in drawn.into_iter().enumerate() {
        let mut hold = OwnedListSexp::new(if back.is_some() { 2 } else { 1 }, false)?;
        hold.set_value(0, expr::to_r(&fwd, m)?)?;
        let base = e_cur.as_ref().map_or(edges, |h| h.get());
        let e1 = set_named(base, gp.targets[k], elt(hold.inner(), 0))?;
        let e2 = match back {
            Some(b) => {
                hold.set_value(1, expr::to_r(&b, m)?)?;
                set_named(e1.get(), gp.all_targets[k], elt(hold.inner(), 1))?
            }
            None => set_named(e1.get(), gp.all_targets[k], elt(hold.inner(), 0))?,
        };
        e_cur = Some(e2);
    }
    let st2 = match &e_cur {
        // SAFETY: reading a static of R's (unused name: `ei` is in range).
        Some(e2) => with_elt(st, ei, e2.get(), unsafe { rapi::R_BlankString })?,
        None => keep(st)?,
    };
    Ok(Some(without(st2.get(), b"stale_draws")?))
}

/// Relation `abm_link(via = ...)` / `abm_unlink(via = ...)` natively
/// ([run_link()] / [run_unlink()] + [relation_pairs()] + [add_relation_rows()]
/// / [drop_relation_rows()]).
fn exec_link_rel(plan: &Plan, lp: &LinkPlan, via: SEXP, st: SEXP) -> savvy::Result<Option<H>> {
    use std::borrow::Cow;
    use std::collections::HashSet;
    use tidyabm_core::sample::NA_INT;
    let nil = rapi_nil();
    let Some(ri) = find_str(st, b"relations")? else {
        return decline("no relations (R's error)");
    };
    let rels_s = elt(st, ri);
    // SAFETY: reading a type tag.
    if rels_s == nil || unsafe { rapi::TYPEOF(rels_s) } != VECSXP {
        return decline("no relations (R's error)");
    }
    let Some(rk) = find(names_of(rels_s)?, via) else {
        return decline("relation not found (R's error)");
    };
    let rel = elt(rels_s, rk);
    let Some(ei) = find_str(rel, b"edges")? else {
        return decline("relation without edges");
    };
    let edges = elt(rel, ei);
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
    if !lp.has_to {
        let Some(mi) = find_str(st, b"match")? else {
            return decline("no standing match (R's error)");
        };
        let mobj = elt(st, mi);
        if mobj == nil {
            return decline("no standing match (R's error)");
        }
        let (Some(mti), Some(msi)) = (find_str(mobj, b"match")?, find_str(mobj, b"size")?) else {
            return decline("malformed match");
        };
        let msz_s = elt(mobj, msi);
        let Some(msz) = store::numbers_of::<i32>(msz_s, INTSXP)?
            .and_then(|v| v.first().copied())
            .or_else(|| {
                store::numbers_of::<f64>(msz_s, rapi::REALSXP)
                    .ok()
                    .flatten()
                    .and_then(|v| v.first().copied())
                    .map(|x| x as i32)
            })
        else {
            return decline("non-numeric match size");
        };
        if msz != 2 {
            return decline("relation link requires match size 2 (R's error)");
        }
        if rows(elt(mobj, mti)).unwrap_or(0) == 0 {
            return Ok(Some(keep(st)?));
        }
    }
    let Some(Vals::Int(ids)) = stacked_col_named(groups, b".id", &sizes)? else {
        return decline("ids that are not integers");
    };
    let mut keep_row = vec![true; n];
    let mut tgt: Option<Vec<i32>> = None;
    let mut rule_targets: Vec<SEXP> = Vec::new();
    let mut rule_vals: Vec<Vals<'static>> = Vec::new();
    if let Some(gp) = plan.groups.first() {
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
            match match_inputs_ids_rel(st, groups, &ids, &mnames, &lp.rels, None)? {
                Some(v) => ins.extend(v),
                None => return decline("match or relation columns that cannot be built natively"),
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
        let mut idx = 0;
        if lp.has_when {
            let Some(c) = condition(&res[idx], n) else {
                return decline("a when condition that is not logical");
            };
            keep_row = c;
            idx += 1;
        }
        if lp.has_to {
            let tv = recycle(res[idx].clone(), n);
            let tv_i: Vec<i32> = match &tv {
                Vals::Int(v) | Vals::Lgl(v) => v.to_vec(),
                Vals::Dbl(v) => {
                    let mut out = Vec::with_capacity(n);
                    for &d in v.iter() {
                        if d.is_nan() {
                            out.push(NA_INT);
                        } else if (-2147483647.0..=2147483647.0).contains(&d) {
                            out.push(d as i32);
                        } else {
                            return decline("to id out of integer range");
                        }
                    }
                    out
                }
                Vals::Str(_) => return decline("to of character"),
            };
            tgt = Some(tv_i);
            idx += 1;
        }
        rule_targets = gp.targets.clone();
        for v in &res[idx..] {
            rule_vals.push(expr::owned(recycle(v.clone(), n)));
        }
    }
    let tgt_vec: Vec<i32> = match tgt {
        Some(v) => v,
        None => {
            let Some(mut mv) = match_inputs_ids(st, groups, &ids, &[b".partner"])? else {
                return decline("no .partner for relation link");
            };
            match mv.pop() {
                Some(Vals::Int(v)) => v.into_owned(),
                _ => return decline(".partner not integer"),
            }
        }
    };
    let kept: Vec<usize> = (0..n)
        .filter(|&i| keep_row[i] && tgt_vec[i] != NA_INT)
        .collect();
    if kept.is_empty() {
        return Ok(Some(keep(st)?));
    }
    let at_id = IdIndex::new(&ids);
    for &i in &kept {
        if at_id.get(tgt_vec[i]).is_none() {
            return decline("relation link to unknown agent (R's error)");
        }
        if ids[i] == tgt_vec[i] {
            return decline("relation self-link (R's error)");
        }
    }
    let (Some(fi_col), Some(ti_col)) = (find_str(edges, b"from")?, find_str(edges, b"to")?) else {
        return decline("relation edges without from/to");
    };
    let (ec_f, ec_t) = (elt(edges, fi_col), elt(edges, ti_col));
    if !all_bare(&[ec_f, ec_t])? {
        return decline("relation edges with attributes");
    }
    let (Some(ef), Some(et)) = (
        store::numbers_of::<i32>(ec_f, INTSXP)?,
        store::numbers_of::<i32>(ec_t, INTSXP)?,
    ) else {
        return decline("relation edges not integer");
    };
    // SAFETY: reading a static of R's (indices are in range).
    let blank = unsafe { rapi::R_BlankString };
    if lp.drop {
        if ef.is_empty() {
            return Ok(Some(keep(st)?));
        }
        let drop_set: HashSet<(i32, i32)> = kept.iter().map(|&i| (ids[i], tgt_vec[i])).collect();
        let keep_e: Vec<bool> = ef
            .iter()
            .zip(et)
            .map(|(&f, &t)| !drop_set.contains(&(f, t)))
            .collect();
        if keep_e.iter().all(|&b| b) {
            return Ok(Some(keep(st)?));
        }
        let Some(pe) = slice_tibble(edges, &keep_e)? else {
            return decline("relation edges that cannot be sliced natively");
        };
        let r2 = with_elt(rel, ei, pe.get(), blank)?;
        let rels2 = with_elt(rels_s, rk, r2.get(), blank)?;
        return Ok(Some(with_elt(st, ri, rels2.get(), blank)?));
    }
    // add_relation_rows(): skip pairs already present and deduplicate new pairs
    let mut seen: HashSet<(i32, i32)> = ef.iter().copied().zip(et.iter().copied()).collect();
    let mut add_rows: Vec<usize> = Vec::new();
    for &i in &kept {
        if seen.insert((ids[i], tgt_vec[i])) {
            add_rows.push(i);
        }
    }
    if add_rows.is_empty() {
        return Ok(Some(keep(st)?));
    }
    let old_n = ef.len();
    let add_n = add_rows.len();
    let total = old_n + add_n;
    let ecn: Vec<SEXP> = names_of(edges)?.to_vec();
    let defs = find_str(rel, b"defaults")?.map(|i| elt(rel, i));
    let def_names: &[SEXP] = match defs {
        Some(d) if d != nil => names_of(d)?,
        _ => &[],
    };
    // SAFETY: reading statics of R's.
    let (na_real, na_str) = unsafe { (rapi::R_NaReal, rapi::R_NaString as usize) };
    let mut out = OwnedListSexp::new(ecn.len(), false)?;
    for (j, &cn) in ecn.iter().enumerate() {
        if j == fi_col {
            let mut nf = Vec::with_capacity(total);
            nf.extend_from_slice(ef);
            nf.extend(add_rows.iter().map(|&i| ids[i]));
            out.set_value(j, expr::to_r(&Vals::Int(Cow::Owned(nf)), total)?)?;
            continue;
        }
        if j == ti_col {
            let mut nt = Vec::with_capacity(total);
            nt.extend_from_slice(et);
            nt.extend(add_rows.iter().map(|&i| tgt_vec[i]));
            out.set_value(j, expr::to_r(&Vals::Int(Cow::Owned(nt)), total)?)?;
            continue;
        }
        let old_s = elt(edges, j);
        if !all_bare(&[old_s])? {
            return decline("relation column with attributes");
        }
        let Some(old_v) = expr::vals_of(old_s, false)? else {
            return decline("relation column of another type");
        };
        let add_v: Vals<'static> = if let Some(ti) = rule_targets.iter().position(|&t| t == cn) {
            tidyabm_core::expr::take(&rule_vals[ti], &add_rows)
        } else {
            let Some(d) = defs.filter(|&d| d != nil) else {
                return decline("relation without defaults");
            };
            let Some(dj) = find(def_names, cn) else {
                return decline("relation column default not found");
            };
            let ds = elt(d, dj);
            if !all_bare(&[ds])? || len(ds) != 1 {
                return decline("relation default not scalar bare");
            }
            let Some(dv) = expr::vals_of(ds, false)? else {
                return decline("relation default of another type");
            };
            expr::owned(recycle(dv, add_n))
        };
        let comb: Vals<'static> = match (&old_v, &add_v) {
            (Vals::Dbl(_), _) | (_, Vals::Dbl(_)) => {
                let (Some(mut a), Some(b)) =
                    (vals_as_f64(&old_v, na_real), vals_as_f64(&add_v, na_real))
                else {
                    return decline("incompatible relation column types");
                };
                a.extend_from_slice(&b);
                Vals::Dbl(Cow::Owned(a))
            }
            (Vals::Int(_), Vals::Int(_) | Vals::Lgl(_)) | (Vals::Lgl(_), Vals::Int(_)) => {
                let to_i = |v: &Vals| -> Option<Vec<i32>> {
                    match v {
                        Vals::Int(x) | Vals::Lgl(x) => Some(x.to_vec()),
                        _ => None,
                    }
                };
                let (Some(mut a), Some(b)) = (to_i(&old_v), to_i(&add_v)) else {
                    return decline("incompatible relation column types");
                };
                a.extend_from_slice(&b);
                Vals::Int(Cow::Owned(a))
            }
            (Vals::Lgl(a), Vals::Lgl(b)) => Vals::Lgl(Cow::Owned([&a[..], &b[..]].concat())),
            (Vals::Str(a), Vals::Str(b)) => Vals::Str(Cow::Owned([&a[..], &b[..]].concat())),
            (Vals::Str(a), Vals::Lgl(b)) if b.iter().all(|&e| e == NA_INT) => {
                let mut v = a.to_vec();
                v.resize(total, na_str);
                Vals::Str(Cow::Owned(v))
            }
            _ => return decline("incompatible relation column types"),
        };
        out.set_value(j, expr::to_r(&comb, total)?)?;
    }
    let frame = tbl_frame(out.inner(), &ecn, total)?;
    let r2 = with_elt(rel, ei, frame.get(), blank)?;
    let rels2 = with_elt(rels_s, rk, r2.get(), blank)?;
    Ok(Some(with_elt(st, ri, rels2.get(), blank)?))
}

/// Network or relation `abm_link()` / `abm_unlink()` (FR15a + `via =`).
pub(super) fn exec_link(plan: &Plan, st: SEXP) -> savvy::Result<Option<H>> {
    use std::borrow::Cow;
    use std::collections::{BTreeMap, HashSet};
    use tidyabm_core::sample::NA_INT;
    let lp = plan.link.as_ref().ok_or_else(|| bad("no link"))?;
    if let Some(via) = lp.via {
        return exec_link_rel(plan, lp, via, st);
    }
    let (drop_edges, has_when) = (lp.drop, lp.has_when);
    let nil = rapi_nil();
    let ei_opt = find_str(st, b"edges")?;
    let edges = ei_opt.map_or(nil, |i| elt(st, i));
    if drop_edges {
        if edges == nil || rows(edges).unwrap_or(0) == 0 {
            return Ok(Some(keep(st)?));
        }
    } else if edges == nil {
        return decline("no network (R's error)");
    }
    let ei = ei_opt.ok_or_else(|| bad("no edges slot"))?;
    let Some(mi) = find_str(st, b"match")? else {
        return decline("no standing match (R's error)");
    };
    let mobj = elt(st, mi);
    if mobj == nil {
        return decline("no standing match (R's error)");
    }
    let (Some(mti), Some(msi)) = (find_str(mobj, b"match")?, find_str(mobj, b"size")?) else {
        return decline("malformed match");
    };
    let mtbl = elt(mobj, mti);
    let msz_s = elt(mobj, msi);
    let Some(msz) = store::numbers_of::<i32>(msz_s, INTSXP)?
        .and_then(|v| v.first().copied())
        .or_else(|| {
            store::numbers_of::<f64>(msz_s, rapi::REALSXP)
                .ok()
                .flatten()
                .and_then(|v| v.first().copied())
                .map(|x| x as i32)
        })
    else {
        return decline("non-numeric match size");
    };
    if rows(mtbl).unwrap_or(0) == 0 {
        return Ok(Some(keep(st)?));
    }
    // validate existing edges columns (`from`/`to`; `!drop` requires no drawn/extra cols)
    let (Some(fi_col), Some(ti_col)) = (find_str(edges, b"from")?, find_str(edges, b"to")?) else {
        return decline("edges without from/to");
    };
    let (ec_f, ec_t) = (elt(edges, fi_col), elt(edges, ti_col));
    if !all_bare(&[ec_f, ec_t])? {
        return decline("edges with attributes");
    }
    let (Some(ef), Some(et)) = (
        store::numbers_of::<i32>(ec_f, INTSXP)?,
        store::numbers_of::<i32>(ec_t, INTSXP)?,
    ) else {
        return decline("edges that are not integers");
    };
    if ef.len() != et.len() {
        return decline("malformed edges");
    }
    if !drop_edges && len(edges) != 2 {
        return decline("edges with drawn or extra columns");
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
    let keep_row: Vec<bool> = if has_when {
        let gp = plan.groups.first().ok_or_else(|| bad("no link program"))?;
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
        let Some(c) = condition(&res[0], n) else {
            return decline("a when condition that is not logical");
        };
        c
    } else {
        vec![true; n]
    };
    // build canonical deduplicated pairs from the standing match
    let Some(mid_j) = find_str(mtbl, b".id")? else {
        return decline("match without .id");
    };
    let Some(mids) = store::numbers_of::<i32>(elt(mtbl, mid_j), INTSXP)? else {
        return decline("match .id not integer");
    };
    let midx = IdIndex::new(mids);
    let mut seen_pair: HashSet<(i32, i32)> = HashSet::new();
    let mut pairs: Vec<(i32, i32)> = Vec::new();
    if msz == 2 {
        let Some(mp_j) = find_str(mtbl, b".partner")? else {
            return decline("match without .partner");
        };
        let Some(mpart) = store::numbers_of::<i32>(elt(mtbl, mp_j), INTSXP)? else {
            return decline("match .partner not integer");
        };
        for i in 0..n {
            if !keep_row[i] {
                continue;
            }
            if let Some(mr) = midx.get(ids[i]) {
                let p = mpart[mr];
                if p != NA_INT {
                    let pair = (ids[i].min(p), ids[i].max(p));
                    if seen_pair.insert(pair) {
                        pairs.push(pair);
                    }
                }
            }
        }
    } else {
        let Some(mg_j) = find_str(mtbl, b".group_id")? else {
            return decline("match without .group_id");
        };
        let Some(mgid) = store::numbers_of::<i32>(elt(mtbl, mg_j), INTSXP)? else {
            return decline("match .group_id not integer");
        };
        let mut by_gid: BTreeMap<i32, Vec<i32>> = BTreeMap::new();
        for i in 0..n {
            if !keep_row[i] {
                continue;
            }
            if let Some(mr) = midx.get(ids[i]) {
                let g = mgid[mr];
                if g != NA_INT {
                    by_gid.entry(g).or_default().push(ids[i]);
                }
            }
        }
        for (_, grp) in by_gid {
            if grp.len() < 2 {
                continue;
            }
            for a in 0..grp.len() {
                for b in (a + 1)..grp.len() {
                    let pair = (grp[a].min(grp[b]), grp[a].max(grp[b]));
                    if seen_pair.insert(pair) {
                        pairs.push(pair);
                    }
                }
            }
        }
    }
    if pairs.is_empty() {
        return Ok(Some(keep(st)?));
    }
    if !drop_edges {
        let mut existing: HashSet<(i32, i32)> = HashSet::with_capacity(ef.len());
        for (&a, &b) in ef.iter().zip(et) {
            existing.insert((a.min(b), a.max(b)));
        }
        let add: Vec<(i32, i32)> = pairs
            .into_iter()
            .filter(|p| !existing.contains(p))
            .collect();
        if add.is_empty() {
            return Ok(Some(keep(st)?));
        }
        let total = ef.len() + add.len();
        let mut nf = Vec::with_capacity(total);
        let mut nt = Vec::with_capacity(total);
        nf.extend_from_slice(ef);
        nt.extend_from_slice(et);
        for (a, b) in add {
            nf.push(a);
            nt.push(b);
        }
        let mut out = OwnedListSexp::new(2, false)?;
        out.set_value(0, expr::to_r(&Vals::Int(Cow::Owned(nf)), total)?)?;
        out.set_value(1, expr::to_r(&Vals::Int(Cow::Owned(nt)), total)?)?;
        let frame = tbl_frame_named(out.inner(), &[c"from", c"to"], total)?;
        // SAFETY: reading a static of R's (unused name: `ei` is in range).
        Ok(Some(with_elt(st, ei, frame.get(), unsafe {
            rapi::R_BlankString
        })?))
    } else {
        let keep_e: Vec<bool> = ef
            .iter()
            .zip(et)
            .map(|(&a, &b)| !seen_pair.contains(&(a.min(b), a.max(b))))
            .collect();
        if keep_e.iter().all(|&b| b) {
            return Ok(Some(keep(st)?));
        }
        let Some(pe) = slice_tibble(edges, &keep_e)? else {
            return decline("edges that cannot be sliced natively");
        };
        // SAFETY: reading a static of R's (unused name: `ei` is in range).
        Ok(Some(with_elt(st, ei, pe.get(), unsafe {
            rapi::R_BlankString
        })?))
    }
}

/// `abm_pairs(via = <rel>, ..., .when =)` natively (FR15b, [run_pairs()]):
/// `.when` (optional) and rules evaluated over `rel$edges` plus `from_<col>` /
/// `to_<col>`, updating existing columns via `assign_rows()` (`vctrs` common type).
pub(super) fn exec_pairs(plan: &Plan, st: SEXP) -> savvy::Result<Option<H>> {
    let (via, has_when) = plan.pairs.ok_or_else(|| bad("no pairs"))?;
    let gp = plan.groups.first().ok_or_else(|| bad("no program"))?;
    let nil = rapi_nil();
    let Some(ri) = find_str(st, b"relations")? else {
        return decline("no relations (R's error)");
    };
    let rels = elt(st, ri);
    // SAFETY: reading a type tag.
    if rels == nil || unsafe { rapi::TYPEOF(rels) } != VECSXP {
        return decline("no relations (R's error)");
    }
    let rnames = names_of(rels)?;
    let Some(rel_i) = find(rnames, via) else {
        return decline("relation not found (R's error)");
    };
    let rel = elt(rels, rel_i);
    let Some(ei) = find_str(rel, b"edges")? else {
        return decline("relation without edges");
    };
    let edges = elt(rel, ei);
    let Some(m) = rows(edges) else {
        return decline("malformed relation edges");
    };
    if m == 0 {
        return Ok(Some(keep(st)?));
    }
    if gp.wslot.len() != gp.cols.len() {
        return Err(bad("pairs vkind length"));
    }
    let need_agents = gp.wslot.iter().any(|&k| k != 0);
    let mut fi: Vec<usize> = Vec::new();
    let mut ti: Vec<usize> = Vec::new();
    let mut groups_sexp = nil;
    let mut sizes: Vec<usize> = Vec::new();
    if need_agents {
        let gi = find_str(st, b"groups")?.ok_or_else(|| bad("no groups"))?;
        groups_sexp = elt(st, gi);
        let ng = len(groups_sexp);
        sizes.reserve(ng);
        for k in 0..ng {
            match rows(elt(groups_sexp, k)) {
                Some(r) => sizes.push(r),
                None => return decline("a group without columns"),
            }
        }
        let Some(Vals::Int(all_ids)) = stacked_col_named(groups_sexp, b".id", &sizes)? else {
            return decline("ids that are not integers");
        };
        let (Some(fc), Some(tc)) = (find_str(edges, b"from")?, find_str(edges, b"to")?) else {
            return decline("relation edges without from/to");
        };
        let (ecf, ect) = (elt(edges, fc), elt(edges, tc));
        if !all_bare(&[ecf, ect])? {
            return decline("relation edges with attributes");
        }
        let (Some(ef), Some(et)) = (
            store::numbers_of::<i32>(ecf, INTSXP)?,
            store::numbers_of::<i32>(ect, INTSXP)?,
        ) else {
            return decline("relation edges that are not integers");
        };
        if ef.len() != m || et.len() != m {
            return decline("malformed relation edges");
        }
        let at_id = IdIndex::new(&all_ids);
        fi.reserve(m);
        ti.reserve(m);
        for r in 0..m {
            let (Some(a), Some(b)) = (at_id.get(ef[r]), at_id.get(et[r])) else {
                return decline("relation endpoint not in population");
            };
            fi.push(a);
            ti.push(b);
        }
    }
    let enms = names_of(edges)?;
    let mut ins: Vec<Vals> = Vec::with_capacity(gp.cols.len() + gp.globs.len() + gp.scope.len());
    for (&c, &vk) in gp.cols.iter().zip(&gp.wslot) {
        match vk {
            0 => {
                let Some(ej) = find(enms, c) else {
                    return decline("relation column not found");
                };
                let col = elt(edges, ej);
                if !all_bare(&[col])? {
                    return decline("relation column with attributes");
                }
                match expr::vals_of(col, false)? {
                    Some(v) => ins.push(v),
                    None => return decline("relation column of another type"),
                }
            }
            1 | 2 => {
                let Some(v) = stacked_pad(groups_sexp, c, &sizes)? else {
                    return decline("agent column of another kind");
                };
                let idx = if vk == 1 { &fi } else { &ti };
                ins.push(tidyabm_core::expr::take(&v, idx));
            }
            _ => return Err(bad("pairs vkind")),
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
    let res = match prog.compute(ins, m, m)? {
        Ok(r) => r,
        Err(d) => {
            expr::note_decline(&d);
            return Ok(None);
        }
    };
    let (keep_row, vals) = if has_when {
        let Some(k) = condition(&res[0], m) else {
            return decline("a pairs .when that is not logical (R's error)");
        };
        (k, &res[1..])
    } else {
        (vec![true; m], &res[..])
    };
    if vals.len() != gp.targets.len() {
        return Err(bad("pairs rule count"));
    }
    let mut out = OwnedListSexp::new(vals.len(), false)?;
    for (i, (&tg, v)) in gp.targets.iter().zip(vals).enumerate() {
        let Some(ej) = find(enms, tg) else {
            return decline("new relation column (R updates rel$cols/defaults)");
        };
        let old_c = elt(edges, ej);
        if !all_bare(&[old_c])? {
            return decline("relation target column with attributes");
        }
        let Some(old) = expr::vals_of(old_c, false)? else {
            return decline("relation target column of another type");
        };
        match expr::assign_rows(Some(old), v, &keep_row)? {
            Ok(col) => out.set_value(i, expr::to_r(&col, m)?)?,
            Err(d) => {
                expr::note_decline(&d);
                return Ok(None);
            }
        }
    }
    let mut e2: Option<H> = None;
    for (i, &tg) in gp.targets.iter().enumerate() {
        let base = e2.as_ref().map_or(edges, |h| h.get());
        e2 = Some(set_named(base, tg, elt(out.inner(), i))?);
    }
    let Some(e2) = e2 else {
        return Ok(Some(keep(st)?));
    };
    // SAFETY: reading a static of R's (unused names: indices are in range).
    let blank = unsafe { rapi::R_BlankString };
    let rel2 = with_elt(rel, ei, e2.get(), blank)?;
    let rels2 = with_elt(rels, rel_i, rel2.get(), blank)?;
    Ok(Some(with_elt(st, ri, rels2.get(), blank)?))
}
