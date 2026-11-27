use super::*;

/// `x` reordered by `sample(x)` (R's `shuffle()`: a vector of one or none is
/// left alone): `sample.int(n)` without replacement drawn from R's generator.
pub(super) fn shuffle(r: &mut crate::rng::RRng, x: &mut Vec<i32>) -> savvy::Result<()> {
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
pub(super) fn match_table(
    id: &[i32],
    partner: &[i32],
    role: &[SEXP],
    gid: &[i32],
    cost: Option<&[f64]>,
) -> savvy::Result<H> {
    let n = id.len();
    let (ip, pp, gp) = (id.as_ptr(), partner.as_ptr(), gid.as_ptr());
    let rp = role.as_ptr();
    let has_role = role.len() == n;
    let has_cost = n > 0 && cost.is_some_and(|c| c.len() == n);
    let cp = cost.map_or(std::ptr::null(), |c| c.as_ptr());
    let ncol: isize = if has_cost { 5 } else { 4 };
    // SAFETY: main thread; every vector is protected from its allocation until
    // it is stored in the protected list; `ip`/`pp`/`gp`/`cp` point at `n` elements
    // each, alive for this call; the list is kept before anything else allocates.
    unsafe {
        build(move || {
            let x = rapi::Rf_protect(rapi::Rf_allocVector(VECSXP, ncol));
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
            if has_cost {
                let cv = rapi::Rf_allocVector(rapi::REALSXP, n as isize);
                rapi::SET_VECTOR_ELT(x, 4, cv);
                std::ptr::copy_nonoverlapping(cp, rapi::DATAPTR_OR_NULL(cv) as *mut f64, n);
            }
            let nm = rapi::Rf_protect(rapi::Rf_allocVector(STRSXP, ncol));
            for (k, s) in [c".id", c".partner", c".role", c".group_id"]
                .iter()
                .enumerate()
            {
                rapi::SET_STRING_ELT(nm, k as isize, rapi::Rf_mkChar(s.as_ptr()));
            }
            if has_cost {
                rapi::SET_STRING_ELT(nm, 4, rapi::Rf_mkChar(c".cost".as_ptr()));
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
pub(super) fn exec_match(plan: &Plan, st: SEXP) -> savvy::Result<Option<H>> {
    use tidyabm_core::sample::NA_INT;
    let mt = plan.mt.as_ref().ok_or_else(|| bad("no match"))?;
    let gp = plan.groups.first().ok_or_else(|| bad("no program"))?;
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
    let Some(svals) = scope_values(plan)? else {
        return decline("a formula-environment value that cannot be read");
    };
    // the conditions over the agents
    let mut elig = vec![true; n];
    let mut among = vec![true; n];
    let mut role_c: Option<(Vec<bool>, Vec<bool>)> = None;
    let mut weight_vals: Option<Vec<f64>> = None;
    if (mt.eligible || mt.among || !mt.roles.is_empty() || mt.has_weight) && n > 0 {
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
            k += 2;
        }
        if mt.has_weight {
            let wv = recycle(res[k].clone(), n);
            let ws: Vec<f64> = match &wv {
                Vals::Dbl(x) => x
                    .iter()
                    .map(|&v| if v.is_nan() { 0.0 } else { v.max(0.0) })
                    .collect(),
                Vals::Int(x) => x
                    .iter()
                    .map(|&v| {
                        if v == NA_INT {
                            0.0
                        } else {
                            f64::from(v).max(0.0)
                        }
                    })
                    .collect(),
                // R aborts `tidyABM_bad_weight` for logical / character
                Vals::Lgl(_) | Vals::Str(_) => return decline("non-numeric weight (R's error)"),
            };
            if ws.iter().any(|v| !v.is_finite()) {
                return decline("non-finite weight");
            }
            weight_vals = Some(ws);
        }
    }
    // FR15a: `nearest` with `cost =`: prepare pair-view column inputs before RNG
    let mut cost_prog: *const RxProgram = std::ptr::null();
    let mut cost_cols: Vec<(i32, Option<Vals<'static>>)> = Vec::new();
    let mut cost_rest: Vec<Vals> = Vec::new();
    let mut cost_xy: Option<(Vec<f64>, Option<Vec<f64>>)> = None;
    if mt.pair == 6 && n > 0 {
        let gp2 = plan.groups.get(1).ok_or_else(|| bad("no cost program"))?;
        if gp2.prog.is_null() || gp2.wslot.len() != gp2.cols.len() {
            return Err(bad("malformed cost program"));
        }
        cost_prog = gp2.prog;
        for (j, &c) in gp2.cols.iter().enumerate() {
            let vk = gp2.wslot[j];
            if vk == 2 || vk == 3 {
                cost_cols.push((vk, None));
            } else {
                let Some(v) = stacked_pad(groups, c, &sizes)? else {
                    return decline("a cost column of another kind");
                };
                cost_cols.push((vk, Some(v)));
            }
        }
        if gp2.wslot.contains(&3) {
            // SAFETY: reading a static of R's.
            let na_real = unsafe { rapi::R_NaReal };
            let Some(vx) = stacked_col_named(groups, b".x", &sizes)? else {
                return decline("no .x for .dist in cost");
            };
            let Some(cx) = vals_as_f64(&vx, na_real) else {
                return decline(".x not numeric for .dist");
            };
            let cy = if mt.dims.len() >= 2 {
                match stacked_col_named(groups, b".y", &sizes)? {
                    Some(vy) => {
                        let Some(y) = vals_as_f64(&vy, na_real) else {
                            return decline(".y not numeric for .dist");
                        };
                        Some(y)
                    }
                    None => None,
                }
            } else {
                None
            };
            cost_xy = Some((cx, cy));
        }
        let mut xs = Vec::new();
        if !read_globals(st, &gp2.globs, &mut xs)? {
            return decline("a cost global not found");
        }
        if !read_scope(&svals, &gp2.scope, &mut xs) {
            return decline("a cost formula-environment value not found");
        }
        let Some(more) = inputs(&xs)? else {
            return decline("a cost global or scope value of an unsupported type");
        };
        cost_rest = more;
    }
    let mut by_cols: Vec<Vals<'static>> = Vec::with_capacity(mt.by.len());
    for &c in &mt.by {
        let Some(v) = stacked_pad(groups, c, &sizes)? else {
            return decline("a by column of another kind");
        };
        by_cols.push(v);
    }
    let dot_by = match mt.dot_by {
        Some(c) => match stacked_pad(groups, c, &sizes)? {
            Some(v) => Some(v),
            None => return decline("a .by column of another kind"),
        },
        None => None,
    };
    // `pair = "one_of"` with `among = .R` / `.R_back`: per-chooser candidate rows
    // in agent order (matching `choice_set()` + `draw_choices()`).
    let rel_cands: Option<std::collections::HashMap<i32, Vec<usize>>> =
        if let Some((rel_nm, back)) = mt.among_rel {
            let Some(ri) = find_str(st, b"relations")? else {
                return decline("no relations");
            };
            let rels_s = elt(st, ri);
            if rels_s == rapi_nil() {
                return decline("no relations");
            }
            let Some(rk) = find(names_of(rels_s)?, rel_nm) else {
                return decline("relation not found");
            };
            let rel = elt(rels_s, rk);
            let Some(ei) = find_str(rel, b"edges")? else {
                return decline("relation without edges");
            };
            let edges = elt(rel, ei);
            let (Some(fi_c), Some(ti_c)) = (find_str(edges, b"from")?, find_str(edges, b"to")?)
            else {
                return decline("relation edges without from/to");
            };
            let (Some(ef), Some(et)) = (
                store::numbers_of::<i32>(elt(edges, fi_c), INTSXP)?,
                store::numbers_of::<i32>(elt(edges, ti_c), INTSXP)?,
            ) else {
                return decline("relation edges not integer");
            };
            let at_id = IdIndex::new(&ids);
            let mut map: std::collections::HashMap<i32, Vec<usize>> =
                std::collections::HashMap::new();
            for (&f, &t) in ef.iter().zip(et) {
                let (focal_id, cand_id) = if back { (t, f) } else { (f, t) };
                if focal_id == cand_id {
                    continue;
                }
                if let Some(cr) = at_id.get(cand_id) {
                    map.entry(focal_id).or_default().push(cr);
                }
            }
            for v in map.values_mut() {
                v.sort_unstable();
                v.dedup();
            }
            Some(map)
        } else {
            None
        };
    let pool: Vec<i32> = (0..n).filter(|&i| elig[i]).map(|i| ids[i]).collect();
    let mut out_id: Vec<i32> = Vec::new();
    let mut out_partner: Vec<i32> = Vec::new();
    let mut out_gid: Vec<i32> = Vec::new();
    let mut out_role: Vec<SEXP> = Vec::new();
    let mut out_cost: Vec<f64> = Vec::new();
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
                // Draw 1 candidate with distinct positive weights (descending CDF,
                // matching R's `sample.int(k, 1L, prob = wi)`). Returns `None` on
                // tied positive weights so R's Shell-sort tie order is used.
                let sample_one_w = |r: &mut crate::rng::RRng,
                                    cands: &[usize],
                                    ws: &[f64]|
                 -> Option<Option<usize>> {
                    let mut total = 0.0f64;
                    let mut pos: Vec<(f64, usize)> = Vec::new();
                    for &c in cands {
                        let w = ws[c];
                        total += w;
                        if w > 0.0 {
                            pos.push((w, c));
                        }
                    }
                    if total <= 0.0 || !total.is_finite() || pos.is_empty() {
                        return Some(None);
                    }
                    pos.sort_by(|a, b| b.0.total_cmp(&a.0));
                    if pos.windows(2).any(|pair| pair[0].0 == pair[1].0) {
                        return None;
                    }
                    let u = tidyabm_core::sample::RDraws::unif(r);
                    let mut cs = 0.0f64;
                    let mut pick = pos[pos.len() - 1].1;
                    for &(w, c) in &pos {
                        cs += w / total;
                        if u <= cs {
                            pick = c;
                            break;
                        }
                    }
                    Some(Some(pick))
                };
                let (p, draw) = if let Some(ref ws) = weight_vals {
                    if pool.is_empty() || n == 0 {
                        return Ok(Some(()));
                    }
                    let all_cands: Vec<usize> = if rel_cands.is_none() {
                        (0..n).filter(|&i| among[i]).collect()
                    } else {
                        Vec::new()
                    };
                    if rel_cands.is_none() && all_cands.is_empty() {
                        return Ok(Some(()));
                    }
                    let mut pk = Vec::with_capacity(pool.len());
                    let mut dk = Vec::with_capacity(pool.len());
                    let mut cand_buf: Vec<usize> = Vec::new();
                    for &pid in &pool {
                        let cands: &[usize] = if let Some(ref rc_map) = rel_cands {
                            match rc_map.get(&pid) {
                                Some(v) if !v.is_empty() => v.as_slice(),
                                _ => continue,
                            }
                        } else {
                            cand_buf.clear();
                            cand_buf.extend(all_cands.iter().copied().filter(|&c| ids[c] != pid));
                            if cand_buf.is_empty() {
                                continue;
                            }
                            cand_buf.as_slice()
                        };
                        match sample_one_w(r, cands, ws) {
                            None => return Ok(None),
                            Some(None) => continue,
                            Some(Some(cr)) => {
                                pk.push(pid);
                                dk.push(ids[cr]);
                            }
                        }
                    }
                    if pk.is_empty() {
                        return Ok(Some(()));
                    }
                    (pk, dk)
                } else if let Some(ref rc_map) = rel_cands {
                    if pool.is_empty() || n == 0 {
                        return Ok(Some(()));
                    }
                    let mut pk = Vec::with_capacity(pool.len());
                    let mut dk = Vec::with_capacity(pool.len());
                    for &pid in &pool {
                        let Some(cands) = rc_map.get(&pid) else {
                            continue;
                        };
                        if cands.is_empty() {
                            continue;
                        }
                        let j = tidyabm_core::sample::RDraws::unif_index(r, cands.len() as f64)
                            as usize;
                        pk.push(pid);
                        dk.push(ids[cands[j]]);
                    }
                    if pk.is_empty() {
                        return Ok(Some(()));
                    }
                    (pk, dk)
                } else {
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
                            .map(|_| {
                                cands[tidyabm_core::sample::RDraws::unif_index(r, dn) as usize]
                            })
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
                    (p, draw)
                };
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
                // match_nearest() on 1..k coordinates (optionally torus-aware):
                // per chooser, the candidate with the least sum_k (diff_k)^2
                // (itself excluded), the first of ties (`max.col(-d, "first")`);
                // a chooser whose distances are none finite sits out.
                if by_cols.is_empty() {
                    return Ok(Some(()));
                }
                // SAFETY: reading a static of R's.
                let na = unsafe { rapi::R_NaReal };
                let mut coords: Vec<Vec<f64>> = Vec::with_capacity(by_cols.len());
                for xy in &by_cols {
                    let Some(v) = vals_as_f64(xy, na) else {
                        return Ok(None);
                    };
                    coords.push(v);
                }
                let subs: Vec<usize> = (0..n).filter(|&i| elig[i]).collect();
                let cands: Vec<usize> = (0..n).filter(|&i| among[i]).collect();
                if subs.is_empty() || cands.is_empty() {
                    return Ok(Some(()));
                }
                // a missing coordinate makes max.col() answer NA: R's path
                if coords
                    .iter()
                    .any(|x| subs.iter().chain(&cands).any(|&i| x[i].is_nan()))
                {
                    return Ok(None);
                }
                if coords.len() == 1 {
                    let x0 = &coords[0];
                    let tx = mt.use_torus && mt.torus.first().copied().unwrap_or(false);
                    let lx = mt.dims.first().copied().unwrap_or(0.0);
                    for &s in &subs {
                        let sid = ids[s];
                        let sx = x0[s];
                        let mut best_d = f64::INFINITY;
                        let mut best_c = usize::MAX;
                        for &c in &cands {
                            if sid == ids[c] {
                                continue;
                            }
                            let mut dx = (sx - x0[c]).abs();
                            if tx {
                                dx = dx.min(lx - dx);
                            }
                            let d = dx * dx;
                            if d < best_d {
                                best_d = d;
                                best_c = c;
                            }
                        }
                        if best_c != usize::MAX && best_d.is_finite() {
                            out_id.push(sid);
                            out_partner.push(ids[best_c]);
                            out_cost.push(best_d.max(0.0).sqrt());
                        }
                    }
                } else if coords.len() == 2 {
                    let (x0, y0) = (&coords[0], &coords[1]);
                    let tx = mt.use_torus && mt.torus.first().copied().unwrap_or(false);
                    let ty = mt.use_torus && mt.torus.get(1).copied().unwrap_or(false);
                    let lx = mt.dims.first().copied().unwrap_or(0.0);
                    let ly = mt.dims.get(1).copied().unwrap_or(0.0);
                    for &s in &subs {
                        let sid = ids[s];
                        let (sx, sy) = (x0[s], y0[s]);
                        let mut best_d = f64::INFINITY;
                        let mut best_c = usize::MAX;
                        for &c in &cands {
                            if sid == ids[c] {
                                continue;
                            }
                            let mut dx = (sx - x0[c]).abs();
                            if tx {
                                dx = dx.min(lx - dx);
                            }
                            let mut dy = (sy - y0[c]).abs();
                            if ty {
                                dy = dy.min(ly - dy);
                            }
                            let d = dx * dx + dy * dy;
                            if d < best_d {
                                best_d = d;
                                best_c = c;
                            }
                        }
                        if best_c != usize::MAX && best_d.is_finite() {
                            out_id.push(sid);
                            out_partner.push(ids[best_c]);
                            out_cost.push(best_d.max(0.0).sqrt());
                        }
                    }
                } else {
                    for &s in &subs {
                        let mut best: Option<(f64, usize)> = None;
                        for &c in &cands {
                            let d = if ids[s] == ids[c] {
                                f64::INFINITY
                            } else {
                                let mut acc = 0.0f64;
                                for (k, xk) in coords.iter().enumerate() {
                                    let mut dk = (xk[s] - xk[c]).abs();
                                    if mt.use_torus && mt.torus.get(k).copied().unwrap_or(false) {
                                        dk = dk.min(mt.dims[k] - dk);
                                    }
                                    acc += dk * dk;
                                }
                                acc
                            };
                            if best.map_or(true, |(m, _)| d < m) {
                                best = Some((d, c));
                            }
                        }
                        if let Some((m, c)) = best {
                            if m.is_finite() {
                                out_id.push(ids[s]);
                                out_partner.push(ids[c]);
                                out_cost.push(m.max(0.0).sqrt());
                            }
                        }
                    }
                }
                out_gid = (1..=out_id.len() as i32).collect();
            }
            6 => {
                // FR15a: match_cheapest() — evaluate compiled `cost =` over the
                // (chooser, candidate) pair view, drop self and NA/NaN, take the
                // first minimum per chooser, output ordered by chooser `.id`
                let subs: Vec<usize> = (0..n).filter(|&i| elig[i]).collect();
                let cands: Vec<usize> = (0..n).filter(|&i| among[i]).collect();
                if subs.is_empty() || cands.is_empty() {
                    return Ok(Some(()));
                }
                let ns = subs.len();
                let nc = cands.len();
                let nv = ns * nc;
                let mut si = Vec::with_capacity(nv);
                let mut ci = Vec::with_capacity(nv);
                for &s in &subs {
                    for &c in &cands {
                        si.push(s);
                        ci.push(c);
                    }
                }
                let mut cins: Vec<Vals> = Vec::with_capacity(cost_cols.len() + cost_rest.len());
                for (vk, v) in &cost_cols {
                    match vk {
                        0 => cins.push(tidyabm_core::expr::take(
                            v.as_ref().ok_or_else(|| bad("cost col"))?,
                            &ci,
                        )),
                        1 => cins.push(tidyabm_core::expr::take(
                            v.as_ref().ok_or_else(|| bad("cost own col"))?,
                            &si,
                        )),
                        2 => cins.push(Vals::Int(std::borrow::Cow::Owned(
                            si.iter().map(|&s| ids[s]).collect(),
                        ))),
                        3 => {
                            let (cx, cy) = cost_xy.as_ref().ok_or_else(|| bad("cost xy"))?;
                            let dv: Vec<f64> = si
                                .iter()
                                .zip(&ci)
                                .map(|(&s, &c)| {
                                    space_dist_pair(
                                        cx[c],
                                        cy.as_ref().map(|y| y[c]),
                                        cx[s],
                                        cy.as_ref().map(|y| y[s]),
                                        &mt.dims,
                                        &mt.torus,
                                    )
                                })
                                .collect();
                            cins.push(Vals::Dbl(std::borrow::Cow::Owned(dv)));
                        }
                        _ => return Err(bad("cost input kind")),
                    }
                }
                cins.extend(cost_rest.iter().cloned());
                let mut ev = expr::Evaluator::new(Some(r));
                // SAFETY: the cost program is kept alive by the plan's descriptor.
                let prog = unsafe { &*cost_prog };
                let val = match ev.eval(prog, 0, &cins, nv)? {
                    Ok(v) => recycle(v, nv),
                    Err(d) => {
                        expr::note_decline(&d);
                        return Ok(None);
                    }
                };
                let costs: Vec<f64> = match &val {
                    Vals::Dbl(x) => x.to_vec(),
                    Vals::Int(x) => x
                        .iter()
                        .map(|&v| if v == NA_INT { f64::NAN } else { f64::from(v) })
                        .collect(),
                    // R aborts `tidyABM_bad_cost` for logical / character
                    Vals::Lgl(_) | Vals::Str(_) => return Ok(None),
                };
                let mut picked: Vec<(i32, i32, f64)> = Vec::with_capacity(ns);
                for (a, &s) in subs.iter().enumerate() {
                    let sid = ids[s];
                    let base = a * nc;
                    let mut best: Option<(f64, i32)> = None;
                    for (b, &c) in cands.iter().enumerate() {
                        let cid = ids[c];
                        if sid == cid {
                            continue;
                        }
                        let v = costs[base + b];
                        if v.is_nan() {
                            continue;
                        }
                        if best.map_or(true, |(m, _)| v < m) {
                            best = Some((v, cid));
                        }
                    }
                    if let Some((bv, cid)) = best {
                        picked.push((sid, cid, bv));
                    }
                }
                picked.sort_by_key(|&(sid, _, _)| sid);
                for (sid, cid, bv) in picked {
                    out_id.push(sid);
                    out_partner.push(cid);
                    out_cost.push(bv);
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
                let side = by_cols.first().ok_or_else(|| bad("no side"))?;
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
    let tbl = match_table(
        &out_id,
        &out_partner,
        &out_role,
        &out_gid,
        if mt.pair == 5 || mt.pair == 6 {
            Some(&out_cost)
        } else {
            None
        },
    )?;
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
