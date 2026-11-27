use super::*;

/// `abm_neighbours(within = <col> == own_<col> [& rest])` natively, as
/// [nb_one()] answers it: each focal agent's one candidate (its `<col>` matched
/// against the candidates' distinct values), kept where the rest holds there,
/// and each rule's argument read off at that candidate; written to every group,
/// `NA` (`0` for a bare `n()`) for an agent without one.
pub(super) fn exec_nb1(plan: &Plan, st: SEXP) -> savvy::Result<Option<H>> {
    use std::borrow::Cow;
    use tidyabm_core::sample::NA_INT;
    let nb = plan.nb1.as_ref().ok_or_else(|| bad("no nb1"))?;
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
    if n == 0 {
        return Ok(Some(keep(st)?));
    }
    let (Some(Vals::Int(focal)), Some(Vals::Int(cand))) = (
        stacked_pad(groups, nb.focal, &sizes)?,
        stacked_pad(groups, nb.cand, &sizes)?,
    ) else {
        return decline("join columns that are not integers");
    };
    // detect whether non-NA candidate keys repeat (`nb_one` vs `equijoin_view`)
    let mut at: std::collections::HashMap<i32, usize> = std::collections::HashMap::with_capacity(n);
    let mut has_dup = false;
    for (j, &c) in cand.iter().enumerate() {
        if at.insert(c, j).is_some() && !nb.cand_is_id && c != NA_INT {
            has_dup = true;
            break;
        }
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
    // SAFETY: reading a static of R's (its NA_real_).
    let na_real = unsafe { rapi::R_NaReal };
    if has_dup {
        // multi-match equijoin: matches equijoin_view() (skips NA_INT, ascending candidate rows)
        let mut by_key: std::collections::HashMap<i32, Vec<usize>> =
            std::collections::HashMap::new();
        for (j, &c) in cand.iter().enumerate() {
            if c != NA_INT {
                by_key.entry(c).or_default().push(j);
            }
        }
        let mut next = 0;
        let keep_cand: Option<Vec<bool>> = if nb.rest {
            let Vals::Lgl(k) = &res[0] else {
                return decline("a `within` condition that is not logical");
            };
            next = 1;
            Some((0..n).map(|j| k[pick(&res[0], j)] == 1).collect())
        } else {
            None
        };
        let mut offsets = Vec::with_capacity(n + 1);
        let mut flat = Vec::new();
        offsets.push(0usize);
        for &f in focal.iter() {
            if f != NA_INT {
                if let Some(js) = by_key.get(&f) {
                    if let Some(ref kc) = keep_cand {
                        flat.extend(js.iter().copied().filter(|&j| kc[j]));
                    } else {
                        flat.extend_from_slice(js);
                    }
                }
            }
            offsets.push(flat.len());
        }
        if flat.is_empty() {
            return decline("nobody with a candidate");
        }
        let mut full_outs: Vec<Vals> = Vec::with_capacity(nb.fns.len());
        let mut buf_i: Vec<i32> = Vec::new();
        for &f in &nb.fns {
            let v = match f {
                3 => Vals::Int(Cow::Owned(
                    (0..n)
                        .map(|i| (offsets[i + 1] - offsets[i]) as i32)
                        .collect(),
                )),
                _ => {
                    let x = &res[next];
                    next += 1;
                    match (f, x) {
                        (0 | 1, Vals::Lgl(a)) => {
                            let mut out = Vec::with_capacity(n);
                            for i in 0..n {
                                let s = &flat[offsets[i]..offsets[i + 1]];
                                if s.is_empty() {
                                    out.push(NA_INT);
                                } else {
                                    buf_i.clear();
                                    buf_i.extend(s.iter().map(|&j| a[pick(x, j)]));
                                    out.push(tidyabm_core::reduce::any_all(&buf_i, f == 0, false));
                                }
                            }
                            Vals::Lgl(Cow::Owned(out))
                        }
                        (0 | 1, _) => return decline("any()/all() of a value that is not logical"),
                        (_, Vals::Dbl(a)) => {
                            if (0..n).any(|i| offsets[i + 1] - offsets[i] > 1) {
                                return decline("a multi-match sum() of doubles (rowsum())");
                            }
                            let out = (0..n)
                                .map(|i| {
                                    let s = &flat[offsets[i]..offsets[i + 1]];
                                    if s.is_empty() {
                                        na_real
                                    } else {
                                        0.0 + a[pick(x, s[0])]
                                    }
                                })
                                .collect();
                            Vals::Dbl(Cow::Owned(out))
                        }
                        (_, Vals::Int(a) | Vals::Lgl(a)) => {
                            let mut out = Vec::with_capacity(n);
                            for i in 0..n {
                                let s = &flat[offsets[i]..offsets[i + 1]];
                                if s.is_empty() {
                                    out.push(NA_INT);
                                } else {
                                    buf_i.clear();
                                    buf_i.extend(s.iter().map(|&j| a[pick(x, j)]));
                                    let Some(sum) = tidyabm_core::reduce::sum_int(&buf_i, false)
                                    else {
                                        return decline("an integer sum outside the range");
                                    };
                                    out.push(sum);
                                }
                            }
                            Vals::Int(Cow::Owned(out))
                        }
                        _ => return decline("sum() of strings"),
                    }
                }
            };
            full_outs.push(v);
        }
        let mut cur: Option<H> = None;
        for (r, v) in full_outs.iter().enumerate() {
            let t = gp.targets[r];
            let mut start = 0;
            for (g, &sz) in sizes.iter().enumerate() {
                let end = start + sz;
                let range = start..end;
                start = end;
                if sz == 0 {
                    continue;
                }
                let col = match v {
                    Vals::Lgl(a) => Vals::Lgl(Cow::Owned(a[range.clone()].to_vec())),
                    Vals::Int(a) => Vals::Int(Cow::Owned(a[range.clone()].to_vec())),
                    Vals::Dbl(a) => Vals::Dbl(Cow::Owned(a[range.clone()].to_vec())),
                    Vals::Str(_) => return Err(bad("a string neighbour value")),
                };
                let miss_g: Vec<bool> = range.map(|i| offsets[i + 1] == offsets[i]).collect();
                let Some(col) = apply_nb_default(col, &miss_g, plan.nb_default)? else {
                    return decline("a .default that cannot be applied natively");
                };
                let gs = cur.as_ref().map_or(groups, |h| h.get());
                let base = elt(gs, g);
                let mut hold = OwnedListSexp::new(1, false)?;
                hold.set_value(0, expr::to_r(&col, sz)?)?;
                let g2 = set_named(base, t, elt(hold.inner(), 0))?;
                // SAFETY: reading a static of R's (unused name: `g` is in range).
                cur = Some(with_elt(gs, g, g2.get(), unsafe { rapi::R_BlankString })?);
            }
        }
        return match cur {
            // SAFETY: as above.
            Some(gs) => Ok(Some(with_elt(st, gi, gs.get(), unsafe {
                rapi::R_BlankString
            })?)),
            None => Ok(Some(keep(st)?)),
        };
    }
    // keep the first of a repeated value (match() semantics)
    for (j, &c) in cand.iter().enumerate().rev() {
        at.insert(c, j);
    }
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
                        .clone()
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
                        .clone()
                        .map(|i| match slot[i] {
                            usize::MAX if miss_zero && plan.nb_default.is_none() => 0,
                            usize::MAX => NA_INT,
                            p => a[p],
                        })
                        .collect(),
                )),
                Vals::Dbl(a) => Vals::Dbl(Cow::Owned(
                    rows_g
                        .clone()
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
            let miss_g: Vec<bool> = rows_g.map(|i| slot[i] == usize::MAX).collect();
            let Some(col) = apply_nb_default(col, &miss_g, plan.nb_default)? else {
                return decline("a .default that cannot be applied natively");
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
pub(super) fn exec_nbw(plan: &Plan, st: SEXP) -> savvy::Result<Option<H>> {
    use std::borrow::Cow;
    use tidyabm_core::sample::NA_INT;
    let nb = plan.nbw.as_ref().ok_or_else(|| bad("no nbw"))?;
    let gp = plan.groups.first().ok_or_else(|| bad("no program"))?;
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
                        .clone()
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
                        .clone()
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
                        .clone()
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
            let miss_g: Vec<bool> = rows_g.map(|i| slot[i] == usize::MAX).collect();
            let Some(col) = apply_nb_default(col, &miss_g, plan.nb_default)? else {
                return decline("a .default that cannot be applied natively");
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

/// The cross-product `within` natively ([attribute_view()] then
/// [neighbour_aggregate()] or `summarise()`): for each focal agent, `within`
/// over every candidate (or torus-aware spatial cell buckets when `.dist <= r` /
/// `.dist < r`), then each rule over the candidates kept.
pub(super) fn exec_cross(plan: &Plan, st: SEXP) -> savvy::Result<Option<H>> {
    use std::borrow::Cow;
    use tidyabm_core::sample::NA_INT;
    let cp = plan.cross.as_ref().ok_or_else(|| bad("no cross"))?;
    let fns = &cp.fns;
    let narm = &cp.narm;
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
    if n == 0 {
        return Ok(Some(keep(st)?));
    }
    // SAFETY: reading statics of R's.
    let (na_real, na_str) = unsafe { (rapi::R_NaReal, rapi::R_NaString as usize) };
    // candidate columns, `own_<col>`, and `.dist` (both None)
    let mut cand: Vec<Option<Vals>> = Vec::with_capacity(gp.cols.len());
    let mut own: Vec<Option<Vals>> = Vec::with_capacity(gp.cols.len());
    for &c in &gp.cols {
        let b = bytes(c);
        if b == b".dist" {
            cand.push(None);
            own.push(None);
            continue;
        }
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
    let coord_xy: Option<(Vec<f64>, Option<Vec<f64>>)> = if cp.has_dist {
        let Some(vx) = stacked_col_named(groups, b".x", &sizes)? else {
            return decline("no .x for .dist");
        };
        let Some(cx) = vals_as_f64(&vx, na_real) else {
            return decline(".x not numeric for .dist");
        };
        let cy = if cp.dims.len() >= 2 {
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
        Some((cx, cy))
    } else {
        None
    };
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
    // Optional O(N*k) spatial cell bucketing for `.dist <= r` / `.dist < r`
    struct Buckets {
        r: f64,
        nx_b: usize,
        ny_b: usize,
        is_2d: bool,
        fbx: Vec<Option<usize>>,
        fby: Vec<Option<usize>>,
        cells: Vec<Vec<usize>>,
    }
    let mut buckets: Option<Buckets> = None;
    let mut neg_radius = false;
    if cp.dist_bucket && n > 64 && (!cp.dist_cand_rest || rest.iter().all(|v| v.len() == 1)) {
        if let Some((ref cx, ref cy)) = coord_xy {
            let rad_idx = 1 + fns.iter().filter(|&&f| f != 0).count();
            let mut rins: Vec<Vals> = Vec::with_capacity(cand.len() + rest.len());
            for _ in 0..cand.len() {
                rins.push(Vals::Dbl(Cow::Borrowed(&[0.0])));
            }
            rins.extend(rest.iter().cloned());
            if let Ok(rv) = prog.compute_one(rad_idx, &rins, 1)? {
                let rad_opt: Option<f64> = match &rv {
                    Vals::Dbl(v) if v.len() == 1 && v[0].is_finite() => Some(v[0]),
                    Vals::Int(v) if v.len() == 1 && v[0] != NA_INT => Some(f64::from(v[0])),
                    _ => None,
                };
                if let Some(r) = rad_opt {
                    if r < 0.0 {
                        neg_radius = true;
                    } else if !cp.dims.is_empty() {
                        let is_2d = cp.dims.len() >= 2 && cy.is_some();
                        let cell_w = r.max(1e-9);
                        let nx_f = (cp.dims[0] / cell_w).floor();
                        let ny_f = if is_2d {
                            (cp.dims[1] / cell_w).floor()
                        } else {
                            1.0
                        };
                        if nx_f >= 3.0 && (!is_2d || ny_f >= 3.0) {
                            let cand_mask: Option<Vec<bool>> = if cp.dist_cand_rest {
                                let mut cins: Vec<Vals> =
                                    Vec::with_capacity(cand.len() + rest.len());
                                for c in &cand {
                                    cins.push(match c {
                                        Some(v) => match v {
                                            Vals::Lgl(x) => Vals::Lgl(Cow::Borrowed(&x[..])),
                                            Vals::Int(x) => Vals::Int(Cow::Borrowed(&x[..])),
                                            Vals::Dbl(x) => Vals::Dbl(Cow::Borrowed(&x[..])),
                                            Vals::Str(x) => Vals::Str(Cow::Borrowed(&x[..])),
                                        },
                                        None => Vals::Dbl(Cow::Borrowed(&[0.0])),
                                    });
                                }
                                cins.extend(rest.iter().cloned());
                                let cr = match prog.compute_one(rad_idx + 1, &cins, n)? {
                                    Ok(v) => v,
                                    Err(d) => {
                                        expr::note_decline(&d);
                                        return Ok(None);
                                    }
                                };
                                match &cr {
                                    Vals::Lgl(x) if x.len() == n => {
                                        Some(x.iter().map(|&v| v == 1).collect())
                                    }
                                    Vals::Lgl(x) if x.len() == 1 => Some(vec![x[0] == 1; n]),
                                    _ => {
                                        return decline(
                                            "a within that is not a condition (R's error)",
                                        );
                                    }
                                }
                            } else {
                                None
                            };
                            let nx_b = nx_f.min(256.0) as usize;
                            let ny_b = if is_2d { ny_f.min(256.0) as usize } else { 1 };
                            let bx_w = cp.dims[0] / nx_b as f64;
                            let by_w = if is_2d { cp.dims[1] / ny_b as f64 } else { 1.0 };
                            let b1d = |v: f64, w: f64, nb: usize| -> Option<usize> {
                                if v.is_nan() {
                                    None
                                } else {
                                    let b = ((v - cp.shift) / w).floor();
                                    Some((b.clamp(0.0, (nb - 1) as f64)) as usize)
                                }
                            };
                            let fbx: Vec<Option<usize>> =
                                cx.iter().map(|&v| b1d(v, bx_w, nx_b)).collect();
                            let fby: Vec<Option<usize>> = if is_2d {
                                let cy_v = cy.as_ref().ok_or_else(|| bad("bucket y"))?;
                                cy_v.iter().map(|&v| b1d(v, by_w, ny_b)).collect()
                            } else {
                                Vec::new()
                            };
                            let mut cells: Vec<Vec<usize>> = vec![Vec::new(); nx_b * ny_b];
                            for j in 0..n {
                                if cand_mask.as_ref().is_some_and(|m| !m[j]) {
                                    continue;
                                }
                                if let Some(bx) = fbx[j] {
                                    if is_2d {
                                        if let Some(by) = fby[j] {
                                            cells[bx + by * nx_b].push(j);
                                        }
                                    } else {
                                        cells[bx].push(j);
                                    }
                                }
                            }
                            buckets = Some(Buckets {
                                r,
                                nx_b,
                                ny_b,
                                is_2d,
                                fbx,
                                fby,
                                cells,
                            });
                        }
                    }
                }
            }
        }
    }
    if neg_radius {
        return decline("nobody has a neighbour");
    }
    let excl_ids: Option<Vec<i32>> = if cp.dist_excl_self {
        if ids_are_rows(groups)?.is_some() {
            None
        } else {
            let Some(Vals::Int(v)) = stacked_col_named(groups, b".id", &sizes)? else {
                return decline(".id not integer");
            };
            Some(v.into_owned())
        }
    } else {
        None
    };
    let is_self_excl = |i: usize, j: usize| -> bool {
        if !cp.dist_excl_self {
            return false;
        }
        match &excl_ids {
            None => j == i,
            Some(ids) => ids[j] == NA_INT || ids[i] == NA_INT || ids[j] == ids[i],
        }
    };
    let nr = fns.len();
    let empty_nan_dbl = crate::reduce::mean(&[]);
    let empty_nan_int = crate::reduce::mean_int(&[]);
    let mut cands_buf: Vec<usize> = Vec::new();
    let build_at = |i: usize, rows: Option<&[usize]>, dists: Option<&[f64]>| -> Vec<Vals> {
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
                (None, None) => {
                    if let Some(d) = dists {
                        Vals::Dbl(Cow::Owned(d.to_vec()))
                    } else {
                        let (cx, cy) = coord_xy.as_ref().expect("has_dist");
                        let fy = cy.as_ref().map(|y| y[i]);
                        let dv: Vec<f64> = match rows {
                            None => (0..n)
                                .map(|j| {
                                    space_dist_pair(
                                        cx[j],
                                        cy.as_ref().map(|y| y[j]),
                                        cx[i],
                                        fy,
                                        &cp.dims,
                                        &cp.torus,
                                    )
                                })
                                .collect(),
                            Some(rs) => rs
                                .iter()
                                .map(|&j| {
                                    space_dist_pair(
                                        cx[j],
                                        cy.as_ref().map(|y| y[j]),
                                        cx[i],
                                        fy,
                                        &cp.dims,
                                        &cp.torus,
                                    )
                                })
                                .collect(),
                        };
                        Vals::Dbl(Cow::Owned(dv))
                    }
                }
            });
        }
        ins.extend(rest.iter().cloned());
        ins
    };
    let fast_cand = cp.cand_only.iter().all(|&b| b) && rest.iter().all(|v| v.len() == 1);
    let (cols_out, seen_all): (Vec<Vals<'static>>, Vec<bool>) = if fast_cand {
        // Pre-evaluate every candidate-only rule ONCE over 0..n, then reduce directly
        // into flat per-agent buffers without per-agent gather/compute/allocations.
        let mut cand_ins: Vec<Vals> = Vec::with_capacity(cand.len() + rest.len());
        for c in &cand {
            cand_ins.push(match c {
                Some(v) => match v {
                    Vals::Lgl(x) => Vals::Lgl(Cow::Borrowed(&x[..])),
                    Vals::Int(x) => Vals::Int(Cow::Borrowed(&x[..])),
                    Vals::Dbl(x) => Vals::Dbl(Cow::Borrowed(&x[..])),
                    Vals::Str(x) => Vals::Str(Cow::Borrowed(&x[..])),
                },
                None => Vals::Dbl(Cow::Borrowed(&[0.0])),
            });
        }
        cand_ins.extend(rest.iter().cloned());
        let mut cand_vals: Vec<Option<Vals<'static>>> = Vec::with_capacity(nr);
        let mut p = 1;
        for &f in fns.iter() {
            if f == 0 {
                cand_vals.push(None);
                continue;
            }
            let v = match prog.compute_one(p, &cand_ins, n)? {
                Ok(v) => expr::owned(recycle(v, n)),
                Err(d) => {
                    expr::note_decline(&d);
                    return Ok(None);
                }
            };
            p += 1;
            match f {
                1 | 2 if matches!(v, Vals::Str(_)) => return decline("a summary of strings"),
                3 | 4 if !matches!(v, Vals::Lgl(_)) => {
                    return decline("any()/all() of other than logicals");
                }
                _ => {}
            }
            cand_vals.push(Some(v));
        }
        enum OutCol {
            Int(Vec<i32>),
            Lgl(Vec<i32>),
            Dbl(Vec<f64>),
        }
        let mut outs: Vec<OutCol> = Vec::with_capacity(nr);
        for (r, &f) in fns.iter().enumerate() {
            let col = match f {
                0 => OutCol::Int(vec![0i32; n]),
                1 => match cand_vals[r].as_ref() {
                    Some(Vals::Dbl(_)) => OutCol::Dbl(vec![na_real; n]),
                    _ => OutCol::Int(vec![NA_INT; n]),
                },
                2 => OutCol::Dbl(vec![na_real; n]),
                3 | 4 => OutCol::Lgl(vec![NA_INT; n]),
                _ => return Err(bad("fast cross fn")),
            };
            outs.push(col);
        }
        // Only floating-point reductions depend on ascending candidate order.
        let need_sort = cand_vals.iter().any(|v| matches!(v, Some(Vals::Dbl(_))));
        let mut seen = vec![false; n];
        let mut any_rows = false;
        let mut kept: Vec<usize> = Vec::new();
        let mut kd: Vec<f64> = Vec::new();
        for i in 0..n {
            kept.clear();
            if let Some(ref b) = buckets {
                let (cx, cy) = coord_xy.as_ref().ok_or_else(|| bad("bucket xy"))?;
                let cy_slice = cy.as_deref();
                let valid = b.fbx[i].is_some() && (!b.is_2d || b.fby[i].is_some());
                if valid {
                    let bx0 = b.fbx[i].unwrap_or(0) as isize;
                    let nx_b = b.nx_b as isize;
                    let tx = cp.torus.first().copied().unwrap_or(false);
                    let mut bxs = [0usize; 3];
                    let mut nbx = 0;
                    for d in -1..=1 {
                        let v = bx0 + d;
                        let u = if tx {
                            Some(v.rem_euclid(nx_b) as usize)
                        } else if v >= 0 && v < nx_b {
                            Some(v as usize)
                        } else {
                            None
                        };
                        if let Some(u) = u {
                            bxs[nbx] = u;
                            nbx += 1;
                        }
                    }
                    let fy = cy_slice.map(|y| y[i]);
                    let fx = cx[i];
                    if !need_sort && !cp.dist_has_rest {
                        let check_cell = |cell: &[usize], kept: &mut Vec<usize>| {
                            for &j in cell {
                                if is_self_excl(i, j) {
                                    continue;
                                }
                                let d = space_dist_pair(
                                    cx[j],
                                    cy_slice.map(|y| y[j]),
                                    fx,
                                    fy,
                                    &cp.dims,
                                    &cp.torus,
                                );
                                let hit =
                                    !d.is_nan() && if cp.dist_strict { d < b.r } else { d <= b.r };
                                if hit {
                                    kept.push(j);
                                }
                            }
                        };
                        if b.is_2d {
                            let by0 = b.fby[i].unwrap_or(0) as isize;
                            let ny_b = b.ny_b as isize;
                            let ty = cp.torus.get(1).copied().unwrap_or(false);
                            for dy in -1..=1 {
                                let vy = by0 + dy;
                                let uy = if ty {
                                    Some(vy.rem_euclid(ny_b) as usize)
                                } else if vy >= 0 && vy < ny_b {
                                    Some(vy as usize)
                                } else {
                                    None
                                };
                                if let Some(uy) = uy {
                                    for &ux in &bxs[..nbx] {
                                        check_cell(&b.cells[ux + uy * b.nx_b], &mut kept);
                                    }
                                }
                            }
                        } else {
                            for &ux in &bxs[..nbx] {
                                check_cell(&b.cells[ux], &mut kept);
                            }
                        }
                    } else {
                        cands_buf.clear();
                        if b.is_2d {
                            let by0 = b.fby[i].unwrap_or(0) as isize;
                            let ny_b = b.ny_b as isize;
                            let ty = cp.torus.get(1).copied().unwrap_or(false);
                            for dy in -1..=1 {
                                let vy = by0 + dy;
                                let uy = if ty {
                                    Some(vy.rem_euclid(ny_b) as usize)
                                } else if vy >= 0 && vy < ny_b {
                                    Some(vy as usize)
                                } else {
                                    None
                                };
                                if let Some(uy) = uy {
                                    for &ux in &bxs[..nbx] {
                                        cands_buf.extend_from_slice(&b.cells[ux + uy * b.nx_b]);
                                    }
                                }
                            }
                        } else {
                            for &ux in &bxs[..nbx] {
                                cands_buf.extend_from_slice(&b.cells[ux]);
                            }
                        }
                        if need_sort {
                            cands_buf.sort_unstable();
                        }
                        kd.clear();
                        for &j in &cands_buf {
                            if is_self_excl(i, j) {
                                continue;
                            }
                            let d = space_dist_pair(
                                cx[j],
                                cy_slice.map(|y| y[j]),
                                fx,
                                fy,
                                &cp.dims,
                                &cp.torus,
                            );
                            let hit =
                                !d.is_nan() && if cp.dist_strict { d < b.r } else { d <= b.r };
                            if hit {
                                kept.push(j);
                                if cp.dist_has_rest {
                                    kd.push(d);
                                }
                            }
                        }
                        if !kept.is_empty() && cp.dist_has_rest {
                            let kn = kept.len();
                            let keep = match prog.compute_one(
                                0,
                                &build_at(i, Some(&kept), Some(&kd)),
                                kn,
                            )? {
                                Ok(v) => v,
                                Err(d) => {
                                    expr::note_decline(&d);
                                    return Ok(None);
                                }
                            };
                            match &keep {
                                Vals::Lgl(x) if x.len() == kn => {
                                    let mut w = 0;
                                    for idx in 0..kn {
                                        if x[idx] == 1 {
                                            kept[w] = kept[idx];
                                            w += 1;
                                        }
                                    }
                                    kept.truncate(w);
                                }
                                Vals::Lgl(x) if x.len() == 1 => {
                                    if x[0] != 1 {
                                        kept.clear();
                                    }
                                }
                                _ => {
                                    return decline("a within that is not a condition (R's error)");
                                }
                            }
                        }
                    }
                }
            } else {
                let keep = match prog.compute_one(0, &build_at(i, None, None), n)? {
                    Ok(v) => v,
                    Err(d) => {
                        expr::note_decline(&d);
                        return Ok(None);
                    }
                };
                match &keep {
                    Vals::Lgl(x) if x.len() == n => {
                        for (j, &flag) in x.iter().enumerate() {
                            if flag == 1 {
                                kept.push(j);
                            }
                        }
                    }
                    Vals::Lgl(x) if x.len() == 1 => {
                        if x[0] == 1 {
                            kept.extend(0..n);
                        }
                    }
                    _ => return decline("a within that is not a condition (R's error)"),
                }
            }
            if kept.is_empty() {
                continue;
            }
            seen[i] = true;
            any_rows = true;
            let m = kept.len();
            for (r, &f) in fns.iter().enumerate() {
                if f == 0 {
                    if let OutCol::Int(ref mut v) = outs[r] {
                        v[i] = m as i32;
                    }
                    continue;
                }
                let cv = cand_vals[r].as_ref().ok_or_else(|| bad("cand val"))?;
                match f {
                    1 | 2 => match cv {
                        Vals::Dbl(x) => {
                            let mut s = 0.0f64;
                            let mut cnt = 0i32;
                            for &j in &kept {
                                let e = x[j];
                                if e.is_nan() {
                                    if !narm[r] {
                                        s += e;
                                        cnt += 1;
                                    }
                                } else {
                                    s += e;
                                    cnt += 1;
                                }
                            }
                            let val = if f == 1 {
                                s
                            } else if cnt == 0 {
                                empty_nan_dbl
                            } else {
                                let mut mean = s / f64::from(cnt);
                                if mean.is_finite() {
                                    let mut resid = 0.0f64;
                                    for &j in &kept {
                                        let e = x[j];
                                        if !(narm[r] && e.is_nan()) {
                                            resid += e - mean;
                                        }
                                    }
                                    mean += resid / f64::from(cnt);
                                }
                                mean
                            };
                            if let OutCol::Dbl(ref mut out) = outs[r] {
                                out[i] = val;
                            }
                        }
                        Vals::Lgl(x) | Vals::Int(x) => {
                            let mut s: i64 = 0;
                            let mut cnt: i64 = 0;
                            let mut has_na = false;
                            for &j in &kept {
                                let e = x[j];
                                if e == NA_INT {
                                    if !narm[r] {
                                        has_na = true;
                                    }
                                } else {
                                    s += i64::from(e);
                                    cnt += 1;
                                }
                            }
                            if f == 1 {
                                let val = if has_na {
                                    NA_INT
                                } else {
                                    if s.abs() > i64::from(i32::MAX) {
                                        return decline("an integer sum outside the range");
                                    }
                                    s as i32
                                };
                                if let OutCol::Int(ref mut out) = outs[r] {
                                    out[i] = val;
                                }
                            } else {
                                let val = if has_na {
                                    na_real
                                } else if cnt == 0 {
                                    empty_nan_int
                                } else {
                                    s as f64 / cnt as f64
                                };
                                if let OutCol::Dbl(ref mut out) = outs[r] {
                                    out[i] = val;
                                }
                            }
                        }
                        Vals::Str(_) => return decline("a summary of strings"),
                    },
                    3 | 4 => {
                        let Vals::Lgl(x) = cv else {
                            return decline("any()/all() of other than logicals");
                        };
                        let want = if f == 3 { 1 } else { 0 };
                        let mut hit = false;
                        let mut na = false;
                        for &j in &kept {
                            let e = x[j];
                            if e == want {
                                hit = true;
                                break;
                            }
                            if e == NA_INT {
                                na = true;
                            }
                        }
                        let o = if f == 3 { hit } else { !hit };
                        let val = if !narm[r] && !hit && na {
                            NA_INT
                        } else {
                            o as i32
                        };
                        if let OutCol::Lgl(ref mut out) = outs[r] {
                            out[i] = val;
                        }
                    }
                    _ => return Err(bad("fast cross fn")),
                }
            }
        }
        if !any_rows {
            return decline("nobody has a neighbour");
        }
        let cols_out = outs
            .into_iter()
            .map(|o| match o {
                OutCol::Int(v) => Vals::Int(Cow::Owned(v)),
                OutCol::Lgl(v) => Vals::Lgl(Cow::Owned(v)),
                OutCol::Dbl(v) => Vals::Dbl(Cow::Owned(v)),
            })
            .collect();
        (cols_out, seen)
    } else {
        let mut vals: Vec<Vec<Option<Vals<'static>>>> = vec![Vec::with_capacity(n); nr];
        let mut any_rows = false;
        for i in 0..n {
            let mut kept: Vec<usize>;
            let mut kept_d: Option<Vec<f64>> = None;
            if let Some(ref b) = buckets {
                let (cx, cy) = coord_xy.as_ref().ok_or_else(|| bad("bucket xy"))?;
                kept = Vec::new();
                let mut kd = Vec::new();
                let valid = b.fbx[i].is_some() && (!b.is_2d || b.fby[i].is_some());
                if valid {
                    let bx0 = b.fbx[i].unwrap_or(0) as isize;
                    let nx_b = b.nx_b as isize;
                    let tx = cp.torus.first().copied().unwrap_or(false);
                    let mut bxs = [0usize; 3];
                    let mut nbx = 0;
                    for d in -1..=1 {
                        let v = bx0 + d;
                        let u = if tx {
                            Some(v.rem_euclid(nx_b) as usize)
                        } else if v >= 0 && v < nx_b {
                            Some(v as usize)
                        } else {
                            None
                        };
                        if let Some(u) = u {
                            bxs[nbx] = u;
                            nbx += 1;
                        }
                    }
                    cands_buf.clear();
                    if b.is_2d {
                        let by0 = b.fby[i].unwrap_or(0) as isize;
                        let ny_b = b.ny_b as isize;
                        let ty = cp.torus.get(1).copied().unwrap_or(false);
                        for dy in -1..=1 {
                            let vy = by0 + dy;
                            let uy = if ty {
                                Some(vy.rem_euclid(ny_b) as usize)
                            } else if vy >= 0 && vy < ny_b {
                                Some(vy as usize)
                            } else {
                                None
                            };
                            if let Some(uy) = uy {
                                for &ux in &bxs[..nbx] {
                                    cands_buf.extend_from_slice(&b.cells[ux + uy * b.nx_b]);
                                }
                            }
                        }
                    } else {
                        for &ux in &bxs[..nbx] {
                            cands_buf.extend_from_slice(&b.cells[ux]);
                        }
                    }
                    cands_buf.sort_unstable();
                    let fy = cy.as_ref().map(|y| y[i]);
                    for &j in &cands_buf {
                        if is_self_excl(i, j) {
                            continue;
                        }
                        let d = space_dist_pair(
                            cx[j],
                            cy.as_ref().map(|y| y[j]),
                            cx[i],
                            fy,
                            &cp.dims,
                            &cp.torus,
                        );
                        let hit = !d.is_nan() && if cp.dist_strict { d < b.r } else { d <= b.r };
                        if hit {
                            kept.push(j);
                            kd.push(d);
                        }
                    }
                    if !kept.is_empty() && cp.dist_has_rest {
                        let kn = kept.len();
                        let keep =
                            match prog.compute_one(0, &build_at(i, Some(&kept), Some(&kd)), kn)? {
                                Ok(v) => v,
                                Err(d) => {
                                    expr::note_decline(&d);
                                    return Ok(None);
                                }
                            };
                        match &keep {
                            Vals::Lgl(x) if x.len() == kn => {
                                let mut nk = Vec::new();
                                let mut nd = Vec::new();
                                for (idx, (&j, &d)) in kept.iter().zip(&kd).enumerate() {
                                    if x[idx] == 1 {
                                        nk.push(j);
                                        nd.push(d);
                                    }
                                }
                                kept = nk;
                                kd = nd;
                            }
                            Vals::Lgl(x) if x.len() == 1 => {
                                if x[0] != 1 {
                                    kept.clear();
                                    kd.clear();
                                }
                            }
                            _ => return decline("a within that is not a condition (R's error)"),
                        }
                    }
                }
                kept_d = Some(kd);
            } else {
                let keep = match prog.compute_one(0, &build_at(i, None, None), n)? {
                    Ok(v) => v,
                    Err(d) => {
                        expr::note_decline(&d);
                        return Ok(None);
                    }
                };
                kept = match &keep {
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
            }
            if kept.is_empty() {
                for v in vals.iter_mut() {
                    v.push(None);
                }
                continue;
            }
            any_rows = true;
            let ins = build_at(i, Some(&kept), kept_d.as_deref());
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
                    1 | 2 => match &v {
                        Vals::Dbl(x) => {
                            let mut s = 0.0f64;
                            let mut cnt = 0i32;
                            for &e in x.iter() {
                                if e.is_nan() {
                                    if !narm[r] {
                                        s += e;
                                        cnt += 1;
                                    }
                                } else {
                                    s += e;
                                    cnt += 1;
                                }
                            }
                            if f == 1 {
                                Vals::Dbl(Cow::Owned(vec![s]))
                            } else if cnt == 0 {
                                Vals::Dbl(Cow::Owned(vec![empty_nan_dbl]))
                            } else {
                                let mut mean = s / f64::from(cnt);
                                if mean.is_finite() {
                                    let mut resid = 0.0f64;
                                    for &e in x.iter() {
                                        if !(narm[r] && e.is_nan()) {
                                            resid += e - mean;
                                        }
                                    }
                                    mean += resid / f64::from(cnt);
                                }
                                Vals::Dbl(Cow::Owned(vec![mean]))
                            }
                        }
                        Vals::Lgl(x) | Vals::Int(x) => {
                            let mut s: i64 = 0;
                            let mut cnt: i64 = 0;
                            let mut has_na = false;
                            for &e in x.iter() {
                                if e == NA_INT {
                                    if !narm[r] {
                                        has_na = true;
                                    }
                                } else {
                                    s += i64::from(e);
                                    cnt += 1;
                                }
                            }
                            if f == 1 {
                                if has_na {
                                    Vals::Int(Cow::Owned(vec![NA_INT]))
                                } else {
                                    if s.abs() > i64::from(i32::MAX) {
                                        return decline("an integer sum outside the range");
                                    }
                                    Vals::Int(Cow::Owned(vec![s as i32]))
                                }
                            } else if has_na {
                                Vals::Dbl(Cow::Owned(vec![na_real]))
                            } else if cnt == 0 {
                                Vals::Dbl(Cow::Owned(vec![empty_nan_int]))
                            } else {
                                Vals::Dbl(Cow::Owned(vec![s as f64 / cnt as f64]))
                            }
                        }
                        Vals::Str(_) => return decline("a summary of strings"),
                    },
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
        let seen = vals[0].iter().map(Option::is_some).collect();
        (cols_out, seen)
    };
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
            let col = expr::owned(tidyabm_core::expr::take(v, &rows_g));
            let miss_g: Vec<bool> = rows_g.iter().map(|&i| !seen_all[i]).collect();
            let Some(col) = apply_nb_default(col, &miss_g, plan.nb_default)? else {
                return decline("a .default that cannot be applied natively");
            };
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
    let ef_bytes = if nb.efield == nil {
        b"edges".as_slice()
    } else {
        bytes(nb.efield)
    };
    let cn_bytes = bytes(nb.cache_name);
    if cn_bytes == b"nb_cache" {
        if let Some(si) = find_str(st, b"stale_draws")? {
            if elt(st, si) != nil {
                return decline("stale draws");
            }
        }
    }
    let Some(e) = find_str(st, ef_bytes)?.map(|i| elt(st, i)) else {
        return decline("no network (R's error)");
    };
    if e == nil || len(e) != 2 {
        return decline("edges with columns besides from and to");
    }
    let en = names_of(e)?;
    if bytes(en[0]) != b"from" || bytes(en[1]) != b"to" {
        return decline("edges with columns besides from and to");
    }
    // `if (!identical(state[[cname]]$edges, edges))` the cache is rebuilt:
    // `list(edges = <edges>, nb = neighbour_table(<edges>))`
    let mut cached: Option<SEXP> = None;
    if let Some(i) = find_str(st, cn_bytes)? {
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
    let mut seen_any = vec![false; n];
    for &(f, _) in &rows {
        seen_any[f] = true;
    }
    let mut vals: Vec<Vals> = Vec::with_capacity(res.len());
    for (r, x) in res.into_iter().enumerate() {
        let x = recycle(x, n);
        let Vals::Lgl(x) = &x else {
            return decline("any()/all() of other than logicals");
        };
        let want = if nb.anyall[r] == 1 { 1 } else { 0 };
        let mut hit = vec![false; n];
        let mut na = vec![false; n];
        for &(f, b) in &rows {
            hit[f] |= x[b] == want;
            na[f] |= x[b] == NA_INT;
        }
        let out: Vec<i32> = (0..n)
            .map(|i| {
                if !seen_any[i] || (!nb.narm[r] && !hit[i] && na[i]) {
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
        let miss_g: Vec<bool> = gids.iter().map(|&id| !seen_any[id as usize - 1]).collect();
        for (r, &t) in nb.targets.iter().enumerate() {
            let Vals::Lgl(d) = &vals[r] else {
                return Err(bad("an any/all value"));
            };
            let v = Vals::Lgl(std::borrow::Cow::Owned(
                gids.iter().map(|&id| d[id as usize - 1]).collect(),
            ));
            let Some(v) = apply_nb_default(v, &miss_g, plan.nb_default)? else {
                return decline("a .default that cannot be applied natively");
            };
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
    Ok(Some(set_named(st2.get(), nb.cache_name, cache.get())?))
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

/// `abm_neighbours()` over a network natively (P4's `rk_neighbours()` shapes):
/// each rule's column stacked over the groups, folded over each agent's
/// neighbours in `nb_fold()`'s order, written to every group (by `.id` when
/// there are several), and `state$nb_cache` set as R sets it.
pub(super) fn exec_nb(plan: &Plan, nb: &NbPlan, st: SEXP) -> savvy::Result<Option<H>> {
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
        let miss_all: Vec<bool> = if plan.nb_default.is_some() {
            cnt.iter().map(|&c| c == 0).collect()
        } else {
            Vec::new()
        };
        match folded {
            Some(v) => {
                let v = if plan.nb_default.is_some() {
                    match apply_nb_default(v, &miss_all, plan.nb_default)? {
                        Some(v2) => v2,
                        None => return decline("a .default that cannot be applied natively"),
                    }
                } else {
                    v
                };
                vals.push(v);
            }
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

/// Network `abm_neighbours()` over the `(focal, neighbour)` view (FR14:
/// `own_<col>`, edge-draw columns `<name>`/`<name>_back`, `na.rm`, mixed
/// `n()`/`sum()`/`mean()`/`any()`/`all()`, [network_view()] + [neighbour_aggregate()]).
pub(super) fn exec_nb_view(plan: &Plan, st: SEXP) -> savvy::Result<Option<H>> {
    use std::borrow::Cow;
    use tidyabm_core::sample::NA_INT;
    let nbv = plan.nbv.as_ref().ok_or_else(|| bad("no nbv"))?;
    let gp = plan.groups.first().ok_or_else(|| bad("no program"))?;
    let nil = rapi_nil();
    let ef_bytes = if nbv.efield == nil {
        b"edges".as_slice()
    } else {
        bytes(nbv.efield)
    };
    let cn_bytes = if nbv.cache_name == nil {
        b"nb_cache".as_slice()
    } else {
        bytes(nbv.cache_name)
    };
    let is_rel = nbv.rel_name.is_some();
    if !is_rel && cn_bytes == b"nb_cache" {
        if let Some(si) = find_str(st, b"stale_draws")? {
            if elt(st, si) != nil {
                return decline("stale draws");
            }
        }
    }
    let e = if let Some(rn) = nbv.rel_name {
        let Some(ri) = find_str(st, b"relations")? else {
            return decline("no relations");
        };
        let rels_s = elt(st, ri);
        if rels_s == nil {
            return decline("no relations");
        }
        let Some(rk) = find(names_of(rels_s)?, rn) else {
            return decline("relation not found");
        };
        let rel = elt(rels_s, rk);
        let Some(ei) = find_str(rel, b"edges")? else {
            return decline("relation without edges");
        };
        elt(rel, ei)
    } else {
        let Some(e) = find_str(st, ef_bytes)?.map(|i| elt(st, i)) else {
            return decline("no network (R's error)");
        };
        if e == nil {
            return decline("no network (R's error)");
        }
        e
    };
    let (Some(fi_col), Some(ti_col)) = (find_str(e, b"from")?, find_str(e, b"to")?) else {
        return decline("edges without from/to");
    };
    let (ec_f, ec_t) = (elt(e, fi_col), elt(e, ti_col));
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
    let m = ef.len();
    // if any global/scope symbol is now shadowed by a drawn edge column, decline
    if !is_rel && !gp.all_targets.is_empty() {
        let enms = names_of(e)?;
        for &en in enms {
            if let Some(base) = bytes(en).strip_prefix(b".draw_") {
                for &nv in &gp.all_targets {
                    let b = bytes(nv);
                    if b == base || b.strip_suffix(b"_back") == Some(base) {
                        return decline("a drawn column shadows a global or scope name");
                    }
                }
            }
        }
    }
    // `state[[cname]]` check (same as FR13)
    let mut cached: Option<SEXP> = None;
    if !is_rel {
        if let Some(i) = find_str(st, cn_bytes)? {
            let c = elt(st, i);
            if c != nil {
                cached = find_str(c, b"edges")?.map(|j| elt(c, j));
            }
        }
    }
    let current = if is_rel {
        true
    } else {
        match cached {
            Some(ce) if ce == e => true,
            Some(ce) => {
                // SAFETY: main thread; both are protected by the state.
                let same = unsafe {
                    rapi::protect_value(0, move || rapi::R_compute_identical(ce, e, 16))
                }?;
                same != 0
            }
            None => false,
        }
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
    // SAFETY: reading a static of R's (its NA_real_, not a Rust constant).
    let na_real = unsafe { rapi::R_NaReal };
    let fast_cand = !is_rel
        && !nbv.vkind.iter().any(|&vk| vk != 0)
        && ids_are_rows(groups)?.is_some()
        && more.iter().all(|v| v.len() == 1);
    let (vals, seen_any, cache_vecs) = if fast_cand {
        // Candidate-only network/space view with dense 1..=n ids: evaluate each rule
        // once over 0..n and fold directly across (ef, et) then (et, ef).
        for r in 0..m {
            let f = ef[r];
            let b = et[r];
            if f < 1 || (f as usize) > n || b < 1 || (b as usize) > n {
                return decline("an edge endpoint outside 1..n");
            }
        }
        let mut ins: Vec<Vals> = Vec::with_capacity(gp.cols.len() + more.len());
        for &col_nm in gp.cols.iter().take(nbv.vkind.len()) {
            let Some(col) = stacked_pad(groups, col_nm, &sizes)? else {
                return decline("a candidate column of another kind");
            };
            ins.push(col);
        }
        ins.extend(more);
        let res: Vec<Vals> = if !gp.prog.is_null() {
            // SAFETY: the program is kept alive by the plan's descriptor.
            let prog = unsafe { &*gp.prog };
            match prog.compute(ins, n, n)? {
                Ok(r) => r,
                Err(d) => {
                    expr::note_decline(&d);
                    return Ok(None);
                }
            }
        } else {
            Vec::new()
        };
        let mut vals: Vec<Vals<'static>> = Vec::with_capacity(nbv.fns.len());
        let mut pi = 0;
        for (r, &f) in nbv.fns.iter().enumerate() {
            if f == 0 {
                let mut cnt = vec![0i32; n];
                for j in 0..m {
                    cnt[ef[j] as usize - 1] += 1;
                    cnt[et[j] as usize - 1] += 1;
                }
                vals.push(Vals::Int(Cow::Owned(cnt)));
                continue;
            }
            let x = recycle(res[pi].clone(), n);
            pi += 1;
            let out = match f {
                1 | 2 => {
                    let narm = nbv.narm[r];
                    match &x {
                        Vals::Dbl(xs) => {
                            let mut sum = vec![0.0f64; n];
                            let mut cnt = vec![0i32; n];
                            let mut seen = vec![false; n];
                            for (a, b) in [(ef, et), (et, ef)] {
                                for j in 0..m {
                                    let fl = a[j] as usize - 1;
                                    let cr = b[j] as usize - 1;
                                    seen[fl] = true;
                                    let v = xs[cr];
                                    if v.is_nan() {
                                        if !narm {
                                            sum[fl] += v;
                                            cnt[fl] += 1;
                                        }
                                    } else {
                                        sum[fl] += v;
                                        cnt[fl] += 1;
                                    }
                                }
                            }
                            if f == 1 {
                                Vals::Dbl(Cow::Owned(
                                    (0..n)
                                        .map(|i| if !seen[i] { na_real } else { sum[i] })
                                        .collect(),
                                ))
                            } else {
                                let empty_nan = crate::reduce::mean(&[]);
                                let mut out: Vec<f64> = (0..n)
                                    .map(|i| {
                                        if !seen[i] {
                                            na_real
                                        } else if cnt[i] == 0 {
                                            empty_nan
                                        } else {
                                            sum[i] / f64::from(cnt[i])
                                        }
                                    })
                                    .collect();
                                if out.iter().any(|v| v.is_finite()) {
                                    let mut resid = vec![0.0f64; n];
                                    for (a, b) in [(ef, et), (et, ef)] {
                                        for j in 0..m {
                                            let fl = a[j] as usize - 1;
                                            if !out[fl].is_finite() {
                                                continue;
                                            }
                                            let v = xs[b[j] as usize - 1];
                                            if !(narm && v.is_nan()) {
                                                resid[fl] += v - out[fl];
                                            }
                                        }
                                    }
                                    for i in 0..n {
                                        if out[i].is_finite() {
                                            out[i] += resid[i] / f64::from(cnt[i]);
                                        }
                                    }
                                }
                                Vals::Dbl(Cow::Owned(out))
                            }
                        }
                        Vals::Int(xs) | Vals::Lgl(xs) => {
                            let mut sum = vec![0i64; n];
                            let mut cnt = vec![0i32; n];
                            let mut has_na = vec![false; n];
                            let mut seen = vec![false; n];
                            let mut ovf = vec![false; n];
                            for (a, b) in [(ef, et), (et, ef)] {
                                for j in 0..m {
                                    let fl = a[j] as usize - 1;
                                    let cr = b[j] as usize - 1;
                                    seen[fl] = true;
                                    let v = xs[cr];
                                    if v == NA_INT {
                                        if !narm {
                                            has_na[fl] = true;
                                        }
                                    } else {
                                        sum[fl] += i64::from(v);
                                        if sum[fl].abs() > i64::from(i32::MAX) {
                                            ovf[fl] = true;
                                        }
                                        cnt[fl] += 1;
                                    }
                                }
                            }
                            for i in 0..n {
                                if seen[i] && !has_na[i] && ovf[i] {
                                    return decline("an integer sum outside the range");
                                }
                            }
                            if f == 1 {
                                Vals::Int(Cow::Owned(
                                    (0..n)
                                        .map(|i| {
                                            if !seen[i] || has_na[i] {
                                                NA_INT
                                            } else {
                                                sum[i] as i32
                                            }
                                        })
                                        .collect(),
                                ))
                            } else {
                                let empty_nan = crate::reduce::mean_int(&[]);
                                Vals::Dbl(Cow::Owned(
                                    (0..n)
                                        .map(|i| {
                                            if !seen[i] || has_na[i] {
                                                na_real
                                            } else if cnt[i] == 0 {
                                                empty_nan
                                            } else {
                                                sum[i] as f64 / f64::from(cnt[i])
                                            }
                                        })
                                        .collect(),
                                ))
                            }
                        }
                        _ => return decline("a view aggregate of strings"),
                    }
                }
                3 | 4 => {
                    let Vals::Lgl(xs) = &x else {
                        return decline("any()/all() of other than logicals");
                    };
                    let want = if f == 3 { 1 } else { 0 };
                    let narm = nbv.narm[r];
                    let mut seen = vec![false; n];
                    let mut hit = vec![false; n];
                    let mut na = vec![false; n];
                    for (a, b) in [(ef, et), (et, ef)] {
                        for j in 0..m {
                            let fl = a[j] as usize - 1;
                            let v = xs[b[j] as usize - 1];
                            seen[fl] = true;
                            hit[fl] |= v == want;
                            na[fl] |= v == NA_INT;
                        }
                    }
                    Vals::Lgl(Cow::Owned(
                        (0..n)
                            .map(|i| {
                                if !seen[i] || (!narm && !hit[i] && na[i]) {
                                    NA_INT
                                } else {
                                    i32::from(if want == 1 { hit[i] } else { !hit[i] })
                                }
                            })
                            .collect(),
                    ))
                }
                _ => return Err(bad("nbv fn")),
            };
            vals.push(out);
        }
        let mut seen_any = vec![false; n];
        for j in 0..m {
            seen_any[ef[j] as usize - 1] = true;
            seen_any[et[j] as usize - 1] = true;
        }
        let cv = if current {
            None
        } else {
            Some(([ef, et].concat(), [et, ef].concat()))
        };
        (vals, seen_any, cv)
    } else {
        let (focal, nbr): (Vec<i32>, Vec<i32>) = if is_rel {
            if nbv.rel_back {
                (et.to_vec(), ef.to_vec())
            } else {
                (ef.to_vec(), et.to_vec())
            }
        } else {
            ([ef, et].concat(), [et, ef].concat())
        };
        let total_rows = focal.len();
        // map endpoints to stacked row indices 0..n
        let mut fi: Vec<usize> = Vec::with_capacity(total_rows);
        let mut ci: Vec<usize> = Vec::with_capacity(total_rows);
        let mut ei_v: Vec<usize> = Vec::with_capacity(total_rows);
        let mut fwd_v: Vec<bool> = Vec::with_capacity(total_rows);
        let mut all_ids_owned: Vec<i32> = Vec::new();
        let need_of = nbv.vkind.contains(&4);
        if !is_rel && ids_are_rows(groups)?.is_some() {
            for r in 0..total_rows {
                let f = focal[r];
                let b = nbr[r];
                if f < 1 || (f as usize) > n || b < 1 || (b as usize) > n {
                    return decline("an edge endpoint outside 1..n");
                }
                fi.push(f as usize - 1);
                ci.push(b as usize - 1);
                ei_v.push(if r < m { r } else { r - m });
                fwd_v.push(r < m);
            }
            if need_of {
                all_ids_owned = (1..=n as i32).collect();
            }
        } else {
            let Some(Vals::Int(ids)) = stacked_col_named(groups, b".id", &sizes)? else {
                return decline(".id not integer");
            };
            let idx = IdIndex::new(&ids);
            for r in 0..total_rows {
                if let (Some(fr), Some(cr)) = (idx.get(focal[r]), idx.get(nbr[r])) {
                    fi.push(fr);
                    ci.push(cr);
                    ei_v.push(if is_rel || r < m { r } else { r - m });
                    fwd_v.push(is_rel || r < m);
                }
            }
            if need_of {
                all_ids_owned = ids.into_owned();
            }
        }
        let nv = fi.len();
        if is_rel && nv == 0 && nbv.fns.iter().any(|&f| f != 0) {
            return decline("empty relation view");
        }
        let enms = names_of(e)?;
        let mut ins: Vec<Vals> = Vec::with_capacity(gp.cols.len() + more.len());
        for (idx, &vk) in nbv.vkind.iter().enumerate() {
            match vk {
                0 => {
                    let Some(col) = stacked_pad(groups, gp.cols[idx], &sizes)? else {
                        return decline("a candidate column of another kind");
                    };
                    ins.push(tidyabm_core::expr::take(&col, &ci));
                }
                1 => {
                    let Some(col) = stacked_pad(groups, gp.cols[idx], &sizes)? else {
                        return decline("an own_ column of another kind");
                    };
                    ins.push(tidyabm_core::expr::take(&col, &fi));
                }
                2 | 3 => {
                    let (Some(di), Some(bi)) =
                        (find(enms, gp.cols[idx]), find(enms, nbv.vback[idx]))
                    else {
                        return decline("a drawn edge column not found");
                    };
                    let (dc, bc) = (elt(e, di), elt(e, bi));
                    if !all_bare(&[dc, bc])? || len(dc) != m || len(bc) != m {
                        return decline("a drawn edge column with attributes or wrong length");
                    }
                    let (Some(dv), Some(bv)) =
                        (expr::vals_of(dc, false)?, expr::vals_of(bc, false)?)
                    else {
                        return decline("a drawn edge column of another type");
                    };
                    let is_fwd = vk == 2;
                    let picked = match (&dv, &bv) {
                        (Vals::Dbl(d), Vals::Dbl(b)) => Vals::Dbl(Cow::Owned(
                            (0..nv)
                                .map(|r| {
                                    if fwd_v[r] == is_fwd {
                                        d[ei_v[r]]
                                    } else {
                                        b[ei_v[r]]
                                    }
                                })
                                .collect(),
                        )),
                        (Vals::Int(d), Vals::Int(b)) => Vals::Int(Cow::Owned(
                            (0..nv)
                                .map(|r| {
                                    if fwd_v[r] == is_fwd {
                                        d[ei_v[r]]
                                    } else {
                                        b[ei_v[r]]
                                    }
                                })
                                .collect(),
                        )),
                        (Vals::Lgl(d), Vals::Lgl(b)) => Vals::Lgl(Cow::Owned(
                            (0..nv)
                                .map(|r| {
                                    if fwd_v[r] == is_fwd {
                                        d[ei_v[r]]
                                    } else {
                                        b[ei_v[r]]
                                    }
                                })
                                .collect(),
                        )),
                        (Vals::Str(d), Vals::Str(b)) => Vals::Str(Cow::Owned(
                            (0..nv)
                                .map(|r| {
                                    if fwd_v[r] == is_fwd {
                                        d[ei_v[r]]
                                    } else {
                                        b[ei_v[r]]
                                    }
                                })
                                .collect(),
                        )),
                        _ => return decline("drawn forward and back columns of different types"),
                    };
                    ins.push(picked);
                }
                4 => {
                    ins.push(Vals::Int(Cow::Owned(
                        fi.iter().map(|&r| all_ids_owned[r]).collect(),
                    )));
                }
                5 => {
                    let Some(ci_col) = find(enms, gp.cols[idx]) else {
                        return decline("a relation column not found");
                    };
                    let csexp = elt(e, ci_col);
                    if !all_bare(&[csexp])? || len(csexp) != m {
                        return decline("a relation column with attributes or wrong length");
                    }
                    let Some(cv) = expr::vals_of(csexp, false)? else {
                        return decline("a relation column of another type");
                    };
                    ins.push(tidyabm_core::expr::take(&cv, &ei_v));
                }
                6 => {
                    ins.push(Vals::Lgl(Cow::Owned(vec![1i32; nv])));
                }
                _ => return Err(bad("nbv input kind")),
            }
        }
        ins.extend(more);
        let res: Vec<Vals> = if !gp.prog.is_null() {
            // SAFETY: the program is kept alive by the plan's descriptor.
            let prog = unsafe { &*gp.prog };
            match prog.compute(ins, nv, nv)? {
                Ok(r) => r,
                Err(d) => {
                    expr::note_decline(&d);
                    return Ok(None);
                }
            }
        } else {
            Vec::new()
        };
        let mut vals: Vec<Vals<'static>> = Vec::with_capacity(nbv.fns.len());
        let mut pi = 0;
        for (r, &f) in nbv.fns.iter().enumerate() {
            if f == 0 {
                let mut cnt = vec![0i32; n];
                for &fl in &fi {
                    cnt[fl] += 1;
                }
                vals.push(Vals::Int(Cow::Owned(cnt)));
                continue;
            }
            let x = recycle(res[pi].clone(), nv);
            pi += 1;
            let out = match f {
                1 | 2 => {
                    let narm = nbv.narm[r];
                    match &x {
                        Vals::Dbl(xs) => {
                            let mut sum = vec![0.0f64; n];
                            let mut cnt = vec![0i32; n];
                            let mut seen = vec![false; n];
                            for j in 0..nv {
                                let fl = fi[j];
                                seen[fl] = true;
                                let v = xs[j];
                                if v.is_nan() {
                                    if !narm {
                                        sum[fl] += v;
                                        cnt[fl] += 1;
                                    }
                                } else {
                                    sum[fl] += v;
                                    cnt[fl] += 1;
                                }
                            }
                            if f == 1 {
                                Vals::Dbl(Cow::Owned(
                                    (0..n)
                                        .map(|i| if !seen[i] { na_real } else { sum[i] })
                                        .collect(),
                                ))
                            } else {
                                let empty_nan = crate::reduce::mean(&[]);
                                let mut out: Vec<f64> = (0..n)
                                    .map(|i| {
                                        if !seen[i] {
                                            na_real
                                        } else if cnt[i] == 0 {
                                            empty_nan
                                        } else {
                                            sum[i] / f64::from(cnt[i])
                                        }
                                    })
                                    .collect();
                                // Base R's two-pass mean() residual correction, matching neighbour_aggregate()
                                if out.iter().any(|v| v.is_finite()) {
                                    let mut resid = vec![0.0f64; n];
                                    for j in 0..nv {
                                        let fl = fi[j];
                                        if !out[fl].is_finite() {
                                            continue;
                                        }
                                        let v = xs[j];
                                        if !(narm && v.is_nan()) {
                                            resid[fl] += v - out[fl];
                                        }
                                    }
                                    for i in 0..n {
                                        if out[i].is_finite() {
                                            out[i] += resid[i] / f64::from(cnt[i]);
                                        }
                                    }
                                }
                                Vals::Dbl(Cow::Owned(out))
                            }
                        }
                        Vals::Int(xs) | Vals::Lgl(xs) => {
                            let mut sum = vec![0i64; n];
                            let mut cnt = vec![0i32; n];
                            let mut has_na = vec![false; n];
                            let mut seen = vec![false; n];
                            let mut ovf = vec![false; n];
                            for j in 0..nv {
                                let fl = fi[j];
                                seen[fl] = true;
                                let v = xs[j];
                                if v == NA_INT {
                                    if !narm {
                                        has_na[fl] = true;
                                    }
                                } else {
                                    sum[fl] += i64::from(v);
                                    if sum[fl].abs() > i64::from(i32::MAX) {
                                        ovf[fl] = true;
                                    }
                                    cnt[fl] += 1;
                                }
                            }
                            for i in 0..n {
                                if seen[i] && !has_na[i] && ovf[i] {
                                    return decline("an integer sum outside the range");
                                }
                            }
                            if f == 1 {
                                Vals::Int(Cow::Owned(
                                    (0..n)
                                        .map(|i| {
                                            if !seen[i] || has_na[i] {
                                                NA_INT
                                            } else {
                                                sum[i] as i32
                                            }
                                        })
                                        .collect(),
                                ))
                            } else {
                                let empty_nan = crate::reduce::mean_int(&[]);
                                Vals::Dbl(Cow::Owned(
                                    (0..n)
                                        .map(|i| {
                                            if !seen[i] || has_na[i] {
                                                na_real
                                            } else if cnt[i] == 0 {
                                                empty_nan
                                            } else {
                                                sum[i] as f64 / f64::from(cnt[i])
                                            }
                                        })
                                        .collect(),
                                ))
                            }
                        }
                        _ => return decline("a view aggregate of strings"),
                    }
                }
                3 | 4 => {
                    let Vals::Lgl(xs) = &x else {
                        return decline("any()/all() of other than logicals");
                    };
                    let want = if f == 3 { 1 } else { 0 };
                    let narm = nbv.narm[r];
                    let mut seen = vec![false; n];
                    let mut hit = vec![false; n];
                    let mut na = vec![false; n];
                    for j in 0..nv {
                        let fl = fi[j];
                        seen[fl] = true;
                        hit[fl] |= xs[j] == want;
                        na[fl] |= xs[j] == NA_INT;
                    }
                    Vals::Lgl(Cow::Owned(
                        (0..n)
                            .map(|i| {
                                if !seen[i] || (!narm && !hit[i] && na[i]) {
                                    NA_INT
                                } else {
                                    i32::from(if want == 1 { hit[i] } else { !hit[i] })
                                }
                            })
                            .collect(),
                    ))
                }
                _ => return Err(bad("nbv fn")),
            };
            vals.push(out);
        }
        let mut seen_any = vec![false; n];
        for &fl in &fi {
            seen_any[fl] = true;
        }
        let cv = if current { None } else { Some((focal, nbr)) };
        (vals, seen_any, cv)
    };
    let mut cur: Option<H> = None;
    for (r, v) in vals.iter().enumerate() {
        let t = gp.targets[r];
        let mut start = 0;
        for (g, &sz) in sizes.iter().enumerate() {
            let rows_g: Vec<usize> = (start..start + sz).collect();
            start += sz;
            if sz == 0 {
                continue;
            }
            let col = expr::owned(tidyabm_core::expr::take(v, &rows_g));
            let miss_g: Vec<bool> = rows_g.iter().map(|&i| !seen_any[i]).collect();
            let Some(col) = apply_nb_default(col, &miss_g, plan.nb_default)? else {
                return decline("a .default that cannot be applied natively");
            };
            let gs = cur.as_ref().map_or(groups, |h| h.get());
            let base = elt(gs, g);
            let mut hold = OwnedListSexp::new(1, false)?;
            hold.set_value(0, expr::to_r(&col, sz)?)?;
            let g2 = set_named(base, t, elt(hold.inner(), 0))?;
            // SAFETY: reading a static of R's (unused name: `g` is in range).
            cur = Some(with_elt(gs, g, g2.get(), unsafe { rapi::R_BlankString })?);
        }
    }
    let st2 = match &cur {
        // SAFETY: as above.
        Some(gs) => with_elt(st, gi, gs.get(), unsafe { rapi::R_BlankString })?,
        None => keep(st)?,
    };
    let Some((focal, nbr)) = cache_vecs else {
        return Ok(Some(st2));
    };
    let cache = nb_cache_of(e, &focal, &nbr, m)?;
    let key = if nbv.cache_name == nil {
        // SAFETY: making the CHARSXP may allocate; kept at once.
        Some(unsafe { build(|| rapi::Rf_mkChar(c"nb_cache".as_ptr())) }?)
    } else {
        None
    };
    let key_s = key.as_ref().map_or(nbv.cache_name, |h| h.get());
    Ok(Some(set_named(st2.get(), key_s, cache.get())?))
}
