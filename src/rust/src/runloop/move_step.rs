use super::*;

/// `lat$x[cell]` (or `lat$y`) as a new R vector of `xy`'s type (integer or double).
pub(super) fn coords(xy: SEXP, cell: &[i32]) -> savvy::Result<Option<Vals<'static>>> {
    use std::borrow::Cow;
    Ok(match expr::vals_of(xy, false)? {
        Some(Vals::Int(v)) => Some(Vals::Int(Cow::Owned(
            cell.iter().map(|&c| v[c as usize - 1]).collect(),
        ))),
        Some(Vals::Dbl(v)) => Some(Vals::Dbl(Cow::Owned(
            cell.iter().map(|&c| v[c as usize - 1]).collect(),
        ))),
        _ => None,
    })
}

/// `abm_move()` natively ([run_move()]): `stay`, continuous `offset()`/`forward()`,
/// or grid `random_neighbour` / `random_empty_neighbour` / `random_empty` /
/// `random_cell` / `direction` / `uphill` / `downhill`, with optional `when =`
/// and per-agent `range = <expr>`.
pub(super) fn exec_move(plan: &Plan, st: SEXP) -> savvy::Result<Option<H>> {
    use std::borrow::Cow;
    use tidyabm_core::sample::NA_INT;
    let mv = plan.mv.as_ref().ok_or_else(|| bad("no move"))?;
    if mv.mode == 1 {
        return Ok(Some(keep(st)?));
    }
    let gi = find_str(st, b"groups")?.ok_or_else(|| bad("no groups"))?;
    let groups = elt(st, gi);
    let gnames = names_of(groups)?;

    // Build a shallow list of the moving groups (`who_groups`) so stacked `when`
    // evaluation matches `move_when_masks()` across single or multiple groups.
    let mut ks = Vec::with_capacity(mv.who.len());
    for &w in &mv.who {
        let Some(k) = find(gnames, w) else {
            return decline("a moving group not found");
        };
        ks.push(k);
    }
    let eval_when = |svals: &Option<H>,
                     sizes: &[usize],
                     cells_flat: Option<&[i32]>|
     -> savvy::Result<Option<Vec<bool>>> {
        let n_all: usize = sizes.iter().sum();
        let wgp = plan
            .groups
            .iter()
            .find(|e| e.part == 5)
            .ok_or_else(|| bad("no move when program"))?;
        let mut wg_list = OwnedListSexp::new(ks.len(), false)?;
        for (p, &k) in ks.iter().enumerate() {
            wg_list.set_value(p, Sexp(elt(groups, k)))?;
        }
        let wg_sexp = wg_list.inner();
        let mut ins: Vec<Vals> = Vec::with_capacity(
            wgp.cols.len() + wgp.globs.len() + wgp.scope.len() + wgp.mcols.len(),
        );
        for &c in &wgp.cols {
            match stacked_pad(wg_sexp, c, sizes)? {
                Some(v) => ins.push(v),
                None => return Ok(None),
            }
        }
        let mut xs = Vec::new();
        if !read_globals(st, &wgp.globs, &mut xs)? || !read_scope(svals, &wgp.scope, &mut xs) {
            return Ok(None);
        }
        let Some(more) = inputs(&xs)? else {
            return Ok(None);
        };
        ins.extend(more);
        for &mc in &wgp.mcols {
            let Some(ccol) = bytes(mc).strip_prefix(b"cell_") else {
                return Ok(None);
            };
            let Some(cf) = cells_flat else {
                return Ok(None);
            };
            let Some(v) = cell_col_for_cells(st, groups, cf, ccol)? else {
                return Ok(None);
            };
            ins.push(v);
        }
        // SAFETY: the program is kept alive by the plan's descriptor.
        let prog = unsafe { &*wgp.prog };
        let res = match prog.compute(ins, n_all, n_all)? {
            Ok(r) => r,
            Err(d) => {
                expr::note_decline(&d);
                return Ok(None);
            }
        };
        Ok(condition(&res[0], n_all))
    };

    // Continuous-space movement: `offset()` / `forward()`
    if mv.mode == 5 || mv.mode == 6 {
        match find_str(st, b"space")? {
            Some(si) if elt(st, si) == mv.space => {}
            _ => return decline("the space changed"),
        }
        let nd = mv.dims.len();
        if (nd != 1 && nd != 2) || mv.torus.len() != nd {
            return decline("unsupported continuous space dims");
        }
        let mut sizes = Vec::with_capacity(ks.len());
        for &k in &ks {
            let Some(r) = rows(elt(groups, k)) else {
                return decline("a moving group without columns");
            };
            sizes.push(r);
        }
        let n_all: usize = sizes.iter().sum();
        if n_all == 0 {
            return Ok(Some(keep(st)?));
        }
        let Some(svals) = scope_values(plan)? else {
            return decline("a formula-environment value that cannot be read");
        };
        let when_all: Option<Vec<bool>> = if mv.has_when {
            match eval_when(&svals, &sizes, None)? {
                Some(w) => Some(w),
                None => return decline("move when failed natively"),
            }
        } else {
            None
        };
        let wrap_or_clamp = |v: f64, l: f64, wrap: bool| -> f64 {
            if v.is_nan() {
                v
            } else if wrap {
                v - (v / l).floor() * l
            } else {
                v.clamp(0.0, l)
            }
        };
        let to_dbl = |v: &Vals, k: usize| -> Option<Vec<f64>> {
            // SAFETY: reading a static of R's.
            let na_real = unsafe { rapi::R_NaReal };
            let pick = |i: usize| if v.len() == 1 { 0 } else { i };
            match v {
                Vals::Dbl(x) => Some((0..k).map(|i| x[pick(i)]).collect()),
                Vals::Int(x) | Vals::Lgl(x) => Some(
                    (0..k)
                        .map(|i| {
                            let val = x[pick(i)];
                            if val == NA_INT {
                                na_real
                            } else {
                                f64::from(val)
                            }
                        })
                        .collect(),
                ),
                Vals::Str(_) => None,
            }
        };
        let mut cur: Option<H> = None;
        let mut off = 0usize;
        for (p, &k) in ks.iter().enumerate() {
            let sz = sizes[p];
            let mask_slice = when_all.as_ref().map(|w| &w[off..off + sz]);
            off += sz;
            if sz == 0 {
                continue;
            }
            let active: Vec<usize> = match mask_slice {
                Some(m) => (0..sz).filter(|&i| m[i]).collect(),
                None => (0..sz).collect(),
            };
            if active.is_empty() {
                continue;
            }
            let gs = cur.as_ref().map_or(groups, |h| h.get());
            let g = elt(gs, k);
            let gp = plan
                .groups
                .iter()
                .find(|e| e.part == 0 && e.name == mv.who[p])
                .ok_or_else(|| bad("no continuous move program"))?;
            let sub = if active.len() < sz {
                Some(active.as_slice())
            } else {
                None
            };
            let Some(ins) = entry_inputs(st, groups, g, gp, &svals, sub)? else {
                return decline("continuous move inputs that cannot be built natively");
            };
            // SAFETY: the program is kept alive by the plan's descriptor.
            let prog = unsafe { &*gp.prog };
            let n_eval = active.len();
            let res = match prog.compute(ins, n_eval, n_eval)? {
                Ok(r) => r,
                Err(d) => {
                    expr::note_decline(&d);
                    return Ok(None);
                }
            };
            let (dx, dy): (Vec<f64>, Option<Vec<f64>>) = if mv.mode == 5 {
                let Some(dx) = to_dbl(&res[0], n_eval) else {
                    return decline("non-numeric offset dx");
                };
                let dy = if nd == 2 {
                    let Some(d) = to_dbl(&res[1], n_eval) else {
                        return decline("non-numeric offset dy");
                    };
                    Some(d)
                } else {
                    None
                };
                (dx, dy)
            } else {
                let (Some(by), Some(hd)) = (to_dbl(&res[0], n_eval), to_dbl(&res[1], n_eval))
                else {
                    return decline("non-numeric forward args");
                };
                let dx: Vec<f64> = by.iter().zip(&hd).map(|(&b, &h)| b * h.cos()).collect();
                let dy = if nd == 2 {
                    Some(by.iter().zip(&hd).map(|(&b, &h)| b * h.sin()).collect())
                } else {
                    None
                };
                (dx, dy)
            };
            let Some(xj) = find_str(g, b".x")? else {
                return decline("missing .x");
            };
            let Some(old_x) = store::numbers_of::<f64>(elt(g, xj), rapi::REALSXP)? else {
                return decline(".x that is not double");
            };
            let mut nx = old_x.to_vec();
            for (q, &row_i) in active.iter().enumerate() {
                nx[row_i] = wrap_or_clamp(nx[row_i] + dx[q], mv.dims[0], mv.torus[0]);
            }
            let mut hold = OwnedListSexp::new(2, false)?;
            hold.set_value(0, expr::to_r(&Vals::Dbl(Cow::Owned(nx)), sz)?)?;
            // SAFETY: main thread; mkChar kept in handle.
            let xn = unsafe { build(|| rapi::Rf_mkChar(c".x".as_ptr())) }?;
            let g1 = set_named(g, xn.get(), elt(hold.inner(), 0))?;
            let g2 = if let Some(dy) = dy {
                let Some(yj) = find_str(g, b".y")? else {
                    return decline("missing .y");
                };
                let Some(old_y) = store::numbers_of::<f64>(elt(g, yj), rapi::REALSXP)? else {
                    return decline(".y that is not double");
                };
                let mut ny = old_y.to_vec();
                for (q, &row_i) in active.iter().enumerate() {
                    ny[row_i] = wrap_or_clamp(ny[row_i] + dy[q], mv.dims[1], mv.torus[1]);
                }
                hold.set_value(1, expr::to_r(&Vals::Dbl(Cow::Owned(ny)), sz)?)?;
                // SAFETY: main thread; mkChar kept in handle.
                let yn = unsafe { build(|| rapi::Rf_mkChar(c".y".as_ptr())) }?;
                set_named(g1.get(), yn.get(), elt(hold.inner(), 1))?
            } else {
                g1
            };
            // SAFETY: reading a static of R's (unused name: `k` is in range).
            cur = Some(with_elt(gs, k, g2.get(), unsafe { rapi::R_BlankString })?);
        }
        return match cur {
            // SAFETY: as above.
            Some(gs) => Ok(Some(with_elt(st, gi, gs.get(), unsafe {
                rapi::R_BlankString
            })?)),
            None => Ok(Some(keep(st)?)),
        };
    }

    match find_str(st, b"lattice")? {
        Some(li) if elt(st, li) == mv.lattice => {}
        _ => return decline("the lattice changed"),
    }
    // the moving groups' cells
    let mut cells: Vec<Vec<i32>> = Vec::with_capacity(mv.who.len());
    for &k in &ks {
        let g = elt(groups, k);
        let Some(j) = find_str(g, b".cell")? else {
            return decline("a moving group without .cell");
        };
        match store::numbers_of::<i32>(elt(g, j), INTSXP)? {
            Some(v) => cells.push(v.to_vec()),
            None => return decline("a .cell that is not integer"),
        }
    }
    let sizes: Vec<usize> = cells.iter().map(Vec::len).collect();
    let n_all: usize = sizes.iter().sum();
    if n_all == 0 {
        return Ok(Some(keep(st)?));
    }
    let Some(svals) = scope_values(plan)? else {
        return decline("a formula-environment value that cannot be read");
    };
    let when_all: Option<Vec<bool>> = if mv.has_when {
        let cells_flat: Vec<i32> = cells.iter().flatten().copied().collect();
        match eval_when(&svals, &sizes, Some(&cells_flat))? {
            Some(w) => Some(w),
            None => return decline("move when failed natively"),
        }
    } else {
        None
    };
    // (group, row) of every active mover
    let mut owner: Vec<(usize, usize)> = Vec::with_capacity(n_all);
    let mut off = 0usize;
    for (gk, &sz) in sizes.iter().enumerate() {
        for r in 0..sz {
            if when_all.as_ref().map_or(true, |w| w[off + r]) {
                owner.push((gk, r));
            }
        }
        off += sz;
    }
    let n_movers = owner.len();
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
    // per-agent `range = <expr>` if supplied
    let mut ranges: Vec<Vec<i32>> = Vec::new();
    if mv.has_range {
        for (p, &k) in ks.iter().enumerate() {
            let g = elt(groups, k);
            let gp = plan
                .groups
                .iter()
                .find(|e| e.part == 6 && e.name == mv.who[p])
                .ok_or_else(|| bad("no range program"))?;
            let ng = sizes[p];
            if ng == 0 {
                ranges.push(Vec::new());
                continue;
            }
            let Some(ins) = entry_inputs(st, groups, g, gp, &svals, None)? else {
                return decline("range inputs that cannot be built natively");
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
            let rv: Vec<i32> = match &v {
                Vals::Int(x) => x.to_vec(),
                Vals::Dbl(x) => x
                    .iter()
                    .map(|&d| {
                        let r = d.round_ties_even();
                        if r.is_nan() || !(-2147483647.0..=2147483647.0).contains(&r) {
                            NA_INT
                        } else {
                            r as i32
                        }
                    })
                    .collect(),
                _ => return decline("a range that is not numeric (R's error)"),
            };
            ranges.push(rv);
        }
    }
    // `direction =`: each mover's side of the lattice (`heading_names()`), as an
    // index into `dir_nb`, `None` for `NA`
    let is_dir = mv.mode == 3;
    let mut heading: Vec<Vec<Option<usize>>> = Vec::new();
    let mut dir_vecs: Vec<&[i32]> = Vec::new();
    let mut axis_vecs: Vec<&[i32]> = Vec::new();
    if is_dir || mv.axes {
        let dn: Vec<&[u8]> = names_of(mv.dir)?.iter().map(|&c| bytes(c)).collect();
        for k in 0..len(mv.dir) {
            match store::numbers_of::<i32>(elt(mv.dir, k), INTSXP)? {
                Some(v) => dir_vecs.push(v),
                None => return decline("a direction table that is not integer"),
            }
        }
        if mv.axes {
            for want in [b"north".as_slice(), b"south", b"east", b"west"] {
                if let Some(pos) = dn.iter().position(|&d| d == want) {
                    axis_vecs.push(dir_vecs[pos]);
                }
            }
        }
        if is_dir {
            let compass: [&[u8]; 4] = [b"north", b"east", b"south", b"west"];
            for (p, &k) in ks.iter().enumerate() {
                let g = elt(groups, k);
                let gp = plan
                    .groups
                    .iter()
                    .find(|e| e.part == 0 && e.name == mv.who[p])
                    .ok_or_else(|| bad("no heading program"))?;
                let ng = sizes[p];
                if ng == 0 {
                    heading.push(Vec::new());
                    continue;
                }
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
    }
    // `uphill()` / `downhill()` (FR14): prepare wired index and per-group inputs
    // before opening the RNG scope so any type/attribute decline happens cleanly
    struct GradGroup<'a> {
        prog: &'a RxProgram,
        /// `(vkind, col)`: 0 = patch col, 1 = mover col, 2 = `.dist` (None)
        cols: Vec<(u8, Option<Vals<'a>>)>,
        rest: Vec<Vals<'a>>,
        mover_x: Option<Vec<f64>>,
        mover_y: Option<Vec<f64>>,
    }
    let mut widx: Option<IdIndex> = None;
    let mut grad_groups: Vec<GradGroup> = Vec::new();
    let mut patch_xy: Option<(Vec<f64>, Option<Vec<f64>>)> = None;
    if mv.grad != 0 {
        let wired_info = match mv.on {
            Some(on) => {
                let Some(wk) = find(gnames, on) else {
                    return decline("wired group not found");
                };
                let wired = elt(groups, wk);
                if rows(wired).unwrap_or(0) == 0 {
                    return decline("empty wired group (R's error)");
                }
                let Some(wj) = find_str(wired, b".id")? else {
                    return decline("wired group without .id");
                };
                let Some(wids) = store::numbers_of::<i32>(elt(wired, wj), INTSXP)? else {
                    return decline("wired .id not integer");
                };
                widx = Some(IdIndex::new(wids));
                Some((wired, names_of(wired)?))
            }
            None => {
                if mv.ncell <= 0 {
                    return decline("empty lattice for gradient move");
                }
                None
            }
        };
        let ncell_u = mv.ncell.max(0) as usize;
        // SAFETY: reading a static of R's.
        let na_real = unsafe { rapi::R_NaReal };
        let need_dist = plan
            .groups
            .iter()
            .any(|e| e.part == 0 && e.wslot.contains(&2));
        if need_dist {
            let (px_s, py_s) = match wired_info {
                Some((wired, wnames)) => {
                    let Some(xi) = wnames.iter().position(|&c| bytes(c) == b".x") else {
                        return decline("wired group without .x for .dist");
                    };
                    let yi = wnames.iter().position(|&c| bytes(c) == b".y");
                    (elt(wired, xi), yi.map(|i| elt(wired, i)))
                }
                None => (mv.x, (mv.y != rapi_nil()).then_some(mv.y)),
            };
            if !all_bare(&[px_s])? {
                return decline("patch .x with attributes");
            }
            let Some(vx) = expr::vals_of(px_s, false)? else {
                return decline("patch .x of another type");
            };
            let Some(px) = vals_as_f64(&vx, na_real) else {
                return decline("patch .x not numeric");
            };
            let py = match py_s {
                Some(ys) if mv.dims.len() >= 2 => {
                    if !all_bare(&[ys])? {
                        return decline("patch .y with attributes");
                    }
                    let Some(vy) = expr::vals_of(ys, false)? else {
                        return decline("patch .y of another type");
                    };
                    let Some(py) = vals_as_f64(&vy, na_real) else {
                        return decline("patch .y not numeric");
                    };
                    Some(py)
                }
                _ => None,
            };
            patch_xy = Some((px, py));
        }
        for (p, &k) in ks.iter().enumerate() {
            let g = elt(groups, k);
            let mnames = names_of(g)?;
            let gp = plan
                .groups
                .iter()
                .find(|e| e.part == 0 && e.name == mv.who[p])
                .ok_or_else(|| bad("no gradient program"))?;
            if gp.wslot.len() != gp.cols.len() {
                return Err(bad("gradient column kinds"));
            }
            let mut cols = Vec::with_capacity(gp.cols.len());
            for (j, &c) in gp.cols.iter().enumerate() {
                let vk = gp.wslot[j] as u8;
                if vk == 2 {
                    cols.push((2, None));
                    continue;
                }
                let is_mover = vk != 0;
                let v = if is_mover {
                    let Some(ci) = find(mnames, c) else {
                        return decline("a gradient column not found");
                    };
                    let col = elt(g, ci);
                    if !all_bare(&[col])? {
                        return decline("a gradient column with attributes");
                    }
                    let Some(v) = expr::vals_of(col, false)? else {
                        return decline("a gradient column of another type");
                    };
                    v
                } else if let Some((wired, wnames)) = wired_info {
                    let Some(ci) = find(wnames, c) else {
                        return decline("a gradient column not found");
                    };
                    let col = elt(wired, ci);
                    if !all_bare(&[col])? {
                        return decline("a gradient column with attributes");
                    }
                    let Some(v) = expr::vals_of(col, false)? else {
                        return decline("a gradient column of another type");
                    };
                    v
                } else {
                    match bytes(c) {
                        b".id" => Vals::Int(std::borrow::Cow::Owned((1..=mv.ncell).collect())),
                        b".x" => {
                            if !all_bare(&[mv.x])? || len(mv.x) != ncell_u {
                                return decline("lattice .x of another kind");
                            }
                            let Some(v) = expr::vals_of(mv.x, false)? else {
                                return decline("lattice .x of another type");
                            };
                            v
                        }
                        b".y" if mv.y != rapi_nil() => {
                            if !all_bare(&[mv.y])? || len(mv.y) != ncell_u {
                                return decline("lattice .y of another kind");
                            }
                            let Some(v) = expr::vals_of(mv.y, false)? else {
                                return decline("lattice .y of another type");
                            };
                            v
                        }
                        _ => return decline("a patchless gradient column not found"),
                    }
                };
                cols.push((vk, Some(v)));
            }
            let (mover_x, mover_y) = if gp.wslot.contains(&2) {
                let Some(xi) = mnames.iter().position(|&c| bytes(c) == b".x") else {
                    return decline("mover without .x for .dist");
                };
                let xs_c = elt(g, xi);
                if !all_bare(&[xs_c])? {
                    return decline("mover .x with attributes");
                }
                let Some(vx) = expr::vals_of(xs_c, false)? else {
                    return decline("mover .x of another type");
                };
                let Some(mx) = vals_as_f64(&vx, na_real) else {
                    return decline("mover .x not numeric");
                };
                let my = if mv.dims.len() >= 2 {
                    match mnames.iter().position(|&c| bytes(c) == b".y") {
                        Some(yi) => {
                            let ys_c = elt(g, yi);
                            if !all_bare(&[ys_c])? {
                                return decline("mover .y with attributes");
                            }
                            let Some(vy) = expr::vals_of(ys_c, false)? else {
                                return decline("mover .y of another type");
                            };
                            let Some(my) = vals_as_f64(&vy, na_real) else {
                                return decline("mover .y not numeric");
                            };
                            Some(my)
                        }
                        None => None,
                    }
                } else {
                    None
                };
                (Some(mx), my)
            } else {
                (None, None)
            };
            let mut xs = Vec::new();
            if !read_globals(st, &gp.globs, &mut xs)? {
                return decline("a global not found");
            }
            if !read_scope(&svals, &gp.scope, &mut xs) {
                return decline("a formula-environment value not found");
            }
            let Some(rest) = inputs(&xs)? else {
                return decline("a global or scope value of an unsupported type");
            };
            // SAFETY: the program is kept alive by the plan's descriptor.
            grad_groups.push(GradGroup {
                prog: unsafe { &*gp.prog },
                cols,
                rest,
                mover_x,
                mover_y,
            });
        }
    }
    let cand_cells = |here: i32, rk: i32| -> savvy::Result<Option<Vec<i32>>> {
        if rk == NA_INT || rk < 1 {
            return Ok(Some(Vec::new()));
        }
        let rk = rk as usize;
        if rk <= 1 && !mv.axes {
            return Ok(nb_at(here)?.map(|v| v.to_vec()));
        }
        if mv.axes {
            let mut out = Vec::new();
            for tbl in &axis_vecs {
                let mut cur = here;
                for _ in 0..rk {
                    if cur < 1 {
                        return Ok(None);
                    }
                    if cur as usize > tbl.len() {
                        break;
                    }
                    cur = tbl[cur as usize - 1];
                    if cur == NA_INT || cur == here {
                        break;
                    }
                    if !out.contains(&cur) {
                        out.push(cur);
                    }
                }
            }
            return Ok(Some(out));
        }
        let mut seen = vec![here];
        let mut frontier = vec![here];
        for _ in 0..rk {
            let mut next = Vec::new();
            for &c in &frontier {
                let Some(ns) = nb_at(c)? else {
                    return Ok(None);
                };
                for &nb in ns {
                    if !seen.contains(&nb) && !next.contains(&nb) {
                        next.push(nb);
                    }
                }
            }
            if next.is_empty() {
                break;
            }
            seen.extend_from_slice(&next);
            frontier = next;
        }
        seen.remove(0);
        Ok(Some(seen))
    };
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
        let build_occ = |cells: &[Vec<i32>]| -> Option<Vec<i32>> {
            let mx = cells
                .iter()
                .flatten()
                .filter(|&&c| c != NA_INT)
                .copied()
                .max()
                .unwrap_or(0)
                .max(0);
            let nbins = n_nb.max(mv.ncell.max(0) as usize).max(mx as usize);
            let mut occ = vec![0; nbins];
            for &c in cells.iter().flatten() {
                if c != NA_INT {
                    if c < 1 {
                        return None;
                    }
                    occ[c as usize - 1] += 1;
                }
            }
            Some(occ)
        };
        if mv.mode == 2 {
            let mut occupied: Vec<i32> = if mv.avoid {
                let Some(o) = build_occ(&cells) else {
                    return Ok(None);
                };
                o
            } else {
                Vec::new()
            };
            let ncell = mv.ncell.max(0) as usize;
            for &m in &ord {
                let (gk, rw) = owner[m];
                let here = cells[gk][rw];
                if here == NA_INT {
                    continue;
                }
                let target = if mv.avoid {
                    let free: Vec<i32> = (0..ncell)
                        .filter(|&i| occupied[i] == 0)
                        .map(|i| i as i32 + 1)
                        .collect();
                    if free.is_empty() {
                        continue;
                    }
                    let t = if free.len() == 1 {
                        free[0]
                    } else {
                        free[tidyabm_core::sample::RDraws::unif_index(r, free.len() as f64)
                            as usize]
                    };
                    if here < 1 || here as usize > occupied.len() {
                        return Ok(None);
                    }
                    occupied[here as usize - 1] -= 1;
                    occupied[t as usize - 1] += 1;
                    t
                } else {
                    if ncell == 0 {
                        return Ok(None);
                    }
                    tidyabm_core::sample::RDraws::unif_index(r, ncell as f64) as i32 + 1
                };
                cells[gk][rw] = target;
            }
            return Ok(Some(()));
        }
        if !heading.is_empty() {
            let mut occupied: Vec<i32> = if mv.avoid {
                let Some(o) = build_occ(&cells) else {
                    return Ok(None);
                };
                o
            } else {
                Vec::new()
            };
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
        if mv.grad == 0 && !mv.avoid && !mv.has_range && mv.range <= 1 && !mv.axes {
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
        // one mover at a time (random with avoid/range/axes, or uphill/downhill)
        let mut occupied: Vec<i32> = if mv.avoid {
            let Some(o) = build_occ(&cells) else {
                return Ok(None);
            };
            o
        } else {
            Vec::new()
        };
        let taken =
            |occ: &Vec<i32>, i: i32| i >= 1 && (i as usize) <= occ.len() && occ[i as usize - 1] > 0;
        if mv.grad != 0 {
            let uphill = mv.grad > 0;
            let fill_na = if uphill {
                f64::NEG_INFINITY
            } else {
                f64::INFINITY
            };
            let mut ev = expr::Evaluator::new(Some(r));
            for &m in &ord {
                let (gk, rw) = owner[m];
                let here = cells[gk][rw];
                if here == NA_INT {
                    continue;
                }
                let rk = if mv.has_range {
                    ranges[gk][rw]
                } else {
                    mv.range as i32
                };
                let Some(raw) = cand_cells(here, rk)? else {
                    return Ok(None);
                };
                let mut cand = Vec::with_capacity(raw.len() + 1);
                cand.push(here);
                for c in raw {
                    if c != NA_INT && !cand.contains(&c) {
                        cand.push(c);
                    }
                }
                if mv.avoid {
                    cand.retain(|&c| c == here || !taken(&occupied, c));
                }
                if cand.is_empty() {
                    continue;
                }
                let np = cand.len();
                let mut wrows = Vec::with_capacity(np);
                for &c in &cand {
                    let wr = match &widx {
                        Some(ix) => ix.get(c),
                        None => (c >= 1 && c <= mv.ncell).then_some(c as usize - 1),
                    };
                    let Some(wr) = wr else {
                        return Ok(None);
                    };
                    wrows.push(wr);
                }
                let gg = &grad_groups[gk];
                let mut ins = Vec::with_capacity(gg.cols.len() + gg.rest.len());
                for (vk, v) in &gg.cols {
                    match vk {
                        0 => ins.push(tidyabm_core::expr::take(
                            v.as_ref().ok_or_else(|| bad("grad patch col"))?,
                            &wrows,
                        )),
                        1 => ins.push(recycle(
                            one_at(v.as_ref().ok_or_else(|| bad("grad mover col"))?, rw),
                            np,
                        )),
                        2 => {
                            let (px, py) = patch_xy.as_ref().ok_or_else(|| bad("patch xy"))?;
                            let mx = gg.mover_x.as_ref().ok_or_else(|| bad("mover x"))?;
                            let my = gg.mover_y.as_deref();
                            let dv: Vec<f64> = wrows
                                .iter()
                                .map(|&wr| {
                                    space_dist_pair(
                                        px[wr],
                                        py.as_ref().map(|y| y[wr]),
                                        mx[rw],
                                        my.map(|y| y[rw]),
                                        &mv.dims,
                                        &mv.torus,
                                    )
                                })
                                .collect();
                            ins.push(Vals::Dbl(std::borrow::Cow::Owned(dv)));
                        }
                        _ => return Err(bad("grad col kind")),
                    }
                }
                ins.extend(gg.rest.iter().cloned());
                let val = match ev.eval(gg.prog, 0, &ins, np)? {
                    Ok(v) => recycle(v, np),
                    Err(d) => {
                        expr::note_decline(&d);
                        return Ok(None);
                    }
                };
                let scores: Vec<f64> = match &val {
                    Vals::Dbl(x) => x
                        .iter()
                        .map(|&v| if v.is_nan() { fill_na } else { v })
                        .collect(),
                    Vals::Int(x) | Vals::Lgl(x) => x
                        .iter()
                        .map(|&v| if v == NA_INT { fill_na } else { f64::from(v) })
                        .collect(),
                    Vals::Str(_) => return Ok(None),
                };
                let best_val = if uphill {
                    scores.iter().copied().fold(f64::NEG_INFINITY, f64::max)
                } else {
                    scores.iter().copied().fold(f64::INFINITY, f64::min)
                };
                let best: Vec<usize> = scores
                    .iter()
                    .enumerate()
                    .filter_map(|(i, &s)| (s == best_val).then_some(i))
                    .collect();
                if best.is_empty() {
                    continue;
                }
                let pick = if best.len() == 1 {
                    best[0]
                } else {
                    let rng = ev.rng().ok_or_else(|| bad("no move rng"))?;
                    best[tidyabm_core::sample::RDraws::unif_index(rng, best.len() as f64) as usize]
                };
                let target = cand[pick];
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
            return Ok(Some(()));
        }
        for &m in &ord {
            let (gk, rw) = owner[m];
            let here = cells[gk][rw];
            if here == NA_INT {
                continue;
            }
            let rk = if mv.has_range {
                ranges[gk][rw]
            } else {
                mv.range as i32
            };
            let Some(all) = cand_cells(here, rk)? else {
                return Ok(None);
            };
            let mut cand: Vec<i32> = all.into_iter().filter(|&c| c != NA_INT).collect();
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
