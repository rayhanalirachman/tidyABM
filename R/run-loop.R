# The Rust-driven run (full-Rust stages 1-2; PLAN.md "Full-Rust plan"). The tick
# loop runs in Rust (`RunLoop`, src/rust/src/runloop.rs), which holds the run's
# `state` for the whole run and runs native steps on it without any R code; the R
# engine is called back for every other step (one callback per run of consecutive
# R steps, `seg`), for the book-keeping between ticks (`tick`) and to compile a
# native step for the state's schema (`prep`). A native step that declines at run
# time is replayed on the R path from the state and `.Random.seed` it started from.
#
# The R engine stays the reference: "both" mode runs the same native executor
# (`rs_native_exec()`, kernel `rk_native()`) step by step against it from
# [run_once_rust()]; this loop is checked by its results being identical to the R
# engine's. Keep in step with [run_once()] (decision D12: R first, Rust follows).

#' [run_once()] with the tick loop in Rust
#' @noRd
run_once_loop <- function(model, go, ticks, seed, keep, measures, checks = NULL,
                          bar = NULL, until = NULL, record = "all",
                          kernels = active_kernels("rust")) {
  if (!is.null(seed)) local_seed(seed)
  rm(list = ls(.bound), envir = .bound)
  rm(list = ls(.eval_env), envir = .eval_env)
  rm(list = ls(.std_memo, all.names = TRUE), envir = .std_memo)
  .bound$on <- n_agents(model) >= 5000L
  .run_ctx$tick <- 0L

  state <- list(
    groups      = model$groups,
    globals     = model$globals,
    edges       = model$edges,
    lattice     = model$lattice,
    space       = model$space,
    space_edges = model$space_edges,
    relations   = model$relations,
    match       = NULL,
    tick        = 0L,
    next_id     = n_agents(model) + 1L
  )
  # recorded as run_once() records them (measured: the column store's
  # load/snapshot/stack path was slower than R's own for recorded runs)
  snapshots <- vector("list", ticks + 1L)

  global_log <- vector("list", ticks + 1L)
  measure_log <- if (length(measures)) vector("list", ticks + 1L) else NULL
  first <- snapshot(state, 0L)
  empty <- first[0, , drop = FALSE]
  # the recorded ticks' groups (zero copy), stacked once after the run (FR9b)
  slog <- vector("list", ticks + 1L)
  if (keep[[1]]) slog[[1]] <- state$groups
  global_log[[1]] <- global_cells(state, 0L)
  state0_globals <- state$globals
  if (!is.null(measure_log)) measure_log[[1]] <- measure_row(measures, state, 0L)
  if (length(checks)) check_row(checks, state, 0L)

  # `abm_repeat()` blocks flattened: the loop runs their steps itself (FR11)
  flat <- rl_flatten(plan_steps(go$steps))
  steps <- flat$steps
  top_at <- flat$at
  kinds <- flat$kinds
  kinds[kinds == 0L] <- vapply(steps[kinds == 0L], rl_kind, 0L, kernels = kernels)
  measure_reads <- read_vars(measures)
  check_reads <- read_vars(checks)
  measure_plan <- new.env(parent = emptyenv())
  check_plan <- new.env(parent = emptyenv())
  # the step running in R, and the frame whose `st` is the state it started from
  # (read only when reporting an error: a second reference to the state would
  # make every R step copy what it changes instead of changing it in place)
  at <- 0L
  frame <- NULL
  saved <- NULL

  # the state is bound in `ctx` only: the callbacks take it out while they work on
  # it, so the R engine's steps change it in place, as under run_once()
  ctx <- new.env(parent = emptyenv())
  seg <- function(i, j, restore) {
    st <- ctx$st
    ctx$st <- NULL
    if (restore) rng_set(saved)
    frame <<- environment()
    for (k in i:j) {
      at <<- k
      st <- run_step(steps[[k]], st)
    }
    at <<- 0L
    ctx$st <- st
    st
  }
  tick <- function(done, nxt) {
    st <- ctx$st
    ctx$st <- NULL
    if (done > 0L) {
      t <- done
      .run_ctx$tick <- as.integer(t)
      # (a recorded tick's groups are logged by the loop itself, into `slog`)
      global_log[[t + 1L]] <<- global_cells(st, t)
      if (!is.null(measure_log)) {
        measure_log[[t + 1L]] <<- measure_row(measures, st, t, measure_reads, measure_plan)
      }
      if (length(checks)) check_row(checks, st, t, check_reads, check_plan)
      if (!is.null(bar)) cli::cli_progress_update(id = bar)
    }
    if (nxt > 0L) {
      st$match <- NULL
      st$tick <- nxt
      .run_ctx$tick <- as.integer(nxt)
    }
    ctx$st <- st
    st
  }
  prep <- function(i) rl_prep(steps[[i]], ctx$st, i)
  # a block's `until` after a pass ([repeat_until()]), or k == 0 for run-level `until`
  until_reads <- if (!is.null(until)) read_vars(list(until))
  until_plan <- new.env(parent = emptyenv())
  until_cb <- function(k) {
    if (k == 0L) {
      t <- as.integer(ctx$st$tick)
      .run_ctx$tick <- t
      return(until_met(until, ctx$st, t, until_reads, until_plan))
    }
    at <<- k
    frame <<- ctx
    out <- repeat_until(steps[[k]], ctx$st)
    at <<- 0L
    out
  }
  # measures (1), checks (2), and run-level until (3) compiled as value-only global programs (FR9b)
  tick_steps <- list(rl_tick_step(measures), rl_tick_step(checks),
                     if (!is.null(until)) rl_tick_step(list(until = until)))
  mprep <- function(k) rl_prep_tick(tick_steps[[k]], ctx$st)
  rng <- function() {
    saved <<- rng_get()
    NULL
  }

  lp <- RunLoop$new(kinds, prep, seg, tick, rng, ctx, mprep, until_cb)
  # the ticks whose book-keeping needs R; the loop logs the others' globals into
  # `glog` itself (FR9) and they are turned into rows below
  # (measures and checks too, when they compile; a tick whose native ones
  # decline or whose check fails is closed by the R callback instead)
  need <- rep(!is.null(bar), ticks + 1L)
  glog <- vector("list", ticks + 1L)
  mlog <- vector("list", ticks + 1L)
  has <- c(length(measures) > 0L, length(checks) > 0L, !is.null(until),
           identical(record, "globals"))
  logs <- list(glog, mlog, slog)
  state <- withCallingHandlers(lp$run(state, ticks, need, has, rep_len(as.logical(keep), ticks + 1L), logs), error = function(e) {
    # a fault in the Rust loop itself: classed as rust_call() does
    if (startsWith(conditionMessage(e), "tidyABM-rust-")) rust_call(stop(e))
    if (at == 0L || inherits(e, "tidyABM_error")) return()
    # a step inside a block is reported as [run_repeat()] reports it
    rethrow_step_error(e, steps[[at]], get("st", envir = frame),
                       if (is.na(top_at[[at]])) NULL else top_at[[at]])
  })
  .rl_last$stats <- lp$stats()
  if (nzchar(Sys.getenv("TIDYABM_RUST_TIMING"))) .rl_last$times <- lp$times()
  ran_ticks <- if (ticks == 0L) 0L else as.integer(state$tick %||% ticks)
  klen <- seq_len(ran_ticks + 1L)
  glog <- logs[[1]][klen]
  mlog <- logs[[2]][klen]
  slog <- logs[[3]][klen]
  global_log <- global_log[klen]
  if (!is.null(measure_log)) measure_log <- measure_log[klen]
  need <- need[klen]
  # globals: every tick closed natively with one-value bare globals of the same
  # names is one column per global ([global_table()]'s fast path, without a row
  # per tick); anything else goes through [global_cells()] and [global_table()]
  globals_tbl <- rl_globals_fast(glog, global_log, state0_globals)
  for (i in setdiff(which(!need), 1L)) {
    # closed natively (else the R callback has filled the logs)
    if (!is.null(global_log[[i]])) next
    if (is.null(globals_tbl)) global_log[[i]] <- global_cells(list(globals = glog[[i]]), i - 1L)
    if (!is.null(measure_log)) {
      measure_log[[i]] <- tibble::new_tibble(
        c(list(tick = i - 1L), stats::setNames(as.list(mlog[[i]]), names(measures))), nrow = 1L)
    }
  }


  list(
    population = rl_stack(slog, snapshots[klen], empty),
    globals    = globals_tbl %||% global_table(global_log),
    measures   = if (is.null(measure_log)) NULL else dplyr::bind_rows(measure_log),
    network    = strip_draws(state$edges),
    relations  = relation_tables(state$relations),
    ticks      = ran_ticks
  )
}

# The steps with every `abm_repeat()` block flattened: the block's steps between
# a begin marker (kind -1) and an end marker (kind -(2 + 2 * max + has_until)),
# both the repeat step itself; `at` the go block position of a top-level step
# (`NA` inside a block), `kinds` 0 for an ordinary step. A block whose `max` the
# encoding cannot hold stays one R step.
rl_flatten <- function(steps, top = TRUE) {
  has_block <- function(s) inherits(s, "abm_repeat") || (inherits(s, "abm_match") && !is.null(s$steps))
  if (top && !any(vapply(steps, has_block, NA))) {
    return(list(steps = steps, at = seq_along(steps), kinds = integer(length(steps))))
  }
  out <- list()
  at <- integer()
  kinds <- integer()
  for (i in seq_along(steps)) {
    s <- steps[[i]]
    pos <- if (top) i else NA_integer_
    if (inherits(s, "abm_repeat") && s$max <= 1e8) {
      inner <- rl_flatten(s$steps, top = FALSE)
      out <- c(out, list(s), inner$steps, list(s))
      at <- c(at, pos, inner$at, pos)
      kinds <- c(kinds, -1L, inner$kinds,
                 -(2L + 2L * as.integer(s$max) + as.integer(!is.null(s$until))))
    } else if (inherits(s, "abm_match") && !is.null(s$steps)) {
      # Scoped match block: save outer match (-1), run bare match + child steps,
      # restore outer match after 1 pass (-4 = -(2 + 2*1 + 0)).
      bare_m <- s
      bare_m$steps <- NULL
      inner <- rl_flatten(s$steps, top = FALSE)
      out <- c(out, list(s), list(bare_m), inner$steps, list(s))
      at <- c(at, pos, pos, inner$at, pos)
      kinds <- c(kinds, -1L, 0L, inner$kinds, -4L)
    } else {
      out <- c(out, list(s))
      at <- c(at, pos)
      kinds <- c(kinds, 0L)
    }
  }
  list(steps = out, at = at, kinds = kinds)
}

# measures or checks as an `abm_global()`-like step the loop compiles into a
# value-only program; `NULL` when there are none
rl_tick_step <- function(quos) {
  if (!length(quos)) return(NULL)
  structure(list(rules = lapply(names(quos), function(nm) list(target = nm, quo = quos[[nm]])),
                 by = NULL, plan = new.env(parent = emptyenv())),
            class = c("abm_global", "abm_step"))
}

# a block's `until` as a value-only program ([repeat_until()]'s stacked
# columns and globals; anything the program does not take, a match column for
# one, leaves it to the R callback)
rl_prep_until <- function(step, state) {
  if (is.null(step$until)) return(FALSE)
  rl_prep_tick(rl_tick_step(list(until = step$until)), state)
}

# a measure/check program for this schema: `FALSE` never (none, or a formula
# that draws: the R callback would draw again when it takes over a tick)
rl_prep_tick <- function(step, state) {
  if (is.null(step)) return(FALSE)
  heads <- unique(unlist(lapply(rx_rules(step), function(cp) if (!is_rx_no(cp)) cp$heads)))
  if (any(rx_draw_heads %in% heads)) return(FALSE)
  rl_prep_global(step, state, mode = "values")
}

# [global_table()] of the per-tick rows, built straight from the globals lists
# when every tick after 0 was closed natively (`global_log[-1]` empty) and every
# global of every tick is a bare atomic value of length 1, all ticks with the same
# names in the same order, of one type each (the rows [global_cells()] would
# make, which [global_table()] would concatenate); `NULL` otherwise
rl_globals_fast <- function(glog, global_log, g0) {
  n <- length(glog)
  if (n < 2L || !all(vapply(global_log[-1L], is.null, NA))) return(NULL)
  rows <- c(list(g0), glog[-1L])
  nms <- names(g0)
  if (!length(nms) || anyDuplicated(nms) || anyNA(nms) || !all(nzchar(nms)) ||
      "tick" %in% nms) return(NULL)
  ok_types <- c("logical", "integer", "double", "complex", "character", "raw")
  cols <- vector("list", length(nms))
  for (k in seq_along(nms)) {
    v <- lapply(rows, .subset2, nms[[k]])
    t1 <- typeof(v[[1]])
    if (!t1 %in% ok_types || !all(vapply(v, function(x) typeof(x) == t1 && length(x) == 1L &&
                                          is.null(attributes(x)), NA))) return(NULL)
    cols[[k]] <- unlist(v, use.names = FALSE)
  }
  if (!all(vapply(rows, function(r) identical(names(r), nms), NA))) return(NULL)
  names(cols) <- nms
  new_tbl(c(list(tick = seq.int(0L, n - 1L)), cols), n)
}

# the recorded ticks: stacked in Rust when every column is a bare vector of one
# type throughout, else R's own snapshots stacked by R
rl_stack <- function(slog, snapshots, empty) {
  kept <- which(!vapply(slog, is.null, NA))
  if (!length(kept)) return(rl_population(snapshots, empty))
  cols <- rs_stack_ticks(slog[kept], as.integer(kept - 1L))
  if (!is.null(cols)) return(tibble::new_tibble(cols, nrow = length(cols[[1]])))
  for (i in kept) snapshots[[i]] <- snapshot(list(groups = slog[[i]]), i - 1L)
  rl_population(snapshots, empty)
}

# the recorded ticks stacked, `tick`, `.id` and `.group` first, as run_once() does
rl_population <- function(snapshots, empty) {
  snapshots <- Filter(Negate(is.null), snapshots)
  population <- if (length(snapshots)) dplyr::bind_rows(snapshots) else empty
  front <- c("tick", ".id", ".group")
  cols <- names(population)
  tibble::new_tibble(unclass(population)[c(front, cols[!cols %in% front])],
                     nrow = vctrs::vec_size(population))
}

# the last loop's counters (native steps run, declined, compiled; R callbacks),
# for tests and coverage reports
.rl_last <- new.env(parent = emptyenv())

#' A step's native kind for the loop: 0 the R path, else the registry's `native`
#' @noRd
rl_kind <- function(step, kernels) {
  k <- kernels[[class(step)[[1]]]]
  if (is.null(k$native) || !(is.null(k$can) || isTRUE(k$can(step)))) return(0L)
  # a `within` neighbourhood is compiled per schema only (5); a network one also
  # per edge set and agent count (3)
  if (inherits(step, "abm_neighbours") && !is.null(step$within)) return(5L)
  if (inherits(step, "abm_neighbours") && !is.null(step$where)) return(6L)
  if (inherits(step, "abm_match")) return(7L)
  if (inherits(step, "abm_death")) return(8L)
  if (inherits(step, "abm_birth")) return(9L)
  if (inherits(step, "abm_move")) return(10L)
  if (inherits(step, "abm_tell")) return(11L)
  if (inherits(step, "abm_sequential")) return(12L)
  if (inherits(step, "abm_global") && !is.null(step$by)) return(13L)
  if (inherits(step, "abm_draw")) return(16L)
  if (inherits(step, c("abm_link", "abm_unlink"))) return(18L)
  if (inherits(step, "abm_pairs")) return(19L)
  if (inherits(step, "abm_neighbours") && is.null(step$within) && is.null(step$where)) {
    sh <- planned(step, "rl_nbshapes", rl_nb_shapes(step))
    if (is.null(sh)) return(17L)
    if (isTRUE(sh[[1]]$fn %in% c("any", "all"))) return(15L)
  }
  as.integer(k$native)
}

#' Compile a native step for `state`'s schema: a descriptor for
#' `rs_native_exec()` / `RunLoop`, `NULL` (the R path for this schema) or `FALSE`
#' (never native)
#' @noRd
rl_prep <- function(step, state, slot = NULL) {
  switch(class(step)[[1]],
         abm_pairs = rl_prep_memo(step, state, slot, rl_prep_pairs),
         abm_link = ,
         abm_unlink = rl_prep_memo(step, state, slot, rl_prep_link),
         abm_draw = rl_prep_memo(step, state, slot, rl_prep_draw),
         abm_match = rl_prep_memo(step, state, slot, rl_prep_match),
         abm_death = rl_prep_memo(step, state, slot, rl_prep_death),
         abm_birth = rl_prep_memo(step, state, slot, rl_prep_birth),
         abm_move = rl_prep_memo(step, state, slot, rl_prep_move),
         abm_tell = rl_prep_memo(step, state, slot, rl_prep_tell),
         abm_sequential = rl_prep_memo(step, state, slot, rl_prep_seq),
         abm_repeat = rl_prep_memo(step, state, slot, rl_prep_until),
         abm_global = rl_prep_memo(step, state, slot,
                                   if (is.null(step$by)) rl_prep_global else rl_prep_global_by),
         abm_neighbours = rl_prep_neighbours(step, state, slot),
         rl_prep_memo(step, state, slot, rl_prep_rules))
}

# Mirror of [resolve_topology_edges()] returning `NULL` instead of aborting
rl_topo_spec <- function(via, state) {
  has_net <- !is.null(state$edges)
  has_sp <- !is.null(state$space_edges)
  if (!is.null(via)) {
    if (identical(via, "space")) {
      if (has_sp) return(list(efield = "space_edges", cname = "space_nb_cache"))
      if (!is.null(state$lattice) && has_net) return(list(efield = "edges", cname = "nb_cache"))
      return(NULL)
    }
    if (has_net && (is.null(state$lattice) || has_sp)) {
      return(list(efield = "edges", cname = "nb_cache"))
    }
    return(NULL)
  }
  if (has_net && has_sp) return(NULL)
  if (has_sp) list(efield = "space_edges", cname = "space_nb_cache")
  else if (has_net) list(efield = "edges", cname = "nb_cache")
  else NULL
}

# Validate scalar atomic bare `.default` for native `abm_neighbours()`
rl_nb_default_ok <- function(def) {
  is.null(def) || (is.atomic(def) && length(def) == 1L && is.null(attributes(def)) &&
                     typeof(def) %in% c("logical", "integer", "double", "character"))
}

# A rules/global descriptor depends on the step and the state's names only (and
# on what decides which functions its names find), so the loop keeps the last one
# per step position (`slot`) for the session: the runs of a sweep over one go
# block compile each step once, and a go block built afresh per run replaces the
# entry rather than piling them up
rl_prep_memo <- function(step, state, slot, f) {
  if (is.null(slot)) return(f(step, state))
  key <- paste0("d", slot)
  topo <- if (inherits(step, c("abm_neighbours", "abm_draw", "abm_tell"))) {
    rl_topo_spec(step$via, state)
  }
  sk <- list(schema_key(state), Filter(is.function, state$globals), isTRUE(.bound$on),
             RNGkind(), step$by, step$scope, step$via, step$default,
             if (length(state$relations)) lapply(state$relations, function(r) names(r$edges)),
             if (inherits(step, "abm_neighbours")) {
               list(topo$efield, if (!is.null(topo)) drawn_names(state[[topo$efield]]))
             } else if (inherits(step, "abm_draw")) {
               if (is.null(topo)) FALSE
               else {
                 enms <- names(state[[topo$efield]])
                 list(topo$efield, enms[!startsWith(enms, ".draw_") & !startsWith(enms, ".back_")])
               }
             } else if (inherits(step, "abm_tell") && identical(step$to, "neighbours")) {
               topo$efield
             })
  # a few schemas per step (a run's first tick usually adds columns)
  memo <- .rl_desc[[key]]
  # rules and global steps by their rules (with `.by`/`.scope` in `sk`); any
  # other step by itself without its per-run plan (a match has no rules to tell
  # two steps apart by)
  me <- if (inherits(step, "abm_rules")) list(step$rules, step$when)
        else if (inherits(step, "abm_global")) step$rules
        else unclass(step)[names(step) != "plan"]
  check_lat <- inherits(step, "abm_move") ||
    (inherits(step, "abm_neighbours") && !is.null(step$where))
  for (hit in memo) {
    if (identical(hit$step, me) && identical(hit$sk, sk) &&
        (!check_lat || (is.null(hit$lat) && is.null(state$lattice)) ||
           rlang::is_reference(hit$lat, state$lattice))) {
      return(hit$desc)
    }
  }
  desc <- f(step, state)
  assign(key, c(utils::tail(memo, 3L),
                list(list(step = me, sk = sk,
                          lat = if (check_lat) state$lattice, desc = desc))),
         envir = .rl_desc)
  desc
}
.rl_desc <- new.env(parent = emptyenv())

# the rules each group runs, compiled per group as the R engine routes them
rl_prep_rules <- function(step, state) {
  if (!is.null(step$by)) return(rl_prep_rules_by(step, state))
  # A `.cell` write moves agents ([run_step()] re-derives `.x`/`.y`); a
  # `partner_` target writes another agent's row: never native
  if (!planned(step, "rx_shape", rx_rules_mixed_ok(step))) return(FALSE)
  rel_spec <- NULL
  rel_mnames <- character()
  rel_tg_names <- character()
  rel_names <- relation_derived_names(state$relations)
  if (length(rel_names)) {
    all_tg <- vapply(step$rules, `[[`, "", "target")
    all_sy <- unique(c(
      unlist(lapply(step$rules, function(r) all.vars(rlang::quo_get_expr(r$quo))), use.names = FALSE),
      if (!is.null(step$when)) all.vars(rlang::quo_get_expr(step$when))
    ))
    rel_tg <- intersect(all_tg, rel_names)
    rel_rd <- intersect(all_sy, rel_names)
    rel_all <- union(rel_rd, rel_tg)
    if (length(rel_all)) {
      hits <- lapply(rel_all, resolve_relation_name, relations = state$relations)
      for (k in seq_along(rel_all)) {
        h <- hits[[k]]
        if (rel_all[[k]] %in% rel_tg) {
          if (is.null(h$col) || !h$col %in% names(state$relations[[h$rel]]$edges)) {
            return(NULL)
          }
        }
      }
      rel_mnames <- rel_rd
      rel_tg_names <- rel_tg
      rel_spec <- list(
        names = rel_all,
        rel = vapply(hits, `[[`, "", "rel"),
        col = vapply(hits, function(h) h$col %||% "", ""),
        back = vapply(hits, function(h) isTRUE(h$back), NA)
      )
    }
  }
  routes <- if (length(state$groups) > 1L) rule_routes(step, state)
  when_routes <- if (!is.null(step$when) && length(state$groups) > 1L) {
    condition_routes(step, state)
  }
  groups <- list()
  envs <- list()
  compiled <- !vapply(rx_rules(step), is_rx_no, NA)
  mixed <- FALSE
  quos_all <- c(lapply(step$rules, `[[`, "quo"), if (!is.null(step$when)) list(step$when))
  needs_aux <- (step$uses_match %||% planned(step, "uses_match", reads_match(quos_all))) ||
    length(planned(step, "cvars", cell_vars(quos_all))) > 0L
  mnames <- if (needs_aux) {
    planned(step, "rx_mnames", rx_match_names(step)) %||% return(NULL)
  } else character()
  # `partner_<col>` of a column the model does not have: R's error
  pcols <- sub("^partner_", "", mnames[startsWith(mnames, "partner_")])
  if (!all(pcols %in% model_columns(state$groups))) return(NULL)
  ccols <- sub("^cell_", "", mnames[startsWith(mnames, "cell_")])
  if (length(ccols)) {
    if (is.null(state$lattice) || is.null(state$lattice$on) ||
        !state$lattice$on %in% names(state$groups) ||
        !all(ccols %in% names(state$groups[[state$lattice$on]]))) {
      return(NULL)
    }
  }
  mnames <- union(mnames, rel_mnames)
  when_fake <- if (!is.null(step$when)) {
    planned(step, "rx_when_fake",
            rl_fake_rules(list(list(target = ".when", quo = step$when))))
  }
  for (nm in names(state$groups)) {
    todo <- if (length(state$groups) == 1L) step$rules else routes[[nm]]
    if (!length(todo)) next
    if (!is.null(when_routes) && !when_routes[[nm]]) next
    idx <- which(vapply(step$rules, function(r) any(vapply(todo, identical, NA, r)), NA))
    nat <- idx[compiled[idx]]
    if (!length(nat)) return(NULL)
    p <- rx_program(step, nat, names(state$groups[[nm]]), state$globals,
                    match_names = mnames)
    if (is_rx_no(p)) return(NULL)
    if (length(p$scope)) envs <- c(envs, list(p$env))
    # each rule in order: its place in the program (0-based), or -1 for a rule
    # evaluated by R (`rule_fn`); `ridx` its index in `step$rules`
    order <- ifelse(compiled[idx], match(idx, nat) - 1L, -1L)
    mixed <- mixed || any(order < 0L)
    groups[[length(groups) + 1L]] <- list(name = nm, prog = p$prog$.ptr, cols = p$cols,
                                          globs = p$globs, targets = p$targets,
                                          scope = p$scope,
                                          ew = all(vapply(todo, function(r) isTRUE(r$elementwise), NA)),
                                          order = as.integer(order), ridx = as.integer(idx),
                                          mcols = p$mcols,
                                          rew = vapply(todo, function(r) isTRUE(r$elementwise), NA),
                                          all_targets = vapply(step$rules[idx], `[[`, "", "target"))
    if (!is.null(when_fake)) {
      pw <- rx_program(when_fake, 1L, names(state$groups[[nm]]), state$globals,
                       match_names = mnames)
      if (is_rx_no(pw)) return(NULL)
      if (length(pw$scope)) envs <- c(envs, list(pw$env))
      groups[[length(groups) + 1L]] <- list(name = nm, part = "when",
                                            prog = pw$prog$.ptr, cols = pw$cols,
                                            globs = pw$globs, targets = character(),
                                            scope = pw$scope, ew = FALSE, mcols = pw$mcols)
    }
  }
  if (mixed && length(rel_tg_names)) return(NULL)
  rl_scope_bind(c(list(kind = 1L, scope_match = (step$scope %||% "match") == "match",
                       has_when = !is.null(step$when),
                       groups = groups,
                       rel = rel_spec), if (mixed) rl_rule_fns(step)), envs)
}

# For a step that mixes native rules with rules R evaluates: `rule_fn(g, globals,
# i, multi)` gives rule `i`'s value over group `g` as [run_rules()] works it out
# (its shared scope for a plain rule among several, else [eval_rule()]), letting
# warnings and messages through as the R engine would raise them, and an error
# as a non-vector (the step is then replayed on the R path, which raises it);
# `rng_save()` / `rng_restore()` bracket a native rule that draws after an R rule
# has run (a decline then continues on R from the stream before that rule).
rl_rule_fns <- function(step) {
  saved <- NULL
  list(
    rule_fn = function(g, globals, i, multi) {
      r <- step$rules[[i]]
      tryCatch({
        out <- NULL
        if (isTRUE(r$plain) && multi) {
          out <- shared_eval(g, globals)$eval(r$quo)
          if (!(length(out) == vctrs::vec_size(g) || length(out) == 1L)) out <- NULL
        }
        if (is.null(out)) out <- eval_rule(r, g, globals, grouped = FALSE)
        out
      }, error = function(e) list())
    },
    rng_save = function() {
      saved <<- rng_get()
      NULL
    },
    rng_restore = function() {
      rng_set(saved)
      NULL
    })
}

# Values a step reads from its formulas' environment (`envs`: the environment of
# each group program that reads some; each group's `scope` holds their names): the
# R engine looks them up every time it evaluates the formulas, so the native step
# does too, through `scope_fn()` (a list of the values, or `NULL` when one cannot
# be read: the step then declines and R raises whatever it raises). Each group's
# `scope` becomes the 0-based positions of its names in that list. `NULL` (R for
# this schema) for names the engine's own scope binds (`n()`, `%in%`, `==`) or
# programs written in different environments.
rl_scope_bind <- function(desc, envs) {
  if (!length(envs)) return(desc)
  e <- envs[[1]]
  if (!all(vapply(envs, identical, NA, e))) return(NULL)
  n <- unique(unlist(lapply(desc$groups, `[[`, "scope"), use.names = FALSE))
  if (any(n %in% c("n", "%in%", "=="))) return(NULL)
  desc$groups <- lapply(desc$groups, function(g) {
    g$scope <- match(g$scope, n) - 1L
    g
  })
  desc$scope_fn <- function() tryCatch(mget(n, envir = e, inherits = TRUE),
                                       error = function(err) NULL)
  desc
}

# For a step that mixes native rules with rules R evaluates: `rule_fn(g, globals,
# i, multi)` gives rule `i`'s value over group `g` as [run_rules()] works it out
# (its shared scope for a plain rule among several, else [eval_rule()]), letting
# warnings and messages through as the R engine would raise them, and an error
# as a non-vector (the step is then replayed on the R path, which raises it);
# `rng_save()` / `rng_restore()` bracket a native rule that draws after an R rule
# has run (a decline then continues on R from the stream before that rule).
rl_rule_fns <- function(step) {
  saved <- NULL
  list(
    rule_fn = function(g, globals, i, multi) {
      r <- step$rules[[i]]
      tryCatch({
        out <- NULL
        if (isTRUE(r$plain) && multi) {
          out <- shared_eval(g, globals)$eval(r$quo)
          if (!(length(out) == vctrs::vec_size(g) || length(out) == 1L)) out <- NULL
        }
        if (is.null(out)) out <- eval_rule(r, g, globals, grouped = FALSE)
        out
      }, error = function(e) list())
    },
    rng_save = function() {
      saved <<- rng_get()
      NULL
    },
    rng_restore = function() {
      rng_set(saved)
      NULL
    })
}

# Values a step reads from its formulas' environment: the R engine looks them up
# every time it evaluates the formulas, so the native step does too, through
# `scope_fn()` (a list of the values, or `NULL` when one cannot be read: the step
# then declines and R raises whatever it raises)
rl_scope <- function() {
  env <- NULL
  nms <- character()
  list(
    add = function(p) {
      if (!length(p$scope)) return(TRUE)
      # names the engine's own scope binds (`n()`, `%in%`, `==`) hide the formula's
      if (any(p$scope %in% c("n", "%in%", "=="))) return(FALSE)
      if (!is.null(env) && !identical(env, p$env)) return(FALSE)
      env <<- p$env
      nms <<- union(nms, p$scope)
      TRUE
    },
    idx = function(p) match(p$scope, nms) - 1L,
    desc = function() {
      if (!length(nms)) return(list())
      e <- env
      n <- nms
      list(scope_fn = function() tryCatch(mget(n, envir = e, inherits = TRUE),
                                          error = function(err) NULL))
    }
  )
}

# `.by` rules ([run_rules_by()]): one program over every group stacked, each rule
# evaluated per value of the `.by` column in turn, later rules reading what the
# earlier ones wrote
rl_prep_rules_by <- function(step, state) {
  if (!planned(step, "rx_byshape", rx_by_ok(step))) return(FALSE)
  groups <- state$groups
  by <- by_columns(step$by)
  cols <- Reduce(intersect, lapply(groups, names))
  if (!by %in% cols) return(NULL)
  rel_names <- relation_derived_names(state$relations)
  if (length(rel_names)) {
    all_tg <- vapply(step$rules, `[[`, "", "target")
    all_sy <- unique(unlist(lapply(step$rules, function(r) all.vars(rlang::quo_get_expr(r$quo))),
                            use.names = FALSE))
    if (any(all_tg %in% rel_names) || any(all_sy %in% rel_names)) return(NULL)
  }
  p <- rx_program(step, seq_along(step$rules), cols, state$globals, global_step = "by")
  if (is_rx_no(p)) return(NULL)
  # a column some group lacks is padded with NA by the R engine: not native
  partial <- setdiff(model_columns(groups), cols)
  if (any(c(p$names, p$targets) %in% partial)) return(NULL)
  # a target a rule reads must be one of the columns (else the R engine reads
  # what an earlier rule wrote where the program would read a global)
  if (!all(intersect(p$targets, p$names) %in% p$cols)) return(NULL)
  rl_scope_bind(list(kind = 4L, scope_match = FALSE, by = by,
                     groups = list(list(name = "", prog = p$prog$.ptr, cols = p$cols,
                                        globs = p$globs, targets = p$targets, scope = p$scope,
                                        ew = FALSE))),
                if (length(p$scope)) list(p$env))
}

# FR11: `abm_global(.by = <name>)` natively ([run_global_by()]) when the keys
# are a global or a formula-environment value (not a column): per rule one
# program over the groups stacked, evaluated per key with the target bound to
# the key's current value and `.key` to the key (input source 4)
rl_global_by_ok <- function(step) {
  !is.null(step$by) && rlang::is_symbol(rlang::quo_get_expr(step$by)) &&
    !any(vapply(rx_rules(step), is_rx_no, NA))
}
rl_prep_global_by <- function(step, state) {
  if (!planned(step, "rx_gbyshape", rl_global_by_ok(step))) return(FALSE)
  groups <- state$groups
  cols <- Reduce(intersect, lapply(groups, names))
  all_cols <- model_columns(groups)
  by <- rlang::as_string(rlang::quo_get_expr(step$by))
  benv <- rlang::quo_get_env(step$by)
  # keys from a column (sorted, merged with the global's names): R
  if (by %in% all_cols || ".key" %in% all_cols) return(NULL)
  by_src <- if (by %in% names(state$globals)) 2L else if (exists(by, envir = benv)) 3L
            else return(NULL)
  out <- list()
  envs <- list()
  for (i in seq_along(step$rules)) {
    r <- step$rules[[i]]
    if (r$target %in% all_cols) return(NULL)
    p <- rx_program(step, i, cols, state$globals, match_names = c(r$target, ".key"))
    if (is_rx_no(p) || any(p$names %in% setdiff(all_cols, cols))) return(NULL)
    if (length(p$scope)) envs <- c(envs, list(p$env))
    out[[i]] <- list(name = "", prog = p$prog$.ptr, cols = p$cols, globs = p$globs,
                     targets = r$target, scope = p$scope, ew = FALSE, mcols = p$mcols)
  }
  if (by_src == 3L) {
    envs <- c(envs, list(benv))
    out[[length(out) + 1L]] <- list(name = "", part = "order", prog = NULL,
                                    cols = character(), globs = character(),
                                    targets = character(), scope = by, ew = FALSE)
  }
  rl_scope_bind(list(kind = 13L, scope_match = FALSE, groups = out,
                     gby = list(by = by, src = by_src)), envs)
}

# one program over every group stacked (missing columns padded with NA)
rl_prep_global <- function(step, state, mode = TRUE) {
  if (!planned(step, "rx_gshape", rx_global_ok(step))) return(FALSE)
  cols <- model_columns(state$groups)
  p <- rx_program(step, seq_along(step$rules), cols, state$globals, global_step = mode)
  if (is_rx_no(p)) return(NULL)
  rl_scope_bind(list(kind = 2L, scope_match = FALSE,
                     groups = list(list(name = "", prog = p$prog$.ptr, cols = p$cols,
                                        globs = p$globs, targets = p$targets, scope = p$scope,
                                        ew = FALSE))),
                if (length(p$scope)) list(p$env))
}

# `within = <col> == own_<col> [& <rest>]` with candidate values that are
# distinct, rules `any()`/`all()`/`sum()` of quiet elementwise arguments or
# `n()` ([nb_one()]'s shapes): the rest and the arguments compiled over the
# groups stacked, each focal agent reading its one candidate
rl_nb1_shape <- function(step) {
  if (!is.null(step$where) || is.null(step$within) || !rl_nb_default_ok(step$default)) return(NULL)
  key <- equijoin_key(rlang::quo_get_expr(step$within))
  if (is.null(key)) return(NULL)
  shape <- nb_one_shape(step, key)
  if (is.null(shape)) return(NULL)
  exprs <- c(if (!is.null(shape$rest)) list(shape$rest),
             lapply(Filter(function(r) r$fn != "n", shape$rules), `[[`, "arg"))
  comp <- lapply(exprs, rx_compile_memo)
  if (any(vapply(comp, is_rx_no, NA))) return(NULL)
  list(key = key, shape = shape, exprs = exprs, comp = comp)
}
rl_nb_within_ok <- function(step) !is.null(rl_nb1_shape(step))

# FR12: the cross product ([attribute_view()]): `within` over every (focal,
# candidate) pair, compiled over the candidates with the focal agent's `own_`
# columns as values, one focal agent at a time; each rule either an aggregate
# [neighbour_aggregate()] takes (`sum`/`mean`/`any`/`all` of an elementwise
# argument, or a bare `n()`) or any other formula evaluated over the agent's
# neighbours (the `summarise()` path). Not a join (`<col> == own_<col>`), no
# draws, no `n()` in `within` (it counts the pairs), no `.of`.
rl_nb_cross_shape <- function(step) {
  if (!is.null(step$where) || is.null(step$within) || !rl_nb_default_ok(step$default)) return(NULL)
  we <- rlang::quo_get_expr(step$within)
  if (!is.null(equijoin_key(we)) && is.null(dist_bound_key(we))) return(NULL)
  shapes <- lapply(step$rules, function(r) {
    e <- rlang::quo_get_expr(r$quo)
    if (is_bare_count(r$quo)) return(list(fn = "n"))
    if (rlang::is_call(e) && rlang::is_symbol(e[[1]]) &&
        rlang::as_string(e[[1]]) %in% c("sum", "mean", "any", "all")) {
      args <- as.list(e)[-1]
      nms <- names(args) %||% rep("", length(args))
      narm <- FALSE
      if (length(args) == 2L && nms[[2]] == "na.rm" && rlang::is_bool(args[[2]])) {
        narm <- args[[2]]
        args <- args[1]
        nms <- nms[1]
      }
      if (length(args) == 1L && !nzchar(nms[[1]]) && is_elementwise(args[[1]])) {
        return(list(fn = rlang::as_string(e[[1]]), narm = narm, expr = args[[1]]))
      }
    }
    list(fn = "expr", expr = e)
  })
  exprs <- c(list(we), lapply(Filter(function(s) s$fn != "n", shapes), `[[`, "expr"))
  comp <- lapply(exprs, rx_compile_memo)
  if (any(vapply(comp, is_rx_no, NA))) return(NULL)
  heads <- unique(unlist(lapply(comp, `[[`, "heads")))
  syms <- unique(unlist(lapply(comp, `[[`, "syms")))
  if (any(rx_draw_heads %in% heads) || "n" %in% comp[[1]]$heads ||
      ".of" %in% syms) return(NULL)
  envs <- lapply(c(list(step$within), lapply(step$rules, `[[`, "quo")), rlang::quo_get_env)
  if (!all(vapply(envs, identical, NA, envs[[1]]))) return(NULL)
  db <- dist_bound_key(we)
  db_info <- NULL
  if (!is.null(db)) {
    rcp <- rx_compile_memo(db$radius_expr)
    if (!is_rx_no(rcp) && !any(c(rx_draw_heads, "n") %in% rcp$heads) &&
        !any(c(".dist", ".of") %in% rcp$syms)) {
      is_excl_self <- function(p) {
        is.call(p) && length(p) == 3L && identical(p[[1]], as.name("!=")) &&
          ((identical(p[[2]], as.name(".id")) && identical(p[[3]], as.name("own_.id"))) ||
           (identical(p[[2]], as.name("own_.id")) && identical(p[[3]], as.name(".id"))))
      }
      excl <- vapply(db$rest, is_excl_self, NA)
      rem <- db$rest[!excl]
      cand_rest_expr <- NULL
      cand_rest_cp <- NULL
      if (length(rem)) {
        rem_ok <- all(vapply(rem, function(p) {
          if (!is_elementwise(p)) return(FALSE)
          pc <- rx_compile_memo(p)
          !is_rx_no(pc) && !any(c(rx_draw_heads, "n") %in% pc$heads) &&
            !any(startsWith(pc$syms, "own_")) && !any(c(".dist", ".of") %in% pc$syms)
        }, NA))
        if (rem_ok) {
          cand_rest_expr <- Reduce(function(a, b) call("&", a, b), rem)
          cand_rest_cp <- rx_compile_memo(cand_rest_expr)
          if (is_rx_no(cand_rest_cp)) {
            cand_rest_expr <- NULL
            cand_rest_cp <- NULL
          }
        }
      }
      db_info <- list(strict = isTRUE(db$strict),
                      excl_self = any(excl),
                      has_rest = length(rem) > 0L && is.null(cand_rest_expr),
                      cand_rest_expr = cand_rest_expr,
                      cand_rest_cp = cand_rest_cp,
                      rad_expr = db$radius_expr, rad_cp = rcp)
    }
  }
  cand_only <- logical(length(shapes))
  pi <- 2L
  for (r in seq_along(shapes)) {
    fn <- shapes[[r]]$fn
    if (fn == "n") {
      cand_only[[r]] <- TRUE
    } else if (fn == "expr") {
      cand_only[[r]] <- FALSE
      pi <- pi + 1L
    } else {
      rc <- comp[[pi]]
      cand_only[[r]] <- !any(startsWith(rc$syms, "own_")) &&
        !(".dist" %in% rc$syms) && !("n" %in% rc$heads)
      pi <- pi + 1L
    }
  }
  list(shapes = shapes, exprs = exprs, comp = comp, db = db_info, cand_only = cand_only)
}
rl_nb_cross_ok <- function(step) !is.null(rl_nb_cross_shape(step))

rl_prep_nb_cross <- function(step, state) {
  sh <- planned(step, "rx_nbx", rl_nb_cross_shape(step))
  if (is.null(sh)) return(FALSE)
  cols <- model_columns(state$groups)
  sp <- state$space %||% state$lattice
  names <- unique(unlist(lapply(sh$comp, `[[`, "syms")))
  targets <- vapply(step$rules, `[[`, "", "target")
  rel_names <- relation_derived_names(state$relations)
  if (length(rel_names) && any(c(names, targets) %in% rel_names)) return(NULL)
  has_dist <- ".dist" %in% names
  if (has_dist && (is.null(sp) || !".x" %in% cols)) return(NULL)
  use_bucket <- has_dist && !is.null(sh$db) &&
    !any(sh$db$rad_cp$syms %in% cols) && !any(startsWith(sh$db$rad_cp$syms, "own_"))
  use_cand_rest <- use_bucket && !is.null(sh$db$cand_rest_expr)
  exprs <- sh$exprs
  comp <- sh$comp
  if (use_bucket) {
    exprs <- c(exprs, list(sh$db$rad_expr))
    comp <- c(comp, list(sh$db$rad_cp))
    names <- unique(c(names, sh$db$rad_cp$syms))
    if (use_cand_rest) {
      exprs <- c(exprs, list(sh$db$cand_rest_expr))
      comp <- c(comp, list(sh$db$cand_rest_cp))
      names <- unique(c(names, sh$db$cand_rest_cp$syms))
    }
  }
  own <- startsWith(names, "own_") & sub("^own_", "", names) %in% cols
  src <- ifelse(names %in% cols | own | names == ".dist", 1L,
                ifelse(names %in% names(state$globals) | names == ".tick", 2L, 3L))
  env <- rlang::quo_get_env(step$within)
  for (nm in names[src == 3L]) if (!exists(nm, envir = env)) return(NULL)
  if (!rx_heads_ok(unique(unlist(lapply(comp, `[[`, "heads"))),
                   abm_eval_env(step$within, state$globals))) return(NULL)
  o <- order(src)
  names <- names[o]
  src <- src[o]
  prog <- rx_new_program(exprs, names, rep(-1L, length(exprs)),
                         lapply(comp, function(cp) rx_bind(cp$ir, names)))
  dims <- if (!is.null(sp)) as.double(sp$dims) else double()
  torus <- if (!is.null(sp)) as.logical(rep_len(sp$torus %||% FALSE, length(dims))) else logical()
  shift <- if (!is.null(sp) && (identical(sp$type, "grid") || identical(sp$type, "line"))) 1 else 0
  rl_scope_bind(list(kind = 14L, scope_match = FALSE, nb_default = step$default,
       groups = list(list(name = "", prog = prog$.ptr, cols = names[src == 1L],
                          globs = names[src == 2L], targets = vapply(step$rules, `[[`, "", "target"),
                          scope = names[src == 3L], ew = FALSE)),
       cross = list(fns = vapply(sh$shapes, `[[`, "", "fn"),
                    narm = vapply(sh$shapes, function(s) isTRUE(s$narm), NA),
                    cand_only = sh$cand_only,
                    has_dist = has_dist, dims = dims, torus = torus, shift = shift,
                    dist_bucket = use_bucket,
                    dist_strict = if (use_bucket) sh$db$strict else FALSE,
                    dist_excl_self = if (use_bucket) sh$db$excl_self else FALSE,
                    dist_cand_rest = use_cand_rest,
                    dist_has_rest = if (use_bucket) sh$db$has_rest else FALSE)),
    if (any(src == 3L)) list(env))
}

rl_prep_nb_within <- function(step, state) {
  sh <- planned(step, "rx_nb1", rl_nb1_shape(step))
  if (is.null(sh)) return(FALSE)
  cols <- model_columns(state$groups)
  key <- sh$key
  targets <- vapply(step$rules, `[[`, "", "target")
  rel_names <- relation_derived_names(state$relations)
  if (length(rel_names) && any(c(key$focal, key$cand, sh$shape$vars, targets) %in% rel_names)) {
    return(NULL)
  }
  if (!key$focal %in% cols || !key$cand %in% cols || !all(sh$shape$vars %in% cols)) {
    return(NULL)
  }
  env <- abm_eval_env(step$within, state$globals, n = 1L)
  heads <- unique(unlist(lapply(sh$comp, `[[`, "heads")))
  if (!rx_heads_ok(heads, env) || !standard_fns_in(sh$shape$heads, env)) return(NULL)
  names <- unique(unlist(lapply(sh$comp, `[[`, "syms")))
  if (rx_special_sym(names)) return(NULL)
  prog <- if (length(sh$exprs)) {
    rx_new_program(sh$exprs, names, rep(-1L, length(sh$exprs)),
                   lapply(sh$comp, function(cp) rx_bind(cp$ir, names)))
  } else NULL
  if (is.null(prog)) return(NULL)
  list(kind = 5L, scope_match = FALSE, nb_default = step$default,
       groups = list(list(name = "", prog = prog$.ptr, cols = names, globs = character(),
                          targets = vapply(step$rules, `[[`, "", "target"),
                          scope = integer(), ew = FALSE)),
       nb1 = list(focal = key$focal, cand = key$cand, rest = !is.null(sh$shape$rest),
                  fns = vapply(sh$shape$rules, `[[`, "", "fn"),
                  counts = vapply(step$rules, function(r) is_bare_count(r$quo), NA)))
}

# `.where = <direction>` on a lattice ([directional_view()] then
# [neighbour_aggregate()]): each agent's one neighbour in that direction; rules
# `sum`/`mean`/`any`/`all` of a compiled elementwise argument over the candidate's
# columns and the focal agent's `own_` ones, or `n()`
rl_nbw_shape <- function(step) {
  if (is.null(step$where) || !is.null(step$within) || !rl_nb_default_ok(step$default)) return(NULL)
  rules <- lapply(step$rules, function(r) {
    e <- rlang::quo_get_expr(r$quo)
    if (!rlang::is_call(e) || !rlang::is_symbol(e[[1]]) || !is.null(names(e))) return(NULL)
    fn <- rlang::as_string(e[[1]])
    if (fn == "n" && length(e) == 1L) return(list(fn = fn))
    if (!fn %in% c("sum", "mean", "any", "all") || length(e) != 2L ||
        !is_elementwise(e[[2]])) return(NULL)
    cp <- rx_compile_memo(e[[2]])
    if (is_rx_no(cp) || "n" %in% cp$heads || ".of" %in% cp$syms) return(NULL)
    list(fn = fn, arg = e[[2]], cp = cp)
  })
  if (!length(rules) || any(vapply(rules, is.null, NA))) return(NULL)
  envs <- lapply(step$rules, function(r) rlang::quo_get_env(r$quo))
  if (!all(vapply(envs, identical, NA, envs[[1]]))) return(NULL)
  rules
}
rl_nb_where_ok <- function(step) !is.null(rl_nbw_shape(step))

rl_prep_nb_where <- function(step, state) {
  rules <- planned(step, "rx_nbw", rl_nbw_shape(step))
  if (is.null(rules)) return(FALSE)
  lat <- state$lattice
  if (is.null(lat) || !tolower(step$where) %in% names(lat$dir_nb)) {
    return(NULL)
  }
  dir <- lat$dir_nb[[tolower(step$where)]]
  if (!is.integer(dir) || !is.null(attributes(dir))) return(NULL)
  cols <- model_columns(state$groups)
  args <- Filter(function(r) r$fn != "n", rules)
  comp <- lapply(args, `[[`, "cp")
  names <- unique(unlist(lapply(comp, `[[`, "syms"))) %||% character()
  if (rx_special_sym(names)) return(NULL)
  targets <- vapply(step$rules, `[[`, "", "target")
  rel_names <- relation_derived_names(state$relations)
  if (length(rel_names) && any(c(names, targets) %in% rel_names)) return(NULL)
  own <- startsWith(names, "own_") & sub("^own_", "", names) %in% cols
  src <- ifelse(names %in% cols | own, 1L,
                ifelse(names %in% names(state$globals) | names == ".tick", 2L, 3L))
  env <- rlang::quo_get_env(step$rules[[1]]$quo)
  for (nm in names[src == 3L]) if (!exists(nm, envir = env)) return(NULL)
  if (!rx_heads_ok(unique(unlist(lapply(comp, `[[`, "heads"))),
                   abm_eval_env(step$rules[[1]]$quo, state$globals))) return(NULL)
  if (!length(args)) return(NULL)
  o <- order(src)
  names <- names[o]
  src <- src[o]
  prog <- rx_new_program(lapply(args, `[[`, "arg"), names, rep(-1L, length(args)),
                         lapply(comp, function(cp) rx_bind(cp$ir, names)))
  rl_scope_bind(list(kind = 6L, scope_match = FALSE, nb_default = step$default,
       groups = list(list(name = "", prog = prog$.ptr, cols = names[src == 1L],
                          globs = names[src == 2L], targets = vapply(step$rules, `[[`, "", "target"),
                          scope = names[src == 3L], ew = FALSE)),
       nbw = list(on = lat$on, dir = dir, lattice = lat,
                  fns = vapply(rules, `[[`, "", "fn"))),
    if (any(src == 3L)) list(env))
}

# [rl_prep_neighbours()]'s argument program: `list(prog_g, envs)` or `NULL`
rl_nb_program <- function(step, state) {
  shapes <- planned(step, "rl_nbshapes", rl_nb_shapes(step))
  # the arguments compiled over the agents: names that are columns of every
  # group, else one value (a global or from the formulas' environment), as
  # [nb_fast_scalars()] requires; a column some group lacks (NA padding): R
  envs <- lapply(step$rules, function(r) rlang::quo_get_env(r$quo))
  if (!all(vapply(envs, identical, NA, envs[[1]]))) return(NULL)
  comp <- lapply(shapes, function(sh) rx_compile_memo(sh$arg))
  cols <- Reduce(intersect, lapply(state$groups, names))
  names <- unique(unlist(lapply(comp, `[[`, "syms")))
  if (rx_special_sym(names)) return(NULL)
  if (any(names %in% setdiff(model_columns(state$groups), cols))) return(NULL)
  env <- abm_eval_env(step$rules[[1]]$quo, state$globals, n = 1L)
  heads <- unique(unlist(lapply(comp, `[[`, "heads")))
  if (!rx_heads_ok(heads, env)) return(NULL)
  src <- rx_sources(names, cols, state$globals, envs[[1]])
  if (is.null(src)) return(NULL)
  o <- order(src)
  names <- names[o]
  src <- src[o]
  args <- lapply(shapes, `[[`, "arg")
  prog <- rx_new_program(args, names, rep(-1L, length(args)),
                         lapply(comp, function(cp) rx_bind(cp$ir, names)))
  prog_g <- list(name = "", prog = prog$.ptr, cols = names[src == 1L],
                 globs = names[src == 2L], targets = character(), scope = names[src == 3L],
                 ew = FALSE)
  list(prog_g = prog_g, envs = envs)
}

# FR5: `abm_match()` natively for `pair = "random"`, `"one_of"` (no `weight`,
# `among` about the candidate only) and `"opposite_group"` (also with a number
# `.by`: [match_by_opposite()]), no roles; `eligible`/`among` compiled over the
# groups stacked as [bind_groups()] stacks them
rl_match_shape <- function(step) {
  if (!is.null(step$steps)) return(NULL)
  if (!step$pair %in% c("random", "one_of", "opposite_group", "network", "nearest")) return(NULL)
  has_weight <- !is.null(step$weight)
  if (has_weight && (step$pair != "one_of" || mentions_pair(step$weight))) return(NULL)
  has_cost <- !is.null(step$cost)
  if (has_cost && step$pair != "nearest") return(NULL)
  # `nearest` on 1..k coordinates (or continuous `.x`/`.y`) or compiled `cost =`
  if (step$pair == "nearest" && (!is.null(step$role) || mentions_pair(step$among))) return(NULL)
  if (step$pair == "network" && (!identical(step$from, "neighbour") || !is.null(step$role))) {
    return(NULL)
  }
  if (!step$pair %in% c("one_of", "nearest") && !is.null(step$among)) return(NULL)
  if (!is.null(step$dot_by) && (step$pair != "opposite_group" || !is.null(step$role))) return(NULL)
  by <- if (step$pair == "opposite_group") {
    if (is.null(step$by)) return(NULL)
    e <- rlang::quo_get_expr(step$by)
    if (!rlang::is_symbol(e)) return(NULL)
    rlang::as_string(e)
  } else if (step$pair == "nearest" && !has_cost && !is.null(step$by)) {
    b <- tryCatch(by_columns(step$by), error = function(e) NULL)
    if (!length(b)) return(NULL)
    b
  }
  cost_cp <- if (has_cost) {
    cp <- rx_compile_memo(rlang::quo_get_expr(step$cost))
    if (is_rx_no(cp)) return(NULL)
    cp
  }
  dot_by <- if (!is.null(step$dot_by)) {
    e <- rlang::quo_get_expr(step$dot_by)
    if (!rlang::is_symbol(e)) return(NULL)
    rlang::as_string(e)
  }
  # roles: two named conditions ([assign_roles()]), evaluated after the draws,
  # so they must not draw themselves
  roles <- NULL
  if (!is.null(step$role)) {
    re <- rlang::quo_get_expr(step$role)
    if (!is.call(re)) return(NULL)
    roles <- as.list(re)[-1]
    if (length(roles) != 2L || is.null(names(roles)) || !all(nzchar(names(roles)))) return(NULL)
  }
  conds <- c(Filter(Negate(is.null), list(eligible = step$eligible, among = step$among)),
             if (!is.null(roles)) lapply(roles, rlang::new_quosure, env = rlang::quo_get_env(step$role)),
             if (has_weight) list(weight = step$weight))
  if (!is.null(step$among) && mentions_pair(step$among)) return(NULL)
  comp <- lapply(conds, function(q) rx_compile_memo(rlang::quo_get_expr(q)))
  if (any(vapply(comp, is_rx_no, NA))) return(NULL)
  if (!is.null(roles)) {
    r_idx <- seq.int(length(comp) - as.integer(has_weight) - 1L, length(comp) - as.integer(has_weight))
    if (any(rx_draw_heads %in% unlist(lapply(comp[r_idx], `[[`, "heads")))) return(NULL)
  }
  envs <- lapply(conds, rlang::quo_get_env)
  if (length(envs) > 1L && !all(vapply(envs, identical, NA, envs[[1]]))) return(NULL)
  list(by = by, dot_by = dot_by, conds = conds, comp = comp, cost_cp = cost_cp,
       has_weight = has_weight,
       roles = if (!is.null(roles)) names(roles))
}
rl_match_ok <- function(step) !is.null(rl_match_shape(step))

rl_prep_match <- function(step, state) {
  sh <- planned(step, "rx_match", rl_match_shape(step))
  if (is.null(sh)) return(FALSE)
  rel_names <- relation_derived_names(state$relations)
  among_rel <- ""
  among_back <- FALSE
  conds <- sh$conds
  comp <- sh$comp
  has_among_prog <- !is.null(step$among)
  if (length(rel_names)) {
    # `pair = "one_of"` with bare `among = .R` / `.R_back`
    if (step$pair == "one_of" && !is.null(step$among) &&
        rlang::is_symbol(rlang::quo_get_expr(step$among))) {
      rh <- resolve_relation_name(rlang::as_string(rlang::quo_get_expr(step$among)), state$relations)
      if (!is.null(rh) && is.null(rh$col)) {
        among_rel <- rh$rel
        among_back <- isTRUE(rh$back)
        ai <- match("among", names(conds))
        if (!is.na(ai)) {
          conds <- conds[-ai]
          comp <- comp[-ai]
        }
        has_among_prog <- FALSE
      }
    }
    msyms <- c(sh$by, sh$dot_by,
               unlist(lapply(comp, `[[`, "syms"), use.names = FALSE),
               if (!is.null(sh$cost_cp)) sh$cost_cp$syms)
    if (any(msyms %in% rel_names)) return(NULL)
  }
  cols <- model_columns(state$groups)
  by_cols <- sh$by
  use_torus <- FALSE
  if (step$pair == "nearest" && is.null(step$cost) && is.null(step$by)) {
    if (is.null(state$space) || !identical(state$space$type, "continuous")) return(NULL)
    by_cols <- if (length(state$space$dims) == 1L) ".x" else c(".x", ".y")
    use_torus <- any(state$space$torus)
  }
  if (length(by_cols) && !all(by_cols %in% cols)) return(NULL)
  if (!is.null(sh$dot_by) && !sh$dot_by %in% cols) return(NULL)
  prog_g <- list(name = "", prog = NULL, cols = character(), globs = character(),
                 targets = character(), scope = character(), ew = FALSE)
  envs <- list()
  if (length(conds)) {
    q1 <- conds[[1]]
    env <- rlang::quo_get_env(q1)
    names <- unique(unlist(lapply(comp, `[[`, "syms"))) %||% character()
    if (rx_special_sym(names)) return(NULL)
    heads <- unique(unlist(lapply(comp, `[[`, "heads"))) %||% character()
    if (!rx_heads_ok(heads, abm_eval_env(q1, state$globals, n = 1L))) return(NULL)
    if (!rx_rng_heads_ok(heads)) return(NULL)
    src <- ifelse(names %in% cols, 1L, ifelse(names %in% names(state$globals) | names == ".tick", 2L, 3L))
    for (nm in names[src == 3L]) if (!exists(nm, envir = env)) return(NULL)
    o <- order(src)
    names <- names[o]
    src <- src[o]
    exprs <- lapply(conds, rlang::quo_get_expr)
    prog <- rx_new_program(exprs, names, rep(-1L, length(exprs)),
                           lapply(comp, function(cp) rx_bind(cp$ir, names)))
    prog_g$prog <- prog$.ptr
    prog_g$cols <- names[src == 1L]
    prog_g$globs <- names[src == 2L]
    prog_g$scope <- names[src == 3L]
    if (any(src == 3L)) envs <- list(env)
  }
  groups <- list(prog_g)
  pair_code <- match(step$pair, c("random", "one_of", "opposite_group", "network", "nearest"))
  sp <- state$space %||% state$lattice
  dims <- if (!is.null(sp)) as.double(sp$dims) else double()
  torus <- if (!is.null(sp)) as.logical(rep_len(sp$torus %||% FALSE, length(dims))) else logical()
  if (!is.null(sh$cost_cp)) {
    cp <- sh$cost_cp
    cenv <- rlang::quo_get_env(step$cost)
    if (!rx_heads_ok(cp$heads, abm_eval_env(step$cost, state$globals, n = 1L))) return(NULL)
    if (!rx_rng_heads_ok(cp$heads)) return(NULL)
    csyms <- cp$syms
    csrc <- integer(length(csyms))
    vkind <- integer(length(csyms))
    vcol <- character(length(csyms))
    for (j in seq_along(csyms)) {
      s <- csyms[[j]]
      if (s %in% cols) {
        csrc[[j]] <- 1L; vkind[[j]] <- 0L; vcol[[j]] <- s
      } else if (startsWith(s, "own_") && sub("^own_", "", s) %in% cols) {
        csrc[[j]] <- 1L; vkind[[j]] <- 1L; vcol[[j]] <- sub("^own_", "", s)
      } else if (s == ".of") {
        csrc[[j]] <- 1L; vkind[[j]] <- 2L; vcol[[j]] <- ".of"
      } else if (s == ".dist") {
        if (is.null(sp) || !".x" %in% cols) return(NULL)
        csrc[[j]] <- 1L; vkind[[j]] <- 3L; vcol[[j]] <- ".dist"
      } else if (s %in% names(state$globals) || s == ".tick") {
        csrc[[j]] <- 2L
      } else if (exists(s, envir = cenv)) {
        csrc[[j]] <- 3L
      } else {
        return(NULL)
      }
    }
    o <- order(csrc)
    csyms <- csyms[o]; csrc <- csrc[o]; vkind <- vkind[o]; vcol <- vcol[o]
    cprog <- rx_new_program(list(rlang::quo_get_expr(step$cost)), csyms, -1L,
                            list(rx_bind(cp$ir, csyms)))
    if (any(csrc == 3L)) envs <- c(envs, list(cenv))
    groups[[2L]] <- list(name = "", prog = cprog$.ptr, cols = vcol[csrc == 1L],
                         globs = csyms[csrc == 2L], targets = character(),
                         scope = csyms[csrc == 3L], ew = FALSE, mcols = character(),
                         wslot = as.integer(vkind[csrc == 1L]))
    pair_code <- 6L
  }
  rl_scope_bind(list(kind = 7L, scope_match = FALSE, groups = groups,
                     mt = list(pair = pair_code,
                               roles = sh$roles %||% character(),
                               size = step$size, by = by_cols %||% character(),
                               dot_by = sh$dot_by %||% character(),
                               eligible = !is.null(step$eligible),
                               among = has_among_prog,
                               among_rel = among_rel,
                               among_back = among_back,
                               has_weight = isTRUE(sh$has_weight),
                               use_torus = use_torus,
                               dims = dims, torus = torus)), envs)
}

# FR6: `abm_death(when =)` natively: the condition compiled per group it applies
# to ([condition_routes()]), reading the standing match's columns as a rule does
rl_death_step <- function(step) {
  structure(list(rules = list(list(target = ".dead", quo = step$when)), by = NULL,
                 plan = new.env(parent = emptyenv())), class = c("abm_rules", "abm_step"))
}
rl_death_ok <- function(step) {
  !is_rx_no(rx_compile_memo(rlang::quo_get_expr(step$when)))
}
rl_prep_death <- function(step, state) {
  if (!planned(step, "rx_death", rl_death_ok(step))) return(FALSE)
  rel_names <- relation_derived_names(state$relations)
  if (length(rel_names) && any(all.vars(rlang::quo_get_expr(step$when)) %in% rel_names)) {
    return(NULL)
  }
  fake <- planned(step, "rx_death_step", rl_death_step(step))
  mnames <- if (planned(step, "uses_match", reads_match(list(step$when))) ||
                length(planned(step, "cvars", cell_vars(list(step$when)))) > 0L) {
    rx_match_names(fake) %||% return(NULL)
  } else character()
  pcols <- sub("^partner_", "", mnames[startsWith(mnames, "partner_")])
  if (!all(pcols %in% model_columns(state$groups))) return(NULL)
  ccols <- sub("^cell_", "", mnames[startsWith(mnames, "cell_")])
  if (length(ccols) && (is.null(state$lattice$on) ||
      !all(ccols %in% names(state$groups[[state$lattice$on]])))) return(NULL)
  applies <- condition_routes(step, state)
  wired_on <- if (!is.null(state$lattice) && !is.null(state$lattice$on)) state$lattice$on else ""
  groups <- list()
  envs <- list()
  for (nm in names(state$groups)[applies]) {
    p <- rx_program(fake, 1L, names(state$groups[[nm]]), state$globals, match_names = mnames)
    if (is_rx_no(p)) return(NULL)
    if (length(p$scope)) envs <- c(envs, list(p$env))
    groups[[length(groups) + 1L]] <- list(name = nm, prog = p$prog$.ptr, cols = p$cols,
                                          globs = p$globs, targets = character(),
                                          scope = p$scope, ew = FALSE, mcols = p$mcols)
  }
  rl_scope_bind(list(kind = 8L, scope_match = FALSE, groups = groups,
                     death = list(prune = isTRUE(step$prune_edges), wired = wired_on)), envs)
}

# FR6: `abm_birth(when =)` natively (FR12: with `attach_via`): per group it applies
# to, programs for `when` (over the group), `times` and `cost` (over the
# parents) and `inherit` (over one row per offspring)
rl_fake_rules <- function(rules) {
  structure(list(rules = rules, by = NULL, plan = new.env(parent = emptyenv())),
            class = c("abm_rules", "abm_step"))
}
rl_birth_parts <- function(step) {
  list(when = if (!is.null(step$when)) rl_fake_rules(list(list(target = ".parent", quo = step$when))),
       times = if (!is.null(step$times)) rl_fake_rules(list(list(target = ".times", quo = step$times))),
       cost = if (length(step$cost)) rl_fake_rules(step$cost),
       inherit = if (length(step$inherit)) rl_fake_rules(step$inherit))
}
rl_birth_ok <- function(step) {
  targets <- vapply(c(step$cost, step$inherit), `[[`, "", "target")
  if (any(targets %in% c(".cell", "partner_.cell", ".id", ".group"))) return(FALSE)
  if (is.null(step$when)) {
    # `n =` ([birth_by_count()], FR12): each rule over the template as the rules
    # before it left it, with the newborns' ids; natively when no rule reads an
    # id, a group or an earlier target. `from = "parent"` is R's error there.
    if (identical(step$attach_via$from, "parent")) return(FALSE)
    rules <- c(step$cost, step$inherit)
    for (i in seq_along(rules)) {
      v <- all.vars(rlang::quo_get_expr(rules[[i]]$quo))
      if (any(v %in% c(".id", ".group", targets[seq_len(i - 1L)]))) return(FALSE)
    }
  }
  quos <- c(list(step$when, step$times), lapply(c(step$cost, step$inherit), `[[`, "quo"))
  all(vapply(Filter(Negate(is.null), quos), function(q) {
    !is_rx_no(rx_compile_memo(rlang::quo_get_expr(q)))
  }, NA))
}
rl_prep_birth <- function(step, state) {
  if (!planned(step, "rx_birth", rl_birth_ok(step))) return(FALSE)
  parts <- planned(step, "rx_birth_parts", rl_birth_parts(step))
  quos <- c(list(step$when, step$times), lapply(c(step$cost, step$inherit), `[[`, "quo"))
  rel_names <- relation_derived_names(state$relations)
  if (length(rel_names)) {
    btg <- vapply(c(step$cost, step$inherit), `[[`, "", "target")
    bsy <- unlist(lapply(Filter(Negate(is.null), quos), function(q) all.vars(rlang::quo_get_expr(q))),
                  use.names = FALSE)
    if (any(c(btg, bsy) %in% rel_names)) return(NULL)
  }
  mnames <- if (planned(step, "uses_match", reads_match(quos)) ||
                length(planned(step, "cvars", cell_vars(quos))) > 0L) {
    rx_match_names(rl_fake_rules(lapply(Filter(Negate(is.null), quos), function(q) {
      list(target = ".x", quo = q)
    }))) %||% return(NULL)
  } else character()
  pcols <- sub("^partner_", "", mnames[startsWith(mnames, "partner_")])
  if (!all(pcols %in% model_columns(state$groups))) return(NULL)
  ccols <- sub("^cell_", "", mnames[startsWith(mnames, "cell_")])
  if (length(ccols) && (is.null(state$lattice$on) ||
      !all(ccols %in% names(state$groups[[state$lattice$on]])))) return(NULL)
  wired_on <- if (!is.null(state$lattice)) state$lattice$on else NULL
  target_group <- ""
  if (is.null(step$when)) {
    if (!is.null(step$group)) {
      if (!step$group %in% names(state$groups)) return(NULL)
      if (!is.null(wired_on) && identical(step$group, wired_on)) return(NULL)
      target_group <- step$group
    } else {
      cand <- setdiff(names(state$groups), wired_on)
      if (length(cand) != 1L) return(NULL)
      target_group <- cand[[1]]
    }
    applies <- target_group
  } else {
    applies <- names(state$groups)[condition_routes(step, state)]
  }
  groups <- list()
  envs <- list()
  for (nm in applies) {
    cols <- names(state$groups[[nm]])
    for (part in names(parts)) {
      f <- parts[[part]]
      if (is.null(f)) next
      p <- rx_program(f, seq_along(f$rules), cols, state$globals, match_names = mnames)
      if (is_rx_no(p) && part == "when") return(NULL)
      # `times`/`cost`/`inherit` are only evaluated where there are parents: a
      # group they do not fit (Wolf-Sheep's patches have no `energy`) declines
      # only when it has parents
      if (is_rx_no(p) || (part %in% c("cost", "inherit") && !all(p$targets %in% cols))) {
        groups[[length(groups) + 1L]] <- list(name = nm, part = part, prog = NULL,
                                              cols = character(), globs = character(),
                                              targets = character(), scope = character(),
                                              ew = FALSE, mcols = character())
        next
      }
      if (length(p$scope)) envs <- c(envs, list(p$env))
      groups[[length(groups) + 1L]] <- list(name = nm, part = part, prog = p$prog$.ptr,
                                            cols = p$cols, globs = p$globs, targets = p$targets,
                                            scope = p$scope, ew = FALSE, mcols = p$mcols)
    }
  }
  # `as.integer(round(<times>))`, R's own functions over the counts
  rnd <- rx_compile_memo(quote(as.integer(round(.t))))
  nb <- rx_number_calls(list(rx_bind(rnd$ir, ".t")))
  round_prog <- planned(step, "rx_round", RxProgram$new(nb$irs, -1L, nb$fns))
  attach_code <- if (is.null(step$attach_via)) 0L
                 else if (identical(step$attach_via$from, "parent")) 1L
                 else if (identical(step$attach_via$from, "random_edge")) 3L
                 else 2L
  rl_scope_bind(list(kind = 9L, scope_match = FALSE, groups = groups,
                     birth = list(round = round_prog$.ptr,
                                  attach = attach_code,
                                  links = as.integer(step$links %||% 1L),
                                  count = if (is.null(step$when)) as.integer(step$n) else -1L,
                                  group = target_group,
                                  wired = wired_on %||% "")),
                envs)
}

# FR7/FR13/FR14 + Batch B: `abm_move()` on a lattice (random, random_empty,
# random_cell, stay, direction, uphill/downhill, when=, per-agent range=) or
# continuous space (`offset()`, `forward()`)
rl_move_ok <- function(step) {
  if (!is.null(step$when) && is_rx_no(rx_compile_memo(rlang::quo_get_expr(step$when)))) {
    return(FALSE)
  }
  if (!is.null(step$range_quo) && is_rx_no(rx_compile_memo(rlang::quo_get_expr(step$range_quo)))) {
    return(FALSE)
  }
  if (identical(step$mode, "stay")) return(TRUE)
  if (step$mode %in% c("offset", "forward")) {
    return(all(vapply(step$move_args, function(q) !is_rx_no(rx_compile_memo(rlang::quo_get_expr(q))), NA)))
  }
  if (identical(step$mode, "direction")) {
    # FR13: a compiled heading that does not draw (it is evaluated after the
    # movers' order is drawn)
    cp <- rx_compile_memo(rlang::quo_get_expr(step$direction))
    return(!is_rx_no(cp) && !any(rx_draw_heads %in% cp$heads))
  }
  if (step$mode %in% c("uphill", "downhill")) {
    cp <- rx_compile_memo(rlang::quo_get_expr(step$expr))
    return(!is_rx_no(cp))
  }
  step$mode %in% c("random_neighbour", "random_empty_neighbour", "random_empty", "random_cell")
}
rl_prep_move <- function(step, state) {
  if (!planned(step, "rx_move", rl_move_ok(step))) return(FALSE)
  if (identical(step$mode, "stay")) {
    return(list(kind = 10L, scope_match = FALSE, groups = list(),
                mv = list(mode = 1L, who = character(), avoid = FALSE, lattice = NULL,
                          nb = NULL, x = NULL, y = NULL, ncell = 0L, dir = NULL,
                          range = 1L, axes = FALSE, grad = 0L, on = "",
                          has_when = FALSE, has_range = FALSE)))
  }
  if (step$mode %in% c("offset", "forward")) {
    sp <- state$space
    if (is.null(sp) || !identical(sp$type, "continuous")) return(NULL)
    dims <- as.double(sp$dims)
    nd <- length(dims)
    torus <- as.logical(rep_len(sp$torus, nd))
    if (identical(step$mode, "offset") && length(step$move_args) != nd) return(NULL)
    with_x <- names(state$groups)[vapply(state$groups, function(g) ".x" %in% names(g), NA)]
    who <- step$who %||% with_x
    if (!length(who) || !all(who %in% with_x)) return(NULL)
    if (nd == 2L && !all(vapply(state$groups[who], function(g) ".y" %in% names(g), NA))) {
      return(NULL)
    }
    groups <- list()
    envs <- list()
    if (!is.null(step$when)) {
      if (length(cell_vars(list(step$when))) > 0L) return(NULL)
      wfake <- planned(step, "rx_move_when",
                       rl_fake_rules(list(list(target = ".when", quo = step$when))))
      wcols <- model_columns(state$groups[who])
      pw <- rx_program(wfake, 1L, wcols, state$globals)
      if (is_rx_no(pw)) return(NULL)
      if (length(pw$scope)) envs <- c(envs, list(pw$env))
      groups[[length(groups) + 1L]] <- list(name = "", part = "when", prog = pw$prog$.ptr,
                                            cols = pw$cols, globs = pw$globs,
                                            targets = character(), scope = pw$scope,
                                            ew = FALSE, mcols = character())
    }
    mfake <- planned(step, "rx_move_cont", rl_fake_rules(lapply(step$move_args, function(q) {
      list(target = ".d", quo = q)
    })))
    for (nm in who) {
      p <- rx_program(mfake, seq_along(step$move_args), names(state$groups[[nm]]), state$globals)
      if (is_rx_no(p)) return(NULL)
      if (length(p$scope)) envs <- c(envs, list(p$env))
      groups[[length(groups) + 1L]] <- list(name = nm, prog = p$prog$.ptr, cols = p$cols,
                                            globs = p$globs, targets = character(),
                                            scope = p$scope, ew = FALSE, mcols = character())
    }
    return(rl_scope_bind(list(kind = 10L, scope_match = FALSE, groups = groups,
         mv = list(mode = if (identical(step$mode, "offset")) 5L else 6L,
                   who = who, avoid = FALSE, lattice = NULL, nb = NULL,
                   x = NULL, y = NULL, ncell = 0L, dir = NULL, range = 1L,
                   axes = FALSE, grad = 0L, on = "",
                   has_when = !is.null(step$when), has_range = FALSE,
                   space = sp, dims = dims, torus = torus)), envs))
  }
  lat <- state$lattice
  if (is.null(lat)) return(NULL)
  along_ok <- is.null(step$along) || identical(step$along, "space") ||
    (!is.null(lat$on) && identical(step$along, lat$on)) ||
    (identical(step$along, "network") && is.null(state$space))
  if (!along_ok) return(NULL)
  with_cell <- names(state$groups)[vapply(state$groups, function(g) ".cell" %in% names(g), NA)]
  who <- step$who %||% with_cell
  # an unknown group, or one not on the lattice, is R's error
  if (!length(who) || !all(who %in% with_cell)) return(NULL)
  if (!is.list(lat$nb) || !is.null(lat$x) && !is.atomic(lat$x)) return(NULL)
  groups <- list()
  envs <- list()
  dir <- NULL
  grad <- if (identical(step$mode, "uphill")) 1L else if (identical(step$mode, "downhill")) -1L else 0L
  on <- ""
  # cell_<col> check for when / direction / range_quo
  aux_quos <- Filter(Negate(is.null), list(step$when, step$direction, step$range_quo))
  ccols <- cell_vars(aux_quos)
  mnames <- if (length(ccols)) {
    if (is.null(lat$on) || !lat$on %in% names(state$groups) ||
        !all(ccols %in% names(state$groups[[lat$on]]))) return(NULL)
    paste0("cell_", ccols)
  } else character()
  if (!is.null(step$when)) {
    wfake <- planned(step, "rx_move_when",
                     rl_fake_rules(list(list(target = ".when", quo = step$when))))
    wcols <- model_columns(state$groups[who])
    pw <- rx_program(wfake, 1L, wcols, state$globals, match_names = mnames)
    if (is_rx_no(pw)) return(NULL)
    if (length(pw$scope)) envs <- c(envs, list(pw$env))
    groups[[length(groups) + 1L]] <- list(name = "", part = "when", prog = pw$prog$.ptr,
                                          cols = pw$cols, globs = pw$globs,
                                          targets = character(), scope = pw$scope,
                                          ew = FALSE, mcols = pw$mcols)
  }
  if (!is.null(step$range_quo)) {
    rfake <- planned(step, "rx_move_range",
                     rl_fake_rules(list(list(target = ".range", quo = step$range_quo))))
    for (nm in who) {
      pr <- rx_program(rfake, 1L, names(state$groups[[nm]]), state$globals, match_names = mnames)
      if (is_rx_no(pr)) return(NULL)
      if (length(pr$scope)) envs <- c(envs, list(pr$env))
      groups[[length(groups) + 1L]] <- list(name = nm, part = "range", prog = pr$prog$.ptr,
                                            cols = pr$cols, globs = pr$globs,
                                            targets = character(), scope = pr$scope,
                                            ew = FALSE, mcols = pr$mcols)
    }
  }
  if (identical(step$mode, "direction") || isTRUE(step$axes_only)) {
    dir <- lat$dir_nb
    if (!is.list(dir) || is.null(names(dir))) return(NULL)
  }
  if (identical(step$mode, "direction")) {
    fake <- planned(step, "rx_move_dir",
                    rl_fake_rules(list(list(target = ".heading", quo = step$direction))))
    for (nm in who) {
      p <- rx_program(fake, 1L, names(state$groups[[nm]]), state$globals, match_names = mnames)
      if (is_rx_no(p)) return(NULL)
      if (length(p$scope)) envs <- c(envs, list(p$env))
      groups[[length(groups) + 1L]] <- list(name = nm, prog = p$prog$.ptr, cols = p$cols,
                                            globs = p$globs, targets = character(),
                                            scope = p$scope, ew = FALSE, mcols = p$mcols)
    }
  } else if (grad != 0L) {
    on <- lat$on %||% ""
    wired <- if (nzchar(on)) state$groups[[on]] else NULL
    if (nzchar(on) && (is.null(wired) || !nrow(wired))) return(NULL)
    cp <- rx_compile_memo(rlang::quo_get_expr(step$expr))
    env <- rlang::quo_get_env(step$expr)
    if (!rx_heads_ok(cp$heads, abm_eval_env(step$expr, state$globals, n = 1L))) return(NULL)
    if (!rx_rng_heads_ok(cp$heads)) return(NULL)
    wcols <- if (!is.null(wired)) names(wired) else c(".id", ".x", if (!is.null(lat$y)) ".y")
    reserved <- c(".id", ".group", ".x", ".y", ".cell")
    for (nm in who) {
      mcols <- names(state$groups[[nm]])
      m_over <- setdiff(mcols, reserved)
      syms <- cp$syms
      src <- integer(length(syms))
      col_nm <- character(length(syms))
      vkind <- integer(length(syms))
      for (j in seq_along(syms)) {
        s <- syms[[j]]
        if (startsWith(s, "own_") && sub("^own_", "", s) %in% mcols) {
          src[[j]] <- 1L; col_nm[[j]] <- sub("^own_", "", s); vkind[[j]] <- 1L
        } else if (s == ".dist") {
          sp <- state$space %||% lat
          if (is.null(sp) || !".x" %in% wcols || !".x" %in% mcols) return(NULL)
          src[[j]] <- 1L; col_nm[[j]] <- ".dist"; vkind[[j]] <- 2L
        } else if (s %in% m_over) {
          src[[j]] <- 1L; col_nm[[j]] <- s; vkind[[j]] <- 1L
        } else if (s %in% wcols) {
          src[[j]] <- 1L; col_nm[[j]] <- s; vkind[[j]] <- 0L
        } else if (s %in% names(state$globals) || s == ".tick") {
          src[[j]] <- 2L
        } else if (exists(s, envir = env)) {
          src[[j]] <- 3L
        } else {
          return(NULL)
        }
      }
      o <- order(src)
      syms <- syms[o]; src <- src[o]; col_nm <- col_nm[o]; vkind <- vkind[o]
      exprs <- list(rlang::quo_get_expr(step$expr))
      prog <- rx_new_program(exprs, syms, -1L, list(rx_bind(cp$ir, syms)))
      if (any(src == 3L)) envs <- c(envs, list(env))
      groups[[length(groups) + 1L]] <- list(name = nm, prog = prog$.ptr,
                                            cols = col_nm[src == 1L],
                                            globs = syms[src == 2L], targets = character(),
                                            scope = syms[src == 3L], ew = FALSE,
                                            mcols = character(),
                                            wslot = as.integer(vkind[src == 1L]))
    }
  }
  mode_code <- if (step$mode %in% c("random_empty", "random_cell")) 2L
               else if (identical(step$mode, "direction")) 3L
               else if (grad != 0L) 4L
               else 0L
  sp_grad <- if (grad != 0L) state$space %||% lat else NULL
  dims_g <- if (!is.null(sp_grad)) as.double(sp_grad$dims) else double()
  torus_g <- if (!is.null(sp_grad)) as.logical(rep_len(sp_grad$torus %||% FALSE, length(dims_g))) else logical()
  rl_scope_bind(list(kind = 10L, scope_match = FALSE, groups = groups,
       mv = list(mode = mode_code, who = who, avoid = isTRUE(step$avoid_occupied),
                 lattice = lat, nb = lat$nb, x = lat$x, y = lat$y %||% NULL,
                 ncell = as.integer(lat$ncell), dir = dir,
                 range = as.integer(step$range %||% 1L), axes = isTRUE(step$axes_only),
                 grad = grad, on = on,
                 has_when = !is.null(step$when),
                 has_range = !is.null(step$range_quo),
                 dims = dims_g, torus = torus_g)), envs)
}

# FR14: `abm_draw()` natively ([run_draw()]): rules evaluated over the edge
# table (excluding `.draw_*`/`.back_*`), once per edge or twice for `"endpoint"`,
# written to `.draw_<target>` and `.back_<target>`, clearing `state$stale_draws`
rl_draw_ok <- function(step) {
  !any(vapply(rx_rules(step), is_rx_no, NA))
}
rl_prep_draw <- function(step, state) {
  if (!planned(step, "rx_draw", rl_draw_ok(step))) return(FALSE)
  topo <- rl_topo_spec(step$via, state)
  if (is.null(topo)) return(NULL)
  edges <- state[[topo$efield]]
  if (is.null(edges)) return(NULL)
  agent_cols <- setdiff(model_columns(state$groups), c(".id", ".group"))
  tg <- vapply(step$rules, `[[`, "", "target")
  if (any(tg %in% agent_cols)) return(NULL)
  enms <- names(edges)
  vcols <- enms[!startsWith(enms, ".draw_") & !startsWith(enms, ".back_")]
  p <- rx_program(step, seq_along(step$rules), vcols, state$globals)
  if (is_rx_no(p)) return(NULL)
  rl_scope_bind(list(kind = 16L, scope_match = FALSE,
       groups = list(list(name = "", prog = p$prog$.ptr, cols = p$cols, globs = p$globs,
                          targets = paste0(".draw_", tg), scope = p$scope, ew = FALSE,
                          all_targets = paste0(".back_", tg))),
       draw = list(endpoint = identical(step$each, "endpoint"), efield = topo$efield)),
    if (length(p$scope)) list(p$env))
}

# FR15a + relation `via =`: network and relation `abm_link()` / `abm_unlink()`
rl_link_ok <- function(step) {
  (is.null(step$when) || !is_rx_no(rx_compile_memo(rlang::quo_get_expr(step$when)))) &&
    (is.null(step$to) || !is_rx_no(rx_compile_memo(rlang::quo_get_expr(step$to)))) &&
    (!length(step$rules) ||
      all(vapply(step$rules, function(r) !is_rx_no(rx_compile_memo(rlang::quo_get_expr(r$quo))), NA)))
}
rl_prep_link <- function(step, state) {
  if (!planned(step, "rx_link", rl_link_ok(step))) return(FALSE)
  if (is.null(step$via)) {
    if (mentions_relation(step$when, state$relations)) return(NULL)
    groups <- list()
    envs <- list()
    if (!is.null(step$when)) {
      fake <- planned(step, "rx_link_fake",
                      rl_fake_rules(list(list(target = ".keep", quo = step$when))))
      mnames <- if (planned(step, "uses_match", reads_match(list(step$when))) ||
                    length(planned(step, "cvars", cell_vars(list(step$when)))) > 0L) {
        rx_match_names(fake) %||% return(NULL)
      } else character()
      cols <- model_columns(state$groups)
      pcols <- sub("^partner_", "", mnames[startsWith(mnames, "partner_")])
      if (!all(pcols %in% cols)) return(NULL)
      ccols <- sub("^cell_", "", mnames[startsWith(mnames, "cell_")])
      if (length(ccols) && (is.null(state$lattice$on) ||
          !all(ccols %in% names(state$groups[[state$lattice$on]])))) return(NULL)
      p <- rx_program(fake, 1L, cols, state$globals, match_names = mnames)
      if (is_rx_no(p)) return(NULL)
      if (length(p$scope)) envs <- list(p$env)
      groups <- list(list(name = "", prog = p$prog$.ptr, cols = p$cols, globs = p$globs,
                          targets = character(), scope = p$scope, ew = FALSE, mcols = p$mcols))
    }
    return(rl_scope_bind(list(kind = 18L, scope_match = FALSE, groups = groups,
         link = list(drop = isTRUE(step$drop), when = !is.null(step$when),
                     via = "", has_to = FALSE)), envs))
  }
  rel <- state$relations[[step$via]]
  if (is.null(rel)) return(NULL)
  tg <- if (length(step$rules)) vapply(step$rules, `[[`, "", "target") else character()
  if (!all(tg %in% rel$cols)) return(NULL)
  quos <- c(if (!is.null(step$when)) list(step$when),
            if (!is.null(step$to)) list(step$to),
            lapply(step$rules %||% list(), `[[`, "quo"))
  rel_spec <- NULL
  rel_mnames <- character()
  rel_names <- relation_derived_names(state$relations)
  if (length(rel_names) && length(quos)) {
    qsyms <- unique(unlist(lapply(quos, function(q) all.vars(rlang::quo_get_expr(q))), use.names = FALSE))
    rel_rd <- intersect(qsyms, rel_names)
    if (length(rel_rd)) {
      hits <- lapply(rel_rd, resolve_relation_name, relations = state$relations)
      rel_mnames <- rel_rd
      rel_spec <- list(
        names = rel_rd,
        rel = vapply(hits, `[[`, "", "rel"),
        col = vapply(hits, function(h) h$col %||% "", ""),
        back = vapply(hits, function(h) isTRUE(h$back), NA)
      )
    }
  }
  groups <- list()
  envs <- list()
  if (length(quos)) {
    fake <- planned(step, "rx_link_rel_fake", rl_fake_rules(lapply(quos, function(q) {
      list(target = ".link", quo = q)
    })))
    mnames <- if (planned(step, "uses_match", reads_match(quos)) ||
                  length(planned(step, "cvars", cell_vars(quos))) > 0L) {
      rx_match_names(fake) %||% return(NULL)
    } else character()
    cols <- model_columns(state$groups)
    pcols <- sub("^partner_", "", mnames[startsWith(mnames, "partner_")])
    if (!all(pcols %in% cols)) return(NULL)
    ccols <- sub("^cell_", "", mnames[startsWith(mnames, "cell_")])
    if (length(ccols) && (is.null(state$lattice$on) ||
        !all(ccols %in% names(state$groups[[state$lattice$on]])))) return(NULL)
    mnames <- union(mnames, rel_mnames)
    p <- rx_program(fake, seq_along(quos), cols, state$globals, match_names = mnames)
    if (is_rx_no(p)) return(NULL)
    if (length(p$scope)) envs <- list(p$env)
    groups <- list(list(name = "", prog = p$prog$.ptr, cols = p$cols, globs = p$globs,
                        targets = tg, scope = p$scope, ew = FALSE, mcols = p$mcols))
  }
  rl_scope_bind(list(kind = 18L, scope_match = FALSE, groups = groups,
       link = list(drop = isTRUE(step$drop), when = !is.null(step$when),
                   via = step$via, has_to = !is.null(step$to), rel = rel_spec)), envs)
}

# FR7/FR15a + Batch B: `abm_tell(to = <id> | "neighbours")` natively ([run_tell()],
# including `.order =`, `via =`, `space_edges`, `.resolve = "add"`, `cell_<col>`)
rl_tell_ok <- function(step) {
  if (!step$to %in% c("id", "neighbours") ||
      !step$resolve %in% c("last", "first", "sum", "add", "mean", "error", "collect", "min", "max")) {
    return(FALSE)
  }
  if (any(vapply(step$rules, function(r) r$target %in% c(".cell", "partner_.cell", ".id"), NA))) {
    return(FALSE)
  }
  quos <- c(list(step$when), if (identical(step$to, "id")) list(step$to_quo),
            list(step$order), lapply(step$rules, `[[`, "quo"))
  all(vapply(Filter(Negate(is.null), quos), function(q) {
    !is_rx_no(rx_compile_memo(rlang::quo_get_expr(q)))
  }, NA))
}
rl_prep_tell <- function(step, state) {
  if (!planned(step, "rx_tell", rl_tell_ok(step))) return(FALSE)
  to_nb <- identical(step$to, "neighbours")
  efield <- "edges"
  if (to_nb) {
    topo <- rl_topo_spec(step$via, state)
    if (is.null(topo)) return(NULL)
    efield <- topo$efield
  }
  quos <- c(if (!is.null(step$when)) list(step$when),
            if (!to_nb) list(step$to_quo),
            if (!is.null(step$order)) list(step$order),
            lapply(step$rules, `[[`, "quo"))
  rel_names <- relation_derived_names(state$relations)
  if (length(rel_names)) {
    ttg <- vapply(step$rules, `[[`, "", "target")
    tsy <- unlist(lapply(quos, function(q) all.vars(rlang::quo_get_expr(q))), use.names = FALSE)
    if (any(c(ttg, tsy) %in% rel_names)) return(NULL)
  }
  fake <- planned(step, "rx_tell_fake", rl_fake_rules(lapply(quos, function(q) {
    list(target = ".tell", quo = q)
  })))
  mnames <- if (planned(step, "uses_match", reads_match(quos)) ||
                length(planned(step, "cvars", cell_vars(quos))) > 0L) {
    rx_match_names(fake) %||% return(NULL)
  } else character()
  pcols <- sub("^partner_", "", mnames[startsWith(mnames, "partner_")])
  if (!all(pcols %in% model_columns(state$groups))) return(NULL)
  ccols <- sub("^cell_", "", mnames[startsWith(mnames, "cell_")])
  if (length(ccols) && (is.null(state$lattice$on) ||
      !all(ccols %in% names(state$groups[[state$lattice$on]])))) return(NULL)
  p <- rx_program(fake, seq_along(quos), model_columns(state$groups), state$globals,
                  match_names = mnames)
  if (is_rx_no(p)) return(NULL)
  rl_scope_bind(list(kind = 11L, scope_match = FALSE,
                     groups = list(list(name = "", prog = p$prog$.ptr, cols = p$cols,
                                        globs = p$globs, targets = vapply(step$rules, `[[`, "", "target"),
                                        scope = p$scope, ew = FALSE, mcols = p$mcols)),
                     tell = list(when = !is.null(step$when), resolve = step$resolve,
                                 nb = to_nb, order = !is.null(step$order), efield = efield)),
                if (length(p$scope)) list(p$env))
}

# FR8: `abm_sequential()` natively ([run_sequential()]): every rule compiled,
# each group's rules (as [applicable_rules()] routes them) one program evaluated
# agent by agent; `.order` compiled over the groups stacked. No relation, no
# `.cell` and no new column (R adds it and routes the rules again).
rl_seq_ok <- function(step) {
  (is.null(step$when) || !is_rx_no(rx_compile_memo(rlang::quo_get_expr(step$when)))) &&
    !any(vapply(step$rules, function(r) r$target %in% c(".cell", "partner_.cell"), NA)) &&
    !any(vapply(rx_rules(step), is_rx_no, NA)) &&
    (is.null(step$order) || !is_rx_no(rx_compile_memo(rlang::quo_get_expr(step$order))))
}
rl_prep_seq <- function(step, state) {
  if (!planned(step, "rx_seq", rl_seq_ok(step))) return(FALSE)
  groups <- state$groups
  cols_all <- model_columns(groups)
  gl <- names(state$globals)
  targets <- vapply(step$rules, `[[`, "", "target")
  syms <- unique(unlist(lapply(rx_rules(step), `[[`, "syms"))) %||% character()
  # `n` is the agent's own `n()` in the R engine's mask; a `partner_` column of
  # the model's, or a target that is both a global and a column, reads otherwise
  if ("n" %in% c(cols_all, syms) || any(startsWith(cols_all, "partner_")) ||
      any(targets %in% gl & targets %in% cols_all)) return(NULL)
  rel_spec <- NULL
  rel_mnames <- character()
  rel_tg_names <- character()
  if (length(state$relations)) {
    rel_names <- relation_derived_names(state$relations)
    rel_tg <- intersect(targets, rel_names)
    rel_rd <- intersect(syms, rel_names)
    rel_all <- union(rel_rd, rel_tg)
    if (length(rel_all)) {
      hits <- lapply(rel_all, resolve_relation_name, relations = state$relations)
      for (k in seq_along(rel_all)) {
        h <- hits[[k]]
        if (rel_all[[k]] %in% rel_tg) {
          if (is.null(h$col) || !h$col %in% names(state$relations[[h$rel]]$edges)) {
            return(NULL)
          }
        }
      }
      rel_mnames <- rel_rd
      rel_tg_names <- rel_tg
      rel_spec <- list(
        names = rel_all,
        rel = vapply(hits, `[[`, "", "rel"),
        col = vapply(hits, function(h) h$col %||% "", ""),
        back = vapply(hits, function(h) isTRUE(h$back), NA)
      )
    }
  }
  quos <- c(lapply(step$rules, `[[`, "quo"), if (!is.null(step$when)) list(step$when))
  mnames <- if (planned(step, "uses_match", reads_match(quos)) ||
                length(planned(step, "cvars", cell_vars(quos))) > 0L) {
    planned(step, "rx_mnames", rx_match_names(step)) %||% return(NULL)
  } else character()
  ccols <- sub("^cell_", "", mnames[startsWith(mnames, "cell_")])
  if (length(ccols) && (is.null(state$lattice$on) ||
      !all(ccols %in% names(state$groups[[state$lattice$on]])))) return(NULL)
  mnames <- union(mnames, rel_mnames)
  out <- list()
  envs <- list()
  for (nm in names(groups)) {
    gnames <- names(groups[[nm]])
    idx <- applicable_rules(step, gnames, groups, state$globals)
    if (!length(idx)) next
    tg <- targets[idx]
    if (!all(startsWith(tg, "partner_") | tg %in% gnames | tg %in% gl | tg %in% rel_tg_names)) {
      return(NULL)
    }
    p <- rx_program(step, idx, gnames, state$globals, match_names = mnames)
    if (is_rx_no(p)) return(NULL)
    if (length(p$scope)) envs <- c(envs, list(p$env))
    wslot <- match(tg, p$names) - 1L
    wslot[is.na(wslot)] <- -1L
    out[[length(out) + 1L]] <- list(name = nm, prog = p$prog$.ptr, cols = p$cols,
                                    globs = p$globs, targets = tg, scope = p$scope,
                                    ew = FALSE, mcols = p$mcols, wslot = as.integer(wslot))
  }
  if (!is.null(step$when)) {
    w_routes <- condition_routes(step, state)
    w_fake <- planned(step, "rx_seq_when",
                      rl_fake_rules(list(list(target = ".when", quo = step$when))))
    for (nm in names(groups)[w_routes]) {
      p <- rx_program(w_fake, 1L, names(groups[[nm]]), state$globals, match_names = mnames)
      if (is_rx_no(p)) return(NULL)
      if (length(p$scope)) envs <- c(envs, list(p$env))
      out[[length(out) + 1L]] <- list(name = nm, part = "when", prog = p$prog$.ptr,
                                      cols = p$cols, globs = p$globs, targets = character(),
                                      scope = p$scope, ew = FALSE, mcols = p$mcols,
                                      wslot = integer())
    }
  }
  if (!is.null(step$order)) {
    if (planned(step, "order_match", reads_match(list(step$order))) ||
        mentions_relation(step$order, state$relations) ||
        length(cell_vars(list(step$order))) > 0L) return(NULL)
    fake <- planned(step, "rx_seq_order",
                    rl_fake_rules(list(list(target = ".order", quo = step$order))))
    p <- rx_program(fake, 1L, cols_all, state$globals)
    if (is_rx_no(p)) return(NULL)
    if (length(p$scope)) envs <- c(envs, list(p$env))
    out[[length(out) + 1L]] <- list(name = "", part = "order", prog = p$prog$.ptr,
                                    cols = p$cols, globs = p$globs, targets = character(),
                                    scope = p$scope, ew = FALSE, mcols = character(),
                                    wslot = integer())
  }
  rl_scope_bind(list(kind = 12L, scope_match = FALSE, groups = out,
                     seq = list(order = !is.null(step$order),
                                scope_pop = identical(step$scope %||% "match", "population"),
                                when = !is.null(step$when),
                                rel = rel_spec)), envs)
}

# FR15b: `abm_pairs(via = <rel>, ..., .when =)` natively ([run_pairs()]):
# `.when` (optional) and rules compiled over the relation's edge columns plus
# `from_<col>` / `to_<col>`, globals and scope; existing target columns only
# (a new column goes to R once so it updates `rel$cols` and `rel$defaults`)
rl_pairs_ok <- function(step) {
  (is.null(step$when) || !is_rx_no(rx_compile_memo(rlang::quo_get_expr(step$when)))) &&
    !any(vapply(rx_rules(step), is_rx_no, NA))
}
rl_prep_pairs <- function(step, state) {
  if (!rl_pairs_ok(step)) return(FALSE)
  rel <- state$relations[[step$via]]
  if (is.null(rel)) return(NULL)
  ecols <- names(rel$edges)
  tg <- vapply(step$rules, `[[`, "", "target")
  if (!all(tg %in% ecols)) return(NULL)
  quos <- c(if (!is.null(step$when)) list(step$when), lapply(step$rules, `[[`, "quo"))
  envs <- lapply(quos, rlang::quo_get_env)
  if (!all(vapply(envs, identical, NA, envs[[1]]))) return(NULL)
  env <- envs[[1]]
  comp <- lapply(quos, function(q) rx_compile_memo(rlang::quo_get_expr(q)))
  if (any(vapply(comp, is_rx_no, NA))) return(NULL)
  heads <- unique(unlist(lapply(comp, `[[`, "heads"))) %||% character()
  if (!rx_heads_ok(heads, abm_eval_env(quos[[1]], state$globals, n = 1L))) return(NULL)
  if (!rx_rng_heads_ok(heads)) return(NULL)
  acols <- model_columns(state$groups)
  syms <- unique(unlist(lapply(comp, `[[`, "syms"))) %||% character()
  if (rx_special_sym(syms)) return(NULL)
  src <- integer(length(syms))
  vkind <- integer(length(syms))
  vcol <- character(length(syms))
  for (j in seq_along(syms)) {
    s <- syms[[j]]
    if (startsWith(s, "from_") && sub("^from_", "", s) %in% acols) {
      src[[j]] <- 1L; vkind[[j]] <- 1L; vcol[[j]] <- sub("^from_", "", s)
    } else if (startsWith(s, "to_") && sub("^to_", "", s) %in% acols) {
      src[[j]] <- 1L; vkind[[j]] <- 2L; vcol[[j]] <- sub("^to_", "", s)
    } else if (s %in% ecols) {
      src[[j]] <- 1L; vkind[[j]] <- 0L; vcol[[j]] <- s
    } else if (s %in% names(state$globals) || s == ".tick") {
      src[[j]] <- 2L
    } else if (exists(s, envir = env)) {
      src[[j]] <- 3L
    } else {
      return(NULL)
    }
  }
  o <- order(src)
  syms <- syms[o]; src <- src[o]; vkind <- vkind[o]; vcol <- vcol[o]
  exprs <- lapply(quos, rlang::quo_get_expr)
  prog <- rx_new_program(exprs, syms, rep(-1L, length(exprs)),
                         lapply(comp, function(cp) rx_bind(cp$ir, syms)))
  rl_scope_bind(list(kind = 19L, scope_match = FALSE,
       groups = list(list(name = "", prog = prog$.ptr, cols = vcol[src == 1L],
                          globs = syms[src == 2L], targets = tg,
                          scope = syms[src == 3L], ew = FALSE, mcols = character(),
                          wslot = as.integer(vkind[src == 1L]))),
       pairs = list(via = step$via, when = !is.null(step$when))),
    if (any(src == 3L)) list(env))
}

# the network neighbour shapes the loop folds natively: every rule `sum()` or
# `mean()` of a compiled elementwise argument ([neighbour_fast()]'s shapes), no
# lattice direction, no `within`
# FR13: network `any()`/`all()` ([network_view()] then [neighbour_aggregate()]):
# compiled per schema only (its own loop kind, 15, so the slot is not keyed by
# the edges); the native step reads the neighbour table from `state$nb_cache`
# when it is the current edges' (else R builds it, as it would)
rl_prep_nb_anyall <- function(step, state) {
  shapes <- planned(step, "rl_nbshapes", rl_nb_shapes(step))
  topo <- rl_topo_spec(step$via, state)
  if (is.null(topo)) return(NULL)
  rel_names <- relation_derived_names(state$relations)
  if (length(rel_names)) {
    ntg <- vapply(step$rules, `[[`, "", "target")
    nsy <- unlist(lapply(shapes, function(s) all.vars(s$arg)), use.names = FALSE)
    if (any(c(ntg, nsy) %in% rel_names)) return(NULL)
  }
  pg <- rl_nb_program(step, state)
  if (is.null(pg)) return(NULL)
  rl_scope_bind(list(kind = 3L, scope_match = FALSE, nb_default = step$default,
       groups = list(pg$prog_g),
       nb = list(mean = rep(FALSE, length(shapes)),
                 anyall = match(vapply(shapes, `[[`, "", "fn"), c("any", "all"), 0L),
                 narm = vapply(shapes, function(s) isTRUE(s$narm), NA),
                 targets = vapply(step$rules, `[[`, "", "target"), cols = list(),
                 cnt = integer(), cache = NULL, cache_name = topo$cname,
                 efield = topo$efield, edges = NULL, n = 0L)),
    if (length(pg$prog_g$scope)) list(pg$envs[[1]]))
}

rl_nb_ok <- function(step) {
  is.null(step$where) && is.null(step$within) && rl_nb_default_ok(step$default) &&
    !is.null(planned(step, "rl_nbshapes", rl_nb_shapes(step)))
}

# Each rule's shape for the network fold: every rule `sum()`/`mean()`
# ([neighbour_fast()]'s shapes), or every rule `any()`/`all()` of an
# elementwise argument over the candidate (FR13: [network_view()] then
# [neighbour_aggregate()], where the order of the rows does not matter);
# `NULL` for anything else or a mix (R then folds them all through `rowsum()`)
rl_nb_shapes <- function(step) {
  fast <- lapply(step$rules, function(r) nb_fast_shape(r$quo))
  ok <- function(sh) all(vapply(sh, function(s) !is.null(s) && !is_rx_no(rx_compile_memo(s$arg)), NA))
  if (ok(fast)) return(fast)
  aa <- lapply(step$rules, function(r) {
    e <- rlang::quo_get_expr(r$quo)
    if (!rlang::is_call(e) || !rlang::is_symbol(e[[1]]) ||
        !rlang::as_string(e[[1]]) %in% c("any", "all")) return(NULL)
    args <- as.list(e)[-1]
    nms <- names(args) %||% rep("", length(args))
    narm <- FALSE
    if (length(args) == 2L && nms[[2]] == "na.rm" && rlang::is_bool(args[[2]])) {
      narm <- args[[2]]
      args <- args[1]
      nms <- nms[1]
    }
    if (length(args) != 1L || nzchar(nms[[1]]) || !is_elementwise(args[[1]])) return(NULL)
    vars <- all.vars(args[[1]])
    if (any(startsWith(vars, "own_")) || ".of" %in% vars) return(NULL)
    list(fn = rlang::as_string(e[[1]]), arg = args[[1]], narm = narm)
  })
  if (ok(aa)) return(aa)
  NULL
}

# FR14: network `abm_neighbours()` over the `(focal, neighbour)` view with
# `own_<col>`, edge-draw columns (`<name>`, `<name>_back`), `na.rm` and mixes
# of `sum`/`mean`/`any`/`all`/`n()` ([network_view()] + [neighbour_aggregate()])
rl_nb_agg_shapes <- function(step) {
  shapes <- lapply(step$rules, function(r) {
    e <- rlang::quo_get_expr(r$quo)
    if (is_bare_count(r$quo)) return(list(fn = "n", narm = FALSE))
    if (!rlang::is_call(e) || !rlang::is_symbol(e[[1]])) return(NULL)
    fn <- rlang::as_string(e[[1]])
    if (!fn %in% c("sum", "mean", "any", "all")) return(NULL)
    args <- as.list(e)[-1]
    nms <- names(args) %||% rep("", length(args))
    narm <- FALSE
    if (length(args) == 2L && nms[[2]] == "na.rm" && rlang::is_bool(args[[2]])) {
      narm <- args[[2]]
      args <- args[1]
      nms <- nms[1]
    }
    if (length(args) != 1L || nzchar(nms[[1]]) || !is_elementwise(args[[1]])) return(NULL)
    cp <- rx_compile_memo(args[[1]])
    if (is_rx_no(cp) || any(c(rx_draw_heads, "n") %in% cp$heads)) return(NULL)
    list(fn = fn, narm = narm, arg = args[[1]], cp = cp)
  })
  if (!length(shapes) || any(vapply(shapes, is.null, NA))) return(NULL)
  envs <- lapply(step$rules, function(r) rlang::quo_get_env(r$quo))
  if (!all(vapply(envs, identical, NA, envs[[1]]))) return(NULL)
  shapes
}
rl_nb_view_shape <- function(step) {
  if (!is.null(step$where) || !is.null(step$within) || !rl_nb_default_ok(step$default)) return(NULL)
  rl_nb_agg_shapes(step)
}
rl_nb_view_ok <- function(step) !is.null(rl_nb_view_shape(step))
rl_nb_rel_ok <- function(step) {
  is.null(step$where) && !is.null(step$within) && rl_nb_default_ok(step$default) &&
    rlang::is_symbol(rlang::quo_get_expr(step$within)) && !is.null(rl_nb_agg_shapes(step))
}

rl_prep_nb_rel <- function(step, state) {
  if (!rl_nb_default_ok(step$default) || is.null(step$within)) return(FALSE)
  we <- rlang::quo_get_expr(step$within)
  if (!rlang::is_symbol(we) || !length(state$relations)) return(NULL)
  hit <- resolve_relation_name(rlang::as_string(we), state$relations)
  if (is.null(hit) || !is.null(hit$col)) return(NULL)
  shapes <- planned(step, "rx_nbrel", rl_nb_agg_shapes(step))
  if (is.null(shapes)) return(FALSE)
  rel <- state$relations[[hit$rel]]
  agent_cols <- model_columns(state$groups)
  args <- Filter(function(s) s$fn != "n", shapes)
  comp <- lapply(args, `[[`, "cp")
  syms <- unique(unlist(lapply(comp, `[[`, "syms"))) %||% character()
  if (rx_special_sym(syms)) return(NULL)
  targets <- vapply(step$rules, `[[`, "", "target")
  rel_names <- relation_derived_names(state$relations)
  if (any(targets %in% rel_names)) return(NULL)
  env <- rlang::quo_get_env(step$rules[[1]]$quo)
  heads <- unique(unlist(lapply(comp, `[[`, "heads"))) %||% character()
  if (!rx_heads_ok(heads, abm_eval_env(step$rules[[1]]$quo, state$globals, n = 1L))) return(NULL)
  src <- integer(length(syms))
  vkind <- integer(length(syms))
  vcol <- character(length(syms))
  vback <- character(length(syms))
  for (j in seq_along(syms)) {
    nm <- syms[[j]]
    rh <- if (nm %in% rel_names) resolve_relation_name(nm, state$relations) else NULL
    if (!is.null(rh)) {
      if (!identical(rh$rel, hit$rel) || !identical(isTRUE(rh$back), isTRUE(hit$back))) {
        return(NULL)
      }
      src[[j]] <- 1L
      if (is.null(rh$col)) {
        vkind[[j]] <- 6L; vcol[[j]] <- ""
      } else {
        vkind[[j]] <- 5L; vcol[[j]] <- rh$col
      }
    } else if (nm %in% agent_cols) {
      src[[j]] <- 1L; vkind[[j]] <- 0L; vcol[[j]] <- nm
    } else if (startsWith(nm, "own_") && sub("^own_", "", nm) %in% agent_cols) {
      src[[j]] <- 1L; vkind[[j]] <- 1L; vcol[[j]] <- sub("^own_", "", nm)
    } else if (nm == ".of") {
      src[[j]] <- 1L; vkind[[j]] <- 4L
    } else if (nm %in% names(state$globals) || nm == ".tick") {
      src[[j]] <- 2L
    } else if (exists(nm, envir = env)) {
      src[[j]] <- 3L
    } else {
      return(NULL)
    }
  }
  o <- order(src)
  syms <- syms[o]; src <- src[o]; vkind <- vkind[o]; vcol <- vcol[o]; vback <- vback[o]
  prog_ptr <- if (length(args)) {
    exprs <- lapply(args, `[[`, "arg")
    rx_new_program(exprs, syms, rep(-1L, length(exprs)),
                   lapply(comp, function(cp) rx_bind(cp$ir, syms)))$.ptr
  } else NULL
  rl_scope_bind(list(kind = 17L, scope_match = FALSE, nb_default = step$default,
       groups = list(list(name = "", prog = prog_ptr, cols = vcol[src == 1L],
                          globs = syms[src == 2L], targets = targets,
                          scope = syms[src == 3L], ew = FALSE,
                          all_targets = character())),
       nbv = list(fns = match(vapply(shapes, `[[`, "", "fn"),
                              c("n", "sum", "mean", "any", "all")) - 1L,
                  narm = vapply(shapes, function(s) isTRUE(s$narm), NA),
                  vkind = vkind[src == 1L],
                  vback = vback[src == 1L],
                  efield = "edges", cache_name = "nb_cache",
                  rel_name = hit$rel, rel_back = isTRUE(hit$back))),
    if (any(src == 3L)) list(env))
}

rl_prep_nb_view <- function(step, state) {
  shapes <- planned(step, "rx_nbv", rl_nb_view_shape(step))
  if (is.null(shapes)) return(FALSE)
  topo <- rl_topo_spec(step$via, state)
  if (is.null(topo)) return(NULL)
  edges <- state[[topo$efield]]
  if (is.null(edges) || (topo$cname == "nb_cache" && !is.null(state$stale_draws))) return(NULL)
  agent_cols <- model_columns(state$groups)
  drawn <- drawn_names(edges)
  args <- Filter(function(s) s$fn != "n", shapes)
  comp <- lapply(args, `[[`, "cp")
  syms <- unique(unlist(lapply(comp, `[[`, "syms"))) %||% character()
  if (rx_special_sym(syms)) return(NULL)
  targets <- vapply(step$rules, `[[`, "", "target")
  rel_names <- relation_derived_names(state$relations)
  if (length(rel_names) && any(c(syms, targets) %in% rel_names)) return(NULL)
  env <- rlang::quo_get_env(step$rules[[1]]$quo)
  heads <- unique(unlist(lapply(comp, `[[`, "heads"))) %||% character()
  if (!rx_heads_ok(heads, abm_eval_env(step$rules[[1]]$quo, state$globals, n = 1L))) return(NULL)
  src <- integer(length(syms))
  vkind <- integer(length(syms))
  vcol <- character(length(syms))
  vback <- character(length(syms))
  for (j in seq_along(syms)) {
    nm <- syms[[j]]
    if (nm %in% drawn) {
      src[[j]] <- 1L; vkind[[j]] <- 2L
      vcol[[j]] <- paste0(".draw_", nm); vback[[j]] <- paste0(".back_", nm)
    } else if (endsWith(nm, "_back") && sub("_back$", "", nm) %in% drawn) {
      base <- sub("_back$", "", nm)
      src[[j]] <- 1L; vkind[[j]] <- 3L
      vcol[[j]] <- paste0(".draw_", base); vback[[j]] <- paste0(".back_", base)
    } else if (nm %in% agent_cols) {
      src[[j]] <- 1L; vkind[[j]] <- 0L; vcol[[j]] <- nm
    } else if (startsWith(nm, "own_") && sub("^own_", "", nm) %in% agent_cols) {
      src[[j]] <- 1L; vkind[[j]] <- 1L; vcol[[j]] <- sub("^own_", "", nm)
    } else if (nm == ".of") {
      src[[j]] <- 1L; vkind[[j]] <- 4L
    } else if (nm %in% names(state$globals) || nm == ".tick") {
      src[[j]] <- 2L
    } else if (exists(nm, envir = env)) {
      src[[j]] <- 3L
    } else {
      return(NULL)
    }
  }
  o <- order(src)
  syms <- syms[o]; src <- src[o]; vkind <- vkind[o]; vcol <- vcol[o]; vback <- vback[o]
  prog_ptr <- if (length(args)) {
    exprs <- lapply(args, `[[`, "arg")
    rx_new_program(exprs, syms, rep(-1L, length(exprs)),
                   lapply(comp, function(cp) rx_bind(cp$ir, syms)))$.ptr
  } else NULL
  rl_scope_bind(list(kind = 17L, scope_match = FALSE, nb_default = step$default,
       groups = list(list(name = "", prog = prog_ptr, cols = vcol[src == 1L],
                          globs = syms[src == 2L],
                          targets = vapply(step$rules, `[[`, "", "target"),
                          scope = syms[src == 3L], ew = FALSE,
                          all_targets = syms[src > 1L])),
       nbv = list(fns = match(vapply(shapes, `[[`, "", "fn"),
                              c("n", "sum", "mean", "any", "all")) - 1L,
                  narm = vapply(shapes, function(s) isTRUE(s$narm), NA),
                  vkind = vkind[src == 1L],
                  vback = vback[src == 1L],
                  efield = topo$efield,
                  cache_name = topo$cname)),
    if (any(src == 3L)) list(env))
}

# [run_neighbours()]'s set-up and [neighbour_fast()]'s index for these edges and
# this population, as R would leave them in `state$nb_cache`
rl_prep_neighbours <- function(step, state, slot = NULL) {
  if (!rl_nb_default_ok(step$default)) return(FALSE)
  if (!is.null(step$within)) {
    if (length(state$relations) && rlang::is_symbol(rlang::quo_get_expr(step$within))) {
      h <- resolve_relation_name(rlang::as_string(rlang::quo_get_expr(step$within)), state$relations)
      if (!is.null(h) && is.null(h$col)) {
        return(rl_prep_memo(step, state, slot, rl_prep_nb_rel))
      }
    }
    return(rl_prep_memo(step, state, slot, if (rl_nb_within_ok(step)) rl_prep_nb_within
                                           else rl_prep_nb_cross))
  }
  if (!is.null(step$where)) return(rl_prep_nb_where(step, state))
  topo <- rl_topo_spec(step$via, state)
  if (is.null(topo)) return(NULL)
  edges <- state[[topo$efield]]
  if (!planned(step, "rx_nbshape", rl_nb_ok(step))) {
    return(rl_prep_memo(step, state, slot, rl_prep_nb_view))
  }
  shapes <- planned(step, "rl_nbshapes", rl_nb_shapes(step))
  if (shapes[[1]]$fn %in% c("any", "all")) {
    if (length(drawn_names(edges))) return(rl_prep_memo(step, state, slot, rl_prep_nb_view))
    return(rl_prep_memo(step, state, slot, rl_prep_nb_anyall))
  }
  if (topo$cname != "nb_cache") {
    return(rl_prep_memo(step, state, slot, rl_prep_nb_view))
  }
  if (is.null(edges) || length(state$relations) || !is.null(state$stale_draws)) return(NULL)
  drawn <- drawn_names(edges)
  if (length(drawn)) {
    bad <- c(drawn, paste0(drawn, "_back"))
    if (any(vapply(shapes, function(s) any(s$vars %in% bad), NA))) {
      return(rl_prep_memo(step, state, slot, rl_prep_nb_view))
    }
  }
  shapes <- planned(step, "rl_nbshapes", rl_nb_shapes(step))
  # the arguments' program depends on the step and the names only: kept per
  # step position for the session, like a rules descriptor (the neighbour index
  # below is per run)
  pg <- rl_prep_memo(step, state, if (!is.null(slot)) paste0("nb", slot), rl_nb_program)
  if (is.null(pg)) return(NULL)
  prog_g <- pg$prog_g
  envs <- pg$envs
  n <- sum(vapply(state$groups, vctrs::vec_size, 0L))
  if (n == 0L) return(NULL)
  # the whole descriptor, while the edges (the same object: a sweep's runs share
  # the model's) and the agent count stand: the neighbour index R would build
  # anew in each run comes out the same
  dkey <- if (!is.null(slot)) paste0("nbd", slot)
  memo <- if (!is.null(dkey)) .rl_desc[[dkey]]
  for (hit in memo) {
    # the same objects (the native step checks the edges by address)
    if (identical(hit$pg, pg) && hit$n == n && identical(hit$def, step$default) &&
        rlang::is_reference(hit$edges, edges) &&
        (is.null(hit$cache0) && is.null(state$nb_cache) ||
           rlang::is_reference(hit$cache0, state$nb_cache))) {
      return(hit$desc)
    }
  }
  cache0 <- state$nb_cache
  cache <- state$nb_cache
  if (!identical(cache$edges, edges)) {
    cache <- list(edges = edges, nb = neighbour_table(edges))
  }
  ix <- cache$ix
  if (is.null(ix) || ix$n != n) {
    ix <- nb_index(cache$nb, n)
    cache$ix <- ix
  }
  if (!ix$ok) return(NULL)
  desc <- rl_scope_bind(list(kind = 3L, scope_match = FALSE, nb_default = step$default,
       groups = list(prog_g),
       nb = list(mean = vapply(shapes, function(s) s$fn == "mean", NA),
                 targets = vapply(step$rules, `[[`, "", "target"), cols = ix$cols,
                 cnt = ix$cnt, cache = cache, cache_name = "nb_cache",
                 efield = "edges", edges = edges, n = n)),
    if (length(prog_g$scope)) list(envs[[1]]))
  if (!is.null(dkey)) {
    assign(dkey, c(utils::tail(memo, 3L),
                   list(list(pg = pg, n = n, def = step$default, edges = edges,
                             cache0 = cache0, desc = desc))),
           envir = .rl_desc)
  }
  desc
}

#' The native step as a kernel of [run_once_rust()] ("both" mode compares it with
#' the R engine step by step)
#' @noRd
rk_native <- function(step, state, store) {
  d <- rl_prep(step, state)
  if (isFALSE(d)) return(rx_never(step))
  if (is.null(d)) return(NULL)
  rs_native_exec(d, state) %||% rx_declined(step)
}
