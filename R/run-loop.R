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
                          bar = NULL, kernels = active_kernels("rust")) {
  if (!is.null(seed)) local_seed(seed)
  rm(list = ls(.bound), envir = .bound)
  rm(list = ls(.eval_env), envir = .eval_env)
  rm(list = ls(.std_memo, all.names = TRUE), envir = .std_memo)
  .bound$on <- n_agents(model) >= 5000L

  state <- list(
    groups    = model$groups,
    globals   = model$globals,
    edges     = model$edges,
    lattice   = model$lattice,
    relations = model$relations,
    match     = NULL,
    next_id   = n_agents(model) + 1L
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
    }
    ctx$st <- st
    st
  }
  prep <- function(i) rl_prep(steps[[i]], ctx$st, i)
  # a block's `until` after a pass ([repeat_until()]), on the state as it is
  until <- function(k) {
    at <<- k
    frame <<- ctx
    out <- repeat_until(steps[[k]], ctx$st)
    at <<- 0L
    out
  }
  # measures (1) and checks (2) compiled as value-only global programs (FR9b)
  tick_steps <- list(rl_tick_step(measures), rl_tick_step(checks))
  mprep <- function(k) rl_prep_tick(tick_steps[[k]], ctx$st)
  rng <- function() {
    saved <<- rng_get()
    NULL
  }

  lp <- RunLoop$new(kinds, prep, seg, tick, rng, ctx, mprep, until)
  # the ticks whose book-keeping needs R; the loop logs the others' globals into
  # `glog` itself (FR9) and they are turned into rows below
  # (measures and checks too, when they compile; a tick whose native ones
  # decline or whose check fails is closed by the R callback instead)
  need <- rep(!is.null(bar), ticks + 1L)
  glog <- vector("list", ticks + 1L)
  mlog <- vector("list", ticks + 1L)
  has <- c(length(measures) > 0L, length(checks) > 0L)
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
  glog <- logs[[1]]
  mlog <- logs[[2]]
  slog <- logs[[3]]
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
    population = rl_stack(slog, snapshots, empty),
    globals    = globals_tbl %||% global_table(global_log),
    measures   = if (is.null(measure_log)) NULL else dplyr::bind_rows(measure_log),
    network    = strip_draws(state$edges),
    relations  = relation_tables(state$relations)
  )
}

# The steps with every `abm_repeat()` block flattened: the block's steps between
# a begin marker (kind -1) and an end marker (kind -(2 + 2 * max + has_until)),
# both the repeat step itself; `at` the go block position of a top-level step
# (`NA` inside a block), `kinds` 0 for an ordinary step. A block whose `max` the
# encoding cannot hold stays one R step.
rl_flatten <- function(steps, top = TRUE) {
  if (top && !any(vapply(steps, inherits, NA, "abm_repeat"))) {
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
  if (any(c("runif", "rnorm", "sample") %in% heads)) return(FALSE)
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
  as.integer(k$native)
}

#' Compile a native step for `state`'s schema: a descriptor for
#' `rs_native_exec()` / `RunLoop`, `NULL` (the R path for this schema) or `FALSE`
#' (never native)
#' @noRd
rl_prep <- function(step, state, slot = NULL) {
  switch(class(step)[[1]],
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

# A rules/global descriptor depends on the step and the state's names only (and
# on what decides which functions its names find), so the loop keeps the last one
# per step position (`slot`) for the session: the runs of a sweep over one go
# block compile each step once, and a go block built afresh per run replaces the
# entry rather than piling them up
rl_prep_memo <- function(step, state, slot, f) {
  if (is.null(slot)) return(f(step, state))
  key <- paste0("d", slot)
  sk <- list(schema_key(state), Filter(is.function, state$globals), isTRUE(.bound$on),
             RNGkind(), step$by, step$scope)
  # a few schemas per step (a run's first tick usually adds columns)
  memo <- .rl_desc[[key]]
  # rules and global steps by their rules (with `.by`/`.scope` in `sk`); any
  # other step by itself without its per-run plan (a match has no rules to tell
  # two steps apart by)
  me <- if (inherits(step, c("abm_rules", "abm_global"))) step$rules
        else unclass(step)[names(step) != "plan"]
  for (hit in memo) {
    if (identical(hit$step, me) && identical(hit$sk, sk)) return(hit$desc)
  }
  desc <- f(step, state)
  assign(key, c(utils::tail(memo, 3L), list(list(step = me, sk = sk, desc = desc))),
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
  routes <- if (length(state$groups) > 1L) rule_routes(step, state)
  groups <- list()
  envs <- list()
  compiled <- !vapply(rx_rules(step), is_rx_no, NA)
  mixed <- FALSE
  mnames <- if (step$uses_match %||% planned(step, "uses_match",
                                             reads_match(lapply(step$rules, `[[`, "quo")))) {
    planned(step, "rx_mnames", rx_match_names(step))
  } else character()
  # `partner_<col>` of a column the model does not have: R's error
  pcols <- sub("^partner_", "", mnames[startsWith(mnames, "partner_")])
  if (!all(pcols %in% model_columns(state$groups))) return(NULL)
  for (nm in names(state$groups)) {
    todo <- if (length(state$groups) == 1L) step$rules else routes[[nm]]
    if (!length(todo)) next
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
  }
  rl_scope_bind(c(list(kind = 1L, scope_match = (step$scope %||% "match") == "match",
                       groups = groups), if (mixed) rl_rule_fns(step)), envs)
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
  if (!by %in% cols || length(state$relations)) return(NULL)
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

# one program over every group stacked
rl_prep_global <- function(step, state, mode = TRUE) {
  if (!planned(step, "rx_gshape", rx_global_ok(step))) return(FALSE)
  groups <- state$groups
  cols <- Reduce(intersect, lapply(groups, names))
  p <- rx_program(step, seq_along(step$rules), cols, state$globals, global_step = mode)
  if (is_rx_no(p)) return(NULL)
  # a column some group lacks is padded with NA by the R engine: not native
  if (any(p$names %in% setdiff(model_columns(groups), cols))) return(NULL)
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
  if (!is.null(step$where) || is.null(step$within)) return(NULL)
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
  if (!is.null(step$where) || is.null(step$within)) return(NULL)
  we <- rlang::quo_get_expr(step$within)
  if (!is.null(equijoin_key(we))) return(NULL)
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
  if (any(c("runif", "rnorm", "sample") %in% heads) || "n" %in% comp[[1]]$heads ||
      ".of" %in% syms) return(NULL)
  envs <- lapply(c(list(step$within), lapply(step$rules, `[[`, "quo")), rlang::quo_get_env)
  if (!all(vapply(envs, identical, NA, envs[[1]]))) return(NULL)
  list(shapes = shapes, exprs = exprs, comp = comp)
}
rl_nb_cross_ok <- function(step) !is.null(rl_nb_cross_shape(step))

rl_prep_nb_cross <- function(step, state) {
  sh <- planned(step, "rx_nbx", rl_nb_cross_shape(step))
  if (is.null(sh)) return(FALSE)
  if (length(state$relations)) return(NULL)
  cols <- model_columns(state$groups)
  names <- unique(unlist(lapply(sh$comp, `[[`, "syms")))
  own <- startsWith(names, "own_") & sub("^own_", "", names) %in% cols
  src <- ifelse(names %in% cols | own, 1L, ifelse(names %in% names(state$globals), 2L, 3L))
  env <- rlang::quo_get_env(step$within)
  for (nm in names[src == 3L]) if (!exists(nm, envir = env)) return(NULL)
  if (!rx_heads_ok(unique(unlist(lapply(sh$comp, `[[`, "heads"))),
                   abm_eval_env(step$within, state$globals))) return(NULL)
  o <- order(src)
  names <- names[o]
  src <- src[o]
  prog <- rx_new_program(sh$exprs, names, rep(-1L, length(sh$exprs)),
                         lapply(sh$comp, function(cp) rx_bind(cp$ir, names)))
  rl_scope_bind(list(kind = 14L, scope_match = FALSE,
       groups = list(list(name = "", prog = prog$.ptr, cols = names[src == 1L],
                          globs = names[src == 2L], targets = vapply(step$rules, `[[`, "", "target"),
                          scope = names[src == 3L], ew = FALSE)),
       cross = list(fns = vapply(sh$shapes, `[[`, "", "fn"),
                    narm = vapply(sh$shapes, function(s) isTRUE(s$narm), NA))),
    if (any(src == 3L)) list(env))
}

rl_prep_nb_within <- function(step, state) {
  sh <- planned(step, "rx_nb1", rl_nb1_shape(step))
  if (is.null(sh)) return(FALSE)
  if (length(state$relations)) return(NULL)
  cols <- model_columns(state$groups)
  key <- sh$key
  if (!key$focal %in% cols || !key$cand %in% cols || !all(sh$shape$vars %in% cols)) {
    return(NULL)
  }
  env <- abm_eval_env(step$within, state$globals, n = 1L)
  heads <- unique(unlist(lapply(sh$comp, `[[`, "heads")))
  if (!rx_heads_ok(heads, env) || !standard_fns_in(sh$shape$heads, env)) return(NULL)
  names <- unique(unlist(lapply(sh$comp, `[[`, "syms")))
  prog <- if (length(sh$exprs)) {
    rx_new_program(sh$exprs, names, rep(-1L, length(sh$exprs)),
                   lapply(sh$comp, function(cp) rx_bind(cp$ir, names)))
  } else NULL
  if (is.null(prog)) return(NULL)
  list(kind = 5L, scope_match = FALSE,
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
  if (is.null(step$where) || !is.null(step$within)) return(NULL)
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
  if (length(state$relations) || is.null(lat) || !tolower(step$where) %in% names(lat$dir_nb)) {
    return(NULL)
  }
  dir <- lat$dir_nb[[tolower(step$where)]]
  if (!is.integer(dir) || !is.null(attributes(dir))) return(NULL)
  cols <- model_columns(state$groups)
  args <- Filter(function(r) r$fn != "n", rules)
  comp <- lapply(args, `[[`, "cp")
  names <- unique(unlist(lapply(comp, `[[`, "syms")))
  own <- startsWith(names, "own_") & sub("^own_", "", names) %in% cols
  src <- ifelse(names %in% cols | own, 1L,
                ifelse(names %in% names(state$globals), 2L, 3L))
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
  rl_scope_bind(list(kind = 6L, scope_match = FALSE,
       groups = list(list(name = "", prog = prog$.ptr, cols = names[src == 1L],
                          globs = names[src == 2L], targets = vapply(step$rules, `[[`, "", "target"),
                          scope = names[src == 3L], ew = FALSE)),
       nbw = list(on = lat$on, dir = dir, lattice = lat,
                  fns = vapply(rules, `[[`, "", "fn"))),
    if (any(src == 3L)) list(env))
}

# [rl_prep_neighbours()]'s argument program: `list(prog_g, envs)` or `NULL`
rl_nb_program <- function(step, state) {
  shapes <- planned(step, "shapes", lapply(step$rules, function(r) nb_fast_shape(r$quo)))
  # the arguments compiled over the agents: names that are columns of every
  # group, else one value (a global or from the formulas' environment), as
  # [nb_fast_scalars()] requires; a column some group lacks (NA padding): R
  envs <- lapply(step$rules, function(r) rlang::quo_get_env(r$quo))
  if (!all(vapply(envs, identical, NA, envs[[1]]))) return(NULL)
  comp <- lapply(shapes, function(sh) rx_compile_memo(sh$arg))
  cols <- Reduce(intersect, lapply(state$groups, names))
  names <- unique(unlist(lapply(comp, `[[`, "syms")))
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
  if (!step$pair %in% c("random", "one_of", "opposite_group", "network") ||
      !is.null(step$weight) || !is.null(step$cost)) return(NULL)
  if (step$pair == "network" && (!identical(step$from, "neighbour") || !is.null(step$role))) {
    return(NULL)
  }
  if (!step$pair %in% "one_of" && !is.null(step$among)) return(NULL)
  if (!is.null(step$dot_by) && (step$pair != "opposite_group" || !is.null(step$role))) return(NULL)
  by <- if (step$pair == "opposite_group") {
    e <- rlang::quo_get_expr(step$by)
    if (!rlang::is_symbol(e)) return(NULL)
    rlang::as_string(e)
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
             if (!is.null(roles)) lapply(roles, rlang::new_quosure, env = rlang::quo_get_env(step$role)))
  if (!is.null(step$among) && mentions_pair(step$among)) return(NULL)
  comp <- lapply(conds, function(q) rx_compile_memo(rlang::quo_get_expr(q)))
  if (any(vapply(comp, is_rx_no, NA))) return(NULL)
  if (!is.null(roles) &&
      any(c("runif", "rnorm", "sample") %in% unlist(lapply(utils::tail(comp, 2L), `[[`, "heads")))) {
    return(NULL)
  }
  envs <- lapply(conds, rlang::quo_get_env)
  if (length(envs) > 1L && !all(vapply(envs, identical, NA, envs[[1]]))) return(NULL)
  list(by = by, dot_by = dot_by, conds = conds, comp = comp,
       roles = if (!is.null(roles)) names(roles))
}
rl_match_ok <- function(step) !is.null(rl_match_shape(step))

rl_prep_match <- function(step, state) {
  sh <- planned(step, "rx_match", rl_match_shape(step))
  if (is.null(sh)) return(FALSE)
  if (length(state$relations)) return(NULL)
  cols <- model_columns(state$groups)
  if (!is.null(sh$by) && !sh$by %in% cols) return(NULL)
  if (!is.null(sh$dot_by) && !sh$dot_by %in% cols) return(NULL)
  prog_g <- list(name = "", prog = NULL, cols = character(), globs = character(),
                 targets = character(), scope = character(), ew = FALSE)
  envs <- list()
  if (length(sh$conds)) {
    q1 <- sh$conds[[1]]
    env <- rlang::quo_get_env(q1)
    names <- unique(unlist(lapply(sh$comp, `[[`, "syms")))
    if (!rx_heads_ok(unique(unlist(lapply(sh$comp, `[[`, "heads"))),
                     abm_eval_env(q1, state$globals, n = 1L))) return(NULL)
    src <- ifelse(names %in% cols, 1L, ifelse(names %in% names(state$globals), 2L, 3L))
    for (nm in names[src == 3L]) if (!exists(nm, envir = env)) return(NULL)
    o <- order(src)
    names <- names[o]
    src <- src[o]
    exprs <- lapply(sh$conds, rlang::quo_get_expr)
    prog <- rx_new_program(exprs, names, rep(-1L, length(exprs)),
                           lapply(sh$comp, function(cp) rx_bind(cp$ir, names)))
    prog_g$prog <- prog$.ptr
    prog_g$cols <- names[src == 1L]
    prog_g$globs <- names[src == 2L]
    prog_g$scope <- names[src == 3L]
    if (any(src == 3L)) envs <- list(env)
  }
  rl_scope_bind(list(kind = 7L, scope_match = FALSE, groups = list(prog_g),
                     mt = list(pair = match(step$pair, c("random", "one_of", "opposite_group",
                                                         "network")),
                               roles = sh$roles %||% character(),
                               size = step$size, by = sh$by %||% character(),
                               dot_by = sh$dot_by %||% character(),
                               eligible = !is.null(step$eligible),
                               among = !is.null(step$among))), envs)
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
  if (length(state$relations)) return(NULL)
  fake <- planned(step, "rx_death_step", rl_death_step(step))
  mnames <- if (planned(step, "uses_match", reads_match(list(step$when)))) {
    rx_match_names(fake) %||% return(NULL)
  } else character()
  pcols <- sub("^partner_", "", mnames[startsWith(mnames, "partner_")])
  if (!all(pcols %in% model_columns(state$groups))) return(NULL)
  applies <- condition_routes(step, state)
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
                     death = list(prune = isTRUE(step$prune_edges))), envs)
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
  # `attach_via` from a parent or to random agents (FR12); `random_edge` grows
  # the edges while it draws from them: R
  if (identical(step$attach_via$from, "random_edge")) return(FALSE)
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
  if (length(state$relations)) return(NULL)
  parts <- planned(step, "rx_birth_parts", rl_birth_parts(step))
  quos <- c(list(step$when, step$times), lapply(c(step$cost, step$inherit), `[[`, "quo"))
  mnames <- if (planned(step, "uses_match", reads_match(quos))) {
    rx_match_names(rl_fake_rules(lapply(Filter(Negate(is.null), quos), function(q) {
      list(target = ".x", quo = q)
    }))) %||% return(NULL)
  } else character()
  pcols <- sub("^partner_", "", mnames[startsWith(mnames, "partner_")])
  if (!all(pcols %in% model_columns(state$groups))) return(NULL)
  applies <- if (is.null(step$when)) names(state$groups)[1L]
             else names(state$groups)[condition_routes(step, state)]
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
  rl_scope_bind(list(kind = 9L, scope_match = FALSE, groups = groups,
                     birth = list(round = round_prog$.ptr,
                                  attach = if (is.null(step$attach_via)) 0L
                                           else if (identical(step$attach_via$from, "parent")) 1L
                                           else 2L,
                                  links = as.integer(step$links %||% 1L),
                                  count = if (is.null(step$when)) as.integer(step$n) else -1L)),
                envs)
}

# FR7: `abm_move()` to a random (or random empty) adjacent cell on the lattice
# ([run_move()]'s `range = 1` shapes), then `sync_cell_coords()`
rl_move_ok <- function(step) {
  step$mode %in% c("random_neighbour", "random_empty_neighbour") &&
    identical(step$range, 1L) && !isTRUE(step$axes_only)
}
rl_prep_move <- function(step, state) {
  if (!planned(step, "rx_move", rl_move_ok(step))) return(FALSE)
  lat <- state$lattice
  if (is.null(lat) || !(identical(step$along, lat$on) || identical(step$along, "network"))) {
    return(NULL)
  }
  with_cell <- names(state$groups)[vapply(state$groups, function(g) ".cell" %in% names(g), NA)]
  who <- step$who %||% with_cell
  # an unknown group, or one not on the lattice, is R's error
  if (!length(who) || !all(who %in% with_cell)) return(NULL)
  if (!is.list(lat$nb) || !is.null(lat$x) && !is.atomic(lat$x)) return(NULL)
  list(kind = 10L, scope_match = FALSE, groups = list(),
       mv = list(who = who, avoid = isTRUE(step$avoid_occupied), lattice = lat,
                 nb = lat$nb, x = lat$x, y = lat$y %||% NULL,
                 ncell = as.integer(lat$ncell)))
}

# FR7: `abm_tell(to = <id expression>)` natively ([run_tell()] without `.order`,
# `"neighbours"`, list recipients, `.resolve = "collect"`/`"max"`/`"min"`): `when`,
# `to` and each message compiled over the groups stacked, read in that order
rl_tell_ok <- function(step) {
  if (!identical(step$to, "id") || !is.null(step$order) ||
      !step$resolve %in% c("last", "first", "sum", "mean", "error")) return(FALSE)
  if (any(vapply(step$rules, function(r) r$target %in% c(".cell", "partner_.cell", ".id"), NA))) {
    return(FALSE)
  }
  quos <- c(list(step$when, step$to_quo), lapply(step$rules, `[[`, "quo"))
  all(vapply(Filter(Negate(is.null), quos), function(q) {
    !is_rx_no(rx_compile_memo(rlang::quo_get_expr(q)))
  }, NA))
}
rl_prep_tell <- function(step, state) {
  if (!planned(step, "rx_tell", rl_tell_ok(step))) return(FALSE)
  if (length(state$relations)) return(NULL)
  quos <- c(if (!is.null(step$when)) list(step$when), list(step$to_quo),
            lapply(step$rules, `[[`, "quo"))
  fake <- planned(step, "rx_tell_fake", rl_fake_rules(lapply(quos, function(q) {
    list(target = ".tell", quo = q)
  })))
  mnames <- if (planned(step, "uses_match", reads_match(quos))) {
    rx_match_names(fake) %||% return(NULL)
  } else character()
  if (any(startsWith(mnames, "partner_"))) return(NULL)
  p <- rx_program(fake, seq_along(quos), model_columns(state$groups), state$globals,
                  match_names = mnames)
  if (is_rx_no(p)) return(NULL)
  rl_scope_bind(list(kind = 11L, scope_match = FALSE,
                     groups = list(list(name = "", prog = p$prog$.ptr, cols = p$cols,
                                        globs = p$globs, targets = vapply(step$rules, `[[`, "", "target"),
                                        scope = p$scope, ew = FALSE, mcols = p$mcols)),
                     tell = list(when = !is.null(step$when), resolve = step$resolve)),
                if (length(p$scope)) list(p$env))
}

# FR8: `abm_sequential()` natively ([run_sequential()]): every rule compiled,
# each group's rules (as [applicable_rules()] routes them) one program evaluated
# agent by agent; `.order` compiled over the groups stacked. No relation, no
# `.cell` and no new column (R adds it and routes the rules again).
rl_seq_ok <- function(step) {
  !any(vapply(step$rules, function(r) r$target %in% c(".cell", "partner_.cell"), NA)) &&
    !any(vapply(rx_rules(step), is_rx_no, NA)) &&
    (is.null(step$order) || !is_rx_no(rx_compile_memo(rlang::quo_get_expr(step$order))))
}
rl_prep_seq <- function(step, state) {
  if (!planned(step, "rx_seq", rl_seq_ok(step))) return(FALSE)
  if (length(state$relations)) return(NULL)
  groups <- state$groups
  cols_all <- model_columns(groups)
  gl <- names(state$globals)
  targets <- vapply(step$rules, `[[`, "", "target")
  syms <- unique(unlist(lapply(rx_rules(step), `[[`, "syms")))
  # `n` is the agent's own `n()` in the R engine's mask; a `partner_` column of
  # the model's, or a target that is both a global and a column, reads otherwise
  if ("n" %in% c(cols_all, syms) || any(startsWith(cols_all, "partner_")) ||
      any(targets %in% gl & targets %in% cols_all)) return(NULL)
  quos <- lapply(step$rules, `[[`, "quo")
  mnames <- if (planned(step, "uses_match", reads_match(quos))) {
    planned(step, "rx_mnames", rx_match_names(step)) %||% return(NULL)
  } else character()
  out <- list()
  envs <- list()
  for (nm in names(groups)) {
    gnames <- names(groups[[nm]])
    idx <- applicable_rules(step, gnames, groups, state$globals)
    if (!length(idx)) next
    tg <- targets[idx]
    if (!all(startsWith(tg, "partner_") | tg %in% gnames | tg %in% gl)) return(NULL)
    p <- rx_program(step, idx, gnames, state$globals, match_names = mnames)
    if (is_rx_no(p)) return(NULL)
    if (length(p$scope)) envs <- c(envs, list(p$env))
    wslot <- match(tg, p$names) - 1L
    wslot[is.na(wslot)] <- -1L
    out[[length(out) + 1L]] <- list(name = nm, prog = p$prog$.ptr, cols = p$cols,
                                    globs = p$globs, targets = tg, scope = p$scope,
                                    ew = FALSE, mcols = p$mcols, wslot = as.integer(wslot))
  }
  if (!is.null(step$order)) {
    if (planned(step, "order_match", reads_match(list(step$order)))) return(NULL)
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
                     seq = list(order = !is.null(step$order))), envs)
}

# the network neighbour shapes the loop folds natively: every rule `sum()` or
# `mean()` of a compiled elementwise argument ([neighbour_fast()]'s shapes), no
# lattice direction, no `within`
rl_nb_ok <- function(step) {
  is.null(step$where) && is.null(step$within) &&
    all(vapply(planned(step, "shapes", lapply(step$rules, function(r) nb_fast_shape(r$quo))),
               function(s) !is.null(s) && !is_rx_no(rx_compile_memo(s$arg)), NA))
}

# [run_neighbours()]'s set-up and [neighbour_fast()]'s index for these edges and
# this population, as R would leave them in `state$nb_cache`
rl_prep_neighbours <- function(step, state, slot = NULL) {
  if (!is.null(step$within)) {
    return(rl_prep_memo(step, state, slot, if (rl_nb_within_ok(step)) rl_prep_nb_within
                                           else rl_prep_nb_cross))
  }
  if (!is.null(step$where)) return(rl_prep_nb_where(step, state))
  if (!planned(step, "rx_nbshape", rl_nb_ok(step))) return(FALSE)
  if (is.null(state$edges) || length(state$relations) || !is.null(state$stale_draws) ||
      length(drawn_names(state$edges))) return(NULL)
  shapes <- planned(step, "shapes", lapply(step$rules, function(r) nb_fast_shape(r$quo)))
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
    if (identical(hit$pg, pg) && hit$n == n && rlang::is_reference(hit$edges, state$edges) &&
        (is.null(hit$cache0) && is.null(state$nb_cache) ||
           rlang::is_reference(hit$cache0, state$nb_cache))) {
      return(hit$desc)
    }
  }
  cache0 <- state$nb_cache
  cache <- state$nb_cache
  if (!identical(cache$edges, state$edges)) {
    cache <- list(edges = state$edges, nb = neighbour_table(state$edges))
  }
  ix <- cache$ix
  if (is.null(ix) || ix$n != n) {
    ix <- nb_index(cache$nb, n)
    cache$ix <- ix
  }
  if (!ix$ok) return(NULL)
  desc <- rl_scope_bind(list(kind = 3L, scope_match = FALSE, groups = list(prog_g),
       nb = list(mean = vapply(shapes, function(s) s$fn == "mean", NA),
                 targets = vapply(step$rules, `[[`, "", "target"), cols = ix$cols,
                 cnt = ix$cnt, cache = cache, cache_name = "nb_cache",
                 edges = state$edges, n = n)),
    if (length(prog_g$scope)) list(envs[[1]]))
  if (!is.null(dkey)) {
    assign(dkey, c(utils::tail(memo, 3L),
                   list(list(pg = pg, n = n, edges = state$edges, cache0 = cache0, desc = desc))),
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
