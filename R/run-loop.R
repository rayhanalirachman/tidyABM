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
  if (keep[[1]]) snapshots[[1]] <- first
  global_log[[1]] <- global_cells(state, 0L)
  if (!is.null(measure_log)) measure_log[[1]] <- measure_row(measures, state, 0L)
  if (length(checks)) check_row(checks, state, 0L)

  steps <- plan_steps(go$steps)
  kinds <- vapply(steps, rl_kind, 0L, kernels = kernels)
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
      if (keep[[t + 1L]]) snapshots[[t + 1L]] <<- snapshot(st, t)
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
  prep <- function(i) rl_prep(steps[[i]], ctx$st)
  rng <- function() {
    saved <<- rng_get()
    NULL
  }

  lp <- RunLoop$new(kinds, prep, seg, tick, rng, ctx)
  state <- withCallingHandlers(lp$run(state, ticks), error = function(e) {
    # a fault in the Rust loop itself: classed as rust_call() does
    if (startsWith(conditionMessage(e), "tidyABM-rust-")) rust_call(stop(e))
    if (at == 0L || inherits(e, "tidyABM_error")) return()
    rethrow_step_error(e, steps[[at]], get("st", envir = frame), at)
  })
  .rl_last$stats <- lp$stats()

  list(
    population = rl_population(snapshots, empty),
    globals    = global_table(global_log),
    measures   = if (is.null(measure_log)) NULL else dplyr::bind_rows(measure_log),
    network    = strip_draws(state$edges),
    relations  = relation_tables(state$relations)
  )
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
  as.integer(k$native)
}

#' Compile a native step for `state`'s schema: a descriptor for
#' `rs_native_exec()` / `RunLoop`, `NULL` (the R path for this schema) or `FALSE`
#' (never native)
#' @noRd
rl_prep <- function(step, state) {
  switch(class(step)[[1]],
         abm_global = rl_prep_global(step, state),
         abm_neighbours = rl_prep_neighbours(step, state),
         rl_prep_rules(step, state))
}

# the rules each group runs, compiled per group as the R engine routes them
rl_prep_rules <- function(step, state) {
  # A `.cell` write moves agents ([run_step()] re-derives `.x`/`.y`); a
  # `partner_` target writes another agent's row: never native
  if (!planned(step, "rx_shape", rx_rules_ok(step))) return(FALSE)
  routes <- if (length(state$groups) > 1L) rule_routes(step, state)
  groups <- list()
  for (nm in names(state$groups)) {
    todo <- if (length(state$groups) == 1L) step$rules else routes[[nm]]
    if (!length(todo)) next
    idx <- which(vapply(step$rules, function(r) any(vapply(todo, identical, NA, r)), NA))
    p <- rx_program(step, idx, names(state$groups[[nm]]), state$globals)
    # values from the formula's environment are re-read every step by the R
    # engine (PLAN.md risk R8): not native yet
    if (is_rx_no(p) || length(p$scope)) return(NULL)
    groups[[length(groups) + 1L]] <- list(name = nm, prog = p$prog$.ptr, cols = p$cols,
                                          globs = p$globs, targets = p$targets)
  }
  list(kind = 1L, scope_match = (step$scope %||% "match") == "match", groups = groups)
}

# one program over every group stacked
rl_prep_global <- function(step, state) {
  if (!planned(step, "rx_gshape", rx_global_ok(step))) return(FALSE)
  groups <- state$groups
  cols <- Reduce(intersect, lapply(groups, names))
  p <- rx_program(step, seq_along(step$rules), cols, state$globals, global_step = TRUE)
  if (is_rx_no(p) || length(p$scope)) return(NULL)
  # a column some group lacks is padded with NA by the R engine: not native
  if (any(p$names %in% setdiff(model_columns(groups), cols))) return(NULL)
  list(kind = 2L, scope_match = FALSE,
       groups = list(list(name = "", prog = p$prog$.ptr, cols = p$cols,
                          globs = p$globs, targets = p$targets)))
}

# the network neighbour shapes the loop folds natively: every rule `sum(<col>)`
# or `mean(<col>)` of a bare name, no lattice direction, no `within`
rl_nb_ok <- function(step) {
  is.null(step$where) && is.null(step$within) &&
    all(vapply(planned(step, "shapes", lapply(step$rules, function(r) nb_fast_shape(r$quo))),
               function(s) !is.null(s) && is.symbol(s$arg), NA))
}

# [run_neighbours()]'s set-up and [neighbour_fast()]'s index for these edges and
# this population, as R would leave them in `state$nb_cache`
rl_prep_neighbours <- function(step, state) {
  if (!planned(step, "rx_nbshape", rl_nb_ok(step))) return(FALSE)
  if (is.null(state$edges) || length(state$relations) || !is.null(state$stale_draws) ||
      length(drawn_names(state$edges))) return(NULL)
  shapes <- planned(step, "shapes", lapply(step$rules, function(r) nb_fast_shape(r$quo)))
  args <- vapply(shapes, function(s) as.character(s$arg), "")
  # a bare name that is a column of every group (else a global, or NA padding)
  if (!all(vapply(state$groups, function(g) all(args %in% names(g)), NA))) return(NULL)
  n <- sum(vapply(state$groups, vctrs::vec_size, 0L))
  if (n == 0L) return(NULL)
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
  list(kind = 3L, scope_match = FALSE, groups = list(),
       nb = list(mean = vapply(shapes, function(s) s$fn == "mean", NA), args = args,
                 targets = vapply(step$rules, `[[`, "", "target"), cols = ix$cols,
                 cnt = ix$cnt, cache = cache, cache_name = "nb_cache",
                 edges = state$edges, n = n))
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
