# The Rust run path (migration phase 3): the run's agent groups live in the Rust
# column store (src/rust/src/store.rs), the snapshots `record` keeps are recorded
# there, and the result is stacked there. The R engine's code is not changed:
# `abm_run()` picks [run_once()] or [run_once_rust()] once per call
# ([engine_run_once()]), and every step without a Rust kernel runs through the
# *step adapter* -- the store's groups are handed to the R engine's own step
# function and what it returns is loaded back (PLAN.md design rules 6 and 7).
#
# Steps with a Rust kernel (R/rust-kernels.R, registry `rust_kernels`) run it;
# "auto" takes the Rust path only for a run with a kernel proven in "both" mode
# and faster than the R engine (`auto = TRUE` in the registry), "rust" runs every
# step through its kernel or the adapter, and "both" runs each step on the R
# engine and on the Rust path from the same state and random stream, compares
# the two after every step, and compares the stacked results at the end.
#
# Not in the store: globals, edges, lattice, relations and the standing match
# stay in the R-side `state` list; the kernels read them from there.

#' The kernels a run under `engine` uses, by step class
#'
#' `options(tidyABM.rust_kernels = c(neighbours = FALSE))` turns a kernel off
#' (named by its step class without `abm_`); its steps then run through the
#' adapter. "auto" uses only the kernels registered with `auto = TRUE`.
#' @noRd
active_kernels <- function(engine) {
  ks <- rust_kernels
  off <- getOption("tidyABM.rust_kernels")
  if (length(off)) {
    off <- names(off)[!vapply(off, isTRUE, NA)]
    ks <- ks[!sub("^abm_", "", names(ks)) %in% off]
  }
  if (identical(engine, "auto")) ks <- Filter(function(k) isTRUE(k$auto), ks)
  ks
}

#' The run function `abm_run()` calls for one run under `engine`
#'
#' "r" never uses the store; "auto" uses it only when a step has an "auto"
#' kernel; "rust" always uses it; "both" compares the two paths step by step. A
#' run with a step a native kernel takes (`native` in the registry) has its tick
#' loop in Rust ([run_once_loop()]); otherwise the R-driven [run_once_rust()].
#' @noRd
engine_run_once <- function(engine, steps, has_until = FALSE) {
  kernels <- if (engine != "r") active_kernels(engine)
  # the steps inside `abm_repeat()` blocks count: the loop runs them itself
  leaves <- rl_flatten(steps)
  steps <- leaves$steps[leaves$kinds == 0L]
  has <- any(vapply(steps, function(s) {
    k <- kernels[[class(s)[[1]]]]
    !is.null(k) && (is.null(k$can) || isTRUE(k$can(s)))
  }, NA))
  native <- has && any(vapply(steps, rl_kind, 0L, kernels = kernels) > 0L)
  # a run with no step the loop takes natively is the R engine's own: the store
  # adapter only added conversion costs (measured, FR3); "both" still tests it
  rust_fn <- if (native) function(...) run_once_loop(..., kernels = kernels)
             else if (has) function(...) run_once_rust(..., kernels = kernels)
             else run_once
  switch(engine,
    r = run_once,
    auto = rust_fn,
    rust = rust_fn,
    both = function(...) run_once_rust(..., kernels = kernels, both = TRUE))
}

# the names the store's stacking treats specially, in the order it expects them
store_special <- c(".id", ".group", "tick", "")

#' [run_once()] with the groups in the Rust store
#'
#' The same run, step for step: the same seed handling, cache resets, tick-0
#' record, globals log, measures, checks and error reporting, so a result and an
#' error are the R engine's. `both = TRUE` also runs the R engine alongside (see
#' [rust_step_both()]) and returns its result once the two have agreed throughout.
#' Keep in step with [run_once()] (decision D12: R first, Rust follows).
#' @noRd
run_once_rust <- function(model, go, ticks, seed, keep, measures, checks = NULL,
                          bar = NULL, until = NULL, record = "all",
                          both = FALSE, kernels = active_kernels("rust")) {
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
    next_id     = n_agents(model) + 1L
  )
  # "both" exports fresh copies, so every step tests the conversion itself
  store <- rust_call(AgentStore$new(state$groups, store_special, !both))
  if (!both) state$groups <- store$export()
  # outside "both" the store is brought up to date only when it is read: before a
  # kernel and before a snapshot that may reach the result (see rust_adapter())
  sync <- if (!both) new.env(parent = emptyenv())
  if (!both) sync$ok <- TRUE

  snapshots <- if (both) vector("list", ticks + 1L)
  global_log <- vector("list", ticks + 1L)
  measure_log <- if (length(measures)) vector("list", ticks + 1L) else NULL
  store$snapshot(0L, keep[[1]])
  if (both) {
    first <- snapshot(state, 0L)
    if (keep[[1]]) snapshots[[1]] <- first
  }
  global_log[[1]] <- global_cells(state, 0L)
  if (!is.null(measure_log)) measure_log[[1]] <- measure_row(measures, state, 0L)
  if (length(checks)) check_row(checks, state, 0L)

  steps <- plan_steps(go$steps)
  has_kernel <- vapply(steps, function(s) class(s)[[1]] %in% names(kernels), NA)
  measure_reads <- read_vars(measures)
  check_reads <- read_vars(checks)
  until_reads <- if (!is.null(until)) read_vars(list(until))
  measure_plan <- new.env(parent = emptyenv())
  check_plan <- new.env(parent = emptyenv())
  until_plan <- new.env(parent = emptyenv())
  at <- 0L
  ran_ticks <- 0L
  withCallingHandlers({
    for (t in seq_len(ticks)) {
      state$match <- NULL
      state$tick <- t
      .run_ctx$tick <- t
      for (i in seq_along(steps)) {
        at <- i
        if (both) {
          state <- rust_step_both(steps[[i]], state, store, i, kernels)
        } else if (has_kernel[[i]]) {
          state <- rust_step(steps[[i]], state, store, kernels, sync = sync)
        } else {
          # rust_adapter(steps[[i]], state, store, sync), inlined: it is most steps
          state <- run_step(steps[[i]], state)
          sync$ok <- FALSE
        }
      }
      at <- 0L
      ran_ticks <- t
      stop_now <- !is.null(until) && until_met(until, state, t, until_reads, until_plan)
      keep_now <- keep[[t + 1L]] || (stop_now && !identical(record, "globals"))
      if (keep_now) {
        if (!both && !sync$ok) {
          store$load(state$groups)
          sync$ok <- TRUE
        }
        store$snapshot(t, TRUE)
      }
      if (both && keep_now) snapshots[[t + 1L]] <- snapshot(state, t)
      global_log[[t + 1L]] <- global_cells(state, t)
      if (!is.null(measure_log)) {
        measure_log[[t + 1L]] <- measure_row(measures, state, t, measure_reads, measure_plan)
      }
      if (length(checks)) check_row(checks, state, t, check_reads, check_plan)
      if (!is.null(bar)) cli::cli_progress_update(id = bar)
      if (stop_now) break
    }
  }, error = function(e) {
    if (at == 0L || inherits(e, "tidyABM_error")) return()
    rethrow_step_error(e, steps[[at]], state, at)
  })

  global_log <- global_log[seq_len(ran_ticks + 1L)]
  if (!is.null(measure_log)) measure_log <- measure_log[seq_len(ran_ticks + 1L)]
  population <- store_population(store)
  if (both) {
    snapshots <- Filter(Negate(is.null), snapshots[seq_len(ran_ticks + 1L)])
    ref <- if (length(snapshots)) dplyr::bind_rows(snapshots) else first[0, , drop = FALSE]
    front <- c("tick", ".id", ".group")
    cols <- names(ref)
    ref <- tibble::new_tibble(unclass(ref)[c(front, cols[!cols %in% front])],
                              nrow = vctrs::vec_size(ref))
    where <- diff_where(ref, population)
    if (!is.null(where)) {
      abm_abort(c("The R engine and the Rust store stack the recorded ticks differently.",
                  "x" = "{where}"),
                class = "tidyABM_engine_mismatch", call = quote(abm_run()))
    }
    population <- ref
  }
  list(
    population = population,
    globals    = global_table(global_log),
    measures   = if (is.null(measure_log)) NULL else dplyr::bind_rows(measure_log),
    network    = strip_draws(state$edges),
    relations  = relation_tables(state$relations),
    ticks      = ran_ticks
  )
}

#' One step on the Rust path: its kernel, or the step adapter
#'
#' A kernel returns the new `state`, or `NULL` to decline before it has changed
#' anything. With `replay` (every engine but "both") a kernel that declines,
#' errors, warns or messages hands the step to the adapter from the state and
#' `.Random.seed` it started from, so the R engine's own code raises the error,
#' warning or message exactly as it would have. Under "both" (`replay = FALSE`)
#' only a decline falls back; anything else the kernel raises is compared with
#' the R engine's, so a kernel that misbehaves is caught, not hidden.
#' @noRd
rust_step <- function(step, state, store, kernels, replay = TRUE, sync = NULL) {
  k <- kernels[[class(step)[[1]]]]
  # no kernel, or one that said this step's shape never compiles
  if (is.null(k) || isTRUE(.subset2(step, "plan")$rust_no)) {
    return(rust_adapter(step, state, store, sync))
  }
  kernel <- k$fun
  if (!isFALSE(k$store) && !is.null(sync) && !sync$ok) {
    store$load(state$groups)
    sync$ok <- TRUE
  }
  seed <- rng_get()
  out <- if (!replay) kernel(step, state, store) else {
    spoke <- FALSE
    hush <- function(c) {
      spoke <<- TRUE
      tryInvokeRestart(if (inherits(c, "warning")) "muffleWarning" else "muffleMessage")
    }
    res <- tryCatch(withCallingHandlers(kernel(step, state, store),
                                        warning = hush, message = hush),
                    error = function(e) NULL)
    if (spoke) NULL else res
  }
  if (!is.null(out)) {
    # a kernel that left the store aside changed `state$groups` as an R step does
    if (isFALSE(k$store)) return(rust_synced(out, store, sync))
    return(out)
  }
  rng_set(seed)
  rust_adapter(step, state, store, sync)
}

#' The step adapter: the R engine's own step function over the store's groups
#'
#' `state$groups` holds the store's groups (exported after the previous step).
#' Under "both" (`sync = NULL`) what the step returns is loaded back and exported
#' again at once, so every step crosses the store and is compared. Otherwise the
#' store is only marked out of date (`sync$ok`) and loaded when it is next read
#' -- before a kernel, before a recorded snapshot -- which costs one load per
#' read instead of a load and an export (~5 us) per step: with a kernel on one
#' step in five that difference was the kernel's whole gain.
#' @noRd
rust_adapter <- function(step, state, store, sync = NULL) {
  rust_synced(run_step(step, state), store, sync)
}

# after a step that changed `state$groups` in R: mark the store stale, or under
# "both" load it and export again at once
rust_synced <- function(state, store, sync) {
  if (!is.null(sync)) {
    sync$ok <- FALSE
    return(state)
  }
  # not through rust_call(): its tryCatch() costs 3 us a step, more than the load;
  # the store raises only on an internal fault, which the run reports as the step's
  store$load(state$groups)
  state$groups <- store$export()
  state
}

#' One step on both paths, compared ("both" mode)
#'
#' The R engine runs the step first, from `state`; its result, warnings, messages
#' and `.Random.seed` are the reference, and its errors are the run's errors. The
#' Rust path then runs from the same `state` with the groups exported from the
#' store, the same `.Random.seed` and the same package caches; its printed output
#' is dropped and its warnings and messages are muffled (each is shown once, by
#' the R run) but compared. Any difference -- in the state after the step, in the
#' conditions, in the random stream, or an error only one side raised -- stops
#' the run with [diff_where()]'s description of the first one.
#' @noRd
rust_step_both <- function(step, state, store, at, kernels) {
  seed0 <- rng_get()
  cache0 <- caches_get()
  conds_r <- list()
  state_r <- withCallingHandlers(run_step(step, state),
    warning = function(w) conds_r[[length(conds_r) + 1L]] <<- w,
    message = function(m) conds_r[[length(conds_r) + 1L]] <<- m)
  seed_r <- rng_get()
  cache_r <- caches_get()

  rng_set(seed0)
  caches_set(cache0)
  conds_s <- list()
  in_s <- state
  in_s$groups <- store$export()
  state_s <- tryCatch({
    utils::capture.output(out <- withCallingHandlers(rust_step(step, in_s, store, kernels, FALSE),
      warning = function(w) {
        conds_s[[length(conds_s) + 1L]] <<- w
        invokeRestart("muffleWarning")
      },
      message = function(m) {
        conds_s[[length(conds_s) + 1L]] <<- m
        invokeRestart("muffleMessage")
      }))
    out
  }, error = function(e) e)
  seed_s <- rng_get()
  rng_set(seed_r)
  caches_set(cache_r)

  where <- if (inherits(state_s, "error")) {
    paste0("The Rust path failed where the R engine did not: ", conditionMessage(state_s))
  } else {
    diff_where(state_r, state_s, "state") %||%
      diff_where(cond_digest(conds_r), cond_digest(conds_s), "warnings and messages") %||%
      diff_where(seed_r, seed_s, ".Random.seed")
  }
  if (!is.null(where)) {
    fn <- class(step)[[1]]
    tick <- state$tick %||% 0L
    abm_abort(c("The R engine and the Rust path differ after step {at}, {.fn {fn}}, at tick {tick}.",
                "x" = "{where}",
                "i" = "{.code options(tidyABM.engine = \"both\")} compares the two; {.val r} runs the R engine alone."),
              class = "tidyABM_engine_mismatch", call = quote(abm_run()))
  }
  state_r
}

#' The R-built result from the store, with the R engine's classes and attributes
#'
#' The store stacks the recorded ticks itself when every column is a plain typed
#' vector of one type; otherwise each recorded tick is handed back as groups and
#' stacked by the R engine's own code, as [run_once()] does.
#' @noRd
store_population <- function(store) {
  cols <- store$assemble()
  if (!is.null(cols)) return(tibble::new_tibble(cols, nrow = length(cols[[1]])))
  ticks <- store$ticks()
  snaps <- lapply(seq_along(ticks), function(k) {
    snapshot(list(groups = store$recorded_groups(k - 1L)), ticks[[k]])
  })
  population <- if (length(snaps)) dplyr::bind_rows(snaps) else
    snapshot(list(groups = store$recorded_groups(-1L)), 0L)[0, , drop = FALSE]
  front <- c("tick", ".id", ".group")
  cols <- names(population)
  tibble::new_tibble(unclass(population)[c(front, cols[!cols %in% front])],
                     nrow = vctrs::vec_size(population))
}

# `.Random.seed`, or NULL when there is none; and back
rng_get <- function() {
  if (exists(".Random.seed", globalenv(), inherits = FALSE)) get(".Random.seed", globalenv())
}
rng_set <- function(seed) {
  if (!is.null(seed)) {
    assign(".Random.seed", seed, envir = globalenv())
  } else if (exists(".Random.seed", globalenv(), inherits = FALSE)) {
    rm(".Random.seed", envir = globalenv())
  }
}

# the package-level caches a step may fill, so both paths start from the same ones
cache_envs <- function() list(.bound, .eval_env, .std_memo)
caches_get <- function() lapply(cache_envs(), as.list, all.names = TRUE)
caches_set <- function(saved) {
  Map(function(env, vals) {
    rm(list = ls(env, all.names = TRUE), envir = env)
    list2env(vals, envir = env)
  }, cache_envs(), saved)
  invisible()
}

cond_digest <- function(conds) {
  lapply(conds, function(c) list(class = class(c), message = conditionMessage(c)))
}

#' Where `a` (the R engine's) and `b` (the Rust path's) first differ, or `NULL`
#'
#' Walks lists and data frames by name down to the first differing element of a
#' vector, so the message names the group, the column, the row and both values.
#' @noRd
diff_where <- function(a, b, path = character()) {
  if (identical(a, b)) return(NULL)
  here <- if (length(path)) paste(path, collapse = " > ") else "the result"
  same_shape <- identical(names(a), names(b)) && length(a) == length(b)
  if (is.list(a) && is.list(b) && same_shape) {
    nms <- names(a) %||% as.character(seq_along(a))
    for (i in seq_along(a)) {
      d <- diff_where(a[[i]], b[[i]], c(path, if (is.data.frame(a)) paste("column", nms[[i]]) else nms[[i]]))
      if (!is.null(d)) return(d)
    }
  } else if (is.atomic(a) && is.atomic(b) && identical(typeof(a), typeof(b)) &&
             length(a) == length(b)) {
    show <- function(v) if (is.double(v)) sprintf("%.17g", v) else format(v)
    for (i in seq_along(a)) {
      if (!identical(unclass(a)[i], unclass(b)[i])) {
        return(sprintf("%s, row %d: R %s, Rust %s", here, i, show(unclass(a)[i]),
                       show(unclass(b)[i])))
      }
    }
  } else {
    return(sprintf("%s: R has %s of length %d, Rust %s of length %d", here,
                   class(a)[[1]], length(a), class(b)[[1]], length(b)))
  }
  paste0(here, ": the attributes differ")
}
