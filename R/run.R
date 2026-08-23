# The scheduler ----------------------------------------------------------

#' Run a model
#'
#' `abm_run()` is the last of the three functions a model is made of --
#' [abm_setup()], then [abm_go()], then `abm_run()`. It is the scheduler: it
#' takes the model and the behavioural block, replays the block `ticks` times,
#' and records the whole population after every tick.
#'
#' The result is one long tibble, `tick`, `.id`, `.group`, then every agent
#' column, which is what you want for plotting and summarising. Tick 0 is the
#' state produced by `abm_setup()`, before any step has run, so a run of `n`
#' ticks returns `n + 1` snapshots. Global values are recorded alongside and
#' are available with [abm_globals()].
#'
#' Agent-based models are stochastic, so `seed` is a first-class argument
#' rather than something to arrange yourself: it makes the run reproducible
#' without touching the global random state.
#'
#' Every tick's whole population is recorded by default, which is right for a
#' fixed population and wrong for a growing one -- a run that ends with fifty
#' thousand agents has been keeping every one of them, every tick, since the
#' start. `record` says how much to keep.
#'
#' It fixes the run, though, not the model. If the agents' starting columns
#' were drawn at random, they were drawn when [abm_setup()] was called, and
#' this seed comes too late to affect them. Seed both for an experiment that
#' reproduces end to end:
#'
#' ``` m <- abm_setup(agents = abm_agents(n = 100, x = ~runif(n)), seed = 1) r
#' <- abm_run(m, go, ticks = 100, seed = 1) ```
#'
#' @param model An `abm_model` from [abm_setup()].
#' @param go An `abm_go` sequence.
#' @param ticks Number of ticks to run.
#' @param seed Optional integer seed for the run. Set locally, so the caller's
#'   random state is left untouched. See the details above on why a random
#'   starting population also needs [abm_setup()]'s `seed`.
#' @param record Which ticks' populations to keep. `"all"` (the default) keeps
#'   every one; a positive whole number keeps every `record`-th tick, plus tick 0
#'   and the last; `"final"` keeps only the last; `"globals"` keeps none. Globals
#'   are recorded every tick whatever this says, since they are one row each. A
#'   model whose population grows needs this: recording every agent of every tick
#'   is what makes such a run die of memory rather than merely take a while.
#'
#' @return An `abm_result`: a tibble of one row per agent per tick, carrying
#'   the run's globals and final network as attributes.
#' @export
#' @examples
#' economy <- abm_setup(agents = abm_agents(n = 50, money = 100))
#' go <- abm_go(
#'   abm_match(pair = "random", role = list(giver = money > 0, receiver = TRUE)),
#'   abm_rules(money ~ if_else(.role == "giver", money - 1, money + 1))
#' )
#' result <- abm_run(economy, go, ticks = 10, seed = 1)
#' result
abm_run <- function(model, go, ticks, seed = NULL, record = "all") {
  if (!inherits(model, "abm_model")) {
    abm_abort("{.arg model} must come from {.fn abm_setup}.",
              class = "tidyABM_bad_model")
  }
  if (!inherits(go, "abm_go")) {
    abm_abort("{.arg go} must come from {.fn abm_go}.",
              class = "tidyABM_bad_go")
  }
  if (!rlang::is_scalar_integerish(ticks) || ticks < 0) {
    abm_abort("{.arg ticks} must be a single non-negative whole number.",
              class = "tidyABM_bad_ticks")
  }
  ticks <- as.integer(ticks)
  record <- check_record(record)

  if (!is.null(seed)) {
    old <- if (exists(".Random.seed", .GlobalEnv)) get(".Random.seed", .GlobalEnv) else NULL
    set.seed(seed)
    on.exit({
      if (!is.null(old)) assign(".Random.seed", old, envir = .GlobalEnv)
    }, add = TRUE)
  }

  state <- list(
    groups  = model$groups,
    globals = model$globals,
    edges   = model$edges,
    match   = NULL,
    next_id = n_agents(model) + 1L
  )

  keep <- recorded_ticks(record, ticks)
  snapshots <- vector("list", ticks + 1L)
  global_log <- vector("list", ticks + 1L)
  empty <- snapshot(state, 0L)[0, , drop = FALSE]
  if (keep[[1]]) snapshots[[1]] <- snapshot(state, 0L)
  global_log[[1]] <- global_row(state, 0L)

  for (t in seq_len(ticks)) {
    state$match <- NULL
    for (step in go$steps) {
      state <- run_step(step, state)
    }
    # a population that is not recorded is not kept, which is the whole point:
    # the run then slows down rather than filling memory
    if (keep[[t + 1L]]) snapshots[[t + 1L]] <- snapshot(state, t)
    global_log[[t + 1L]] <- global_row(state, t)
  }

  snapshots <- Filter(Negate(is.null), snapshots)
  out <- if (length(snapshots)) dplyr::bind_rows(snapshots) else empty
  out <- dplyr::relocate(out, "tick", ".id", ".group")
  structure(
    out,
    globals = dplyr::bind_rows(global_log),
    network = strip_draws(state$edges),
    ticks   = ticks,
    record  = record,
    class   = c("abm_result", class(out))
  )
}

#' Check the `record` argument and return it in a canonical form
#' @noRd
check_record <- function(record, call = rlang::caller_env()) {
  if (is.character(record) && length(record) == 1L &&
      record %in% c("all", "final", "globals")) {
    return(record)
  }
  if (rlang::is_scalar_integerish(record) && !is.na(record) && record >= 1) {
    return(as.integer(record))
  }
  abm_abort(
    c("{.arg record} must be {.val all}, {.val final}, {.val globals}, or a whole number.",
      "x" = "Got {.code {deparse1(record)}}."),
    class = "tidyABM_bad_record", call = call
  )
}

#' Which ticks' populations are kept, as a logical over 0..ticks
#' @noRd
recorded_ticks <- function(record, ticks) {
  t <- 0:ticks
  if (identical(record, "all")) return(rep(TRUE, ticks + 1L))
  if (identical(record, "globals")) return(rep(FALSE, ticks + 1L))
  if (identical(record, "final")) return(t == ticks)
  # every record-th tick, and the two ends, so a thinned run still has a before
  # and an after
  t %% record == 0L | t == ticks
}

run_step <- function(step, state) {
  switch(
    class(step)[[1]],
    abm_match = {
      res <- run_match(step, bind_groups(state$groups), state$edges, state$globals)
      state$match <- list(match = res$match, size = step$size)
      if (!is.null(res$updates)) state <- apply_updates(state, res$updates)
      state
    },
    abm_rules      = run_rules(step, state),
    abm_sequential = run_sequential(step, state),
    abm_global     = run_global(step, state),
    abm_neighbours = run_neighbours(step, state),
    abm_draw       = run_draw(step, state),
    abm_tell       = run_tell(step, state),
    abm_birth      = run_birth(step, state),
    abm_death      = run_death(step, state),
    abm_link       = run_link(step, state),
    abm_unlink     = run_unlink(step, state),
    abm_repeat     = run_repeat(step, state),
    abm_abort("Unknown step type {.cls {class(step)[[1]]}}.",
              class = "tidyABM_unknown_step")
  )
}

#' Write columns produced by a match (e.g. negotiation results) back into groups
#' @noRd
apply_updates <- function(state, updates) {
  cols <- setdiff(names(updates), ".id")
  for (nm in names(state$groups)) {
    g <- state$groups[[nm]]
    idx <- match(g$.id, updates$.id)
    for (cl in cols) {
      g[[cl]] <- updates[[cl]][idx]
    }
    state$groups[[nm]] <- g
  }
  state
}

snapshot <- function(state, t) {
  out <- bind_groups(state$groups)
  out$tick <- t
  out
}

global_row <- function(state, t) {
  if (!length(state$globals)) return(tibble::tibble(tick = t))
  # A global need not be a scalar -- a lookup table, a matrix of payoffs, a
  # vector of prices. One row per tick is still the right shape for the log, so
  # anything that is not a single value is wrapped in a list column rather than
  # silently recycling the tick column.
  vals <- lapply(state$globals, function(v) {
    if (is.null(dim(v)) && length(v) == 1L) v else list(v)
  })
  dplyr::bind_cols(tibble::tibble(tick = t), tibble::as_tibble(vals))
}

#' Global values recorded during a run
#'
#' @param x An `abm_result` from [abm_run()].
#' @return A tibble with one row per tick and one column per global.
#' @export
#' @examples
#' m  <- abm_setup(agents = abm_agents(n = 20, x = 1), globals = list(total = 0))
#' go <- abm_go(abm_global(total ~ sum(x)))
#' r  <- abm_run(m, go, ticks = 3, seed = 1)
#' abm_globals(r)
abm_globals <- function(x) {
  if (!inherits(x, "abm_result")) {
    abm_abort("{.arg x} must be the result of {.fn abm_run}.",
              class = "tidyABM_bad_result")
  }
  attr(x, "globals")
}

#' The network at the end of a run
#'
#' @param x An `abm_result` from [abm_run()].
#' @return A tibble of `from`/`to` edges, or `NULL` if the model had no network.
#' @export
#' @examples
#' m <- abm_setup(agents = abm_agents(n = 10, x = 1),
#'                network = abm_network(type = "random", degree = 2))
#' go <- abm_go(abm_match(pair = "network"),
#'              abm_rules(x ~ partner_x))
#' r  <- abm_run(m, go, ticks = 2, seed = 1)
#' abm_edges(r)
abm_edges <- function(x) {
  if (!inherits(x, "abm_result")) {
    abm_abort("{.arg x} must be the result of {.fn abm_run}.",
              class = "tidyABM_bad_result")
  }
  attr(x, "network")
}

#' @export
n_agents.abm_result <- function(x, ...) {
  length(unique(x$.id))
}

#' @export
print.abm_result <- function(x, ...) {
  rec <- attr(x, "record")
  note <- if (identical(rec, "all") || is.null(rec)) "" else
    cli::format_inline(", recording {.val {rec}}")
  cli::cli_text(
    "{.cls abm_result} {attr(x, 'ticks')} tick{?s}, {n_agents(x)} agent{?s} seen, {nrow(x)} row{?s}{note}"
  )
  print(tibble::as_tibble(x), ...)
  invisible(x)
}
