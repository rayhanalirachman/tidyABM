# The scheduler ----------------------------------------------------------

#' Run a model
#'
#' `abm_run()` is the scheduler: it takes a model built by [abm_setup()] and a
#' behavioural block built by [abm_go()], replays the block `ticks` times, and
#' records the whole population after every tick.
#'
#' The result is one long tibble — `tick`, `.id`, `.group`, then every agent
#' column — which is what you want for plotting and summarising. Tick 0 is the
#' state produced by `abm_setup()`, before any step has run, so a run of `n`
#' ticks returns `n + 1` snapshots. Global values are recorded alongside and are
#' available with [abm_globals()].
#'
#' Agent-based models are stochastic, so `seed` is a first-class argument rather
#' than something to arrange yourself: it makes the run reproducible without
#' touching the global random state.
#'
#' It fixes the run, though, not the model. If the agents' starting columns were
#' drawn at random, they were drawn when [abm_setup()] was called, and this seed
#' comes too late to affect them. Seed both for an experiment that reproduces end
#' to end:
#'
#' ```
#' m <- abm_setup(agents = abm_agents(n = 100, x = ~runif(n)), seed = 1)
#' r <- abm_run(m, go, ticks = 100, seed = 1)
#' ```
#'
#' @param model An `abm_model` from [abm_setup()].
#' @param go An `abm_go` sequence.
#' @param ticks Number of ticks to run.
#' @param seed Optional integer seed for the run. Set locally, so the caller's
#'   random state is left untouched. See the details above on why a random
#'   starting population also needs [abm_setup()]'s `seed`.
#'
#' @return An `abm_result`: a tibble of one row per agent per tick, carrying the
#'   run's globals and final network as attributes.
#' @export
#' @examples
#' economy <- abm_setup(agents = abm_agents(n = 50, money = 100))
#' go <- abm_go(
#'   abm_match(pair = "random", role = list(giver = money > 0, receiver = TRUE)),
#'   abm_rules(money ~ if_else(.role == "giver", money - 1, money + 1))
#' )
#' result <- abm_run(economy, go, ticks = 10, seed = 1)
#' result
abm_run <- function(model, go, ticks, seed = NULL) {
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

  snapshots <- vector("list", ticks + 1L)
  global_log <- vector("list", ticks + 1L)
  snapshots[[1]] <- snapshot(state, 0L)
  global_log[[1]] <- global_row(state, 0L)

  for (t in seq_len(ticks)) {
    state$match <- NULL
    for (step in go$steps) {
      state <- run_step(step, state)
    }
    snapshots[[t + 1L]] <- snapshot(state, t)
    global_log[[t + 1L]] <- global_row(state, t)
  }

  out <- dplyr::bind_rows(snapshots)
  out <- dplyr::relocate(out, "tick", ".id", ".group")
  structure(
    out,
    globals = dplyr::bind_rows(global_log),
    network = state$edges,
    ticks   = ticks,
    class   = c("abm_result", class(out))
  )
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
#' m <- abm_setup(agents = abm_agents(n = 20, x = 1), globals = list(total = 0))
#' r <- abm_run(m, abm_go(abm_global(total ~ sum(x))), ticks = 3, seed = 1)
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
#' r <- abm_run(m, abm_go(abm_match(pair = "network"),
#'                        abm_rules(x ~ partner_x)), ticks = 2, seed = 1)
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
  cli::cli_text(
    "{.cls abm_result} {attr(x, 'ticks')} tick{?s}, {n_agents(x)} agent{?s} seen, {nrow(x)} row{?s}"
  )
  print(tibble::as_tibble(x), ...)
  invisible(x)
}
