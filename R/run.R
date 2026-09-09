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
#' state produced by [abm_setup()], before any step has run, so a run of `n`
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
#' @section Many runs:
#'
#' `params` and `reps` turn one call into several runs. `params` is a named list
#' whose entries override the model's `globals` before each run; an entry holding
#' more than one value is swept, one run per value, and several entries give
#' every combination of them. `reps` repeats each of those combinations.
#'
#' ``` abm_run(m, go, ticks = 500, params = list(d = c(0.1, 0.2, 0.3)), reps =
#' 20, seed = 1) ```
#'
#' is sixty runs. Their results come back stacked into one `abm_result`, with
#' `.run`, `.rep` and one column per parameter in front of the ordinary columns,
#' and [abm_globals()], [abm_measures()] and [abm_edges()] carry the same
#' identifying columns. A single run is left exactly as it was: no `.run`, no
#' `.rep`, and the same columns the function has always returned.
#'
#' A parameter is a global, and only a global. `n`, [abm_network()]'s `degree`
#' and a lattice's `dims` are fixed when [abm_setup()] builds the world, so
#' sweeping one of those still means one world per value.
#'
#' `reps` replicates the *run*, not the population. `abm_run()` is handed a world
#' that has already been built, so a starting column drawn with `~runif(n)` was
#' drawn once, before this function saw it, and every replicate begins from that
#' same population. What varies across replicates is everything the run itself
#' draws -- who is matched with whom, which agent acts first, what a rule
#' samples. To vary the starting population too, build a world per replicate.
#'
#' @section Measures:
#'
#' `measures` records a summary of the population once per tick, without keeping
#' the population. Each entry is a one-sided formula evaluated the way an
#' [abm_global()] right-hand side is -- the globals are in scope, `n()` is the
#' population size -- and must collapse to one value.
#'
#' ``` abm_run(m, go, ticks = 500, record = "globals", measures = list(gini =
#' ~gini(money), broke = ~mean(money == 0))) ```
#'
#' A measure is an observation, not model state, and the difference is not
#' cosmetic: writing the same summary with [abm_global()] would put it where
#' every rule in the model can read it. A measure is written to the table
#' [abm_measures()] returns and to nowhere else, so nothing in the model can see
#' it. Together with `record = "globals"` this is what makes a sweep affordable:
#' the whole trajectory of every run, at a few thousand rows rather than a few
#' million.
#'
#' @param model An `abm_model` from [abm_setup()].
#' @param go An `abm_go` sequence.
#' @param ticks Number of ticks to run.
#' @param params Optional named list of values overriding the model's `globals`,
#'   one run per combination. An entry holding several values is swept; a global
#'   that is itself a vector is held fixed by wrapping it in a `list()`, the same
#'   way a non-scalar global is written into the log. See *Many runs* below.
#' @param reps Number of replicates of each parameter combination. Replicates
#'   differ in what the run draws, not in the population they start from.
#' @param measures Optional named list of one-sided formulas, each evaluated over
#'   the whole population once per tick and recorded with [abm_measures()]. A
#'   measure cannot be read by any step. See *Measures* below.
#' @param seed Optional integer seed. Set locally, so the caller's random state
#'   is left untouched. With more than one run this seeds the experiment and a
#'   seed per run is derived from it, so the whole thing reproduces at once. See
#'   the details above on why a random starting population also needs
#'   [abm_setup()]'s `seed`.
#' @param record Which ticks' populations to keep. `"all"` (the default) keeps
#'   every one; a positive whole number keeps every `record`-th tick, plus tick 0
#'   and the last; `"final"` keeps only the last; `"globals"` keeps none. Globals
#'   are recorded every tick whatever this says, since they are one row each. A
#'   model whose population grows needs this: recording every agent of every tick
#'   is what makes such a run die of memory rather than merely take a while. It
#'   applies to each run, so its cost multiplies by how many there are.
#' @param progress Whether to show a progress bar with an ETA while the run is
#'   going. `NULL` (the default) follows the session: a bar at an interactive
#'   console once the run has been going long enough to be worth reporting, and
#'   nothing while knitr, pkgdown or `R CMD check` is running the code, so it
#'   never turns up in a rendered page. `TRUE` forces it on from the first tick,
#'   `FALSE` off. The bar counts ticks for a single run and runs for many.
#'
#' @return An `abm_result`: a tibble of one row per agent per tick, carrying
#'   the run's globals, measures and final network as attributes.
#' @export
#' @examples
#' economy <- abm_setup(agents = abm_agents(n = 50, money = 100))
#' go <- abm_go(
#'   abm_match(pair = "random", role = list(giver = money > 0, receiver = TRUE)),
#'   abm_rules(money ~ if_else(.role == "giver", money - 1, money + 1))
#' )
#' result <- abm_run(economy, go, ticks = 10, seed = 1)
#' result
#'
#' # ten replicates, summarised as they go and keeping no populations
#' many <- abm_run(economy, go, ticks = 10, reps = 10, seed = 1,
#'                 record = "globals",
#'                 measures = list(richest = ~max(money)))
#' abm_measures(many)
abm_run <- function(model, go, ticks, params = NULL, reps = 1, measures = NULL,
                    seed = NULL, record = "all", progress = NULL) {
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
  # `seed` used to be handed straight to `set.seed()`, which made `seed = 1:20`
  # -- the obvious way to ask for twenty replicates -- a single run on seed 1,
  # silently. Replicates are `reps` now, and this says so.
  if (!is.null(seed) && !rlang::is_scalar_integerish(seed)) {
    abm_abort(
      c("{.arg seed} must be a single whole number.",
        "i" = "For several runs use {.arg reps} or {.arg params}; the seed for each
               one is derived from this."),
      class = "tidyABM_bad_seed"
    )
  }
  if (!rlang::is_scalar_integerish(reps) || is.na(reps) || reps < 1) {
    abm_abort("{.arg reps} must be a single whole number, at least 1.",
              class = "tidyABM_bad_reps")
  }
  reps <- as.integer(reps)
  params <- check_params(params, model)
  measures <- check_measures(measures)

  if (!is.null(progress) && !rlang::is_bool(progress)) {
    abm_abort("{.arg progress} must be {.code TRUE}, {.code FALSE} or {.code NULL}.",
              class = "tidyABM_bad_progress")
  }

  runs <- param_grid(params, reps)
  n_runs <- length(runs)
  seeds <- run_seeds(seed, n_runs)
  keep <- recorded_ticks(record, ticks)

  # `NULL` follows the session: a bar at a console, nothing when knitr, pkgdown
  # or `R CMD check` is running the code, so it never lands in a rendered page.
  # `TRUE` overrides that and also drops cli's wait, so it shows from tick 1.
  # One run counts ticks, as it always has; many count runs, because "tick 340
  # of 500" says little when there are sixty of them.
  show_progress <- (ticks > 0L || n_runs > 1L) &&
    (if (is.null(progress)) rlang::is_interactive() else progress)
  if (isTRUE(progress)) {
    rlang::local_options(cli.progress_show_after = 0)
  }
  by_run <- n_runs > 1L
  bar <- NULL
  if (show_progress) {
    bar <- cli::cli_progress_bar(
      if (by_run) "Running experiment" else "Running model",
      total = if (by_run) n_runs else ticks
    )
  }

  out <- vector("list", n_runs)
  for (i in seq_len(n_runs)) {
    out[[i]] <- run_once(apply_params(model, runs[[i]]$values), go, ticks,
                         seeds[[i]], keep, measures,
                         bar = if (by_run) NULL else bar)
    if (by_run && !is.null(bar)) cli::cli_progress_update(id = bar)
  }
  if (!is.null(bar)) cli::cli_progress_done(id = bar)

  if (!by_run) {
    one <- out[[1]]
    return(new_abm_result(one$population, one$globals, one$measures,
                          one$network, ticks, record, 1L))
  }

  meta <- lapply(runs, run_meta)
  bind_piece <- function(what) {
    pieces <- Map(function(o, mt) with_meta(o[[what]], mt), out, meta)
    pieces <- Filter(Negate(is.null), pieces)
    if (!length(pieces)) return(NULL)
    dplyr::bind_rows(pieces)
  }
  new_abm_result(bind_piece("population"), bind_piece("globals"),
                 bind_piece("measures"), bind_piece("network"),
                 ticks, record, n_runs)
}

#' Assemble an `abm_result` from the pieces one or many runs produced
#' @noRd
new_abm_result <- function(population, globals, measures, network, ticks,
                           record, n_runs) {
  structure(
    population,
    globals  = globals,
    measures = measures,
    network  = network,
    ticks    = ticks,
    record   = record,
    runs     = n_runs,
    class    = c("abm_result", class(population))
  )
}

#' Replay `go` once, for `ticks` ticks
#'
#' One run and nothing else: the parameters are already written into `model`,
#' the seed is already chosen, and which ticks to keep is already decided. What
#' comes back are the four tables the run produced, without the columns that say
#' which run it was -- `abm_run()` adds those, and only when there is more than
#' one run to tell apart.
#' @noRd
run_once <- function(model, go, ticks, seed, keep, measures, bar = NULL) {
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
    lattice = model$lattice,
    match   = NULL,
    next_id = n_agents(model) + 1L
  )

  snapshots <- vector("list", ticks + 1L)
  global_log <- vector("list", ticks + 1L)
  measure_log <- if (length(measures)) vector("list", ticks + 1L) else NULL
  empty <- snapshot(state, 0L)[0, , drop = FALSE]
  if (keep[[1]]) snapshots[[1]] <- snapshot(state, 0L)
  global_log[[1]] <- global_row(state, 0L)
  if (!is.null(measure_log)) measure_log[[1]] <- measure_row(measures, state, 0L)

  for (t in seq_len(ticks)) {
    state$match <- NULL
    for (step in go$steps) {
      state <- run_step(step, state)
    }
    # a population that is not recorded is not kept, which is the whole point:
    # the run then slows down rather than filling memory
    if (keep[[t + 1L]]) snapshots[[t + 1L]] <- snapshot(state, t)
    global_log[[t + 1L]] <- global_row(state, t)
    if (!is.null(measure_log)) measure_log[[t + 1L]] <- measure_row(measures, state, t)
    if (!is.null(bar)) cli::cli_progress_update(id = bar)
  }

  snapshots <- Filter(Negate(is.null), snapshots)
  population <- if (length(snapshots)) dplyr::bind_rows(snapshots) else empty
  population <- dplyr::relocate(population, "tick", ".id", ".group")
  list(
    population = population,
    globals    = dplyr::bind_rows(global_log),
    measures   = if (is.null(measure_log)) NULL else dplyr::bind_rows(measure_log),
    network    = strip_draws(state$edges)
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
      m <- run_match(step, bind_groups(state$groups), state$edges, state$globals)
      state$match <- list(match = m, size = step$size)
      state
    },
    abm_rules      = run_rules(step, state),
    abm_sequential = run_sequential(step, state),
    abm_global     = run_global(step, state),
    abm_neighbours = run_neighbours(step, state),
    abm_move       = run_move(step, state),
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
#' A global is model state: every rule in the model can read it. For a summary
#' that the model must *not* be able to read, use [abm_run()]'s `measures` and
#' [abm_measures()].
#'
#' @param x An `abm_result` from [abm_run()].
#' @return A tibble with one row per tick and one column per global, preceded by
#'   the run's `.run`, `.rep` and parameter columns when there was more than one
#'   run.
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
#'   With more than one run the edges of every run are stacked, preceded by that
#'   run's `.run`, `.rep` and parameter columns.
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
  # a result that has been subset down to a few columns may no longer carry
  # `.id`; that is a narrower view of the run, not a run with no agents in it
  if (!".id" %in% names(x)) return(NA_integer_)
  length(unique(x$.id))
}

#' @export
print.abm_result <- function(x, ...) {
  rec <- attr(x, "record")
  note <- if (identical(rec, "all") || is.null(rec)) "" else
    cli::format_inline(", recording {.val {rec}}")
  runs <- attr(x, "runs") %||% 1L
  runs_txt <- if (runs > 1L) cli::format_inline("{runs} runs, ") else ""
  seen <- n_agents(x)
  seen_txt <- if (is.na(seen)) "" else
    cli::format_inline("{seen} agent{?s} seen, ")
  cli::cli_text(
    "{.cls abm_result} {runs_txt}{attr(x, 'ticks')} tick{?s}, {seen_txt}{nrow(x)} row{?s}{note}"
  )
  print(tibble::as_tibble(x), ...)
  invisible(x)
}
