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
#' ```r
#' m <- abm_setup(agents = abm_agents(n = 100, x = ~runif(n)), seed = 1)
#' r <- abm_run(m, go, ticks = 100, seed = 1)
#' ```
#'
#' @section Many runs:
#'
#' `params` and `reps` turn one call into several runs. `params` is a named list
#' whose entries override the model's `globals` before each run; an entry holding
#' more than one value is swept, one run per value, and several entries give
#' every combination of them. `reps` repeats each of those combinations.
#'
#' ```r
#' abm_run(m, go, ticks = 500, params = list(d = c(0.1, 0.2, 0.3)), reps = 20,
#'         seed = 1)
#' ```
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
#' samples. That includes a random network: a `"poisson"`, `"random"` or
#' `"scale_free"` graph is built once, by [abm_setup()], and shared by every
#' replicate.
#'
#' # A world per run
#'
#' To vary the starting population and the network too, hand `abm_run()` a
#' *function* that builds the world instead of the world itself. It is called
#' once per run, with that run's `params` as its arguments, under the run's
#' seed, so `seed` reproduces the worlds as well as the runs. Because the
#' parameters are arguments, `n`, `degree` and `dims` can be swept here, which
#' they cannot when `model` is a built world.
#'
#' ```r
#' world <- function(net, qo) abm_setup(
#'   agents  = abm_agents(n = 441, income = ~runif(n, 0, 100)),
#'   network = abm_network(net, degree = 4),
#'   globals = list(qo = qo))
#' abm_run(world, go, ticks = 40, reps = 25, seed = 1, record = "globals",
#'         params = list(net = c("random", "ring", "scale_free"),
#'                       qo = c(0.02, 0.1)))
#' ```
#'
#' The run draws from a stream that continues after the world's, not from the
#' same seed again, so a rule's random numbers are independent of the setup's.
#' A function given a single run gets `seed` for the world, and the run uses a
#' seed derived from it, which is not `seed`: a model pinned to a number with a
#' built world will not return that number if it is rebuilt as a function.
#'
#' @section Measures:
#'
#' `measures` records a summary of the population once per tick, without keeping
#' the population. Each entry is a one-sided formula evaluated the way an
#' [abm_global()] right-hand side is -- the globals are in scope, `n()` is the
#' population size -- and must collapse to one value.
#'
#' ```r
#' abm_run(m, go, ticks = 500, record = "globals",
#'         measures = list(gini = ~gini(money), broke = ~mean(money == 0)))
#' ```
#'
#' A measure is an observation, not model state, and the difference is not
#' cosmetic: writing the same summary with [abm_global()] would put it where
#' every rule in the model can read it. A measure is written to the table
#' [abm_measures()] returns and to nowhere else, so nothing in the model can see
#' it. Together with `record = "globals"` this is what makes a sweep affordable:
#' the whole trajectory of every run, at a few thousand rows rather than a few
#' million.
#'
#' @section Checks:
#'
#' `checks` is the same shape as `measures`, one-sided formulas over the whole
#' population with the globals in scope, but each must return a single `TRUE`.
#' They are evaluated at tick 0 and after every tick, and the first one that
#' returns `FALSE` or `NA` stops the run with an error of class
#' `tidyABM_check_failed` naming the check and the tick (the condition carries
#' them as `check` and `tick`). That is what an accounting identity wants: a
#' stock-flow consistent model that leaks money should stop where it leaks,
#' not finish and leave the leak to be found in a plot.
#'
#' ```
#' abm_run(m, go, ticks = 500, seed = 1,
#'         checks = list(money = ~abs(sum(m) - total) < 1e-9 * total))
#' ```
#'
#' The tolerance is part of the formula, because what counts as zero depends
#' on the scale of the ledger. A check is never recorded and no step can read
#' it, so a run with checks returns exactly what the same run without them
#' does. In a sweep the error also names the run, with its parameters and the
#' seed it ran on: a single run with that seed and those `params` reproduces it.
#'
#' @param model An `abm_model` from [abm_setup()], or a function that builds one
#'   from a run's `params` and is called once per run. See *A world per run*.
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
#' @param checks Optional named list of one-sided formulas, evaluated like
#'   `measures` at tick 0 and after every tick, each of which must return a
#'   single `TRUE`. The first that does not stops the run. See *Checks* below.
#' @param until Optional one-sided formula evaluated after each tick over the
#'   whole population (with globals and `.tick` in scope). When it evaluates to
#'   `TRUE`, the run stops early at that tick, keeping the stopping tick in the
#'   recorded output.
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
#' @param cores How many CPU cores share the runs: a whole number, or `"max"`
#'   for all of them. Each run is forked into its own process with
#'   [parallel::mclapply()], and the result is identical to `cores = 1` (the
#'   default). Windows cannot fork: there the runs go to a pool of R sessions
#'   started for the sweep ([parallel::makePSOCKcluster()]), each with the
#'   caller's attached packages, `tidyABM` options and global workspace, which
#'   costs a second or two to start. A parallel sweep shows no progress bar.
#'
#' @return An `abm_result`: a tibble of one row per agent per tick, carrying
#'   the run's globals, measures and final network as attributes. With several
#'   agent groups the tibble holds the union of their columns, so a column one
#'   group does not have is `NA` on the other group's rows.
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
                    checks = NULL, until = NULL, seed = NULL, record = "all",
                    progress = NULL, cores = 1) {
  if (!is.function(model) && !inherits(model, "abm_model")) {
    abm_abort(
      c("{.arg model} must come from {.fn abm_setup}, or be a function that returns one.",
        "i" = "See {.field Many runs} in {.fn abm_run} for a world per run."),
      class = "tidyABM_bad_model")
  }
  if (!inherits(go, "abm_go")) {
    abm_abort("{.arg go} must come from {.fn abm_go}.",
              class = "tidyABM_bad_go")
  }
  if (!rlang::is_scalar_integerish(ticks) || is.na(ticks) || ticks < 0) {
    abm_abort("{.arg ticks} must be a single non-negative whole number.",
              class = "tidyABM_bad_ticks")
  }
  ticks <- as.integer(ticks)
  record <- check_record(record)
  # `seed` used to be handed straight to `set.seed()`, which made `seed = 1:20`
  # -- the obvious way to ask for twenty replicates -- a single run on seed 1,
  # silently. Replicates are `reps` now, and this says so.
  if (!is.null(seed) && (!rlang::is_scalar_integerish(seed) || is.na(seed))) {
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
  checks <- check_measures(checks, "checks", "tidyABM_bad_checks")
  until <- check_until(until)
  inform_stochastic_setup(model, reps)
  engine <- engine_resolve()

  if (!is.null(progress) && !rlang::is_bool(progress)) {
    abm_abort("{.arg progress} must be {.code TRUE}, {.code FALSE} or {.code NULL}.",
              class = "tidyABM_bad_progress")
  }

  if (identical(cores, "max")) cores <- parallel::detectCores()
  if (!rlang::is_scalar_integerish(cores) || is.na(cores) || cores < 1) {
    abm_abort("{.arg cores} must be a whole number, at least 1, or {.val max}.",
              class = "tidyABM_bad_cores")
  }
  cores <- as.integer(cores)

  runs <- param_grid(params, reps)
  n_runs <- length(runs)
  seeds <- run_seeds(seed, n_runs)
  keep <- recorded_ticks(record, ticks)

  # `NULL` follows the session: a bar at a console, nothing when knitr, pkgdown
  # or `R CMD check` is running the code, so it never lands in a rendered page.
  # `TRUE` overrides that and also drops cli's wait, so it shows from tick 1.
  # One run counts ticks, as it always has; many count runs, because "tick 340
  # of 500" says little when there are sixty of them.
  # a forked worker cannot move the parent's bar, so a parallel sweep has none
  show_progress <- (ticks > 0L || n_runs > 1L) && (cores == 1L || n_runs == 1L) &&
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

  # the formulas marked for [eval_rule()] once, not once per run
  go$steps <- mark_steps(go$steps)
  run_fn <- engine_run_once(engine, go$steps, has_until = !is.null(until))  # run_once() unless a Rust path is asked for
  one_run <- function(i) {
    o <- withCallingHandlers({
      start <- build_run(model, runs[[i]]$values, seeds[[i]])
      run_fn(start$model, go, ticks, start$seed, keep, measures, checks,
             bar = if (by_run) NULL else bar, until = until, record = record)
    },
      # one run of many failing says which one, and with what it would rerun
      tidyABM_check_failed = function(e) {
        if (!by_run) return()
        v <- runs[[i]]$values
        which_run <- c(if (length(v)) paste(names(v), "=", vapply(v, deparse1, "")),
                       paste(".rep =", runs[[i]]$.rep),
                       if (!is.null(seeds[[i]])) paste("seed =", seeds[[i]]))
        abm_abort("Run {i} of {n_runs} failed a check: {which_run}.",
                  class = "tidyABM_check_failed", parent = e,
                  tick = e$tick, check = e$check, run = i)
      }
    )
    if (by_run && !is.null(bar)) cli::cli_progress_update(id = bar)
    o
  }
  # each run has its own seed already, so a worker process per core returns
  # exactly what the loop would: forked, or (Windows) a pool of R sessions
  if (cores > 1L && by_run) {
    out <- if (.Platform$OS.type == "windows" || isTRUE(getOption("tidyABM.psock"))) {
      run_psock(n_runs, one_run, cores)
    } else {
      # mclapply's own warnings only announce the failures handled just below
      suppressWarnings(parallel::mclapply(seq_len(n_runs), one_run, mc.cores = cores))
    }
    failed <- Filter(function(o) inherits(o, "try-error"), out)
    if (length(failed)) stop(attr(failed[[1]], "condition"))
    # a worker killed outright (out of memory, say) returns NULL, not an error
    if (any(vapply(out, is.null, logical(1)))) {
      abm_abort("A parallel worker died without a result; try fewer {.arg cores}.",
                class = "tidyABM_worker_died")
    }
  } else {
    out <- lapply(seq_len(n_runs), one_run)
  }
  if (!is.null(bar)) cli::cli_progress_done(id = bar)

  if (!by_run) {
    one <- out[[1]]
    actual_ticks <- one$ticks %||% ticks
    return(new_abm_result(one$population, one$globals, one$measures,
                          one$network, one$relations, actual_ticks, record, 1L))
  }

  meta <- lapply(runs, run_meta)
  bind_piece <- function(what) {
    pieces <- Map(function(o, mt) with_meta(o[[what]], mt), out, meta)
    pieces <- Filter(Negate(is.null), pieces)
    if (!length(pieces)) return(NULL)
    dplyr::bind_rows(pieces)
  }
  # a relation is one table per name, so each name is bound across runs on
  # its own, the way the other pieces are
  rel_names <- names(out[[1]]$relations)
  relations <- if (!length(rel_names)) NULL else stats::setNames(lapply(rel_names, function(nm) {
    pieces <- Map(function(o, mt) with_meta(o$relations[[nm]], mt), out, meta)
    dplyr::bind_rows(Filter(Negate(is.null), pieces))
  }), rel_names)
  new_abm_result(bind_piece("population"), bind_piece("globals"),
                 bind_piece("measures"), bind_piece("network"), relations,
                 ticks, record, n_runs)
}

#' Assemble an `abm_result` from the pieces one or many runs produced
#' @noRd
new_abm_result <- function(population, globals, measures, network, relations,
                           ticks, record, n_runs) {
  structure(
    population,
    globals   = globals,
    measures  = measures,
    network   = network,
    relations = relations,
    ticks     = ticks,
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
run_once <- function(model, go, ticks, seed, keep, measures, checks = NULL,
                     bar = NULL, until = NULL, record = "all") {
  if (!is.null(seed)) local_seed(seed)
  # nothing stacked carries over from another run; see [bind_groups()]
  # ponytail: a fixed size, decided at the start; below it the bookkeeping
  # costs more than the stacking it saves
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
  # what the measures and checks read, worked out once rather than per tick
  measure_reads <- read_vars(measures)
  check_reads <- read_vars(checks)
  until_reads <- if (!is.null(until)) read_vars(list(until))
  # their group aggregates, planned once per schema
  measure_plan <- new.env(parent = emptyenv())
  check_plan <- new.env(parent = emptyenv())
  until_plan <- new.env(parent = emptyenv())
  # one handler for every step of every tick, rather than one set up per
  # step: what [run_step_reporting()] does, for the step running when it fails
  at <- 0L
  ran_ticks <- 0L
  withCallingHandlers({
    for (t in seq_len(ticks)) {
      state$match <- NULL
      state$tick <- t
      .run_ctx$tick <- t
      for (i in seq_along(steps)) {
        at <- i
        state <- run_step(steps[[i]], state)
      }
      at <- 0L
      ran_ticks <- t
      stop_now <- !is.null(until) && until_met(until, state, t, until_reads, until_plan)
      keep_now <- keep[[t + 1L]] || (stop_now && !identical(record, "globals"))
      # a population that is not recorded is not kept, which is the whole point:
      # the run then slows down rather than filling memory
      if (keep_now) snapshots[[t + 1L]] <- snapshot(state, t)
      global_log[[t + 1L]] <- global_cells(state, t)
      if (!is.null(measure_log)) {
        measure_log[[t + 1L]] <- measure_row(measures, state, t, measure_reads, measure_plan)
      }
      if (length(checks)) check_row(checks, state, t, check_reads, check_plan)
      if (!is.null(bar)) cli::cli_progress_update(id = bar)
      if (stop_now) break
    }
  }, error = function(e) {
    # outside a step (a measure, a check) the error goes on as it is
    if (at == 0L || inherits(e, "tidyABM_error")) return()
    rethrow_step_error(e, steps[[at]], state, at)
  })

  global_log <- global_log[seq_len(ran_ticks + 1L)]
  if (!is.null(measure_log)) measure_log <- measure_log[seq_len(ran_ticks + 1L)]
  snapshots <- Filter(Negate(is.null), snapshots[seq_len(ran_ticks + 1L)])
  population <- if (length(snapshots)) dplyr::bind_rows(snapshots) else empty
  # `tick`, `.id` and `.group` first: what `dplyr::relocate()` does, without
  # its tidyselect machinery once per run
  front <- c("tick", ".id", ".group")
  cols <- names(population)
  population <- tibble::new_tibble(unclass(population)[c(front, cols[!cols %in% front])],
                                   nrow = vctrs::vec_size(population))
  list(
    population = population,
    globals    = global_table(global_log),
    measures   = if (is.null(measure_log)) NULL else dplyr::bind_rows(measure_log),
    network    = strip_draws(state$edges),
    relations  = relation_tables(state$relations),
    ticks      = ran_ticks
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

#' Run one step, and say which step it was if it fails
#'
#' An error raised inside a rule used to surface as R's bare message -- "object
#' 'monney' not found" -- with no step, no tick and no column list to check it
#' against. Every error the package raises itself passes through untouched;
#' anything else is rethrown with the step, the tick, the rule that read the
#' missing name and the names that were in scope. `run_step()` itself stays
#' the bare dispatch, the switch `describe_step()` mirrors.
#' @noRd
run_step_reporting <- function(step, state, at = NULL) {
  withCallingHandlers(
    run_step(step, state),
    error = function(e) {
      if (inherits(e, "tidyABM_error")) return()
      rethrow_step_error(e, step, state, at)
    }
  )
}

#' @noRd
rethrow_step_error <- function(e, step, state, at, call = rlang::caller_env()) {
  fn <- class(step)[[1]]
  tick <- state$tick %||% 0L
  head <- if (is.null(at)) "A step inside {.fn abm_repeat}, {.fn {fn}}, failed at tick {tick}."
          else "Step {at} of the go block, {.fn {fn}}, failed at tick {tick}."
  bullets <- c(head, error_lines(e))
  miss <- missing_object_name(e)
  class <- "tidyABM_step_error"
  if (is.null(miss) && grepl("must be size", conditionMessage(e), fixed = TRUE)) {
    bullets <- c(bullets, "i" = "A rule returns one value per agent it is evaluated over, or a single value to recycle. Under a match that is per pair or per group; {.code .scope = \"population\"} makes it the whole population.")
  }
  # `runif(n)` inside a step: `n` is bound to a function there, so the error
  # is "invalid arguments" rather than a missing object
  reads_n <- !is.null(step$rules) &&
    any(vapply(step$rules, function(r) "n" %in% all.vars(rlang::quo_get_expr(r$quo)), logical(1)))
  if (is.null(miss) && reads_n) {
    bullets <- c(bullets, "i" = "A rule reads {.code n} without brackets. Inside a step the number of agents is {.code n()}; {.code n} is the plain number only in an {.fn abm_agents} setup formula.")
  }

  if (!is.null(miss)) {
    name <- miss$name
    class <- c("tidyABM_missing_column", class)
    # the rule(s) that read the name, so a step of ten rules points at one
    if (!is.null(step$rules)) {
      reads <- Filter(function(r) name %in% c(r$vars, all.vars(rlang::quo_get_expr(r$quo))),
                      step$rules)
      if (length(reads)) {
        bullets <- c(bullets, "i" = "In rule{?s} {.code {rule_labels(list(rules = reads))}}.")
      }
    }
    cols <- setdiff(model_columns(state$groups), c(".id", ".group"))
    globals <- names(state$globals)
    pairing <- !is.null(state$match)
    pair <- pairing && state$match$size == 2L
    scope <- c(cols, globals, if (pair) paste0("partner_", cols))

    if (miss$fn && name == "n") {
      bullets <- c(bullets, "i" = "Inside a step {.code n()} is a function; in an {.fn abm_agents} setup formula {.code n} is a number.")
    } else if (!miss$fn && name == "n") {
      bullets <- c(bullets, "i" = "Inside a step the number of agents is {.code n()}, with brackets; {.code n} is the plain number only in an {.fn abm_agents} setup formula.")
    } else if (miss$fn) {
      # a missing function is the user's own problem; nothing to add
    } else if (startsWith(name, "partner_")) {
      base <- sub("^partner_", "", name)
      if (!pairing) {
        bullets <- c(bullets, "i" = "No {.fn abm_match} stands before this step, so there are no {.code partner_<col>} columns. Put the match earlier in the tick.")
      } else if (!pair) {
        bullets <- c(bullets, "i" = "The standing match groups {state$match$size} agents, and {.code partner_<col>} exists only for pairs ({.code size = 2}). In a group, aggregate instead: {.code sum({base})}.")
      } else if (!base %in% cols) {
        bullets <- c(bullets, "i" = "No agent has a column {.field {base}}, so there is no {.code partner_{base}}. Agent columns: {.field {cols}}.")
      }
    } else if (startsWith(name, "own_")) {
      base <- sub("^own_", "", name)
      if (!fn %in% c("abm_neighbours", "abm_match", "abm_move")) {
        bullets <- c(bullets, "i" = "{.code own_<col>} exists only where a focal agent looks at candidates: {.code abm_neighbours(within =)}, {.code abm_match(cost =, among =, weight =)} and {.code abm_move(to = uphill())}. In a rule, an agent's own column is just {.field {base}}.")
      } else if (!base %in% c(cols, ".id", ".group", ".cell", ".x", ".y")) {
        bullets <- c(bullets, "i" = "No agent has a column {.field {base}}. Agent columns: {.field {cols}}.")
      }
    } else if (name %in% c("role", "partner", "id", "group", "cell", "key", "group_id")) {
      bullets <- c(bullets, "i" = "The columns tidyABM manages are dot-prefixed: did you mean {.code .{name}}?")
    } else {
      near <- near_names(name, c(scope, ".role", ".partner", ".id", ".group"))
      if (length(near)) {
        bullets <- c(bullets, "i" = "Did you mean {.field {near}}?")
      }
      bullets <- c(bullets,
                   "i" = if (length(cols)) "Agent columns: {.field {cols}}." else "The population has no columns.",
                   if (length(globals)) c("i" = "Globals: {.field {globals}}."),
                   if (pair) c("i" = "The standing match adds {.code partner_<col>} for each of those, plus {.code .partner} and {.code .role}."))
    }
  }
  abm_abort(bullets, class = class, call = quote(abm_run()), .envir = environment())
}

run_step <- function(step, state) {
  .run_ctx$tick <- as.integer(state$tick %||% 0L)
  state <- switch(
    class(step)[[1]],
    abm_match = {
      # these modes read agents only through their conditions and `by` columns
      mquos <- step[c("role", "eligible", "among", "weight", "cost")]
      cols <- planned(step, "cols", if (step$pair %in% c("random", "one_of", "opposite_group")) {
        reads <- read_vars(mquos)
        if (!is.null(reads)) c(reads, by_columns(step$by), by_columns(step$dot_by))
      })
      comb <- bind_groups(state$groups, cols)
      cvars <- planned(step, "cvars", cell_vars(mquos))
      if (length(cvars)) comb <- attach_cell_columns(comb, state, cvars)
      m <- run_match(step, comb, state$edges, state$globals,
                     relations = state$relations, space = state$space %||% state$lattice)
      if (!is.null(step$steps)) {
        outer_match <- state$match
        state$match <- list(match = m, size = step$size)
        for (s in step$steps) state <- run_step_reporting(s, state)
        state$match <- outer_match
      } else {
        state$match <- list(match = m, size = step$size)
      }
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
    abm_pairs      = run_pairs(step, state),
    abm_repeat     = run_repeat(step, state),
    abm_abort("Unknown step type {.cls {class(step)[[1]]}}.",
              class = "tidyABM_unknown_step")
  )
  # a rule, a message or an inherited value may have written `.cell`, which is
  # how a mobile agent is moved by hand; `.x` / `.y` have to follow
  # (only if the step can have: re-deriving them is a pass over every group)
  if (!is.null(state$lattice) && planned(step, "writes_cell",
      inherits(step, c("abm_rules", "abm_sequential", "abm_tell", "abm_birth")) &&
      any(c(".cell", "partner_.cell") %in%
            vapply(c(step$rules, step$cost, step$inherit), `[[`, "", "target")))) {
    state <- sync_cell_coords(state)
  }
  state
}

#' The run's own copy of the steps, each carrying an empty `plan`
#'
#' What a step works out from its formulas alone -- the columns they read,
#' whether one writes `.cell` -- is the same on every tick, and [planned()]
#' keeps it in the step's plan for the rest of the run. What also depends on
#' the population's columns or on the globals, such as which rule applies to
#' which group, is kept by [planned_for()] with the names it was worked out
#' for, and worked out again when they change. The plan lives on a copy made
#' per run, so the user's `go` is never touched and a step reused with another
#' world starts afresh.
#' @noRd
plan_steps <- function(steps) {
  lapply(steps, function(s) {
    if (inherits(s, "abm_repeat") || (inherits(s, "abm_match") && !is.null(s$steps))) {
      s$steps <- plan_steps(s$steps)
    }
    s$plan <- new.env(parent = emptyenv())
    s
  })
}

#' `value`, worked out on the first ask of the run and kept in the step's plan
#'
#' `value` is a promise, so it is only evaluated when the plan does not hold
#' it yet. A step with no plan (run outside [run_once()]) evaluates it each time.
#' @noRd
planned <- function(step, name, value) {
  p <- .subset2(step, "plan")
  if (is.null(p)) return(value)
  v <- p[[name]]
  if (is.null(v)) {
    v <- list(value)
    assign(name, v, envir = p)
  }
  v[[1]]
}

#' [planned()] for a value that also depends on `key`: kept while `key` is
#' identical to the one it was worked out for
#' @noRd
planned_for <- function(step, name, key, value) {
  p <- .subset2(step, "plan")
  if (is.null(p)) return(value)
  v <- p[[name]]
  if (is.null(v) || !identical(v$key, key)) {
    v <- list(key = key, value = value)
    assign(name, v, envir = p)
  }
  v$value
}

#' The population's column names per group and the globals' names: what
#' decides which rules apply where
#' @noRd
schema_key <- function(state) {
  list(lapply(state$groups, names), names(state$globals))
}

snapshot <- function(state, t) {
  out <- bind_groups(state$groups)
  # the tick column is added on a bare list rather than through `$<-.tbl_df`,
  # which re-checks the whole tibble once per recorded tick
  n <- nrow(out)
  cols <- as.list(out)
  cols$tick <- rep(t, n)
  new_tbl(cols, n)
}

#' One tick's globals as a bare list, for [global_table()]
#'
#' A global need not be a scalar -- a lookup table, a matrix of payoffs, a
#' vector of prices. One row per tick is still the right shape for the log, so
#' anything that is not a single value is wrapped in a list cell rather than
#' silently recycling the tick column.
#' @noRd
global_cells <- function(state, t) {
  g <- state$globals
  # a single value stays as it is; anything else goes in a list cell
  wrap <- lengths(g) != 1L | !vapply(lapply(g, dim), is.null, NA)
  if (any(wrap)) g[wrap] <- lapply(g[wrap], list)
  c(list(tick = t), g)
}

#' The log of [global_cells()] rows as the tibble `bind_rows()` of
#' one-row tibbles of them makes
#'
#' When every row has the same names and each column is a bare scalar of one
#' type, the columns are simply concatenated. Anything else -- a global that
#' appears mid-run, one that changes type, a list cell or a classed value --
#' goes through `bind_rows()`, as it always did.
#' @noRd
global_table <- function(rows) {
  nms <- names(rows[[1]])
  # a global named `tick` (or a repeated or empty name) is left to
  # `bind_rows()`, whose name repair says what it renamed
  if (!anyDuplicated(nms) && !anyNA(nms) && all(nzchar(nms)) &&
      all(vapply(rows, function(r) identical(names(r), nms), NA))) {
    cols <- lapply(nms, function(nm) {
      v <- lapply(rows, .subset2, nm)
      # every cell a bare atomic of the first one's type
      types <- vapply(v, typeof, "")
      if (types[[1]] %in% c("logical", "integer", "double", "complex", "character", "raw") &&
          all(types == types[[1]]) && !any(lengths(lapply(v, attributes)))) {
        unlist(v, use.names = FALSE)
      }
    })
    if (!any(vapply(cols, is.null, NA))) {
      names(cols) <- nms
      return(new_tbl(cols, length(rows)))
    }
  }
  dplyr::bind_rows(lapply(rows, tibble::new_tibble, nrow = 1L))
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
  # distinct `.id`s over the whole run, which is more than the final count
  # once agents have been born
  seen_txt <- if (is.na(seen) || nrow(x) == 0L) "" else
    cli::format_inline("{seen} distinct agent{?s}, ")
  cli::cli_text(
    "{.cls abm_result} {runs_txt}{attr(x, 'ticks')} tick{?s}, {seen_txt}{nrow(x)} row{?s}{note}"
  )
  print(tibble::as_tibble(x), ...)
  invisible(x)
}
