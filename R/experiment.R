# Parameters, replicates and measures -------------------------------------
#
# The experiment layer sits above the scheduler in `run.R`: it decides *which*
# runs happen and what is recorded about each, while `run_once()` still does
# exactly one run. Everything here is inert when `abm_run()` is called the way
# it always has been, so a single run takes the same path and returns the same
# columns it did before any of this existed.

#' Check `params` against the model's globals
#'
#' A parameter is a global. That is the whole rule, and the error says so,
#' because the first thing anyone tries is `n` or `degree` -- both of which are
#' fixed when the world is built and cannot be reached from here.
#' @noRd
check_params <- function(params, model, call = rlang::caller_env()) {
  if (is.null(params)) return(NULL)
  if (!is.list(params) || !length(params) || !rlang::is_named(params)) {
    abm_abort(
      c("{.arg params} must be a named list of values to override globals with.",
        "i" = "For example {.code params = list(d = c(0.1, 0.2, 0.3))}."),
      class = "tidyABM_bad_params", call = call
    )
  }
  unknown <- setdiff(names(params), names(model$globals))
  if (length(unknown)) {
    abm_abort(
      c("{.arg params} can only override values that are already globals.",
        "x" = "{.field {unknown}} {?is/are} not in the model's {.arg globals}.",
        "i" = "A structural parameter such as {.arg n}, {.arg degree} or {.arg dims} is
               fixed when {.fn abm_setup} is called, so sweeping one still needs a
               world per value."),
      class = "tidyABM_bad_params", call = call
    )
  }
  params
}

#' The candidate values a `params` entry names
#'
#' A bare vector sweeps, one run per element. A global that is itself a vector --
#' a payoff matrix, a table of prices -- is held fixed by wrapping it in a
#' `list()`, which is the same rule `global_row()` uses when it writes a
#' non-scalar global into the log.
#' @noRd
param_values <- function(x) if (is.list(x)) x else as.list(x)

#' The runs an experiment is made of
#'
#' One entry per run, in the order they are run: the parameter combinations in
#' `expand.grid()` order, each repeated `reps` times.
#' @noRd
param_grid <- function(params, reps) {
  combos <- if (is.null(params)) {
    list(stats::setNames(list(), character()))
  } else {
    vals <- lapply(params, param_values)
    idx <- expand.grid(lapply(vals, seq_along), KEEP.OUT.ATTRS = FALSE)
    lapply(seq_len(nrow(idx)), function(i) {
      Map(function(v, j) v[[j]], vals, as.integer(idx[i, ]))
    })
  }
  out <- vector("list", length(combos) * reps)
  k <- 0L
  for (ci in seq_along(combos)) {
    for (r in seq_len(reps)) {
      k <- k + 1L
      out[[k]] <- list(.run = k, .rep = r, values = combos[[ci]])
    }
  }
  out
}

#' A world with one run's parameters written into its globals
#' @noRd
apply_params <- function(model, values) {
  for (nm in names(values)) model$globals[[nm]] <- values[[nm]]
  model
}

#' One row identifying a run: `.run`, `.rep` and the parameters it used
#' @noRd
run_meta <- function(spec) {
  # a parameter may be a vector -- the same reason `global_row()` list-wraps a
  # non-scalar global, and the same treatment
  vals <- lapply(spec$values, function(v) {
    if (is.null(dim(v)) && length(v) == 1L) v else list(v)
  })
  meta <- tibble::tibble(.run = spec$.run, .rep = spec$.rep)
  if (length(vals)) meta <- dplyr::bind_cols(meta, tibble::as_tibble(vals))
  meta
}

#' Per-run seeds derived from the experiment seed
#'
#' A single run uses `seed` as given, so every model written before this argument
#' existed returns the number it always did. More than one run derives a seed per
#' run from it, so the experiment reproduces as a unit rather than each run
#' needing its own bookkeeping.
#' @noRd
run_seeds <- function(seed, n) {
  if (is.null(seed)) return(vector("list", n))
  if (n == 1L) return(list(seed))
  old <- if (exists(".Random.seed", .GlobalEnv)) get(".Random.seed", .GlobalEnv) else NULL
  set.seed(seed)
  s <- sample.int(.Machine$integer.max, n)
  if (!is.null(old)) {
    assign(".Random.seed", old, envir = .GlobalEnv)
  } else if (exists(".Random.seed", .GlobalEnv)) {
    rm(".Random.seed", envir = .GlobalEnv)
  }
  as.list(s)
}

#' Check `measures` and return the quosures to evaluate
#' @noRd
check_measures <- function(measures, call = rlang::caller_env()) {
  if (is.null(measures)) return(NULL)
  if (!is.list(measures) || !length(measures) || !rlang::is_named(measures)) {
    abm_abort(
      c("{.arg measures} must be a named list of one-sided formulas.",
        "i" = "For example {.code measures = list(mean_x = ~mean(x))}."),
      class = "tidyABM_bad_measures", call = call
    )
  }
  bad <- names(measures)[!vapply(measures, is_formula1, logical(1))]
  if (length(bad)) {
    abm_abort(
      c("Every entry of {.arg measures} must be a one-sided formula.",
        "x" = "{.field {bad}} {?is/are} not.",
        "i" = "A measure has no left-hand side: it is named by the list, as in
               {.code list(gini = ~gini(money))}."),
      class = "tidyABM_bad_measures", call = call
    )
  }
  taken <- intersect(names(measures), c("tick", ".run", ".rep"))
  if (length(taken)) {
    abm_abort(
      c("A measure cannot be named {.field {taken}}.",
        "i" = "Those name the run itself in the table {.fn abm_measures} returns."),
      class = "tidyABM_bad_measures", call = call
    )
  }
  lapply(measures, rlang::as_quosure)
}

#' One tick's measures, as a single row
#'
#' Evaluated through the same mask an [abm_global()] right-hand side is, so
#' `n()` means the population size and the globals are in scope. The result is
#' written to the log and nowhere else: no step can read a measure back, which
#' is the whole reason it is not an `abm_global()`.
#' @noRd
measure_row <- function(quos, state, t) {
  combined <- bind_groups(state$groups)
  vals <- lapply(names(quos), function(nm) {
    quo <- rlang::quo_set_env(
      quos[[nm]],
      rlang::new_environment(state$globals, parent = rlang::quo_get_env(quos[[nm]]))
    )
    val <- dplyr::pull(dplyr::reframe(combined, .abm_value = !!quo), ".abm_value")
    if (length(val) != 1L) {
      abm_abort(
        c("A measure must collapse to a single value.",
          "x" = "{.code {nm} = ~{deparse1(rlang::quo_get_expr(quos[[nm]]))}} returned {length(val)} values at tick {t}.",
          "i" = "Wrap it in an aggregate such as {.fn sum} or {.fn mean}."),
        class = "tidyABM_bad_measures"
      )
    }
    if (is.null(dim(val)) && length(val) == 1L) val else list(val)
  })
  names(vals) <- names(quos)
  dplyr::bind_cols(tibble::tibble(tick = t), tibble::as_tibble(vals))
}

#' Prefix a run's identifying columns onto one of its tables
#'
#' A parameter is a global, so the table [abm_globals()] returns already has a
#' column for it, holding the value the run actually used and any later change
#' an `abm_global()` step made to it. Prefixing a second copy would say the same
#' thing worse, so a meta column the table already has is dropped and `.run` is
#' left to tell the runs apart.
#' @noRd
with_meta <- function(x, meta) {
  if (is.null(x)) return(NULL)
  meta <- meta[, !names(meta) %in% names(x), drop = FALSE]
  if (!ncol(meta)) return(x)
  if (!nrow(x)) return(dplyr::bind_cols(meta[0, , drop = FALSE], x))
  dplyr::bind_cols(meta[rep(1L, nrow(x)), , drop = FALSE], x)
}

#' Measures recorded during a run
#'
#' The table `measures =` built: one row per tick of every run, with the run's
#' identifying columns in front of it when there was more than one run.
#'
#' Measures are observations rather than model state, which is what separates
#' them from [abm_globals()]. A global is readable by every rule in the model; a
#' measure is written to this table and is invisible to the model that produced
#' it.
#'
#' @param x An `abm_result` from [abm_run()].
#' @return A tibble with one row per tick per run, or `NULL` if the run was made
#'   without `measures`.
#' @export
#' @examples
#' m  <- abm_setup(agents = abm_agents(n = 20, x = ~runif(20)))
#' go <- abm_go(abm_rules(x ~ x * 0.9))
#' r  <- abm_run(m, go, ticks = 5, seed = 1,
#'               measures = list(total = ~sum(x), spread = ~sd(x)))
#' abm_measures(r)
abm_measures <- function(x) {
  if (!inherits(x, "abm_result")) {
    abm_abort("{.arg x} must be the result of {.fn abm_run}.",
              class = "tidyABM_bad_result")
  }
  attr(x, "measures")
}
