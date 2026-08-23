# Model setup ------------------------------------------------------------

new_abm_model <- function(groups, globals, edges, network_spec) {
  structure(
    list(groups = groups, globals = globals, edges = edges,
         network_spec = network_spec),
    class = "abm_model"
  )
}

#' Set up a model
#'
#' `abm_setup()` is the first of the three functions a model is made of --
#' `abm_setup()`, then [abm_go()], then [abm_run()]. It turns declarations into
#' an initial population: it evaluates the [abm_agents()] specifications into
#' tibbles, builds the [abm_network()], and stores the starting values of any
#' shared globals. The result is the `model` argument of [abm_run()].
#'
#' Agent ids (`.id`) are unique across the whole model, and every agent carries a
#' `.group` column naming the group it belongs to. A single-group model is given
#' the group name `"agents"`.
#'
#' @param agents Either one [abm_agents()] specification, or a *named* list of
#'   them for a model with several kinds of agent (for example
#'   `list(buyers = abm_agents(...), sellers = abm_agents(...))`).
#' @param network Optionally an [abm_network()] specification.
#' @param globals A named list of population-level values shared by every agent,
#'   for example `list(last_attendance = 60)`. Globals are readable inside every
#'   rule and are updated by [abm_global()].
#' @param seed Optional integer. Sets the random seed for setup only, so the
#'   starting population is reproducible independently of the run. Note that this
#'   and [abm_run()]'s `seed` do different jobs: this one fixes *who the agents
#'   are*, `abm_run()`'s fixes *what happens to them*. A model whose columns are
#'   drawn at random needs both to be reproducible end to end.
#'
#' @return An `abm_model` object.
#' @export
#' @examples
#' abm_setup(
#'   agents  = abm_agents(n = 100, threshold = ~runif(n, 40, 80)),
#'   globals = list(last_attendance = 60)
#' )
#'
#' # The world is the first of the three parts a model is made of.
#' economy <- abm_setup(agents = abm_agents(n = 500, money = 100))
#'
#' go <- abm_go(
#'   abm_match(pair = "random", role = list(giver = money > 0, receiver = TRUE)),
#'   abm_rules(money ~ if_else(.role == "giver", money - 1, money + 1))
#' )
#'
#' abm_run(economy, go, ticks = 10, seed = 1)
abm_setup <- function(agents, network = NULL, globals = list(), seed = NULL) {
  if (!is.null(seed)) {
    if (!rlang::is_scalar_integerish(seed)) {
      abm_abort("{.arg seed} must be a single whole number.",
                class = "tidyABM_bad_seed")
    }
    old <- if (exists(".Random.seed", .GlobalEnv)) get(".Random.seed", .GlobalEnv) else NULL
    set.seed(seed)
    on.exit({
      if (!is.null(old)) assign(".Random.seed", old, envir = .GlobalEnv)
    }, add = TRUE)
  }

  specs <- normalise_agent_specs(agents)

  if (!is.list(globals) || (length(globals) && !rlang::is_named(globals))) {
    abm_abort("{.arg globals} must be a named list.",
              class = "tidyABM_bad_globals")
  }

  groups <- list()
  offset <- 0L
  for (nm in names(specs)) {
    groups[[nm]] <- materialise_agents(specs[[nm]], nm, id_offset = offset)
    offset <- offset + specs[[nm]]$n
  }

  if (!is.null(network) && !inherits(network, "abm_network")) {
    abm_abort("{.arg network} must be built with {.fn abm_network}.",
              class = "tidyABM_bad_network")
  }
  edges <- materialise_network(network, n = offset)

  new_abm_model(groups, as.list(globals), edges, network)
}

#' Coerce the `agents` argument into a named list of specs
#' @noRd
normalise_agent_specs <- function(agents, call = rlang::caller_env()) {
  if (inherits(agents, "abm_agents")) {
    return(list(agents = agents))
  }
  if (is.list(agents) && length(agents) &&
      all(vapply(agents, inherits, logical(1), "abm_agents"))) {
    if (!rlang::is_named(agents)) {
      abm_abort(
        c("A multi-group {.arg agents} list must be named.",
          "i" = 'For example {.code list(buyers = abm_agents(...), sellers = abm_agents(...))}.'),
        class = "tidyABM_unnamed_group", call = call
      )
    }
    return(agents)
  }
  abm_abort(
    c("{.arg agents} must be an {.fn abm_agents} object, or a named list of them.",
      "x" = "Got {.cls {class(agents)[[1]]}}."),
    class = "tidyABM_bad_agents", call = call
  )
}

#' Total number of agents in a model
#' @export
#' @param x An `abm_model` or `abm_result` object.
#' @param ... Unused.
#' @return An integer.
n_agents <- function(x, ...) UseMethod("n_agents")

#' @export
n_agents.abm_model <- function(x, ...) {
  sum(vapply(x$groups, nrow, integer(1)))
}

#' @export
print.abm_model <- function(x, ...) {
  cli::cli_text("{.cls abm_model} {.strong {n_agents(x)}} agent{?s} in {length(x$groups)} group{?s}")
  for (nm in names(x$groups)) {
    g <- x$groups[[nm]]
    cols <- setdiff(names(g), c(".id", ".group"))
    cli::cli_bullets(c("*" = "{.field {nm}}: {nrow(g)} agent{?s} [{.val {cols}}]"))
  }
  if (length(x$globals)) {
    cli::cli_bullets(c("*" = "globals: {.val {names(x$globals)}}"))
  }
  if (!is.null(x$edges)) {
    cli::cli_bullets(c("*" = "network: {nrow(x$edges)} edge{?s}"))
  }
  invisible(x)
}
