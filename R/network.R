# Networks ---------------------------------------------------------------

new_abm_network <- function(type, degree, edges, dims = NULL, diagonals = NULL,
                            torus = NULL, on = NULL) {
  structure(list(type = type, degree = degree, edges = edges, dims = dims,
                 diagonals = diagonals, torus = torus, on = on),
            class = "abm_network")
}

#' Declare a persistent network between agents
#'
#' A network is an edge list that lives alongside the agent tibble for the
#' whole run. It is built once at [abm_setup()] time and is read-only
#' thereafter, with one exception: [abm_birth()]'s `attach_via` argument can
#' append one edge per newborn agent, and [abm_death()] prunes the edges of
#' agents that are removed.
#'
#' `type = "regular"` (or its alias `"random"`) builds a `degree`-regular random
#' graph with [igraph::sample_k_regular()], so every agent ends up with
#' *exactly* `degree` neighbours and every edge is symmetric. `degree = 1`
#' therefore gives a fixed one-to-one pairing of the whole population.
#'
#' `type = "complete"` connects every agent to every other one. That is the
#' well-mixed population written as a graph, and it is what lets
#' [abm_neighbours()] mean "over everybody else" in a model with no spatial or
#' social structure at all.
#'
#' The degree distribution is not a detail. `"regular"` (`"random"`) is *regular*,
#' every agent has exactly `degree` neighbours, and a threshold model on a
#' regular graph behaves quite differently from the same model on a graph with
#' the same mean degree but a spread of degrees, because the low-degree agents
#' are the ones a cascade can get started on. `"poisson"` is the Erdos-Renyi
#' graph `G(n, p)` with `p` chosen to give mean degree `degree`, and it is what
#' the random-graph literature means by "a random graph". `"scale_free"` grows a
#' Barabasi-Albert graph attaching `degree` edges per new agent, giving the
#' heavy tail. `"ring"` is the one-dimensional lattice, each agent joined to
#' the `degree / 2` agents on either side of it.
#'
#' # Spatial grids
#'
#' Spatial geometry now lives in [abm_space()] (`space = abm_space(type = "grid", ...)`
#' or `type = "continuous"`), which also lets a model combine geography and a
#' social network at the same time. `type = "grid"` and `type = "line"` remain
#' supported on `abm_network()` as backward-compatible aliases.
#'
#' @param type How the network is built. `"regular"` (or `"random"`) for a
#'   `degree`-regular random graph, `"poisson"` for an Erdos-Renyi graph of mean
#'   degree `degree`, `"scale_free"` for a Barabasi-Albert graph, `"ring"` for a
#'   one-dimensional ring, `"complete"` for every possible edge, `"manual"` to
#'   supply `edges` yourself, or `"empty"` for a network that starts with no
#'   edges (useful with [abm_birth()]). `"grid"` and `"line"` are deprecated
#'   aliases for [abm_space()].
#' @param degree Number of neighbours per agent, exactly, for `"regular"`
#'   (`"random"`) and `"ring"`; on average, for `"poisson"`; per newly attached
#'   agent, for `"scale_free"`. Required for all four. `n * degree` must be even
#'   for `"regular"` / `"random"`, and `degree` must be even for `"ring"`.
#' @param edges A two-column data frame of `from`/`to` agent ids. Required for
#'   `type = "manual"`.
#' @param dims Deprecated lattice shape; prefer [abm_space()].
#' @param diagonals Deprecated lattice option; prefer [abm_space()].
#' @param torus Deprecated lattice option; prefer [abm_space()].
#' @param on Optional name of the agent group to wire. In multi-group models
#'   this restricts the network (or deprecated lattice) to that group; defaults
#'   to the whole population.
#'
#' @return An `abm_network` specification object.
#' @export
#' @examples
#' abm_network(type = "regular", degree = 4)
#' abm_network(type = "poisson", degree = 3)
#' abm_network(type = "ring", degree = 2)
#' abm_network(type = "complete")
#' abm_network(type = "manual", edges = data.frame(from = 1, to = 2))
#'
#' # For spatial grids and continuous worlds, use abm_space():
#' abm_space(type = "grid", dims = c(50, 50), diagonals = FALSE,
#'           torus = FALSE, on = "patches")
abm_network <- function(type = c("regular", "random", "poisson", "scale_free", "ring",
                                 "complete", "grid", "line", "manual", "empty"),
                        degree = NULL, edges = NULL, dims = NULL,
                        diagonals = NULL, torus = NULL, on = NULL) {
  type <- rlang::arg_match(type)

  # the lattice types are the spatial grammar's entry point; everything about
  # them lives in spatial-network.R
  if (type %in% c("grid", "line")) {
    irrelevant <- c(if (!is.null(degree)) "degree", if (!is.null(edges)) "edges")
    if (length(irrelevant)) {
      abm_abort(
        c('{.arg {irrelevant}} {?is/are} not used when {.code type = "{type}"}.',
          "i" = 'That type uses {.arg dims}, {.arg diagonals}, {.arg torus} and {.arg on}.'),
        class = "tidyABM_irrelevant_arg"
      )
    }
    return(lattice_network_spec(type, dims, diagonals, torus, on))
  }

  lattice_args <- c(if (!is.null(dims)) "dims",
                    if (!is.null(diagonals)) "diagonals",
                    if (!is.null(torus)) "torus")
  if (length(lattice_args)) {
    abm_abort(
      c('{.arg {lattice_args}} {?is/are} only used by {.code type = "grid"} and {.code type = "line"}.',
        "x" = 'Got {.code type = "{type}"}.'),
      class = "tidyABM_irrelevant_arg"
    )
  }
  if (!is.null(on) && !rlang::is_string(on)) {
    abm_abort("{.arg on} must be a single group name.",
              class = "tidyABM_bad_arg")
  }

  relevant <- list(regular = "degree", random = "degree", poisson = "degree",
                   scale_free = "degree", ring = "degree",
                   complete = character(), manual = "edges",
                   empty = character())
  supplied <- c(if (!is.null(degree)) "degree", if (!is.null(edges)) "edges")
  irrelevant <- setdiff(supplied, relevant[[type]])
  if (length(irrelevant)) {
    abm_abort(
      c('{.arg {irrelevant}} {?is/are} not used when {.code type = "{type}"}.',
        "i" = if (length(relevant[[type]]))
          'That type uses {.arg {relevant[[type]]}}.'
        else 'That type uses neither {.arg degree} nor {.arg edges}.'),
      class = "tidyABM_irrelevant_arg"
    )
  }

  if (type %in% c("regular", "random", "poisson", "scale_free", "ring")) {
    if (is.null(degree)) {
      abm_abort(
        '{.arg degree} is required when {.code type = "{type}"}.',
        class = "tidyABM_missing_arg"
      )
    }
    if (!rlang::is_scalar_integerish(degree) && type == "poisson") {
      if (!rlang::is_scalar_double(degree) || is.na(degree) || degree <= 0) {
        abm_abort("{.arg degree} must be a single positive number.",
                  class = "tidyABM_bad_degree")
      }
    } else if (!rlang::is_scalar_integerish(degree) || is.na(degree) ||
               degree < 1) {
      abm_abort("{.arg degree} must be a single positive whole number.",
                class = "tidyABM_bad_degree")
    }
    if (type == "ring" && degree %% 2 != 0) {
      abm_abort(
        c('A ring lattice is symmetric, so {.arg degree} must be even.',
          "x" = "Got {.code degree = {degree}}."),
        class = "tidyABM_bad_degree"
      )
    }
  }

  if (type == "manual") {
    if (is.null(edges)) {
      abm_abort('{.arg edges} is required when {.code type = "manual"}.',
                class = "tidyABM_missing_arg")
    }
    if (!is.data.frame(edges) || !all(c("from", "to") %in% names(edges))) {
      abm_abort(
        "{.arg edges} must be a data frame with {.field from} and {.field to} columns.",
        class = "tidyABM_bad_edges"
      )
    }
    extra <- setdiff(names(edges), c("from", "to"))
    if (length(extra)) {
      abm_abort(
        c("{.arg edges} {?has a column/has columns} {.field {extra}} that {?is/are} not used.",
          "i" = "An edge is {.field from} and {.field to}. A value drawn fresh each tick goes on the network with {.fn abm_draw}; a value that belongs to the pair and persists is a relation: {.fn abm_relation}.",
          "i" = "Keep only the two: {.code edges[c(\"from\", \"to\")]}."),
        class = "tidyABM_bad_edges"
      )
    }
    ids <- c(edges$from, edges$to)
    if (!is.numeric(ids) || anyNA(ids) || any(ids < 1 | ids != round(ids))) {
      abm_abort(
        c("{.arg edges} must hold agent ids: whole numbers from 1 up, with no {.val NA}.",
          "x" = "{.field from} or {.field to} holds something else."),
        class = "tidyABM_bad_edges"
      )
    }
    edges <- tibble::tibble(from = as.integer(edges$from),
                            to   = as.integer(edges$to))
  }

  new_abm_network(type, degree, edges, on = on)
}

#' Turn a network spec into an edge tibble for `n` agents
#' @noRd
materialise_network <- function(spec, n, groups = NULL, call = rlang::caller_env()) {
  if (is.null(spec)) return(NULL)
  if (!is.null(spec$on) && !is.null(groups)) {
    if (!spec$on %in% names(groups)) {
      abm_abort(
        c("{.arg on} names a group that is not in {.arg agents}.",
          "x" = "No group {.field {spec$on}}.",
          "i" = "Groups: {.field {names(groups)}}."),
        class = "tidyABM_bad_arg", call = call
      )
    }
    ids <- groups[[spec$on]]$.id
    sub_spec <- spec
    sub_spec$on <- NULL
    e <- materialise_network(sub_spec, n = length(ids), groups = NULL, call = call)
    if (nrow(e)) {
      e$from <- ids[e$from]
      e$to <- ids[e$to]
    }
    return(e)
  }

  if (spec$type == "empty") {
    return(tibble::tibble(from = integer(), to = integer()))
  }
  if (spec$type == "complete") {
    if (n < 2L) return(tibble::tibble(from = integer(), to = integer()))
    pairs <- utils::combn(n, 2L)
    return(tibble::tibble(from = as.integer(pairs[1, ]),
                          to   = as.integer(pairs[2, ])))
  }
  if (spec$type == "manual") {
    if (any(spec$edges$from > n | spec$edges$to > n)) {
      abm_abort(
        c("{.arg edges} refers to agents that do not exist.",
          "i" = "There {?is/are} {n} agent{?s}."),
        class = "tidyABM_bad_edges", call = call
      )
    }
    return(spec$edges)
  }

  if (spec$type == "poisson") {
    # G(n, p) with p set so that the mean degree is `degree`
    if (n < 2L) return(tibble::tibble(from = integer(), to = integer()))
    p <- spec$degree / (n - 1)
    if (p > 1) {
      abm_abort(
        c("Mean {.arg degree} cannot exceed {.code n - 1}.",
          "x" = "Got {.code degree = {spec$degree}} with {n} agent{?s}."),
        class = "tidyABM_bad_degree", call = call
      )
    }
    g <- igraph::sample_gnp(n = n, p = p, directed = FALSE)
    return(edge_tibble(g))
  }

  if (spec$type == "scale_free") {
    if (n < 2L) return(tibble::tibble(from = integer(), to = integer()))
    g <- igraph::sample_pa(n = n, m = spec$degree, directed = FALSE)
    return(edge_tibble(g))
  }

  if (spec$type == "ring") {
    if (n < 3L) return(tibble::tibble(from = integer(), to = integer()))
    half <- spec$degree %/% 2L
    if (2L * half >= n) {
      abm_abort(
        c("{.arg degree} must be smaller than the number of agents.",
          "x" = "Got {.code degree = {spec$degree}} with {n} agent{?s}."),
        class = "tidyABM_bad_degree", call = call
      )
    }
    a <- rep(seq_len(n), each = half)
    step <- rep(seq_len(half), times = n)
    b <- ((a + step - 1L) %% n) + 1L
    return(dplyr::distinct(tibble::tibble(
      from = as.integer(pmin(a, b)),
      to   = as.integer(pmax(a, b))
    )))
  }

  # type == "random"
  if (spec$degree >= n) {
    abm_abort(
      c("{.arg degree} must be smaller than the number of agents.",
        "x" = "Got {.code degree = {spec$degree}} with {n} agent{?s}."),
      class = "tidyABM_bad_degree", call = call
    )
  }
  if ((n * spec$degree) %% 2 != 0) {
    abm_abort(
      c("A {spec$degree}-regular graph on {n} agents does not exist.",
        "i" = "{.code n * degree} must be even."),
      class = "tidyABM_bad_degree", call = call
    )
  }
  g <- igraph::sample_k_regular(no.of.nodes = n, k = spec$degree)
  edge_tibble(g)
}

#' An igraph object as a from/to tibble
#' @noRd
edge_tibble <- function(g) {
  el <- igraph::as_edgelist(g)
  if (nrow(el) == 0L) return(tibble::tibble(from = integer(), to = integer()))
  tibble::tibble(from = as.integer(el[, 1]), to = as.integer(el[, 2]))
}

#' Neighbours of each agent, as a two-column tibble (agent, neighbour)
#' @noRd
neighbour_table <- function(edges) {
  if (is.null(edges) || nrow(edges) == 0L) {
    return(tibble::new_tibble(list(.id = integer(), .neighbour = integer()),
                              nrow = 0L))
  }
  # `.edge` and `.forward` say which edge this row came from and which way round
  # it is being read, so that a value attached to the edge by [abm_draw()] can be
  # handed to whichever endpoint is looking at it.
  n <- nrow(edges)
  tibble::new_tibble(
    list(
      .id        = c(edges$from, edges$to),
      .neighbour = c(edges$to,   edges$from),
      .edge      = c(seq_len(n), seq_len(n)),
      .forward   = c(rep(TRUE, n), rep(FALSE, n))
    ),
    nrow = 2L * n
  )
}

#' @export
print.abm_network <- function(x, ...) {
  cli::cli_text('{.cls abm_network} type {.val {x$type}}')
  if (!is.null(x$degree)) cli::cli_bullets(c("*" = "degree = {x$degree}"))
  if (!is.null(x$edges)) cli::cli_bullets(c("*" = "{nrow(x$edges)} edge{?s} supplied"))
  if (!is.null(x$dims)) {
    cli::cli_bullets(c("*" = "dims = {paste(x$dims, collapse = ' x ')} ({prod(x$dims)} cell{?s})"))
    if (!is.null(x$diagonals)) {
      cli::cli_bullets(c("*" = "diagonals = {x$diagonals}"))
    }
    cli::cli_bullets(c("*" = "torus = {x$torus}"))
    if (!is.null(x$on)) cli::cli_bullets(c("*" = "on = {.field {x$on}}"))
  }
  invisible(x)
}
