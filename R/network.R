# Networks ---------------------------------------------------------------

new_abm_network <- function(type, degree, edges) {
  structure(list(type = type, degree = degree, edges = edges),
            class = "abm_network")
}

#' Declare a persistent network between agents
#'
#' A network is an edge list that lives alongside the agent tibble for the whole
#' run. It is built once at [abm_setup()] time and is read-only thereafter, with
#' one exception: [abm_birth()]'s `attach_via` argument can append one edge per
#' newborn agent, and [abm_death()] prunes the edges of agents that are removed.
#'
#' `type = "random"` builds a `degree`-regular random graph with
#' [igraph::sample_k_regular()], so every agent ends up with *exactly* `degree`
#' neighbours and every edge is symmetric. `degree = 1` therefore gives a fixed
#' one-to-one pairing of the whole population.
#'
#' `type = "complete"` connects every agent to every other one. That is the
#' well-mixed population written as a graph, and it is what lets
#' [abm_neighbours()] mean "over everybody else" in a model with no spatial or
#' social structure at all.
#'
#' The degree distribution is not a detail. `"random"` is *regular* — every
#' agent has exactly `degree` neighbours — and a threshold model on a regular
#' graph behaves quite differently from the same model on a graph with the same
#' mean degree but a spread of degrees, because the low-degree agents are the
#' ones a cascade can get started on. `"poisson"` is the Erdos-Renyi graph
#' `G(n, p)` with `p` chosen to give mean degree `degree`, and it is what the
#' random-graph literature means by "a random graph". `"scale_free"` grows a
#' Barabasi-Albert graph attaching `degree` edges per new agent, giving the
#' heavy tail. `"ring"` is the one-dimensional lattice, each agent joined to the
#' `degree / 2` agents on either side of it.
#'
#' @param type How the network is built. `"random"` for a `degree`-regular
#'   random graph, `"poisson"` for an Erdos-Renyi graph of mean degree `degree`,
#'   `"scale_free"` for a Barabasi-Albert graph, `"ring"` for a one-dimensional
#'   lattice, `"complete"` for every possible edge, `"manual"` to supply `edges`
#'   yourself, or `"empty"` for a network that starts with no edges (useful with
#'   [abm_birth()]).
#' @param degree Number of neighbours per agent — exactly, for `"random"` and
#'   `"ring"`; on average, for `"poisson"`; per newly attached agent, for
#'   `"scale_free"`. Required for all four. `n * degree` must be even for
#'   `"random"`, and `degree` must be even for `"ring"`.
#' @param edges A two-column data frame of `from`/`to` agent ids. Required for
#'   `type = "manual"`.
#'
#' @return An `abm_network` specification object.
#' @export
#' @examples
#' abm_network(type = "random", degree = 4)
#' abm_network(type = "poisson", degree = 3)
#' abm_network(type = "ring", degree = 2)
#' abm_network(type = "complete")
#' abm_network(type = "manual", edges = data.frame(from = 1, to = 2))
abm_network <- function(type = c("random", "poisson", "scale_free", "ring",
                                 "complete", "manual", "empty"),
                        degree = NULL, edges = NULL) {
  type <- rlang::arg_match(type)

  relevant <- list(random = "degree", poisson = "degree",
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

  if (type %in% c("random", "poisson", "scale_free", "ring")) {
    if (is.null(degree)) {
      abm_abort(
        '{.arg degree} is required when {.code type = "{type}"}.',
        class = "tidyABM_missing_arg"
      )
    }
    if (!rlang::is_scalar_integerish(degree) && type == "poisson") {
      if (!rlang::is_scalar_double(degree) || degree <= 0) {
        abm_abort("{.arg degree} must be a single positive number.",
                  class = "tidyABM_bad_degree")
      }
    } else if (!rlang::is_scalar_integerish(degree) || degree < 1) {
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
    edges <- tibble::tibble(from = as.integer(edges$from),
                            to   = as.integer(edges$to))
  }

  new_abm_network(type, degree, edges)
}

#' Turn a network spec into an edge tibble for `n` agents
#' @noRd
materialise_network <- function(spec, n, call = rlang::caller_env()) {
  if (is.null(spec)) return(NULL)

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
    return(tibble::tibble(.id = integer(), .neighbour = integer()))
  }
  tibble::tibble(
    .id        = c(edges$from, edges$to),
    .neighbour = c(edges$to,   edges$from)
  )
}

#' @export
print.abm_network <- function(x, ...) {
  cli::cli_text('{.cls abm_network} type {.val {x$type}}')
  if (!is.null(x$degree)) cli::cli_bullets(c("*" = "degree = {x$degree}"))
  if (!is.null(x$edges)) cli::cli_bullets(c("*" = "{nrow(x$edges)} edge{?s} supplied"))
  invisible(x)
}
