# Network mutation and neighbourhood aggregates --------------------------

new_abm_link <- function(when, drop) {
  structure(list(when = when, drop = drop),
            class = c(if (drop) "abm_unlink" else "abm_link", "abm_step"))
}

#' Add edges between matched agents
#'
#' `abm_link()` turns the pairing produced by the preceding [abm_match()] into
#' permanent edges. It is how a network grows during a run without anyone being
#' born, random-graph percolation, tie formation, coalition building.
#'
#' An edge is added once per matched pair, and pairs that are already connected
#' are left alone, so the network never gains a duplicate edge.
#'
#' After a match with `size > 2` the group is linked as a *clique*, every pair
#' inside it gains an edge. That is what a team, a committee or a coalition
#' means once it is written as a network.
#'
#' @param when Optional condition. Only pairs where it holds are linked. It can
#'   use the agent's own columns, `partner_<col>`, `.role`, and any global.
#'
#' @return An `abm_link` step object.
#' @seealso [abm_go()], which lists every step and fixes the order they run
#'   in.
#' @family network topology steps
#' @export
#' @examples
#' # a random graph that grows one batch of edges per tick
#' abm_go(
#'   abm_match(pair = "random", eligible = runif(n()) < 0.05),
#'   abm_link()
#' )
abm_link <- function(when = NULL) {
  new_abm_link(enquo_or_null(rlang::enquo(when)), drop = FALSE)
}

#' Remove edges between matched agents
#'
#' `abm_unlink()` is the mirror of [abm_link()]: it deletes the edge joining
#' each matched pair. Paired with `abm_match(pair = "network")` it detaches an
#' agent from one of its neighbours, which, followed by a match and an
#' [abm_link()], is how you rewire a network.
#'
#' @param when Optional condition. Only pairs where it holds are unlinked.
#'
#' @return An `abm_unlink` step object.
#' @seealso [abm_go()], which lists every step and fixes the order they run
#'   in.
#' @family network topology steps
#' @export
#' @examples
#' # Watts-Strogatz rewiring: drop a neighbour, pick up a stranger
#' abm_go(
#'   abm_match(pair = "network", eligible = runif(n()) < 0.1),
#'   abm_unlink(),
#'   abm_match(pair = "random", eligible = runif(n()) < 0.1),
#'   abm_link()
#' )
abm_unlink <- function(when = NULL) {
  new_abm_link(enquo_or_null(rlang::enquo(when)), drop = TRUE)
}

#' Which matched pairs pass this step's condition?
#' @noRd
linked_pairs <- function(step, state) {
  if (is.null(state$match)) {
    abm_abort(
      c("{.fn {if (step$drop) 'abm_unlink' else 'abm_link'}} needs a standing pairing.",
        "i" = "Put an {.fn abm_match} before it."),
      class = "tidyABM_no_match"
    )
  }
  # a match that ran and paired nobody is a quiet no-op, not an error
  if (nrow(state$match$match) == 0L) {
    return(tibble::tibble(from = integer(), to = integer()))
  }
  combined <- bind_groups(state$groups)
  aug <- augment_group(combined, state$match, combined)
  keep <- eval_condition(step$when, aug, state$globals)

  if (state$match$size == 2L) {
    keep <- keep & !is.na(aug$.partner)
    pairs <- tibble::tibble(from = aug$.id[keep], to = aug$.partner[keep])
  } else {
    # a group of three or more is linked as a clique: every pair inside it.
    # That is what a team, a committee or a coalition means as a network.
    keep <- keep & !is.na(aug$.group_id)
    ids <- split(aug$.id[keep], aug$.group_id[keep])
    ids <- ids[lengths(ids) > 1L]
    pairs <- dplyr::bind_rows(lapply(ids, function(g) {
      cb <- utils::combn(g, 2L)
      tibble::tibble(from = as.integer(cb[1, ]), to = as.integer(cb[2, ]))
    }))
    if (nrow(pairs) == 0L) return(tibble::tibble(from = integer(), to = integer()))
  }
  # each pair may appear twice; keep one copy, oriented consistently
  out <- tibble::tibble(from = pmin(pairs$from, pairs$to),
                        to   = pmax(pairs$from, pairs$to))
  dplyr::distinct(out)
}

#' @noRd
run_link <- function(step, state) {
  if (is.null(state$edges)) {
    abm_abort(
      c("{.fn abm_link} needs a network to add edges to.",
        "i" = 'Start one with {.code abm_network(type = "empty")}.'),
      class = "tidyABM_no_network"
    )
  }
  new <- linked_pairs(step, state)
  if (nrow(new) == 0L) return(state)
  existing <- tibble::tibble(from = pmin(state$edges$from, state$edges$to),
                             to   = pmax(state$edges$from, state$edges$to))
  new <- dplyr::anti_join(new, existing, by = c("from", "to"))
  state$edges <- dplyr::bind_rows(state$edges, new)
  state
}

#' @noRd
run_unlink <- function(step, state) {
  if (is.null(state$edges) || nrow(state$edges) == 0L) return(state)
  drop <- linked_pairs(step, state)
  if (nrow(drop) == 0L) return(state)
  key <- function(a, b) paste(pmin(a, b), pmax(a, b))
  state$edges <- state$edges[
    !key(state$edges$from, state$edges$to) %in% key(drop$from, drop$to), ,
    drop = FALSE]
  state
}

# Neighbourhood aggregates ------------------------------------------------

#' Summarise each agent's neighbourhood
#'
#' A match gives an agent *one* partner. Plenty of models need the whole
#' neighbourhood instead, how many of my neighbours are infected, what fraction
#' of them are flashing, what my neighbours believe on average.
#' `abm_neighbours()` writes exactly that: for every agent, an aggregate over
#' the agents around it.
#'
#' Each rule is `column ~ aggregate_expression`, and the expression is
#' evaluated over the neighbours' rows, so `sum(infected)` means "how many of
#' my neighbours are infected" and `mean(opinion)` means "what my neighbours
#' think on average". An agent with no neighbours gets `NA`.
#'
#' Alongside each neighbour column the expression also sees `own_<col>`, the
#' focal agent's own value of that column, recycled down its neighbourhood.
#' That is what makes a *comparison* possible, `sum(wealth > own_wealth)` is
#' "how many of my neighbours are richer than me", which no aggregate over the
#' neighbours alone can express.
#'
#' # Two kinds of neighbourhood
#'
#' By default the neighbourhood is the model's [abm_network()]: the agents this
#' one shares an edge with. `within =` replaces it with a neighbourhood in
#' **attribute space**, everybody whose columns satisfy a condition, whether or
#' not the model has a network at all. The condition is evaluated once per
#' (focal, candidate) pair, with the candidate's columns under their own names
#' and the focal agent's under `own_<col>`, which is the same view
#' `abm_match(cost =)` minimises over.
#'
#' ``` abm_neighbours(opinion ~ mean(opinion), within = abs(opinion -
#' own_opinion) <= eps) ```
#'
#' is Hegselmann–Krause's confidence set, and it is a step rather than a
#' hand-rolled `vapply()` over the population.
#'
#' The two differ in one respect beyond how the neighbourhood is found: an
#' agent is **part of its own** attribute neighbourhood whenever the condition
#' holds of it, because "the mean opinion of everyone I take seriously"
#' includes the agent's own. It is never part of its network neighbourhood,
#' because an agent is not joined to itself. Write `within = ... & .id !=
#' own_.id` to exclude it.
#'
#' They also differ in cost. The network form does work proportional to the
#' number of edges; `within =` builds every (focal, candidate) pair and then
#' filters, so it is quadratic in the population. That is the same order as the
#' `vapply()` it replaces, with a tibble's constant factor on top, and it is
#' worth knowing before reaching for it on a very large population.
#'
#' @param ... One or more `column ~ aggregate_expression` rules. The expression
#'   sees the neighbours' agent columns, the focal agent's own columns as
#'   `own_<col>`, any column [abm_draw()] attached to the edge, and any global.
#' # One named lattice neighbour
#'
#' On a grid or line [abm_network()] the two neighbours either side of a cell
#' are not always interchangeable: a 1-D cellular automaton reads an *ordered*
#' triple, and "the cell to my north" is a different question from "my
#' neighbourhood". `.where` restricts the aggregate to the single neighbour in
#' a named lattice direction, so the aggregate runs over a one-row set and both
#' `col ~ s` and `col ~ sum(s)` yield that neighbour's value. A missing
#' neighbour, at a bounded edge, yields `NA`.
#'
#' ```r
#' abm_neighbours(s_w ~ sum(s), .where = "west"),
#' abm_neighbours(s_e ~ sum(s), .where = "east"),
#' abm_rules(s ~ rule[[4 * s_w + 2 * s + s_e + 1]])
#' ```
#'
#' @param within Optional condition defining a neighbourhood in attribute space
#'   rather than in the network. Evaluated once per (focal, candidate) pair, with
#'   the candidate's columns under their own names and the focal agent's under
#'   `own_<col>`. When it is supplied the model needs no network.
#'
#'   A condition of the shape `<col> == own_<col>` (optionally `&`-ed with
#'   more) is recognised and resolved as a hash join rather than by building
#'   every pair, so the co-location lookup
#'   `within = .group == "patches" & .id == own_.cell` -- "the cell I am
#'   standing on" -- is linear in the population rather than quadratic.
#' @param .where Optional lattice direction restricting the neighbourhood to a
#'   single neighbour: `"north"`, `"south"`, `"east"` or `"west"` on a grid;
#'   `"left"` / `"right"` (or `"west"` / `"east"`) on a line. Needs a lattice,
#'   and cannot be combined with `within`.
#'
#' @return An `abm_neighbours` step object.
#' @seealso [abm_go()], which lists every step and fixes the order they run
#'   in.
#' @family agent update steps
#' @export
#' @examples
#' abm_neighbours(infected_neighbours ~ sum(state == "infected"))
#' abm_neighbours(richer_neighbours ~ sum(wealth > own_wealth))
#'
#' # a neighbourhood in opinion space rather than in a network
#' abm_neighbours(opinion ~ mean(opinion), within = abs(opinion - own_opinion) <= 0.2)
#'
#' # the one cell to the west, on a lattice
#' abm_neighbours(s_w ~ sum(s), .where = "west")
#'
#' # what is on the cell I am standing on
#' abm_neighbours(grass_here ~ any(grass),
#'                within = .group == "patches" & .id == own_.cell)
abm_neighbours <- function(..., within = NULL, .where = NULL) {
  step <- new_rule_step(collect_rules(rlang::list2(...), "abm_neighbours"),
                        "abm_neighbours")
  step$within <- enquo_or_null(rlang::enquo(within))
  step$where <- .where
  if (!is.null(.where)) {
    if (!rlang::is_string(.where)) {
      abm_abort("{.arg .where} must be a single direction name.",
                class = "tidyABM_bad_where")
    }
    if (!is.null(step$within)) {
      abm_abort(
        c("{.arg .where} and {.arg within} are different neighbourhoods.",
          "i" = "{.arg .where} names one lattice neighbour; {.arg within} is a condition."),
        class = "tidyABM_conflicting_args"
      )
    }
  }
  step
}

#' One row per (focal, candidate) pair, carrying both sides' columns
#'
#' The candidate's columns keep their own names and the focal agent's are
#' prefixed `own_`. `.of` names the focal agent. This is the view
#' `abm_match(cost =)` minimises over and the one `abm_neighbours()` aggregates
#' over, so a comparison written for one means the same thing in the other.
#' @noRd
pair_view <- function(combined, focal_idx, cand_idx) {
  cols <- names(combined)
  view <- combined[cand_idx, cols, drop = FALSE]
  own <- combined[focal_idx, cols, drop = FALSE]
  names(own) <- paste0("own_", cols)
  view <- dplyr::bind_cols(view, own)
  view$.of <- combined$.id[focal_idx]
  view
}

#' Evaluate one quosure against a pair view, with the globals in scope
#' @noRd
eval_over_view <- function(quo, view, globals) {
  env <- rlang::quo_get_env(quo)
  if (length(globals)) env <- rlang::new_environment(globals, parent = env)
  quo <- rlang::quo_set_env(quo, env)
  dplyr::pull(dplyr::mutate(view, .abm_value = !!quo), ".abm_value")
}

#' The (focal, neighbour) view for a network neighbourhood
#' @noRd
network_view <- function(combined, state) {
  nb <- neighbour_table(state$edges)
  keep <- nb$.id %in% combined$.id & nb$.neighbour %in% combined$.id
  nb <- nb[keep, , drop = FALSE]
  view <- pair_view(combined, match(nb$.id, combined$.id),
                    match(nb$.neighbour, combined$.id))
  attach_edge_columns(view, state$edges, nb)
}

#' The (focal, candidate) view for a neighbourhood in attribute space
#' @noRd
attribute_view <- function(step, combined, globals) {
  n <- nrow(combined)
  ci <- rep(seq_len(n), times = n)
  si <- rep(seq_len(n), each = n)
  view <- pair_view(combined, si, ci)
  keep <- eval_over_view(step$within, view, globals)
  if (!is.logical(keep)) {
    abm_abort(
      c("{.arg within} must be a condition.",
        "x" = "{.code {deparse1(rlang::quo_get_expr(step$within))}} returned {.cls {class(keep)[[1]]}}."),
      class = "tidyABM_bad_within"
    )
  }
  if (length(keep) == 1L) keep <- rep(keep, nrow(view))
  keep[is.na(keep)] <- FALSE
  view[keep, , drop = FALSE]
}

#' @noRd
run_neighbours <- function(step, state) {
  combined <- bind_groups(state$groups)
  if (nrow(combined) == 0L) return(state)

  if (!is.null(step$where)) {
    # L1: the single lattice neighbour in a named direction
    view <- directional_view(step, combined, state)
  } else if (!is.null(step$within)) {
    # a `<col> == own_<col>` condition is a join, not a cross product
    view <- equijoin_view(step, combined, state$globals) %||%
      attribute_view(step, combined, state$globals)
  } else {
    if (is.null(state$edges)) {
      abm_abort(
        c("{.fn abm_neighbours} needs a network.",
          "i" = "Add one with {.code abm_setup(..., network = abm_network(...))}.",
          "i" = "Or give it a {.arg within} condition, which needs no network."),
        class = "tidyABM_no_network"
      )
    }
    view <- network_view(combined, state)
  }

  for (r in step$rules) {
    env <- rlang::quo_get_env(r$quo)
    if (length(state$globals)) {
      env <- rlang::new_environment(state$globals, parent = env)
    }
    quo <- rlang::quo_set_env(r$quo, env)
    agg <- dplyr::summarise(dplyr::group_by(view, .data$.of),
                            .abm_value = !!quo, .groups = "drop")
    for (nm in names(state$groups)) {
      g <- state$groups[[nm]]
      if (nrow(g) == 0L) next
      value <- agg$.abm_value[match(g$.id, agg$.of)]
      g[[r$target]] <- value
      state$groups[[nm]] <- g
    }
  }
  state
}

#' @export
print.abm_link <- function(x, ...) {
  cli::cli_text("{.cls abm_link}")
  if (!is.null(x$when)) {
    cli::cli_bullets(c("*" = "when = {.code {deparse1(rlang::quo_get_expr(x$when))}}"))
  }
  invisible(x)
}

#' @export
print.abm_unlink <- function(x, ...) {
  cli::cli_text("{.cls abm_unlink}")
  if (!is.null(x$when)) {
    cli::cli_bullets(c("*" = "when = {.code {deparse1(rlang::quo_get_expr(x$when))}}"))
  }
  invisible(x)
}

#' @export
print.abm_neighbours <- function(x, ...) print_rule_step(x, "abm_neighbours")
