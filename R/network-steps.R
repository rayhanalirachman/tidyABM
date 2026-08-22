# Network mutation and neighbourhood aggregates --------------------------

new_abm_link <- function(when, drop) {
  structure(list(when = when, drop = drop),
            class = c(if (drop) "abm_unlink" else "abm_link", "abm_step"))
}

#' Add edges between matched agents
#'
#' `abm_link()` turns the pairing produced by the preceding [abm_match()] into
#' permanent edges. It is how a network grows during a run without anyone being
#' born — random-graph percolation, tie formation, coalition building.
#'
#' An edge is added once per matched pair, and pairs that are already connected
#' are left alone, so the network never gains a duplicate edge.
#'
#' After a match with `size > 2` the group is linked as a *clique* — every pair
#' inside it gains an edge. That is what a team, a committee or a coalition
#' means once it is written as a network.
#'
#' @param when Optional condition. Only pairs where it holds are linked. It can
#'   use the agent's own columns, `partner_<col>`, `.role`, and any global.
#'
#' @return An `abm_link` step object.
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
#' `abm_unlink()` is the mirror of [abm_link()]: it deletes the edge joining each
#' matched pair. Paired with `abm_match(pair = "network")` it detaches an agent
#' from one of its neighbours, which — followed by a match and an [abm_link()] —
#' is how you rewire a network.
#'
#' @param when Optional condition. Only pairs where it holds are unlinked.
#'
#' @return An `abm_unlink` step object.
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

#' Summarise each agent's network neighbours
#'
#' A match gives an agent *one* partner. Plenty of models need the whole
#' neighbourhood instead — how many of my neighbours are infected, what fraction
#' of them are flashing, what my neighbours believe on average.
#' `abm_neighbours()` writes exactly that: for every agent, an aggregate over the
#' agents it is connected to in the model's [abm_network()].
#'
#' Each rule is `column ~ aggregate_expression`, and the expression is evaluated
#' over the neighbours' rows, so `sum(infected)` means "how many of my neighbours
#' are infected" and `mean(opinion)` means "what my neighbours think on average".
#' An agent with no neighbours gets `NA`.
#'
#' Alongside each neighbour column the expression also sees `own_<col>`, the
#' focal agent's own value of that column, recycled down its neighbourhood. That
#' is what makes a *comparison* possible — `sum(wealth > own_wealth)` is "how many
#' of my neighbours are richer than me", which no aggregate over the neighbours
#' alone can express.
#'
#' @param ... One or more `column ~ aggregate_expression` rules. The expression
#'   sees the neighbours' agent columns, the focal agent's own columns as
#'   `own_<col>`, and any global.
#'
#' @return An `abm_neighbours` step object.
#' @export
#' @examples
#' abm_neighbours(infected_neighbours ~ sum(state == "infected"))
#' abm_neighbours(richer_neighbours ~ sum(wealth > own_wealth))
abm_neighbours <- function(...) {
  new_rule_step(collect_rules(rlang::list2(...), "abm_neighbours"),
                "abm_neighbours")
}

#' @noRd
run_neighbours <- function(step, state) {
  if (is.null(state$edges)) {
    abm_abort(
      c("{.fn abm_neighbours} needs a network.",
        "i" = "Add one with {.code abm_setup(..., network = abm_network(...))}."),
      class = "tidyABM_no_network"
    )
  }
  combined <- bind_groups(state$groups)
  nb <- neighbour_table(state$edges)
  nb <- nb[nb$.id %in% combined$.id & nb$.neighbour %in% combined$.id, ,
           drop = FALSE]

  # one row per (agent, neighbour), carrying the neighbour's columns and, as
  # `own_<col>`, the focal agent's own — so a rule can compare the two.
  cols <- setdiff(names(combined), c(".id", ".group"))
  idx <- match(nb$.neighbour, combined$.id)
  view <- combined[idx, cols, drop = FALSE]
  own <- combined[match(nb$.id, combined$.id), cols, drop = FALSE]
  names(own) <- paste0("own_", cols)
  view <- dplyr::bind_cols(view, own)
  view$.of <- nb$.id

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
