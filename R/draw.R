# Values attached to edges ------------------------------------------------

new_abm_draw <- function(rules, each) {
  structure(list(rules = rules, each = each),
            class = c("abm_draw", "abm_step"))
}

#' Attach a value to every edge, visible from both ends
#'
#' Two [abm_neighbours()] passes over the same network draw their random numbers
#' independently, which is fine when each pass is a separate event and wrong when
#' it is the *same* event seen from two sides. "How many of my neighbours
#' punished me" and "how much punishing did I do" have to add up to the same
#' thing, agent by agent, and they cannot if each aggregate rolls its own dice.
#'
#' `abm_draw()` fixes that by putting the draw on the **edge** rather than in the
#' aggregate. Each rule is evaluated once per edge, and the value it produces is
#' then readable inside every later [abm_neighbours()] rule in the same tick,
#' from either endpoint, as a column of that name. Because both endpoints read
#' the same number, two aggregates over it describe the same events.
#'
#' The right-hand side is evaluated over the **edge table** — one row per edge,
#' with `from` and `to` — so `n()` is the number of edges and
#' `runif(n())` is "a uniform per edge". Globals are in scope. The values live
#' until the next `abm_draw()` of that name replaces them, so a draw made once
#' per tick lasts the tick and one made in `abm_setup()`'s first step lasts the
#' run.
#'
#' `.each` says who the value belongs to:
#'
#' * `"edge"` (the default) — one value for the edge. Both endpoints read the
#'   same number in the column of that name. This is a coin the *pair* tosses:
#'   whether they met, what the encounter was worth.
#' * `"endpoint"` — one value each. The focal agent reads its own in the column
#'   of that name and its neighbour's as `<name>_back`. This is a coin each of
#'   them tosses privately about the other, which is what an asymmetric
#'   interaction needs: whether I noticed *you*, and separately whether you
#'   noticed *me*.
#'
#' @param ... One or more `name ~ expression` rules, evaluated once per edge.
#' @param .each `"edge"` for one value per edge, shared by both endpoints;
#'   `"endpoint"` for one value per endpoint, read as `name` and `name_back`.
#'
#' @return An `abm_draw` step object.
#' @export
#' @examples
#' # did this pair meet this tick? Both of them agree on the answer.
#' abm_go(
#'   abm_draw(met ~ runif(n()) < 0.3),
#'   abm_neighbours(partners ~ sum(met))
#' )
#'
#' # whether I saw you, and whether you saw me, are two different coins
#' abm_draw(noticed ~ runif(n()), .each = "endpoint")
abm_draw <- function(..., .each = c("edge", "endpoint")) {
  .each <- rlang::arg_match(.each)
  new_abm_draw(collect_rules(rlang::list2(...), "abm_draw"), .each)
}

#' @noRd
run_draw <- function(step, state) {
  if (is.null(state$edges)) {
    abm_abort(
      c("{.fn abm_draw} needs a network to attach values to.",
        "i" = "Add one with {.code abm_setup(..., network = abm_network(...))}."),
      class = "tidyABM_no_network"
    )
  }
  agent_cols <- setdiff(model_columns(state$groups), c(".id", ".group"))
  for (r in step$rules) {
    if (r$target %in% agent_cols) {
      abm_abort(
        c("{.fn abm_draw} would shadow an agent column.",
          "x" = "{.field {r$target}} is already a column of the agents.",
          "i" = "Edge values and agent columns share one namespace inside {.fn abm_neighbours}."),
        class = "tidyABM_draw_collision"
      )
    }
    fwd <- edge_value(r, state)
    back <- if (step$each == "endpoint") edge_value(r, state) else fwd
    state$edges[[paste0(".draw_", r$target)]] <- fwd
    state$edges[[paste0(".back_", r$target)]] <- back
  }
  state
}

#' Evaluate one draw rule over the edge table
#' @noRd
edge_value <- function(r, state) {
  quo <- rlang::quo_set_env(
    r$quo,
    rlang::new_environment(state$globals, parent = rlang::quo_get_env(r$quo))
  )
  edges <- state$edges
  # the draw columns are internal; a rule sees `from`, `to` and the values it
  # can already read by name
  view <- edges[, !startsWith(names(edges), ".draw_") &
                  !startsWith(names(edges), ".back_"), drop = FALSE]
  val <- dplyr::pull(dplyr::mutate(view, .abm_value = !!quo), ".abm_value")
  if (length(val) == 1L) val <- rep(val, nrow(edges))
  if (length(val) != nrow(edges)) {
    abm_abort(
      c("{.fn abm_draw} rules must give one value per edge.",
        "x" = "{.code {r$target} ~ {deparse1(rlang::quo_get_expr(r$quo))}} returned {length(val)} for {nrow(edges)} edge{?s}."),
      class = "tidyABM_bad_draw"
    )
  }
  val
}

#' Which values has [abm_draw()] attached to the edges?
#' @noRd
drawn_names <- function(edges) {
  if (is.null(edges)) return(character())
  sub("^\\.draw_", "", grep("^\\.draw_", names(edges), value = TRUE))
}

#' Hand each side of an edge the values attached to it
#'
#' `nb` carries `.edge` and `.forward`, so an endpoint-scoped value can be
#' oriented: the focal agent reads its own under the plain name and its
#' neighbour's under `<name>_back`. An edge-scoped value is the same number both
#' ways round, which is the whole point of it.
#' @noRd
attach_edge_columns <- function(view, edges, nb) {
  for (nm in drawn_names(edges)) {
    fwd <- edges[[paste0(".draw_", nm)]][nb$.edge]
    bck <- edges[[paste0(".back_", nm)]][nb$.edge]
    mine <- fwd
    theirs <- bck
    flip <- !nb$.forward
    mine[flip] <- bck[flip]
    theirs[flip] <- fwd[flip]
    view[[nm]] <- mine
    view[[paste0(nm, "_back")]] <- theirs
  }
  view
}

#' @export
print.abm_draw <- function(x, ...) {
  cli::cli_text("{.cls abm_draw} {length(x$rules)} value{?s} per {x$each}")
  labs <- rule_labels(x)
  cli::cli_bullets(stats::setNames(paste0("{.code ", labs, "}"),
                                   rep("*", length(labs))))
  invisible(x)
}
