# Network mutation and neighbourhood aggregates --------------------------

new_abm_link <- function(when, drop, via = NULL, to = NULL, rules = NULL) {
  structure(list(when = when, drop = drop, via = via, to = to, rules = rules),
            class = c(if (drop) "abm_unlink" else "abm_link", "abm_step"))
}

#' Check the `via` / `to` / rules combination a link step was given
#' @noRd
check_link_args <- function(via, to, n_rules, fn, call) {
  if (!is.null(via) && !rlang::is_string(via)) {
    abm_abort("{.arg via} must be the name of one relation, as a string.",
              class = "tidyABM_no_relation", call = call)
  }
  if (is.null(via) && !is.null(to)) {
    abm_abort(
      c("{.arg to} names the other end of a relation pair, so it needs {.arg via}.",
        "i" = "Without {.arg via}, {.fn {fn}} works on the network and the pair is the standing match."),
      class = "tidyABM_bad_link", call = call
    )
  }
  if (is.null(via) && n_rules > 0L) {
    abm_abort(
      c("Value rules set columns on a relation's new rows, so they need {.arg via}.",
        "i" = "Values on the *network* are drawn per tick with {.fn abm_draw}."),
      class = "tidyABM_bad_link", call = call
    )
  }
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
#' # On a relation
#'
#' With `via = "R"` the step adds rows to the [abm_relation()] named `R`
#' instead. A relation is directed, so the row is `(me -> them)`: from each
#' agent to its `.partner`, or to the agent named by `to =`. With `to =` no
#' pairing is needed at all -- the other agent is read from a column, which is
#' what a swap needs when the incumbent being dropped and the newcomer being
#' taken on are two different agents held in two columns. `size > 2` pairings
#' are not relations.
#'
#' Rules in `...` set the new rows' value columns; a column not named takes the
#' relation's default. A pair that already exists is left exactly as it is --
#' the rules do not re-apply -- so "add to an existing balance" is a link
#' followed by a write:
#'
#' ```r
#' abm_link(via = "loans"),                          # creates the pair at 0 if new
#' abm_rules(loans_balance ~ loans_balance + take)   # adds either way
#' ```
#'
#' @param ... For `via =` only: `col ~ expr` rules setting value columns on the
#'   rows this step creates, evaluated per agent over the population with the
#'   standing match in scope.
#' @param when Optional condition. Only pairs where it holds are linked. It can
#'   use the agent's own columns, `partner_<col>`, `.role`, any global and, on
#'   a relation, the pair's `R_<col>` values.
#' @param via Optional name of a relation, as a string. Absent, the step acts
#'   on the network.
#' @param to For `via =`: an expression naming the other agent's `.id`, one per
#'   agent. Defaults to `.partner`.
#'
#' @return An `abm_link` step object.
#' @seealso [abm_go()], which lists every step and fixes the order they run
#'   in; [abm_relation()] for what a relation is.
#' @family network topology steps
#' @export
#' @examples
#' # a random graph that grows one batch of edges per tick
#' abm_go(
#'   abm_match(pair = "random", eligible = runif(n()) < 0.05),
#'   abm_link()
#' )
#'
#' # a household takes on the cheaper firm it found, held in `cand`
#' abm_link(via = "sellers", to = cand, when = swap)
#'
#' # a new loan from my partner, starting at the amount just borrowed
#' abm_link(via = "loans", balance ~ take)
abm_link <- function(..., when = NULL, via = NULL, to = NULL) {
  dots <- rlang::list2(...)
  to <- enquo_or_null(rlang::enquo(to))
  check_link_args(via, to, length(dots), "abm_link", rlang::caller_env())
  rules <- if (length(dots)) collect_rules(dots, "abm_link") else NULL
  new_abm_link(enquo_or_null(rlang::enquo(when)), drop = FALSE,
               via = via, to = to, rules = rules)
}

#' Remove edges between matched agents
#'
#' `abm_unlink()` is the mirror of [abm_link()]: it deletes the edge joining
#' each matched pair. Paired with `abm_match(pair = "network")` it detaches an
#' agent from one of its neighbours, which, followed by a match and an
#' [abm_link()], is how you rewire a network.
#'
#' With `via = "R"` it removes rows from the relation `R` instead, the row
#' `(me -> them)` for each agent's `.partner` or for the agent named by `to =`;
#' see [abm_link()] for how those are read.
#'
#' @param when Optional condition. Only pairs where it holds are unlinked. On
#'   a relation it can read the pair's `R_<col>` values, so
#'   `when = loans_balance <= 0` is "once repaid".
#' @inheritParams abm_link
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
#'
#' # a loan is closed once it is repaid
#' abm_unlink(via = "loans", when = loans_balance <= 0)
abm_unlink <- function(when = NULL, via = NULL, to = NULL) {
  to <- enquo_or_null(rlang::enquo(to))
  check_link_args(via, to, 0L, "abm_unlink", rlang::caller_env())
  new_abm_link(enquo_or_null(rlang::enquo(when)), drop = TRUE, via = via, to = to)
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
  aug <- augment_group(combined, state$match, combined, state$relations)
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

#' The directed `(me -> target)` pairs a `via =` link step acts on
#'
#' The target is `.partner` by default, which needs a standing pairing of two;
#' or the agent named by `to =`, evaluated per agent over the population, which
#' needs no pairing at all -- the incumbent a household drops and the newcomer
#' it takes on are two different agents held in two columns, and neither is
#' the partner of anything. No canonicalisation: a relation is directed.
#' @noRd
relation_pairs <- function(step, state, fn) {
  empty <- list(from = integer(), to = integer(), values = list())
  combined <- bind_groups(state$groups)
  if (!nrow(combined)) return(empty)
  if (is.null(step$to)) {
    if (is.null(state$match)) {
      abm_abort(
        c("{.fn {fn}} with {.arg via} needs a standing pairing, or a {.arg to}.",
          "i" = "Put an {.fn abm_match} before it, or name the other agent with {.code to = <column>}."),
        class = "tidyABM_no_match"
      )
    }
    if (state$match$size != 2L) {
      abm_abort(
        c("A relation joins two agents, and this pairing groups {state$match$size}.",
          "i" = "Use a match of {.code size = 2}, or name the other agent with {.arg to}."),
        class = "tidyABM_bad_link"
      )
    }
    if (nrow(state$match$match) == 0L) return(empty)
  }
  aug <- augment_group(combined, state$match, combined, state$relations)
  keep <- eval_condition(step$when, aug, state$globals)
  keep[is.na(keep)] <- FALSE

  if (is.null(step$to)) {
    tgt <- aug$.partner
  } else {
    tgt <- eval_rule(list(quo = step$to), aug, state$globals, grouped = FALSE)
    if (length(tgt) == 1L) tgt <- rep(tgt, nrow(aug))
    if (is.list(tgt) || length(tgt) != nrow(aug)) {
      abm_abort(
        c("{.arg to} must give one agent {.code .id} per agent.",
          "x" = "{.code {deparse1(rlang::quo_get_expr(step$to))}} did not."),
        class = "tidyABM_bad_link"
      )
    }
    tgt <- as.integer(tgt)
  }
  keep <- keep & !is.na(tgt)
  if (!any(keep)) return(empty)
  from <- aug$.id[keep]; to <- tgt[keep]
  unknown <- setdiff(to, combined$.id)
  if (length(unknown)) {
    abm_abort(
      c("{.fn {fn}} was pointed at an agent that does not exist.",
        "x" = "No agent has {.code .id} {unknown[[1]]}."),
      class = "tidyABM_bad_link"
    )
  }
  if (any(from == to)) {
    abm_abort("{.fn {fn}} would relate agent {from[from == to][[1]]} to itself.",
              class = "tidyABM_bad_link")
  }
  values <- list()
  for (r in step$rules) {
    v <- eval_rule(r, aug, state$globals, grouped = FALSE)
    if (!is.list(v) && !is.null(names(v))) v <- unname(v)
    values[[r$target]] <- vctrs::vec_recycle(v, nrow(aug))[keep]
  }
  list(from = from, to = to, values = values)
}

#' @noRd
run_link <- function(step, state) {
  if (!is.null(step$via)) {
    rel <- relation_or_abort(state, step$via, "abm_link")
    p <- relation_pairs(step, state, "abm_link")
    state$relations[[step$via]] <- add_relation_rows(rel, p$from, p$to, p$values)
    return(state)
  }
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
  if (nrow(new) == 0L) return(state)
  state$edges <- dplyr::bind_rows(state$edges, new)
  invalidate_draws(state, "abm_link")
}

#' @noRd
run_unlink <- function(step, state) {
  if (!is.null(step$via)) {
    rel <- relation_or_abort(state, step$via, "abm_unlink")
    p <- relation_pairs(step, state, "abm_unlink")
    state$relations[[step$via]] <- drop_relation_rows(rel, p$from, p$to)
    return(state)
  }
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
#'
#'   `within = .R` for a relation `R` (or `.R_back`, the reverse direction) is
#'   the neighbourhood *being* the relation: the rows are the pairs, so it is
#'   linear in the relation's size and the aggregate sees the pair's values --
#'   `abm_neighbours(rationed ~ any(sellers_unmet > 0), within = .sellers)`.
#'   A rule's target must be an agent column; a relation's values are updated
#'   with [abm_pairs()].
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
pair_view <- function(combined, focal_idx, cand_idx, relations = NULL) {
  cols <- names(combined)
  view <- combined[cand_idx, cols, drop = FALSE]
  own <- combined[focal_idx, cols, drop = FALSE]
  names(own) <- paste0("own_", cols)
  view <- dplyr::bind_cols(view, own)
  view$.of <- combined$.id[focal_idx]
  attach_relation_columns(view, relations, view$.of, view$.id)
}

#' Evaluate one quosure against a pair view, with the globals in scope
#' @noRd
eval_over_view <- function(quo, view, globals) {
  quo <- rlang::quo_set_env(quo, abm_eval_env(quo, globals))
  dplyr::pull(dplyr::mutate(view, .abm_value = !!quo), ".abm_value")
}

#' The (focal, neighbour) view for a network neighbourhood
#' @noRd
network_view <- function(combined, state) {
  nb <- neighbour_table(state$edges)
  keep <- nb$.id %in% combined$.id & nb$.neighbour %in% combined$.id
  nb <- nb[keep, , drop = FALSE]
  view <- pair_view(combined, match(nb$.id, combined$.id),
                    match(nb$.neighbour, combined$.id), state$relations)
  attach_edge_columns(view, state$edges, nb)
}

#' The (focal, candidate) view for `within = .R` or `within = .R_back`
#'
#' A neighbourhood that *is* a relation needs no cross product: the relation's
#' rows are the pairs. Linear in the relation's size, which is what keeps "the
#' mean price across my sellers" cheap. Returns `NULL` for any other condition.
#' @noRd
relation_view <- function(step, combined, relations) {
  expr <- rlang::quo_get_expr(step$within)
  if (!rlang::is_symbol(expr) || !length(relations)) return(NULL)
  hit <- resolve_relation_name(rlang::as_string(expr), relations)
  if (is.null(hit) || !is.null(hit$col)) return(NULL)
  e <- relations[[hit$rel]]$edges
  focal <- if (hit$back) e$to else e$from
  cand  <- if (hit$back) e$from else e$to
  fi <- match(focal, combined$.id); ci <- match(cand, combined$.id)
  ok <- !is.na(fi) & !is.na(ci)
  pair_view(combined, fi[ok], ci[ok], relations)
}

#' The (focal, candidate) view for a neighbourhood in attribute space
#' @noRd
attribute_view <- function(step, combined, globals, relations = NULL) {
  n <- nrow(combined)
  ci <- rep(seq_len(n), times = n)
  si <- rep(seq_len(n), each = n)
  view <- pair_view(combined, si, ci, relations)
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
    # `within = .R` walks the relation's rows; a `<col> == own_<col>` condition
    # is a join; anything else is the cross product
    view <- relation_view(step, combined, state$relations) %||%
      equijoin_view(step, combined, state$globals, state$relations) %||%
      attribute_view(step, combined, state$globals, state$relations)
  } else {
    if (is.null(state$edges)) {
      abm_abort(
        c("{.fn abm_neighbours} needs a network.",
          "i" = "Add one with {.code abm_setup(..., network = abm_network(...))}.",
          "i" = "Or give it a {.arg within} condition, which needs no network."),
        class = "tidyABM_no_network"
      )
    }
    check_stale_draws(step, state)
    view <- network_view(combined, state)
  }

  for (r in step$rules) {
    hit <- resolve_relation_name(r$target, state$relations)
    if (!is.null(hit)) {
      abm_abort(
        c("{.fn abm_neighbours} writes agent columns, and {.field {r$target}} names a value on relation {.field {hit$rel}}.",
          "i" = 'Update every pair with {.code abm_pairs(via = "{hit$rel}", ...)}, or one pair under a match with {.fn abm_rules}.'),
        class = "tidyABM_bad_target"
      )
    }
    quo <- rlang::quo_set_env(r$quo, abm_eval_env(r$quo, state$globals))
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
  cli::cli_text("{.cls abm_link}{if (!is.null(x$via)) paste0(' via ', x$via) else ''}")
  if (!is.null(x$to)) {
    cli::cli_bullets(c("*" = "to = {.code {deparse1(rlang::quo_get_expr(x$to))}}"))
  }
  for (r in x$rules) {
    cli::cli_bullets(c("*" = "{.field {r$target}} ~ {.code {deparse1(rlang::quo_get_expr(r$quo))}}"))
  }
  if (!is.null(x$when)) {
    cli::cli_bullets(c("*" = "when = {.code {deparse1(rlang::quo_get_expr(x$when))}}"))
  }
  invisible(x)
}

#' @export
print.abm_unlink <- function(x, ...) {
  cli::cli_text("{.cls abm_unlink}{if (!is.null(x$via)) paste0(' via ', x$via) else ''}")
  if (!is.null(x$to)) {
    cli::cli_bullets(c("*" = "to = {.code {deparse1(rlang::quo_get_expr(x$to))}}"))
  }
  if (!is.null(x$when)) {
    cli::cli_bullets(c("*" = "when = {.code {deparse1(rlang::quo_get_expr(x$when))}}"))
  }
  invisible(x)
}

#' @export
print.abm_neighbours <- function(x, ...) print_rule_step(x, "abm_neighbours")
