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
  check_rule_dots(rlang::enexprs(...), "abm_link")
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
    return(edge_tbl(integer(), integer()))
  }
  combined <- bind_groups(state$groups)
  aug <- augment_group(combined, state$match, combined, state$relations)
  cvars <- cell_vars(list(step$when))
  if (length(cvars)) aug <- attach_cell_columns(aug, state, cvars)
  keep <- eval_condition(step$when, aug, state$globals)

  if (state$match$size == 2L) {
    keep <- keep & !is.na(aug$.partner)
    pairs <- edge_tbl(aug$.id[keep], aug$.partner[keep])
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
    if (nrow(pairs) == 0L) return(edge_tbl(integer(), integer()))
  }
  # each pair may appear twice; keep one copy, oriented consistently
  out <- edge_tbl(pmin(pairs$from, pairs$to), pmax(pairs$from, pairs$to))
  dplyr::distinct(out)
}

#' A `from`/`to` edge tibble, built without `tibble()`'s per-call overhead
#' @noRd
edge_tbl <- function(from, to) {
  new_tbl(list(from = from, to = to), length(from))
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
  cvars <- cell_vars(c(list(step$when, step$to), lapply(step$rules, `[[`, "quo")))
  if (length(cvars)) aug <- attach_cell_columns(aug, state, cvars)
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
  existing <- edge_tbl(pmin(state$edges$from, state$edges$to),
                       pmax(state$edges$from, state$edges$to))
  new <- dplyr::anti_join(new, existing, by = c("from", "to"))
  if (nrow(new) == 0L) return(state)
  state$edges <- vctrs::vec_rbind(state$edges, new)
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
#' think on average".
#'
#' An agent with no neighbours gets `NA`, with one exception: a rule that is
#' exactly `n()` gets `0`. The size of a neighbourhood is never unknown, so a
#' count of nobody is nought and needs no `coalesce()` to guard it. Every other
#' aggregate keeps `NA`, because `mean()` over nobody is genuinely unknown and
#' because a `sum()` over a neighbourhood of one -- a lookup of the single
#' agent a column points at -- is how a model asks whether that agent exists
#' at all.
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
#' ```r
#' abm_neighbours(opinion ~ mean(opinion),
#'                within = abs(opinion - own_opinion) <= eps)
#' ```
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
#' @param ... One or more `column ~ aggregate_expression` rules. The expression
#'   sees the neighbours' agent columns, the focal agent's own columns as
#'   `own_<col>`, any column [abm_draw()] attached to the edge, and any global.
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
#' @param .default Optional atomic scalar value to use for agents with zero
#'   neighbours instead of `NA` (other than bare `n()`, which already returns
#'   `0L`).
#' @param via Optional `"space"` or `"network"` when a model has both a spatial
#'   lattice and a social network, selecting which topology defines neighbours.
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
abm_neighbours <- function(..., within = NULL, .where = NULL,
                           .default = NULL, via = NULL) {
  check_rule_dots(rlang::enexprs(...), "abm_neighbours")
  step <- new_rule_step(collect_rules(rlang::list2(...), "abm_neighbours"),
                        "abm_neighbours")
  step$within <- enquo_or_null(rlang::enquo(within))
  step$where <- .where
  if (!is.null(.default)) {
    if (!is.atomic(.default) || length(.default) != 1L) {
      abm_abort(
        c("{.arg .default} must be a single atomic value.",
          "x" = "Got {.cls {class(.default)[[1]]}} of length {length(.default)}."),
        class = "tidyABM_bad_arg"
      )
    }
  }
  step$default <- .default
  if (!is.null(via)) {
    if (!rlang::is_string(via)) {
      abm_abort('{.arg via} must be {.val space} or {.val network}.',
                class = "tidyABM_bad_arg")
    }
    via <- rlang::arg_match0(via, c("space", "network"), arg_nm = "via")
  }
  step$via <- via
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
  if (!is.null(via) && !is.null(step$within)) {
    abm_abort(
      c("{.arg via} and {.arg within} are different neighbourhoods.",
        "i" = "{.arg via} picks between the model's space and network; {.arg within} is a condition."),
      class = "tidyABM_conflicting_args"
    )
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
pair_view <- function(combined, focal_idx, cand_idx, relations = NULL,
                      vars = NULL, space = NULL) {
  # `vars` are the names the caller's expressions read; only those columns
  # are copied into the view, which for a lattice is tens of thousands of
  # rows wide. `NULL` copies every column.
  cols <- names(combined)
  # `%in%` rather than `intersect()`: column names are already distinct, and
  # since R 4.5 the set operations check their arguments' classes first
  cand_cols <- if (is.null(vars)) cols else cols[cols %in% c(".id", vars)]
  own_cols <- if (is.null(vars)) cols else
    cols[cols %in% sub("^own_", "", vars[startsWith(vars, "own_")])]
  # sliced a bare column at a time: `[.tbl_df` re-checks the tibble per tick
  cols <- unclass(combined)
  view <- c(
    lapply(cols[cand_cols], vctrs::vec_slice, cand_idx),
    stats::setNames(lapply(cols[own_cols], vctrs::vec_slice, focal_idx),
                    sprintf("own_%s", own_cols)),
    list(.of = combined$.id[focal_idx])
  )
  if ((is.null(vars) || ".dist" %in% vars) && !is.null(space) &&
      ".x" %in% names(cols)) {
    view$.dist <- space_distance(
      cols$.x[cand_idx], if (".y" %in% names(cols)) cols$.y[cand_idx] else NULL,
      cols$.x[focal_idx], if (".y" %in% names(cols)) cols$.y[focal_idx] else NULL,
      space
    )
  }
  view <- attach_relation_columns(view, relations, view$.of, view$.id)
  new_tbl(view, length(cand_idx))
}

#' Evaluate one quosure against a pair view, with the globals in scope
#' @noRd
eval_over_view <- function(quo, view, globals) {
  # the same shortcut `eval_rule()` takes: `eval_tidy()` over the view unless
  # the expression needs something only a dplyr mask supplies, or comes back
  # the wrong length. `n()` is bound to the number of rows either way.
  n <- nrow(view)
  if (!needs_mask(rlang::quo_get_expr(quo))) {
    val <- rlang::eval_tidy(
      rlang::quo_get_expr(quo), data = view, env = abm_eval_env(quo, globals, n = n))
    if (length(val) == 1L || length(val) == n) return(val)
  }
  quo <- rlang::quo_set_env(quo, abm_eval_env(quo, globals))
  dplyr::pull(dplyr::mutate(view, .abm_value = !!quo), ".abm_value")
}

#' The (focal, neighbour) view for a network neighbourhood
#' @noRd
network_view <- function(combined, state, vars = NULL, edges = state$edges,
                         cache_name = "nb_cache") {
  nb <- state[[cache_name]]$nb
  ids <- combined$.id
  if (identical(ids, seq_along(ids))) {
    # the ids are the row numbers, which is every patch-only lattice model:
    # an edge endpoint is then its own row index and nobody is missing
    focal_idx <- nb$.id
    cand_idx <- nb$.neighbour
  } else {
    keep <- nb$.id %in% ids & nb$.neighbour %in% ids
    nb <- vctrs::vec_slice(nb, keep)
    focal_idx <- match(nb$.id, ids)
    cand_idx <- match(nb$.neighbour, ids)
  }
  view <- pair_view(combined, focal_idx, cand_idx, state$relations, vars,
                    space = state$space %||% state$lattice)
  attach_edge_columns(view, edges, nb)
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
attribute_view <- function(step, combined, globals, relations = NULL,
                           vars = NULL, space = NULL) {
  n <- nrow(combined)
  # AST-recognised spatial cell bucketing for `.dist <= r` / `.dist < r`
  if (!is.null(space) && n > 64L) {
    db <- dist_bound_key(rlang::quo_get_expr(step$within))
    if (!is.null(db)) {
      env <- rlang::quo_get_env(step$within)
      rq <- rlang::new_quosure(db$radius_expr, env)
      bp <- spatial_radius_pairs(combined, seq_len(n), seq_len(n), rq, db$strict,
                                 globals, space)
      if (!is.null(bp)) {
        view <- pair_view(combined, bp$focal_idx, bp$cand_idx, relations, vars,
                          space = space)
        if (length(db$rest) && nrow(view) > 0L) {
          rest_expr <- Reduce(function(a, b) rlang::call2("&", a, b), db$rest)
          keep <- eval_over_view(rlang::new_quosure(rest_expr, env), view, globals)
          if (!is.logical(keep)) {
            abm_abort(
              c("{.arg within} must be a condition.",
                "x" = "{.code {deparse1(rlang::quo_get_expr(step$within))}} returned {.cls {class(keep)[[1]]}}."),
              class = "tidyABM_bad_within"
            )
          }
          if (length(keep) == 1L) keep <- rep(keep, nrow(view))
          keep[is.na(keep)] <- FALSE
          view <- vctrs::vec_slice(view, keep)
        }
        return(view)
      }
    }
  }
  ci <- rep(seq_len(n), times = n)
  si <- rep(seq_len(n), each = n)
  # every (focal, candidate) pair, so only the columns the rules read are
  # copied in: on a lattice the cross product is the population squared
  view <- pair_view(combined, si, ci, relations, vars, space = space)
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
  vctrs::vec_slice(view, keep)
}

#' @noRd
run_neighbours <- function(step, state) {
  # only what the rules and `within` read, unless `where` looks along the lattice
  combined <- bind_groups(state$groups, if (is.null(step$where))
    planned(step, "reads", read_vars(c(lapply(step$rules, `[[`, "quo"), list(step$within)))))
  if (nrow(combined) == 0L) return(state)
  vars <- planned(step, "vars", unique(c(
    unlist(lapply(step$rules, `[[`, "vars"), use.names = FALSE),
    if (!is.null(step$within)) all.vars(rlang::quo_get_expr(step$within)))))
  sp <- state$space %||% state$lattice

  one <- NULL
  if (!is.null(step$where)) {
    # L1: the single lattice neighbour in a named direction
    view <- directional_view(step, combined, state)
  } else if (!is.null(step$within)) {
    one <- nb_one(step, combined, state)
    # `within = .R` walks the relation's rows; a `<col> == own_<col>` condition
    # is a join; anything else is the cross product (or spatial bucket join)
    if (is.null(one)) view <- relation_view(step, combined, state$relations) %||%
      equijoin_view(step, combined, state$globals, state$relations, vars, space = sp) %||%
      attribute_view(step, combined, state$globals, state$relations, vars, space = sp)
  } else {
    topo <- resolve_topology_edges(step$via, state, "abm_neighbours")
    if (is.null(topo)) {
      abm_abort(
        c("{.fn abm_neighbours} needs a network.",
          "i" = "Add one with {.code abm_setup(..., network = abm_network(...))}.",
          "i" = "Or give it a {.arg within} condition, which needs no network."),
        class = "tidyABM_no_network"
      )
    }
    edges <- topo$edges
    cname <- topo$cache
    if (cname == "nb_cache") check_stale_draws(step, state)
    # the neighbour table only changes when the edges do, and edges rarely do
    if (!identical(state[[cname]]$edges, edges)) {
      state[[cname]] <- list(edges = edges, nb = neighbour_table(edges))
    }
    if (cname == "nb_cache") {
      fast <- neighbour_fast(step, combined, state)
      if (!is.null(fast)) return(fast)
    }
    view <- network_view(combined, state, vars, edges = edges, cache_name = cname)
  }

  counts <- planned(step, "counts", vapply(step$rules, function(r) is_bare_count(r$quo), NA))
  for (i in seq_along(step$rules)) {
    r <- step$rules[[i]]
    hit <- resolve_relation_name(r$target, state$relations)
    if (!is.null(hit)) {
      abm_abort(
        c("{.fn abm_neighbours} writes agent columns, and {.field {r$target}} names a value on relation {.field {hit$rel}}.",
          "i" = 'Update every pair with {.code abm_pairs(via = "{hit$rel}", ...)}, or one pair under a match with {.fn abm_rules}.'),
        class = "tidyABM_bad_target"
      )
    }
    agg <- if (!is.null(one)) list(.of = one$of, .abm_value = one$values[[i]]) else {
      quo <- rlang::quo_set_env(r$quo, abm_eval_env(r$quo, state$globals))
      neighbour_aggregate(quo, view) %||%
        dplyr::summarise(dplyr::group_by(view, .data$.of),
                         .abm_value = !!quo, .groups = "drop")
    }
    # An agent with no neighbours contributes no row, so the aggregate has no
    # value for it and the agent gets `NA`. That is right for `mean()`, whose
    # value over nobody is genuinely unknown, and for a `sum()` used as a
    # lookup, where `NA` is how a model asks whether the neighbour exists at
    # all. It is wrong for `n()` alone: the size of a neighbourhood is never
    # unknown, and the count of nobody is nought.
    count_rule <- counts[[i]] && is.null(step$where)
    start <- 0L
    for (nm in names(state$groups)) {
      g <- state$groups[[nm]]
      ng <- vctrs::vec_size(g)
      start <- start + ng
      if (ng == 0L) next
      idx <- if (is.null(one)) match(g$.id, agg$.of) else {
        # the stack is the groups in order, so a focal agent's row says
        # where it is in its group: no lookup by id
        at <- one$rows - (start - ng)
        sel <- at >= 1L & at <= ng
        out <- rep(NA_integer_, ng)
        out[at[sel]] <- which(sel)
        out
      }
      value <- agg$.abm_value[idx]
      miss <- is.na(idx)
      if (any(miss)) {
        if (!is.null(step$default)) {
          common <- vctrs::vec_cast_common(value, step$default)
          value <- common[[1]]
          value[miss] <- common[[2]]
        } else if (count_rule) {
          value[miss] <- 0L
        }
      }
      state$groups[[nm]] <- assign_rule(g, r$target, value, TRUE)
    }
  }
  state
}

#' `abm_neighbours()` over a join in which nobody meets more than one candidate
#'
#' `within = .id == own_.cell & <condition>` -- a mover reading the cell it
#' stands on -- joins on a column whose values are distinct, so each focal
#' agent's neighbourhood is one candidate or none. When the rest of `within`
#' and every rule's argument are comparisons and logic over the candidates'
#' own bare columns (they can neither warn nor draw), they are evaluated once
#' over the population and read off at each focal agent's candidate, and an
#' `any()`, `all()` or `sum()` of one value is that value (`0 + x` for a
#' double, as `rowsum()` makes it); `n()` is one. Returns `list(of, values)`,
#' or `NULL` for anything else -- including a neighbourhood that comes out
#' empty, or any error -- which then takes the general path.
#' @noRd
nb_one <- function(step, combined, state) {
  if (length(state$relations)) return(NULL)
  key <- planned(step, "equijoin", equijoin_key(rlang::quo_get_expr(step$within)))
  if (is.null(key)) return(NULL)
  shape <- planned(step, "nb_one", nb_one_shape(step, key))
  if (is.null(shape)) return(NULL)
  cols <- names(combined)
  if (!key$focal %in% cols || !key$cand %in% cols || !all(shape$vars %in% cols)) {
    return(NULL)
  }
  for (v in shape$vars) {
    x <- combined[[v]]
    if (!is.atomic(x) || !is.null(attributes(x))) return(NULL)
  }
  cand_key <- combined[[key$cand]]
  if (key$cand != ".id" && anyDuplicated(cand_key[!is.na(cand_key)])) return(NULL)
  cand_idx <- match(combined[[key$focal]], cand_key)
  focal_idx <- which(!is.na(cand_idx))
  if (!length(focal_idx)) return(NULL)
  cand_idx <- cand_idx[focal_idx]
  n <- nrow(combined)
  env <- abm_eval_env(step$within, state$globals, n = n)
  if (!standard_fns_in(shape$heads, env)) return(NULL)
  ev <- function(e) {
    x <- tryCatch(eval(e, combined, env), error = function(err) NULL)
    if (length(x) == 1L) x <- rep(x, n)
    if (length(x) != n || is.object(x)) NULL else x
  }
  if (!is.null(shape$rest)) {
    keep <- ev(shape$rest)
    if (!is.logical(keep)) return(NULL)
    keep <- keep[cand_idx]
    keep[is.na(keep)] <- FALSE
    focal_idx <- focal_idx[keep]
    cand_idx <- cand_idx[keep]
    if (!length(focal_idx)) return(NULL)
  }
  values <- vector("list", length(shape$rules))
  for (i in seq_along(shape$rules)) {
    r <- shape$rules[[i]]
    if (r$fn == "n") {
      values[[i]] <- rep(1L, length(cand_idx))
      next
    }
    x <- ev(r$arg)
    if (r$fn %in% c("any", "all")) {
      if (!is.logical(x)) return(NULL)
      values[[i]] <- x[cand_idx]
    } else {
      if (!(is.double(x) || is.integer(x) || is.logical(x))) return(NULL)
      x <- x[cand_idx]
      values[[i]] <- if (is.double(x)) 0 + x else as.integer(x)
    }
  }
  list(of = combined$.id[focal_idx], rows = focal_idx, values = values)
}

#' What [nb_one()] reads off a step's text: the rest of `within` and each
#' rule as `any()` / `all()` / `sum()` of an argument, or `n()`; `NULL` when
#' anything is not that shape
#' @noRd
nb_one_shape <- function(step, key) {
  quiet <- function(e) is_elementwise(e, narrow_quiet_fns) &&
    !any(startsWith(all.vars(e), "own_")) && !".of" %in% all.vars(e)
  rest <- if (length(key$rest)) Reduce(function(a, b) rlang::call2("&", a, b), key$rest)
  if (!is.null(rest) && !quiet(rest)) return(NULL)
  rules <- lapply(step$rules, function(r) {
    e <- rlang::quo_get_expr(r$quo)
    if (!rlang::is_call(e) || !rlang::is_symbol(e[[1]]) || !is.null(names(e))) return(NULL)
    fn <- rlang::as_string(e[[1]])
    if (fn == "n" && length(e) == 1L) return(list(fn = fn))
    if (!fn %in% c("any", "all", "sum") || length(e) != 2L || !quiet(e[[2]])) return(NULL)
    list(fn = fn, arg = e[[2]])
  })
  if (!length(rules) || any(vapply(rules, is.null, NA))) return(NULL)
  exprs <- c(list(rest), lapply(rules, `[[`, "arg"))
  exprs <- Filter(Negate(is.null), exprs)
  # every formula written where `within` was, so one scope serves them all
  env <- rlang::quo_get_env(step$within)
  if (!all(vapply(step$rules, function(r) identical(rlang::quo_get_env(r$quo), env), NA))) {
    return(NULL)
  }
  heads <- unique(unlist(lapply(exprs, call_heads), use.names = FALSE))
  if (anyNA(heads)) return(NULL)
  list(rest = rest, rules = rules,
       vars = unique(unlist(lapply(exprs, all.vars), use.names = FALSE)),
       heads = as.character(heads))
}

#' A network neighbourhood's `sum()` / `mean()` without the pair view
#'
#' The common case -- `mean(x)` over a lattice, every rule `sum(<elementwise>)`
#' or `mean(<elementwise>)` of a double, the ids the row numbers, no relation,
#' nothing read from the focal agent or the edge -- evaluates the argument once
#' over the agents and folds it through a padded neighbour index, kept with
#' the neighbour table, column by column. That is the addition order
#' `rowsum()` uses in [neighbour_aggregate()] (view rows in order within each
#' focal agent, starting from `0`), so the answer is the same bit for bit.
#' Returns the new `state`, or `NULL` for anything else, which then takes the
#' general path. Rules in the step all read the population as it was.
#' @noRd
neighbour_fast <- function(step, combined, state) {
  if (length(state$relations)) return(NULL)
  ids <- combined$.id
  n <- length(ids)
  if (!identical(ids, seq_len(n))) return(NULL)
  shapes <- planned(step, "shapes", lapply(step$rules, function(r) nb_fast_shape(r$quo)))
  if (any(vapply(shapes, is.null, NA))) return(NULL)
  drawn <- drawn_names(state$edges)
  if (length(drawn)) {
    bad <- c(drawn, paste0(drawn, "_back"))
    if (any(vapply(shapes, function(s) any(s$vars %in% bad), NA))) return(NULL)
  }
  # every argument is evaluated and type-checked before the index is built, so
  # a rule the fast path cannot take costs no index
  xs <- vector("list", length(shapes))
  for (i in seq_along(shapes)) {
    env <- abm_eval_env(step$rules[[i]]$quo, state$globals)
    if (!nb_fast_scalars(shapes[[i]], names(combined), env)) return(NULL)
    x <- nb_fast_eval(shapes[[i]]$arg, env, combined)
    if (length(x) == 1L) x <- rep(x, n)
    if (is.object(x) || length(x) != n ||
        !(is.double(x) || is.integer(x) || is.logical(x))) return(NULL)
    xs[[i]] <- x
  }
  ix <- state$nb_cache$ix
  if (is.null(ix) || ix$n != n) {
    ix <- nb_index(state$nb_cache$nb, n)
    state$nb_cache$ix <- ix
  }
  if (!ix$ok) return(NULL)
  vals <- vector("list", length(shapes))
  for (i in seq_along(shapes)) {
    x <- xs[[i]]
    v <- if (is.double(x)) nb_fold(shapes[[i]]$fn, x, ix)
         else nb_fold_int(shapes[[i]]$fn, as.integer(x), ix)
    if (!is.null(step$default) && any(ix$none)) {
      common <- vctrs::vec_cast_common(v, step$default)
      v <- common[[1]]
      v[ix$none] <- common[[2]]
    }
    vals[[i]] <- v
  }
  for (i in seq_along(shapes)) {
    for (nm in names(state$groups)) {
      g <- state$groups[[nm]]
      if (nrow(g) == 0L) next
      state$groups[[nm]] <- assign_rule(g, step$rules[[i]]$target, vals[[i]][g$.id], TRUE)
    }
  }
  state
}

#' `list(fn, arg, vars)` for a rule `sum(<arg>)` / `mean(<arg>)` the fast path
#' takes, or `NULL`
#' @noRd
nb_fast_shape <- function(quo) {
  expr <- rlang::quo_get_expr(quo)
  if (!rlang::is_call(expr) || !rlang::is_symbol(expr[[1]]) ||
      length(expr) != 2L || !is.null(names(expr))) return(NULL)
  fn <- rlang::as_string(expr[[1]])
  if (!fn %in% c("sum", "mean")) return(NULL)
  arg <- expr[[2]]
  if (!is_elementwise(arg, nb_fast_fns)) return(NULL)
  vars <- all.vars(arg)
  if (any(startsWith(vars, "own_")) || ".of" %in% vars) return(NULL)
  fns <- call_heads(arg)
  if (anyNA(fns)) return(NULL)
  list(fn = fn, arg = arg, vars = vars, fns = unique(fns))
}

#' The function names an expression calls (`base::f` and `dplyr::f` as `f`);
#' `NA` for a call qualified with any other package or with no name
#' @noRd
call_heads <- function(expr) {
  if (!is.call(expr)) return(character())
  head <- expr[[1]]
  own <- if (rlang::is_call(head, "::")) {
    if (rlang::as_string(head[[2]]) %in% c("base", "dplyr")) rlang::as_string(head[[3]])
    else NA_character_
  } else if (!rlang::is_symbol(head)) {
    NA_character_
  } else {
    rlang::as_string(head)
  }
  c(own, unlist(lapply(as.list(expr)[-1], call_heads), use.names = FALSE))
}

#' The argument over the agents; `NULL` on an error or a warning, so the
#' general path raises it as it always has
#' @noRd
nb_fast_eval <- function(arg, env, data) {
  # a bare column is the column, as the mask would find it first
  if (is.symbol(arg) && !is.null(x <- .subset2(data, as.character(arg)))) return(x)
  warned <- FALSE
  x <- tryCatch(
    withCallingHandlers(
      rlang::eval_tidy(rlang::new_quosure(arg, env), data = data),
      warning = function(w) {
        warned <<- TRUE
        invokeRestart("muffleWarning")
      }),
    error = function(e) NULL)
  if (warned) NULL else x
}

#' Each agent's neighbours as columns of row indices, in view-row order, padded
#' with `n + 1`. `ok` is `FALSE` when an edge names an agent beyond the `n`
#' rows or there are no edges.
#' @noRd
nb_index <- function(nb, n) {
  if (!nrow(nb) || max(nb$.id) > n) return(list(n = n, ok = FALSE))
  o <- order(nb$.id, method = "radix")        # stable: view order within a focal
  id <- nb$.id[o]
  cnt <- tabulate(id, n)
  pos <- sequence(cnt[cnt > 0L])
  maxd <- max(cnt)
  k <- matrix(n + 1L, n, maxd)
  k[cbind(id, pos)] <- nb$.neighbour[o]
  cols <- lapply(seq_len(maxd), function(j) k[, j])
  list(n = n, ok = TRUE, cols = cols, pad = lapply(cols, function(j) j > n),
       cnt = cnt, none = cnt == 0L)
}

#' Is every name the argument reads a column, or else a single value?
#'
#' The general path evaluates the argument over the pair view, so a name that
#' is neither (a vector global as long as the population, say) means something
#' else there; and every function it calls must be the one the allowlist means,
#' not a user's function of the same name.
#' @noRd
nb_fast_scalars <- function(shape, cols, env) {
  for (v in shape$vars[!shape$vars %in% cols]) {
    val <- get0(v, envir = env, inherits = TRUE)
    if (is.null(val) || is.function(val) || length(val) != 1L) return(FALSE)
  }
  for (f in shape$fns) {
    fun <- get0(f, envir = env, mode = "function")
    ok <- identical(fun, get0(f, envir = baseenv(), mode = "function")) ||
      (f == "==" && identical(fun, group_is)) ||
      (f %in% c("if_else", "case_when", "coalesce", "between") &&
         identical(fun, getExportedValue("dplyr", f)))
    if (!ok) return(FALSE)
  }
  TRUE
}

#' `rowsum()`'s integer `sum()` / `mean()` of `x` over each agent's neighbours
#'
#' `rowsum()` adds integers in view-row order and turns a group whose running
#' sum ever leaves the integer range into `NA` (silently) and keeps it there.
#' Doubles hold every such partial sum exactly, so they are added as doubles
#' and checked against the range as they go.
#' @noRd
nb_fold_int <- function(fn, x, ix) {
  xe <- c(as.double(x), 0)
  big <- .Machine$integer.max
  m <- suppressWarnings(max(abs(as.double(x)), na.rm = TRUE))
  track <- is.finite(m) && m * length(ix$cols) > big
  s <- 0
  out_of_range <- FALSE
  for (k in ix$cols) {
    s <- s + xe[k]
    if (track) out_of_range <- out_of_range | (!is.na(s) & abs(s) > big)
  }
  if (track) s[out_of_range] <- NA
  s <- as.integer(s)
  if (fn == "sum") {
    s[ix$none] <- NA
    return(s)
  }
  out <- s / ix$cnt
  out[ix$none] <- NA
  out
}

#' `rowsum()`'s `sum()` / `mean()` of `x` over each agent's neighbours
#' @noRd
nb_fold <- function(fn, x, ix) {
  xe <- c(x, 0)
  s <- 0
  for (k in ix$cols) s <- s + xe[k]
  # which of `NA` and `NaN` a sum that met both comes out as is down to the
  # order `rowsum()`'s C loop adds in, not R's `+`: those few are summed by it
  bad <- which(is.na(s) & !ix$none)
  if (length(bad)) {
    idx <- t(do.call(cbind, lapply(ix$cols, `[`, bad)))
    grp <- rep(bad, each = nrow(idx))
    keep <- idx <= ix$n
    s[bad] <- as.vector(rowsum(x[idx[keep]], grp[keep]))
  }
  if (fn == "sum") {
    s[ix$none] <- NA
    return(s)
  }
  out <- s / ix$cnt
  # base's second pass: the mean of the residuals, where the first was finite
  r <- 0
  for (j in seq_along(ix$cols)) {
    d <- xe[ix$cols[[j]]] - out
    d[ix$pad[[j]]] <- 0
    r <- r + d
  }
  fin <- is.finite(out)
  out[fin] <- (out + r / ix$cnt)[fin]
  out[ix$none] <- NA
  out
}

#' `sum()`, `mean()`, `any()`, `all()` and `n()` over every neighbourhood at once
#'
#' The argument is elementwise, so it is evaluated once over the whole view and
#' folded per focal agent with `rowsum()`, instead of `summarise()` evaluating
#' the aggregate once per group. Exact for integer and logical input, which is
#' what a count or a share of neighbours is. A double `sum()` or `mean()` is
#' the same arithmetic base does, in the same order, but in double precision:
#' bit for bit base's answer where `long double` is a double (arm64), and
#' within a unit in the last place of it where base accumulates in 80 bits
#' (x86). A literal `na.rm = TRUE` is honoured, as base does: the missing rows
#' are left out of the sum and the count. Returns `NULL` for
#' any other shape.
#' ponytail: five aggregate shapes, extend when a model's hot rule has another
#' @noRd
neighbour_aggregate <- function(quo, view) {
  expr <- rlang::quo_get_expr(quo)
  if (!rlang::is_call(expr) || !rlang::is_symbol(expr[[1]])) return(NULL)
  fn <- rlang::as_string(expr[[1]])
  g <- view$.of
  if (fn == "n" && length(expr) == 1L) {
    s <- group_sum(rep(1L, length(g)), g)
    return(list(.of = s$keys, .abm_value = s$sums))
  }
  if (!fn %in% c("sum", "mean", "any", "all")) return(NULL)
  args <- as.list(expr)[-1]
  nms <- names(args) %||% rep("", length(args))
  na_rm <- FALSE
  if (length(args) == 2L && nms[[2]] == "na.rm" && rlang::is_bool(args[[2]])) {
    na_rm <- args[[2]]
    args <- args[1]; nms <- nms[1]
  }
  if (length(args) != 1L || nzchar(nms[[1]]) || !is_elementwise(args[[1]])) {
    return(NULL)
  }
  v <- rlang::eval_tidy(rlang::new_quosure(args[[1]], rlang::quo_get_env(quo)),
                        data = view)
  if (length(v) == 1L) v <- rep(v, nrow(view))
  if (length(v) != nrow(view)) return(NULL)
  if (fn %in% c("sum", "mean")) {
    if (is.object(v) || !(is.numeric(v) || is.logical(v))) return(NULL)
    s <- group_sum(if (is.double(v)) v else as.integer(v), g, na_rm)
    out <- s$sums
    if (fn == "mean") {
      # `mean(x, na.rm = TRUE)` divides by the number of values that were not
      # missing, so a neighbourhood of nothing but `NA` comes out `NaN`
      # a count needs no `rowsum()`: each row's place in the result, tabulated
      at <- match(g, s$keys)
      n <- tabulate(if (na_rm) at[!is.na(v)] else at, length(s$keys))
      out <- out / n
      if (is.double(v)) {
        # base's `mean()` of doubles then adds the mean of the residuals,
        # wherever the first pass came out finite
        resid <- group_sum(v - out[at], g, na_rm)$sums
        fin <- is.finite(out)
        out[fin] <- (out + resid / n)[fin]
      }
    }
  } else {
    if (!is.logical(v)) return(NULL)
    na <- if (anyNA(v)) group_sum(as.integer(is.na(v)), g)$sums > 0 else FALSE
    s <- group_sum(as.integer(v %in% (fn == "any")), g)
    hit <- s$sums > 0
    out <- if (fn == "any") hit else !hit
    if (!na_rm) out[!hit & na] <- NA
  }
  list(.of = s$keys, .abm_value = out)
}

#' `rowsum(x, g)` as `list(keys, sums)`, the keys sorted
#'
#' Where every key is distinct -- a mover reading the one cell it stands on --
#' each sum is of one value, which `rowsum()` makes as `0 + x`, so that is
#' done here without its sort and its character row names.
#' @noRd
group_sum <- function(x, g, na_rm = FALSE) {
  if (!anyDuplicated(g)) {
    o <- order(g)
    x <- x[o]
    if (na_rm) x[is.na(x)] <- 0L
    return(list(keys = as.integer(g[o]), sums = if (is.double(x)) 0 + x else x))
  }
  s <- rowsum(x, g, na.rm = na_rm)
  list(keys = as.integer(rownames(s)), sums = as.vector(s))
}

#' Is this rule exactly `n()` -- the size of the neighbourhood and nothing else?
#'
#' Only the bare count is treated as nought over an empty neighbourhood. `n()`
#' inside a larger expression is not: `sum(on) / n()` over nobody is `0 / 0`,
#' and answering `0` there would be a different claim than the arithmetic makes.
#' @noRd
is_bare_count <- function(quo) {
  expr <- rlang::quo_get_expr(quo)
  if (!rlang::is_call(expr) || length(expr) != 1L) return(FALSE)
  head <- expr[[1]]
  if (rlang::is_call(head, "::")) head <- head[[3]]
  rlang::is_symbol(head) && rlang::as_string(head) == "n"
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
