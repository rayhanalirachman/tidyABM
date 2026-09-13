# Relations: state that belongs to a pair of agents -----------------------

#' Declare a relation between agents
#'
#' A *relation* is a table of directed pairs of agents, each pair carrying
#' values of its own. It is where a quantity lives when it belongs to neither
#' agent alone but to the two of them together: how much this seller has
#' rationed *this* household, what this bank owes *that* one, how highly I rank
#' *you*. A model may declare several, by name, in
#' `abm_setup(relations = list(...))`, and they sit alongside the network, which
#' is unchanged.
#'
#' The network cannot hold this. It is one per model, it is undirected, and the
#' only values that attach to its edges come from [abm_draw()], which is a fresh
#' coin per tick that cannot read its own previous value. A relation is
#' directed -- `(from, to)` is a different row from `(to, from)` -- and its
#' values persist until a rule writes them or the row is removed.
#'
#' # What a relation gives a rule
#'
#' For a relation named `R` with a value column `v`, every place that sees a
#' *pair* of agents also sees:
#'
#' | name | meaning |
#' |---|---|
#' | `.R` | `TRUE` when the row `(me -> them)` exists |
#' | `.R_back` | `TRUE` when `(them -> me)` exists |
#' | `R_v` | the value on `(me -> them)`, `NA` if no row |
#' | `R_v_back` | the value on `(them -> me)` |
#'
#' "Me" is the focal agent and "them" the candidate in [abm_match()]'s `among`,
#' `weight` and `cost`, and in [abm_neighbours()]'s `within`; under a standing
#' match in [abm_rules()] and [abm_sequential()], "them" is `.partner`. There
#' `R_v ~ expr` *writes* the pair's value, and `R_v_back ~ expr` writes the
#' reverse row. That is the whole point: the number has one home, so two sides
#' of a transaction cannot drift apart.
#'
#' Rows are added and removed with [abm_link()] and [abm_unlink()] using
#' `via = "R"`, updated in bulk with [abm_pairs()], and read back after a run
#' with [abm_relations()]. [abm_global()], measures and [abm_tell()] evaluate
#' over the bare population and do not see relation columns; aggregate through
#' `abm_neighbours(within = .R)` first.
#'
#' @param edges A data frame with integer columns `from` and `to`, the `.id`s
#'   of the two agents, and optionally further columns holding each pair's
#'   starting values. `(from, to)` must be unique and `from != to`.
#' @param ... Named default values, one per value column: `unmet = 0`. A column
#'   named here but absent from `edges` is added at the default; a column in
#'   `edges` but not named here defaults to `NA` of its type. These defaults are
#'   what [abm_link()] fills in for a row it creates.
#'
#' @return An `abm_relation` object, for `abm_setup(relations = )`.
#' @seealso [abm_setup()], [abm_link()], [abm_pairs()], [abm_relations()].
#' @export
#' @examples
#' # each of 3 households buys from 2 of 4 firms (ids 4:7); nothing owed yet
#' who <- data.frame(from = c(1L, 1L, 2L, 2L, 3L, 3L),
#'                   to   = c(4L, 5L, 5L, 6L, 6L, 7L))
#' abm_relation(edges = who, unmet = 0)
abm_relation <- function(edges, ...) {
  defaults <- rlang::list2(...)
  if (!is.data.frame(edges) || !all(c("from", "to") %in% names(edges))) {
    abm_abort(
      "{.arg edges} must be a data frame with {.field from} and {.field to} columns.",
      class = "tidyABM_bad_relation"
    )
  }
  if (length(defaults) && !rlang::is_named(defaults)) {
    abm_abort("Default values in {.arg ...} must be named: {.code unmet = 0}.",
              class = "tidyABM_bad_relation")
  }
  for (nm in names(defaults)) {
    if (!is.atomic(defaults[[nm]]) || length(defaults[[nm]]) != 1L) {
      abm_abort(
        c("A relation value defaults to one atomic scalar.",
          "x" = "{.field {nm}} is {.cls {class(defaults[[nm]])[[1]]}} of length {length(defaults[[nm]])}."),
        class = "tidyABM_bad_relation"
      )
    }
  }

  edges <- tibble::as_tibble(edges)
  edges$from <- as.integer(edges$from)
  edges$to   <- as.integer(edges$to)
  if (anyNA(edges$from) || anyNA(edges$to)) {
    abm_abort("{.field from} and {.field to} must be agent {.code .id}s, not {.val NA}.",
              class = "tidyABM_bad_relation")
  }
  if (any(edges$from == edges$to)) {
    abm_abort("A relation pairs two different agents: {.code from == to} on row {which(edges$from == edges$to)[[1]]}.",
              class = "tidyABM_bad_relation")
  }
  key <- relation_key(edges$from, edges$to)
  if (anyDuplicated(key)) {
    d <- which(duplicated(key))[[1]]
    abm_abort(
      c("Each {.code (from, to)} pair appears once in a relation.",
        "x" = "({edges$from[[d]]}, {edges$to[[d]]}) is repeated."),
      class = "tidyABM_bad_relation"
    )
  }

  for (nm in names(defaults)) {
    if (!nm %in% names(edges)) edges[[nm]] <- rep_len(defaults[[nm]], nrow(edges))
  }
  cols <- setdiff(names(edges), c("from", "to"))
  for (cc in cols) {
    if (!is.atomic(edges[[cc]])) {
      abm_abort(
        c("Relation values are atomic columns.",
          "x" = "{.field {cc}} is {.cls {class(edges[[cc]])[[1]]}}."),
        class = "tidyABM_bad_relation"
      )
    }
    if (!cc %in% names(defaults)) defaults[[cc]] <- vctrs::vec_init(edges[[cc]], 1L)
  }
  new_abm_relation(edges[c("from", "to", cols)], cols, defaults[cols])
}

#' @noRd
new_abm_relation <- function(edges, cols, defaults) {
  structure(list(edges = edges, cols = cols, defaults = defaults),
            class = "abm_relation")
}

#' @export
print.abm_relation <- function(x, ...) {
  cli::cli_text("{.cls abm_relation} {nrow(x$edges)} pair{?s}")
  if (length(x$cols)) {
    for (cc in x$cols) {
      cli::cli_bullets(c("*" = "{.field {cc}}: {.cls {class(x$edges[[cc]])[[1]]}}, default {.val {x$defaults[[cc]]}}"))
    }
  } else {
    cli::cli_bullets(c("*" = "no value columns: the pair either exists or does not"))
  }
  invisible(x)
}

# --- naming --------------------------------------------------------------

#' The key one pair is looked up by
#' @noRd
relation_key <- function(from, to) paste(from, to)

#' Row indices of `(from, to)` pairs in a relation, `NA` where absent
#' @noRd
relation_lookup <- function(rel, from, to) {
  if (!nrow(rel$edges)) return(rep(NA_integer_, length(from)))
  match(relation_key(from, to), relation_key(rel$edges$from, rel$edges$to))
}

#' Every name a set of relations puts in scope
#' @noRd
relation_derived_names <- function(relations) {
  if (!length(relations)) return(character())
  unlist(lapply(names(relations), function(rn) {
    cols <- relations[[rn]]$cols
    c(paste0(".", rn), paste0(".", rn, "_back"),
      paste0(rn, "_", cols), paste0(rn, "_", cols, "_back"))
  }), use.names = FALSE)
}

#' Is this name a relation reference, and which?
#'
#' Returns `list(rel, col, back)` -- `col` is `NULL` for the existence forms
#' `.R` / `.R_back` -- or `NULL` when the name is not about any relation.
#' Longer relation names are tried first, so `a_b` beats `a` when both exist.
#' @noRd
resolve_relation_name <- function(name, relations) {
  if (!length(relations) || !rlang::is_string(name)) return(NULL)
  for (rn in names(relations)[order(-nchar(names(relations)))]) {
    if (identical(name, paste0(".", rn))) {
      return(list(rel = rn, col = NULL, back = FALSE))
    }
    if (identical(name, paste0(".", rn, "_back"))) {
      return(list(rel = rn, col = NULL, back = TRUE))
    }
    pre <- paste0(rn, "_")
    if (!startsWith(name, pre)) next
    rest <- substring(name, nchar(pre) + 1L)
    cols <- relations[[rn]]$cols
    if (rest %in% cols) return(list(rel = rn, col = rest, back = FALSE))
    if (endsWith(rest, "_back")) {
      fwd <- substring(rest, 1L, nchar(rest) - 5L)
      if (fwd %in% cols) return(list(rel = rn, col = fwd, back = TRUE))
    }
  }
  NULL
}

#' Does a quosure mention any relation-derived name?
#' @noRd
mentions_relation <- function(quo, relations) {
  if (is.null(quo) || !length(relations)) return(FALSE)
  any(all.vars(rlang::quo_get_expr(quo)) %in% relation_derived_names(relations))
}

# --- setup ---------------------------------------------------------------

#' Check the relations declared to `abm_setup()` against the population
#' @noRd
validate_relations <- function(relations, n, agent_cols, globals,
                               call = rlang::caller_env()) {
  if (is.null(relations)) return(NULL)
  if (!is.list(relations) || !length(relations) || !rlang::is_named(relations)) {
    abm_abort("{.arg relations} must be a named list of {.fn abm_relation} objects.",
              class = "tidyABM_bad_relation", call = call)
  }
  for (rn in names(relations)) {
    rel <- relations[[rn]]
    if (!inherits(rel, "abm_relation")) {
      abm_abort("{.arg relations${rn}} must be built with {.fn abm_relation}.",
                class = "tidyABM_bad_relation", call = call)
    }
    if (rn %in% c("partner", "own") || startsWith(rn, ".") || !nzchar(rn)) {
      abm_abort(
        c("{.val {rn}} cannot be a relation name.",
          "i" = "{.code partner_} and {.code own_} are the grammar's prefixes, and names starting with {.code .} are reserved."),
        class = "tidyABM_bad_relation", call = call
      )
    }
    ids <- c(rel$edges$from, rel$edges$to)
    bad <- ids[ids < 1L | ids > n]
    if (length(bad)) {
      abm_abort(
        c("Relation {.field {rn}} names an agent that does not exist.",
          "x" = "No agent has {.code .id} {bad[[1]]}; the model has {n}."),
        class = "tidyABM_bad_relation", call = call
      )
    }
  }
  derived <- relation_derived_names(relations)
  if (anyDuplicated(derived)) {
    abm_abort(
      c("Two relations put the same name in scope: {.field {derived[duplicated(derived)][[1]]}}.",
        "i" = "Rename one relation or one value column."),
      class = "tidyABM_bad_relation", call = call
    )
  }
  clash <- intersect(c(names(relations), derived), c(agent_cols, names(globals)))
  if (length(clash)) {
    abm_abort(
      c("Relation name{?s} {.field {clash}} collide{?s/} with an agent column or global.",
        "i" = "A relation {.field R} with value {.field v} puts {.code .R}, {.code R_v} and their {.code _back} forms in scope; none may already be a column."),
      class = "tidyABM_bad_relation", call = call
    )
  }
  relations
}

# --- the pair view -------------------------------------------------------

#' Add `.R`, `.R_back`, `R_<col>` and `R_<col>_back` to a (focal, candidate) view
#' @noRd
attach_relation_columns <- function(view, relations, focal_ids, cand_ids) {
  if (!length(relations)) return(view)
  for (rn in names(relations)) {
    rel <- relations[[rn]]
    fwd <- relation_lookup(rel, focal_ids, cand_ids)
    bck <- relation_lookup(rel, cand_ids, focal_ids)
    view[[paste0(".", rn)]] <- !is.na(fwd)
    view[[paste0(".", rn, "_back")]] <- !is.na(bck)
    for (cc in rel$cols) {
      view[[paste0(rn, "_", cc)]] <- rel$edges[[cc]][fwd]
      view[[paste0(rn, "_", cc, "_back")]] <- rel$edges[[cc]][bck]
    }
  }
  view
}

# --- writes --------------------------------------------------------------

#' Write one value column of a relation at the given pairs
#'
#' Every `(from, to)` must already be a row: a write is about a pair that
#' exists, and creating the pair is [abm_link()]'s job.
#' @noRd
write_relation <- function(rel, rn, col, from, to, values, step,
                           call = rlang::caller_env()) {
  idx <- relation_lookup(rel, from, to)
  miss <- which(is.na(idx))
  if (length(miss)) {
    i <- miss[[1]]
    abm_abort(
      c("{.fn {step}} writes {.field {rn}_{col}}, but agent {from[[i]]} is not related to agent {to[[i]]} via {.field {rn}}.",
        "i" = 'Create the pair first with {.code abm_link(via = "{rn}")}.'),
      class = "tidyABM_not_related", call = call
    )
  }
  values <- vctrs::vec_recycle(unname(values), length(idx))
  if (!col %in% names(rel$edges)) {
    rel$edges[[col]] <- vctrs::vec_init(values, nrow(rel$edges))
    rel$cols <- c(rel$cols, col)
    rel$defaults[[col]] <- vctrs::vec_init(values, 1L)
  }
  common <- vctrs::vec_cast_common(rel$edges[[col]], values)
  rel$edges[[col]] <- common[[1]]
  rel$edges[[col]][idx] <- common[[2]]
  rel
}

#' Add rows to a relation, filling value columns from rules or defaults
#'
#' Pairs that already exist are left exactly as they are: the rules apply to
#' the rows this call creates, which is what lets "link if absent, then add to
#' the balance" be two steps that each mean one thing.
#' @noRd
add_relation_rows <- function(rel, from, to, values = list()) {
  if (!length(from)) return(rel)
  from <- as.integer(from); to <- as.integer(to)
  # a pair already present is untouched; one requested twice is created once
  keep <- is.na(relation_lookup(rel, from, to)) & !duplicated(relation_key(from, to))
  if (!any(keep)) return(rel)
  new <- tibble::tibble(from = from[keep], to = to[keep])
  values <- lapply(values, function(v) vctrs::vec_recycle(unname(v), length(from))[keep])
  for (cc in rel$cols) {
    new[[cc]] <- if (cc %in% names(values)) {
      vctrs::vec_recycle(values[[cc]], nrow(new))
    } else {
      rep_len(rel$defaults[[cc]], nrow(new))
    }
  }
  for (cc in setdiff(names(values), rel$cols)) {
    v <- vctrs::vec_recycle(values[[cc]], nrow(new))
    rel$edges[[cc]] <- vctrs::vec_init(v, nrow(rel$edges))
    new[[cc]] <- v
    rel$cols <- c(rel$cols, cc)
    rel$defaults[[cc]] <- vctrs::vec_init(v, 1L)
  }
  rel$edges <- dplyr::bind_rows(rel$edges, new[names(rel$edges)])
  rel
}

#' Remove rows from a relation
#' @noRd
drop_relation_rows <- function(rel, from, to) {
  if (!length(from) || !nrow(rel$edges)) return(rel)
  gone <- relation_key(rel$edges$from, rel$edges$to) %in% relation_key(from, to)
  rel$edges <- rel$edges[!gone, , drop = FALSE]
  rel
}

#' Drop every row that touches a removed agent
#' @noRd
prune_relations <- function(relations, removed) {
  if (!length(relations) || !length(removed)) return(relations)
  for (rn in names(relations)) {
    e <- relations[[rn]]$edges
    keep <- !(e$from %in% removed | e$to %in% removed)
    relations[[rn]]$edges <- e[keep, , drop = FALSE]
  }
  relations
}

#' The relation a step names, or a clear error
#' @noRd
relation_or_abort <- function(state, via, fn, call = rlang::caller_env()) {
  if (!rlang::is_string(via)) {
    abm_abort("{.arg via} must be the name of one relation, as a string.",
              class = "tidyABM_no_relation", call = call)
  }
  if (!via %in% names(state$relations)) {
    have <- names(state$relations)
    abm_abort(
      c("{.fn {fn}} names a relation {.field {via}} that the model does not declare.",
        "i" = if (length(have)) "Declared: {.field {have}}."
              else "Declare one with {.code abm_setup(relations = list({via} = abm_relation(...)))}."),
      class = "tidyABM_no_relation", call = call
    )
  }
  state$relations[[via]]
}

# --- abm_pairs -----------------------------------------------------------

#' Update every pair of a relation
#'
#' `abm_pairs()` is [abm_rules()] for a relation: each rule is a `value ~ expr`
#' formula evaluated once per row of the relation named by `via`, all rules
#' simultaneously. It is how a value that belongs to a pair changes for every
#' pair at once -- interest accruing on every loan, last month's rationing
#' being forgotten -- with one write and nothing to keep aligned.
#'
#' Inside a rule the relation's own value columns are visible by name, the two
#' agents' `.id`s as `from` and `to`, and their agent columns as `from_<col>`
#' and `to_<col>`. Globals are in scope. A target that is not yet a column of
#' the relation is created.
#'
#' @param via The relation's name, a string.
#' @param ... One or more `value ~ expr` rules.
#' @param .when Optional condition, evaluated per row; rows where it does not
#'   hold are left as they are.
#'
#' @return An `abm_pairs` step object.
#' @seealso [abm_relation()], [abm_rules()], [abm_go()].
#' @export
#' @examples
#' abm_pairs(via = "loans", balance ~ balance * 1.004)
#' abm_pairs(via = "sellers", unmet ~ 0)
#' abm_pairs(via = "loans", balance ~ 0, .when = to_bankrupt)
abm_pairs <- function(via, ..., .when = NULL) {
  if (!rlang::is_string(via)) {
    abm_abort("{.arg via} must be the name of one relation, as a string.",
              class = "tidyABM_no_relation")
  }
  rules <- collect_rules(rlang::list2(...), "abm_pairs")
  for (r in rules) {
    if (r$target %in% c("from", "to") || startsWith(r$target, "from_") ||
        startsWith(r$target, "to_")) {
      abm_abort(
        c("{.fn abm_pairs} writes a relation's value columns.",
          "x" = "{.field {r$target}} names one of the two agents, not a value on the pair."),
        class = "tidyABM_bad_formula"
      )
    }
  }
  structure(list(via = via, rules = rules,
                 when = enquo_or_null(rlang::enquo(.when))),
            class = c("abm_pairs", "abm_step"))
}

#' @noRd
run_pairs <- function(step, state) {
  rel <- relation_or_abort(state, step$via, "abm_pairs")
  if (!nrow(rel$edges)) return(state)
  combined <- bind_groups(state$groups)
  view <- pairs_view(rel, combined)

  keep <- rep(TRUE, nrow(view))
  if (!is.null(step$when)) {
    keep <- eval_over_view(step$when, view, state$globals)
    if (length(keep) == 1L) keep <- rep(keep, nrow(view))
    if (!is.logical(keep)) {
      abm_abort(
        c("{.arg .when} must be a condition.",
          "x" = "{.code {deparse1(rlang::quo_get_expr(step$when))}} returned {.cls {class(keep)[[1]]}}."),
        class = "tidyABM_bad_when"
      )
    }
    keep[is.na(keep)] <- FALSE
  }

  # all rules see the state at the start of the step, as abm_rules() does
  values <- lapply(step$rules, function(r) {
    v <- eval_over_view(r$quo, view, state$globals)
    if (!is.list(v) && !is.null(names(v))) v <- unname(v)
    vctrs::vec_recycle(v, nrow(view))
  })
  for (i in seq_along(step$rules)) {
    target <- step$rules[[i]]$target
    v <- values[[i]]
    if (!target %in% names(rel$edges)) {
      rel$edges[[target]] <- vctrs::vec_init(v, nrow(rel$edges))
      rel$cols <- c(rel$cols, target)
      rel$defaults[[target]] <- vctrs::vec_init(v, 1L)
    }
    common <- vctrs::vec_cast_common(rel$edges[[target]], v)
    rel$edges[[target]] <- common[[1]]
    rel$edges[[target]][keep] <- common[[2]][keep]
  }
  state$relations[[step$via]] <- rel
  state
}

#' One row per pair, with both agents' columns alongside the pair's values
#' @noRd
pairs_view <- function(rel, combined) {
  view <- rel$edges
  fi <- match(view$from, combined$.id)
  ti <- match(view$to, combined$.id)
  for (nm in names(combined)) {
    view[[paste0("from_", nm)]] <- combined[[nm]][fi]
    view[[paste0("to_", nm)]]   <- combined[[nm]][ti]
  }
  view
}

#' @export
print.abm_pairs <- function(x, ...) {
  cli::cli_text("{.cls abm_pairs} via {.field {x$via}}")
  for (r in x$rules) {
    cli::cli_bullets(c("*" = "{.field {r$target}} ~ {.code {deparse1(rlang::quo_get_expr(r$quo))}}"))
  }
  if (!is.null(x$when)) {
    cli::cli_bullets(c("*" = ".when = {.code {deparse1(rlang::quo_get_expr(x$when))}}"))
  }
  invisible(x)
}

# --- results -------------------------------------------------------------

#' Read a run's relations back
#'
#' The state of each relation at the end of the run, as a tibble of `from`,
#' `to` and the value columns -- the counterpart of [abm_edges()] for the
#' network. Under `abm_run(reps =)` or `params =`, every table carries the
#' `.run`, `.rep` and parameter columns in front.
#'
#' @param x An `abm_result` from [abm_run()].
#' @param via Optional relation name. Omitted, the whole named list is
#'   returned; `NULL` when the model declared no relations.
#' @return A named list of tibbles, or one tibble.
#' @seealso [abm_relation()], [abm_edges()], [abm_globals()].
#' @export
#' @examples
#' who <- data.frame(from = 1:2, to = c(3L, 3L))
#' m <- abm_setup(agents = abm_agents(n = 3, x = 1),
#'                relations = list(buys = abm_relation(who, times = 0)))
#' r <- abm_run(m, abm_go(abm_pairs(via = "buys", times ~ times + 1)),
#'              ticks = 3, seed = 1)
#' abm_relations(r, "buys")
abm_relations <- function(x, via = NULL) {
  if (!inherits(x, "abm_result")) {
    abm_abort("{.arg x} must be the result of {.fn abm_run}.",
              class = "tidyABM_bad_result")
  }
  rels <- attr(x, "relations")
  if (is.null(via)) return(rels)
  if (!rlang::is_string(via) || !via %in% names(rels)) {
    abm_abort(
      c("No relation named {.field {via}} in this result.",
        "i" = if (length(rels)) "Declared: {.field {names(rels)}}."
              else "The model declared no relations."),
      class = "tidyABM_no_relation"
    )
  }
  rels[[via]]
}

#' The relations' edge tables, for the result
#' @noRd
relation_tables <- function(relations) {
  if (!length(relations)) return(NULL)
  lapply(relations, function(rel) rel$edges)
}
