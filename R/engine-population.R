# Births and deaths ------------------------------------------------------

#' Run one abm_birth step
#' @noRd
run_birth <- function(step, state) {
  if (!is.null(step$when)) {
    state <- birth_by_condition(step, state)
  } else {
    state <- birth_by_count(step, state)
  }
  state
}

birth_by_condition <- function(step, state) {
  newborns <- integer()
  parents_of <- integer()
  # the other groups are only read through `partner_<col>`, under a match
  quos <- planned(step, "quos", c(list(step$when, step$times),
                                  lapply(c(step$cost, step$inherit), `[[`, "quo")))
  pvars <- planned(step, "pvars", partner_vars(quos))
  combined <- if (!is.null(state$match) && length(pvars)) bind_groups(state$groups, pvars)
  # the match columns are looked up per agent, so only when they are read
  bare <- !length(state$relations) && !planned(step, "uses_match", reads_match(quos))
  applies <- condition_routes(step, state)
  for (nm in names(state$groups)) {
    g <- state$groups[[nm]]
    if (nrow(g) == 0L) next
    if (!applies[[nm]]) next
    aug <- if (bare) g else
      augment_group(g, state$match, combined, state$relations, partner = pvars,
                    match_cols = planned(step, "mcols", match_cols_read(quos, state)))
    parents <- eval_condition(step$when, aug, state$globals)
    if (!any(parents)) next

    pg <- vctrs::vec_slice(aug, parents)
    reps <- offspring_counts(step, pg, state$globals)
    if (!sum(reps)) next
    # one row per offspring, so a parent having three of them is three rows and
    # `inherit` is evaluated once for each -- siblings mutate separately
    kid_rows <- rep(which(parents), reps)
    kids <- vctrs::vec_slice(g, kid_rows)
    kids <- set_col(kids, ".id", state$next_id + seq_len(nrow(kids)) - 1L)
    state$next_id <- state$next_id + nrow(kids)

    if (!is.null(step$cost)) {
      for (r in step$cost) {
        val <- eval_rule(r, pg, state$globals, grouped = FALSE)
        if (length(val) == 1L) val <- rep(val, nrow(pg))
        g <- set_col(g, r$target, replace(g[[r$target]], parents, val))
        kids <- set_col(kids, r$target, rep(val, reps))
      }
    }
    if (!is.null(step$inherit)) {
      kg <- aug[kid_rows, , drop = FALSE]
      for (r in step$inherit) {
        val <- eval_rule(r, kg, state$globals, grouped = FALSE)
        if (length(val) == 1L) val <- rep(val, nrow(kg))
        kids <- set_col(kids, r$target, val)
      }
    }
    state$groups[[nm]] <- append_rows(g, kids)
    newborns <- c(newborns, kids$.id)
    parents_of <- c(parents_of, rep(g$.id[parents], reps))
  }
  attach_newborns(step, state, newborns, parents_of)
}

#' `vctrs::vec_rbind(g, kids)` for offspring sliced from their own group
#'
#' The two have the same columns; when every one is a bare atomic vector of
#' the same type on both sides, stacking them is `c()` column by column.
#' Anything else goes to `vec_rbind()`.
#' @noRd
append_rows <- function(g, kids) {
  a <- unclass(g)
  b <- unclass(kids)
  if (identical(names(a), names(b)) &&
      all(vapply(seq_along(a), function(j) {
        x <- a[[j]]; y <- b[[j]]
        is.atomic(x) && is.null(attributes(x)) && is.null(attributes(y)) &&
          typeof(x) == typeof(y)
      }, NA))) {
    return(new_tbl(Map(c, a, b), length(a[[1]]) + length(b[[1]])))
  }
  vctrs::vec_rbind(g, kids)
}

#' How many offspring each reproducing parent has
#'
#' One each unless `times` says otherwise. `NA` and negative counts are no
#' offspring rather than an error, so that a fertility drawn from a distribution
#' does not have to be clamped by the model.
#' @noRd
offspring_counts <- function(step, pg, globals) {
  if (is.null(step$times)) return(rep(1L, nrow(pg)))
  val <- eval_rule(list(quo = step$times), pg, globals, grouped = FALSE)
  if (length(val) == 1L) val <- rep(val, nrow(pg))
  if (!is.numeric(val)) {
    abm_abort(
      c("{.arg times} must be a count.",
        "x" = "{.code {deparse1(rlang::quo_get_expr(step$times))}} returned {.cls {class(val)[[1]]}}."),
      class = "tidyABM_bad_times"
    )
  }
  val <- as.integer(round(val))
  val[is.na(val) | val < 0L] <- 0L
  val
}

birth_by_count <- function(step, state) {
  if (step$n == 0L) return(state)
  nm <- names(state$groups)[[1]]
  g <- state$groups[[nm]]

  if (nrow(g) == 0L) {
    abm_abort("{.fn abm_birth} cannot add agents to an empty group.",
              class = "tidyABM_empty_group")
  }
  template <- g[sample.int(nrow(g), step$n, replace = TRUE), , drop = FALSE]
  template$.id <- state$next_id + seq_len(step$n) - 1L
  template$.group <- nm
  state$next_id <- state$next_id + step$n

  for (r in c(step$cost, step$inherit)) {
    val <- eval_rule(r, template, state$globals, grouped = FALSE)
    template[[r$target]] <- val
  }
  state$groups[[nm]] <- vctrs::vec_rbind(g, template)
  attach_newborns(step, state, template$.id, rep(NA_integer_, step$n))
}

#' Connect newborn agents to the network, if the step asked for it
#' @noRd
attach_newborns <- function(step, state, newborns, parents = NULL) {
  if (is.null(step$attach_via) || !length(newborns)) return(state)
  if (step$attach_via$from == "parent" && all(is.na(parents %||% NA))) {
    abm_abort(
      c('{.code from = "parent"} needs a parent to attach to.',
        "i" = "It only works with {.code abm_birth(when = ...)}, which clones an existing agent."),
      class = "tidyABM_no_parent"
    )
  }
  if (is.null(state$edges)) {
    abm_abort(
      c("{.arg attach_via} needs a network.",
        "i" = "Add one with {.code abm_setup(..., network = abm_network(...))}."),
      class = "tidyABM_no_network"
    )
  }
  combined <- bind_groups(state$groups)
  links <- step$links %||% 1L
  # the same for every newborn, since each one is in `newborns`: built once,
  # and the new edges bound once at the end, rather than an O(population)
  # copy of each per newborn. `random_edge` draws from the edges as they
  # grow, so it still sees each newborn's links as they are made.
  existing <- combined[!combined$.id %in% newborns, , drop = FALSE]
  grow <- identical(step$attach_via$from, "random_edge")
  new_from <- new_to <- vector("list", length(newborns))
  added <- FALSE
  for (k in seq_along(newborns)) {
    id <- newborns[[k]]
    # A newborn joins the population, not the cohort it was born with: its
    # siblings are excluded from every target, so a clutch does not link to
    # itself and count as a neighbourhood.
    targets <- if (step$attach_via$from == "parent") {
      c(parents[[k]],
        parent_neighbours(state$edges, parents[[k]], links - 1L,
                          exclude = c(id, newborns)))
    } else {
      attach_targets(step$attach_via, existing, state$edges, links)
    }
    targets <- unique(targets[!is.na(targets)])
    if (!length(targets)) next
    if (grow) {
      state$edges <- vctrs::vec_rbind(
        state$edges,
        edge_tbl(rep(as.integer(id), length(targets)), as.integer(targets))
      )
    } else {
      new_from[[k]] <- rep(as.integer(id), length(targets))
      new_to[[k]] <- as.integer(targets)
    }
    added <- TRUE
  }
  if (added && !grow) {
    state$edges <- vctrs::vec_rbind(
      state$edges,
      edge_tbl(as.integer(unlist(new_from)), as.integer(unlist(new_to))))
  }
  if (added) state <- invalidate_draws(state, "abm_birth")
  state
}

#' Which groups a birth or death step's `when` is about, per
#' [condition_applies()], kept in the plan while the column and global names
#' stay the same
#' @noRd
condition_routes <- function(step, state) {
  planned_for(step, "applies", schema_key(state), {
    all_cols <- model_columns(state$groups)
    vapply(state$groups, function(g) {
      condition_applies(step$when, g, all_cols, state$globals)
    }, NA)
  })
}

#' Run one abm_death step
#' @noRd
run_death <- function(step, state) {
  removed <- integer()
  # the other groups are only read through `partner_<col>`, under a match
  pvars <- planned(step, "pvars", partner_vars(list(step$when)))
  combined <- if (!is.null(state$match) && length(pvars)) bind_groups(state$groups, pvars)
  # the match columns are looked up per agent, so only when they are read
  bare <- !length(state$relations) &&
    !planned(step, "uses_match", reads_match(list(step$when)))
  # a condition about a column this group has not got is not about this group
  applies <- condition_routes(step, state)
  for (nm in names(state$groups)) {
    g <- state$groups[[nm]]
    if (nrow(g) == 0L) next
    if (!applies[[nm]]) next
    aug <- if (bare) g else
      augment_group(g, state$match, combined, state$relations, partner = pvars,
                    match_cols = planned(step, "mcols", match_cols_read(list(step$when), state)))
    dead <- eval_condition(step$when, aug, state$globals)
    if (!any(dead)) next
    removed <- c(removed, g$.id[dead])
    state$groups[[nm]] <- vctrs::vec_slice(g, !dead)
  }
  # ids only grow, so the dead of a lattice model (born after its cells were
  # wired) are past every endpoint, and the edges need no scan
  if (length(removed) && step$prune_edges && length(state$edges$from)) {
    # the highest endpoint, kept with the edges it was taken over
    if (!identical(state$edge_max$edges, state$edges)) {
      state$edge_max <- list(edges = state$edges,
                             max = max(state$edges$from, state$edges$to))
    }
  }
  if (length(removed) && step$prune_edges && length(state$edges$from) &&
      min(removed) <= state$edge_max$max) {
    keep <- !(state$edges$from %in% removed | state$edges$to %in% removed)
    # on a lattice the dead are rarely an endpoint: the patches are
    if (!all(keep)) state$edges <- state$edges[keep, , drop = FALSE]
  }
  # a dead agent holds no relation: its rows go, on either side
  state$relations <- prune_relations(state$relations, removed)
  state
}
