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
  combined <- bind_groups(state$groups)
  for (nm in names(state$groups)) {
    g <- state$groups[[nm]]
    if (nrow(g) == 0L) next
    aug <- augment_group(g, state$match, combined)
    parents <- eval_condition(step$when, aug, state$globals)
    if (!any(parents)) next

    kids <- g[parents, , drop = FALSE]
    kids$.id <- state$next_id + seq_len(nrow(kids)) - 1L
    state$next_id <- state$next_id + nrow(kids)

    if (!is.null(step$cost)) {
      pg <- aug[parents, , drop = FALSE]
      for (r in step$cost) {
        val <- eval_rule(r, pg, state$globals, grouped = FALSE)
        if (length(val) == 1L) val <- rep(val, nrow(pg))
        g[[r$target]][parents] <- val
        kids[[r$target]] <- val
      }
    }
    if (!is.null(step$inherit)) {
      pg <- aug[parents, , drop = FALSE]
      for (r in step$inherit) {
        val <- eval_rule(r, pg, state$globals, grouped = FALSE)
        if (length(val) == 1L) val <- rep(val, nrow(pg))
        kids[[r$target]] <- val
      }
    }
    state$groups[[nm]] <- dplyr::bind_rows(g, kids)
    newborns <- c(newborns, kids$.id)
    parents_of <- c(parents_of, g$.id[parents])
  }
  attach_newborns(step, state, newborns, parents_of)
}

birth_by_count <- function(step, state) {
  if (step$n == 0L) return(state)
  nm <- names(state$groups)[[1]]
  g <- state$groups[[nm]]

  if (nrow(g) == 0L) {
    abm_abort("{.fn abm_birth} cannot add agents to an empty group.",
              class = "tidyABM_empty_group")
  }
  template <- g[sample(nrow(g), step$n, replace = TRUE), , drop = FALSE]
  template$.id <- state$next_id + seq_len(step$n) - 1L
  template$.group <- nm
  state$next_id <- state$next_id + step$n

  for (r in c(step$cost, step$inherit)) {
    val <- eval_rule(r, template, state$globals, grouped = FALSE)
    template[[r$target]] <- val
  }
  state$groups[[nm]] <- dplyr::bind_rows(g, template)
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
  for (k in seq_along(newborns)) {
    id <- newborns[[k]]
    existing <- combined[combined$.id != id & !combined$.id %in% newborns, ,
                         drop = FALSE]
    target <- if (step$attach_via$from == "parent") {
      parents[[k]]
    } else {
      attach_target(step$attach_via, existing, state$edges)
    }
    if (is.na(target)) next
    state$edges <- dplyr::bind_rows(
      state$edges,
      tibble::tibble(from = as.integer(id), to = as.integer(target))
    )
  }
  state
}

#' Run one abm_death step
#' @noRd
run_death <- function(step, state) {
  removed <- integer()
  combined <- bind_groups(state$groups)
  for (nm in names(state$groups)) {
    g <- state$groups[[nm]]
    if (nrow(g) == 0L) next
    aug <- augment_group(g, state$match, combined)
    dead <- eval_condition(step$when, aug, state$globals)
    if (!any(dead)) next
    removed <- c(removed, g$.id[dead])
    state$groups[[nm]] <- g[!dead, , drop = FALSE]
  }
  if (length(removed) && step$prune_edges && !is.null(state$edges)) {
    keep <- !(state$edges$from %in% removed | state$edges$to %in% removed)
    state$edges <- state$edges[keep, , drop = FALSE]
  }
  state
}
