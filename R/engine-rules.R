# Rule execution ---------------------------------------------------------

#' Union of the agent columns across all groups
#' @noRd
model_columns <- function(groups) {
  unique(unlist(lapply(groups, names), use.names = FALSE))
}

#' Does a rule apply to this group?
#'
#' A rule applies when every agent column it mentions exists in the group. Names
#' that are globals, package-managed (`.role` and friends), partner columns, or
#' simply variables from the environment the rule was written in do not count.
#' @noRd
rule_applies <- function(rule, g, all_cols, globals) {
  vars <- setdiff(rule$vars,
                  c(".id", ".group", ".group_id", ".partner", ".role",
                    names(globals)))
  vars <- vars[!startsWith(vars, "partner_")]
  vars <- intersect(vars, all_cols)
  if (!all(vars %in% names(g))) return(FALSE)
  if (rule$target %in% all_cols && !(rule$target %in% names(g))) return(FALSE)
  TRUE
}

#' Add match information and partner columns to a group tibble
#' @noRd
augment_group <- function(g, match_state, combined) {
  if (is.null(match_state)) {
    g$.group_id <- seq_len(nrow(g))
    g$.role <- NA_character_
    g$.partner <- NA_integer_
    return(g)
  }
  if (nrow(match_state$match) == 0L) {
    # a match step ran and paired nobody. The partner columns still have to
    # exist, all missing, so that a tick where nobody meets anyone does not
    # crash the rules written for the ticks where they do.
    g$.group_id <- NA_integer_
    g$.role <- NA_character_
    g$.partner <- NA_integer_
    if (match_state$size == 2L) {
      for (nm in setdiff(names(combined), c(".id", ".group"))) {
        g[[paste0("partner_", nm)]] <- combined[[nm]][NA_integer_][rep(1L, nrow(g))]
      }
    }
    return(g)
  }
  m <- match_state$match
  idx <- match(g$.id, m$.id)
  g$.partner  <- m$.partner[idx]
  g$.role     <- m$.role[idx]
  g$.group_id <- m$.group_id[idx]

  if (match_state$size == 2L) {
    pcols <- setdiff(names(combined), c(".id", ".group"))
    pidx <- match(g$.partner, combined$.id)
    for (nm in pcols) {
      g[[paste0("partner_", nm)]] <- combined[[nm]][pidx]
    }
  }
  g
}

#' Evaluate one rule against an augmented group tibble
#' @noRd
eval_rule <- function(rule, aug, globals, grouped) {
  quo <- rule$quo
  env <- rlang::quo_get_env(quo)
  if (length(globals)) env <- rlang::new_environment(globals, parent = env)
  quo <- rlang::quo_set_env(quo, env)

  dat <- if (grouped) dplyr::group_by(aug, .data$.group_id) else aug
  out <- dplyr::mutate(dat, .abm_value = !!quo)
  dplyr::pull(dplyr::ungroup(out), ".abm_value")
}

#' Write a computed vector into a group column, for participating rows only
#' @noRd
assign_rule <- function(g, target, value, rows) {
  if (length(value) == 1L) value <- rep(value, nrow(g))
  if (!target %in% names(g)) {
    g[[target]] <- vctrs::vec_init(value, nrow(g))
  }
  if (all(rows)) {
    g[[target]] <- value
  } else {
    g[[target]] <- vctrs::vec_cast_common(g[[target]], value)[[1]]
    value <- vctrs::vec_cast(value, g[[target]])
    g[[target]][rows] <- value[rows]
  }
  g
}

#' Run one abm_rules step across every group
#' @noRd
run_rules <- function(step, state) {
  combined <- bind_groups(state$groups)
  all_cols <- setdiff(model_columns(state$groups), c(".id", ".group"))
  active <- !is.null(state$match) && nrow(state$match$match) > 0L
  # When a match is standing, rules are evaluated group-by-group: per pair for
  # size 2, per group for size > 2, per agent for the directional modes. That is
  # what makes `sample(x, 1)` inside a rule mean "once per pair" rather than
  # "once for the whole population".
  grouped <- active && (step$scope %||% "match") == "match"

  applied_anywhere <- FALSE
  for (nm in names(state$groups)) {
    g <- state$groups[[nm]]
    if (nrow(g) == 0L) next
    aug <- augment_group(g, state$match, combined)
    rows <- if (active && grouped) !is.na(aug$.group_id) else rep(TRUE, nrow(g))

    todo <- Filter(function(r) rule_applies(r, g, all_cols, state$globals),
                   step$rules)
    if (!length(todo)) next
    applied_anywhere <- TRUE

    values <- lapply(todo, eval_rule, aug = aug, globals = state$globals,
                     grouped = grouped)
    for (i in seq_along(todo)) {
      g <- assign_rule(g, todo[[i]]$target, values[[i]], rows)
    }
    state$groups[[nm]] <- g
  }

  if (!applied_anywhere) {
    abm_abort(
      c("An {.fn abm_rules} step applied to no agent group.",
        "x" = "Rule{?s} {.code {rule_labels(step)}} mention{?s/} columns that no group has.",
        "i" = "Available columns: {.field {all_cols}}."),
      class = "tidyABM_unapplied_rule"
    )
  }
  state
}

#' Run one abm_global step
#' @noRd
run_global <- function(step, state) {
  combined <- bind_groups(state$groups)
  for (r in step$rules) {
    # Globals go through the same dplyr mask as rules, so `n()` means the
    # population size here just as it does inside `abm_rules()`.
    quo <- rlang::quo_set_env(
      r$quo,
      rlang::new_environment(state$globals, parent = rlang::quo_get_env(r$quo))
    )
    val <- dplyr::pull(dplyr::reframe(combined, .abm_value = !!quo),
                       ".abm_value")
    if (length(val) != 1L) {
      abm_abort(
        c("{.fn abm_global} rules must collapse to a single value.",
          "x" = "{.code {r$target} ~ {deparse1(rlang::quo_get_expr(r$quo))}} returned {length(val)} values.",
          "i" = "Wrap it in an aggregate such as {.fn sum} or {.fn mean}."),
        class = "tidyABM_bad_global"
      )
    }
    state$globals[[r$target]] <- val
  }
  state
}

#' Run one abm_sequential step
#' @noRd
run_sequential <- function(step, state) {
  all_cols <- setdiff(model_columns(state$groups), c(".id", ".group"))
  order_ids <- unlist(lapply(state$groups, function(g) g$.id), use.names = FALSE)
  if (!length(order_ids)) return(state)
  order_ids <- sample(order_ids)

  group_of <- rep(names(state$groups),
                  vapply(state$groups, nrow, integer(1)))
  names(group_of) <- unlist(lapply(state$groups, function(g) g$.id),
                            use.names = FALSE)

  for (id in order_ids) {
    gname <- group_of[[as.character(id)]]
    g <- state$groups[[gname]]
    i <- match(id, g$.id)
    if (is.na(i)) next
    row <- g[i, , drop = FALSE]

    # A rule whose target is a global still has to be routed: a rule written
    # for buyers must not be evaluated in a seller's row just because the
    # thing it writes to happens to be shared.
    todo <- Filter(function(r) rule_applies(r, g, all_cols, state$globals),
                   step$rules)
    if (!length(todo)) next

    # Rules cascade *within* the agent: this is the one-agent-at-a-time step,
    # so a rule sees what the rule above it just wrote, both to the agent's own
    # row and to the globals. (`abm_rules()` is the simultaneous one.)
    for (r in todo) {
      value <- eval_rule(r, row, state$globals, grouped = FALSE)
      if (r$target %in% names(state$globals)) {
        state$globals[[r$target]] <- value[[1]]
      } else {
        g <- assign_rule(g, r$target, value, rows = seq_len(nrow(g)) == i)
        row <- g[i, , drop = FALSE]
      }
    }
    state$groups[[gname]] <- g
  }
  state
}
