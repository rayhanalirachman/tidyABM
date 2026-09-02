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
eval_rule <- function(rule, aug, globals, grouped, by = NULL) {
  quo <- rule$quo
  env <- rlang::quo_get_env(quo)
  if (length(globals)) env <- rlang::new_environment(globals, parent = env)
  quo <- rlang::quo_set_env(quo, env)

  dat <- if (!is.null(by)) dplyr::group_by(aug, .data[[by]])
         else if (grouped) dplyr::group_by(aug, .data$.group_id)
         else aug
  out <- dplyr::mutate(dat, .abm_value = !!quo)
  dplyr::pull(dplyr::ungroup(out), ".abm_value")
}

#' Write a computed vector into a group column, for participating rows only
#' @noRd
assign_rule <- function(g, target, value, rows) {
  # a value looked up out of a named global (`price[good]`) arrives carrying the
  # global's names; an agent column is not a lookup table, so they come off here
  # rather than travelling through the whole run and into the result.
  if (!is.list(value) && !is.null(names(value))) value <- unname(value)
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
  if (nrow(combined) == 0L) return(state)
  if (!is.null(step$by)) return(run_rules_by(step, state, combined))
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

#' Run one abm_rules step grouped by an agent column
#'
#' `.by` partitions the whole population, which may cut across agent groups, so
#' the rules are evaluated once over the combined tibble and the results are
#' scattered back by `.id`. That is also what makes the partition mutable: an
#' agent that writes a new value into the `.by` column has changed which group
#' it is in, and the next step sees the new one.
#' @noRd
run_rules_by <- function(step, state, combined) {
  by <- by_columns(step$by)
  if (length(by) != 1L || !by %in% names(combined)) {
    abm_abort(
      c("{.arg .by} must name one existing agent column.",
        "x" = "No column {.field {by}}."),
      class = "tidyABM_missing_column"
    )
  }
  aug <- augment_group(combined, state$match, combined)
  # `NA` is not a group. An agent with no value for the `.by` column sits the
  # step out and keeps what it had, the way `NA` does in `.order`.
  taking_part <- !is.na(combined[[by]])
  for (r in step$rules) {
    value <- eval_rule(r, aug, state$globals, grouped = FALSE, by = by)
    if (length(value) == nrow(combined) && !all(taking_part)) {
      value[!taking_part] <- if (r$target %in% names(combined))
        combined[[r$target]][!taking_part] else NA
    }
    for (nm in names(state$groups)) {
      g <- state$groups[[nm]]
      if (nrow(g) == 0L) next
      idx <- match(g$.id, combined$.id)
      g <- assign_rule(g, r$target, value[idx], taking_part[idx])
      state$groups[[nm]] <- g
    }
    combined[[r$target]] <- value
    aug[[r$target]] <- value
  }
  state
}

#' Run one abm_global step
#' @noRd
run_global <- function(step, state) {
  combined <- bind_groups(state$groups)
  if (!is.null(step$by)) return(run_global_by(step, state, combined))
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

#' The keys a `.by` index names
#'
#' Either a vector given straight, or the distinct values of an agent column.
#' A column-derived index keeps the keys the global already has, so a category
#' that empties this tick keeps its last value instead of disappearing.
#' @noRd
global_keys <- function(by_quo, combined, current, globals) {
  expr <- rlang::quo_get_expr(by_quo)
  if (rlang::is_symbol(expr) && rlang::as_string(expr) %in% names(combined)) {
    vals <- combined[[rlang::as_string(expr)]]
    keys <- sort(unique(vals[!is.na(vals)]))
    if (!is.null(names(current))) {
      keys <- unique(c(cast_keys(names(current), keys), keys))
    }
    return(keys)
  }
  env <- rlang::quo_get_env(by_quo)
  if (length(globals)) env <- rlang::new_environment(globals, parent = env)
  keys <- rlang::eval_tidy(rlang::quo_set_env(by_quo, env), data = combined)
  if (!length(keys)) {
    abm_abort("{.arg .by} named no keys.", class = "tidyABM_empty_by")
  }
  keys
}

#' Character keys, back in the type of the keys found in the data
#' @noRd
cast_keys <- function(chr, like) {
  out <- switch(
    class(like)[[1]],
    integer   = suppressWarnings(as.integer(chr)),
    numeric   = suppressWarnings(as.numeric(chr)),
    logical   = as.logical(chr),
    character = chr,
    factor    = chr,
    NULL
  )
  if (is.null(out) || anyNA(out)) chr[0] else out
}

#' Run one abm_global step with a `.by` index
#'
#' The global becomes a named vector. Each key is evaluated against the whole
#' population, a stimulus balance is about the colony, not about the workers on
#' one task, with `.key` bound to the key and the global's own name bound to
#' that key's current value, so the rule reads exactly as the scalar version of
#' it does.
#' @noRd
run_global_by <- function(step, state, combined) {
  for (r in step$rules) {
    current <- state$globals[[r$target]]
    if (!is.null(current) && length(current) > 1L && is.null(names(current))) {
      abm_abort(
        c("{.arg .by} needs the global to be a named vector or a single value.",
          "x" = "{.field {r$target}} holds {length(current)} unnamed values."),
        class = "tidyABM_bad_global"
      )
    }
    keys <- global_keys(step$by, combined, current, state$globals)
    out <- if (is.null(names(current))) NULL else current

    for (k in keys) {
      key_chr <- as.character(k)
      here <- if (is.null(names(current))) {
        if (length(current) == 1L) current else NA
      } else if (key_chr %in% names(current)) {
        current[[key_chr]]
      } else {
        NA
      }
      scope <- c(state$globals, stats::setNames(list(here, k),
                                                c(r$target, ".key")))
      quo <- rlang::quo_set_env(
        r$quo, rlang::new_environment(scope, parent = rlang::quo_get_env(r$quo))
      )
      val <- dplyr::pull(dplyr::reframe(combined, .abm_value = !!quo),
                         ".abm_value")
      if (length(val) != 1L) {
        abm_abort(
          c("{.fn abm_global} rules must collapse to a single value per key.",
            "x" = "{.code {r$target} ~ {deparse1(rlang::quo_get_expr(r$quo))}} returned {length(val)} values for key {.val {k}}.",
            "i" = "Wrap it in an aggregate such as {.fn sum} or {.fn mean}."),
          class = "tidyABM_bad_global"
        )
      }
      if (is.null(out)) {
        out <- stats::setNames(val, key_chr)
      } else {
        out[[key_chr]] <- val
      }
    }
    state$globals[[r$target]] <- out
  }
  state
}

#' Run one abm_sequential step
#'
#' The step is a loop over agents, so the cost of one agent-rule is the cost of
#' the whole thing. It used to be a `dplyr::mutate()` on a one-row tibble, which
#' is a few hundred microseconds of machinery for what is usually one arithmetic
#' operation on one number. Nothing about the semantics needs that: an agent's
#' row is a handful of scalars, so the rules are evaluated against a plain data
#' mask built from them, and the group's columns are held as bare vectors for the
#' duration of the loop rather than rebuilt as a tibble on every write.
#' @noRd
run_sequential <- function(step, state) {
  order_ids <- unlist(lapply(state$groups, function(g) g$.id), use.names = FALSE)
  if (!length(order_ids)) return(state)

  # A standing match narrows the step to the agents it placed in a group, the
  # way it narrows `abm_rules()`, and puts `.partner` and `partner_<col>` in
  # scope. Read live from the working columns, so the second buyer at a shop
  # sees the stock the first one left.
  m <- NULL
  if (!is.null(state$match)) {
    m <- state$match$match
    # a match step that paired nobody leaves the step with nothing to do, the
    # way it leaves `abm_rules()` with no rows to write
    if (nrow(m) == 0L) return(state)
    order_ids <- order_ids[order_ids %in% m$.id]
    if (!length(order_ids)) return(state)
  }

  if (is.null(step$order)) {
    order_ids <- shuffle(order_ids)
  } else {
    # The order agents are processed in is part of some models -- a queue, a
    # sequential-service constraint -- and a fresh shuffle is then the wrong
    # answer rather than an arbitrary one.
    combined <- bind_groups(state$groups)
    key <- eval_rule(list(quo = step$order), combined, state$globals,
                     grouped = FALSE)
    if (length(key) == 1L) key <- rep(key, nrow(combined))
    keep <- !is.na(key) & combined$.id %in% order_ids
    order_ids <- combined$.id[keep][order(key[keep])]
    if (!length(order_ids)) return(state)
  }

  partner_of <- role_of <- gid_of <- NULL
  p_cols <- character()
  if (!is.null(m)) {
    partner_of <- stats::setNames(m$.partner,  as.character(m$.id))
    role_of    <- stats::setNames(m$.role,     as.character(m$.id))
    gid_of     <- stats::setNames(m$.group_id, as.character(m$.id))
    # only the partner columns the step names are materialised: doing it for
    # every column of every agent is what would make this loop slow
    named <- c(unlist(lapply(step$rules, `[[`, "vars"), use.names = FALSE),
               vapply(step$rules, `[[`, "", "target"))
    p_cols <- unique(sub("^partner_", "", grep("^partner_", named, value = TRUE)))
  }

  group_of <- rep(names(state$groups),
                  vapply(state$groups, nrow, integer(1)))
  names(group_of) <- unlist(lapply(state$groups, function(g) g$.id),
                            use.names = FALSE)

  # each rule gets an environment of its own, holding the globals. Writing a
  # global assigns into those environments rather than rebuilding a mask, which
  # is what keeps "the next agent sees what I just spent" cheap.
  envs <- lapply(step$rules, function(r) {
    rlang::new_environment(state$globals, parent = rlang::quo_get_env(r$quo))
  })
  quos <- Map(function(r, e) rlang::quo_set_env(r$quo, e), step$rules, envs)
  is_global <- vapply(step$rules, function(r) r$target %in% names(state$globals),
                      logical(1))

  cols <- lapply(state$groups, function(g) as.list(g))
  sizes <- vapply(state$groups, nrow, integer(1))
  index <- lapply(state$groups, function(g) stats::setNames(seq_len(nrow(g)), g$.id))
  todo <- stats::setNames(vector("list", length(cols)), names(cols))

  for (id in order_ids) {
    gname <- group_of[[as.character(id)]]
    v <- cols[[gname]]
    i <- index[[gname]][[as.character(id)]]
    if (is.na(i)) next

    if (is.null(todo[[gname]])) {
      todo[[gname]] <- applicable_rules(step, names(v), cols, state$globals)
    }
    which_rules <- todo[[gname]]
    if (!length(which_rules)) next

    # one mask per agent, kept up to date in place: a rule sees what the rule
    # above it wrote, which is what "one agent at a time" already implies
    data <- lapply(v, function(x) x[[i]])
    if (!"n" %in% names(data)) data$n <- function() 1L

    pid <- NA_integer_; pg <- NULL; pj <- NA_integer_
    if (!is.null(m)) {
      key <- as.character(id)
      data$.partner  <- partner_of[[key]]
      data$.role     <- role_of[[key]]
      data$.group_id <- gid_of[[key]]
      pid <- data$.partner
      if (!is.na(pid)) {
        pg <- group_of[[as.character(pid)]]
        pj <- index[[pg]][[as.character(pid)]]
      }
      for (nm in p_cols) {
        src <- if (is.na(pid)) NULL
               else if (identical(pg, gname)) v else cols[[pg]]
        data[[paste0("partner_", nm)]] <-
          if (!is.null(src) && nm %in% names(src)) src[[nm]][[pj]] else NA
      }
    }

    for (k in which_rules) {
      value <- rlang::eval_tidy(quos[[k]], data = data)
      target <- step$rules[[k]]$target
      if (startsWith(target, "partner_") && !is.null(m)) {
        # the only place a sequential rule leaves its own row: the agent it is
        # matched with, which is what a transaction is
        nm <- sub("^partner_", "", target)
        if (!is.na(pid)) {
          if (identical(pg, gname)) {
            if (nm %in% names(v)) {
              v[[nm]] <- write_at(v[[nm]], pj, value, sizes[[gname]])
            }
          } else if (nm %in% names(cols[[pg]])) {
            cols[[pg]][[nm]] <- write_at(cols[[pg]][[nm]], pj, value,
                                         sizes[[pg]])
          }
          data[[target]] <- value[[1]]
        }
      } else if (is_global[[k]]) {
        val <- value[[1]]
        state$globals[[target]] <- val
        for (e in envs) assign(target, val, envir = e)
      } else {
        new_col <- !target %in% names(v)
        v[[target]] <- write_at(v[[target]], i, value, sizes[[gname]])
        data[[target]] <- value[[1]]
        if (new_col) todo[[gname]] <- NULL
      }
    }
    cols[[gname]] <- v
    if (is.null(todo[[gname]])) {
      todo[[gname]] <- applicable_rules(step, names(v), cols, state$globals)
    }
  }

  for (nm in names(cols)) {
    state$groups[[nm]] <- tibble::new_tibble(cols[[nm]], nrow = sizes[[nm]])
  }
  state
}

#' Which of a step's rules apply to a group with these columns?
#' @noRd
applicable_rules <- function(step, group_names, cols, globals) {
  all_cols <- setdiff(unique(unlist(lapply(cols, names), use.names = FALSE)),
                      c(".id", ".group"))
  g <- stats::setNames(vector("list", length(group_names)), group_names)
  which(vapply(step$rules, rule_applies, logical(1),
               g = g, all_cols = all_cols, globals = globals))
}

#' Write one value into one slot of a bare column vector
#'
#' `NULL` creates the column. A value of a type the column cannot hold widens the
#' column, the way [assign_rule()] does for the whole-population steps.
#' @noRd
write_at <- function(column, i, value, n) {
  if (length(value) != 1L) {
    abm_abort(
      c("An {.fn abm_sequential} rule must give one value per agent.",
        "x" = "It returned {length(value)}."),
      class = "tidyABM_bad_sequential"
    )
  }
  if (is.null(column)) column <- vctrs::vec_init(value, n)
  if (is.list(column)) {
    column[i] <- list(value[[1]])
    return(column)
  }
  if (!identical(class(column), class(value))) {
    both <- vctrs::vec_cast_common(column, value)
    column <- both[[1]]
    value <- both[[2]]
  }
  column[[i]] <- value[[1]]
  column
}
