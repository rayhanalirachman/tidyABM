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
  # plain `%in%` rather than `setdiff()` / `intersect()`: asked of every rule
  # for every group on every tick
  vars <- rule$vars
  if (any(startsWith(vars, "cell_")) && !".cell" %in% names(g)) return(FALSE)
  vars <- vars[vars %in% all_cols & !startsWith(vars, "partner_") &
                 !startsWith(vars, "cell_") &
                 !vars %in% c(".id", ".group", ".group_id", ".partner", ".role",
                              ".cost", ".tick", ".dist", names(globals))]
  if (!all(vars %in% names(g))) return(FALSE)
  if (rule$target %in% all_cols && !(rule$target %in% names(g))) return(FALSE)
  TRUE
}

#' Does a *condition* apply to this group?
#'
#' The same question [rule_applies()] asks, without a target column: a
#' condition that mentions an agent column this group does not have is not
#' about this group. That is what lets one `abm_death(when = ... & energy < 0)`
#' sit in a model whose patches have no `energy`, exactly as one `abm_rules()`
#' step routes itself to the groups its columns exist in.
#' @noRd
condition_applies <- function(quo, g, all_cols, globals) {
  if (is.null(quo)) return(TRUE)
  # plain `%in%`, as in [rule_applies()]: asked per group on every tick
  vars <- all.vars(rlang::quo_get_expr(quo))
  if (any(startsWith(vars, "cell_")) && !".cell" %in% names(g)) return(FALSE)
  vars <- vars[vars %in% all_cols & !startsWith(vars, "partner_") &
                 !startsWith(vars, "cell_") &
                 !vars %in% c(".id", ".group", ".group_id", ".partner", ".role",
                              ".cost", ".tick", ".dist", names(globals))]
  all(vars %in% names(g))
}

#' Add match information and partner columns to a group tibble
#'
#' With a `size = 2` match standing, the pair's relation columns come too:
#' `.R`, `R_<col>` for `(me -> .partner)` and their `_back` forms, `NA` (or
#' `FALSE`) for an agent with no partner. They travel with `partner_<col>`
#' because they are about the same pair.
#' @noRd
augment_group <- function(g, match_state, combined, relations = NULL,
                          partner = NULL, match_cols = NULL) {
  # `partner` names the columns the step reads as `partner_<col>`; the rest
  # are not built, since copying every column of every agent for every step
  # is most of what a step costs. `NULL` builds them all.
  # the columns are added to a bare list and the tibble rebuilt once at the
  # end: `$<-.tbl_df` re-checks the whole tibble on every assignment, several
  # times per group per step
  n <- vctrs::vec_size(g)    # `nrow()` dispatches twice on a tibble
  cols <- as.list(g)
  if (is.null(match_state)) {
    cols$.group_id <- seq_len(n)
    cols$.role <- rep(NA_character_, n)
    cols$.partner <- rep(NA_integer_, n)
    if (is.null(match_cols) || ".cost" %in% match_cols) cols$.cost <- rep(NA_real_, n)
    return(new_tbl(cols, n))
  }
  pcols <- names(combined)
  pcols <- pcols[!pcols %in% c(".id", ".group")]
  if (!is.null(partner)) pcols <- pcols[pcols %in% partner]
  if (nrow(match_state$match) == 0L) {
    # a match step ran and paired nobody. The partner columns still have to
    # exist, all missing, so that a tick where nobody meets anyone does not
    # crash the rules written for the ticks where they do.
    cols$.group_id <- rep(NA_integer_, n)
    cols$.role <- rep(NA_character_, n)
    cols$.partner <- rep(NA_integer_, n)
    if (is.null(match_cols) || ".cost" %in% match_cols) cols$.cost <- rep(NA_real_, n)
    if (match_state$size == 2L) {
      for (nm in pcols) {
        cols[[paste0("partner_", nm)]] <- combined[[nm]][NA_integer_][rep(1L, n)]
      }
      cols <- attach_relation_columns(cols, relations, cols$.id, cols$.partner)
    }
    return(new_tbl(cols, n))
  }
  m <- match_state$match
  idx <- match_rows(cols$.id, m$.id)
  # `match_cols` names the ones a caller's expressions read; `NULL` adds all
  # the partner ids are taken from the match itself, never from `cols`: a
  # caller may leave `.partner` out of `match_cols` and still read
  # `partner_<col>` or relation columns, which are found through it
  partner_ids <- m$.partner[idx]
  if (is.null(match_cols) || ".partner" %in% match_cols) cols$.partner <- partner_ids
  if (is.null(match_cols) || ".role" %in% match_cols) cols$.role <- m$.role[idx]
  if (is.null(match_cols) || ".group_id" %in% match_cols) cols$.group_id <- m$.group_id[idx]
  if (is.null(match_cols) || ".cost" %in% match_cols) {
    cols$.cost <- if (".cost" %in% names(m)) m$.cost[idx] else rep(NA_real_, n)
  }

  if (match_state$size == 2L) {
    pidx <- if (length(pcols)) match(partner_ids, combined$.id)
    for (nm in pcols) {
      cols[[paste0("partner_", nm)]] <- combined[[nm]][pidx]
    }
    cols <- attach_relation_columns(cols, relations, cols$.id, partner_ids)
  }
  new_tbl(cols, n)
}

#' `match(ids, mids)`, without hashing when `mids` is the tail of `ids`
#'
#' The usual standing match pairs the last group's agents in order, so its ids
#' are the stacked ids' tail. `ids` strictly increasing makes them distinct,
#' and every row before the tail then has no match.
#' @noRd
match_rows <- function(ids, mids) {
  n <- length(ids)
  k <- length(mids)
  if (k > 0L && k <= n && is.integer(ids) && is.integer(mids) &&
      identical(ids[[n]], mids[[k]]) &&
      isFALSE(is.unsorted(ids, strictly = TRUE)) &&
      identical(mids, ids[seq.int(n - k + 1L, n)])) {
    return(c(rep(NA_integer_, n - k), seq_len(k)))
  }
  # a group none of whose ids is in the match -- the cells, when only the
  # movers pair up -- needs no hashing either
  if (n > 0L && k > 0L && is.integer(ids) && is.integer(mids) && !anyNA(mids) &&
      !is.na(ids[[1L]]) && !is.na(ids[[n]]) && isFALSE(is.unsorted(ids, strictly = TRUE))) {
    r <- range(mids)
    if (r[[2L]] < ids[[1L]] || r[[1L]] > ids[[n]]) return(rep(NA_integer_, n))
  }
  match(ids, mids)
}

#' Route a relation-targeted rule's values into the relation, under a match
#'
#' The pair written is `(me -> .partner)`, or the reverse for a `_back` target.
#' An agent with no partner this step is skipped, exactly as a `partner_<col>`
#' write is; a partner with no row is the error, because a write is about a
#' pair that exists and creating it is `abm_link()`'s job.
#' @noRd
write_relation_rule <- function(state, hit, target, value, aug, rows, step,
                                call = rlang::caller_env()) {
  if (is.null(state$match) || state$match$size != 2L) {
    abm_abort(
      c("{.fn {step}} writes {.field {target}}, a value on a pair, but no pairing is standing.",
        "i" = "Put an {.fn abm_match} that pairs agents two at a time before it."),
      class = "tidyABM_no_match", call = call
    )
  }
  if (is.null(hit$col)) {
    abm_abort(
      c("{.field {target}} says whether the pair exists; it is not written by a rule.",
        "i" = 'Add or remove the pair with {.code abm_link(via = "{hit$rel}")} / {.code abm_unlink(via = "{hit$rel}")}.'),
      class = "tidyABM_bad_target", call = call
    )
  }
  w <- rows & !is.na(aug$.partner)
  if (!any(w)) return(state)
  value <- vctrs::vec_recycle(unname(value), nrow(aug))
  from <- if (hit$back) aug$.partner[w] else aug$.id[w]
  to   <- if (hit$back) aug$.id[w]      else aug$.partner[w]
  state$relations[[hit$rel]] <- write_relation(
    state$relations[[hit$rel]], hit$rel, hit$col, from, to, value[w], step, call)
  state
}

#' One data mask and one scope for every rule a step evaluates over `data`
#'
#' Building the mask and the scope ([abm_eval_env()]) is most of what a short
#' rule costs, and a step's rules all read the same rows and the same globals.
#' `eval(quo)` evaluates one rule; `set()` records a global a rule has just
#' written, as [run_global()]'s rules see each other's results. Only for a
#' rule marked `plain`: one that assigns nothing, so it leaves nothing behind
#' in the mask for the next rule to trip over.
#' @noRd
shared_eval <- function(data, globals, n = nrow(data)) {
  mask <- NULL     # built for the first rule that needs one
  env <- NULL
  of <- NULL       # the environment the rules were written in, which `env` sits on
  env_for <- function(quo) {
    if (!identical(rlang::quo_get_env(quo), of)) {
      of <<- rlang::quo_get_env(quo)
      env <<- abm_eval_env(quo, globals, n = n)
    }
    env
  }
  list(
    eval = function(quo) {
      # a formula [base_eval_ok()] passed needs no mask, as in [eval_rule()]
      if (isTRUE(attr(quo, "abm_base", exact = TRUE))) {
        return(eval(rlang::quo_get_expr(quo), data, env_for(quo)))
      }
      if (is.null(mask)) mask <<- rlang::as_data_mask(data)
      rlang::eval_tidy(rlang::quo_get_expr(quo), data = mask, env = env_for(quo))
    },
    env = env_for,
    set = function(name, value) {
      # with no globals yet there is no scope of theirs to write into
      if (!length(globals)) of <<- NULL
      else if (!is.null(of)) assign(name, value, envir = parent.env(env))
      globals[[name]] <<- value
    }
  )
}

#' Evaluate one rule against an augmented group tibble
#' @noRd
eval_rule <- function(rule, aug, globals, grouped, by = NULL, cache = NULL) {
  quo <- rule$quo
  # ungrouped, the data mask gives an expression nothing `eval_tidy()` over the
  # same tibble does not, and `mutate()` costs a hundred times more. A result
  # of the wrong length goes back through `mutate()` so its error is the one
  # raised.
  if (is.null(by) && !grouped) {
    expr <- rlang::quo_get_expr(quo)
    # nothing a mask or a fresh scope is needed for: base `eval()` over the
    # columns, in a scope kept while the globals stay the same. A result of
    # the wrong length goes to `mutate()`, as it does from `eval_tidy()`.
    # decided once for a step's own formulas, see [mark_steps()]
    base <- attr(quo, "abm_base", exact = TRUE) %||% base_eval_ok(expr)
    if (base) {
      n <- vctrs::vec_size(aug)
      out <- eval(expr, aug, eval_env_cached(quo, globals, n))
      if (length(out) == n || length(out) == 1L) return(out)
    } else if (!needs_mask(expr)) {
      n <- vctrs::vec_size(aug)
      env <- if (isTRUE(attr(quo, "abm_scope", exact = TRUE))) {
        eval_env_cached(quo, globals, n)
      } else {
        abm_eval_env(quo, globals, n = n)
      }
      out <- rlang::eval_tidy(expr, data = aug, env = env)
      if (length(out) == n || length(out) == 1L) return(out)
    }
  }
  if (!is.null(by) || grouped) {
    out <- eval_by_key(quo, aug, globals, aug[[by %||% ".group_id"]], cache)
    if (!is.null(out)) return(out)
  }
  quo <- rlang::quo_set_env(quo, abm_eval_env(quo, globals))

  dat <- if (!is.null(by)) dplyr::group_by(aug, .data[[by]])
         else if (grouped) dplyr::group_by(aug, .data$.group_id)
         else aug
  out <- dplyr::mutate(dat, .abm_value = !!quo)
  dplyr::pull(dplyr::ungroup(out), ".abm_value")
}

#' Evaluate a rule once per value of `key`, as a grouped `mutate()` would
#'
#' The expression is evaluated with base `eval()` over each group's rows, in
#' the order `group_by()` visits them, so a rule that draws sees the same
#' stream. `mutate()` builds a mask and its bookkeeping per group; here a group
#' costs a slice of the columns the rule reads.
#'
#' The usual grouped rule is elementwise around a few aggregates -- a share of
#' the group's total, a distance from its mean. Only the aggregates need the
#' groups, so they alone are evaluated per group and the rest of the rule once
#' over every row, with each aggregate standing in as a column: 200 groups
#' then cost 200 `sum()`s, not 200 `if_else()`s. Nothing in such a rule draws.
#'
#' Returns `NULL`, and the caller goes through `mutate()`, for anything not
#' covered: a key that is not a plain number or logical (their order is
#' locale-free), an expression that needs a dplyr mask or a pronoun, and any
#' error or result of the wrong size, so the error raised is still `mutate()`'s.
#' @noRd
eval_by_key <- function(quo, aug, globals, key, cache = NULL) {
  info <- tryCatch(by_key_info(quo, cache), error = function(e) list(ok = FALSE))
  if (is.object(key) || !(is.numeric(key) || is.logical(key)) || !info$ok) {
    return(NULL)
  }
  locate <- function() {
    grp <- vctrs::vec_group_loc(key)
    loc <- grp$loc[vctrs::vec_order(grp$key)]
    of <- integer(length(key))
    of[unlist(loc, use.names = FALSE)] <- rep(seq_along(loc), lengths(loc))
    list(loc = loc, of = of)
  }
  # the groups' rows, kept in `cache` (a step's plan) while the key stands
  if (is.null(cache)) {
    hit <- locate()
  } else {
    hit <- cache$by_loc
    if (is.null(hit) || !identical(hit$key, key, num.eq = FALSE)) {
      hit <- c(list(key = key), locate())
      assign("by_loc", hit, envir = cache)
    }
  }
  loc <- hit$loc
  reads <- info$reads
  cols <- unclass(aug)[names(aug) %in% reads]
  size <- 0L
  env <- abm_eval_env(quo, globals)
  env$n <- function() size
  # every expression in `exprs`, once per group
  per_group <- function(exprs, cols) {
    chops <- lapply(cols, vctrs::vec_chop, indices = loc)
    lapply(seq_along(loc), function(i) {
      size <<- length(loc[[i]])
      data <- lapply(chops, .subset2, i)
      lapply(exprs, eval, data, env)
    })
  }
  tryCatch({
    parts <- info$parts
    # a free name in the elementwise part must be one value, not a vector
    # that would be recycled along each group
    free <- info$outer_vars[!info$outer_vars %in% c(names(cols), info$agg_names)]
    if (info$split && all(vapply(free, function(v) length(get0(v, env)) == 1L, NA))) {
      of <- hit$of
      vals <- aggregates_by_key(parts$aggs, cols, env, loc, of, info$specs) %||% {
        per <- per_group(parts$aggs, cols[names(cols) %in% info$agg_cols])
        lapply(seq_along(parts$aggs), function(k) {
          val <- lapply(per, `[[`, k)
          stopifnot(lengths(val) == 1L)
          vctrs::list_unchop(val)
        })
      }
      for (k in seq_along(parts$aggs)) {
        stopifnot(length(vals[[k]]) == length(loc))
        cols[[info$agg_names[[k]]]] <- vctrs::vec_slice(vals[[k]], of)
      }
      vctrs::vec_recycle(eval(parts$outer, cols, env), nrow(aug))
    } else {
      out <- Map(function(val, rows) vctrs::vec_recycle(val[[1]], length(rows)),
                 per_group(list(rlang::quo_get_expr(quo)), cols), loc)
      vctrs::list_unchop(out, indices = loc)
    }
  }, error = function(e) NULL)
}

#' What [eval_by_key()] works out from a formula's text alone, kept in the
#' step's plan with the quosure it was worked out for
#' @noRd
by_key_info <- function(quo, cache = NULL) {
  if (!is.null(cache)) {
    for (k in cache$by_info) if (identical(k$quo, quo)) return(k$info)
  }
  expr <- rlang::quo_get_expr(quo)
  reads <- read_vars(list(quo))
  info <- list(ok = !(is.null(reads) || ".env" %in% reads || needs_mask(expr)),
               reads = reads)
  if (info$ok) {
    parts <- split_aggregates(expr)
    info$parts <- parts
    info$agg_names <- paste0(".agg", seq_along(parts$aggs))
    info$outer_vars <- all.vars(parts$outer)
    # not `ifelse()`, `%in%` or `paste()`: with a one-value test, a table or
    # `collapse =` their answer depends on which rows they are handed
    info$split <- is_elementwise(parts$outer, nb_fast_fns) &&
      all(vapply(parts$aggs, is_elementwise, NA,
                 fns = c(elementwise_fns, aggregate_fns, "[")))
    info$agg_cols <- unlist(lapply(parts$aggs, all.vars), use.names = FALSE)
    info$specs <- aggregate_specs(parts$aggs)
  }
  if (!is.null(cache)) {
    assign("by_info", c(cache$by_info, list(list(quo = quo, info = info))), envir = cache)
  }
  info
}

#' [eval_by_key()]'s aggregates `f(X)` / `f(X[cond])`, `X` and `cond`
#' elementwise, without evaluating them once per group
#'
#' `X` and `cond` are evaluated once over every row, the rows `cond` keeps (or
#' leaves `NA`, where `X[cond]` has an `NA`) are split by group, and each
#' group's `f()` is taken of exactly the elements, in exactly the order, that
#' `X[cond]` evaluated over that group's rows has. The groups are visited in the
#' same order, each group's aggregates in turn. `NULL` for any other shape, a
#' name that is neither a column nor one value, a non-standard function, a
#' classed column or result, or any warning or error, which leaves it to the
#' group-by-group evaluation.
#' @noRd
aggregates_by_key <- function(aggs, cols, env, loc, of, specs = aggregate_specs(aggs)) {
  n <- length(of)
  if (is.null(specs)) return(NULL)
  tryCatch({
    picked <- lapply(specs, function(s) {
      if (!standard_fns_in(s$heads, env)) return(NULL)
      for (v in s$vars) {
        val <- if (v %in% names(cols)) cols[[v]] else get0(v, envir = env)
        if (is.null(val) || !is.atomic(val) || !is.null(attributes(val)) ||
            !(v %in% names(cols) || length(val) == 1L)) return(NULL)
      }
      x <- eval(s$x, cols, env)
      if (length(x) != n || !is.atomic(x) || !is.null(attributes(x))) return(NULL)
      rows <- seq_len(n)
      if (!is.null(s$cond)) {
        keep <- eval(s$cond, cols, env)
        if (!is.logical(keep) || !is.null(attributes(keep)) ||
            !length(keep) %in% c(1L, n)) return(NULL)
        if (length(keep) == 1L) keep <- rep(keep, n)
        miss <- is.na(keep)
        x[miss] <- NA
        rows <- which(keep | miss)
      }
      list(f = s$f, x = x, rows = rows, of = of[rows],
           fun = get0(s$f, envir = env, mode = "function"))
    })
    if (any(vapply(picked, is.null, NA))) return(NULL)
    # one value per group and aggregate
    lapply(picked, function(p) {
      sum_of_one(p, length(loc)) %||% {
        by <- split(p$rows, factor(p$of, levels = seq_along(loc)))
        vctrs::list_unchop(lapply(unname(by), function(r) p$fun(p$x[r])))
      }
    })
  }, warning = function(w) NULL, error = function(e) NULL)
}

#' The shapes [aggregates_by_key()] takes, one per aggregate, from their text
#' alone; `NULL` when any is not one
#' @noRd
aggregate_specs <- function(aggs) {
  specs <- lapply(aggs, function(a) {
    if (!rlang::is_call(a) || !rlang::is_symbol(a[[1]]) || length(a) != 2L ||
        !is.null(names(a))) return(NULL)
    f <- rlang::as_string(a[[1]])
    if (!f %in% setdiff(aggregate_fns, "n")) return(NULL)
    x <- a[[2]]
    cond <- NULL
    if (rlang::is_call(x, "[") && length(x) == 3L && is.null(names(x))) {
      cond <- x[[3]]
      x <- x[[2]]
    }
    parts <- c(list(x), if (!is.null(cond)) list(cond))
    if (!all(vapply(parts, is_elementwise, NA, fns = nb_fast_fns))) return(NULL)
    heads <- c(f, if (!is.null(cond)) "[", unlist(lapply(parts, call_heads)))
    if (anyNA(heads)) return(NULL)
    list(f = f, x = x, cond = cond, heads = unique(heads),
         vars = unique(unlist(lapply(parts, all.vars), use.names = FALSE)))
  })
  if (any(vapply(specs, is.null, NA))) return(NULL)
  specs
}

#' [aggregates_by_key()]'s `sum()` when no group has more than one element
#'
#' The sum of one number is `0 + x`, in any precision, and of none the sum of
#' an empty vector, so the groups need not be visited one by one. A missing
#' value is still summed by `sum()` itself. `NULL` for anything else.
#' @noRd
sum_of_one <- function(p, n_groups) {
  if (p$f != "sum" || !identical(p$fun, base::sum)) return(NULL)
  x <- p$x
  if (!(is.double(x) || is.integer(x) || is.logical(x))) return(NULL)
  # rows in no group (an `NA` key) are not summed
  of <- p$of[p$of > 0L]
  if (anyDuplicated(of)) return(NULL)
  out <- rep(sum(x[0L]), n_groups)
  v <- x[p$rows[p$of > 0L]]
  out[of] <- if (is.double(v)) 0 + v else as.integer(v)
  for (i in which(is.na(v))) out[[of[[i]]]] <- sum(v[[i]])
  out
}

#' Write a computed vector into a group column, for participating rows only
#' @noRd
assign_rule <- function(g, target, value, rows) {
  # a value looked up out of a named global (`price[good]`) arrives carrying the
  # global's names; an agent column is not a lookup table, so they come off here
  # rather than travelling through the whole run and into the result.
  if (!is.list(value) && !is.null(names(value))) value <- unname(value)
  n <- vctrs::vec_size(g)
  if (length(value) == 1L) value <- rep(value, n)
  # written with the class off: `[[<-.tbl_df` re-checks the whole tibble, once
  # per rule per group per tick, and a column of `n` values keeps it a tibble
  cls <- oldClass(g)
  g <- unclass(g)
  if (is.null(g[[target]])) g[[target]] <- vctrs::vec_init(value, n)
  if (all(rows)) {
    g[[target]] <- value
  } else {
    g[[target]] <- vctrs::vec_cast_common(g[[target]], value)[[1]]
    value <- vctrs::vec_cast(value, g[[target]])
    g[[target]][rows] <- value[rows]
  }
  oldClass(g) <- cls
  g
}

#' Write a vector computed over `rows` only (`length(value) %in% c(1, sum(rows))`)
#' @noRd
assign_rule_subset <- function(g, target, value, rows) {
  if (!is.list(value) && !is.null(names(value))) value <- unname(value)
  n <- vctrs::vec_size(g)
  k <- sum(rows)
  if (length(value) == 1L) value <- rep(value, k)
  cls <- oldClass(g)
  g <- unclass(g)
  if (is.null(g[[target]])) g[[target]] <- vctrs::vec_init(value, n)
  common <- vctrs::vec_cast_common(g[[target]], value)
  g[[target]] <- common[[1]]
  g[[target]][rows] <- common[[2]]
  oldClass(g) <- cls
  g
}

#' Run one abm_rules step across every group
#' @noRd
run_rules <- function(step, state) {
  if (!is.null(step$by)) return(run_rules_by(step, state))
  quos <- planned(step, "quos", c(lapply(step$rules, `[[`, "quo"),
                                  if (!is.null(step$when)) list(step$when)))
  uses_match <- step$uses_match %||% planned(step, "uses_match", reads_match(quos))
  cvars <- planned(step, "cvars", cell_vars(quos))
  scope_match <- (step$scope %||% "match") == "match"
  # Empty standing match with match scope writes nobody (consistent with partial
  # matches and abm_sequential()).
  if (!is.null(state$match) && nrow(state$match$match) == 0L && scope_match) {
    return(state)
  }
  if (length(state$groups) == 1L && is.null(state$match) &&
      !length(state$relations) && !uses_match && is.null(step$when) &&
      !length(cvars)) {
    return(run_rules_plain(step, state))
  }
  # only asked for when there is more than one group to route the rules to
  routes <- if (length(state$groups) > 1L) rule_routes(step, state)
  when_routes <- if (!is.null(step$when) && length(state$groups) > 1L) {
    condition_routes(step, state)
  }
  active <- !is.null(state$match) && nrow(state$match$match) > 0L
  # When a match is standing, rules are evaluated group-by-group: per pair for
  # size 2, per group for size > 2, per agent for the directional modes. That is
  # what makes `sample(x, 1)` inside a rule mean "once per pair" rather than
  # "once for the whole population".
  grouped <- active && scope_match
  # a population-scope step that reads nothing of the pairing is not handed
  # it: looking every agent up in the match costs more than its rules do
  match <- if (grouped || length(state$relations) || uses_match) state$match
  # partner columns are only built under a standing match, and they are all a
  # rule reads of another group
  pvars <- if (!is.null(match)) planned(step, "pvars", partner_vars(quos))
  combined <- if (length(pvars)) bind_groups(state$groups, pvars)
  # with no match the match columns are all `NA`: built only if read
  bare <- is.null(match) && !uses_match
  # of those, only what the rules read, and `.group_id`, which says who is paired
  mcols <- planned(step, "mcols", {
    m <- match_cols_read(quos, state)
    if (!is.null(m)) union(".group_id", m)
  })

  applied_anywhere <- FALSE
  for (nm in names(state$groups)) {
    g <- state$groups[[nm]]
    if (vctrs::vec_size(g) == 0L) next
    if (!is.null(when_routes) && !when_routes[[nm]]) next
    # a lone group has every column there is, so every rule applies to it
    todo <- if (length(state$groups) == 1L) step$rules else routes[[nm]]
    if (!length(todo)) next
    applied_anywhere <- TRUE
    aug <- if (bare) g else
      augment_group(g, match, combined, state$relations, partner = pvars,
                    match_cols = mcols)
    if (length(cvars)) aug <- attach_cell_columns(aug, state, cvars)
    rows <- if (active && grouped) !is.na(aug$.group_id) else rep(TRUE, vctrs::vec_size(g))

    narrowed <- FALSE
    if (!is.null(step$when)) {
      w <- eval_condition(step$when, aug, state$globals)
      rows <- rows & w
      if (!any(rows)) next
      if (!all(rows)) {
        aug <- vctrs::vec_slice(aug, rows)
        narrowed <- TRUE
      }
    }

    # an elementwise rule gives the same answer per pair as over the whole
    # group, so it is evaluated once rather than once per pair
    ev <- NULL
    values <- lapply(todo, function(r) {
      by_group <- grouped && !isTRUE(r$elementwise)
      # a lone rule has nothing to share the mask with
      if (isTRUE(r$plain) && !by_group && length(todo) > 1L) {
        if (is.null(ev)) ev <<- shared_eval(aug, state$globals)
        out <- ev$eval(r$quo)
        if (length(out) == vctrs::vec_size(aug) || length(out) == 1L) return(out)
      }
      eval_rule(r, aug, state$globals, grouped = by_group)
    })
    for (i in seq_along(todo)) {
      # a target that names a value on a pair goes to the relation, not to an
      # agent column of the same name
      hit <- resolve_relation_name(todo[[i]]$target, state$relations)
      if (!is.null(hit)) {
        state <- write_relation_rule(state, hit, todo[[i]]$target, values[[i]],
                                     aug, if (narrowed) rep(TRUE, nrow(aug)) else rows,
                                     "abm_rules")
      } else if (narrowed) {
        g <- assign_rule_subset(g, todo[[i]]$target, values[[i]], rows)
      } else {
        g <- assign_rule(g, todo[[i]]$target, values[[i]], rows)
      }
    }
    state$groups[[nm]] <- g
  }

  # an empty population is not an error: there was nobody to apply it to
  if (!applied_anywhere && is.null(step$when) && any(vapply(state$groups, nrow, 0L))) {
    abm_abort(
      c("An {.fn abm_rules} step applied to no agent group.",
        "x" = "Rule{?s} {.code {rule_labels(step)}} mention{?s/} columns that no group has.",
        "i" = "Available columns: {.field {setdiff(model_columns(state$groups), c('.id', '.group'))}}."),
      class = "tidyABM_unapplied_rule"
    )
  }
  state
}

#' Which of a step's rules apply to each group, as [rule_applies()] decides
#'
#' Asked of every rule for every group on every tick, and the answer only
#' changes with the groups' column names or the globals' names, so the plan
#' keeps it for as long as those stay the same.
#' @noRd
rule_routes <- function(step, state) {
  planned_for(step, "routes", schema_key(state), {
    all_cols <- model_columns(state$groups)
    lapply(state$groups, function(g) {
      Filter(function(r) rule_applies(r, g, all_cols, state$globals), step$rules)
    })
  })
}

#' [run_rules()] for one group, no pairing and no relation
#'
#' The commonest step there is, and the same evaluation the general loop
#' makes of it, without the routing, the match columns and the relation
#' lookups that cannot apply.
#' @noRd
run_rules_plain <- function(step, state) {
  g <- state$groups[[1]]
  n <- vctrs::vec_size(g)
  if (n == 0L) return(state)
  rules <- step$rules
  ev <- NULL
  values <- vector("list", length(rules))
  for (i in seq_along(rules)) {
    r <- rules[[i]]
    out <- NULL
    # a lone rule has nothing to share the mask with
    if (isTRUE(r$plain) && length(rules) > 1L) {
      if (is.null(ev)) ev <- shared_eval(g, state$globals)
      out <- ev$eval(r$quo)
      if (!(length(out) == n || length(out) == 1L)) out <- NULL
    }
    values[i] <- list(if (is.null(out)) eval_rule(r, g, state$globals, grouped = FALSE) else out)
  }
  for (i in seq_along(rules)) g <- assign_rule(g, rules[[i]]$target, values[[i]], TRUE)
  state$groups[[1]] <- g
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
run_rules_by <- function(step, state) {
  by <- by_columns(step$by)
  quos <- c(lapply(step$rules, `[[`, "quo"), if (!is.null(step$when)) list(step$when))
  cols <- read_vars(quos)
  cvars <- cell_vars(quos)
  combined <- bind_groups(state$groups, if (!is.null(cols))
    c(cols, by, vapply(step$rules, `[[`, "", "target")))
  if (nrow(combined) == 0L) return(state)
  if (length(by) != 1L || !by %in% names(combined)) {
    abm_abort(
      c("{.arg .by} must name one existing agent column.",
        "x" = "No column {.field {by}}."),
      class = "tidyABM_missing_column"
    )
  }
  # what a match adds is only built for rules that could read it
  aug <- if (!length(state$relations) && !(step$uses_match %||% reads_match(quos))) {
    combined
  } else {
    augment_group(combined, state$match, combined, state$relations)
  }
  if (length(cvars)) aug <- attach_cell_columns(aug, state, cvars)
  for (r in step$rules) {
    if (!is.null(resolve_relation_name(r$target, state$relations))) {
      abm_abort(
        c("{.field {r$target}} is a value on a pair, and {.arg .by} rules write agent columns.",
          "i" = "Write it from an {.fn abm_rules} step without {.arg .by}."),
        class = "tidyABM_bad_target"
      )
    }
  }
  # `NA` is not a group. An agent with no value for the `.by` column sits the
  # step out and keeps what it had, the way `NA` does in `.order`.
  taking_part <- !is.na(combined[[by]])
  narrowed <- FALSE
  if (!is.null(step$when)) {
    w <- eval_condition(step$when, aug, state$globals)
    taking_part <- taking_part & w
    if (!any(taking_part)) return(state)
    if (!all(taking_part)) {
      aug <- vctrs::vec_slice(aug, taking_part)
      narrowed <- TRUE
    }
  }
  n_rules <- length(step$rules)
  for (k in seq_len(n_rules)) {
    r <- step$rules[[k]]
    value <- eval_rule(r, aug, state$globals, grouped = FALSE, by = by,
                       cache = if (narrowed) NULL else .subset2(step, "plan"))
    if (narrowed) {
      val_full <- if (r$target %in% names(combined)) combined[[r$target]]
                  else vctrs::vec_init(value, nrow(combined))
      if (length(value) == 1L) value <- rep(value, nrow(aug))
      common <- vctrs::vec_cast_common(val_full, value)
      val_full <- common[[1]]
      val_full[taking_part] <- common[[2]]
      at <- 0L
      for (nm in names(state$groups)) {
        g <- state$groups[[nm]]
        idx <- at + seq_len(nrow(g))
        at <- at + nrow(g)
        if (nrow(g) == 0L) next
        if (any(taking_part[idx])) {
          g <- assign_rule(g, r$target, val_full[idx], taking_part[idx])
          state$groups[[nm]] <- g
        }
      }
      if (k < n_rules) aug[[r$target]] <- common[[2]]
      next
    }
    if (length(value) == nrow(combined) && !all(taking_part)) {
      value[!taking_part] <- if (r$target %in% names(combined))
        combined[[r$target]][!taking_part] else NA
    }
    # `combined` is the groups stacked in order, so a group's rows are a run
    at <- 0L
    for (nm in names(state$groups)) {
      g <- state$groups[[nm]]
      idx <- at + seq_len(nrow(g))
      at <- at + nrow(g)
      if (nrow(g) == 0L) next
      g <- assign_rule(g, r$target, value[idx], taking_part[idx])
      state$groups[[nm]] <- g
    }
    # the next rule reads what this one wrote; a bare vector of the right
    # length goes in with the class off, and after the last rule nothing does
    if (is.atomic(value) && is.null(attributes(value)) &&
        length(value) %in% c(1L, nrow(combined))) {
      if (k < n_rules) {
        value <- rep_len(value, nrow(combined))
        combined <- set_col(combined, r$target, value)
        aug <- set_col(aug, r$target, value)
      }
    } else {
      combined[[r$target]] <- value
      aug[[r$target]] <- value
    }
  }
  state
}

#' Run one abm_global step
#' @noRd
run_global <- function(step, state) {
  cvars <- planned(step, "cvars", cell_vars(lapply(step$rules, `[[`, "quo")))
  if (!is.null(step$by)) {
    comb <- bind_groups(state$groups)
    if (length(cvars)) comb <- attach_cell_columns(comb, state, cvars)
    return(run_global_by(step, state, comb))
  }
  if (length(state$groups) > 1L && !length(cvars) &&
      planned(step, "narrowable", narrowable(lapply(step$rules, `[[`, "quo")))) {
    np <- planned_for(step, "narrow", schema_key(state),
                      narrow_plan(lapply(step$rules, `[[`, "quo"), state$groups))
    if (!is.null(np)) return(run_global_narrow(step, state, np))
  }
  combined <- bind_groups(state$groups,
                          planned(step, "reads", read_vars(lapply(step$rules, `[[`, "quo"))),
                          ids = FALSE)
  if (length(cvars)) combined <- attach_cell_columns(combined, state, cvars)
  ev <- if (length(step$rules) > 1L) shared_eval(combined, state$globals)
  masks <- planned(step, "masks", vapply(step$rules, function(r) {
    needs_mask(rlang::quo_get_expr(r$quo))
  }, NA))
  for (i in seq_along(step$rules)) {
    r <- step$rules[[i]]
    # `reframe()` costs a hundred times what `eval_tidy()` does, for the same
    # answer whenever the expression needs nothing only a mask supplies. A
    # result of the wrong length goes back through `reframe()`.
    val <- NULL
    if (!is.null(ev) && isTRUE(r$plain)) {
      val <- ev$eval(r$quo)
    } else if (isTRUE(attr(r$quo, "abm_base", exact = TRUE))) {
      # see [eval_rule()]
      val <- eval(rlang::quo_get_expr(r$quo), combined,
                  eval_env_cached(r$quo, state$globals, nrow(combined)))
    } else if (!masks[[i]]) {
      val <- rlang::eval_tidy(
        rlang::quo_get_expr(r$quo),
        data = combined, env = abm_eval_env(r$quo, state$globals, n = nrow(combined)))
    }
    if (length(val) != 1L) {
      # Globals go through the same dplyr mask as rules, so `n()` means the
      # population size here just as it does inside `abm_rules()`.
      quo <- rlang::quo_set_env(
        r$quo,
        with_tick_env(state$globals, parent = rlang::quo_get_env(r$quo))
      )
      val <- dplyr::pull(dplyr::reframe(combined, .abm_value = !!quo),
                         ".abm_value")
    }
    if (length(val) != 1L) {
      abm_abort(
        c("{.fn abm_global} rules must collapse to a single value.",
          "x" = "{.code {r$target} ~ {deparse1(rlang::quo_get_expr(r$quo))}} returned {length(val)} values.",
          "i" = "Wrap it in an aggregate such as {.fn sum} or {.fn mean}."),
        class = "tidyABM_bad_global"
      )
    }
    state$globals[[r$target]] <- val
    if (!is.null(ev)) ev$set(r$target, val)
  }
  state
}

#' [run_global()] with the group aggregates of some rules evaluated over their
#' group alone (see [narrow_plan()])
#'
#' Only the columns the rules still read are stacked. A rule that cannot be
#' taken this way is evaluated as [run_global()] evaluates it, over a stack of
#' everything the step reads, built when first needed.
#' @noRd
run_global_narrow <- function(step, state, np) {
  groups <- state$groups
  # the globals that are functions: with the formulas' environment, all that
  # decides which function a name in them finds (see [standard_fns_in()])
  gfun <- Filter(is.function, state$globals)
  n <- sum(vapply(groups, vctrs::vec_size, 0L))
  rest <- if (length(np$rest)) bind_groups(groups, np$rest, ids = FALSE)
  full <- NULL
  ev <- shared_eval(if (is.null(rest)) list() else rest, state$globals, n = n)
  masks <- planned(step, "masks", vapply(step$rules, function(r) {
    needs_mask(rlang::quo_get_expr(r$quo))
  }, NA))
  for (i in seq_along(step$rules)) {
    r <- step$rules[[i]]
    p <- np$rules[[i]]
    val <- NULL
    if (!is.null(p)) {
      val <- narrow_eval(p, r$quo, groups, rest, ev$env(r$quo), n,
                         sig = list(rlang::quo_get_env(r$quo), gfun))
      if (length(val) != 1L) {
        if (is.null(full)) {
          full <- bind_groups(groups, planned(step, "reads",
            read_vars(lapply(step$rules, `[[`, "quo"))), ids = FALSE)
        }
        val <- rlang::eval_tidy(rlang::quo_get_expr(r$quo), data = full, env = ev$env(r$quo))
      }
    } else if (isTRUE(r$plain)) {
      val <- ev$eval(r$quo)
    } else if (!masks[[i]]) {
      val <- rlang::eval_tidy(
        rlang::quo_set_env(r$quo, abm_eval_env(r$quo, state$globals, n = n)),
        data = if (is.null(rest)) list() else rest)
    }
    if (length(val) != 1L) {
      if (is.null(full)) {
        full <- bind_groups(groups, planned(step, "reads",
          read_vars(lapply(step$rules, `[[`, "quo"))), ids = FALSE)
      }
      quo <- rlang::quo_set_env(
        r$quo,
        with_tick_env(state$globals, parent = rlang::quo_get_env(r$quo))
      )
      val <- dplyr::pull(dplyr::reframe(full, .abm_value = !!quo), ".abm_value")
    }
    if (length(val) != 1L) {
      abm_abort(
        c("{.fn abm_global} rules must collapse to a single value.",
          "x" = "{.code {r$target} ~ {deparse1(rlang::quo_get_expr(r$quo))}} returned {length(val)} values.",
          "i" = "Wrap it in an aggregate such as {.fn sum} or {.fn mean}."),
        class = "tidyABM_bad_global"
      )
    }
    state$globals[[r$target]] <- val
    ev$set(r$target, val)
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
  env <- abm_eval_env(by_quo, globals)
  keys <- rlang::eval_tidy(rlang::quo_get_expr(by_quo), data = combined, env = env)
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
        r$quo, with_tick_env(scope, parent = rlang::quo_get_env(r$quo))
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
  # way it narrows `abm_rules()`, unless `.scope = "population"`.
  m <- NULL
  scope_match <- (step$scope %||% "match") == "match"
  if (!is.null(state$match) && scope_match) {
    m <- state$match$match
    if (nrow(m) == 0L) return(state)
    order_ids <- order_ids[order_ids %in% m$.id]
    if (!length(order_ids)) return(state)
  }

  quos_all <- c(lapply(step$rules, `[[`, "quo"),
                if (!is.null(step$when)) list(step$when),
                if (!is.null(step$order)) list(step$order))
  cvars <- cell_vars(quos_all)
  wired_cols <- NULL
  wired_ids <- NULL
  if (length(cvars) && !is.null(state$lattice) && !is.null(state$lattice$on) &&
      state$lattice$on %in% names(state$groups)) {
    wg <- state$groups[[state$lattice$on]]
    cvars <- cvars[cvars %in% names(wg)]
    if (length(cvars)) {
      wired_cols <- as.list(wg)[cvars]
      wired_ids <- if (identical(wg$.id, seq_len(nrow(wg)))) NULL else wg$.id
    }
  }

  if (!is.null(step$when)) {
    all_cols <- model_columns(state$groups)
    pvars_w <- if (!is.null(state$match)) partner_vars(list(step$when))
    comb_w <- if (length(pvars_w)) bind_groups(state$groups, pvars_w)
    keep_ids <- integer()
    for (nm in names(state$groups)) {
      g <- state$groups[[nm]]
      if (!nrow(g) || !condition_applies(step$when, g, all_cols, state$globals)) next
      aug_w <- if (is.null(m) && !reads_match(list(step$when))) g else
        augment_group(g, state$match, comb_w %||% bind_groups(state$groups), state$relations,
                      partner = pvars_w)
      if (length(cvars)) aug_w <- attach_cell_columns(aug_w, state, cvars)
      w <- eval_condition(step$when, aug_w, state$globals)
      keep_ids <- c(keep_ids, g$.id[w])
    }
    order_ids <- order_ids[order_ids %in% keep_ids]
    if (!length(order_ids)) return(state)
  }

  if (is.null(step$order)) {
    order_ids <- shuffle(order_ids)
  } else {
    # The order agents are processed in is part of some models -- a queue, a
    # sequential-service constraint -- and a fresh shuffle is then the wrong
    # answer rather than an arbitrary one.
    combined <- bind_groups(state$groups)
    if (length(cvars)) combined <- attach_cell_columns(combined, state, cvars)
    key <- eval_rule(list(quo = step$order), combined, state$globals,
                     grouped = FALSE)
    if (length(key) == 1L) key <- rep(key, nrow(combined))
    keep <- !is.na(key) & combined$.id %in% order_ids
    order_ids <- combined$.id[keep][order(key[keep])]
    if (!length(order_ids)) return(state)
  }

  p_cols <- character()
  named <- c(unlist(lapply(step$rules, `[[`, "vars"), use.names = FALSE),
             vapply(step$rules, `[[`, "", "target"))
  if (!is.null(m)) {
    # only the partner columns the step names are materialised: doing it for
    # every column of every agent is what would make this loop slow
    p_cols <- unique(sub("^partner_", "", grep("^partner_", named, value = TRUE)))
  }

  # Relation columns the step mentions, resolved once. Their values are held
  # as bare vectors keyed by pair for the loop's duration, like `cols`: a
  # goods market makes tens of thousands of these writes per tick.
  rel_refs <- Filter(Negate(is.null),
                     stats::setNames(lapply(unique(named), resolve_relation_name,
                                            relations = state$relations),
                                     unique(named)))
  rel_targets <- lapply(step$rules, function(r) resolve_relation_name(r$target, state$relations))
  if (length(rel_refs) && (is.null(m) || state$match$size != 2L) &&
      any(!vapply(rel_targets, is.null, logical(1)))) {
    tg <- step$rules[[which(!vapply(rel_targets, is.null, logical(1)))[[1]]]]$target
    abm_abort(
      c("{.fn abm_sequential} writes {.field {tg}}, a value on a pair, but no pairing is standing.",
        "i" = "Put an {.fn abm_match} that pairs agents two at a time before it."),
      class = "tidyABM_no_match"
    )
  }
  rels <- lapply(state$relations, function(rel) {
    list(key = relation_key(rel$edges$from, rel$edges$to),
         cols = as.list(rel$edges[rel$cols]), n = nrow(rel$edges))
  })

  # each rule gets an environment of its own, holding the globals. Writing a
  # global assigns into those environments rather than rebuilding a mask, which
  # is what keeps "the next agent sees what I just spent" cheap.
  envs <- lapply(step$rules, function(r) abm_eval_env(r$quo, state$globals))
  quos <- Map(function(r, e) rlang::quo_set_env(r$quo, e), step$rules, envs)
  is_global <- vapply(step$rules, function(r) r$target %in% names(state$globals),
                      logical(1))

  cols <- lapply(state$groups, function(g) as.list(g))
  sizes <- vapply(state$groups, nrow, integer(1))
  todo <- stats::setNames(vector("list", length(cols)), names(cols))

  # Everything the loop would otherwise look up per agent by name -- its group
  # and row, its partner's, and where the pair sits in a relation -- resolved
  # here in one vectorised pass. `x[["17"]]` on a named vector is a linear
  # scan, so per agent it made the loop quadratic in the population. A
  # relation gains no rows inside the loop, so a pair's row cannot move.
  gnames <- names(state$groups)
  all_ids <- unlist(lapply(state$groups, function(g) g$.id), use.names = FALSE)
  all_g <- rep(seq_along(gnames), sizes)
  all_row <- sequence(sizes)
  pos <- match(order_ids, all_ids)
  pid_v <- rep(NA_integer_, length(order_ids))
  cost_v <- rep(NA_real_, length(order_ids))
  if (!is.null(m)) {
    mi <- match(order_ids, m$.id)
    pid_v <- m$.partner[mi]; role_v <- m$.role[mi]; gid_v <- m$.group_id[mi]
    if (".cost" %in% names(m)) cost_v <- m$.cost[mi]
    ppos <- match(pid_v, all_ids)
  }
  pair_row <- function(h) {
    if (is.null(h) || is.null(m)) return(NULL)
    match(if (h$back) relation_key(pid_v, order_ids) else relation_key(order_ids, pid_v),
          rels[[h$rel]]$key)
  }
  ref_ri <- lapply(rel_refs, pair_row)
  tgt_ri <- lapply(rel_targets, pair_row)
  partner_tgt <- sub("^partner_", "", vapply(step$rules, `[[`, "", "target"))
  n_one <- function() 1L

  for (ai in seq_along(order_ids)) {
    id <- order_ids[[ai]]
    gname <- gnames[[all_g[[pos[[ai]]]]]]
    v <- cols[[gname]]
    i <- all_row[[pos[[ai]]]]

    if (is.null(todo[[gname]])) {
      todo[[gname]] <- applicable_rules(step, names(v), cols, state$globals)
    }
    which_rules <- todo[[gname]]
    if (!length(which_rules)) next

    # one mask per agent, kept up to date in place: a rule sees what the rule
    # above it wrote, which is what "one agent at a time" already implies
    # `.subset2` rather than `function(x) x[[i]]`: a closure made per agent
    # is byte-compiled per agent
    data <- lapply(v, .subset2, i)
    if (!"n" %in% names(data)) data$n <- n_one
    if (!is.null(wired_cols) && ".cell" %in% names(v)) {
      ci <- v$.cell[[i]]
      cidx <- if (is.null(wired_ids)) ci else match(ci, wired_ids)
      for (cn in names(wired_cols)) {
        data[[paste0("cell_", cn)]] <- if (is.na(cidx)) wired_cols[[cn]][NA_integer_] else wired_cols[[cn]][[cidx]]
      }
    }

    pid <- NA_integer_; pg <- NULL; pj <- NA_integer_
    if (!is.null(m)) {
      data$.partner  <- pid_v[[ai]]
      data$.role     <- role_v[[ai]]
      data$.group_id <- gid_v[[ai]]
      data$.cost     <- cost_v[[ai]]
      pid <- data$.partner
      if (!is.na(pid)) {
        pg <- gnames[[all_g[[ppos[[ai]]]]]]
        pj <- all_row[[ppos[[ai]]]]
      }
      for (nm in p_cols) {
        src <- if (is.na(pid)) NULL
               else if (identical(pg, gname)) v else cols[[pg]]
        data[[paste0("partner_", nm)]] <-
          if (!is.null(src) && nm %in% names(src)) src[[nm]][[pj]] else NA
      }
      for (nm in names(rel_refs)) {
        h <- rel_refs[[nm]]
        ri <- ref_ri[[nm]][[ai]]
        data[[nm]] <- if (is.null(h$col)) !is.na(ri)
                      else if (is.na(ri) || is.null(rels[[h$rel]]$cols[[h$col]])) NA
                      else rels[[h$rel]]$cols[[h$col]][[ri]]
      }
    }

    for (k in which_rules) {
      value <- rlang::eval_tidy(quos[[k]], data = data)
      target <- step$rules[[k]]$target
      h <- rel_targets[[k]]
      if (!is.null(h)) {
        # the only other place a sequential rule leaves its own row: the pair
        if (is.null(h$col)) {
          abm_abort(
            c("{.field {target}} says whether the pair exists; it is not written by a rule.",
              "i" = 'Add or remove the pair with {.code abm_link(via = "{h$rel}")} / {.code abm_unlink(via = "{h$rel}")}.'),
            class = "tidyABM_bad_target"
          )
        }
        if (is.na(pid)) next
        a <- if (h$back) pid else id; b <- if (h$back) id else pid
        ri <- tgt_ri[[k]][[ai]]
        if (is.na(ri)) {
          abm_abort(
            c("{.fn abm_sequential} writes {.field {target}}, but agent {a} is not related to agent {b} via {.field {h$rel}}.",
              "i" = 'Create the pair first with {.code abm_link(via = "{h$rel}")}.'),
            class = "tidyABM_not_related"
          )
        }
        rels[[h$rel]]$cols[[h$col]] <- write_at(rels[[h$rel]]$cols[[h$col]], ri,
                                                value, rels[[h$rel]]$n)
        data[[target]] <- value[[1]]
      } else if (startsWith(target, "partner_") && !is.null(m)) {
        # the only place a sequential rule leaves its own row: the agent it is
        # matched with, which is what a transaction is
        nm <- partner_tgt[[k]]
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
    state$groups[[nm]] <- new_tbl(cols[[nm]], sizes[[nm]])
  }
  for (rn in names(rels)) {
    rel <- state$relations[[rn]]
    for (cc in names(rels[[rn]]$cols)) {
      rel$edges[[cc]] <- rels[[rn]]$cols[[cc]]
      if (!cc %in% rel$cols) {
        rel$cols <- c(rel$cols, cc)
        rel$defaults[[cc]] <- vctrs::vec_init(rels[[rn]]$cols[[cc]], 1L)
      }
    }
    state$relations[[rn]] <- rel
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
