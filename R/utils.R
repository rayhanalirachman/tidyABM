# Internal helpers -------------------------------------------------------

#' Abort with a tidyABM error class
#' @noRd
abm_abort <- function(message, class = NULL, call = rlang::caller_env(),
                      .envir = parent.frame(), ...) {
  cli::cli_abort(
    message,
    class = c(class, "tidyABM_error"),
    call = call,
    .envir = .envir,
    ...
  )
}

#' Is `x` a one-sided or two-sided formula?
#' @noRd
is_formula2 <- function(x) rlang::is_formula(x) && length(x) == 3L

#' Is `x` a one-sided formula (`~expr`)?
#' @noRd
is_formula1 <- function(x) rlang::is_formula(x) && length(x) == 2L

#' The left-hand side of a two-sided formula, as a single name
#' @noRd
f_lhs_name <- function(x, arg = "rule", call = rlang::caller_env()) {
  lhs <- rlang::f_lhs(x)
  if (!rlang::is_symbol(lhs)) {
    abm_abort(
      c("The left-hand side of a {arg} must be a single column name.",
        "x" = "Got {.code {deparse1(lhs)}}."),
      class = "tidyABM_bad_formula", call = call
    )
  }
  rlang::as_string(lhs)
}

#' Variables referenced on the right-hand side of a formula
#' @noRd
f_rhs_vars <- function(x) {
  all.vars(rlang::f_rhs(x))
}

#' Capture an expression argument as a quosure, or return NULL if absent
#' @noRd
enquo_or_null <- function(quo) {
  if (rlang::quo_is_null(quo) || rlang::quo_is_missing(quo)) NULL else quo
}

#' Column names named by a tidyselect-ish `by =` argument
#'
#' `by` is captured as a quosure. It may be a single symbol (`opinion`),
#' a call to `c()` (`c(x, y)`), or a character vector.
#' @noRd
by_columns <- function(by_quo, call = rlang::caller_env()) {
  if (is.null(by_quo)) return(NULL)
  expr <- rlang::quo_get_expr(by_quo)
  if (rlang::is_symbol(expr)) return(rlang::as_string(expr))
  if (rlang::is_call(expr, "c")) {
    parts <- as.list(expr)[-1]
    return(vapply(parts, function(p) {
      if (rlang::is_symbol(p)) rlang::as_string(p) else as.character(p)
    }, character(1)))
  }
  val <- rlang::eval_tidy(by_quo)
  if (is.character(val)) return(val)
  abm_abort(
    "{.arg by} must be a column name, {.code c(col1, col2)}, or a character vector.",
    class = "tidyABM_bad_by", call = call
  )
}

#' Shuffle a vector, safely
#'
#' `sample(x)` reinterprets a length-1 numeric `x` as `seq_len(x)`, which for a
#' vector of agent ids means a population of one silently becomes a population
#' of `.id` agents. Every shuffle in the package goes through here.
#' @noRd
shuffle <- function(x) if (length(x) <= 1L) x else sample(x)

#' Draw `k` values from `x` with replacement, safely
#'
#' Same trap as [shuffle()]: `sample(x, k, replace = TRUE)` with a length-1 `x`
#' draws from `seq_len(x)` instead of from `x`.
#' @noRd
draw_from <- function(x, k) x[sample.int(length(x), k, replace = TRUE)]

#' Bind a list of tibbles with differing schemas into one long tibble
#' @noRd
bind_groups <- function(groups) {
  if (length(groups) == 1L) {
    return(tibble::as_tibble(groups[[1]]))
  }
  # what `bind_rows()` does, without its argument handling: this runs on
  # every step of every tick
  do.call(vctrs::vec_rbind, unname(groups))
}

#' The columns a set of quosures reads through `partner_<col>`
#' @noRd
partner_vars <- function(quos) {
  vars <- unlist(lapply(Filter(Negate(is.null), quos), function(q) {
    all.vars(rlang::quo_get_expr(q))
  }), use.names = FALSE)
  sub("^partner_", "", grep("^partner_", vars, value = TRUE))
}

#' Edges without the internal columns `abm_draw()` attaches to them
#' @noRd
strip_draws <- function(edges) {
  if (is.null(edges)) return(NULL)
  keep <- !startsWith(names(edges), ".draw_") & !startsWith(names(edges), ".back_")
  edges[, keep, drop = FALSE]
}

#' Membership that understands a set-valued column
#'
#' A set-valued agent column is a list column, so in a pair view `own_sellers`
#' arrives as a list with one element per (chooser, candidate) row. Base `%in%`
#' coerces that side with `as.character()`, compares `"4"` against `"4:5"`, and
#' quietly returns `FALSE` for every row -- which turns `among = .id %in%
#' own_sellers` into a match that pairs nobody, with no error to say so. When
#' the right-hand side is a list the question is asked row by row instead,
#' which is what the formula plainly means.
#'
#' With an atomic right-hand side this is `match(x, table, nomatch = 0) > 0`,
#' base's own definition, so every formula that already worked is unaffected.
#'
#' The list case runs on a pair view, so it is called with (choosers x
#' candidates) rows and sits on the hot path of every `among`. It is done as
#' one comparison over the unlisted sets rather than a `match()` per row: the
#' loop cost 22% of a Lengnick month. `NA` matches `NA`, as it does in base.
#' @noRd
in_rowwise <- function(x, table) {
  if (!is.list(table) || is.data.frame(table)) {
    return(match(x, table, nomatch = 0L) > 0L)
  }
  n <- max(length(x), length(table))
  if (n == 0L) return(logical(0))
  table <- rep_len(table, n)
  len <- lengths(table)

  if (!is.list(x)) {
    if (!sum(len)) return(rep(FALSE, n))
    flat <- unlist(table, use.names = FALSE)
    xi <- rep(rep_len(x, n), len)
    hit <- (flat == xi) | (is.na(flat) & is.na(xi))
    hit[is.na(hit)] <- FALSE
    out <- logical(n)
    out[rep(seq_len(n), len)[hit]] <- TRUE
    return(out)
  }
  x <- rep_len(x, n)
  vapply(seq_len(n),
         function(i) any(match(x[[i]], table[[i]], nomatch = 0L) > 0L),
         logical(1))
}

#' The environment one formula is evaluated in
#'
#' The globals go in scope, and the grammar's own operators go innermost so a
#' formula means the same thing wherever it is written: in `abm_rules()`, in
#' `abm_match(among =)`, in `abm_neighbours(within =)`.
#' @noRd
abm_eval_env <- function(quo, globals, n = NULL) {
  env <- rlang::quo_get_env(quo)
  if (length(globals)) env <- rlang::new_environment(globals, parent = env)
  # `n` is bound when the expression is evaluated outside a dplyr mask, so
  # `n()` still means the number of rows
  ops <- list(`%in%` = in_rowwise)
  if (!is.null(n)) ops$n <- function() n
  rlang::new_environment(ops, parent = env)
}

# dplyr's context helpers: the only things an ungrouped `mutate()` supplies
# that `eval_tidy()` over the same tibble does not. `n()` is bound by
# [abm_eval_env()] instead, so it is not listed.
mask_only_fns <- c(
  "row_number", "cur_group", "cur_group_id", "cur_group_rows", "cur_column",
  "cur_data", "cur_data_all", "across", "pick", "if_any", "if_all", "c_across"
)

#' Does an expression call anything only a dplyr mask can answer?
#'
#' A qualified `dplyr::n()` counts too: it reaches past the bound `n`.
#' @noRd
needs_mask <- function(expr) {
  if (!is.call(expr)) return(FALSE)
  head <- expr[[1]]
  if (rlang::is_call(head, "::")) {
    fn <- rlang::as_string(head[[3]])
    if (fn == "n" || fn %in% mask_only_fns) return(TRUE)
  } else if (rlang::is_symbol(head) && rlang::as_string(head) %in% mask_only_fns) {
    return(TRUE)
  }
  any(vapply(as.list(expr)[-1], needs_mask, logical(1)))
}

# Functions whose value on a row depends on that row alone. A rule built only
# from these means the same thing evaluated per pair as over the population,
# so the per-group evaluation a standing match normally forces can be skipped.
# Nothing here draws, counts, indexes or aggregates: `n()`, `sample()`, `[`,
# `sum()` and any function not listed keep the grouped path.
# ponytail: an allowlist; a rule calling anything else is merely slow, not wrong
elementwise_fns <- c(
  "+", "-", "*", "/", "^", "%%", "%/%", "==", "!=", "<", ">", "<=", ">=",
  "&", "|", "!", "(", "~", "if_else", "ifelse", "case_when", "coalesce",
  "between", "is.na", "pmin", "pmax", "abs", "sqrt", "exp", "log", "log1p",
  "round", "floor", "ceiling", "trunc", "sign", "%in%", "xor", "as.integer",
  "as.numeric", "as.double", "as.logical", "as.character", "paste", "paste0",
  "nchar", "tolower", "toupper"
)

#' Is every call in an expression elementwise?
#' @noRd
is_elementwise <- function(expr) {
  if (!is.call(expr)) return(TRUE)
  head <- expr[[1]]
  if (rlang::is_call(head, "::")) head <- head[[3]]
  if (!rlang::is_symbol(head) || !rlang::as_string(head) %in% elementwise_fns) {
    return(FALSE)
  }
  all(vapply(as.list(expr)[-1], is_elementwise, logical(1)))
}
