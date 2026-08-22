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
  dplyr::bind_rows(groups)
}

#' Shorthand for a tibble of NA-padded columns
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x
