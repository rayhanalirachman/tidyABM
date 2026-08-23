# Agent specifications ---------------------------------------------------

new_abm_agents <- function(n, cols) {
  structure(list(n = n, cols = cols), class = "abm_agents")
}

#' Declare a group of agents
#'
#' `abm_agents()` describes one homogeneous group of agents: how many there are
#' and what columns they start with. It is a *specification*, not a population —
#' nothing is created until the spec is passed to [abm_setup()].
#'
#' Column values follow one rule:
#'
#' * a plain value (`money = 100`) is recycled across every agent;
#' * a one-sided formula (`money = ~runif(n, 0, 50)`) is evaluated **once** and
#'   must return either a length-`n` vector or a length-1 value to recycle.
#'
#' Formulas are evaluated in order, and each one can see `n` as well as any
#' column defined before it, so `abm_agents(n = 200, wtp = ~rnorm(n, 50, 10),
#' offer = ~wtp * 0.8)` works as written.
#'
#' @param n Number of agents in this group. A single positive integer.
#' @param ... Named column specifications. Each is either a plain value (which
#'   is recycled) or a one-sided formula (which is evaluated once, per group).
#'   Names beginning with `.` are reserved for the package.
#'
#' @return An `abm_agents` specification object.
#' @export
#' @examples
#' abm_agents(n = 500, money = 100)
#'
#' # A spec becomes a population in abm_setup(), and a run needs all three parts.
#' economy <- abm_setup(agents = abm_agents(n = 500, money = 100))
#'
#' go <- abm_go(
#'   abm_match(pair = "random", role = list(giver = money > 0, receiver = TRUE)),
#'   abm_rules(money ~ if_else(.role == "giver", money - 1, money + 1))
#' )
#'
#' abm_run(economy, go, ticks = 10, seed = 1)
abm_agents <- function(n, ...) {
  if (!rlang::is_scalar_integerish(n) || is.na(n) || n < 0) {
    abm_abort(
      c("{.arg n} must be a single non-negative number.",
        "x" = "Got {.code {deparse1(substitute(n))}}."),
      class = "tidyABM_bad_n"
    )
  }
  cols <- rlang::list2(...)
  if (length(cols) && !rlang::is_named(cols)) {
    abm_abort(
      "Every column passed to {.fn abm_agents} must be named.",
      class = "tidyABM_unnamed_column"
    )
  }
  bad <- grep("^\\.", names(cols), value = TRUE)
  if (length(bad)) {
    abm_abort(
      c("Column names beginning with {.code .} are reserved by tidyABM.",
        "x" = "Rename {.field {bad}}."),
      class = "tidyABM_reserved_column"
    )
  }
  validate_abm_agents(new_abm_agents(as.integer(n), cols))
}

validate_abm_agents <- function(x, call = rlang::caller_env()) {
  for (nm in names(x$cols)) {
    val <- x$cols[[nm]]
    if (rlang::is_formula(val) && length(val) == 3L) {
      abm_abort(
        c("Column {.field {nm}} was given a two-sided formula.",
          "i" = "Use a one-sided formula, e.g. {.code {nm} = ~runif(n)}."),
        class = "tidyABM_bad_column_spec", call = call
      )
    }
  }
  x
}

#' Build the tibble for one agent group
#' @noRd
materialise_agents <- function(spec, group_name, id_offset = 0L,
                               call = rlang::caller_env()) {
  n <- spec$n
  out <- tibble::tibble(.id = id_offset + seq_len(n))
  mask_extra <- list(n = n)

  for (nm in names(spec$cols)) {
    val <- spec$cols[[nm]]
    if (is_formula1(val)) {
      quo <- rlang::as_quosure(val)
      value <- rlang::eval_tidy(quo, data = c(as.list(out), mask_extra))
    } else {
      value <- val
    }
    if (length(value) == 1L) {
      value <- rep(value, n)
    } else if (length(value) != n) {
      abm_abort(
        c("Column {.field {nm}} has the wrong length.",
          "x" = "It returned {length(value)} value{?s}, but there {?is/are} {n} agent{?s}.",
          "i" = "A column must return one value (to recycle) or exactly {n}."),
        class = "tidyABM_bad_column_length", call = call
      )
    }
    out[[nm]] <- value
  }
  out$.group <- group_name
  dplyr::relocate(out, ".id", ".group")
}

#' @export
print.abm_agents <- function(x, ...) {
  cli::cli_text("{.cls abm_agents} {.strong {x$n}} agent{?s}")
  if (length(x$cols)) {
    for (nm in names(x$cols)) {
      val <- x$cols[[nm]]
      shown <- if (is_formula1(val)) {
        paste0("~", deparse1(rlang::f_rhs(val)))
      } else {
        deparse1(val)
      }
      cli::cli_bullets(c("*" = "{.field {nm}} = {.code {shown}}"))
    }
  } else {
    cli::cli_bullets(c("*" = "{.emph no columns}"))
  }
  invisible(x)
}
