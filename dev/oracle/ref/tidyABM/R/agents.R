# Agent specifications ---------------------------------------------------

new_abm_agents <- function(n, cols, at = NULL) {
  structure(list(n = n, cols = cols, at = at), class = "abm_agents")
}

#' Declare a group of agents
#'
#' `abm_agents()` describes one homogeneous group of agents: how many there are
#' and what columns they start with. It is a *specification*, not a population,
#' nothing is created until the spec is passed to [abm_setup()], which is where
#' a model's first part is declared.
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
#' # On a lattice
#'
#' Two things change when the model has a grid or line network (see
#' [abm_network()]). The group that network is wired to **omits `n`**: its count
#' is `prod(dims)`, and passing a matching `n` is allowed while a mismatch is an
#' error. Every other group gets a `.cell` saying which cell it is standing on,
#' drawn uniformly at random unless `at` says otherwise.
#'
#' @param n Number of agents in this group. A single positive integer. Optional,
#'   and ignored, for the group a grid or line [abm_network()] is wired to,
#'   which inherits `prod(dims)` instead.
#' @param ... Named column specifications. Each is either a plain value (which
#'   is recycled) or a one-sided formula (which is evaluated once, per group).
#'   Names beginning with `.` are reserved for the package.
#' @param at Where on the lattice this group starts, as a one-sided formula
#'   evaluated once, like a column. It must yield a cell id (a wired-group
#'   `.id`), either one to recycle or one per agent. The expression sees this
#'   group's own columns, `n`, `dims`, and the wired group's columns, so both
#'   `at = ~sample(prod(dims), n, replace = TRUE)` and `at = ~which(nest)[1]`
#'   work. Only meaningful in a model with a lattice; the default is a uniform
#'   random placement.
#'
#' @return An `abm_agents` specification object.
#' @export
#' @examples
#' abm_agents(n = 500, money = 100)
#'
#' # A spec is not a population: hand it to abm_setup(), the first of the
#' # three parts a model is made of.
#' economy <- abm_setup(agents = abm_agents(n = 500, money = 100))
#'
#' go <- abm_go(
#'   abm_match(pair = "random", role = list(giver = money > 0, receiver = TRUE)),
#'   abm_rules(money ~ if_else(.role == "giver", money - 1, money + 1))
#' )
#'
#' abm_run(economy, go, ticks = 10, seed = 1)
#'
#' # on a lattice: the wired group inherits its count, the mobile one is placed
#' abm_agents(alive = ~runif(n) < 0.1)
#' abm_agents(n = 100, energy = ~runif(n, 4, 8),
#'            at = ~sample(prod(dims), n, replace = TRUE))
abm_agents <- function(n = NULL, ..., at = NULL) {
  # `n` is optional only so that a grid-wired group can inherit it; every other
  # group is still checked, at abm_setup() time, where it is known whether the
  # model has a lattice at all.
  if (!is.null(n) && (!rlang::is_scalar_integerish(n) || is.na(n) || n < 0)) {
    abm_abort(
      c("{.arg n} must be a single non-negative number.",
        "x" = "Got {.code {deparse1(substitute(n))}}."),
      class = "tidyABM_bad_n"
    )
  }
  at <- enquo_or_null(rlang::enquo(at))
  if (!is.null(at)) {
    at_expr <- rlang::quo_get_expr(at)
    # `at = ~expr` is the documented form, matching a column spec. Unwrap the
    # `~` so the quosure evaluates the way a column formula does; a bare
    # expression is already in that shape.
    if (rlang::is_formula(at_expr) && length(at_expr) == 2L) {
      at <- rlang::new_quosure(at_expr[[2]], rlang::quo_get_env(at))
    }
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
  validate_abm_agents(new_abm_agents(if (is.null(n)) NULL else as.integer(n),
                                     cols, at))
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
      value <- withCallingHandlers(
        rlang::eval_tidy(quo, data = c(as.list(out), mask_extra)),
        error = function(e) {
          if (inherits(e, "tidyABM_error")) return()
          rethrow_column_error(e, nm, group_name, spec, names(out), call)
        })
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

#' Say which setup formula failed, and what it could have read
#'
#' A column formula is evaluated with `n` and the columns declared before it
#' in scope. The bare "object 'x' not found" does not say that, nor that a
#' column declared *after* this one is not yet visible.
#' @noRd
rethrow_column_error <- function(e, nm, group_name, spec, before, call) {
  before <- setdiff(before, ".id")
  after <- names(spec$cols)[seq_along(spec$cols) > match(nm, names(spec$cols))]
  bullets <- c("Column {.field {nm}} of group {.field {group_name}} could not be computed.",
               "x" = cli_escape(conditionMessage(e)))
  miss <- missing_object_name(e)
  class <- "tidyABM_setup_error"
  # `~runif(n())` finds dplyr's `n()` through the attached package and dies
  # with its "data-masking verbs" message
  if (grepl("data-masking", conditionMessage(e), fixed = TRUE) ||
      (!is.null(miss) && miss$fn && miss$name == "n")) {
    bullets <- c(bullets, "i" = "In a setup formula {.code n} is the number of agents, not a function: write {.code n}, not {.code n()}.")
  } else if (!is.null(miss)) {
    name <- miss$name
    class <- c("tidyABM_missing_column", class)
    if (name %in% after) {
      bullets <- c(bullets, "i" = "{.field {name}} is declared after {.field {nm}}. Formulas run in order, so move it up.")
    } else if (!miss$fn) {
      near <- near_names(name, c(before, "n", if (!is.null(spec$lattice_cols)) spec$lattice_cols))
      if (length(near)) bullets <- c(bullets, "i" = "Did you mean {.field {near}}?")
    }
  }
  bullets <- c(bullets,
               "i" = if (length(before)) "A setup formula sees {.code n} and the columns declared before it: {.field {before}}."
                     else "A setup formula sees {.code n}; no column is declared before this one.")
  abm_abort(bullets, class = class, call = call, .envir = environment())
}

#' @export
print.abm_agents <- function(x, ...) {
  cli::cli_text("{.cls abm_agents} {.strong {x$n %||% 'grid-many'}} agent{?s}")
  if (!is.null(x$at)) {
    cli::cli_bullets(c("*" = "at = {.code {deparse1(rlang::quo_get_expr(x$at))}}"))
  }
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
