# Repeating a block of steps within a tick -------------------------------

new_abm_repeat <- function(steps, until, max) {
  structure(list(steps = steps, until = until, max = max),
            class = c("abm_repeat", "abm_step"))
}

#' Repeat a block of steps until a condition holds
#'
#' A tick is normally one pass through [abm_go()]. Some models have a *phase*
#' inside the tick that has to run to completion before the next phase starts:
#' an epidemic that burns out before anyone reconsiders whether to vaccinate, a
#' round of proposals that continues until nobody is rejected, a market that
#' clears. `abm_repeat()` is that phase. It holds a block of steps and replays
#' it until `until` is true, or `max` times, whichever comes first.
#'
#' `until` is evaluated the way an [abm_global()] right-hand side is, over the
#' whole population, with the globals in scope, and must collapse to a single
#' `TRUE` or `FALSE`. It is checked *after* each pass, so the block always runs
#' at least once. `max` is required, because a condition that never becomes
#' true would otherwise hang the run.
#'
#' If a match is standing when the block runs, `until` also sees `.role` and
#' `partner_<col>`, as a rule does. A phase is usually a phase *of an
#' encounter*, and its stopping condition is usually about the pair rather than
#' about either agent alone.
#'
#' The same idea covers early stopping for a whole model: a block wrapped in
#' `abm_repeat(until = <absorbed>, max = <ticks>)` and run for a single tick
#' stops as soon as the model reaches its absorbing state, instead of
#' recomputing a fixed point for the rest of the run.
#'
#' @param ... Step objects, validated as [abm_go()] validates them.
#' @param until A condition checked after each pass. Must collapse to one
#'   logical value. `NULL` (the default) means always run `max` times.
#' @param max The maximum number of passes. Required.
#'
#' @return An `abm_repeat` step object.
#' @seealso [abm_go()], which lists every step and fixes the order they run
#'   in. The block it wraps is validated exactly as [abm_go()] validates a
#'   tick.
#' @export
#' @examples
#' # an epidemic that burns out before the tick ends
#' abm_repeat(
#'   abm_neighbours(exposure ~ sum(state == "I")),
#'   abm_rules(state ~ ifelse(state == "I", "R", state)),
#'   until = sum(state == "I") == 0,
#'   max = 500
#' )
#'
#' # a bargaining phase, which stops when every pair has met or given up
#' abm_repeat(
#'   abm_rules(bid ~ pmin(bid + step, reservation)),
#'   until = all(bid >= partner_ask | bid >= reservation),
#'   max = 20
#' )
abm_repeat <- function(..., until = NULL, max) {
  steps <- rlang::list2(...)
  if (missing(max)) {
    abm_abort(
      c("{.fn abm_repeat} needs a {.arg max}.",
        "i" = "A condition that never becomes true would otherwise hang the run."),
      class = "tidyABM_missing_arg"
    )
  }
  if (!rlang::is_scalar_integerish(max) || max < 1) {
    abm_abort("{.arg max} must be a single whole number of at least 1.",
              class = "tidyABM_bad_max")
  }
  validate_steps(steps, "abm_repeat")
  new_abm_repeat(steps, enquo_or_null(rlang::enquo(until)), as.integer(max))
}

#' @noRd
run_repeat <- function(step, state) {
  # a pairing made inside the block belongs to the block, the way the tick
  # already discards the one it ends on
  outer_match <- state$match
  for (i in seq_len(step$max)) {
    for (s in step$steps) state <- run_step(s, state)
    if (is.null(step$until)) next
    # a phase inside a tick usually runs on a standing match, and its stopping
    # condition is about the pairing ("until nobody's bid is still under their
    # partner's ask"), so `until` sees `.role` and `partner_<col>` as a rule does
    combined <- bind_groups(state$groups)
    combined <- augment_group(combined, state$match, combined)
    quo <- rlang::quo_set_env(
      step$until,
      rlang::new_environment(state$globals,
                             parent = rlang::quo_get_env(step$until))
    )
    val <- dplyr::pull(dplyr::reframe(combined, .abm_value = !!quo),
                       ".abm_value")
    if (length(val) != 1L || !is.logical(val)) {
      abm_abort(
        c("{.arg until} must collapse to a single {.code TRUE} or {.code FALSE}.",
          "x" = "{.code {deparse1(rlang::quo_get_expr(step$until))}} returned {length(val)} value{?s} of type {.cls {class(val)[[1]]}}."),
        class = "tidyABM_bad_until"
      )
    }
    if (isTRUE(val)) break
  }
  state$repeats <- i
  state$match <- outer_match
  state
}

#' @export
print.abm_repeat <- function(x, ...) {
  cli::cli_text("{.cls abm_repeat} {length(x$steps)} step{?s}, at most {x$max} pass{?es}")
  if (!is.null(x$until)) {
    cli::cli_bullets(c("*" = "until = {.code {deparse1(rlang::quo_get_expr(x$until))}}"))
  }
  invisible(x)
}
