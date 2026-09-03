# The behavioural block --------------------------------------------------

new_abm_go <- function(steps) {
  structure(list(steps = steps), class = "abm_go")
}

#' Declare what happens each tick
#'
#' `abm_go()` is the second of the three functions a model is made of --
#' [abm_setup()], then `abm_go()`, then [abm_run()]. It is the behavioural
#' block: an ordered sequence of typed steps replayed once per tick. Steps are
#' dispatched by *type and position*, not by argument name, so a model with
#' several phases is written flat and in order:
#'
#' ``` abm_go( abm_match(pair = "random"), abm_rules(payoff ~ ...),   # phase
#' 1: play abm_match(pair = "random"), abm_rules(strategy ~ ...)  # phase 2:
#' imitate ) ```
#'
#' The sequence is validated once, here, rather than on every tick. Three rules
#' apply:
#'
#' * it cannot be empty;
#' * no two [abm_match()] steps may sit next to each other, since the first
#'   would be discarded unused;
#' * it cannot end on an [abm_match()], for the same reason.
#'
#' Everything else is allowed. In particular a model may use no matching at all
#' (El Farol, or a pure redistribution model), and several update steps may
#' follow a single match, the market model pairs once and then applies separate
#' rules to buyers and to sellers.
#'
#' @param ... Step objects: [abm_match()], [abm_rules()], [abm_sequential()],
#'   [abm_global()], [abm_neighbours()], [abm_tell()], [abm_birth()],
#'   [abm_death()], [abm_link()], [abm_unlink()], [abm_draw()], [abm_repeat()].
#'
#' @return An `abm_go` object.
#' @export
#' @examples
#' # Every model is the same three parts: abm_setup(), abm_go(), abm_run().
#' rumour <- abm_setup(agents = abm_agents(
#'   n = 200, state = ~c("spreader", rep("ignorant", n - 1))))
#'
#' go <- abm_go(
#'   abm_match(pair = "random"),
#'   abm_rules(state ~ ifelse(partner_state == "spreader", "spreader", state))
#' )
#'
#' abm_run(rumour, go, ticks = 10, seed = 1)
abm_go <- function(...) {
  steps <- rlang::list2(...)
  validate_steps(steps, "abm_go")
  new_abm_go(steps)
}

#' Check a list of steps for the three sequencing rules
#'
#' Shared by [abm_go()] and [abm_repeat()], which are the two places a block of
#' steps is declared.
#' @noRd
validate_steps <- function(steps, fn, call = rlang::caller_env()) {
  if (length(steps) == 0L) {
    abm_abort(
      c("{.fn {fn}} needs at least one step.",
        "i" = "Steps are built with {.fn abm_match}, {.fn abm_rules}, {.fn abm_global}, {.fn abm_birth} and {.fn abm_death}."),
      class = "tidyABM_empty_go", call = call
    )
  }

  is_step <- vapply(steps, inherits, logical(1), "abm_step")
  if (!all(is_step)) {
    bad <- which(!is_step)[[1]]
    abm_abort(
      c("Every argument to {.fn {fn}} must be a step.",
        "x" = "Argument {bad} is {.cls {class(steps[[bad]])[[1]]}}.",
        "i" = "Steps come from {.fn abm_match}, {.fn abm_rules}, {.fn abm_sequential}, {.fn abm_global}, {.fn abm_birth} or {.fn abm_death}."),
      class = "tidyABM_not_a_step", call = call
    )
  }

  kinds <- vapply(steps, function(s) class(s)[[1]], character(1))

  dup <- which(kinds[-1] == "abm_match" & kinds[-length(kinds)] == "abm_match")
  if (length(dup)) {
    abm_abort(
      c("Two {.fn abm_match} steps in a row.",
        "x" = "Step {dup[[1]]} and step {dup[[1]] + 1L} are both {.fn abm_match}.",
        "i" = "The first match would be discarded unused. Put an update step between them."),
      class = "tidyABM_bad_sequence", call = call
    )
  }

  if (kinds[[length(kinds)]] == "abm_match") {
    abm_abort(
      c("The sequence ends on an {.fn abm_match}.",
        "x" = "Step {length(kinds)} is the last one, and nothing uses its pairing.",
        "i" = "Follow it with {.fn abm_rules}, or drop it."),
      class = "tidyABM_bad_sequence", call = call
    )
  }
  invisible(steps)
}

#' @export
print.abm_go <- function(x, ...) {
  kinds <- vapply(x$steps, function(s) class(s)[[1]], character(1))
  n_phase <- sum(kinds == "abm_match")
  cli::cli_text("{.cls abm_go} {length(x$steps)} step{?s}, {n_phase} match phase{?s}")
  for (i in seq_along(x$steps)) {
    s <- x$steps[[i]]
    label <- switch(
      kinds[[i]],
      abm_match      = paste0("match  ", rlang::quo_squash(s$pair),
                              if (s$size != 2L) paste0(" (size ", s$size, ")") else ""),
      abm_rules      = paste0("rules  ", length(s$rules), " rule(s)"),
      abm_sequential = paste0("seq    ", length(s$rules), " rule(s)"),
      abm_global     = paste0("global ", length(s$rules), " rule(s)"),
      abm_neighbours = paste0("nbrs   ", length(s$rules), " rule(s)"),
      abm_move       = paste0("move   along ", s$along),
      abm_tell       = paste0("tell   ", length(s$rules), " rule(s)"),
      abm_birth      = "birth",
      abm_death      = "death",
      abm_draw       = paste0("draw   ", length(s$rules), " value(s) per ", s$each),
      abm_link       = "link",
      abm_unlink     = "unlink",
      abm_repeat     = paste0("repeat ", length(s$steps), " step(s), max ", s$max)
    )
    cli::cli_text("{.emph {sprintf('%2d.', i)}} {label}")
  }
  invisible(x)
}
