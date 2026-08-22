# Writing into other agents' rows ----------------------------------------

new_abm_tell <- function(rules, to, to_quo, when, resolve) {
  structure(list(rules = rules, to = to, to_quo = to_quo, when = when,
                 resolve = resolve),
            class = c("abm_tell", "abm_step"))
}

#' Write a value into another agent's row
#'
#' Every other update step writes to the agent it is evaluated on:
#' [abm_rules()] changes your own columns, [abm_neighbours()] *reads* your
#' neighbours' and writes the summary to you. `abm_tell()` is the one that goes
#' the other way — a sender computes a value and writes it into a *recipient's*
#' row. That is the difference between "how many of my neighbours are shouting"
#' and "I shout at my neighbours", and models built on the second shape —
#' outward persuasion, contagion carrying a dose, an order book where a trade
#' has to mark the counterparty — cannot be written without it.
#'
#' Who receives is set by `to`:
#'
#' * `to = "neighbours"` — every agent the sender is joined to in the model's
#'   [abm_network()]. This is the broadcast: one sender, many recipients.
#' * `to = <expression>` — an expression evaluated in the sender's row that
#'   names a single recipient by `.id`. `to = .partner` writes to the partner a
#'   preceding [abm_match()] gave you; `to = best_bid_holder` writes to whichever
#'   agent a global names. `NA` means the sender says nothing.
#'
#' The right-hand side of each rule is evaluated in the **sender's** row, so it
#' sees the sender's columns, `partner_<col>`, `.role` and the globals, exactly
#' as [abm_rules()] would. The value it produces is then written into the
#' recipient's column of that name. An agent that no one wrote to keeps the value
#' it already had — `abm_tell()` never touches a silent agent's row.
#'
#' Two senders can address the same recipient in one step, and `.resolve` says
#' what happens then. The default, `"last"`, takes an arbitrary one of them,
#' which is right for a persuasion rule where the senders all agree anyway;
#' `"sum"` is right for anything additive, like a dose or an order quantity;
#' `"error"` says the collision is a modelling mistake and should stop the run.
#'
#' @param ... One or more `column ~ expression` rules. The expression is
#'   evaluated in the sender's row; the result is written to the recipient's
#'   column of the same name. The column must already exist on the recipient.
#' @param to `"neighbours"`, or an expression naming the recipient's `.id`.
#' @param when Optional condition on the sender. Only agents for which it holds
#'   send anything.
#' @param .resolve What to do when several senders write to the same recipient
#'   in one step: `"last"` (an arbitrary one wins), `"first"`, `"sum"`,
#'   `"mean"`, `"max"`, `"min"`, or `"error"` to stop.
#'
#' @return An `abm_tell` step object.
#' @export
#' @examples
#' # a matched pair pushes its shared opinion onto everyone around it
#' abm_tell(opinion ~ opinion, to = "neighbours", when = opinion == partner_opinion)
#'
#' # a trade marks the counterparty a global named
#' abm_tell(filled ~ TRUE, to = best_bid_holder, when = ask <= best_bid)
#'
#' # contagion that carries a dose, summed over everyone who coughed on you
#' abm_tell(dose ~ dose + load, to = "neighbours", when = infected, .resolve = "sum")
abm_tell <- function(..., to, when = NULL,
                     .resolve = c("last", "first", "sum", "mean", "max", "min",
                                  "error")) {
  .resolve <- rlang::arg_match(.resolve)
  rules <- collect_rules(rlang::list2(...), "abm_tell")

  to_quo <- rlang::enquo(to)
  if (rlang::quo_is_missing(to_quo)) {
    abm_abort(
      c("{.fn abm_tell} needs a {.arg to}.",
        "i" = 'Either {.code to = "neighbours"} or an expression naming the recipient, e.g. {.code to = .partner}.'),
      class = "tidyABM_missing_arg"
    )
  }
  expr <- rlang::quo_get_expr(to_quo)
  mode <- if (identical(expr, "neighbours")) "neighbours" else "id"
  if (is.character(expr) && !identical(expr, "neighbours")) {
    abm_abort(
      c('The only string {.arg to} accepts is {.val neighbours}.',
        "x" = "Got {.val {expr}}.",
        "i" = "To address one agent, pass the expression unquoted, e.g. {.code to = .partner}."),
      class = "tidyABM_bad_to"
    )
  }

  new_abm_tell(rules, mode, if (mode == "id") to_quo else NULL,
               enquo_or_null(rlang::enquo(when)), .resolve)
}

#' Collapse several messages to the same recipient into one value
#' @noRd
resolve_messages <- function(value, to, resolve, target,
                             call = rlang::caller_env()) {
  if (!anyDuplicated(to)) return(list(to = to, value = value))
  if (resolve == "error") {
    dup <- to[duplicated(to)][[1]]
    abm_abort(
      c("Several agents wrote to agent {dup} in one {.fn abm_tell} step.",
        "x" = "Column {.field {target}} got {sum(to == dup)} values.",
        "i" = 'Pick a {.arg .resolve}: {.val sum}, {.val mean}, {.val max}, {.val min}, {.val first} or {.val last}.'),
      class = "tidyABM_tell_collision", call = call
    )
  }
  keys <- unique(to)
  idx <- split(seq_along(to), factor(to, levels = keys))
  out <- switch(
    resolve,
    first = value[vapply(idx, function(i) i[[1]], integer(1))],
    last  = value[vapply(idx, function(i) i[[length(i)]], integer(1))],
    sum   = vapply(idx, function(i) sum(value[i]), numeric(1)),
    mean  = vapply(idx, function(i) mean(value[i]), numeric(1)),
    max   = vapply(idx, function(i) max(value[i]), numeric(1)),
    min   = vapply(idx, function(i) min(value[i]), numeric(1))
  )
  list(to = keys, value = unname(out))
}

#' @noRd
run_tell <- function(step, state) {
  combined <- bind_groups(state$groups)
  if (nrow(combined) == 0L) return(state)
  aug <- augment_group(combined, state$match, combined)

  speaks <- eval_condition(step$when, aug, state$globals)
  speaks[is.na(speaks)] <- FALSE
  if (!any(speaks)) return(state)

  # who each sender is addressing: one row per (sender, recipient)
  if (step$to == "neighbours") {
    if (is.null(state$edges)) {
      abm_abort(
        c('{.code abm_tell(to = "neighbours")} needs a network.',
          "i" = "Add one with {.code abm_setup(..., network = abm_network(...))}."),
        class = "tidyABM_no_network"
      )
    }
    nb <- neighbour_table(state$edges)
    nb <- nb[nb$.id %in% aug$.id[speaks] & nb$.neighbour %in% combined$.id, ,
             drop = FALSE]
    sender_row <- match(nb$.id, aug$.id)
    recipient  <- nb$.neighbour
  } else {
    tgt <- eval_rule(list(quo = step$to_quo), aug, state$globals,
                     grouped = FALSE)
    if (length(tgt) == 1L) tgt <- rep(tgt, nrow(aug))
    keep <- speaks & !is.na(tgt)
    sender_row <- which(keep)
    recipient  <- as.integer(tgt[keep])
    unknown <- setdiff(recipient, combined$.id)
    if (length(unknown)) {
      abm_abort(
        c("{.fn abm_tell} was told to write to an agent that does not exist.",
          "x" = "No agent has {.code .id} {unknown[[1]]}."),
        class = "tidyABM_bad_recipient"
      )
    }
  }
  if (!length(sender_row)) return(state)

  for (r in step$rules) {
    value <- eval_rule(r, aug, state$globals, grouped = FALSE)
    if (length(value) == 1L) value <- rep(value, nrow(aug))
    msg <- resolve_messages(value[sender_row], recipient, step$resolve,
                            r$target)
    for (nm in names(state$groups)) {
      g <- state$groups[[nm]]
      if (nrow(g) == 0L) next
      hit <- match(g$.id, msg$to)
      if (all(is.na(hit))) next
      if (!r$target %in% names(g)) {
        abm_abort(
          c("{.fn abm_tell} can only write to a column the recipient already has.",
            "x" = "Group {.field {nm}} has no column {.field {r$target}}.",
            "i" = "Declare it in {.fn abm_agents}, or restrict the recipients."),
          class = "tidyABM_tell_new_column"
        )
      }
      rows <- !is.na(hit)
      g <- assign_rule(g, r$target,
                       replace_at(g[[r$target]], rows, msg$value[hit[rows]]),
                       rep(TRUE, nrow(g)))
      state$groups[[nm]] <- g
    }
  }
  state
}

#' `x` with `rows` overwritten by `value`, casting to a common type
#' @noRd
replace_at <- function(x, rows, value) {
  both <- vctrs::vec_cast_common(x, value)
  out <- both[[1]]
  out[rows] <- both[[2]]
  out
}

#' @export
print.abm_tell <- function(x, ...) {
  target <- if (x$to == "neighbours") "neighbours" else
    deparse1(rlang::quo_get_expr(x$to_quo))
  cli::cli_text("{.cls abm_tell} to {.emph {target}}")
  bits <- paste0("{.code ", rule_labels(x), "}")
  if (!is.null(x$when)) {
    bits <- c(bits, "when = {.code {deparse1(rlang::quo_get_expr(x$when))}}")
  }
  if (x$resolve != "last") bits <- c(bits, "resolve = {.val {x$resolve}}")
  cli::cli_bullets(stats::setNames(bits, rep("*", length(bits))))
  invisible(x)
}
