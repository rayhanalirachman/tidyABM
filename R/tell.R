# Writing into other agents' rows ----------------------------------------

new_abm_tell <- function(rules, to, to_quo, when, resolve, order) {
  structure(list(rules = rules, to = to, to_quo = to_quo, when = when,
                 resolve = resolve, order = order),
            class = c("abm_tell", "abm_step"))
}

#' Write a value into another agent's row
#'
#' Every other update step writes to the agent it is evaluated on:
#' [abm_rules()] changes your own columns, [abm_neighbours()] *reads* your
#' neighbours' and writes the summary to you. `abm_tell()` is the one that goes
#' the other way, a sender computes a value and writes it into a *recipient's*
#' row. That is the difference between "how many of my neighbours are shouting"
#' and "I shout at my neighbours", and models built on the second shape,
#' outward persuasion, contagion carrying a dose, an order book where a trade
#' has to mark the counterparty, cannot be written without it.
#'
#' Who receives is set by `to`:
#'
#' * `to = "neighbours"`, every agent the sender is joined to in the model's
#'   [abm_network()]. This is the broadcast: one sender, many recipients.
#' * `to = <expression>`, an expression evaluated in the sender's row that
#'   names the recipient by `.id`. A list column names *several*: one sender, a
#'   set of recipients chosen however the model likes, which is what a broadcast
#'   to an audience that is not the network needs. `to = .partner` writes to the
#'   partner a preceding [abm_match()] gave you; `to = best_bid_holder` writes to
#'   whichever agent a global names. `NA` means the sender says nothing.
#'
#' The right-hand side of each rule is evaluated in the **sender's** row, so it
#' sees the sender's columns, `partner_<col>`, `.role` and the globals, exactly
#' as [abm_rules()] would. The value it produces is then written into the
#' recipient's column of that name. An agent that no one wrote to keeps the
#' value it already had, `abm_tell()` never touches a silent agent's row.
#'
#' Two senders can address the same recipient in one step, and `.resolve` says
#' what happens then. The default, `"last"`, takes an arbitrary one of them,
#' which is right for a persuasion rule where the senders all agree anyway;
#' `"sum"` is right for anything additive, like a dose or an order quantity;
#' `"error"` says the collision is a modelling mistake and should stop the run.
#'
#' Three of those resolutions, `"first"`, `"last"` and `"collect"`, pick out a
#' message rather than combining them all, so they only mean something once the
#' messages have an order. `.order` gives them one: an expression evaluated in
#' the sender's row whose ascending order the messages are considered in. That
#' is what *the first person to reach the counter* needs, and without it the
#' counter has to reconstruct the queue from something the senders wrote down.
#'
#' @param ... One or more `column ~ expression` rules. The expression is
#'   evaluated in the sender's row; the result is written to the recipient's
#'   column of the same name. The column must already exist on the recipient.
#' @param to `"neighbours"`, or an expression naming the recipient's `.id`. If
#'   the expression returns a list column, each element is a vector of `.id`s and
#'   the sender writes to all of them.
#' @param when Optional condition on the sender. Only agents for which it holds
#'   send anything.
#' @param .resolve What to do when several senders write to the same recipient
#'   in one step: `"last"` (an arbitrary one wins), `"first"`, `"sum"`, `"mean"`,
#'   `"max"`, `"min"`, `"collect"`, which hands the recipient a list of
#'   everything it was told, so the recipient's own rule decides what to make of
#'   them, or `"error"` to stop.
#' @param .order Optional expression, evaluated in the **sender's** row, whose
#'   ascending order is the order the messages are considered in. Without it a
#'   recipient's messages arrive in whatever order the senders happened to be
#'   stored in, which makes `"first"`, `"last"` and the list `"collect"` hands
#'   over arbitrary. With it they are determinate: `.order = arrival` with
#'   `.resolve = "first"` is *the first person to reach the counter*, and
#'   `"collect"` hands the recipient its messages already in that order. `NA`
#'   sits the sender out of the step.
#'
#' @return An `abm_tell` step object.
#' @seealso [abm_go()], which lists every step and fixes the order they run
#'   in.
#' @family agent update steps
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
#'
#' # whoever got there first is the one the counter serves
#' abm_tell(serving ~ .id, to = counter, when = queueing,
#'          .resolve = "first", .order = arrived_at)
abm_tell <- function(..., to, when = NULL,
                     .resolve = c("last", "first", "sum", "mean", "max", "min",
                                  "collect", "error"),
                     .order = NULL) {
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
               enquo_or_null(rlang::enquo(when)), .resolve,
               enquo_or_null(rlang::enquo(.order)))
}

#' Collapse several messages to the same recipient into one value
#' @noRd
resolve_messages <- function(value, to, resolve, target,
                             call = rlang::caller_env()) {
  if (resolve == "collect") {
    keys <- unique(to)
    idx <- split(seq_along(to), factor(to, levels = keys))
    return(list(to = keys,
                value = I(unname(lapply(idx, function(i) unname(value[i]))))))
  }
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
    if (!is.list(tgt) && length(tgt) == 1L) tgt <- rep(tgt, nrow(aug))
    if (is.list(tgt)) {
      # a list column: one sender, a set of recipients. That is the shape a
      # broadcast to a chosen audience needs -- the onlookers who happened to
      # see this interaction -- and it is not the network, so
      # `to = "neighbours"` cannot say it.
      tgt <- lapply(tgt, function(v) as.integer(v[!is.na(v)]))
      tgt[!speaks] <- list(integer())
      sender_row <- rep(seq_along(tgt), lengths(tgt))
      recipient  <- unlist(tgt, use.names = FALSE)
      if (is.null(recipient)) recipient <- integer()
    } else {
      keep <- speaks & !is.na(tgt)
      sender_row <- which(keep)
      recipient  <- as.integer(tgt[keep])
    }
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

  if (!is.null(step$order)) {
    # messages are considered in the order the model names, so that "first" and
    # "last" mean something and the list "collect" hands over is sorted
    key <- eval_rule(list(quo = step$order), aug, state$globals,
                     grouped = FALSE)
    if (length(key) == 1L) key <- rep(key, nrow(aug))
    key <- key[sender_row]
    keep <- !is.na(key)
    if (!any(keep)) return(state)
    ord <- order(key[keep])
    sender_row <- sender_row[keep][ord]
    recipient  <- recipient[keep][ord]
  }

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
  if (!is.null(x$order)) {
    bits <- c(bits, "order = {.code {deparse1(rlang::quo_get_expr(x$order))}}")
  }
  cli::cli_bullets(stats::setNames(bits, rep("*", length(bits))))
  invisible(x)
}
