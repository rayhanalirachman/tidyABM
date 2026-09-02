# Rules, globals, births and deaths --------------------------------------

new_rule_step <- function(rules, class, scope = "match", by = NULL,
                          order = NULL) {
  structure(list(rules = rules, scope = scope, by = by, order = order),
            class = c(class, "abm_step"))
}

#' Collect and check `col ~ expr` formulas
#' @noRd
collect_rules <- function(dots, fn, call = rlang::caller_env()) {
  if (length(dots) == 0L) {
    abm_abort("{.fn {fn}} needs at least one {.code column ~ formula} rule.",
              class = "tidyABM_empty_step", call = call)
  }
  ok <- vapply(dots, is_formula2, logical(1))
  if (!all(ok)) {
    abm_abort(
      c("Every argument to {.fn {fn}} must be a two-sided formula.",
        "x" = "Argument {which(!ok)[[1]]} is {.cls {class(dots[[which(!ok)[[1]]]])[[1]]}}.",
        "i" = "Rules look like {.code money ~ money - 1}."),
      class = "tidyABM_bad_formula", call = call
    )
  }
  lapply(dots, function(f) {
    list(
      target = f_lhs_name(f, arg = "rule", call = call),
      quo    = rlang::new_quosure(rlang::f_rhs(f), rlang::f_env(f)),
      vars   = f_rhs_vars(f)
    )
  })
}

#' Update agent columns
#'
#' `abm_rules()` is the step that changes agents. Every rule is a two-sided
#' formula: the left-hand side names the column to write, the right-hand side
#' is an expression evaluated against the agent tibble, exactly as it would be
#' inside [dplyr::mutate()].
#'
#' All rules in a single `abm_rules()` call are evaluated **simultaneously**,
#' against the state at the start of the step. So in `abm_rules(a ~ b, b ~ a)`
#' both sides see the old values, this is the synchronous update that
#' agent-based models normally assume, and it is the one place where
#' `abm_rules()` deliberately departs from `mutate()`'s sequential semantics.
#' Write two `abm_rules()` calls if you want one rule to see the other's
#' result.
#'
#' In a model with several agent groups, a rule is applied to a group only if
#' every agent column it mentions exists in that group. That is how the market
#' example routes `offer ~ ...` to buyers and `ask ~ ...` to sellers without
#' any explicit test on agent type.
#'
#' While a match is standing, rules are evaluated group by group, per pair, per
#' group, or per agent depending on the pairing mode. That is usually what you
#' want, and it is what makes `sample(x, 1)` mean "once per pair". Occasionally
#' a step in the middle of a tick is about the whole population instead,
#' drawing the next generation from this one, say, and `.scope = "population"`
#' evaluates it against every agent at once, ignoring the standing match.
#'
#' @param ... One or more `column ~ expression` rules. The expression can use
#'   any column of the agent tibble, any global, any `partner_<col>` produced by
#'   a preceding [abm_match()], `.role`, and anything visible where the rule was
#'   written.
#' @param .scope `"match"` (the default) evaluates the rules within whatever
#'   grouping the preceding [abm_match()] produced. `"population"` ignores it and
#'   evaluates across all agents, so aggregates like `sum()` and draws like
#'   `sample(x, n())` see everybody.
#' @param .by A column naming a partition of the agents, a firm, a household, a
#'   team, a cohort. The rules are evaluated once per distinct value, across the
#'   whole population, so `sum(effort)` means "the total effort of my firm" and
#'   the answer is written back to every member of it. This is the third grouping
#'   a rule can have, alongside the standing match and the whole population, and
#'   it is the only one the *agents themselves* can change: an agent that writes
#'   a new value into the `.by` column has moved to a different group. Cannot be
#'   combined with `.scope = "population"`.
#'
#' @return An `abm_rules` step object.
#' @seealso [abm_go()], which lists every step and fixes the order they run
#'   in.
#' @family agent update steps
#' @export
#' @examples
#' abm_rules(money ~ ifelse(.role == "giver", money - 1, money + 1))
#' abm_rules(opinion ~ partner_opinion)
#'
#' # the next generation, drawn from this one in proportion to fitness
#' abm_rules(strategy ~ sample(strategy, n(), replace = TRUE, prob = fitness),
#'           .scope = "population")
#'
#' # every member of a firm is paid an equal share of what the firm produces
#' abm_rules(pay ~ output(sum(effort)) / n(), .by = firm)
abm_rules <- function(..., .scope = c("match", "population"), .by = NULL) {
  .scope_given <- !missing(.scope)
  .scope <- rlang::arg_match(.scope)
  by <- enquo_or_null(rlang::enquo(.by))
  if (!is.null(by) && .scope_given && .scope == "population") {
    abm_abort(
      c("{.arg .by} and {.code .scope = \"population\"} are different groupings.",
        "i" = "{.arg .by} already ignores the standing match."),
      class = "tidyABM_conflicting_args"
    )
  }
  new_rule_step(collect_rules(rlang::list2(...), "abm_rules"), "abm_rules",
                scope = .scope, by = by)
}

#' Update agent columns one agent at a time
#'
#' `abm_sequential()` is the order-dependent sibling of [abm_rules()]. Agents
#' are processed one at a time in a freshly shuffled order, and each agent's
#' writes to **globals** are visible to every agent processed after it within
#' the same step. This mirrors NetLogo's `ask turtles`, and it is what you need
#' when agents compete for a shared resource that is *depleted* rather than
#' merely divided, a bank's lendable reserves, say.
#'
#' Rules also cascade *within* each agent: the second rule sees what the first
#' one just wrote, to the agent's own row and to the globals alike. That is the
#' opposite of [abm_rules()], where every rule reads the state at the start of
#' the step, and it is what "one agent at a time" already implies, an agent
#' that draws a quote and then decides whether the quote crosses the book has
#' to be able to read the number it just drew.
#'
#' During the per-agent loop a rule can read and write its own agent's columns
#' and any global. If an [abm_match()] is standing it can also read and write
#' its **partner's**: the step is narrowed to the agents the match placed in a
#' group, `.partner` and `partner_<col>` come into scope, and a rule whose
#' left-hand side is `partner_<col>` writes into that agent's row. Because the
#' partner is read live rather than copied at the start of the step, the
#' second buyer at a shop sees the stock the first one took, which is what a
#' decentralised market is and what [abm_tell()] cannot say: `abm_tell()`
#' resolves every sender at once, so a stock it draws down can go negative.
#'
#' It reaches no further than that. To write into a row that is neither its own
#' nor its partner's, use [abm_tell()]. Use [abm_rules()] unless you
#' specifically need the ordering, since sequential evaluation is both slower
#' and harder to reason about.
#'
#' @param ... One or more `column ~ expression` rules. The left-hand side may
#'   name either an agent column or a global.
#' @param .order Optional expression, evaluated over the whole population,
#'   whose ascending order is the order agents are processed in. The default is a
#'   fresh shuffle every step, which is right when the order is meant to be
#'   arbitrary and wrong when it is part of the model: a queue at a counter, a
#'   sequential-service constraint, a fixed speaking order. `NA` sits the agent
#'   out of the step.
#'
#' @return An `abm_sequential` step object.
#' @seealso [abm_go()], which lists every step and fixes the order they run
#'   in.
#' @family agent update steps
#' @export
#' @examples
#' abm_sequential(
#'   loan          ~ ifelse(wallet < 0, loan + 1, loan),
#'   bank_reserves ~ ifelse(wallet < 0, bank_reserves - 1, bank_reserves)
#' )
#'
#' # a sale: what is left on the shelf is what the customers before me left
#' abm_sequential(
#'   got         ~ pmin(want, money / partner_price, partner_stock),
#'   money       ~ money - got * partner_price,
#'   partner_stock ~ partner_stock - got
#' )
abm_sequential <- function(..., .order = NULL) {
  new_rule_step(collect_rules(rlang::list2(...), "abm_sequential"),
                "abm_sequential", order = enquo_or_null(rlang::enquo(.order)))
}

#' Update a shared, population-level value
#'
#' `abm_global()` writes to a value held once for the whole model rather than
#' once per agent, El Farol's `last_attendance`, a zakah pool, a bank's ledger.
#' The right-hand side is an aggregate expression evaluated over the agent
#' tibble, so it normally collapses to a single value.
#'
#' Unlike the other update steps, `abm_global()` does not need a preceding
#' [abm_match()]: a population-level summary does not depend on who was paired
#' with whom.
#'
#' # A global indexed by a category
#'
#' Models keep wanting a shared *table* rather than a shared number: a stimulus
#' per task, a price per good, a queue length per counter. `.by` writes one.
#' The rules are evaluated once per key and the global becomes a **named
#' vector** indexed by them, which an ordinary rule reads back with
#' `price[good]`.
#'
#' Two things are in scope during that evaluation and nowhere else. `.key` is
#' the key being written, so `sum(task == .key)` is "how many agents are on
#' *this* task". And the global's own name is bound to **this key's** current
#' value, not to the whole vector, so an update reads exactly as the scalar
#' version does:
#'
#' ``` abm_global(stimulus ~ stimulus + delta - alpha * sum(task == .key) /
#' n(), .by = 1:2) ```
#'
#' Each key still sees the **whole population**, `n()` is everybody, not
#' everybody on this task, because that is what a colony-level stimulus balance
#' means.
#'
#' `.by` names the index either way round. Give it a vector and the index is
#' declared by the model, which is right when the categories are fixed and a
#' key with nobody in it still has to be updated. Give it an agent column and
#' the index is whatever values the agents currently hold, which is right when
#' the categories come and go. A key the global already has stays in the index
#' and is still updated even when no agent holds it this tick, so a task nobody
#' is working on has its stimulus rise rather than dropping out of the table.
#'
#' @param ... One or more `global_name ~ aggregate_expression` rules. The
#'   expression can use agent columns and other globals; each rule sees the
#'   globals as updated by the rules before it in the same call.
#' @param .by Optional index. Either a vector of keys or the name of an agent
#'   column whose distinct values are the keys. The global becomes a named
#'   vector, `.key` is in scope, and the global's own name refers to that key's
#'   value.
#'
#' @return An `abm_global` step object.
#' @seealso [abm_go()], which lists every step and fixes the order they run
#'   in.
#' @family agent update steps
#' @export
#' @examples
#' abm_global(last_attendance ~ sum(go_today))
#'
#' # one stimulus per task, decaying with the number of workers on it
#' abm_global(stimulus ~ pmax(0, stimulus + 1 - 3 * sum(task == .key) / n()),
#'            .by = 1:2)
abm_global <- function(..., .by = NULL) {
  new_rule_step(collect_rules(rlang::list2(...), "abm_global"), "abm_global",
                by = enquo_or_null(rlang::enquo(.by)))
}

# Population -------------------------------------------------------------

new_abm_birth <- function(when, n, times, cost, inherit, attach_via, links) {
  structure(list(when = when, n = n, times = times, cost = cost,
                 inherit = inherit, attach_via = attach_via, links = links),
            class = c("abm_birth", "abm_step"))
}

#' Add agents
#'
#' `abm_birth()` is one of the two steps that change the size of the
#' population. It has two modes, and exactly one of them must be used:
#'
#' * `when = <condition>` clones every agent that satisfies the condition. The
#'   newborn inherits all of its parent's columns.
#' * `n = <count>` adds that many brand-new agents, whose columns are copied
#'   from a randomly chosen existing agent of the same group.
#'
#' One parent, one offspring, unless `times` says otherwise. Any fertility
#' above one, a clutch, a litter, a Poisson number of seeds, is `times`, and
#' each offspring is evaluated separately, so a mutation drawn in `inherit`
#' differs between siblings.
#'
#' @param when A condition. Agents satisfying it reproduce.
#' @param n A count of new agents to add unconditionally.
#' @param times How many offspring each reproducing agent has. An expression
#'   evaluated in the parent's row, so it can be a column, a draw
#'   (`rpois(dplyr::n(), 2)`) or a number. `0` means that parent has none this
#'   tick and `NA` is treated as `0`. Only used with `when`, since `n` is already
#'   a count. Each offspring gets its own evaluation of `inherit`, so a mutation
#'   drawn there differs from sibling to sibling.
#' @param cost One or more `column ~ expression` formulas applied to the parent
#'   *and* the newborn after the split, expressing what reproduction costs, for
#'   example `cost = resource ~ resource / 2` to halve a resource between them.
#'   Only `when` has a parent to charge; with `n` there is none, so columns the
#'   new agents are to be given belong in `inherit`.
#' @param inherit One or more `column ~ expression` formulas applied to the
#'   newborn *only*, expressing what the offspring gets that the parent does not
#'   keep: a reset age, a mutated trait, a sex drawn at birth. The expressions
#'   are evaluated in the parent's row, so they can use the parent's columns and,
#'   when an [abm_match()] is standing, the other parent's, as `partner_<col>`.
#'   That is how two-parent inheritance is written: `inherit = trait ~ (trait +
#'   partner_trait) / 2`.
#' @param attach_via An [abm_match()] object with `pair = "network"`, used to
#'   connect each newborn to an existing agent. This is the only way the network
#'   grows during a run; `from = "random_edge"` gives degree-proportional
#'   (preferential) attachment.
#' @param links How many edges the newborn gets, one by default. One edge makes
#'   the newborn a leaf, so a population that also dies erodes whatever network
#'   it started with into a forest of parent-child pairs, however dense that
#'   network was. `links` gives the newborn the degree the model means instead.
#'   With `from = "parent"` it takes the parent *and* a sample of the parent's
#'   own neighbours, which is what "the offspring settles in a site next to its
#'   parent" means on a lattice; with `from = "random_edge"` it is the *m*
#'   degree-proportional edges per node that preferential attachment is defined
#'   with. Targets are distinct, and a newborn takes what there is when there
#'   are fewer than `links` of them.
#'
#' @return An `abm_birth` step object.
#' @seealso [abm_go()], which lists every step and fixes the order they run
#'   in.
#' @family demographic steps
#' @export
#' @examples
#' abm_birth(when = resource > 20, cost = resource ~ resource / 2)
#'
#' # a clutch rather than a single offspring
#' abm_birth(when = mature, times = rpois(dplyr::n(), 2), inherit = age ~ 0)
#' abm_birth(n = 1, attach_via = abm_match(pair = "network", from = "random_edge"))
#'
#' # the offspring takes a place beside its parent, with a neighbourhood of its
#' # own rather than a single edge back to the parent
#' abm_birth(when = runif(dplyr::n()) < ptr, links = 4,
#'           attach_via = abm_match(pair = "network", from = "parent"))
#'
#' # a child of two parents, with its own age and a mutated trait
#' abm_birth(
#'   when = sex == "female",
#'   inherit = list(age ~ 0, trait ~ (trait + partner_trait) / 2 + rnorm(n(), 0, 0.01))
#' )
abm_birth <- function(when = NULL, n = NULL, times = NULL, cost = NULL,
                      inherit = NULL, attach_via = NULL, links = NULL) {
  when <- enquo_or_null(rlang::enquo(when))
  times <- enquo_or_null(rlang::enquo(times))

  if (is.null(when) && is.null(n)) {
    abm_abort(
      c("{.fn abm_birth} needs either {.arg when} or {.arg n}.",
        "i" = "{.arg when} clones agents that satisfy a condition; {.arg n} adds new ones."),
      class = "tidyABM_missing_arg"
    )
  }
  if (!is.null(when) && !is.null(n)) {
    abm_abort("Supply either {.arg when} or {.arg n}, not both.",
              class = "tidyABM_conflicting_args")
  }
  if (!is.null(times) && !is.null(n)) {
    abm_abort(
      c("{.arg times} says how many offspring each parent has, and {.arg n} is already a count.",
        "i" = "Use {.arg times} with {.arg when}."),
      class = "tidyABM_conflicting_args"
    )
  }
  if (!is.null(n) && (!rlang::is_scalar_integerish(n) || n < 0)) {
    abm_abort("{.arg n} must be a single non-negative whole number.",
              class = "tidyABM_bad_n")
  }
  if (!is.null(cost)) {
    if (is_formula2(cost)) cost <- list(cost)
    cost <- collect_rules(cost, "abm_birth")
  }
  if (!is.null(inherit)) {
    if (is_formula2(inherit)) inherit <- list(inherit)
    inherit <- collect_rules(inherit, "abm_birth")
  }
  if (!is.null(attach_via)) {
    if (!inherits(attach_via, "abm_match") || attach_via$pair != "network") {
      abm_abort(
        '{.arg attach_via} must be {.code abm_match(pair = "network", ...)}.',
        class = "tidyABM_bad_attach"
      )
    }
  }
  if (!is.null(links)) {
    if (is.null(attach_via)) {
      abm_abort(
        c("{.arg links} counts the edges {.arg attach_via} makes.",
          "i" = "Supply {.arg attach_via} too, or drop {.arg links}."),
        class = "tidyABM_missing_arg"
      )
    }
    if (!rlang::is_scalar_integerish(links) || is.na(links) || links < 1) {
      abm_abort("{.arg links} must be a single whole number of at least 1.",
                class = "tidyABM_bad_links")
    }
    links <- as.integer(links)
  }
  new_abm_birth(when, if (is.null(n)) NULL else as.integer(n), times, cost,
                inherit, attach_via, links)
}

new_abm_death <- function(when, prune_edges) {
  structure(list(when = when, prune_edges = prune_edges),
            class = c("abm_death", "abm_step"))
}

#' Remove agents
#'
#' `abm_death()` drops every agent satisfying `when`. By default it also removes
#' those agents' edges from the network, because leaving them in would make
#' `abm_match(pair = "network")` draw partners that no longer exist.
#'
#' @param when A condition. Agents satisfying it are removed.
#' @param prune_edges Whether to delete the removed agents' network edges.
#'   Defaults to `TRUE`; set to `FALSE` only if you want a network whose node set
#'   deliberately outlives its agents.
#'
#' @return An `abm_death` step object.
#' @seealso [abm_go()], which lists every step and fixes the order they run
#'   in.
#' @family demographic steps
#' @export
#' @examples
#' abm_death(when = resource <= 0)
abm_death <- function(when, prune_edges = TRUE) {
  when <- enquo_or_null(rlang::enquo(when))
  if (is.null(when)) {
    abm_abort("{.arg when} is required.", class = "tidyABM_missing_arg")
  }
  if (!rlang::is_bool(prune_edges)) {
    abm_abort("{.arg prune_edges} must be {.code TRUE} or {.code FALSE}.",
              class = "tidyABM_bad_arg")
  }
  new_abm_death(when, prune_edges)
}

# Printing ---------------------------------------------------------------

rule_labels <- function(x) {
  vapply(x$rules, function(r) {
    paste0(r$target, " ~ ", deparse1(rlang::quo_get_expr(r$quo)))
  }, character(1))
}

#' @export
print.abm_rules <- function(x, ...) print_rule_step(x, "abm_rules")

#' @export
print.abm_sequential <- function(x, ...) print_rule_step(x, "abm_sequential")

#' @export
print.abm_global <- function(x, ...) print_rule_step(x, "abm_global")

print_rule_step <- function(x, cls) {
  # cli does not interpolate a second time, so the qualifier is formatted here
  # rather than handed to cli_text() as a string full of braces
  scope <- if (identical(x$scope, "population")) {
    cli::format_inline(" {.emph (population scope)}")
  } else if (!is.null(x$by)) {
    cli::format_inline(" {.emph (by {deparse1(rlang::quo_get_expr(x$by))})}")
  } else if (!is.null(x$order)) {
    cli::format_inline(" {.emph (in order of {deparse1(rlang::quo_get_expr(x$order))})}")
  } else if (!is.null(x$within)) {
    cli::format_inline(" {.emph (within {deparse1(rlang::quo_get_expr(x$within))})}")
  } else ""
  cli::cli_text("{.cls {cls}} {length(x$rules)} rule{?s}{scope}")
  labs <- rule_labels(x)
  cli::cli_bullets(stats::setNames(paste0("{.code ", labs, "}"),
                                   rep("*", length(labs))))
  invisible(x)
}

#' @export
print.abm_birth <- function(x, ...) {
  cli::cli_text("{.cls abm_birth}")
  bits <- c(
    if (!is.null(x$when)) "when = {.code {deparse1(rlang::quo_get_expr(x$when))}}",
    if (!is.null(x$n)) "n = {x$n}",
    if (!is.null(x$times)) "times = {.code {deparse1(rlang::quo_get_expr(x$times))}}",
    if (!is.null(x$cost)) "cost = {.code {rule_labels(list(rules = x$cost))}}",
    if (!is.null(x$inherit)) "inherit = {.code {rule_labels(list(rules = x$inherit))}}",
    if (!is.null(x$attach_via)) 'attach_via = network ({.val {x$attach_via$from}})',
    if (!is.null(x$links)) "links = {x$links}"
  )
  cli::cli_bullets(stats::setNames(bits, rep("*", length(bits))))
  invisible(x)
}

#' @export
print.abm_death <- function(x, ...) {
  cli::cli_text("{.cls abm_death}")
  cli::cli_bullets(c("*" = "when = {.code {deparse1(rlang::quo_get_expr(x$when))}}"))
  invisible(x)
}
