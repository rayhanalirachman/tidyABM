# Matching ---------------------------------------------------------------

# Which arguments each pairing mode actually uses. Anything else is an error
# rather than being silently ignored.
match_relevant_args <- list(
  random          = c("size", "role", "eligible"),
  one_of          = c("role", "eligible", "among"),
  opposite_group  = c("size", "by", "role", "eligible", "resolve", "rounds",
                      "positions", "limits"),
  nearest         = c("by", "cost", "size", "eligible", "among"),
  network         = c("from", "eligible")
)

new_abm_match <- function(pair, size, by, role, eligible, resolve, rounds,
                          positions, limits, from, among, cost) {
  structure(
    list(pair = pair, size = size, by = by, role = role, eligible = eligible,
         resolve = resolve, rounds = rounds, positions = positions,
         limits = limits, from = from, among = among, cost = cost),
    class = c("abm_match", "abm_step")
  )
}

#' Match agents into pairs or groups
#'
#' `abm_match()` is the step that decides *who interacts with whom* this tick.
#' It does not change any agent column; it only produces a partner (for `size =
#' 2`) or a group (for `size > 2`), which the [abm_rules()] steps that follow
#' it then use.
#'
#' After a match, every rule can see:
#'
#' * `partner_<col>` for each of the partner's columns, when `size = 2`;
#' * grouped-mutate semantics when `size > 2`, so `sum(contribution)` inside a
#'   rule means "sum across this agent's group";
#' * `.role`, when `role` is supplied.
#'
#' Each `pair` mode uses a fixed set of the other arguments, and passing one
#' that the chosen mode does not use is an error rather than being ignored:
#'
#' | mode               | uses
#' |
#' |--------------------|-------------------------------------------------------|
#' | `"random"`         | `size`, `role`, `eligible`
#' | | `"one_of"`         | `role`, `eligible`, `among`
#' | | `"opposite_group"` | `by`, `role`, `eligible`, `resolve`, `rounds`,
#' `positions`, `limits` | | `"nearest"`        | `by` *or* `cost`, `size`,
#' `eligible`, `among`         | | `"network"`        | `from`, `eligible`
#' |
#'
#' `eligible` and `among` ask different questions, and the difference only has
#' teeth in the directional modes. `eligible` says who *takes part*; `among`
#' says who may be *picked*. A consumer choosing the nearest shop wants `among
#' = .group == "shops"`, or it will find that the nearest agent to it is
#' another consumer.
#'
#' Two of the modes are *mutual*: `"random"` and `"opposite_group"` partition
#' the eligible agents into pairs or groups, so being matched is symmetric and
#' every agent appears once. The other three are *directional*: `"one_of"`,
#' `"nearest"` and `"network"` give each agent a partner of its own, and your
#' partner need not have picked you. `"one_of"` is NetLogo's `one-of other
#' turtles`; `"random"` is a fresh shuffle of the whole population into
#' couples. They are different models of who meets whom, and which one you want
#' depends on the source you are porting.
#'
#' @param pair The pairing mode. `"random"` shuffles agents into groups of
#'   `size`; `"one_of"` gives each agent one partner drawn uniformly from the
#'   whole population; `"opposite_group"` pairs each agent with one from the
#'   other group named by `by`; `"nearest"` gives each agent its closest other
#'   agent in the space defined by `by`; `"network"` draws the partner from the
#'   agent's neighbours in the model's [abm_network()].
#' @param size Group size. Defaults to 2 (a pair). Only `"random"` and
#'   `"nearest"` support `size > 2`; `"opposite_group"` is undefined above 2.
#' @param by Column(s) defining the space or the split. For `"opposite_group"`
#'   a single column that takes exactly two values (use `.group` for a
#'   multi-group model); for `"nearest"` one or more numeric columns, compared by
#'   Euclidean distance.
#' @param role A named list of two conditions, e.g. `list(giver = money > 0,
#'   receiver = TRUE)`. Within each pair, roles are assigned so that each member
#'   satisfies its own role's condition; if no assignment works the pair is
#'   dropped for this step. Rules then see `.role`.
#' @param eligible A condition. Agents for which it is `FALSE` sit the step
#'   out.
#' @param cost For `"nearest"`, an expression naming what the chooser is
#'   minimising, used instead of `by`. It is evaluated once per (chooser,
#'   candidate) pair: the candidate's columns are visible under their own names
#'   and the chooser's under `own_<col>`, the same convention [abm_neighbours()]
#'   uses. `.id` and `.group` are included, so a cost can be a lookup into the
#'   chooser's own preference list as easily as a price. `by` is the special case
#'   `cost = (x - own_x)^2`; anything else, a delivered price `price + travel *
#'   abs(x - own_x)`, an energy deficit, a position in a preference list, needs
#'   this. `NA` means the candidate is not acceptable to that chooser, and a
#'   chooser with no acceptable candidate sits the step out.
#' @param among A condition naming the agents that may be *chosen*, for the
#'   directional modes `"one_of"` and `"nearest"`. Defaults to everybody. An
#'   agent is never matched to itself.
#' @param resolve For `"opposite_group"`, `"none"` (the default) or
#'   `"negotiate"`, which runs `rounds` rounds of offer/counter-offer and sets
#'   `traded` and `price` columns.
#' @param rounds Number of negotiation rounds when `resolve = "negotiate"`.
#' @param positions For `resolve = "negotiate"`, `c(<first group's bid column>,
#'   <second group's ask column>)`.
#' @param limits For `resolve = "negotiate"`, `c(<first group's reservation
#'   column>, <second group's reservation column>)`, the bid never rises above
#'   the first, and the ask never falls below the second.
#' @param from For `"network"`, `"neighbour"` (the default) picks a random
#'   neighbour of the agent; `"random_edge"` picks a random edge of the whole
#'   network and then one of its endpoints, which selects agents in proportion to
#'   their degree (used by preferential attachment); `"parent"` is only
#'   meaningful inside [abm_birth()]'s `attach_via` and links a newborn to the
#'   agent it was cloned from, which is what puts offspring next to their kin.
#'
#' @return An `abm_match` step object.
#' @export
#' @examples
#' abm_match(pair = "random")
#' abm_match(pair = "one_of")
#' abm_match(pair = "random", size = 4)
#' abm_match(pair = "nearest", by = opinion)
#' abm_match(pair = "nearest", by = position, among = .group == "shops")
#' abm_match(pair = "nearest", cost = price + abs(x - own_x),
#'           among = .group == "shops")
#' abm_match(pair = "random", role = list(giver = money > 0, receiver = TRUE))
abm_match <- function(pair = c("random", "one_of", "opposite_group", "nearest",
                                "network"),
                      size = NULL, by = NULL, role = NULL, eligible = NULL,
                      resolve = NULL, rounds = NULL, positions = NULL,
                      limits = NULL, from = NULL, among = NULL, cost = NULL) {
  pair <- rlang::arg_match(pair)

  by        <- enquo_or_null(rlang::enquo(by))
  cost      <- enquo_or_null(rlang::enquo(cost))
  role      <- enquo_or_null(rlang::enquo(role))
  eligible  <- enquo_or_null(rlang::enquo(eligible))
  positions <- enquo_or_null(rlang::enquo(positions))
  limits    <- enquo_or_null(rlang::enquo(limits))
  among     <- enquo_or_null(rlang::enquo(among))

  supplied <- c(
    if (!is.null(size))      "size",
    if (!is.null(by))        "by",
    if (!is.null(role))      "role",
    if (!is.null(eligible))  "eligible",
    if (!is.null(resolve))   "resolve",
    if (!is.null(rounds))    "rounds",
    if (!is.null(positions)) "positions",
    if (!is.null(limits))    "limits",
    if (!is.null(from))      "from",
    if (!is.null(among))     "among",
    if (!is.null(cost))      "cost"
  )
  irrelevant <- setdiff(supplied, match_relevant_args[[pair]])
  if (length(irrelevant)) {
    abm_abort(
      c('{.arg {irrelevant}} {?is/are} not used when {.code pair = "{pair}"}.',
        "i" = 'That mode uses {.arg {match_relevant_args[[pair]]}}.'),
      class = "tidyABM_irrelevant_arg"
    )
  }

  size <- size %||% 2L
  if (!rlang::is_scalar_integerish(size) || size < 2) {
    abm_abort("{.arg size} must be a single whole number of at least 2.",
              class = "tidyABM_bad_size")
  }
  size <- as.integer(size)

  if (pair == "nearest" && !is.null(by) && !is.null(cost)) {
    abm_abort(
      c('Supply either {.arg by} or {.arg cost}, not both.',
        "i" = "{.arg by} is a coordinate to be close to; {.arg cost} is a number to be minimised."),
      class = "tidyABM_conflicting_args")
  }
  if (pair == "opposite_group" && is.null(by)) {
    abm_abort('{.arg by} is required when {.code pair = "{pair}"}.',
              class = "tidyABM_missing_arg")
  }
  if (pair == "nearest" && is.null(by) && is.null(cost)) {
    abm_abort('{.arg by} or {.arg cost} is required when {.code pair = "nearest"}.',
              class = "tidyABM_missing_arg")
  }
  if (pair == "nearest" && size > 2L) {
    abm_abort(
      c('{.code pair = "nearest"} is only defined for pairs.',
        "x" = "Got {.code size = {size}}.",
        "i" = "Nearest-neighbour groups of 3 or more overlap, so they cannot be used as grouping for {.fn abm_rules}."),
      class = "tidyABM_bad_size"
    )
  }
  if (pair == "opposite_group" && size > 2L) {
    abm_abort(
      c('{.code pair = "opposite_group"} is only defined for pairs.',
        "x" = "Got {.code size = {size}}."),
      class = "tidyABM_bad_size"
    )
  }

  resolve <- resolve %||% "none"
  resolve <- rlang::arg_match0(resolve, c("none", "negotiate"),
                               arg_nm = "resolve")
  if (resolve == "negotiate") {
    if (is.null(positions) || is.null(limits)) {
      abm_abort(
        c('{.arg positions} and {.arg limits} are required when {.code resolve = "negotiate"}.',
          "i" = 'For example {.code positions = c(offer, ask), limits = c(wtp, wta)}.'),
        class = "tidyABM_missing_arg"
      )
    }
    rounds <- rounds %||% 5L
  } else if (!is.null(rounds)) {
    abm_abort('{.arg rounds} is only used when {.code resolve = "negotiate"}.',
              class = "tidyABM_irrelevant_arg")
  }

  from <- from %||% "neighbour"
  from <- rlang::arg_match0(from, c("neighbour", "random_edge", "parent"),
                            arg_nm = "from")

  if (!is.null(role)) {
    role_expr <- rlang::quo_get_expr(role)
    if (!rlang::is_call(role_expr, "list") || length(role_expr) != 3L ||
        !rlang::is_named(as.list(role_expr)[-1])) {
      abm_abort(
        c("{.arg role} must be a named list of exactly two conditions.",
          "i" = "For example {.code list(giver = money > 0, receiver = TRUE)}."),
        class = "tidyABM_bad_role"
      )
    }
  }

  new_abm_match(pair, size, by, role, eligible, resolve, rounds,
                positions, limits, from, among, cost)
}

#' @export
print.abm_match <- function(x, ...) {
  bits <- c(
    if (x$size != 2L) "size = {x$size}",
    if (!is.null(x$by)) "by = {.code {deparse1(rlang::quo_get_expr(x$by))}}",
    if (!is.null(x$cost)) "cost = {.code {deparse1(rlang::quo_get_expr(x$cost))}}",
    if (!is.null(x$role)) "roles = {.val {names(as.list(rlang::quo_get_expr(x$role))[-1])}}",
    if (!is.null(x$eligible)) "eligible = {.code {deparse1(rlang::quo_get_expr(x$eligible))}}",
    if (!is.null(x$among)) "among = {.code {deparse1(rlang::quo_get_expr(x$among))}}",
    if (x$resolve != "none") "resolve = {.val {x$resolve}} ({x$rounds} rounds)",
    if (x$pair == "network" && x$from != "neighbour") "from = {.val {x$from}}"
  )
  cli::cli_text('{.cls abm_match} pair = {.val {x$pair}}')
  if (length(bits)) cli::cli_bullets(stats::setNames(bits, rep("*", length(bits))))
  invisible(x)
}
