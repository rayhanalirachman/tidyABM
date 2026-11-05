# The grammar's error messages and print methods: what a newcomer is told when
# a model is written wrongly, and what a step says about itself.

# everything print() says, whether cli sent it to the message stream or not
printed <- function(x) {
  out <- character()
  msgs <- testthat::capture_messages(out <- utils::capture.output(print(x)))
  cli::ansi_strip(paste(c(msgs, out), collapse = "\n"))
}

test_that("a dot-prefixed qualifier written without its dot is named", {
  expect_error(abm_rules(x ~ 1, scope = "population"),
               class = "tidyABM_bad_arg")
  expect_error(abm_rules(x ~ 1, scope = "population"), "\\.scope")
  expect_error(abm_rules(x ~ sum(y), by = firm), "\\.by")
  expect_error(abm_sequential(x ~ 1, order = place), "\\.order")
  expect_error(abm_neighbours(x ~ sum(y), where = "north"), "\\.where")
  expect_error(abm_tell(x ~ 1, to = .partner, resolve = "sum"), "\\.resolve")
  expect_error(abm_draw(x ~ runif(n()), each = "endpoint"), "\\.each")
  expect_error(abm_pairs(via = "r", x ~ 1, when = x > 0), "\\.when")
  expect_error(abm_global(x ~ sum(y), by = task), "\\.by")
})

test_that("a named argument that is not a rule is reported, not evaluated", {
  # these used to fail with "object 'money' not found"
  expect_error(abm_rules(money = money - 1), class = "tidyABM_bad_arg")
  expect_error(abm_rules(money ~ money - 1, when = money > 0),
               class = "tidyABM_bad_arg")
  expect_error(abm_rules(money ~ money - 1, when = money > 0), "abm_death")
  # named formulas, and names bound to formulas, still pass as they always did
  f <- x ~ x + 1
  expect_s3_class(abm_rules(r1 = f), "abm_rules")
  expect_s3_class(abm_rules(r1 = x ~ 1), "abm_rules")
  rules <- list(x ~ 1, y ~ 2)
  expect_length(abm_rules(!!!rules)$rules, 2)
  # a named call that builds a formula is evaluated, as it always was
  make_rule <- function() x ~ x + 1
  expect_s3_class(abm_rules(r1 = make_rule()), "abm_rules")
  expect_error(abm_rules(money = money - 1), "money ~ money - 1")
})

test_that("engine-owned columns cannot be the target of a rule", {
  for (nm in c(".id", ".group", ".group_id", ".partner", ".role")) {
    f <- rlang::new_formula(rlang::sym(nm), 1)
    expect_error(abm_rules(!!f), class = "tidyABM_reserved_column")
  }
  expect_error(abm_tell(.id ~ 1, to = .partner), class = "tidyABM_reserved_column")
  expect_error(abm_birth(when = TRUE, inherit = .group ~ "b"),
               class = "tidyABM_reserved_column")
  # `.cell` is the one dot column a rule may write
  expect_s3_class(abm_rules(.cell ~ 1L), "abm_rules")
})

test_that("a missing name inside a step is reported with the step, the tick and the columns", {
  m <- abm_setup(agents = abm_agents(n = 5, money = 1), globals = list(step = 1))
  e <- expect_error(abm_run(m, abm_go(abm_rules(money ~ monney - 1)), ticks = 1),
                    class = "tidyABM_missing_column")
  expect_s3_class(e, "tidyABM_step_error")
  msg <- cli::ansi_strip(conditionMessage(e))
  expect_match(msg, "Step 1 of the go block")
  expect_match(msg, "abm_rules")
  expect_match(msg, "tick 1")
  expect_match(msg, "object 'monney' not found")
  expect_match(msg, "money ~ monney - 1")
  expect_match(msg, "Did you mean money")
  expect_match(msg, "Globals: step")
  # the tick is the one the step failed on
  go <- abm_go(abm_rules(money ~ money + 1),
               abm_rules(money ~ if (mean(money) > 3) nothere else money))
  e <- expect_error(abm_run(m, go, ticks = 5), class = "tidyABM_missing_column")
  expect_match(cli::ansi_strip(conditionMessage(e)), "Step 2 of the go block")
  expect_match(cli::ansi_strip(conditionMessage(e)), "tick 3")
  # a global typo is matched against the globals
  e <- expect_error(abm_run(m, abm_go(abm_rules(money ~ money - stp)), ticks = 1))
  expect_match(cli::ansi_strip(conditionMessage(e)), "Did you mean step")
  # inside abm_repeat() the position is not known, the step kind still is
  e <- expect_error(abm_run(m, abm_go(abm_repeat(abm_rules(money ~ zz), max = 2)), ticks = 1))
  expect_match(cli::ansi_strip(conditionMessage(e)), "inside `abm_repeat\\(\\)`")
})

test_that("partner_<col>, own_<col>, undotted names and n get their own hints", {
  m <- abm_setup(agents = abm_agents(n = 6, money = 1),
                 network = abm_network(type = "ring", degree = 2))
  strip <- function(e) cli::ansi_strip(conditionMessage(e))
  e <- expect_error(abm_run(m, abm_go(abm_rules(money ~ partner_money)), ticks = 1),
                    class = "tidyABM_missing_column")
  expect_match(strip(e), "No `abm_match\\(\\)` stands before this step")
  e <- expect_error(abm_run(m, abm_go(abm_match(pair = "random", size = 3),
                                      abm_rules(money ~ partner_money)), ticks = 1))
  expect_match(strip(e), "groups 3 agents")
  e <- expect_error(abm_run(m, abm_go(abm_match(pair = "random"),
                                      abm_rules(money ~ partner_wealth)), ticks = 1))
  expect_match(strip(e), "no `partner_wealth`")
  e <- expect_error(abm_run(m, abm_go(abm_match(pair = "random"),
                                      abm_rules(money ~ if_else(role == "a", 1, 2))), ticks = 1))
  expect_match(strip(e), "did you mean `.role`")
  e <- expect_error(abm_run(m, abm_go(abm_rules(money ~ own_money)), ticks = 1))
  expect_match(strip(e), "own_<col>` exists only")
  e <- expect_error(abm_run(m, abm_go(abm_neighbours(k ~ sum(money > own_wealth))), ticks = 1))
  expect_match(strip(e), "No agent has a column wealth")
  e <- expect_error(abm_run(m, abm_go(abm_rules(money ~ runif(n))), ticks = 1),
                    class = "tidyABM_step_error")
  expect_match(strip(e), "reads `n` without brackets")
  # the sequential loop and a condition go through the same wrapper
  e <- expect_error(abm_run(m, abm_go(abm_sequential(money ~ monny)), ticks = 1),
                    class = "tidyABM_missing_column")
  expect_match(strip(e), "abm_sequential")
  e <- expect_error(abm_run(m, abm_go(abm_death(when = energy < 0)), ticks = 1),
                    class = "tidyABM_missing_column")
  expect_match(strip(e), "abm_death")
})

test_that("the package's own errors pass through the step wrapper untouched", {
  m <- abm_setup(agents = abm_agents(n = 4, x = 1))
  e <- expect_error(abm_run(m, abm_go(abm_global(t ~ x)), ticks = 1),
                    class = "tidyABM_bad_global")
  expect_false(inherits(e, "tidyABM_step_error"))
  e <- expect_error(abm_run(m, abm_go(abm_rules(x ~ c(1, 2))), ticks = 1),
                    class = "tidyABM_step_error")
  expect_match(cli::ansi_strip(conditionMessage(e)), "one value per agent")
})

test_that("a setup formula that fails says what it could see", {
  e <- expect_error(abm_setup(agents = abm_agents(n = 5, a = ~runif(n), b = ~a * cc)),
                    class = "tidyABM_setup_error")
  msg <- cli::ansi_strip(conditionMessage(e))
  expect_match(msg, "Column b of group agents")
  expect_match(msg, "columns declared before it: a")
  e <- expect_error(abm_setup(agents = abm_agents(n = 5, b = ~a * 2, a = ~runif(n))))
  expect_match(cli::ansi_strip(conditionMessage(e)), "declared after")
  e <- expect_error(abm_setup(agents = abm_agents(n = 5, a = ~runif(n()))))
  expect_match(cli::ansi_strip(conditionMessage(e)), "not `n\\(\\)`")
  e <- expect_error(abm_setup(agents = abm_agents(n = 5, alpha = 1, b = ~alpah + 1)))
  expect_match(cli::ansi_strip(conditionMessage(e)), "Did you mean alpha")
})

test_that("a capital N becomes a column, and the missing n says so", {
  e <- expect_error(abm_setup(agents = abm_agents(N = 10, money = 1)),
                    class = "tidyABM_bad_n")
  expect_match(cli::ansi_strip(conditionMessage(e)), "case-sensitive")
})

test_that("a quoted condition is refused with the unquoted form", {
  m <- abm_setup(agents = abm_agents(n = 4, x = 1))
  expect_error(abm_run(m, abm_go(abm_match(pair = "random", eligible = "x > 0"),
                                 abm_rules(x ~ 1)), ticks = 1),
               class = "tidyABM_bad_condition")
  expect_error(abm_run(m, abm_go(abm_death(when = "x > 0")), ticks = 1),
               class = "tidyABM_bad_condition")
  # a numeric condition still coerces as it always did
  r <- abm_run(m, abm_go(abm_death(when = x)), ticks = 1)
  expect_equal(sum(r$tick == 1), 0)
})

test_that("opposite_group checks its two values among the agents taking part", {
  withr::local_seed(1)
  m <- abm_setup(agents = abm_agents(n = 6, side = c("a", "b", "none", "a", "b", "none"),
                                     x = 0))
  go <- abm_go(abm_match(pair = "opposite_group", by = side, eligible = side != "none"),
               abm_rules(x ~ 1))
  r <- abm_run(m, go, ticks = 1, seed = 1)
  expect_equal(r$x[r$tick == 1], c(1, 1, 0, 1, 1, 0))
  # three values among the participants is still an error
  go3 <- abm_go(abm_match(pair = "opposite_group", by = side), abm_rules(x ~ 1))
  e <- expect_error(abm_run(m, go3, ticks = 1), class = "tidyABM_bad_by")
  expect_match(cli::ansi_strip(conditionMessage(e)), "taking part")
})

test_that("nearest refuses a non-numeric coordinate", {
  m <- abm_setup(agents = abm_agents(n = 4, k = c("a", "b", "c", "d"), x = 0))
  e <- expect_error(abm_run(m, abm_go(abm_match(pair = "nearest", by = k), abm_rules(x ~ 1)),
                            ticks = 1), class = "tidyABM_bad_by")
  expect_match(cli::ansi_strip(conditionMessage(e)), "cost")
})

test_that("abm_go() says what a non-step is", {
  m <- abm_setup(agents = abm_agents(n = 2, x = 1))
  strip <- function(e) cli::ansi_strip(conditionMessage(e))
  e <- expect_error(abm_go(abm_rules(x ~ 1), m), class = "tidyABM_not_a_step")
  expect_match(strip(e), "goes to `abm_run\\(\\)`")
  e <- expect_error(abm_go(x ~ 1), class = "tidyABM_not_a_step")
  expect_match(strip(e), "abm_rules\\(x ~ 1\\)")
  e <- expect_error(abm_go(abm_rules(x ~ 1), abm_death), class = "tidyABM_not_a_step")
  expect_match(strip(e), "bare function")
  e <- expect_error(abm_go(abm_rules(x ~ 1), abm_agents(n = 1)), class = "tidyABM_not_a_step")
  expect_match(strip(e), "abm_setup")
})

test_that("print(abm_go) names what each step writes", {
  go <- abm_go(
    abm_match(pair = "random"),
    abm_rules(money ~ 1, payoff ~ 2),
    abm_rules(x ~ 1, .scope = "population"),
    abm_rules(E ~ sum(effort), .by = firm),
    abm_neighbours(k ~ sum(x), within = x > own_x),
    abm_tell(x ~ 1, to = .partner),
    abm_global(a ~ 1, b ~ 2, c ~ 3, d ~ 4, e ~ 5),
    abm_birth(when = x > 1),
    abm_death(when = x < 0)
  )
  out <- printed(go)
  expect_match(out, "rules\\s+money, payoff")
  expect_match(out, "rules\\s+x \\(population\\)")
  expect_match(out, "rules\\s+E \\(by firm\\)")
  expect_match(out, "nbrs\\s+k \\(within\\)")
  expect_match(out, "tell\\s+x to .partner")
  expect_match(out, "global\\s+a, b, c, d, \\+1")
  expect_match(out, "birth\\s+when x > 1")
  expect_match(out, "death\\s+when x < 0")
})

test_that("print(abm_result) counts distinct agents and says so", {
  m <- abm_setup(agents = abm_agents(n = 3, x = 1))
  r <- abm_run(m, abm_go(abm_birth(when = x > 0)), ticks = 1)
  out <- printed(r)
  expect_match(out, "6 distinct agents")
  r <- abm_run(m, abm_go(abm_rules(x ~ 1)), ticks = 1, record = "globals")
  out <- printed(r)
  expect_false(grepl("distinct", out))
})
