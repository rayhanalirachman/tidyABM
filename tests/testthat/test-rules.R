test_that("rules in one call are evaluated simultaneously", {
  withr::local_seed(1001)
  r <- abm_run(abm_setup(agents = abm_agents(n = 3, a = 1, b = 2)),
               abm_go(abm_rules(a ~ b, b ~ a)), ticks = 1, seed = 1)
  last <- r[r$tick == 1, ]
  expect_true(all(last$a == 2))
  expect_true(all(last$b == 1))
})

test_that("rules are routed to the group that has the columns", {
  withr::local_seed(1002)
  m <- abm_setup(agents = list(buyers  = abm_agents(n = 5, offer = 10),
                               sellers = abm_agents(n = 5, ask = 20)))
  r <- abm_run(m, abm_go(abm_rules(offer ~ offer + 1), abm_rules(ask ~ ask - 1)),
               ticks = 1, seed = 1)
  last <- r[r$tick == 1, ]
  expect_equal(unique(last$offer[last$.group == "buyers"]), 11)
  expect_equal(unique(last$ask[last$.group == "sellers"]), 19)
  expect_true(all(is.na(last$ask[last$.group == "buyers"])))
})

test_that("a rule that matches no group is an error, not a silent no-op", {
  withr::local_seed(1003)
  m <- abm_setup(agents = list(a = abm_agents(n = 3, x = 1),
                               b = abm_agents(n = 3, y = 1)))
  # `x` and `y` live in different groups, so this rule fits neither
  expect_error(
    abm_run(m, abm_go(abm_rules(z ~ x + y)), ticks = 1),
    class = "tidyABM_unapplied_rule"
  )
})

test_that("agents left out of a match keep their values", {
  withr::local_seed(1004)
  # only one agent can give, so only one pair acts
  m <- abm_setup(agents = abm_agents(n = 10, money = ~c(rep(0, 9), 5)))
  r <- abm_run(m, abm_go(
    abm_match(pair = "random", role = list(giver = money > 0, receiver = TRUE)),
    abm_rules(money ~ if_else(.role == "giver", money - 1, money + 1))
  ), ticks = 1, seed = 3)
  last <- r[r$tick == 1, ]
  expect_equal(sum(last$money), 5)          # conserved
  expect_equal(sum(last$money == 0), 8)     # eight agents untouched
})

test_that("rules are evaluated per matched group, not per population", {
  withr::local_seed(1005)
  # sample(x, 1) must be drawn once per pair
  r <- abm_run(abm_setup(agents = abm_agents(n = 40, w = 0)),
               abm_go(abm_match(pair = "random"),
                      abm_rules(gift ~ sample(c(0, 2, 5), 1))),
               ticks = 1, seed = 4)
  last <- r[r$tick == 1, ]
  expect_gt(length(unique(last$gift)), 1)
  # ...and both members of a pair share it
  expect_true(all(table(last$gift) %% 2 == 0))
})

test_that("group aggregates use the matched group", {
  withr::local_seed(1006)
  r <- abm_run(
    abm_setup(agents = abm_agents(n = 100, contribution = ~rep(c(0, 1), 50), payoff = 0)),
    abm_go(abm_match(pair = "random", size = 4),
           abm_rules(payoff ~ sum(contribution) * 2 / 4)),
    ticks = 1, seed = 5)
  last <- r[r$tick == 1, ]
  expect_true(all(last$payoff %in% c(0, 0.5, 1, 1.5, 2)))
})

test_that("globals are readable in rules and writable by abm_global", {
  withr::local_seed(1007)
  m <- abm_setup(agents = abm_agents(n = 10, x = 1), globals = list(k = 5))
  r <- abm_run(m, abm_go(abm_rules(x ~ x + k), abm_global(k ~ mean(x))),
               ticks = 2, seed = 1)
  expect_equal(abm_globals(r)$k, c(5, 6, 12))
})

test_that("abm_global rejects a rule that does not collapse", {
  withr::local_seed(1008)
  m <- abm_setup(agents = abm_agents(n = 10, x = 1), globals = list(k = 0))
  expect_error(abm_run(m, abm_go(abm_global(k ~ x)), ticks = 1),
               class = "tidyABM_bad_global")
})

test_that("abm_sequential lets each agent see the previous agent's writes", {
  withr::local_seed(1009)
  # a pot of 10 units, 20 claimants of 1 unit each: exactly 10 get served
  m <- abm_setup(agents = abm_agents(n = 20, got = 0), globals = list(pot = 10))
  r <- abm_run(m, abm_go(abm_sequential(
    got ~ if_else(pot > 0, 1, 0),
    pot ~ if_else(pot > 0, pot - 1, pot)
  )), ticks = 1, seed = 6)
  last <- r[r$tick == 1, ]
  expect_equal(sum(last$got), 10)
  expect_equal(abm_globals(r)$pot[[2]], 0)
})

test_that("abm_rules would NOT have produced that (the reason abm_sequential exists)", {
  withr::local_seed(1010)
  m <- abm_setup(agents = abm_agents(n = 20, got = 0), globals = list(pot = 10))
  r <- abm_run(m, abm_go(abm_rules(got ~ if_else(pot > 0, 1, 0))),
               ticks = 1, seed = 6)
  expect_equal(sum(r$got[r$tick == 1]), 20)
})
