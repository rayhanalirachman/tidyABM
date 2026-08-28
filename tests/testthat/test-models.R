# End-to-end regression tests: each of the models the package was designed
# against. These assert the qualitative behaviour the model is known for, with
# tolerances loose enough to survive an internal reordering of RNG calls.

test_that("simple economy conserves money and spreads it out", {
  withr::local_seed(1001)
  r <- abm_run(
    abm_setup(agents = abm_agents(n = 200, money = 100)),
    abm_go(abm_match(pair = "random",
                     role = list(giver = money > 0, receiver = TRUE)),
           abm_rules(money ~ if_else(.role == "giver", money - 1, money + 1))),
    ticks = 100, seed = 1)
  by_tick <- tapply(r$money, r$tick, sum)
  expect_true(all(by_tick == 200 * 100))
  expect_equal(stats::sd(r$money[r$tick == 0]), 0)
  expect_gt(stats::sd(r$money[r$tick == 100]), 5)
})

test_that("el farol updates attendance from the agents' own decisions", {
  withr::local_seed(1002)
  r <- abm_run(
    abm_setup(agents = abm_agents(n = 100, threshold = ~runif(n, 40, 80)),
              globals = list(last_attendance = 60)),
    abm_go(abm_rules(go_today ~ last_attendance < threshold),
           abm_global(last_attendance ~ sum(go_today))),
    ticks = 50, seed = 2)
  att <- abm_globals(r)$last_attendance
  expect_equal(att[[1]], 60)
  expect_true(all(att >= 0 & att <= 100))
  expect_equal(att[-1], as.numeric(tapply(r$go_today[r$tick > 0],
                                          r$tick[r$tick > 0], sum)))
})

test_that("well-mixed prisoner's dilemma collapses to defection", {
  withr::local_seed(1003)
  r <- abm_run(
    abm_setup(agents = abm_agents(
      n = 100, strategy = ~sample(c("cooperate", "defect"), n, replace = TRUE),
      payoff = 0)),
    abm_go(abm_match(pair = "random"),
           abm_rules(payoff ~ case_when(
             strategy == "cooperate" & partner_strategy == "cooperate" ~ 3,
             strategy == "defect"    & partner_strategy == "defect"    ~ 1,
             strategy == "defect"    & partner_strategy == "cooperate" ~ 5,
             strategy == "cooperate" & partner_strategy == "defect"    ~ 0)),
           abm_match(pair = "random"),
           abm_rules(strategy ~ if_else(partner_payoff > payoff,
                                        partner_strategy, strategy))),
    ticks = 100, seed = 3)
  coop <- mean(r$strategy[r$tick == 100] == "cooperate")
  expect_lt(coop, 0.1)
})

test_that("ethnocentrism runs with a changing population", {
  withr::local_seed(1004)
  r <- abm_run(
    abm_setup(agents = abm_agents(
      n = 100, tag = ~sample(c("red", "blue"), n, replace = TRUE),
      strategy = ~sample(c("cooperate", "defect"), n, replace = TRUE),
      resource = 10)),
    abm_go(abm_match(pair = "random"),
           abm_rules(resource ~ case_when(
             strategy == "cooperate" & partner_strategy == "cooperate" ~ resource + 2,
             strategy == "defect"    & partner_strategy == "cooperate" ~ resource + 4,
             strategy == "cooperate" & partner_strategy == "defect"    ~ resource - 1,
             TRUE ~ resource) - 1),
           abm_birth(when = resource > 20, cost = resource ~ resource / 2),
           abm_death(when = resource <= 0)),
    ticks = 30, seed = 4)
  pop <- as.integer(table(r$tick))
  expect_equal(pop[[1]], 100)
  expect_true(any(diff(pop) != 0))          # the population actually moves
  expect_true(all(r$resource > 0))          # the dead are gone
})

test_that("a rumour burns out short of the whole population", {
  withr::local_seed(1005)
  r <- abm_run(
    abm_setup(agents = abm_agents(n = 200, state = ~c("spreader", rep("ignorant", n - 1)))),
    abm_go(abm_match(pair = "random"),
           abm_rules(state ~ case_when(
             state == "ignorant" & partner_state == "spreader" ~ "spreader",
             state == "spreader" & partner_state == "spreader" ~ "stifler",
             state == "spreader" & partner_state == "stifler"  ~ "stifler",
             TRUE ~ state))),
    ticks = 100, seed = 5)
  final <- r$state[r$tick == 100]
  expect_false("spreader" %in% final)       # it dies out
  expect_gt(mean(final == "stifler"), 0.5)  # but most people heard it
})

test_that("party clusters opinions", {
  withr::local_seed(1006)
  r <- abm_run(
    abm_setup(agents = abm_agents(n = 200, opinion = ~runif(n, 0, 1))),
    abm_go(abm_match(pair = "nearest", by = opinion),
           abm_rules(opinion ~ if_else(abs(opinion - partner_opinion) > 0.05,
                                       runif(1), opinion))),
    ticks = 100, seed = 6)
  gap <- function(x) mean(diff(sort(x)))
  expect_lt(gap(r$opinion[r$tick == 100]), gap(r$opinion[r$tick == 0]) * 1.05)
})

test_that("market prices converge toward the clearing price", {
  withr::local_seed(1007)
  market <- abm_setup(agents = list(
    buyers  = abm_agents(n = 200, wtp = ~rnorm(n, 50, 10), offer = ~wtp * 0.8),
    sellers = abm_agents(n = 200, wta = ~rnorm(n, 40, 10), ask   = ~wta * 1.2)),
    seed = 7)
  r <- abm_run(
    market,
    abm_go(abm_match(pair = "opposite_group", by = .group),
           abm_rules(price  ~ (offer + partner_ask) / 2,
                     price  ~ (partner_offer + ask) / 2),
           abm_rules(traded ~ price <= wtp & price >= partner_wta,
                     traded ~ price >= wta & price <= partner_wtp),
           abm_rules(price  ~ if_else(traded, price, NA_real_)),
           abm_rules(offer  ~ pmin(if_else(traded, offer * 0.98, offer * 1.02), wtp)),
           abm_rules(ask    ~ pmax(if_else(traded, ask * 1.02,   ask * 0.98),   wta))),
    ticks = 200, seed = 7)

  expect_true(all(is.na(r$price[r$tick == 0])))
  expect_true(any(r$traded[r$tick == 200], na.rm = TRUE))

  # the clearing price for N(50, 10) buyers against N(40, 10) sellers is 45
  price <- tapply(r$price, r$tick, function(z) mean(z, na.rm = TRUE))
  expect_equal(price[["200"]], 45, tolerance = 0.1)

  # and the prices tighten around it rather than merely passing through
  spread <- tapply(r$price, r$tick, function(z) stats::sd(z, na.rm = TRUE))
  expect_lt(spread[["200"]], spread[["1"]])

  # nobody trades outside their own valuation, which is what the pmin/pmax buy
  b <- !is.na(r$offer); s <- !is.na(r$ask)
  expect_true(all(r$offer[b] <= r$wtp[b] + 1e-9))
  expect_true(all(r$ask[s]   >= r$wta[s] - 1e-9))
  expect_true(all(r$price[b & r$traded %in% TRUE] <= r$wtp[b & r$traded %in% TRUE] + 1e-9))
})

test_that("the voter model moves toward consensus along the network", {
  withr::local_seed(1008)
  r <- abm_run(
    abm_setup(agents = abm_agents(n = 100, opinion = ~sample(c(0, 1), n, replace = TRUE)),
              network = abm_network(type = "random", degree = 4)),
    abm_go(abm_match(pair = "network"), abm_rules(opinion ~ partner_opinion)),
    ticks = 200, seed = 8)
  disagreement <- function(t) {
    o <- r$opinion[r$tick == t]; min(mean(o), 1 - mean(o))
  }
  expect_lt(disagreement(200), disagreement(0))
})

test_that("public goods payoffs are shared within a group", {
  withr::local_seed(1009)
  r <- abm_run(
    abm_setup(agents = abm_agents(
      n = 100, contribution = ~sample(c(0, 1), n, replace = TRUE), payoff = 0)),
    abm_go(abm_match(pair = "random", size = 4),
           abm_rules(payoff ~ sum(contribution) * 2 / 4)),
    ticks = 20, seed = 9)
  last <- r[r$tick == 20, ]
  expect_true(all(last$payoff %in% c(0, 0.5, 1, 1.5, 2)))
  # each distinct payoff level is shared by whole groups of 4
  expect_true(all(table(last$payoff) %% 4 == 0))
})

test_that("iterated PD with fixed partners sustains cooperation", {
  withr::local_seed(1010)
  r <- abm_run(
    abm_setup(agents = abm_agents(n = 100, move = "cooperate", payoff = 0),
              network = abm_network(type = "random", degree = 1)),
    abm_go(abm_match(pair = "network"),
           abm_rules(payoff ~ case_when(
             move == "cooperate" & partner_move == "cooperate" ~ 3,
             move == "defect"    & partner_move == "defect"    ~ 1,
             move == "defect"    & partner_move == "cooperate" ~ 5,
             move == "cooperate" & partner_move == "defect"    ~ 0)),
           abm_rules(move ~ partner_move)),
    ticks = 50, seed = 10)
  expect_true(all(r$move[r$tick == 50] == "cooperate"))
  expect_true(all(r$payoff[r$tick == 50] == 3))
})

test_that("zakah moves wealth from payers above nisab to recipients below the poverty line", {
  withr::local_seed(1011)
  nisab <- 100; poverty_line <- 30
  # 4 payers at 200, 2 recipients at 10, nobody in between
  m <- abm_setup(
    agents = abm_agents(n = 6, wealth = ~c(rep(200, 4), 10, 10), income = 0),
    globals = list(zakah_pool = 0))
  r <- abm_run(m, abm_go(
    abm_global(zakah_pool ~ sum(if_else(wealth > nisab, wealth * 0.025, 0))),
    abm_rules(wealth ~ if_else(wealth > nisab, wealth * 0.975, wealth)),
    abm_rules(wealth ~ if_else(wealth < poverty_line,
                               wealth + zakah_pool / sum(wealth < poverty_line),
                               wealth))),
    ticks = 1, seed = 12)

  last <- r[r$tick == 1, ]
  expect_equal(abm_globals(r)$zakah_pool[[2]], 4 * 200 * 0.025)  # 20
  expect_equal(sort(last$wealth), c(20, 20, rep(195, 4)))        # 10 + 20/2 each
  expect_equal(sum(last$wealth), sum(r$wealth[r$tick == 0]))     # nothing lost
})

test_that("zakah with a realistic distribution keeps collecting", {
  withr::local_seed(1012)
  nisab <- 100; poverty_line <- 30
  r <- abm_run(
    abm_setup(agents = abm_agents(n = 300, wealth = ~rlnorm(n, 4, 0.5),
                                  income = ~rlnorm(n, 3, 0.4)),
              globals = list(zakah_pool = 0)),
    abm_go(
      abm_rules(wealth ~ wealth + income - (0.6 * income + 0.02 * wealth)),
      abm_global(zakah_pool ~ sum(if_else(wealth > nisab, wealth * 0.025, 0))),
      abm_rules(wealth ~ if_else(wealth > nisab, wealth * 0.975, wealth)),
      abm_rules(wealth ~ if_else(wealth < poverty_line,
                                 wealth + zakah_pool / sum(wealth < poverty_line),
                                 wealth))),
    ticks = 50, seed = 12)
  pool <- abm_globals(r)$zakah_pool
  expect_equal(pool[[1]], 0)
  expect_true(all(pool[-1] > 0))
  expect_true(all(r$wealth > 0))
})

test_that("bank reserves lends only what the bank has", {
  withr::local_seed(1013)
  reserve_ratio <- 0.1
  r <- abm_run(
    abm_setup(agents = abm_agents(n = 100, wallet = ~runif(n, 0, 50),
                                  savings = 0, loan = 0, draw = 0),
              globals = list(bank_deposits = 0, bank_loans = 0, bank_reserves = 0)),
    abm_go(
      abm_match(pair = "random", role = list(giver = TRUE, receiver = TRUE)),
      abm_rules(gift ~ sample(c(0, 2, 5), 1)),
      abm_rules(wallet ~ if_else(.role == "giver", wallet - gift, wallet + partner_gift)),
      abm_rules(
        savings ~ if_else(wallet > 0, savings + wallet, savings - pmin(savings, abs(wallet))),
        wallet  ~ if_else(wallet > 0, 0, wallet + pmin(savings, abs(wallet)))),
      abm_global(bank_deposits ~ sum(savings)),
      abm_sequential(
        draw          ~ if_else(wallet < 0 & bank_reserves > 0,
                                pmin(-wallet, bank_reserves), 0),
        loan          ~ loan + draw,
        wallet        ~ wallet + draw,
        bank_reserves ~ bank_reserves - draw),
      abm_global(bank_loans    ~ sum(loan)),
      abm_global(bank_reserves ~ bank_deposits * reserve_ratio - bank_loans)),
    ticks = 100, seed = 13)

  g <- abm_globals(r)
  expect_true(all(g$bank_loans >= 0))
  # lending is bounded by the reserve requirement
  expect_true(all(g$bank_loans <= g$bank_deposits * reserve_ratio + 10))
  expect_true(all(r$loan >= 0))
})

test_that("a depleting reserve is shared out, not handed to everybody", {
  # five borrowers, each wanting 10, against a reserve of 25: they get 25
  # between them, not 50
  m <- abm_setup(agents = abm_agents(n = 5, wallet = -10, loan = 0, draw = 0),
                 globals = list(bank_reserves = 25))
  r <- abm_run(m, abm_go(abm_sequential(
    draw          ~ if_else(wallet < 0, pmin(-wallet, bank_reserves), 0),
    loan          ~ loan + draw,
    wallet        ~ wallet + draw,
    bank_reserves ~ bank_reserves - draw
  )), ticks = 1, seed = 1)
  expect_equal(sum(r$loan[r$tick == 1]), 25)
  expect_equal(abm_globals(r)$bank_reserves[[2]], 0)
})
