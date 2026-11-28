# The three models whose short form runs correctly but does not reproduce the
# behaviour they are known for. These tests pin the corrected mechanisms.

test_that("a single shared forecast makes El Farol degenerate", {
  withr::local_seed(1001)
  r <- abm_run(
    abm_setup(agents  = abm_agents(n = 100, threshold = ~runif(n, 40, 80)),
              globals = list(last_attendance = 60)),
    abm_go(abm_rules(go_today ~ last_attendance < threshold),
           abm_global(last_attendance ~ sum(go_today))),
    ticks = 40, seed = 2)
  # it locks into a two-cycle: at most two distinct attendance levels
  expect_lte(length(unique(tail(abm_globals(r)$last_attendance, 20))), 2)
})

test_that("El Farol fluctuates once agents switch between predictors", {
  skip_on_cran()
  # to-report predict-attendance [strategy subhistory]
  predict_attendance <- function(strategy, subhistory) {
    100 * strategy[1] + sum(strategy[-1] * subhistory)
  }

  # how far off would ONE strategy have been, over the last `memory` weeks?
  score_strategy <- function(strategy, history, memory) {
    sum(vapply(1:memory, function(week) abs(
      history[week] - predict_attendance(strategy, history[week + 1:memory])),
      numeric(1)))
  }

  # the strategy that has been least wrong lately (to update-strategies)
  best_strategy_of <- function(strategies, history, memory) {
    strategies[[which.min(vapply(strategies, score_strategy, numeric(1),
                                 history, memory))]]
  }

  farol <- function(n_strat = 10, setup_seed = 1, ticks = 300, memory = 5) {
    m <- abm_setup(
      agents = abm_agents(
        n = 100,
        # each strategy is a baseline plus one random weight per week of lag
        strategies = ~replicate(n, replicate(
          n_strat, stats::runif(memory + 1, -1, 1), simplify = FALSE), simplify = FALSE),
        best       = ~lapply(strategies, function(s) s[[1]]),  # replaced at tick 1
        prediction = 0,
        attend     = FALSE),
      globals = list(history = list(sample(0:99, memory * 2, replace = TRUE)),
                     attendance = 0, memory = memory, threshold = 60),
      seed = setup_seed)

    go <- abm_go(
      # update-strategies runs first, so the placeholder never decides anything
      abm_rules(best ~ lapply(strategies, best_strategy_of, history[[1]], memory)),
      abm_rules(prediction ~ vapply(best, predict_attendance, numeric(1),
                                    history[[1]][1:memory])),
      abm_rules(attend ~ prediction <= threshold),
      abm_global(attendance ~ sum(attend),
                 history    ~ list(c(attendance, head(history[[1]], -1)))))

    abm_run(m, go, ticks = ticks, record = "globals")
  }

  attendance <- function(n_strat, setup_seed)
    abm_globals(farol(n_strat, setup_seed))$attendance[-(1:101)]
  settles <- function(a) {
    any(vapply(1:6, function(lag) all(utils::tail(a, 60) ==
      utils::tail(dplyr::lag(a, lag), 60)), logical(1)))
  }

  a <- attendance(10, 1)
  expect_false(settles(a))                 # no cycle to find
  expect_gt(length(unique(a)), 5)
  expect_gt(stats::sd(a), 1)               # with real amplitude
  expect_true(abs(mean(a) - 60) < 8)       # around the bar's capacity

  # a second population draw, because the mechanism is not one lucky seed
  b <- attendance(10, 2)
  expect_false(settles(b))
  expect_gt(stats::sd(b), 1)

  # and it is the pool of predictors that does it: with one, the agents are
  # back to a single shared forecast and the run locks solid
  expect_true(settles(attendance(1, 1)))
})

test_that("ethnocentrism needs local reproduction, not just conditional strategies", {
  withr::local_seed(1003)
  skip_on_cran()
  COST <- 0.01; BENEFIT <- 0.03; BASE <- 0.12; DEATH <- 0.10; CAP <- 800

  traits <- list(
    tag      ~ sample(c("red", "blue"), n(), replace = TRUE),
    coop_in  ~ sample(c(TRUE, FALSE),   n(), replace = TRUE),
    coop_out ~ sample(c(TRUE, FALSE),   n(), replace = TRUE))

  pop <- function(network = NULL) abm_setup(
    agents = abm_agents(
      n = 400,
      tag      = ~sample(c("red", "blue"), n, replace = TRUE),
      coop_in  = ~sample(c(TRUE, FALSE), n, replace = TRUE),
      coop_out = ~sample(c(TRUE, FALSE), n, replace = TRUE),
      ptr      = BASE),
    network = network, seed = 1)

  share <- function(r, type) {
    x <- r[r$tick == max(r$tick), ]
    switch(type,
      ethnocentric = mean(x$coop_in & !x$coop_out),
      egoist       = mean(!x$coop_in & !x$coop_out),
      in_group     = mean(x$coop_in))
  }

  interact <- list(
    abm_rules(give ~ if_else(partner_tag == tag, coop_in, coop_out)),
    abm_rules(ptr ~ BASE - COST * give + BENEFIT * partner_give))
  reap <- abm_death(when = runif(n()) < DEATH + 0.25 * pmax(0, (n() - CAP) / CAP))

  mixed <- abm_run(pop(), do.call(abm_go, c(
    list(abm_match(pair = "random")), interact,
    list(abm_birth(when = runif(n()) < ptr), reap,
         abm_birth(n = 8, inherit = traits)))),
    ticks = 400, seed = 1)

  local <- abm_run(pop(abm_network(type = "random", degree = 4)),
    do.call(abm_go, c(
      list(abm_match(pair = "network")), interact,
      list(abm_birth(when = runif(n()) < ptr, links = 4,
                     attach_via = abm_match(pair = "network", from = "parent")),
           reap,
           abm_birth(n = 8, links = 4, inherit = traits,
                     attach_via = abm_match(pair = "network"))))),
    ticks = 400, seed = 1)

  # well mixed, the paper's control condition: cooperating with your own kind
  # buys nothing, and egoists lead
  expect_gt(share(mixed, "egoist"), share(mixed, "ethnocentric"))
  # local reproduction is what flips it. Which of the two in-group strategies
  # ends up ahead is not robust across seeds, so the claim is about egoists
  # collapsing and in-group cooperation taking the population
  expect_lt(share(local, "egoist"), share(mixed, "egoist") - 0.15)
  expect_gt(share(local, "in_group"), share(mixed, "in_group") + 0.2)
  expect_gt(share(local, "in_group"), 0.6)

  # ...because neighbours end up being kin
  e <- abm_edges(local)
  last <- local[local$tick == 400, ]
  tg <- stats::setNames(last$tag, last$.id)
  same <- mean(tg[as.character(e$from)] == tg[as.character(e$to)], na.rm = TRUE)
  expect_gt(same, 0.7)

  # and because there is still a network to be local in: one edge per newborn
  # erodes a 4-regular graph into parent-child pairs, and the same-tag share
  # then measures kinship rather than neighbourhood
  expect_gt(2 * nrow(e) / nrow(last), 2.5)
  expect_lt(mean(!last$.id %in% c(e$from, e$to)), 0.15)
})

test_that("from = 'parent' links a newborn to the agent it was cloned from", {
  withr::local_seed(1004)
  m <- abm_setup(agents = abm_agents(n = 4, x = ~c(1, 0, 0, 0)),
                 network = abm_network(type = "empty"))
  r <- abm_run(m, abm_go(abm_birth(
    when = x == 1, attach_via = abm_match(pair = "network", from = "parent"))),
    ticks = 1, seed = 1)
  e <- abm_edges(r)
  expect_equal(nrow(e), 1)
  expect_equal(sort(c(e$from, e$to)), c(1L, 5L))   # agent 1 and its clone
})

test_that("from = 'parent' is refused where there is no parent", {
  withr::local_seed(1005)
  m <- abm_setup(agents = abm_agents(n = 4, x = 1),
                 network = abm_network(type = "empty"))
  expect_error(
    abm_run(m, abm_go(abm_birth(
      n = 1, attach_via = abm_match(pair = "network", from = "parent"))), ticks = 1),
    class = "tidyABM_no_parent"
  )
})

