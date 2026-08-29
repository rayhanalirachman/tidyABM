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
  MEMORY <- 5; CAPACITY <- 60

  # The predictors live in list columns: `w` is one matrix of weights per agent,
  # `e` one score per predictor. Both seeds matter, so both are fixed.
  farol <- function(n_strat, setup_seed) abm_setup(
    agents = abm_agents(
      n = 100,
      w = ~lapply(seq_len(n), function(i)
            matrix(stats::runif(n_strat * (MEMORY + 1), -1, 1),
                   n_strat, MEMORY + 1)),
      e = ~lapply(seq_len(n), function(i) numeric(n_strat)),
      p = ~lapply(seq_len(n), function(i) numeric(n_strat)),
      go_today = FALSE),
    globals = as.list(stats::setNames(rep(CAPACITY, MEMORY),
                                      paste0("att", 1:MEMORY))),
    seed = setup_seed)

  go <- abm_go(
    abm_rules(p ~ lapply(w, function(W)
      as.vector(W %*% c(100, att1, att2, att3, att4, att5)))),
    abm_rules(go_today ~ mapply(function(pi, ei) pi[which.min(ei)], p, e) < CAPACITY),
    abm_global(att5 ~ att4, att4 ~ att3, att3 ~ att2, att2 ~ att1,
               att1 ~ sum(go_today)),
    abm_rules(e ~ mapply(function(ei, pi) 0.8 * ei + 0.2 * abs(pi - att1),
                         e, p, SIMPLIFY = FALSE)))

  attendance <- function(n_strat, setup_seed)
    abm_globals(abm_run(farol(n_strat, setup_seed), go,
                        ticks = 300, seed = 2))$att1[-(1:101)]
  settles <- function(a) {
    any(vapply(1:6, function(lag) all(utils::tail(a, 60) ==
      utils::tail(dplyr::lag(a, lag), 60)), logical(1)))
  }

  a <- attendance(10, 1)
  expect_false(settles(a))                 # no cycle to find
  expect_gt(length(unique(a)), 5)
  expect_gt(stats::sd(a), 1)               # with real amplitude
  expect_true(abs(mean(a) - CAPACITY) < 8) # around the bar's capacity

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

test_that("the zakah model's recipient pool empties with an absolute poverty line", {
  withr::local_seed(1006)
  NISAB <- 100
  r <- abm_run(
    abm_setup(agents  = abm_agents(n = 500, wealth = ~rlnorm(n, 4, 0.5),
                                   income = ~rlnorm(n, 3, 0.4)),
              globals = list(zakah_pool = 0)),
    abm_go(abm_rules(wealth ~ wealth + income - (0.6 * income + 0.02 * wealth)),
           abm_global(zakah_pool ~ sum(if_else(wealth > NISAB, wealth * 0.025, 0))),
           abm_rules(wealth ~ if_else(wealth > NISAB, wealth * 0.975, wealth)),
           abm_rules(wealth ~ if_else(wealth < 30,
                                      wealth + zakah_pool / sum(wealth < 30), wealth))),
    ticks = 60, seed = 12)
  poor <- tapply(r$wealth < 30, r$tick, sum)
  expect_gt(poor[["0"]], 0)
  expect_equal(unname(poor[["60"]]), 0)   # nobody left to receive
  expect_gt(abm_globals(r)$zakah_pool[[61]], 0)  # ...but it is still collected
})

test_that("zakah reduces inequality once the model has a risk process", {
  withr::local_seed(1007)
  skip_on_cran()
  NISAB <- 100
  shocks <- abm_rules(
    income ~ exp(0.9 * log(income) + 0.1 * 3 + rnorm(n(), 0, 0.15)),
    wealth ~ wealth - if_else(runif(n()) < 0.03, wealth * 0.6, 0))
  consume <- abm_rules(
    wealth ~ pmax(0.01, wealth + income - (0.6 * income + 0.02 * wealth)))
  line <- abm_global(poverty_line ~ 0.5 * median(wealth))

  start <- function() abm_setup(
    agents  = abm_agents(n = 500, wealth = ~rlnorm(n, 4, 0.5),
                         income = ~rlnorm(n, 3, 0.4)),
    globals = list(zakah_pool = 0, poverty_line = 30))

  with_z <- abm_run(start(), abm_go(
    shocks, consume, line,
    abm_global(zakah_pool ~ sum(if_else(wealth > NISAB, wealth * 0.025, 0))),
    abm_rules(wealth ~ if_else(wealth > NISAB, wealth * 0.975, wealth)),
    abm_rules(wealth ~ if_else(wealth < poverty_line,
                               wealth + zakah_pool / pmax(1, sum(wealth < poverty_line)),
                               wealth))), ticks = 300, seed = 12)
  base <- abm_run(start(), abm_go(shocks, consume, line), ticks = 300, seed = 12)

  gini <- function(x) {
    x <- sort(x); n <- length(x); sum((2 * seq_len(n) - n - 1) * x) / (n * sum(x))
  }
  wz <- with_z$wealth[with_z$tick == 300]
  wb <- base$wealth[base$tick == 300]

  expect_gt(stats::quantile(wz, 0.1), stats::quantile(wb, 0.1))
  expect_lt(gini(wz), gini(wb))
  # the recipient pool stays non-empty in the baseline, so there is something
  # for zakah to act on
  pl <- abm_globals(base)$poverty_line[match(base$tick, abm_globals(base)$tick)]
  expect_gt(mean(tail(tapply(base$wealth < pl, base$tick, mean), 100)), 0.02)
})
