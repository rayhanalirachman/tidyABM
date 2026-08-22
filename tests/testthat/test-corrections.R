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
  withr::local_seed(1002)
  skip_on_cran()
  MEMORY <- 5; N_STRAT <- 10; CAPACITY <- 60
  f <- function(lhs, rhs) rlang::new_formula(str2lang(lhs), str2lang(rhs))
  lags <- paste0("att", 1:MEMORY)

  cols <- list()
  for (s in seq_len(N_STRAT)) {
    for (j in 0:MEMORY) cols[[sprintf("w%d_%d", s, j)]] <- ~runif(n, -1, 1)
    cols[[sprintf("e%d", s)]] <- 0
  }
  cols$active <- 1L

  farol <- abm_setup(
    agents  = do.call(abm_agents, c(list(n = 100), cols)),
    globals = stats::setNames(as.list(rep(CAPACITY, MEMORY)), lags))

  forecast <- do.call(abm_rules, lapply(seq_len(N_STRAT), function(s)
    f(paste0("p", s), paste(c(sprintf("w%d_0 * 100", s),
                              sprintf("w%d_%d * %s", s, 1:MEMORY, lags)),
                            collapse = " + "))))
  act <- abm_rules(f("go_today", paste0(
    "case_when(", paste(sprintf("active == %d ~ p%d", 1:(N_STRAT - 1), 1:(N_STRAT - 1)),
                        collapse = ", "), ", TRUE ~ p", N_STRAT, ") < ", CAPACITY)))
  observe <- do.call(abm_global, c(
    lapply(MEMORY:2, function(j) f(paste0("att", j), paste0("att", j - 1))),
    list(f("att1", "sum(go_today)"))))
  rescore <- do.call(abm_rules, lapply(seq_len(N_STRAT), function(s)
    f(paste0("e", s), sprintf("0.8 * e%d + 0.2 * abs(p%d - att1)", s, s))))
  best <- abm_rules(f("active", paste0(
    "case_when(",
    paste(sprintf("e%d <= pmin(%s) ~ %dL", 1:(N_STRAT - 1),
                  vapply(1:(N_STRAT - 1), function(s)
                    paste0("e", setdiff(1:N_STRAT, s), collapse = ", "), character(1)),
                  1:(N_STRAT - 1)), collapse = ", "),
    ", TRUE ~ ", N_STRAT, "L)")))

  r <- abm_run(farol, abm_go(forecast, act, observe, rescore, best),
               ticks = 300, seed = 2)
  a <- abm_globals(r)$att1[-(1:100)]

  expect_gt(length(unique(a)), 5)          # never settles
  expect_gt(stats::sd(a), 1)               # with real amplitude
  expect_true(abs(mean(a) - 60) < 8)       # around the bar's capacity
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
    network = network)

  share <- function(r, type) {
    x <- r[r$tick == max(r$tick), ]
    switch(type,
      ethnocentric = mean(x$coop_in & !x$coop_out),
      egoist       = mean(!x$coop_in & !x$coop_out))
  }

  common <- function(...) abm_go(
    abm_match(...),
    abm_rules(give ~ if_else(partner_tag == tag, coop_in, coop_out)),
    abm_rules(ptr ~ BASE - COST * give + BENEFIT * partner_give),
    abm_birth(when = runif(n()) < ptr),
    abm_death(when = runif(n()) < DEATH + 0.25 * pmax(0, (n() - CAP) / CAP)),
    do.call(abm_birth, list(n = 8, cost = traits)))

  mixed <- abm_run(pop(), common(pair = "random"), ticks = 400, seed = 1)

  local <- abm_run(pop(abm_network(type = "random", degree = 4)), abm_go(
    abm_match(pair = "network"),
    abm_rules(give ~ if_else(partner_tag == tag, coop_in, coop_out)),
    abm_rules(ptr ~ BASE - COST * give + BENEFIT * partner_give),
    abm_birth(when = runif(n()) < ptr,
              attach_via = abm_match(pair = "network", from = "parent")),
    abm_death(when = runif(n()) < DEATH + 0.25 * pmax(0, (n() - CAP) / CAP)),
    do.call(abm_birth, list(n = 8, attach_via = abm_match(pair = "network"),
                            cost = traits))
  ), ticks = 400, seed = 1)

  # well mixed, the paper's control condition: ethnocentrism does not take hold
  # and egoists do at least as well
  expect_gte(share(mixed, "egoist"), share(mixed, "ethnocentric"))
  # local reproduction is what flips it
  expect_gt(share(local, "ethnocentric"), share(local, "egoist"))
  expect_gt(share(local, "ethnocentric"), share(mixed, "ethnocentric") + 0.05)
  expect_lt(share(local, "egoist"), share(mixed, "egoist") - 0.05)

  # ...because neighbours end up being kin
  e <- abm_edges(local)
  last <- local[local$tick == 400, ]
  tg <- stats::setNames(last$tag, last$.id)
  same <- mean(tg[as.character(e$from)] == tg[as.character(e$to)], na.rm = TRUE)
  expect_gt(same, 0.8)
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
