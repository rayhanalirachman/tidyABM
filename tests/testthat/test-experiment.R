# params, reps and measures ------------------------------------------------

econ <- function(n = 30) {
  abm_setup(agents = abm_agents(n = n, m = 100), globals = list(step = 1, tax = 0))
}
give <- abm_go(
  abm_match(pair = "random", role = list(g = m > 0, r = TRUE)),
  abm_rules(m ~ if_else(.role == "g", m - step, m + step))
)

test_that("a single run keeps the columns it always had", {
  withr::local_seed(2001)
  r <- abm_run(econ(), give, ticks = 5, seed = 1)
  expect_named(r, c("tick", ".id", ".group", "m"))
  expect_equal(attr(r, "runs"), 1L)
  expect_null(abm_measures(r))
})

test_that("a single run is unaffected by the arguments it does not use", {
  withr::local_seed(2002)
  plain <- abm_run(econ(), give, ticks = 8, seed = 42)
  # reps = 1 and a one-value sweep are both one run, and one run uses `seed` as
  # given -- so every number pinned before these arguments existed still holds
  same <- abm_run(econ(), give, ticks = 8, seed = 42, reps = 1)
  swept <- abm_run(econ(), give, ticks = 8, seed = 42, params = list(step = 1))
  expect_equal(as.data.frame(plain), as.data.frame(same))
  expect_equal(plain$m, swept$m)
})

test_that("reps runs the model that many times, tagged .run and .rep", {
  withr::local_seed(2003)
  r <- abm_run(econ(), give, ticks = 4, reps = 3, seed = 1)
  expect_equal(attr(r, "runs"), 3L)
  expect_equal(sort(unique(r$.run)), 1:3)
  expect_equal(sort(unique(r$.rep)), 1:3)
  expect_equal(nrow(r), 3 * 5 * 30)
  expect_true(all(c(".run", ".rep") %in% names(abm_globals(r))))
})

test_that("replicates differ from each other but the experiment reproduces", {
  withr::local_seed(2004)
  once <- function() {
    m <- abm_measures(
      abm_run(econ(), give, ticks = 30, reps = 4, seed = 7, record = "globals",
              measures = list(spread = ~stats::sd(m)))
    )
    m$spread[m$tick == 30]
  }
  a <- once()
  expect_equal(length(a), 4L)
  expect_gt(length(unique(a)), 1L)   # the runs are not copies of one another
  expect_equal(a, once())            # but the experiment as a whole repeats
})

test_that("params overrides globals, one run per value", {
  withr::local_seed(2005)
  r <- abm_run(econ(), give, ticks = 6, seed = 1, record = "final",
               params = list(step = c(1, 5)))
  expect_equal(sort(unique(r$step)), c(1, 5))
  expect_equal(attr(r, "runs"), 2L)
  # a bigger transfer spreads the money further in the same number of ticks
  spread <- vapply(split(r$m, r$.run), stats::sd, numeric(1))
  expect_gt(spread[[2]], spread[[1]])
})

test_that("several params give every combination, times reps", {
  withr::local_seed(2006)
  r <- abm_run(econ(), give, ticks = 2, seed = 1, record = "final",
               params = list(step = c(1, 2), tax = c(0, 1)), reps = 2)
  expect_equal(attr(r, "runs"), 8L)
  combos <- unique(r[, c("step", "tax")])
  expect_equal(nrow(combos), 4L)
})

test_that("a params entry wrapped in list() is one run, not several", {
  withr::local_seed(2007)
  m <- abm_setup(agents = abm_agents(n = 10, x = 1),
                 globals = list(payoff = c(3, 0, 5, 1)))
  r <- abm_run(m, abm_go(abm_rules(x ~ x * payoff[1])), ticks = 2, seed = 1,
               params = list(payoff = list(c(2, 0, 4, 1))))
  expect_equal(attr(r, "runs"), 1L)
  expect_equal(r$x[r$tick == 2], rep(4, 10))
})

test_that("params only reaches globals", {
  withr::local_seed(2008)
  expect_error(abm_run(econ(), give, ticks = 2, params = list(degree = c(2, 4))),
               class = "tidyABM_bad_params")
  expect_error(abm_run(econ(), give, ticks = 2, params = list(c(1, 2))),
               class = "tidyABM_bad_params")
})

test_that("measures record one value per tick without keeping the population", {
  withr::local_seed(2009)
  r <- abm_run(econ(), give, ticks = 5, seed = 1, record = "globals",
               measures = list(total = ~sum(m), top = ~max(m)))
  expect_equal(nrow(r), 0L)
  meas <- abm_measures(r)
  expect_named(meas, c("tick", "total", "top"))
  expect_equal(meas$tick, 0:5)
  expect_equal(meas$total, rep(3000, 6))   # a pure transfer conserves the money
  expect_gte(meas$top[[6]], meas$top[[1]])
})

test_that("a measure is invisible to the model that produced it", {
  withr::local_seed(2010)
  r <- abm_run(econ(), give, ticks = 3, seed = 1,
               measures = list(total = ~sum(m)))
  expect_false("total" %in% names(r))
  expect_false("total" %in% names(abm_globals(r)))
  # a rule reaching for it does not find it, the way it would find a global
  expect_error(
    abm_run(econ(), abm_go(abm_rules(m ~ total)), ticks = 1, seed = 1,
            measures = list(total = ~sum(m))),
    "total"
  )
})

test_that("a measure must collapse to one value and cannot take a reserved name", {
  withr::local_seed(2011)
  expect_error(abm_run(econ(), give, ticks = 2, measures = list(x = ~m)),
               class = "tidyABM_bad_measures")
  expect_error(abm_run(econ(), give, ticks = 2, measures = list(tick = ~sum(m))),
               class = "tidyABM_bad_measures")
  expect_error(abm_run(econ(), give, ticks = 2, measures = list(x = m ~ sum(m))),
               class = "tidyABM_bad_measures")
  expect_error(abm_run(econ(), give, ticks = 2, measures = list(~sum(m))),
               class = "tidyABM_bad_measures")
})

test_that("measures carry the run columns when there is more than one run", {
  withr::local_seed(2012)
  r <- abm_run(econ(), give, ticks = 3, seed = 1, record = "globals", reps = 2,
               params = list(step = c(1, 2)),
               measures = list(total = ~sum(m)))
  meas <- abm_measures(r)
  expect_named(meas, c(".run", ".rep", "step", "tick", "total"))
  expect_equal(nrow(meas), 4 * 4)
})

test_that("a check that holds lets the run finish and changes nothing", {
  withr::local_seed(2017)
  plain <- abm_run(econ(), give, ticks = 6, seed = 3)
  checked <- abm_run(econ(), give, ticks = 6, seed = 3,
                     checks = list(money = ~sum(m) == 3000))
  expect_equal(as.data.frame(checked), as.data.frame(plain))
  expect_equal(abm_globals(checked), abm_globals(plain))
  expect_null(abm_measures(checked))
})

test_that("a check that fails stops the run at the tick it fails", {
  withr::local_seed(2018)
  leak <- abm_go(abm_global(t ~ t + 1), abm_rules(m ~ m - (t == 3)))
  m <- abm_setup(agents = abm_agents(n = 5, m = 10), globals = list(t = 0))
  err <- expect_error(abm_run(m, leak, ticks = 6, checks = list(money = ~sum(m) == 50)),
                      class = "tidyABM_check_failed")
  expect_equal(err$tick, 3L)
  expect_equal(err$check, "money")
  expect_match(conditionMessage(err), "ticks = 3")
  # inconsistent starting stocks fail before any step has run
  err0 <- expect_error(abm_run(m, leak, ticks = 6, checks = list(money = ~sum(m) == 49)),
                       class = "tidyABM_check_failed")
  expect_equal(err0$tick, 0L)
})

test_that("NA fails a check", {
  m <- abm_setup(agents = abm_agents(n = 3, m = NA_real_))
  err <- expect_error(abm_run(m, give, ticks = 1, checks = list(ok = ~sum(m) > 0)),
                      class = "tidyABM_check_failed")
  expect_equal(err$tick, 0L)
})

test_that("a check must be a named one-sided formula returning one logical", {
  withr::local_seed(2019)
  expect_error(abm_run(econ(), give, ticks = 1, checks = list(total = ~sum(m))),
               class = "tidyABM_bad_checks")
  expect_error(abm_run(econ(), give, ticks = 1, checks = list(two = ~c(TRUE, TRUE))),
               class = "tidyABM_bad_checks")
  expect_error(abm_run(econ(), give, ticks = 1, checks = list(~sum(m) == 3000)),
               class = "tidyABM_bad_checks")
  expect_error(abm_run(econ(), give, ticks = 1, checks = list(a = m ~ TRUE)),
               class = "tidyABM_bad_checks")
})

test_that("in a sweep the failed check names the run", {
  withr::local_seed(2020)
  m <- abm_setup(agents = abm_agents(n = 4, m = 10), globals = list(leak = 0))
  go <- abm_go(abm_rules(m ~ m - leak / n()))
  err <- expect_error(abm_run(m, go, ticks = 2, seed = 1, params = list(leak = c(0, 2)),
                              checks = list(money = ~abs(sum(m) - 40) < 1e-9)),
                      class = "tidyABM_check_failed")
  expect_equal(err$run, 2L)
  expect_equal(err$tick, 1L)
  expect_match(conditionMessage(err), "leak = 2")
})

test_that("a parameter is not repeated in the globals it came from", {
  withr::local_seed(2013)
  g <- abm_globals(abm_run(econ(), give, ticks = 2, seed = 1,
                           params = list(step = c(1, 2))))
  expect_equal(sum(names(g) == "step"), 1L)
  expect_equal(sort(unique(g$step)), c(1, 2))
})

test_that("edges from several runs are stacked and tagged", {
  withr::local_seed(2014)
  m <- abm_setup(agents = abm_agents(n = 12, x = 1),
                 network = abm_network(type = "random", degree = 2),
                 globals = list(k = 1))
  r <- abm_run(m, abm_go(abm_match(pair = "network"), abm_rules(x ~ x + k)),
               ticks = 2, seed = 1, reps = 3)
  e <- abm_edges(r)
  expect_true(".run" %in% names(e))
  expect_equal(sort(unique(e$.run)), 1:3)
})

test_that("seed and reps are checked rather than silently misread", {
  withr::local_seed(2015)
  # `seed = 1:20` used to run once, on seed 1
  expect_error(abm_run(econ(), give, ticks = 2, seed = c(1, 2, 3)),
               class = "tidyABM_bad_seed")
  expect_error(abm_run(econ(), give, ticks = 2, reps = 0),
               class = "tidyABM_bad_reps")
  expect_error(abm_run(econ(), give, ticks = 2, reps = c(2, 3)),
               class = "tidyABM_bad_reps")
})

test_that("an experiment leaves the caller's random state alone", {
  withr::local_seed(2016)
  set.seed(7); before <- runif(3)
  invisible(abm_run(econ(), give, ticks = 3, reps = 4, seed = 123,
                    record = "globals"))
  set.seed(7); after <- runif(3)
  expect_equal(before, after)
})

test_that("abm_measures rejects anything that is not a result", {
  expect_error(abm_measures(1), class = "tidyABM_bad_result")
})

# a world per run -------------------------------------------------------------

world <- function(net, n = 20) {
  abm_setup(agents = abm_agents(n = n, w = ~runif(n)),
            network = abm_network(net, degree = 2))
}
count <- abm_go(abm_neighbours(deg ~ n()),
                abm_global(edges ~ sum(deg) / 2, w1 ~ w[1]))

test_that("a function model builds a fresh world for every run", {
  r <- abm_run(world, count, ticks = 1, reps = 4, seed = 1, record = "globals",
               params = list(net = "poisson"))
  g <- as.data.frame(abm_globals(r))
  g <- g[g$tick == 1, ]
  expect_equal(nrow(g), 4)
  # a built world would repeat the starting population and the graph
  expect_gt(length(unique(g$w1)), 1)
  expect_gt(length(unique(g$edges)), 1)
})

test_that("a function model can sweep what a built world cannot", {
  r <- abm_run(world, count, ticks = 1, seed = 1, record = "globals",
               params = list(net = c("ring", "poisson"), n = c(10, 30)))
  g <- as.data.frame(abm_globals(r))
  g <- g[g$tick == 1 & g$net == "ring", ]
  expect_equal(g$edges[order(g$n)], c(10, 30))   # a degree-2 ring has n edges
})

test_that("worlds and runs reproduce from the experiment seed", {
  go <- function() abm_run(world, count, ticks = 2, reps = 3, seed = 7,
                           record = "globals", params = list(net = "poisson"))
  expect_equal(as.data.frame(abm_globals(go())), as.data.frame(abm_globals(go())))
})

test_that("the run's draws are not the setup's replayed", {
  # setup draws w = runif(n) from the run's seed; if the run restarted that
  # stream, its first runif(n()) would equal w
  m <- function() abm_setup(agents = abm_agents(n = 50, w = ~runif(n), u = 0))
  go <- abm_go(abm_rules(u ~ runif(n()), .scope = "population"))
  r <- abm_run(m, go, ticks = 1, seed = 3, record = "all")
  last <- r[r$tick == 1, ]
  expect_false(isTRUE(all.equal(last$u, last$w)))
})

test_that("params must be arguments of the function, and it must return a model", {
  expect_error(abm_run(world, count, ticks = 1, params = list(qo = 1)),
               class = "tidyABM_bad_params")
  expect_error(abm_run(function() 1, count, ticks = 1), class = "tidyABM_bad_model")
  expect_error(abm_run(1, count, ticks = 1), class = "tidyABM_bad_model")
})

# cores ----------------------------------------------------------------------

test_that("a parallel sweep returns exactly the serial result", {
  go <- function(..., seed = 1) abm_run(econ(), give, ticks = 3, reps = 3,
                                        seed = seed, params = list(step = 1:2), ...)
  expect_identical(go(cores = 2), go())
  # no seed: each run's seed comes from the caller's stream, so cores still
  # cannot change the result
  expect_identical(withr::with_seed(5, go(cores = 2, seed = NULL)),
                   withr::with_seed(5, go(seed = NULL)))
})

test_that("a function model runs in parallel, and a failed check surfaces", {
  r <- function(...) abm_run(world, count, ticks = 1, reps = 2, seed = 7,
                             record = "globals", params = list(net = "poisson"), ...)
  expect_identical(r(cores = 2), r())
  m <- abm_setup(agents = abm_agents(n = 5, m = 10))
  expect_error(abm_run(m, give, ticks = 1, reps = 2, cores = 2,
                       checks = list(no = ~FALSE)), class = "tidyABM_check_failed")
})

test_that("cores must be a whole number or \"max\"", {
  for (bad in list(0, 1.5, "all", NA, c(1, 2))) {
    expect_error(abm_run(econ(), give, ticks = 1, cores = bad),
                 class = "tidyABM_bad_cores")
  }
})
