test_that("a run of n ticks returns n + 1 snapshots, starting at tick 0", {
  withr::local_seed(1001)
  m <- abm_setup(agents = abm_agents(n = 5, x = 0))
  r <- abm_run(m, abm_go(abm_rules(x ~ x + 1)), ticks = 3, seed = 1)
  expect_equal(sort(unique(r$tick)), 0:3)
  expect_equal(nrow(r), 20)
  expect_equal(r$x[r$tick == 0], rep(0, 5))
  expect_equal(r$x[r$tick == 3], rep(3, 5))
})

test_that("the same seed gives the same run", {
  withr::local_seed(1002)
  build <- function() {
    abm_run(abm_setup(agents = abm_agents(n = 30, m = 100)),
            abm_go(abm_match(pair = "random",
                             role = list(g = m > 0, r = TRUE)),
                   abm_rules(m ~ if_else(.role == "g", m - 1, m + 1))),
            ticks = 20, seed = 99)
  }
  expect_equal(as.data.frame(build()), as.data.frame(build()))
})

test_that("abm_run leaves the caller's random state alone", {
  withr::local_seed(1003)
  set.seed(7); before <- runif(3)
  invisible(abm_run(abm_setup(agents = abm_agents(n = 5, m = 1)),
                    abm_go(abm_rules(m ~ m + runif(1))), ticks = 3, seed = 123))
  set.seed(7); after <- runif(3)
  expect_equal(before, after)
})

test_that("arguments are checked", {
  withr::local_seed(1004)
  m <- abm_setup(agents = abm_agents(n = 3, x = 1))
  go <- abm_go(abm_rules(x ~ x))
  expect_error(abm_run("nope", go, 1), class = "tidyABM_bad_model")
  expect_error(abm_run(m, "nope", 1), class = "tidyABM_bad_go")
  expect_error(abm_run(m, go, -1), class = "tidyABM_bad_ticks")
})

test_that("accessors work and reject the wrong input", {
  withr::local_seed(1005)
  m <- abm_setup(agents = abm_agents(n = 5, x = 1), globals = list(g = 0))
  r <- abm_run(m, abm_go(abm_global(g ~ sum(x))), ticks = 2, seed = 1)
  expect_equal(nrow(abm_globals(r)), 3)
  expect_null(abm_edges(r))
  expect_equal(n_agents(r), 5)
  expect_error(abm_globals(mtcars), class = "tidyABM_bad_result")
})

test_that("seeding setup and run together reproduces an experiment end to end", {
  experiment <- function() {
    m <- abm_setup(agents = abm_agents(n = 50, x = ~runif(n)), seed = 4)
    abm_run(m, abm_go(abm_match(pair = "random"), abm_rules(x ~ partner_x)),
            ticks = 10, seed = 5)
  }
  withr::local_seed(1)
  a <- experiment()
  withr::local_seed(999)          # a different ambient state...
  b <- experiment()               # ...must not change the experiment
  expect_equal(as.data.frame(a), as.data.frame(b))
})

test_that("abm_run's seed alone does not fix a randomly drawn population", {
  # documented sharp edge: the population is drawn when abm_setup() runs
  go <- abm_go(abm_rules(x ~ x))
  withr::local_seed(1); a <- abm_run(abm_setup(agents = abm_agents(n = 50, x = ~runif(n))), go, 1, seed = 5)
  withr::local_seed(2); b <- abm_run(abm_setup(agents = abm_agents(n = 50, x = ~runif(n))), go, 1, seed = 5)
  expect_false(isTRUE(all.equal(a$x, b$x)))
})
