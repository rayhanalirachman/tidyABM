test_that("scalars recycle and formulas evaluate once per agent", {
  withr::local_seed(1001)
  m <- abm_setup(agents = abm_agents(n = 10, money = 100, x = ~seq_len(n)))
  g <- m$groups$agents
  expect_equal(nrow(g), 10)
  expect_true(all(g$money == 100))
  expect_equal(g$x, 1:10)
})

test_that("a formula can see columns defined before it", {
  withr::local_seed(1002)
  m <- abm_setup(agents = abm_agents(n = 5, wtp = ~rep(10, n), offer = ~wtp * 0.8))
  expect_equal(m$groups$agents$offer, rep(8, 5))
})

test_that("agent ids are unique across groups", {
  withr::local_seed(1003)
  m <- abm_setup(agents = list(a = abm_agents(n = 3, x = 1),
                               b = abm_agents(n = 4, y = 2)))
  expect_equal(sort(c(m$groups$a$.id, m$groups$b$.id)), 1:7)
  expect_equal(unique(m$groups$b$.group), "b")
})

test_that("a wrong-length column is rejected", {
  withr::local_seed(1004)
  expect_error(
    abm_setup(agents = abm_agents(n = 10, x = ~c(1, 2))),
    class = "tidyABM_bad_column_length"
  )
})

test_that("reserved and unnamed columns are rejected", {
  withr::local_seed(1005)
  expect_error(abm_agents(n = 5, .id = 1), class = "tidyABM_reserved_column")
  expect_error(abm_agents(n = 5, 1), class = "tidyABM_unnamed_column")
})

test_that("a two-sided formula in a column spec is a clear error", {
  withr::local_seed(1006)
  expect_error(abm_agents(n = 5, x = y ~ z), class = "tidyABM_bad_column_spec")
})

test_that("n must be a single non-negative number", {
  withr::local_seed(1007)
  expect_error(abm_agents(n = -1), class = "tidyABM_bad_n")
  expect_error(abm_agents(n = c(1, 2)), class = "tidyABM_bad_n")
})

test_that("agents must be an abm_agents object or a named list of them", {
  withr::local_seed(1008)
  expect_error(abm_setup(agents = data.frame(x = 1)), class = "tidyABM_bad_agents")
  expect_error(
    abm_setup(agents = list(abm_agents(n = 2), abm_agents(n = 2))),
    class = "tidyABM_unnamed_group"
  )
})

test_that("setup formulas read plain globals", {
  base <- -1 # an R variable of the same name loses to the global
  m <- abm_setup(
    abm_agents(n = 3, x = ~rep(base, n), y = ~x + step, step = 100, z = ~step),
    globals = list(base = 2, step = 1, x = 99))
  expect_equal(m$groups$agents$x, rep(2, 3))
  expect_equal(m$groups$agents$y, rep(3, 3)) # earlier column beats global x
  expect_equal(m$groups$agents$z, rep(100, 3)) # and, once declared, global step
})

test_that("lattice, grid and continuous setup formulas read plain globals", {
  g <- list(level = 7, home = 5L, px = 0.25)
  m <- abm_setup(
    agents = list(patches = abm_agents(v = ~rep(level, n)),
                  ant = abm_agents(n = 2, e = ~rep(level, n), at = ~home)),
    space = abm_space(type = "grid", dims = c(3, 3), on = "patches"),
    globals = g)
  expect_equal(m$groups$patches$v, rep(7, 9))
  expect_equal(m$groups$ant$e, rep(7, 2))
  expect_equal(m$groups$ant$.cell, rep(5L, 2))

  m <- abm_setup(abm_grid(dims = c(2, 2), v = ~level), globals = g)
  expect_equal(m$groups$patches$v, rep(7, 4))

  m <- abm_setup(abm_agents(n = 2, at = ~px),
                 space = abm_space(type = "continuous", dims = 1), globals = g)
  expect_equal(m$groups$agents$.x, rep(0.25, 2))

  m <- abm_setup(abm_agents(n = 2, at = list(x = ~px, y = ~px * 2)),
                 space = abm_space(type = "continuous", dims = c(1, 1)),
                 globals = g)
  expect_equal(m$groups$agents$.y, rep(0.5, 2))
})

test_that("formula globals stay out of setup formulas, and say why", {
  err <- expect_error(
    abm_setup(abm_agents(n = 2, a = 1, b = ~tot), globals = list(tot = ~sum(a))),
    class = "tidyABM_missing_column")
  expect_match(conditionMessage(err), "formula global")
})

test_that("params changes a global but not the population built from it", {
  m <- abm_setup(abm_agents(n = 2, x = ~rep(base, n), y = 0),
                 globals = list(base = 1))
  r <- abm_run(m, abm_go(abm_rules(y ~ base)), ticks = 1,
               params = list(base = c(5, 9)))
  expect_equal(unique(r$x), 1)
  expect_setequal(r$y[r$tick == 1], c(5, 9))
})
