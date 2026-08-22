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
