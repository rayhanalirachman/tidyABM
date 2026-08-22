test_that("rules inside abm_sequential see what the rule above wrote", {
  m <- abm_setup(agents = abm_agents(n = 5, a = 1, b = 0, c = 0))
  r <- abm_run(m, abm_go(abm_sequential(b ~ a + 1, c ~ b + 1)),
               ticks = 1, seed = 1)
  final <- r[r$tick == 1, ]
  expect_true(all(final$b == 2))
  expect_true(all(final$c == 3))   # 3, not 1: c saw the new b
})

test_that("abm_rules is still simultaneous", {
  m <- abm_setup(agents = abm_agents(n = 5, a = 1, b = 0, c = 0))
  r <- abm_run(m, abm_go(abm_rules(b ~ a + 1, c ~ b + 1)), ticks = 1, seed = 1)
  final <- r[r$tick == 1, ]
  expect_true(all(final$b == 2))
  expect_true(all(final$c == 1))   # c saw the old b
})

test_that("a sequential rule writing a global is still routed by its columns", {
  m <- abm_setup(
    agents = list(
      lenders = abm_agents(n = 2, lends = 1),
      savers  = abm_agents(n = 2, saves = 1)
    ),
    globals = list(pot = 0)
  )
  r <- abm_run(m, abm_go(abm_sequential(pot ~ pot + lends)), ticks = 1,
               seed = 1)
  # only the two lenders have `lends`, so the pot moved by 2 and not by 4
  expect_equal(abm_globals(r)$pot[2], 2)
})

test_that("n() works inside abm_global", {
  m <- abm_setup(agents = abm_agents(n = 7, x = 2), globals = list(k = 0))
  r <- abm_run(m, abm_go(abm_global(k ~ n())), ticks = 1, seed = 1)
  expect_equal(abm_globals(r)$k[2], 7L)
})
