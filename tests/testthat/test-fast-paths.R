# The engine takes three shortcuts that must be invisible: an elementwise rule
# under a standing match is evaluated once rather than per pair, an ungrouped
# expression skips the dplyr mask, and common neighbourhood aggregates are
# folded with rowsum(). Each is checked against the path it replaces.

test_that("is_elementwise() admits only row-local expressions", {
  expect_true(is_elementwise(quote(if_else(a > 0, b - 1, b + 1))))
  expect_true(is_elementwise(quote(case_when(a ~ 1, TRUE ~ 2))))
  expect_true(is_elementwise(quote(dplyr::coalesce(x, 0))))
  expect_false(is_elementwise(quote(sample(x, 1))))
  expect_false(is_elementwise(quote(x[1])))
  expect_false(is_elementwise(quote(runif(n()))))
  expect_false(is_elementwise(quote(sum(x))))
})

test_that("an elementwise rule under a match equals its per-pair evaluation", {
  m <- abm_setup(agents = abm_agents(n = 20, money = ~runif(n)), seed = 1)
  fast <- abm_go(abm_match(pair = "random"),
                 abm_rules(money ~ if_else(partner_money > money, money - 1, money)))
  slow <- abm_go(abm_match(pair = "random"),
                 abm_rules(money ~ identity(if_else(partner_money > money, money - 1, money))))
  expect_equal(abm_run(m, fast, ticks = 5, seed = 2)$money,
               abm_run(m, slow, ticks = 5, seed = 2)$money)
})

test_that("skipping the mask keeps n() and the wrong-length error", {
  m <- abm_setup(agents = abm_agents(n = 7, x = 0))
  r <- abm_run(m, abm_go(abm_rules(x ~ n())), ticks = 1)
  expect_equal(r$x[r$tick == 1], rep(7, 7))
  expect_error(abm_run(m, abm_go(abm_death(when = c(TRUE, FALSE, TRUE))), ticks = 1))
})

test_that("rowsum aggregates agree with summarise(), NA included", {
  edges <- data.frame(from = c(1, 1, 2, 3), to = c(2, 3, 3, 4))
  m <- abm_setup(agents = abm_agents(n = 5, v = c(1L, NA, 3L, 4L, 5L),
                                     b = c(TRUE, NA, FALSE, TRUE, TRUE)),
                 network = abm_network(type = "manual", edges = edges))
  go <- abm_go(abm_neighbours(
    s1 ~ sum(v),  s2 ~ identity(sum(v)),
    m1 ~ mean(b), m2 ~ identity(mean(b)),
    a1 ~ any(b),  a2 ~ identity(any(b)),
    l1 ~ all(b),  l2 ~ identity(all(b)),
    n1 ~ n(),     n2 ~ identity(n())))
  r <- abm_run(m, go, ticks = 1)
  r <- r[r$tick == 1, ]
  expect_identical(r$s1, r$s2)
  expect_identical(r$m1, r$m2)
  expect_identical(r$a1, r$a2)
  expect_identical(r$l1, r$l2)
  # `n1` is the bare count and is nought for the isolated agent, `n2` is not
  # a bare count and stays NA; everywhere else the two routes must agree
  has_nb <- !is.na(r$n2)
  expect_identical(r$n1[has_nb], r$n2[has_nb])
  # agent 5 has no neighbours: only the bare count is nought there
  expect_true(is.na(r$s1[5]))
  expect_equal(r$n1[5], 0L)
  expect_true(is.na(r$a1[5]))
})

test_that("only the bare count is nought over an empty neighbourhood", {
  m <- abm_setup(agents = abm_agents(n = 3, x = c(1L, 2L, 3L)),
                 network = abm_network(type = "manual",
                                       edges = data.frame(from = 1, to = 2)))
  r <- abm_run(m, abm_go(abm_neighbours(
    k ~ n(), s ~ sum(x), mu ~ mean(x),
    # the same shapes past the rowsum fast path, so both routes are checked
    s2 ~ identity(sum(x)), mu2 ~ identity(mean(x)))), ticks = 1)
  r <- r[r$tick == 1, ]
  expect_identical(r$s, r$s2)
  expect_identical(r$mu, r$mu2)
  expect_equal(r$k[3], 0L)
  # a sum over a one-agent neighbourhood is how a model looks a pointed-at
  # agent up, and NA is how it asks whether there is one -- so sum keeps NA
  expect_true(is.na(r$s[3]))
  expect_true(is.na(r$mu[3]))
})

test_that("a count is only nought when the rule is exactly n()", {
  m <- abm_setup(agents = abm_agents(n = 3, x = 1L),
                 network = abm_network(type = "manual",
                                       edges = data.frame(from = 1, to = 2)))
  r <- abm_run(m, abm_go(abm_neighbours(a ~ n(), b ~ dplyr::n(),
                                        c ~ n() + 0L, d ~ sum(x) / n())), ticks = 1)
  r <- r[r$tick == 1, ]
  expect_equal(r$a[3], 0L)
  expect_equal(r$b[3], 0L)
  # inside a larger expression the arithmetic is the model s to make, not ours
  expect_true(is.na(r$c[3]))
  expect_true(is.na(r$d[3]))
})

test_that("a .where neighbour that does not exist is still NA", {
  # `.where` names one neighbour rather than a set, so a bounded edge is
  # missing, not empty
  m <- abm_setup(agents = abm_agents(v = ~seq_len(n)),
                 network = abm_network(type = "line", dims = 4, torus = FALSE))
  r <- abm_run(m, abm_go(abm_neighbours(w ~ sum(v), .where = "west")), ticks = 1)
  expect_equal(r$w[r$tick == 1], c(NA, 1, 2, 3))
})
