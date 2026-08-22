test_that("among restricts who may be picked, eligible who takes part", {
  m <- abm_setup(agents = list(
    shops  = abm_agents(n = 3, x = c(10, 50, 90), custom = 0),
    buyers = abm_agents(n = 9, x = ~seq(0, 100, length.out = n))
  ), seed = 1)
  r <- abm_run(m, abm_go(
    abm_match(pair = "nearest", by = x,
              eligible = .group == "buyers", among = .group == "shops"),
    abm_tell(custom ~ 1, to = .partner, when = .group == "buyers",
             .resolve = "sum")
  ), ticks = 1, seed = 1)
  shops <- r[r$tick == 1 & r$.group == "shops", ]
  expect_equal(shops$custom, c(3, 3, 3))
})

test_that("one_of only ever draws a candidate", {
  m <- abm_setup(agents = abm_agents(n = 10, side = ~rep(c("a", "b"), 5),
                                     got = NA_character_))
  r <- abm_run(m, abm_go(abm_match(pair = "one_of", among = side == "b"),
                         abm_rules(got ~ partner_side)),
               ticks = 1, seed = 1)
  expect_true(all(r$got[r$tick == 1] == "b"))
})

test_that("among is refused by the modes that cannot use it", {
  expect_error(abm_match(pair = "random", among = TRUE),
               class = "tidyABM_irrelevant_arg")
  expect_error(abm_match(pair = "network", among = TRUE),
               class = "tidyABM_irrelevant_arg")
})
