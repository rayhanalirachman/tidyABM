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

test_that("a per-chooser among reads a set-valued column with %in%", {
  # `own_sellers` reaches a per-pair `among` as a list column. Base `%in%`
  # coerces that with as.character() and returns FALSE for every row, so this
  # used to be a match that paired nobody, silently.
  m <- abm_setup(agents = list(
    hh = abm_agents(n = 3, pick = NA_integer_,
                    sellers = ~lapply(seq_len(n), function(i) c(4L, 5L))),
    f  = abm_agents(n = 3, price = 1)), seed = 1)

  picked <- function(among) {
    r <- abm_run(m, abm_go(abm_match(pair = "one_of", eligible = .group == "hh",
                                     among = !!among),
                           abm_rules(pick ~ .partner, .scope = "population")),
                 ticks = 1, seed = 1)
    r$pick[r$tick == 1 & r$.group == "hh"]
  }

  expect_true(all(picked(rlang::quo(.group == "f" & .id %in% own_sellers))
                  %in% c(4L, 5L)))
  expect_equal(picked(rlang::quo(.group == "f" & !.id %in% own_sellers)),
               rep(6L, 3))
})

test_that("%in% over an atomic column is unchanged by the list-aware form", {
  expect_equal(in_rowwise(c(1L, 2L, 7L), c(1L, 2L, 3L)), c(TRUE, TRUE, FALSE))
  expect_equal(in_rowwise(character(0), letters), logical(0))
  expect_equal(in_rowwise(NA_integer_, 1:3), FALSE)
  # a list right-hand side is asked row by row, and recycles a scalar left side
  expect_equal(in_rowwise(c(1L, 9L), list(1:3, 1:3)), c(TRUE, FALSE))
  expect_equal(in_rowwise(2L, list(1:3, 5:7)), c(TRUE, FALSE))
  expect_equal(in_rowwise(1L, list(NULL)), FALSE)
})
