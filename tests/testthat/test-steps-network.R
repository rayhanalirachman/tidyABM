# The steps the second round of models forced into existence.

test_that("abm_link turns a pairing into edges, without duplicates", {
  withr::local_seed(2001)
  m <- abm_setup(agents = abm_agents(n = 10, x = 1),
                 network = abm_network(type = "empty"))
  r <- abm_run(m, abm_go(abm_match(pair = "random"), abm_link()),
               ticks = 1, seed = 1)
  expect_equal(nrow(abm_edges(r)), 5)          # 10 agents, one edge per pair

  # running the same pairing repeatedly cannot double up an edge
  r2 <- abm_run(abm_setup(agents = abm_agents(n = 4, x = 1),
                          network = abm_network(type = "manual",
                                                edges = data.frame(from = c(1, 3),
                                                                   to   = c(2, 4)))),
                abm_go(abm_match(pair = "network"), abm_link()),
                ticks = 5, seed = 1)
  expect_equal(nrow(abm_edges(r2)), 2)
})

test_that("abm_unlink removes exactly the matched edges", {
  withr::local_seed(2002)
  m <- abm_setup(agents = abm_agents(n = 20, x = 1),
                 network = abm_network(type = "random", degree = 2))
  before <- nrow(m$edges)
  r <- abm_run(m, abm_go(abm_match(pair = "network"), abm_unlink()),
               ticks = 1, seed = 1)
  expect_lt(nrow(abm_edges(r)), before)
  expect_gt(nrow(abm_edges(r)), 0)
})

test_that("link and unlink need a match, and a match that paired nobody is a no-op", {
  m <- abm_setup(agents = abm_agents(n = 10, x = 1),
                 network = abm_network(type = "empty"))
  expect_error(abm_run(m, abm_go(abm_link()), ticks = 1),
               class = "tidyABM_no_match")
  # nobody eligible: the step runs and changes nothing
  r <- abm_run(m, abm_go(abm_match(pair = "random", eligible = x > 99), abm_link()),
               ticks = 1, seed = 1)
  expect_equal(nrow(abm_edges(r)), 0)
})

test_that("abm_link refuses a model with no network", {
  m <- abm_setup(agents = abm_agents(n = 10, x = 1))
  expect_error(abm_run(m, abm_go(abm_match(pair = "random"), abm_link()), ticks = 1),
               class = "tidyABM_no_network")
})

test_that("abm_neighbours aggregates over the whole neighbourhood", {
  # a path graph 1-2-3-4: the ends have one neighbour, the middle two have two
  edges <- data.frame(from = c(1, 2, 3), to = c(2, 3, 4))
  m <- abm_setup(agents = abm_agents(n = 4, x = ~c(10, 20, 30, 40)),
                 network = abm_network(type = "manual", edges = edges))
  r <- abm_run(m, abm_go(abm_neighbours(k ~ n(), total ~ sum(x))),
               ticks = 1, seed = 1)
  last <- r[r$tick == 1, ]
  expect_equal(last$k, c(1, 2, 2, 1))
  expect_equal(last$total, c(20, 40, 60, 30))
})

test_that("an agent with no neighbours gets NA", {
  m <- abm_setup(agents = abm_agents(n = 3, x = 1),
                 network = abm_network(type = "manual",
                                       edges = data.frame(from = 1, to = 2)))
  r <- abm_run(m, abm_go(abm_neighbours(k ~ n())), ticks = 1, seed = 1)
  expect_equal(r$k[r$tick == 1], c(1, 1, NA))
})

test_that("abm_neighbours refuses a model with no network", {
  m <- abm_setup(agents = abm_agents(n = 4, x = 1))
  expect_error(abm_run(m, abm_go(abm_neighbours(k ~ n())), ticks = 1),
               class = "tidyABM_no_network")
})

test_that("one_of gives every eligible agent its own partner, itself excluded", {
  withr::local_seed(2003)
  m <- abm_setup(agents = abm_agents(n = 50, x = ~seq_len(n)))
  res <- run_match(abm_match(pair = "one_of"), m$groups$agents, NULL, list())
  mm <- res$match
  expect_equal(sort(mm$.id), 1:50)             # everyone gets one
  expect_false(any(mm$.id == mm$.partner))     # nobody meets themselves
  # ...and it is directional, so partnership is not generally symmetric
  sym <- mm$.partner[match(mm$.partner, mm$.id)] == mm$.id
  expect_false(all(sym))
})

test_that("one_of, unlike random, can leave an odd population fully matched", {
  withr::local_seed(2004)
  m <- abm_setup(agents = abm_agents(n = 7, x = 1))
  a <- run_match(abm_match(pair = "random"), m$groups$agents, NULL, list())
  b <- run_match(abm_match(pair = "one_of"), m$groups$agents, NULL, list())
  expect_equal(nrow(a$match), 6)   # one agent is left over
  expect_equal(nrow(b$match), 7)   # everybody draws somebody
})

test_that(".scope = 'population' ignores a standing match", {
  withr::local_seed(2005)
  m <- abm_setup(agents = abm_agents(n = 100, x = ~seq_len(n)))
  # with match scope, sum(x) is the pair total; with population scope it is the
  # whole population's
  a <- abm_run(m, abm_go(abm_match(pair = "random"), abm_rules(s ~ sum(x))),
               ticks = 1, seed = 1)
  b <- abm_run(m, abm_go(abm_match(pair = "random"),
                         abm_rules(s ~ sum(x), .scope = "population")),
               ticks = 1, seed = 1)
  expect_true(all(a$s[a$tick == 1] < sum(1:100)))
  expect_true(all(b$s[b$tick == 1] == sum(1:100)))
})

test_that("abm_birth inherit applies to the newborn only", {
  withr::local_seed(2006)
  m <- abm_setup(agents = abm_agents(n = 3, age = 7, trait = 1))
  r <- abm_run(m, abm_go(abm_birth(when = age == 7,
                                   inherit = list(age ~ 0, trait ~ trait * 2))),
               ticks = 1, seed = 1)
  last <- r[r$tick == 1, ]
  expect_equal(sort(last$age), c(0, 0, 0, 7, 7, 7))
  expect_equal(sort(last$trait), c(1, 1, 1, 2, 2, 2))
})

test_that("births and deaths can see the standing match", {
  withr::local_seed(2007)
  # only agents paired with a bigger partner die
  m <- abm_setup(agents = abm_agents(n = 100, size = ~seq_len(n)))
  r <- abm_run(m, abm_go(abm_match(pair = "random"),
                         abm_death(when = partner_size > size)),
               ticks = 1, seed = 1)
  expect_equal(nrow(r[r$tick == 1, ]), 50)

  # ...and two-parent inheritance works through partner_ columns
  m2 <- abm_setup(agents = abm_agents(n = 100, v = ~rep(c(0, 10), 50)))
  r2 <- abm_run(m2, abm_go(
    abm_match(pair = "opposite_group", by = v),
    abm_birth(when = v == 0, inherit = v ~ (v + partner_v) / 2)),
    ticks = 1, seed = 1)
  expect_equal(sum(r2$v[r2$tick == 1] == 5), 50)   # one child per couple
})

test_that("opposite_group tolerates a population that is briefly all one kind", {
  withr::local_seed(2008)
  m <- abm_setup(agents = abm_agents(n = 10, sex = "female"))
  r <- abm_run(m, abm_go(abm_match(pair = "opposite_group", by = sex),
                         abm_rules(mated ~ !is.na(.partner))),
               ticks = 1, seed = 1)
  expect_true(all(!r$mated[r$tick == 1]))
  # three values is still a specification error
  m3 <- abm_setup(agents = abm_agents(n = 9, k = ~rep(c("a", "b", "c"), 3)))
  expect_error(abm_run(m3, abm_go(abm_match(pair = "opposite_group", by = k),
                                  abm_rules(z ~ 1)), ticks = 1),
               class = "tidyABM_bad_by")
})
