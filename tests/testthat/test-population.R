test_that("abm_birth clones agents that satisfy `when` and applies the cost", {
  withr::local_seed(1001)
  m <- abm_setup(agents = abm_agents(n = 4, resource = ~c(30, 5, 5, 5)))
  r <- abm_run(m, abm_go(abm_birth(when = resource > 20,
                                   cost = resource ~ resource / 2)),
               ticks = 1, seed = 1)
  last <- r[r$tick == 1, ]
  expect_equal(nrow(last), 5)
  expect_equal(sum(last$resource == 15), 2)   # parent and child split the 30
  expect_equal(sum(last$resource), 45)        # nothing created or destroyed
})

test_that("newborns get fresh ids", {
  withr::local_seed(1002)
  m <- abm_setup(agents = abm_agents(n = 3, x = 1))
  r <- abm_run(m, abm_go(abm_birth(when = x == 1)), ticks = 1, seed = 1)
  expect_equal(sort(unique(r$.id)), 1:6)
})

test_that("abm_death removes agents and prunes their edges", {
  withr::local_seed(1003)
  m <- abm_setup(agents = abm_agents(n = 20, hp = ~seq_len(n)),
                 network = abm_network(type = "random", degree = 2))
  r <- abm_run(m, abm_go(abm_death(when = hp < 5)), ticks = 1, seed = 1)
  expect_equal(nrow(r[r$tick == 1, ]), 16)
  e <- abm_edges(r)
  expect_equal(sum(e$from < 5 | e$to < 5), 0)
})

test_that("prune_edges = FALSE keeps the edges", {
  withr::local_seed(1004)
  m <- abm_setup(agents = abm_agents(n = 20, hp = ~seq_len(n)),
                 network = abm_network(type = "random", degree = 2))
  r <- abm_run(m, abm_go(abm_death(when = hp < 5, prune_edges = FALSE)),
               ticks = 1, seed = 1)
  expect_equal(nrow(abm_edges(r)), 20)
})

test_that("abm_birth needs exactly one of when/n", {
  withr::local_seed(1005)
  expect_error(abm_birth(), class = "tidyABM_missing_arg")
  expect_error(abm_birth(when = x > 1, n = 2), class = "tidyABM_conflicting_args")
})

test_that("attach_via must be a network match", {
  withr::local_seed(1006)
  expect_error(abm_birth(n = 1, attach_via = abm_match(pair = "random")),
               class = "tidyABM_bad_attach")
})

test_that("preferential attachment produces a skewed degree distribution", {
  withr::local_seed(1007)
  m <- abm_setup(agents = abm_agents(n = 2),
                 network = abm_network(type = "manual",
                                       edges = data.frame(from = 1, to = 2)))
  r <- abm_run(m, abm_go(abm_birth(
    n = 1, attach_via = abm_match(pair = "network", from = "random_edge"))),
    ticks = 200, seed = 1)
  e <- abm_edges(r)
  expect_equal(nrow(e), 201)
  deg <- as.integer(table(c(e$from, e$to)))
  # rich-get-richer: the busiest node should be far above the median
  expect_gt(max(deg), 5 * stats::median(deg))
})
