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

test_that("links is a count, and only means something with attach_via", {
  withr::local_seed(1016)
  expect_error(abm_birth(when = x > 1, links = 2), class = "tidyABM_missing_arg")
  expect_error(
    abm_birth(when = x > 1, links = 0,
              attach_via = abm_match(pair = "network", from = "parent")),
    class = "tidyABM_bad_links")
  expect_error(
    abm_birth(when = x > 1, links = c(2, 3),
              attach_via = abm_match(pair = "network", from = "parent")),
    class = "tidyABM_bad_links")
})

test_that("links puts a newborn in its parent's neighbourhood", {
  withr::local_seed(1017)
  m <- abm_setup(agents = abm_agents(n = 20, x = 1),
                 network = abm_network(type = "ring", degree = 6))
  r <- abm_run(m, abm_go(abm_birth(
    when = .id == 1, links = 4,
    attach_via = abm_match(pair = "network", from = "parent"))),
    ticks = 1, seed = 1)

  e <- abm_edges(r)
  kid <- max(r$.id)
  kid_nb <- c(e$to[e$from == kid], e$from[e$to == kid])
  par_nb <- c(e$to[e$from == 1 & e$to != kid], e$from[e$to == 1 & e$from != kid])

  expect_length(kid_nb, 4)
  expect_true(1 %in% kid_nb)                        # the parent
  expect_true(all(setdiff(kid_nb, 1) %in% par_nb))  # and the parent's own
  expect_false(kid %in% kid_nb)
})

test_that("links defaults to the single edge it always made", {
  withr::local_seed(1018)
  m <- abm_setup(agents = abm_agents(n = 20, x = 1),
                 network = abm_network(type = "ring", degree = 6))
  go <- function(...) abm_go(abm_birth(
    when = .id == 1, ...,
    attach_via = abm_match(pair = "network", from = "parent")))
  e <- abm_edges(abm_run(m, go(), ticks = 1, seed = 1))
  kid <- 21L
  expect_equal(sum(e$from == kid | e$to == kid), 1)
  # and asking for one explicitly is the same run
  expect_equal(e, abm_edges(abm_run(m, go(links = 1), ticks = 1, seed = 1)))
})

test_that("a newborn does not count its own cohort as a neighbourhood", {
  withr::local_seed(1019)
  # every agent reproduces at once, so each newborn's siblings are on offer
  m <- abm_setup(agents = abm_agents(n = 8, x = 1),
                 network = abm_network(type = "complete"))
  r <- abm_run(m, abm_go(abm_birth(
    when = TRUE, links = 3,
    attach_via = abm_match(pair = "network", from = "parent"))),
    ticks = 1, seed = 2)
  e <- abm_edges(r)
  newborns <- setdiff(r$.id[r$tick == 1], 1:8)
  new_edges <- e[e$from %in% newborns, ]
  expect_true(all(new_edges$to %in% 1:8))
})

test_that("links gives preferential attachment m edges per node", {
  withr::local_seed(1020)
  m <- abm_setup(agents = abm_agents(n = 4),
                 network = abm_network(type = "complete"))
  r <- abm_run(m, abm_go(abm_birth(
    n = 1, links = 3,
    attach_via = abm_match(pair = "network", from = "random_edge"))),
    ticks = 50, seed = 1)
  e <- abm_edges(r)
  expect_equal(nrow(e), 6 + 50 * 3)
  deg <- as.integer(table(c(e$from, e$to)))
  expect_gt(max(deg), 5 * stats::median(deg))
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
