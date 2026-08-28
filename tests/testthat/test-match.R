test_that("each mode rejects arguments it does not use", {
  withr::local_seed(1001)
  expect_error(abm_match(pair = "random", by = x), class = "tidyABM_irrelevant_arg")
  expect_error(abm_match(pair = "nearest", by = x, role = list(a = TRUE, b = TRUE)),
               class = "tidyABM_irrelevant_arg")
  expect_error(abm_match(pair = "network", size = 3), class = "tidyABM_irrelevant_arg")
  expect_error(abm_match(pair = "opposite_group", by = g, among = x > 0),
               class = "tidyABM_irrelevant_arg")
})

test_that("`by` is required where the mode needs it", {
  withr::local_seed(1002)
  expect_error(abm_match(pair = "nearest"), class = "tidyABM_missing_arg")
  expect_error(abm_match(pair = "opposite_group"), class = "tidyABM_missing_arg")
})

test_that("size > 2 is refused by the modes that cannot support it", {
  withr::local_seed(1003)
  expect_error(abm_match(pair = "opposite_group", by = g, size = 3),
               class = "tidyABM_bad_size")
  expect_error(abm_match(pair = "nearest", by = x, size = 3),
               class = "tidyABM_bad_size")
  expect_s3_class(abm_match(pair = "random", size = 4), "abm_match")
})

test_that("a match decides who meets whom and writes no agent column", {
  withr::local_seed(1004)
  m <- abm_setup(agents = list(
    a = abm_agents(n = 6, x = 1),
    b = abm_agents(n = 6, y = 2)))
  r <- abm_run(m, abm_go(abm_match(pair = "opposite_group", by = .group),
                         abm_rules(x ~ x + partner_y)),
               ticks = 1, seed = 4)
  expect_setequal(setdiff(names(r), c("tick", ".id", ".group")), c("x", "y"))
})

test_that("role must be a named list of two conditions", {
  withr::local_seed(1005)
  expect_error(abm_match(pair = "random", role = list(a = TRUE)),
               class = "tidyABM_bad_role")
  expect_error(abm_match(pair = "random", role = c(a = 1, b = 2)),
               class = "tidyABM_bad_role")
})

test_that("random pairing partitions the population", {
  withr::local_seed(1006)
  m <- abm_setup(agents = abm_agents(n = 10, x = 1))
  res <- run_match(abm_match(pair = "random"), m$groups$agents, NULL, list())
  mm <- res
  expect_equal(sort(mm$.id), 1:10)
  # partnership is symmetric
  expect_equal(mm$.partner[match(mm$.partner, mm$.id)], mm$.id)
})

test_that("roles are only assigned where both conditions can hold", {
  withr::local_seed(1007)
  m <- abm_setup(agents = abm_agents(n = 10, money = ~c(rep(0, 9), 5)))
  spec <- abm_match(pair = "random",
                    role = list(giver = money > 0, receiver = TRUE))
  res <- run_match(spec, m$groups$agents, NULL, list())
  # only one agent can give, so exactly one pair survives
  expect_equal(nrow(res), 2)
  expect_setequal(res$.role, c("giver", "receiver"))
})

test_that("eligible filters agents out of the step", {
  withr::local_seed(1008)
  m <- abm_setup(agents = abm_agents(n = 10, ok = ~rep(c(TRUE, FALSE), each = 5)))
  res <- run_match(abm_match(pair = "random", eligible = ok),
                   m$groups$agents, NULL, list())
  expect_true(all(res$.id %in% 1:5))
  # an odd eligible pool leaves one agent over
  expect_equal(nrow(res), 4)
})

test_that("network matching only draws partners from a agent's own edges", {
  withr::local_seed(1009)
  edges <- data.frame(from = c(1, 3), to = c(2, 4))
  m <- abm_setup(agents = abm_agents(n = 4, x = 1),
                 network = abm_network(type = "manual", edges = edges))
  res <- run_match(abm_match(pair = "network"), m$groups$agents, m$edges, list())
  pairs <- paste(pmin(res$.id, res$.partner),
                 pmax(res$.id, res$.partner))
  expect_true(all(pairs %in% c("1 2", "3 4")))
})

test_that("a k-regular network gives every agent exactly k neighbours", {
  withr::local_seed(1010)
  m <- abm_setup(agents = abm_agents(n = 20, x = 1),
                 network = abm_network(type = "random", degree = 3))
  deg <- table(c(m$edges$from, m$edges$to))
  expect_true(all(deg == 3))
  expect_length(deg, 20)
})

test_that("impossible k-regular graphs are refused up front", {
  withr::local_seed(1011)
  expect_error(
    abm_setup(agents = abm_agents(n = 5, x = 1),
              network = abm_network(type = "random", degree = 3)),
    class = "tidyABM_bad_degree"
  )
  expect_error(
    abm_setup(agents = abm_agents(n = 3, x = 1),
              network = abm_network(type = "random", degree = 5)),
    class = "tidyABM_bad_degree"
  )
})

test_that("network specs reject arguments the type does not use", {
  withr::local_seed(1012)
  expect_error(abm_network("random", degree = 2, edges = data.frame(from = 1, to = 2)),
               class = "tidyABM_irrelevant_arg")
  expect_error(abm_network("manual", degree = 2), class = "tidyABM_irrelevant_arg")
  expect_error(abm_network("manual"), class = "tidyABM_missing_arg")
})
