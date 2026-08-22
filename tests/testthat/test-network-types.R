test_that("a ring joins each agent to the ones beside it", {
  m <- abm_setup(agents = abm_agents(n = 8, x = 1),
                 network = abm_network(type = "ring", degree = 2))
  expect_equal(nrow(m$edges), 8L)
  deg <- table(c(m$edges$from, m$edges$to))
  expect_true(all(deg == 2L))

  m4 <- abm_setup(agents = abm_agents(n = 8, x = 1),
                  network = abm_network(type = "ring", degree = 4))
  expect_true(all(table(c(m4$edges$from, m4$edges$to)) == 4L))
  expect_error(abm_network(type = "ring", degree = 3),
               class = "tidyABM_bad_degree")
})

test_that("a poisson graph has the requested mean degree and a spread of them", {
  m <- abm_setup(agents = abm_agents(n = 600, x = 1),
                 network = abm_network(type = "poisson", degree = 4),
                 seed = 1)
  deg <- as.integer(table(factor(c(m$edges$from, m$edges$to),
                                 levels = seq_len(600))))
  expect_equal(mean(deg), 4, tolerance = 0.15)
  # the point of it: unlike "random", the degrees are not all the same
  expect_gt(stats::var(deg), 2)
})

test_that("a scale-free graph has a heavy tail", {
  m <- abm_setup(agents = abm_agents(n = 500, x = 1),
                 network = abm_network(type = "scale_free", degree = 2),
                 seed = 1)
  deg <- as.integer(table(factor(c(m$edges$from, m$edges$to),
                                 levels = seq_len(500))))
  reg <- abm_setup(agents = abm_agents(n = 500, x = 1),
                   network = abm_network(type = "random", degree = 4),
                   seed = 1)
  dreg <- as.integer(table(factor(c(reg$edges$from, reg$edges$to),
                                  levels = seq_len(500))))
  expect_gt(max(deg), 4 * max(dreg) / 3)
})

test_that("the new types still refuse arguments they do not use", {
  expect_error(abm_network(type = "ring", degree = 2, edges = data.frame()),
               class = "tidyABM_irrelevant_arg")
  expect_error(abm_network(type = "poisson"), class = "tidyABM_missing_arg")
})
