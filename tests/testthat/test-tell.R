test_that("abm_tell writes into the partner's row, not the sender's", {
  m <- abm_setup(agents = abm_agents(n = 6, tag = 0L))
  r <- abm_run(m, abm_go(abm_match(pair = "random"),
                         abm_tell(tag ~ .id, to = .partner)),
               ticks = 1, seed = 2)
  final <- r[r$tick == 1, ]
  # everyone was told by exactly one other agent, and never by themselves
  expect_setequal(final$tag, final$.id)
  expect_true(all(final$tag != final$.id))
})

test_that("abm_tell to neighbours reaches every neighbour", {
  m <- abm_setup(agents = abm_agents(n = 5, load = 0,
                                     shouting = ~seq_len(n) <= 2L),
                 network = abm_network(type = "complete"))
  r <- abm_run(m, abm_go(abm_tell(load ~ 1, to = "neighbours",
                                  when = shouting, .resolve = "sum")),
               ticks = 1, seed = 1)
  final <- r[r$tick == 1, ]
  # two shouters: the quiet three heard both, each shouter heard the other
  expect_equal(final$load[final$shouting], c(1, 1))
  expect_equal(final$load[!final$shouting], c(2, 2, 2))
})

test_that(".resolve decides what happens when two senders collide", {
  build <- function(resolve) {
    m <- abm_setup(agents = abm_agents(n = 3, load = 0,
                                       shouting = ~seq_len(n) <= 2L),
                   network = abm_network(type = "complete"))
    r <- abm_run(m, abm_go(abm_tell(load ~ .id, to = "neighbours",
                                    when = shouting, .resolve = resolve)),
                 ticks = 1, seed = 1)
    r$load[r$tick == 1 & r$.id == 3L]
  }
  expect_equal(build("sum"), 3)
  expect_equal(build("max"), 2)
  expect_equal(build("min"), 1)
  expect_error(build("error"), class = "tidyABM_tell_collision")
})

test_that("agents nobody wrote to keep what they had", {
  m <- abm_setup(agents = abm_agents(n = 4, tag = -1L, speaks = ~seq_len(n) == 1L))
  r <- abm_run(m, abm_go(abm_match(pair = "one_of"),
                         abm_tell(tag ~ 99L, to = .partner, when = speaks)),
               ticks = 1, seed = 1)
  final <- r[r$tick == 1, ]
  expect_equal(sum(final$tag == 99L), 1L)
  expect_equal(sum(final$tag == -1L), 3L)
})

test_that("abm_tell rejects a target that is not an agent or a column", {
  m <- abm_setup(agents = abm_agents(n = 3, tag = 0L),
                 globals = list(who = 99L))
  expect_error(
    abm_run(m, abm_go(abm_tell(tag ~ 1L, to = who)), ticks = 1, seed = 1),
    class = "tidyABM_bad_recipient"
  )
  m2 <- abm_setup(agents = abm_agents(n = 3, tag = 0L))
  expect_error(
    abm_run(m2, abm_go(abm_match(pair = "random"),
                       abm_tell(brand_new ~ 1L, to = .partner)),
            ticks = 1, seed = 1),
    class = "tidyABM_tell_new_column"
  )
  expect_error(abm_tell(tag ~ 1L, to = "everyone"), class = "tidyABM_bad_to")
  expect_error(abm_tell(tag ~ 1L), class = "tidyABM_missing_arg")
})

test_that("abm_tell to neighbours needs a network", {
  m <- abm_setup(agents = abm_agents(n = 3, tag = 0L))
  expect_error(
    abm_run(m, abm_go(abm_tell(tag ~ 1L, to = "neighbours")), ticks = 1,
            seed = 1),
    class = "tidyABM_no_network"
  )
})
