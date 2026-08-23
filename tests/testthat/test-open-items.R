# The seven gaps closed in the sixth round -------------------------------

test_that("within = gives a neighbourhood in attribute space", {
  pop <- abm_setup(agents = abm_agents(n = 200, opinion = ~runif(n)),
                   globals = list(eps = 0.2), seed = 42)
  new <- abm_run(pop,
                 abm_go(abm_neighbours(opinion ~ mean(opinion),
                                       within = abs(opinion - own_opinion) <= eps)),
                 ticks = 20)
  # the hand-rolled O(n^2) vapply this replaces
  old <- abm_run(pop,
                 abm_go(abm_rules(opinion ~ vapply(
                   opinion,
                   function(x) mean(opinion[abs(opinion - x) <= eps]),
                   numeric(1)))),
                 ticks = 20)
  expect_equal(new$opinion, old$opinion)
})

test_that("within = needs no network, and the focal agent is inside its own", {
  pop <- abm_setup(agents = abm_agents(n = 10, x = 1:10), seed = 1)
  r <- abm_run(pop, abm_go(abm_neighbours(near ~ dplyr::n(), within = x == own_x)),
               ticks = 1)
  expect_true(all(dplyr::filter(r, tick == 1)$near == 1))

  r2 <- abm_run(pop, abm_go(abm_neighbours(
    near ~ dplyr::n(), within = x == own_x & .id != own_.id)), ticks = 1)
  expect_true(all(is.na(dplyr::filter(r2, tick == 1)$near)))
})

test_that("bounded confidence fragments below its critical eps", {
  clusters <- function(eps) {
    pop <- abm_setup(agents = abm_agents(n = 300, opinion = ~runif(n)),
                     globals = list(eps = eps), seed = 42)
    r <- abm_run(pop, abm_go(abm_neighbours(
      opinion ~ mean(opinion), within = abs(opinion - own_opinion) <= eps)),
      ticks = 40)
    length(unique(round(dplyr::filter(r, tick == 40)$opinion, 3)))
  }
  expect_equal(clusters(0.30), 1)
  expect_gt(clusters(0.05), 3)
})

test_that("abm_draw() hands both endpoints the same number", {
  m <- abm_setup(agents = abm_agents(n = 40, met = 0L),
                 network = abm_network(type = "poisson", degree = 6), seed = 3)
  r <- abm_run(m, abm_go(abm_draw(happened ~ runif(n()) < 0.4),
                         abm_neighbours(met ~ sum(happened))),
               ticks = 3, seed = 1)
  last <- dplyr::filter(r, tick == 3)
  # every meeting is counted by both of its participants, so the total is even
  expect_equal(sum(last$met, na.rm = TRUE) %% 2, 0)
})

test_that("endpoint draws make two neighbourhood passes describe one event", {
  m <- abm_setup(
    agents = abm_agents(n = 30, seen = ~runif(n), veng = ~runif(n),
                        acts = 0L, suffered = 0L, defected = TRUE),
    network = abm_network(type = "complete"), seed = 5)
  go <- abm_go(
    abm_draw(saw ~ runif(n()), hit ~ runif(n()), .each = "endpoint"),
    abm_neighbours(acts     ~ sum(defected & saw < seen & hit < own_veng)),
    abm_neighbours(suffered ~ sum(own_defected & saw_back < own_seen &
                                    hit_back < veng))
  )
  last <- dplyr::filter(abm_run(m, go, ticks = 1, seed = 2), tick == 1)
  # what I did to you, summed, is what was done to you, summed
  expect_equal(sum(last$acts), sum(last$suffered))
})

test_that("abm_draw() refuses to shadow an agent column", {
  m <- abm_setup(agents = abm_agents(n = 5, x = 1),
                 network = abm_network(type = "complete"))
  expect_error(
    abm_run(m, abm_go(abm_draw(x ~ runif(n())), abm_rules(x ~ x)), ticks = 1),
    class = "tidyABM_draw_collision"
  )
})

test_that("abm_draw() needs a network", {
  m <- abm_setup(agents = abm_agents(n = 5, x = 1))
  expect_error(
    abm_run(m, abm_go(abm_draw(u ~ runif(n())), abm_rules(x ~ x)), ticks = 1),
    class = "tidyABM_no_network"
  )
})

test_that(".by turns a global into a table indexed by a category", {
  m <- abm_setup(agents = abm_agents(n = 6, kind = rep(c("a", "b"), 3)),
                 globals = list(cnt = c(a = 0, b = 0)), seed = 1)
  r <- abm_run(m, abm_go(abm_global(cnt ~ sum(kind == .key), .by = kind)),
               ticks = 1)
  expect_equal(abm_globals(r)$cnt[[2]], c(a = 3, b = 3))
})

test_that("a declared index updates keys no agent currently holds", {
  m <- abm_setup(agents = abm_agents(n = 4, task = c(1L, 1L, 2L, 2L)),
                 globals = list(s = c("1" = 0, "2" = 0, "3" = 0)), seed = 1)
  r <- abm_run(m, abm_go(abm_global(s ~ s + 1 - sum(task == .key), .by = 1:3)),
               ticks = 1)
  # task 3 has nobody on it, so its stimulus rises unopposed
  expect_equal(abm_globals(r)$s[[2]], c("1" = -1, "2" = -1, "3" = 1))
})

test_that("a keyed global is read back by an ordinary rule", {
  m <- abm_setup(agents = abm_agents(n = 4, good = c("x", "y", "x", "y"),
                                     paid = 0),
                 globals = list(price = c(x = 2, y = 5)), seed = 1)
  r <- abm_run(m, abm_go(abm_rules(paid ~ price[good])), ticks = 1)
  expect_equal(dplyr::filter(r, tick == 1)$paid, c(2, 5, 2, 5))
})

test_that("a keyed global still has to collapse to one value per key", {
  m <- abm_setup(agents = abm_agents(n = 4, kind = c("a", "a", "b", "b")),
                 globals = list(g = c(a = 0, b = 0)))
  expect_error(
    abm_run(m, abm_go(abm_global(g ~ kind, .by = kind)), ticks = 1),
    class = "tidyABM_bad_global"
  )
})

test_that("the response-threshold colony puts delta/alpha on each task", {
  skip_on_cran()
  pop <- abm_setup(agents = abm_agents(n = 100, task = 0L, th1 = 500, th2 = 500),
                   globals = list(s = c("1" = 0, "2" = 0)), seed = 1)
  go <- abm_go(
    abm_rules(task ~ dplyr::if_else(task > 0L & runif(dplyr::n()) < 0.2, 0L, task)),
    abm_rules(f1 ~ task == 0L & runif(dplyr::n()) < s[["1"]]^2 / (s[["1"]]^2 + th1^2),
              f2 ~ task == 0L & runif(dplyr::n()) < s[["2"]]^2 / (s[["2"]]^2 + th2^2)),
    abm_rules(task ~ dplyr::if_else(
      task == 0L,
      dplyr::if_else(f1 & f2, sample(1:2, dplyr::n(), TRUE),
                     dplyr::if_else(f1, 1L, dplyr::if_else(f2, 2L, 0L))),
      task)),
    abm_global(s ~ max(0, s + 1 - 3 * sum(task == .key) / dplyr::n()), .by = 1:2)
  )
  last <- dplyr::filter(abm_run(pop, go, ticks = 2000, seed = 7), tick > 1800)
  expect_equal(mean(last$task == 1), 1 / 3, tolerance = 0.05)
  expect_equal(mean(last$task == 2), 1 / 3, tolerance = 0.05)
})

test_that(".order makes abm_tell's first, last and collect determinate", {
  # four agents queue at agent 5; the counter serves whoever arrived first
  m <- abm_setup(agents = abm_agents(
    n = 5,
    arrived = c(40, 10, 30, 20, NA),
    queueing = c(TRUE, TRUE, TRUE, TRUE, FALSE),
    serving = NA_integer_), seed = 1)
  r <- abm_run(m, abm_go(abm_tell(serving ~ .id, to = 5L, when = queueing,
                                  .resolve = "first", .order = arrived)),
               ticks = 1)
  expect_equal(dplyr::filter(r, tick == 1, .id == 5)$serving, 2L)

  r2 <- abm_run(m, abm_go(abm_tell(serving ~ .id, to = 5L, when = queueing,
                                   .resolve = "last", .order = arrived)),
                ticks = 1)
  expect_equal(dplyr::filter(r2, tick == 1, .id == 5)$serving, 1L)
})

test_that("collect hands the recipient its messages in the order it named", {
  m <- abm_setup(agents = abm_agents(
    n = 5, arrived = c(40, 10, 30, 20, NA),
    queueing = c(TRUE, TRUE, TRUE, TRUE, FALSE),
    queue = ~vector("list", n)), seed = 1)
  r <- abm_run(m, abm_go(abm_tell(queue ~ .id, to = 5L, when = queueing,
                                  .resolve = "collect", .order = arrived)),
               ticks = 1)
  got <- unlist(dplyr::filter(r, tick == 1, .id == 5)$queue[[1]])
  expect_equal(got, c(2L, 4L, 3L, 1L))
})

test_that("an NA order sits the sender out", {
  m <- abm_setup(agents = abm_agents(
    n = 4, arrived = c(3, NA, 1, NA),
    queueing = c(TRUE, TRUE, TRUE, FALSE),
    queue = ~vector("list", n)), seed = 1)
  r <- abm_run(m, abm_go(abm_tell(queue ~ .id, to = 4L, when = queueing,
                                  .resolve = "collect", .order = arrived)),
               ticks = 1)
  expect_equal(unlist(dplyr::filter(r, tick == 1, .id == 4)$queue[[1]]), c(3L, 1L))
})

test_that("times gives a parent more than one offspring", {
  m <- abm_setup(agents = abm_agents(n = 4, mature = c(TRUE, TRUE, FALSE, FALSE),
                                     litter = c(3, 2, 0, 0)), seed = 1)
  r <- abm_run(m, abm_go(abm_birth(when = mature, times = litter)), ticks = 1)
  expect_equal(nrow(dplyr::filter(r, tick == 1)), 9L)
})

test_that("times = 0 and NA are simply no offspring", {
  m <- abm_setup(agents = abm_agents(n = 3, k = c(0, NA, 2)), seed = 1)
  r <- abm_run(m, abm_go(abm_birth(when = TRUE, times = k)), ticks = 1)
  expect_equal(nrow(dplyr::filter(r, tick == 1)), 5L)
})

test_that("each offspring gets its own inherit, so siblings differ", {
  m <- abm_setup(agents = abm_agents(n = 1, trait = 0), seed = 1)
  r <- abm_run(m, abm_go(abm_birth(when = TRUE, times = 5,
                                   inherit = trait ~ runif(dplyr::n()))),
               ticks = 1, seed = 2)
  kids <- dplyr::filter(r, tick == 1, .id > 1)$trait
  expect_length(unique(kids), 5L)
})

test_that("cost is paid once by the parent and shared by every offspring", {
  m <- abm_setup(agents = abm_agents(n = 1, resource = 12), seed = 1)
  r <- abm_run(m, abm_go(abm_birth(when = TRUE, times = 3,
                                   cost = resource ~ resource / 4)), ticks = 1)
  expect_equal(dplyr::filter(r, tick == 1)$resource, rep(3, 4))
})

test_that("times and n are different questions", {
  expect_error(abm_birth(n = 2, times = 3), class = "tidyABM_conflicting_args")
})

test_that("attach_via reaches the right parent when there are several offspring", {
  m <- abm_setup(agents = abm_agents(n = 2, mature = c(TRUE, FALSE)),
                 network = abm_network(type = "manual",
                                       edges = data.frame(from = 1L, to = 2L)),
                 seed = 1)
  r <- abm_run(m, abm_go(abm_birth(
    when = mature, times = 3,
    attach_via = abm_match(pair = "network", from = "parent"))), ticks = 1)
  e <- abm_edges(r)
  expect_setequal(e$to[e$from %in% 3:5], rep(1L, 3))
})

test_that("record thins what abm_run keeps without touching what it computes", {
  m <- abm_setup(agents = abm_agents(n = 10, x = 0), seed = 1)
  go <- abm_go(abm_rules(x ~ x + 1))
  all <- abm_run(m, go, ticks = 10)
  every2 <- abm_run(m, go, ticks = 10, record = 2)
  final <- abm_run(m, go, ticks = 10, record = "final")
  none <- abm_run(m, go, ticks = 10, record = "globals")

  expect_equal(sort(unique(all$tick)), 0:10)
  expect_equal(sort(unique(every2$tick)), c(0L, 2L, 4L, 6L, 8L, 10L))
  expect_equal(unique(final$tick), 10L)
  expect_equal(nrow(none), 0L)
  # the run itself is unchanged: the last tick is the same however much is kept
  expect_equal(dplyr::filter(every2, tick == 10)$x,
               dplyr::filter(all, tick == 10)$x)
  expect_equal(final$x, dplyr::filter(all, tick == 10)$x)
})

test_that("globals are recorded every tick however little else is", {
  m <- abm_setup(agents = abm_agents(n = 5, x = 1), globals = list(total = 0))
  r <- abm_run(m, abm_go(abm_global(total ~ sum(x))), ticks = 6,
               record = "globals")
  expect_equal(nrow(abm_globals(r)), 7L)
  expect_equal(nrow(r), 0L)
})

test_that("a thinned run keeps its columns and its attributes", {
  m <- abm_setup(agents = abm_agents(n = 4, x = 1),
                 network = abm_network(type = "ring", degree = 2))
  r <- abm_run(m, abm_go(abm_match(pair = "network"), abm_rules(x ~ x + 1)),
               ticks = 5, record = "globals")
  expect_named(r, c("tick", ".id", ".group", "x"))
  expect_equal(nrow(abm_edges(r)), 4L)
})

test_that("record has to be something abm_run can act on", {
  m <- abm_setup(agents = abm_agents(n = 2, x = 1))
  expect_error(abm_run(m, abm_go(abm_rules(x ~ x)), ticks = 1, record = "some"),
               class = "tidyABM_bad_record")
  expect_error(abm_run(m, abm_go(abm_rules(x ~ x)), ticks = 1, record = 0),
               class = "tidyABM_bad_record")
})

test_that("abm_sequential widens a column rather than refusing the write", {
  m <- abm_setup(agents = abm_agents(n = 3, x = 1L), seed = 1)
  r <- abm_run(m, abm_go(abm_sequential(x ~ x + 0.5, .order = .id)), ticks = 1)
  expect_equal(dplyr::filter(r, tick == 1)$x, rep(1.5, 3))
})

test_that("an abm_sequential rule must give one value", {
  m <- abm_setup(agents = abm_agents(n = 3, x = 1), seed = 1)
  expect_error(
    abm_run(m, abm_go(abm_sequential(x ~ c(1, 2))), ticks = 1),
    class = "tidyABM_bad_sequential"
  )
})

test_that("abm_sequential creates a column mid-step and later rules see it", {
  m <- abm_setup(agents = abm_agents(n = 4, a = 1:4), seed = 1)
  r <- abm_run(m, abm_go(abm_sequential(b ~ a * 2, c ~ b + 1, .order = .id)),
               ticks = 1)
  last <- dplyr::filter(r, tick == 1)
  expect_equal(last$b, c(2, 4, 6, 8))
  expect_equal(last$c, c(3, 5, 7, 9))
})

test_that("abm_sequential is fast enough to reproduce a bank run", {
  skip_on_cran()
  m <- abm_setup(
    agents = abm_agents(n = 200, pos = ~sample(n), theta = ~runif(n),
                        belief = 0, ran = FALSE, paid = 0, dry = FALSE),
    globals = list(till = 100, empty = 0), seed = 1)
  go <- abm_go(
    abm_global(till ~ 0.5 * dplyr::n()),
    abm_sequential(
      ran    ~ runif(1) < 0.44 | belief > theta,
      paid   ~ dplyr::if_else(ran, pmin(1.2, till), 0),
      till   ~ till - paid,
      dry    ~ till < 1.2,
      belief ~ 0.8 * belief + 0.2 * as.numeric(dry),
      .order = pos),
    abm_global(empty ~ mean(ran))
  )
  elapsed <- system.time(r <- abm_run(m, go, ticks = 50, seed = 1))[["elapsed"]]
  expect_equal(mean(dplyr::filter(r, tick > 30)$ran), 0.507, tolerance = 0.01)
  # 50,000 agent-rules. The dplyr::mutate()-per-agent version took over a
  # minute; anything near that is a regression, not a slow machine.
  expect_lt(elapsed, 20)
})
