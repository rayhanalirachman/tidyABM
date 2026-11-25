# The Rust-driven tick loop (full-Rust stages 1-2, R/run-loop.R and
# src/rust/src/runloop.rs): native abm_rules()/abm_global() steps, R steps called
# back, declines replayed on the R path, errors reported as the R engine does.

loop_model <- function() {
  abm_setup(agents = list(a = abm_agents(n = 40, x = ~runif(n), y = 1L, s = "p"),
                          b = abm_agents(n = 25, x = ~runif(n))),
            globals = list(k = 2, tot = 0), seed = 3)
}

loop_stats <- function() {
  s <- .rl_last$stats
  stats::setNames(s, c("native", "declined", "compiled", "r_calls"))
}

test_that("native rules and globals run in the loop and agree with the R engine", {
  go <- abm_go(abm_rules(x ~ x * k + runif(n()), z ~ x > 0.5),
               abm_global(tot ~ sum(x), cnt ~ mean(x)),
               abm_rules(x ~ if_else(z, x - 1, x), .scope = "population"),
               abm_global(tot ~ tot + n()))
  expect_engines_agree(loop_model(), go, ticks = 15, seed = 1, what = "loop")
  run_in("rust", loop_model(), go, ticks = 15, seed = 1)
  st <- loop_stats()
  expect_equal(st[["native"]], 4 * 15)
  expect_equal(st[["declined"]], 0)
  expect_equal(st[["r_calls"]], 0)
})

test_that("R steps between native steps are called back, record and measures as R", {
  m <- loop_model()
  go <- abm_go(abm_rules(x ~ x + 1),
               abm_rules(y ~ y + length(x)),               # not compiled: R
               abm_death(when = x > 30),
               abm_global(tot ~ sum(x)))
  for (rec in list("all", 3L, "final", "globals")) {
    expect_engines_agree(m, go, ticks = 12, seed = 2, record = rec,
                         measures = list(mx = ~max(x)), checks = list(ok = ~tot >= 0),
                         what = paste("mixed, record", rec))
  }
  run_in("rust", m, go, ticks = 12, seed = 2)
  expect_gt(loop_stats()[["r_calls"]], 0)
})

test_that("a declined drawing step is replayed from the saved .Random.seed", {
  m <- abm_setup(agents = abm_agents(n = 30, x = ~runif(n), w = 1L), seed = 4)
  # integer overflow on the third tick: the evaluator declines after drawing
  go <- abm_go(abm_rules(x ~ x + runif(n()), w = w ~ w * 40000L))
  expect_warning(ref <- run_in("r", m, go, ticks = 4, seed = 7), "NA")
  withr::local_seed(1)
  expect_warning(got <- run_in("rust", m, go, ticks = 4, seed = 7), "NA")
  expect_identical(got, ref)
  expect_gt(loop_stats()[["declined"]], 0)
  withr::local_seed(1)
  run_in("r", m, go, ticks = 4, seed = NULL) |> suppressWarnings()
  s_r <- rng_get()
  withr::local_seed(1)
  run_in("rust", m, go, ticks = 4, seed = NULL) |> suppressWarnings()
  expect_identical(rng_get(), s_r)
})

test_that("shapes the loop leaves to R give the R engine's result", {
  m <- loop_model()
  here <- 3
  cases <- list(
    scope   = abm_go(abm_rules(x ~ x + here)),
    match   = abm_go(abm_match(pair = "random"), abm_rules(x ~ x + 1)),
    attrs   = abm_go(abm_rules(x ~ x + 1), abm_rules(x ~ structure(x, u = 1)),
                     abm_rules(x ~ x * 2)),
    by      = abm_go(abm_rules(x ~ mean(x), .by = s)),
    newcol  = abm_go(abm_rules(v ~ x * 2), abm_rules(v ~ v + 1), abm_global(g ~ sum(v)))
  )
  for (nm in names(cases)) {
    expect_engines_agree(m, cases[[nm]], ticks = 5, seed = 3, what = nm)
  }
})

test_that("errors inside the loop are reported as the R engine reports them", {
  m <- loop_model()
  bad <- abm_go(abm_rules(x ~ x + 1), abm_rules(y ~ monney + 1))
  e_r <- tryCatch(run_in("r", m, bad, ticks = 3, seed = 1), error = identity)
  e_s <- tryCatch(run_in("rust", m, bad, ticks = 3, seed = 1), error = identity)
  expect_identical(class(e_s), class(e_r))
  expect_identical(conditionMessage(e_s), conditionMessage(e_r))
  chk <- list(small = ~tot < 5)
  go <- abm_go(abm_global(tot ~ tot + 1))
  e_r <- tryCatch(run_in("r", m, go, ticks = 9, seed = 1, checks = chk), error = identity)
  e_s <- tryCatch(run_in("rust", m, go, ticks = 9, seed = 1, checks = chk), error = identity)
  expect_s3_class(e_s, "tidyABM_check_failed")
  expect_identical(conditionMessage(e_s), conditionMessage(e_r))
  expect_identical(e_s$tick, e_r$tick)
  # a warning raised by an R step reaches the user once
  noisy <- abm_go(abm_rules(x ~ x + 1), abm_rules(y ~ {warning("careful"); y}))
  expect_warning(run_in("rust", m, noisy, ticks = 1, seed = 1), "careful")
})

test_that("several runs and a world per run agree", {
  m <- loop_model()
  go <- abm_go(abm_rules(x ~ x * k), abm_global(tot ~ sum(x)))
  expect_engines_agree(m, go, ticks = 4, params = list(k = c(0.5, 2)), reps = 2,
                       seed = 5, what = "sweep")
  world <- function(k) abm_setup(agents = abm_agents(n = 10, x = ~runif(n)),
                                 globals = list(k = k, tot = 0))
  expect_engines_agree(world, go, ticks = 4, params = list(k = c(1, 3)), seed = 6,
                       what = "world per run")
})

test_that("network neighbours fold natively over several groups, by id", {
  m <- abm_setup(agents = list(cells = abm_agents(x = ~runif(n)),
                               movers = abm_agents(n = 5, x = ~runif(n))),
                 network = abm_network("grid", dims = c(4, 4), on = "cells", torus = TRUE),
                 seed = 9)
  go <- abm_go(abm_neighbours(avg ~ mean(x), tot ~ sum(x)),
               abm_rules(x ~ if_else(is.na(avg), x, (x + avg) / 2)))
  expect_engines_agree(m, go, ticks = 4, seed = 2, what = "two groups, grid on one")
  run_in("rust", m, go, ticks = 4, seed = 2)
  expect_identical(.rl_last$stats[[2]], 0L)
})

test_that("ticks logged natively (FR9) give R's globals table", {
  m <- abm_setup(agents = abm_agents(n = 20, x = ~runif(n), g = ~sample(1:3, n, TRUE)),
                 globals = list(tot = 0, v = c(1, 2)), seed = 8)
  go <- abm_go(abm_global(tot ~ tot + sum(x)),
               abm_global(by_g ~ sum(x), .by = g),           # a named vector: list cells
               abm_rules(x ~ x * 0.9 + runif(n())))
  for (rec in list("final", "globals", 4L, "all")) {
    expect_engines_agree(m, go, ticks = 9, seed = 2, record = rec, what = paste("FR9", rec))
  }
  later <- abm_go(abm_rules(x ~ x + 1), abm_global(newg ~ if_else(sum(x) > 30, 1, 0)))
  expect_engines_agree(m, later, ticks = 6, seed = 1, record = "final", what = "FR9 later")
})

test_that("native steps keep every new R object protected (gctorture)", {
  # ~3 minutes: opt-in (TIDYABM_GCTORTURE=run); found a column left unprotected
  # in the neighbour fold's commit, which freed it now and then under load
  skip_if_not(Sys.getenv("TIDYABM_GCTORTURE") == "run", "gctorture test is opt-in")
  m <- abm_setup(agents = list(a = abm_agents(n = 6, x = ~runif(n), k = ~sample(1:2, n, TRUE)),
                               b = abm_agents(n = 3, x = ~runif(n), k = 1L)),
                 network = abm_network(type = "ring", degree = 2), globals = list(t = 0), seed = 1)
  w <- 2
  go <- abm_go(abm_neighbours(s ~ mean(x), c ~ sum(k)),
               abm_rules(x ~ x * 0.5 + runif(n()) * w, y ~ vapply(x, function(v) v, 0)),
               abm_rules(x ~ x - mean(x), .by = k),
               abm_global(t ~ t + sum(x)))
  ref <- run_in("r", m, go, ticks = 2, seed = 3)
  gctorture(TRUE)
  got <- tryCatch(run_in("rust", m, go, ticks = 2, seed = 3), error = identity)
  gctorture(FALSE)
  expect_identical(got, ref)
  # FR4b: `within` lookups and `.where` neighbours (their own commit paths)
  wm <- abm_setup(agents = list(patches = abm_agents(grass = ~runif(n) < 0.5, food = ~runif(n)),
                               sheep = abm_agents(n = 4, e = 1)),
                 network = abm_network(type = "grid", dims = c(3, 3), on = "patches"), seed = 2)
  go2 <- abm_go(abm_move(along = "patches", to = "random_neighbour", who = "sheep"),
                abm_neighbours(here ~ any(grass), f ~ sum(food), c ~ n(),
                               within = .group == "patches" & .id == own_.cell),
                abm_neighbours(nf ~ sum(food), .where = "north"))
  ref2 <- run_in("r", wm, go2, ticks = 2, seed = 3)
  gctorture(TRUE)
  got2 <- tryCatch(run_in("rust", wm, go2, ticks = 2, seed = 3), error = identity)
  gctorture(FALSE)
  expect_identical(got2, ref2)
  # FR9b: native measures/checks and the stacked recorded ticks
  ms <- list(mx = ~max(x), s = ~sum(x))
  ref3 <- run_in("r", m, go, ticks = 2, seed = 3, record = "all", measures = ms,
                 checks = list(ok = ~all(x > -1e9)))
  gctorture(TRUE)
  got3 <- tryCatch(run_in("rust", m, go, ticks = 2, seed = 3, record = "all", measures = ms,
                          checks = list(ok = ~all(x > -1e9))), error = identity)
  gctorture(FALSE)
  expect_identical(got3, ref3)
  # FR5/FR6: native match (with roles), rules reading the match, death, birth
  mb <- abm_setup(agents = list(a = abm_agents(n = 7, e = ~runif(n, 1, 5), s = ~sample(c("u", "v"), n, TRUE)),
                                b = abm_agents(n = 3, e = 2, s = "u")), seed = 3)
  go4 <- abm_go(abm_match(pair = "random", role = list(g = e > 2, r = TRUE)),
                abm_rules(e ~ e + coalesce(partner_e, 0) / 10, k ~ sum(e)),
                abm_match(pair = "opposite_group", by = s),
                abm_birth(when = e > 3, times = 1, cost = e ~ e / 2, inherit = e ~ e + 0.1),
                abm_death(when = e < 1.5))
  ref4 <- run_in("r", mb, go4, ticks = 2, seed = 3)
  gctorture(TRUE)
  got4 <- tryCatch(run_in("rust", mb, go4, ticks = 2, seed = 3), error = identity)
  gctorture(FALSE)
  expect_identical(got4, ref4)
  # FR7: native move and tell
  mt <- abm_setup(agents = list(patches = abm_agents(grass = ~runif(n) < 0.5, heat = 0),
                                sheep = abm_agents(n = 4, e = ~runif(n))),
                  network = abm_network(type = "grid", dims = c(3, 3), on = "patches", torus = TRUE),
                  seed = 4)
  go5 <- abm_go(abm_move(along = "patches", who = "sheep"),
                abm_move(along = "patches", to = "random_empty_neighbour", who = "sheep"),
                abm_tell(grass ~ FALSE, heat ~ e, to = .cell, when = .group == "sheep", .resolve = "sum"))
  ref5 <- run_in("r", mt, go5, ticks = 2, seed = 3)
  gctorture(TRUE)
  got5 <- tryCatch(run_in("rust", mt, go5, ticks = 2, seed = 3), error = identity)
  gctorture(FALSE)
  expect_identical(got5, ref5)
  # FR8: native sequential (columns of two groups, partner writes, globals, a
  # string column and global, `.order`)
  ms <- abm_setup(agents = list(hh = abm_agents(n = 5, c = ~runif(n, 1, 3), g = 0, s = "x"),
                                firm = abm_agents(n = 2, inv = 4, p = ~runif(n))),
                  globals = list(tot = 0, who = "none"), seed = 5)
  go6 <- abm_go(abm_match(pair = "one_of", eligible = .group == "hh", among = .group == "firm"),
                abm_sequential(g ~ pmin(c, partner_inv), partner_inv ~ partner_inv - g,
                               tot ~ tot + g, who ~ s, s ~ if (g > 1) "y" else who),
                abm_sequential(c ~ c + runif(1), .order = c))
  ref6 <- run_in("r", ms, go6, ticks = 2, seed = 3)
  gctorture(TRUE)
  got6 <- tryCatch(run_in("rust", ms, go6, ticks = 2, seed = 3), error = identity)
  gctorture(FALSE)
  expect_identical(got6, ref6)
  # FR11: a repeat block (match restored on leaving it, native `until`)
  go7 <- abm_go(abm_repeat(abm_match(pair = "random"), abm_rules(x ~ x + 0.1),
                           abm_global(t ~ sum(x)), until = t > 20, max = 3))
  ref7 <- run_in("r", m, go7, ticks = 2, seed = 3)
  gctorture(TRUE)
  got7 <- tryCatch(run_in("rust", m, go7, ticks = 2, seed = 3), error = identity)
  gctorture(FALSE)
  expect_identical(got7, ref7)
  # FR11: a global `.by` (the named vector it builds)
  ky <- 1:2
  go8 <- abm_go(abm_global(s ~ sum(k == .key) + 0.5, .by = ky), abm_global(v ~ .key, .by = ky))
  m8 <- m
  m8$globals$s <- stats::setNames(c(0, 0), 1:2)
  ref8 <- run_in("r", m8, go8, ticks = 2, seed = 3)
  gctorture(TRUE)
  got8 <- tryCatch(run_in("rust", m8, go8, ticks = 2, seed = 3), error = identity)
  gctorture(FALSE)
  expect_identical(got8, ref8)
  # FR12: births attached to the network, births by count, deaths that drop edges
  go9 <- abm_go(abm_birth(when = x > 0.5, links = 2,
                          attach_via = abm_match(pair = "network", from = "parent")),
                abm_birth(n = 2, inherit = list(k ~ 1L), attach_via = abm_match(pair = "network")),
                abm_death(when = x < 0.3))
  ref9 <- run_in("r", m, go9, ticks = 2, seed = 3)
  gctorture(TRUE)
  got9 <- tryCatch(run_in("rust", m, go9, ticks = 2, seed = 3), error = identity)
  gctorture(FALSE)
  expect_identical(got9, ref9)
  # FR12: the cross-product within
  go10 <- abm_go(abm_neighbours(c ~ sum(k > 1L), s ~ 1 + sum(x > 0.2), within = abs(x - own_x) < 0.3))
  ref10 <- run_in("r", m, go10, ticks = 2, seed = 3)
  gctorture(TRUE)
  got10 <- tryCatch(run_in("rust", m, go10, ticks = 2, seed = 3), error = identity)
  gctorture(FALSE)
  expect_identical(got10, ref10)
  # FR13: match nearest; network any()/all()
  go11 <- abm_go(abm_match(pair = "nearest", by = x), abm_rules(s ~ coalesce(partner_x, 0)),
                 abm_neighbours(h ~ any(x > 0.5), a ~ all(k > 1L)))
  ref11 <- run_in("r", m, go11, ticks = 2, seed = 3)
  gctorture(TRUE)
  got11 <- tryCatch(run_in("rust", m, go11, ticks = 2, seed = 3), error = identity)
  gctorture(FALSE)
  expect_identical(got11, ref11)
  # FR13: moves along a heading
  go12 <- abm_go(abm_move(along = "patches", who = "sheep", direction = 1L, avoid_occupied = TRUE))
  ref12 <- run_in("r", mt, go12, ticks = 2, seed = 3)
  gctorture(TRUE)
  got12 <- tryCatch(run_in("rust", mt, go12, ticks = 2, seed = 3), error = identity)
  gctorture(FALSE)
  expect_identical(got12, ref12)
  # FR14: abm_draw + network-view abm_neighbours (own_, edge draws, na.rm)
  go13 <- abm_go(abm_draw(w ~ runif(n()), .each = "endpoint"),
                 abm_neighbours(c ~ sum(w < own_x), b ~ sum(w_back < x),
                                p ~ mean(k == own_k, na.rm = TRUE), d ~ n()))
  ref13 <- run_in("r", m, go13, ticks = 2, seed = 3)
  gctorture(TRUE)
  got13 <- tryCatch(run_in("rust", m, go13, ticks = 2, seed = 3), error = identity)
  gctorture(FALSE)
  expect_identical(got13, ref13)
  # FR14: uphill/downhill moves with range/axes_only/avoid_occupied and draws
  go14 <- abm_go(abm_move(along = "patches", who = "sheep",
                          to = uphill(if_else(grass, 1, 0) + runif(n())),
                          range = 2, axes_only = TRUE, avoid_occupied = TRUE),
                 abm_move(along = "patches", who = "sheep", to = downhill(heat + own_e)))
  ref14 <- run_in("r", mt, go14, ticks = 2, seed = 3)
  gctorture(TRUE)
  got14 <- tryCatch(run_in("rust", mt, go14, ticks = 2, seed = 3), error = identity)
  gctorture(FALSE)
  expect_identical(got14, ref14)
  # FR15a: nearest+cost match, tell(.resolve = "collect"), network link/unlink
  m15 <- abm_setup(agents = list(c = abm_agents(n = 4, v = ~runif(n), inbox = ~vector("list", n)),
                                 p = abm_agents(n = 6, v = ~runif(n))),
                   network = abm_network(type = "empty"), seed = 6)
  go15 <- abm_go(
    abm_match(pair = "nearest", cost = abs(v - own_v),
              eligible = .group == "p", among = .group == "c"),
    abm_tell(inbox ~ .id, to = .partner, when = !is.na(.partner), .resolve = "collect"),
    abm_link(),
    abm_match(pair = "random", size = 3),
    abm_link(),
    abm_match(pair = "network"),
    abm_unlink())
  ref15 <- run_in("r", m15, go15, ticks = 2, seed = 3)
  gctorture(TRUE)
  got15 <- tryCatch(run_in("rust", m15, go15, ticks = 2, seed = 3), error = identity)
  gctorture(FALSE)
  expect_identical(got15, ref15)
  # FR15b: relations in abm_sequential() and abm_pairs()
  m15b <- abm_setup(
    agents = list(b = abm_agents(n = 4, w = 2, g = 0), s = abm_agents(n = 2, inv = 5)),
    relations = list(rel = abm_relation(data.frame(from = 1:4, to = c(5L, 5L, 6L, 6L)), u = 0L)),
    seed = 4)
  go15b <- abm_go(
    abm_rules(w ~ 2, g ~ 0, .scope = "population"),
    abm_match(pair = "one_of", eligible = .group == "b", among = .rel),
    abm_sequential(g ~ pmin(w, partner_inv), partner_inv ~ partner_inv - g,
                   rel_u ~ rel_u + as.integer(g < w)),
    abm_pairs(via = "rel", u ~ u + 1L, .when = from_w > 0))
  ref15b <- run_in("r", m15b, go15b, ticks = 2, seed = 4)
  gctorture(TRUE)
  got15b <- tryCatch(run_in("rust", m15b, go15b, ticks = 2, seed = 4), error = identity)
  gctorture(FALSE)
  expect_identical(got15b, ref15b)
})

test_that("FR9b: measures and checks compiled natively give R's tables and errors", {
  m <- abm_setup(agents = list(a = abm_agents(n = 20, x = ~runif(n)), b = abm_agents(n = 5, x = 1)),
                 globals = list(tot = 0), seed = 3)
  go <- abm_go(abm_rules(x ~ x * 1.1), abm_global(tot ~ sum(x)))
  ms <- list(mx = ~max(x), tot = ~tot * 2, n = ~n(), q = ~quantile(x, 0.5))
  ck <- list(pos = ~all(x > 0), small = ~tot < 1e6)
  for (rec in list("final", "globals", 3L)) {
    expect_engines_agree(m, go, ticks = 8, seed = 1, record = rec, measures = ms, checks = ck,
                         what = paste("FR9b", rec))
  }
  fails <- list(late = ~tot < 30)
  err <- function(e) tryCatch(run_in(e, m, go, ticks = 20, seed = 1, record = "final",
                                     checks = fails), error = identity)
  a <- err("rust"); b <- err("r")
  expect_identical(conditionMessage(a), conditionMessage(b))
  expect_identical(class(a), class(b))
  expect_identical(a$tick, b$tick)
})

test_that("abm_repeat() blocks run inside the loop, natively where their steps are", {
  m <- abm_setup(agents = abm_agents(n = 30, x = ~runif(n), k = 0L), globals = list(tot = 0),
                 seed = 2)
  cases <- list(
    until   = abm_go(abm_repeat(abm_rules(x ~ x * 0.9 + runif(n()) * 0.1),
                                abm_global(tot ~ sum(x)), until = tot < 10, max = 20),
                     abm_rules(k ~ k + 1L)),
    no_until = abm_go(abm_repeat(abm_rules(x ~ x + 1), max = 3)),
    # a match made inside the block does not escape it; `until` reads the
    # standing match (the R callback)
    match   = abm_go(abm_match(pair = "random"),
                     abm_repeat(abm_match(pair = "random"),
                                abm_rules(x ~ x + coalesce(partner_x, 0) / 10),
                                until = any(x > 3), max = 5),
                     abm_rules(x ~ x + coalesce(partner_x, 0))),
    nested  = abm_go(abm_repeat(abm_repeat(abm_rules(x ~ x + 0.1), until = mean(x) > 1, max = 4),
                                abm_global(tot ~ tot + 1), until = tot > 6, max = 3)),
    # an R step inside the block
    r_step  = abm_go(abm_repeat(abm_rules(x ~ vapply(x, function(v) v + 1, 0)),
                                abm_global(tot ~ sum(x)), until = tot > 100, max = 9)),
    # an R step immediately before a block end whose `until` stays on R (Slot::Never
    # on the end marker must never be swallowed into the R segment on pass 2+)
    r_end   = abm_go(abm_repeat(abm_rules(k ~ k + 1L),
                                abm_rules(x ~ vapply(x, function(v) v + 0.5, 0)),
                                until = any(x > 2), max = 5))
  )
  for (nm in names(cases)) {
    expect_engines_agree(m, cases[[nm]], ticks = 4, seed = 1, what = paste("repeat", nm))
    expect(.rl_last$stats[[1]] > 0, paste("repeat", nm, "ran no native step"))
  }
  # errors: `until` of the wrong shape, a missing name in `until`, in a step of
  # the block, in a nested block's `until`
  bad <- list(abm_go(abm_repeat(abm_rules(x ~ x + 1), until = x > 2, max = 3)),
              abm_go(abm_repeat(abm_rules(x ~ x + 1), until = nope > 2, max = 3)),
              abm_go(abm_rules(x ~ x), abm_repeat(abm_rules(x ~ x + nope), max = 3)),
              abm_go(abm_repeat(abm_repeat(abm_rules(x ~ x), until = nope > 1, max = 2), max = 2)))
  for (go in bad) {
    err <- function(e) tryCatch(run_in(e, m, go, ticks = 2, seed = 1), error = identity)
    a <- err("rust"); b <- err("r")
    expect_identical(conditionMessage(a), conditionMessage(b))
    expect_identical(class(a), class(b))
  }
})
