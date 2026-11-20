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
