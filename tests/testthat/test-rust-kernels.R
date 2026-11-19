# Rust step kernels (migration phase 4): each kernel gives exactly the R engine's
# state, run through abm_run() under every engine; a kernel that declines, errors
# or warns hands the step to the R engine's own code; the per-kernel switch turns
# a kernel off. Failures name the platform and the first differing element.

# how often the Rust fold ran, with the real one doing the work
count_folds <- function(env = parent.frame()) {
  calls <- new.env()
  calls$n <- 0L
  real <- rs_nb_fold
  local_mocked_bindings(rs_nb_fold = function(...) {
    calls$n <- calls$n + 1L
    real(...)
  }, .env = env)
  calls
}

grid_world <- function(n_side = 9, x = ~runif(n), seed = 4, ...) {
  abm_setup(agents = abm_agents(n = n_side^2, x = x, k = ~sample.int(5L, n, TRUE),
                                on = ~runif(n) < 0.5, ...),
            network = abm_network("grid", dims = c(n_side, n_side), diagonals = FALSE),
            globals = list(w = 2), seed = seed)
}

test_that("the Rust fold is nb_fold() bit for bit", {
  withr::local_seed(11)
  w <- grid_world(12)
  nb <- neighbour_table(w$edges)
  ix <- nb_index(nb, 144L)
  tiny <- 2^-1074
  pools <- list(runif(144), rnorm(144, sd = 1e300), c(rep(c(Inf, -Inf, 0, -0, tiny), 28), 1:4),
                rexp(144) * 1e-300)
  for (p in seq_along(pools)) {
    x <- pools[[p]]
    for (fn in c("sum", "mean")) {
      got <- rs_nb_fold(fn == "mean", x, ix$cols, ix$cnt)
      want <- nb_fold(fn, x, ix)
      if (is.null(got)) {
        # only a neighbourhood summing to NA/NaN may decline
        expect_true(any(is.na(want[!ix$none])), label = paste("pool", p, fn, "declined"))
      } else {
        expect_same_as_r(got, want, paste("pool", p, fn))
      }
    }
  }
  x <- runif(144)
  x[c(3, 50)] <- c(NA, NaN)
  expect_null(rs_nb_fold(TRUE, x, ix$cols, ix$cnt))
  expect_null(rs_nb_fold(TRUE, 1:144, ix$cols, ix$cnt))     # integers: R's fold
})

test_that("abm_neighbours() on a network agrees under every engine and uses the kernel", {
  go <- abm_go(
    abm_neighbours(avg ~ mean(x), tot ~ sum(x * w), cnt ~ sum(on), ks ~ mean(k)),
    abm_rules(x ~ 0.5 * x + 0.5 * avg))
  calls <- count_folds()
  expect_engines_agree(grid_world(), go, ticks = 4, seed = 2, what = "grid neighbours")
  # rust and both run it (both twice per step: once is the kernel), 2 double rules x 4 ticks
  expect_gt(calls$n, 0L)
  expect_engines_agree(grid_world(), go, ticks = 3, seed = 2, record = "all",
                       what = "grid neighbours, every tick recorded")
})

test_that("missing values, an isolated agent and infinities take the R fold where they must", {
  go <- abm_go(abm_neighbours(avg ~ mean(x), tot ~ sum(x)))
  na_x <- ~ifelse(seq_len(n) %in% c(5, 17), NA, runif(n))
  expect_engines_agree(grid_world(x = na_x), go, ticks = 2, seed = 3, what = "NA neighbours")
  inf_x <- ~ifelse(seq_len(n) == 9, Inf, runif(n))
  expect_engines_agree(grid_world(x = inf_x), go, ticks = 2, seed = 3, what = "Inf neighbours")
  sparse <- abm_setup(agents = abm_agents(n = 30, x = ~runif(n)),
                      network = abm_network("manual", edges = data.frame(from = c(1, 2, 7), to = c(2, 3, 8))), seed = 8)
  expect_engines_agree(sparse, go, ticks = 2, seed = 3, what = "isolated agents")
})

test_that("shapes the kernel does not take decline to the step adapter", {
  w <- grid_world()
  calls <- count_folds()
  go <- abm_go(abm_neighbours(top ~ max(x)),                       # not sum/mean
               abm_neighbours(near ~ n(), within = abs(x - own_x) < 0.1))
  expect_engines_agree(w, go, ticks = 2, seed = 5, what = "declined shapes")
  expect_identical(calls$n, 0L)
})

test_that("a kernel that errors or warns is replayed on the R path; the switch turns it off", {
  w <- grid_world()
  go <- abm_go(abm_neighbours(avg ~ mean(x)), abm_rules(x ~ x + avg / 10))
  ref <- run_in("r", w, go, ticks = 3, seed = 6)
  seed_r <- rng_get()
  real <- rust_kernels$abm_neighbours
  for (bad in list(function(step, state, store) stop("kernel bug"),
                   function(step, state, store) { warning("kernel noise"); real$fun(step, state, store) },
                   function(step, state, store) { stats::runif(3); NULL })) {
    local_mocked_bindings(rust_kernels = list(abm_neighbours = list(fun = bad, auto = TRUE)))
    for (engine in c("auto", "rust")) {
      expect_no_warning(got <- run_in(engine, w, go, ticks = 3, seed = 6))
      expect_identical(got, ref)
      expect_identical(rng_get(), seed_r)
    }
  }
  # off by name: the steps go through the adapter, and the kernel never runs
  calls <- new.env()
  calls$n <- 0L
  spy <- function(step, state, store) { calls$n <- calls$n + 1L; real$fun(step, state, store) }
  local_mocked_bindings(rust_kernels = list(abm_neighbours = list(fun = spy, auto = TRUE)))
  withr::local_options(tidyABM.rust_kernels = c(neighbours = FALSE))
  expect_identical(run_in("rust", w, go, ticks = 3, seed = 6), ref)
  expect_identical(calls$n, 0L)
  expect_identical(engine_run_once("auto", go$steps), run_once)
})

test_that("an error in a rule is the R engine's own under every engine", {
  w <- grid_world()
  go <- abm_go(abm_neighbours(avg ~ mean(no_such_column)))
  errs <- lapply(c("r", "rust", "both"), function(engine) {
    tryCatch(run_in(engine, w, go, ticks = 1, seed = 1), error = function(e) e)
  })
  for (e in errs[-1]) {
    expect_identical(class(e), class(errs[[1]]))
    expect_identical(conditionMessage(e), conditionMessage(errs[[1]]))
  }
})

test_that("\"auto\" leaves runs whose kernels are not proven faster on the R engine", {
  go <- abm_go(abm_neighbours(avg ~ mean(x)))
  expect_false(rust_kernels$abm_neighbours$auto)
  expect_identical(engine_run_once("auto", go$steps), run_once)
})
