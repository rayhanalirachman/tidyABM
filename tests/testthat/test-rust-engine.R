# The Rust run path (migration phase 3): the groups in the Rust column store, steps
# without a kernel through the step adapter, the result stacked by the store.
# "rust" and "both" must give exactly what the R engine gives, "auto" is the R
# engine unless a step has an "auto" kernel, and "both" must catch a kernel that
# differs. (run_in() and expect_engines_agree() are in helper-rust.R.)

economy <- function() abm_setup(
  agents = list(rich = abm_agents(n = 30, money = ~runif(n, 50, 100), kind = "r"),
                poor = abm_agents(n = 20, money = ~runif(n, 0, 10), debt = 0L)),
  globals = list(tax = 0.1, total = 0), seed = 3)

test_that("a two-group economy with matches, rules and globals agrees", {
  go <- abm_go(
    abm_match(pair = "random", role = list(giver = money > 1, receiver = TRUE)),
    abm_rules(money ~ if_else(.role == "giver", money - 1, money + 1)),
    abm_global(total ~ sum(money)))
  expect_engines_agree(economy(), go, ticks = 6, seed = 1,
                       measures = list(top = ~max(money)),
                       checks = list(kept = ~total == 0 || abs(sum(money) - total) < 1e-8),
                       what = "economy")
  for (record in list("final", "globals", 2L)) {
    expect_engines_agree(economy(), go, ticks = 5, seed = 2, record = record,
                         what = paste("economy, record", record))
  }
})

test_that("births and deaths that change the rows agree", {
  m <- abm_setup(agents = abm_agents(
    n = 60, strategy = ~sample(c("cooperate", "defect"), n, replace = TRUE),
    resource = 10), seed = 4)
  go <- abm_go(abm_match(pair = "random"),
               abm_rules(resource ~ case_when(
                 strategy == "cooperate" & partner_strategy == "cooperate" ~ resource + 2,
                 strategy == "defect" & partner_strategy == "cooperate" ~ resource + 4,
                 TRUE ~ resource - 1) - 1),
               abm_birth(when = resource > 20, cost = resource ~ resource / 2),
               abm_death(when = resource <= 0))
  r <- expect_engines_agree(m, go, ticks = 15, seed = 4, what = "births and deaths")
  expect_true(length(unique(table(r$tick))) > 1)
})

test_that("a network, a lattice and moving agents agree", {
  net <- abm_setup(agents = abm_agents(n = 40, x = ~runif(n)),
                   network = abm_network(type = "random", degree = 3), seed = 5)
  expect_engines_agree(net, abm_go(abm_neighbours(nb ~ mean(x)),
                                   abm_match(pair = "network"),
                                   abm_rules(x ~ (x + partner_x) / 2),
                                   abm_match(pair = "random", eligible = runif(n()) < 0.1),
                                   abm_link()),
                       ticks = 4, seed = 6, what = "network")
  grid <- abm_setup(agents = list(
    patches = abm_grid(dims = c(5, 5), grass = ~runif(n)),
    bugs = abm_agents(n = 15, energy = 5, on = "patches")), seed = 7)
  expect_engines_agree(grid, abm_go(abm_move(along = "patches", who = "bugs"),
                                    abm_rules(energy ~ energy - 1)),
                       ticks = 4, seed = 8, what = "lattice")
})

test_that("classed and list columns go through the store and R's own stacking", {
  m <- abm_setup(agents = abm_agents(
    n = 12, f = ~factor(sample(c("lo", "hi"), n, TRUE), levels = c("lo", "hi")),
    born = ~as.Date("2020-01-01") + seq_len(n), x = 1), seed = 9)
  m$groups[[1]]$genome <- lapply(seq_len(12), function(i) i * 1:3)
  go <- abm_go(abm_rules(x ~ x + as.integer(f), born ~ born + 1L,
                         genome ~ lapply(genome, rev)))
  r <- expect_engines_agree(m, go, ticks = 3, seed = 10, what = "classed columns")
  expect_s3_class(r$f, "factor")
  expect_type(r$genome, "list")
})

test_that("sweeps, replicates, worlds per run and cores agree", {
  go <- abm_go(abm_rules(money ~ money * (1 - tax)), abm_global(total ~ sum(money)))
  expect_engines_agree(economy(), go, ticks = 3, seed = 11, reps = 2,
                       params = list(tax = c(0.1, 0.2)), what = "sweep")
  world <- function(n) abm_setup(agents = abm_agents(n = n, money = ~runif(n)),
                                 globals = list(tax = 0.5, total = 0))
  expect_engines_agree(world, go, ticks = 2, seed = 12, params = list(n = c(5, 8)),
                       what = "worlds")
  # (Windows: a pool of R sessions, which load the installed tidyABM)
  if (.Platform$OS.type == "windows") skip_if(dev_loaded(), "workers load the installed tidyABM")
  expect_engines_agree(economy(), go, ticks = 3, seed = 13, reps = 3, cores = 2,
                       what = "cores")
})

test_that("FR10: a sweep on a pool of R sessions returns the serial result", {
  skip_on_cran()
  skip_if(dev_loaded(), "workers load the installed tidyABM")
  go <- abm_go(abm_match(pair = "random"), abm_rules(money ~ money * (1 - tax) + runif(n())),
               abm_global(total ~ sum(money)))
  rate <- 0.5
  world <- function(n) abm_setup(agents = abm_agents(n = n, money = ~runif(n) * rate),
                                 globals = list(tax = 0.1, total = 0))
  for (eng in c("r", "rust")) {
    withr::local_options(tidyABM.engine = eng)
    one <- abm_run(world, go, ticks = 3, seed = 14, reps = 2, params = list(n = c(5, 9)))
    withr::local_options(tidyABM.psock = TRUE)
    expect_identical(abm_run(world, go, ticks = 3, seed = 14, reps = 2,
                             params = list(n = c(5, 9)), cores = 2), one,
                     label = paste("psock", eng))
    expect_error(abm_run(world, go, ticks = 2, reps = 2, params = list(n = 4), cores = 2,
                         checks = list(no = ~FALSE)), class = "tidyABM_check_failed")
    withr::local_options(tidyABM.psock = NULL)
  }
})

test_that("seed = NULL continues the caller's stream identically", {
  m <- abm_setup(agents = abm_agents(n = 8, x = 0))
  go <- abm_go(abm_rules(x ~ x + runif(n())))
  expect_engines_agree(m, go, ticks = 3, what = "seed = NULL")
})

test_that("errors, failed checks, warnings and printing are the R engine's", {
  m <- abm_setup(agents = abm_agents(n = 5, money = 1))
  cnd_of <- function(engine, ...) {
    withr::local_options(tidyABM.engine = engine)
    tryCatch(abm_run(...), error = identity)
  }
  same_error <- function(..., what) {
    ref <- cnd_of("r", ...)
    expect_s3_class(ref, "error")
    for (engine in c("rust", "both")) {
      got <- cnd_of(engine, ...)
      expect_identical(class(got), class(ref), label = paste(what, engine, "class"))
      expect_identical(conditionMessage(got), conditionMessage(ref),
                       label = paste(what, engine, "message"))
      expect_identical(got$tick, ref$tick)
      expect_identical(got$check, ref$check)
    }
  }
  same_error(m, abm_go(abm_rules(money ~ monney + 1)), ticks = 2, what = "missing column")
  same_error(m, abm_go(abm_rules(money ~ c(1, 2))), ticks = 2, what = "wrong size")
  same_error(m, abm_go(abm_rules(money ~ money - 1)), ticks = 3,
             checks = list(solvent = ~all(money >= 0)), what = "check")

  warn_go <- abm_go(abm_rules(money ~ sqrt(money - 2)))
  for (engine in c("r", "rust", "both")) {
    withr::local_options(tidyABM.engine = engine)
    w <- testthat::capture_warnings(abm_run(m, warn_go, ticks = 1))
    expect_identical(w, "NaNs produced", label = paste(engine, "warnings"))
  }
  say <- function(x) { cat("tick\n"); x }
  print_go <- abm_go(abm_rules(money ~ say(money)))
  for (engine in c("r", "rust", "both")) {
    withr::local_options(tidyABM.engine = engine)
    expect_identical(capture.output(r <- abm_run(m, print_go, ticks = 2)),
                     c("tick", "tick"), label = paste(engine, "printed once per tick"))
  }
})

test_that("\"auto\" uses the Rust loop when a step has an auto kernel, else R", {
  go <- abm_go(abm_rules(money ~ money + 1))
  expect_false(identical(engine_run_once("auto", go$steps), run_once))
  expect_identical(engine_run_once("r", go$steps), run_once)
  expect_true(is.function(engine_run_once("rust", go$steps)))
  go_r <- abm_go(abm_rules(money ~ identity(money)))
  expect_identical(engine_run_once("auto", go_r$steps), run_once)
})

# A stand-in for a phase-4 kernel: the step adapter, then `tamper` applied to the
# groups it loads back
fake_kernel <- function(tamper = identity, calls = NULL) {
  function(step, state, store) {
    if (!is.null(calls)) calls$n <- calls$n + 1L
    state <- run_step(step, state)
    state$groups <- tamper(state$groups)
    store$load(state$groups)
    state$groups <- store$export()
    state
  }
}

test_that("a step with a kernel takes the Rust path under \"auto\", and agrees", {
  calls <- new.env()
  calls$n <- 0L
  local_mocked_bindings(rust_kernels = list(abm_rules = list(fun = fake_kernel(calls = calls), auto = TRUE)))
  go <- abm_go(abm_rules(money ~ money + runif(n())), abm_global(total ~ sum(money)))
  expect_false(identical(engine_run_once("auto", go$steps), run_once))
  expect_engines_agree(economy(), go, ticks = 3, seed = 14, what = "with a kernel")
  # auto, rust and both (twice: the R path is never the kernel) x 3 ticks
  expect_identical(calls$n, 9L)
})

test_that("\"both\" stops at the first step where a kernel differs, and says where", {
  go <- abm_go(abm_global(total ~ sum(money)),
               abm_rules(money ~ money + 1),
               abm_global(total ~ sum(money)))
  bump <- function(groups) {
    if (groups$poor$money[[3]] > 5) groups$poor$money[[3]] <- groups$poor$money[[3]] + 1e-9
    groups
  }
  local_mocked_bindings(rust_kernels = list(abm_rules = list(fun = fake_kernel(bump), auto = TRUE)))
  withr::local_options(tidyABM.engine = "both")
  err <- expect_error(abm_run(economy(), go, ticks = 50, seed = 15),
                      class = "tidyABM_engine_mismatch")
  msg <- cli::ansi_strip(conditionMessage(err))
  expect_match(msg, "after step 2, `abm_rules()`, at tick", fixed = TRUE)
  expect_match(msg, "state > groups > poor > column money, row 3", fixed = TRUE)
  expect_match(msg, "R [0-9.]+, Rust [0-9.]+")

  # an extra draw shows in the random stream, an extra warning in the conditions
  draw <- function(groups) { stats::runif(1); groups }
  local_mocked_bindings(rust_kernels = list(abm_rules = list(fun = fake_kernel(draw), auto = TRUE)))
  err <- expect_error(abm_run(economy(), go, ticks = 2, seed = 15),
                      class = "tidyABM_engine_mismatch")
  expect_match(conditionMessage(err), ".Random.seed", fixed = TRUE)
  noisy <- function(groups) { warning("extra"); groups }
  local_mocked_bindings(rust_kernels = list(abm_rules = list(fun = fake_kernel(noisy), auto = TRUE)))
  err <- expect_error(abm_run(economy(), go, ticks = 2, seed = 15),
                      class = "tidyABM_engine_mismatch")
  expect_match(conditionMessage(err), "warnings and messages", fixed = TRUE)
  broken <- function(groups) stop("kernel bug")
  local_mocked_bindings(rust_kernels = list(abm_rules = list(fun = fake_kernel(broken), auto = TRUE)))
  err <- expect_error(abm_run(economy(), go, ticks = 2, seed = 15),
                      class = "tidyABM_engine_mismatch")
  expect_match(conditionMessage(err), "kernel bug", fixed = TRUE)
})
