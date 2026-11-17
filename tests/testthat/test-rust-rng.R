# Migration phase 2: Rust draws through R's own generator (C API) give R's stream.
# Every comparison is identical() on the values and on `.Random.seed` afterwards.
# The rs_* entry points are called directly so that the Rust path, not the R
# fallback, is what is compared.

seeds <- c(1, 42, 2024, 777, 123456)

local_rng <- function(env = parent.frame()) {
  old <- RNGkind()
  withr::defer(suppressWarnings(RNGkind(old[[1]], old[[2]], old[[3]])), envir = env)
  withr::local_preserve_seed(.local_envir = env)
}

draw_cases <- function(n) { n <- as.integer(n); list(
  unif = list(function() rs_draw("unif", n, 0, 0), function() runif(n)),
  norm = list(function() rs_draw("norm", n, 0, 0), function() rnorm(n)),
  exp = list(function() rs_draw("exp", n, 0, 0), function() rexp(n)),
  index7 = list(function() rs_draw("index", n, 7, 0),
                function() as.double(sample.int(7, n, TRUE) - 1L)),
  index_max = list(function() rs_draw("index", n, as.double(.Machine$integer.max), 0),
                   function() as.double(sample.int(.Machine$integer.max, n, TRUE) - 1L)),
  runif = list(function() rs_draw("runif", n, -2, 3), function() runif(n, -2, 3)),
  runif_eq = list(function() rs_draw("runif", n, 1.5, 1.5), function() runif(n, 1.5, 1.5)),
  rnorm = list(function() rs_draw("rnorm", n, 2, 3), function() rnorm(n, 2, 3)),
  rnorm_sd0 = list(function() rs_draw("rnorm", n, 2, 0), function() rnorm(n, 2, 0)),
  rlnorm = list(function() rs_draw("rlnorm", n, 0.1, 0.5), function() rlnorm(n, 0.1, 0.5)),
  rexp = list(function() rs_draw("rexp", n, 2.5, 0), function() rexp(n, 2.5)),
  rpois = list(function() rs_draw("rpois", n, 3.5, 0), function() rpois(n, 3.5)),
  rpois_big = list(function() rs_draw("rpois", n, 50, 0), function() rpois(n, 50)),
  rbinom = list(function() rs_draw("rbinom", n, 10, 0.3), function() rbinom(n, 10, 0.3)),
  rbinom_big = list(function() rs_draw("rbinom", n, 1000, 0.3), function() rbinom(n, 1000, 0.3)))
}

test_that("draws through R's C API are R's draws, value and state", {
  local_rng()
  n <- big_n(1e6)
  for (seed in seeds) for (nm in names(cases <- draw_cases(n))) {
    expect_same_draws(cases[[nm]][[1]], cases[[nm]][[2]], seed, nm)
  }
  expect_identical(rs_draw("unif", 0L, 0, 0), double())
})

sample_cases <- function(n) list(
  perm_10 = c(10, 10, 0), part_1000 = c(1000, 300, 0), perm_big = c(n, n, 0),
  repl = c(1000, n, 1), one = c(1, 1, 0), one_repl = c(1, 5, 1),
  empty = c(0, 0, 0), none_repl = c(5, 0, 1),
  hash_1 = c(.Machine$integer.max, 1, 0), hash_150 = c(.Machine$integer.max, 150, 0),
  hash_big = c(.Machine$integer.max, min(n, 1e5), 0))

test_that("sample.int() from Rust matches R on every path", {
  local_rng()
  cases <- sample_cases(big_n(1e6))
  check <- function(a, seed, nm) {
    expect_same_draws(function() rs_sample_int(as.integer(a[1]), as.integer(a[2]), a[3] == 1),
                      function() sample.int(a[1], a[2], a[3] == 1), seed, nm)
  }
  for (seed in seeds) for (nm in names(cases)) check(cases[[nm]], seed, nm)
  if (identical(Sys.getenv("NOT_CRAN"), "true")) {
    # just above 1e7: hash version at size <= n / 2, selection above it
    check(c(1e7 + 1, 5e6, 0), 1, "hash edge")
    check(c(1e7 + 1, 5e6 + 1, 0), 1, "selection edge")
  }
})

test_that("the engine's sampling idioms are reproduced", {
  local_rng()
  x <- c("w", "x", "y", "z", "v")
  for (seed in seeds) {
    # shuffle() (utils.R), abm_sequential order, match candidate shuffles
    expect_same_draws(function() x[rs_sample_int(length(x), length(x), FALSE)],
                      function() shuffle(x), seed, "shuffle")
    # draw_from(): partner draws with replacement
    expect_same_draws(function() x[rs_sample_int(length(x), 12L, TRUE)],
                      function() draw_from(x, 12), seed, "draw_from")
    # single picks, move targets per mover, k neighbours without replacement
    expect_same_draws(function() rs_sample_int(17L, 1L, FALSE),
                      function() sample.int(17, 1L), seed, "pick one")
    expect_same_draws(function() rs_sample_int(8L, 40L, TRUE),
                      function() sample.int(8, 40, replace = TRUE), seed, "targets")
    expect_same_draws(function() rs_sample_int(9L, 4L, FALSE),
                      function() sample.int(9, 4), seed, "k neighbours")
    # run_seeds(): one seed per run
    expect_same_draws(function() rs_sample_int(.Machine$integer.max, 150L, FALSE),
                      function() sample.int(.Machine$integer.max, 150), seed, "run seeds")
    # coin flips
    expect_same_draws(function() rs_bernoulli(25L, 0.5),
                      function() stats::runif(25) < 0.5, seed, "coin flips")
  }
})

test_that("Bernoulli draws are runif(n) < p, NA probabilities included", {
  local_rng()
  p <- c(0, 1, 0.5, NA, NaN, -1, 2, 0.25)
  for (seed in seeds) {
    expect_same_draws(function() rs_bernoulli(1000L, 0.3),
                      function() runif(1000) < 0.3, seed, "scalar p")
    expect_same_draws(function() rs_bernoulli(length(p), p),
                      function() runif(length(p)) < p, seed, "vector p")
    expect_same_draws(function() rs_bernoulli(3L, NA_real_),
                      function() runif(3) < NA_real_, seed, "NA p")
  }
  expect_identical(rs_bernoulli(0L, 0.5), logical())
  expect_error(rust_call(rs_bernoulli(3L, c(0.1, 0.2))), class = "tidyABM_rust_error")
})

test_that("Rust draws follow every built-in RNG kind", {
  local_rng()
  n <- 2000L
  battery <- c(draw_cases(n)[c("unif", "norm", "exp", "rnorm", "runif", "rexp",
                               "rpois", "rbinom", "index7")],
    list(
      perm = list(function() rs_sample_int(300L, 300L, FALSE), function() sample.int(300)),
      repl = list(function() rs_sample_int(30L, n, TRUE), function() sample.int(30, n, TRUE)),
      hash = list(function() rs_sample_int(.Machine$integer.max, 50L, FALSE),
                  function() sample.int(.Machine$integer.max, 50)),
      bern = list(function() rs_bernoulli(n, 0.4), function() runif(n) < 0.4)))
  for (k in rust_rng_kinds$kind) for (nk in rust_rng_kinds$normal) {
    for (sk in rust_rng_kinds$sample) {
      suppressWarnings(RNGkind(k, nk, sk))
      expect_true(rust_rng_ok("rbinom"))
      for (nm in names(battery)) {
        expect_same_draws(battery[[nm]][[1]], battery[[nm]][[2]], 3,
                          paste(nm, "under", k, nk, sk))
      }
    }
  }
})

test_that("unverified RNG kinds make the Rust path decline", {
  def <- c("Mersenne-Twister", "Inversion", "Rejection")
  expect_true(rust_rng_ok(k = def))
  expect_false(rust_rng_ok(k = c("user-supplied", def[2:3])))
  expect_false(rust_rng_ok(k = c(def[1], "user-supplied", def[3])))
  expect_false(rust_rng_ok(k = c("A-Future-Kind", def[2:3])))
  # R-devel's 4th element is the binomial kind: only rbinom depends on it
  expect_true(rust_rng_ok("rbinom", k = c(def, "BTPE")))
  expect_false(rust_rng_ok("rbinom", k = c(def, "Another")))
  expect_true(rust_rng_ok("unif", k = c(def, "Another")))
})

test_that("declined draws are R's own, warnings included", {
  local_rng()
  local_mocked_bindings(rust_rng_ok = function(...) FALSE)
  for (seed in seeds[1:2]) {
    expect_same_draws(function() rust_draw("norm", 50), function() rnorm(50), seed, "norm")
    expect_same_draws(function() rust_draw("rbinom", 50, 5, 0.2),
                      function() rbinom(50, 5, 0.2), seed, "rbinom")
    expect_same_draws(function() rust_sample_int(20, 5), function() sample.int(20, 5),
                      seed, "sample.int")
    expect_same_draws(function() rust_bernoulli(9, 0.5), function() runif(9) < 0.5,
                      seed, "bernoulli")
  }
  local_mocked_bindings(rust_rng_ok = function(...) TRUE)
  # parameters for which R warns or gives NaN: Rust declines, R draws and warns
  bad <- list(list("runif", 2, 1), list("runif", NaN, 1), list("rnorm", 0, -1),
              list("rnorm", Inf, 1), list("rlnorm", 0, -1), list("rexp", -1, 0), list("rexp", 1e-320, 0),
              list("rpois", -1, 0), list("rbinom", 2.5, 0.5), list("rbinom", 5, 1.5))
  r_fun <- list(runif = runif, rnorm = rnorm, rlnorm = rlnorm, rexp = function(n, a, b) rexp(n, a),
                rpois = function(n, a, b) rpois(n, a), rbinom = rbinom)
  for (b in bad) {
    expect_null(rs_draw(b[[1]], 3L, b[[2]], b[[3]]))
    set.seed(1)
    w1 <- tryCatch(rust_draw(b[[1]], 3, b[[2]], b[[3]]), warning = conditionMessage)
    set.seed(1)
    w2 <- tryCatch(r_fun[[b[[1]]]](3, b[[2]], b[[3]]), warning = conditionMessage)
    expect_identical(w1, w2)
  }
  expect_identical(rust_draw("rpois", 0, 1), integer())
  expect_error(rust_call(rs_draw("rgamma", 1L, 1, 1)), class = "tidyABM_rust_error")
  expect_error(rust_call(rs_draw("unif", -1L, 0, 0)), class = "tidyABM_rust_error")
})

test_that("Rust and R draws interleave into one stream", {
  local_rng()
  for (nk in c("Inversion", "Box-Muller")) {
    suppressWarnings(RNGkind("Mersenne-Twister", nk, "Rejection"))
    for (seed in seeds) {
      expect_same_draws(
        function() list(runif(3), rs_draw("norm", 5L, 0, 0), rnorm(3),
                        rs_sample_int(10L, 4L, FALSE), sample(5),
                        rs_draw("unif", 1L, 0, 0), rnorm(1), rs_bernoulli(4L, 0.5),
                        rs_draw("norm", 1L, 0, 0), rexp(2)),
        function() list(runif(3), rnorm(5), rnorm(3), sample.int(10, 4), sample(5),
                        runif(1), rnorm(1), runif(4) < 0.5, rnorm(1), rexp(2)),
        seed, paste("interleaved,", nk))
    }
  }
})

test_that("without set.seed() the caller's stream simply continues", {
  local_rng()
  set.seed(11)
  runif(1)
  s0 <- rng_seed()
  a <- list(rs_draw("unif", 3L, 0, 0), rs_sample_int(6L, 6L, FALSE))
  s1 <- rng_seed()
  assign(".Random.seed", s0, envir = globalenv())
  b <- list(runif(3), sample.int(6))
  expect_identical(a, b)
  expect_identical(s1, rng_seed())
  # no .Random.seed at all: R seeds itself as for runif(), and writes the seed
  rm(".Random.seed", envir = globalenv())
  expect_length(rs_draw("unif", 2L, 0, 0), 2L)
  expect_true(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
  expect_length(get(".Random.seed", envir = globalenv()), 626L)
})

test_that("errors and panics after drawing write the RNG state back", {
  local_rng()
  for (how in c("error", "panic")) {
    set.seed(9)
    out <- capture.output(type = "message", expect_error(
      rust_call(rs_test_rng_fail(7L, how)),
      class = if (how == "error") "tidyABM_rust_error" else "tidyABM_rust_panic"))
    expect_identical(out, character())
    s1 <- rng_seed()
    set.seed(9)
    runif(7)
    expect_identical(s1, rng_seed())
  }
  # the next draws continue R's stream
  set.seed(9)
  try(rust_call(rs_test_rng_fail(3L, "panic")), silent = TRUE)
  a <- rs_draw("unif", 2L, 0, 0)
  set.seed(9)
  expect_identical(a, runif(5)[4:5])
})

test_that("R conditions raised while loading the RNG state pass through safely", {
  local_rng()
  # a broken .Random.seed: R warns and reseeds; under warn = 2 that is an error,
  # which must not unwind through Rust frames
  withr::local_options(warn = 2)
  msg <- function(expr) tryCatch(expr, error = conditionMessage)
  assign(".Random.seed", "broken", envir = globalenv())
  m_rust <- msg(rs_draw("unif", 1L, 0, 0))
  assign(".Random.seed", "broken", envir = globalenv())
  m_r <- msg(runif(1))
  rm(".Random.seed", envir = globalenv())
  expect_identical(m_rust, m_r)
  expect_match(m_rust, ".Random.seed", fixed = TRUE)
  # an exiting warning handler around a Rust call (R_pow warns for huge exponents)
  withr::local_options(warn = 0)
  expect_identical(tryCatch(rs_pow(-Inf, .Machine$double.xmax), warning = conditionMessage),
                   tryCatch((-Inf)^.Machine$double.xmax, warning = conditionMessage))
  # the session and the library are fine afterwards
  set.seed(1)
  a <- rs_draw("unif", 2L, 0, 0)
  set.seed(1)
  expect_identical(a, runif(2))
})
