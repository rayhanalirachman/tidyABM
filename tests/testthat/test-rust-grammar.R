# Full-Rust FR3 grammar (R/rust-expr.R, tidyabm-core/src/expr.rs): every
# construct the native rules take, over values that tell implementations apart
# (NA vs NaN, -0, Inf, integer extremes), run under "r" and "rust" and compared
# bit for bit, with the warnings and `.Random.seed` after. A failure names the
# platform and the first differing element.

gram_model <- function(n = 60) {
  abm_setup(agents = abm_agents(
    n = n,
    x = ~c(NA, NaN, -0, 0, Inf, -Inf, 1e308, runif(n - 7)),
    k = ~c(NA, .Machine$integer.max, -.Machine$integer.max, sample(1:5, n - 3, TRUE)),
    p = ~sample(1:5, n, TRUE),
    s = ~sample(c("a", "b", NA), n, TRUE),
    y = ~ifelse(runif(n) < 0.2, NA, runif(n)),
    b = ~sample(c(TRUE, FALSE, NA), n, TRUE)),
    globals = list(g = 2, t = 0), seed = 5)
}

# every double column bit for bit, then the rest with identical()
expect_bits_agree <- function(got, ref, what) {
  for (nm in names(ref)) {
    if (is.double(ref[[nm]])) {
      expect_same_as_r(got[[nm]], ref[[nm]], paste(what, nm))
    }
  }
  expect(identical(got, ref), paste0(what, ": results differ on ", rust_platform(), "; ",
                                     diff_where(ref, got) %||% "?"))
}

run_capture <- function(engine, ...) {
  w <- character()
  set.seed(11)
  r <- withCallingHandlers(run_in(engine, ...), warning = function(c) {
    w <<- c(w, conditionMessage(c))
    invokeRestart("muffleWarning")
  })
  list(r = r, w = w, seed = rng_get(), stats = .rl_last$stats)
}

expect_native_agree <- function(m, go, what, ticks = 4, native = TRUE) {
  .rl_last$stats <- NULL
  ref <- run_capture("r", m, go, ticks = ticks, seed = NULL)
  got <- run_capture("rust", m, go, ticks = ticks, seed = NULL)
  expect_bits_agree(got$r, ref$r, what)
  expect_identical(got$w, ref$w, label = paste(what, "warnings"))
  expect_identical(got$seed, ref$seed, label = paste(what, ".Random.seed after"))
  if (native) {
    expect(!is.null(got$stats) && got$stats[[1]] > 0,
           paste(what, "did not run natively:", paste(got$stats, collapse = " ")))
  }
  withr::local_options(tidyABM.engine = "both")
  set.seed(11)
  suppressWarnings(abm_run(m, go, ticks = ticks, seed = NULL))
  invisible(got)
}

test_that("each FR3 construct is native and bit-identical to R", {
  m <- gram_model()
  mu <- 0.25
  cases <- list(
    pow      = abm_rules(a ~ x^2, a2 ~ k^2L, a3 ~ (x - 1)^0.5, a4 ~ 2^k),
    abs_sign = abm_rules(a ~ abs(x) + sign(x), a2 ~ abs(k), a3 ~ floor(x * 3) - ceiling(x)),
    round    = abm_rules(a ~ round(x * 10, 1), a2 ~ trunc(y * 100) %% 7, a3 ~ k %/% 3L),
    cum      = abm_rules(a ~ cumsum(p), a2 ~ cumsum(y), a3 ~ cummax(p) + cumprod(p > 0)),
    minmax   = abm_rules(a ~ max(p) + min(y), a2 ~ pmax(p, 3L) - min(x)),
    asint    = abm_rules(a ~ as.integer(y * 10), a2 ~ as.numeric(p) / 2),
    coalesce = abm_rules(a ~ coalesce(y, 0), a2 ~ coalesce(k, 0L), a3 ~ coalesce(s, "z"),
                         a4 ~ coalesce(x, y, -1)),
    in_set   = abm_rules(a ~ s %in% c("a", NA), a2 ~ p %in% c(1L, 3L), a3 ~ p %in% c(2, 4)),
    narm     = abm_rules(a ~ sum(y, na.rm = TRUE), a2 ~ mean(y, na.rm = TRUE),
                         a3 ~ sum(k, na.rm = TRUE) + 0L, a4 ~ mean(b, na.rm = TRUE)),
    draws    = abm_rules(a ~ runif(n(), -mu, mu), a2 ~ rnorm(n(), g, 2), a3 ~ rnorm(n()),
                         a4 ~ runif(n(), max = 3)),
    sample   = abm_rules(a ~ sample(p, n(), replace = TRUE), a2 ~ sample(s, n(), TRUE),
                         a3 ~ sample(5, n(), replace = TRUE), a4 ~ sample(n(), n(), TRUE)),
    index    = abm_rules(a ~ y[p], a2 ~ s[p] , a3 ~ x[sample(n(), n(), TRUE)]),
    scope    = abm_rules(a ~ y * mu + g)
  )
  for (nm in names(cases)) {
    expect_native_agree(m, abm_go(cases[[nm]]), nm)
  }
  # each of these declines on some tick (a warning, NaN, an index of 0): R's
  # result, conditions and random stream come out all the same
  declines <- list(
    overflow = abm_rules(a ~ cumsum(k)),
    coerce   = abm_rules(a ~ as.integer(x * 1e10)),
    nanrand  = abm_rules(a ~ runif(n(), 1, 0)),
    past_end = abm_rules(a ~ y[p + 58L]),
    empty    = abm_rules(a ~ max(y[p > 9])),
    in_na    = abm_rules(a ~ y %in% c(1, 2))
  )
  for (nm in names(declines)) {
    expect_native_agree(m, abm_go(abm_rules(z ~ runif(n())), declines[[nm]]), nm,
                        native = FALSE)
  }
  # an index of 0 drops elements: declined, and R raises its own error
  go <- abm_go(abm_rules(a ~ y[p - 1L]))
  err <- function(e) conditionMessage(tryCatch(run_in(e, m, go, ticks = 2, seed = 1),
                                               error = identity))
  expect_identical(err("rust"), err("r"))
})

test_that("formula-environment values are read every step, as R reads them", {
  m <- gram_model(20)
  step <- local({
    rate <- 1
    list(go = abm_go(abm_rules(a ~ p * rate), abm_global(t ~ t + rate)),
         set = function(v) rate <<- v)
  })
  got <- expect_native_agree(m, step$go, "scope")
  step$set(2.5)
  expect_native_agree(m, step$go, "scope changed")
  # a name the engine's own scope binds (`n`) is left to R
  n <- 3
  expect_native_agree(m, abm_go(abm_rules(a ~ p + 1), abm_rules(a2 ~ n())), "n()")
})

test_that("rules under a standing match run natively for the placed agents", {
  m <- abm_setup(agents = abm_agents(n = 31, x = ~runif(n), k = ~sample(1:3, n, TRUE)), seed = 2)
  go <- abm_go(abm_match(pair = "random"),
               abm_rules(x ~ x + 1, v ~ k * 2.5, k ~ k + 1L, w ~ x > 0.5))
  got <- expect_native_agree(m, go, "match", ticks = 5)
  # nobody placed in the tick: every agent written
  none <- abm_go(abm_match(pair = "random", eligible = .id < 0), abm_rules(x ~ x + 1))
  expect_native_agree(m, none, "empty match", ticks = 3)
  # a rule that is not elementwise under a standing match goes to R
  agg <- abm_go(abm_match(pair = "random"), abm_rules(x ~ x + mean(x)))
  expect_native_agree(m, agg, "aggregate under match", ticks = 3, native = FALSE)
})

test_that("integer and logical neighbour folds are native and exact", {
  m <- abm_setup(agents = abm_agents(n = 40, on = ~runif(n) < 0.3, c = ~sample(0:4, n, TRUE)),
                 network = abm_network(type = "poisson", degree = 2), seed = 3)
  go <- abm_go(abm_neighbours(share ~ mean(on), tot ~ sum(c), cnt ~ sum(on)),
               abm_rules(on ~ on | coalesce(share, 0) >= 0.2))
  expect_native_agree(m, go, "integer folds", ticks = 6)
})

test_that("a step mixing native rules with rules R evaluates agrees, effects once", {
  m <- abm_setup(agents = list(a = abm_agents(n = 25, x = ~runif(n), s = ~sample(c("u", "v"), n, TRUE)),
                               b = abm_agents(n = 10, x = ~runif(n))), seed = 4)
  calls <- new.env()
  calls$n <- 0L
  pick <- function(v) {
    calls$n <- calls$n + 1L
    v[sample.int(length(v), length(v), replace = TRUE)]
  }
  go <- abm_go(abm_rules(a ~ x + runif(n()),
                         b ~ pick(x),                                  # R, draws, effect
                         c ~ sapply(x, function(v) v * 2),             # R, plain
                         d ~ vapply(seq_along(x), function(i) i * 1.5, 0), # R
                         e ~ x * 2 + runif(n())))
  calls$n <- 0L
  ref <- run_capture("r", m, go, ticks = 4, seed = NULL)
  n_r <- calls$n
  calls$n <- 0L
  got <- run_capture("rust", m, go, ticks = 4, seed = NULL)
  expect_bits_agree(got$r, ref$r, "mixed")
  expect_identical(got$seed, ref$seed)
  expect_identical(calls$n, n_r)
  expect_gt(got$stats[[1]], 0)
  # a native rule declining after an R rule ran: the rest goes to R, effects once
  bad <- abm_go(abm_rules(b ~ pick(x), a ~ runif(n(), 1, 0), e ~ x + runif(n())))
  calls$n <- 0L
  ref <- run_capture("r", m, bad, ticks = 3, seed = NULL)
  n_r <- calls$n
  calls$n <- 0L
  got <- run_capture("rust", m, bad, ticks = 3, seed = NULL)
  expect_bits_agree(got$r, ref$r, "mixed decline")
  expect_identical(got$w, ref$w)
  expect_identical(got$seed, ref$seed)
  expect_identical(calls$n, n_r)
  # an R rule erring: R's error, as the R engine raises it
  boom <- abm_go(abm_rules(a ~ x + 1, b ~ stop("boom here")))
  err <- function(e) conditionMessage(tryCatch(run_in(e, m, boom, ticks = 2, seed = 1),
                                               error = identity))
  expect_identical(err("rust"), err("r"))
  withr::local_options(tidyABM.engine = "both")
  expect_no_error(suppressWarnings(abm_run(m, go, ticks = 2, seed = 1)))
})

test_that("`.by` rules run natively in group_by() order, later rules reading earlier", {
  m <- abm_setup(agents = list(
    a = abm_agents(n = 30, x = ~runif(n), k = ~sample(c(1L, 2L, NA, 5L), n, TRUE),
                   d = ~sample(c(-0, 0, 2.5, -1), n, TRUE), b = ~sample(c(TRUE, FALSE, NA), n, TRUE)),
    z = abm_agents(n = 12, x = ~runif(n), k = ~sample(1:3, n, TRUE), d = 1,
                   b = ~sample(c(TRUE, FALSE), n, TRUE))), seed = 6)
  cases <- list(
    int_key = abm_go(abm_rules(x ~ x - mean(x) + runif(n()), w ~ n(), .by = k)),
    seq     = abm_go(abm_rules(x ~ x / sum(x), y ~ x * 2 + max(x), .by = k)),
    rekey   = abm_go(abm_rules(k ~ k %% 2L + 1L, x ~ sum(x) + runif(n()), .by = k)),
    dbl_key = abm_go(abm_rules(x ~ mean(x) + n(), .by = d)),
    lgl_key = abm_go(abm_rules(v ~ sum(x > 0.5), x ~ x + v, .by = b)),
    types   = abm_go(abm_rules(k ~ if_else(x > 0.5, 1.5, 2), .by = b))
  )
  for (nm in names(cases)) expect_native_agree(m, cases[[nm]], paste(".by", nm))
  # a string key goes to R
  s <- abm_setup(agents = abm_agents(n = 20, x = ~runif(n), g = ~sample(c("p", "q"), n, TRUE)))
  expect_native_agree(s, abm_go(abm_rules(x ~ mean(x), .by = g)), ".by string", native = FALSE)
})
