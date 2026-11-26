# The Rust expression evaluator (migration phase 5): every formula it takes gives
# R's value bit for bit, draws R's stream in R's order, and declines wherever R
# would warn or error, so the R engine then says it itself.

rx_platform <- function() {
  info <- rs_info()
  paste0(info[["target_arch"]], "-", info[["target_os"]], ", long double ",
         info[["long_double_bytes"]], " bytes, ", R.version.string, ", dplyr ",
         utils::packageVersion("dplyr"))
}

# R's value of `e` over `data`, as the engine evaluates a rule: base eval() in a
# scope holding the globals and the engine's `n()`
rx_r_value <- function(e, data, globals, n) {
  env <- list2env(c(globals, list(n = function() n, if_else = dplyr::if_else, runif = stats::runif,
                                  case_when = dplyr::case_when, coalesce = dplyr::coalesce,
                                  rnorm = stats::rnorm, "%in%" = in_rowwise)), parent = baseenv())
  warned <- FALSE
  val <- tryCatch(withCallingHandlers(eval(e, data, env), warning = function(w) {
    warned <<- TRUE
    invokeRestart("muffleWarning")
  }), error = function(err) structure(list(), class = "rx_r_error"))
  if (warned || inherits(val, "rx_r_error")) return(NULL)
  val
}

rx_rust_value <- function(e, data, globals, n) {
  cp <- rx_compile(e)
  if (is_rx_no(cp)) return(list(no = unclass(cp)))
  ins <- c(data, globals)[cp$syms]
  nb <- rx_number_calls(list(rx_bind(cp$ir, cp$syms)))
  prog <- RxProgram$new(nb$irs, -1L, nb$fns)
  out <- prog$eval(unname(ins), as.integer(n), as.integer(n), FALSE)
  if (is.null(out)) list(no = rs_rx_reason()) else list(value = out[[1]])
}

bits <- function(x) if (is.double(x)) sprintf("%016s", vapply(x, function(v) {
  paste(sprintf("%02x", rev(as.integer(writeBin(v, raw(), endian = "little")))), collapse = "")
}, "")) else format(x)

# a random formula over the columns d1 d2 (double), i1 (integer), l1 l2
# (logical), s1 (character) and the globals g1 (double), k1 (integer)
rx_random_expr <- function(depth, type = sample(c("num", "lgl", "chr"), 1, prob = c(5, 3, 1))) {
  leaf <- function(type) switch(type,
    num = sample(list(quote(d1), quote(d2), quote(i1), quote(g1), quote(k1), 2.5, -1, 0L,
                      3L, NA_real_, NA_integer_, 1e300, -0, Inf), 1)[[1]],
    lgl = sample(list(quote(l1), quote(l2), TRUE, FALSE, NA), 1)[[1]],
    chr = sample(list(quote(s1), "a", "b", NA_character_), 1)[[1]])
  if (depth <= 0 || stats::runif(1) < 0.25) return(leaf(type))
  sub <- function(t = type) rx_random_expr(depth - 1, t)
  switch(type,
    num = switch(sample(17, 1),
      call(sample(c("+", "-", "*", "/"), 1), sub(), sub()),
      call("-", sub()),
      call("if_else", sub("lgl"), sub(), sub()),
      call("case_when", call("~", sub("lgl"), sub()), call("~", sub("lgl"), sub()), .default = sub()),
      call(sample(c("pmin", "pmax"), 1), sub(), sub()),
      call(sample(c("exp", "log", "sqrt"), 1), sub()),
      call(sample(c("sum", "mean"), 1), sub()),
      quote(runif(n())),
      quote(n()),
      # FR3
      call("^", sub(), sub()),
      call(sample(c("abs", "cumsum", "round", "floor", "sign", "max", "as.integer"), 1), sub()),
      call("coalesce", sub(), sub()),
      call(sample(c("sum", "mean"), 1), sub(), na.rm = TRUE),
      call(sample(c("runif", "rnorm"), 1), quote(n()), sub(), sub()),
      call("sample", sample(list(quote(i1), quote(d2), 3L, quote(n())), 1)[[1]], quote(n()),
           replace = TRUE),
      call("[", sample(list(quote(d1), quote(i1), quote(d2)), 1)[[1]],
           quote(sample(n(), n(), replace = TRUE))),
      call(sample(c("%%", "%/%"), 1), sub(), sub())),
    lgl = switch(sample(7, 1),
      call(sample(c("<", ">", "<=", ">=", "==", "!="), 1), sub("num"), sub("num")),
      call(sample(c("==", "!="), 1), sub("chr"), sub("chr")),
      call(sample(c("&", "|"), 1), sub(), sub()),
      call("!", sub()),
      call("is.na", sub(sample(c("num", "chr", "lgl"), 1))),
      call("if_else", sub(), sub(), sub()),
      call("%in%", sub(sample(c("num", "chr"), 1)), sample(list(quote(c(1, 2.5, 3)),
           quote(c(-1L, 3L)), quote(c("a", NA)), quote(i1)), 1)[[1]])),
    chr = switch(sample(3, 1),
      call("coalesce", sub(), sub()),
      call("if_else", sub("lgl"), sub(), sub()),
      call("case_when", call("~", sub("lgl"), sub()), .default = sub())))
}

test_that("random formulas give R's bits, R's draws, or a decline where R warns", {
  skip_on_cran()
  set.seed(20261120)
  n <- 12L
  cases <- if (identical(Sys.getenv("NOT_CRAN"), "true")) 3000L else 300L
  agree <- 0L
  declined <- 0L
  for (k in seq_len(cases)) {
    data <- list(
      d1 = sample(c(stats::rnorm(n, sd = 10), NA, NaN, Inf, -Inf, 0, -0, 1e-310), n, TRUE),
      d2 = stats::runif(n, -2, 5),
      i1 = sample(c(-3:3, NA, .Machine$integer.max, -.Machine$integer.max), n, TRUE),
      l1 = sample(c(TRUE, FALSE, NA), n, TRUE),
      l2 = stats::runif(n) < 0.5,
      s1 = sample(c("a", "b", "c", NA), n, TRUE))
    globals <- list(g1 = stats::rnorm(1), k1 = sample(c(2L, NA), 1))
    e <- rx_random_expr(4)
    seed <- sample.int(1e6, 1)
    set.seed(seed)
    r <- rx_r_value(e, data, globals, n)
    seed_r <- .Random.seed
    set.seed(seed)
    s <- rx_rust_value(e, data, globals, n)
    seed_s <- .Random.seed
    if (!is.null(s$no)) {
      declined <- declined + 1L
      next
    }
    where <- paste0("\n  platform: ", rx_platform(), "\n  formula: ", deparse1(e))
    expect(!is.null(r), paste0("Rust gave a value where R warned or failed", where))
    if (is.null(r)) next
    if (length(r) == 1L) r <- rep(r, n)
    same <- identical(r, s$value)
    if (!same && typeof(r) == typeof(s$value) && length(r) == length(s$value)) {
      i <- which(bits(r) != bits(s$value))[1]
      where <- paste0(where, "\n  first difference at row ", i, ": R ", bits(r)[i],
                      ", Rust ", bits(s$value)[i])
    }
    expect(same, paste0("values differ", where))
    expect(identical(seed_r, seed_s), paste0(".Random.seed differs", where))
    agree <- agree + same
  }
  # the evaluator covers most of what it is given (the rest R evaluates)
  expect_gt(agree, cases * 0.4)
})

test_that("the engine path matches the R engine on compiled rules (\"both\" compares each step)", {
  m <- abm_setup(
    agents = list(
      a = abm_agents(n = 30, x = ~stats::runif(n, -1, 3), k = ~sample(c(1L, NA, 4L), n, TRUE),
                     s = ~sample(c("u", "v"), n, TRUE), ok = TRUE),
      b = abm_agents(n = 20, x = ~stats::rnorm(n), k = 2L, s = "v", ok = FALSE)),
    globals = list(rate = 0.3, tot = 0), seed = 3)
  go <- abm_go(
    abm_rules(x ~ case_when(s == "u" & x > 1 ~ pmin(x, 2), is.na(k) ~ exp(x) / 10,
                            .default = if_else(ok, x - rate, log(x + 3))),
              k ~ k + 1L,
              ok ~ runif(n()) < rate | x > mean(x)),
    abm_global(tot ~ sum(x) / n(), cnt ~ sum(ok)))
  for (eng in c("rust", "both")) {
    withr::local_options(tidyABM.engine = "r")
    ref <- abm_run(m, go, ticks = 6, seed = 9)
    withr::local_options(tidyABM.engine = eng)
    expect_identical(abm_run(m, go, ticks = 6, seed = 9), ref)
  }
  cov <- rx_coverage(go)
  expect_true(all(cov$path == "rust"))
})

test_that("a compiled rule that would warn is left to R, which warns", {
  m <- abm_setup(agents = abm_agents(n = 5, x = c(1, 2, -3, 4, 5), i = .Machine$integer.max - 2L),
                 seed = 1)
  for (go in list(abm_go(abm_rules(y ~ log(x))), abm_go(abm_rules(i ~ i + 1L)))) {
    withr::local_options(tidyABM.engine = "r")
    w_r <- character()
    ref <- withCallingHandlers(abm_run(m, go, ticks = 2, seed = 1), warning = function(w) {
      w_r <<- c(w_r, conditionMessage(w)); invokeRestart("muffleWarning")
    })
    for (eng in c("rust", "both")) {
      withr::local_options(tidyABM.engine = eng)
      w_s <- character()
      got <- withCallingHandlers(abm_run(m, go, ticks = 2, seed = 1), warning = function(w) {
        w_s <<- c(w_s, conditionMessage(w)); invokeRestart("muffleWarning")
      })
      expect_identical(got, ref)
      expect_identical(w_s, w_r)
    }
  }
})

test_that("errors, masked functions and uncovered formulas are the R engine's", {
  m <- abm_setup(agents = abm_agents(n = 4, x = 1:4 / 2), seed = 1)
  bad <- abm_go(abm_rules(y ~ x + nope))
  withr::local_options(tidyABM.engine = "r")
  ref <- tryCatch(abm_run(m, bad, ticks = 1), error = function(e) conditionMessage(e))
  withr::local_options(tidyABM.engine = "rust")
  expect_identical(tryCatch(abm_run(m, bad, ticks = 1), error = function(e) conditionMessage(e)), ref)

  pmin <- function(...) base::pmax(...)
  masked <- abm_go(abm_rules(y ~ pmin(x, 1)))
  for (eng in c("rust", "both")) {
    withr::local_options(tidyABM.engine = "r")
    ref <- abm_run(m, masked, ticks = 1, seed = 1)
    withr::local_options(tidyABM.engine = eng)
    expect_identical(abm_run(m, masked, ticks = 1, seed = 1), ref)
  }
  # `.by` other than a name is R's (a name compiles; keys from a column are
  # left to R at run time)
  cov <- rx_coverage(abm_go(abm_rules(y ~ rank(x)^2, z ~ rank(x)),
                            abm_global(w ~ sum(x), .by = c(1, 2)), abm_global(w ~ sum(x), .by = j)))
  expect_identical(cov$path, c("r", "r", "r", "rust"))
  expect_match(cov$reason[[1]], "`rank()` is not compiled yet", fixed = TRUE)
  expect_match(cov$reason[[3]], ".by", fixed = TRUE)
})

test_that("strings compare by identity, and non-ASCII strings are left to R", {
  m <- abm_setup(agents = abm_agents(n = 4, s = c("café", "b", NA, "café")), seed = 1)
  latin <- iconv("café", "UTF-8", "latin1")
  go <- abm_go(abm_rules(hit ~ s == latin, other ~ s != "b"))
  withr::local_options(tidyABM.engine = "r")
  ref <- abm_run(m, go, ticks = 1, record = "all")
  for (eng in c("rust", "both")) {
    withr::local_options(tidyABM.engine = eng)
    expect_identical(abm_run(m, go, ticks = 1, record = "all"), ref)
  }
})


test_that("native min()/max() match R and .rx_cache stays bounded", {
  m <- abm_setup(agents = abm_agents(n = 6,
                                     x = c(1.5, -2, 3, 0, 4, -1),
                                     k = c(2L, -5L, 1L, 0L, 4L, 3L),
                                     flag = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE)),
                 seed = 1)
  go <- abm_go(
    abm_match(pair = "random"),
    abm_rules(
      mn_d ~ min(x, 0),
      mx_d ~ max(x, k, 1.5),
      mn_i ~ min(k, flag),
      mx_l ~ max(flag)
    )
  )
  expect_engines_agree(m, go, ticks = 3, seed = 2, what = "native min/max")
  # NA propagation and empty decline match R
  m_na <- abm_setup(agents = abm_agents(n = 4, x = c(1, NA_real_, 2, 3), k = c(1L, NA_integer_, 2L, 3L)))
  go_na <- abm_go(abm_rules(a ~ min(x), b ~ max(k)))
  expect_engines_agree(m_na, go_na, ticks = 1, seed = 1, what = "min/max NA")
  # fast-reject unknown call head matches rx_no format and stays bounded
  bad <- rx_compile(quote(unknown_fn(x + 1)))
  expect_true(is_rx_no(bad))
  expect_match(unclass(bad), "unknown_fn", fixed = TRUE)
  expect_true(length(.rx_cache) <= .rx_cache_max)
})

test_that("Tier 1 expression built-ins match R bit-for-bit", {
  # is.finite / is.infinite / is.nan / any / all / between / seq_len / seq_along
  m <- abm_setup(agents = abm_agents(
    n = 6,
    x = c(0, Inf, -Inf, NaN, NA_real_, 2.5),
    k = c(1L, NA_integer_, 3L, 0L, 5L, 2L),
    b = c(TRUE, FALSE, NA, TRUE, FALSE, NA),
    s = c("a", NA_character_, "b", "c", "d", "e")
  ), seed = 1)
  go <- abm_go(abm_rules(
    fd ~ is.finite(x), fi ~ is.finite(k), fl ~ is.finite(b), fs ~ is.finite(s),
    id ~ is.infinite(x), ii ~ is.infinite(k), is_ ~ is.infinite(s),
    nd ~ is.nan(x), ni ~ is.nan(k), ns ~ is.nan(s),
    a1 ~ any(b), a2 ~ any(b, na.rm = TRUE), a3 ~ all(b), a4 ~ all(b, na.rm = TRUE),
    bt ~ between(k, 1L, 3L), bt2 ~ between(x, 0, 3),
    sl ~ seq_len(n()), sa ~ seq_along(x)
  ))
  expect_engines_agree(m, go, ticks = 2, seed = 1, what = "Tier 1 unary/reduce/seq/between")
  expect_identical(.rl_last$stats[[2]], 0L)
  expect_identical(.rl_last$stats[[4]], 0L)

  # lazy base ifelse(): unused branch must NOT draw from RNG
  m_if <- abm_setup(agents = abm_agents(
    n = 8,
    p = c(TRUE, FALSE, NA, TRUE, FALSE, TRUE, FALSE, NA),
    t_all = TRUE, f_all = FALSE, na_all = NA,
    s = c("a", "b", "c", "d", "e", "f", "g", "h")
  ), seed = 2)
  go_if <- abm_go(abm_rules(
    v1 ~ ifelse(p, 1L, 2.5),
    v2 ~ ifelse(p, s, NA),
    v3 ~ ifelse(t_all, 1L, runif(n())),
    v4 ~ ifelse(f_all, runif(n()), 2L),
    v5 ~ ifelse(na_all, runif(n()), runif(n())),
    u  ~ runif(n())
  ))
  expect_engines_agree(m_if, go_if, ticks = 3, seed = 7, what = "Tier 1 lazy ifelse")
  expect_identical(.rl_last$stats[[2]], 0L)
  expect_identical(.rl_last$stats[[4]], 0L)

  # sample.int() and rbinom / rpois / rexp / rlnorm (n() and scalar 1)
  m_rng <- abm_setup(agents = abm_agents(n = 12, x = 0), seed = 3)
  go_rng <- abm_go(abm_rules(
    p1 ~ sample.int(n()),
    p2 ~ sample.int(n(), n(), replace = TRUE),
    rb ~ rbinom(n(), 5, 0.3),
    rb1 ~ rbinom(1, 4, 0.5),
    rp ~ rpois(n(), 2.5),
    rp1 ~ rpois(1, 3),
    re ~ rexp(n(), 0.5),
    re1 ~ rexp(1),
    rl ~ rlnorm(n(), 0.2, 0.4),
    rl1 ~ rlnorm(1)
  ))
  expect_engines_agree(m_rng, go_rng, ticks = 3, seed = 9, what = "Tier 1 sample.int & rdist")
  expect_identical(.rl_last$stats[[2]], 0L)
  expect_identical(.rl_last$stats[[4]], 0L)

  # invalid parameter declines to R (warning parity)
  go_bad <- abm_go(abm_rules(z ~ suppressWarnings(rexp(n(), 0))))
  expect_engines_agree(m_rng, go_bad, ticks = 1, seed = 1, what = "Tier 1 rdist decline")
})
