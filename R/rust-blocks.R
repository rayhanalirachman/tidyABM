# Building blocks of the Rust engine (migration phase 2): R's random number
# generator reached from Rust, and reductions with R's order and accumulator.
# Nothing in `abm_run()` uses them yet; the kernels of phases 3-4 will. They are
# tested against R with `identical()` in tests/testthat/test-rust-rng.R and
# test-rust-reduce.R.
#
# Every draw is made by R's own generator through its public C API (unif_rand,
# norm_rand, exp_rand, R_unif_index and the Rmath r* functions), so the stream
# follows `RNGkind()` and stays R's stream, interleaved with R's own draws. The
# Rust path is still only taken under the kinds the tests verified
# (`rust_rng_ok()`); anything else, and any parameter for which R would warn or
# return NaN, takes R's own function. Weighted sampling (`sample(prob = )`) stays
# in R: its tie order cannot be reproduced through the public API.

#' RNG kinds under which the Rust draws were checked identical to R's
#'
#' All built-in kinds of R 4.6 (7 x 5 x 2 combinations, test-rust-rng.R). A
#' "user-supplied" kind or a kind added by a later R version is not listed, so the
#' R path is used for it.
#' @noRd
rust_rng_kinds <- list(
  kind = c("Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper",
           "Mersenne-Twister", "Knuth-TAOCP", "Knuth-TAOCP-2002", "L'Ecuyer-CMRG"),
  normal = c("Buggy Kinderman-Ramage", "Ahrens-Dieter", "Box-Muller",
             "Inversion", "Kinderman-Ramage"),
  sample = c("Rounding", "Rejection"))

#' May the Rust path draw now?
#'
#' TRUE when the current `RNGkind()` is one the tests verified. R-devel adds a 4th
#' element (the binomial kind); binomial draws take the Rust path only under its
#' default, "BTPE", the algorithm of R's exported `rbinom()`.
#' @noRd
rust_rng_ok <- function(draw = "", k = RNGkind()) {
  all(k[[1L]] %in% rust_rng_kinds$kind, k[[2L]] %in% rust_rng_kinds$normal,
      k[[3L]] %in% rust_rng_kinds$sample) &&
    (draw != "rbinom" || length(k) < 4L || identical(k[[4L]], "BTPE"))
}

#' `n` draws of one kind, from Rust when allowed, else from R
#'
#' `kind`: "unif", "norm", "exp" (`runif(n)`, `rnorm(n)`, `rexp(n)`), "index"
#' (`sample.int(a, n, TRUE) - 1` as doubles), or "runif", "rnorm", "rlnorm",
#' "rexp", "rpois", "rbinom" with parameters `a`, `b` in R's order.
#' @noRd
rust_draw <- function(kind, n, a = 0, b = 0) {
  out <- if (rust_rng_ok(kind)) {
    rust_call(rs_draw(kind, as.integer(n), as.double(a), as.double(b)))
  }
  out %||% switch(kind,
    unif = stats::runif(n), norm = stats::rnorm(n), exp = stats::rexp(n),
    index = as.double(sample.int(a, n, replace = TRUE) - 1L),
    runif = stats::runif(n, a, b), rnorm = stats::rnorm(n, a, b),
    rlnorm = stats::rlnorm(n, a, b), rexp = stats::rexp(n, a),
    rpois = stats::rpois(n, a), rbinom = stats::rbinom(n, a, b))
}

#' `sample.int(n, size, replace)`, from Rust when allowed
#' @noRd
rust_sample_int <- function(n, size = n, replace = FALSE) {
  if (!rust_rng_ok()) return(sample.int(n, size, replace))
  rust_call(rs_sample_int(as.integer(n), as.integer(size), replace))
}

#' `runif(n) < p`, from Rust when allowed (`p` of length 1 or `n`)
#' @noRd
rust_bernoulli <- function(n, p) {
  if (!rust_rng_ok()) return(stats::runif(n) < p)
  rust_call(rs_bernoulli(as.integer(n), as.double(p)))
}

#' A reduction with R's order, accumulator and NA rules
#'
#' `op` is "sum", "mean", "cumsum", "max", "min", "any" or "all"; `x` a bare
#' double, integer or logical vector. Where R would warn (integer overflow, the
#' extremes of an empty vector) or the type is not covered, R computes it.
#' @noRd
rust_reduce <- function(op, x, na_rm = FALSE) {
  rust_call(rs_reduce(op, x, na_rm)) %||%
    if (op == "cumsum") cumsum(x) else match.fun(op)(x, na.rm = na_rm)
}
