# Migration phase 2: reductions from Rust (and the long-double C helper
# src/reduce.c) give R's bits: same order, same accumulator, same NA/NaN rules.
# Doubles are compared bit for bit, which also tells NA from NaN.

specials <- c(NA, NaN, Inf, -Inf, 0, -0, .Machine$double.xmax, -.Machine$double.xmax,
              .Machine$double.xmin, 5e-324)

# the S2 spike's corpus (spikes/S2-reductions/compare.R)
corpus <- function(n) list(
  unif = runif(n),
  wide = (runif(n) - 0.5) * 10^runif(n, -300, 300),
  cancel = c(rnorm(n / 2) * 1e16, rnorm(n / 2)),
  na_nan = { v <- rnorm(n); v[sample.int(n, max(1, n %/% 100))] <- sample(c(NA, NaN), max(1, n %/% 100), TRUE); v },
  nan_first = { v <- rnorm(n); v[1] <- NaN; v[n] <- NA; v },
  na_first = { v <- rnorm(n); v[1] <- NA; v[n] <- NaN; v },
  inf = { v <- rnorm(n); v[c(1, n)] <- c(Inf, -Inf); v },
  overflow = rep(.Machine$double.xmax / 3, n),
  specials = sample(c(specials, rnorm(10)), n, TRUE))

double_ops <- list(
  sum = function(x) sum(x), sum_na_rm = function(x) sum(x, na.rm = TRUE),
  mean = function(x) mean(x), mean_na_rm = function(x) mean(x, na.rm = TRUE),
  cumsum = function(x) cumsum(x), max = function(x) max(x), min = function(x) min(x),
  max_na_rm = function(x) suppressWarnings(max(x, na.rm = TRUE)),
  min_na_rm = function(x) suppressWarnings(min(x, na.rm = TRUE)))

check_ops <- function(x, label, ops = double_ops) {
  for (op in names(ops)) {
    na_rm <- endsWith(op, "_na_rm")
    rust <- suppressWarnings(rust_reduce(sub("_na_rm$", "", op), x, na_rm))
    expect_same_as_r(rust, ops[[op]](x), paste(op, "of", label))
  }
}

test_that("double reductions match R bit for bit on the S2 corpus", {
  withr::local_seed(20261118)
  sizes <- c(1e2, 1e4, big_n(1e6, 1e4))
  for (n in sizes) for (kind in names(cs <- corpus(n))) {
    check_ops(cs[[kind]], paste0(kind, " (n = ", n, ")"))
  }
  if (identical(Sys.getenv("NOT_CRAN"), "true")) {
    cs <- corpus(1e7)[c("unif", "wide", "cancel")]
    for (kind in names(cs)) check_ops(cs[[kind]], paste0(kind, " (n = 1e7)"), double_ops[1:5])
  }
})

test_that("the Rust path is taken for every covered double reduction", {
  x <- c(1.5, NA, 3)
  for (op in c("sum", "mean", "cumsum", "max", "min")) {
    expect_false(is.null(rs_reduce(op, x, FALSE)), label = op)
  }
})

test_that("mean() keeps R's result when the sum overflows", {
  withr::local_seed(3)
  for (i in 1:300) {
    n <- sample(c(2:20, 100, 1000), 1)
    x <- sample(c(runif(n, 1.5e308, 1.79e308), -runif(n, 0, 1e308 * runif(1))))
    expect_same_as_r(rust_reduce("mean", x), mean(x), paste("overflowing mean", i))
  }
})

test_that("NA, NaN and infinities combine as in R, in every order", {
  vals <- c(1, NA, NaN, Inf, -Inf, -0, NA_real_ + 1) # the last: NA with quiet bits
  for (len in 1:3) {
    combos <- as.matrix(expand.grid(rep(list(seq_along(vals)), len)))
    for (r in seq_len(nrow(combos))) check_ops(vals[combos[r, ]], deparse(vals[combos[r, ]]))
  }
})

test_that("integer and logical reductions match R, NA and overflow included", {
  withr::local_seed(5)
  big <- .Machine$integer.max
  ints <- list(
    small = sample(-100:100, 1000, TRUE), with_na = c(3L, NA, -2L),
    na_first = c(NA, 1L), large = sample(c(big - 0:10, 1L), 1e5, TRUE),
    neg_large = -sample(c(big - 0:10, 1L), 1e5, TRUE),
    overflow = c(big, 1L), cum_overflow = c(big - 1L, 1L, 1L, NA),
    neg_overflow = c(-big, -1L), empty = integer(), one = 7L)
  lgls <- list(mixed = c(TRUE, FALSE, NA, TRUE), no_na = c(TRUE, FALSE, TRUE),
               all_true = rep(TRUE, 5), all_na = c(NA, NA), empty = logical(),
               lots = sample(c(TRUE, FALSE, NA), 1e4, TRUE))
  int_ops <- c(double_ops[c("sum", "sum_na_rm", "mean", "mean_na_rm", "cumsum", "max",
                            "min", "max_na_rm", "min_na_rm")])
  wrap <- function(f) function(x) suppressWarnings(f(x))
  int_ops <- lapply(int_ops, wrap)
  lgl_ops <- c(int_ops, list(any = function(x) any(x), all = function(x) all(x),
                             any_na_rm = function(x) any(x, na.rm = TRUE),
                             all_na_rm = function(x) all(x, na.rm = TRUE)))
  for (nm in names(ints)) check_ops(ints[[nm]], paste("integer", nm), int_ops)
  for (nm in names(lgls)) check_ops(lgls[[nm]], paste("logical", nm), lgl_ops)
  # where R warns or changes type (an integer sum that leaves the integer range
  # is NA with a warning in older R, a double in newer R), Rust declines (NULL)
  # and R computes it
  expect_null(rs_reduce("sum", c(big, 1L), FALSE))
  expect_identical(suppressWarnings(rust_reduce("sum", c(big, 1L))),
                   suppressWarnings(sum(c(big, 1L))))
  expect_null(rs_reduce("cumsum", c(big, 1L), FALSE))
  expect_warning(rust_reduce("cumsum", c(big, 1L)), "integer overflow")
  expect_warning(expect_identical(rust_reduce("max", integer()), -Inf))
  expect_null(rs_reduce("any", 1:3, FALSE))
  expect_null(rs_reduce("sum", "a", FALSE))
})

test_that("empty vectors reduce as in R", {
  for (x in list(double(), integer(), logical())) {
    check_ops(x, paste("empty", typeof(x)), lapply(double_ops, function(f) function(x) suppressWarnings(f(x))))
  }
})

test_that("group sums are per-group sum(), and rowsum() where R makes them equal", {
  withr::local_seed(7)
  for (n in c(10, 1000, big_n(1e5))) {
    k <- 12L
    g <- sample.int(k, n, TRUE)
    x <- rnorm(n) * 10^runif(n, -10, 10)
    x[sample.int(n, max(1, n %/% 50))] <- NA
    for (na_rm in c(FALSE, TRUE)) {
      want <- vapply(split(x, factor(g, levels = seq_len(k))), sum, numeric(1), na.rm = na_rm,
                     USE.NAMES = FALSE)
      expect_same_as_r(rs_group_sum(x, g, k, na_rm), want,
                       paste0("group sums, n = ", n, ", na_rm = ", na_rm))
      # tell(.resolve = "sum") uses rowsum() only where sum_is_rowsum()
      if (sum_is_rowsum(x)) {
        rs <- rowsum(x, g, na.rm = na_rm)
        expect_same_as_r(rs_group_sum(x, g, k, na_rm)[as.integer(rownames(rs))],
                         unname(rs[, 1]),
                         paste0("rowsum, n = ", n, ", na_rm = ", na_rm))
      }
    }
  }
  expect_identical(rs_group_sum(c(1, 2), c(2L, 2L), 3L, FALSE), c(0, 3, 0))
  expect_error(rust_call(rs_group_sum(c(1, 2), c(1L, 4L), 3L, FALSE)),
               class = "tidyABM_rust_error")
  expect_error(rust_call(rs_group_sum(c(1, 2), 1L, 3L, FALSE)), class = "tidyABM_rust_error")
})

test_that("R_pow() through the C API is R's `^`, integer exponents included", {
  withr::local_seed(11)
  n <- big_n(1e6)
  px <- c((runif(n) - 0.5) * 10^runif(n, -5, 5), sample(c(specials, -11:11, 0.5, -0.5), 1e4, TRUE))
  py <- c(rnorm(n) * 10, sample(c(specials, 0:4, 0.5, -1, -2, 1 / 3), 1e4, TRUE))
  ok <- is.finite(px) | is.finite(py) # (-Inf)^xmax warns; tested in test-rust-rng.R
  expect_same_as_r(suppressWarnings(rs_pow(px, py)), suppressWarnings(px^py), "x^y")
  k <- sample(-30:30, length(px), TRUE)
  expect_same_as_r(rs_pow(px, as.double(k)), px^k, "x^k, integer k")
  expect_same_as_r(rs_pow(px, 2), px^2, "x^2")
  expect_same_as_r(rs_pow(px, 0.5), px^0.5, "x^0.5")
  expect_error(rust_call(rs_pow(1:2 + 0, c(1, 2, 3))), class = "tidyABM_rust_error")
})
