test_that("eval_by_key() gives what a grouped mutate() gives, draw for draw", {
  set.seed(1)
  aug <- tibble::tibble(k = sample(c(3L, 1L, NA, 2L), 60, TRUE), x = runif(60),
                        g = sample(c("a", "b"), 60, TRUE))
  globals <- list(scale = 2)
  rules <- list(
    quote(x / sum(x)),                                   # elementwise around an aggregate
    quote(if_else(g == "a", sum(x[g == "b"]), 0)),       # the Gross wage rule's shape
    quote(x - mean(x) + n() * scale),
    quote(sum(x)),                                       # one value per group
    quote(x * 2),                                        # no aggregate at all
    quote(ifelse(sum(x) > 1, x, -x)),                    # not hoisted: `ifelse()` sizes by its test
    quote(sample(x, 1) + runif(n())),                    # draws: per group, in group order
    quote(cumsum(x))                                     # neither elementwise nor an aggregate
  )
  for (e in rules) {
    quo <- rlang::new_quosure(e, environment())
    set.seed(2)
    fast <- eval_by_key(quo, aug, globals, aug$k)
    after_fast <- stats::runif(1)
    set.seed(2)
    slow <- dplyr::pull(dplyr::mutate(
      dplyr::group_by(aug, .data$k),
      .v = !!rlang::quo_set_env(quo, abm_eval_env(quo, globals))), ".v")
    expect_identical(fast, slow, info = deparse1(e))
    expect_identical(after_fast, stats::runif(1), info = deparse1(e))
  }
  # anything it does not cover is left to mutate(): a character key, a mask helper
  quo <- rlang::quo(x / sum(x))
  expect_null(eval_by_key(quo, aug, globals, aug$g))
  expect_null(eval_by_key(rlang::quo(row_number()), aug, globals, aug$k))
  expect_null(eval_by_key(rlang::quo(x[1:2]), aug, globals, aug$k))
})
