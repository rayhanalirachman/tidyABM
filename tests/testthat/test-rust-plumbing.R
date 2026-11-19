# The Rust library and the engine switch (migration phase 1): the build, the calls
# and the error transport. The Rust run path itself is tested in test-rust-engine.R.

test_that("the engine option defaults to the R engine and is validated", {
  withr::local_options(tidyABM.engine = NULL)
  expect_identical(engine_resolve(), "r")
  for (engine in c("r", "auto", "rust", "both")) {
    withr::local_options(tidyABM.engine = engine)
    expect_identical(engine_resolve(), engine)
  }
  for (bad in list("fast", c("r", "auto"), 1, NA_character_, TRUE)) {
    withr::local_options(tidyABM.engine = bad)
    expect_error(engine_resolve(), class = "tidyABM_bad_engine")
  }
})

test_that("abm_run() checks the engine option", {
  m <- abm_setup(agents = abm_agents(n = 5, x = ~runif(n)), seed = 1)
  go <- abm_go(abm_rules(x ~ x + runif(n())))
  withr::local_options(tidyABM.engine = NULL)
  ref <- abm_run(m, go, ticks = 3, seed = 2)
  withr::local_options(tidyABM.engine = "auto")
  expect_identical(abm_run(m, go, ticks = 3, seed = 2), ref)
  withr::local_options(tidyABM.engine = "fast")
  expect_snapshot(abm_run(m, go, ticks = 3, seed = 2), error = TRUE)
})

test_that("the Rust library is loaded and answers", {
  info <- rust_info()
  expect_type(info, "character")
  expect_named(info, c("version", "target_arch", "target_os", "panic", "long_double_bytes"))
  expect_match(info[["long_double_bytes"]], "^(8|10|12|16)$")
  expect_match(info[["version"]], "^[0-9]+\\.[0-9]+\\.[0-9]+$")
  expect_identical(info[["panic"]], "unwind")
  expect_null(rs_noop())
})

test_that("doubles cross the boundary bit for bit", {
  x <- c(0, -0, 1.5, NA_real_, NaN, Inf, -Inf, .Machine$double.xmin, 1 / 3)
  y <- rs_copy_doubles(x)
  expect_identical(y, x)
  # NA_real_ and NaN keep their payloads (R tells them apart by the low word)
  expect_identical(is.na(y) & !is.nan(y), is.na(x) & !is.nan(x))
  expect_identical(rs_copy_doubles(double()), double())
  # savvy does not coerce: an integer vector is an R error, the session goes on
  expect_error(rs_copy_doubles(1:3))
})

test_that("a typed Rust error becomes a classed tidyABM error", {
  err <- expect_error(rust_call(rs_test_error("bad input")),
                      class = "tidyABM_rust_error")
  expect_s3_class(err, "tidyABM_rust")
  expect_s3_class(err, "tidyABM_error")
  expect_match(conditionMessage(err), "bad input", fixed = TRUE)
})

test_that("a panic, on the main thread or a worker, becomes an R error", {
  for (worker in c(FALSE, TRUE)) {
    out <- capture.output(type = "message", {
      err <- expect_error(rust_call(rs_test_panic("index out of range", worker)),
                          class = "tidyABM_rust_panic")
    })
    expect_s3_class(err, "tidyABM_error")
    expect_match(conditionMessage(err), "index out of range", fixed = TRUE)
    # tidyABM's own panic hook is silent: savvy's would print from the worker thread
    expect_identical(out, character())
  }
  # the session survives and the library still answers
  expect_null(rs_noop())
})

test_that("R conditions raised inside a Rust call pass through unchanged", {
  err <- tryCatch(rust_call(stop(structure(
    class = c("my_condition", "error", "condition"),
    list(message = "from R", call = NULL)))), error = identity)
  expect_s3_class(err, "my_condition")
  expect_identical(conditionMessage(err), "from R")
})
