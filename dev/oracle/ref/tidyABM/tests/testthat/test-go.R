test_that("an empty sequence is rejected", {
  withr::local_seed(1001)
  expect_error(abm_go(), class = "tidyABM_empty_go")
})

test_that("non-steps are rejected", {
  withr::local_seed(1002)
  expect_error(abm_go(abm_rules(a ~ b), 1), class = "tidyABM_not_a_step")
})

test_that("two matches in a row are rejected", {
  withr::local_seed(1003)
  expect_error(
    abm_go(abm_match(pair = "random"), abm_match(pair = "random"),
           abm_rules(a ~ b)),
    class = "tidyABM_bad_sequence"
  )
})

test_that("a sequence cannot end on a bare match", {
  withr::local_seed(1004)
  expect_error(
    abm_go(abm_rules(a ~ b), abm_match(pair = "random")),
    class = "tidyABM_bad_sequence"
  )
})

test_that("valid shapes are accepted", {
  withr::local_seed(1005)
  # no matching at all
  expect_s3_class(abm_go(abm_rules(a ~ b), abm_global(g ~ sum(a))), "abm_go")
  # several update steps after one match
  expect_s3_class(
    abm_go(abm_match(pair = "random"), abm_rules(a ~ b), abm_rules(c ~ d)),
    "abm_go"
  )
  # several phases
  expect_s3_class(
    abm_go(abm_match(pair = "random"), abm_rules(a ~ b),
           abm_match(pair = "random"), abm_rules(c ~ d)),
    "abm_go"
  )
})

test_that("rules must be two-sided formulas", {
  withr::local_seed(1006)
  expect_error(abm_rules(~a), class = "tidyABM_bad_formula")
  expect_error(abm_rules(), class = "tidyABM_empty_step")
  expect_error(abm_rules(f(x) ~ 1), class = "tidyABM_bad_formula")
})
