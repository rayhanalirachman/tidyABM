# The engine switch and the R side of the Rust bridge (migration phases 1 and 3).
#
# `options(tidyABM.engine = )` picks the engine a run uses:
#   "r"    the R engine (the default, and the reference implementation);
#   "auto" the Rust path for a run with at least one step that has a Rust kernel,
#          else the R engine;
#   "rust" the Rust path: the groups in the Rust column store, every step without
#          a kernel run by the R engine's step function through the step adapter;
#   "both" each step on the R engine and on the Rust path, compared after every
#          step (a developer and CI tool: user code in a step runs twice).
# Phase 3 has the store and the adapter but no kernel, so "auto" runs the R engine
# (R/run-rust.R).

engine_choices <- c("auto", "r", "rust", "both")

#' The engine a run uses, from `getOption("tidyABM.engine")`
#' @noRd
engine_resolve <- function(call = rlang::caller_env()) {
  engine <- getOption("tidyABM.engine", "r")
  if (!rlang::is_string(engine) || !engine %in% engine_choices) {
    abm_abort(
      c("{.code options(tidyABM.engine = )} must be one of {.or {.val {engine_choices}}}.",
        "x" = if (rlang::is_string(engine)) "It is {.val {engine}}."
              else "It is {.obj_type_friendly {engine}}."),
      class = "tidyABM_bad_engine", call = call)
  }
  engine
}

#' Call into the Rust library, raising its errors as tidyABM conditions
#'
#' Rust returns typed errors as messages with a prefix and never formats what the
#' user reads (PLAN.md design rule 1); this maps them to classed R errors. R errors
#' raised inside the call (through savvy's unwind protection) pass through unchanged.
#' @noRd
rust_call <- function(expr, call = rlang::caller_env()) {
  tryCatch(expr, error = function(e) {
    msg <- conditionMessage(e)
    for (k in list(c("tidyABM-rust-panic: ", "tidyABM_rust_panic",
                     "tidyABM's Rust code stopped unexpectedly (a panic)."),
                   c("tidyABM-rust-error: ", "tidyABM_rust_error",
                     "tidyABM's Rust code returned an error."))) {
      if (startsWith(msg, k[[1]])) {
        detail <- substring(msg, nchar(k[[1]]) + 1L)
        abm_abort(c(k[[3]], "x" = "{detail}",
                    "i" = if (k[[2]] == "tidyABM_rust_panic") "This is a bug in tidyABM; please report it."),
                  class = c(k[[2]], "tidyABM_rust"), call = call)
      }
    }
    stop(e)
  })
}

#' Facts about the compiled Rust library
#'
#' @return A named character vector: `version` (the Rust engine's crate version),
#'   `target_arch`, `target_os`, `panic` (the panic strategy, `"unwind"` in
#'   every build) and `long_double_bytes` (`sizeof(long double)` of the C compiler,
#'   8 where R's sums accumulate in a plain double).
#' @noRd
rust_info <- function() rust_call(rs_info())
