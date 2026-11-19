# Shared by the Rust differential tests (test-rust-rng.R, test-rust-reduce.R). A
# failure names the platform, the long double size, the RNG kind and the first
# differing element, so a CI log is enough to see what differs where.

rust_platform <- function() {
  paste0(R.version$platform, ", ", R.version.string, ", long double ",
         rust_info()[["long_double_bytes"]], " bytes (R: ",
         .Machine$sizeof.longdouble, "), RNGkind ", paste(RNGkind(), collapse = "/"))
}

# NA and NaN told apart, and doubles compared bit for bit (identical() treats all
# NaN payloads and both zeros alike)
same_bits <- function(a, b) {
  identical(a, b) &&
    (!is.double(a) || identical(writeBin(as.vector(a), raw()), writeBin(as.vector(b), raw())))
}

first_diff <- function(rust, r) {
  if (!identical(typeof(rust), typeof(r)) || length(rust) != length(r)) {
    return(sprintf("Rust gives %s[%d], R %s[%d]", typeof(rust), length(rust),
                   typeof(r), length(r)))
  }
  for (i in seq_along(r)) {
    if (!same_bits(rust[i], r[i])) {
      show <- function(v) if (is.double(v)) sprintf("%.17g (%s)", v,
        paste(rev(as.character(writeBin(v, raw()))), collapse = "")) else format(v)
      return(sprintf("first difference at [%d] of %d: Rust %s, R %s", i, length(r),
                     show(rust[i]), show(r[i])))
    }
  }
  "attributes differ"
}

expect_same_as_r <- function(rust, r, what) {
  ok <- same_bits(rust, r)
  testthat::expect(ok, if (!ok) paste0(what, ": Rust and R differ on ", rust_platform(),
                                       "; ", first_diff(rust, r)) else "")
  invisible(ok)
}

big_n <- function(full, cran = 1e4) {
  if (identical(Sys.getenv("NOT_CRAN"), "true")) full else min(full, cran)
}

rng_seed <- function() get(".Random.seed", envir = globalenv())

# Run `rust` and `r` from the same seed; compare the values and `.Random.seed`.
expect_same_draws <- function(rust, r, seed, what) {
  set.seed(seed)
  a <- rust()
  sa <- rng_seed()
  set.seed(seed)
  b <- r()
  expect_same_as_r(a, b, paste0(what, ", seed ", seed))
  expect_same_as_r(sa, rng_seed(), paste0(what, ", .Random.seed after, seed ", seed))
}

# The engines (test-rust-engine.R, test-rust-kernels.R)
run_in <- function(engine, ...) {
  withr::local_options(tidyABM.engine = engine)
  abm_run(...)
}

# the same call under every engine: identical results and the same random state after
expect_engines_agree <- function(..., what = "model") {
  withr::local_seed(99)
  ref <- run_in("r", ...)
  seed_r <- rng_get()
  for (engine in c("auto", "rust", "both")) {
    withr::local_seed(99)
    got <- run_in(engine, ...)
    expect(identical(got, ref),
           paste0(what, ": engine \"", engine, "\" differs from \"r\" on ", rust_platform(),
                  "; ", diff_where(ref, got) %||% "?"))
    for (a in c("globals", "measures", "network", "relations")) {
      expect(identical(attr(got, a), attr(ref, a)), paste(what, engine, a))
    }
    expect_identical(rng_get(), seed_r, label = paste(what, engine, ".Random.seed after"))
  }
  invisible(ref)
}
