# Spike S2: Rust reductions / libm / R_pow vs R, and the ldsum.c helper vs R.
# usage: Rscript compare.R <apply-binary> <ldsum.so> <tmpdir>
args <- commandArgs(TRUE); bin <- args[1]; so <- args[2]; tmp <- args[3]
dyn.load(so)
cat(R.version.string, R.version$platform, " sizeof(long double) =", .Call("ld_size"),
    " capabilities('long.double') =", capabilities("long.double"), "\n")
wr <- function(x, y = NULL) { f <- tempfile(tmpdir = tmp, fileext = ".bin"); writeBin(c(length(x), x, y), f, endian = "little"); f }
run <- function(op, x, y = NULL) {
  i <- wr(x, y); o <- tempfile(tmpdir = tmp, fileext = ".bin")
  ns <- as.numeric(system2(bin, c(op, i, o), stdout = TRUE))
  r <- readBin(o, "double", n = max(1, length(x)) + 1, endian = "little"); unlink(c(i, o)); list(r = r, ns = ns)
}
set.seed(20261117)
specials <- c(NA, NaN, Inf, -Inf, 0, -0, .Machine$double.xmax, -.Machine$double.xmax, .Machine$double.xmin, 5e-324)
mk <- function(n, kind) switch(kind,
  unif = runif(n),
  wide = (runif(n) - 0.5) * 10^runif(n, -300, 300),
  cancel = c(rnorm(n / 2) * 1e16, rnorm(n / 2)),
  na_nan = { v <- rnorm(n); v[sample.int(n, max(1, n %/% 100))] <- sample(c(NA, NaN), max(1, n %/% 100), TRUE); v },
  nan_first = { v <- rnorm(n); v[1] <- NaN; v[n] <- NA; v },
  na_first  = { v <- rnorm(n); v[1] <- NA; v[n] <- NaN; v },
  inf = { v <- rnorm(n); v[c(1, n)] <- c(Inf, -Inf); v },
  overflow = rep(.Machine$double.xmax / 3, n),
  specials = sample(c(specials, rnorm(10)), n, TRUE))
res <- list()
add <- function(...) res[[length(res) + 1]] <<- data.frame(...)
for (n in c(1e2, 1e4, 1e6, 1e7)) for (kind in c("unif", "wide", "cancel", "na_nan", "nan_first", "na_first", "inf", "overflow", "specials")) {
  if (n == 1e7 && !(kind %in% c("unif", "wide", "cancel"))) next
  x <- mk(n, kind)
  for (op in c("sum", "sum_narm", "mean", "cumsum")) {
    want <- switch(op, sum = sum(x), sum_narm = sum(x, na.rm = TRUE), mean = mean(x), cumsum = cumsum(x))
    got <- run(op, x); g <- got$r[seq_along(want)]
    ld <- switch(op, sum = .Call("ld_sum", x), mean = .Call("ld_mean", x), cumsum = { v <- .Call("ld_cumsum", x); if (is.nan(v[n])) want else v }, NULL)
    add(test = op, n = n, input = kind, rust_identical = identical(g, want),
        ldsum_identical = if (is.null(ld)) NA else identical(ld, want), rust_ns = got$ns)
  }
}
# libm and ^ over 1e7 inputs
n <- 1e7
xs <- list(
  exp = c(runif(n / 2, -745, 710), rnorm(n / 2) * 10, rnorm(1e5) * 1e-8, specials),
  log = c(runif(n / 2) * 10^runif(n / 2, -300, 300), runif(n / 2, 0.5, 2), specials),
  sqrt = c(runif(n) * 10^runif(n, -300, 300), (0:1e5)^2, specials),
  log2 = c(runif(n) * 10^runif(n, -300, 300), 2^(-1074:1023), specials),
  log10 = c(runif(n) * 10^runif(n, -300, 300), 10^(-300:300), specials),
  log1p = c(runif(n / 2, -1, 1), runif(n / 2) * 10^runif(n / 2, -300, 300), rnorm(1e5) * 1e-16, specials),
  expm1 = c(runif(n / 2, -1, 1), runif(n / 2, -50, 700), rnorm(1e5) * 1e-16, specials))
fn <- list(exp = exp, log = log, sqrt = sqrt, log2 = log2, log10 = log10, log1p = log1p, expm1 = expm1)
for (op in names(xs)) {
  x <- xs[[op]]; want <- fn[[op]](x); got <- run(op, x); g <- got$r[seq_along(want)]
  add(test = op, n = length(x), input = "1e7 wide", rust_identical = identical(g, want),
      ldsum_identical = NA, rust_ns = got$ns)
  if (!identical(g, want)) cat(op, "differs in", sum(g != want | xor(is.na(g), is.na(want)), na.rm = TRUE), "\n")
}
for (op in c("exp", "log1p")) { x <- xs[[op]]; got <- run(paste0(op, "_libm"), x); g <- got$r[seq_along(x)]
  add(test = paste(op, "(plain libm, no R wrapper)"), n = length(x), input = "1e7 wide", rust_identical = identical(g, fn[[op]](x)), ldsum_identical = NA, rust_ns = got$ns) }
px <- c((runif(n) - 0.5) * 10^runif(n, -5, 5), sample(c(specials, -11:11, 0.5, -0.5), 1e5, TRUE))
py <- c(rnorm(n) * 10, sample(c(specials, 0:4, 0.5, -1, -2, 1/3), 1e5, TRUE))
want <- px^py; got <- run("pow", px, py); g <- got$r[seq_along(want)]
add(test = "pow (x^y)", n = length(px), input = "1e7 + specials", rust_identical = identical(g, want), ldsum_identical = NA, rust_ns = got$ns)
res <- do.call(rbind, res); print(res, row.names = FALSE)
cat("\nALL RUST IDENTICAL (R-ported functions):", all(res$rust_identical[!grepl("plain libm", res$test)]), "  ALL ldsum.c IDENTICAL:", all(res$ldsum_identical, na.rm = TRUE), "\n")
saveRDS(res, file.path(tmp, "s2-results.rds"))
