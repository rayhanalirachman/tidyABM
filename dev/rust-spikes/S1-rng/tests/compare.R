# Spike S1: compare the pure-Rust RNG port (target/release/dump) with R itself.
# usage: Rscript compare.R <dump-binary> <tmpdir> [count=1e7]
# Every comparison is identical() on the draws AND on .Random.seed afterwards.
args <- commandArgs(TRUE); bin <- args[1]; tmp <- args[2]
count <- if (length(args) >= 3) as.numeric(args[3]) else 1e7
stopifnot(identical(RNGkind(), c("Mersenne-Twister", "Inversion", "Rejection")))
w <- function(n) as.numeric((seq_len(n) * 7919) %% 13 + 1)
# gen -> list(n values to try, count scale, R expression, result type)
specs <- list(
  unif    = list(ns = 0,   cnt = 1,     r = function(c, n) runif(c),  type = "double"),
  norm    = list(ns = 0,   cnt = 1,     r = function(c, n) rnorm(c),  type = "double"),
  exp     = list(ns = 0,   cnt = 1,     r = function(c, n) rexp(c),   type = "double"),
  idx     = list(ns = c(1, 7, 65536, 65537, 1e6, 2147483647), cnt = 0.1,
                 r = function(c, n) sample.int(n, c, replace = TRUE), type = "integer"),
  fy      = list(ns = c(10, 441, 1e5), cnt = 0.01,
                 r = function(c, n) unlist(lapply(seq_len(c), function(i) sample.int(n, min(n, 10)))), type = "integer"),
  sample2 = list(ns = 0,   cnt = 1e-3,
                 r = function(c, n) sample.int(.Machine$integer.max, c), type = "integer"),
  probr   = list(ns = c(5, 50, 199, 250, 1000), cnt = 0.01,
                 r = function(c, n) sample.int(n, c, replace = TRUE, prob = w(n)), type = "integer"),
  probnr  = list(ns = c(10, 50, 300), cnt = 1e-3,
                 r = function(c, n) unlist(lapply(seq_len(c), function(i) sample.int(n, min(n, 10), prob = w(n)))), type = "integer")
)
res <- list()
for (g in names(specs)) {
  s <- specs[[g]]
  for (n in s$ns) for (seed in c(1L, 2L, 42L, 2026L, -7L)) {
    cnt <- max(1, round(count * s$cnt))
    f <- file.path(tmp, sprintf("%s-%s-%s.bin", g, seed, n))
    msg <- system2(bin, c(g, seed, format(cnt, scientific = FALSE), format(n, scientific = FALSE), f), stdout = TRUE)
    set.seed(seed); want <- s$r(cnt, n); want_seed <- .Random.seed
    con <- file(f, "rb")
    got <- if (s$type == "double") readBin(con, "double", length(want), 8, endian = "little")
           else readBin(con, "integer", length(want), 4, endian = "little")
    got_seed <- readBin(con, "integer", 626L, 4, endian = "little"); close(con); unlink(f)
    ok <- identical(got, want) && identical(got_seed, want_seed)
    nd <- if (length(got) == length(want)) sum(got != want) else NA
    res[[length(res) + 1]] <- data.frame(gen = g, n = n, seed = seed, draws = length(want),
      identical = ok, n_differ = nd, seed_identical = identical(got_seed, want_seed),
      rust_ns = as.numeric(sub(".*: ([0-9.]+) ns.*", "\\1", msg)))
    cat(sprintf("%-8s n=%-10s seed=%-5d draws=%-9d identical=%s differ=%s\n", g, format(n), seed, length(want), ok, nd))
  }
}
res <- do.call(rbind, res)
cat("\nALL IDENTICAL:", all(res$identical), " (", sum(res$identical), "/", nrow(res), ")\n")
# R-side speed of the same generators, for scale
t <- system.time(runif(1e7))[["elapsed"]]; cat(sprintf("R runif(1e7): %.1f ns per draw\n", t * 100))
t <- system.time(rnorm(1e7))[["elapsed"]]; cat(sprintf("R rnorm(1e7): %.1f ns per draw\n", t * 100))
saveRDS(res, file.path(tmp, "s1-results.rds"))
print(aggregate(cbind(identical, rust_ns) ~ gen, res, function(x) c(mean = mean(x))))
