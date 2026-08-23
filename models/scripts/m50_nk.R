# 50. Adaptation on a rugged landscape (Kauffman 1993; Levinthal 1997)
library(tidyABM)

make_nk <- function(N = 10, K = 0, seed = 1) {
  set.seed(seed)
  # for each locus, which loci it depends on, and a table of 2^(K+1) draws
  deps <- lapply(seq_len(N), function(i) ((i - 1 + 0:K) %% N) + 1L)
  tbl  <- matrix(runif(N * 2^(K + 1)), nrow = N)
  list(N = N, K = K, deps = deps, tbl = tbl)
}
fitness <- function(x, nk) {
  # x is a matrix, one row per organisation, one column per locus
  out <- matrix(0, nrow = nrow(x), ncol = nk$N)
  for (i in seq_len(nk$N)) {
    idx <- 1L + as.integer(x[, nk$deps[[i]], drop = FALSE] %*% 2^(0:nk$K))
    out[, i] <- nk$tbl[i, idx]
  }
  rowMeans(out)
}
fit1 <- function(v, nk) fitness(matrix(v, nrow = 1), nk)

landscape_search <- function(n_firms = 100, N = 10, K = 0, ticks = 200,
                             jump = 0, seed = 1) {
  nk <- make_nk(N = N, K = K, seed = seed)
  m <- abm_setup(
    agents = abm_agents(
      n = n_firms,
      form = ~lapply(seq_len(n), function(i) as.integer(runif(N) < 0.5))),
    seed = seed
  )
  go <- abm_go(
    abm_rules(
      trial ~ lapply(form, function(v) {
        if (runif(1) < jump) as.integer(runif(N) < 0.5)
        else { i <- sample(N, 1); v[i] <- 1L - v[i]; v }
      }),
      .scope = "population"),
    abm_rules(
      form ~ Map(function(a, b) if (fit1(b, nk) > fit1(a, nk)) b else a,
                 form, trial),
      .scope = "population")
  )
  r <- abm_run(m, go, ticks = ticks, seed = seed)
  f <- r[r$tick == ticks, ]
  list(fit   = mean(vapply(f$form, fit1, numeric(1), nk = nk)),
       forms = length(unique(vapply(f$form, paste, character(1), collapse = ""))))
}


if (sys.nframe() == 0L) {
  cat("N = 10 attributes, 100 organisations, 200 periods of search\n\n")
  cat(sprintf("%3s %12s %10s %12s %10s\n", "K", "fitness", "forms",
              "fit (jumps)", "forms"))
  for (K in c(0, 1, 3, 5, 9)) {
    a <- lapply(1:5, function(s) landscape_search(K = K, seed = s))
    b <- lapply(1:5, function(s) landscape_search(K = K, jump = 0.1, seed = s))
    cat(sprintf("%3d %12.3f %10.1f %12.3f %10.1f\n", K,
                mean(vapply(a, function(o) o$fit, numeric(1))),
                mean(vapply(a, function(o) o$forms, numeric(1))),
                mean(vapply(b, function(o) o$fit, numeric(1))),
                mean(vapply(b, function(o) o$forms, numeric(1)))))
  }
}
