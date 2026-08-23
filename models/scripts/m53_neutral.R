# 53. Random copying / the neutral model of cultural change
#     (Bentley, Hahn & Shennan 2004, Proc. R. Soc. B 271: 1443-1450)
library(tidyABM)

neutral <- function(n = 500, mu = 0.01, ticks = 2000, seed = 1) {
  m <- abm_setup(
    agents  = abm_agents(n = n, variant = ~seq_len(n)),
    globals = list(coined = n),
    seed    = seed
  )
  go <- abm_go(
    abm_rules(copied   ~ sample(variant, n(), replace = TRUE),
              innovate ~ runif(n()) < mu,
              .scope = "population"),
    abm_rules(variant ~ if_else(innovate, coined + cumsum(innovate), copied),
              .scope = "population"),
    abm_global(coined ~ coined + sum(innovate))
  )
  abm_run(m, go, ticks = ticks, seed = seed)
}

# Ewens: expected number of distinct variants in a sample of n,
# E[k] = sum_{i=0}^{n-1} theta / (theta + i), with theta = 2*n*mu
ewens_k <- function(n, mu) { th <- 2 * n * mu; sum(th / (th + seq_len(n) - 1)) }

# log-log slope of the complementary cumulative frequency distribution,
# pooled over the last 500 ticks
ccdf_slope <- function(r) {
  late <- r[r$tick > max(r$tick) - 500, ]
  f <- unlist(lapply(split(late$variant, late$tick), function(v) as.numeric(table(v))))
  f <- sort(f, decreasing = TRUE)
  d <- data.frame(x = log(f), y = log(seq_along(f) / length(f)))
  d <- d[f > 1, ]
  if (nrow(d) < 5) return(NA_real_)
  unname(-coef(stats::lm(y ~ x, d))[2])
}

if (sys.nframe() == 0L) {
  cat(sprintf("%6s %8s %10s %10s %10s\n", "N", "mu", "variants", "Ewens",
              "ccdf slope"))
  for (par in list(c(500, 0.001), c(500, 0.005), c(500, 0.01), c(500, 0.05),
                   c(2000, 0.0025))) {
    n <- par[1]; mu <- par[2]
    r <- neutral(n = n, mu = mu, ticks = 3000, seed = 1)
    late <- r[r$tick > 2000, ]
    k <- mean(tapply(late$variant, late$tick, function(v) length(unique(v))))
    cat(sprintf("%6d %8.4f %10.1f %10.1f %10.2f\n", n, mu, k,
                ewens_k(n, mu), ccdf_slope(r)))
  }
}
