# 49. The emergence of firms in a population of agents
#     (Axtell 1999, Brookings CSED WP 3; Axtell 2018, J. Econ. Dyn. Control)
library(tidyABM)

GRID <- seq(0, 1, by = 0.05)

# best effort, and the utility it yields, for an agent joining a firm whose
# other members already supply `S` effort and which will then have `k` members
best <- function(S, k, theta) {
  n <- max(length(S), length(k), length(theta))
  S <- rep_len(S, n); k <- rep_len(k, n); theta <- rep_len(theta, n)
  o <- outer(S, GRID, function(s, e) A_ * (s + e) + B_ * (s + e)^2)
  u <- (o / k)^theta * outer(rep(1, n), 1 - GRID)^(1 - theta)
  j <- max.col(u, ties.method = "first")
  list(e = GRID[j], u = u[cbind(seq_along(j), j)])
}
A_ <- 1; B_ <- 1

firms <- function(n = 500, ticks = 400, degree = 4, active = 0.1, seed = 1) {
  m <- abm_setup(
    agents  = abm_agents(n = n, theta = ~runif(n), effort = 0.2,
                         firm = ~seq_len(n), E = 0, sz = 1,
                         u_stay = 0, u_move = 0, u_solo = 0),
    network = abm_network(type = "random", degree = degree),
    seed    = seed
  )
  go <- abm_go(
    # the one aggregate this model is made of: what my firm produces, and how
    # many of us there are to share it
    abm_rules(E ~ sum(effort), sz ~ n(), .by = firm),
    abm_match(pair = "network", eligible = runif(n()) < active),
    abm_rules(
      u_stay ~ best(E - effort, sz,                theta)$u,
      u_solo ~ best(0,          1,                 theta)$u,
      u_move ~ if_else(is.na(.partner), -Inf,
                       best(partner_E, partner_sz + 1, theta)$u),
      .scope = "population"),
    abm_rules(
      firm ~ case_when(is.na(.partner)                     ~ firm,
                       u_move >= u_stay & u_move >= u_solo ~ partner_firm,
                       u_solo >= u_stay                    ~ .id,
                       TRUE                                ~ firm),
      .scope = "population"),
    abm_rules(E2 ~ sum(effort) - effort, sz2 ~ n(), .by = firm),
    abm_rules(effort ~ if_else(is.na(.partner), effort, best(E2, sz2, theta)$e),
              .scope = "population"),
    abm_global(n_firms ~ length(unique(firm)),
               biggest ~ max(table(firm)),
               mean_sz ~ n() / length(unique(firm)))
  )
  abm_run(m, go, ticks = ticks, seed = seed)
}

if (sys.nframe() == 0L) {
  cat("N = 500 agents, degree-4 friendship network, 400 periods, 5 seeds\n\n")
  cat(sprintf("%8s %8s %10s %10s %12s\n", "firms", "mean size", "biggest",
              "effort", "ccdf slope"))
  out <- vapply(1:5, function(s) {
    r <- firms(seed = s)
    f <- r[r$tick == max(r$tick), ]
    sz <- sort(as.numeric(table(f$firm)), decreasing = TRUE)
    cd <- data.frame(x = log(sz), y = log(seq_along(sz) / length(sz)))
    cd <- cd[sz > 1, ]
    c(length(sz), mean(sz), max(sz), mean(f$effort),
      if (nrow(cd) > 3) -stats::coef(stats::lm(y ~ x, cd))[[2]] else NA_real_)
  }, numeric(5))
  o <- rowMeans(out)
  cat(sprintf("%8.0f %8.2f %10.0f %10.3f %12.2f\n", o[1], o[2], o[3], o[4], o[5]))

  cat("\nturnover: the biggest firm at 5 snapshots of one run\n")
  r <- firms(seed = 1)
  r_fig <- r
  for (t in seq(80, 400, by = 80)) {
    f <- r[r$tick == t, ]
    tb <- sort(table(f$firm), decreasing = TRUE)
    cat(sprintf("  t = %3d   firm %-5s size %3d   firms %3d\n", t,
                names(tb)[1], tb[[1]], length(tb)))
  }

  # --- figure -------------------------------------------------------------
  # Final-state distribution: the firm-size CCDF on log-log axes, which is
  # where Axtell's power law is supposed to show as a straight line.
  library(ggplot2)

  fig_file <- function(name) {
    a <- grep("^--file=", commandArgs(), value = TRUE)
    d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
    dir.create(file.path(d, "..", "figures"), showWarnings = FALSE)
    file.path(d, "..", "figures", name)
  }

  f <- r_fig[r_fig$tick == max(r_fig$tick), ]
  sz <- sort(as.numeric(table(f$firm)), decreasing = TRUE)
  d <- data.frame(size = sz, ccdf = seq_along(sz) / length(sz))
  d <- d[d$size > 1, ]

  fig <- ggplot(d, aes(size, ccdf)) +
    geom_point(size = 1.2, alpha = 0.7) +
    scale_x_log10() + scale_y_log10() +
    theme_minimal() +
    labs(title = "Emergence of firms: a heavy-tailed size distribution",
         subtitle = "500 agents, degree-4 friendship network, 400 periods",
         x = "firm size", y = "share of firms at least this big")
  print(fig)
  ggsave(fig_file("49-emergence-of-firms.png"), fig, width = 6, height = 4,
         dpi = 120)
}
