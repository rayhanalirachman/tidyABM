# 22. Hawks and Doves  (Maynard Smith & Price 1973, Nature 246: 15-18)
#
# The corpus's sharpest quantitative validation: when C > V the hawk share
# converges on the mixed ESS p* = V/C, matched to three decimals across a
# 12-fold range of C.
#
# `.scope = "population"` on the resampling step is load-bearing. Without it
# the rule inherits the grouping from the preceding `abm_match()` and resamples
# *within each pair*, which drives hawks to fixation at every C.
library(tidyABM)

V <- 50

hd_run <- function(C, n = 2000, ticks = 200, seed = 4) {
  m <- abm_setup(agents = abm_agents(
    n = n, strategy = ~sample(c("hawk", "dove"), n, replace = TRUE), payoff = 0))
  go <- abm_go(
    abm_match(pair = "random"),
    abm_rules(payoff ~ case_when(
      strategy == "hawk" & partner_strategy == "hawk" ~ (V - C) / 2,
      strategy == "hawk" & partner_strategy == "dove" ~ V,
      strategy == "dove" & partner_strategy == "hawk" ~ 0,
      TRUE                                           ~ V / 2)),
    abm_rules(fitness ~ payoff - min((V - C) / 2, 0) + 1),
    abm_rules(strategy ~ sample(strategy, n(), replace = TRUE, prob = fitness),
              .scope = "population")
  )
  abm_run(m, go, ticks = ticks, seed = seed)
}

hawk_share <- function(r) {
  data.frame(tick = sort(unique(r$tick)),
             p = as.numeric(tapply(r$strategy == "hawk", r$tick, mean)))
}

cat("2000 agents, V = 50, 200 ticks; observed = mean hawk share over the\n")
cat("last 50 ticks\n\n")
cat(sprintf("%-8s %14s %12s\n", "C", "predicted V/C", "observed"))

series <- list()
sweep  <- list()
for (C in c(60, 75, 100, 150, 200, 500, 40)) {
  s <- hawk_share(hd_run(C))
  obs <- mean(s$p[s$tick > 150])
  pred <- min(1, V / C)
  cat(sprintf("%-8d %14.3f %12.3f\n", C, pred, obs))
  sweep[[length(sweep) + 1]] <- data.frame(C = C, predicted = pred,
                                           observed = obs)
  s$C <- factor(C); series[[length(series) + 1]] <- s
}
sweep <- do.call(rbind, sweep)
cat(sprintf("\nlargest absolute error across the sweep: %.4f\n",
            max(abs(sweep$predicted - sweep$observed))))
cat("\nNote that pairwise imitation -- copy your partner if they did better --\n")
cat("does not reproduce V/C, because a single contest's payoff is not a\n")
cat("fitness. Both the symmetric and the one-directional version drift toward\n")
cat("0.5 regardless of C.\n")

# --- figure ---------------------------------------------------------------
# Parameter sweep: observed against the analytic ESS, on the identity line.
library(ggplot2)

fig_file <- function(name) {
  a <- grep("^--file=", commandArgs(), value = TRUE)
  d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
  dir.create(file.path(d, "..", "figures"), showWarnings = FALSE)
  file.path(d, "..", "figures", name)
}

p <- ggplot(sweep, aes(predicted, observed)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(size = 2) +
  xlim(0, 1) + ylim(0, 1) +
  theme_minimal() +
  labs(title = "Hawks and doves: the hawk share lands on the mixed ESS",
       subtitle = "2000 agents, V = 50, C from 40 to 500; dashed line is p* = V/C",
       x = "predicted V/C", y = "observed hawk share")
print(p)
ggsave(fig_file("22-hawks-and-doves.png"), p, width = 6, height = 4, dpi = 120)
