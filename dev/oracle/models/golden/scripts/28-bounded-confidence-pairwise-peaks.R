library(tidyABM); library(dplyr)
deffuant <- function(d, mu = 0.5, n = 1000, ticks = 600, seed = 1) {
  pop <- abm_setup(agents = abm_agents(n = n, opinion = ~runif(n)),
                   globals = list(d = d, mu = mu), seed = 42)
  go <- abm_go(abm_match(pair = "random"),
    abm_rules(opinion ~ if_else(abs(opinion - partner_opinion) < d,
                                opinion + mu * (partner_opinion - opinion), opinion)))
  abm_run(pop, go, ticks = ticks, seed = seed)
}
peaks <- function(res, tol = 0.02, min_share = 0.05) {
  x <- sort(res$opinion[res$tick == max(res$tick)])
  g <- cumsum(c(TRUE, diff(x) > tol))
  tab <- tapply(x, g, function(v) c(centre = mean(v), share = length(v) / length(x)))
  do.call(rbind, tab)
}
found <- lapply(c(0.5, 0.3, 0.25, 0.2, 0.15, 0.1), function(d) {
  p <- peaks(deffuant(d))
  major <- p[p[, "share"] >= 0.05, , drop = FALSE]
  cat(sprintf("d=%.2f  1/(2d)=%.2f  major peaks=%d  at %s (shares %s)\n",
      d, 1/(2*d), nrow(major),
      paste(round(major[, "centre"], 2), collapse = ","),
      paste(round(major[, "share"], 2), collapse = ",")))
  data.frame(d = d, centre = major[, "centre"], share = major[, "share"])
})
sweep <- do.call(rbind, found)

# --- figure ---------------------------------------------------------------
# Where the mass actually sits: every major peak, at the d that produced it,
# sized by the share of the population it holds.
library(ggplot2)

fig_file <- function(name) {
  a <- grep("^--file=", commandArgs(), value = TRUE)
  dd <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
  dir.create(file.path(dd, "..", "figures"), showWarnings = FALSE)
  file.path(dd, "..", "figures", name)
}

fig <- ggplot(sweep, aes(d, centre, size = share)) +
  geom_point(alpha = 0.8) +
  theme_minimal() +
  labs(title = "Deffuant: major opinion peaks, by confidence bound",
       subtitle = "1000 agents, mu = 0.5, 600 ticks; peaks holding >= 5%",
       x = "confidence bound d", y = "peak location", size = "share")
print(fig)
ggsave(fig_file("28-bounded-confidence-pairwise-peaks.png"), fig,
       width = 6, height = 4, dpi = 120)
