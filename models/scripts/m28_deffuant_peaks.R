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
for (d in c(0.5, 0.3, 0.25, 0.2, 0.15, 0.1)) {
  p <- peaks(deffuant(d))
  major <- p[p[, "share"] >= 0.05, , drop = FALSE]
  cat(sprintf("d=%.2f  1/(2d)=%.2f  major peaks=%d  at %s (shares %s)\n",
      d, 1/(2*d), nrow(major),
      paste(round(major[, "centre"], 2), collapse = ","),
      paste(round(major[, "share"], 2), collapse = ",")))
}
