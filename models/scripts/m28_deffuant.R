library(tidyABM)
library(dplyr)

# 28. Deffuant et al. (2000) bounded confidence -----------------------------
deffuant <- function(d, mu = 0.5, n = 1000, ticks = 600, seed = 1) {
  pop <- abm_setup(
    agents  = abm_agents(n = n, opinion = ~runif(n)),
    globals = list(d = d, mu = mu),
    seed    = 42
  )
  go <- abm_go(
    abm_match(pair = "random"),
    abm_rules(opinion ~ if_else(abs(opinion - partner_opinion) < d,
                                opinion + mu * (partner_opinion - opinion),
                                opinion))
  )
  abm_run(pop, go, ticks = ticks, seed = seed)
}

n_clusters <- function(res, tol = 0.01) {
  x <- sort(res$opinion[res$tick == max(res$tick)])
  sum(diff(x) > tol) + 1L
}

for (d in c(0.5, 0.3, 0.2, 0.15, 0.1)) {
  r <- deffuant(d)
  cat(sprintf("d = %-5.2f  clusters = %d   predicted floor(1/(2d)) = %d\n",
              d, n_clusters(r), floor(1 / (2 * d))))
}

# where the mass actually sits
r <- deffuant(0.2)
x <- r$opinion[r$tick == max(r$tick)]
print(round(sort(unique(round(x, 3))), 3))
