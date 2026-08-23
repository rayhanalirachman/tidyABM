library(tidyABM)

# 29. Hegselmann & Krause (2002) bounded confidence, all-neighbour averaging
hk <- function(eps, n = 500, ticks = 50, seed = 42) {
  pop <- abm_setup(agents = abm_agents(n = n, opinion = ~runif(n)),
                   globals = list(eps = eps), seed = seed)
  # "the mean opinion of everyone within eps of me" is a neighbourhood in
  # opinion space rather than in a network, which is what `within =` says. The
  # agent is inside its own -- the condition holds of it -- which is what
  # Hegselmann and Krause mean by a confidence set.
  go <- abm_go(
    abm_neighbours(opinion ~ mean(opinion),
                   within = abs(opinion - own_opinion) <= eps)
  )
  abm_run(pop, go, ticks = ticks)
}

clusters <- function(res, tol = 1e-6) {
  x <- sort(res$opinion[res$tick == max(res$tick)])
  sum(diff(x) > tol) + 1L
}

for (eps in c(0.30, 0.25, 0.20, 0.15, 0.10, 0.05)) {
  r <- hk(eps)
  cat(sprintf("eps = %.2f  clusters = %d   1/(2*eps) = %.1f\n",
              eps, clusters(r), 1 / (2 * eps)))
}
