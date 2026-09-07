library(tidyABM)
library(rlang)

# 35. Simple Genetic Algorithm (Wilensky 1998, NetLogo Computer Science) ----
# Bit-string chromosomes evolve toward all-ones under tournament selection,
# single-point crossover and per-bit mutation. The chromosome is one list
# column, a bit vector per agent.

ga <- function(L = 20, N = 100, mutation = 0.03, crossover = 0.7,
               generations = 100, seed = 1) {
  pop <- abm_setup(
    agents  = abm_agents(n = N, genome = ~lapply(seq_len(n), function(i)
                                            sample(0:1, L, replace = TRUE))),
    globals = list(mut = mutation, xover = crossover, L = L),
    seed    = seed
  )

  fitness_rule <- abm_rules(fitness ~ vapply(genome, sum, numeric(1)))

  # tournament of three: the fittest of three random draws becomes a parent
  tournament <- function(out) {
    list(
      abm_rules(t1 ~ sample(n(), n(), replace = TRUE),
                t2 ~ sample(n(), n(), replace = TRUE),
                t3 ~ sample(n(), n(), replace = TRUE)),
      abm_rules(new_formula(sym(out), expr(
        if_else(fitness[t1] >= fitness[t2] & fitness[t1] >= fitness[t3], t1,
                if_else(fitness[t2] >= fitness[t3], t2, t3)))))
    )
  }

  go <- do.call(abm_go, c(
    list(fitness_rule),
    tournament("p1"), tournament("p2"),
    list(abm_rules(cross  ~ sample(L, n(), replace = TRUE),
                   sexual ~ runif(n()) < xover)),
    # single-point crossover: the first `cross` bits from one parent, the rest
    # from the other. genome[p1] and genome[p2] are the parents' genomes, so
    # the whole generation is bred in one rule and reads the *old* one.
    list(abm_rules(genome ~ Map(
      function(g1, g2, s, k) if (!s) g1 else c(g1[seq_len(k)], g2[-seq_len(k)]),
      genome[p1], genome[p2], sexual, cross))),
    list(abm_rules(genome ~ lapply(genome, function(g)
      if_else(runif(L) < mut, 1L - g, g)))),
    list(fitness_rule)
  ))

  abm_run(pop, go, ticks = generations, seed = seed)
}

r <- ga()
best <- tapply(r$fitness, r$tick, max)
mean_fit <- tapply(r$fitness, r$tick, mean)
cat("best fitness by generation :", best[c(6, 11, 21, 51, 101)], "of 20\n")
cat("mean fitness by generation :", round(mean_fit[c(6, 11, 21, 51, 101)], 1), "\n")
cat("first generation at the optimum:",
    names(best)[which(best == 20)[1]], "\n")
cat("optimum held from then on     :",
    all(best[which(best == 20)[1]:length(best)] == 20), "\n")

for (mu in c(0.01, 0.03, 0.1, 0.3)) {
  rr <- ga(mutation = mu)
  fin <- rr[rr$tick == max(rr$tick), ]
  cat(sprintf("mutation %.2f  best %2d  mean %.1f\n",
              mu, max(fin$fitness), mean(fin$fitness)))
}
