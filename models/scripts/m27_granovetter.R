library(tidyABM)

# 27. Granovetter (1978) threshold model of collective behaviour ------------
# Each agent has a threshold: the number of OTHER people already rioting that
# it needs to see before it joins. Everyone updates simultaneously against
# last tick's count.

riot_run <- function(thresholds, ticks = 120, seed = 1) {
  crowd <- abm_setup(
    agents  = abm_agents(n = length(thresholds), threshold = thresholds,
                         rioting = FALSE),
    globals = list(n_rioting = 0)
  )
  go <- abm_go(
    abm_rules(rioting ~ n_rioting >= threshold),
    abm_global(n_rioting ~ sum(rioting))
  )
  abm_run(crowd, go, ticks = ticks, seed = seed)
}

# Granovetter's own example: thresholds 0,1,2,...,99 -> everyone riots
r_uniform <- riot_run(0:99)
cat("uniform 0..99 final rioters:",
    sum(r_uniform$rioting[r_uniform$tick == 120]), "\n")

# one person changed: the agent with threshold 1 now has threshold 2
bumped <- 0:99; bumped[2] <- 2
r_bumped <- riot_run(bumped)
cat("with the 1 changed to a 2:",
    sum(r_bumped$rioting[r_bumped$tick == 120]), "\n")

# the cascade itself
cat("cascade (uniform):",
    head(tapply(r_uniform$rioting, r_uniform$tick, sum), 12), "\n")
