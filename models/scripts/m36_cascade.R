library(tidyABM)

# 36. Information cascade (Bikhchandani, Hirshleifer & Welch 1992) ----------
# Agents decide one at a time. Each sees a private signal that is right with
# probability p, and the *decisions* — not the signals — of everyone before it.
# Once the public tally leads by two, the signal stops mattering and everybody
# after copies, right or wrong.

side <- function(signal, nA, nB) {
  d <- nA - nB
  if_else(d >= 2, "A", if_else(d <= -2, "B", signal))
}

cascade <- function(p = 0.7, n = 50, seed = 1) {
  # the truth is A; a signal is right with probability p
  pop <- abm_setup(
    agents  = abm_agents(n = n,
                         signal = ~if_else(runif(n) < p, "A", "B"),
                         decision = NA_character_),
    globals = list(nA = 0, nB = 0),
    seed    = seed
  )
  go <- abm_go(
    abm_sequential(
      decision ~ side(signal, nA, nB),
      nA ~ nA + (side(signal, nA, nB) == "A"),
      nB ~ nB + (side(signal, nA, nB) == "B")
    )
  )
  abm_run(pop, go, ticks = 1, seed = seed)
}

share_A <- function(p, seeds = 1:400) {
  vapply(seeds, function(s) {
    r <- cascade(p = p, seed = s)
    mean(r$decision[r$tick == 1] == "A")
  }, numeric(1))
}

for (p in c(0.55, 0.6, 0.7, 0.8, 0.9)) {
  runs <- share_A(p)
  cat(sprintf("p = %.2f   correct cascades %.3f   BHW p^2/(p^2+(1-p)^2) = %.3f\n",
              p, mean(runs > 0.5), p^2 / (p^2 + (1 - p)^2)))
}
