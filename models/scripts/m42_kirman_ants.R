# 42. Kirman's ants (Kirman 1993, QJE 108(1): 137-156)
#
# Two identical food sources and a colony of ants. An ant switches source
# either on its own, with small probability epsilon, or by meeting another ant
# at random and being converted with probability delta. Nothing distinguishes
# the two sources and nothing about the ants changes -- and yet the colony does
# not sit at 50/50. It spends long stretches almost entirely at one source,
# flips, and spends a long stretch at the other.
#
# That is the point of the model as an economics paper: herding and sustained
# majorities out of agents with no information, no leaders and no memory.
#
# Reported: the stationary distribution of the fraction at source A is
# Beta(c, c) with c = epsilon (n - 1) / delta -- U-shaped and herding when
# c < 1, single-peaked and boring when c > 1. The whole claim turns on that one
# number, and the two runs below sit either side of it with everything else
# held fixed.
#
# One tick here is a full sweep -- every ant meets somebody -- rather than the
# single meeting of Kirman's continuous-time process. With delta small enough
# that only a few ants convert per sweep, the two agree; the stationary
# distribution depends on the ratio epsilon/delta and not on their scale, so
# the scale is set by what makes the chain mix in a reasonable number of ticks.

library(tidyABM)

kirman <- function(n = 60, epsilon = 0.0002, delta = 0.05, ticks = 4000,
                   seed = 1) {
  m <- abm_setup(
    agents = abm_agents(n = n, at_a = ~seq_len(n) <= n %/% 2),
    seed   = seed
  )

  go <- abm_go(
    # every ant bumps into one other ant, drawn from the whole colony --
    # NetLogo's `one-of other turtles`, and the reason the mode exists
    abm_match(pair = "one_of"),
    abm_rules(at_a ~ dplyr::case_when(
      runif(n()) < epsilon ~ !at_a,                    # switch on your own
      runif(n()) < delta   ~ partner_at_a,             # or be converted
      TRUE                 ~ at_a
    )),
    abm_global(share_a ~ mean(at_a))
  )

  abm_globals(abm_run(m, go, ticks = ticks, seed = seed))$share_a
}

if (sys.nframe() == 0L) {
  n <- 60
  runs <- list(
    "herding    (c = 0.24)" = list(epsilon = 0.0002, delta = 0.05),
    "no herding (c = 3.5)"  = list(epsilon = 0.0030, delta = 0.05)
  )
  for (nm in names(runs)) {
    p <- runs[[nm]]
    x <- kirman(n = n, epsilon = p$epsilon, delta = p$delta)
    x <- x[-seq_len(1000)]                 # burn-in
    a <- p$epsilon * (n - 1) / p$delta
    cat(sprintf("\n%s\n", nm))
    cat(sprintf("  time at the extremes (share < .1 or > .9): %.2f  [Beta: %.2f]\n",
                mean(x < 0.1 | x > 0.9),
                stats::pbeta(0.1, a, a) + (1 - stats::pbeta(0.9, a, a))))
    cat(sprintf("  time near the middle  (.4 < share < .6):   %.2f  [Beta: %.2f]\n",
                mean(x > 0.4 & x < 0.6),
                stats::pbeta(0.6, a, a) - stats::pbeta(0.4, a, a)))
    cat(sprintf("  s.d. of the share:                         %.3f  [Beta: %.3f]\n",
                stats::sd(x), sqrt(a * a / ((2 * a)^2 * (2 * a + 1)))))
  }
}
