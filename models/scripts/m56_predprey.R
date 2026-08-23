# 56. Predator-prey without space (Lotka 1925; Volterra 1926;
#     after Wilensky 1997, NetLogo Wolf Sheep Predation)
library(tidyABM)

wolfsheep <- function(response = c("mass_action", "ratio"),
                      n_sheep = 1000, n_wolves = 700, ticks = 600,
                      sheep_rep = 0.03, wolf_rep = 0.02, catch = 0.04,
                      gain = 30, K = 5000, area = 1200, seed = 1,
                      record = "globals") {
  response <- match.arg(response)
  m <- abm_setup(
    agents = list(
      sheep  = abm_agents(n = n_sheep,  energy = 0),
      wolves = abm_agents(n = n_wolves, energy = ~runif(n, 1, gain))
    ),
    globals = list(n_sheep = n_sheep, n_wolves = n_wolves),
    seed = seed
  )
  # The pairing mode *is* the functional response. "opposite_group" makes
  # min(S, W) encounters, which is ratio-dependent; filtering the hunters by
  # sheep density first makes S*W/area of them, which is mass action.
  hunt <- if (response == "mass_action") {
    abm_match(pair = "opposite_group", by = .group,
              eligible = .group == "sheep" | runif(n()) < n_sheep / area)
  } else {
    abm_match(pair = "opposite_group", by = .group)
  }
  go <- abm_go(
    abm_global(n_sheep  ~ sum(.group == "sheep"),
               n_wolves ~ sum(.group == "wolves")),
    abm_rules(caught ~ FALSE, .scope = "population"),
    hunt,
    abm_rules(caught ~ runif(1) < catch),
    abm_rules(energy ~ if_else(.group == "wolves" & caught, energy + gain,
                               energy - (.group == "wolves")),
              .scope = "population"),
    abm_death(when = (.group == "sheep" & caught) |
                     (.group == "wolves" & energy <= 0)),
    abm_birth(when = .group == "sheep" &
                     runif(n()) < sheep_rep * (1 - n_sheep / K)),
    abm_birth(when = .group == "wolves" & runif(n()) < wolf_rep,
              cost = energy ~ energy / 2)
  )
  # everything this model reports is a count per tick, and the population it
  # is counting can reach tens of thousands. `record = "globals"` keeps the two
  # counts and none of the agents, which is the difference between a run that
  # finishes and one the kernel stops.
  abm_run(m, go, ticks = ticks, seed = seed, record = record)
}

if (sys.nframe() == 0L) {
  cat("mass action: S* = area / (catch * gain), W* = (r * area / catch)(1 - S*/K)\n")
  cat("Lotka-Volterra: the predator peak lags the prey peak by a quarter cycle\n\n")
  r <- wolfsheep(response = "mass_action", n_sheep = 250, n_wolves = 175,
                 K = 1250, area = 300, ticks = 2000, seed = 1)
  g <- abm_globals(r)[-1, ]; h <- g[g$tick > 200, ]
  sp <- stats::spectrum(h$n_sheep, plot = FALSE, spans = 7)
  per <- 1 / sp$freq[which.max(sp$spec)]
  cc <- stats::ccf(h$n_sheep, h$n_wolves, lag.max = 400, plot = FALSE)
  lag <- cc$lag[which.max(cc$acf)]
  cat(sprintf("%-14s %8s %8s %8s %8s\n", "", "mean", "min", "max", "predicted"))
  cat(sprintf("%-14s %8.0f %8.0f %8.0f %8.0f\n", "sheep",
              mean(h$n_sheep), min(h$n_sheep), max(h$n_sheep), 300 / (0.04 * 30)))
  cat(sprintf("%-14s %8.0f %8.0f %8.0f %8.0f\n", "wolves",
              mean(h$n_wolves), min(h$n_wolves), max(h$n_wolves),
              0.03 * 300 / 0.04 * (1 - 250 / 1250)))
  cat(sprintf("\nperiod %.0f ticks; predator lag %.0f ticks (a quarter cycle is %.0f)\n",
              per, -lag, per / 4))

  cat("\nratio-dependent pairing, same parameters:\n")
  r2 <- wolfsheep(response = "ratio", n_sheep = 250, n_wolves = 175,
                  K = 1250, area = 300, ticks = 1000, seed = 1)
  g2 <- abm_globals(r2)[-1, ]; h2 <- g2[g2$tick > 500, ]
  cat(sprintf("sheep %.0f [%d, %d]   wolves %.0f [%d, %d]\n",
              mean(h2$n_sheep), min(h2$n_sheep), max(h2$n_sheep),
              mean(h2$n_wolves), min(h2$n_wolves), max(h2$n_wolves)))
}
