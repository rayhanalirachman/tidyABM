# 51. Imitation dynamics of vaccination behaviour on social networks
#     (Fu, Rosenbloom, Wang & Nowak 2011, Proc. R. Soc. B 278: 42-49)
library(tidyABM)

vaccination <- function(n = 500, cost = 0.2, type = "random", degree = 6,
                        r = 0.4, g = 0.4, i0 = 5, beta = 10,
                        seasons = 300, seed = 1) {
  m <- abm_setup(
    agents  = abm_agents(n = n, vax = ~runif(n) < 0.5, state = "S",
                         payoff = 0, infected = FALSE),
    network = abm_network(type = type, degree = degree),
    seed    = seed
  )
  go <- abm_go(
    # a season starts: the vaccinated are immune, a few others are seeded
    abm_rules(state ~ if_else(vax, "V", "S"), infected ~ FALSE,
              .scope = "population"),
    abm_rules(state ~ {
      s <- state; k <- which(s == "S")
      if (length(k)) s[sample(k, min(i0, length(k)))] <- "I"
      s
    }, .scope = "population"),
    # ...and the epidemic runs to the end before anyone reconsiders
    abm_repeat(
      abm_neighbours(exposure ~ sum(state == "I")),
      abm_rules(state ~ case_when(
        state == "S" & runif(n()) < 1 - (1 - r)^coalesce(exposure, 0L) ~ "E",
        state == "I" & runif(n()) < g ~ "R",
        TRUE ~ state), .scope = "population"),
      abm_rules(state ~ if_else(state == "E", "I", state),
                infected ~ infected | state %in% c("I", "R"),
                .scope = "population"),
      until = sum(state == "I") == 0,
      max = 5000
    ),
    # payoffs, then everyone copies a neighbour with the Fermi rule
    abm_rules(payoff ~ if_else(vax, -cost, if_else(infected, -1, 0)),
              .scope = "population"),
    abm_match(pair = "network"),
    abm_rules(vax ~ if_else(!is.na(.partner) &
                            runif(1) < 1 / (1 + exp(-beta * (partner_payoff - payoff))),
                            partner_vax, vax)),
    abm_global(coverage ~ mean(vax), attack ~ mean(infected & !vax))
  )
  abm_run(m, go, ticks = seasons, seed = seed)
}

if (sys.nframe() == 0L) {
  cat("N = 500, mean degree 6, r = 0.4, g = 0.4, 2 seeds, 100 seasons\n\n")
  cat(sprintf("%12s %8s %12s %12s\n", "network", "cost", "vaccinated",
              "infected"))
  for (nt in c("random", "poisson", "scale_free")) {
    for (cc in c(0.05, 0.1, 0.2, 0.4, 0.8)) {
      o <- rowMeans(vapply(1:2, function(s) {
        r <- vaccination(cost = cc, type = nt, seasons = 100, seed = s)
        g <- abm_globals(r); g <- g[g$tick > 50, ]
        c(mean(g$coverage), mean(g$attack))
      }, numeric(2)))
      cat(sprintf("%12s %8.2f %12.3f %12.3f\n", nt, cc, o[1], o[2]))
    }
  }
}
