# 37. Virus on a Network (Stonedahl & Wilensky 2008, NetLogo Networks)
#
# Susceptible / Infected / Resistant on a fixed network. Infected nodes push the
# virus along their edges each tick; every node runs a virus check on its own
# private timer, and a check that succeeds either cures the node back to
# susceptible or leaves it permanently resistant.
#
# Reported: the resistance switch. gain_resistance = 0 is an SIS process and
# the virus stays endemic; gain_resistance = 1 is SIR and the epidemic burns out.

library(tidyABM)

virus_run <- function(gain_resistance, ticks = 500, seed = 1,
                      n = 150, degree = 6, outbreak = 3,
                      spread = 0.025, check_freq = 20, recovery = 0.05) {
  m <- abm_setup(
    agents = abm_agents(
      n     = n,
      state = ~ifelse(seq_len(n) <= outbreak, "infected", "susceptible"),
      timer = ~sample.int(check_freq, n, replace = TRUE) - 1L
    ),
    network = abm_network(type = "random", degree = degree),
    seed    = seed
  )

  go <- abm_go(
    # every node advances its own check clock, which wraps at check_freq
    abm_rules(timer ~ (timer + 1L) %% check_freq),

    # how many of my neighbours are infected, counted before anyone moves
    abm_neighbours(exposure ~ sum(state == "infected")),

    # k independent Bernoulli(spread) draws, one per infected neighbour
    abm_rules(state ~ if_else(
      state == "susceptible" & runif(n()) < 1 - (1 - spread)^coalesce(exposure, 0L),
      "infected", state
    )),

    # a check only happens on the tick the clock comes round
    abm_rules(state ~ if_else(
      state == "infected" & timer == 0L & runif(n()) < recovery,
      if_else(runif(n()) < gain_resistance, "resistant", "susceptible"),
      state
    )),

    abm_global(infected  ~ mean(state == "infected"),
               resistant ~ mean(state == "resistant"))
  )

  abm_globals(abm_run(m, go, ticks = ticks, seed = seed))
}

if (sys.nframe() == 0L) {
  ticks <- 2000
  sis <- virus_run(gain_resistance = 0, ticks = ticks)
  sir <- virus_run(gain_resistance = 1, ticks = ticks)
  tail_of <- function(g, col) mean(g[[col]][g$tick > ticks - 200])
  cat(sprintf("SIS (gain_resistance = 0): infected %.3f, resistant %.3f\n",
              tail_of(sis, "infected"), tail_of(sis, "resistant")))
  cat(sprintf("SIR (gain_resistance = 1): infected %.3f, resistant %.3f\n",
              tail_of(sir, "infected"), tail_of(sir, "resistant")))

  # --- figure -------------------------------------------------------------
  # Time series: the infected share under both settings of the resistance
  # switch. SIS stays endemic; SIR burns out.
  library(ggplot2)

  fig_file <- function(name) {
    a <- grep("^--file=", commandArgs(), value = TRUE)
    d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
    dir.create(file.path(d, "..", "figures"), showWarnings = FALSE)
    file.path(d, "..", "figures", name)
  }

  d <- rbind(data.frame(tick = sis$tick, model = "SIS (gain = 0)",
                        infected = sis$infected),
             data.frame(tick = sir$tick, model = "SIR (gain = 1)",
                        infected = sir$infected))
  d <- d[!is.na(d$infected), ]

  fig <- ggplot(d, aes(tick, infected, colour = model)) +
    geom_line(linewidth = 0.5) +
    theme_minimal() +
    labs(title = "Virus on a network: the resistance switch decides everything",
         subtitle = "150 nodes, degree 6, spread 0.025, 2000 ticks",
         x = "tick", y = "share infected", colour = NULL)
  print(fig)
  ggsave(fig_file("37-virus-on-a-network.png"), fig, width = 6, height = 4,
         dpi = 120)
}
