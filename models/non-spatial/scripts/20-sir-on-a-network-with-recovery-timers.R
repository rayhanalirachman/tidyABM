# 20. SIR on a network with recovery timers
#
# A duration-based state machine is a counter column plus two rules, and needs
# nothing beyond `abm_neighbours()`. Reported: the epidemic threshold, three
# values of beta either side of R0 = 1.
library(tidyABM)

DURATION <- 8; DEGREE <- 6

sir_run <- function(beta, n = 1000, ticks = 100, seed = 2) {
  m <- abm_setup(
    agents  = abm_agents(n = n,
                         state = ~c("infected", rep("susceptible", n - 1)),
                         timer = 0),
    network = abm_network(type = "random", degree = DEGREE))
  go <- abm_go(
    abm_neighbours(inf_nbrs ~ sum(state == "infected")),
    abm_rules(state ~ if_else(
      state == "susceptible" &
        runif(n()) < 1 - (1 - beta)^coalesce(inf_nbrs, 0),
      "infected", state)),
    abm_rules(timer ~ if_else(state == "infected", timer + 1, timer)),
    abm_rules(state ~ if_else(state == "infected" & timer > DURATION,
                              "recovered", state))
  )
  abm_run(m, go, ticks = ticks, seed = seed)
}

cat("1000 agents, 6-regular network, one seed case, recovery after 8 ticks\n")
cat(sprintf("R0 = beta * degree * duration = beta * %d\n\n", DEGREE * DURATION))
cat(sprintf("%-8s %8s %14s %12s %10s\n", "beta", "R0", "ever infected",
            "peak cases", "peak tick"))

runs <- list()
for (beta in c(0.01, 0.03, 0.06)) {
  r <- sir_run(beta)
  inf <- data.frame(
    tick = sort(unique(r$tick)),
    n = as.numeric(tapply(r$state == "infected", r$tick, sum)))
  final <- r[r$tick == 100, ]
  ever <- mean(final$state != "susceptible")
  cat(sprintf("%-8.2f %8.1f %13.1f%% %12d %10d\n", beta, beta * DEGREE * DURATION,
              100 * ever, max(inf$n), inf$tick[which.max(inf$n)]))
  inf$beta <- sprintf("beta = %.2f", beta)
  runs[[length(runs) + 1]] <- inf
}
cat("\nbelow R0 = 1 the seed case burns out; above it the epidemic takes the\n")
cat("whole network\n")

# --- figure ---------------------------------------------------------------
library(ggplot2)

fig_file <- function(name) {
  a <- grep("^--file=", commandArgs(), value = TRUE)
  d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
  dir.create(file.path(d, "..", "figures"), showWarnings = FALSE)
  file.path(d, "..", "figures", name)
}

d <- do.call(rbind, runs)

p <- ggplot(d, aes(tick, n, colour = beta)) +
  geom_line(linewidth = 0.7) +
  theme_minimal() +
  labs(title = "SIR on a network: an epidemic threshold at R0 = 1",
       subtitle = "1000 agents, degree 6, recovery after 8 ticks",
       x = "tick", y = "simultaneous cases", colour = NULL)
print(p)
ggsave(fig_file("20-sir-on-a-network-with-recovery-timers.png"), p,
       width = 6, height = 4, dpi = 120)
