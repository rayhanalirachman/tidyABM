library(tidyABM)

# 34. epiDEM Basic (Yang & Wilensky 2011, NetLogo) --------------------------
# A well-mixed SIR epidemic: each tick every agent bumps into one other agent
# and may pass on the infection; recovery is drawn from a per-agent timer and
# confers permanent immunity. R0 is estimated from the susceptible curve.

epidem <- function(infection_chance = 0.4, recovery_chance = 0.5,
                   avg_recovery = 30, n = 400, i0 = 5, ticks = 300, seed = 1) {
  pop <- abm_setup(
    agents = abm_agents(
      n        = n,
      state    = ~if_else(seq_len(n) <= i0, "I", "S"),
      sick_for = 0,
      recover_after = ~pmax(1, rnorm(n, avg_recovery, avg_recovery / 4))
    ),
    globals = list(inf_p = infection_chance, rec_p = recovery_chance,
                   N = n, S0 = n - i0, S = n - i0, R0_hat = NA_real_),
    seed = seed
  )

  go <- abm_go(
    # everyone meets one other agent, NetLogo's `one-of other turtles`
    abm_match(pair = "one_of"),
    abm_rules(state ~ if_else(state == "S" & partner_state == "I" &
                              runif(n()) < inf_p, "I", state)),
    abm_rules(sick_for ~ if_else(state == "I", sick_for + 1, sick_for)),
    abm_rules(state ~ if_else(state == "I" & sick_for > recover_after &
                              runif(n()) < rec_p, "R", state)),
    abm_global(S ~ sum(state == "S")),
    abm_global(R0_hat ~ if_else(S > 0 & S < S0, N * log(S0 / S) / (N - S), R0_hat))
  )
  abm_run(pop, go, ticks = ticks, seed = seed)
}

sweep <- do.call(rbind, lapply(c(0.005, 0.01, 0.02, 0.05, 0.1, 0.2),
  function(ic) {
    r <- epidem(infection_chance = ic)
    final <- r[r$tick == max(r$tick), ]
    row <- data.frame(chance = ic, attack = mean(final$state != "S"),
                      R0 = tail(abm_globals(r)$R0_hat, 1))
    cat(sprintf("infection chance %.3f   attack rate %.2f   R0-hat %.2f\n",
                ic, row$attack, row$R0))
    row
  }))

# --- figure ---------------------------------------------------------------
# Parameter sweep: the attack rate against the estimated R0, which is the
# Kermack-McKendrick final-size relation the model is meant to recover.
library(ggplot2)

fig_file <- function(name) {
  a <- grep("^--file=", commandArgs(), value = TRUE)
  d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
  dir.create(file.path(d, "..", "figures"), showWarnings = FALSE)
  file.path(d, "..", "figures", name)
}

fig <- ggplot(sweep, aes(R0, attack)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_line(linewidth = 0.7) + geom_point() +
  ylim(0, 1) +
  theme_minimal() +
  labs(title = "epiDEM Basic: the final size follows the estimated R0",
       subtitle = "400 agents, well mixed; dashed line is the threshold R0 = 1",
       x = "estimated R0", y = "attack rate")
print(fig)
ggsave(fig_file("34-epidem-basic.png"), fig, width = 6, height = 4, dpi = 120)
