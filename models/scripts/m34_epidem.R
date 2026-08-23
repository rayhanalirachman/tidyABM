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

for (ic in c(0.005, 0.01, 0.02, 0.05, 0.1, 0.2)) {
  r <- epidem(infection_chance = ic)
  final <- r[r$tick == max(r$tick), ]
  cat(sprintf("infection chance %.3f   attack rate %.2f   R0-hat %.2f\n",
              ic, mean(final$state != "S"), tail(abm_globals(r)$R0_hat, 1)))
}
