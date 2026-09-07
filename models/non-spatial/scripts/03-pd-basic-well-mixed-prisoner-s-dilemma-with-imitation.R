# 3. PD Basic, a well-mixed prisoner's dilemma with imitation
#
# Play a random partner, then pair again and copy whoever did better. Two match
# phases in one tick, written flat and in order. Cooperation collapses.
library(tidyABM)

pd <- abm_setup(agents = abm_agents(
  n = 100, strategy = ~sample(c("cooperate", "defect"), n, replace = TRUE),
  payoff = 0))

go <- abm_go(
  abm_match(pair = "random"),
  abm_rules(payoff ~ case_when(
    strategy == "cooperate" & partner_strategy == "cooperate" ~ 3,
    strategy == "defect"    & partner_strategy == "defect"    ~ 1,
    strategy == "defect"    & partner_strategy == "cooperate" ~ 5,
    strategy == "cooperate" & partner_strategy == "defect"    ~ 0)),
  abm_match(pair = "random"),
  abm_rules(strategy ~ if_else(partner_payoff > payoff, partner_strategy, strategy))
)

result <- abm_run(pd, go, ticks = 100, seed = 3)

coop <- tapply(result$strategy == "cooperate", result$tick, mean)
series <- data.frame(tick = as.integer(names(coop)), coop = as.numeric(coop))

cat("100 agents, well mixed, T=5 R=3 P=1 S=0, 100 ticks\n\n")
cat(sprintf("%-6s %s\n", "tick", "cooperating"))
for (t in c(0, 1, 2, 5, 10, 20, 50, 100)) {
  cat(sprintf("%-6d %11.2f\n", t, series$coop[series$tick == t]))
}
cat(sprintf("\nfirst tick with no cooperators: %s\n",
            if (any(series$coop == 0)) min(series$tick[series$coop == 0]) else "none"))

# --- figure ---------------------------------------------------------------
library(ggplot2)

fig_file <- function(name) {
  a <- grep("^--file=", commandArgs(), value = TRUE)
  d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
  dir.create(file.path(d, "..", "figures"), showWarnings = FALSE)
  file.path(d, "..", "figures", name)
}

p <- ggplot(series, aes(tick, coop)) +
  geom_line() +
  ylim(0, 1) +
  theme_minimal() +
  labs(title = "PD Basic: cooperation collapses under pairwise imitation",
       x = "tick", y = "share cooperating")
print(p)
ggsave(fig_file("03-pd-basic-well-mixed-prisoner-s-dilemma-with-imitation.png"),
       p, width = 6, height = 4, dpi = 120)
