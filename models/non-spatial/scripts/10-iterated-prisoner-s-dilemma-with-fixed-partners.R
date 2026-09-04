# 10. Iterated Prisoner's Dilemma with fixed partners
#
# `abm_network(degree = 1)` gives permanent partnerships and the `partner_*`
# convention already carries last tick's move forward, so one round of memory
# comes free. Cooperation survives, unlike the well-mixed model 3.
library(tidyABM)

ipd <- abm_setup(
  agents  = abm_agents(n = 100, move = "cooperate", payoff = 0),
  network = abm_network(type = "random", degree = 1))

go <- abm_go(
  abm_match(pair = "network"),
  abm_rules(payoff ~ case_when(
    move == "cooperate" & partner_move == "cooperate" ~ 3,
    move == "defect"    & partner_move == "defect"    ~ 1,
    move == "defect"    & partner_move == "cooperate" ~ 5,
    move == "cooperate" & partner_move == "defect"    ~ 0)),
  abm_rules(move ~ partner_move)
)

result <- abm_run(ipd, go, ticks = 50, seed = 10)

series <- data.frame(
  tick = sort(unique(result$tick)),
  coop = as.numeric(tapply(result$move == "cooperate", result$tick, mean)),
  pay  = as.numeric(tapply(result$payoff, result$tick, mean)))

cat("100 agents in fixed pairs, copy-your-partner, 50 ticks\n\n")
cat(sprintf("%-6s %13s %12s\n", "tick", "cooperating", "mean payoff"))
for (t in c(0, 1, 5, 10, 25, 50)) {
  r <- series[series$tick == t, ]
  cat(sprintf("%-6d %13.2f %12.2f\n", r$tick, r$coop, r$pay))
}
cat(sprintf("\ncooperation at tick 50: %.2f (model 3, well mixed, reaches 0)\n",
            series$coop[series$tick == 50]))

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
  labs(title = "IPD with fixed partners: cooperation holds",
       subtitle = "a permanent partner is one round of memory, for free",
       x = "tick", y = "share cooperating")
print(p)
ggsave(fig_file("10-iterated-prisoner-s-dilemma-with-fixed-partners.png"), p,
       width = 6, height = 4, dpi = 120)
