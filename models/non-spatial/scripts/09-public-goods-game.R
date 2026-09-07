# 9. Public Goods Game
#
# Split into random groups of four; the pot is doubled and shared equally.
# `abm_match(size = 4)` plus dplyr's grouped-mutate semantics, so
# `sum(contribution)` inside `abm_rules()` means the group's pot.
#
# Reported: the free-rider's advantage, which is what makes contributing
# collapse under any imitation rule laid on top of this payoff.
library(tidyABM)

pgg <- abm_setup(agents = abm_agents(
  n = 100, contribution = ~sample(c(0, 1), n, replace = TRUE), payoff = 0))

go <- abm_go(
  abm_match(pair = "random", size = 4),
  abm_rules(payoff ~ sum(contribution) * 2 / 4)
)

result <- abm_run(pgg, go, ticks = 20, seed = 9)

r <- result[result$tick > 0, ]
r$net <- r$payoff - r$contribution
by_type <- data.frame(
  contribution = c(0, 1),
  gross = as.numeric(tapply(r$payoff, r$contribution, mean)),
  net   = as.numeric(tapply(r$net,    r$contribution, mean)))

cat("100 agents, groups of 4, pot doubled and split, 20 ticks\n\n")
cat(sprintf("%-14s %8s %8s\n", "contribution", "gross", "net"))
for (i in 1:2) {
  cat(sprintf("%-14.0f %8.3f %8.3f\n", by_type$contribution[i],
              by_type$gross[i], by_type$net[i]))
}
cat(sprintf("\nfree-riding is worth %.3f per round, against the 1 - 2/4 = 0.5 the\n",
            by_type$net[1] - by_type$net[2]))
cat("marginal per-capita return implies, and the two contribution types earn\n")
cat("the same gross payoff whenever they land in the same group. That gap is\n")
cat("the dilemma: the pot is worth having and nobody wants to pay into it.\n")

# --- figure ---------------------------------------------------------------
# Final-state comparison: what a contributor and a free-rider take home.
library(ggplot2)

fig_file <- function(name) {
  a <- grep("^--file=", commandArgs(), value = TRUE)
  d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
  dir.create(file.path(d, "..", "figures"), showWarnings = FALSE)
  file.path(d, "..", "figures", name)
}

d <- data.frame(
  who = factor(rep(c("free-rider", "contributor"), 2),
               levels = c("free-rider", "contributor")),
  what = rep(c("gross payoff", "net of own contribution"), each = 2),
  value = c(by_type$gross, by_type$net))

p <- ggplot(d, aes(who, value, fill = what)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(title = "Public goods game: the free-rider is always ahead",
       subtitle = "100 agents, groups of 4, mean over 20 ticks",
       x = NULL, y = "payoff per round", fill = NULL)
print(p)
ggsave(fig_file("09-public-goods-game.png"), p, width = 6, height = 4, dpi = 120)
