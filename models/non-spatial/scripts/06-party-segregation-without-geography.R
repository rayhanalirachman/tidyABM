# 6. Party, segregation without geography
#
# Find your nearest neighbour in opinion space; if even they are too far away,
# jump to a fresh opinion. `pair = "nearest"` is the first directional mode:
# your nearest neighbour need not have you as theirs, so each agent gets its own
# `.group_id` and `runif(1)` is drawn once per agent rather than once per pair.
#
# Reported: the page's own run at n = 200, and a population sweep, because at
# n = 200 the 0.05 tolerance is never binding and the model is a no-op.
library(tidyABM)

party_run <- function(n, ticks = 100, seed = 6) {
  m <- abm_setup(agents = abm_agents(n = n, opinion = ~runif(n, 0, 1)))
  go <- abm_go(
    abm_match(pair = "nearest", by = opinion),
    abm_rules(opinion ~ if_else(abs(opinion - partner_opinion) > 0.05,
                                runif(1), opinion))
  )
  abm_run(m, go, ticks = ticks, seed = seed)
}

# nearest-neighbour gap for each agent, and the clumps a 0.05 cut leaves
nn_gap  <- function(o) { o <- sort(o); pmin(c(Inf, diff(o)), c(diff(o), Inf)) }
n_clump <- function(o) sum(diff(sort(o)) > 0.05) + 1

result <- party_run(200)
cat("The page's run: 200 agents, tolerance 0.05, 100 ticks\n\n")
cat(sprintf("%-6s %9s %10s %12s\n", "tick", "clumps", "mean gap", "settled"))
for (t in c(0, 1, 10, 100)) {
  o <- result$opinion[result$tick == t]
  cat(sprintf("%-6d %9d %10.4f %12.2f\n", t, n_clump(o), mean(nn_gap(o)),
              mean(nn_gap(o) <= 0.05)))
}
cat("\nNothing moves. 200 agents on the unit interval sit 0.003 apart on\n")
cat("average, so every agent is already inside its neighbour's tolerance and\n")
cat("the jump rule never fires. The mechanism needs a sparse population.\n\n")

sweep <- do.call(rbind, lapply(c(10, 20, 40, 80, 200), function(n) {
  r  <- party_run(n)
  o0 <- r$opinion[r$tick == 0]; o1 <- r$opinion[r$tick == 100]
  data.frame(n = n,
             clumps_0 = n_clump(o0), clumps_100 = n_clump(o1),
             settled_0 = mean(nn_gap(o0) <= 0.05),
             settled_100 = mean(nn_gap(o1) <= 0.05))
}))
cat("Population sweep, 100 ticks each\n\n")
cat(sprintf("%-6s %9s %9s %11s %11s\n", "n", "clumps 0", "clumps", "settled 0",
            "settled"))
for (i in seq_len(nrow(sweep))) {
  cat(sprintf("%-6d %9d %9d %11.2f %11.2f\n", sweep$n[i], sweep$clumps_0[i],
              sweep$clumps_100[i], sweep$settled_0[i], sweep$settled_100[i]))
}
cat(sprintf(
  "\nAt n = 10 the model does what it is meant to: %d scattered opinions\n",
  sweep$clumps_0[1]))
cat(sprintf("condense into %d clumps and every agent ends within 0.05 of a neighbour.\n",
            sweep$clumps_100[1]))

# --- figure ---------------------------------------------------------------
# Parameter sweep: the share of agents that have found a neighbour inside the
# tolerance, before and after, against population size.
library(ggplot2)

fig_file <- function(name) {
  a <- grep("^--file=", commandArgs(), value = TRUE)
  d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
  dir.create(file.path(d, "..", "figures"), showWarnings = FALSE)
  file.path(d, "..", "figures", name)
}

d <- rbind(
  data.frame(n = sweep$n, when = "tick 0",   settled = sweep$settled_0),
  data.frame(n = sweep$n, when = "tick 100", settled = sweep$settled_100))
d$when <- factor(d$when, levels = c("tick 0", "tick 100"))

p <- ggplot(d, aes(n, settled, colour = when)) +
  geom_line(linewidth = 0.7) + geom_point() +
  scale_x_log10() + ylim(0, 1) +
  theme_minimal() +
  labs(title = "Party: clustering only bites when opinions are sparse",
       subtitle = "share of agents within 0.05 of a neighbour",
       x = "agents (log scale)", y = "share settled", colour = NULL)
print(p)
ggsave(fig_file("06-party-segregation-without-geography.png"), p,
       width = 6, height = 4, dpi = 120)
