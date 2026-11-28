# 14. El Farol with inductive agents  (Arthur 1994, Am. Econ. Rev. 84: 406-411)
#
# Each agent holds a set of candidate predictors in a list column, re-scores
# every one against the last `memory` weeks each tick, and acts on whichever
# would have been least wrong. The functions follow the NetLogo reporters of
# the same names.
#
# Reported: attendance over the last 200 of 300 ticks, and the pool-size sweep
# over the number of predictors per agent.
library(tidyABM)

# to-report predict-attendance [strategy subhistory]
predict_attendance <- function(strategy, subhistory) {
  100 * strategy[1] + sum(strategy[-1] * subhistory)
}

# how far off would ONE strategy have been, over the last `memory` weeks?
score_strategy <- function(strategy, history, memory) {
  sum(vapply(1:memory, function(week) abs(
    history[week] - predict_attendance(strategy, history[week + 1:memory])),
    numeric(1)))
}

# the strategy that has been least wrong lately (to update-strategies)
best_strategy_of <- function(strategies, history, memory) {
  strategies[[which.min(vapply(strategies, score_strategy, numeric(1),
                               history, memory))]]
}

farol <- function(n_strat = 10, setup_seed = 1, ticks = 300, memory = 5) {
  m <- abm_setup(
    agents = abm_agents(
      n = 100,
      # each strategy is a baseline plus one random weight per week of lag
      strategies = ~replicate(n, replicate(
        n_strat, runif(memory + 1, -1, 1), simplify = FALSE), simplify = FALSE),
      best       = ~lapply(strategies, function(s) s[[1]]),  # replaced at tick 1
      prediction = 0,
      attend     = FALSE),
    globals = list(history = list(sample(0:99, memory * 2, replace = TRUE)),
                   attendance = 0, memory = memory, threshold = 60),
    seed = setup_seed)

  go <- abm_go(
    # update-strategies runs first, so the placeholder never decides anything
    abm_rules(best ~ lapply(strategies, best_strategy_of, history[[1]], memory)),
    abm_rules(prediction ~ vapply(best, predict_attendance, numeric(1),
                                  history[[1]][1:memory])),
    abm_rules(attend ~ prediction <= threshold),
    abm_global(attendance ~ sum(attend),
               history    ~ list(c(attendance, head(history[[1]], -1)))))

  abm_run(m, go, ticks = ticks, record = "globals")
}

g <- abm_globals(farol(10))
tail200 <- g$attendance[g$tick > 100]

cat("100 agents, 10 predictors each, memory 5, capacity 60, 300 ticks\n\n")
cat(sprintf("last 200 ticks: mean %.1f, sd %.1f, range %d-%d, %d distinct levels\n",
            mean(tail200), sd(tail200), min(tail200), max(tail200),
            length(unique(tail200))))
ac <- sapply(1:6, function(k) cor(head(tail200, -k), tail(tail200, -k)))
cat(sprintf("autocorrelation at lags 1..6: %s\n",
            paste(sprintf("%+.2f", ac), collapse = " ")))
cat("no exact period, though the correlation at lag 6 comes close to one\n\n")

# `abm_setup(seed =)` fixes the predictors the agents are born with and the
# starting history; nothing in the tick draws a random number.
cat("Predictors per agent, across four population draws\n\n")
cat(sprintf("%-10s %8s %8s %8s %8s\n", "predictors", "draw 1", "draw 2",
            "draw 3", "draw 4"))
for (ns in c(1, 3, 10)) {
  levels_seen <- vapply(1:4, function(s) {
    gg <- abm_globals(farol(ns, setup_seed = s))
    length(unique(gg$attendance[gg$tick > 100]))
  }, integer(1))
  cat(sprintf("%-10d %8d %8d %8d %8d\n", ns, levels_seen[1], levels_seen[2],
              levels_seen[3], levels_seen[4]))
}
cat("\ndistinct levels in the last 200 ticks. One and three predictors end in a\n")
cat("fixed point or an exact cycle in every draw; ten never do.\n")

# --- figure ---------------------------------------------------------------
library(ggplot2)

.file <- grep("^--file=", commandArgs(), value = TRUE)
source(file.path(dirname(sub("^--file=", "", c(.file, "x")[1])), "../../fig_file.R"))

p <- ggplot(g[g$tick > 0, ], aes(tick, attendance)) +
  geom_hline(yintercept = g$threshold[1], linetype = "dashed", colour = "grey50") +
  geom_line(linewidth = 0.4) +
  theme_minimal() +
  labs(title = "El Farol with inductive agents: attendance hovers at capacity",
       subtitle = "100 agents, 10 predictors each; dashed line is the capacity of 60",
       x = "tick", y = "attendance")
print(p)
ggsave(fig_file("14-el-farol-with-inductive-agents.png"), p,
       width = 6, height = 4, dpi = 120)
