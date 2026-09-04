# 14. El Farol with inductive agents  (Arthur 1994, Am. Econ. Rev. 84: 406-411)
#
# Each agent holds a matrix of predictor weights and a vector of scores, both in
# list columns, and acts on whichever predictor has been working. There is no
# switching step: "act on whichever has been working" is `which.min(e)` read at
# the moment of acting.
#
# Reported: attendance over the last 200 of 300 ticks, and the pool-size sweep
# that shows the inductive machinery is not sufficient on its own.
library(tidyABM)

MEMORY <- 5; CAPACITY <- 60

farol <- function(n_strat, setup_seed = 1, run_seed = 2, ticks = 300) {
  m <- abm_setup(
    agents = abm_agents(
      n = 100,
      w = ~lapply(seq_len(n), function(i)
            matrix(runif(n_strat * (MEMORY + 1), -1, 1), n_strat, MEMORY + 1)),
      e = ~lapply(seq_len(n), function(i) numeric(n_strat)),
      p = ~lapply(seq_len(n), function(i) numeric(n_strat)),
      go_today = FALSE),
    globals = as.list(setNames(rep(CAPACITY, MEMORY), paste0("att", 1:MEMORY))),
    seed = setup_seed)

  go <- abm_go(
    abm_rules(p ~ lapply(w, function(W)
      as.vector(W %*% c(100, att1, att2, att3, att4, att5)))),
    abm_rules(go_today ~ mapply(function(pi, ei) pi[which.min(ei)], p, e) < CAPACITY),
    abm_global(att5 ~ att4, att4 ~ att3, att3 ~ att2, att2 ~ att1,
               att1 ~ sum(go_today)),
    abm_rules(e ~ mapply(function(ei, pi) 0.8 * ei + 0.2 * abs(pi - att1),
                         e, p, SIMPLIFY = FALSE))
  )
  abm_run(m, go, ticks = ticks, seed = run_seed, record = "globals")
}

g <- abm_globals(farol(10))
tail200 <- g$att1[g$tick > 100]

cat("100 agents, 10 predictors each, memory 5, capacity 60, 300 ticks\n\n")
cat(sprintf("last 200 ticks: mean %.1f, sd %.1f, range %d-%d, %d distinct levels\n",
            mean(tail200), sd(tail200), min(tail200), max(tail200),
            length(unique(tail200))))
ac <- sapply(1:6, function(k) cor(head(tail200, -k), tail(tail200, -k)))
cat(sprintf("autocorrelation at lags 1..6: %s\n",
            paste(sprintf("%+.2f", ac), collapse = " ")))
cat("no period up to lag 6\n\n")

# Both seeds are load-bearing: `abm_setup(seed =)` fixes which predictors the
# agents are born with and changes the answer as much as `abm_run(seed =)`.
cat("Predictors per agent, across four population draws\n\n")
cat(sprintf("%-10s %8s %8s %8s %8s\n", "predictors", "draw 1", "draw 2",
            "draw 3", "draw 4"))
for (ns in c(1, 3, 10)) {
  levels_seen <- vapply(1:4, function(s) {
    gg <- abm_globals(farol(ns, setup_seed = s, run_seed = 2))
    length(unique(gg$att1[gg$tick > 100]))
  }, integer(1))
  cat(sprintf("%-10d %8d %8d %8d %8d\n", ns, levels_seen[1], levels_seen[2],
              levels_seen[3], levels_seen[4]))
}
cat("\nlevels in the last 200 ticks: 1 or 2 is a fixed point or a two-cycle.\n")
cat("Ten predictors never settles; one settles in every draw.\n")

# --- figure ---------------------------------------------------------------
library(ggplot2)

fig_file <- function(name) {
  a <- grep("^--file=", commandArgs(), value = TRUE)
  d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
  dir.create(file.path(d, "..", "figures"), showWarnings = FALSE)
  file.path(d, "..", "figures", name)
}

p <- ggplot(g[g$tick > 0, ], aes(tick, att1)) +
  geom_hline(yintercept = CAPACITY, linetype = "dashed", colour = "grey50") +
  geom_line(linewidth = 0.4) +
  theme_minimal() +
  labs(title = "El Farol with inductive agents: attendance hovers at capacity",
       subtitle = "100 agents, 10 predictors each; dashed line is the capacity of 60",
       x = "tick", y = "attendance")
print(p)
ggsave(fig_file("14-el-farol-with-inductive-agents.png"), p,
       width = 6, height = 4, dpi = 120)
