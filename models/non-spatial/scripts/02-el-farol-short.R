# 2. El Farol, short form  (Arthur 1994, Am. Econ. Rev. 84: 406-411)
#
# One shared forecast, so the population behaves as a single agent. It does not
# oscillate around the bar's capacity; it locks into a 0/100 two-cycle. The
# corrected version is model 14.
library(tidyABM)

elfarol <- abm_setup(
  agents  = abm_agents(n = 100, threshold = ~runif(n, 40, 80)),
  globals = list(last_attendance = 60)
)

go <- abm_go(
  abm_rules(go_today ~ last_attendance < threshold),
  abm_global(last_attendance ~ sum(go_today))
)

result <- abm_run(elfarol, go, ticks = 20, seed = 2)
g <- abm_globals(result)

cat("100 agents, capacity 60, thresholds ~ U(40, 80), 20 ticks\n\n")
cat("attendance by tick:\n")
cat(" ", paste(g$last_attendance, collapse = " "), "\n\n")
cat(sprintf("distinct attendance levels after tick 2: %d\n",
            length(unique(g$last_attendance[g$tick > 2]))))
cat(sprintf("mean attendance over the last 10 ticks: %.1f (capacity 60)\n",
            mean(g$last_attendance[g$tick > 10])))

# --- figure ---------------------------------------------------------------
# The failure is a time series, so the figure is one: attendance against the
# capacity it is supposed to hover around.
library(ggplot2)

fig_file <- function(name) {
  a <- grep("^--file=", commandArgs(), value = TRUE)
  d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
  dir.create(file.path(d, "..", "figures"), showWarnings = FALSE)
  file.path(d, "..", "figures", name)
}

p <- ggplot(g, aes(tick, last_attendance)) +
  geom_hline(yintercept = 60, linetype = "dashed", colour = "grey50") +
  geom_line() + geom_point(size = 1) +
  theme_minimal() +
  labs(title = "El Farol, short form: a 0/100 two-cycle, not an oscillation",
       subtitle = "dashed line is the bar's capacity of 60",
       x = "tick", y = "attendance")
print(p)
ggsave(fig_file("02-el-farol-short.png"), p, width = 6, height = 4, dpi = 120)
