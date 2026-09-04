# 12. Random Consumption Zakah, short form (custom)
#
# Consume, collect 2.5% from everyone above the nisab, share it among those
# below an absolute poverty line. It stops working within a handful of ticks:
# wealth grows, the fixed line is left behind, and there is nobody to pay. The
# page says "about ten"; at this seed and n = 300 it is tick 2, because the
# first transfer is large enough to lift the whole lower tail at once.
# The corrected version is model 16.
library(tidyABM)

NISAB <- 100; poverty_line <- 30

zakah <- abm_setup(
  agents  = abm_agents(n = 300, wealth = ~rlnorm(n, 4, 0.5),
                       income = ~rlnorm(n, 3, 0.4)),
  globals = list(zakah_pool = 0))

go <- abm_go(
  abm_rules(wealth ~ wealth + income - (0.6 * income + 0.02 * wealth)),
  abm_global(zakah_pool ~ sum(if_else(wealth > NISAB, wealth * 0.025, 0))),
  abm_rules(wealth ~ if_else(wealth > NISAB, wealth * 0.975, wealth)),
  abm_rules(wealth ~ if_else(wealth < poverty_line,
                             wealth + zakah_pool / sum(wealth < poverty_line),
                             wealth))
)

result <- abm_run(zakah, go, ticks = 50, seed = 12)

gini <- function(x) {
  x <- sort(x); n <- length(x)
  sum((2 * seq_len(n) - n - 1) * x) / (n * sum(x))
}
series <- data.frame(
  tick = sort(unique(result$tick)),
  poor = as.numeric(tapply(result$wealth < poverty_line, result$tick, sum)),
  med  = as.numeric(tapply(result$wealth, result$tick, median)),
  gini = as.numeric(tapply(result$wealth, result$tick, gini)))

cat("300 households, nisab 100, absolute poverty line 30, 50 ticks\n\n")
cat(sprintf("%-6s %8s %10s %8s\n", "tick", "poor", "median", "gini"))
for (t in c(0, 2, 5, 10, 15, 25, 50)) {
  r <- series[series$tick == t, ]
  cat(sprintf("%-6d %8d %10.1f %8.3f\n", r$tick, r$poor, r$med, r$gini))
}
empty <- series$tick[series$poor == 0]
cat(sprintf("\nnobody is below the line from tick %s onward: the transfer has no\n",
            if (length(empty)) min(empty) else "never"))
cat("recipients and the model has nothing left to say\n")

# --- figure ---------------------------------------------------------------
library(ggplot2)

fig_file <- function(name) {
  a <- grep("^--file=", commandArgs(), value = TRUE)
  d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
  dir.create(file.path(d, "..", "figures"), showWarnings = FALSE)
  file.path(d, "..", "figures", name)
}

p <- ggplot(series, aes(tick, poor)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Zakah, short form: the absolute poverty line empties out",
       subtitle = "households below a fixed line of 30, in a world where wealth grows",
       x = "tick", y = "households below the line")
print(p)
ggsave(fig_file("12-random-consumption-zakah-short.png"), p,
       width = 6, height = 4, dpi = 120)
