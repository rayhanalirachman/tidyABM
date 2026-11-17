# 1. Simple Economy  (Wilensky & Rand, *An Introduction to Agent-Based
#    Modeling*, ch. 2)
#
# Everyone starts with $100 and every tick each agent with money hands $1 to
# someone else. Nothing about the rule prefers anyone, and the wealth
# distribution still goes from a spike at 100 to an exponential tail.
library(tidyABM)

economy <- abm_setup(agents = abm_agents(n = 500, money = 100))

go <- abm_go(
  abm_match(pair = "random", role = list(giver = money > 0, receiver = TRUE)),
  abm_rules(money ~ if_else(.role == "giver", money - 1, money + 1))
)

result <- abm_run(economy, go, ticks = 1000, seed = 1)

start <- result$money[result$tick == 0]
final <- result$money[result$tick == 1000]

cat("500 agents, $100 each, 1000 ticks\n\n")
cat(sprintf("%-8s %7s %7s %7s %7s %7s\n", "tick", "min", "p25", "median",
            "p90", "max"))
for (t in c(0, 10, 100, 1000)) {
  m <- result$money[result$tick == t]
  cat(sprintf("%-8d %7.0f %7.0f %7.0f %7.0f %7.0f\n", t, min(m),
              quantile(m, 0.25), median(m), quantile(m, 0.90), max(m)))
}
cat(sprintf("\nmean is conserved: %.1f -> %.1f\n", mean(start), mean(final)))
cat(sprintf("share held by the top decile: %.3f -> %.3f\n",
            sum(sort(start, decreasing = TRUE)[1:50]) / sum(start),
            sum(sort(final, decreasing = TRUE)[1:50]) / sum(final)))
cat(sprintf("agents with nothing at tick 1000: %d\n", sum(final == 0)))

# --- figure ---------------------------------------------------------------
# The claim is about the shape of the final distribution, so that is what the
# figure shows.
library(ggplot2)

fig_file <- function(name) {
  a <- grep("^--file=", commandArgs(), value = TRUE)
  d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
  dir.create(file.path(d, "..", "figures"), showWarnings = FALSE)
  file.path(d, "..", "figures", name)
}

p <- ggplot(data.frame(money = final), aes(money)) +
  geom_histogram(binwidth = 10, boundary = 0, fill = "grey30") +
  theme_minimal() +
  labs(title = "Simple Economy: wealth after 1000 ticks of $1 gifts",
       subtitle = "500 agents, all starting at $100",
       x = "money", y = "agents")
print(p)
ggsave(fig_file("01-simple-economy.png"), p, width = 6, height = 4, dpi = 120)
