# 4. Ethnocentrism, short form
#
# Tags, unconditional strategies, births above 20 resource and deaths at 0. The
# tag does no work because the strategies never look at it, so defection
# dominates and the population runs down. The corrected version is model 15.
library(tidyABM)

ethno <- abm_setup(agents = abm_agents(
  n = 100,
  tag      = ~sample(c("red", "blue"), n, replace = TRUE),
  strategy = ~sample(c("cooperate", "defect"), n, replace = TRUE),
  resource = 10))

go <- abm_go(
  abm_match(pair = "random"),
  abm_rules(resource ~ case_when(
    strategy == "cooperate" & partner_strategy == "cooperate" ~ resource + 2,
    strategy == "defect"    & partner_strategy == "cooperate" ~ resource + 4,
    strategy == "cooperate" & partner_strategy == "defect"    ~ resource - 1,
    TRUE ~ resource) - 1),
  abm_birth(when = resource > 20, cost = resource ~ resource / 2),
  abm_death(when = resource <= 0)
)

result <- abm_run(ethno, go, ticks = 30, seed = 4)

series <- do.call(rbind, lapply(split(result, result$tick), function(d)
  data.frame(tick = d$tick[1], n = nrow(d),
             coop = mean(d$strategy == "cooperate"),
             red  = mean(d$tag == "red"))))

cat("100 agents, two tags, unconditional strategies, 30 ticks\n\n")
cat(sprintf("%-6s %6s %12s %8s\n", "tick", "n", "cooperating", "red"))
for (t in c(0, 5, 10, 15, 20, 25, 30)) {
  r <- series[series$tick == t, ]
  if (nrow(r)) cat(sprintf("%-6d %6d %12.2f %8.2f\n", r$tick, r$n, r$coop, r$red))
}
cat("\nthe tag never enters a rule, so its share is a random walk and the\n")
cat("cooperators are eaten: this is the model failing, not converging\n")

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
  labs(title = "Ethnocentrism, short form: unconditional strategies lose the tag",
       subtitle = "cooperators are driven out; the tag predicts nothing",
       x = "tick", y = "share cooperating")
print(p)
ggsave(fig_file("04-ethnocentrism-short.png"), p, width = 6, height = 4, dpi = 120)
