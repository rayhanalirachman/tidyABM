# 5. Rumour Mill  (Daley & Kendall 1964, the stochastic rumour)
#
# Ignorant + spreader -> spreader; a spreader who meets another spreader or a
# stifler becomes a stifler. The rumour dies out before everyone has heard it.
library(tidyABM)

rumour <- abm_setup(agents = abm_agents(
  n = 200, state = ~c("spreader", rep("ignorant", n - 1))))

go <- abm_go(
  abm_match(pair = "random"),
  abm_rules(state ~ case_when(
    state == "ignorant" & partner_state == "spreader" ~ "spreader",
    state == "spreader" & partner_state == "spreader" ~ "stifler",
    state == "spreader" & partner_state == "stifler"  ~ "stifler",
    TRUE ~ state))
)

result <- abm_run(rumour, go, ticks = 100, seed = 5)

states <- c("ignorant", "spreader", "stifler")
series <- do.call(rbind, lapply(states, function(s)
  data.frame(state = s,
             tick  = sort(unique(result$tick)),
             n     = as.numeric(tapply(result$state == s, result$tick, sum)))))

cat("200 agents, one spreader, 100 ticks\n\n")
cat(sprintf("%-6s %10s %10s %10s\n", "tick", "ignorant", "spreader", "stifler"))
for (t in c(0, 2, 4, 6, 8, 10, 20, 50, 100)) {
  r <- series[series$tick == t, ]
  cat(sprintf("%-6d %10d %10d %10d\n", t,
              r$n[r$state == "ignorant"], r$n[r$state == "spreader"],
              r$n[r$state == "stifler"]))
}
never <- series$n[series$state == "ignorant" & series$tick == 100]
cat(sprintf("\nnever heard it: %d of 200 (%.1f%%)\n", never, 100 * never / 200))

# --- figure ---------------------------------------------------------------
library(ggplot2)

fig_file <- function(name) {
  a <- grep("^--file=", commandArgs(), value = TRUE)
  d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
  dir.create(file.path(d, "..", "figures"), showWarnings = FALSE)
  file.path(d, "..", "figures", name)
}

p <- ggplot(series, aes(tick, n, colour = state)) +
  geom_line(linewidth = 0.7) +
  theme_minimal() +
  labs(title = "Rumour Mill: the rumour stalls before everyone hears it",
       x = "tick", y = "agents", colour = NULL)
print(p)
ggsave(fig_file("05-rumour-mill.png"), p, width = 6, height = 4, dpi = 120)
