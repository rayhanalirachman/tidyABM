# 23. Divide the Cake  (Skyrms 1996; Harms; NetLogo)
#
# `abm_death(when = appetite + partner_appetite > 6)` is the whole model, and it
# is what motivated births and deaths seeing the standing match: before that,
# `when` was evaluated against the bare agent tibble with no `partner_*`.
library(tidyABM)

cake <- abm_setup(agents = abm_agents(
  n = 900, appetite = ~sample(c(2, 3, 4), n, replace = TRUE)))

go <- abm_go(
  abm_match(pair = "random"),
  abm_death(when = appetite + partner_appetite > 6),
  abm_birth(when = runif(n()) < appetite / 6),
  abm_death(when = runif(n()) < pmax(0, (n() - 1200) / n()))
)

result <- abm_run(cake, go, ticks = 150, seed = 1)

names3 <- c("2" = "modest", "3" = "fair", "4" = "greedy")
series <- do.call(rbind, lapply(c(2, 3, 4), function(a)
  data.frame(demand = factor(names3[[as.character(a)]],
                             levels = c("modest", "fair", "greedy")),
             tick = sort(unique(result$tick)),
             share = as.numeric(tapply(result$appetite == a, result$tick, mean)))))

cat("900 agents demanding 1/3, 1/2 or 2/3 of a cake worth 6, 150 ticks\n\n")
cat(sprintf("%-6s %8s %9s %9s %9s\n", "tick", "n", "modest", "fair", "greedy"))
for (t in c(0, 5, 10, 20, 50, 100, 150)) {
  d <- result[result$tick == t, ]
  cat(sprintf("%-6d %8d %9.3f %9.3f %9.3f\n", t, nrow(d),
              mean(d$appetite == 2), mean(d$appetite == 3),
              mean(d$appetite == 4)))
}
gone <- function(lbl) {
  s <- series[series$demand == lbl, ]
  hit <- s$tick[s$share == 0]
  if (length(hit)) min(hit) else NA_integer_
}
cat(sprintf("\ngreedy gone by tick %s, modest gone by tick %s; fair holds\n",
            gone("greedy"), gone("modest")))

# --- figure ---------------------------------------------------------------
library(ggplot2)

fig_file <- function(name) {
  a <- grep("^--file=", commandArgs(), value = TRUE)
  d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
  dir.create(file.path(d, "..", "figures"), showWarnings = FALSE)
  file.path(d, "..", "figures", name)
}

p <- ggplot(series, aes(tick, share, colour = demand)) +
  geom_line(linewidth = 0.7) +
  ylim(0, 1) +
  theme_minimal() +
  labs(title = "Divide the Cake: the fair demand takes over",
       subtitle = "greedy pairs starve each other; modest ones simply breed slower",
       x = "tick", y = "share of population", colour = NULL)
print(p)
ggsave(fig_file("23-divide-the-cake.png"), p, width = 6, height = 4, dpi = 120)
