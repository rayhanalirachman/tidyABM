# 19. Fireflies  (Buck 1988; NetLogo Biology)
#
# Reading the flashes *before* advancing the clock reproduces NetLogo's
# two-loop structure, where everyone sees the pattern as it stood at the end of
# the previous tick. `abm_neighbours()` is the point: a match gives one partner,
# and this model needs a count over the whole neighbourhood.
library(tidyABM)

CYCLE <- 10; FLASH <- 1

ff <- abm_setup(
  agents  = abm_agents(n = 600,
                       clock = ~sample(0:(CYCLE - 1), n, replace = TRUE),
                       flashing = FALSE),
  network = abm_network(type = "random", degree = 6))

go <- abm_go(
  abm_neighbours(seen ~ sum(flashing)),                       # last tick's flashes
  abm_rules(clock ~ (clock + 1) %% CYCLE),
  abm_rules(clock ~ if_else(clock >= FLASH & coalesce(seen, 0) >= 1, FLASH, clock)),
  abm_rules(flashing ~ clock < FLASH)                         # phase delay
)

result <- abm_run(ff, go, ticks = 120, seed = 1)

series <- data.frame(
  tick = sort(unique(result$tick)),
  lit  = as.numeric(tapply(result$flashing, result$tick, sum)))

cat("600 fireflies, 10-tick cycle, 6 neighbours each, 120 ticks\n\n")
cat("flashing count, ticks 1-20:\n  ")
cat(paste(series$lit[series$tick %in% 1:20], collapse = " "), "\n")
cat("flashing count, ticks 101-120:\n  ")
cat(paste(series$lit[series$tick %in% 101:120], collapse = " "), "\n\n")

early <- series$lit[series$tick %in% 1:30]
late  <- series$lit[series$tick > 90]
cat(sprintf("ticks 1-30:   peak %d of 600, %d dark ticks\n",
            max(early), sum(early == 0)))
cat(sprintf("ticks 91-120: peak %d of 600, %d dark ticks\n",
            max(late), sum(late == 0)))
cat(sprintf("the settled pattern is a sawtooth: a burst of %d, then dark\n",
            max(late)))

# --- figure ---------------------------------------------------------------
library(ggplot2)

fig_file <- function(name) {
  a <- grep("^--file=", commandArgs(), value = TRUE)
  d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
  dir.create(file.path(d, "..", "figures"), showWarnings = FALSE)
  file.path(d, "..", "figures", name)
}

p <- ggplot(series, aes(tick, lit)) +
  geom_line(linewidth = 0.4) +
  theme_minimal() +
  labs(title = "Fireflies: scattered noise resolves into a clean sawtooth",
       subtitle = "600 fireflies, 10-tick cycle, no leader",
       x = "tick", y = "fireflies flashing")
print(p)
ggsave(fig_file("19-fireflies.png"), p, width = 6, height = 4, dpi = 120)
