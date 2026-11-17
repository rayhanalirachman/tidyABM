library(tidyABM)

# 27. Granovetter (1978) threshold model of collective behaviour ------------
# Each agent has a threshold: the number of OTHER people already rioting that
# it needs to see before it joins. Everyone updates simultaneously against
# last tick's count.

riot_run <- function(thresholds, ticks = 120, seed = 1) {
  crowd <- abm_setup(
    agents  = abm_agents(n = length(thresholds), threshold = thresholds,
                         rioting = FALSE),
    globals = list(n_rioting = 0)
  )
  go <- abm_go(
    abm_rules(rioting ~ n_rioting >= threshold),
    abm_global(n_rioting ~ sum(rioting))
  )
  abm_run(crowd, go, ticks = ticks, seed = seed)
}

# Granovetter's own example: thresholds 0,1,2,...,99 -> everyone riots
r_uniform <- riot_run(0:99)
cat("uniform 0..99 final rioters:",
    sum(r_uniform$rioting[r_uniform$tick == 120]), "\n")

# one person changed: the agent with threshold 1 now has threshold 2
bumped <- 0:99; bumped[2] <- 2
r_bumped <- riot_run(bumped)
cat("with the 1 changed to a 2:",
    sum(r_bumped$rioting[r_bumped$tick == 120]), "\n")

# the cascade itself
cat("cascade (uniform):",
    head(tapply(r_uniform$rioting, r_uniform$tick, sum), 12), "\n")

# --- figure ---------------------------------------------------------------
# The cascade itself: the number rioting against tick, for the two crowds that
# differ by one person's threshold.
library(ggplot2)

fig_file <- function(name) {
  a <- grep("^--file=", commandArgs(), value = TRUE)
  d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
  dir.create(file.path(d, "..", "figures"), showWarnings = FALSE)
  file.path(d, "..", "figures", name)
}

curve_of <- function(r, lab) data.frame(
  crowd = lab, tick = sort(unique(r$tick)),
  rioting = as.numeric(tapply(r$rioting, r$tick, sum)))

d <- rbind(curve_of(r_uniform, "thresholds 0..99"),
           curve_of(r_bumped,  "the 1 changed to a 2"))
d <- d[d$tick <= 15, ]

p <- ggplot(d, aes(tick, rioting, colour = crowd)) +
  geom_line(linewidth = 0.7) + geom_point(size = 1) +
  theme_minimal() +
  labs(title = "Granovetter: one person's threshold decides the whole crowd",
       subtitle = "100 agents; everyone riots, or three people do",
       x = "tick", y = "rioting", colour = NULL)
print(p)
ggsave(fig_file("27-threshold-model-of-collective-behaviour.png"), p,
       width = 6, height = 4, dpi = 120)
