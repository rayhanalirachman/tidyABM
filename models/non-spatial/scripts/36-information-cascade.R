library(tidyABM)

# 36. Information cascade (Bikhchandani, Hirshleifer & Welch 1992) ----------
# Agents decide one at a time. Each sees a private signal that is right with
# probability p, and the *decisions*, not the signals, of everyone before it.
# Once the public tally leads by two, the signal stops mattering and everybody
# after copies, right or wrong.

side <- function(signal, nA, nB) {
  d <- nA - nB
  if_else(d >= 2, "A", if_else(d <= -2, "B", signal))
}

cascade <- function(p = 0.7, n = 50, seed = 1) {
  # the truth is A; a signal is right with probability p
  pop <- abm_setup(
    agents  = abm_agents(n = n,
                         signal = ~if_else(runif(n) < p, "A", "B"),
                         decision = NA_character_),
    globals = list(nA = 0, nB = 0),
    seed    = seed
  )
  go <- abm_go(
    abm_sequential(
      decision ~ side(signal, nA, nB),
      nA ~ nA + (side(signal, nA, nB) == "A"),
      nB ~ nB + (side(signal, nA, nB) == "B")
    )
  )
  abm_run(pop, go, ticks = 1, seed = seed)
}

share_A <- function(p, seeds = 1:400) {
  vapply(seeds, function(s) {
    r <- cascade(p = p, seed = s)
    mean(r$decision[r$tick == 1] == "A")
  }, numeric(1))
}

sweep <- do.call(rbind, lapply(c(0.55, 0.6, 0.7, 0.8, 0.9), function(p) {
  runs <- share_A(p)
  row <- data.frame(p = p, observed = mean(runs > 0.5),
                    bhw = p^2 / (p^2 + (1 - p)^2))
  cat(sprintf("p = %.2f   correct cascades %.3f   BHW p^2/(p^2+(1-p)^2) = %.3f\n",
              p, row$observed, row$bhw))
  row
}))

# --- figure ---------------------------------------------------------------
# Parameter sweep: the share of runs that cascade on the right answer, against
# Bikhchandani, Hirshleifer and Welch's closed form.
library(ggplot2)

fig_file <- function(name) {
  a <- grep("^--file=", commandArgs(), value = TRUE)
  d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
  dir.create(file.path(d, "..", "figures"), showWarnings = FALSE)
  file.path(d, "..", "figures", name)
}

d <- rbind(
  data.frame(p = sweep$p, what = "observed",        value = sweep$observed),
  data.frame(p = sweep$p, what = "BHW closed form", value = sweep$bhw))

fig <- ggplot(d, aes(p, value, colour = what)) +
  geom_line(linewidth = 0.7) + geom_point() +
  ylim(0, 1) +
  theme_minimal() +
  labs(title = "Information cascade: right answers are not guaranteed",
       subtitle = "50 agents deciding in sequence, 400 replicates per point",
       x = "signal accuracy p", y = "share of runs cascading on A",
       colour = NULL)
print(fig)
ggsave(fig_file("36-information-cascade.png"), fig, width = 6, height = 4,
       dpi = 120)
