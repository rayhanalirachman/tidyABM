library(tidyABM)

# 33. Language Change (Troutman & Wilensky 2007, NetLogo Social Science) -----
# Language users on a preferential-attachment network. Each tick everyone
# utters a form drawn from their own grammar weight and then updates on what
# they heard. Two of the paper's update algorithms are shown.

# the network comes from running the package's preferential-attachment model
pa_network <- function(n, seed = 1) {
  seed_net <- abm_setup(
    agents  = abm_agents(n = 2, dummy = 0),
    network = abm_network(type = "manual", edges = data.frame(from = 1, to = 2))
  )
  grow <- abm_go(
    abm_birth(n = 1,
              attach_via = abm_match(pair = "network", from = "random_edge")),
    abm_rules(dummy ~ dummy)
  )
  grown <- abm_run(seed_net, grow, ticks = n - 2, seed = seed)
  abm_edges(grown)
}

language <- function(algorithm = c("threshold", "reward"), n = 200,
                     alpha = 0.5, rate = 0.2, start = 0.3,
                     ticks = 100, seed = 1) {
  algorithm <- match.arg(algorithm)
  pop <- abm_setup(
    agents  = abm_agents(n = n, w = ~as.numeric(runif(n) < start)),
    network = abm_network(type = "manual", edges = pa_network(n, seed)),
    globals = list(alpha = alpha, rate = rate),
    seed    = seed
  )
  update <- if (algorithm == "threshold") {
    abm_rules(w ~ if_else(is.na(heard), w, as.numeric(heard >= alpha)))
  } else {
    abm_rules(w ~ if_else(is.na(heard), w, (1 - rate) * w + rate * heard))
  }
  go <- abm_go(
    abm_rules(utterance ~ as.numeric(runif(n()) < w)),   # speak
    abm_neighbours(heard ~ mean(utterance)),             # listen
    update
  )
  abm_run(pop, go, ticks = ticks, seed = seed)
}

degree_of <- function(res) {
  e <- abm_edges(res)
  table(c(e$from, e$to))
}

sweep <- do.call(rbind, lapply(c("threshold", "reward"), function(alg)
  do.call(rbind, lapply(c(0.2, 0.4, 0.5, 0.6, 0.8), function(s0) {
    r <- language(alg, start = s0)
    final <- r[r$tick == max(r$tick), ]
    row <- data.frame(algorithm = alg, target = s0,
                      start = mean(r$w[r$tick == 0]), end = mean(final$w))
    cat(sprintf("%-9s start %.1f (actual %.2f) -> end %.2f\n",
                alg, s0, row$start, row$end))
    row
  }))))

# --- figure ---------------------------------------------------------------
# Parameter sweep: where the grammar ends up against where it started, for the
# two update algorithms. The threshold rule is a step; the reward rule is not.
library(ggplot2)

fig_file <- function(name) {
  a <- grep("^--file=", commandArgs(), value = TRUE)
  d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
  dir.create(file.path(d, "..", "figures"), showWarnings = FALSE)
  file.path(d, "..", "figures", name)
}

fig <- ggplot(sweep, aes(start, end, colour = algorithm)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              colour = "grey50") +
  geom_line(linewidth = 0.7) + geom_point() +
  xlim(0, 1) + ylim(0, 1) +
  theme_minimal() +
  labs(title = "Language change: thresholds tip, rewards drift",
       subtitle = "200 speakers on a preferential-attachment network, 100 ticks",
       x = "share using the new form at tick 0",
       y = "share at tick 100", colour = NULL)
print(fig)
ggsave(fig_file("33-language-change.png"), fig, width = 6, height = 4,
       dpi = 120)
