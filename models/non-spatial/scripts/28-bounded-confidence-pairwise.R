library(tidyABM)
library(dplyr)

# 28. Deffuant et al. (2000) bounded confidence -----------------------------
deffuant <- function(d, mu = 0.5, n = 1000, ticks = 600, seed = 1) {
  pop <- abm_setup(
    agents  = abm_agents(n = n, opinion = ~runif(n)),
    globals = list(d = d, mu = mu),
    seed    = 42
  )
  go <- abm_go(
    abm_match(pair = "random"),
    abm_rules(opinion ~ if_else(abs(opinion - partner_opinion) < d,
                                opinion + mu * (partner_opinion - opinion),
                                opinion))
  )
  abm_run(pop, go, ticks = ticks, seed = seed)
}

n_clusters <- function(res, tol = 0.01) {
  x <- sort(res$opinion[res$tick == max(res$tick)])
  sum(diff(x) > tol) + 1L
}

sweep <- do.call(rbind, lapply(c(0.5, 0.3, 0.2, 0.15, 0.1), function(d) {
  r <- deffuant(d)
  k <- n_clusters(r)
  cat(sprintf("d = %-5.2f  clusters = %d   predicted floor(1/(2d)) = %d\n",
              d, k, floor(1 / (2 * d))))
  data.frame(d = d, clusters = k, predicted = floor(1 / (2 * d)))
}))

# where the mass actually sits
r <- deffuant(0.2)
x <- r$opinion[r$tick == max(r$tick)]
print(round(sort(unique(round(x, 3))), 3))

# --- figure ---------------------------------------------------------------
# Parameter sweep: the cluster count against the confidence bound, next to the
# floor(1/(2d)) rule the literature quotes.
library(ggplot2)

fig_file <- function(name) {
  a <- grep("^--file=", commandArgs(), value = TRUE)
  d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
  dir.create(file.path(d, "..", "figures"), showWarnings = FALSE)
  file.path(d, "..", "figures", name)
}

fd <- rbind(
  data.frame(d = sweep$d, what = "observed",        k = sweep$clusters),
  data.frame(d = sweep$d, what = "floor(1 / (2d))", k = sweep$predicted))

fig <- ggplot(fd, aes(d, k, colour = what)) +
  geom_line(linewidth = 0.7) + geom_point() +
  theme_minimal() +
  labs(title = "Deffuant: the confidence bound sets the number of clusters",
       subtitle = "1000 agents, mu = 0.5, 600 ticks",
       x = "confidence bound d", y = "opinion clusters", colour = NULL)
print(fig)
ggsave(fig_file("28-bounded-confidence-pairwise.png"), fig,
       width = 6, height = 4, dpi = 120)
