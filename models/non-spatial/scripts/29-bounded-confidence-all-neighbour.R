library(tidyABM)

# 29. Hegselmann & Krause (2002) bounded confidence, all-neighbour averaging
hk <- function(eps, n = 500, ticks = 50, seed = 42) {
  pop <- abm_setup(agents = abm_agents(n = n, opinion = ~runif(n)),
                   globals = list(eps = eps), seed = seed)
  # "the mean opinion of everyone within eps of me" is a neighbourhood in
  # opinion space rather than in a network, which is what `within =` says. The
  # agent is inside its own -- the condition holds of it -- which is what
  # Hegselmann and Krause mean by a confidence set.
  go <- abm_go(
    abm_neighbours(opinion ~ mean(opinion),
                   within = abs(opinion - own_opinion) <= eps)
  )
  abm_run(pop, go, ticks = ticks)
}

clusters <- function(res, tol = 1e-6) {
  x <- sort(res$opinion[res$tick == max(res$tick)])
  sum(diff(x) > tol) + 1L
}

sweep <- do.call(rbind, lapply(c(0.30, 0.25, 0.20, 0.15, 0.10, 0.05),
  function(eps) {
    r <- hk(eps)
    k <- clusters(r)
    cat(sprintf("eps = %.2f  clusters = %d   1/(2*eps) = %.1f\n",
                eps, k, 1 / (2 * eps)))
    data.frame(eps = eps, clusters = k, predicted = 1 / (2 * eps))
  }))

# --- figure ---------------------------------------------------------------
# Parameter sweep: cluster count against the confidence radius, against the
# 1/(2 eps) rule.
library(ggplot2)

fig_file <- function(name) {
  a <- grep("^--file=", commandArgs(), value = TRUE)
  d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
  dir.create(file.path(d, "..", "figures"), showWarnings = FALSE)
  file.path(d, "..", "figures", name)
}

fd <- rbind(
  data.frame(eps = sweep$eps, what = "observed",     k = sweep$clusters),
  data.frame(eps = sweep$eps, what = "1 / (2 eps)",  k = sweep$predicted))

fig <- ggplot(fd, aes(eps, k, colour = what)) +
  geom_line(linewidth = 0.7) + geom_point() +
  theme_minimal() +
  labs(title = "Hegselmann-Krause: consensus clusters scale as 1 / (2 eps)",
       subtitle = "500 agents, all-neighbour averaging, 50 ticks",
       x = "confidence radius eps", y = "opinion clusters", colour = NULL)
print(fig)
ggsave(fig_file("29-bounded-confidence-all-neighbour.png"), fig,
       width = 6, height = 4, dpi = 120)
