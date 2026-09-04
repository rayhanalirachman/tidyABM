# 11. Preferential Attachment  (Barabasi & Albert 1999; Wilensky & Rand ch. 5)
#
# One new agent per tick, linking to an existing agent chosen in proportion to
# its degree. `from = "random_edge"` is NetLogo's trick -- pick an edge
# uniformly, then one of its endpoints -- so attachment is degree-proportional
# without anyone storing a degree.
library(tidyABM)

pa <- abm_setup(
  agents  = abm_agents(n = 2),
  network = abm_network(type = "manual", edges = data.frame(from = 1, to = 2)))

go <- abm_go(abm_birth(
  n = 1, attach_via = abm_match(pair = "network", from = "random_edge")))

result <- abm_run(pa, go, ticks = 300, seed = 11)

e <- abm_edges(result)
deg <- table(factor(c(e$from, e$to), levels = sort(unique(result$.id))))
deg <- as.integer(deg)

cat("2 seed agents, 300 arrivals, one edge each\n\n")
cat(sprintf("agents %d, edges %d, mean degree %.2f\n",
            length(deg), nrow(e), mean(deg)))
cat(sprintf("max degree %d, top five: %s\n", max(deg),
            paste(sort(deg, decreasing = TRUE)[1:5], collapse = " ")))
cat(sprintf("degree 1 holds %.1f%% of agents; the top 5%% hold %.1f%% of edges\n",
            100 * mean(deg == 1),
            100 * sum(sort(deg, decreasing = TRUE)[1:ceiling(0.05 * length(deg))]) /
              sum(deg)))
cat("\ndegree distribution\n")
tab <- table(deg)
cat(sprintf("%-8s %8s\n", "degree", "agents"))
for (k in names(tab)) cat(sprintf("%-8s %8d\n", k, as.integer(tab[[k]])))

# --- figure ---------------------------------------------------------------
# Final-state distribution, on log axes, which is where a scale-free tail is
# supposed to look like a line.
library(ggplot2)

fig_file <- function(name) {
  a <- grep("^--file=", commandArgs(), value = TRUE)
  d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
  dir.create(file.path(d, "..", "figures"), showWarnings = FALSE)
  file.path(d, "..", "figures", name)
}

d <- data.frame(degree = as.integer(names(tab)), agents = as.integer(tab))

p <- ggplot(d, aes(degree, agents)) +
  geom_point() +
  scale_x_log10() + scale_y_log10() +
  theme_minimal() +
  labs(title = "Preferential attachment: a heavy-tailed degree distribution",
       subtitle = "302 agents grown one at a time, log-log axes",
       x = "degree", y = "agents")
print(p)
ggsave(fig_file("11-preferential-attachment.png"), p,
       width = 6, height = 4, dpi = 120)
