# 17. Giant Component  (Erdos & Renyi 1960; NetLogo Networks)
#
# 400 isolated nodes, edges added at random. The largest connected component
# stays small until the mean degree passes 1, then suddenly holds almost
# everybody. `abm_link()` is what makes a graph process with a fixed population
# expressible at all.
library(tidyABM)

gc_model <- abm_setup(agents = abm_agents(n = 400),
                      network = abm_network(type = "empty"))

go <- abm_go(
  abm_match(pair = "random", eligible = runif(n()) < 0.05),
  abm_link()
)

# The result carries only the final network, so the sweep is one run per
# tick budget rather than one run read at several ticks.
largest <- function(n, edges) {
  parent <- seq_len(n)
  root <- function(i) { while (parent[i] != i) i <- parent[i]; i }
  for (k in seq_len(nrow(edges))) {
    a <- root(edges$from[k]); b <- root(edges$to[k])
    if (a != b) parent[a] <- b
  }
  max(table(vapply(seq_len(n), root, integer(1))))
}

cat("400 nodes, random edges, largest component against mean degree\n\n")
cat(sprintf("%-8s %12s %14s %12s\n", "ticks", "edges", "mean degree",
            "largest"))
sweep <- do.call(rbind, lapply(c(10, 20, 30, 40, 50, 60, 80, 100, 150, 200),
  function(tk) {
    r <- abm_run(gc_model, go, ticks = tk, seed = 1)
    e <- abm_edges(r)
    d <- 2 * nrow(e) / 400
    l <- largest(400, e)
    cat(sprintf("%-8d %12d %14.2f %11.0f%%\n", tk, nrow(e), d, 100 * l / 400))
    data.frame(ticks = tk, degree = d, largest = l / 400)
  }))
cat("\nthe jump happens as the mean degree crosses 1, which is the\n")
cat("Erdos-Renyi percolation threshold\n")

# --- figure ---------------------------------------------------------------
# Parameter sweep: the order parameter against the control parameter.
library(ggplot2)

fig_file <- function(name) {
  a <- grep("^--file=", commandArgs(), value = TRUE)
  d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
  dir.create(file.path(d, "..", "figures"), showWarnings = FALSE)
  file.path(d, "..", "figures", name)
}

p <- ggplot(sweep, aes(degree, largest)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_line() + geom_point() +
  ylim(0, 1) +
  theme_minimal() +
  labs(title = "Giant component: percolation at mean degree 1",
       subtitle = "400 nodes; dashed line is the Erdos-Renyi threshold",
       x = "mean degree", y = "largest component, share of nodes")
print(p)
ggsave(fig_file("17-giant-component.png"), p, width = 6, height = 4, dpi = 120)
