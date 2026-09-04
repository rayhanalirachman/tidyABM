# 18. Small Worlds  (Watts & Strogatz 1998, Nature 393: 440-442)
#
# A ring lattice, rewired a little. Path length collapses while clustering
# barely moves. The re-link must use `pair = "one_of"`, not `"random"`:
# `"random"` partitions the eligible agents among themselves, so R agents drop
# R edges and gain only R/2, and the network bleeds edges every tick.
library(tidyABM)

N <- 300
ring <- do.call(rbind, lapply(1:2, function(d)
  data.frame(from = 1:N, to = ((1:N + d - 1) %% N) + 1)))

sw <- abm_setup(agents = abm_agents(n = N, rewiring = FALSE),
                network = abm_network(type = "manual", edges = ring))

go <- function(p) abm_go(
  abm_rules(rewiring ~ runif(n()) < p, .scope = "population"),
  abm_match(pair = "network", eligible = rewiring), abm_unlink(),
  abm_match(pair = "one_of",  eligible = rewiring), abm_link()
)

# --- graph measures, base R -----------------------------------------------
adj <- function(n, e) {
  a <- vector("list", n)
  for (k in seq_len(nrow(e))) {
    a[[e$from[k]]] <- c(a[[e$from[k]]], e$to[k])
    a[[e$to[k]]]   <- c(a[[e$to[k]]],   e$from[k])
  }
  lapply(a, unique)
}
clustering <- function(a) {
  cc <- vapply(a, function(nb) {
    k <- length(nb)
    if (k < 2) return(NA_real_)
    links <- sum(vapply(nb, function(u) sum(a[[u]] %in% nb), integer(1))) / 2
    links / (k * (k - 1) / 2)
  }, numeric(1))
  mean(cc, na.rm = TRUE)
}
path_length <- function(a) {   # mean geodesic, BFS from every node
  n <- length(a)
  tot <- 0; cnt <- 0
  for (s in seq_len(n)) {
    dist <- rep(NA_integer_, n); dist[s] <- 0L
    frontier <- s
    while (length(frontier)) {
      nxt <- unique(unlist(a[frontier]))
      nxt <- nxt[is.na(dist[nxt])]
      if (!length(nxt)) break
      dist[nxt] <- dist[frontier[1]] + 1L
      frontier <- nxt
    }
    d <- dist[-s]; d <- d[!is.na(d)]
    tot <- tot + sum(d); cnt <- cnt + length(d)
  }
  tot / cnt
}
report <- function(e, label) {
  a <- adj(N, e)
  cat(sprintf("%-22s %8d %12.2f %14.1f\n", label, nrow(e), clustering(a),
              path_length(a)))
  data.frame(label = label, edges = nrow(e), clustering = clustering(a),
             path = path_length(a))
}

cat("300 nodes, ring lattice of degree 4, 20 ticks of rewiring\n\n")
cat(sprintf("%-22s %8s %12s %14s\n", "", "edges", "clustering", "path length"))
rows <- list(report(ring, "ring lattice"))
for (p in c(0.001, 0.005, 0.02, 0.05)) {
  r <- abm_run(sw, go(p), ticks = 20, seed = 2)
  rows[[length(rows) + 1]] <- report(abm_edges(r), sprintf("p = %.3f", p))
}
sweep <- do.call(rbind, rows)
cat("\nthe edge count holds at 600 throughout, which is what `one_of` buys;\n")
cat("`pair = \"random\"` here loses roughly half the rewired edges every tick\n")

# --- figure ---------------------------------------------------------------
# Parameter sweep: both measures normalised to the ring lattice, which is the
# figure Watts and Strogatz actually drew.
library(ggplot2)

fig_file <- function(name) {
  a <- grep("^--file=", commandArgs(), value = TRUE)
  d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
  dir.create(file.path(d, "..", "figures"), showWarnings = FALSE)
  file.path(d, "..", "figures", name)
}

s <- sweep[-1, ]
s$p <- c(0.001, 0.005, 0.02, 0.05)
d <- rbind(
  data.frame(p = s$p, measure = "clustering C(p)/C(0)",
             value = s$clustering / sweep$clustering[1]),
  data.frame(p = s$p, measure = "path length L(p)/L(0)",
             value = s$path / sweep$path[1]))

p <- ggplot(d, aes(p, value, colour = measure)) +
  geom_line() + geom_point() +
  scale_x_log10() + ylim(0, 1) +
  theme_minimal() +
  labs(title = "Small worlds: path length falls long before clustering does",
       subtitle = "300 nodes, degree 4, 20 ticks of rewiring at rate p",
       x = "rewiring probability per tick (log scale)",
       y = "relative to the ring lattice", colour = NULL)
print(p)
ggsave(fig_file("18-small-worlds.png"), p, width = 6, height = 4, dpi = 120)
