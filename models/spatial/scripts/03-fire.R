# 3. Forest Fire  (Fire, NetLogo Models Library, Wilensky 1997)
#
# Patches only, von Neumann-4, bounded. `diagonals = FALSE` is load-bearing: it
# is what puts the transition at the square-lattice *site*-percolation
# threshold rather than at the Moore one.
#
# Setup is a single random field; ignition is its own `abm_rules()` step, so the
# start pattern is one swappable line (`.x == 1` for the left edge,
# `.x == w %/% 2 & .y == w %/% 2` for a centre point).
#
# For a density study a run is an *experiment*, not an animation, so the spread
# is wrapped in `abm_repeat(until = !any(state == "burning"))` and the model is
# run for a single tick: one run, one completed fire, stopping the moment the
# front goes out instead of grinding through a fixed tick budget.
#
# Reported: fraction of trees burned against density.

library(tidyABM)

burn <- function(density, w = 100, seed = 1) {
  m <- abm_setup(
    agents  = abm_agents(state = ~if_else(runif(n) < density, "tree", "empty")),
    network = abm_network(type = "grid", dims = c(w, w),
                          diagonals = FALSE, torus = FALSE),
    seed = seed)

  go <- abm_go(
    abm_rules(state ~ if_else(state == "tree" & .x == 1, "burning", state)),
    abm_repeat(
      abm_neighbours(hot ~ any(state == "burning")),
      abm_rules(state ~ case_when(state == "burning"    ~ "burnt",
                                  state == "tree" & hot ~ "burning",
                                  TRUE                  ~ state)),
      until = !any(state == "burning"),
      max   = 4 * w)
  )

  r <- abm_run(m, go, ticks = 1, seed = seed, record = "final", progress = FALSE)
  fin <- r[r$tick == 1, ]
  trees <- sum(fin$state != "empty")
  c(burned  = if (trees) sum(fin$state == "burnt") / trees else 0,
    spanned = as.numeric(any(fin$state == "burnt" & fin$.x == w)))
}

dens <- seq(0.40, 0.80, by = 0.05)
out <- t(vapply(dens, function(d) {
  rowMeans(vapply(1:3, function(s) burn(d, seed = s), numeric(2)))
}, numeric(2)))

cat("100x100 bounded von Neumann grid, ignition on the left edge, 3 seeds\n\n")
print(data.frame(density = dens,
                 burned  = round(out[, 1], 3),
                 spanned = out[, 2]),
      row.names = FALSE)
cat("\n  `burned` is the fraction of trees that burned, `spanned` the fraction\n")
cat("  of seeds where the fire reached the far edge.\n")
cat("  p_c for site percolation on the square lattice is about 0.5927; the\n")
cat("  jump in both columns brackets it. With diagonals = TRUE (Moore spread)\n")
cat("  the transition drops to about 0.41 instead.\n")

# --- figure ---------------------------------------------------------------
# Parameter sweep: the percolation transition, with the square-lattice site
# threshold drawn on.
library(ggplot2)

fig_file <- function(name) {
  a <- grep("^--file=", commandArgs(), value = TRUE)
  d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
  dir.create(file.path(d, "..", "figures"), showWarnings = FALSE)
  file.path(d, "..", "figures", name)
}

d <- rbind(
  data.frame(density = dens, measure = "fraction of trees burned",
             value = out[, 1]),
  data.frame(density = dens, measure = "fires that spanned the grid",
             value = out[, 2]))

p <- ggplot(d, aes(density, value, colour = measure)) +
  geom_vline(xintercept = 0.5927, linetype = "dashed", colour = "grey50") +
  geom_line(linewidth = 0.7) + geom_point() +
  ylim(0, 1) +
  theme_minimal() +
  labs(title = "Forest Fire: the transition brackets p_c = 0.5927",
       subtitle = "100x100 bounded von Neumann grid, ignition on the left edge, 3 seeds",
       x = "tree density", y = NULL, colour = NULL)
print(p)
ggsave(fig_file("03-fire.png"), p, width = 6, height = 4, dpi = 120)
