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

forest <- function(density, w = 100, seed = 1) {
  abm_setup(
    agents  = abm_agents(state = ~if_else(runif(n) < density, "tree", "empty")),
    network = abm_network(type = "grid", dims = c(w, w),
                          diagonals = FALSE, torus = FALSE),
    seed = seed)
}
fire_go <- function(w = 100) abm_go(
  abm_rules(state ~ if_else(state == "tree" & .x == 1, "burning", state)),
  abm_repeat(
    abm_neighbours(hot ~ any(state == "burning")),
    abm_rules(state ~ case_when(state == "burning"    ~ "burnt",
                                state == "tree" & hot ~ "burning",
                                TRUE                  ~ state)),
    until = !any(state == "burning"),
    max   = 4 * w)
)

# one completed fire, returned as its final frame
burn_once <- function(density, w = 100, seed = 1) {
  r <- abm_run(forest(density, w, seed), fire_go(w), ticks = 1, seed = seed,
               record = "final", progress = FALSE)
  r[r$tick == 1, ]
}
burn <- function(density, w = 100, seed = 1) {
  fin <- burn_once(density, w, seed)
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

# --- figures --------------------------------------------------------------
# Parameter sweep: the percolation transition, with the square-lattice site
# threshold drawn on. Then two burnt forests, one either side of it: at 0.55
# the fire dies a few columns in, at 0.65 it crosses the grid. Then the fire
# itself, tick by tick: a strip of the front crossing the grid, and a viewer.
library(ggplot2)

fig_file <- function(name, dir = "figures") {
  a <- grep("^--file=", commandArgs(), value = TRUE)
  d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
  dir.create(file.path(d, "..", dir), showWarnings = FALSE)
  file.path(d, "..", dir, name)
}
# Okabe-Ito, colour-blind safe; the same series colours in every script
pal <- c("#0072B2", "#E69F00", "#009E73")
fig_theme <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom",
        plot.title.position = "plot")

d <- rbind(
  data.frame(density = dens, measure = "fraction of trees burned",
             value = out[, 1]),
  data.frame(density = dens, measure = "fires that spanned the grid",
             value = out[, 2]))

p <- ggplot(d, aes(density, value, colour = measure)) +
  geom_vline(xintercept = 0.5927, linetype = "dashed", colour = "grey50") +
  geom_line(linewidth = 0.7) + geom_point(size = 2) +
  scale_colour_manual(values = pal) +
  scale_y_continuous(limits = c(0, 1)) +
  fig_theme +
  labs(title = "Forest Fire: the transition brackets p_c = 0.5927",
       subtitle = "100x100 bounded von Neumann grid, ignition on the left edge, 3 seeds; dashed line is p_c",
       x = "tree density", y = NULL, colour = NULL)
print(p)
ggsave(fig_file("03-fire.png"), p, width = 7, height = 4.5, dpi = 150, bg = "white")

frames <- rbind(
  cbind(burn_once(0.55), density = "density 0.55: the fire dies out"),
  cbind(burn_once(0.65), density = "density 0.65: the fire crosses the grid"))
fire_colours <- c(tree = "#009E73", burning = "#D55E00", burnt = "grey25",
                  empty = "grey96")
p2 <- abm_plot_lattice(frames, fill = state) +
  facet_wrap(~density) +
  scale_fill_manual(values = fire_colours, breaks = c("tree", "burnt", "empty"),
                    name = NULL) +
  labs(title = "Either side of the threshold, seed 1",
       subtitle = "lit along the left edge; a burnt cell is one the front reached")
print(p2)
ggsave(fig_file("03-fire-lattice.png"), p2, width = 8, height = 4.4, dpi = 150,
       bg = "white")

# --- the spread, tick by tick --------------------------------------------
# The density study wraps a whole fire in one tick. To watch one spread, the
# same two steps are the tick and the ignition moves into the setup; the
# forest is the same draw as before (one runif(n) against the density).
lit_forest <- function(density, w = 100, seed = 1) {
  abm_setup(
    agents  = abm_agents(state = ~case_when(runif(n) >= density ~ "empty",
                                            .x == 1 ~ "burning",
                                            TRUE ~ "tree")),
    network = abm_network(type = "grid", dims = c(w, w),
                          diagonals = FALSE, torus = FALSE),
    seed = seed)
}
spread_go <- abm_go(
  abm_neighbours(hot ~ any(state == "burning")),
  abm_rules(state ~ case_when(state == "burning"    ~ "burnt",
                              state == "tree" & hot ~ "burning",
                              TRUE                  ~ state)),
  abm_global(burning ~ sum(state == "burning")))
out_tick <- function(r) {                     # the first tick with no front
  g <- abm_globals(r)
  out <- g$tick[which(g$tick > 0 & g$burning == 0)[1]]
  if (is.na(out)) max(g$tick) else out
}

spread <- abm_run(lit_forest(0.65), spread_go, ticks = 400, seed = 1,
                  record = 10, progress = FALSE)
done <- out_tick(spread)
strip_ticks <- unique(round(seq(done / 5, done, length.out = 5) / 10) * 10)
p3 <- abm_plot_lattice(spread, fill = state, tick = strip_ticks) +
  scale_fill_manual(values = fire_colours,
                    breaks = c("tree", "burning", "burnt", "empty"), name = NULL) +
  theme(legend.position = "bottom") +
  labs(title = "Density 0.65: the front crosses the grid",
       subtitle = sprintf("100x100, lit along the left edge at tick 0; the fire is out by tick %d",
                          done))
print(p3)
ggsave(fig_file("03-fire-strip.png"), p3, width = 11, height = 3.2, dpi = 150,
       bg = "white")

# --- viewer ---------------------------------------------------------------
# A smaller forest near the threshold, every other tick until the fire is out.
small <- abm_run(lit_forest(0.63, w = 60, seed = 3), spread_go, ticks = 400,
                 seed = 3, record = 2, progress = FALSE)
done_small <- out_tick(small)
abm_view_lattice(
  small, fill = state, tick = sort(unique(small$tick[small$tick <= done_small])),
  file = fig_file("03-fire.html", dir = "viewers"),
  title = "Forest Fire", subtitle = sprintf("60x60 bounded grid, density 0.63, seed 3; out by tick %d", done_small),
  labels = c(fill = "state"), fill_colours = fire_colours)
