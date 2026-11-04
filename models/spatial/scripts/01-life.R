# 1. Conway's Game of Life  (Life Simple, NetLogo Models Library, Wilensky 1998)
#
# Patches only, Moore-8, torus, synchronous update. `abm_neighbours()` is the
# first `ask patches` and `abm_rules()` is the second, and `abm_rules()`'s
# simultaneity *is* the two-`ask` structure -- the synchronous update cannot be
# written wrongly.
#
# Reported: the blinker's period, the glider's velocity, and the density a
# random soup relaxes to.

library(tidyABM)

life_go <- abm_go(
  abm_neighbours(live_n ~ sum(alive)),
  abm_rules(alive ~ live_n == 3 | (alive & live_n == 2)),
  abm_global(density ~ mean(alive))
)

seeded <- function(w, h, cells) {
  abm_setup(agents  = abm_agents(alive = ~seq_len(n) %in% cells),
            network = abm_network(type = "grid", dims = c(w, h)))
}
cid <- function(x, y, w) x + (y - 1) * w

# --- the blinker ---------------------------------------------------------
w <- h <- 12
r <- abm_run(seeded(w, h, c(5, 6, 7) + (6 - 1) * w), life_go, ticks = 4, seed = 1)
cat("Blinker\n")
for (t in 0:4) {
  s <- r[r$tick == t & r$alive, ]
  cat(sprintf("  tick %d  %s\n", t,
              paste(sprintf("(%d,%d)", s$.x, s$.y), collapse = " ")))
}

# --- the glider ----------------------------------------------------------
# started near the top of a 40x40 field so it does not wrap inside the window
w <- h <- 40
glider <- c(cid(3, 32, w), cid(4, 31, w), cid(2, 30, w), cid(3, 30, w),
            cid(4, 30, w))
r <- abm_run(seeded(w, h, glider), life_go, ticks = 24, seed = 1)
cen <- do.call(rbind, lapply(seq(0, 24, by = 4), function(t) {
  s <- r[r$tick == t & r$alive, ]
  data.frame(tick = t, cells = nrow(s), cx = mean(s$.x), cy = mean(s$.y))
}))
cat("\nGlider centroid\n")
print(cen, row.names = FALSE)
cat(sprintf("  velocity: (%+.2f, %+.2f) per tick -- one cell diagonally per 4\n",
            coef(lm(cx ~ tick, cen))[[2]], coef(lm(cy ~ tick, cen))[[2]]))

# --- the random soup -----------------------------------------------------
# The published "ash" density is about 0.0287, and it is approached slowly from
# above; this shows the trajectory rather than claiming the limit at t = 400.
# One run per seed, kept for the figures too: the globals are recorded every
# tick whatever `record =` says, and `"final"` keeps the last frame.
cat("\nRandom soup, 100x100 torus, density 0.10 at t = 0\n")
soups <- lapply(1:3, function(s) {
  m <- abm_setup(agents  = abm_agents(alive = ~runif(n) < 0.10),
                 network = abm_network(type = "grid", dims = c(100, 100)),
                 seed = s)
  abm_run(m, life_go, ticks = 1500, seed = s, record = "final", progress = FALSE)
})
soup <- vapply(soups, function(r) {
  g <- abm_globals(r)
  g$density[match(c(50, 100, 250, 500, 1000, 1500), g$tick)]
}, numeric(6))
tab <- data.frame(tick = c(50, 100, 250, 500, 1000, 1500),
                  seed_1 = round(soup[, 1], 4), seed_2 = round(soup[, 2], 4),
                  seed_3 = round(soup[, 3], 4),
                  mean = round(rowMeans(soup), 4))
print(tab, row.names = FALSE)
cat("  the density decays from above towards the published ash density,\n")
cat("  about 0.0287; a 100x100 torus is small enough that the tail is noisy.\n")

# --- figures --------------------------------------------------------------
# Time series: the random soup's density relaxing towards the published ash
# density, which is the quantitative claim on this page. Then the lattice
# itself: what is left of seed 1's soup at tick 1500 -- still lifes, blinkers
# and the odd glider, which is what "ash" means.
library(ggplot2)

fig_file <- function(name) {
  a <- grep("^--file=", commandArgs(), value = TRUE)
  d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
  dir.create(file.path(d, "..", "figures"), showWarnings = FALSE)
  file.path(d, "..", "figures", name)
}
# Okabe-Ito, colour-blind safe; the same series colours in every script
pal <- c("#0072B2", "#E69F00", "#009E73")
fig_theme <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom",
        plot.title.position = "plot")

d <- do.call(rbind, lapply(1:3, function(s) {
  g <- abm_globals(soups[[s]])
  data.frame(seed = factor(s), tick = g$tick, density = g$density)
}))
d <- d[d$tick > 0, ]

p <- ggplot(d, aes(tick, density, colour = seed)) +
  geom_hline(yintercept = 0.0287, linetype = "dashed", colour = "grey40") +
  geom_line(linewidth = 0.5) +
  scale_colour_manual(values = pal) +
  fig_theme +
  labs(title = "Game of Life: a random soup relaxes to the ash density",
       subtitle = "100x100 torus, started at density 0.10; dashed line is the published 0.0287",
       x = "tick", y = "live fraction", colour = "seed")
print(p)
ggsave(fig_file("01-life.png"), p, width = 7, height = 4.5, dpi = 150, bg = "white")

p2 <- abm_plot_lattice(soups[[1]], fill = alive) +
  scale_fill_manual(values = c(`FALSE` = "grey94", `TRUE` = "grey10"),
                    guide = "none") +
  labs(title = "The ash: seed 1 at tick 1500",
       subtitle = sprintf("live fraction %.4f; still lifes and oscillators, nothing moving",
                          soup[6, 1]))
print(p2)
ggsave(fig_file("01-life-lattice.png"), p2, width = 6, height = 6.4, dpi = 150,
       bg = "white")
