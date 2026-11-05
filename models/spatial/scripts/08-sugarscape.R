# 8. Sugarscape  (Sugarscape 2 Constant Growback, NetLogo Models Library,
#    Li & Wilensky 2009, after Epstein & Axtell 1996)
#
# The model that forced the three L2+ arguments of `abm_move()`. An agent looks
# `vision` cells along the four axes only (`axes_only = TRUE`), takes the
# richest unoccupied cell it can see (`avoid_occupied = TRUE`) and eats what
# is there; the sugar grows back one unit a tick. Rule M and rule G1.
#
# `range` is a single number -- the model's longest sight -- and an agent's
# own `vision` masks the candidates inside `uphill()`: a cell further away
# than the agent can see scores -Inf. The cell it stands on is always a
# candidate, so an agent that sees nothing better stays. Epstein & Axtell
# break ties towards the *nearest* best cell; here ties are random.
#
# Reported: the carrying capacity, the selection it exerts on vision and
# metabolism, and the wealth distribution that skews under it.

library(tidyABM)

w <- 50
peak <- function(x, y, px, py) 4 * exp(-((x - px)^2 + (y - py)^2) / (2 * 9^2))
gini <- function(x) {
  x <- sort(x); n <- length(x)
  if (n < 2 || sum(x) == 0) return(NA_real_)
  2 * sum(x * seq_len(n)) / (n * sum(x)) - (n + 1) / n
}

sugarscape <- abm_setup(
  agents = list(
    # two sugar mountains, capacity 4 at the summits, terraced down to 0
    patches = abm_agents(
      capacity = ~as.integer(round(pmax(peak(.x, .y, 15, 35), peak(.x, .y, 35, 15)))),
      sugar    = ~capacity),
    people = abm_agents(n = 400,
                        vision     = ~sample.int(6, n, TRUE),
                        metabolism = ~sample.int(4, n, TRUE),
                        wealth     = ~sample(5:25, n, TRUE),
                        at         = ~sample(prod(dims), n))),   # one per cell
  network = abm_network(type = "grid", dims = c(w, w), on = "patches",
                        diagonals = FALSE, torus = TRUE),
  seed = 1)

go <- abm_go(
  # rule M: the richest unoccupied cell within `vision` along the axes
  abm_move(along = "patches", who = "people",
           to = uphill(if_else(pmin(abs(.x - own_.x), w - abs(.x - own_.x)) +
                               pmin(abs(.y - own_.y), w - abs(.y - own_.y)) <= vision,
                               sugar, -Inf)),
           range = 6, axes_only = TRUE, avoid_occupied = TRUE),
  # eat what is there, pay the metabolism
  abm_neighbours(here ~ sum(sugar), within = .group == "patches" & .id == own_.cell),
  abm_rules(wealth ~ wealth + here - metabolism, .scope = "population"),
  abm_tell(sugar ~ 0L, to = .cell, when = .group == "people"),
  abm_death(when = .group == "people" & wealth <= 0),
  # rule G1: constant growback
  abm_rules(sugar ~ pmin(capacity, sugar + 1L), .scope = "population"),
  abm_global(pop    ~ sum(.group == "people"),
             wealth ~ mean(wealth[.group == "people"]),
             gini   ~ gini(wealth[.group == "people"]),
             vis    ~ mean(vision[.group == "people"]),
             met    ~ mean(metabolism[.group == "people"]))
)

cat("50x50 torus, two sugar mountains, 400 agents, 500 ticks\n\n")
# every fifth frame is kept, for the figures and the viewer
r <- abm_run(sugarscape, go, ticks = 500, seed = 1, record = 5, progress = FALSE)
g <- abm_globals(r)
# a global is first computed at the end of tick 1, so tick 0 is read off the
# starting population
p0 <- sugarscape$groups$people
g[g$tick == 0, c("pop", "wealth", "gini", "vis", "met")] <-
  list(nrow(p0), mean(p0$wealth), gini(p0$wealth), mean(p0$vision), mean(p0$metabolism))
show <- c(0, 10, 25, 50, 100, 200, 300, 400, 500)
print(data.frame(tick = show,
                 population = g$pop[match(show, g$tick)],
                 mean_wealth = round(g$wealth[match(show, g$tick)], 1),
                 gini = round(g$gini[match(show, g$tick)], 3),
                 mean_vision = round(g$vis[match(show, g$tick)], 2),
                 mean_metabolism = round(g$met[match(show, g$tick)], 2)),
      row.names = FALSE)

fin <- r[r$tick == 500 & r$.group == "people", ]
last <- g[g$tick > 400, ]
cat(sprintf("\n  carrying capacity: %d to %d agents over the last 100 ticks\n",
            min(last$pop), max(last$pop)))
cat(sprintf("  survivors: mean vision %.2f (started %.2f), mean metabolism %.2f (started %.2f)\n",
            g$vis[nrow(g)], g$vis[1], g$met[nrow(g)], g$met[1]))
cat(sprintf("  wealth: mean %.1f at the start, %.1f at the end; Gini %.3f -> peak %.3f at tick %d -> %.3f\n",
            g$wealth[1], g$wealth[nrow(g)], g$gini[1], max(g$gini),
            g$tick[which.max(g$gini)], g$gini[nrow(g)]))
cat(sprintf("  wealth at the end: median %.0f, top tenth above %.0f, richest %.0f\n",
            median(fin$wealth), quantile(fin$wealth, 0.9), max(fin$wealth)))
cat(sprintf("  one agent per cell at every recorded tick: %s\n",
            !any(tapply(r$.cell[r$.group == "people"], r$tick[r$.group == "people"],
                        anyDuplicated) > 0)))
cat("\n  The population falls to what the landscape can feed and stays there.\n")
cat("  Who survives is not random: the far-sighted and the frugal, which is\n")
cat("  Epstein & Axtell's selection result. Inequality spikes while the poor\n")
cat("  are dying off and then settles: once the survivors are the survivors,\n")
cat("  wealth grows at a rate set by each one's vision and metabolism, so the\n")
cat("  wealth Gini converges on the Gini of those income rates.\n")

# --- figures --------------------------------------------------------------
# Time series: population and wealth inequality. Then the landscape at tick
# 500 with the survivors on it: the sugar standing on each cell, which the
# agents graze down to zero, and the agents clustered on the mountains. Then
# a strip of the die-off, and the run as a viewer.
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

d <- rbind(data.frame(tick = g$tick, measure = "population", value = g$pop),
           data.frame(tick = g$tick, measure = "wealth Gini", value = g$gini))
p <- ggplot(d, aes(tick, value)) +
  geom_line(linewidth = 0.6, colour = pal[1]) +
  facet_wrap(~measure, ncol = 1, scales = "free_y") +
  fig_theme +
  theme(strip.text = element_text(face = "bold", hjust = 0)) +
  labs(title = "Sugarscape: a carrying capacity, and inequality under it",
       subtitle = "50x50 torus, 400 agents, vision 1-6, metabolism 1-4, constant growback",
       x = "tick", y = NULL)
print(p)
ggsave(fig_file("08-sugarscape.png"), p, width = 7, height = 4.5, dpi = 150,
       bg = "white")

sugar_scale <- scale_fill_gradient(low = "#FFFCF5", high = pal[2], name = "sugar",
                                   breaks = 0:4, limits = c(0, 4))
p2 <- abm_plot_lattice(r, fill = sugar, agents = TRUE, size = 1.4) +
  sugar_scale +
  labs(title = "The sugarscape at tick 500",
       subtitle = sprintf("%d survivors on two sugar mountains; a cell an agent stands on has just been eaten bare",
                          nrow(fin)))
print(p2)
ggsave(fig_file("08-sugarscape-lattice.png"), p2, width = 7, height = 6, dpi = 150,
       bg = "white")

p3 <- abm_plot_lattice(r, fill = sugar, tick = c(0, 5, 20, 100, 500), agents = TRUE,
                       size = 0.5) +
  sugar_scale +
  theme(legend.position = "bottom", legend.key.width = unit(24, "pt")) +
  labs(title = "The die-off: 400 agents scattered at random, the survivors on the mountains",
       subtitle = "the sugar on each cell, 0 to 4; the agents in blue")
print(p3)
ggsave(fig_file("08-sugarscape-strip.png"), p3, width = 11, height = 3.4, dpi = 150,
       bg = "white")

# --- viewer ---------------------------------------------------------------
abm_view_lattice(
  r, fill = sugar, agents = TRUE, hover = c("wealth", "vision", "metabolism"),
  file = fig_file("08-sugarscape.html", dir = "viewers"),
  title = "Sugarscape", subtitle = "50x50 torus, 400 agents, every fifth tick of 500",
  fill_colours = c("#FFFCF5", pal[2]))
