# 6. Ants  (Ants, NetLogo Models Library, Wilensky 1997)
#
# Everything in Wolf-Sheep, plus a diffusing/evaporating scalar field on the
# patches and gradient-following movement. The diffusion is a documented
# two-line idiom over existing steps, not a new one; `abm_move(to = uphill())`
# is the argmax flavour of the same step Wolf-Sheep needs.
#
# Two things worth reading closely:
#
#  * Mobile agents read their current patch directly via `cell_<col>` and write
#    additive deposits directly via `abm_tell(..., to = .cell, .resolve = "add")`
#    (`+=`), with no per-tick mailbox columns needed.
#  * A pure lattice argmax makes brittle trails, because an ant climbs to a
#    local maximum and stops exploring. NetLogo's ants `wiggle`; the lattice
#    analogue is a small random term inside `uphill()`, which is just an
#    expression like any other.

library(tidyABM)

world <- abm_setup(
  agents = list(
    patches = abm_agents(
      nest       = ~sqrt((.x - cx)^2 + (.y - cy)^2) < 3,
      nest_scent = ~200 - sqrt((.x - cx)^2 + (.y - cy)^2),
      food = ~as.integer(((.x - cx - 9)^2 + (.y - cy)^2 < 7) |
                         ((.x - cx)^2 + (.y - cy + 11)^2 < 7)),
      chemical = 0),
    ants = abm_agents(n = 80, carrying = FALSE, at = ~which(nest)[1])),
  space = abm_space(type = "grid", dims = c(35, 35), on = "patches",
                    diagonals = TRUE, torus = FALSE),
  globals = list(cx = 18, cy = 18, evap = 0.10, diff = 0.30, wiggle = 4))

go <- abm_go(
  # pick up food or drop it at the nest, then decrement food on picked-up cells
  abm_rules(picked   ~ !carrying & cell_food > 0,
            carrying ~ case_when(!carrying & cell_food > 0 ~ TRUE,
                                 carrying & cell_nest      ~ FALSE,
                                 TRUE ~ carrying)),
  abm_tell(food ~ -1L, to = .cell, .resolve = "add",
           when = .group == "ants" & picked),

  # NetLogo `diffuse` + evaporation (using exact per-cell degree `deg` on bounded grid),
  # then this tick's additive pheromone deposits
  abm_neighbours(inflow ~ sum(chemical), deg ~ n()),
  abm_rules(food     ~ pmax(0L, food),
            chemical ~ (1 - evap) * ((1 - diff) * chemical + diff * inflow / deg)),
  abm_tell(chemical ~ 60, to = .cell, .resolve = "add",
           when = .group == "ants" & carrying),

  # head home if carrying, up the trail if not
  abm_move(who = "ants",
           to = uphill(if_else(carrying, nest_scent,
                               chemical + runif(n()) * wiggle))),

  abm_global(food_left  ~ sum(food, na.rm = TRUE),
             carrying_n ~ sum(carrying, na.rm = TRUE),
             chem_total ~ round(sum(chemical, na.rm = TRUE)))
)

cat("35x35 bounded grid, 80 ants, nest at the centre, two food piles\n\n")
# every other frame is kept for the figures and the viewer; the globals are
# recorded every tick
r <- abm_run(world, go, ticks = 400, seed = 1, record = 2, progress = FALSE)
g <- abm_globals(r)
g <- g[-1, ]
print(as.data.frame(g[seq(1, nrow(g), by = 50),
                      c("tick", "food_left", "carrying_n", "chem_total")]),
      row.names = FALSE)
cat(sprintf("\n  food consumed: %d of %d\n", g$food_left[1] - g$food_left[nrow(g)],
            g$food_left[1]))
cat(sprintf("  peak trail strength: %.0f, at tick %d\n",
            max(g$chem_total), g$tick[which.max(g$chem_total)]))
cat(sprintf("  trail strength at the end: %.0f  (spent piles evaporate)\n",
            g$chem_total[nrow(g)]))

# --- figures --------------------------------------------------------------
# Time series: food remaining and trail strength. The trail builds while there
# is food to fetch and evaporates once the piles are spent. Then the field at
# tick 150, near the trail's peak: the pheromone, the two piles, and where the
# ants are, coloured by whether they are carrying food home. Then a strip of
# the field through the run, and the whole run as an interactive viewer.
library(ggplot2)

.file <- grep("^--file=", commandArgs(), value = TRUE)
source(file.path(dirname(sub("^--file=", "", c(.file, "x")[1])), "../../fig_file.R"))
# Okabe-Ito, colour-blind safe; the same series colours in every script
pal <- c("#0072B2", "#E69F00", "#009E73")
fig_theme <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom",
        plot.title.position = "plot")

d <- rbind(
  data.frame(tick = g$tick, measure = "food remaining", value = g$food_left),
  data.frame(tick = g$tick, measure = "trail strength (total chemical)",
             value = g$chem_total))

p <- ggplot(d, aes(tick, value)) +
  geom_line(linewidth = 0.6, colour = pal[1]) +
  facet_wrap(~measure, ncol = 1, scales = "free_y") +
  fig_theme +
  theme(strip.text = element_text(face = "bold", hjust = 0)) +
  labs(title = "Ants: a pheromone trail builds, then evaporates",
       subtitle = "35x35 bounded grid, 80 ants, two food piles, 400 ticks",
       x = "tick", y = NULL)
print(p)
ggsave(fig_file("06-ants.png"), p, width = 7, height = 4.5, dpi = 150, bg = "white")

# The pheromone is the fill; the nest and the food piles are tiles drawn over
# it in fixed colours, which is the ggplot idiom for a second, categorical
# layer on the same cells.
patches <- r[r$.group == "patches", ]
food_cells <- patches[patches$food > 0, ]
nest_cells <- patches[patches$nest, ]
ant_colours <- c(`carrying food` = "#D55E00", foraging = "grey10")
trail_scale <- scale_fill_gradient(low = "#F4F7FA", high = pal[1], name = "pheromone",
                                   trans = "sqrt")

at150 <- r[r$tick == 150, ]
p2 <- abm_plot_lattice(at150, fill = chemical, agents = TRUE,
                       colour = if_else(carrying, "carrying food", "foraging"),
                       colours = ant_colours, size = 1.6) +
  geom_tile(data = food_cells[food_cells$tick == 150, ], fill = pal[2]) +
  geom_tile(data = nest_cells[nest_cells$tick == 150, ], fill = pal[3]) +
  trail_scale +
  labs(title = "The trail at tick 150",
       subtitle = sprintf("%d ants; the nest (green) at the centre, %d units of food (orange) left in two piles",
                          sum(at150$.group == "ants"),
                          sum(food_cells$food[food_cells$tick == 150])),
       colour = NULL)
print(p2)
ggsave(fig_file("06-ants-lattice.png"), p2, width = 7, height = 6, dpi = 150,
       bg = "white")

strip_ticks <- c(20, 60, 100, 150, 250, 400)
p3 <- abm_plot_lattice(r, fill = chemical, tick = strip_ticks, agents = TRUE,
                       colour = if_else(carrying, "carrying food", "foraging"),
                       colours = ant_colours, size = 0.5) +
  geom_tile(data = food_cells[food_cells$tick %in% strip_ticks, ], fill = pal[2]) +
  geom_tile(data = nest_cells[nest_cells$tick %in% strip_ticks, ], fill = pal[3]) +
  trail_scale +
  theme(legend.position = "bottom", legend.key.width = unit(24, "pt")) +
  labs(title = "The trail forms, forks to the second pile, and evaporates once the food is gone",
       subtitle = "the nest in green, food in orange; ants carrying food in red, foraging in black",
       colour = NULL)
print(p3)
ggsave(fig_file("06-ants-strip.png"), p3, width = 11, height = 3.2, dpi = 150,
       bg = "white")

# --- viewer ---------------------------------------------------------------
# The pheromone on a square-root scale so the faint edges of the trail show;
# the exact value is in the readout.
abm_view_lattice(
  r, fill = sqrt(chemical), agents = TRUE,
  mark = case_when(nest ~ "nest", food > 0 ~ "food"),
  colour = if_else(carrying, "carrying food", "foraging"),
  hover = "chemical", file = fig_file("06-ants.html", dir = "../../pkgdown/assets/viewers"),
  title = "Ants", subtitle = "35x35 bounded grid, 80 ants, every other tick of 400",
  labels = c(fill = "sqrt(pheromone)"),
  fill_colours = c("#F4F7FA", pal[1]),
  mark_colours = c(nest = pal[3], food = pal[2]), agent_colours = ant_colours)
