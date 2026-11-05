# 13. Rebellion  (Rebellion, NetLogo Models Library, Wilensky 2004, after
#     Epstein 2002)
#
# Two mobile groups on a grid of bare slots. A canonical social-science model
# landing in the same bucket as Wolf-Sheep is the useful signal: the L2 bundle
# is not biology-specific.
#
# The vision count is a `within =` on `.x` / `.y`, which is why turtles need
# coordinates and not just a `.cell` id -- the engine keeps `.x` / `.y` equal to
# the coordinates of `.cell` for any agent that has one. The arrest is
# `abm_match(pair = "one_of", among =)` plus `abm_tell(to = .partner)`, both of
# which already existed.
#
# Reported: punctuated equilibrium, and the legitimacy threshold.

library(tidyABM)

run_one <- function(legitimacy, w = 22, ticks = 200, seed = 1, record = "globals") {
  rebel <- abm_setup(
    agents = list(
      patches = abm_agents(z = 0),
      people  = abm_agents(n = round(w * w * 0.70), risk = ~runif(n),
                           hardship = ~runif(n), active = FALSE, jail = 0L),
      cops    = abm_agents(n = round(w * w * 0.04))),
    network = abm_network(type = "grid", dims = c(w, w), on = "patches",
                          torus = TRUE),
    globals = list(legitimacy = legitimacy, threshold = 0.1,
                   vision = 3, max_jail = 30),
    seed = seed)

  go <- abm_go(
    abm_move(along = "patches", to = "random_empty_neighbour",
             who = c("people", "cops")),
    abm_neighbours(
      C ~ sum(.group == "cops"),
      A ~ 1 + sum(.group == "people" & active),
      within = .group != "patches" & .id != own_.id &
               abs(.x - own_.x) <= vision & abs(.y - own_.y) <= vision),
    abm_rules(active ~ jail == 0L &
        (hardship * (1 - legitimacy) - risk * (1 - exp(-2.3 * C / A)) > threshold),
      .scope = "population"),
    abm_match(pair = "one_of", eligible = .group == "cops",
              among = .group == "people" & active),
    abm_tell(active ~ FALSE, jail ~ sample.int(max_jail, 1L), to = .partner,
             when = .group == "cops" & !is.na(.partner)),
    abm_rules(jail ~ pmax(0L, jail - 1L), .scope = "population"),
    abm_global(n_active ~ sum(active, na.rm = TRUE),
               n_jail   ~ sum(jail > 0, na.rm = TRUE)))

  abm_run(rebel, go, ticks = ticks, seed = seed, record = record, progress = FALSE)
}
series <- function(r) abm_globals(r)[-1, ]

cat("22x22 torus, 339 people, 19 cops, 200 ticks\n")
cat("(kept small: the vision-radius `within` is an O(n^2) pair scan)\n\n")
traces <- list()
for (leg in c(0.88, 0.82, 0.70)) {
  # the middle regime keeps every frame, for the lattice figure and the viewer
  r <- run_one(leg, record = if (leg == 0.82) "all" else "globals")
  if (leg == 0.82) r82 <- r
  g <- series(r)
  traces[[length(traces) + 1]] <- data.frame(
    legitimacy = sprintf("legitimacy %.2f", leg),
    tick = g$tick, active = g$n_active)
  quiet <- mean(g$n_active <= 2)
  cat(sprintf("legitimacy %.2f\n", leg))
  cat(sprintf("  active: median %3.0f  mean %5.1f  max %3d\n",
              median(g$n_active), mean(g$n_active), max(g$n_active)))
  cat(sprintf("  ticks with 2 or fewer rebels: %.0f%%\n", 100 * quiet))
  cat(sprintf("  trace: %s ...\n",
              paste(g$n_active[seq(1, 100, by = 7)], collapse = " ")))
  cat("\n")
}
cat("  High legitimacy gives long quiet stretches broken by short bursts;\n")
cat("  lowering it past a threshold flips the system to sustained rebellion.\n")

cat("\n  `abm_neighbours(within =)` builds every (focal, candidate) pair and\n")
cat("  then filters, so a vision-radius count is quadratic in the population.\n")
cat("  An equality against an own_ column (the co-location join) is recognised\n")
cat("  and hash-joined instead; a range condition like this one cannot be.\n")

# --- figures --------------------------------------------------------------
# Time series: rebels active per tick at three levels of legitimacy. The
# punctuated equilibrium is the point -- long quiet stretches, short bursts.
# Then one burst on the lattice: the tick before it, the peak, and the
# arrests that follow. And the whole middle run as a viewer.
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

d <- do.call(rbind, traces)
d$legitimacy <- factor(d$legitimacy, levels = unique(d$legitimacy))

p <- ggplot(d, aes(tick, active)) +
  geom_line(linewidth = 0.5, colour = pal[1]) +
  facet_wrap(~legitimacy, ncol = 1, scales = "free_y") +
  fig_theme +
  theme(strip.text = element_text(face = "bold", hjust = 0)) +
  labs(title = "Rebellion: quiet, punctuated by bursts, until legitimacy falls",
       subtitle = "22x22 torus, 339 people, 19 cops, 200 ticks",
       x = "tick", y = "rebels active")
print(p)
ggsave(fig_file("13-rebellion.png"), p, width = 7, height = 5, dpi = 150,
       bg = "white")

# Who is doing what, on the grid: a cop, a quiet citizen, one actively
# rebelling, or one in jail. The patches carry nothing, so they stay grey.
status_colours <- c(quiet = "#009E73", active = "#D55E00", jailed = "grey45",
                    cop = pal[1])
g82 <- series(r82)
settled <- g82[g82$tick > 10, ]                # past the opening flare
peak <- settled$tick[which.max(settled$n_active)]
burst_ticks <- c(peak - 1, peak, peak + 1, peak + 5, peak + 20)
p2 <- abm_plot_lattice(r82, tick = burst_ticks, agents = TRUE, size = 1.3,
                       colour = case_when(.group == "cops" ~ "cop",
                                          active ~ "active",
                                          jail > 0 ~ "jailed",
                                          TRUE ~ "quiet"),
                       colours = status_colours) +
  theme(legend.position = "bottom") +
  labs(title = sprintf("Legitimacy 0.82: a burst at tick %d, and the arrests after it", peak),
       subtitle = "22x22 torus; the lattice carries nothing, so the patches stay grey",
       colour = NULL)
print(p2)
ggsave(fig_file("13-rebellion-lattice.png"), p2, width = 11, height = 3.2, dpi = 150,
       bg = "white")

# --- viewer ---------------------------------------------------------------
# The first 150 ticks: a burst is a one-tick event, so every tick is kept and
# the run is shortened instead.
abm_view_lattice(
  r82, agents = TRUE, hover = "jail", tick = 0:150,
  colour = case_when(.group == "cops" ~ "cop", active ~ "active",
                     jail > 0 ~ "jailed", TRUE ~ "quiet"),
  file = fig_file("13-rebellion.html", dir = "viewers"),
  title = "Rebellion", subtitle = "22x22 torus, legitimacy 0.82, 339 people and 19 cops, the first 150 ticks",
  labels = c(colour = "status"), agent_colours = status_colours)
