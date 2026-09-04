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

run_one <- function(legitimacy, w = 22, ticks = 200, seed = 1) {
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

  g <- abm_globals(abm_run(rebel, go, ticks = ticks, seed = seed,
                           record = "globals", progress = FALSE))
  g[-1, ]
}

cat("22x22 torus, 339 people, 19 cops, 200 ticks\n")
cat("(kept small: the vision-radius `within` is an O(n^2) pair scan)\n\n")
traces <- list()
for (leg in c(0.88, 0.82, 0.70)) {
  g <- run_one(leg)
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

# --- figure ---------------------------------------------------------------
# Time series: rebels active per tick at three levels of legitimacy. The
# punctuated equilibrium is the point -- long quiet stretches, short bursts.
library(ggplot2)

fig_file <- function(name) {
  a <- grep("^--file=", commandArgs(), value = TRUE)
  d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
  dir.create(file.path(d, "..", "figures"), showWarnings = FALSE)
  file.path(d, "..", "figures", name)
}

d <- do.call(rbind, traces)

p <- ggplot(d, aes(tick, active)) +
  geom_line(linewidth = 0.4) +
  facet_wrap(~legitimacy, ncol = 1, scales = "free_y") +
  theme_minimal() +
  labs(title = "Rebellion: quiet, punctuated by bursts, until legitimacy falls",
       subtitle = "22x22 torus, 339 people, 19 cops, 200 ticks",
       x = "tick", y = "rebels active")
print(p)
ggsave(fig_file("13-rebellion.png"), p, width = 6, height = 4, dpi = 120)
