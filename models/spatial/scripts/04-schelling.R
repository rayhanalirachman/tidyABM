# 4. Schelling segregation, with geography  (Segregation, Wilensky 1997)
#
# One agent per patch. Relocation is a mutual `opposite_group` match between
# unhappy cells and empty cells -- the same exclusion guarantee that stops two
# wolves eating one sheep stops two families taking one house. The lattice
# never moves; a resident relocating is a data write on two rows.
#
# Reported: mean same-type neighbour share against the tolerance.

library(tidyABM)

run_one <- function(tol, w = 51, vacancy = 0.10, minority = 0.30,
                    ticks = 100, seed = 6) {
  n_cells <- w * w; n_occ <- round(n_cells * (1 - vacancy))
  m <- abm_setup(
    agents = abm_agents(
      occ  = ~sample(c(rep(TRUE, n_occ), rep(FALSE, n - n_occ))),
      type = ~if_else(occ, sample(c("A", "B"), n, TRUE, c(1 - minority, minority)),
                      NA_character_)),
    network = abm_network(type = "grid", dims = c(w, w),
                          diagonals = TRUE, torus = TRUE),
    globals = list(tol = tol), seed = seed)

  go <- abm_go(
    abm_neighbours(same ~ mean(type == own_type, na.rm = TRUE)),
    abm_rules(unhappy ~ occ & !is.na(same) & same < tol),
    abm_match(pair = "opposite_group", by = occ, eligible = unhappy | !occ),
    abm_rules(
      type ~ if_else(!occ & !is.na(partner_type), partner_type,
                     if_else(unhappy, NA_character_, type)),
      occ  ~ case_when(!occ & !is.na(partner_type) ~ TRUE,
                       unhappy ~ FALSE, TRUE ~ occ)),
    abm_global(seg ~ mean(same, na.rm = TRUE),
               unhappy_n ~ sum(unhappy, na.rm = TRUE)))

  g <- abm_globals(abm_run(m, go, ticks = ticks, seed = seed,
                           record = "globals", progress = FALSE))
  c(start = g$seg[2], end = g$seg[nrow(g)], unhappy = g$unhappy_n[nrow(g)])
}

cat("51x51 torus, 10% vacant, 30% minority, 100 ticks\n\n")
tols <- c(0.20, 0.30, 0.40, 0.50, 0.60)
tab <- as.data.frame(t(vapply(tols, run_one, numeric(3))))
tab <- cbind(tolerance = tols, round(tab, 3))
print(tab, row.names = FALSE)
cat("\n  `start` is the same-type share after one tick, `end` after 100.\n")
cat("  A randomly mixed 70/30 population already sits at 0.7^2 + 0.3^2 = 0.58,\n")
cat("  which is what `start` measures; segregation is the rise above it.\n")
cat("  Asking for more similarity gets you more of it, well past what was\n")
cat("  asked for -- which is the whole Schelling point.\n")

# --- figure ---------------------------------------------------------------
# Parameter sweep: the same-type neighbour share before and after, against the
# tolerance asked for. The gap between them is the Schelling point.
library(ggplot2)

fig_file <- function(name) {
  a <- grep("^--file=", commandArgs(), value = TRUE)
  d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
  dir.create(file.path(d, "..", "figures"), showWarnings = FALSE)
  file.path(d, "..", "figures", name)
}

d <- rbind(
  data.frame(tolerance = tab$tolerance, when = "after 1 tick",
             share = tab$start),
  data.frame(tolerance = tab$tolerance, when = "after 100 ticks",
             share = tab$end))
d$when <- factor(d$when, levels = c("after 1 tick", "after 100 ticks"))

p <- ggplot(d, aes(tolerance, share, colour = when)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              colour = "grey50") +
  geom_line(linewidth = 0.7) + geom_point() +
  theme_minimal() +
  labs(title = "Schelling: mild preferences, severe segregation",
       subtitle = "51x51 torus, 10% vacant, 30% minority; dashed line is what was asked for",
       x = "tolerance (minimum same-type share wanted)",
       y = "mean same-type neighbour share", colour = NULL)
print(p)
ggsave(fig_file("04-schelling.png"), p, width = 6, height = 4, dpi = 120)
