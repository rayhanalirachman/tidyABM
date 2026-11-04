# 10. Ising model  (Ising, NetLogo Models Library, Wilensky 2003)
#
# On a bipartite lattice every site's neighbours lie on the *other* sublattice,
# so all black sites can be updated at once and then all white ones -- a
# standard, physically valid parallel scheme that needs only L0.
#
# The literal NetLogo dynamics (one site at a time, in random order, each
# reading neighbours that may already have flipped this tick) is NOT expressible:
# `abm_sequential()` processes agents one at a time but can read only its own
# row and the globals, never a neighbour. The checkerboard form is the
# supported one, and the short form -- one `abm_rules()` over the whole lattice,
# no sublattice split -- runs and is subtly wrong: simultaneous updates on a
# bipartite graph oscillate rather than equilibrate.
#
# Reported: the Onsager transition at T_c = 2 / ln(1 + sqrt(2)) = 2.2692.

library(tidyABM)

sweep <- function(side) list(
  abm_neighbours(nbr ~ sum(spin)),
  abm_rules(spin ~ {
    Ediff  <- 2 * spin * nbr
    accept <- (Ediff <= 0) | (runif(n()) < exp(-Ediff / temp))
    if_else(black == side & accept, -spin, spin)
  })
)
go <- do.call(abm_go, c(sweep(TRUE), sweep(FALSE),
                        list(abm_global(mag ~ abs(mean(spin))))))

# one run per temperature; the last frame is kept for the figure
run_one <- function(temp, w = 50, ticks = 400, seed = 1) {
  m <- abm_setup(
    agents  = abm_agents(spin  = ~rep(1L, n),   # ordered start
                         black = ~(.x + .y) %% 2 == 0),
    network = abm_network(type = "grid", dims = c(w, w),
                          diagonals = FALSE, torus = TRUE),
    globals = list(temp = temp), seed = seed)
  abm_run(m, go, ticks = ticks, seed = seed, record = "final", progress = FALSE)
}

cat("50x50 torus, von Neumann, checkerboard Metropolis, 400 sweeps\n")
cat("T_c = 2 / log(1 + sqrt(2)) =", round(2 / log(1 + sqrt(2)), 4), "\n\n")
temps <- c(1.0, 1.5, 2.0, 2.27, 2.6, 3.5)
runs <- lapply(temps, run_one)
tails <- lapply(runs, function(r) tail(abm_globals(r)$mag, 100))   # the equilibrated tail
tab <- data.frame(
  temp = temps,
  magnetisation = round(vapply(tails, mean, numeric(1)), 4),
  susceptibility = round(vapply(tails, var, numeric(1)) * 2500, 3))
print(tab, row.names = FALSE)
cat("\n  Below T_c the mean spin settles on a non-zero magnetisation and\n")
cat("  above it the lattice is disordered. The susceptibility (the variance\n")
cat("  of the magnetisation, scaled by N) is two orders of magnitude larger\n")
cat("  at 2.27 and 2.6 than at 2.0 or 3.5: on a 50x50 lattice with a\n")
cat("  six-point sweep the finite-size peak sits just above T_c.\n")
cat("  Started ordered and equilibrated, which is how spontaneous\n")
cat("  magnetisation is measured. Quenching from a random start instead\n")
cat("  traps the lattice in striped metastable states at low T -- a\n")
cat("  finite-size artifact of the model, not of the grammar.\n")

# --- figures --------------------------------------------------------------
# Parameter sweep: magnetisation across the Onsager transition, with T_c
# drawn on. Then the lattice itself at three temperatures: ordered, critical
# (domains at every scale), disordered.
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

Tc <- 2 / log(1 + sqrt(2))

p <- ggplot(tab, aes(temp, magnetisation)) +
  geom_vline(xintercept = Tc, linetype = "dashed", colour = "grey50") +
  geom_line(linewidth = 0.7, colour = pal[1]) +
  geom_point(size = 2, colour = pal[1]) +
  scale_y_continuous(limits = c(0, 1)) +
  fig_theme +
  labs(title = "Ising: spontaneous magnetisation vanishes at T_c",
       subtitle = sprintf("50x50 torus, checkerboard Metropolis, 400 sweeps; dashed line is T_c = %.4f", Tc),
       x = "temperature", y = "|mean spin|")
print(p)
ggsave(fig_file("10-ising.png"), p, width = 7, height = 4.5, dpi = 150, bg = "white")

show <- c(1.5, 2.27, 3.5)
frames <- do.call(rbind, lapply(show, function(t) {
  r <- runs[[match(t, temps)]]
  cbind(r[r$tick == 400, ],
        temperature = sprintf("T = %.2f  (|m| = %.2f)", t,
                              tab$magnetisation[tab$temp == t]))
}))
frames$temperature <- factor(frames$temperature, levels = unique(frames$temperature))
p2 <- abm_plot_lattice(frames, fill = factor(spin)) +
  facet_wrap(~temperature) +
  scale_fill_manual(values = c(`-1` = "grey15", `1` = "white"), name = "spin") +
  theme(panel.border = element_rect(fill = NA, colour = "grey70")) +
  labs(title = "The lattice after 400 sweeps: ordered, critical, disordered",
       subtitle = "at T_c the domains come at every scale, which is what a critical point looks like")
print(p2)
ggsave(fig_file("10-ising-lattice.png"), p2, width = 9, height = 3.9, dpi = 150,
       bg = "white")
