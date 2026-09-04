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

run_one <- function(temp, w = 50, ticks = 400, seed = 1) {
  m <- abm_setup(
    agents  = abm_agents(spin  = ~rep(1L, n),   # ordered start
                         black = ~(.x + .y) %% 2 == 0),
    network = abm_network(type = "grid", dims = c(w, w),
                          diagonals = FALSE, torus = TRUE),
    globals = list(temp = temp), seed = seed)
  g <- abm_globals(abm_run(m, go, ticks = ticks, seed = seed,
                           record = "globals", progress = FALSE))
  tail(g$mag, 100)   # the equilibrated tail
}

cat("50x50 torus, von Neumann, checkerboard Metropolis, 400 sweeps\n")
cat("T_c = 2 / log(1 + sqrt(2)) =", round(2 / log(1 + sqrt(2)), 4), "\n\n")
temps <- c(1.0, 1.5, 2.0, 2.27, 2.6, 3.5)
tails <- lapply(temps, run_one)          # one run per temperature
tab <- data.frame(
  temp = temps,
  magnetisation = round(vapply(tails, mean, numeric(1)), 4),
  susceptibility = round(vapply(tails, var, numeric(1)) * 2500, 3))
print(tab, row.names = FALSE)
cat("\n  Below T_c the mean spin settles on a non-zero magnetisation and\n")
cat("  above it the lattice is disordered; the susceptibility peaks at T_c.\n")
cat("  Started ordered and equilibrated, which is how spontaneous\n")
cat("  magnetisation is measured. Quenching from a random start instead\n")
cat("  traps the lattice in striped metastable states at low T -- a\n")
cat("  finite-size artifact of the model, not of the grammar.\n")

# --- figure ---------------------------------------------------------------
# Parameter sweep: magnetisation across the Onsager transition, with T_c
# drawn on.
library(ggplot2)

fig_file <- function(name) {
  a <- grep("^--file=", commandArgs(), value = TRUE)
  d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
  dir.create(file.path(d, "..", "figures"), showWarnings = FALSE)
  file.path(d, "..", "figures", name)
}

Tc <- 2 / log(1 + sqrt(2))

p <- ggplot(tab, aes(temp, magnetisation)) +
  geom_vline(xintercept = Tc, linetype = "dashed", colour = "grey50") +
  geom_line(linewidth = 0.7) + geom_point() +
  ylim(0, 1) +
  theme_minimal() +
  labs(title = "Ising: spontaneous magnetisation vanishes at T_c",
       subtitle = sprintf("50x50 torus, checkerboard Metropolis; dashed line is T_c = %.4f", Tc),
       x = "temperature", y = "|mean spin|")
print(p)
ggsave(fig_file("10-ising.png"), p, width = 6, height = 4, dpi = 120)
