# 38. Global cascades on random networks (Watts 2002, PNAS 99(9): 5766-5771)
#
# Every agent has a threshold phi. It switches on when at least phi of its
# neighbours are on. One agent is switched on at t = 0; the question is whether
# that single seed takes the whole network with it.
#
# The answer depends on mean degree z in a way that is not monotone. Watts's
# cascade condition, sum_k k(k-1) rho_k p_k = z with rho_k = 1[1/k >= phi],
# gives a *window*: too sparse and the seed is isolated, too dense and every
# agent is too well connected to be tipped by one neighbour. For phi = 0.18 the
# window is z in (1.021, 5.765), and that is what this reproduces.
#
# The degree distribution is the whole point, so this needs
# abm_network(type = "poisson") -- on the k-regular graph the package used to
# be limited to, no cascade ever starts once k > 1/phi.

library(tidyABM)

cascade_run <- function(z, phi = 0.18, n = 2000, seed = 1, ticks = 60) {
  m <- abm_setup(
    agents  = abm_agents(n = n, on = ~seq_len(n) == 1L, share = 0),
    network = abm_network(type = "poisson", degree = z),
    seed    = seed
  )
  go <- abm_go(
    abm_neighbours(share ~ mean(on)),
    abm_rules(on ~ on | coalesce(share, 0) >= phi),
    abm_global(active ~ mean(on))
  )
  g <- abm_globals(abm_run(m, go, ticks = ticks, seed = seed))
  g$active[nrow(g)]
}

# the analytic boundaries, for comparison
cascade_window <- function(phi) {
  f <- function(z) sum(vapply(1:floor(1/phi),
                              function(k) k * (k - 1) * stats::dpois(k, z),
                              numeric(1))) - z
  c(lower = stats::uniroot(f, c(1e-3, 1.5))$root,
    upper = stats::uniroot(f, c(2, 20))$root)
}

if (sys.nframe() == 0L) {
  phi <- 0.18
  win <- cascade_window(phi)
  cat(sprintf("analytic cascade window for phi = %.2f: z in (%.3f, %.3f)\n\n",
              phi, win[["lower"]], win[["upper"]]))

  zs <- c(0.5, 1.5, 3, 5, 6.5, 9)
  cat(sprintf("%5s  %9s  %s\n", "z", "P(cascade)", "in window?"))
  for (z in zs) {
    finals <- vapply(1:40, function(s) cascade_run(z, phi, seed = s), numeric(1))
    cat(sprintf("%5.1f  %9.2f  %s\n", z, mean(finals > 0.2),
                if (z > win[["lower"]] && z < win[["upper"]]) "yes" else "no"))
  }
}
