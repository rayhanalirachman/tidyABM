library(tidyABM)

# 33. Language Change (Troutman & Wilensky 2007, NetLogo Social Science) -----
# Language users on a preferential-attachment network. Each tick everyone
# utters a form drawn from their own grammar weight and then updates on what
# they heard. Two of the paper's update algorithms are shown.

# the network comes from running the package's preferential-attachment model
pa_network <- function(n, seed = 1) {
  seed_net <- abm_setup(
    agents  = abm_agents(n = 2, dummy = 0),
    network = abm_network(type = "manual", edges = data.frame(from = 1, to = 2))
  )
  grow <- abm_go(
    abm_birth(n = 1,
              attach_via = abm_match(pair = "network", from = "random_edge")),
    abm_rules(dummy ~ dummy)
  )
  grown <- abm_run(seed_net, grow, ticks = n - 2, seed = seed)
  abm_edges(grown)
}

language <- function(algorithm = c("threshold", "reward"), n = 200,
                     alpha = 0.5, rate = 0.2, start = 0.3,
                     ticks = 100, seed = 1) {
  algorithm <- match.arg(algorithm)
  pop <- abm_setup(
    agents  = abm_agents(n = n, w = ~as.numeric(runif(n) < start)),
    network = abm_network(type = "manual", edges = pa_network(n, seed)),
    globals = list(alpha = alpha, rate = rate),
    seed    = seed
  )
  update <- if (algorithm == "threshold") {
    abm_rules(w ~ if_else(is.na(heard), w, as.numeric(heard >= alpha)))
  } else {
    abm_rules(w ~ if_else(is.na(heard), w, (1 - rate) * w + rate * heard))
  }
  go <- abm_go(
    abm_rules(utterance ~ as.numeric(runif(n()) < w)),   # speak
    abm_neighbours(heard ~ mean(utterance)),             # listen
    update
  )
  abm_run(pop, go, ticks = ticks, seed = seed)
}

degree_of <- function(res) {
  e <- abm_edges(res)
  table(c(e$from, e$to))
}

for (alg in c("threshold", "reward")) {
  for (s0 in c(0.2, 0.4, 0.5, 0.6, 0.8)) {
    r <- language(alg, start = s0)
    final <- r[r$tick == max(r$tick), ]
    cat(sprintf("%-9s start %.1f (actual %.2f) -> end %.2f\n",
                alg, s0, mean(r$w[r$tick == 0]), mean(final$w)))
  }
}
