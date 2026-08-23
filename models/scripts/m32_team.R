library(tidyABM)

# 32. Team Assembly (Guimera, Uzzi, Spiro & Amaral 2005; NetLogo Networks) ---
# A team of m people is assembled one member at a time. Each new member is an
# incumbent with probability p, and with probability q an incumbent is drawn
# from the past collaborators of someone already on the team. Everyone on the
# finished team is linked to everyone else.

team_assembly <- function(p, q, m = 5, ticks = 300, downtime = 30, seed = 1) {
  pop <- abm_setup(
    agents  = abm_agents(n = m, collabs = 0, idle = 0, on_team = FALSE),
    network = abm_network(type = "empty"),
    globals = list(p = p, q = q, u_type = 0, u_q = 0),
    seed    = seed
  )

  # one member joins the team
  recruit <- list(
    abm_global(u_type ~ runif(1), u_q ~ runif(1)),
    abm_neighbours(near_team ~ any(on_team)),
    abm_rules(score ~ (!on_team) * case_when(
      u_type <  p & u_q < q & collabs > 0 & coalesce(near_team, FALSE) ~ 3,
      u_type <  p & collabs > 0                                       ~ 2,
      u_type >= p & collabs == 0                                      ~ 2,
      collabs == 0                                                    ~ 1,
      TRUE                                                            ~ 0)),
    abm_rules(score   ~ score + runif(n())),
    abm_rules(on_team ~ on_team | (score > 1 & rank(-score, ties.method = "first") == 1))
  )

  go <- do.call(abm_go, c(
    list(
      abm_birth(n = m, inherit = list(collabs ~ 0, idle ~ 0, on_team ~ FALSE)),
      abm_rules(on_team ~ FALSE, idle ~ idle + 1)
    ),
    rep(recruit, m),
    list(
      abm_match(pair = "random", size = m, eligible = on_team),
      abm_link(),
      abm_rules(collabs ~ collabs + 1, idle ~ 0),
      abm_death(when = collabs == 0 & idle > downtime)
    )
  ))

  abm_run(pop, go, ticks = ticks, seed = seed)
}

giant_share <- function(res) {
  e <- abm_edges(res)
  ids <- unique(res$.id[res$tick == max(res$tick) & res$collabs > 0])
  e <- e[e$from %in% ids & e$to %in% ids, ]
  g <- igraph::graph_from_data_frame(e, directed = FALSE,
                                     vertices = data.frame(name = as.character(ids)))
  max(igraph::components(g)$csize) / length(ids)
}

for (par in list(c(.2, .5), c(.5, .5), c(.8, .5), c(.95, .5), c(.5, .95))) {
  r <- team_assembly(par[1], par[2])
  cat(sprintf("p = %.2f  q = %.2f   giant component = %.2f of the collaborators\n",
              par[1], par[2], giant_share(r)))
}
