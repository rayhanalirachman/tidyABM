suppressMessages(pkgload::load_all("tidyABM", quiet = TRUE))
run_one <- function(wolf_gain, w = 40, ticks = 400, seed = 1, record = "globals") {
  world <- abm_setup(
    agents = list(
      patches = abm_agents(grass     = ~sample(c(TRUE, FALSE), n, TRUE),
                           countdown = ~sample.int(30, n, TRUE)),
      sheep   = abm_agents(n = 120, energy = ~runif(n, 4, 8)),
      wolves  = abm_agents(n = 40,  energy = ~runif(n, 4, 8))),
    network = abm_network(type = "grid", dims = c(w, w), on = "patches",
                          diagonals = TRUE, torus = TRUE),
    globals = list(regrow = 30, sheep_gain = 4, wolf_gain = wolf_gain,
                   sheep_repro = 0.04, wolf_repro = 0.05),
    seed = seed)

  go <- abm_go(
    abm_move(along = "patches", to = "random_neighbour", who = c("sheep", "wolves")),
    abm_rules(energy ~ energy - 1),

    # sheep eat the grass on their cell
    abm_neighbours(grass_here ~ any(grass),
                   within = .group == "patches" & .id == own_.cell),
    abm_rules(energy ~ if_else(.group == "sheep" & grass_here,
                               energy + sheep_gain, energy)),
    abm_tell(grass ~ FALSE, countdown ~ regrow, to = .cell,
             when = .group == "sheep" & grass_here),

    # wolves eat a sheep sharing their cell: one prey per predator, per cell
    abm_match(pair = "opposite_group", by = .group, .by = .cell,
              eligible = .group %in% c("wolves", "sheep")),
    abm_rules(energy ~ if_else(.group == "wolves" & !is.na(.partner),
                               energy + wolf_gain, energy)),
    abm_death(when = .group == "sheep" & !is.na(.partner)),
    abm_death(when = .group %in% c("sheep", "wolves") & energy < 0),

    abm_birth(when = .group == "sheep"  & runif(n()) < sheep_repro,
              cost = energy ~ energy / 2),
    abm_birth(when = .group == "wolves" & runif(n()) < wolf_repro,
              cost = energy ~ energy / 2),

    abm_rules(grass     ~ grass | countdown == 0,
              countdown ~ case_when(grass          ~ countdown,
                                    countdown <= 0 ~ regrow,
                                    TRUE           ~ countdown - 1),
              .scope = "population"),
    abm_global(n_sheep  ~ sum(.group == "sheep"),
               n_wolves ~ sum(.group == "wolves")))

  abm_run(world, go, ticks = ticks, seed = seed, record = record,
          progress = FALSE)
}
# benchmark: standard regime (wolf_gain = 20), 40x40 torus, 400 ticks, seeds 1..5; arg 1 = output rds
Rprof("../rustq/ws.prof", interval = 0.005, gc.profiling = TRUE); p <- proc.time(); out <- list()
for (s in 1:5) { r <- run_one(20, ticks = 400, seed = s); out[[s]] <- cbind(seed = s, as.data.frame(abm_globals(r))) }
d <- proc.time() - p; Rprof(NULL)
cat(sprintf("wolf-sheep 5 seeds: real %.1f s, cpu %.1f s\n", d[["elapsed"]], d[["user.self"]] + d[["sys.self"]]))
