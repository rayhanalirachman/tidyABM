## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", eval = FALSE)


## -----------------------------------------------------------------------------
world  <- abm_setup(agents = abm_agents(n = 100, money = 100),   # 1. who
                    network = abm_network(type = "random", degree = 4),
                    globals = list(price = 1), seed = 1)
go     <- abm_go(abm_match(...), abm_rules(...), ...)            # 2. what happens each tick
result <- abm_run(world, go, ticks = 100, seed = 1)              # 3. run it


## -----------------------------------------------------------------------------
result                      # one row per agent per tick; tick 0 is the start
abm_globals(result)         # one row per tick
abm_measures(result)        # the `measures =` summaries, one row per tick
abm_edges(result)           # the network at the end
abm_relations(result)       # the relations at the end
abm_odd(world, go, ticks)   # an ODD protocol skeleton read off the model

