ring <- function(...) abm_setup(agents = abm_agents(n = 6, ...), network = abm_network("ring", degree = 2),
                                globals = list(k = 2))
run <- function(m, ..., ticks = 2) abm_run(m, abm_go(...), ticks = ticks, seed = 1)
cases <- list(
  named_col_setup = function() run(ring(x = setNames(as.numeric(1:6), letters[1:6])), abm_neighbours(s ~ sum(x), a ~ mean(x))),
  named_col_rule = function() run(ring(x = as.numeric(1:6)), abm_rules(x ~ setNames(x, letters[.id])),
                                  abm_neighbours(s ~ sum(x), a ~ mean(x))),
  named_global_k = function() run(ring(x = as.numeric(1:6)), abm_neighbours(s ~ sum(k * x))),
  matrix_col = function() run(ring(x = as.numeric(1:6)), abm_rules(x ~ matrix(x, ncol = 1)),
                              abm_neighbours(s ~ sum(x))),
  plain_named = function() run(ring(x = as.numeric(1:6)), abm_rules(x ~ setNames(x, letters[.id]), y ~ x + 1))
)
