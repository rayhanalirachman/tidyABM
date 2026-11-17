ring6 <- function(globals = list(w = c(10, 20, 30, 40, 50, 60), k = 2)) abm_setup(
  agents = abm_agents(n = 6, x = as.numeric(1:6), i = 1:6, b = c(TRUE, NA, FALSE, TRUE, TRUE, FALSE)),
  network = abm_network("ring", degree = 2), globals = globals, seed = 1)
run1 <- function(m, ..., ticks = 2) abm_run(m, abm_go(...), ticks = ticks, seed = 1)
cases <- list(
  d1_global_vec = function() run1(ring6(), abm_neighbours(s ~ sum(w))),
  d1_global_vec_mean = function() run1(ring6(), abm_neighbours(s ~ mean(x * w))),
  d1_env_vec = function() { ev <- c(1, 2, 3, 4, 5, 6); run1(ring6(), abm_neighbours(s ~ sum(x + ev))) },
  d1_matrix = function() { mat <- matrix(1:6, 6, 1); run1(ring6(), abm_neighbours(s ~ sum(x * mat))) },
  d1_global_int_vec = function() run1(ring6(list(w = 1:6)), abm_neighbours(s ~ sum(w), t ~ mean(i + w))),
  d1_shadow_abs = function() { abs <- function(z) rev(z); run1(ring6(), abm_neighbours(s ~ sum(abs(x)))) },
  d1_scalar_env = function() { one <- 3L; run1(ring6(), abm_neighbours(s ~ sum(i * one), t ~ mean(x * one))) },
  d1_missing_name = function() run1(ring6(), abm_neighbours(s ~ sum(nosuch))),
  d3_int = function() run1(ring6(), abm_neighbours(s ~ sum(i), m ~ mean(i), l ~ sum(b), q ~ mean(b))),
  d3_int_overflow = function() {
    m <- abm_setup(agents = abm_agents(n = 20, h = ~c(.Machine$integer.max, -.Machine$integer.max, rep(1e9L, n - 2))),
                   network = abm_network("ring", degree = 4), seed = 1)
    run1(m, abm_neighbours(s ~ sum(h), a ~ mean(h)))
  },
  d3_int_arith_overflow = function() {
    m <- abm_setup(agents = abm_agents(n = 10, h = 2e9L), network = abm_network("ring", degree = 2), seed = 1)
    run1(m, abm_neighbours(s ~ sum(h + h)))   # warns in the argument
  },
  d3_life = function() {
    m <- abm_setup(agents = abm_agents(alive = ~runif(n) < 0.3),
                   network = abm_network(type = "grid", dims = c(12, 9)), seed = 1)
    run1(m, abm_neighbours(live_n ~ sum(alive)),
         abm_rules(alive ~ live_n == 3 | (alive & live_n == 2)), ticks = 8)
  },
  d3_two_groups_births = function() {
    m <- abm_setup(agents = list(a = abm_agents(n = 10, b = ~runif(n) < 0.5),
                                 c = abm_agents(n = 5, b = TRUE)),
                   network = abm_network("ring", degree = 2), seed = 3)
    run1(m, abm_neighbours(s ~ sum(b), t ~ mean(as.integer(b) * 3L)),
         abm_birth(when = runif(n()) < 0.2, attach_via = abm_match(pair = "network", from = "parent")),
         abm_rules(b ~ runif(n()) < 0.5), ticks = 6)
  },
  d2_tick_global = function() { m <- abm_setup(agents = abm_agents(n = 3, x = 1), globals = list(tick = 5))
    abm_globals(run1(m, abm_rules(x ~ x + 1))) }
)
