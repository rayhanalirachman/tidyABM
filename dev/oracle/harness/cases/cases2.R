ring_m <- function(n = 30, globals = list(k = 2)) abm_setup(
  agents = abm_agents(n = n, x = ~rnorm(n), y = ~runif(n)),
  network = abm_network("ring", degree = 2), globals = globals, seed = 3)
run <- function(m, ..., ticks = 4, seed = 1, record = "all", params = NULL, reps = 1,
                measures = NULL, checks = NULL, cores = 1)
  abm_run(m, abm_go(...), ticks = ticks, seed = seed, record = record, params = params,
          reps = reps, measures = measures, checks = checks, cores = cores)
lat <- function() abm_setup(agents = abm_grid(dims = c(5, 5), diagonals = FALSE, x = ~rnorm(n)), seed = 1)
no_plan <- function(steps) all(vapply(steps, function(s)
  is.null(s$plan) && (!inherits(s, "abm_repeat") || no_plan(s$steps)), NA))

cases <- list(
  nb_lattice = function() run(lat(), abm_neighbours(s ~ sum(x), a ~ mean(x)), ticks = 2),
  nb_where = function() run(lat(), abm_neighbours(s ~ sum(x), .where = "north"), ticks = 2),
  nb_matrix_global = function() run(ring_m(n = 6, globals = list(M = matrix(as.numeric(1:6), 2))),
                                    abm_neighbours(s ~ sum(M)), ticks = 1),
  nb_vec_times_col = function() run(ring_m(n = 6, globals = list(w = list(as.numeric(1:6)))),
                                    abm_neighbours(s ~ sum(x * w)), ticks = 1),
  nb_global_len1_list = function() run(ring_m(globals = list(k = list(3))), abm_neighbours(s ~ sum(x)), ticks = 1),
  nb_tell_neighbours = function() run(ring_m(), abm_tell(y ~ x, to = "neighbours"), abm_neighbours(s ~ mean(y))),
  global_tick_name = function() run(ring_m(globals = list(tick = 5)), abm_rules(x ~ x + 1), ticks = 2),
  col_tick_name = function() { m <- abm_setup(agents = abm_agents(n = 3, tick = 1)); run(m, abm_rules(tick ~ tick + 1), ticks = 1) },
  col_dot_names = function() { m <- abm_setup(agents = abm_agents(n = 3, z = 1, a = 2)); run(m, abm_rules(z ~ a), ticks = 1) },
  measure_bad = function() run(ring_m(), abm_rules(x ~ x), measures = list(m = ~x)),
  measure_reads_new_col = function() run(ring_m(), abm_rules(z ~ x * 2), measures = list(m = ~mean(z))),
  check_reads_global = function() run(ring_m(), abm_global(k ~ k - 1), checks = list(ok = ~k > 0)),
  params_factor = function() run(ring_m(), abm_rules(x ~ x), params = list(k = factor(c("a", "b")))),
  params_named = function() run(ring_m(), abm_rules(x ~ x), params = list(k = c(a = 1, b = 2))),
  params_dup_like = function() run(ring_m(), abm_rules(x ~ x), params = list(.run = c(1, 2))),
  params_df = function() run(ring_m(), abm_rules(x ~ x), params = list(k = list(data.frame(a = 1:2)))),
  params_globals_rec = function() run(ring_m(), abm_rules(x ~ x * k), params = list(k = c(0.5, 1, 2)), reps = 3,
                                      record = "globals", measures = list(m = ~mean(x))),
  no_globals = function() { m <- abm_setup(agents = abm_agents(n = 3, z = 1)); run(m, abm_rules(z ~ z + 1), ticks = 2) },
  route_group_empties = function() {
    m <- abm_setup(agents = list(a = abm_agents(n = 3, x = 0, q = 1), b = abm_agents(n = 2, x = 0)))
    run(m, abm_rules(x ~ x + 1), abm_death(when = q > 0), abm_rules(x ~ x + 2), ticks = 3)
  },
  route_group_refills = function() {
    m <- abm_setup(agents = list(a = abm_agents(n = 3, x = 0, q = 1), b = abm_agents(n = 2, x = 0, r = 1)), seed = 1)
    run(m, abm_death(when = q > 0 & x > 1), abm_rules(x ~ x + q), abm_rules(r ~ r + 1),
        abm_birth(n = 2), ticks = 5)
  },
  plain_role_no_match = function() run(ring_m(), abm_rules(y ~ if_else(is.na(.role), 1, 2))),
  plain_partner_no_match = function() run(ring_m(), abm_rules(y ~ partner_x)),
  plain_scope_pop = function() run(ring_m(), abm_match(pair = "random"), abm_rules(y ~ x + 1, .scope = "population")),
  plain_in_repeat_until = function() run(ring_m(), abm_repeat(abm_rules(x ~ x + 1), until = mean(x) > 2, max = 10)),
  nested_repeat = function() run(ring_m(), abm_repeat(abm_repeat(abm_neighbours(s ~ mean(x)), abm_rules(x ~ s), max = 2),
                                                      abm_global(k ~ k + 1), max = 2)),
  go_not_mutated = function() {
    go <- abm_go(abm_neighbours(s ~ sum(x)), abm_rules(x ~ x + s * 0.01),
                 abm_repeat(abm_rules(y ~ y + 1), max = 2), abm_death(when = y > 100), abm_global(k ~ mean(x)))
    nm0 <- lapply(go$steps, names); go0 <- go
    r1 <- abm_run(ring_m(), go, ticks = 3, seed = 1)
    m2 <- abm_setup(agents = list(a = abm_agents(n = 5, x = 1, y = 0), b = abm_agents(n = 4, x = 2, y = 1)),
                    network = abm_network("ring", degree = 2), globals = list(k = 0))
    r2 <- abm_run(m2, go, ticks = 3, seed = 1, params = list(k = c(1, 2)), reps = 2, cores = 2)
    r3 <- abm_run(ring_m(), go, ticks = 3, seed = 1)
    list(r1, r2, identical(r1, r3), identical(nm0, lapply(go$steps, names)), identical(go0, go), no_plan(go$steps))
  },
  odd = function() { go <- abm_go(abm_neighbours(s ~ sum(x)), abm_rules(x ~ x)); abm_run(ring_m(), go, ticks = 1); capture.output(abm_odd(ring_m(), go)) }
)
