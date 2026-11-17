grid_m <- function(n1 = 6, n2 = 7, ...) abm_setup(
  agents = abm_agents(n = n1 * n2, x = ~rnorm(n), y = ~runif(n), i = ~sample.int(5L, n, TRUE),
                      b = ~runif(n) < 0.5),
  network = abm_network("grid", dims = c(n1, n2), ...), seed = 7)
ring_m <- function(n = 30, globals = list(k = 2)) abm_setup(
  agents = abm_agents(n = n, x = ~rnorm(n), y = ~runif(n)),
  network = abm_network("ring", degree = 2), globals = globals, seed = 3)
run <- function(m, ..., ticks = 4, seed = 1, record = "all", params = NULL, reps = 1,
                measures = NULL, checks = NULL, cores = 1)
  abm_run(m, abm_go(...), ticks = ticks, seed = seed, record = record, params = params,
          reps = reps, measures = measures, checks = checks, cores = cores)

cases <- list(
  nb_basic = function() run(grid_m(), abm_neighbours(a ~ mean(x), s ~ sum(y * 2 - x)),
                            abm_rules(x ~ x + rnorm(n()) * 0.1)),
  nb_na_nan_inf = function() {
    m <- abm_setup(agents = abm_agents(n = 25, x = ~c(NA, NaN, Inf, -Inf, -0, NA, NaN, rnorm(18))),
                   network = abm_network("grid", dims = c(5, 5)), seed = 1)
    run(m, abm_neighbours(a ~ mean(x), s ~ sum(x), c ~ mean(x * 0), d ~ sum(-x)),
        abm_rules(x ~ if_else(is.na(a), x, a)))
  },
  nb_integer_col = function() run(grid_m(), abm_neighbours(s ~ sum(i), m ~ mean(i))),
  nb_logical_col = function() run(grid_m(), abm_neighbours(s ~ sum(b), m ~ mean(b))),
  nb_asnum = function() run(grid_m(), abm_neighbours(s ~ sum(as.numeric(i)), m ~ mean(as.double(b)))),
  nb_const = function() run(grid_m(), abm_neighbours(s ~ sum(1), m ~ mean(2), z ~ sum(0))),
  nb_global_scalar = function() run(ring_m(), abm_neighbours(s ~ sum(x * k)), abm_global(k ~ k + 1)),
  # a global vector as long as the population: general path evaluates it over the
  # view (length 2E) and falls to summarise()
  nb_global_vec_n = function() run(ring_m(globals = list(w = seq(0.5, 15, by = 0.5))),
                                   abm_neighbours(s ~ sum(w), m ~ mean(w * x)), ticks = 1),
  nb_global_vec_2 = function() run(ring_m(globals = list(w = list(c(1, 0)))),
                                   abm_neighbours(s ~ sum(x * w)), ticks = 1),
  nb_env_vec_n = function() { wv <- as.numeric(1:30)
    run(ring_m(), abm_neighbours(s ~ sum(wv)), ticks = 1) },
  nb_zero_degree = function() {
    m <- abm_setup(agents = abm_agents(n = 6, x = as.numeric(1:6)),
                   network = abm_network("manual", edges = data.frame(from = c(1, 2), to = c(2, 3))))
    run(m, abm_neighbours(s ~ sum(x), a ~ mean(x), k ~ n()), ticks = 2)
  },
  nb_empty_network = function() {
    m <- abm_setup(agents = abm_agents(n = 6, x = as.numeric(1:6)), network = abm_network("empty"))
    run(m, abm_neighbours(s ~ sum(x), a ~ mean(x)), ticks = 2)
  },
  nb_multi_edges_selfloop = function() {
    m <- abm_setup(agents = abm_agents(n = 5, x = c(1.1, 2.2, 3.3, 4.4, 5.5)),
                   network = abm_network("manual", edges = data.frame(from = c(1, 1, 2, 3, 3), to = c(2, 2, 2, 4, 5))))
    run(m, abm_neighbours(s ~ sum(x), a ~ mean(x)), ticks = 2)
  },
  nb_drawn = function() run(ring_m(), abm_draw(w ~ runif(n())),
                            abm_neighbours(s ~ sum(w * x), a ~ mean(x)), ticks = 3),
  nb_drawn_endpoint = function() run(ring_m(), abm_draw(w ~ runif(n()), .each = "endpoint"),
                                     abm_neighbours(s ~ sum(w_back * x), a ~ mean(x)), ticks = 3),
  nb_col_named_like_draw = function() {
    m <- abm_setup(agents = abm_agents(n = 12, x = ~rnorm(n), w = 5),
                   network = abm_network("ring", degree = 2), seed = 2)
    run(m, abm_draw(w ~ runif(n())), abm_neighbours(s ~ sum(w)), ticks = 2)
  },
  nb_births_deaths = function() run(ring_m(),
      abm_neighbours(s ~ mean(x)),
      abm_rules(x ~ if_else(is.na(s), x, (x + s) / 2)),
      abm_birth(when = runif(n()) < 0.1, attach_via = abm_match(pair = "network", from = "parent")),
      abm_death(when = runif(n()) < 0.05), ticks = 10),
  nb_death_noprune_last = function() {
    m <- abm_setup(agents = abm_agents(n = 8, x = as.numeric(1:8)),
                   network = abm_network("ring", degree = 2))
    run(m, abm_neighbours(s ~ sum(x)), abm_death(when = .id == 8L, prune_edges = FALSE),
        abm_neighbours(t ~ sum(x)), ticks = 2)
  },
  nb_death_prune_last = function() {
    m <- abm_setup(agents = abm_agents(n = 8, x = as.numeric(1:8)),
                   network = abm_network("ring", degree = 2))
    run(m, abm_neighbours(s ~ sum(x)), abm_death(when = .id == 8L),
        abm_neighbours(t ~ mean(x)), ticks = 2)
  },
  nb_link = function() {
    m <- abm_setup(agents = abm_agents(n = 20, x = ~rnorm(n)), network = abm_network("empty"), seed = 4)
    run(m, abm_neighbours(s ~ sum(x), a ~ mean(x)), abm_match(pair = "random"), abm_link(), ticks = 5)
  },
  nb_unlink = function() run(ring_m(), abm_neighbours(s ~ sum(x)), abm_match(pair = "network"),
                             abm_unlink(when = runif(n()) < 0.3), ticks = 5),
  nb_two_groups = function() {
    m <- abm_setup(agents = list(a = abm_agents(n = 10, x = ~runif(n), q = 1),
                                 b = abm_agents(n = 6, x = ~rnorm(n))),
                   network = abm_network("ring", degree = 2), seed = 6)
    run(m, abm_neighbours(s ~ sum(x), m ~ mean(q)), abm_rules(x ~ x + 1))
  },
  nb_target_only_in_one_group = function() {
    m <- abm_setup(agents = list(a = abm_agents(n = 10, x = ~runif(n), q = 1),
                                 b = abm_agents(n = 6, x = ~rnorm(n))),
                   network = abm_network("ring", degree = 2), seed = 6)
    run(m, abm_neighbours(q ~ sum(x)))
  },
  nb_relation_present = function() {
    m <- abm_setup(agents = abm_agents(n = 10, x = ~runif(n)), network = abm_network("ring", degree = 2),
                   relations = list(r = abm_relation(edges = data.frame(from = 1:3, to = 4:6), v = 1)), seed = 1)
    run(m, abm_neighbours(s ~ sum(x)))
  },
  nb_within = function() run(grid_m(), abm_neighbours(s ~ sum(x), within = abs(y - own_y) < 0.2), ticks = 2),
  nb_where = function() {
    m <- abm_setup(agents = abm_agents(n = 25, x = ~rnorm(n)), network = abm_grid(c(5, 5)), seed = 1)
    run(m, abm_neighbours(s ~ sum(x), .where = "north"), ticks = 2)
  },
  nb_lattice = function() {
    m <- abm_setup(agents = abm_agents(n = 25, x = ~rnorm(n)), network = abm_grid(c(5, 5)), seed = 1)
    run(m, abm_neighbours(s ~ sum(x), a ~ mean(x)), ticks = 2)
  },
  nb_warns = function() run(grid_m(), abm_neighbours(s ~ sum(log(x)))),
  nb_errors = function() run(grid_m(), abm_neighbours(s ~ sum(x + "a"))),
  nb_errors_second_rule = function() run(grid_m(), abm_neighbours(s ~ sum(x), t ~ sum(nope))),
  nb_mixed_rules = function() run(grid_m(), abm_neighbours(s ~ sum(x), k ~ n(), z ~ max(x))),
  nb_reads_own = function() run(grid_m(), abm_neighbours(s ~ sum(x - own_x))),
  nb_dot_id = function() run(grid_m(), abm_neighbours(s ~ sum(as.double(.id)))),
  nb_overflow = function() {
    m <- abm_setup(agents = abm_agents(n = 9, x = c(1e308, 1e308, -1e308, 1, 2, 3, 4, 5, 6)),
                   network = abm_network("grid", dims = c(3, 3)))
    run(m, abm_neighbours(s ~ sum(x), a ~ mean(x)), ticks = 1)
  },
  nb_in_repeat = function() run(ring_m(), abm_repeat(abm_neighbours(s ~ mean(x)),
                                                     abm_rules(x ~ if_else(is.na(s), x, s)), max = 3)),
  nb_case_when = function() run(grid_m(), abm_neighbours(s ~ sum(case_when(x > 0 ~ x, TRUE ~ -x)))),
  nb_if_else_scalar_cond = function() run(ring_m(), abm_neighbours(s ~ sum(if_else(k > 1, x, 0)))),
  nb_coalesce = function() run(ring_m(), abm_neighbours(s ~ sum(coalesce(x, 0)))),
  nb_namespace = function() run(ring_m(), abm_neighbours(s ~ sum(base::abs(x)))),
  nb_na_rm = function() run(ring_m(), abm_neighbours(s ~ sum(x, na.rm = TRUE))),
  # plain rules
  plain_rng = function() run(ring_m(), abm_rules(x ~ x + runif(n()), y ~ sample(x))),
  plain_simultaneous = function() run(ring_m(), abm_rules(x ~ y, y ~ x), abm_rules(x ~ x + y)),
  plain_warn = function() run(ring_m(), abm_rules(y ~ sqrt(x), x ~ log(x))),
  plain_error = function() run(ring_m(), abm_rules(y ~ x + "a")),
  plain_error_2nd = function() run(ring_m(), abm_rules(y ~ x + 1, z ~ nope * 2)),
  plain_wrong_len = function() run(ring_m(), abm_rules(y ~ c(1, 2, 3))),
  plain_globals = function() run(ring_m(), abm_rules(x ~ x * k), abm_global(k ~ k + mean(x)),
                                 abm_rules(y ~ k)),
  plain_new_col = function() run(ring_m(), abm_rules(z ~ x + 1), abm_rules(w ~ z * 2)),
  plain_type_change = function() run(ring_m(), abm_rules(x ~ as.integer(round(x * 10))),
                                     abm_rules(x ~ x / 3)),
  plain_empty = function() {
    m <- abm_setup(agents = abm_agents(n = 3, x = 1))
    run(m, abm_death(when = TRUE), abm_rules(x ~ x + 1), ticks = 2)
  },
  plain_unknown_col = function() run(ring_m(), abm_rules(z ~ nope + 1)),
  plain_n = function() run(ring_m(), abm_rules(x ~ x + n(), y ~ mean(x))),
  plain_by = function() run(ring_m(), abm_rules(g ~ as.integer(x > 0)), abm_rules(y ~ mean(x), .by = g)),
  plain_seq = function() run(ring_m(), abm_sequential(x ~ x + sum(y), y ~ y + 1)),
  # groups / routing
  route_groups = function() {
    m <- abm_setup(agents = list(a = abm_agents(n = 3, x = 0, q = 1), b = abm_agents(n = 2, x = 0, k = 1L)))
    run(m, abm_rules(x ~ x + q), abm_rules(q ~ 5, .by = k), ticks = 3)
  },
  route_global_mid = function() {
    m <- abm_setup(agents = list(a = abm_agents(n = 2, x = 0, w = 1), b = abm_agents(n = 2, x = 0)))
    run(m, abm_rules(x ~ x + w), abm_global(w ~ 10), ticks = 3)
  },
  route_group_empties = function() {
    m <- abm_setup(agents = list(a = abm_agents(n = 3, x = 0, q = 1), b = abm_agents(n = 2, x = 0)))
    run(m, abm_rules(x ~ x + 1), abm_death(when = q > 0), abm_rules(x ~ x + q), ticks = 3)
  },
  route_unapplied = function() {
    m <- abm_setup(agents = list(a = abm_agents(n = 3, x = 0), b = abm_agents(n = 2, x = 0)))
    run(m, abm_rules(x ~ x + zz), ticks = 1)
  },
  death_routes = function() {
    m <- abm_setup(agents = list(a = abm_agents(n = 20, e = ~runif(n)), b = abm_agents(n = 5, v = 1)), seed = 1)
    run(m, abm_rules(e ~ e - 0.1), abm_death(when = e < 0.2), abm_birth(when = e > 0.9, cost = list(e ~ e / 2)),
        ticks = 6)
  },
  birth_new_col_routes = function() {
    m <- abm_setup(agents = list(a = abm_agents(n = 6, e = ~runif(n)), b = abm_agents(n = 4, v = ~runif(n))), seed = 1)
    run(m, abm_birth(when = e > 0.5, cost = list(v ~ 1)), abm_death(when = v > 0.9), ticks = 3)
  },
  global_change_name = function() run(ring_m(), abm_global(newg ~ sum(x)), abm_rules(y ~ newg), ticks = 3),
  global_type_change = function() run(ring_m(globals = list(k = 1L)), abm_global(k ~ k + 0.5), ticks = 3),
  global_vector = function() run(ring_m(), abm_global(v ~ range(x)), ticks = 3),
  global_appears_late = function() run(ring_m(), abm_rules(x ~ x + 1),
                                       abm_global(late ~ if (mean(x) > 1) 1 else NULL), ticks = 4),
  global_named = function() run(ring_m(globals = list(k = c(a = 1))), abm_global(k ~ k + 1), ticks = 2),
  global_na_then_num = function() run(ring_m(globals = list(k = NA)), abm_global(k ~ 1.5), ticks = 2),
  global_char = function() run(ring_m(globals = list(k = "a")), abm_global(k ~ paste0(k, "b")), ticks = 2),
  global_date = function() run(ring_m(globals = list(k = as.Date("2020-01-01"))), abm_global(k ~ k + 1), ticks = 2),
  global_tick_name = function() run(ring_m(globals = list(tick = 5)), ticks = 2),
  col_tick_name = function() { m <- abm_setup(agents = abm_agents(n = 3, tick = 1)); run(m, ticks = 1) },
  record_globals = function() run(ring_m(), abm_rules(x ~ x + 1), record = "globals"),
  record_int = function() run(ring_m(), abm_rules(x ~ x + 1), record = 3, ticks = 7),
  record_final = function() run(ring_m(), abm_rules(x ~ x + 1), record = "final"),
  measures_checks = function() run(ring_m(), abm_rules(x ~ x + rnorm(n())),
                                   measures = list(m = ~mean(x), s = ~sd(y)), checks = list(ok = ~all(y >= 0))),
  check_fails = function() run(ring_m(), abm_rules(y ~ y - 0.3), checks = list(ok = ~all(y >= 0))),
  measure_bad = function() run(ring_m(), measures = list(m = ~x)),
  params_reps = function() run(ring_m(), abm_rules(x ~ x * k + rnorm(n())), params = list(k = c(1, 2)), reps = 2),
  params_vec = function() run(ring_m(), abm_rules(x ~ x + sum(k)), params = list(k = list(c(1, 2)))),
  params_named_char = function() run(ring_m(), abm_global(s ~ paste(k)), params = list(k = c("a", "b"))),
  params_factor = function() run(ring_m(), params = list(k = factor(c("a", "b")))),
  params_cores = function() run(ring_m(), abm_neighbours(s ~ mean(x)), abm_rules(x ~ x + rnorm(n())),
                                params = list(k = c(1, 2)), reps = 2, cores = 2),
  go_reuse = function() {
    go <- abm_go(abm_neighbours(s ~ sum(x)), abm_rules(x ~ x + s * 0.01),
                 abm_repeat(abm_rules(y ~ y + 1), max = 2))
    before <- serialize(go, NULL)
    r1 <- abm_run(ring_m(), go, ticks = 3, seed = 1)
    m2 <- abm_setup(agents = list(a = abm_agents(n = 5, x = 1, y = 0), b = abm_agents(n = 4, x = 2, y = 1)),
                    network = abm_network("ring", degree = 2))
    r2 <- abm_run(m2, go, ticks = 3, seed = 1)
    r3 <- abm_run(ring_m(), go, ticks = 3, seed = 1)
    list(r1, r2, identical(r1, r3), identical(before, serialize(go, NULL)))
  },
  model_fn = function() abm_run(function(k) ring_m(globals = list(k = k)), abm_go(abm_neighbours(s ~ mean(x * k))),
                                ticks = 2, params = list(k = c(1, 3)), seed = 2),
  tell = function() run(ring_m(), abm_tell(y ~ x, to = abm_match(pair = "random"))),
  match_rules = function() run(ring_m(), abm_match(pair = "random"), abm_rules(x ~ partner_x), abm_rules(y ~ x + 1))
)
