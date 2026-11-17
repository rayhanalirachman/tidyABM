# round-9 cases: Part 0 holes (tell + match columns, relations, size > 2, cores, overflow)
one <- function(n = 8, seed = 1) abm_setup(agents = abm_agents(n = n, x = ~as.numeric(seq_len(n)), z = 0,
                                                              t = ~sample.int(n, n, TRUE)), seed = seed)
rel <- function(seed = 1) abm_setup(agents = abm_agents(n = 8, x = ~as.numeric(1:8), z = 0, t = ~sample.int(8L, 8L, TRUE)),
  relations = list(r = abm_relation(edges = data.frame(from = c(1:4, 5:8), to = c(5:8, 1:4)), v = ~runif(8))), seed = seed)
two <- function(seed = 1) abm_setup(agents = list(a = abm_agents(n = 5, x = ~rnorm(n), z = 0),
                                                  b = abm_agents(n = 7, x = ~rnorm(n), z = 0, t = ~sample.int(12L, n, TRUE))),
                                    globals = list(q = 0), seed = seed)
big <- function() abm_setup(agents = list(a = abm_agents(n = 3, x = 1, t = .Machine$integer.max - 1L),
                                          b = abm_agents(n = 4, x = 2, t = .Machine$integer.max - 2L)),
                            globals = list(q = 0), seed = 1)
R <- function(m, ..., ticks = 2, measures = NULL, checks = NULL, reps = 1, cores = 1)
  abm_run(m, abm_go(...), ticks = ticks, seed = 1, measures = measures, checks = checks, reps = reps, cores = cores)
cases <- list(
  rel_tell_px = function() R(rel(), abm_match(pair = "random"), abm_tell(z ~ partner_x, to = t, .resolve = "sum")),
  rel_tell_R = function() R(rel(), abm_match(pair = "random"), abm_tell(z ~ coalesce(R_v, -1), to = t, when = .R, .resolve = "sum")),
  rel_tell_px_when = function() R(rel(), abm_match(pair = "random"), abm_tell(z ~ x, to = 1L, when = partner_x > 3, .resolve = "sum")),
  rel_tell_back = function() R(rel(), abm_match(pair = "random"), abm_tell(z ~ coalesce(R_v_back, 0) + partner_x, to = t, .resolve = "sum")),
  rel_rules_px = function() R(rel(), abm_match(pair = "random"), abm_rules(z ~ coalesce(partner_x, 0) + coalesce(R_v, 0))),
  s3_tell_px = function() R(one(9), abm_match(pair = "random", size = 3), abm_tell(z ~ partner_x, to = t, .resolve = "sum")),
  s3_tell_px_when = function() R(one(9), abm_match(pair = "random", size = 3), abm_tell(z ~ x, to = t, when = partner_x > 2, .resolve = "sum")),
  s3_rules_px = function() R(one(9), abm_match(pair = "random", size = 3), abm_rules(z ~ partner_x)),
  s3_tell_gid = function() R(one(9), abm_match(pair = "random", size = 3), abm_tell(z ~ x, to = t, when = .group_id == 1L, .resolve = "sum")),
  cores_tell_px = function() R(one(), abm_match(pair = "random"), abm_tell(z ~ partner_x, to = t, .resolve = "sum"),
    abm_rules(x ~ x + rnorm(n())), reps = 2, cores = 2),
  cores_tell_px_only = function() R(two(), abm_match(pair = "one_of", eligible = .group == "b", among = .group == "a"),
    abm_tell(z ~ partner_x, to = 1L, when = .group == "b", .resolve = "mean"), reps = 3, cores = 2),
  ovf_global = function() abm_globals(R(big(), abm_global(q ~ sum(t[.group == "b"])))),
  ovf_global_all = function() abm_globals(R(big(), abm_global(q ~ sum(t)))),
  ovf_global_mixed = function() abm_globals(R(big(), abm_global(q ~ sum(t[.group == "a"]) + mean(x)))),
  ovf_measure = function() R(big(), abm_rules(x ~ x + 1), measures = list(s = ~sum(t[.group == "b"])))
)
