# round-8 adversarial cases, part 2 (verifier v8)
one <- function(n = 8, seed = 1) abm_setup(agents = abm_agents(n = n, x = ~as.numeric(seq_len(n)), z = 0,
                                                              t = ~sample.int(n, n, TRUE)), seed = seed)
two <- function(seed = 1) abm_setup(agents = list(a = abm_agents(n = 5, x = ~rnorm(n), z = 0),
                                                  b = abm_agents(n = 7, x = ~rnorm(n), z = 0, t = ~sample.int(12L, n, TRUE))),
                                    globals = list(q = 0, big = .Machine$integer.max), seed = seed)
R <- function(m, ..., ticks = 2, measures = NULL, checks = NULL)
  abm_run(m, abm_go(...), ticks = ticks, seed = 1, measures = measures, checks = checks)
cases <- list(
  # abm_tell + standing match, partner_<col> read without `.partner`
  tp_value = function() R(one(), abm_match(pair = "random"), abm_tell(z ~ partner_x, to = t, .resolve = "sum")),
  tp_when = function() R(one(), abm_match(pair = "random"), abm_tell(z ~ x, to = t, when = partner_x > 3, .resolve = "sum")),
  tp_order = function() R(one(), abm_match(pair = "random"), abm_tell(z ~ x, to = 1L, .order = partner_x, .resolve = "first")),
  tp_to = function() R(one(), abm_match(pair = "random"), abm_tell(z ~ x, to = as.integer(partner_x), .resolve = "sum")),
  tp_with_partner = function() R(one(), abm_match(pair = "random"), abm_tell(z ~ partner_x, to = .partner, .resolve = "sum")),
  tp_role_only = function() R(one(), abm_match(pair = "random", role = list(p = x > 4, q = x <= 4)),
    abm_tell(z ~ if_else(.role == "p", 1, 2), to = t, .resolve = "sum")),
  tp_groupid_only = function() R(one(), abm_match(pair = "random"), abm_tell(z ~ .group_id, to = t, .resolve = "sum")),
  tp_size3 = function() R(one(9), abm_match(pair = "random", size = 3), abm_tell(z ~ .group_id, to = t, .resolve = "sum")),
  tp_two_groups = function() R(two(), abm_match(pair = "one_of", eligible = .group == "b", among = .group == "a"),
    abm_tell(z ~ partner_x, to = 1L, when = .group == "b", .resolve = "mean")),
  # abm_rules (non-.by) reading partner cols with deaths in between
  rules_partner = function() R(two(), abm_match(pair = "random"), abm_death(when = .id == 3L), abm_rules(z ~ coalesce(partner_x, 0))),
  # match_rows: births make stacked ids unsorted
  births_match = function() R(two(), abm_birth(when = .group == "a" & x > 0), abm_match(pair = "random"),
    abm_rules(z ~ coalesce(partner_x, -1) + .group_id), ticks = 3),
  # .by with a match standing
  by_match = function() R(two(), abm_match(pair = "random"), abm_rules(z ~ sum(x) + max(x[x > 0]), .by = .group_id)),
  by_match_partner = function() R(two(), abm_match(pair = "random"), abm_rules(z ~ sum(partner_x, na.rm = TRUE), .by = t)),
  # narrowing: global with big integer scalar, abm_global with .by, measures with check on narrowed NA
  g_big = function() abm_globals(R(two(), abm_global(q ~ sum(t[.group == "b"]) + big))),
  g_big2 = function() abm_globals(R(two(), abm_global(q ~ sum(t[.group == "b"] + big)))),
  g_narm_mixed = function() abm_globals(R(two(), abm_global(q ~ sum(t, na.rm = TRUE) + mean(x[.group == "a"])))),
  g_narm_shared = function() abm_globals(R(two(), abm_global(q ~ sum(x, na.rm = TRUE)))),
  g_narm_pad_nonNA = function() abm_globals(R(two(), abm_global(q ~ sum(t^0, na.rm = TRUE)))),
  g_narm_pad_isna = function() abm_globals(R(two(), abm_global(q ~ sum(is.na(t), na.rm = TRUE)))),
  g_narm_pmax = function() abm_globals(R(two(), abm_global(q ~ sum(pmax(t, 3L), na.rm = TRUE)))),
  g_narm_char = function() abm_globals(R(two(), abm_rules(lab ~ as.character(x)), abm_global(q ~ max(lab, na.rm = TRUE)))),
  g_check_twice = function() R(two(), abm_rules(x ~ x * 2), ticks = 4,
    checks = list(c1 = ~sum(x[.group == "a"]) < 30, c2 = ~TRUE)),
  g_reps = function() abm_run(two(), abm_go(abm_global(q ~ sum(x[.group == "a"]))), ticks = 2, seed = 1, reps = 2,
    measures = list(m = ~sum(x[.group == "b"]))),
  g_params = function() abm_run(two(), abm_go(abm_global(q ~ sum(x[.group == "a"]) * k)), ticks = 2, seed = 1,
    params = list(k = c(1, 2)), measures = list(m = ~mean(t, na.rm = TRUE) * k))
)
