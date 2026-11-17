tg <- function(nf = 5, nh = 12, seed = 1) abm_setup(
  agents = list(
    firm = abm_agents(n = nf, loan = ~c(NA, runif(n) - 0.2)[seq_len(n)],
                      status = ~sample(0:3, n, TRUE), x = ~rnorm(n), on = ~runif(n) < 0.5,
                      wage_out = ~runif(n), sales = 0, active = ~as.numeric(runif(n) < 0.8)),
    hh = abm_agents(n = nh, dep = ~c(runif(1), NaN, runif(n))[seq_len(n)], x = ~rnorm(n),
                    firm = ~sample.int(max(nf, 1L), n, TRUE), inc = 0, spend = 0)),
  globals = list(k = 2, v = c(1, 2), a = 0, b = 0, c = 0, d = 0, e = 0, f = 0, g = 0, h = 0,
                 i = 0, j = 0, l = 0, m = 0, o = 0, p = 0, r = 0, s = 0, u = 0, z = 0, directed = 0),
  seed = seed)
glob <- function() abm_global(
  a ~ sum(loan[.group == "firm"]), b ~ sum(loan[.group == "firm"] > 0, na.rm = TRUE),
  c ~ max(abs(loan - x)[.group == "firm"], na.rm = TRUE), d ~ sum(status == 3L, na.rm = TRUE),
  e ~ median(loan, na.rm = TRUE), f ~ mean(dep * k, na.rm = TRUE),
  g ~ sum(dep[.group == "hh" & dep > 0.5]), h ~ sum(x[.group == "hh"]) + sum(x[.group == "firm"]),
  i ~ sum(loan[.group == "firm" & NA]), j ~ length(loan[.group == "firm" & FALSE]),
  l ~ sum(is.na(loan), na.rm = TRUE), m ~ sum(dep[.group == "hh"] * k),
  o ~ sum((dep * v)[.group == "hh"]), p ~ a + sum(loan[.group == "firm"], na.rm = TRUE),
  r ~ sum(as.integer(status)[.group == "firm"]), s ~ sum(loan[.group == "nobody"]),
  u ~ if (isTRUE(a > 0)) sum(on[.group == "firm"]) else -1, z ~ sum(runif(1) + dep[.group == "hh"]))
meas <- list(m1 = ~sum(loan[.group == "firm"], na.rm = TRUE) / max(1, sum(dep[.group == "hh"], na.rm = TRUE)),
             m2 = ~max(abs(dep - 1)[.group == "hh"], na.rm = TRUE), m3 = ~sum(status == 2L, na.rm = TRUE))
chk <- list(c1 = ~sum(x[.group == "firm"]) < 1e9)
gross_like <- function(m) abm_run(m, abm_go(
  glob(),
  abm_rules(inc ~ if_else(.group == "hh", sum(wage_out[.group == "firm"]), 0), .by = firm),
  abm_rules(dep ~ dep + inc),
  abm_match(pair = "one_of", eligible = .group == "hh", among = .group == "firm" & active == 1),
  abm_rules(spend ~ 0.5 * dep, .scope = "population"),
  abm_tell(sales ~ spend, to = if (directed == 1) firm else .partner, when = .group == "hh", .resolve = "sum"),
  abm_rules(loan ~ loan + runif(n()) - 0.5, dep ~ dep * 0.9, status ~ sample(0:3, n(), TRUE),
            active ~ as.numeric(runif(n()) < 0.8))),
  ticks = 4, seed = 3, measures = meas, checks = chk, record = "all")
cases <- list(
  g_basic = function() gross_like(tg()),
  g_one_firm = function() gross_like(tg(1, 7)),
  g_one_hh = function() gross_like(tg(6, 1)),
  g_no_firms = function() gross_like(tg(0, 9)),
  g_no_hh = function() gross_like(tg(4, 0)),
  g_warn = function() abm_globals(abm_run(tg(), abm_go(abm_global(a ~ sum(sqrt(loan - 2)[.group == "firm"]))), ticks = 2, seed = 1)),
  g_empty_max = function() abm_globals(abm_run(tg(), abm_go(abm_global(a ~ max(loan[.group == "firm" & FALSE]))), ticks = 2, seed = 1)),
  g_order_warn = function() abm_globals(abm_run(tg(), abm_go(abm_global(a ~ log(-1) + max(dep[.group == "hh"] > 2, na.rm = TRUE))), ticks = 1, seed = 1)),
  g_err = function() abm_globals(abm_run(tg(), abm_go(abm_global(a ~ sum(loan[.group == "firm"]) + "x")), ticks = 1, seed = 1)),
  g_len = function() abm_globals(abm_run(tg(), abm_go(abm_global(a ~ loan[.group == "firm"])), ticks = 1, seed = 1)),
  g_check_fail = function() abm_run(tg(), abm_go(abm_rules(x ~ x * 1e10)), ticks = 3, seed = 1, checks = chk),
  by_shapes = function() abm_run(tg(6, 15), abm_go(
    abm_rules(x ~ mean(x[x > 0]) + max(x), .by = firm),
    abm_rules(x ~ length(x[NA]) + sum(x[FALSE]) + prod(x), .by = firm),
    abm_rules(x ~ sum((x * v)[x > 0]), .by = firm),
    abm_rules(x ~ sum(x[runif(n()) > 0.5]), .by = firm)), ticks = 3, seed = 2),
  by_warn = function() abm_run(tg(6, 15), abm_go(abm_rules(x ~ sum(sqrt(x)[x > -1]), .by = firm)), ticks = 2, seed = 2),
  by_key_rewritten = function() abm_run(tg(6, 15), abm_go(
    abm_rules(firm ~ firm %% 3, x ~ sum(x[x > 0]), .by = firm),
    abm_rules(firm ~ firm + 1L, .by = firm)), ticks = 3, seed = 2),
  tell_roles = function() abm_run(tg(6, 15), abm_go(
    abm_match(pair = "one_of", eligible = .group == "hh", among = .group == "firm"),
    abm_tell(x ~ if_else(is.na(.role), dep, 0) + .group_id, to = .partner, when = .group == "hh", .resolve = "mean")),
    ticks = 3, seed = 5),
  tell_deaths = function() abm_run(tg(6, 15), abm_go(
    abm_death(when = .group == "hh" & runif(n()) < 0.2),
    abm_match(pair = "one_of", eligible = .group == "hh", among = .group == "firm"),
    abm_tell(x ~ dep, to = .partner, when = .group == "hh", .resolve = "max")), ticks = 3, seed = 5),
  tell_bad = function() abm_run(tg(), abm_go(abm_tell(x ~ dep, to = firm + 100L, when = .group == "hh")), ticks = 1, seed = 1),
  tell_bad_dead = function() abm_run(tg(), abm_go(abm_death(when = .id == 3L), abm_tell(x ~ dep, to = firm + 100L, when = .group == "hh")), ticks = 1, seed = 1),
  one_of_single = function() abm_run(tg(1, 1), abm_go(abm_match(pair = "one_of"), abm_rules(x ~ coalesce(partner_x, x))), ticks = 3, seed = 1),
  one_of_roles = function() abm_run(tg(3, 5), abm_go(abm_match(pair = "one_of", role = list(a = x > 0, b = x <= 0)),
    abm_rules(x ~ if_else(is.na(.role), x, -x))), ticks = 3, seed = 1)
)
