# round-8 adversarial cases (verifier v8). Same format as v7/cases*.R
m3 <- function(na = 6, nb = 8, nc = 5, seed = 1, globals = list()) {
  ag <- list()
  if (!is.null(na)) ag$a <- abm_agents(n = na, x = ~sample.int(9L, n, TRUE), y = ~c(NA, rnorm(n))[seq_len(n)],
                                       big = 2e9L, f = ~factor(sample(c("u", "v"), n, TRUE)),
                                       d = ~as.Date("2020-01-01") + seq_len(n), k = ~sample(1:3, n, TRUE),
                                       on = ~c(NA, runif(n) < 0.5)[seq_len(n)], z = ~rnorm(n))
  if (!is.null(nb)) ag$b <- abm_agents(n = nb, x = ~rnorm(n), w = ~runif(n), k = ~c(NA, sample(1:3, n, TRUE))[seq_len(n)],
                                       on = ~runif(n) < 0.5, lst = ~as.list(seq_len(n)))
  if (!is.null(nc)) ag$c <- abm_agents(n = nc, x = ~rnorm(n), v = ~sample.int(4L, n, TRUE), on = TRUE)
  abm_setup(agents = ag, globals = c(list(q = 0, s1 = 2, lab = "a", vec = c(1, 2), flag = TRUE,
                                          nmd = c(u = 3)), globals), seed = seed)
}
G <- function(m, ..., ticks = 3, measures = NULL, checks = NULL)
  abm_globals(abm_run(m, abm_go(abm_global(...), abm_rules(x ~ x + 1L)), ticks = ticks, seed = 1,
                      measures = measures, checks = checks))
R <- function(m, ..., ticks = 3, measures = NULL, checks = NULL, record = "all")
  abm_run(m, abm_go(...), ticks = ticks, seed = 1, measures = measures, checks = checks, record = record)

cases <- list(
  # ---- narrowing in abm_global
  n_types_differ = function() G(m3(), q ~ sum(x[.group == "a"])),            # x int in a, dbl in b/c
  n_types_differ_b = function() G(m3(), q ~ sum(x[.group == "b"])),
  n_shared_cond = function() G(m3(), q ~ sum(x[.group == "c" & on])),         # on lgl in all, NA in a
  n_shared_cond_a = function() G(m3(), q ~ sum(z[.group == "a" & on])),
  n_reversed = function() G(m3(), q ~ sum(x["a" == .group])),
  n_neq = function() G(m3(), q ~ sum(x[.group != "a"])),
  n_in = function() G(m3(), q ~ sum(x[.group %in% "a"])),
  n_lab_global = function() G(m3(), q ~ sum(x[.group == lab])),
  n_vec_label = function() G(m3(), q ~ sum(x[.group == c("a", "b")])),
  n_nolabel = function() G(m3(), q ~ sum(x[.group == "zz"])),
  n_na_label = function() G(m3(), q ~ sum(x[.group == NA_character_])),
  n_nested = function() G(m3(), q ~ sum(x[.group == "b"][2:3])),
  n_nested2 = function() G(m3(), q ~ sum(w[.group == "b" & w > mean(w[.group == "b"])])),
  n_scalar_global = function() G(m3(), q ~ sum((w * s1)[.group == "b"]) + s1),
  n_vec_global = function() G(m3(), q ~ sum((w * vec)[.group == "b"])),
  n_named_global = function() G(m3(), q ~ sum(w[.group == "b"]) * nmd),
  n_named_global_in = function() G(m3(), q ~ sum((w * nmd)[.group == "b"])),
  n_nfun = function() G(m3(), q ~ sum(w[.group == "b"]) / n()),
  n_rng = function() G(m3(), q ~ sum(w[.group == "b"]) + runif(1), s1 ~ sum((w + rnorm(1))[.group == "b"])),
  n_userfun = function() { ff <- function(v) v * 2; G(m3(), q ~ sum(ff(w)[.group == "b"])) },
  n_shadow_sum = function() { sum <- function(...) 42; G(m3(), q ~ sum(w[.group == "b"])) },
  n_shadow_abs = function() { abs <- function(v) -v; G(m3(), q ~ sum(abs(w)[.group == "b"]), s1 ~ max(abs(w), na.rm = TRUE)) },
  n_shadow_eq = function() { `==` <- function(e1, e2) base::`==`(e1, e2) | TRUE; G(m3(), q ~ sum(w[.group == "b"])) },
  n_shadow_bracket = function() { `[` <- function(x, i) base::`[`(x, i)[1]; G(m3(), q ~ sum(w[.group == "b"])) },
  n_qualified = function() G(m3(), q ~ base::sum(w[.group == "b"]) + sum(x[.group == "c"]), s1 ~ base::max(w, na.rm = TRUE)),
  n_zero_rows = function() G(m3(na = 0), q ~ sum(x[.group == "a"]), s1 ~ max(y, na.rm = TRUE)),
  n_zero_rows_mean = function() G(m3(na = 0), q ~ mean(y, na.rm = TRUE), s1 ~ median(y, na.rm = TRUE)),
  n_two_groups_only = function() G(m3(nc = NULL), q ~ sum(x[.group == "a"]) - sum(w, na.rm = TRUE)),
  n_narm_false = function() G(m3(), q ~ sum(y, na.rm = FALSE), s1 ~ sum(w, na.rm = flag)),
  n_narm_T = function() G(m3(), q ~ sum(w, na.rm = T)),
  n_allna_mean = function() G(m3(), q ~ mean(y * NA, na.rm = TRUE), s1 ~ median(y[FALSE], na.rm = TRUE)),
  n_empty_max = function() G(m3(), q ~ max(w[.group == "b" & w > 5]), s1 ~ min(y + NA, na.rm = TRUE)),
  n_overflow = function() G(m3(), q ~ sum(big[.group == "a"]), s1 ~ sum(big, na.rm = TRUE)),
  n_overflow_v = function() G(m3(), q ~ sum(v * 1000000000L, na.rm = TRUE)),
  n_factor = function() G(m3(), q ~ length(f[.group == "a"]), s1 ~ sum(f == "u", na.rm = TRUE)),
  n_date = function() G(m3(), q ~ as.numeric(max(d[.group == "a"])), s1 ~ sum(d > as.Date("2020-01-03"), na.rm = TRUE)),
  n_list = function() G(m3(), q ~ length(lst[.group == "b"])),
  n_error = function() G(m3(), q ~ sum(w[.group == "b"]) + nosuch),
  n_error2 = function() G(m3(), q ~ sum(lst[.group == "b"])),
  n_error3 = function() G(m3(), q ~ sum(w, nosuch, na.rm = TRUE)),
  n_seq_rules = function() G(m3(), q ~ sum(w[.group == "b"]), s1 ~ q * 2 + sum(v[.group == "c"]), q ~ s1 + max(z, na.rm = TRUE)),
  n_new_global = function() G(m3(globals = list()), newg ~ sum(w[.group == "b"]), s2 ~ newg + 1),
  n_any_all = function() G(m3(), q ~ any(on[.group == "b"]) + all(on[.group == "a"]), s1 ~ any(w > 0.5, na.rm = TRUE) + prod(v, na.rm = TRUE)),
  n_if = function() G(m3(), q ~ if (sum(w[.group == "b"]) > 3) 1 else sum(v[.group == "c"])),
  n_isna = function() G(m3(), q ~ sum(is.na(y[.group == "a"])), s1 ~ sum(!is.na(k), na.rm = TRUE)),
  n_k_shared = function() G(m3(), q ~ sum(k[.group == "b"], na.rm = TRUE), s1 ~ sum(k[.group == "a" & k > 1L])),
  n_births_deaths = function() R(m3(na = 3), abm_global(q ~ sum(x[.group == "a"]), s1 ~ mean(y, na.rm = TRUE)),
    abm_death(when = .group == "a" & runif(n()) < 0.6), abm_birth(when = .group == "c" & runif(n()) < 0.3),
    ticks = 6, measures = list(m1 = ~sum(x[.group == "a"]), m2 = ~max(y, na.rm = TRUE))),
  n_group_appears = function() R(m3(na = 0), abm_birth(when = .group == "a" | (.group == "b" & runif(n()) < 0.1)),
    abm_global(q ~ sum(z[.group == "a"]) + max(z, na.rm = TRUE)), ticks = 5),
  n_large_pop = function() G(m3(na = 3000, nb = 4000, nc = 200), q ~ sum(w[.group == "b" & on]), s1 ~ mean(z, na.rm = TRUE)),
  # ---- measures / checks
  mc_patterns = function() R(m3(), abm_rules(x ~ x + 1L), ticks = 4,
    measures = list(a1 = ~sum(x[.group == "a"]), a2 = ~"a" == lab, a3 = ~max(y, na.rm = TRUE),
                    a4 = ~sum(w[.group == "b" & w > 2]), a5 = ~mean(y * NA, na.rm = TRUE),
                    a6 = ~sum(big[.group == "a"]), a7 = ~sum(x[.group != "a"])),
    checks = list(c1 = ~sum(w[.group == "b"]) >= 0, c2 = ~max(z, na.rm = TRUE) < 1e6)),
  mc_check_fail = function() R(m3(), abm_rules(z ~ z * 100), ticks = 4,
    checks = list(c1 = ~max(z, na.rm = TRUE) < 50)),
  mc_check_na = function() R(m3(), abm_rules(x ~ x), ticks = 2, checks = list(c1 = ~sum(y[.group == "a"]) > 0)),
  mc_check_bad = function() R(m3(), abm_rules(x ~ x), ticks = 2, checks = list(c1 = ~w[.group == "b"] > 0)),
  mc_measure_len = function() R(m3(), abm_rules(x ~ x), ticks = 2, measures = list(m = ~w[.group == "b"])),
  mc_measure_warn = function() R(m3(), abm_rules(x ~ x), ticks = 2, measures = list(m = ~max(w[.group == "b" & w > 9]), m2 = ~sum(big[.group == "a"]))),
  # ---- .by rules
  by_na_key = function() R(m3(nc = NULL), abm_rules(w ~ sum(w) + mean(w[w > 0.3]), .by = k), ticks = 3),
  by_key_one_group = function() R(m3(), abm_rules(z ~ sum(z) + max(z[z > 0]), .by = d), ticks = 2),
  by_key_int_one_group = function() R(m3(), abm_rules(v ~ sum(v) + length(v[v > 2L]), .by = v), ticks = 3),
  by_key_changes = function() R(m3(), abm_rules(k ~ (k %% 3L) + 1L), abm_rules(x ~ sum(x) + min(x[x > 0]), .by = k), ticks = 4),
  by_several = function() R(m3(), abm_rules(x ~ sum(x) * mean(x) - max(x[x > 1]) + prod(x[x < 0]) + median(x), .by = k), ticks = 3),
  by_constants = function() R(m3(), abm_rules(x ~ sum(1) + mean(2) + max(s1), .by = k), ticks = 2),
  by_rng = function() R(m3(), abm_rules(x ~ sum(x[runif(n()) < 0.5]) + rnorm(1), .by = k), ticks = 3),
  by_n = function() R(m3(), abm_rules(x ~ sum(x) / n(), .by = k), ticks = 3),
  by_int_overflow = function() R(m3(), abm_rules(big ~ sum(big), .by = k), ticks = 2),
  by_shadow = function() { sum <- function(...) -1; R(m3(), abm_rules(x ~ sum(x), .by = k), ticks = 2) },
  by_global_vec = function() R(m3(), abm_rules(x ~ sum(x * vec), .by = k), ticks = 2),
  by_lgl_key = function() R(m3(), abm_rules(z ~ sum(z) + mean(z[z > 0]), .by = on), ticks = 2),
  by_dbl_key_negzero = function() R(m3(nb = NULL, nc = NULL, globals = list()), abm_rules(kk ~ if_else(z > 0, 0, -0)),
    abm_rules(z ~ sum(z), .by = kk), ticks = 3),
  by_deaths = function() R(m3(), abm_death(when = runif(n()) < 0.15), abm_rules(x ~ sum(x[x > 0]), .by = k), ticks = 4),
  by_cond_na = function() R(m3(), abm_rules(z ~ sum(z[on]) + length(z[on]), .by = k), ticks = 2),
  by_cond_scalar = function() R(m3(), abm_rules(z ~ sum(z[flag]) + sum(z[NA]), .by = k), ticks = 2),
  by_named = function() R(m3(), abm_rules(z ~ sum(z * nmd), .by = k), ticks = 2),
  by_global_rule = function() R(m3(), abm_global(q ~ sum(z), .by = k), ticks = 2),
  # ---- abm_tell
  tell_partner_col = function() R(m3(nc = NULL), abm_match(pair = "random"),
    abm_tell(z ~ partner_x, to = k, when = !is.na(k), .resolve = "sum"), ticks = 3),
  tell_partner_col2 = function() R(m3(nc = NULL), abm_match(pair = "random"),
    abm_tell(z ~ partner_x + .group_id, to = .partner, .resolve = "mean"), ticks = 3),
  tell_partner_onlycol = function() R(m3(nc = NULL), abm_match(pair = "one_of", eligible = .group == "b", among = .group == "a"),
    abm_tell(z ~ partner_z, to = 1L, when = .group == "b", .resolve = "collect"), ticks = 2),
  tell_nondense = function() R(m3(), abm_death(when = .id %in% c(2L, 5L)),
    abm_tell(z ~ w, to = 3L, when = .group == "b", .resolve = "sum"), ticks = 2),
  tell_dead_recipient = function() R(m3(), abm_death(when = .id %in% c(2L, 5L)),
    abm_tell(z ~ w, to = 5L, when = .group == "b"), ticks = 2),
  tell_out_range = function() R(m3(), abm_tell(z ~ w, to = 0L, when = .group == "b"), ticks = 1),
  tell_out_range_hi = function() R(m3(), abm_tell(z ~ w, to = 1000L, when = .group == "b"), ticks = 1),
  tell_dbl_recipient = function() R(m3(), abm_tell(z ~ w, to = 2.7, when = .group == "b"), ticks = 1),
  tell_na_recipient = function() R(m3(), abm_tell(z ~ w, to = if_else(w > 0.5, NA_integer_, 1L), when = .group == "b"), ticks = 2),
  tell_neg = function() R(m3(), abm_tell(z ~ w, to = -1L, when = .group == "b"), ticks = 1),
  tell_list = function() R(m3(), abm_tell(z ~ w, to = list(c(1L, 1L, 2L)), when = .group == "b", .resolve = "collect"), ticks = 1),
  tell_resolves = function() {
    out <- list()
    for (rv in c("last", "first", "sum", "mean", "max", "min", "collect", "error"))
      out[[rv]] <- tryCatch(R(m3(), abm_tell(z ~ w, to = (.id %% 3L) + 1L, when = .group == "b", .resolve = rv), ticks = 2),
                            error = function(e) conditionMessage(e))
    out
  },
  tell_order = function() R(m3(), abm_tell(z ~ w, to = 1L, when = .group == "b", .resolve = "first", .order = -w), ticks = 2),
  tell_roles = function() R(m3(nc = NULL), abm_match(pair = "random", role = list(p = on %in% TRUE, q = !(on %in% TRUE))),
    abm_tell(z ~ if_else(.role == "p", 1, 2), to = .partner, .resolve = "sum"), ticks = 3),
  tell_after_births = function() R(m3(), abm_birth(when = .group == "c"),
    abm_tell(z ~ w, to = max(.id), when = .group == "b", .resolve = "sum"), ticks = 2),
  # ---- abm_match one_of
  oo_zero = function() R(m3(), abm_match(pair = "one_of", eligible = .group == "b", among = .group == "zz"),
    abm_rules(z ~ coalesce(partner_z, -1)), ticks = 2),
  oo_one = function() R(m3(), abm_match(pair = "one_of", eligible = .group == "b", among = .id == 1L),
    abm_rules(z ~ coalesce(partner_z, -1) + runif(n())), ticks = 3),
  oo_one_self = function() R(m3(), abm_match(pair = "one_of", among = .id == 1L),
    abm_rules(z ~ coalesce(partner_z, -1) + runif(n())), ticks = 3),
  oo_two = function() R(m3(), abm_match(pair = "one_of", among = .id %in% c(1L, 2L)),
    abm_rules(z ~ coalesce(partner_z, -1) + runif(n())), ticks = 3),
  oo_overlap = function() R(m3(), abm_match(pair = "one_of", eligible = .group != "c", among = .group == "b"),
    abm_rules(z ~ coalesce(partner_z, -1) + runif(n())), ticks = 3),
  oo_roles = function() R(m3(), abm_match(pair = "one_of", among = .group == "b", role = list(r1 = on %in% TRUE, r2 = !(on %in% TRUE))),
    abm_rules(z ~ if_else(is.na(.role), z, runif(n()))), ticks = 3),
  oo_seed = function() { R(m3(), abm_match(pair = "one_of", among = .group == "b"), ticks = 3); runif(3) },
  # ---- neighbour fast path
  nb_lgl_na = function() {
    m <- abm_setup(agents = abm_agents(n = 12, b = c(TRUE, NA, FALSE, TRUE, NA, NA, TRUE, FALSE, TRUE, TRUE, NA, FALSE),
                                       i = c(1L, NA, 3L, .Machine$integer.max, .Machine$integer.max, 2L, NA, 1L, 1L, -5L, 0L, 7L)),
                   network = abm_network("ring", degree = 4), seed = 1)
    R(m, abm_neighbours(s ~ sum(b), a ~ mean(b), t ~ sum(i), u ~ mean(i), v ~ sum(!b), w ~ sum(i > 0L), y ~ sum(is.na(i))), ticks = 1)
  },
  nb_qualified = function() {
    m <- abm_setup(agents = abm_agents(n = 12, x = ~rnorm(n)), network = abm_network("ring", degree = 2), seed = 1)
    R(m, abm_neighbours(s ~ base::sum(x), a ~ base::mean(abs(x)), t ~ sum(dplyr::if_else(x > 0, x, 0)), u ~ sum(base::abs(x))), ticks = 2)
  },
  nb_shadow_mean = function() { mean <- function(x, ...) 99
    m <- abm_setup(agents = abm_agents(n = 12, x = ~rnorm(n)), network = abm_network("ring", degree = 2), seed = 1)
    R(m, abm_neighbours(a ~ mean(x), s ~ base::mean(x)), ticks = 1) },
  nb_vec_global2 = function() {
    m <- abm_setup(agents = abm_agents(n = 12, x = ~rnorm(n)), network = abm_network("ring", degree = 2),
                   globals = list(g2 = c(1, -1), g12 = as.numeric(1:12)), seed = 1)
    R(m, abm_neighbours(s ~ sum(x * g2), t ~ mean(g12), u ~ sum(x + g12)), ticks = 1)
  },
  nb_matrix_global = function() {
    m <- abm_setup(agents = abm_agents(n = 6, x = ~rnorm(n)), network = abm_network("ring", degree = 2),
                   globals = list(mm = matrix(1:6, 2)), seed = 1)
    R(m, abm_neighbours(s ~ sum(x * mm)), ticks = 1)
  },
  nb_groups = function() {
    m <- abm_setup(agents = list(a = abm_agents(n = 5, x = ~sample.int(3L, n, TRUE)), b = abm_agents(n = 5, x = ~rnorm(n))),
                   network = abm_network("ring", degree = 2), seed = 1)
    R(m, abm_neighbours(s ~ sum(x), a ~ mean(x > 1)), ticks = 2)
  }
)
