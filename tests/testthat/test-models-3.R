# Part 4 of the model corpus: ten more non-spatial models, run at reduced
# scale. Each test pins the behavioural claim the model is known for.

# --- the three grammar additions this round forced -------------------------

test_that("abm_neighbours() can compare a neighbour to the focal agent", {
  m <- abm_setup(
    agents  = abm_agents(n = 4, wealth = ~seq_len(n)),
    network = abm_network(type = "complete")
  )
  r <- abm_run(m, abm_go(abm_neighbours(richer ~ sum(wealth > own_wealth))),
               ticks = 1, seed = 1)
  final <- r[r$tick == 1, ]
  expect_equal(final$richer[order(final$.id)], c(3, 2, 1, 0))
})

test_that('abm_network(type = "complete") connects every pair', {
  m <- abm_setup(agents = abm_agents(n = 6, x = 1),
                 network = abm_network(type = "complete"))
  expect_equal(nrow(m$edges), choose(6, 2))
})

test_that("abm_link() on a group of three or more links it as a clique", {
  m <- abm_setup(agents = abm_agents(n = 9, x = 1),
                 network = abm_network(type = "empty"))
  r <- abm_run(m, abm_go(abm_match(pair = "random", size = 3),
                         abm_link(), abm_rules(x ~ x)),
               ticks = 1, seed = 1)
  # three teams of three, each a triangle
  expect_equal(nrow(abm_edges(r)), 9L)
})

# --- 27. Granovetter (1978) ------------------------------------------------

test_that("Granovetter's threshold crowd is decided by one person's threshold", {
  riot <- function(thresholds) {
    m <- abm_setup(
      agents  = abm_agents(n = length(thresholds), threshold = thresholds,
                           rioting = FALSE),
      globals = list(n_rioting = 0)
    )
    r <- abm_run(m, abm_go(abm_rules(rioting ~ n_rioting >= threshold),
                           abm_global(n_rioting ~ sum(rioting))),
                 ticks = 120, seed = 1)
    sum(r$rioting[r$tick == 120])
  }
  bumped <- 0:99; bumped[2] <- 2
  expect_equal(riot(0:99), 100L)   # everybody
  expect_equal(riot(bumped), 1L)   # one person
})

# --- 28. Deffuant et al. (2000) -------------------------------------------

test_that("Deffuant's bounded confidence gives floor(1/(2d)) opinion clusters", {
  deffuant <- function(d, n = 300, ticks = 300) {
    m <- abm_setup(agents = abm_agents(n = n, opinion = ~runif(n)),
                   globals = list(d = d, mu = 0.5), seed = 42)
    abm_run(m, abm_go(
      abm_match(pair = "random"),
      abm_rules(opinion ~ if_else(abs(opinion - partner_opinion) < d,
                                  opinion + mu * (partner_opinion - opinion),
                                  opinion))),
      ticks = ticks, seed = 1)
  }
  majors <- function(res, tol = 0.02, share = 0.05) {
    x <- sort(res$opinion[res$tick == max(res$tick)])
    g <- cumsum(c(TRUE, diff(x) > tol))
    sum(table(g) / length(x) >= share)
  }
  expect_equal(majors(deffuant(0.50)), 1L)
  expect_equal(majors(deffuant(0.25)), 2L)
  expect_equal(majors(deffuant(0.15)), 3L)
})

# --- 29. Hegselmann & Krause (2002) ---------------------------------------

test_that("Hegselmann-Krause reaches consensus above the confidence threshold", {
  hk <- function(eps, n = 200, ticks = 40) {
    m <- abm_setup(agents = abm_agents(n = n, opinion = ~runif(n)),
                   globals = list(eps = eps), seed = 7)
    abm_run(m, abm_go(abm_rules(
      opinion ~ vapply(opinion, function(x) mean(opinion[abs(opinion - x) <= eps]),
                       numeric(1)))), ticks = ticks)
  }
  clusters <- function(res) {
    x <- sort(res$opinion[res$tick == max(res$tick)])
    sum(diff(x) > 1e-6) + 1L
  }
  expect_equal(clusters(hk(0.30)), 1L)
  expect_gt(clusters(hk(0.10)), 1L)
})

# --- 31. Simple Birth Rates (Wilensky 1997) -------------------------------

test_that("a small fertility advantage drives the other population extinct", {
  birth_rates <- function(red_f, blue_f, K = 200, generations = 50) {
    m <- abm_setup(
      agents = abm_agents(
        n      = K,
        colour = ~rep(c("red", "blue"), length.out = n),
        fert   = ~if_else(rep(c("red", "blue"), length.out = n) == "red",
                          red_f, blue_f),
        parent = FALSE),
      globals = list(K = K), seed = 1)
    child <- abm_birth(when    = parent & runif(n()) < pmin(fert - born, 1),
                       cost    = born ~ born + 1,
                       inherit = list(parent ~ FALSE, born ~ 0))
    go <- do.call(abm_go, c(
      list(abm_rules(parent ~ TRUE, born ~ 0)),
      rep(list(child), ceiling(max(red_f, blue_f))),
      list(abm_death(when = rank(runif(n()), ties.method = "random") > K))))
    r <- abm_run(m, go, ticks = generations, seed = 1)
    tapply(r$colour, r$tick, function(x) mean(x == "red"))
  }
  advantage <- birth_rates(3.4, 3.0)
  expect_gt(advantage[["50"]], 0.95)                  # blues all but gone
  expect_gt(advantage[["50"]], advantage[["10"]])     # and monotonically so
  expect_lt(diff(range(birth_rates(3.0, 3.0))), 0.6)  # equal fertility: drift only
})

# --- 32. Team Assembly (Guimera et al. 2005) ------------------------------

test_that("more incumbents give a larger collaboration giant component", {
  giant <- function(p, q = 0.5, m = 5, ticks = 120) {
    pop <- abm_setup(
      agents  = abm_agents(n = m, collabs = 0, idle = 0, on_team = FALSE),
      network = abm_network(type = "empty"),
      globals = list(p = p, q = q, u_type = 0, u_q = 0), seed = 1)
    recruit <- list(
      abm_global(u_type ~ runif(1), u_q ~ runif(1)),
      abm_neighbours(near_team ~ any(on_team)),
      abm_rules(score ~ (!on_team) * case_when(
        u_type <  p & u_q < q & collabs > 0 & coalesce(near_team, FALSE) ~ 3,
        u_type <  p & collabs > 0                                       ~ 2,
        u_type >= p & collabs == 0                                      ~ 2,
        collabs == 0                                                    ~ 1,
        TRUE                                                            ~ 0)),
      abm_rules(score   ~ score + runif(n())),
      abm_rules(on_team ~ on_team |
                          (score > 1 & rank(-score, ties.method = "first") == 1)))
    go <- do.call(abm_go, c(
      list(abm_birth(n = m, inherit = list(collabs ~ 0, idle ~ 0, on_team ~ FALSE)),
           abm_rules(on_team ~ FALSE, idle ~ idle + 1)),
      rep(recruit, m),
      list(abm_match(pair = "random", size = m, eligible = on_team),
           abm_link(),
           abm_rules(collabs ~ collabs + 1, idle ~ 0),
           abm_death(when = collabs == 0 & idle > 30))))
    r <- abm_run(pop, go, ticks = ticks, seed = 1)
    ids <- unique(r$.id[r$tick == ticks & r$collabs > 0])
    e <- abm_edges(r)
    e <- e[e$from %in% ids & e$to %in% ids, ]
    g <- igraph::graph_from_data_frame(
      e, directed = FALSE, vertices = data.frame(name = as.character(ids)))
    max(igraph::components(g)$csize) / length(ids)
  }
  expect_gt(giant(0.9), giant(0.2))
})

# --- 33. Language Change (Troutman & Wilensky 2007) -----------------------

test_that("the threshold algorithm amplifies a majority, the reward one averages", {
  pa_network <- function(n) {
    seed_net <- abm_setup(
      agents  = abm_agents(n = 2, dummy = 0),
      network = abm_network(type = "manual", edges = data.frame(from = 1, to = 2)))
    grown <- abm_run(seed_net, abm_go(
      abm_birth(n = 1,
                attach_via = abm_match(pair = "network", from = "random_edge")),
      abm_rules(dummy ~ dummy)), ticks = n - 2, seed = 1)
    abm_edges(grown)
  }
  edges <- pa_network(150)
  language <- function(update, start, ticks = 60) {
    m <- abm_setup(
      agents  = abm_agents(n = 150, w = ~as.numeric(runif(n) < start)),
      network = abm_network(type = "manual", edges = edges),
      globals = list(alpha = 0.5, rate = 0.2), seed = 3)
    r <- abm_run(m, abm_go(
      abm_rules(utterance ~ as.numeric(runif(n()) < w)),
      abm_neighbours(heard ~ mean(utterance)),
      update), ticks = ticks, seed = 3)
    c(start = mean(r$w[r$tick == 0]), end = mean(r$w[r$tick == ticks]))
  }
  thresh <- abm_rules(w ~ if_else(is.na(heard), w, as.numeric(heard >= alpha)))
  reward <- abm_rules(w ~ if_else(is.na(heard), w, (1 - rate) * w + rate * heard))

  t8 <- language(thresh, 0.8)
  r8 <- language(reward, 0.8)
  expect_gt(t8[["end"]], t8[["start"]])   # threshold pushes the majority further
  expect_lt(r8[["end"]], r8[["start"]])   # reward pulls everyone to the middle
})

# --- 34. epiDEM Basic (Yang & Wilensky 2011) ------------------------------

test_that("the epidemic takes off only when the estimated R0 exceeds one", {
  epidem <- function(inf_chance, n = 200, i0 = 5, ticks = 150) {
    m <- abm_setup(
      agents = abm_agents(
        n = n,
        state    = ~if_else(seq_len(n) <= i0, "I", "S"),
        sick_for = 0,
        recover_after = ~pmax(1, rnorm(n, 30, 7.5))),
      globals = list(inf_p = inf_chance, rec_p = 0.5, N = n, S0 = n - i0,
                     S = n - i0, R0_hat = NA_real_), seed = 1)
    r <- abm_run(m, abm_go(
      abm_match(pair = "one_of"),
      abm_rules(state ~ if_else(state == "S" & partner_state == "I" &
                                runif(n()) < inf_p, "I", state)),
      abm_rules(sick_for ~ if_else(state == "I", sick_for + 1, sick_for)),
      abm_rules(state ~ if_else(state == "I" & sick_for > recover_after &
                                runif(n()) < rec_p, "R", state)),
      abm_global(S ~ sum(state == "S")),
      abm_global(R0_hat ~ if_else(S > 0 & S < S0,
                                  N * log(S0 / S) / (N - S), R0_hat))),
      ticks = ticks, seed = 1)
    c(attack = mean(r$state[r$tick == ticks] != "S"),
      R0 = utils::tail(abm_globals(r)$R0_hat, 1))
  }
  quiet <- epidem(0.02)
  loud  <- epidem(0.20)
  expect_lt(quiet[["R0"]], 1)
  expect_lt(quiet[["attack"]], 0.10)
  expect_gt(loud[["R0"]], 1)
  expect_gt(loud[["attack"]], 0.80)
})

# --- 35. Simple Genetic Algorithm (Wilensky 1998) -------------------------

test_that("the genetic algorithm finds all-ones, and loses it at high mutation", {
  bs <- paste0("b", 1:12)
  ga <- function(mutation, N = 60, generations = 60) {
    start <- stats::setNames(
      lapply(bs, function(nm)
        rlang::new_formula(NULL, quote(sample(0:1, n, replace = TRUE)))), bs)
    m <- abm_setup(agents  = do.call(abm_agents, c(list(n = N), start)),
                   globals = list(mut = mutation, xover = 0.7, L = length(bs)),
                   seed = 1)
    fit <- rlang::new_formula(
      rlang::sym("fitness"),
      Reduce(function(a, b) rlang::call2("+", a, b), lapply(bs, rlang::sym)))
    child <- lapply(bs, function(nm) rlang::new_formula(rlang::sym(nm), rlang::expr(
      if_else(!sexual, (!!rlang::sym(nm))[p1],
              if_else(!!which(bs == nm) <= cross,
                      (!!rlang::sym(nm))[p1], (!!rlang::sym(nm))[p2])))))
    mut_r <- lapply(bs, function(nm) rlang::new_formula(rlang::sym(nm), rlang::expr(
      if_else(runif(n()) < mut, 1L - (!!rlang::sym(nm)), (!!rlang::sym(nm))))))
    tournament <- function(out) list(
      abm_rules(t1 ~ sample(n(), n(), replace = TRUE),
                t2 ~ sample(n(), n(), replace = TRUE),
                t3 ~ sample(n(), n(), replace = TRUE)),
      abm_rules(rlang::new_formula(rlang::sym(out), rlang::expr(
        if_else(fitness[t1] >= fitness[t2] & fitness[t1] >= fitness[t3], t1,
                if_else(fitness[t2] >= fitness[t3], t2, t3))))))
    go <- do.call(abm_go, c(
      list(abm_rules(fit)), tournament("p1"), tournament("p2"),
      list(abm_rules(cross  ~ sample(L, n(), replace = TRUE),
                     sexual ~ runif(n()) < xover)),
      list(do.call(abm_rules, child)), list(do.call(abm_rules, mut_r)),
      list(abm_rules(fit))))
    r <- abm_run(m, go, ticks = generations, seed = 1)
    max(r$fitness[r$tick == generations])
  }
  expect_equal(ga(0.02), length(bs))
  expect_lt(ga(0.30), length(bs))
})

# --- 36. Information cascade (Bikhchandani et al. 1992) -------------------

test_that("cascade direction matches the analytic p^2 / (p^2 + (1-p)^2)", {
  side <- function(signal, nA, nB) {
    d <- nA - nB
    if_else(d >= 2, "A", if_else(d <= -2, "B", signal))
  }
  run_one <- function(p, seed, n = 40) {
    m <- abm_setup(
      agents  = abm_agents(n = n,
                           signal = ~if_else(runif(n) < p, "A", "B"),
                           decision = NA_character_),
      globals = list(nA = 0, nB = 0), seed = seed)
    r <- abm_run(m, abm_go(abm_sequential(
      decision ~ side(signal, nA, nB),
      nA ~ nA + (side(signal, nA, nB) == "A"),
      nB ~ nB + (side(signal, nA, nB) == "B"))), ticks = 1, seed = seed)
    mean(r$decision[r$tick == 1] == "A")
  }
  p <- 0.7
  runs <- vapply(1:120, function(s) run_one(p, s), numeric(1))
  expect_equal(mean(runs > 0.5), p^2 / (p^2 + (1 - p)^2), tolerance = 0.10)
})

# --- 30. Axelrod (1986): slow, so it is skipped on CRAN --------------------

test_that("the metanorm keeps vengefulness alive where the bare norm does not", {
  skip_on_cran()
  T_ <- 3; H_ <- -1; P_ <- -9; E_ <- -2
  mutate3 <- function(x, rate = 0.01) {
    for (b in 0:2) {
      flip <- runif(length(x)) < rate
      x <- bitwXor(x, as.integer(flip) * bitwShiftL(1L, b))
    }
    as.integer(x)
  }
  opportunity <- function(metanorms) {
    steps <- list(
      abm_rules(seen     ~ runif(n())),
      abm_rules(defected ~ boldness / 7 > seen),
      abm_global(n_def ~ sum(defected)),
      abm_rules(payoff ~ payoff + if_else(defected, T_, 0) +
                          H_ * (n_def - as.integer(defected))),
      abm_neighbours(witnessed ~ sum(defected & runif(n()) < seen)),
      abm_rules(witnessed   ~ coalesce(witnessed, 0L)),
      abm_rules(punish_acts ~ rbinom(n(), witnessed, vengefulness / 7)),
      abm_rules(shirked ~ witnessed - punish_acts,
                payoff  ~ payoff + E_ * punish_acts),
      abm_neighbours(punishers ~ sum(rbinom(n(), 1, own_seen) *
                                     rbinom(n(), 1, vengefulness / 7))),
      abm_rules(payoff ~ payoff +
                          P_ * coalesce(punishers, 0L) * as.integer(defected)))
    if (!metanorms) return(steps)
    c(steps, list(
      abm_neighbours(meta_hits ~ sum(rbinom(n(), 1, own_seen) *
                                     rbinom(n(), 1, vengefulness / 7))),
      abm_rules(payoff ~ payoff + P_ * coalesce(meta_hits, 0L) * shirked),
      abm_neighbours(meta_seen ~ sum(shirked * rbinom(n(), 1, seen))),
      abm_rules(meta_acts ~ rbinom(n(), coalesce(meta_seen, 0L),
                                   vengefulness / 7)),
      abm_rules(payoff   ~ payoff + E_ * meta_acts)))
  }
  evolution <- list(
    abm_global(mu_p ~ mean(payoff), sd_p ~ stats::sd(payoff)),
    abm_rules(offspring ~ case_when(payoff > mu_p + sd_p ~ 2,
                                    payoff < mu_p - sd_p ~ 0,
                                    TRUE                 ~ 1)),
    abm_rules(pick ~ sample(n(), n(), replace = TRUE, prob = offspring + 1e-9)),
    abm_rules(boldness ~ boldness[pick], vengefulness ~ vengefulness[pick]),
    abm_rules(boldness ~ mutate3(boldness), vengefulness ~ mutate3(vengefulness)),
    abm_rules(payoff ~ 0))
  axelrod <- function(metanorms, seed) {
    pop <- abm_setup(
      agents  = abm_agents(n = 20,
                           boldness     = ~sample(0:7, n, replace = TRUE),
                           vengefulness = ~sample(0:7, n, replace = TRUE),
                           payoff = 0),
      network = abm_network(type = "complete"),
      globals = list(n_def = 0, mu_p = 0, sd_p = 0), seed = seed)
    go <- do.call(abm_go, c(rep(opportunity(metanorms), 4), evolution))
    r <- abm_run(pop, go, ticks = 100, seed = seed)
    c(n = sum(r$tick == 100),
      v = mean(r$vengefulness[r$tick >= 95]))
  }
  norms <- vapply(1:3, function(s) axelrod(FALSE, s), numeric(2))
  meta  <- vapply(1:3, function(s) axelrod(TRUE,  s), numeric(2))
  expect_true(all(norms["n", ] == 20))          # the resample holds n fixed
  expect_gt(mean(meta["v", ]), mean(norms["v", ]))
})
