# Ten more models, chosen to stress parts of the grammar the first thirteen did
# not reach. Each test asserts the result the model is known for.

test_that("giant component appears as mean degree passes 1", {
  withr::local_seed(3001)
  N <- 400
  m <- abm_setup(agents = abm_agents(n = N), network = abm_network(type = "empty"))
  go <- abm_go(abm_match(pair = "random", eligible = runif(n()) < 0.05), abm_link())

  largest <- function(ticks) {
    r <- abm_run(m, go, ticks = ticks, seed = 1)
    e <- abm_edges(r)
    g <- igraph::graph_from_data_frame(
      e, directed = FALSE, vertices = data.frame(name = as.character(1:N)))
    c(degree = 2 * nrow(e) / N, share = max(igraph::components(g)$csize) / N)
  }
  sparse <- largest(20)    # mean degree around 1
  dense  <- largest(60)    # mean degree around 3

  expect_lt(sparse[["share"]], 0.5)
  expect_gt(dense[["share"]], 0.9)
})

test_that("rewiring a ring lattice makes it a small world", {
  skip_on_cran()
  withr::local_seed(3002)
  N <- 300; K <- 4
  ring <- do.call(rbind, lapply(1:(K / 2), function(d)
    data.frame(from = 1:N, to = ((1:N + d - 1) %% N) + 1)))
  m <- abm_setup(agents = abm_agents(n = N, rewiring = FALSE),
                 network = abm_network(type = "manual", edges = ring))
  go <- function(p) abm_go(
    abm_rules(rewiring ~ runif(n()) < p, .scope = "population"),
    abm_match(pair = "network", eligible = rewiring), abm_unlink(),
    abm_match(pair = "one_of",  eligible = rewiring), abm_link())

  stats <- function(edges) {
    g <- igraph::simplify(igraph::graph_from_data_frame(
      edges, directed = FALSE, vertices = data.frame(name = as.character(1:N))))
    c(cc = igraph::transitivity(g, type = "global"),
      apl = igraph::mean_distance(g, unconnected = TRUE), m = igraph::ecount(g))
  }
  before <- stats(ring)
  after  <- stats(abm_edges(abm_run(m, go(0.005), ticks = 20, seed = 2)))

  # the small-world signature: path length collapses, clustering barely moves
  expect_lt(after[["apl"]], before[["apl"]] / 3)
  expect_gt(after[["cc"]], 0.7 * before[["cc"]])
  # rewiring conserves edges, unlike deleting and adding independently
  expect_equal(after[["m"]], before[["m"]], tolerance = 0.02)
})

test_that("fireflies synchronise", {
  skip_on_cran()
  withr::local_seed(3003)
  CYCLE <- 10; FLASH <- 1
  m <- abm_setup(
    agents  = abm_agents(n = 600, clock = ~sample(0:(CYCLE - 1), n, replace = TRUE),
                         flashing = FALSE),
    network = abm_network(type = "random", degree = 6))
  r <- abm_run(m, abm_go(
    abm_neighbours(seen ~ sum(flashing)),
    abm_rules(clock ~ (clock + 1) %% CYCLE),
    abm_rules(clock ~ if_else(clock >= FLASH & coalesce(seen, 0) >= 1, FLASH, clock)),
    abm_rules(flashing ~ clock < FLASH)), ticks = 120, seed = 1)

  lit <- tapply(r$flashing, r$tick, sum)
  early <- lit[2:30]; late <- tail(lit, 40)
  expect_lt(max(early), 400)          # scattered phases to begin with
  expect_gt(max(late), 3 * max(early))# then almost everyone flashing at once
  expect_gt(max(late), 450)
  expect_equal(min(late), 0)          # and dark in between: a sawtooth, not a blur
})

test_that("an epidemic on a network has a threshold in the transmission rate", {
  skip_on_cran()
  withr::local_seed(3004)
  DURATION <- 8
  m <- abm_setup(
    agents  = abm_agents(n = 1000, state = ~c("infected", rep("susceptible", n - 1)),
                         timer = 0),
    network = abm_network(type = "random", degree = 6))
  attack <- function(beta, ticks = 100) {
    r <- abm_run(m, abm_go(
      abm_neighbours(inf_nbrs ~ sum(state == "infected")),
      abm_rules(state ~ if_else(
        state == "susceptible" & runif(n()) < 1 - (1 - beta)^coalesce(inf_nbrs, 0),
        "infected", state)),
      abm_rules(timer ~ if_else(state == "infected", timer + 1, timer)),
      abm_rules(state ~ if_else(state == "infected" & timer > DURATION,
                                "recovered", state))), ticks = ticks, seed = 2)
    mean(r$state[r$tick == ticks] != "susceptible")
  }
  expect_lt(attack(0.01), 0.05)   # R0 well below 1: it dies out
  expect_gt(attack(0.06), 0.7)    # R0 well above 1: it takes off
})

test_that("neutral drift fixes, and each allele wins with its starting frequency", {
  withr::local_seed(3005)
  go <- abm_go(abm_rules(allele ~ sample(allele, n(), replace = TRUE)))

  r <- abm_run(abm_setup(agents = abm_agents(
    n = 200, allele = ~sample(1:5, n, replace = TRUE))), go, ticks = 800, seed = 3)
  expect_equal(length(unique(r$allele[r$tick == 0])), 5)
  expect_equal(length(unique(r$allele[r$tick == 800])), 1)

  skip_on_cran()
  # theory: P(allele 1 fixes) = its starting frequency, here 0.3
  wins <- replicate(120, {
    rr <- abm_run(abm_setup(agents = abm_agents(
      n = 80, allele = ~c(rep(1, 24), rep(2, 56)))), go, ticks = 450)
    last <- rr$allele[rr$tick == 450]
    if (length(unique(last)) == 1) last[[1]] else NA
  })
  # essentially everything fixes; the tail of the fixation-time distribution is
  # long, so allow a couple of runs still segregating
  expect_lt(sum(is.na(wins)), 6)
  expect_equal(mean(wins == 1, na.rm = TRUE), 0.3, tolerance = 0.12)
})

test_that("hawks settle at the evolutionarily stable frequency V/C", {
  skip_on_cran()
  withr::local_seed(3006)
  V <- 50
  m <- abm_setup(agents = abm_agents(
    n = 2000, strategy = ~sample(c("hawk", "dove"), n, replace = TRUE), payoff = 0))
  go <- function(cc) abm_go(
    abm_match(pair = "random"),
    abm_rules(payoff ~ case_when(
      strategy == "hawk" & partner_strategy == "hawk" ~ (V - cc) / 2,
      strategy == "hawk" & partner_strategy == "dove" ~ V,
      strategy == "dove" & partner_strategy == "hawk" ~ 0,
      TRUE                                           ~ V / 2)),
    abm_rules(fitness ~ payoff - min((V - cc) / 2, 0) + 1),
    abm_rules(strategy ~ sample(strategy, n(), replace = TRUE, prob = fitness),
              .scope = "population"))

  observed <- function(cc) {
    r <- abm_run(m, go(cc), ticks = 200, seed = 4)
    mean(tapply(r$strategy == "hawk", r$tick, mean)[as.character(120:200)])
  }
  expect_equal(observed(100), 0.50, tolerance = 0.05)
  expect_equal(observed(200), 0.25, tolerance = 0.05)
  skip_on_cran()
  expect_equal(observed(500), 0.10, tolerance = 0.05)
  # when the resource is worth more than the fight, hawks take over
  expect_gt(observed(40), 0.95)
})

test_that("divide the cake settles on the fair split", {
  skip_on_cran()
  withr::local_seed(3007)
  K <- 1200
  m <- abm_setup(agents = abm_agents(
    n = 900, appetite = ~sample(c(2, 3, 4), n, replace = TRUE)))
  r <- abm_run(m, abm_go(
    abm_match(pair = "random"),
    abm_death(when = appetite + partner_appetite > 6),
    abm_birth(when = runif(n()) < appetite / 6),
    abm_death(when = runif(n()) < pmax(0, (n() - K) / n()))),
    ticks = 150, seed = 1)

  start <- table(r$appetite[r$tick == 0]) / sum(r$tick == 0)
  end   <- table(factor(r$appetite[r$tick == 150], levels = c(2, 3, 4)))
  end   <- end / sum(end)
  expect_true(all(abs(start - 1/3) < 0.05))   # started at random
  expect_gt(end[["3"]], 0.95)                 # fair division wins
  expect_lt(end[["4"]], 0.01)                 # greedy is gone
})

test_that("the sex ratio converges on a half from either side", {
  skip_on_cran()
  withr::local_seed(3008)
  LONGEVITY <- 8; MATURITY <- 2; MATING <- 0.6; CAP <- 1500
  pop <- function(p0) abm_setup(agents = abm_agents(
    n = 1000,
    sex = ~sample(c("male", "female"), n, replace = TRUE, prob = c(p0, 1 - p0)),
    mcc = ~pmin(0.95, pmax(0.05, rnorm(n, p0, 0.08))),
    age = ~sample(0:LONGEVITY, n, replace = TRUE)))
  go <- abm_go(
    abm_rules(age ~ age + 1),
    abm_death(when = age > LONGEVITY),
    abm_death(when = runif(n()) < pmax(0, (n() - CAP) / n())),
    abm_match(pair = "opposite_group", by = sex, eligible = age >= MATURITY),
    abm_birth(when = sex == "female" & !is.na(.partner) & runif(n()) < MATING,
              inherit = list(
                age ~ 0,
                mcc ~ pmin(0.95, pmax(0.05, (mcc + partner_mcc) / 2 + rnorm(n(), 0, 0.05))),
                sex ~ if_else(runif(n()) < (mcc + partner_mcc) / 2, "male", "female"))))

  for (p0 in c(0.25, 0.75)) {
    r <- abm_run(pop(p0), go, ticks = 1200, seed = 2)
    share <- tapply(r$sex == "male", r$tick, mean)
    expect_equal(share[["0"]], p0, tolerance = 0.06)
    expect_equal(share[["1200"]], 0.5, tolerance = 0.06)
    # the heritable trait, not just the head count, has moved
    expect_equal(mean(r$mcc[r$tick == 1200]), 0.5, tolerance = 0.06)
  }
})

test_that("axelrod leaves more cultures the more traits there are", {
  skip_on_cran()
  withr::local_seed(3009)
  culture <- function(q) {
    cols <- stats::setNames(lapply(1:5, function(i)
      rlang::new_formula(NULL, rlang::expr(sample(1:!!q, n, replace = TRUE)))),
      paste0("f", 1:5))
    m <- abm_setup(agents = do.call(abm_agents, c(list(n = 400), cols)),
                   network = abm_network(type = "random", degree = 4))
    r <- abm_run(m, abm_go(
      abm_match(pair = "network"),
      abm_rules(similarity ~ ((f1 == partner_f1) + (f2 == partner_f2) +
                              (f3 == partner_f3) + (f4 == partner_f4) +
                              (f5 == partner_f5)) / 5),
      abm_rules(interact ~ similarity < 1 & runif(n()) < similarity),
      abm_rules(k1 ~ if_else(f1 != partner_f1, runif(n()), -1),
                k2 ~ if_else(f2 != partner_f2, runif(n()), -1),
                k3 ~ if_else(f3 != partner_f3, runif(n()), -1),
                k4 ~ if_else(f4 != partner_f4, runif(n()), -1),
                k5 ~ if_else(f5 != partner_f5, runif(n()), -1)),
      abm_rules(best ~ pmax(k1, k2, k3, k4, k5)),
      abm_rules(f1 ~ if_else(interact & k1 == best, partner_f1, f1),
                f2 ~ if_else(interact & k2 == best, partner_f2, f2),
                f3 ~ if_else(interact & k3 == best, partner_f3, f3),
                f4 ~ if_else(interact & k4 == best, partner_f4, f4),
                f5 ~ if_else(interact & k5 == best, partner_f5, f5))),
      ticks = 400, seed = 3)
    last <- r[r$tick == 400, ]
    nrow(dplyr::distinct(last[, paste0("f", 1:5)]))
  }
  few <- culture(2); many <- culture(10)
  expect_lt(few, 100)          # few traits: culture collapses to a handful
  expect_gt(many, 2 * few)     # many traits: it stays fragmented
})

test_that("iterated PD with per-opponent memory shows tit-for-tat overtaking defect", {
  skip_on_cran()
  withr::local_seed(3010)
  f <- function(lhs, rhs) rlang::new_formula(str2lang(lhs), str2lang(rhs))

  play <- function(counts, ticks) {
    strategies <- rep(names(counts), counts)
    N <- length(strategies)
    hist <- stats::setNames(rep(list(FALSE), N), paste0("h", 1:N))
    m <- abm_setup(agents = do.call(abm_agents, c(
      list(n = N, strategy = strategies, score = 0, games = 0, defect_now = FALSE),
      hist)))
    recall <- f("remembered", paste0(
      "case_when(", paste(sprintf(".partner == %d ~ h%d", 1:N, 1:N), collapse = ", "),
      ", TRUE ~ FALSE)"))
    update <- lapply(1:N, function(k) f(paste0("h", k), sprintf(
      "case_when(.partner != %d ~ h%d,
                 strategy == 'unforgiving' ~ h%d | partner_defect_now,
                 strategy == 'tit-for-tat' ~ partner_defect_now,
                 TRUE ~ h%d)", k, k, k, k)))
    r <- abm_run(m, abm_go(
      abm_match(pair = "random"),
      abm_rules(recall),
      abm_rules(defect_now ~ case_when(
        strategy == "defect"    ~ TRUE,
        strategy == "cooperate" ~ FALSE,
        strategy == "random"    ~ runif(n()) < 0.5,
        TRUE                    ~ remembered)),
      abm_rules(payoff ~ case_when(
        !defect_now & !partner_defect_now ~ 3,
        !defect_now &  partner_defect_now ~ 0,
         defect_now & !partner_defect_now ~ 5,
        TRUE                              ~ 1)),
      abm_rules(score ~ score + payoff, games ~ games + 1),
      do.call(abm_rules, update)), ticks = ticks, seed = 5)
    r$avg <- r$score / pmax(1, r$games)
    r
  }

  # defect strictly beats unconditional cooperation
  cd <- play(c(cooperate = 12, defect = 12), 200)
  last <- cd[cd$tick == 200, ]
  expect_gt(mean(last$avg[last$strategy == "defect"]),
            mean(last$avg[last$strategy == "cooperate"]))

  # tit-for-tat never defects first, so against cooperators it earns exactly 3
  tc <- play(c(`tit-for-tat` = 12, cooperate = 12), 100)
  expect_true(all(abs(tc$avg[tc$tick == 100] - 3) < 1e-8))

  # the signature curve: defect is ahead early, tit-for-tat ahead once it learns
  td <- play(c(`tit-for-tat` = 12, defect = 12), 400)
  avg_at <- function(t, s) mean(td$avg[td$tick == t & td$strategy == s])
  expect_gt(avg_at(5, "defect"), avg_at(5, "tit-for-tat"))
  expect_gt(avg_at(400, "tit-for-tat"), avg_at(400, "defect"))
})
