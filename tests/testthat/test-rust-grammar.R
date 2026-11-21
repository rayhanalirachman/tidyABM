# Full-Rust FR3 grammar (R/rust-expr.R, tidyabm-core/src/expr.rs): every
# construct the native rules take, over values that tell implementations apart
# (NA vs NaN, -0, Inf, integer extremes), run under "r" and "rust" and compared
# bit for bit, with the warnings and `.Random.seed` after. A failure names the
# platform and the first differing element.

gram_model <- function(n = 60) {
  abm_setup(agents = abm_agents(
    n = n,
    x = ~c(NA, NaN, -0, 0, Inf, -Inf, 1e308, runif(n - 7)),
    k = ~c(NA, .Machine$integer.max, -.Machine$integer.max, sample(1:5, n - 3, TRUE)),
    p = ~sample(1:5, n, TRUE),
    s = ~sample(c("a", "b", NA), n, TRUE),
    y = ~ifelse(runif(n) < 0.2, NA, runif(n)),
    b = ~sample(c(TRUE, FALSE, NA), n, TRUE)),
    globals = list(g = 2, t = 0), seed = 5)
}

# every double column bit for bit, then the rest with identical()
expect_bits_agree <- function(got, ref, what) {
  for (nm in names(ref)) {
    if (is.double(ref[[nm]])) {
      expect_same_as_r(got[[nm]], ref[[nm]], paste(what, nm))
    }
  }
  expect(identical(got, ref), paste0(what, ": results differ on ", rust_platform(), "; ",
                                     diff_where(ref, got) %||% "?"))
}

run_capture <- function(engine, ...) {
  w <- character()
  set.seed(11)
  r <- withCallingHandlers(run_in(engine, ...), warning = function(c) {
    w <<- c(w, conditionMessage(c))
    invokeRestart("muffleWarning")
  })
  list(r = r, w = w, seed = rng_get(), stats = .rl_last$stats)
}

expect_native_agree <- function(m, go, what, ticks = 4, native = TRUE) {
  .rl_last$stats <- NULL
  ref <- run_capture("r", m, go, ticks = ticks, seed = NULL)
  got <- run_capture("rust", m, go, ticks = ticks, seed = NULL)
  expect_bits_agree(got$r, ref$r, what)
  expect_identical(got$w, ref$w, label = paste(what, "warnings"))
  expect_identical(got$seed, ref$seed, label = paste(what, ".Random.seed after"))
  if (native) {
    expect(!is.null(got$stats) && got$stats[[1]] > 0,
           paste(what, "did not run natively:", paste(got$stats, collapse = " ")))
  }
  withr::local_options(tidyABM.engine = "both")
  set.seed(11)
  suppressWarnings(abm_run(m, go, ticks = ticks, seed = NULL))
  invisible(got)
}

test_that("each FR3 construct is native and bit-identical to R", {
  m <- gram_model()
  mu <- 0.25
  cases <- list(
    pow      = abm_rules(a ~ x^2, a2 ~ k^2L, a3 ~ (x - 1)^0.5, a4 ~ 2^k),
    abs_sign = abm_rules(a ~ abs(x) + sign(x), a2 ~ abs(k), a3 ~ floor(x * 3) - ceiling(x)),
    round    = abm_rules(a ~ round(x * 10, 1), a2 ~ trunc(y * 100) %% 7, a3 ~ k %/% 3L),
    cum      = abm_rules(a ~ cumsum(p), a2 ~ cumsum(y), a3 ~ cummax(p) + cumprod(p > 0)),
    minmax   = abm_rules(a ~ max(p) + min(y), a2 ~ pmax(p, 3L) - min(x)),
    asint    = abm_rules(a ~ as.integer(y * 10), a2 ~ as.numeric(p) / 2),
    coalesce = abm_rules(a ~ coalesce(y, 0), a2 ~ coalesce(k, 0L), a3 ~ coalesce(s, "z"),
                         a4 ~ coalesce(x, y, -1)),
    in_set   = abm_rules(a ~ s %in% c("a", NA), a2 ~ p %in% c(1L, 3L), a3 ~ p %in% c(2, 4)),
    narm     = abm_rules(a ~ sum(y, na.rm = TRUE), a2 ~ mean(y, na.rm = TRUE),
                         a3 ~ sum(k, na.rm = TRUE) + 0L, a4 ~ mean(b, na.rm = TRUE)),
    draws    = abm_rules(a ~ runif(n(), -mu, mu), a2 ~ rnorm(n(), g, 2), a3 ~ rnorm(n()),
                         a4 ~ runif(n(), max = 3)),
    sample   = abm_rules(a ~ sample(p, n(), replace = TRUE), a2 ~ sample(s, n(), TRUE),
                         a3 ~ sample(5, n(), replace = TRUE), a4 ~ sample(n(), n(), TRUE)),
    index    = abm_rules(a ~ y[p], a2 ~ s[p] , a3 ~ x[sample(n(), n(), TRUE)]),
    scope    = abm_rules(a ~ y * mu + g)
  )
  for (nm in names(cases)) {
    expect_native_agree(m, abm_go(cases[[nm]]), nm)
  }
  # each of these declines on some tick (a warning, NaN, an index of 0): R's
  # result, conditions and random stream come out all the same
  declines <- list(
    overflow = abm_rules(a ~ cumsum(k)),
    coerce   = abm_rules(a ~ as.integer(x * 1e10)),
    nanrand  = abm_rules(a ~ runif(n(), 1, 0)),
    past_end = abm_rules(a ~ y[p + 58L]),
    empty    = abm_rules(a ~ max(y[p > 9])),
    in_na    = abm_rules(a ~ y %in% c(1, 2))
  )
  for (nm in names(declines)) {
    expect_native_agree(m, abm_go(abm_rules(z ~ runif(n())), declines[[nm]]), nm,
                        native = FALSE)
  }
  # an index of 0 drops elements: declined, and R raises its own error
  go <- abm_go(abm_rules(a ~ y[p - 1L]))
  err <- function(e) conditionMessage(tryCatch(run_in(e, m, go, ticks = 2, seed = 1),
                                               error = identity))
  expect_identical(err("rust"), err("r"))
})

test_that("formula-environment values are read every step, as R reads them", {
  m <- gram_model(20)
  step <- local({
    rate <- 1
    list(go = abm_go(abm_rules(a ~ p * rate), abm_global(t ~ t + rate)),
         set = function(v) rate <<- v)
  })
  got <- expect_native_agree(m, step$go, "scope")
  step$set(2.5)
  expect_native_agree(m, step$go, "scope changed")
  # a name the engine's own scope binds (`n`) is left to R
  n <- 3
  expect_native_agree(m, abm_go(abm_rules(a ~ p + 1), abm_rules(a2 ~ n())), "n()")
})

test_that("rules under a standing match run natively for the placed agents", {
  m <- abm_setup(agents = abm_agents(n = 31, x = ~runif(n), k = ~sample(1:3, n, TRUE)), seed = 2)
  go <- abm_go(abm_match(pair = "random"),
               abm_rules(x ~ x + 1, v ~ k * 2.5, k ~ k + 1L, w ~ x > 0.5))
  got <- expect_native_agree(m, go, "match", ticks = 5)
  # nobody placed in the tick: every agent written
  none <- abm_go(abm_match(pair = "random", eligible = .id < 0), abm_rules(x ~ x + 1))
  expect_native_agree(m, none, "empty match", ticks = 3)
  # a rule that is not elementwise under a standing match goes to R
  agg <- abm_go(abm_match(pair = "random"), abm_rules(x ~ x + mean(x)))
  expect_native_agree(m, agg, "aggregate under match", ticks = 3, native = FALSE)
})

test_that("integer and logical neighbour folds are native and exact", {
  m <- abm_setup(agents = abm_agents(n = 40, on = ~runif(n) < 0.3, c = ~sample(0:4, n, TRUE)),
                 network = abm_network(type = "poisson", degree = 2), seed = 3)
  go <- abm_go(abm_neighbours(share ~ mean(on), tot ~ sum(c), cnt ~ sum(on)),
               abm_rules(on ~ on | coalesce(share, 0) >= 0.2))
  expect_native_agree(m, go, "integer folds", ticks = 6)
})

test_that("a step mixing native rules with rules R evaluates agrees, effects once", {
  m <- abm_setup(agents = list(a = abm_agents(n = 25, x = ~runif(n), s = ~sample(c("u", "v"), n, TRUE)),
                               b = abm_agents(n = 10, x = ~runif(n))), seed = 4)
  calls <- new.env()
  calls$n <- 0L
  pick <- function(v) {
    calls$n <- calls$n + 1L
    v[sample.int(length(v), length(v), replace = TRUE)]
  }
  go <- abm_go(abm_rules(a ~ x + runif(n()),
                         b ~ pick(x),                                  # R, draws, effect
                         c ~ sapply(x, function(v) v * 2),             # R, plain
                         d ~ vapply(seq_along(x), function(i) i * 1.5, 0), # R
                         e ~ x * 2 + runif(n())))
  calls$n <- 0L
  ref <- run_capture("r", m, go, ticks = 4, seed = NULL)
  n_r <- calls$n
  calls$n <- 0L
  got <- run_capture("rust", m, go, ticks = 4, seed = NULL)
  expect_bits_agree(got$r, ref$r, "mixed")
  expect_identical(got$seed, ref$seed)
  expect_identical(calls$n, n_r)
  expect_gt(got$stats[[1]], 0)
  # a native rule declining after an R rule ran: the rest goes to R, effects once
  bad <- abm_go(abm_rules(b ~ pick(x), a ~ runif(n(), 1, 0), e ~ x + runif(n())))
  calls$n <- 0L
  ref <- run_capture("r", m, bad, ticks = 3, seed = NULL)
  n_r <- calls$n
  calls$n <- 0L
  got <- run_capture("rust", m, bad, ticks = 3, seed = NULL)
  expect_bits_agree(got$r, ref$r, "mixed decline")
  expect_identical(got$w, ref$w)
  expect_identical(got$seed, ref$seed)
  expect_identical(calls$n, n_r)
  # an R rule erring: R's error, as the R engine raises it
  boom <- abm_go(abm_rules(a ~ x + 1, b ~ stop("boom here")))
  err <- function(e) conditionMessage(tryCatch(run_in(e, m, boom, ticks = 2, seed = 1),
                                               error = identity))
  expect_identical(err("rust"), err("r"))
  withr::local_options(tidyABM.engine = "both")
  expect_no_error(suppressWarnings(abm_run(m, go, ticks = 2, seed = 1)))
})

test_that("`.by` rules run natively in group_by() order, later rules reading earlier", {
  m <- abm_setup(agents = list(
    a = abm_agents(n = 30, x = ~runif(n), k = ~sample(c(1L, 2L, NA, 5L), n, TRUE),
                   d = ~sample(c(-0, 0, 2.5, -1), n, TRUE), b = ~sample(c(TRUE, FALSE, NA), n, TRUE)),
    z = abm_agents(n = 12, x = ~runif(n), k = ~sample(1:3, n, TRUE), d = 1,
                   b = ~sample(c(TRUE, FALSE), n, TRUE))), seed = 6)
  cases <- list(
    int_key = abm_go(abm_rules(x ~ x - mean(x) + runif(n()), w ~ n(), .by = k)),
    seq     = abm_go(abm_rules(x ~ x / sum(x), y ~ x * 2 + max(x), .by = k)),
    rekey   = abm_go(abm_rules(k ~ k %% 2L + 1L, x ~ sum(x) + runif(n()), .by = k)),
    dbl_key = abm_go(abm_rules(x ~ mean(x) + n(), .by = d)),
    lgl_key = abm_go(abm_rules(v ~ sum(x > 0.5), x ~ x + v, .by = b)),
    types   = abm_go(abm_rules(k ~ if_else(x > 0.5, 1.5, 2), .by = b))
  )
  for (nm in names(cases)) expect_native_agree(m, cases[[nm]], paste(".by", nm))
  # a string key goes to R
  s <- abm_setup(agents = abm_agents(n = 20, x = ~runif(n), g = ~sample(c("p", "q"), n, TRUE)))
  expect_native_agree(s, abm_go(abm_rules(x ~ mean(x), .by = g)), ".by string", native = FALSE)
})

test_that("FR4b: network folds of compiled arguments and `within` lookups are native", {
  m <- abm_setup(agents = abm_agents(n = 40, state = ~sample(c("S", "I", "R"), n, TRUE),
                                     x = ~runif(n), k = ~sample(0:3, n, TRUE)),
                 network = abm_network(type = "poisson", degree = 3),
                 globals = list(cut = 0.4), seed = 7)
  lim <- 2L
  go <- abm_go(abm_neighbours(inf ~ sum(state == "I"), hi ~ mean(x > cut),
                              big ~ sum(k * lim + x), one ~ mean(cut)),
               abm_rules(state ~ if_else(inf > 0 & runif(n()) < 0.3, "I", state)))
  expect_native_agree(m, go, "network fold of expressions", ticks = 5)
  # within = .id == own_.cell (Wolf-Sheep's lookup), over groups some of which
  # lack the columns, with a rest condition, any/all/sum/n
  w <- abm_setup(agents = list(
    patches = abm_agents(grass = ~runif(n) < 0.5, food = ~runif(n), cnt = ~sample(0:2, n, TRUE)),
    sheep = abm_agents(n = 15, energy = ~runif(n, 1, 5)),
    wolves = abm_agents(n = 5, energy = 2)),
    network = abm_network(type = "grid", dims = c(6, 6), on = "patches"), seed = 4)
  go2 <- abm_go(
    abm_move(along = "patches", to = "random_neighbour", who = c("sheep", "wolves")),
    abm_neighbours(here ~ any(grass), here_all ~ all(!grass), f ~ sum(food), c ~ sum(cnt),
                   g ~ sum(grass), nn ~ n(),
                   within = .group == "patches" & .id == own_.cell),
    abm_rules(energy ~ energy + if_else(coalesce(here, FALSE), 1, -1)))
  expect_native_agree(w, go2, "within lookup", ticks = 5)
})

test_that("FR4b: `.where` lattice neighbours are native and exact", {
  ca <- abm_setup(agents = abm_agents(s = ~as.integer(seq_len(n) %% 3 == 0), x = ~runif(n)),
                  network = abm_network(type = "line", dims = 31, torus = FALSE),
                  globals = list(rule = as.integer(intToBits(90L))[1:8]))
  go <- abm_go(abm_neighbours(s_l ~ sum(s), m_l ~ mean(x), b_l ~ any(s > 0), .where = "west"),
               abm_neighbours(s_r ~ sum(s), d_r ~ sum(x - own_x), a_r ~ all(s == own_s),
                              c_r ~ n(), mi_r ~ mean(s), .where = "east"),
               abm_rules(s ~ rule[4 * coalesce(s_l, 0L) + 2 * s + coalesce(s_r, 0L) + 1]))
  expect_native_agree(ca, go, ".where line", ticks = 6)
  g <- abm_setup(agents = list(patches = abm_agents(food = ~runif(n)),
                               ants = abm_agents(n = 12, carry = 0)),
                 network = abm_network(type = "grid", dims = c(5, 5), on = "patches"), seed = 2)
  go2 <- abm_go(abm_move(along = "patches", to = "random_neighbour", who = "ants"),
                abm_neighbours(north_food ~ sum(food), .where = "north"))
  expect_native_agree(g, go2, ".where grid with movers", ticks = 5)
})

test_that("FR5: abm_match() random, one_of and opposite_group are native, same draws", {
  m <- abm_setup(agents = list(
    a = abm_agents(n = 23, x = ~runif(n), s = ~sample(c("left", "right"), n, TRUE),
                   cell = ~sample(c(1L, 2L, 3L, NA), n, TRUE)),
    b = abm_agents(n = 9, x = ~runif(n), s = "left", cell = ~sample(1:3, n, TRUE))), seed = 3)
  lim <- 0.2
  cases <- list(
    random   = abm_match(pair = "random"),
    random4  = abm_match(pair = "random", size = 4, eligible = x > lim),
    one_of   = abm_match(pair = "one_of", eligible = x > 0.5),
    one_of_a = abm_match(pair = "one_of", among = s == "left"),
    one_of_1 = abm_match(pair = "one_of", among = x > 0.99),
    opp      = abm_match(pair = "opposite_group", by = s),
    opp_by   = abm_match(pair = "opposite_group", by = s, .by = cell, eligible = x > 0.1),
    opp_grp  = abm_match(pair = "opposite_group", by = .group)
  )
  for (nm in names(cases)) {
    go <- abm_go(cases[[nm]], abm_rules(x ~ x * 0.99, .scope = "population"))
    got <- expect_native_agree(m, go, paste("match", nm), ticks = 4)
    expect(got$stats[[1]] == 8L && got$stats[[4]] == 0L,
           paste("match", nm, "not fully native:", paste(got$stats, collapse = " ")))
  }
  # a side with three values errors in R: declined, R's error
  three <- abm_setup(agents = abm_agents(n = 9, k = ~rep(1:3, 3)))
  go <- abm_go(abm_match(pair = "opposite_group", by = k), abm_rules(k ~ k + 0L))
  err <- function(e) conditionMessage(tryCatch(run_in(e, three, go, ticks = 1, seed = 1),
                                               error = identity))
  expect_identical(err("rust"), err("r"))
})

test_that("FR5: rules reading the standing match (partner_<col>, .partner, ...) are native", {
  m <- abm_setup(agents = list(
    a = abm_agents(n = 21, x = ~runif(n), st = ~sample(c("C", "D"), n, TRUE), pay = 0),
    b = abm_agents(n = 6, x = ~runif(n), pay = 1)), seed = 5)
  cases <- list(
    pd = abm_go(abm_match(pair = "random"),
                abm_rules(pay ~ pay + if_else(coalesce(partner_st, "C") == "C", 3, 0),
                          x ~ if_else(abs(x - partner_x) > 0.05, (x + partner_x) / 2, x))),
    ids = abm_go(abm_match(pair = "one_of", eligible = x > 0.3),
                 abm_rules(pay ~ pay + coalesce(.partner, 0L) / 100, w ~ is.na(.partner))),
    pop = abm_go(abm_match(pair = "random"),
                 abm_rules(x ~ x + coalesce(partner_x, 0) / 10, .scope = "population")),
    grp = abm_go(abm_match(pair = "random", size = 3),
                 abm_rules(g ~ coalesce(.group_id, 0L), r ~ is.na(.role))),
    none = abm_go(abm_match(pair = "random", eligible = x < 0),
                  abm_rules(y ~ coalesce(partner_x, -1)))
  )
  for (nm in names(cases)) {
    got <- expect_native_agree(m, cases[[nm]], paste("match rules", nm), ticks = 4)
    expect(got$stats[[2]] == 0L && got$stats[[4]] == 0L,
           paste("match rules", nm, "not fully native:", paste(got$stats, collapse = " ")))
  }
})

test_that("FR5: roles and network matching are native, same draws", {
  m <- abm_setup(agents = abm_agents(n = 25, money = ~sample(0:3, n, TRUE), x = ~runif(n),
                                     s = ~sample(c("p", "q"), n, TRUE)),
                 network = abm_network(type = "poisson", degree = 2), seed = 9)
  cases <- list(
    giver = abm_match(pair = "random", role = list(giver = money > 0, receiver = TRUE)),
    coin  = abm_match(pair = "random", role = list(speaker = TRUE, hearer = TRUE)),
    one   = abm_match(pair = "one_of", role = list(a = x > 0.5, b = TRUE)),
    opp   = abm_match(pair = "opposite_group", by = s, role = list(u = money > 1, v = money < 2)),
    net   = abm_match(pair = "network"),
    net_e = abm_match(pair = "network", eligible = runif(n()) < 0.4)
  )
  for (nm in names(cases)) {
    go <- abm_go(cases[[nm]], abm_rules(money ~ money + if_else(coalesce(.role, "") == "giver", -1L, 0L),
                                        x ~ coalesce(partner_x, x)))
    got <- expect_native_agree(m, go, paste("match", nm), ticks = 4)
    expect(got$stats[[2]] == 0L && got$stats[[4]] == 0L,
           paste("match", nm, "not fully native:", paste(got$stats, collapse = " ")))
  }
})

test_that("FR5: grouped rules under a standing match run per pair or group, natively", {
  m <- abm_setup(agents = list(a = abm_agents(n = 22, c = ~sample(0:1, n, TRUE), pay = 0, x = ~runif(n)),
                               b = abm_agents(n = 7, c = 1L, pay = 0, x = ~runif(n))), seed = 2)
  cases <- list(
    pg = abm_go(abm_match(pair = "random", size = 4),
                abm_rules(pay ~ pay + 1.6 * sum(c) / n() - c, k ~ n())),
    draw = abm_go(abm_match(pair = "random"),
                  abm_rules(x ~ x + runif(n()), y ~ mean(x) + partner_x)),
    opp = abm_go(abm_match(pair = "opposite_group", by = c),
                 abm_rules(pay ~ pay + sum(x) - x, z ~ max(x)))
  )
  for (nm in names(cases)) {
    got <- expect_native_agree(m, cases[[nm]], paste("grouped", nm), ticks = 4)
    expect(got$stats[[2]] == 0L && got$stats[[4]] == 0L,
           paste("grouped", nm, "not fully native:", paste(got$stats, collapse = " ")))
  }
})

test_that("FR6: abm_death() is native, also under a match and on a network", {
  m <- abm_setup(agents = list(a = abm_agents(n = 30, e = ~runif(n), s = ~sample(c("u", "v"), n, TRUE),
                                              l = ~lapply(seq_len(n), function(i) i)),
                               b = abm_agents(n = 8, e = ~runif(n))), seed = 4)
  cases <- list(
    plain = abm_go(abm_rules(e ~ e - 0.1), abm_death(when = e < 0.2)),
    draws = abm_go(abm_death(when = runif(n()) < 0.1), abm_rules(e ~ e * 1.01)),
    group = abm_go(abm_death(when = s == "u" & e < 0.5), abm_rules(e ~ e + 0.01)),
    match = abm_go(abm_match(pair = "random"), abm_death(when = !is.na(.partner) & partner_e > 0.9),
                   abm_rules(e ~ e * 0.99))
  )
  for (nm in names(cases)) {
    got <- expect_native_agree(m, cases[[nm]], paste("death", nm), ticks = 5)
    expect(got$stats[[2]] == 0L && got$stats[[4]] == 0L,
           paste("death", nm, "not fully native:", paste(got$stats, collapse = " ")))
  }
  # on a network: dead agents born after the wiring are past every endpoint
  net <- abm_setup(agents = abm_agents(n = 30, e = ~runif(n)),
                   network = abm_network(type = "ring", degree = 2), seed = 1)
  expect_native_agree(net, abm_go(abm_death(when = e < 0.3), abm_rules(e ~ e + 0.05)),
                      "death with edges", ticks = 3, native = FALSE)
})

test_that("FR6: abm_birth(when =) with times, cost and inherit is native", {
  m <- abm_setup(agents = list(a = abm_agents(n = 20, e = ~runif(n, 1, 5), k = ~sample(0:2, n, TRUE),
                                              s = ~sample(c("u", "v"), n, TRUE)),
                               b = abm_agents(n = 6, e = ~runif(n, 1, 5), k = 1L, s = "u")), seed = 6)
  cases <- list(
    plain  = abm_go(abm_birth(when = e > 4), abm_rules(e ~ e - 0.5)),
    cost   = abm_go(abm_birth(when = .group == "b" & runif(n()) < 0.3, cost = e ~ e / 2),
                    abm_rules(e ~ e + 0.3)),
    times  = abm_go(abm_birth(when = e > 3.5, times = k + 0.5, cost = e ~ e - 1,
                              inherit = list(e ~ e / 2 + runif(n()), s ~ if_else(runif(n()) < 0.5, "u", s))),
                    abm_rules(e ~ e + 0.2), abm_death(when = e < 1)),
    match  = abm_go(abm_match(pair = "random"),
                    abm_birth(when = !is.na(.partner) & partner_e > 4, cost = e ~ e * 0.9),
                    abm_rules(e ~ e + 0.1))
  )
  for (nm in names(cases)) {
    got <- expect_native_agree(m, cases[[nm]], paste("birth", nm), ticks = 4)
    expect(got$stats[[2]] == 0L && got$stats[[4]] == 0L,
           paste("birth", nm, "not fully native:", paste(got$stats, collapse = " ")))
  }
})

test_that("FR7: abm_move() to a random (empty) neighbour is native, same draws", {
  mk <- function(torus) abm_setup(agents = list(
    patches = abm_agents(food = ~runif(n)),
    sheep = abm_agents(n = 14, e = ~runif(n)), wolves = abm_agents(n = 4, e = 1)),
    network = abm_network(type = "grid", dims = c(6, 5), on = "patches", torus = torus,
                          diagonals = torus), seed = 3)
  cases <- list(
    torus  = list(mk(TRUE), abm_move(along = "patches", to = "random_neighbour", who = c("sheep", "wolves"))),
    edge   = list(mk(FALSE), abm_move(along = "patches", to = "random_neighbour")),
    empty  = list(mk(FALSE), abm_move(along = "patches", to = "random_empty_neighbour", who = "sheep")),
    avoid  = list(mk(TRUE), abm_move(along = "patches", avoid_occupied = TRUE)),
    net    = list(mk(TRUE), abm_move(along = "network", who = "wolves"))
  )
  for (nm in names(cases)) {
    go <- abm_go(cases[[nm]][[2]], abm_rules(e ~ e * 0.99))
    got <- expect_native_agree(cases[[nm]][[1]], go, paste("move", nm), ticks = 5)
    expect(got$stats[[2]] == 0L && got$stats[[4]] == 0L,
           paste("move", nm, "not fully native:", paste(got$stats, collapse = " ")))
  }
})

test_that("FR7: abm_tell(to = <id>) is native for every resolve it takes", {
  w <- abm_setup(agents = list(
    patches = abm_agents(grass = ~runif(n) < 0.6, countdown = ~sample(0:5, n, TRUE), heat = 0),
    sheep = abm_agents(n = 25, e = ~runif(n), k = ~sample(1:3, n, TRUE))),
    network = abm_network(type = "grid", dims = c(4, 4), on = "patches", torus = TRUE), seed = 2)
  regrow <- 7L
  cases <- list(
    last  = abm_tell(grass ~ FALSE, countdown ~ regrow, to = .cell, when = .group == "sheep"),
    first = abm_tell(heat ~ e, to = .cell, when = .group == "sheep", .resolve = "first"),
    sum   = abm_tell(heat ~ e * 2, to = .cell, when = .group == "sheep" & e > 0.3, .resolve = "sum"),
    sumi  = abm_tell(heat ~ k, to = .cell, when = .group == "sheep", .resolve = "sum"),
    mean  = abm_tell(heat ~ e, to = .cell, when = .group == "sheep", .resolve = "mean")
  )
  for (nm in names(cases)) {
    go <- abm_go(abm_move(along = "patches", who = "sheep"), cases[[nm]],
                 abm_rules(e ~ e + runif(n()) / 10, .scope = "population"))
    got <- expect_native_agree(w, go, paste("tell", nm), ticks = 4)
    expect(got$stats[[2]] == 0L && got$stats[[4]] == 0L,
           paste("tell", nm, "not fully native:", paste(got$stats, collapse = " ")))
  }
  # to a partner, under a match
  m <- abm_setup(agents = abm_agents(n = 20, x = ~runif(n), got = 0), seed = 1)
  go <- abm_go(abm_match(pair = "one_of"), abm_tell(got ~ x, to = .partner, .resolve = "sum"),
               abm_rules(x ~ x * 0.9 + got / 100))
  got <- expect_native_agree(m, go, "tell partner", ticks = 4)
  expect(got$stats[[4]] == 0L, paste("tell partner not fully native:", paste(got$stats, collapse = " ")))
  # two messages to one agent with .resolve = "error": R's error
  e1 <- abm_go(abm_tell(got ~ x, to = 1L, .resolve = "error"), abm_rules(x ~ x))
  err <- function(e) conditionMessage(tryCatch(run_in(e, m, e1, ticks = 1, seed = 1),
                                               error = identity))
  expect_identical(err("rust"), err("r"))
})
