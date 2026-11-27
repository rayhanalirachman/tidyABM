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

test_that("FR8: abm_sequential() is native and bit-identical to R", {
  m <- abm_setup(agents = abm_agents(
    n = 40, w = ~runif(n, -1, 1), k = ~sample(1:5, n, TRUE), q = 0,
    key = ~c(NA, -0, 0, 0, sample(c(1, 2, 2.5), n - 4, TRUE)),
    s = ~sample(c("a", "b"), n, TRUE), f = FALSE),
    globals = list(pool = 3, cnt = 0L, last = NA_character_), seed = 3)
  cases <- list(
    # a later rule and the next agent read what was written; a global cascade
    cascade = abm_sequential(q ~ if_else(w < 0 & pool > 0, pmin(-w, pool), 0),
                             pool ~ pool - q, w ~ w + q),
    # base if() (lazy: draws only on the branch taken), runif(1), rnorm(1, a, b)
    draws   = abm_sequential(f ~ runif(1) < 0.5,
                             w ~ if (f) w + runif(1, 0, 0.1) else rnorm(1, w, 0.01),
                             q ~ as.numeric(f) + as.integer(k > 2)),
    # writes cast to the column's type (integer into a double column, logical
    # into an integer one); a global that changes type; strings
    casts   = abm_sequential(q ~ k, k ~ f, cnt ~ cnt + 1L, pool ~ cnt * 0.5,
                             last ~ s, s ~ if (f) NA else last),
    # .order: ties (-0 and 0) in place, NA sits out
    ordered = abm_sequential(cnt ~ cnt + 1L, q ~ cnt, .order = key),
    by_draw = abm_sequential(q ~ q + 1, pool ~ pool + q, .order = runif(n()))
  )
  for (nm in names(cases)) {
    go <- abm_go(cases[[nm]], abm_rules(f ~ !f, w ~ w * 0.9))
    got <- expect_native_agree(m, go, paste("seq", nm), ticks = 4)
    expect(got$stats[[2]] == 0L && got$stats[[4]] == 0L,
           paste("seq", nm, "not fully native:", paste(got$stats, collapse = " ")))
  }
  # two groups, each with the rules its columns route to it
  two <- abm_setup(agents = list(
    buyers = abm_agents(n = 8, v = ~runif(n), bid = 0),
    sellers = abm_agents(n = 8, c = ~runif(n), ask = 0)),
    globals = list(best = -Inf, who = NA_integer_), seed = 4)
  go <- abm_go(abm_sequential(bid ~ runif(1, 0, v), best ~ if (bid > best) bid else best,
                              who ~ if (bid >= best) .id else who,
                              ask ~ runif(1, c, 1), best ~ if (ask < best) ask * 0.5 else best))
  got <- expect_native_agree(two, go, "seq two groups", ticks = 4)
  expect(got$stats[[4]] == 0L, paste("seq two groups not native:", paste(got$stats, collapse = " ")))
  # a standing match: partner reads and writes, live within the queue
  shop <- abm_setup(agents = list(
    hh = abm_agents(n = 12, cash = ~runif(n, 1, 5), want = ~runif(n, 1, 3), got = 0),
    firm = abm_agents(n = 3, inv = ~runif(n, 2, 6), price = ~runif(n, 0.5, 1.5), sold = 0)),
    seed = 6)
  go <- abm_go(
    abm_match(pair = "one_of", eligible = .group == "hh", among = .group == "firm"),
    abm_sequential(got ~ pmin(want, cash / partner_price, partner_inv),
                   cash ~ cash - got * partner_price,
                   partner_inv ~ partner_inv - got,
                   partner_sold ~ partner_sold + got * partner_price),
    abm_rules(inv ~ inv + 1, .scope = "population"))
  got <- expect_native_agree(shop, go, "seq partner", ticks = 4)
  expect(got$stats[[2]] == 0L, paste("seq partner declined:", paste(got$stats, collapse = " ")))
  # pairs within one group, roles read
  pairs <- abm_setup(agents = abm_agents(n = 11, x = ~runif(n), y = 0), seed = 7)
  go <- abm_go(abm_match(pair = "random", role = list(a = TRUE, b = TRUE)),
               abm_sequential(y ~ if_else(.role == "a", partner_x, -partner_x),
                              partner_x ~ partner_x + 0.1, x ~ coalesce(x * 0.5, 0)))
  expect_native_agree(pairs, go, "seq pairs", ticks = 4)
  # declined at run time (a write that would change a column's type), compiled
  # nowhere (a new column)
  for (nm in c("widen", "newcol")) {
    st <- switch(nm, widen = abm_sequential(k ~ k + 0.5), newcol = abm_sequential(z ~ w * 2))
    expect_native_agree(m, abm_go(st, abm_rules(f ~ !f)), paste("seq", nm), ticks = 2,
                        native = FALSE)
  }
  # R's errors: two values, `if` without `else` (NULL), an NA condition
  for (st in list(abm_sequential(q ~ c(w, w)), abm_sequential(q ~ if (f) 1),
                  abm_sequential(q ~ if (NA) 1 else 2))) {
    e1 <- abm_go(st)
    err <- function(e) conditionMessage(tryCatch(run_in(e, m, e1, ticks = 1, seed = 1),
                                                 error = identity))
    expect_identical(err("rust"), err("r"))
  }
})

test_that("pkg::f naming a compiled function compiles as f", {
  expect_false(is_rx_no(rx_compile(quote(dplyr::case_when(stats::runif(n()) < 0.5 ~ !b, TRUE ~ b)))))
  # not the function the IR takes: R
  expect_true(is_rx_no(rx_compile(quote(base::`%in%`(s, "a")))))
  m <- abm_setup(agents = abm_agents(n = 30, b = ~runif(n) < 0.5, e = 0.3), seed = 1)
  go <- abm_go(abm_match(pair = "one_of"),
               abm_rules(b ~ dplyr::case_when(runif(n()) < e ~ !b,
                                              stats::runif(n()) < 0.4 ~ partner_b,
                                              TRUE ~ b)))
  got <- expect_native_agree(m, go, "pkg::f", ticks = 4)
  expect(got$stats[[2]] == 0L && got$stats[[4]] == 0L,
         paste("pkg::f not fully native:", paste(got$stats, collapse = " ")))
})

test_that("abm_global(.by = <name>) is native and bit-identical to R", {
  j <- 1:3
  m <- abm_setup(agents = abm_agents(n = 40, task = ~sample(0:3, n, TRUE)),
                 globals = list(s = stats::setNames(rep(0, 3), 1:3), w = stats::setNames(rep(5, 3), 1:3),
                                keys = c("a", "b")), seed = 1)
  cases <- list(
    named   = abm_global(s ~ max(0, s + 1 - 3 * sum(task == .key) / n()), .by = j),
    fresh   = abm_global(u2 ~ sum(task == .key) * 1L, .by = j),
    promote = abm_global(w ~ sum(task >= .key), w ~ w + 0.5, .by = j),
    strings = abm_global(v ~ .key, .by = keys),
    draws   = abm_global(s ~ s + runif(1), .by = j)
  )
  for (nm in names(cases)) {
    go <- abm_go(cases[[nm]], abm_rules(task ~ sample(0:3, n(), TRUE)))
    got <- expect_native_agree(m, go, paste("global .by", nm), ticks = 4)
    expect(got$stats[[2]] == 0L, paste("global .by", nm, "declined:", paste(got$stats, collapse = " ")))
  }
  # a value of several elements: R's error
  e1 <- abm_go(abm_global(s ~ task, .by = j))
  err <- function(e) conditionMessage(tryCatch(run_in(e, m, e1, ticks = 1, seed = 1),
                                               error = identity))
  expect_identical(err("rust"), err("r"))
})

test_that("abm_birth(attach_via =), abm_birth(n =) and deaths on a network are native", {
  m <- abm_setup(agents = abm_agents(n = 40, tag = ~sample(c("red", "blue"), n, TRUE),
                                     c = ~runif(n) < 0.5, e = ~runif(n)),
                 network = abm_network(type = "random", degree = 4), seed = 1)
  rt <- list(tag ~ sample(c("red", "blue"), n(), replace = TRUE),
             c ~ sample(c(TRUE, FALSE), n(), replace = TRUE))
  cases <- list(
    parent  = abm_birth(when = runif(n()) < 0.05, links = 4,
                        attach_via = abm_match(pair = "network", from = "parent")),
    random  = abm_birth(when = runif(n()) < 0.05, times = 2, links = 3,
                        attach_via = abm_match(pair = "network")),
    count   = abm_birth(n = 5, links = 4, inherit = rt, attach_via = abm_match(pair = "network")),
    cost    = abm_birth(n = 3, cost = list(e ~ 0.5), inherit = rt)
  )
  for (nm in names(cases)) {
    go <- abm_go(cases[[nm]], abm_death(when = runif(n()) < 0.05))
    got <- expect_native_agree(m, go, paste("birth", nm), ticks = 5)
    expect(got$stats[[2]] == 0L && got$stats[[4]] == 0L,
           paste("birth", nm, "not fully native:", paste(got$stats, collapse = " ")))
  }
  # from = "parent" with `n =` and no network: R's errors
  for (go in list(abm_go(abm_birth(n = 2, attach_via = abm_match(pair = "network", from = "parent"))),
                  abm_go(abm_birth(when = TRUE, attach_via = abm_match(pair = "network"))))) {
    m0 <- if (length(go$steps[[1]]$when)) abm_setup(agents = abm_agents(n = 3, e = 1)) else m
    err <- function(e) conditionMessage(tryCatch(run_in(e, m0, go, ticks = 1, seed = 1),
                                                 error = identity))
    expect_identical(err("rust"), err("r"))
  }
})

test_that("the cross-product within is native and bit-identical to R", {
  m <- abm_setup(agents = list(
    people = abm_agents(n = 50, x = ~runif(n), y = ~runif(n), active = ~runif(n) < 0.3,
                        k = ~sample(c(1L, 2L, NA), n, TRUE)),
    cops = abm_agents(n = 8, x = ~runif(n), y = ~runif(n))), seed = 1)
  v <- 0.2
  cases <- list(
    rebellion = abm_neighbours(C ~ sum(.group == "cops"), A ~ 1 + sum(.group == "people" & active),
                               K ~ n(), within = .id != own_.id & abs(x - own_x) <= v &
                                 abs(y - own_y) <= v),
    shapes    = abm_neighbours(p ~ mean(x > 0.5), an ~ any(active), al ~ all(active, na.rm = TRUE),
                               s ~ sum(k, na.rm = TRUE), d ~ max(y) - min(y),
                               within = abs(x - own_x) < 0.1)
  )
  for (nm in names(cases)) {
    go <- abm_go(cases[[nm]], abm_rules(x ~ pmin(1, pmax(0, x + runif(n()) * 0.1 - 0.05))))
    got <- expect_native_agree(m, go, paste("cross", nm), ticks = 4)
    expect(got$stats[[2]] == 0L, paste("cross", nm, "declined:", paste(got$stats, collapse = " ")))
  }
  # a mean of doubles (rowsum()'s sums) and an NA in a sum: left to R
  for (st in list(abm_neighbours(mx ~ mean(x), within = abs(x - own_x) < 0.1),
                  abm_neighbours(s ~ sum(k), within = abs(x - own_x) < 0.1))) {
    expect_native_agree(m, abm_go(st), "cross declined", ticks = 2, native = FALSE)
  }
})

test_that("match nearest on one coordinate and network any()/all() are native", {
  m <- abm_setup(agents = list(
    buyers = abm_agents(n = 30, x = ~runif(n), k = ~sample(1:5, n, TRUE), customers = 0),
    shops = abm_agents(n = 5, x = ~runif(n), k = ~sample(1:5, n, TRUE), customers = 0)), seed = 1)
  cases <- list(
    shops = abm_go(abm_match(pair = "nearest", by = x, eligible = .group == "buyers",
                             among = .group == "shops"),
                   abm_tell(customers ~ 1, to = .partner, when = .group == "buyers", .resolve = "sum"),
                   abm_rules(x ~ x + runif(n()) * 0.01)),
    ties  = abm_go(abm_match(pair = "nearest", by = k), abm_rules(customers ~ coalesce(partner_x, 0))))
  for (nm in names(cases)) {
    got <- expect_native_agree(m, cases[[nm]], paste("nearest", nm), ticks = 4)
    expect(got$stats[[2]] == 0L && got$stats[[4]] == 0L,
           paste("nearest", nm, "not fully native:", paste(got$stats, collapse = " ")))
  }
  f <- abm_setup(agents = abm_agents(state = ~sample(c("tree", "empty", "burning"), n, TRUE,
                                                     c(0.6, 0.38, 0.02)),
                                     b = ~sample(c(TRUE, FALSE, NA), n, TRUE)),
                 network = abm_network(type = "grid", dims = c(15, 15), diagonals = FALSE), seed = 1)
  go <- abm_go(abm_neighbours(hot ~ any(state == "burning"), a2 ~ all(b),
                              a3 ~ all(b, na.rm = TRUE), a4 ~ any(b)),
               abm_rules(state ~ case_when(state == "burning" ~ "burnt",
                                           state == "tree" & hot ~ "burning", TRUE ~ state)))
  got <- expect_native_agree(f, go, "network any/all", ticks = 5)
  expect(got$stats[[2]] == 0L && got$stats[[4]] == 0L,
         paste("network any/all not fully native:", paste(got$stats, collapse = " ")))
  # a network that starts empty and grows: the neighbour table is rebuilt
  # whenever the edges change, as R rebuilds it
  t0 <- abm_setup(agents = abm_agents(n = 12, on = ~runif(n) < 0.3),
                  network = abm_network(type = "empty"), seed = 2)
  go <- abm_go(abm_neighbours(near ~ any(on)), abm_match(pair = "random"), abm_link(),
               abm_rules(on ~ on | coalesce(near, FALSE)))
  expect_native_agree(t0, go, "network any/all growing", ticks = 5)
})

test_that("abm_move(direction =) is native and bit-identical to R", {
  ant <- abm_setup(agents = list(patches = abm_agents(white = TRUE),
                                 ant = abm_agents(n = 3, heading = ~sample(0:3, n, TRUE), at = 113L)),
                   network = abm_network(type = "grid", dims = c(15, 15), on = "patches", torus = TRUE),
                   seed = 1)
  cases <- list(
    langton = abm_go(abm_neighbours(here_white ~ all(white), within = .id == own_.cell),
                     abm_rules(heading ~ (heading + if_else(here_white, 1L, 3L)) %% 4L,
                               .scope = "population"),
                     abm_tell(white ~ !here_white, to = .cell),
                     abm_move(along = "patches", who = "ant", direction = heading)),
    strings = abm_go(abm_move(along = "patches", who = "ant", avoid_occupied = TRUE,
                              direction = c("North", "east", "SOUTH")[(heading %% 3L) + 1L]),
                     abm_rules(heading ~ heading + 1L, .scope = "population")),
    halves  = abm_go(abm_move(along = "patches", who = "ant", direction = heading + 0.5),
                     abm_rules(heading ~ heading - 1L, .scope = "population")))
  for (nm in names(cases)) {
    got <- expect_native_agree(ant, cases[[nm]], paste("direction", nm), ticks = 8)
    expect(got$stats[[2]] == 0L && got$stats[[4]] == 0L,
           paste("direction", nm, "not fully native:", paste(got$stats, collapse = " ")))
  }
  # a heading the lattice has no side for: R's error
  e1 <- abm_go(abm_move(along = "patches", who = "ant", direction = "up"))
  err <- function(e) conditionMessage(tryCatch(run_in(e, ant, e1, ticks = 1, seed = 1),
                                               error = identity))
  expect_identical(err("rust"), err("r"))
})

test_that("FR14: abm_draw() and network-view abm_neighbours() (own_, edge draws, na.rm) are native", {
  # complete graph with endpoint draws, edge draws, own_ columns and mixed aggregates
  m <- abm_setup(
    agents = abm_agents(n = 14, b = ~sample(0:7, n, TRUE), v = ~sample(0:7, n, TRUE),
                        type = ~sample(c("A", "B", NA), n, TRUE)),
    network = abm_network(type = "complete"), globals = list(k = 0.5), seed = 1)
  go <- abm_go(
    abm_rules(seen ~ runif(n())),
    abm_rules(def ~ b / 7 > seen),
    abm_draw(saw ~ runif(n()), zeal ~ runif(n()), .each = "endpoint"),
    abm_neighbours(w ~ sum(def & saw < seen),
                   pa ~ sum(def & saw < seen & zeal < own_v / 7)),
    abm_neighbours(pun ~ sum(own_def & saw_back < own_seen & zeal_back < v / 7)),
    abm_draw(coin ~ runif(n()) < k, .each = "edge"),
    abm_neighbours(mc ~ sum(coin), mn ~ mean(type == own_type, na.rm = TRUE),
                   ma ~ any(coin), ml ~ all(coin), deg ~ n()))
  got <- expect_native_agree(m, go, "draw + nb_view", ticks = 4)
  expect(got$stats[[2]] == 0L && got$stats[[4]] == 0L,
         paste("draw + nb_view not fully native:", paste(got$stats, collapse = " ")))

  # isolated agents, all-NA neighbourhoods (0/0 NaN), and non-sequential ids after death
  g <- abm_setup(
    agents = abm_agents(n = 16, type = ~sample(c("A", "B", NA), n, TRUE, c(0.2, 0.2, 0.6)),
                        e = ~runif(n)),
    network = abm_network(type = "poisson", degree = 1.5), seed = 2)
  go2 <- abm_go(
    abm_death(when = e < 0.15),
    abm_draw(w ~ runif(n()) < 0.4),
    abm_neighbours(same ~ mean(type == own_type, na.rm = TRUE),
                   s_na ~ sum(type == own_type),
                   cnt ~ n(),
                   any_w ~ any(w),
                   of_hit ~ sum(.of > 0L)))
  got2 <- expect_native_agree(g, go2, "nb_view sparse/death", ticks = 4)
  expect(got2$stats[[2]] == 0L && got2$stats[[4]] == 0L,
         paste("nb_view sparse/death not fully native:", paste(got2$stats, collapse = " ")))

  # bare n() alone (zero argument programs)
  got_n <- expect_native_agree(g, abm_go(abm_neighbours(deg ~ n())), "nb_view bare n", ticks = 2)
  expect(got_n$stats[[2]] == 0L && got_n$stats[[4]] == 0L,
         paste("nb_view bare n not fully native:", paste(got_n$stats, collapse = " ")))

  # errors: draw with no network, draw shadowing an agent column, stale draw after link
  m_nonet <- abm_setup(agents = abm_agents(n = 4, x = 1))
  e_cases <- list(
    list(m_nonet, abm_go(abm_draw(z ~ runif(n())))),
    list(m, abm_go(abm_draw(b ~ runif(n())))),
    list(g, abm_go(abm_draw(z ~ runif(n())), abm_match(pair = "random"), abm_link(),
                   abm_neighbours(s ~ sum(z > 0.5))))
  )
  for (ec in e_cases) {
    err <- function(e) conditionMessage(tryCatch(run_in(e, ec[[1]], ec[[2]], ticks = 2, seed = 1),
                                                 error = identity))
    expect_identical(err("rust"), err("r"))
  }
})

test_that("FR14: abm_move() uphill/downhill and range/axes_only are native and bit-identical", {
  w <- 12L
  mk <- function(torus, diag) {
    abm_setup(
      agents = list(
        patches = abm_agents(food = ~sample(c(0, 1, 2, 3, NA), n, TRUE, c(0.3, 0.3, 0.2, 0.15, 0.05)),
                             scent = ~runif(n), x = ~runif(n)),
        a = abm_agents(n = 15, carrying = ~runif(n) < 0.4, vision = ~sample.int(4, n, TRUE),
                       x = ~runif(n), at = ~sample(prod(dims), n)),
        b = abm_agents(n = 8, carrying = FALSE, vision = 2L, x = 0.5)),
      network = abm_network(type = "grid", dims = c(w, w), on = "patches",
                            torus = torus, diagonals = diag),
      globals = list(wiggle = 0.5), seed = 5)
  }
  cases <- list(
    ants = list(mk(FALSE, TRUE),
                abm_move(along = "patches", who = "a",
                         to = uphill(if_else(carrying, scent, coalesce(food, 0) + runif(n()) * wiggle)))),
    sugar = list(mk(TRUE, FALSE),
                 abm_move(along = "patches", who = "a",
                          to = uphill(if_else(pmin(abs(.x - own_.x), w - abs(.x - own_.x)) +
                                              pmin(abs(.y - own_.y), w - abs(.y - own_.y)) <= vision,
                                              food, -Inf)),
                          range = 4, axes_only = TRUE, avoid_occupied = TRUE)),
    down = list(mk(TRUE, TRUE),
                abm_move(along = "patches", who = c("a", "b"),
                         to = downhill(if_else(x > 0.5, food, own_x + scent)),
                         range = 2, avoid_occupied = TRUE)),
    ball = list(mk(FALSE, FALSE),
                abm_move(along = "patches", to = "random_neighbour", who = "a", range = 2)),
    axes_rand = list(mk(TRUE, TRUE),
                     abm_move(along = "patches", to = "random_empty_neighbour",
                              who = "a", range = 3, axes_only = TRUE))
  )
  for (nm in names(cases)) {
    go <- abm_go(cases[[nm]][[2]])
    got <- expect_native_agree(cases[[nm]][[1]], go, paste("move FR14", nm), ticks = 5)
    expect(got$stats[[2]] == 0L && got$stats[[4]] == 0L,
           paste("move FR14", nm, "not fully native:", paste(got$stats, collapse = " ")))
  }
  # empty wired group: R's tidyABM_no_lattice error
  m_empty <- mk(TRUE, FALSE)
  m_empty$groups$patches <- m_empty$groups$patches[0, ]
  err <- function(e) conditionMessage(tryCatch(
    run_in(e, m_empty, abm_go(abm_move(along = "patches", who = "a", to = uphill(food))),
           ticks = 1, seed = 1), error = identity))
  expect_identical(err("rust"), err("r"))
})

test_that("FR15a: nearest+cost match, network link/unlink, and tell(.resolve = 'collect') are native", {
  # nearest + cost (own_, .of, ties, NA, -Inf, non-monotonic ids, multi-group)
  open <- FALSE
  m <- abm_setup(
    agents = list(
      choices = abm_agents(n = 6, kind = ~rep(1:3, 2), deficit = ~c(3, 1, 2, 1, 4, 2),
                           members = ~vector("list", n), score = 0),
      problems = abm_agents(n = 12, kind = ~sample(1:3, n, TRUE), active = ~runif(n) < 0.8)
    ),
    globals = list(bonus = 0.5), seed = 11)
  go <- abm_go(
    abm_rules(members ~ vector("list", n()), .scope = "population"),
    abm_match(pair = "nearest",
              cost = if_else(open | kind == own_kind, deficit - bonus * (kind == 1L), NA_real_),
              eligible = .group == "problems" & active, among = .group == "choices"),
    abm_tell(members ~ .id, to = .partner,
             when = .group == "problems" & !is.na(.partner), .resolve = "collect"),
    abm_tell(score ~ 1L, to = .partner,
             when = .group == "problems" & !is.na(.partner), .resolve = "sum"),
    abm_rules(deficit ~ deficit + 0.1 * (.id %% 3L), .scope = "population"))
  got <- expect_native_agree(m, go, "FR15a nearest+cost & tell collect", ticks = 4)
  # step 1 (`vector("list", n())`) stays R, all other 4 steps run natively
  expect_identical(got$stats[[2]], 0L)
  expect_identical(got$stats[[1]], 16L)

  # error parity: cost returning logical -> tidyABM_bad_cost; collect on non-list column
  err <- function(e, mod, g) conditionMessage(tryCatch(run_in(e, mod, g, ticks = 1, seed = 1),
                                                       error = identity))
  expect_identical(err("rust", m, abm_go(abm_match(pair = "nearest", cost = kind == own_kind))),
                   err("r",    m, abm_go(abm_match(pair = "nearest", cost = kind == own_kind))))
  expect_identical(err("rust", m, abm_go(abm_match(pair = "random"),
                                         abm_tell(score ~ .id, to = .partner, .resolve = "collect"))),
                   err("r",    m, abm_go(abm_match(pair = "random"),
                                         abm_tell(score ~ .id, to = .partner, .resolve = "collect"))))

  # network abm_link() and abm_unlink(): size = 2, size = 3 cliques, when =, duplicates, empty match
  g <- abm_setup(agents = abm_agents(n = 15, x = ~runif(n)),
                 network = abm_network(type = "empty"), seed = 12)
  go_l <- abm_go(
    abm_match(pair = "random", eligible = x > 0.2),
    abm_link(when = x + partner_x > 0.7),
    abm_match(pair = "random", size = 3, eligible = x > 0.4),
    abm_link(),
    abm_match(pair = "network", eligible = runif(n()) < 0.5),
    abm_unlink(when = x > 0.3),
    abm_match(pair = "random", eligible = FALSE),
    abm_link(),
    abm_unlink())
  got_l <- expect_native_agree(g, go_l, "FR15a link/unlink", ticks = 4)
  expect(got_l$stats[[2]] == 0L && got_l$stats[[4]] == 0L,
         paste("FR15a link/unlink not fully native:", paste(got_l$stats, collapse = " ")))

  # error parity: link without network, link without match
  m_nonet <- abm_setup(agents = abm_agents(n = 4, x = 1))
  expect_identical(err("rust", m_nonet, abm_go(abm_match(pair = "random"), abm_link())),
                   err("r",    m_nonet, abm_go(abm_match(pair = "random"), abm_link())))
  expect_identical(err("rust", g, abm_go(abm_link())),
                   err("r",    g, abm_go(abm_link())))
})


test_that("FR15b: relations in abm_sequential(), non-relation steps alongside relations, and abm_pairs()", {
  # two groups (buyers 1..6, sellers 7..10) + relation `sellers` with `unmet` and `trades`
  pairs_df <- data.frame(from = rep(1:6, each = 2L),
                         to   = c(7L, 8L, 7L, 9L, 8L, 9L, 8L, 10L, 9L, 10L, 7L, 10L))
  m <- abm_setup(
    agents = list(
      buyers  = abm_agents(n = 6, want = 3, got = 0, cash = 10, shop = ~c(7L, 7L, 8L, 8L, 9L, 10L)),
      sellers = abm_agents(n = 4, inv = 5, price = ~c(1, 1.5, 2, 1.2), sales = 0)
    ),
    globals = list(rate = 0.5),
    relations = list(sellers = abm_relation(pairs_df, unmet = 0L, trades = 0)),
    seed = 21)

  # 1. non-relation steps (abm_rules, abm_neighbours within, abm_global, abm_match)
  #    + abm_sequential reading/writing relation cols (incl. .sellers and _back) + abm_pairs
  go_rel <- abm_go(
    abm_rules(want ~ 3, got ~ 0, .scope = "population"),
    abm_neighbours(within = .id == own_shop, shop_price ~ sum(price)),
    abm_match(pair = "one_of", eligible = .group == "buyers", among = .sellers),
    abm_sequential(
      got ~ if_else(.sellers, pmin(want, partner_inv), 0),
      partner_inv ~ if_else(.sellers, partner_inv - got, partner_inv),
      sellers_unmet ~ if_else(got < want, sellers_unmet + 1L, 0L),
      sellers_trades ~ sellers_trades + got,
      cash ~ cash - if_else(.sellers & sellers_unmet == 0L, got * partner_price, 0)
    ),
    abm_match(pair = "one_of", eligible = .group == "sellers", among = .group == "buyers"),
    abm_sequential(
      sales ~ sales + if_else(.sellers_back, coalesce(sellers_trades_back, 0), 0),
      .order = -.id
    ),
    abm_pairs(via = "sellers",
              unmet ~ if_else(from_cash < 5, unmet + 1L, unmet),
              trades ~ trades * rate,
              .when = to_price >= 1.2),
    abm_global(rate ~ rate * 0.99)
  )
  got <- expect_native_agree(m, go_rel, "FR15b relations full", ticks = 4)
  # all 8 steps (including `among = .sellers`) run natively every tick (32)
  expect_identical(got$stats[[2]], 0L)
  expect_identical(got$stats[[1]], 32L)
  expect_identical(got$stats[[4]], 0L)

  # 2. abm_pairs creating a new column on tick 1 (R updates rel$cols/defaults), then native tick 2+
  go_new <- abm_go(
    abm_pairs(via = "sellers", fresh ~ unmet + 1L),
    abm_pairs(via = "sellers", fresh ~ fresh + 2L)
  )
  got_new <- expect_native_agree(m, go_new, "FR15b pairs new col", ticks = 3)
  expect_true(got_new$stats[[1]] >= 4L)

  # 3. abm_death with relations still declines safely so R prunes relations
  go_death <- abm_go(
    abm_death(when = .id == 1L),
    abm_pairs(via = "sellers", unmet ~ unmet + 1L)
  )
  expect_engines_agree(m, go_death, ticks = 2, seed = 1, what = "FR15b death prunes relations")

  # 4. error parity: not_related, no_match (missing & size > 2), bad_target (.R on LHS),
  #    no_relation & bad_when in abm_pairs
  err <- function(e, mod, g) conditionMessage(tryCatch(run_in(e, mod, g, ticks = 1, seed = 1),
                                                       error = identity))
  # non-related pair write in abm_sequential -> tidyABM_not_related
  go_nr <- abm_go(
    abm_match(pair = "one_of", eligible = .id == 1L, among = .id == 10L),
    abm_sequential(sellers_unmet ~ 1L)
  )
  expect_identical(err("rust", m, go_nr), err("r", m, go_nr))
  # no match standing -> tidyABM_no_match
  go_nm <- abm_go(abm_sequential(sellers_unmet ~ 1L))
  expect_identical(err("rust", m, go_nm), err("r", m, go_nm))
  # size > 2 match -> tidyABM_no_match
  go_sz <- abm_go(abm_match(pair = "random", size = 3), abm_sequential(sellers_unmet ~ 1L))
  expect_identical(err("rust", m, go_sz), err("r", m, go_sz))
  # .sellers on LHS -> tidyABM_bad_target
  go_bt <- abm_go(
    abm_match(pair = "one_of", eligible = .group == "buyers", among = .sellers),
    abm_sequential(.sellers ~ TRUE)
  )
  expect_identical(err("rust", m, go_bt), err("r", m, go_bt))
  # abm_pairs errors: unknown relation & non-logical .when
  go_nrel <- abm_go(abm_pairs(via = "nope", unmet ~ 1L))
  expect_identical(err("rust", m, go_nrel), err("r", m, go_nrel))
  go_bw <- abm_go(abm_pairs(via = "sellers", unmet ~ 1L, .when = unmet + 1L))
  expect_identical(err("rust", m, go_bw), err("r", m, go_bw))
})

test_that("Tier 1: tell(to = 'neighbours' & min/max), partial-col abm_global, multi-match equijoin", {
  # 1. abm_tell(to = "neighbours") + .resolve = "min"/"max"
  g <- abm_setup(
    agents = abm_agents(n = 12, val = ~sample(c(1L, 2L, 5L, NA_integer_), n, TRUE),
                        x = ~runif(n), out = 0, out_i = 0L, box = ~vector("list", n)),
    network = abm_network(type = "grid", dims = c(3, 4), diagonals = FALSE),
    seed = 31)
  for (res in c("last", "first", "sum", "mean", "min", "max")) {
    go_t <- abm_go(
      abm_tell(out ~ x, to = "neighbours", when = .id %% 2L == 1L, .resolve = res),
      abm_tell(out_i ~ val, to = "neighbours", when = .id <= 6L, .resolve = res)
    )
    got <- expect_native_agree(g, go_t, paste("tell neighbours", res), ticks = 3)
    expect(got$stats[[2]] == 0L && got$stats[[4]] == 0L,
           paste("tell neighbours", res, "not fully native:", paste(got$stats, collapse = " ")))
  }
  # no_network error parity
  m_nonet <- abm_setup(agents = abm_agents(n = 4, x = 1))
  err <- function(e, mod, go) conditionMessage(tryCatch(run_in(e, mod, go, ticks = 1, seed = 1),
                                                        error = identity))
  expect_identical(err("rust", m_nonet, abm_go(abm_tell(x ~ 2, to = "neighbours"))),
                   err("r",    m_nonet, abm_go(abm_tell(x ~ 2, to = "neighbours"))))
  # silent abm_tell() (when all FALSE or all NA targets) must NOT evaluate drawing rules
  go_silent <- abm_go(
    abm_tell(out_i ~ sample.int(10L, 1L), to = .id, when = .id < 0L),
    abm_tell(out_i ~ sample.int(10L, 1L), to = ifelse(.id > 0L, NA_integer_, .id)),
    abm_rules(x ~ runif(n()))
  )
  got_s <- expect_native_agree(g, go_silent, "tell silent no draw", ticks = 3)
  expect_identical(got_s$stats[[2]], 0L)
  expect_identical(got_s$stats[[4]], 0L)

  # 2. Multi-group abm_global() and abm_repeat(until =) with partial columns
  mg <- abm_setup(
    agents = list(
      firms = abm_agents(n = 4, wage = ~c(10, 12, 8, 15)),
      workers = abm_agents(n = 10, employer = ~c(1L, 1L, 1L, 2L, 2L, 3L, NA_integer_, 1L, 2L, NA_integer_),
                           active = ~c(TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
                           hours = ~c(4L, 5L, 6L, 3L, 2L, 8L, 1L, 4L, 5L, 2L))
    ),
    globals = list(avg_wage = 0, unemp = 0L), seed = 32)
  go_mg <- abm_go(
    abm_global(avg_wage ~ mean(wage, na.rm = TRUE),
               unemp ~ sum(.group == "workers" & is.na(employer))),
    abm_neighbours(n_emp ~ n(), any_act ~ any(active), all_act ~ all(active),
                   tot_h ~ sum(hours), within = employer == own_.id),
    abm_neighbours(n_act ~ n(), sum_act ~ sum(hours),
                   within = employer == own_.id & active)
  )
  got_mg <- expect_native_agree(mg, go_mg, "partial-col global + multi-match nb1", ticks = 3)
  expect(got_mg$stats[[2]] == 0L && got_mg$stats[[4]] == 0L,
         paste("partial-col global + multi-match nb1 not fully native:",
               paste(got_mg$stats, collapse = " ")))
})
