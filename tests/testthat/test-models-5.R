# Models 47-56, the fourth stress test, at reduced scale. The full-scale runs
# that produced the numbers in models/ live in models/scripts/.

# 47. Response thresholds and the division of labour ------------------------

choose_task <- function(...) {
  f <- cbind(...)
  vapply(seq_len(nrow(f)), function(i) {
    w <- which(f[i, ])
    if (length(w) == 0L) 0L else if (length(w) == 1L) w else sample(w, 1L)
  }, integer(1))
}

dol_run <- function(castes, n = 60, delta = 1, alpha = 3, p = 0.2,
                    ticks = 1200, seed = 1) {
  m <- abm_setup(
    agents = abm_agents(n = n, task = 0L,
                        theta_1 = ~sample(castes, n, replace = TRUE),
                        theta_2 = ~sample(castes, n, replace = TRUE)),
    globals = list(s_1 = 0, s_2 = 0), seed = seed)
  go <- abm_go(
    abm_rules(task ~ if_else(task > 0L & runif(n()) < p, 0L, task)),
    abm_rules(fire_1 ~ task == 0L & runif(n()) < s_1^2 / (s_1^2 + theta_1^2),
              fire_2 ~ task == 0L & runif(n()) < s_2^2 / (s_2^2 + theta_2^2)),
    abm_rules(task ~ if_else(task == 0L, choose_task(fire_1, fire_2), task)),
    abm_global(s_1 ~ max(0, s_1 + delta - alpha * sum(task == 1L) / n()),
               s_2 ~ max(0, s_2 + delta - alpha * sum(task == 2L) / n()))
  )
  abm_run(m, go, ticks = ticks, seed = seed)
}

test_that("the fraction of the colony on a task settles at delta / alpha", {
  late <- function(r) r[r$tick > 900, ]
  one <- late(dol_run(c(500, 500)))
  two <- late(dol_run(c(50, 5000), seed = 2))
  # delta / alpha = 1/3, and it does not depend on the thresholds
  expect_equal(mean(one$task == 1L), 1 / 3, tolerance = 0.06)
  expect_equal(mean(two$task == 1L), 1 / 3, tolerance = 0.06)
})

test_that("unequal thresholds turn the same rule into a division of labour", {
  late <- dol_run(c(50, 5000), seed = 2)
  late <- late[late$tick > 900, ]
  responsive <- mean(late$task[late$theta_1 == 50] == 1L)
  reserve    <- mean(late$task[late$theta_1 == 5000] == 1L)
  expect_gt(responsive, 0.4)
  expect_lt(reserve, 0.05)
})

# 49. The emergence of firms ------------------------------------------------

test_that(".by aggregates over a partition the agents themselves control", {
  m <- abm_setup(agents = abm_agents(n = 9, firm = ~rep(1:3, 3),
                                     e = ~seq_len(n)))
  r <- abm_run(m, abm_go(abm_rules(tot ~ sum(e), sz ~ n(), .by = firm)),
               ticks = 1, seed = 1)
  f <- r[r$tick == 1, ]
  expect_equal(f$tot[f$firm == 1], rep(12, 3))
  expect_equal(f$tot[f$firm == 2], rep(15, 3))
  expect_true(all(f$sz == 3))

  # a rule that writes to the .by column moves the agent to another group
  r2 <- abm_run(m, abm_go(abm_rules(firm ~ 1L, .scope = "population"),
                          abm_rules(sz ~ n(), .by = firm)), ticks = 1, seed = 1)
  expect_true(all(r2$sz[r2$tick == 1] == 9))
})

test_that(".by and .scope = \"population\" are different groupings", {
  expect_error(abm_rules(x ~ sum(y), .by = g, .scope = "population"),
               class = "tidyABM_conflicting_args")
})

# 50. Adaptation on a rugged landscape --------------------------------------

make_nk <- function(N = 8, K = 0, seed = 1) {
  withr::with_seed(seed, {
    list(N = N, K = K,
         deps = lapply(seq_len(N), function(i) ((i - 1 + 0:K) %% N) + 1L),
         tbl = matrix(stats::runif(N * 2^(K + 1)), nrow = N))
  })
}
nk_fit <- function(v, nk) {
  mean(vapply(seq_len(nk$N), function(i) {
    nk$tbl[i, 1L + sum(v[nk$deps[[i]]] * 2^(0:nk$K))]
  }, numeric(1)))
}
nk_run <- function(K, n = 40, N = 8, ticks = 120, seed = 1) {
  nk <- make_nk(N = N, K = K, seed = seed)
  m <- abm_setup(agents = abm_agents(
    n = n, form = ~lapply(seq_len(n), function(i) as.integer(runif(N) < 0.5))),
    seed = seed)
  r <- abm_run(m, abm_go(
    abm_rules(trial ~ lapply(form, function(v) {
      i <- sample(N, 1); v[i] <- 1L - v[i]; v }), .scope = "population"),
    abm_rules(form ~ Map(function(a, b) if (nk_fit(b, nk) > nk_fit(a, nk)) b else a,
                         form, trial), .scope = "population")),
    ticks = ticks, seed = seed)
  f <- r[r$tick == ticks, ]
  length(unique(vapply(f$form, paste, character(1), collapse = "")))
}

test_that("interdependence is what makes organisational forms diverge", {
  # K = 0 is a single-peaked landscape: everybody climbs the same hill
  expect_equal(nk_run(K = 0), 1)
  expect_gt(nk_run(K = 7), 5)
})

test_that("a global may be a table, and the log keeps one row per tick", {
  m <- abm_setup(agents = abm_agents(n = 3, x = 1),
                 globals = list(tbl = matrix(1:4, 2)))
  r <- abm_run(m, abm_go(abm_rules(x ~ x + tbl[1, 1])), ticks = 2, seed = 1)
  g <- abm_globals(r)
  expect_equal(nrow(g), 3L)
  expect_equal(g$tick, 0:2)
  expect_equal(g$tbl[[1]], matrix(1:4, 2))
})

# 51. Imitation dynamics of vaccination -------------------------------------

test_that("abm_repeat runs a phase to absorption inside one tick", {
  m <- abm_setup(agents = abm_agents(n = 40, state = ~ifelse(seq_len(n) <= 2,
                                                             "I", "S")),
                 network = abm_network(type = "random", degree = 6), seed = 1)
  r <- abm_run(m, abm_go(abm_repeat(
    abm_neighbours(exposure ~ sum(state == "I")),
    abm_rules(state ~ dplyr::case_when(
      state == "S" & runif(n()) < 1 - (1 - 0.4)^coalesce(exposure, 0L) ~ "E",
      state == "I" & runif(n()) < 0.4 ~ "R",
      TRUE ~ state), .scope = "population"),
    abm_rules(state ~ if_else(state == "E", "I", state), .scope = "population"),
    until = sum(state == "I") == 0, max = 500)), ticks = 1, seed = 1)
  # one tick, and the epidemic is over: nobody is left infectious
  expect_equal(sum(r$state[r$tick == 1] == "I"), 0L)
  expect_gt(sum(r$state[r$tick == 1] == "R"), 2L)
})

test_that("abm_repeat stops at max and checks until after each pass", {
  m <- abm_setup(agents = abm_agents(n = 5, x = 0))
  never <- abm_run(m, abm_go(abm_repeat(abm_rules(x ~ x + 1),
                                        until = mean(x) > 1e6, max = 7)),
                   ticks = 1, seed = 1)
  expect_equal(unique(never$x[never$tick == 1]), 7)
  # the block always runs at least once, even if `until` already holds
  once <- abm_run(m, abm_go(abm_repeat(abm_rules(x ~ x + 1),
                                       until = TRUE, max = 7)),
                  ticks = 1, seed = 1)
  expect_equal(unique(once$x[once$tick == 1]), 1)
  expect_error(abm_repeat(abm_rules(x ~ x + 1)), class = "tidyABM_missing_arg")
  expect_error(
    abm_run(m, abm_go(abm_repeat(abm_rules(x ~ x + 1), until = x, max = 3)),
            ticks = 1, seed = 1),
    class = "tidyABM_bad_until")
})

# 52. Bank runs and the sequential service constraint -----------------------

bank_run <- function(ordered, impatient, n = 60, days = 30, r1 = 1.2,
                     liquid = 0.5, memory = 0.8, seed = 1) {
  m <- abm_setup(
    agents = abm_agents(n = n, pos = ~sample(n), theta = ~runif(n),
                        belief = 0, ran = FALSE, paid = 0, dry = FALSE),
    globals = list(till = liquid * n), seed = seed)
  rules <- list(
    ran    = ran    ~ runif(1) < impatient | belief > theta,
    paid   = paid   ~ if_else(ran, pmin(r1, till), 0),
    till   = till   ~ till - paid,
    dry    = dry    ~ till < r1,
    belief = belief ~ memory * belief + (1 - memory) * as.numeric(dry))
  serve <- if (ordered) {
    do.call(abm_sequential, c(unname(rules), list(.order = quote(pos))))
  } else {
    do.call(abm_sequential, unname(rules))
  }
  abm_run(m, abm_go(abm_global(till ~ liquid * n()), serve), ticks = days,
          seed = seed)
}

test_that("a fixed queue puts the panic at the back of it", {
  r <- bank_run(ordered = TRUE, impatient = 0.5)
  f <- r[r$tick > 20, ]
  front <- mean(f$ran[f$pos <= 15])
  back  <- mean(f$ran[f$pos > 45])
  expect_gt(back, front + 0.15)
})

test_that("reshuffling the queue spreads the panic instead of localising it", {
  s <- bank_run(ordered = FALSE, impatient = 0.5)
  f <- s[s$tick > 20, ]
  front <- mean(f$ran[f$pos <= 15])
  back  <- mean(f$ran[f$pos > 45])
  expect_lt(abs(back - front), 0.12)
  # and it produces a bigger run overall than the ordered queue does
  q <- bank_run(ordered = TRUE, impatient = 0.5)
  expect_gt(mean(f$ran), mean(q$ran[q$tick > 20]))
})

test_that(".order sends agents through in the order it names", {
  m <- abm_setup(agents = abm_agents(n = 5, q = ~rev(seq_len(n)), got = 0),
                 globals = list(stock = 3))
  r <- abm_run(m, abm_go(abm_sequential(got ~ pmin(1, stock),
                                        stock ~ stock - pmin(1, stock),
                                        .order = q)), ticks = 1, seed = 1)
  f <- r[r$tick == 1, ]
  expect_equal(f$got[order(f$q)], c(1, 1, 1, 0, 0))
})

# 53. Random copying / the neutral model ------------------------------------

test_that("the number of variants in use matches the Ewens formula", {
  neutral <- function(n, mu, ticks, seed = 1) {
    m <- abm_setup(agents = abm_agents(n = n, variant = ~seq_len(n)),
                   globals = list(coined = n), seed = seed)
    abm_run(m, abm_go(
      abm_rules(copied ~ sample(variant, n(), replace = TRUE),
                innovate ~ runif(n()) < mu, .scope = "population"),
      abm_rules(variant ~ if_else(innovate, coined + cumsum(innovate), copied),
                .scope = "population"),
      abm_global(coined ~ coined + sum(innovate))), ticks = ticks, seed = seed)
  }
  ewens <- function(n, mu) { th <- 2 * n * mu; sum(th / (th + seq_len(n) - 1)) }
  r <- neutral(200, 0.01, ticks = 1500)
  late <- r[r$tick > 1000, ]
  k <- mean(tapply(late$variant, late$tick, function(v) length(unique(v))))
  expect_equal(k, ewens(200, 0.01), tolerance = 0.25)
  # popularity is heavy-tailed: the top variant holds far more than its share
  last <- table(late$variant[late$tick == max(late$tick)])
  expect_gt(max(last) / sum(last), 3 / length(last))
})

# 54. Indirect reciprocity by image scoring ---------------------------------

test_that("abm_tell writes to a set of recipients and can collect messages", {
  m <- abm_setup(agents = abm_agents(
    n = 5, heard = 0, inbox = ~vector("list", n),
    aud = ~lapply(seq_len(n), function(i) c(1L, 2L))))
  r <- abm_run(m, abm_go(
    abm_tell(heard ~ 1, to = aud, when = .id > 3, .resolve = "sum"),
    abm_tell(inbox ~ lapply(.id, function(i) i), to = aud, when = .id > 3,
             .resolve = "collect"),
    abm_rules(n_msg ~ lengths(inbox))), ticks = 1, seed = 1)
  f <- r[r$tick == 1, ]
  expect_equal(f$heard, c(2, 2, 0, 0, 0))
  expect_equal(f$n_msg, c(2L, 2L, 0L, 0L, 0L))
  expect_setequal(unlist(f$inbox[[1]]), c(4L, 5L))
})

# 55. Deferred acceptance ---------------------------------------------------

rank_of <- function(lst, i) {
  vapply(seq_along(i), function(k) {
    v <- lst[[k]]
    if (is.na(i[[k]]) || is.null(v) || length(v) < i[[k]]) NA_real_
    else as.numeric(v[[i[[k]]]])
  }, numeric(1))
}

deferred_acceptance <- function(n, seed = 1) {
  ids_w <- (n + 1L):(2L * n); ids_m <- 1L:n
  rank_over <- function(k, targets) lapply(seq_len(k), function(i) {
    r <- rep(NA_real_, 2 * n); r[sample(targets)] <- seq_along(targets); r })
  m <- abm_setup(agents = list(
    men   = abm_agents(n = n, rank = ~rank_over(n, ids_w), worst = 0,
                       fiancee = NA_integer_, win = FALSE),
    women = abm_agents(n = n, rank = ~rank_over(n, ids_m),
                       holder = NA_integer_, best = Inf)), seed = seed)
  abm_run(m, abm_go(abm_repeat(
    abm_rules(best ~ Inf, holder ~ NA_integer_, .scope = "population"),
    abm_match(pair = "nearest",
              cost = if_else(rank_of(own_rank, .id) <= own_worst,
                             NA_real_, rank_of(own_rank, .id)),
              eligible = .group == "men", among = .group == "women"),
    abm_tell(best ~ rank_of(partner_rank, .id), to = .partner,
             when = .group == "men" & !is.na(.partner), .resolve = "min"),
    abm_rules(win ~ !is.na(.partner) &
                    rank_of(partner_rank, .id) == partner_best),
    abm_rules(fiancee ~ if_else(win, .partner, NA_integer_),
              worst ~ if_else(!is.na(.partner) & !win,
                              rank_of(rank, .partner), worst)),
    abm_tell(holder ~ .id, to = .partner, when = win),
    until = sum(.group == "men" & is.na(fiancee)) == 0,
    max = 2000)), ticks = 1, seed = seed)
}

test_that("deferred acceptance matches everybody, stably, and favours the proposers", {
  n <- 40
  f <- deferred_acceptance(n, seed = 1)
  f <- f[f$tick == 1, ]
  men   <- f[f$.group == "men", ]
  women <- f[f$.group == "women", ]

  expect_equal(sum(!is.na(men$fiancee)), n)
  expect_setequal(men$fiancee, women$.id)
  # each pair agrees who it is paired with
  expect_equal(women$holder[match(men$fiancee, women$.id)], men$.id)

  # no blocking pair: nobody prefers someone who also prefers them
  blocking <- 0L
  for (i in seq_len(n)) {
    mine <- men$rank[[i]][men$fiancee[[i]]]
    better <- women$.id[men$rank[[i]][women$.id] < mine]
    for (w in better) {
      j <- match(w, women$.id)
      if (women$rank[[j]][men$.id[[i]]] < women$rank[[j]][women$holder[[j]]]) {
        blocking <- blocking + 1L
      }
    }
  }
  expect_equal(blocking, 0L)

  # Pittel (1989): the proposing side averages about log(n)
  expect_lt(mean(rank_of(men$rank, men$fiancee)),
            mean(rank_of(women$rank, women$holder)))
})

test_that("cost = names what a chooser minimises, and NA means unacceptable", {
  m <- abm_setup(agents = list(
    b = abm_agents(n = 3, x = ~c(0, 5, 10), price = NA_real_),
    s = abm_agents(n = 2, x = ~c(1, 9), price = ~c(4, 0))))
  r <- abm_run(m, abm_go(
    abm_match(pair = "nearest", cost = price + abs(x - own_x),
              eligible = .group == "b", among = .group == "s"),
    abm_rules(chose ~ .partner)), ticks = 1, seed = 1)
  f <- r[r$tick == 1 & r$.group == "b", ]
  # delivered price, not distance: the buyer at 0 pays 4+1 here and 0+9 there
  expect_equal(f$chose, c(4L, 5L, 5L))

  none <- abm_run(m, abm_go(
    abm_match(pair = "nearest", cost = NA_real_,
              eligible = .group == "b", among = .group == "s"),
    abm_rules(chose ~ .partner)), ticks = 1, seed = 1)
  expect_true(all(is.na(none$chose[none$tick == 1])))

  expect_error(abm_match(pair = "nearest", by = x, cost = price),
               class = "tidyABM_conflicting_args")
  expect_error(abm_match(pair = "random", cost = price),
               class = "tidyABM_irrelevant_arg")
})

# 56. Predator and prey without space ---------------------------------------

wolfsheep <- function(response, n_sheep = 250, n_wolves = 175, ticks = 400,
                      sheep_rep = 0.03, wolf_rep = 0.02, catch = 0.04,
                      gain = 30, K = 1250, area = 300, seed = 1) {
  m <- abm_setup(agents = list(
    sheep  = abm_agents(n = n_sheep, energy = 0),
    wolves = abm_agents(n = n_wolves, energy = ~runif(n, 1, gain))),
    globals = list(n_sheep = n_sheep, n_wolves = n_wolves), seed = seed)
  hunt <- if (response == "mass_action") {
    abm_match(pair = "opposite_group", by = .group,
              eligible = .group == "sheep" | runif(n()) < n_sheep / area)
  } else {
    abm_match(pair = "opposite_group", by = .group)
  }
  abm_run(m, abm_go(
    abm_global(n_sheep ~ sum(.group == "sheep"),
               n_wolves ~ sum(.group == "wolves")),
    abm_rules(caught ~ FALSE, .scope = "population"),
    hunt,
    abm_rules(caught ~ runif(1) < catch),
    abm_rules(energy ~ if_else(.group == "wolves" & caught, energy + gain,
                               energy - (.group == "wolves")),
              .scope = "population"),
    abm_death(when = (.group == "sheep" & caught) |
                     (.group == "wolves" & energy <= 0)),
    abm_birth(when = .group == "sheep" &
                     runif(n()) < sheep_rep * (1 - n_sheep / K)),
    abm_birth(when = .group == "wolves" & runif(n()) < wolf_rep,
              cost = energy ~ energy / 2)),
    ticks = ticks, seed = seed)
}

test_that("mass-action predation cycles and the predator lags the prey", {
  g <- abm_globals(wolfsheep("mass_action", ticks = 400))[-1, ]
  expect_true(all(g$n_sheep > 0)); expect_true(all(g$n_wolves > 0))
  # both populations swing by a lot -- this is not a fixed point
  expect_gt(max(g$n_sheep) / min(g$n_sheep), 3)
  # and the wolves turn after the sheep do
  expect_gt(which.max(g$n_wolves), which.max(g$n_sheep))
})

test_that("an unmatched agent keeps the column a match wrote, so reset it", {
  m <- abm_setup(agents = list(a = abm_agents(n = 4, hit = FALSE),
                               b = abm_agents(n = 1, hit = FALSE)))
  go <- abm_go(abm_match(pair = "opposite_group", by = .group),
               abm_rules(hit ~ TRUE))
  r <- abm_run(m, go, ticks = 1, seed = 1)
  # only the pair that formed was written to; the other three were skipped
  expect_equal(sum(r$hit[r$tick == 1]), 2L)
})

test_that("a group of one is a group of one, not a group of `.id`", {
  # sample(x) reinterprets a length-1 numeric as seq_len(x), so a population
  # of one silently became a population of `.id` agents. Found by model 56,
  # where wolves outnumbered sheep and the last sheep paired with everybody.
  m <- abm_setup(agents = list(a = abm_agents(n = 4, p = NA_integer_),
                               b = abm_agents(n = 1, p = NA_integer_)))
  r <- abm_run(m, abm_go(abm_match(pair = "opposite_group", by = .group),
                         abm_rules(p ~ .partner)), ticks = 1, seed = 1)
  f <- r[r$tick == 1, ]
  expect_equal(sum(!is.na(f$p)), 2L)
  expect_true(all(f$p[!is.na(f$p)] %in% f$.id))

  one <- abm_setup(agents = abm_agents(n = 1, x = 0))
  r1 <- abm_run(one, abm_go(abm_match(pair = "random"), abm_rules(x ~ x + 1)),
                ticks = 1, seed = 1)
  expect_equal(nrow(r1), 2L)
  expect_equal(unique(r1$.id), 1L)

  # and a single candidate is the only one anyone can pick
  m3 <- abm_setup(agents = list(a = abm_agents(n = 3, p = NA_integer_),
                                b = abm_agents(n = 1, p = NA_integer_)))
  r3 <- abm_run(m3, abm_go(
    abm_match(pair = "one_of", eligible = .group == "a",
              among = .group == "b"),
    abm_rules(p ~ .partner)), ticks = 1, seed = 1)
  f3 <- r3[r3$tick == 1, ]
  expect_equal(f3$p, c(4L, 4L, 4L, NA_integer_))
})
