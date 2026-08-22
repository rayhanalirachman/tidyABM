# Models 37-46, the third stress test, at reduced scale. The full-scale runs
# that produced the numbers in MODELS.md live in models-part5/.

# 37. Virus on a Network ---------------------------------------------------

virus_go <- function(gain_resistance, spread = 0.05, check_freq = 10,
                     recovery = 0.1) {
  abm_go(
    abm_rules(timer ~ (timer + 1L) %% check_freq),
    abm_neighbours(exposure ~ sum(state == "infected")),
    abm_rules(state ~ if_else(
      state == "susceptible" &
        runif(n()) < 1 - (1 - spread)^coalesce(exposure, 0L),
      "infected", state)),
    abm_rules(state ~ if_else(
      state == "infected" & timer == 0L & runif(n()) < recovery,
      if_else(runif(n()) < gain_resistance, "resistant", "susceptible"),
      state)),
    abm_global(infected ~ mean(state == "infected"),
               resistant ~ mean(state == "resistant"))
  )
}

virus_model <- function(n = 80, degree = 6, seed = 1) {
  abm_setup(
    agents = abm_agents(n = n,
                        state = ~ifelse(seq_len(n) <= 3L, "infected",
                                        "susceptible"),
                        timer = ~sample.int(10L, n, replace = TRUE) - 1L),
    network = abm_network(type = "random", degree = degree),
    seed = seed)
}

test_that("resistance is what turns an endemic virus into a burnt-out one", {
  sis <- abm_globals(abm_run(virus_model(), virus_go(0), ticks = 400, seed = 1))
  sir <- abm_globals(abm_run(virus_model(), virus_go(1), ticks = 400, seed = 1))
  # SIS: nobody ever becomes resistant and the virus stays
  expect_equal(max(sis$resistant, na.rm = TRUE), 0)
  expect_gt(sis$infected[nrow(sis)], 0.5)
  # SIR: it burns through the network and stops
  expect_lt(sir$infected[nrow(sir)], 0.15)
  expect_gt(sir$resistant[nrow(sir)], 0.8)
})

# 38. Watts cascades -------------------------------------------------------

cascade_final <- function(z, phi = 0.18, n = 800, seed = 1, ticks = 40) {
  m <- abm_setup(agents = abm_agents(n = n, on = ~seq_len(n) == 1L, share = 0),
                 network = abm_network(type = "poisson", degree = z),
                 seed = seed)
  g <- abm_globals(abm_run(m, abm_go(
    abm_neighbours(share ~ mean(on)),
    abm_rules(on ~ on | coalesce(share, 0) >= phi),
    abm_global(active ~ mean(on))
  ), ticks = ticks, seed = seed))
  g$active[nrow(g)]
}

test_that("cascades happen inside Watts's window and nowhere else", {
  inside  <- vapply(1:12, function(s) cascade_final(3, seed = s), numeric(1))
  sparse  <- vapply(1:12, function(s) cascade_final(0.5, seed = s), numeric(1))
  dense   <- vapply(1:12, function(s) cascade_final(9, seed = s), numeric(1))
  expect_gt(mean(inside > 0.2), 0.4)     # z = 3 is well inside (1.02, 5.77)
  expect_equal(mean(sparse > 0.2), 0)    # too sparse to spread
  expect_equal(mean(dense > 0.2), 0)     # too dense to tip anybody
})

# 39. Sznajd ---------------------------------------------------------------

sznajd_run <- function(n = 14, p_up = 0.5, ticks = 250, seed = 1) {
  m <- abm_setup(
    agents = abm_agents(n = n, spin = ~ifelse(runif(n) < p_up, 1L, -1L),
                        speaking = FALSE),
    network = abm_network(type = "ring", degree = 2), seed = seed)
  abm_globals(abm_run(m, abm_go(
    abm_match(pair = "network", eligible = seq_len(n()) == sample.int(n(), 1)),
    abm_rules(speaking ~ !is.na(.partner) & spin == partner_spin),
    abm_tell(speaking ~ TRUE, to = .partner, when = speaking),
    abm_tell(spin ~ spin, to = "neighbours", when = speaking),
    abm_rules(speaking ~ FALSE, .scope = "population"),
    abm_global(up ~ mean(spin > 0))
  ), ticks = ticks, seed = seed))$up
}

test_that("outflow dynamics reach consensus and favour the larger camp", {
  ends <- vapply(1:20, function(s) {
    u <- sznajd_run(p_up = 0.8, seed = s); u[length(u)]
  }, numeric(1))
  expect_true(all(ends %in% c(0, 1)))          # always consensus
  expect_gt(mean(ends == 1), 0.7)              # and usually the majority's
})

# 40. Naming game ----------------------------------------------------------

test_that("the naming game converges on exactly one shared name", {
  n <- 30
  m <- abm_setup(agents = abm_agents(n = n, inventory = ~vector("list", n),
                                     utterance = NA_integer_), seed = 1)
  g <- abm_globals(abm_run(m, abm_go(
    abm_match(pair = "random", role = list(speaker = TRUE, hearer = TRUE)),
    abm_rules(utterance ~ {
      s <- which(.role == "speaker")[[1]]
      inv <- inventory[[s]]
      rep(if (length(inv)) inv[sample.int(length(inv), 1L)]
          else .id[[s]] * 1000000L + sum(lengths(inventory)) + 1L, n())
    }),
    abm_rules(inventory ~ {
      u <- utterance[[1]]
      h <- which(.role == "hearer")[[1]]
      if (u %in% inventory[[h]]) rep(list(u), n())
      else { out <- inventory; out[[h]] <- c(out[[h]], u); out }
    }),
    abm_global(words ~ sum(lengths(inventory)),
               distinct_names ~ length(unique(unlist(inventory))))
  ), ticks = 400, seed = 1))
  expect_equal(g$distinct_names[nrow(g)], 1L)
  expect_equal(g$words[nrow(g)], 30L)          # one name each, no more
  expect_gt(max(g$words, na.rm = TRUE), 2 * 30L)  # but it grew a lot on the way
})

# 41. Minority game --------------------------------------------------------

mg_volatility <- function(n = 51, m_mem = 5, s = 2, ticks = 800, seed = 1) {
  n_states <- 2^m_mem
  mg <- abm_setup(
    agents = abm_agents(
      n = n,
      strategies = ~lapply(seq_len(n), function(i)
        matrix(sample(0:1, n_states * s, TRUE), nrow = n_states, ncol = s)),
      score = ~lapply(seq_len(n), function(i) numeric(s)),
      action = 0L),
    globals = list(history = 1L, attendance = 0L), seed = seed)
  g <- abm_globals(abm_run(mg, abm_go(
    abm_rules(action ~ vapply(seq_along(strategies), function(i)
      as.integer(strategies[[i]][history, which.max(score[[i]])]), integer(1))),
    abm_global(attendance ~ sum(action)),
    abm_rules(score ~ {
      winner <- as.integer(attendance < n() / 2)
      lapply(seq_along(score), function(i) score[[i]] +
               ifelse(strategies[[i]][history, ] == winner, 1, -1))
    }),
    abm_global(history ~ (bitwAnd(history - 1L, 2L^(m_mem - 1) - 1L) * 2L +
                            as.integer(attendance < n() / 2)) + 1L)
  ), ticks = ticks, seed = seed))
  a <- 2 * g$attendance[g$tick > ticks / 2] - n
  stats::var(a) / n
}

test_that("the minority game beats chance near alpha_c and loses badly below it", {
  # alpha = 2^m / n; n = 51 puts m = 4 at alpha = 0.31, m = 1 at 0.04
  expect_lt(mg_volatility(m_mem = 4), 0.8)   # better than random play
  expect_gt(mg_volatility(m_mem = 1), 1.5)   # crowded, much worse
})

# 42. Kirman's ants --------------------------------------------------------

kirman_run <- function(n = 40, epsilon, delta = 0.05, ticks = 1500, seed = 1) {
  m <- abm_setup(agents = abm_agents(n = n, at_a = ~seq_len(n) <= n %/% 2),
                 seed = seed)
  abm_globals(abm_run(m, abm_go(
    abm_match(pair = "one_of"),
    abm_rules(at_a ~ dplyr::case_when(runif(n()) < epsilon ~ !at_a,
                                      runif(n()) < delta   ~ partner_at_a,
                                      TRUE                 ~ at_a)),
    abm_global(share_a ~ mean(at_a))
  ), ticks = ticks, seed = seed))$share_a
}

test_that("herding turns on when epsilon (n - 1) / delta drops below one", {
  herd <- kirman_run(epsilon = 0.0003)[-seq_len(300)]   # c = 0.23
  calm <- kirman_run(epsilon = 0.0050)[-seq_len(300)]   # c = 3.9
  expect_gt(stats::sd(herd), 1.5 * stats::sd(calm))
  expect_lt(mean(herd > 0.4 & herd < 0.6), mean(calm > 0.4 & calm < 0.6))
})

# 43. Zero-intelligence traders --------------------------------------------

zi_efficiency <- function(constrained, n_each = 8, rounds = 20, max_price = 200,
                          seed = 1) {
  values <- seq(max_price * 0.95, max_price * 0.15, length.out = n_each)
  costs  <- seq(max_price * 0.05, max_price * 0.85, length.out = n_each)
  best   <- sum(pmax(sort(values, decreasing = TRUE) - sort(costs), 0))
  m <- abm_setup(
    agents = list(
      buyers = abm_agents(n = n_each, value = values,
                          cap = if (constrained) values else max_price,
                          bid = 0, buy_cross = FALSE, buy_improves = FALSE,
                          traded = FALSE, price = NA_real_),
      sellers = abm_agents(n = n_each, cost = costs,
                           floor_ = if (constrained) costs else 0,
                           ask = 0, sell_cross = FALSE, sell_improves = FALSE,
                           traded = FALSE, price = NA_real_)),
    globals = list(best_bid = -Inf, best_bid_id = NA_integer_,
                   best_ask = Inf, best_ask_id = NA_integer_,
                   buyer_id = NA_integer_, seller_id = NA_integer_,
                   trade_price = NA_real_),
    seed = seed)

  r <- abm_run(m, abm_go(
    abm_global(best_bid ~ -Inf, best_bid_id ~ NA_integer_,
               best_ask ~ Inf, best_ask_id ~ NA_integer_,
               buyer_id ~ NA_integer_, seller_id ~ NA_integer_,
               trade_price ~ NA_real_),
    abm_sequential(
      bid ~ if (traded) 0 else runif(1, 0, cap),
      buy_cross ~ !traded & bid >= best_ask,
      buy_improves ~ !traded & !buy_cross & bid > best_bid,
      trade_price ~ if (buy_cross) best_ask else trade_price,
      seller_id ~ if (buy_cross) best_ask_id else seller_id,
      buyer_id ~ if (buy_cross) .id else buyer_id,
      best_bid ~ if (buy_cross) -Inf else if (buy_improves) bid else best_bid,
      best_bid_id ~ if (buy_cross) NA_integer_ else if (buy_improves) .id else best_bid_id,
      best_ask ~ if (buy_cross) Inf else best_ask,
      best_ask_id ~ if (buy_cross) NA_integer_ else best_ask_id,
      ask ~ if (traded) 0 else runif(1, floor_, max_price),
      sell_cross ~ !traded & ask <= best_bid,
      sell_improves ~ !traded & !sell_cross & ask < best_ask,
      trade_price ~ if (sell_cross) best_bid else trade_price,
      buyer_id ~ if (sell_cross) best_bid_id else buyer_id,
      seller_id ~ if (sell_cross) .id else seller_id,
      best_ask ~ if (sell_cross) Inf else if (sell_improves) ask else best_ask,
      best_ask_id ~ if (sell_cross) NA_integer_ else if (sell_improves) .id else best_ask_id,
      best_bid ~ if (sell_cross) -Inf else best_bid,
      best_bid_id ~ if (sell_cross) NA_integer_ else best_bid_id
    ),
    abm_tell(traded ~ TRUE, price ~ trade_price, to = buyer_id,
             when = .id == seller_id),
    abm_tell(traded ~ TRUE, price ~ trade_price, to = seller_id,
             when = .id == buyer_id)
  ), ticks = rounds, seed = seed)

  f <- r[r$tick == max(r$tick), ]
  b <- f[f$.group == "buyers" & f$traded, ]
  s <- f[f$.group == "sellers" & f$traded, ]
  (sum(b$value - b$price) + sum(s$price - s$cost)) / best
}

test_that("the budget constraint, not the trader, is what makes the market work", {
  zic <- mean(vapply(1:4, function(s) zi_efficiency(TRUE, seed = s), numeric(1)))
  ziu <- mean(vapply(1:4, function(s) zi_efficiency(FALSE, seed = s), numeric(1)))
  expect_gt(zic, 0.9)
  expect_lt(ziu, 0.7)
})

# 45. Hotelling ------------------------------------------------------------

test_that("two shops meet in the middle and the buyers pay for it", {
  m <- abm_setup(agents = list(
    shops = abm_agents(n = 2, x = ~runif(n, 10, 90), x_old = 0, step = 1,
                       customers = 0, base = 0),
    buyers = abm_agents(n = 200, x = ~seq(0, 100, length.out = n))), seed = 1)
  count <- list(
    abm_rules(customers ~ 0, .scope = "population"),
    abm_match(pair = "nearest", by = x, eligible = .group == "buyers",
              among = .group == "shops"),
    abm_tell(customers ~ 1, to = .partner, when = .group == "buyers",
             .resolve = "sum"))
  go <- do.call(abm_go, c(count, list(
    abm_rules(base ~ customers, .scope = "population"),
    abm_rules(x_old ~ x, .scope = "population"),
    abm_rules(x ~ pmin(100, pmax(0, x + sample(c(-1, 1), n(), TRUE) * step)),
              .scope = "population")), count,
    list(abm_rules(x ~ if_else(customers > base, x, x_old),
                   .scope = "population"))))
  r <- abm_run(m, go, ticks = 250, seed = 1)
  pos <- r$x[r$tick == 250 & r$.group == "shops"]
  expect_lt(abs(mean(pos) - 50), 4)     # the midpoint
  expect_lt(diff(range(pos)), 4)        # and back to back
})

# 46. Beer game ------------------------------------------------------------

beer_orders <- function(beta, weeks = 60, target = 17, alpha = 0.26) {
  m <- abm_setup(
    agents = abm_agents(n = 4, idx = 1:4, up = c(2L, 3L, 4L, NA_integer_),
                        down = c(NA_integer_, 1L, 2L, 3L),
                        inventory = 12, backlog = 0, orders_in = 4,
                        ship_in1 = 4, ship_in2 = 4, order_sent = 4,
                        want = 0, shipped = 0, supply_line = 0),
    globals = list(week = 0L, demand = 4))
  abm_globals(abm_run(m, abm_go(
    abm_global(week ~ week + 1L),
    abm_global(demand ~ if (week >= 5) 8 else 4),
    abm_rules(inventory ~ inventory + ship_in1, ship_in1 ~ ship_in2,
              ship_in2 ~ 0),
    abm_rules(orders_in ~ if_else(idx == 1L, demand, orders_in)),
    abm_rules(want ~ orders_in + backlog),
    abm_rules(shipped ~ pmin(inventory, want)),
    abm_rules(inventory ~ inventory - shipped, backlog ~ want - shipped),
    abm_tell(ship_in2 ~ shipped, to = down, when = !is.na(down)),
    abm_rules(supply_line ~ ship_in1 + ship_in2 + order_sent),
    abm_rules(order_sent ~ pmax(0, orders_in +
                                  alpha * (target - (inventory - backlog)) -
                                  beta * supply_line)),
    abm_rules(orders_in ~ 0),
    abm_tell(orders_in ~ order_sent, to = up, when = !is.na(up)),
    abm_rules(ship_in2 ~ if_else(idx == 4L, ship_in2 + order_sent, ship_in2)),
    abm_global(o1 ~ order_sent[1], o2 ~ order_sent[2], o3 ~ order_sent[3],
               o4 ~ order_sent[4])
  ), ticks = weeks, seed = 1))
}

test_that("a one-off step in demand amplifies up the chain", {
  g <- beer_orders(beta = 0.34)
  g <- g[g$tick > 0, ]
  sds <- vapply(c("o1", "o2", "o3", "o4"), function(k) stats::sd(g[[k]]),
                numeric(1))
  # the swing grows the further you are from the customer. It levels off at the
  # top of the chain rather than rising forever, which is also what Sterman
  # sees once the factory saturates.
  expect_true(all(diff(sds[1:3]) > 0))
  expect_gt(sds[[4]], 1.5 * sds[[1]])
  expect_gt(max(g$o4), 2 * 8)              # factory peaks above twice demand
})

test_that("ignoring the supply line makes the bullwhip much worse", {
  seen   <- beer_orders(beta = 0.34)
  blind  <- beer_orders(beta = 0)
  amp <- function(g) { g <- g[g$tick > 0, ]; stats::sd(g$o4) / stats::sd(g$o1) }
  expect_gt(amp(blind), 2 * amp(seen))
  expect_gt(max(blind$o4, na.rm = TRUE), max(seen$o4, na.rm = TRUE))
})
