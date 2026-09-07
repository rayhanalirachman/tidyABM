# 43. Zero-intelligence traders in a continuous double auction
#     (Gode & Sunder 1993, JPE 101(1): 119-137)
#
# Buyers hold a redemption value, sellers a cost, and neither has any strategy
# whatever: a buyer shouts a bid drawn uniformly below its value, a seller an
# ask drawn uniformly above its cost, and that is the whole of their
# intelligence. A shout that crosses the standing quote on the other side
# trades immediately at that quote; the two traders leave and the book clears.
#
# Gode & Sunder's result is that this market is nearly fully efficient. The
# discipline comes from the budget constraint plus the auction rules, not from
# the traders -- "the market is a partial substitute for individual
# rationality". Removing the constraint (ZI-U, quotes drawn over the whole
# price range) is what breaks it.
#
# This is the model the corpus could not previously reach, and it is the reason
# for two changes to the grammar. A trade has to mark the *counterparty*, who
# is whoever the book says holds the best quote and is nobody's match partner
# -- that is abm_tell(to = <an id>). And a trader has to draw a quote and then
# decide whether that quote crosses, inside the same step -- which needs the
# rules in abm_sequential() to cascade.

library(tidyABM)

zi_market <- function(constrained = TRUE, n_each = 12, rounds = 25,
                      max_price = 200, seed = 1) {
  values <- seq(max_price * 0.95, max_price * 0.15, length.out = n_each)
  costs  <- seq(max_price * 0.05, max_price * 0.85, length.out = n_each)

  # the competitive equilibrium: the most a market of these traders could earn
  best <- sum(pmax(sort(values, decreasing = TRUE) - sort(costs), 0))

  m <- abm_setup(
    agents = list(
      buyers = abm_agents(
        n = n_each, value = values,
        cap = if (constrained) values else max_price,   # ZI-C vs ZI-U
        bid = 0, buy_cross = FALSE, buy_improves = FALSE,
        traded = FALSE, price = NA_real_
      ),
      sellers = abm_agents(
        n = n_each, cost = costs,
        floor_ = if (constrained) costs else 0,
        ask = 0, sell_cross = FALSE, sell_improves = FALSE,
        traded = FALSE, price = NA_real_
      )
    ),
    globals = list(best_bid = -Inf, best_bid_id = NA_integer_,
                   best_ask =  Inf, best_ask_id = NA_integer_,
                   buyer_id = NA_integer_, seller_id = NA_integer_,
                   trade_price = NA_real_),
    seed = seed
  )

  shout <- abm_go(
    # a fresh book each round, and no trade recorded yet
    abm_global(best_bid ~ -Inf, best_bid_id ~ NA_integer_,
               best_ask ~ Inf,  best_ask_id ~ NA_integer_,
               buyer_id ~ NA_integer_, seller_id ~ NA_integer_,
               trade_price ~ NA_real_),

    # everybody shouts once, in a shuffled order. Each rule below reads what the
    # rule above it wrote, which is what makes "draw, then check the book"
    # expressible at all.
    abm_sequential(
      bid          ~ if (traded) 0 else runif(1, 0, cap),
      buy_cross    ~ !traded & bid >= best_ask,
      buy_improves ~ !traded & !buy_cross & bid > best_bid,
      trade_price  ~ if (buy_cross) best_ask else trade_price,
      seller_id    ~ if (buy_cross) best_ask_id else seller_id,
      buyer_id     ~ if (buy_cross) .id else buyer_id,
      best_bid     ~ if (buy_cross) -Inf else if (buy_improves) bid else best_bid,
      best_bid_id  ~ if (buy_cross) NA_integer_ else if (buy_improves) .id else best_bid_id,
      best_ask     ~ if (buy_cross) Inf else best_ask,
      best_ask_id  ~ if (buy_cross) NA_integer_ else best_ask_id,

      ask           ~ if (traded) 0 else runif(1, floor_, max_price),
      sell_cross    ~ !traded & ask <= best_bid,
      sell_improves ~ !traded & !sell_cross & ask < best_ask,
      trade_price   ~ if (sell_cross) best_bid else trade_price,
      buyer_id      ~ if (sell_cross) best_bid_id else buyer_id,
      seller_id     ~ if (sell_cross) .id else seller_id,
      best_ask      ~ if (sell_cross) Inf else if (sell_improves) ask else best_ask,
      best_ask_id   ~ if (sell_cross) NA_integer_ else if (sell_improves) .id else best_ask_id,
      best_bid      ~ if (sell_cross) -Inf else best_bid,
      best_bid_id   ~ if (sell_cross) NA_integer_ else best_bid_id
    ),

    # the trade itself: each side of it reaches across and fills the other
    abm_tell(traded ~ TRUE, price ~ trade_price,
             to = buyer_id,  when = .id == seller_id),
    abm_tell(traded ~ TRUE, price ~ trade_price,
             to = seller_id, when = .id == buyer_id),

    abm_global(volume ~ sum(traded) / 2)
  )

  r <- abm_run(m, shout, ticks = rounds, seed = seed)
  final <- r[r$tick == max(r$tick), ]

  b <- final[final$.group == "buyers"  & final$traded, ]
  s <- final[final$.group == "sellers" & final$traded, ]
  earned <- sum(b$value - b$price) + sum(s$price - s$cost)

  list(efficiency = earned / best, trades = nrow(b),
       max_trades = sum(sort(values, decreasing = TRUE) > sort(costs)),
       mean_price = mean(c(b$price, s$price)))
}

if (sys.nframe() == 0L) {
  collected <- list()
  for (con in c(TRUE, FALSE)) {
    out <- parallel::mclapply(1:6, function(s) zi_market(constrained = con, seed = s),
                              mc.cores = 2)
    cat(sprintf("%-5s  efficiency %5.1f%%   trades %4.1f of %d   mean price %5.1f\n",
                if (con) "ZI-C" else "ZI-U",
                100 * mean(vapply(out, function(o) o$efficiency, numeric(1))),
                mean(vapply(out, function(o) o$trades, numeric(1))),
                out[[1]]$max_trades,
                mean(vapply(out, function(o) o$mean_price, numeric(1)))))
    collected[[length(collected) + 1]] <- data.frame(
      market = if (con) "ZI-C (budget constrained)" else "ZI-U (unconstrained)",
      seed = 1:6,
      efficiency = vapply(out, function(o) o$efficiency, numeric(1)))
  }

  # --- figure -------------------------------------------------------------
  # Final-state comparison: allocative efficiency, with and without the budget
  # constraint. Gode and Sunder's point is the height of the first bar.
  library(ggplot2)

  fig_file <- function(name) {
    a <- grep("^--file=", commandArgs(), value = TRUE)
    d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
    dir.create(file.path(d, "..", "figures"), showWarnings = FALSE)
    file.path(d, "..", "figures", name)
  }

  d <- do.call(rbind, collected)

  fig <- ggplot(d, aes(market, efficiency)) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
    stat_summary(fun = mean, geom = "col", fill = "grey30", width = 0.55) +
    geom_point(size = 1.5, alpha = 0.7) +
    theme_minimal() +
    labs(title = "Zero-intelligence traders: the market, not the trader",
         subtitle = "allocative efficiency, 12 buyers and 12 sellers, 6 seeds",
         x = NULL, y = "efficiency (share of the competitive surplus)")
  print(fig)
  ggsave(fig_file("43-zero-intelligence-traders.png"), fig, width = 6,
         height = 4, dpi = 120)
}
