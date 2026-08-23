# 43. Zero-intelligence traders in a double auction (Gode & Sunder 1993)

**Concept**

- Setup: 12 buyers holding redemption values from 190 down to 30, 12 sellers
  holding costs from 10 up to 170, one unit each
- Go: everyone shouts once, in a shuffled order. A buyer's bid is drawn
  uniformly below its value, a seller's ask uniformly above its cost. A shout
  that crosses the standing quote on the other side trades immediately at that
  quote; the two traders leave and the book clears. A shout that improves its
  own side becomes the new standing quote.
- Output: allocative efficiency, the surplus actually realised, as a fraction
  of the surplus a perfectly informed planner would have arranged

Gode & Sunder's traders have no strategy, no learning, no memory and no beliefs.
The only thing constraining them is that they will not trade at a loss. The
result is a market that captures almost all of the available surplus, which is
the paper's title: the market is a partial substitute for individual
rationality.

**Package**

```r
n_each <- 12; rounds <- 25; max_price <- 200
values <- seq(max_price * 0.95, max_price * 0.15, length.out = n_each)
costs  <- seq(max_price * 0.05, max_price * 0.85, length.out = n_each)

m <- abm_setup(
  agents = list(
    buyers  = abm_agents(n = n_each, value = values, cap = values,   # ZI-C
                         bid = 0, buy_cross = FALSE, buy_improves = FALSE,
                         traded = FALSE, price = NA_real_),
    sellers = abm_agents(n = n_each, cost = costs, floor_ = costs,
                         ask = 0, sell_cross = FALSE, sell_improves = FALSE,
                         traded = FALSE, price = NA_real_)),
  globals = list(best_bid = -Inf, best_bid_id = NA_integer_,
                 best_ask =  Inf, best_ask_id = NA_integer_,
                 buyer_id = NA_integer_, seller_id = NA_integer_,
                 trade_price = NA_real_),
  seed = 1)

shout <- abm_go(
  # a fresh book each round, and no trade recorded yet
  abm_global(best_bid ~ -Inf, best_bid_id ~ NA_integer_,
             best_ask ~ Inf,  best_ask_id ~ NA_integer_,
             buyer_id ~ NA_integer_, seller_id ~ NA_integer_,
             trade_price ~ NA_real_),

  # everybody shouts once, in a shuffled order. Each rule reads what the rule
  # above it wrote, which is what makes "draw, then check the book" sayable.
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

  # the trade itself: each side reaches across and fills the other
  abm_tell(traded ~ TRUE, price ~ trade_price,
           to = buyer_id,  when = .id == seller_id),
  abm_tell(traded ~ TRUE, price ~ trade_price,
           to = seller_id, when = .id == buyer_id)
)

result <- abm_run(m, shout, ticks = rounds, seed = 1)
```

`cap` is the buyer's value and `floor_` the seller's cost under ZI-C. Under ZI-U
they are the ends of the price range, which is the only difference between the
two treatments.

**Result** (6 seeds, 25 rounds, 12 buyers and 12 sellers)

| | allocative efficiency | trades | of a possible | mean price |
|---|---|---|---|---|
| ZI-C, will not trade at a loss | **95.8%** | 6.8 | 7 | 100.2 |
| ZI-U, unconstrained | **37.0%** | 12.0 | 7 | 104.7 |

The mean price of 100 is the competitive equilibrium price for these schedules,
which nobody in the model knows or is trying to find. ZI-U's failure is not that
it trades too little but that it trades too *much*: twelve trades where seven
were worthwhile, most of them destroying surplus rather than creating it.

*This is the model the corpus explicitly could not reach. "The order book is the
model this corpus cannot reach" sat under Open items from Part 3, and reaching it
forced the two changes that this stress test is really about.*

*The first is that a trade has to mark the counterparty, and the counterparty is
whoever the book says holds the best quote. That is not the agent's match
partner and there is no pairing that would make it one, so no amount of
`abm_match()` gets there. `abm_tell(to = <an id>)` is the answer: an expression
evaluated in the sender's row that names a recipient by `.id`.*

*The second is that a trader has to draw a quote and then decide whether that
quote crosses, inside one step. Rules inside `abm_sequential()` used to be
simultaneous within the agent, so `buy_cross` would have read the previous
tick's bid. They now cascade, which is what "one agent at a time" always
implied. `abm_rules()` is unchanged and still simultaneous, the distinction
between the two steps is now exactly the distinction between the two update
semantics, rather than being about globals.*

---

**Reproduce:** [`m43_zero_intelligence.R`](scripts/m43_zero_intelligence.R)

← [42. Kirman's ants](42-kirmans-ants.md) · [all models](README.md) · [44. Fairness versus reason in the ultimatum game](44-ultimatum-game.md) →
