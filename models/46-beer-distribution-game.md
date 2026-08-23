# 46. The Beer Distribution Game (Sterman 1989)

**Concept**

- Setup: four positions in a line (retailer, wholesaler, distributor, factory),
  each holding stock, each filling the order from the position below it, each
  placing an order on the position above
- Delays: shipments take two weeks to arrive, orders take a week to be seen
- Go: receive, fill what you can, ship it down, then order using
  `max(0, demand + α(target − stock) − β·supply_line)`
- Demand: flat at four cases a week, stepping once to eight in week five, and
  never changing again
- Output: the size of the swing at each position

**Package**

```r
demand_before <- 4; demand_after <- 8; step_week <- 5
target <- 17; alpha <- 0.26; beta <- 0.34

m <- abm_setup(
  agents  = abm_agents(n = 4,
                       idx  = 1:4,                       # 1 retailer ... 4 factory
                       up   = c(2L, 3L, 4L, NA_integer_),
                       down = c(NA_integer_, 1L, 2L, 3L),
                       inventory = 12, backlog = 0,
                       orders_in = 4, ship_in1 = 4, ship_in2 = 4, order_sent = 4,
                       want = 0, shipped = 0, supply_line = 0),
  globals = list(week = 0L, demand = demand_before),
  seed    = 1)

go <- abm_go(
  abm_global(week ~ week + 1L),
  abm_global(demand ~ if (week >= step_week) demand_after else demand_before),

  # this week's shipment lands and the pipeline shuffles up
  abm_rules(inventory ~ inventory + ship_in1,
            ship_in1  ~ ship_in2,
            ship_in2  ~ 0),

  # the customer orders from the retailer
  abm_rules(orders_in ~ if_else(idx == 1L, demand, orders_in)),

  # fill what you can, remember what you could not
  abm_rules(want ~ orders_in + backlog),
  abm_rules(shipped ~ pmin(inventory, want)),
  abm_rules(inventory ~ inventory - shipped, backlog ~ want - shipped),

  # ship it downstream, into the far end of that position's pipeline
  abm_tell(ship_in2 ~ shipped, to = down, when = !is.na(down)),

  # anchor on demand, adjust for the stock gap, discount what is on its way
  abm_rules(supply_line ~ ship_in1 + ship_in2 + order_sent),
  abm_rules(order_sent ~ pmax(0, orders_in +
                                alpha * (target - (inventory - backlog)) -
                                beta * supply_line)),

  # orders are only seen next week, so clear the inbox and post them
  abm_rules(orders_in ~ 0),
  abm_tell(orders_in ~ order_sent, to = up, when = !is.na(up)),
  abm_rules(ship_in2 ~ if_else(idx == 4L, ship_in2 + order_sent, ship_in2))
)

result <- abm_run(m, go, ticks = 60, seed = 1)
```

**Result** (60 weeks, α = 0.26, target stock 17)

β = 0.34, the supply line is accounted for:

| position | peak order | s.d. | amplification |
|---|---|---|---|
| retailer | 11.3 | 2.56 | 1.0× |
| wholesaler | 14.3 | 4.43 | 1.7× |
| distributor | 15.4 | 4.97 | 1.9× |
| factory | 16.2 | 4.85 | 1.9× |

β = 0, the supply line is ignored:

| position | peak order | s.d. | amplification |
|---|---|---|---|
| retailer | 11.2 | 1.04 | 1.0× |
| wholesaler | 15.3 | 1.96 | 1.9× |
| distributor | 20.4 | 3.86 | 3.7× |
| factory | 25.0 | 5.09 | **4.9×** |

A customer who once ordered four cases a week now orders eight. The factory
brews twenty-five. Nobody is irrational, nobody is uninformed about their own
position, and the amplification comes entirely from the delays and from each
position correcting a stock gap whose cause it cannot see.

The second table is Sterman's actual finding about *why* people play this game
so badly. β is how much of the already-ordered-but-not-yet-arrived stock a
position discounts, and the experimental subjects systematically fail to
discount it, they reorder for a shortfall that is already on its way. Setting
β to zero is that mistake and nothing else, and it multiplies the bullwhip by
two and a half.

*Needed `abm_tell(to = <an id>)`, and it is the model that shows what the step is
for beyond the order book. Every other model in this corpus is a population of
interchangeable agents. Here each agent has a* place in a line *and writes to the
specific agent above and below it, a shipment into the agent downstream, an
order into the agent upstream, named by an `up` and a `down` column holding
`.id`s. The pipeline delays need nothing special: `ship_in1` and `ship_in2` are
two ordinary columns, and one simultaneous `abm_rules()` shuffles them up while
the arriving one lands, which is exactly the case `abm_rules()`'s simultaneity
was designed for and would be wrong under `abm_sequential()`.*

---

**Reproduce:** [`m46_beer_game.R`](scripts/m46_beer_game.R)

← [45. Hotelling's Law](45-hotellings-law.md) · [all models](README.md) · [47. Response thresholds and the division of labour](47-response-thresholds-and-division-of-labour.md) →
