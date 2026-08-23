# 46. The Beer Distribution Game (Sterman 1989, Management Science 35: 321-339)
#
# Four positions in a line -- retailer, wholesaler, distributor, factory --
# each holding stock, each filling the order from the position below it, each
# placing an order on the position above. Shipments take two weeks to arrive
# and orders take a week to be seen. Customer demand is flat at four cases a
# week and then, once, steps up to eight, and never changes again.
#
# The bullwhip effect is what happens next: that one small step turns into a
# larger swing at the wholesaler, a larger one again at the distributor, and a
# wild one at the factory. Nobody is irrational and nobody is uninformed about
# their own position; the amplification comes from the delays and from each
# position ordering to correct a stock gap it cannot see the cause of.
#
# Sterman's finding about *why* people are so bad at this is the supply line:
# players systematically fail to discount the orders they have already placed
# and not yet received, and reorder for a shortfall that is already on its way.
# beta is how much of the supply line a position accounts for, and setting it
# to zero is that mistake. It is the second run below.
#
# Grammar note. Every other model in this corpus is a population of
# interchangeable agents. Here each agent has a *place in a line* and writes to
# the specific agent above and below it -- a shipment into the agent
# downstream, an order into the agent upstream. abm_tell(to = <an id>) is what
# says that; before it, no agent could write into another agent's row at all.

library(tidyABM)

beer_game <- function(weeks = 50, demand_before = 4, demand_after = 8,
                      step_week = 5, target = 17, alpha = 0.26, beta = 0.34,
                      seed = 1) {
  m <- abm_setup(
    agents = abm_agents(
      n         = 4,
      idx       = 1:4,                       # 1 retailer ... 4 factory
      up        = c(2L, 3L, 4L, NA_integer_),
      down      = c(NA_integer_, 1L, 2L, 3L),
      inventory = 12, backlog = 0,
      orders_in = 4, ship_in1 = 4, ship_in2 = 4, order_sent = 4,
      want = 0, shipped = 0, supply_line = 0
    ),
    globals = list(week = 0L, demand = demand_before),
    seed = seed
  )

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
    abm_rules(inventory ~ inventory - shipped,
              backlog   ~ want - shipped),

    # ship it downstream, into the far end of that position's pipeline
    abm_tell(ship_in2 ~ shipped, to = down, when = !is.na(down)),

    # anchor on demand, adjust for the stock gap, discount what is already
    # on its way (Sterman's stock-management heuristic)
    abm_rules(supply_line ~ ship_in1 + ship_in2 + order_sent),
    abm_rules(order_sent ~ pmax(0, orders_in +
                                  alpha * (target - (inventory - backlog)) -
                                  beta * supply_line)),

    # orders are only seen next week, so clear the inbox and post them
    abm_rules(orders_in ~ 0),
    abm_tell(orders_in ~ order_sent, to = up, when = !is.na(up)),
    # the factory brews its own order rather than passing it on
    abm_rules(ship_in2 ~ if_else(idx == 4L, ship_in2 + order_sent, ship_in2)),

    abm_global(o1 ~ order_sent[1], o2 ~ order_sent[2],
               o3 ~ order_sent[3], o4 ~ order_sent[4],
               i1 ~ inventory[1] - backlog[1], i4 ~ inventory[4] - backlog[4])
  )

  abm_globals(abm_run(m, go, ticks = weeks, seed = seed))
}

if (sys.nframe() == 0L) {
  stages <- c(o1 = "retailer", o2 = "wholesaler", o3 = "distributor",
              o4 = "factory")
  for (beta in c(0.34, 0)) {
    g <- beer_game(weeks = 60, beta = beta)
    g <- g[g$tick > 0, ]
    cat(sprintf("\nbeta = %.2f  (%s)\n", beta,
                if (beta > 0) "supply line accounted for"
                else "supply line ignored -- Sterman's misperception"))
    cat(sprintf("  %-13s %10s %10s %14s %14s\n",
                "position", "peak order", "sd", "amplification", "worst stock"))
    base <- stats::sd(g$o1)
    for (k in names(stages)) {
      cat(sprintf("  %-13s %10.1f %10.2f %13.1fx %14s\n",
                  stages[[k]], max(g[[k]]), stats::sd(g[[k]]),
                  stats::sd(g[[k]]) / base,
                  if (k == "o1") sprintf("%.0f", min(g$i1))
                  else if (k == "o4") sprintf("%.0f", min(g$i4)) else ""))
    }
  }
  cat("\nCustomer demand stepped from 4 to 8 cases once, in week 5, and never moved again.\n")
}
