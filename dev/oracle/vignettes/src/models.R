## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")


## ----setup--------------------------------------------------------------------
library(tidyABM)


## -----------------------------------------------------------------------------
economy <- abm_setup(agents = abm_agents(n = 500, money = 100))

go <- abm_go(
  abm_match(pair = "random", role = list(giver = money > 0, receiver = TRUE)),
  abm_rules(money ~ if_else(.role == "giver", money - 1, money + 1))
)

r <- abm_run(economy, go, ticks = 500, seed = 1)

round(quantile(r$money[r$tick == 500]))


## -----------------------------------------------------------------------------
elfarol <- abm_setup(
  agents  = abm_agents(n = 100, threshold = ~runif(n, 40, 80)),
  globals = list(last_attendance = 60)
)

go <- abm_go(
  abm_rules(go_today ~ last_attendance < threshold),
  abm_global(last_attendance ~ sum(go_today))
)

r <- abm_run(elfarol, go, ticks = 20, seed = 2)

abm_globals(r)$last_attendance


## -----------------------------------------------------------------------------
pd <- abm_setup(agents = abm_agents(
  n = 100,
  strategy = ~sample(c("cooperate", "defect"), n, replace = TRUE),
  payoff = 0
))

go <- abm_go(
  abm_match(pair = "random"),
  abm_rules(payoff ~ case_when(
    strategy == "cooperate" & partner_strategy == "cooperate" ~ 3,
    strategy == "defect"    & partner_strategy == "defect"    ~ 1,
    strategy == "defect"    & partner_strategy == "cooperate" ~ 5,
    strategy == "cooperate" & partner_strategy == "defect"    ~ 0
  )),
  abm_match(pair = "random"),
  abm_rules(strategy ~ if_else(partner_payoff > payoff, partner_strategy, strategy))
)

r <- abm_run(pd, go, ticks = 100, seed = 3)

mean(r$strategy[r$tick == 100] == "cooperate")


## -----------------------------------------------------------------------------
ethno <- abm_setup(agents = abm_agents(
  n = 100,
  tag      = ~sample(c("red", "blue"), n, replace = TRUE),
  strategy = ~sample(c("cooperate", "defect"), n, replace = TRUE),
  resource = 10
))

go <- abm_go(
  abm_match(pair = "random"),
  abm_rules(resource ~ case_when(
    strategy == "cooperate" & partner_strategy == "cooperate" ~ resource + 2,
    strategy == "defect"    & partner_strategy == "cooperate" ~ resource + 4,
    strategy == "cooperate" & partner_strategy == "defect"    ~ resource - 1,
    TRUE ~ resource
  ) - 1),
  abm_birth(when = resource > 20, cost = resource ~ resource / 2),
  abm_death(when = resource <= 0)
)

r <- abm_run(ethno, go, ticks = 30, seed = 4)

as.integer(table(r$tick))[c(1, 11, 21, 31)]


## -----------------------------------------------------------------------------
rumour <- abm_setup(agents = abm_agents(
  n = 200, state = ~c("spreader", rep("ignorant", n - 1))
))

go <- abm_go(
  abm_match(pair = "random"),
  abm_rules(state ~ case_when(
    state == "ignorant" & partner_state == "spreader" ~ "spreader",
    state == "spreader" & partner_state == "spreader" ~ "stifler",
    state == "spreader" & partner_state == "stifler"  ~ "stifler",
    TRUE ~ state
  ))
)

r <- abm_run(rumour, go, ticks = 100, seed = 5)

table(r$state[r$tick == 100])


## -----------------------------------------------------------------------------
party <- abm_setup(agents = abm_agents(n = 200, opinion = ~runif(n, 0, 1)))

go <- abm_go(
  abm_match(pair = "nearest", by = opinion),
  abm_rules(opinion ~ if_else(abs(opinion - partner_opinion) > 0.05,
                              runif(1), opinion))
)

r <- abm_run(party, go, ticks = 100, seed = 6)

mean_gap <- function(x) mean(diff(sort(x)))
c(start = mean_gap(r$opinion[r$tick == 0]),
  end   = mean_gap(r$opinion[r$tick == 100]))


## -----------------------------------------------------------------------------
market <- abm_setup(agents = list(
  seller = abm_agents(n = 1, limit_price = 20, expected_price = 20,
                      transacted = FALSE, deal_price = NA_real_),
  buyer  = abm_agents(n = 2, limit_price = c(40, 35), expected_price = c(30, 25),
                      transacted = FALSE, deal_price = NA_real_)
))

go <- abm_go(
  abm_rules(transacted ~ FALSE, deal_price ~ NA_real_),

  abm_match(pair = "opposite_group", by = .group),

  abm_rules(
    transacted ~ case_when(
      .group == "seller" & partner_expected_price >= expected_price ~ TRUE,
      .group == "buyer"  & expected_price >= partner_expected_price ~ TRUE,
      TRUE ~ FALSE
    ),
    deal_price ~ if_else(
      (.group == "seller" & partner_expected_price >= expected_price) |
      (.group == "buyer"  & expected_price >= partner_expected_price),
      (expected_price + partner_expected_price) / 2, NA_real_
    )
  ),

  abm_rules(
    expected_price ~ case_when(
      .group == "seller" &  transacted ~ expected_price + 2,
      .group == "seller" & !transacted ~ pmax(expected_price - 1, limit_price),
      .group == "buyer"  &  transacted ~ expected_price - 1,
      .group == "buyer"  & !transacted ~ pmin(expected_price + 1, limit_price),
      TRUE ~ expected_price
    ),
    .scope = "population"
  )
)

result <- abm_run(market, go, ticks = 50, seed = 123)

deals <- result$deal_price[result$.group == "seller"]
round(c(trades = sum(!is.na(deals)),
        first  = deals[!is.na(deals)][1],
        last   = tail(deals[!is.na(deals)], 1)), 2)


## ----eval = FALSE-------------------------------------------------------------
# library(dplyr)
# library(ggplot2)
# 
# result |>
#   filter(.group == "seller") |>
#   select(tick, deal_price) |>
#   ggplot(aes(x = tick, y = deal_price)) +
#   geom_point() +
#   theme_minimal()


## -----------------------------------------------------------------------------
voter <- abm_setup(
  agents  = abm_agents(n = 100, opinion = ~sample(c(0, 1), n, replace = TRUE)),
  network = abm_network(type = "random", degree = 4)
)

go <- abm_go(
  abm_match(pair = "network"),
  abm_rules(opinion ~ partner_opinion)
)

r <- abm_run(voter, go, ticks = 200, seed = 8)

round(c(start = mean(r$opinion[r$tick == 0]),
        end   = mean(r$opinion[r$tick == 200])), 2)


## -----------------------------------------------------------------------------
pgg <- abm_setup(agents = abm_agents(
  n = 100, contribution = ~sample(c(0, 1), n, replace = TRUE), payoff = 0
))

go <- abm_go(
  abm_match(pair = "random", size = 4),
  abm_rules(payoff ~ sum(contribution) * 2 / 4)
)

r <- abm_run(pgg, go, ticks = 20, seed = 9)

table(r$payoff[r$tick == 20])


## -----------------------------------------------------------------------------
ipd <- abm_setup(
  agents  = abm_agents(n = 100, move = "cooperate", payoff = 0),
  network = abm_network(type = "random", degree = 1)
)

go <- abm_go(
  abm_match(pair = "network"),
  abm_rules(payoff ~ case_when(
    move == "cooperate" & partner_move == "cooperate" ~ 3,
    move == "defect"    & partner_move == "defect"    ~ 1,
    move == "defect"    & partner_move == "cooperate" ~ 5,
    move == "cooperate" & partner_move == "defect"    ~ 0
  )),
  abm_rules(move ~ partner_move)
)

r <- abm_run(ipd, go, ticks = 50, seed = 10)

table(r$move[r$tick == 50])


## -----------------------------------------------------------------------------
pa <- abm_setup(
  agents  = abm_agents(n = 2),
  network = abm_network(type = "manual", edges = data.frame(from = 1, to = 2))
)

go <- abm_go(
  abm_birth(n = 1, attach_via = abm_match(pair = "network", from = "random_edge"))
)

r <- abm_run(pa, go, ticks = 300, seed = 11)

deg <- table(c(abm_edges(r)$from, abm_edges(r)$to))
c(max_degree = max(deg), median_degree = median(deg))


## -----------------------------------------------------------------------------
reserve_ratio <- 0.1

bankres <- abm_setup(
  agents  = abm_agents(n = 100, wallet = ~runif(n, 0, 50), savings = 0, loan = 0,
                       draw = 0),
  globals = list(bank_deposits = 0, bank_loans = 0, bank_reserves = 0)
)

go <- abm_go(
  abm_match(pair = "random", role = list(giver = TRUE, receiver = TRUE)),
  abm_rules(gift ~ sample(c(0, 2, 5), 1)),
  abm_rules(wallet ~ if_else(.role == "giver", wallet - gift, wallet + partner_gift)),
  abm_rules(
    savings ~ if_else(wallet > 0, savings + wallet, savings - pmin(savings, abs(wallet))),
    wallet  ~ if_else(wallet > 0, 0, wallet + pmin(savings, abs(wallet)))
  ),
  abm_global(bank_deposits ~ sum(savings)),
  abm_sequential(
    draw          ~ if_else(wallet < 0 & bank_reserves > 0,
                            pmin(-wallet, bank_reserves), 0),
    loan          ~ loan + draw,
    wallet        ~ wallet + draw,
    bank_reserves ~ bank_reserves - draw
  ),
  abm_global(bank_loans    ~ sum(loan)),
  abm_global(bank_reserves ~ bank_deposits * reserve_ratio - bank_loans)
)

r <- abm_run(bankres, go, ticks = 100, seed = 13)

round(tail(abm_globals(r), 3), 2)

