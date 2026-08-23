# Model gallery

``` r

library(tidyABM)
```

These are the models tidyABM was designed against. Each one is here
because it demanded something the previous ones did not, so read them as
a sequence: the grammar grew to fit them, not the other way round.

## Simple economy

*Wilensky & Rand, ch. 2.* Money moves at random and inequality appears
anyway.

``` r

economy <- abm_setup(agents = abm_agents(n = 500, money = 100))

go <- abm_go(
  abm_match(pair = "random", role = list(giver = money > 0, receiver = TRUE)),
  abm_rules(money ~ if_else(.role == "giver", money - 1, money + 1))
)

r <- abm_run(economy, go, ticks = 500, seed = 1)

round(quantile(r$money[r$tick == 500]))
#>   0%  25%  50%  75% 100% 
#>   42   86  100  114  160
```

**What it introduced:** `role`. A transfer needs a direction, and the
two conditions in `role = list(giver = money > 0, receiver = TRUE)` say
which agent in each pair can take which side. A pair where neither can
give is dropped.

## El Farol

*Arthur, 1994, simplified.* Everyone wants to go to the bar unless it is
crowded, and their decisions are what make it crowded.

``` r

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
#>  [1]  60  44  88   0 100   0 100   0 100   0 100   0 100   0 100   0 100   0 100
#> [20]   0 100
```

**What it introduced:**
[`abm_global()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_global.md),
and the fact that matching is optional — this is the first model with no
pairing at all.

**A warning about this version.** With one shared predictor, the model
collapses into a two-cycle: attendance overshoots, everyone stays home,
attendance undershoots, everyone goes. Arthur’s result needs agents that
hold several candidate forecasts and switch to whichever has been
working. The mechanism above is the scaffolding, not the result —
[`vignette("corrections")`](https://rayhanalirachman.github.io/tidyABM/articles/corrections.md)
builds the working version.

## Prisoner’s dilemma, well mixed

Play, then copy whoever did better. Cooperation does not survive it.

``` r

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
#> [1] 0
```

**What it introduced:** multiple match phases in one tick. Playing and
imitating are two different pairings, written one after the other.

## Ethnocentrism

Cooperation with a cost of living, and agents that reproduce and die.

``` r

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
#> [1] 100  98  68   7
```

**What it introduced:**
[`abm_birth()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_birth.md)
and
[`abm_death()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_death.md),
as two more flat steps in the sequence rather than a special population
mechanism.

**A warning about this version.** The tag does no work: strategies are
unconditional, so cooperating is simply dominated and the population
runs itself down. Hammond and Axelrod’s agents decide *separately*
whether to cooperate with their own tag and with others — and even that
is not enough without local reproduction.
[`vignette("corrections")`](https://rayhanalirachman.github.io/tidyABM/articles/corrections.md)
builds both variants and shows why the second one matters.

## Rumour mill

A three-state diffusion where the rumour burns out before everyone hears
it.

``` r

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
#> 
#> ignorant  stifler 
#>       12      188
```

**What it introduced:** nothing new — which was the point. Pairwise
state comparison through `partner_*` columns is self-consistent by
construction, so it needs no `role`. Transfers of a conserved quantity
need `role`; symmetric state changes do not.

## Party

Segregation with no geography: agents move in opinion space until their
nearest neighbour is close enough.

``` r

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
#>       start         end 
#> 0.004968612 0.004968612
```

**What it introduced:** `pair = "nearest"`. Note that this pairing is
*directional* — your nearest neighbour need not have you as theirs — so
each agent gets its own `.group_id`, and `runif(1)` in the rule above is
drawn once per agent rather than once for the population.

## Market

*After Primer’s “Simulating Supply and Demand”.* Buyers and sellers with
private valuations haggle, and the price finds its own level.

``` r

market <- abm_setup(agents = list(
  buyers  = abm_agents(n = 200, wtp = ~rnorm(n, 50, 10), offer = ~wtp * 0.8),
  sellers = abm_agents(n = 200, wta = ~rnorm(n, 40, 10), ask   = ~wta * 1.2)
))

go <- abm_go(
  abm_match(pair = "opposite_group", by = .group, resolve = "negotiate",
            rounds = 5, positions = c(offer, ask), limits = c(wtp, wta)),
  abm_rules(offer ~ if_else(traded, offer * 0.98, offer * 1.02)),
  abm_rules(ask   ~ if_else(traded, ask * 1.02,   ask * 0.98))
)

r <- abm_run(market, go, ticks = 200, seed = 7)

price <- tapply(r$price, r$tick, function(z) mean(z, na.rm = TRUE))
round(price[c("1", "50", "200")], 2)
#>     1    50   200 
#> 42.95 43.80 47.91
```

**What it introduced:** several agent groups, and rule routing by column
existence. The `offer` rule mentions a column only buyers have, so it
applies only to buyers; the `ask` rule only to sellers. No `type ==`
test, no merging of the two tibbles.

## Voter model

Opinions spread along fixed connections rather than through the whole
population.

``` r

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
#> start   end 
#>  0.52  1.00
```

**What it introduced:**
[`abm_network()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_network.md)
and `pair = "network"`. The network is a separate edge list stored
alongside the agent tibble, not a list-column on it.

## Public goods game

Groups of four, a pot that grows and is split evenly whoever paid in.

``` r

pgg <- abm_setup(agents = abm_agents(
  n = 100, contribution = ~sample(c(0, 1), n, replace = TRUE), payoff = 0
))

go <- abm_go(
  abm_match(pair = "random", size = 4),
  abm_rules(payoff ~ sum(contribution) * 2 / 4)
)

r <- abm_run(pgg, go, ticks = 20, seed = 9)

table(r$payoff[r$tick == 20])
#> 
#>   0 0.5   1 1.5   2 
#>   4  32  40  16   8
```

**What it introduced:** `size`, generalising pairs to groups. Rules
reuse dplyr’s grouped-mutate semantics rather than adding
group-aggregate functions of their own, so `sum(contribution)` just
means what you would expect.

## Iterated prisoner’s dilemma

Same game, same partner every round, tit for tat. Cooperation survives.

``` r

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
#> 
#> cooperate 
#>       100
```

**What it introduced:** nothing. It is a composition of
`abm_network(degree = 1)` for fixed partnerships and the `partner_*`
convention, which already carries a partner’s previous move forward.
Memory of one round comes free; anything longer needs an explicit
column.

## Preferential attachment

*Wilensky & Rand, ch. 5.* The network grows, and hubs appear.

``` r

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
#>    max_degree median_degree 
#>            36             1
```

**What it introduced:** the only way the network changes during a run.
`from = "random_edge"` reuses NetLogo’s trick — pick an edge uniformly,
then one of its endpoints — so selection is degree-proportional without
anyone storing a degree.

## Zakah redistribution

Consumption erodes wealth; those above the *nisab* pay 2.5% into a pool;
those below the poverty line share it.

``` r

nisab <- 100
poverty_line <- 30

zakah <- abm_setup(
  agents  = abm_agents(n = 300, wealth = ~rlnorm(n, 4, 0.5),
                       income = ~rlnorm(n, 3, 0.4)),
  globals = list(zakah_pool = 0)
)

go <- abm_go(
  abm_rules(wealth ~ wealth + income - (0.6 * income + 0.02 * wealth)),
  abm_global(zakah_pool ~ sum(if_else(wealth > nisab, wealth * 0.025, 0))),
  abm_rules(wealth ~ if_else(wealth > nisab, wealth * 0.975, wealth)),
  abm_rules(wealth ~ if_else(wealth < poverty_line,
                             wealth + zakah_pool / sum(wealth < poverty_line),
                             wealth))
)

r <- abm_run(zakah, go, ticks = 50, seed = 12)

round(tail(abm_globals(r)$zakah_pool, 3), 1)
#> [1] 1374.0 1368.3 1365.0
```

**What it introduced:** nothing mechanically — it is the first model
made of nothing but individual and population-level steps, which
confirmed matching is optional machinery rather than a requirement.
Collection is modelled as a pool rather than peer-to-peer transfers,
both because that is how zakah institutions actually work and because it
sidesteps matching a mismatched number of payers and recipients.

**Watch the two thresholds — and the missing risk.** `nisab` and
`poverty_line` are deliberately different, so a middle group pays
nothing and receives nothing. But the recipient pool empties within
about ten ticks: the consumption rule is mean-reverting, so every
household converges on `20 * income` and nobody is persistently poor.
Zakah then becomes a pure drag on the payers. Check
`sum(wealth < poverty_line)` over the run before reading anything into
the inequality path;
[`vignette("corrections")`](https://rayhanalirachman.github.io/tidyABM/articles/corrections.md)
fixes it.

## Bank reserves

*Wilensky, NetLogo Sample Models.* A bank can only lend what it has, and
whoever asks first gets it.

``` r

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
#> # A tibble: 3 × 4
#>    tick bank_deposits bank_loans bank_reserves
#>   <dbl>         <dbl>      <dbl>         <dbl>
#> 1    98         3132.       314.         -0.6 
#> 2    99         3143.       314.          0.45
#> 3   100         3133.       314.         -0.98
```

**What it introduced:**
[`abm_sequential()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_sequential.md).
Every earlier model works fine with simultaneous updates, because a
shared pool that is only ever *divided* — the public goods pot, the
zakah pool — gives the same answer either way. Lendable reserves are
different: they are *depleted*, so the first borrower has to change what
the second one sees. The step is scoped narrowly to that case — during
the loop a rule reads and writes its own agent’s columns and any global,
and nothing else.

Note that the last
[`abm_sequential()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_sequential.md)
rule writes to a global. That is what makes the depletion visible to the
next agent; without it the reserve would only be recomputed at the end
of the tick and every agent would borrow against the same balance.

The first rule is doing something too. Rules inside
[`abm_sequential()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_sequential.md)
cascade — each one sees what the rule above it wrote — so the amount
actually drawn is worked out once, into `draw`, and the three rules that
move money all refer to it. Writing the condition out three times would
not work: the second rule brings `wallet` back to zero, and the third
would then find nothing left to test.
