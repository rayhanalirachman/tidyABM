# Getting started with tidyABM

``` r

library(tidyABM)
```

An agent-based model is usually written as nested loops: for each tick,
for each agent, do something. tidyABM asks you to write the model as a
*specification* instead — a declaration of what the population is and an
ordered list of what happens to it — and runs the loops for you.

That buys three things. The model reads as a description of the
mechanism rather than as bookkeeping; mistakes in the structure are
caught once, when you build the model, rather than on tick 400; and the
output is tidy data, so analysing a run is ordinary dplyr.

## The whole game

Wilensky and Rand’s *Simple Economy* is the smallest interesting model
there is. Five hundred agents start with \$100 each. Every tick,
everyone who has money gives \$1 to someone else. Nothing else happens.

``` r

economy <- abm_setup(agents = abm_agents(n = 500, money = 100))
economy
#> <abm_model> 500 agents in 1 group
#> • agents: 500 agents ["money"]
```

[`abm_setup()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_setup.md)
declares the world;
[`abm_agents()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_agents.md)
declares one group of agents within it. A plain value like `money = 100`
is recycled across every agent.

Now the behaviour:

``` r

go <- abm_go(
  abm_match(pair = "random", role = list(giver = money > 0, receiver = TRUE)),
  abm_rules(money ~ if_else(.role == "giver", money - 1, money + 1))
)
go
#> <abm_go> 2 steps, 1 match phase
#> 1. match random
#> 2. rules 1 rule(s)
```

Two steps.
[`abm_match()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_match.md)
shuffles the population into pairs and works out who in each pair is the
giver — the `role` list says the giver must satisfy `money > 0` and the
receiver can be anyone, so a pair where neither agent has money is
dropped for this tick.
[`abm_rules()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_rules.md)
then moves the dollar.

``` r

result <- abm_run(economy, go, ticks = 500, seed = 1)
result
#> <abm_result> 500 ticks, 500 agents seen, 250500 rows
#> # A tibble: 250,500 × 4
#>     tick   .id .group money
#>    <int> <int> <chr>  <dbl>
#>  1     0     1 agents   100
#>  2     0     2 agents   100
#>  3     0     3 agents   100
#>  4     0     4 agents   100
#>  5     0     5 agents   100
#>  6     0     6 agents   100
#>  7     0     7 agents   100
#>  8     0     8 agents   100
#>  9     0     9 agents   100
#> 10     0    10 agents   100
#> # ℹ 250,490 more rows
```

The result is one row per agent per tick. Tick 0 is the state
[`abm_setup()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_setup.md)
produced, before anything has run.

``` r

start <- result$money[result$tick == 0]
end   <- result$money[result$tick == 500]

c(total_start = sum(start), total_end = sum(end))
#> total_start   total_end 
#>       50000       50000
round(summary(end))
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>      42      86     100     100     114     160
```

Money is conserved — no rule creates or destroys any — but the
distribution has gone from a spike at 100 to something with a long right
tail. That is the whole point of the model.

## Setup: agents, networks, globals

[`abm_agents()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_agents.md)
takes `n` and any number of named columns. A plain value recycles; a
one-sided formula is evaluated once and must return `n` values (or one,
to recycle). Formulas run in order, so a later column can use an earlier
one:

``` r

abm_agents(
  n = 200,
  wtp   = ~rnorm(n, 50, 10),
  offer = ~wtp * 0.8
)
#> <abm_agents> 200 agents
#> • wtp = `~rnorm(n, 50, 10)`
#> • offer = `~wtp * 0.8`
```

A model can have several kinds of agent, declared as a named list. Each
keeps its own columns:

``` r

market <- abm_setup(agents = list(
  buyers  = abm_agents(n = 5, wtp = ~rnorm(n, 50, 10), offer = ~wtp * 0.8),
  sellers = abm_agents(n = 5, wta = ~rnorm(n, 40, 10), ask   = ~wta * 1.2)
))
market
#> <abm_model> 10 agents in 2 groups
#> • buyers: 5 agents ["wtp" and "offer"]
#> • sellers: 5 agents ["wta" and "ask"]
```

Agent ids are unique across the whole model, and every agent carries a
`.group` column naming its group.

Values shared by the whole population — a bar’s last attendance, a
bank’s ledger — go in `globals`. They are readable inside every rule:

``` r

elfarol <- abm_setup(
  agents  = abm_agents(n = 100, threshold = ~runif(n, 40, 80)),
  globals = list(last_attendance = 60)
)
```

A network is an edge list built once and kept for the whole run:

``` r

abm_network(type = "random", degree = 4)
#> <abm_network> type "random"
#> • degree = 4
```

`type = "random"` gives every agent *exactly* `degree` neighbours, using
[`igraph::sample_k_regular()`](https://r.igraph.org/reference/sample_k_regular.html).
`degree = 1` therefore pairs the whole population off permanently.

## Go: a sequence of steps

[`abm_go()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_go.md)
takes steps in order and dispatches them by type, not by argument name.
That means a model with several phases per tick is written flat:

``` r

pd_go <- abm_go(
  # phase 1: play
  abm_match(pair = "random"),
  abm_rules(payoff ~ case_when(
    strategy == "cooperate" & partner_strategy == "cooperate" ~ 3,
    strategy == "defect"    & partner_strategy == "defect"    ~ 1,
    strategy == "defect"    & partner_strategy == "cooperate" ~ 5,
    strategy == "cooperate" & partner_strategy == "defect"    ~ 0
  )),
  # phase 2: imitate whoever did better
  abm_match(pair = "random"),
  abm_rules(strategy ~ if_else(partner_payoff > payoff, partner_strategy, strategy))
)
pd_go
#> <abm_go> 4 steps, 2 match phases
#> 1. match random
#> 2. rules 1 rule(s)
#> 3. match random
#> 4. rules 1 rule(s)
```

The sequence is checked when you build it. Two matches in a row, or a
sequence ending on a match, means a pairing nobody uses, and that is an
error rather than a silent waste:

``` r

abm_go(
  abm_match(pair = "random"),
  abm_match(pair = "random"),
  abm_rules(x ~ 1)
)
#> Error in `abm_go()`:
#> ! Two `abm_match()` steps in a row.
#> ✖ Step 1 and step 2 are both `abm_match()`.
#> ℹ The first match would be discarded unused. Put an update step between them.
```

Matching is optional. A model where every agent decides alone needs no
[`abm_match()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_match.md)
at all:

``` r

farol_go <- abm_go(
  abm_rules(go_today ~ last_attendance < threshold),
  abm_global(last_attendance ~ sum(go_today))
)
abm_run(elfarol, farol_go, ticks = 5, seed = 1) |> abm_globals()
#> # A tibble: 6 × 2
#>    tick last_attendance
#>   <int>           <dbl>
#> 1     0              60
#> 2     1              53
#> 3     2              73
#> 4     3              15
#> 5     4             100
#> 6     5               0
```

## What a match gives a rule

For a pair, every rule downstream of the match can see `partner_<col>`
for each of the partner’s columns:

``` r

rumour <- abm_setup(
  agents = abm_agents(n = 100, state = ~c("spreader", rep("ignorant", n - 1)))
)

r <- abm_run(rumour, abm_go(
  abm_match(pair = "random"),
  abm_rules(state ~ case_when(
    state == "ignorant" & partner_state == "spreader" ~ "spreader",
    state == "spreader" & partner_state == "spreader" ~ "stifler",
    state == "spreader" & partner_state == "stifler"  ~ "stifler",
    TRUE ~ state
  ))
), ticks = 100, seed = 1)

table(r$state[r$tick == 100])
#> 
#> ignorant  stifler 
#>        8       92
```

For a group, rules are evaluated group by group, so an aggregate inside
a rule means “across this agent’s group”. The public goods game is one
line because of it:

``` r

pgg <- abm_setup(agents = abm_agents(
  n = 100, contribution = ~sample(c(0, 1), n, replace = TRUE), payoff = 0
))

r <- abm_run(pgg, abm_go(
  abm_match(pair = "random", size = 4),
  abm_rules(payoff ~ sum(contribution) * 2 / 4)
), ticks = 10, seed = 1)

table(r$payoff[r$tick == 10])
#> 
#> 0.5   1 1.5   2 
#>  48  20  28   4
```

Grouping is also what makes randomness inside a rule behave: with a
match standing, `sample(x, 1)` is drawn once per pair, not once for
everybody.

## Simultaneous or sequential

Every rule in one
[`abm_rules()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_rules.md)
call sees the state at the *start* of the step. That is the synchronous
update agent-based models normally assume, and it is the one place
[`abm_rules()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_rules.md)
deliberately differs from
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html):

``` r

swap <- abm_setup(agents = abm_agents(n = 3, a = 1, b = 2))
abm_run(swap, abm_go(abm_rules(a ~ b, b ~ a)), ticks = 1)
#> <abm_result> 1 tick, 3 agents seen, 6 rows
#> # A tibble: 6 × 5
#>    tick   .id .group     a     b
#>   <int> <int> <chr>  <dbl> <dbl>
#> 1     0     1 agents     1     2
#> 2     0     2 agents     1     2
#> 3     0     3 agents     1     2
#> 4     1     1 agents     2     1
#> 5     1     2 agents     2     1
#> 6     1     3 agents     2     1
```

Sometimes that is wrong. If agents compete for a pool that gets *used
up*, the first agent to act has to change what the next one sees.
[`abm_sequential()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_sequential.md)
processes agents one at a time in shuffled order, and its writes to
globals are visible to the agents after it:

``` r

pot <- abm_setup(agents = abm_agents(n = 20, got = 0), globals = list(pot = 10))

seq_run <- abm_run(pot, abm_go(abm_sequential(
  got ~ if_else(pot > 0, 1, 0),
  pot ~ if_else(pot > 0, pot - 1, pot)
)), ticks = 1, seed = 1)

sum(seq_run$got[seq_run$tick == 1])  # exactly the 10 units that existed
#> [1] 10
```

With
[`abm_rules()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_rules.md)
instead, all twenty agents would see a full pot and all twenty would be
served. Use
[`abm_sequential()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_sequential.md)
only when that difference is the point: it is slower and harder to
reason about.

## Changing the population

[`abm_birth()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_birth.md)
and
[`abm_death()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_death.md)
are the only steps that add or remove agents.

``` r

ethno <- abm_setup(agents = abm_agents(
  n = 100,
  strategy = ~sample(c("cooperate", "defect"), n, replace = TRUE),
  resource = 10
))

r <- abm_run(ethno, abm_go(
  abm_match(pair = "random"),
  abm_rules(resource ~ case_when(
    strategy == "cooperate" & partner_strategy == "cooperate" ~ resource + 2,
    strategy == "defect"    & partner_strategy == "cooperate" ~ resource + 4,
    strategy == "cooperate" & partner_strategy == "defect"    ~ resource - 1,
    TRUE ~ resource
  ) - 1),
  abm_birth(when = resource > 20, cost = resource ~ resource / 2),
  abm_death(when = resource <= 0)
), ticks = 20, seed = 1)

table(r$tick)[c(1, 11, 21)]
#> 
#>   0  10  20 
#> 100 110  82
```

`cost` says what reproduction costs, as ordinary `column ~ expression`
formulas applied to the parent and the newborn alike — halving a
resource splits it between them.

## Reproducibility

Agent-based models are stochastic, so `seed` is an argument to
[`abm_run()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_run.md)
rather than something you arrange yourself. It is set locally, so your
session’s random state is untouched:

``` r

set.seed(1); before <- runif(1)
invisible(abm_run(economy, go, ticks = 5, seed = 99))
set.seed(1); after <- runif(1)
identical(before, after)
#> [1] TRUE
```

Two runs of the same model with the same seed are identical:

``` r

a <- abm_run(economy, go, ticks = 20, seed = 42)
b <- abm_run(economy, go, ticks = 20, seed = 42)
identical(as.data.frame(a), as.data.frame(b))
#> [1] TRUE
```

There is a sharp edge here worth knowing about.
[`abm_run()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_run.md)‘s
seed fixes the *run*, not the *model*. If your agents’ starting columns
are random, they were drawn when
[`abm_setup()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_setup.md)
was called — before
[`abm_run()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_run.md)
had a chance to set anything — so two experiments built the same way can
start from different populations:

``` r

random_pop <- function() abm_setup(agents = abm_agents(n = 50, x = ~runif(n)))
set.seed(1); one <- abm_run(random_pop(), abm_go(abm_rules(x ~ x)), 1, seed = 5)
set.seed(2); two <- abm_run(random_pop(), abm_go(abm_rules(x ~ x)), 1, seed = 5)
identical(one$x, two$x)
#> [1] FALSE
```

Seed both, and the whole experiment reproduces:

``` r

experiment <- function() {
  m <- abm_setup(agents = abm_agents(n = 50, x = ~runif(n)), seed = 4)
  abm_run(m, abm_go(abm_rules(x ~ x)), ticks = 1, seed = 5)
}
set.seed(1); one <- experiment()
set.seed(2); two <- experiment()
identical(one$x, two$x)
#> [1] TRUE
```

One further caveat: a seed fixes a run given a version of the package,
not across versions. Reordering an internal random draw changes the
exact numbers without changing the model, so compare runs by their
statistical behaviour rather than by exact equality when you upgrade.

## Where to go next

[`vignette("models")`](https://rayhanalirachman.github.io/tidyabm/articles/models.md)
works through the models the package was designed against, each with the
mechanism it demonstrates.
