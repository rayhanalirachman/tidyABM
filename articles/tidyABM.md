# Getting started with tidyABM

``` r

library(tidyABM)
```

An agent-based model is usually written as nested loops: for each tick,
for each agent, do something. tidyABM asks for a *specification*
instead. You declare what the population is and list what happens to it,
in order, and the package runs the loops.

Three things come out of that. The model reads as a description of the
mechanism rather than as bookkeeping. Mistakes in the structure are
caught when you build the model, not on tick 400. And the output is tidy
data, so analysing a run is ordinary dplyr.

## The three parts

Every model in this package is the same three functions, written as
three statements:

1.  **the world**,
    [`abm_setup()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_setup.md).
    The agents, the network, the globals.
2.  **the tick**,
    [`abm_go()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_go.md).
    The ordered steps replayed once per tick.
3.  **the run**,
    [`abm_run()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_run.md).
    The two above, plus `ticks`, `seed` and `record`.

``` r

world  <- abm_setup(...)                                # 1
go     <- abm_go(...)                                   # 2
result <- abm_run(world, go, ticks = ..., seed = ...)   # 3
```

The first two return objects you can print and inspect, so every example
below builds them on their own lines rather than inside the call to
[`abm_run()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_run.md).
That is what lets you reuse one go block across several worlds, or run
one world through several go blocks.

Everything else sits inside one of the three:
[`abm_agents()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_agents.md)
and
[`abm_network()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_network.md)
are what you hand to
[`abm_setup()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_setup.md),
and every other `abm_*()` function is a step inside
[`abm_go()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_go.md).

## The whole game

Wilensky and Rand’s *Simple Economy* is the smallest interesting model
there is. Five hundred agents start with \$100 each. Every tick,
everyone who has money gives \$1 to someone else. Nothing else happens.

``` r

# 1. the world
economy <- abm_setup(agents = abm_agents(n = 500, money = 100))
economy
#> <abm_model> 500 agents in 1 group
#> • agents: 500 agents ["money"]
```

[`abm_setup()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_setup.md)
declares the world.
[`abm_agents()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_agents.md)
declares one group of agents inside it. A plain value like `money = 100`
is recycled across every agent.

Now the behaviour:

``` r

# 2. the tick
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
[`abm_match()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_match.md)
shuffles the population into pairs and works out who in each pair is the
giver. The `role` list says the giver must satisfy `money > 0` and the
receiver can be anyone, so a pair where neither agent has money is
dropped for this tick. Then
[`abm_rules()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_rules.md)
moves the dollar.

``` r

# 3. the run
result <- abm_run(economy, go, ticks = 500, seed = 1)
#> Running model ■■■■■■                            17% | ETA:  5s
#> Running model ■■■■■■■■                          25% | ETA:  4s
#> Running model ■■■■■■■■■■■■■■■■■■■■■■■■          77% | ETA:  1s
#> Running model ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
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
[`abm_setup()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_setup.md)
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

Money is conserved, because no rule creates or destroys any. The spike
at \$100 is gone, though, and a summary is a poor way to look at a
distribution:

``` r

library(ggplot2)

ggplot(data.frame(money = end), aes(money)) +
  geom_histogram(binwidth = 10, boundary = 0) +
  labs(x = "money at tick 500", y = "agents")
```

![Histogram of agent wealth after 500 ticks: a broad, roughly symmetric
spread centred near 100
dollars.](tidyABM_files/figure-html/wealth-histogram-1.png)

Five hundred ticks of a fair coin have spread the population out, but
the shape is still roughly symmetric. The exponential distribution this
model is known for arrives much later, and the reason is worth knowing:
the tail comes from the floor at zero, and the floor cannot bind until
the spread is comparable to what everyone started with. After *t* ticks
of \$1 steps that spread is about `sqrt(t)`, so \$100 of starting money
needs some ten thousand ticks, not five hundred. Run it that far and the
right tail is unmistakable. It is also a minute of compute, which is why
this vignette stops here.

## Setup: agents, networks, globals

[`abm_agents()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_agents.md)
takes `n` and any number of named columns. A plain value recycles. A
one-sided formula is evaluated once and must return `n` values, or one
value to recycle. Formulas run in order, so a later column can use an
earlier one:

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

Values shared by the whole population go in `globals`: a bar’s last
attendance, a bank’s ledger. They are readable inside every rule:

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

[`abm_go()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_go.md)
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
[`abm_match()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_match.md)
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

rumour_go <- abm_go(
  abm_match(pair = "random"),
  abm_rules(state ~ case_when(
    state == "ignorant" & partner_state == "spreader" ~ "spreader",
    state == "spreader" & partner_state == "spreader" ~ "stifler",
    state == "spreader" & partner_state == "stifler"  ~ "stifler",
    TRUE ~ state
  ))
)

r <- abm_run(rumour, rumour_go, ticks = 100, seed = 1)

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

pgg_go <- abm_go(
  abm_match(pair = "random", size = 4),
  abm_rules(payoff ~ sum(contribution) * 2 / 4)
)

r <- abm_run(pgg, pgg_go, ticks = 10, seed = 1)

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
[`abm_rules()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_rules.md)
call sees the state at the *start* of the step. That is the synchronous
update agent-based models normally assume, and it is the one place
[`abm_rules()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_rules.md)
deliberately differs from
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html):

``` r

swap <- abm_setup(agents = abm_agents(n = 3, a = 1, b = 2))
swap_go <- abm_go(abm_rules(a ~ b, b ~ a))

abm_run(swap, swap_go, ticks = 1)
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
[`abm_sequential()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_sequential.md)
processes agents one at a time in shuffled order, and its writes to
globals are visible to the agents after it:

``` r

pot <- abm_setup(agents = abm_agents(n = 20, got = 0), globals = list(pot = 10))

pot_go <- abm_go(abm_sequential(
  got ~ if_else(pot > 0, 1, 0),
  pot ~ if_else(pot > 0, pot - 1, pot)
))

seq_run <- abm_run(pot, pot_go, ticks = 1, seed = 1)

sum(seq_run$got[seq_run$tick == 1])  # exactly the 10 units that existed
#> [1] 10
```

With
[`abm_rules()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_rules.md)
instead, all twenty agents would see a full pot and all twenty would be
served. Use
[`abm_sequential()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_sequential.md)
only when that difference is the point: it is slower and harder to
reason about.

The order is a fresh shuffle unless you name one. When the order is
itself part of the model, say a queue at a counter or a
sequential-service constraint, pass `.order =` and the agents go through
in its ascending order instead:

``` r

queue <- abm_setup(agents = abm_agents(n = 20, place = ~sample(n), got = 0),
                   globals = list(pot = 10))

queue_go <- abm_go(abm_sequential(
  got ~ if_else(pot > 0, 1, 0),
  pot ~ if_else(pot > 0, pot - 1, pot),
  .order = place
))

q <- abm_run(queue, queue_go, ticks = 1, seed = 1)
sum(q$got[q$tick == 1 & q$place <= 10])   # the front of the queue, every time
#> [1] 10
```

## Repeating a block

A tick is one pass through
[`abm_go()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_go.md).
Some models have a *phase* inside the tick that has to finish first: an
epidemic that burns out before anyone reconsiders vaccinating, a round
of proposals that runs until nobody is rejected.
[`abm_repeat()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_repeat.md)
replays a block until a condition holds:

``` r

grow <- abm_setup(agents = abm_agents(n = 5, x = 0))
grow_go <- abm_go(
  abm_repeat(abm_rules(x ~ x + 1), until = mean(x) >= 4, max = 100)
)

r <- abm_run(grow, grow_go, ticks = 1, seed = 1)
unique(r$x[r$tick == 1])
#> [1] 4
```

`until` is checked after each pass, so the block always runs at least
once, and `max` is required, because a condition that never becomes true
would hang the run.

## Neighbourhoods

A match gives an agent one partner.
[`abm_neighbours()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_neighbours.md)
gives it an aggregate over the agents *around* it, which is what a model
needs when the question is “how many of them” rather than “what did mine
do”. Each rule is `column ~ aggregate`, evaluated over the neighbours’
rows, with the focal agent’s own columns visible as `own_<col>` so that
comparisons are expressible:

``` r

town <- abm_setup(
  agents  = abm_agents(n = 40, wealth = ~round(runif(n, 0, 100))),
  network = abm_network(type = "poisson", degree = 4),
  seed    = 1
)
town_go <- abm_go(
  abm_neighbours(richer ~ sum(wealth > own_wealth))
)

r <- abm_run(town, town_go, ticks = 1, seed = 1)
table(r$richer[r$tick == 1], useNA = "ifany")
#> 
#>    0    1    2    3    4    5    6 <NA> 
#>    9    9    8    5    2    4    2    1
```

The neighbourhood is the network by default. `within =` makes it a
neighbourhood in *attribute space* instead, meaning everybody whose
columns satisfy a condition, network or no network. Hegselmann and
Krause’s bounded confidence model is one step:

``` r

crowd <- abm_setup(agents = abm_agents(n = 200, opinion = ~runif(n)),
                   globals = list(eps = 0.15), seed = 42)
crowd_go <- abm_go(
  abm_neighbours(opinion ~ mean(opinion),
                 within = abs(opinion - own_opinion) <= eps)
)

hk <- abm_run(crowd, crowd_go, ticks = 30)
round(sort(unique(round(hk$opinion[hk$tick == 30], 4))), 3)
#> [1] 0.190 0.478 0.752
```

One difference is worth knowing: an agent is inside its own *attribute*
neighbourhood whenever the condition holds of it, since “the mean
opinion of everyone I take seriously” includes its own, and never inside
its own *network* one. Write `within = ... & .id != own_.id` if you want
it out.

Two
[`abm_neighbours()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_neighbours.md)
passes draw their random numbers independently, which is right when each
is a separate event and wrong when it is one event seen from two sides.
[`abm_draw()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_draw.md)
puts the draw on the edge instead, where both endpoints read the same
number:

``` r

pairs <- abm_setup(agents = abm_agents(n = 30, met = 0L),
                   network = abm_network(type = "poisson", degree = 4),
                   seed = 2)
pairs_go <- abm_go(
  abm_draw(happened ~ runif(n()) < 0.5),
  abm_neighbours(met ~ sum(happened))
)

r <- abm_run(pairs, pairs_go, ticks = 1, seed = 1)
# every meeting is counted by both of the agents in it, so the total is even
sum(r$met[r$tick == 1], na.rm = TRUE)
#> [1] 58
```

`.each = "endpoint"` gives the two ends one draw each instead, read as
`name` from an agent’s own side and `name_back` from the other, which is
what an asymmetric interaction needs, since whether I noticed you and
whether you noticed me are different questions about the same edge.

## Changing the population

[`abm_birth()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_birth.md)
and
[`abm_death()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_death.md)
are the only steps that add or remove agents.

``` r

ethno <- abm_setup(agents = abm_agents(
  n = 100,
  strategy = ~sample(c("cooperate", "defect"), n, replace = TRUE),
  resource = 10
))

ethno_go <- abm_go(
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

r <- abm_run(ethno, ethno_go, ticks = 20, seed = 1)

table(r$tick)[c(1, 11, 21)]
#> 
#>   0  10  20 
#> 100 106  73
```

`cost` says what reproduction costs, as ordinary `column ~ expression`
formulas applied to the parent and the newborn alike, so halving a
resource splits it between them.

One parent, one offspring, unless `times` says otherwise. It takes an
expression evaluated in the parent’s row, so a fertility can be a
column, a number, or a draw:

``` r

seeds <- abm_setup(agents = abm_agents(n = 10, age = 5), seed = 1)
seeds_go <- abm_go(
  abm_birth(when = age > 0, times = rpois(n(), 2), inherit = age ~ 0)
)

r <- abm_run(seeds, seeds_go, ticks = 1, seed = 1)
table(r$tick)
#> 
#>  0  1 
#> 10 31
```

Each offspring is a row of its own before `inherit` is evaluated, so a
mutation drawn there differs from sibling to sibling rather than being
drawn once and copied.

In a model with a network, a newborn is nowhere until `attach_via` puts
it somewhere: an `abm_match(pair = "network")` spec that names who it
attaches to, where `from = "parent"` is next to the agent it was cloned
from and `from = "random_edge"` is degree-proportional. `links` says how
many edges it arrives with, one by default. That default is worth
thinking about whenever agents also die, because a death prunes a full
degree’s worth of edges and a one-edge birth puts back a single link,
which erodes the network over a long run. With `from = "parent"`,
`links` takes the parent and a sample of the parent’s own neighbours, so
the newborn arrives in a neighbourhood rather than on the end of a
single thread.

## Reproducibility

Agent-based models are stochastic, so `seed` is an argument to
[`abm_run()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_run.md)
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
[`abm_run()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_run.md)‘s
seed fixes the *run*, not the *model*. If your agents’ starting columns
are random, they were drawn when
[`abm_setup()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_setup.md)
was called, before
[`abm_run()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_run.md)
had a chance to set anything, so two experiments built the same way can
start from different populations:

``` r

random_pop <- function() abm_setup(agents = abm_agents(n = 50, x = ~runif(n)))
noop_go <- abm_go(abm_rules(x ~ x))

set.seed(1); one <- abm_run(random_pop(), noop_go, ticks = 1, seed = 5)
set.seed(2); two <- abm_run(random_pop(), noop_go, ticks = 1, seed = 5)
identical(one$x, two$x)
#> [1] FALSE
```

Seed both, and the whole experiment reproduces:

``` r

experiment <- function() {
  m <- abm_setup(agents = abm_agents(n = 50, x = ~runif(n)), seed = 4)
  noop_go <- abm_go(abm_rules(x ~ x))
  abm_run(m, noop_go, ticks = 1, seed = 5)
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

## How much to record

By default
[`abm_run()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_run.md)
keeps every agent of every tick, which is right for a fixed population
and wrong for a growing one. A run that ends with fifty thousand agents
has been keeping every one of them since the start. `record` says how
much to keep:

``` r

sizes <- c(
  all      = nrow(abm_run(economy, go, ticks = 20, record = "all")),
  every_5  = nrow(abm_run(economy, go, ticks = 20, record = 5)),
  final    = nrow(abm_run(economy, go, ticks = 20, record = "final")),
  globals  = nrow(abm_run(economy, go, ticks = 20, record = "globals"))
)
sizes
#>     all every_5   final globals 
#>   10500    2500     500       0
```

A whole number keeps every *n*th tick plus the two ends. `"final"` keeps
the last tick only, and `"globals"` keeps none of the populations.
Globals are recorded every tick whatever you say, since they are one row
each, so `"globals"` is the setting for a model whose output is an
aggregate. The run itself is unchanged either way: the same seed gives
the same final state at any setting.

## Where to go next

[`vignette("models")`](https://rayhanalirachman.github.io/tidyABM/articles/models.md)
works through the models the package was designed against, each with the
mechanism it demonstrates.
