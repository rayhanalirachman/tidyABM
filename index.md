# tidyABM

Agent-based models, written as a specification rather than a loop.

tidyABM keeps NetLogo’s two-block structure — a `setup` that declares
the world and a `go` that runs once per tick — but replaces the
imperative `ask turtles` body with an ordered sequence of typed steps.
Agents live in a tibble, rules are dplyr-style formulas, and the output
is tidy data.

## Installation

``` r

# install.packages("pak")
pak::pak("rayhanalirachman/tidyABM")
```

## Three parts

Every tidyABM model is written as the same three components, in the same
order, each one a named object you can print, reuse and pass around:

| part | function | what it is |
|----|----|----|
| 1\. the agents | [`abm_agents()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_agents.md) — inside [`abm_setup()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_setup.md) | who is in the model and what they start with |
| 2\. the go block | [`abm_go()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_go.md) | the ordered steps replayed once per tick |
| 3\. the run | [`abm_run()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_run.md) | the two above, plus `ticks`, `seed` and `record` |

Keep them as three statements.
[`abm_go()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_go.md)
is a first-class object, not an argument you build inside the call to
[`abm_run()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_run.md),
and writing it that way is what lets you print it, validate it once,
reuse one go block across several populations, or swap one population
through several go blocks.

``` r

agents <- abm_setup(agents = abm_agents(...))   # 1
go     <- abm_go(...)                           # 2
result <- abm_run(agents, go, ticks = ..., seed = ...)   # 3
```

## A whole model

Wilensky and Rand’s *Simple Economy*: 500 agents start with \$100 each,
and every tick anyone with money gives \$1 to someone else. The wealth
distribution starts as a spike and ends up exponential — no agent is
doing anything clever, and inequality appears anyway. The three parts
are numbered in the comments.

``` r

library(tidyABM)

# 1. the agents
economy <- abm_setup(agents = abm_agents(n = 500, money = 100))

# 2. the go block
go <- abm_go(
  abm_match(pair = "random", role = list(giver = money > 0, receiver = TRUE)),
  abm_rules(money ~ if_else(.role == "giver", money - 1, money + 1))
)

# 3. the run
result <- abm_run(economy, go, ticks = 1000, seed = 1)
result
#> <abm_result> 1000 ticks, 500 agents seen, 500500 rows
#> # A tibble: 500,500 x 4
#>     tick   .id .group money
#>    <int> <int> <chr>  <dbl>
#>  1     0     1 agents   100
#>  2     0     2 agents   100
#>  ...
```

`result` is a plain long tibble, so the analysis is ordinary dplyr and
ggplot2.

## The grammar

Setup declares the world:

| function | what it declares |
|----|----|
| [`abm_agents()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_agents.md) | how many agents, and what columns they start with |
| [`abm_network()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_network.md) | a persistent set of connections between them |
| [`abm_setup()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_setup.md) | the whole world, plus any shared globals |

[`abm_go()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_go.md)
declares what happens each tick, as a sequence of steps dispatched by
type and position rather than by argument name:

| step | what it does |
|----|----|
| [`abm_match()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_match.md) | decides who interacts with whom this tick |
| [`abm_rules()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_rules.md) | updates agent columns, all agents simultaneously |
| [`abm_sequential()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_sequential.md) | updates them one at a time, in shuffled order or an order you name |
| [`abm_neighbours()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_neighbours.md) | summarises each agent’s neighbourhood — the network, or everyone `within` a condition |
| [`abm_draw()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_draw.md) | attaches a value to every edge, readable from both ends |
| [`abm_tell()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_tell.md) | writes a value into *another* agent’s row |
| [`abm_global()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_global.md) | updates a value shared by the whole population, or a table of them indexed by a category |
| [`abm_birth()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_birth.md) | adds agents, one or `times` many |
| [`abm_death()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_death.md) | removes them |
| [`abm_link()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_link.md) / [`abm_unlink()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_unlink.md) | adds or removes network edges |
| [`abm_repeat()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_repeat.md) | replays a block of steps until a condition holds |

So a two-phase model — play, then imitate — is written flat and in
order:

``` r

go <- abm_go(
  abm_match(pair = "random"),
  abm_rules(payoff ~ case_when(...)),
  abm_match(pair = "random"),
  abm_rules(strategy ~ if_else(partner_payoff > payoff, partner_strategy, strategy))
)
```

The sequence is checked once, when
[`abm_go()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_go.md)
is called, rather than on every tick.

## What the matching modes give you

After `abm_match(pair = ..., size = 2)` every rule can see
`partner_<col>` for each of the partner’s columns. With `size > 2` rules
are grouped instead, so `sum(contribution)` inside a rule means “sum
across this agent’s group”. A rule can also be grouped by an ordinary
agent column with `.by =`, which is how a firm, a household or a team
becomes a thing the model can add up.

| mode | partner is… |  |
|----|----|----|
| `"random"` | drawn from a fresh random partition of the population | mutual |
| `"opposite_group"` | from the other side of a two-valued split | mutual |
| `"one_of"` | drawn from the whole population (NetLogo’s `one-of other turtles`) | directional |
| `"nearest"` | the closest agent in the space you name, or the one that minimises a `cost` you write | directional |
| `"network"` | one of the agent’s neighbours in the model network | directional |

*Mutual* modes partition the eligible agents, so being matched is
symmetric. *Directional* modes give each agent a partner of its own, and
your partner need not have picked you.

## Reproducibility

`abm_run(..., seed =)` sets the seed for the whole run and restores your
session’s random state afterwards, so a model is reproducible without
you having to arrange it.

`abm_run(..., record =)` says how much to keep — every tick, every *n*th
tick, the last one, or none of the populations at all. Globals are
recorded every tick regardless. A fixed population can ignore it; a
growing one cannot, because recording every agent of every tick is what
makes such a run die of memory rather than merely take a while.

## Status

Experimental. The API is shaped by what porting models turns up, and it
is still moving.

Fifty-six models are implemented and documented in
[`models/`](https://rayhanalirachman.github.io/tidyABM/models/README.md),
each on its own page with its code, the result it reproduces and its
source: thirteen the grammar was designed against, three of those
rebuilt because the short version does not show what the model is known
for, and four rounds of ten ported as stress tests.

Every feature in the package that is not in the founding thirteen exists
because one of those forty models asked for it —
[`abm_link()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_link.md),
[`abm_neighbours()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_neighbours.md),
[`abm_tell()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_tell.md),
[`abm_draw()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_draw.md),
[`abm_repeat()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_repeat.md),
the `one_of` pairing mode, `among =`, `cost =`, `within =`, `.by =`,
`.order =`, `times =`, `record =`, `.scope = "population"`, clique
linking, the Poisson and scale-free and ring networks, and the cascading
semantics of
[`abm_sequential()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_sequential.md).
[`models/what-changed.md`](https://rayhanalirachman.github.io/tidyABM/models/what-changed.md)
is the history;
[`models/open-items.md`](https://rayhanalirachman.github.io/tidyABM/models/open-items.md)
is what is still out of reach.

Several validations are quantitative rather than qualitative. Hawks and
Doves matches the analytic evolutionarily stable frequency `V/C` to
three decimals across a twelve-fold range of the cost parameter; the
information cascade matches `p² / (p² + (1-p)²)` at five values of the
signal accuracy; Deffuant’s cluster count matches `floor(1/(2d))` at six
values of the threshold; Watts’s cascade window matches its closed-form
boundaries at both ends; the Minority Game reproduces the σ²/α curve
with its minimum at the reported critical value; and the Naming Game’s
two scaling exponents come out at 1.45 and 1.39 against a reported 3/2.
The fourth round adds three more: the division of labour settles at δ/α
to two decimals whatever the thresholds are, the neutral model’s variant
counts land on the Ewens sampling formula across two orders of magnitude
in the innovation rate, and deferred acceptance reproduces Pittel’s
`ln n` and `n / ln n` on the nose at n = 200.

See
[`vignette("tidyABM")`](https://rayhanalirachman.github.io/tidyABM/articles/tidyABM.md)
to get started,
[`vignette("models")`](https://rayhanalirachman.github.io/tidyABM/articles/models.md)
for a tour of the grammar, and
[`vignette("corrections")`](https://rayhanalirachman.github.io/tidyABM/articles/corrections.md)
for the three models whose short form runs but is wrong.
