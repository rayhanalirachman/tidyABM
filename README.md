# tidyABM

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Agent-based models, written as a specification rather than a loop.

tidyABM keeps NetLogo's `setup` / `go` division but replaces the imperative
`ask turtles` body with an ordered sequence of typed steps. Agents live in a
tibble, rules are dplyr-style formulas, and the output is tidy data.

## Installation

``` r
# install.packages("pak")
pak::pak("rayhanalirachman/tidyABM")
```

## Three parts

Every tidyABM model is the same three functions, written as three statements:

| part | function | what it holds |
|------|----------|---------------|
| 1. the world | `abm_setup()` | the agents, the network, the globals |
| 2. the tick  | `abm_go()`    | the ordered steps replayed once per tick |
| 3. the run   | `abm_run()`   | the two above, plus `ticks`, `seed` and `record` |

``` r
world  <- abm_setup(...)                                # 1
go     <- abm_go(...)                                   # 2
result <- abm_run(world, go, ticks = ..., seed = ...)   # 3
```

`abm_setup()` and `abm_go()` return objects, not arguments. Build each on its
own line and you can print it, check what it holds, and reuse it: one go block
run against several worlds, or one world run through several go blocks. That
second one is what a parameter sweep looks like here.

Everything else in the grammar sits inside one of the three. You hand
`abm_agents()` and `abm_network()` to `abm_setup()`. Every other `abm_*()`
function is a step inside `abm_go()`.

## A whole model

Wilensky and Rand's *Simple Economy*: 500 agents start with $100 each, and every
tick anyone with money gives $1 to someone else. Nobody is doing anything
clever, and inequality shows up anyway: the starting spike spreads, and run far
enough past the thousand ticks below it relaxes into an exponential.

``` r
library(tidyABM)

# 1. the world
economy <- abm_setup(agents = abm_agents(n = 500, money = 100))

# 2. the tick
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

`result` is a plain long tibble, so the analysis is ordinary dplyr and ggplot2.

The rest of this page is the map. [`vignette("tidyABM")`](https://rayhanalirachman.github.io/tidyABM/articles/tidyABM.html)
is the walkthrough, and works the same example line by line.

## The grammar

`abm_setup()` declares the world, out of two other specifications and a list:

| argument    | built with      | what it declares                                  |
|-------------|-----------------|---------------------------------------------------|
| `agents =`  | `abm_agents()`  | how many agents, and what columns they start with |
| `network =` | `abm_network()` | a persistent set of connections between them      |
| `globals =` | a plain `list()`| values the whole population can read              |

`abm_go()` declares what happens each tick, as a sequence of steps dispatched by
type and position rather than by argument name. There are twelve of them, in six
groups. The [reference index](https://rayhanalirachman.github.io/tidyABM/reference/index.html) uses the same six, in the
same order, which is the order a tick works through them.

**1. Interaction and matching.** Who interacts with whom this tick. It comes
first, because everything below reads the pairing it leaves standing.

| step               | what it does                                        |
|--------------------|-----------------------------------------------------|
| `abm_match()`      | decides who interacts with whom this tick           |

**2. Updating agents.** All five write a value somewhere, and the only thing
separating them is *whose row* gets written. They are listed by reach: the agent
itself, the agent itself in an order you control, its neighbourhood, one other
agent, everybody.

| step               | what it does                                        |
|--------------------|-----------------------------------------------------|
| `abm_rules()`      | updates agent columns, all agents simultaneously    |
| `abm_sequential()` | updates them one at a time, in shuffled order or an order you name |
| `abm_neighbours()` | summarises each agent's neighbourhood, either the network or everyone `within` a condition |
| `abm_tell()`       | writes a value into *another* agent's row           |
| `abm_global()`     | updates a value shared by the whole population, or a table of them indexed by a category |

**3. Network topology.** What is connected to what.

| step               | what it does                                        |
|--------------------|-----------------------------------------------------|
| `abm_link()`       | adds an edge between matched agents                 |
| `abm_unlink()`     | removes one                                         |

**4. Edge data.** A value carried by edges that already exist. The topology is
left alone, which is why this is not part of the group above.

| step               | what it does                                        |
|--------------------|-----------------------------------------------------|
| `abm_draw()`       | attaches a value to every edge, readable from both ends |

**5. Demographics.** The only steps that change how many agents there are.

| step               | what it does                                        |
|--------------------|-----------------------------------------------------|
| `abm_birth()`      | adds agents, one or `times` many                    |
| `abm_death()`      | removes them                                        |

**6. Control flow.** Replaying a block of the steps above inside a single tick.

| step               | what it does                                        |
|--------------------|-----------------------------------------------------|
| `abm_repeat()`     | replays a block of steps until a condition holds     |

So a two-phase model (play, then imitate) is written flat and in order:

``` r
go <- abm_go(
  abm_match(pair = "random"),
  abm_rules(payoff ~ case_when(...)),
  abm_match(pair = "random"),
  abm_rules(strategy ~ if_else(partner_payoff > payoff, partner_strategy, strategy))
)
```

The sequence is checked when `abm_go()` is called, not on every tick. Once, and
then never again.

## What the matching modes give you

After `abm_match(pair = ..., size = 2)` every rule can see `partner_<col>` for
each of the partner's columns. With `size > 2` rules are grouped instead, so
`sum(contribution)` inside a rule means "sum across this agent's group". A rule
can also be grouped by an ordinary agent column with `.by =`, which is how a
firm, a household or a team becomes a thing the model can add up.

| mode               | partner is…                                        | |
|--------------------|----------------------------------------------------|---|
| `"random"`         | drawn from a fresh random partition of the population | mutual |
| `"opposite_group"` | from the other side of a two-valued split           | mutual |
| `"one_of"`         | drawn from the whole population (NetLogo's `one-of other turtles`) | directional |
| `"nearest"`        | the closest agent in the space you name, or the one that minimises a `cost` you write | directional |
| `"network"`        | one of the agent's neighbours in the model network  | directional |

*Mutual* modes partition the eligible agents, so being matched is symmetric.
*Directional* modes give each agent a partner of its own, and your partner need
not have picked you.

Which one you want is rarely a matter of taste. A mutual mode guarantees
exclusion, so two wolves cannot eat the same sheep. A directional one gives you
no such promise. Where matching stands in for a rate rather than a rule, the
mode *is* the model.

## Reproducibility

`abm_run(..., seed =)` sets the seed for the whole run and restores your session's
random state afterwards, so a model is reproducible without you having to arrange
it.

`abm_run(..., record =)` controls how much of that to keep: every tick, every
*n*th tick, only the last one, or none of the populations at all. Globals are
recorded every tick whatever you pass. A fixed population can ignore the
argument. A growing one can't, because keeping every agent of every tick is what
turns a slow run into one the kernel stops.

## Status

Experimental. The API is shaped by what porting models turns up, and it is still
moving. Build on it now and expect to edit that code later.

Fifty-six models are implemented and documented in
[`models/`](https://github.com/rayhanalirachman/tidyABM/blob/main/models/README.md), each on its
own page with its code, the result it reproduces and its source: thirteen the
grammar was designed against, three of those rebuilt because the short version
does not show what the model is known for, and four rounds of ten ported as
stress tests.

Every feature in the package that is not in the founding thirteen exists because
one of those forty models asked for it: `abm_link()`, `abm_neighbours()`,
`abm_tell()`, `abm_draw()`, `abm_repeat()`, the `one_of` pairing mode,
`among =`, `cost =`, `within =`, `.by =`, `.order =`, `times =`, `record =`,
`.scope = "population"`, clique linking, the Poisson and scale-free and ring
networks, and the cascading semantics of `abm_sequential()`.
[`models/what-changed.md`](https://github.com/rayhanalirachman/tidyABM/blob/main/models/what-changed.md)
is the history.
[`models/open-items.md`](https://github.com/rayhanalirachman/tidyABM/blob/main/models/open-items.md)
is what is still out of reach.

Several of the validations are quantitative rather than qualitative. Hawks and
Doves matches the analytic evolutionarily stable frequency `V/C` to three
decimals across a twelve-fold range of the cost parameter. The information
cascade matches `p² / (p² + (1-p)²)` at five values of the signal accuracy, and
Deffuant's cluster count matches `floor(1/(2d))` at six values of the threshold.
Watts's cascade window comes out at its closed-form boundaries on both ends. The
Minority Game reproduces the σ²/α curve with its minimum at the reported
critical value, and the Naming Game's two scaling exponents land at 1.45 and
1.39 against a reported 3/2.

The fourth round added three more. Division of labour settles at δ/α to two
decimals whatever the thresholds are. The neutral model's variant counts land on
the Ewens sampling formula across two orders of magnitude in the innovation
rate. Deferred acceptance reproduces Pittel's `ln n` and `n / ln n` on the nose
at n = 200, which for a model this simple still surprises me.

See [`vignette("tidyABM")`](https://rayhanalirachman.github.io/tidyABM/articles/tidyABM.html) to get started,
[`vignette("models")`](https://rayhanalirachman.github.io/tidyABM/articles/models.html) for a tour of the grammar, and
[`vignette("corrections")`](https://rayhanalirachman.github.io/tidyABM/articles/corrections.html) for the three models
whose short form runs but is wrong.
