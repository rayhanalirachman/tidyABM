# tidyABM

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Agent-based models, written as a specification rather than a loop.

tidyABM keeps NetLogo's two-block structure — a `setup` that declares the world
and a `go` that runs once per tick — but replaces the imperative `ask turtles`
body with an ordered sequence of typed steps. Agents live in a tibble, rules are
dplyr-style formulas, and the output is tidy data.

## Installation

tidyABM lives on GitHub. Install it with either of these:

``` r
# install.packages("pak")
pak::pak("rayhanalirachman/tidyabm")

# or
# install.packages("remotes")
remotes::install_github("rayhanalirachman/tidyabm")
```

To get the vignettes as well:

``` r
remotes::install_github("rayhanalirachman/tidyabm", build_vignettes = TRUE)
vignette("tidyABM")
```

Then:

``` r
library(tidyABM)
```

## A whole model

Wilensky and Rand's *Simple Economy*: 500 agents start with $100 each, and every
tick anyone with money gives $1 to someone else. The wealth distribution starts
as a spike and ends up exponential — no agent is doing anything clever, and
inequality appears anyway.

``` r
library(tidyABM)

economy <- abm_setup(agents = abm_agents(n = 500, money = 100))

go <- abm_go(
  abm_match(pair = "random", role = list(giver = money > 0, receiver = TRUE)),
  abm_rules(money ~ if_else(.role == "giver", money - 1, money + 1))
)

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

## The grammar

Setup declares the world:

| function        | what it declares                                  |
|-----------------|---------------------------------------------------|
| `abm_agents()`  | how many agents, and what columns they start with |
| `abm_network()` | a persistent set of connections between them      |
| `abm_setup()`   | the whole world, plus any shared globals          |

`abm_go()` declares what happens each tick, as a sequence of steps dispatched by
type and position rather than by argument name:

| step               | what it does                                        |
|--------------------|-----------------------------------------------------|
| `abm_match()`      | decides who interacts with whom this tick           |
| `abm_rules()`      | updates agent columns, all agents simultaneously    |
| `abm_sequential()` | updates them one at a time, in shuffled order       |
| `abm_neighbours()` | summarises each agent's network neighbourhood       |
| `abm_tell()`       | writes a value into *another* agent's row           |
| `abm_global()`     | updates a value shared by the whole population      |
| `abm_birth()`      | adds agents                                         |
| `abm_death()`      | removes them                                        |
| `abm_link()` / `abm_unlink()` | adds or removes network edges            |

So a two-phase model — play, then imitate — is written flat and in order:

``` r
abm_go(
  abm_match(pair = "random"),
  abm_rules(payoff ~ case_when(...)),
  abm_match(pair = "random"),
  abm_rules(strategy ~ if_else(partner_payoff > payoff, partner_strategy, strategy))
)
```

The sequence is checked once, when `abm_go()` is called, rather than on every
tick.

## What the matching modes give you

After `abm_match(pair = ..., size = 2)` every rule can see `partner_<col>` for
each of the partner's columns. With `size > 2` rules are grouped instead, so
`sum(contribution)` inside a rule means "sum across this agent's group".

| mode               | partner is…                                        | |
|--------------------|----------------------------------------------------|---|
| `"random"`         | drawn from a fresh random partition of the population | mutual |
| `"opposite_group"` | from the other side of a two-valued split           | mutual |
| `"one_of"`         | drawn from the whole population (NetLogo's `one-of other turtles`) | directional |
| `"nearest"`        | the closest agent in the space you name             | directional |
| `"network"`        | one of the agent's neighbours in the model network  | directional |

*Mutual* modes partition the eligible agents, so being matched is symmetric.
*Directional* modes give each agent a partner of its own, and your partner need
not have picked you.

## Reproducibility

`abm_run(..., seed =)` sets the seed for the whole run and restores your session's
random state afterwards, so a model is reproducible without you having to arrange
it.

## Status

Experimental. The API is shaped by what porting models turns up, and it is still
moving.

Forty-six models are implemented and documented in
[`models/`](../models/README.md), each on its own page with its code, the result
it reproduces and its source: thirteen the grammar was designed against, three of
those rebuilt because the short version does not show what the model is known
for, and three rounds of ten ported as stress tests.

Every feature in the package that is not in the founding thirteen exists because
one of those thirty models asked for it — `abm_link()`, `abm_neighbours()`,
`abm_tell()`, the `one_of` pairing mode, `among =`, `.scope = "population"`,
clique linking, the Poisson and scale-free and ring networks, and the cascading
semantics of `abm_sequential()`.
[`models/what-changed.md`](../models/what-changed.md) is the history;
[`models/open-items.md`](../models/open-items.md) is what is still out of reach.

Several validations are quantitative rather than qualitative. Hawks and Doves
matches the analytic evolutionarily stable frequency `V/C` to three decimals
across a twelve-fold range of the cost parameter; the information cascade matches
`p² / (p² + (1-p)²)` at five values of the signal accuracy; Deffuant's cluster
count matches `floor(1/(2d))` at six values of the threshold; Watts's cascade
window matches its closed-form boundaries at both ends; the Minority Game
reproduces the σ²/α curve with its minimum at the reported critical value; and
the Naming Game's two scaling exponents come out at 1.45 and 1.39 against a
reported 3/2.

See `vignette("tidyABM")` to get started, `vignette("models")` for a tour of the
grammar, and `vignette("corrections")` for the three models whose short form runs
but is wrong.
