# Set up a model

`abm_setup()` is the first of the three functions a model is made of –
`abm_setup()`, then
[`abm_go()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_go.md),
then
[`abm_run()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_run.md).
It turns declarations into an initial population: it evaluates the
[`abm_agents()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_agents.md)
specifications into tibbles, builds the
[`abm_network()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_network.md),
and stores the starting values of any shared globals. The result is the
`model` argument of
[`abm_run()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_run.md).

## Usage

``` r
abm_setup(agents, network = NULL, globals = list(), seed = NULL)
```

## Arguments

- agents:

  Either one
  [`abm_agents()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_agents.md)
  specification, or a *named* list of them for a model with several
  kinds of agent (for example
  `list(buyers = abm_agents(...), sellers = abm_agents(...))`). One
  element may be an
  [`abm_grid()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_grid.md),
  which is sugar for a group plus the grid
  [`abm_network()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_network.md)
  wired to it.

- network:

  Optionally an
  [`abm_network()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_network.md)
  specification. A lattice (`type = "grid"` or `"line"`) is built before
  the agent columns are materialised, so the wired group's formulas can
  read `.x` and `.y` and its count is inherited from `dims`.

- globals:

  A named list of population-level values shared by every agent, for
  example `list(last_attendance = 60)`. Globals are readable inside
  every rule and are updated by
  [`abm_global()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_global.md).

- seed:

  Optional integer. Sets the random seed for setup only, so the starting
  population is reproducible independently of the run. Note that this
  and
  [`abm_run()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_run.md)'s
  `seed` do different jobs: this one fixes *who the agents are*,
  [`abm_run()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_run.md)'s
  fixes *what happens to them*. A model whose columns are drawn at
  random needs both to be reproducible end to end.

## Value

An `abm_model` object.

## Details

Agent ids (`.id`) are unique across the whole model, and every agent
carries a `.group` column naming the group it belongs to. A single-group
model is given the group name `"agents"`.

## Examples

``` r
abm_setup(
  agents  = abm_agents(n = 100, threshold = ~runif(n, 40, 80)),
  globals = list(last_attendance = 60)
)
#> <abm_model> 100 agents in 1 group
#> • agents: 100 agents ["threshold"]
#> • globals: "last_attendance"

# The world is the first of the three parts a model is made of.
economy <- abm_setup(agents = abm_agents(n = 500, money = 100))

go <- abm_go(
  abm_match(pair = "random", role = list(giver = money > 0, receiver = TRUE)),
  abm_rules(money ~ if_else(.role == "giver", money - 1, money + 1))
)

abm_run(economy, go, ticks = 10, seed = 1)
#> <abm_result> 10 ticks, 500 agents seen, 5500 rows
#> # A tibble: 5,500 × 4
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
#> # ℹ 5,490 more rows
```
