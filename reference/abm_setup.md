# Set up a model

`abm_setup()` turns declarations into an initial population: it
evaluates the
[`abm_agents()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_agents.md)
specifications into tibbles, builds the network, and stores the starting
values of any shared globals. The result is the `model` argument of
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
  `list(buyers = abm_agents(...), sellers = abm_agents(...))`).

- network:

  Optionally an
  [`abm_network()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_network.md)
  specification.

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
abm_setup(agents = abm_agents(n = 500, money = 100))
#> <abm_model> 500 agents in 1 group
#> • agents: 500 agents ["money"]

abm_setup(
  agents  = abm_agents(n = 100, threshold = ~runif(n, 40, 80)),
  globals = list(last_attendance = 60)
)
#> <abm_model> 100 agents in 1 group
#> • agents: 100 agents ["threshold"]
#> • globals: "last_attendance"
```
