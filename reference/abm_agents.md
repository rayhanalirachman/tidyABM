# Declare a group of agents

`abm_agents()` describes one homogeneous group of agents: how many there
are and what columns they start with. It is a *specification*, not a
population, nothing is created until the spec is passed to
[`abm_setup()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_setup.md),
which is where a model's first part is declared.

## Usage

``` r
abm_agents(n = NULL, ..., at = NULL)
```

## Arguments

- n:

  Number of agents in this group. A single positive integer. Optional,
  and ignored, for the group a grid or line
  [`abm_network()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_network.md)
  is wired to, which inherits `prod(dims)` instead.

- ...:

  Named column specifications. Each is either a plain value (which is
  recycled) or a one-sided formula (which is evaluated once, per group).
  Names beginning with `.` are reserved for the package.

- at:

  Where on the lattice this group starts, as a one-sided formula
  evaluated once, like a column. It must yield a cell id (a wired-group
  `.id`), either one to recycle or one per agent. The expression sees
  this group's own columns, `n`, `dims`, and the wired group's columns,
  so both `at = ~sample(prod(dims), n, replace = TRUE)` and
  `at = ~which(nest)[1]` work. Only meaningful in a model with a
  lattice; the default is a uniform random placement.

## Value

An `abm_agents` specification object.

## Details

Column values follow one rule:

- a plain value (`money = 100`) is recycled across every agent;

- a one-sided formula (`money = ~runif(n, 0, 50)`) is evaluated **once**
  and must return either a length-`n` vector or a length-1 value to
  recycle.

Formulas are evaluated in order, and each one can see `n` as well as any
column defined before it, so
`abm_agents(n = 200, wtp = ~rnorm(n, 50, 10), offer = ~wtp * 0.8)` works
as written.

## On a lattice

Two things change when the model has a grid or line network (see
[`abm_network()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_network.md)).
The group that network is wired to **omits `n`**: its count is
`prod(dims)`, and passing a matching `n` is allowed while a mismatch is
an error. Every other group gets a `.cell` saying which cell it is
standing on, drawn uniformly at random unless `at` says otherwise.

## Examples

``` r
abm_agents(n = 500, money = 100)
#> <abm_agents> 500 agents
#> • money = `100`

# A spec is not a population: hand it to abm_setup(), the first of the
# three parts a model is made of.
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

# on a lattice: the wired group inherits its count, the mobile one is placed
abm_agents(alive = ~runif(n) < 0.1)
#> <abm_agents> grid-many agent
#> • alive = `~runif(n) < 0.1`
abm_agents(n = 100, energy = ~runif(n, 4, 8),
           at = ~sample(prod(dims), n, replace = TRUE))
#> <abm_agents> 100 agents
#> • at = `sample(prod(dims), n, replace = TRUE)`
#> • energy = `~runif(n, 4, 8)`
```
