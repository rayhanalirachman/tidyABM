# Declare a group of agents

`abm_agents()` describes one homogeneous group of agents: how many there
are and what columns they start with. It is a *specification*, not a
population — nothing is created until the spec is passed to
[`abm_setup()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_setup.md),
which is where a model's first part is declared.

## Usage

``` r
abm_agents(n, ...)
```

## Arguments

- n:

  Number of agents in this group. A single positive integer.

- ...:

  Named column specifications. Each is either a plain value (which is
  recycled) or a one-sided formula (which is evaluated once, per group).
  Names beginning with `.` are reserved for the package.

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
```
