# Declare a group of agents

`abm_agents()` describes one homogeneous group of agents: how many there
are and what columns they start with. It is a *specification*, not a
population — nothing is created until the spec is passed to
[`abm_setup()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_setup.md).

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

abm_agents(
  n = 100,
  strategy = ~sample(c("cooperate", "defect"), n, replace = TRUE),
  payoff = 0
)
#> <abm_agents> 100 agents
#> • strategy = `~sample(c("cooperate", "defect"), n, replace = TRUE)`
#> • payoff = `0`
```
