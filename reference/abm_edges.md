# The network at the end of a run

The network at the end of a run

## Usage

``` r
abm_edges(x)
```

## Arguments

- x:

  An `abm_result` from
  [`abm_run()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_run.md).

## Value

A tibble of `from`/`to` edges, or `NULL` if the model had no network.

## Examples

``` r
m <- abm_setup(agents = abm_agents(n = 10, x = 1),
               network = abm_network(type = "random", degree = 2))
r <- abm_run(m, abm_go(abm_match(pair = "network"),
                       abm_rules(x ~ partner_x)), ticks = 2, seed = 1)
abm_edges(r)
#> # A tibble: 10 × 2
#>     from    to
#>    <int> <int>
#>  1     1     5
#>  2     1     6
#>  3     2     4
#>  4     2    10
#>  5     3     8
#>  6     3     9
#>  7     4     8
#>  8     5     7
#>  9     6     9
#> 10     7    10
```
