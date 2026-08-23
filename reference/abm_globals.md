# Global values recorded during a run

Global values recorded during a run

## Usage

``` r
abm_globals(x)
```

## Arguments

- x:

  An `abm_result` from
  [`abm_run()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_run.md).

## Value

A tibble with one row per tick and one column per global.

## Examples

``` r
m  <- abm_setup(agents = abm_agents(n = 20, x = 1), globals = list(total = 0))
go <- abm_go(abm_global(total ~ sum(x)))
r  <- abm_run(m, go, ticks = 3, seed = 1)
abm_globals(r)
#> # A tibble: 4 × 2
#>    tick total
#>   <int> <dbl>
#> 1     0     0
#> 2     1    20
#> 3     2    20
#> 4     3    20
```
