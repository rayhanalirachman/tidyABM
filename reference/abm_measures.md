# Measures recorded during a run

The table `measures =` built: one row per tick of every run, with the
run's identifying columns in front of it when there was more than one
run.

## Usage

``` r
abm_measures(x)
```

## Arguments

- x:

  An `abm_result` from
  [`abm_run()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_run.md).

## Value

A tibble with one row per tick per run, or `NULL` if the run was made
without `measures`.

## Details

Measures are observations rather than model state, which is what
separates them from
[`abm_globals()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_globals.md).
A global is readable by every rule in the model; a measure is written to
this table and is invisible to the model that produced it.

## Examples

``` r
m  <- abm_setup(agents = abm_agents(n = 20, x = ~runif(20)))
go <- abm_go(abm_rules(x ~ x * 0.9))
r  <- abm_run(m, go, ticks = 5, seed = 1,
              measures = list(total = ~sum(x), spread = ~sd(x)))
abm_measures(r)
#> # A tibble: 6 × 3
#>    tick total spread
#>   <int> <dbl>  <dbl>
#> 1     0  9.43  0.242
#> 2     1  8.49  0.218
#> 3     2  7.64  0.196
#> 4     3  6.87  0.176
#> 5     4  6.19  0.159
#> 6     5  5.57  0.143
```
