# Read a run's relations back

The state of each relation at the end of the run, as a tibble of `from`,
`to` and the value columns – the counterpart of
[`abm_edges()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_edges.md)
for the network. Under `abm_run(reps =)` or `params =`, every table
carries the `.run`, `.rep` and parameter columns in front.

## Usage

``` r
abm_relations(x, via = NULL)
```

## Arguments

- x:

  An `abm_result` from
  [`abm_run()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_run.md).

- via:

  Optional relation name. Omitted, the whole named list is returned;
  `NULL` when the model declared no relations.

## Value

A named list of tibbles, or one tibble.

## See also

[`abm_relation()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_relation.md),
[`abm_edges()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_edges.md),
[`abm_globals()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_globals.md).

## Examples

``` r
who <- data.frame(from = 1:2, to = c(3L, 3L))
m <- abm_setup(agents = abm_agents(n = 3, x = 1),
               relations = list(buys = abm_relation(who, times = 0)))
r <- abm_run(m, abm_go(abm_pairs(via = "buys", times ~ times + 1)),
             ticks = 3, seed = 1)
abm_relations(r, "buys")
#> # A tibble: 2 × 3
#>    from    to times
#>   <int> <int> <dbl>
#> 1     1     3     3
#> 2     2     3     3
```
