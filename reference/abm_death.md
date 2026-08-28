# Remove agents

`abm_death()` drops every agent satisfying `when`. By default it also
removes those agents' edges from the network, because leaving them in
would make `abm_match(pair = "network")` draw partners that no longer
exist.

## Usage

``` r
abm_death(when, prune_edges = TRUE)
```

## Arguments

- when:

  A condition. Agents satisfying it are removed.

- prune_edges:

  Whether to delete the removed agents' network edges. Defaults to
  `TRUE`; set to `FALSE` only if you want a network whose node set
  deliberately outlives its agents.

## Value

An `abm_death` step object.

## See also

[`abm_go()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_go.md),
which lists every step and fixes the order they run in.

Other demographic steps:
[`abm_birth()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_birth.md)

## Examples

``` r
abm_death(when = resource <= 0)
#> <abm_death>
#> • when = `resource <= 0`
```
