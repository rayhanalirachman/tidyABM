# Remove edges between matched agents

`abm_unlink()` is the mirror of
[`abm_link()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_link.md):
it deletes the edge joining each matched pair. Paired with
`abm_match(pair = "network")` it detaches an agent from one of its
neighbours, which, followed by a match and an
[`abm_link()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_link.md),
is how you rewire a network.

## Usage

``` r
abm_unlink(when = NULL)
```

## Arguments

- when:

  Optional condition. Only pairs where it holds are unlinked.

## Value

An `abm_unlink` step object.

## See also

[`abm_go()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_go.md),
which lists every step and fixes the order they run in.

Other network topology steps:
[`abm_link()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_link.md)

## Examples

``` r
# Watts-Strogatz rewiring: drop a neighbour, pick up a stranger
abm_go(
  abm_match(pair = "network", eligible = runif(n()) < 0.1),
  abm_unlink(),
  abm_match(pair = "random", eligible = runif(n()) < 0.1),
  abm_link()
)
#> <abm_go> 4 steps, 2 match phases
#> 1. match network
#> 2. unlink
#> 3. match random
#> 4. link
```
