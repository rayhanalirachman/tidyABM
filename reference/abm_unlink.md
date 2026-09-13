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
abm_unlink(when = NULL, via = NULL, to = NULL)
```

## Arguments

- when:

  Optional condition. Only pairs where it holds are unlinked. On a
  relation it can read the pair's `R_<col>` values, so
  `when = loans_balance <= 0` is "once repaid".

- via:

  Optional name of a relation, as a string. Absent, the step acts on the
  network.

- to:

  For `via =`: an expression naming the other agent's `.id`, one per
  agent. Defaults to `.partner`.

## Value

An `abm_unlink` step object.

## Details

With `via = "R"` it removes rows from the relation `R` instead, the row
`(me -> them)` for each agent's `.partner` or for the agent named by
`to =`; see
[`abm_link()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_link.md)
for how those are read.

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

# a loan is closed once it is repaid
abm_unlink(via = "loans", when = loans_balance <= 0)
#> <abm_unlink> via loans
#> • when = `loans_balance <= 0`
```
