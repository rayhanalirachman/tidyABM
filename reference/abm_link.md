# Add edges between matched agents

`abm_link()` turns the pairing produced by the preceding
[`abm_match()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_match.md)
into permanent edges. It is how a network grows during a run without
anyone being born, random-graph percolation, tie formation, coalition
building.

## Usage

``` r
abm_link(when = NULL)
```

## Arguments

- when:

  Optional condition. Only pairs where it holds are linked. It can use
  the agent's own columns, `partner_<col>`, `.role`, and any global.

## Value

An `abm_link` step object.

## Details

An edge is added once per matched pair, and pairs that are already
connected are left alone, so the network never gains a duplicate edge.

After a match with `size > 2` the group is linked as a *clique*, every
pair inside it gains an edge. That is what a team, a committee or a
coalition means once it is written as a network.

## Examples

``` r
# a random graph that grows one batch of edges per tick
abm_go(
  abm_match(pair = "random", eligible = runif(n()) < 0.05),
  abm_link()
)
#> <abm_go> 2 steps, 1 match phase
#> 1. match random
#> 2. link
```
