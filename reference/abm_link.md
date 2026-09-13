# Add edges between matched agents

`abm_link()` turns the pairing produced by the preceding
[`abm_match()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_match.md)
into permanent edges. It is how a network grows during a run without
anyone being born, random-graph percolation, tie formation, coalition
building.

## Usage

``` r
abm_link(..., when = NULL, via = NULL, to = NULL)
```

## Arguments

- ...:

  For `via =` only: `col ~ expr` rules setting value columns on the rows
  this step creates, evaluated per agent over the population with the
  standing match in scope.

- when:

  Optional condition. Only pairs where it holds are linked. It can use
  the agent's own columns, `partner_<col>`, `.role`, any global and, on
  a relation, the pair's `R_<col>` values.

- via:

  Optional name of a relation, as a string. Absent, the step acts on the
  network.

- to:

  For `via =`: an expression naming the other agent's `.id`, one per
  agent. Defaults to `.partner`.

## Value

An `abm_link` step object.

## Details

An edge is added once per matched pair, and pairs that are already
connected are left alone, so the network never gains a duplicate edge.

After a match with `size > 2` the group is linked as a *clique*, every
pair inside it gains an edge. That is what a team, a committee or a
coalition means once it is written as a network.

## On a relation

With `via = "R"` the step adds rows to the
[`abm_relation()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_relation.md)
named `R` instead. A relation is directed, so the row is `(me -> them)`:
from each agent to its `.partner`, or to the agent named by `to =`. With
`to =` no pairing is needed at all – the other agent is read from a
column, which is what a swap needs when the incumbent being dropped and
the newcomer being taken on are two different agents held in two
columns. `size > 2` pairings are not relations.

Rules in `...` set the new rows' value columns; a column not named takes
the relation's default. A pair that already exists is left exactly as it
is – the rules do not re-apply – so "add to an existing balance" is a
link followed by a write:

    abm_link(via = "loans"),                          # creates the pair at 0 if new
    abm_rules(loans_balance ~ loans_balance + take)   # adds either way

## See also

[`abm_go()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_go.md),
which lists every step and fixes the order they run in;
[`abm_relation()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_relation.md)
for what a relation is.

Other network topology steps:
[`abm_unlink()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_unlink.md)

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

# a household takes on the cheaper firm it found, held in `cand`
abm_link(via = "sellers", to = cand, when = swap)
#> <abm_link> via sellers
#> • to = `cand`
#> • when = `swap`

# a new loan from my partner, starting at the amount just borrowed
abm_link(via = "loans", balance ~ take)
#> <abm_link> via loans
#> • balance ~ `take`
```
