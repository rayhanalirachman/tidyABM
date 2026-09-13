# Declare a relation between agents

A *relation* is a table of directed pairs of agents, each pair carrying
values of its own. It is where a quantity lives when it belongs to
neither agent alone but to the two of them together: how much this
seller has rationed *this* household, what this bank owes *that* one,
how highly I rank *you*. A model may declare several, by name, in
`abm_setup(relations = list(...))`, and they sit alongside the network,
which is unchanged.

## Usage

``` r
abm_relation(edges, ...)
```

## Arguments

- edges:

  A data frame with integer columns `from` and `to`, the `.id`s of the
  two agents, and optionally further columns holding each pair's
  starting values. `(from, to)` must be unique and `from != to`.

- ...:

  Named default values, one per value column: `unmet = 0`. A column
  named here but absent from `edges` is added at the default; a column
  in `edges` but not named here defaults to `NA` of its type. These
  defaults are what
  [`abm_link()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_link.md)
  fills in for a row it creates.

## Value

An `abm_relation` object, for `abm_setup(relations = )`.

## Details

The network cannot hold this. It is one per model, it is undirected, and
the only values that attach to its edges come from
[`abm_draw()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_draw.md),
which is a fresh coin per tick that cannot read its own previous value.
A relation is directed – `(from, to)` is a different row from
`(to, from)` – and its values persist until a rule writes them or the
row is removed.

## What a relation gives a rule

For a relation named `R` with a value column `v`, every place that sees
a *pair* of agents also sees:

|            |                                             |
|------------|---------------------------------------------|
| name       | meaning                                     |
| `.R`       | `TRUE` when the row `(me -> them)` exists   |
| `.R_back`  | `TRUE` when `(them -> me)` exists           |
| `R_v`      | the value on `(me -> them)`, `NA` if no row |
| `R_v_back` | the value on `(them -> me)`                 |

"Me" is the focal agent and "them" the candidate in
[`abm_match()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_match.md)'s
`among`, `weight` and `cost`, and in
[`abm_neighbours()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_neighbours.md)'s
`within`; under a standing match in
[`abm_rules()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_rules.md)
and
[`abm_sequential()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_sequential.md),
"them" is `.partner`. There `R_v ~ expr` *writes* the pair's value, and
`R_v_back ~ expr` writes the reverse row. That is the whole point: the
number has one home, so two sides of a transaction cannot drift apart.

Rows are added and removed with
[`abm_link()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_link.md)
and
[`abm_unlink()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_unlink.md)
using `via = "R"`, updated in bulk with
[`abm_pairs()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_pairs.md),
and read back after a run with
[`abm_relations()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_relations.md).
[`abm_global()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_global.md),
measures and
[`abm_tell()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_tell.md)
evaluate over the bare population and do not see relation columns;
aggregate through `abm_neighbours(within = .R)` first.

## See also

[`abm_setup()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_setup.md),
[`abm_link()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_link.md),
[`abm_pairs()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_pairs.md),
[`abm_relations()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_relations.md).

## Examples

``` r
# each of 3 households buys from 2 of 4 firms (ids 4:7); nothing owed yet
who <- data.frame(from = c(1L, 1L, 2L, 2L, 3L, 3L),
                  to   = c(4L, 5L, 5L, 6L, 6L, 7L))
abm_relation(edges = who, unmet = 0)
#> <abm_relation> 6 pairs
#> • unmet: <numeric>, default 0
```
