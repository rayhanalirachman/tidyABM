# Attach a value to every edge, visible from both ends

Two
[`abm_neighbours()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_neighbours.md)
passes over the same network draw their random numbers independently,
which is fine when each pass is a separate event and wrong when it is
the *same* event seen from two sides. "How many of my neighbours
punished me" and "how much punishing did I do" have to add up to the
same thing, agent by agent, and they cannot if each aggregate rolls its
own dice.

## Usage

``` r
abm_draw(..., .each = c("edge", "endpoint"))
```

## Arguments

- ...:

  One or more `name ~ expression` rules, evaluated once per edge.

- .each:

  `"edge"` for one value per edge, shared by both endpoints;
  `"endpoint"` for one value per endpoint, read as `name` and
  `name_back`.

## Value

An `abm_draw` step object.

## Details

`abm_draw()` fixes that by putting the draw on the **edge** rather than
in the aggregate. Each rule is evaluated once per edge, and the value it
produces is then readable inside every later
[`abm_neighbours()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_neighbours.md)
rule in the same tick, from either endpoint, as a column of that name.
Because both endpoints read the same number, two aggregates over it
describe the same events.

The right-hand side is evaluated over the **edge table**, one row per
edge, with `from` and `to`, so
[`n()`](https://dplyr.tidyverse.org/reference/context.html) is the
number of edges and `runif(n())` is "a uniform per edge". Globals are in
scope. The values live until the next `abm_draw()` of that name replaces
them, so a draw made once per tick lasts the tick and one made in
[`abm_setup()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_setup.md)'s
first step lasts the run.

`.each` says who the value belongs to:

- `"edge"` (the default), one value for the edge. Both endpoints read
  the same number in the column of that name. This is a coin the *pair*
  tosses: whether they met, what the encounter was worth.

- `"endpoint"`, one value each. The focal agent reads its own in the
  column of that name and its neighbour's as `<name>_back`. This is a
  coin each of them tosses privately about the other, which is what an
  asymmetric interaction needs: whether I noticed *you*, and separately
  whether you noticed *me*.

## Examples

``` r
# did this pair meet this tick? Both of them agree on the answer.
abm_go(
  abm_draw(met ~ runif(n()) < 0.3),
  abm_neighbours(partners ~ sum(met))
)
#> <abm_go> 2 steps, 0 match phases
#> 1. draw 1 value(s) per edge
#> 2. nbrs 1 rule(s)

# whether I saw you, and whether you saw me, are two different coins
abm_draw(noticed ~ runif(n()), .each = "endpoint")
#> <abm_draw> 1 value per endpoint
#> • `noticed ~ runif(n())`
```
