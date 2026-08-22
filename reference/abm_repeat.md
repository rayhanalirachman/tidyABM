# Repeat a block of steps until a condition holds

A tick is normally one pass through
[`abm_go()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_go.md).
Some models have a *phase* inside the tick that has to run to completion
before the next phase starts: an epidemic that burns out before anyone
reconsiders whether to vaccinate, a round of proposals that continues
until nobody is rejected, a market that clears. `abm_repeat()` is that
phase. It holds a block of steps and replays it until `until` is true,
or `max` times, whichever comes first.

## Usage

``` r
abm_repeat(..., until = NULL, max)
```

## Arguments

- ...:

  Step objects, validated as
  [`abm_go()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_go.md)
  validates them.

- until:

  A condition checked after each pass. Must collapse to one logical
  value. `NULL` (the default) means always run `max` times.

- max:

  The maximum number of passes. Required.

## Value

An `abm_repeat` step object.

## Details

`until` is evaluated the way an
[`abm_global()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_global.md)
right-hand side is — over the whole population, with the globals in
scope — and must collapse to a single `TRUE` or `FALSE`. It is checked
*after* each pass, so the block always runs at least once. `max` is
required, because a condition that never becomes true would otherwise
hang the run.

The same idea covers early stopping for a whole model: a block wrapped
in `abm_repeat(until = <absorbed>, max = <ticks>)` and run for a single
tick stops as soon as the model reaches its absorbing state, instead of
recomputing a fixed point for the rest of the run.

## Examples

``` r
# an epidemic that burns out before the tick ends
abm_repeat(
  abm_neighbours(exposure ~ sum(state == "I")),
  abm_rules(state ~ ifelse(state == "I", "R", state)),
  until = sum(state == "I") == 0,
  max = 500
)
#> <abm_repeat> 2 steps, at most 500 passes
#> • until = `sum(state == "I") == 0`
```
