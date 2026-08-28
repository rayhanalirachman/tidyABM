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
right-hand side is, over the whole population, with the globals in
scope, and must collapse to a single `TRUE` or `FALSE`. It is checked
*after* each pass, so the block always runs at least once. `max` is
required, because a condition that never becomes true would otherwise
hang the run.

If a match is standing when the block runs, `until` also sees `.role`
and `partner_<col>`, as a rule does. A phase is usually a phase *of an
encounter*, and its stopping condition is usually about the pair rather
than about either agent alone.

The same idea covers early stopping for a whole model: a block wrapped
in `abm_repeat(until = <absorbed>, max = <ticks>)` and run for a single
tick stops as soon as the model reaches its absorbing state, instead of
recomputing a fixed point for the rest of the run.

## See also

[`abm_go()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_go.md),
which lists every step and fixes the order they run in. The block it
wraps is validated exactly as
[`abm_go()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_go.md)
validates a tick.

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

# a bargaining phase, which stops when every pair has met or given up
abm_repeat(
  abm_rules(bid ~ pmin(bid + step, reservation)),
  until = all(bid >= partner_ask | bid >= reservation),
  max = 20
)
#> <abm_repeat> 1 step, at most 20 passes
#> • until = `all(bid >= partner_ask | bid >= reservation)`
```
