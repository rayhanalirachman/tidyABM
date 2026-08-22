# Declare what happens each tick

`abm_go()` is the behavioural block: an ordered sequence of typed steps
that is replayed once per tick. Steps are dispatched by *type and
position*, not by argument name, so a model with several phases is
written flat and in order:

## Usage

``` r
abm_go(...)
```

## Arguments

- ...:

  Step objects:
  [`abm_match()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_match.md),
  [`abm_rules()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_rules.md),
  [`abm_sequential()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_sequential.md),
  [`abm_global()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_global.md),
  [`abm_neighbours()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_neighbours.md),
  [`abm_tell()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_tell.md),
  [`abm_birth()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_birth.md),
  [`abm_death()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_death.md),
  [`abm_link()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_link.md),
  [`abm_unlink()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_unlink.md).

## Value

An `abm_go` object.

## Details

    abm_go(
      abm_match(pair = "random"), abm_rules(payoff ~ ...),   # phase 1: play
      abm_match(pair = "random"), abm_rules(strategy ~ ...)  # phase 2: imitate
    )

The sequence is validated once, here, rather than on every tick. Three
rules apply:

- it cannot be empty;

- no two
  [`abm_match()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_match.md)
  steps may sit next to each other, since the first would be discarded
  unused;

- it cannot end on an
  [`abm_match()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_match.md),
  for the same reason.

Everything else is allowed. In particular a model may use no matching at
all (El Farol, or a pure redistribution model), and several update steps
may follow a single match — the market model pairs once and then applies
separate rules to buyers and to sellers.

## Examples

``` r
abm_go(
  abm_match(pair = "random"),
  abm_rules(state ~ ifelse(partner_state == "spreader", "spreader", state))
)
#> <abm_go> 2 steps, 1 match phase
#> 1. match random
#> 2. rules 1 rule(s)
```
