# Declare what happens each tick

`abm_go()` is the second of the three functions a model is made of –
[`abm_setup()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_setup.md),
then `abm_go()`, then
[`abm_run()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_run.md).
It is the behavioural block: an ordered sequence of typed steps replayed
once per tick. Steps are dispatched by *type and position*, not by
argument name, so a model with several phases is written flat and in
order:

## Usage

``` r
abm_go(...)
```

## Arguments

- ...:

  Step objects:
  [`abm_match()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_match.md),
  [`abm_rules()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_rules.md),
  [`abm_sequential()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_sequential.md),
  [`abm_global()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_global.md),
  [`abm_neighbours()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_neighbours.md),
  [`abm_tell()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_tell.md),
  [`abm_birth()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_birth.md),
  [`abm_death()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_death.md),
  [`abm_link()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_link.md),
  [`abm_unlink()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_unlink.md),
  [`abm_draw()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_draw.md),
  [`abm_repeat()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_repeat.md).

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
  [`abm_match()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_match.md)
  steps may sit next to each other, since the first would be discarded
  unused;

- it cannot end on an
  [`abm_match()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_match.md),
  for the same reason.

Everything else is allowed. In particular a model may use no matching at
all (El Farol, or a pure redistribution model), and several update steps
may follow a single match, the market model pairs once and then applies
separate rules to buyers and to sellers.

## Examples

``` r
# Every model is the same three parts: abm_setup(), abm_go(), abm_run().
rumour <- abm_setup(agents = abm_agents(
  n = 200, state = ~c("spreader", rep("ignorant", n - 1))))

go <- abm_go(
  abm_match(pair = "random"),
  abm_rules(state ~ ifelse(partner_state == "spreader", "spreader", state))
)

abm_run(rumour, go, ticks = 10, seed = 1)
#> <abm_result> 10 ticks, 200 agents seen, 2200 rows
#> # A tibble: 2,200 × 4
#>     tick   .id .group state   
#>    <int> <int> <chr>  <chr>   
#>  1     0     1 agents spreader
#>  2     0     2 agents ignorant
#>  3     0     3 agents ignorant
#>  4     0     4 agents ignorant
#>  5     0     5 agents ignorant
#>  6     0     6 agents ignorant
#>  7     0     7 agents ignorant
#>  8     0     8 agents ignorant
#>  9     0     9 agents ignorant
#> 10     0    10 agents ignorant
#> # ℹ 2,190 more rows
```
