# Update agent columns one agent at a time

`abm_sequential()` is the order-dependent sibling of
[`abm_rules()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_rules.md).
Agents are processed one at a time in a freshly shuffled order, and each
agent's writes to **globals** are visible to every agent processed after
it within the same step. This mirrors NetLogo's `ask turtles`, and it is
what you need when agents compete for a shared resource that is
*depleted* rather than merely divided, a bank's lendable reserves, say.

## Usage

``` r
abm_sequential(..., .order = NULL)
```

## Arguments

- ...:

  One or more `column ~ expression` rules. The left-hand side may name
  either an agent column or a global.

- .order:

  Optional expression, evaluated over the whole population, whose
  ascending order is the order agents are processed in. The default is a
  fresh shuffle every step, which is right when the order is meant to be
  arbitrary and wrong when it is part of the model: a queue at a
  counter, a sequential-service constraint, a fixed speaking order. `NA`
  sits the agent out of the step.

## Value

An `abm_sequential` step object.

## Details

Rules also cascade *within* each agent: the second rule sees what the
first one just wrote, to the agent's own row and to the globals alike.
That is the opposite of
[`abm_rules()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_rules.md),
where every rule reads the state at the start of the step, and it is
what "one agent at a time" already implies, an agent that draws a quote
and then decides whether the quote crosses the book has to be able to
read the number it just drew.

During the per-agent loop a rule can read and write its own agent's
columns and any global. If an
[`abm_match()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_match.md)
is standing it can also read and write its **partner's**: the step is
narrowed to the agents the match placed in a group, `.partner` and
`partner_<col>` come into scope, and a rule whose left-hand side is
`partner_<col>` writes into that agent's row. Because the partner is
read live rather than copied at the start of the step, the second buyer
at a shop sees the stock the first one took, which is what a
decentralised market is and what
[`abm_tell()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_tell.md)
cannot say:
[`abm_tell()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_tell.md)
resolves every sender at once, so a stock it draws down can go negative.

It reaches no further than that. To write into a row that is neither its
own nor its partner's, use
[`abm_tell()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_tell.md).
Use
[`abm_rules()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_rules.md)
unless you specifically need the ordering, since sequential evaluation
is both slower and harder to reason about.

## See also

[`abm_go()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_go.md),
which lists every step and fixes the order they run in.

Other agent update steps:
[`abm_global()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_global.md),
[`abm_move()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_move.md),
[`abm_neighbours()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_neighbours.md),
[`abm_rules()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_rules.md),
[`abm_tell()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_tell.md)

## Examples

``` r
abm_sequential(
  loan          ~ ifelse(wallet < 0, loan + 1, loan),
  bank_reserves ~ ifelse(wallet < 0, bank_reserves - 1, bank_reserves)
)
#> <abm_sequential> 2 rules
#> • `loan ~ ifelse(wallet < 0, loan + 1, loan)`
#> • `bank_reserves ~ ifelse(wallet < 0, bank_reserves - 1, bank_reserves)`

# a sale: what is left on the shelf is what the customers before me left
abm_sequential(
  got         ~ pmin(want, money / partner_price, partner_stock),
  money       ~ money - got * partner_price,
  partner_stock ~ partner_stock - got
)
#> <abm_sequential> 3 rules
#> • `got ~ pmin(want, money/partner_price, partner_stock)`
#> • `money ~ money - got * partner_price`
#> • `partner_stock ~ partner_stock - got`
```
