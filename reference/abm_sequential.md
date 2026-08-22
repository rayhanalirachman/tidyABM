# Update agent columns one agent at a time

`abm_sequential()` is the order-dependent sibling of
[`abm_rules()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_rules.md).
Agents are processed one at a time in a freshly shuffled order, and each
agent's writes to **globals** are visible to every agent processed after
it within the same step. This mirrors NetLogo's `ask turtles`, and it is
what you need when agents compete for a shared resource that is
*depleted* rather than merely divided — a bank's lendable reserves, say.

## Usage

``` r
abm_sequential(...)
```

## Arguments

- ...:

  One or more `column ~ expression` rules. The left-hand side may name
  either an agent column or a global.

## Value

An `abm_sequential` step object.

## Details

Rules also cascade *within* each agent: the second rule sees what the
first one just wrote, to the agent's own row and to the globals alike.
That is the opposite of
[`abm_rules()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_rules.md),
where every rule reads the state at the start of the step, and it is
what "one agent at a time" already implies — an agent that draws a quote
and then decides whether the quote crosses the book has to be able to
read the number it just drew.

The step is deliberately narrow: during the per-agent loop a rule can
read its own agent's columns and any global, and it can write its own
agent's columns and any global. It cannot see other agents' column
values — use
[`abm_tell()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_tell.md)
to write into another agent's row. Use
[`abm_rules()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_rules.md)
unless you specifically need the ordering, since sequential evaluation
is both slower and harder to reason about.

## Examples

``` r
abm_sequential(
  loan          ~ ifelse(wallet < 0, loan + 1, loan),
  bank_reserves ~ ifelse(wallet < 0, bank_reserves - 1, bank_reserves)
)
#> <abm_sequential> 2 rules
#> • `loan ~ ifelse(wallet < 0, loan + 1, loan)`
#> • `bank_reserves ~ ifelse(wallet < 0, bank_reserves - 1, bank_reserves)`
```
