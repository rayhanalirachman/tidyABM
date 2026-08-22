# Update agent columns

`abm_rules()` is the step that changes agents. Every rule is a two-sided
formula: the left-hand side names the column to write, the right-hand
side is an expression evaluated against the agent tibble, exactly as it
would be inside
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html).

## Usage

``` r
abm_rules(..., .scope = c("match", "population"))
```

## Arguments

- ...:

  One or more `column ~ expression` rules. The expression can use any
  column of the agent tibble, any global, any `partner_<col>` produced
  by a preceding
  [`abm_match()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_match.md),
  `.role`, and anything visible where the rule was written.

- .scope:

  `"match"` (the default) evaluates the rules within whatever grouping
  the preceding
  [`abm_match()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_match.md)
  produced. `"population"` ignores it and evaluates across all agents,
  so aggregates like [`sum()`](https://rdrr.io/r/base/sum.html) and
  draws like `sample(x, n())` see everybody.

## Value

An `abm_rules` step object.

## Details

All rules in a single `abm_rules()` call are evaluated
**simultaneously**, against the state at the start of the step. So in
`abm_rules(a ~ b, b ~ a)` both sides see the old values — this is the
synchronous update that agent-based models normally assume, and it is
the one place where `abm_rules()` deliberately departs from `mutate()`'s
sequential semantics. Write two `abm_rules()` calls if you want one rule
to see the other's result.

In a model with several agent groups, a rule is applied to a group only
if every agent column it mentions exists in that group. That is how the
market example routes `offer ~ ...` to buyers and `ask ~ ...` to sellers
without any explicit test on agent type.

While a match is standing, rules are evaluated group by group — per
pair, per group, or per agent depending on the pairing mode. That is
usually what you want, and it is what makes `sample(x, 1)` mean "once
per pair". Occasionally a step in the middle of a tick is about the
whole population instead — drawing the next generation from this one,
say — and `.scope = "population"` evaluates it against every agent at
once, ignoring the standing match.

## Examples

``` r
abm_rules(money ~ ifelse(.role == "giver", money - 1, money + 1))
#> <abm_rules> 1 rule
#> • `money ~ ifelse(.role == "giver", money - 1, money + 1)`
abm_rules(opinion ~ partner_opinion)
#> <abm_rules> 1 rule
#> • `opinion ~ partner_opinion`

# the next generation, drawn from this one in proportion to fitness
abm_rules(strategy ~ sample(strategy, n(), replace = TRUE, prob = fitness),
          .scope = "population")
#> <abm_rules> 1 rule {.emph (population scope)}
#> • `strategy ~ sample(strategy, n(), replace = TRUE, prob = fitness)`
```
