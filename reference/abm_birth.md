# Add agents

`abm_birth()` is one of the two steps that change the size of the
population. It has two modes, and exactly one of them must be used:

## Usage

``` r
abm_birth(
  when = NULL,
  n = NULL,
  times = NULL,
  cost = NULL,
  inherit = NULL,
  attach_via = NULL
)
```

## Arguments

- when:

  A condition. Agents satisfying it reproduce.

- n:

  A count of new agents to add unconditionally.

- times:

  How many offspring each reproducing agent has. An expression evaluated
  in the parent's row, so it can be a column, a draw
  (`rpois(dplyr::n(), 2)`) or a number. `0` means that parent has none
  this tick and `NA` is treated as `0`. Only used with `when`, since `n`
  is already a count. Each offspring gets its own evaluation of
  `inherit`, so a mutation drawn there differs from sibling to sibling.

- cost:

  One or more `column ~ expression` formulas applied to the parent *and*
  the newborn after the split, expressing what reproduction costs, for
  example `cost = resource ~ resource / 2` to halve a resource between
  them.

- inherit:

  One or more `column ~ expression` formulas applied to the newborn
  *only*, expressing what the offspring gets that the parent does not
  keep: a reset age, a mutated trait, a sex drawn at birth. The
  expressions are evaluated in the parent's row, so they can use the
  parent's columns and, when an
  [`abm_match()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_match.md)
  is standing, the other parent's, as `partner_<col>`. That is how
  two-parent inheritance is written:
  `inherit = trait ~ (trait + partner_trait) / 2`.

- attach_via:

  An
  [`abm_match()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_match.md)
  object with `pair = "network"`, used to connect each newborn to an
  existing agent. This is the only way the network grows during a run;
  `from = "random_edge"` gives degree-proportional (preferential)
  attachment.

## Value

An `abm_birth` step object.

## Details

- `when = <condition>` clones every agent that satisfies the condition.
  The newborn inherits all of its parent's columns.

- `n = <count>` adds that many brand-new agents, whose columns are
  copied from a randomly chosen existing agent of the same group.

One parent, one offspring, unless `times` says otherwise. Any fertility
above one, a clutch, a litter, a Poisson number of seeds, is `times`,
and each offspring is evaluated separately, so a mutation drawn in
`inherit` differs between siblings.

## Examples

``` r
abm_birth(when = resource > 20, cost = resource ~ resource / 2)
#> <abm_birth>
#> • when = `resource > 20`
#> • cost = `resource ~ resource/2`

# a clutch rather than a single offspring
abm_birth(when = mature, times = rpois(dplyr::n(), 2), inherit = age ~ 0)
#> <abm_birth>
#> • when = `mature`
#> • times = `rpois(dplyr::n(), 2)`
#> • inherit = `age ~ 0`
abm_birth(n = 1, attach_via = abm_match(pair = "network", from = "random_edge"))
#> <abm_birth>
#> • n = 1
#> • attach_via = network ("random_edge")

# a child of two parents, with its own age and a mutated trait
abm_birth(
  when = sex == "female",
  inherit = list(age ~ 0, trait ~ (trait + partner_trait) / 2 + rnorm(n(), 0, 0.01))
)
#> <abm_birth>
#> • when = `sex == "female"`
#> • inherit = `age ~ 0` and `trait ~ (trait + partner_trait)/2 + rnorm(n(), 0,
#>   0.01)`
```
