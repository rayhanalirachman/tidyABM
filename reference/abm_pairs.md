# Update every pair of a relation

`abm_pairs()` is
[`abm_rules()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_rules.md)
for a relation: each rule is a `value ~ expr` formula evaluated once per
row of the relation named by `via`, all rules simultaneously. It is how
a value that belongs to a pair changes for every pair at once – interest
accruing on every loan, last month's rationing being forgotten – with
one write and nothing to keep aligned.

## Usage

``` r
abm_pairs(via, ..., .when = NULL)
```

## Arguments

- via:

  The relation's name, a string.

- ...:

  One or more `value ~ expr` rules.

- .when:

  Optional condition, evaluated per row; rows where it does not hold are
  left as they are.

## Value

An `abm_pairs` step object.

## Details

Inside a rule the relation's own value columns are visible by name, the
two agents' `.id`s as `from` and `to`, and their agent columns as
`from_<col>` and `to_<col>`. Globals are in scope. A target that is not
yet a column of the relation is created.

## See also

[`abm_relation()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_relation.md),
[`abm_rules()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_rules.md),
[`abm_go()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_go.md).

## Examples

``` r
abm_pairs(via = "loans", balance ~ balance * 1.004)
#> <abm_pairs> via loans
#> • balance ~ `balance * 1.004`
abm_pairs(via = "sellers", unmet ~ 0)
#> <abm_pairs> via sellers
#> • unmet ~ `0`
abm_pairs(via = "loans", balance ~ 0, .when = to_bankrupt)
#> <abm_pairs> via loans
#> • balance ~ `0`
#> • .when = `to_bankrupt`
```
