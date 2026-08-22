# Update a shared, population-level value

`abm_global()` writes to a value held once for the whole model rather
than once per agent — El Farol's `last_attendance`, a zakah pool, a
bank's ledger. The right-hand side is an aggregate expression evaluated
over the agent tibble, so it normally collapses to a single value.

## Usage

``` r
abm_global(...)
```

## Arguments

- ...:

  One or more `global_name ~ aggregate_expression` rules. The expression
  can use agent columns and other globals; each rule sees the globals as
  updated by the rules before it in the same call.

## Value

An `abm_global` step object.

## Details

Unlike the other update steps, `abm_global()` does not need a preceding
[`abm_match()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_match.md):
a population-level summary does not depend on who was paired with whom.

## Examples

``` r
abm_global(last_attendance ~ sum(go_today))
#> <abm_global> 1 rule
#> • `last_attendance ~ sum(go_today)`
```
