# Update a shared, population-level value

`abm_global()` writes to a value held once for the whole model rather
than once per agent, El Farol's `last_attendance`, a zakah pool, a
bank's ledger. The right-hand side is an aggregate expression evaluated
over the agent tibble, so it normally collapses to a single value.

## Usage

``` r
abm_global(..., .by = NULL)
```

## Arguments

- ...:

  One or more `global_name ~ aggregate_expression` rules. The expression
  can use agent columns and other globals; each rule sees the globals as
  updated by the rules before it in the same call.

- .by:

  Optional index. Either a vector of keys or the name of an agent column
  whose distinct values are the keys. The global becomes a named vector,
  `.key` is in scope, and the global's own name refers to that key's
  value.

## Value

An `abm_global` step object.

## Details

Unlike the other update steps, `abm_global()` does not need a preceding
\[abm_match()\]: a population-level summary does not depend on who was
paired with whom.

## A global indexed by a category

Models keep wanting a shared *table* rather than a shared number: a
stimulus per task, a price per good, a queue length per counter. `.by`
writes one. The rules are evaluated once per key and the global becomes
a **named vector** indexed by them, which an ordinary rule reads back
with `price[good]`.

Two things are in scope during that evaluation and nowhere else. `.key`
is the key being written, so `sum(task == .key)` is "how many agents are
on *this* task". And the global's own name is bound to **this key's**
current value, not to the whole vector, so an update reads exactly as
the scalar version does:

## See also

[`abm_go()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_go.md),
which lists every step and fixes the order they run in.

Other agent update steps:
[`abm_move()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_move.md),
[`abm_neighbours()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_neighbours.md),
[`abm_rules()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_rules.md),
[`abm_sequential()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_sequential.md),
[`abm_tell()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_tell.md)

## Examples

``` r
abm_global(last_attendance ~ sum(go_today))
#> <abm_global> 1 rule
#> • `last_attendance ~ sum(go_today)`

# one stimulus per task, decaying with the number of workers on it
abm_global(stimulus ~ pmax(0, stimulus + 1 - 3 * sum(task == .key) / n()),
           .by = 1:2)
#> <abm_global> 1 rule (by 1:2)
#> • `stimulus ~ pmax(0, stimulus + 1 - 3 * sum(task == .key)/n())`
```
