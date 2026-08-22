# Write a value into another agent's row

Every other update step writes to the agent it is evaluated on:
[`abm_rules()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_rules.md)
changes your own columns,
[`abm_neighbours()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_neighbours.md)
*reads* your neighbours' and writes the summary to you. `abm_tell()` is
the one that goes the other way — a sender computes a value and writes
it into a *recipient's* row. That is the difference between "how many of
my neighbours are shouting" and "I shout at my neighbours", and models
built on the second shape — outward persuasion, contagion carrying a
dose, an order book where a trade has to mark the counterparty — cannot
be written without it.

## Usage

``` r
abm_tell(
  ...,
  to,
  when = NULL,
  .resolve = c("last", "first", "sum", "mean", "max", "min", "collect", "error")
)
```

## Arguments

- ...:

  One or more `column ~ expression` rules. The expression is evaluated
  in the sender's row; the result is written to the recipient's column
  of the same name. The column must already exist on the recipient.

- to:

  `"neighbours"`, or an expression naming the recipient's `.id`. If the
  expression returns a list column, each element is a vector of `.id`s
  and the sender writes to all of them.

- when:

  Optional condition on the sender. Only agents for which it holds send
  anything.

- .resolve:

  What to do when several senders write to the same recipient in one
  step: `"last"` (an arbitrary one wins), `"first"`, `"sum"`, `"mean"`,
  `"max"`, `"min"`, `"collect"` — which hands the recipient a list of
  everything it was told, so the recipient's own rule decides what to
  make of them, including in what order — or `"error"` to stop.

## Value

An `abm_tell` step object.

## Details

Who receives is set by `to`:

- `to = "neighbours"` — every agent the sender is joined to in the
  model's
  [`abm_network()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_network.md).
  This is the broadcast: one sender, many recipients.

- `to = <expression>` — an expression evaluated in the sender's row that
  names the recipient by `.id`. A list column names *several*: one
  sender, a set of recipients chosen however the model likes, which is
  what a broadcast to an audience that is not the network needs.
  `to = .partner` writes to the partner a preceding
  [`abm_match()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_match.md)
  gave you; `to = best_bid_holder` writes to whichever agent a global
  names. `NA` means the sender says nothing.

The right-hand side of each rule is evaluated in the **sender's** row,
so it sees the sender's columns, `partner_<col>`, `.role` and the
globals, exactly as
[`abm_rules()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_rules.md)
would. The value it produces is then written into the recipient's column
of that name. An agent that no one wrote to keeps the value it already
had — `abm_tell()` never touches a silent agent's row.

Two senders can address the same recipient in one step, and `.resolve`
says what happens then. The default, `"last"`, takes an arbitrary one of
them, which is right for a persuasion rule where the senders all agree
anyway; `"sum"` is right for anything additive, like a dose or an order
quantity; `"error"` says the collision is a modelling mistake and should
stop the run.

## Examples

``` r
# a matched pair pushes its shared opinion onto everyone around it
abm_tell(opinion ~ opinion, to = "neighbours", when = opinion == partner_opinion)
#> <abm_tell> to neighbours
#> • `opinion ~ opinion`
#> • when = `opinion == partner_opinion`

# a trade marks the counterparty a global named
abm_tell(filled ~ TRUE, to = best_bid_holder, when = ask <= best_bid)
#> <abm_tell> to best_bid_holder
#> • `filled ~ TRUE`
#> • when = `ask <= best_bid`

# contagion that carries a dose, summed over everyone who coughed on you
abm_tell(dose ~ dose + load, to = "neighbours", when = infected, .resolve = "sum")
#> <abm_tell> to neighbours
#> • `dose ~ dose + load`
#> • when = `infected`
#> • resolve = "sum"
```
