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
  .resolve = c("last", "first", "sum", "mean", "max", "min", "collect", "error"),
  .order = NULL
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
  make of them — or `"error"` to stop.

- .order:

  Optional expression, evaluated in the **sender's** row, whose
  ascending order is the order the messages are considered in. Without
  it a recipient's messages arrive in whatever order the senders
  happened to be stored in, which makes `"first"`, `"last"` and the list
  `"collect"` hands over arbitrary. With it they are determinate:
  `.order = arrival` with `.resolve = "first"` is *the first person to
  reach the counter*, and `"collect"` hands the recipient its messages
  already in that order. `NA` sits the sender out of the step.

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

Three of those resolutions — `"first"`, `"last"` and `"collect"` — pick
out a message rather than combining them all, so they only mean
something once the messages have an order. `.order` gives them one: an
expression evaluated in the sender's row whose ascending order the
messages are considered in. That is what *the first person to reach the
counter* needs, and without it the counter has to reconstruct the queue
from something the senders wrote down.

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

# whoever got there first is the one the counter serves
abm_tell(serving ~ .id, to = counter, when = queueing,
         .resolve = "first", .order = arrived_at)
#> <abm_tell> to counter
#> • `serving ~ .id`
#> • when = `queueing`
#> • resolve = "first"
#> • order = `arrived_at`
```
