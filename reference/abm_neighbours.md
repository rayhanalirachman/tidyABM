# Summarise each agent's network neighbours

A match gives an agent *one* partner. Plenty of models need the whole
neighbourhood instead — how many of my neighbours are infected, what
fraction of them are flashing, what my neighbours believe on average.
`abm_neighbours()` writes exactly that: for every agent, an aggregate
over the agents it is connected to in the model's
[`abm_network()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_network.md).

## Usage

``` r
abm_neighbours(...)
```

## Arguments

- ...:

  One or more `column ~ aggregate_expression` rules. The expression sees
  the neighbours' agent columns, the focal agent's own columns as
  `own_<col>`, and any global.

## Value

An `abm_neighbours` step object.

## Details

Each rule is `column ~ aggregate_expression`, and the expression is
evaluated over the neighbours' rows, so `sum(infected)` means "how many
of my neighbours are infected" and `mean(opinion)` means "what my
neighbours think on average". An agent with no neighbours gets `NA`.

Alongside each neighbour column the expression also sees `own_<col>`,
the focal agent's own value of that column, recycled down its
neighbourhood. That is what makes a *comparison* possible —
`sum(wealth > own_wealth)` is "how many of my neighbours are richer than
me", which no aggregate over the neighbours alone can express.

## Examples

``` r
abm_neighbours(infected_neighbours ~ sum(state == "infected"))
#> <abm_neighbours> 1 rule
#> • `infected_neighbours ~ sum(state == "infected")`
abm_neighbours(richer_neighbours ~ sum(wealth > own_wealth))
#> <abm_neighbours> 1 rule
#> • `richer_neighbours ~ sum(wealth > own_wealth)`
```
