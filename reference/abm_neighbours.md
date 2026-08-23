# Summarise each agent's neighbourhood

A match gives an agent *one* partner. Plenty of models need the whole
neighbourhood instead, how many of my neighbours are infected, what
fraction of them are flashing, what my neighbours believe on average.
`abm_neighbours()` writes exactly that: for every agent, an aggregate
over the agents around it.

## Usage

``` r
abm_neighbours(..., within = NULL)
```

## Arguments

- ...:

  One or more `column ~ aggregate_expression` rules. The expression sees
  the neighbours' agent columns, the focal agent's own columns as
  `own_<col>`, any column
  [`abm_draw()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_draw.md)
  attached to the edge, and any global.

- within:

  Optional condition defining a neighbourhood in attribute space rather
  than in the network. Evaluated once per (focal, candidate) pair, with
  the candidate's columns under their own names and the focal agent's
  under `own_<col>`. When it is supplied the model needs no network.

## Value

An `abm_neighbours` step object.

## Details

Each rule is `column ~ aggregate_expression`, and the expression is
evaluated over the neighbours' rows, so `sum(infected)` means "how many
of my neighbours are infected" and `mean(opinion)` means "what my
neighbours think on average". An agent with no neighbours gets `NA`.

Alongside each neighbour column the expression also sees `own_<col>`,
the focal agent's own value of that column, recycled down its
neighbourhood. That is what makes a *comparison* possible,
`sum(wealth > own_wealth)` is "how many of my neighbours are richer than
me", which no aggregate over the neighbours alone can express.

## Two kinds of neighbourhood

By default the neighbourhood is the model's
[`abm_network()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_network.md):
the agents this one shares an edge with. `within =` replaces it with a
neighbourhood in **attribute space**, everybody whose columns satisfy a
condition, whether or not the model has a network at all. The condition
is evaluated once per (focal, candidate) pair, with the candidate's
columns under their own names and the focal agent's under `own_<col>`,
which is the same view `abm_match(cost =)` minimises over.

    abm_neighbours(opinion ~ mean(opinion), within = abs(opinion - own_opinion) <= eps)

is Hegselmann–Krause's confidence set, and it is a step rather than a
hand-rolled [`vapply()`](https://rdrr.io/r/base/lapply.html) over the
population.

The two differ in one respect beyond how the neighbourhood is found: an
agent is **part of its own** attribute neighbourhood whenever the
condition holds of it, because "the mean opinion of everyone I take
seriously" includes the agent's own. It is never part of its network
neighbourhood, because an agent is not joined to itself. Write
`within = ... & .id != own_.id` to exclude it.

They also differ in cost. The network form does work proportional to the
number of edges; `within =` builds every (focal, candidate) pair and
then filters, so it is quadratic in the population. That is the same
order as the [`vapply()`](https://rdrr.io/r/base/lapply.html) it
replaces, with a tibble's constant factor on top, and it is worth
knowing before reaching for it on a very large population.

## Examples

``` r
abm_neighbours(infected_neighbours ~ sum(state == "infected"))
#> <abm_neighbours> 1 rule
#> • `infected_neighbours ~ sum(state == "infected")`
abm_neighbours(richer_neighbours ~ sum(wealth > own_wealth))
#> <abm_neighbours> 1 rule
#> • `richer_neighbours ~ sum(wealth > own_wealth)`

# a neighbourhood in opinion space rather than in a network
abm_neighbours(opinion ~ mean(opinion), within = abs(opinion - own_opinion) <= 0.2)
#> <abm_neighbours> 1 rule (within abs(opinion - own_opinion) <= 0.2)
#> • `opinion ~ mean(opinion)`
```
