# Declare a persistent network between agents

A network is an edge list that lives alongside the agent tibble for the
whole run. It is built once at
[`abm_setup()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_setup.md)
time and is read-only thereafter, with one exception:
[`abm_birth()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_birth.md)'s
`attach_via` argument can append one edge per newborn agent, and
[`abm_death()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_death.md)
prunes the edges of agents that are removed.

## Usage

``` r
abm_network(
  type = c("random", "poisson", "scale_free", "ring", "complete", "manual", "empty"),
  degree = NULL,
  edges = NULL
)
```

## Arguments

- type:

  How the network is built. `"random"` for a `degree`-regular random
  graph, `"poisson"` for an Erdos-Renyi graph of mean degree `degree`,
  `"scale_free"` for a Barabasi-Albert graph, `"ring"` for a
  one-dimensional lattice, `"complete"` for every possible edge,
  `"manual"` to supply `edges` yourself, or `"empty"` for a network that
  starts with no edges (useful with
  [`abm_birth()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_birth.md)).

- degree:

  Number of neighbours per agent — exactly, for `"random"` and `"ring"`;
  on average, for `"poisson"`; per newly attached agent, for
  `"scale_free"`. Required for all four. `n * degree` must be even for
  `"random"`, and `degree` must be even for `"ring"`.

- edges:

  A two-column data frame of `from`/`to` agent ids. Required for
  `type = "manual"`.

## Value

An `abm_network` specification object.

## Details

`type = "random"` builds a `degree`-regular random graph with
[`igraph::sample_k_regular()`](https://r.igraph.org/reference/sample_k_regular.html),
so every agent ends up with *exactly* `degree` neighbours and every edge
is symmetric. `degree = 1` therefore gives a fixed one-to-one pairing of
the whole population.

`type = "complete"` connects every agent to every other one. That is the
well-mixed population written as a graph, and it is what lets
[`abm_neighbours()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_neighbours.md)
mean "over everybody else" in a model with no spatial or social
structure at all.

The degree distribution is not a detail. `"random"` is *regular* — every
agent has exactly `degree` neighbours — and a threshold model on a
regular graph behaves quite differently from the same model on a graph
with the same mean degree but a spread of degrees, because the
low-degree agents are the ones a cascade can get started on. `"poisson"`
is the Erdos-Renyi graph `G(n, p)` with `p` chosen to give mean degree
`degree`, and it is what the random-graph literature means by "a random
graph". `"scale_free"` grows a Barabasi-Albert graph attaching `degree`
edges per new agent, giving the heavy tail. `"ring"` is the
one-dimensional lattice, each agent joined to the `degree / 2` agents on
either side of it.

## Examples

``` r
abm_network(type = "random", degree = 4)
#> <abm_network> type "random"
#> • degree = 4
abm_network(type = "poisson", degree = 3)
#> <abm_network> type "poisson"
#> • degree = 3
abm_network(type = "ring", degree = 2)
#> <abm_network> type "ring"
#> • degree = 2
abm_network(type = "complete")
#> <abm_network> type "complete"
abm_network(type = "manual", edges = data.frame(from = 1, to = 2))
#> <abm_network> type "manual"
#> • 1 edge supplied
```
