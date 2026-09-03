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
  type = c("random", "poisson", "scale_free", "ring", "complete", "grid", "line",
    "manual", "empty"),
  degree = NULL,
  edges = NULL,
  dims = NULL,
  diagonals = NULL,
  torus = NULL,
  on = NULL
)
```

## Arguments

- type:

  How the network is built. `"random"` for a `degree`-regular random
  graph, `"poisson"` for an Erdos-Renyi graph of mean degree `degree`,
  `"scale_free"` for a Barabasi-Albert graph, `"ring"` for a
  one-dimensional lattice, `"complete"` for every possible edge,
  `"grid"` for a 2-D lattice, `"line"` for a 1-D one, `"manual"` to
  supply `edges` yourself, or `"empty"` for a network that starts with
  no edges (useful with
  [`abm_birth()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_birth.md)).

- degree:

  Number of neighbours per agent, exactly, for `"random"` and `"ring"`;
  on average, for `"poisson"`; per newly attached agent, for
  `"scale_free"`. Required for all four. `n * degree` must be even for
  `"random"`, and `degree` must be even for `"ring"`.

- edges:

  A two-column data frame of `from`/`to` agent ids. Required for
  `type = "manual"`.

- dims:

  The shape of a lattice: `c(width, height)` for `type = "grid"`, a
  single width for `type = "line"`. The cell count is `prod(dims)`.
  Required for both lattice types and used by neither of the others.

- diagonals:

  For `type = "grid"`, whether a cell's neighbourhood includes the
  diagonals. `TRUE` (the default) is the 8-neighbour Moore neighbourhood
  and matches NetLogo's unmarked `neighbors`; `FALSE` is the 4-neighbour
  von Neumann one. An error for `type = "line"`, which has no diagonals.

- torus:

  For a lattice, whether the edges wrap. `TRUE` is the default. `FALSE`
  gives a bounded lattice whose border cells simply have fewer
  neighbours, which needs no special-casing in a rule:
  [`sum()`](https://rdrr.io/r/base/sum.html) and
  [`any()`](https://rdrr.io/r/base/any.html) see the shorter
  neighbourhood directly.

- on:

  For a lattice, the name of the agent group to wire. The rest of the
  population is not on the lattice; it stands *on* it, via `.cell`.
  Defaults to the whole population, which is what a patch-only model
  wants.

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

The degree distribution is not a detail. `"random"` is *regular*, every
agent has exactly `degree` neighbours, and a threshold model on a
regular graph behaves quite differently from the same model on a graph
with the same mean degree but a spread of degrees, because the
low-degree agents are the ones a cascade can get started on. `"poisson"`
is the Erdos-Renyi graph `G(n, p)` with `p` chosen to give mean degree
`degree`, and it is what the random-graph literature means by "a random
graph". `"scale_free"` grows a Barabasi-Albert graph attaching `degree`
edges per new agent, giving the heavy tail. `"ring"` is the
one-dimensional lattice, each agent joined to the `degree / 2` agents on
either side of it.

## Lattices

`type = "grid"` and `type = "line"` build a lattice, and a lattice **is
a network**: it produces the same `from`/`to` edge tibble every other
type produces, so patches are ordinary agents and
[`abm_neighbours()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_neighbours.md),
[`abm_match()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_match.md)
with `pair = "network"`,
[`abm_link()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_link.md)
and
[`abm_edges()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_edges.md)
all work on it with no change. There is no second medium and no
patch-specific rule syntax.

Two things are injected. The wired group gains `.x` and `.y` (a line
gains `.x` only), integer cell coordinates, 1-based, with
`.id = .x + (.y - 1) * w` and `.y` increasing upward. Every *other*
group gains `.cell`, an integer holding the wired group's `.id` for the
cell that agent is standing on, plus `.x` / `.y` mirroring it. All three
are reserved: a model reads them, it does not declare them.

The wired group also **inherits its count**, so it omits `n` in
[`abm_agents()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_agents.md)
and gets `prod(dims)`. `n` is still usable inside that group's formulas,
and a matching `n` is allowed; a mismatch is an error. The lattice is
built before the agent columns are materialised, so `.x` and `.y` are in
scope in setup formulas as well as in the go block.

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

# a 100x100 torus with the Moore neighbourhood
abm_network(type = "grid", dims = c(100, 100))
#> <abm_network> type "grid"
#> • dims = 100 x 100 (10000 cells)
#> • diagonals = TRUE
#> • torus = TRUE

# a bounded von Neumann grid, wired to the patches of a turtle model
abm_network(type = "grid", dims = c(50, 50), diagonals = FALSE,
            torus = FALSE, on = "patches")
#> <abm_network> type "grid"
#> • dims = 50 x 50 (2500 cells)
#> • diagonals = FALSE
#> • torus = FALSE
#> • on = patches

# a 1-D ring of cells
abm_network(type = "line", dims = 401)
#> <abm_network> type "line"
#> • dims = 401 (401 cells)
#> • torus = TRUE
```
