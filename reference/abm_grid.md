# A grid of patches, as a single declaration

`abm_grid()` is sugar for a patch-heavy model. It desugars to an
[`abm_agents()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_agents.md)
group plus an
[`abm_network()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_network.md)
of `type = "grid"` wired to it, so the
[`abm_go()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_go.md)
block is identical either way. Use the explicit two-line form instead
when the network should be visible in the setup, or when another network
sits alongside it.

## Usage

``` r
abm_grid(dims, diagonals = TRUE, torus = TRUE, ...)
```

## Arguments

- dims:

  `c(width, height)`. The cell count is `prod(dims)`, and the wired
  group inherits it – do not pass `n`.

- diagonals:

  `TRUE` (the default) for an 8-neighbour Moore lattice, `FALSE` for a
  4-neighbour von Neumann one.

- torus:

  `TRUE` (the default) wraps the edges; `FALSE` gives a bounded grid
  whose border cells have fewer neighbours.

- ...:

  Patch column specifications, exactly as
  [`abm_agents()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_agents.md)
  takes them (`n` is available inside a formula and equals
  `prod(dims)`).

## Value

An `abm_grid` object, recognised by
[`abm_setup()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_setup.md).

## Details

    patches <- abm_grid(dims = c(w, h), diagonals = FALSE, torus = FALSE,
                        state = ~sample(c("tree", "empty"), n, TRUE))

desugars to

    agents  = list(patches = abm_agents(state = ~...)),
    network = abm_network(type = "grid", dims = c(w, h),
                          diagonals = FALSE, torus = FALSE, on = "patches")

## See also

[`abm_network()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_network.md),
[`abm_agents()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_agents.md).

## Examples

``` r
life <- abm_setup(
  agents = abm_grid(dims = c(20, 20), alive = ~runif(n) < 0.3)
)
```
