# Draw the state of a lattice

A frame of a lattice model is a filter on the result tibble plus
`geom_raster(aes(.x, .y, fill = ...))`, and every spatial model's figure
starts the same way: pick the tick, keep the cells, fix the aspect
ratio, drop the axes a grid of cells does not need. `abm_plot_lattice()`
is that start. It returns an ordinary ggplot, so scales, labels, titles
and facets are added to it as usual.

## Usage

``` r
abm_plot_lattice(
  result,
  fill = NULL,
  tick = NULL,
  agents = FALSE,
  colour = NULL,
  shape = NULL,
  size = 1.2,
  colours = NULL
)
```

## Arguments

- result:

  An
  [`abm_run()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_run.md)
  result, or any data frame carrying `.x` and `.y` (for example several
  final frames bound together with a column saying which run each came
  from, to facet on).

- fill:

  What colours a cell: a column or an expression over the cell's
  columns, as in
  [`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html).
  Leave it out to draw the cells in a single neutral grey, for example
  under points. A cell whose value is `NA` (an empty cell) is drawn in
  the scale's `na.value`; give the scale `na.translate = FALSE` to leave
  it blank instead, which emits no warning.

- tick:

  Which tick(s) to draw. The last recorded tick by default; several
  ticks are drawn as one facet each.

- agents:

  Whether to draw the mobile agents over the cells.

- colour:

  What colours a mobile agent: a column or an expression over the
  agent's columns. `.group` by default. A discrete value gets one colour
  per level; a numeric one gets a gradient.

- shape:

  What shapes a mobile agent: a column or an expression over the agent's
  columns, or `NULL` for a filled circle.

- size:

  The point size of a mobile agent, in ggplot2's units.

- colours:

  The agents' colours: for a discrete `colour`, a vector of colours
  named by level (an unnamed one is taken in level order); for a numeric
  `colour`, the two ends of the gradient. `NULL` picks Okabe-Ito
  colours, or a light-to-dark blue gradient.

## Value

A ggplot object. Needs the ggplot2 package.

## Details

The wired group's rows are the cells; a row with a `.cell` is an agent
standing on one. With `agents = TRUE` those are drawn over the cells as
points, coloured by `.group` unless `colour` says otherwise, in the
Okabe-Ito palette (colour-blind safe) unless `colours` says otherwise.
The point layer comes with its colour scale, so to change the agents'
colours pass `colours` rather than adding a second `scale_colour_*()`,
which would replace it with a message.

Several ticks are drawn as one facet each, in a single row when there
are six or fewer: a strip showing how the world evolved.

## See also

[`abm_view_lattice()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_view_lattice.md)
for the same frames as an interactive playback;
[`abm_network()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_network.md)
for the lattice itself;
[`abm_move()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_move.md)
for the agents standing on it.

## Examples

``` r
life <- abm_setup(agents = abm_grid(dims = c(30, 30), alive = ~runif(n) < 0.3))
go <- abm_go(abm_neighbours(k ~ sum(alive)),
             abm_rules(alive ~ k == 3 | (alive & k == 2)))
r <- abm_run(life, go, ticks = 20, seed = 1, record = 10)
abm_plot_lattice(r, fill = alive, tick = c(0, 10, 20))
```
