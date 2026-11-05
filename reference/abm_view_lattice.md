# Play a recorded lattice run back in the browser

[`abm_plot_lattice()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_plot_lattice.md)
draws one frame; `abm_view_lattice()` writes every recorded frame into a
single HTML file that plays them back: a canvas of the lattice, play /
pause, a tick slider, a speed control, a legend with the counts at the
current tick, and a readout of whatever cell or agent the pointer is
over. The file is self-contained (its script, style and data are inline;
nothing is fetched), so it opens from disk, attaches to an email and
works on a phone. It follows the browser's light or dark theme and has a
switch of its own.

## Usage

``` r
abm_view_lattice(
  result,
  fill = NULL,
  file = NULL,
  agents = FALSE,
  colour = NULL,
  mark = NULL,
  hover = NULL,
  tick = NULL,
  title = NULL,
  subtitle = NULL,
  labels = NULL,
  fill_colours = NULL,
  agent_colours = NULL,
  mark_colours = NULL,
  fps = 8
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
  columns. A logical, character or factor value gets one colour per
  level; a number gets a gradient (quantised to 64 steps). `NULL` draws
  the cells in a neutral grey, for agents to stand on.

- file:

  Where to write the HTML. `NULL` writes to a temporary file. The path
  is returned invisibly either way.

- agents:

  Whether to draw the mobile agents over the cells.

- colour:

  What colours a mobile agent: a column or an expression over the
  agent's columns. `.group` by default.

- mark:

  Cells to mark over the fill: a character or factor expression over the
  cell's columns whose levels are the marks and whose `NA` is no mark,
  for example `case_when(nest ~ "nest", food > 0 ~ "food")`.

- hover:

  Names of further columns to show in the readout: a cell's columns are
  stored as quantised layers, an agent's as its values at each tick.
  Keep it to a few; each one costs as much as the fill does.

- tick:

  Which ticks to play. Every recorded tick by default.

- title, subtitle:

  The page's heading. The title defaults to the `fill` expression.

- labels:

  What to call the `fill`, `colour` and `mark` values in the legend and
  the readout, as a character vector named by those three: for example
  `c(fill = "grass height")` when `fill` is an expression.

- fill_colours, agent_colours, mark_colours:

  The colours of the fill, the agents and the marks, as `colours` in
  [`abm_plot_lattice()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_plot_lattice.md):
  a vector named by level, or the stops of a gradient for a numeric
  value. `NULL` picks Okabe-Ito colours, or a light-to-blue gradient.

- fps:

  Frames per second at speed 1.

## Value

The path of the file, invisibly.

## Details

The frames are the ticks
[`abm_run()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_run.md)
recorded (`record = "all"` or a number), so the file grows with the
lattice, the number of frames and the number of agents. Cell values are
quantised to at most 64 levels and run-length encoded, and an agent
costs a few bytes a tick, so a 50 x 50 lattice over 200 frames is well
under a megabyte. Thin a long run with `record =` or `tick =` rather
than recording every tick of it.

## See also

[`abm_plot_lattice()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_plot_lattice.md)
for a static frame of the same result.

## Examples

``` r
life <- abm_setup(agents = abm_grid(dims = c(30, 30), alive = ~runif(n) < 0.3))
go <- abm_go(abm_neighbours(k ~ sum(alive)),
             abm_rules(alive ~ k == 3 | (alive & k == 2)))
r <- abm_run(life, go, ticks = 30, seed = 1)
f <- abm_view_lattice(r, fill = alive, title = "Game of Life",
                      file = tempfile(fileext = ".html"))
# browseURL(f)   # open it in the browser
```
