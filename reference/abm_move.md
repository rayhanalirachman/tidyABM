# Move agents across a lattice

A mobile agent's location is a patch `.id` held in a reserved `.cell`
column, and moving is writing that column. `abm_move()` is the step that
writes it: each mover's `.cell` is reassigned to a cell reachable from
the one it is standing on, and `.x` / `.y` follow automatically.

## Usage

``` r
abm_move(
  along,
  to = "random_neighbour",
  who = NULL,
  direction = NULL,
  range = NULL,
  axes_only = FALSE,
  avoid_occupied = FALSE
)
```

## Arguments

- along:

  Which lattice to move on: the name of the grid-wired group (for
  example `"patches"`), or `"network"` to use the model's edge list as
  the adjacency directly.

- to:

  How the destination is chosen. `"random_neighbour"`,
  `"random_empty_neighbour"`, `"stay"`, or `uphill(<expr>)` /
  `downhill(<expr>)`. Ignored when `direction` is given.

- who:

  Character vector of the agent groups that move this step; the rest of
  the population is untouched. Defaults to every group that has a
  `.cell`.

- direction:

  A column holding a per-agent heading. The mover steps one cell along
  it, rather than choosing a cell by value. Cannot be combined with
  `to`, `range` or `axes_only`.

- range:

  How many cells out to look. `1` (the default) is the adjacent cells
  only. Larger values reach further, following the lattice's own
  `diagonals` and `torus` settings.

- axes_only:

  Whether to scan only along the four axes (north, south, east, west)
  rather than over the whole neighbourhood. Only meaningful with
  `range > 1`.

- avoid_occupied:

  Whether to refuse a cell that another mover is already standing on.
  Movers are resolved in a random order and each one claims the cell it
  lands on, so two agents never end up on the same cell through this
  step.

## Value

An `abm_move` step object.

## Choosing where to go

`to` says how the destination is picked among the candidate cells:

- `"random_neighbour"` – a uniform draw from the adjacent cells.

- `"random_empty_neighbour"` – the same, but only cells that no mover is
  standing on. This is `"random_neighbour"` with
  `avoid_occupied = TRUE`.

- `uphill(<expr>)` / `downhill(<expr>)` – the candidate cell maximising
  or minimising `expr`. The cell the agent is already standing on is
  itself a candidate, so nothing can push an agent off the best cell in
  reach.

- `"stay"` – nobody moves. Useful as the inert branch of a model that
  switches movement on and off.

Ties are broken at random, and a mover with no legal target stays where
it is rather than erroring. Because the current cell only ties rather
than wins, an agent on flat ground wanders instead of freezing, which is
what keeps a gradient-follower exploring before it finds the gradient.
Write `uphill()` over an expression that already prefers the status quo
if you want the strict "only move if it is strictly better" reading.

## What `uphill()` sees

`uphill()` and `downhill()` are recognised forms inside `to =`, not
general functions. The expression is evaluated once per (mover,
candidate cell), and it sees:

- the **candidate cell's** patch columns under their own names, which is
  what makes `uphill(chemical)` "step towards more pheromone";

- the **mover's** own columns under their own names too, so
  `uphill(if_else(carrying, nest_scent, chemical))` reads as written.
  Where both sides have a column of the same name the mover wins, except
  for the engine-owned `.id`, `.group`, `.x`, `.y` and `.cell`, which
  stay the candidate cell's;

- every mover column again as `own_<col>`, so a clash can always be said
  explicitly;

- the globals.

## Looking further than one cell

`range`, `axes_only` and `avoid_occupied` widen or narrow the candidate
set. They exist because particular models force them and should be left
alone otherwise: Sugarscape's agents look `vision` cells along the four
axes and will not land on an occupied patch, which is
`range = vision, axes_only = TRUE, avoid_occupied = TRUE`.

`direction` is the other flavour: instead of picking a neighbour by a
value, the mover steps one cell along a heading it is carrying.
Langton's Ant is the model that forces it. The heading column is an
integer `0` north, `1` east, `2` south, `3` west (character names work
too), and a heading that points off a bounded edge leaves the agent
where it is.

## See also

[`abm_go()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_go.md),
which lists every step and fixes the order they run in.
[`abm_network()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_network.md)
for the lattice itself.

Other agent update steps:
[`abm_global()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_global.md),
[`abm_neighbours()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_neighbours.md),
[`abm_rules()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_rules.md),
[`abm_sequential()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_sequential.md),
[`abm_tell()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_tell.md)

## Examples

``` r
# a random walk on the patch lattice
abm_move(along = "patches", to = "random_neighbour", who = c("sheep", "wolves"))
#> <abm_move> along "patches" to random_neighbour
#> • who = "sheep" and "wolves"

# follow the pheromone home, or up the trail
abm_move(along = "patches", who = "ants",
         to = uphill(dplyr::if_else(carrying, nest_scent, chemical)))
#> <abm_move> along "patches" to uphill(dplyr::if_else(carrying, nest_scent,
#> chemical))
#> • who = "ants"

# Sugarscape: look `vision` cells along the axes, skip occupied patches
abm_move(along = "patches", who = "people", to = uphill(psugar),
         range = 6, axes_only = TRUE, avoid_occupied = TRUE)
#> <abm_move> along "patches" to uphill(psugar)
#> • who = "people"
#> • range = 6
#> • axes_only = TRUE
#> • avoid_occupied = TRUE

# Langton's Ant: one cell along a stored heading
abm_move(along = "patches", who = "ant", direction = heading)
#> <abm_move> along "patches" to direction = heading
#> • who = "ant"
```
