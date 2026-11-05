# Spatial reproduction scripts

One script per spatial model, at the size and seed that produced the numbers on
the model's page. Each script prints its table and then writes its figures,
`../figures/<same name>.png` and, where the page shows the lattice itself,
`../figures/<same name>-lattice.png`.

Run any of them with `Rscript <file>`, from the repository root or from this
directory:

```bash
Rscript models/spatial/scripts/01-life.R
```

Every script is self-contained: its own `library(tidyABM)`, its own parameters,
no shared setup file. They need `tidyABM` and `ggplot2` installed.

The `testthat` cases in
[`../../../tests/testthat/test-spatial.R`](../../../tests/testthat/test-spatial.R)
run the same checks at reduced scale so the suite stays quick; these are the
full runs.

| script | model | tier | runtime | figures | viewer |
|---|---|---|---|---|---|
| `01-life.R` | [1. Conway's Game of Life](../01-life.md) | L0 | 40 s | `01-life.png`, `01-life-lattice.png`, `01-life-strip.png` | `01-life.html` |
| `02-ca1d.R` | [2. Elementary 1-D CA, rules 90 and 30](../02-ca1d.md) | L0 + L1 | 2 s | `02-ca1d.png` | |
| `03-fire.R` | [3. Forest Fire, density sweep](../03-fire.md) | L0 | 50 s | `03-fire.png`, `03-fire-lattice.png`, `03-fire-strip.png` | `03-fire.html` |
| `04-schelling.R` | [4. Schelling with geography, tolerance sweep](../04-schelling.md) | L0 | 7 s | `04-schelling.png`, `04-schelling-lattice.png` | `04-schelling.html` |
| `05-wolf-sheep.R` | [5. Wolf–Sheep–Grass, three regimes](../05-wolf-sheep.md) | L2 | 26 s | `05-wolf-sheep.png`, `05-wolf-sheep-lattice.png`, `05-wolf-sheep-strip.png` | `05-wolf-sheep.html` |
| `06-ants.R` | [6. Ants, pheromone foraging](../06-ants.md) | L2 | 21 s | `06-ants.png`, `06-ants-lattice.png`, `06-ants-strip.png` | `06-ants.html` |
| `08-sugarscape.R` | [8. Sugarscape, carrying capacity](../08-sugarscape.md) | L2+ | 17 s | `08-sugarscape.png`, `08-sugarscape-lattice.png`, `08-sugarscape-strip.png` | `08-sugarscape.html` |
| `09-langton.R` | [9. Langton's Ant, against a reference](../09-langton.md) | L2+ | 75 s | `09-langton.png` | |
| `10-ising.R` | [10. Ising, temperature sweep](../10-ising.md) | L0 | 12 s | `10-ising.png`, `10-ising-lattice.png` | |
| `11-daisyworld.R` | [11. Daisyworld, luminosity sweep](../11-daisyworld.md) | L0 | 110 s | `11-daisyworld.png` | |
| `13-rebellion.R` | [13. Rebellion, legitimacy sweep](../13-rebellion.md) | L2+ | 55 s | `13-rebellion.png`, `13-rebellion-lattice.png` | `13-rebellion.html` |

Runtimes are wall-clock on a two-core container and include the figures, which
in a few cases means one extra run at the headline parameters (Langton's Ant
runs on to 11,500 steps for its picture). A `-lattice` figure is one frame of
the world drawn with `abm_plot_lattice()`, a `-strip` figure several ticks of
it side by side, and a viewer is the recorded run written to
[`../viewers/`](../viewers/README.md) with `abm_view_lattice()`.

## A note on cost

Two of these are dominated by one step. Rebellion's vision-radius count is
`abm_neighbours(within = ...)` with a *range* condition, which builds every
(focal, candidate) pair and then filters — quadratic in the population, and the
reason that script is kept small. The co-location form
`within = .group == "patches" & .id == own_.cell` is different: an equality
against an `own_` column is recognised and resolved as a hash join, so
Wolf–Sheep and Ants pay only a linear cost for it.

Models 7 (Flocking) and 12 (Traffic) from the design probe are continuous-space
and need nothing from the spatial grammar, so they have no scripts here.
