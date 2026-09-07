# Spatial reproduction scripts

One script per spatial model, at the size and seed that produced the numbers on
the model's page. Each script prints its table and then writes one figure,
`../figures/<same name>.png`, which is the figure the page embeds.

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

| script | model | tier | runtime | figure |
|---|---|---|---|---|
| `01-life.R` | [1. Conway's Game of Life](../01-life.md) | L0 | 5 min | `../figures/01-life.png` |
| `02-ca1d.R` | [2. Elementary 1-D CA, rules 90 and 30](../02-ca1d.md) | L0 + L1 | 5 s | `../figures/02-ca1d.png` |
| `03-fire.R` | [3. Forest Fire, density sweep](../03-fire.md) | L0 | 90 s | `../figures/03-fire.png` |
| `04-schelling.R` | [4. Schelling with geography, tolerance sweep](../04-schelling.md) | L0 | 3 min | `../figures/04-schelling.png` |
| `05-wolf-sheep.R` | [5. Wolf–Sheep–Grass, three regimes](../05-wolf-sheep.md) | L2 | 4 min | `../figures/05-wolf-sheep.png` |
| `06-ants.R` | [6. Ants, pheromone foraging](../06-ants.md) | L2 | 45 s | `../figures/06-ants.png` |
| `09-langton.R` | [9. Langton's Ant, against a reference](../09-langton.md) | L2+ | 20 s | `../figures/09-langton.png` |
| `10-ising.R` | [10. Ising, temperature sweep](../10-ising.md) | L0 | 2 min | `../figures/10-ising.png` |
| `11-daisyworld.R` | [11. Daisyworld, luminosity sweep](../11-daisyworld.md) | L0 | 2 min | `../figures/11-daisyworld.png` |
| `13-rebellion.R` | [13. Rebellion, legitimacy sweep](../13-rebellion.md) | L2+ | 75 s | `../figures/13-rebellion.png` |

Runtimes are wall-clock on a two-core container and include the figure, which
in a few cases means one extra run at the headline parameters.

## A note on cost

Two of these are dominated by one step. Rebellion's vision-radius count is
`abm_neighbours(within = ...)` with a *range* condition, which builds every
(focal, candidate) pair and then filters — quadratic in the population, and the
reason that script is kept small. The co-location form
`within = .group == "patches" & .id == own_.cell` is different: an equality
against an `own_` column is recognised and resolved as a hash join, so
Wolf–Sheep and Ants pay only a linear cost for it.

Models 7 (Flocking) and 12 (Traffic) from the design probe are continuous-space
and need nothing from the spatial grammar, so they have no scripts here. Model 8
(Sugarscape) is exercised by the `range =` / `axes_only =` / `avoid_occupied =`
cases in the test file rather than by a script of its own.
