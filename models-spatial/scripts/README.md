# Spatial reproduction scripts

One script per spatial model, at the size and seed that produced the numbers in
[`../README.md`](../README.md). The `testthat` cases in
[`../../tests/testthat/test-spatial.R`](../../tests/testthat/test-spatial.R) run
the same checks at reduced scale so the suite stays quick; these are the full
runs.

Run any of them from this directory:

```bash
Rscript s01_life.R
```

`_setup.R` loads the package sources from the working tree in preference to any
installed copy, so a script always exercises the code next to it.

| script | model | tier | runtime |
|---|---|---|---|
| `s01_life.R` | 1. Conway's Game of Life | L0 | 5 min |
| `s02_ca1d.R` | 2. Elementary 1-D CA, rules 90 and 30 | L0 + L1 | 5 s |
| `s03_fire.R` | 3. Forest Fire, density sweep | L0 | 90 s |
| `s04_schelling.R` | 4. Schelling with geography, tolerance sweep | L0 | 3 min |
| `s05_wolf_sheep.R` | 5. Wolf–Sheep–Grass, three regimes | L2 | 4 min |
| `s06_ants.R` | 6. Ants, pheromone foraging | L2 | 45 s |
| `s09_langton.R` | 9. Langton's Ant, against a reference | L2+ | 20 s |
| `s10_ising.R` | 10. Ising, temperature sweep | L0 | 2 min |
| `s11_daisyworld.R` | 11. Daisyworld, luminosity sweep | L0 | 2 min |
| `s13_rebellion.R` | 13. Rebellion, legitimacy sweep | L2+ | 75 s |

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
