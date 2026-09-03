# Spatial models: tidyABM

The spatial corpus, kept **separate from the 56 non-spatial models** in
[`../models/`](../models/README.md). Those models live in attribute space or on
an abstract network; these live on a lattice, and they are what the spatial
grammar was built against.

The design probe that produced the grammar is
[`../work in progress/spatial-grammar-test.md`](../work%20in%20progress/spatial-grammar-test.md);
the surface it settled on is
[`../work in progress/spatial-grammar-spec.md`](../work%20in%20progress/spatial-grammar-spec.md).

## The idea

A lattice **is a network**. `abm_network(type = "grid")` produces the same
`from`/`to` edge tibble every other network type produces, so patches are
ordinary agents and `abm_neighbours()`, `abm_match(pair = "network")`,
`abm_link()` and `abm_edges()` all work on a grid unchanged. A mobile agent's
location is a patch `.id` held in a reserved `.cell` column; moving is writing
that column. There is no second medium and no patch-specific rule syntax.

| tier | adds | unlocks |
|---|---|---|
| **L0** | `abm_network(type = "grid" \| "line", dims, diagonals, torus, on)`, `.x` / `.y`, count inheritance, `abm_grid()` | every patch-only model |
| **L1** | `abm_neighbours(..., .where = "west")` | ordered-neighbour lattices (1-D CA) |
| **L2** | `.cell`, `at =`, the `within =` equijoin, `abm_tell(to = <id col>)`, `abm_match(.by = <col>)`, `abm_move()` | turtle-≠-patch models |
| **L2+** | `abm_move(direction =, range =, axes_only =, avoid_occupied =)` | Sugarscape, Langton's Ant, Rebellion |

## The models

| # | model | tier | script | validated against |
|---|---|---|---|---|
| 1 | Conway's Game of Life | L0 | [`s01_life.R`](scripts/s01_life.R) | blinker period 2; glider centroid gains exactly (+1, −1) every 4 ticks; random soup relaxes to 0.0281 by tick 1500 against a published ash density of 0.0287 |
| 2 | Elementary 1-D CA (rule 90) | L0 + L1 | [`s02_ca1d.R`](scripts/s02_ca1d.R) | row *t* equals `choose(t, 0:t) %% 2` exactly, every row to *t* = 24 |
| 3 | Forest Fire | L0 | [`s03_fire.R`](scripts/s03_fire.R) | burned fraction 0.05 → 0.70 and spanning 0 → 1 between density 0.55 and 0.60, bracketing *p*<sub>c</sub> ≈ 0.5927 |
| 4 | Schelling, with geography | L0 | [`s04_schelling.R`](scripts/s04_schelling.R) | same-type share 0.575 (the random 70/30 baseline) → 0.713 at 30% tolerance |
| 5 | Wolf–Sheep–Grass | L2 | [`s05_wolf_sheep.R`](scripts/s05_wolf_sheep.R) | `wolf_gain = 10` starves the wolves out; at 20 both persist in a bounded cycle, out of phase (peak cross-correlation at lag −39) |
| 6 | Ants (pheromone foraging) | L2 | [`s06_ants.R`](scripts/s06_ants.R) | 35 of 42 food units taken in 400 ticks; trail strength peaks at 938 and decays as piles are spent |
| 9 | Langton's Ant | L2+ | [`s09_langton.R`](scripts/s09_langton.R) | cell-for-cell agreement with a reference implementation at 100, 500 and 2000 steps |
| 10 | Ising (checkerboard) | L0 | [`s10_ising.R`](scripts/s10_ising.R) | magnetisation 0.999 → 0.040 across *T*<sub>c</sub> = 2.2692, susceptibility peaking there |
| 11 | Daisyworld | L0 | [`s11_daisyworld.R`](scripts/s11_daisyworld.R) | temperature spans 12.7 while bare rock spans 16.8; white fraction rises 0.34 → 0.66 to hold it |
| 13 | Rebellion | L2+ | [`s13_rebellion.R`](scripts/s13_rebellion.R) | quiet ticks 100% → 92% → 56% as legitimacy falls 0.88 → 0.82 → 0.70 |

Models 7 (Flocking) and 12 (Traffic) from the probe are **continuous-space** and
need nothing from this grammar — they are ordinary column arithmetic and belong
with the non-spatial corpus. Model 8 (Sugarscape) is covered by the
`range =` / `axes_only =` / `avoid_occupied =` tests in
`../tests/testthat/test-spatial.R` rather than by its own page.

## Running them

```bash
Rscript models-spatial/scripts/s01_life.R
```

Each script loads the package with `devtools::load_all()` if it is not
installed, prints its table, and nothing else. The `testthat` cases in
[`../tests/testthat/test-spatial.R`](../tests/testthat/test-spatial.R) run the
same checks at reduced scale so the suite stays quick.

## Three places the probe's sketches do not run as written

The 13-model probe was a design document, not tested code. Three of its
sketches need a small correction, and each one is the grammar behaving as
documented rather than a gap:

1. **`rule[[i]]` must be `rule[i]`.** `[[` does not vectorise, so the 1-D CA's
   `rule[[4 * s_w + 2 * s + s_e + 1]]` fails on a population of more than one.
2. **`abm_rules()` is simultaneous.** Daisyworld's sketch computes `albedo` and
   then reads it in `local_t` inside one `abm_rules()` call. Every rule in a
   call sees the state at the *start* of the step, so that needs two calls.
3. **`abm_tell()`'s right-hand side is evaluated in the *sender's* row.** Ants'
   `abm_tell(chemical ~ chemical + 60, to = .cell)` reads the *ant's*
   `chemical`, which does not exist. The ant already knows the cell's value
   (`here_chem`, from the `within =` join), and an additive deposit wants a
   per-tick mailbox column cleared at the top of the tick and summed with
   `.resolve = "sum"` — see [`s06_ants.R`](scripts/s06_ants.R).

Two of the probe's predicted *results* also did not survive contact:

* **Wolf–Sheep's collapse is a bottleneck, not an extinction.** The low end
  behaves as predicted — at `wolf_gain = 10` the wolves starve out. The high end
  does not flip the system to extinction so much as squeeze it: minimum sheep
  fall 101 → 36 → 24 → 2 → 1 as `wolf_gain` goes 20 → 40 → 60 → 100 → 160,
  with wolves peaking at 320. Both populations survive 300 ticks even at 160,
  on the edge of ending.
* **Ants needs an exploration term.** A pure lattice argmax
  (`uphill(chemical)`) traps an ant on a local maximum and it stops finding
  food; the probe flagged this as the likely friction, and it is. Adding a
  small random term inside `uphill()` — the lattice analogue of NetLogo's
  `wiggle` — is what makes trails form at all.
