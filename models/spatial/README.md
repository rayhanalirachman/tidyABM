# The spatial corpus: 10 lattice models

The lattice half of [the corpus](../README.md), kept separate from the
[fifty-six non-spatial models](../non-spatial/README.md). Those models live in
attribute space or on an abstract network; these live on a lattice, and they are
what the spatial grammar was built against.

The design probe that produced the grammar is [`design-probe.md`](design-probe.md);
the surface it settled on is [`grammar-spec.md`](grammar-spec.md).

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

Each model has a page with the concept, the package code, the numbers and a
figure, and a script that reproduces all three.

| # | model | tier | validated against |
|---|---|---|---|
| 1 | [Conway's Game of Life](01-life.md) | L0 | blinker period 2; glider centroid gains exactly (+1, −1) every 4 ticks; random soup relaxes to 0.0281 by tick 1500 against a published ash density of 0.0287 |
| 2 | [Elementary 1-D CA (rule 90)](02-ca1d.md) | L0 + L1 | row *t* equals `choose(t, 0:t) %% 2` exactly, every row to *t* = 24 |
| 3 | [Forest Fire](03-fire.md) | L0 | burned fraction 0.05 → 0.70 and spanning 0 → 1 between density 0.55 and 0.60, bracketing *p*<sub>c</sub> ≈ 0.5927 |
| 4 | [Schelling, with geography](04-schelling.md) | L0 | same-type share 0.575 (the random 70/30 baseline) → 0.713 at 30% tolerance |
| 5 | [Wolf–Sheep–Grass](05-wolf-sheep.md) | L2 | `wolf_gain = 10` starves the wolves out; at 20 both persist in a bounded cycle, out of phase (peak cross-correlation at lag −39) |
| 6 | [Ants (pheromone foraging)](06-ants.md) | L2 | 35 of 42 food units taken in 400 ticks; trail strength peaks at 938 and decays as piles are spent |
| 9 | [Langton's Ant](09-langton.md) | L2+ | cell-for-cell agreement with a reference implementation at 100, 500 and 2000 steps |
| 10 | [Ising (checkerboard)](10-ising.md) | L0 | magnetisation 0.999 → 0.040 across *T*<sub>c</sub> = 2.2692, susceptibility peaking there |
| 11 | [Daisyworld](11-daisyworld.md) | L0 | temperature spans 12.7 while bare rock spans 16.8; white fraction rises 0.34 → 0.66 to hold it |
| 13 | [Rebellion](13-rebellion.md) | L2+ | quiet ticks 100% → 92% → 56% as legitimacy falls 0.88 → 0.82 → 0.70 |

The numbering follows the design probe, which is why 7, 8 and 12 are missing.
Models 7 (Flocking) and 12 (Traffic) are **continuous-space** and need nothing
from this grammar — they are ordinary column arithmetic and belong with the
non-spatial corpus. Model 8 (Sugarscape) is covered by the `range =` /
`axes_only =` / `avoid_occupied =` tests in
[`../../tests/testthat/test-spatial.R`](../../tests/testthat/test-spatial.R)
rather than by a page of its own.

## Running them

```bash
Rscript models/spatial/scripts/01-life.R
```

Each script is self-contained — its own `library(tidyABM)` — prints its table
and writes its page's figure into `figures/`. It needs `tidyABM` and `ggplot2`
installed. See [`scripts/README.md`](scripts/README.md) for runtimes.

The `testthat` cases in
[`../../tests/testthat/test-spatial.R`](../../tests/testthat/test-spatial.R) run
the same checks at reduced scale so the suite stays quick.

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
   `.resolve = "sum"` — see [`06-ants.md`](06-ants.md).

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
