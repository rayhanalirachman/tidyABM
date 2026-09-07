# Reproduction scripts

One script per model, at the size and seed that produced the numbers in the
model's reference page. Each script prints its table and then writes one figure,
`../figures/<same name>.png`, which is the figure the page embeds.

Run any of them with `Rscript <file>` from a session where `tidyABM` and
`ggplot2` are installed. They work from the repository root or from this
directory:

```
Rscript models/non-spatial/scripts/43-zero-intelligence-traders.R
```

Every script is self-contained: its own `library(tidyABM)`, its own parameters,
no shared setup file.

The `testthat` cases in `../../../tests/testthat/test-models-3.R`,
`test-models-4.R` and `test-models-5.R` run the same models at reduced scale so
the suite stays quick. These scripts are what reproduce the published tables.

## Part 1: the founding thirteen

| script | model | approx. runtime |
|---|---|---|
| `01-simple-economy.R` | 1. Simple Economy | 6 s |
| `02-el-farol-short.R` | 2. El Farol, short form | 1 s |
| `03-pd-basic-well-mixed-prisoner-s-dilemma-with-imitation.R` | 3. PD Basic | 2 s |
| `04-ethnocentrism-short.R` | 4. Ethnocentrism, short form | 2 s |
| `05-rumour-mill.R` | 5. Rumour Mill | 2 s |
| `06-party-segregation-without-geography.R` | 6. Party, with a population sweep | 10 s |
| `07-market-supply-and-demand.R` | 7. Market, bilateral bargaining | 1 s |
| `08-voter-model-on-a-network.R` | 8. Voter model, with a degree sweep | 10 s |
| `09-public-goods-game.R` | 9. Public Goods Game | 1 s |
| `10-iterated-prisoner-s-dilemma-with-fixed-partners.R` | 10. IPD with fixed partners | 2 s |
| `11-preferential-attachment.R` | 11. Preferential Attachment | 5 s |
| `12-random-consumption-zakah-short.R` | 12. Random Consumption Zakah, short form | 2 s |
| `13-bank-reserves.R` | 13. Bank Reserves | 5 s |

## Part 2: three of those, corrected

| script | model | approx. runtime |
|---|---|---|
| `14-el-farol-with-inductive-agents.R` | 14. El Farol, inductive, plus a predictor-count sweep | 20 s |
| `15-ethnocentrism-hammond-axelrod.R` | 15. Ethnocentrism, 2 conditions × 5 draws | 5 min |
| `16-zakah-with-a-risk-process.R` | 16. Zakah with a risk process, with and without the transfer | 15 s |

## Part 3: the first stress test

| script | model | approx. runtime |
|---|---|---|
| `17-giant-component.R` | 17. Giant Component, 10 tick budgets | 10 s |
| `18-small-worlds.R` | 18. Small Worlds, 4 rewiring rates | 90 s |
| `19-fireflies.R` | 19. Fireflies | 10 s |
| `20-sir-on-a-network-with-recovery-timers.R` | 20. SIR on a network, 3 values of beta | 45 s |
| `21-genetic-drift-wright-fisher.R` | 21. Genetic Drift, plus 120 fixation replicates | 60 s |
| `22-hawks-and-doves.R` | 22. Hawks and Doves, 7 values of C | 2 min |
| `23-divide-the-cake.R` | 23. Divide the Cake | 20 s |
| `24-sex-ratio-equilibrium.R` | 24. Sex Ratio, from 25% and 75% male | 3 min |
| `25-axelrod-s-cultural-dissemination.R` | 25. Cultural dissemination, 3 values of q | 90 s |
| `26-pd-n-person-iterated.R` | 26. PD N-Person, 4 matchups | 3 min |

## Part 4: the second stress test

| script | model | approx. runtime |
|---|---|---|
| `27-threshold-model-of-collective-behaviour.R` | 27. Threshold model of collective behaviour | 1 s |
| `28-bounded-confidence-pairwise.R` | 28. Bounded confidence, pairwise (cluster counts) | 3 min |
| `28-bounded-confidence-pairwise-peaks.R` | 28. The same, reported as major peaks | 3 min |
| `29-bounded-confidence-all-neighbour.R` | 29. Bounded confidence, all-neighbour | 15 s |
| `30-norms-and-metanorms.R` | 30. Norms and metanorms, 20 seeds each | 18 min |
| `31-simple-birth-rates.R` | 31. Simple Birth Rates | 4 s |
| `32-team-assembly.R` | 32. Team Assembly | 2 min |
| `33-language-change.R` | 33. Language Change | 20 s |
| `34-epidem-basic.R` | 34. epiDEM Basic | 4 min |
| `35-simple-genetic-algorithm.R` | 35. Simple Genetic Algorithm | 1 min |
| `36-information-cascade.R` | 36. Information cascade | 4 min |

## Part 5: the third stress test

| script | model | approx. runtime |
|---|---|---|
| `37-virus-on-a-network.R` | 37. Virus on a Network, SIS and SIR | 1 min |
| `38-global-cascades-on-random-networks.R` | 38. Global cascades, 6 mean degrees × 40 seeds | 4 min |
| `39-sznajd-model.R` | 39. Sznajd, 3 densities × 30 runs | 9 min |
| `40-naming-game.R` | 40. Naming Game, 3 sizes × 3 runs | 5 min |
| `41-minority-game.R` | 41. Minority Game, 7 memory lengths | 2 min |
| `42-kirmans-ants.R` | 42. Kirman's ants, herding and not | 7 min |
| `43-zero-intelligence-traders.R` | 43. Zero-intelligence traders, ZI-C and ZI-U | 5 min |
| `44-ultimatum-game.R` | 44. Ultimatum game, 5 values of w × 3 runs | 5 min |
| `45-hotellings-law.R` | 45. Hotelling's Law, 2 / 3 / 5 shops × 5 seeds | 2 min |
| `46-beer-distribution-game.R` | 46. Beer game, supply line seen and ignored | 2 s |

## Part 6: the fourth stress test

| script | model | approx. runtime |
|---|---|---|
| `47-response-thresholds-and-division-of-labour.R` | 47. Response thresholds, 2 regimes × 5 seeds | 5 min |
| `48-garbage-can-model.R` | 48. Garbage can, 2 access structures × 4 loads × 20 seeds | 4 min |
| `49-emergence-of-firms.R` | 49. Emergence of firms, 5 seeds plus one traced run | 2 min |
| `50-rugged-landscapes.R` | 50. Rugged landscapes, 5 values of K, with and without jumps | 2 min |
| `51-vaccination-imitation.R` | 51. Vaccination, 3 networks × 5 costs × 2 seeds | 10 min |
| `52-bank-runs.R` | 52. Bank runs, 4 shocks × 2 orderings | 15 s |
| `53-neutral-model.R` | 53. Random copying, 5 parameterisations | 2 min |
| `54-image-scoring.R` | 54. Image scoring, 6 observation rates × 3 seeds | 15 min |
| `55-deferred-acceptance.R` | 55. Deferred acceptance, 4 sizes × 5 seeds | 4 min |
| `56-predator-prey.R` | 56. Predator–prey, both functional responses | 2 min |

`39-sznajd-model.R` takes the starting densities as command-line arguments, so
`Rscript 39-sznajd-model.R 0.5` runs just the one column. `52-bank-runs.R` and
`54-image-scoring.R` do the same with the impatience shock and the observation
rate respectively, because both are slow enough to be worth splitting across
sittings. Running one of them with arguments still writes the figure, from
whatever subset was asked for.

Timings for Parts 4 to 6 are from a two-core container and are wall-clock, not
CPU. A laptop will be faster. Sizes were chosen so that every script finishes in
one sitting. Where that makes a run too short to settle, as with Kirman's ants,
whose chain makes only a handful of excursions between the two extremes in 4000
ticks, and the response-threshold stimulus, which is still climbing at 2000, the
model's reference page says so rather than quoting a converged number it did not
reach.

## A note on the backfilled scripts

Parts 1 to 3 were written up before the scripts existed; their scripts were
added afterwards and reproduce the numbers each page quotes. Three of them found
something the page had not said out loud, and say so in the script rather than
quietly papering over it:

- **6. Party** is a no-op at the 200 agents the page uses. Two hundred opinions
  on the unit interval sit 0.003 apart, so the 0.05 tolerance never binds and
  nothing moves. The script runs the page's parameters and then sweeps the
  population size, where the mechanism does appear.
- **12. Zakah, short form** stops working at tick 2, not "about ten": the first
  transfer is large enough to lift the whole lower tail past the fixed line at
  once.
- **9. Public Goods Game** has no imitation step, so contributing cannot
  collapse inside this model. What the script reports is the free-rider's
  per-round advantage, which is the thing any imitation rule laid on top would
  act on.

## What Part 7 changed here

Five of these scripts were rewritten against the primitives that closed the
open items, and every one of them reproduces the table it produced before.
`29-bounded-confidence-all-neighbour.R` uses `abm_neighbours(within =)` instead
of a hand-rolled `vapply()`.
`47-response-thresholds-and-division-of-labour.R` uses `abm_global(.by =)`
instead of one global per task. `30-norms-and-metanorms.R` uses `abm_draw()` so
that the punishments handed out and the punishments received are the same events
(its numbers moved slightly, because the random stream is not the same one, and
its result did not). `56-predator-prey.R` passes `record = "globals"`, since
everything it reports is a count per tick.

`52-bank-runs.R` was not rewritten at all and went from about ten minutes to
about fifteen seconds, because `abm_sequential()` was rebuilt underneath it.
Its numbers are bit-identical.

Two timings went the other way. `29-bounded-confidence-all-neighbour.R` and
`47-response-thresholds-and-division-of-labour.R` are slower than the versions
they replace: `within =` builds an explicit (focal, candidate) view, which is
the same O(n²) the `vapply()` was but with a tibble's constant factor, and `.by`
evaluates its rule once per key against the whole population. Both are worth it,
since the models read as specifications now, but the trade is real and worth
knowing before reaching for `within =` on a very large population.
