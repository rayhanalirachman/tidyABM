# Reproduction scripts

One script per model, at the size and seed that produced the numbers in the
model's reference file. The `testthat` cases in
`tidyABM/tests/testthat/test-models-3.R`, `test-models-4.R` and
`test-models-5.R` run the same models at reduced scale so the suite stays quick.
these are what reproduce the published tables.

Run any of them with `Rscript <file>` from a session where tidyABM is
installed. Each one prints its table and nothing else.

## Part 4

| script | model | approx. runtime |
|---|---|---|
| `m27_granovetter.R` | 27. Threshold model of collective behaviour | 1 s |
| `m28_deffuant.R` | 28. Bounded confidence, pairwise (cluster counts) | 3 min |
| `m28_deffuant_peaks.R` | 28. The same, reported as major peaks | 3 min |
| `m29_hk.R` | 29. Bounded confidence, all-neighbour | 15 s |
| `m30_axelrod_norms.R` | 30. Norms and metanorms, 20 seeds each | 18 min |
| `m31_birthrates.R` | 31. Simple Birth Rates | 4 s |
| `m32_team.R` | 32. Team Assembly | 2 min |
| `m33_language.R` | 33. Language Change | 20 s |
| `m34_epidem.R` | 34. epiDEM Basic | 4 min |
| `m35_ga.R` | 35. Simple Genetic Algorithm | 1 min |
| `m36_cascade.R` | 36. Information cascade | 4 min |

## Part 5

| script | model | approx. runtime |
|---|---|---|
| `m37_virus_network.R` | 37. Virus on a Network, SIS and SIR | 1 min |
| `m38_watts_cascades.R` | 38. Global cascades, 6 mean degrees × 40 seeds | 4 min |
| `m39_sznajd.R` | 39. Sznajd, 3 densities × 30 runs | 9 min |
| `m40_naming_game.R` | 40. Naming Game, 3 sizes × 3 runs | 5 min |
| `m41_minority_game.R` | 41. Minority Game, 7 memory lengths | 2 min |
| `m42_kirman_ants.R` | 42. Kirman's ants, herding and not | 7 min |
| `m43_zero_intelligence.R` | 43. Zero-intelligence traders, ZI-C and ZI-U | 5 min |
| `m44_ultimatum.R` | 44. Ultimatum game, 5 values of w × 3 runs | 5 min |
| `m45_hotelling.R` | 45. Hotelling's Law, 2 / 3 / 5 shops × 5 seeds | 2 min |
| `m46_beer_game.R` | 46. Beer game, supply line seen and ignored | 2 s |

## Part 6

| script | model | approx. runtime |
|---|---|---|
| `m47_thresholds.R` | 47. Response thresholds, 2 threshold regimes × 5 seeds | 5 min |
| `m48_garbage_can.R` | 48. Garbage can, 2 access structures × 4 loads × 20 seeds | 4 min |
| `m49_firms.R` | 49. Emergence of firms, 5 seeds plus one traced run | 2 min |
| `m50_nk.R` | 50. Rugged landscapes, 5 values of K, with and without jumps | 2 min |
| `m51_vaccination.R` | 51. Vaccination, 3 networks × 5 costs × 2 seeds | 10 min |
| `m52_bankrun.R` | 52. Bank runs, 4 shocks × 2 orderings | 15 s |
| `m53_neutral.R` | 53. Random copying, 5 parameterisations | 2 min |
| `m54_image_scoring.R` | 54. Image scoring, 6 observation rates × 3 seeds | 15 min |
| `m55_deferred_acceptance.R` | 55. Deferred acceptance, 4 sizes × 5 seeds | 4 min |
| `m56_predprey.R` | 56. Predator–prey, both functional responses | 2 min |

`m39_sznajd.R` takes the starting densities as command-line arguments, so
`Rscript m39_sznajd.R 0.5` runs just the one column. `m52_bankrun.R` and
`m54_image_scoring.R` do the same with the impatience shock and the observation
rate respectively, because both are slow enough to be worth splitting across
sittings.

Timings are from a two-core container and are wall-clock, not CPU. A laptop
will be faster. Sizes were chosen so that every script finishes in one sitting.
Where that makes a run too short to settle, as with Kirman's ants, whose chain makes
only a handful of excursions between the two extremes in 4000 ticks, and the
response-threshold stimulus, which is still climbing at 2000, the model's
reference file says so rather than quoting a converged number it did not reach.

## What Part 7 changed here

Five of these scripts were rewritten against the primitives that closed the
open items, and every one of them reproduces the table it produced before.
`m29_hk.R` uses `abm_neighbours(within =)` instead of a hand-rolled `vapply()`.
`m47_thresholds.R` uses `abm_global(.by =)` instead of one global per task.
`m30_axelrod_norms.R` uses `abm_draw()` so that the punishments handed out and
the punishments received are the same events (its numbers moved slightly,
because the random stream is not the same one, and its result did not).
`m56_predprey.R` passes `record = "globals"`, since everything it reports is a
count per tick.

`m52_bankrun.R` was not rewritten at all and went from about ten minutes to
about fifteen seconds, because `abm_sequential()` was rebuilt underneath it.
Its numbers are bit-identical.

Two timings went the other way. `m29_hk.R` and `m47_thresholds.R` are slower
than the versions they replace: `within =` builds an explicit (focal,
candidate) view, which is the same O(n²) the `vapply()` was but with a tibble's
constant factor, and `.by` evaluates its rule once per key against the whole
population. Both are worth it, since the models read as specifications now, but the
trade is real and worth knowing before reaching for `within =` on a very large
population.
