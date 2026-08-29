# ABM models reference: tidyABM

Fifty-six models implemented in tidyABM, one file each, with the concept, a
NetLogo-style sketch of the original where one exists, the working package code
and the numbers the run produced. Every model here runs. Every one with a
behavioural claim is pinned by a test in `tidyABM/tests/testthat/`.

Each is written as the same three parts, `abm_setup()` then `abm_go()` then
`abm_run()`, kept as three separate statements, so the code on a page shows the
whole model rather than a fragment:

```r
world  <- abm_setup(...)                                # 1. who is in the model
go     <- abm_go(...)                                   # 2. what happens each tick
result <- abm_run(world, go, ticks = ..., seed = ...)   # 3. run it
```

Start with [the grammar](grammar.md) if you have not written a tidyABM model
before. [Open items](open-items.md) is the list of things the grammar still
cannot say, and is the most useful page in here if you are extending the
package rather than using it. [What each stress test changed](what-changed.md)
is the history of how the grammar got its current shape, and
[sources](sources.md) is every citation in one table.

## How this is organised

Models 1–13 are the set the grammar was designed against. 14–16 are corrected
versions of three of those, whose short form runs but does not reproduce the
result the model is known for. 17–26, 27–36, 37–46 and 47–56 are four rounds of
stress testing, each chosen to ask for something the previous rounds did not.

The stress tests are what the package is made of. Of the forty models in
Parts 3 to 6, eighteen needed a change to the grammar, and every feature in
tidyABM that is not in the founding thirteen exists because one of them asked
for it.

Part 7 adds no models. Each round left entries behind on
[open items](open-items.md), shapes a model had asked for that were too small to
stop a stress test over, and by the end of Part 6 there were nine. That
round was spent closing seven of them instead of collecting more models, so
five of the pages here now show different code for the same result: 29 uses
`abm_neighbours(within =)`, 30 uses `abm_draw()`, 47 uses `abm_global(.by =)`,
52 runs forty-four times faster without being touched, and 56 uses
`abm_run(record =)`. [What each stress test changed](what-changed.md) says what
that exercise showed that a stress test cannot.

The corrections themselves were then read back the same way, which is where 14
and 15 got the code they show now. El Farol turned out to need nothing at all,
only the list column two later models had already found, and ethnocentrism
turned out to have been running on a network that its own births and deaths had
eroded to a mean degree of 0.97, which `abm_birth(links =)` fixes.

### Part 1: the founding thirteen

The set the grammar was designed against.

| # | model |
|---|---|
| 1 | [Simple Economy (Wilensky & Rand, ch. 2)](01-simple-economy.md) |
| 2 | [El Farol, short form (Arthur 1994)](02-el-farol-short.md) |
| 3 | [PD Basic, a well-mixed prisoner's dilemma with imitation](03-pd-basic-well-mixed-prisoner-s-dilemma-with-imitation.md) |
| 4 | [Ethnocentrism, short form](04-ethnocentrism-short.md) |
| 5 | [Rumour Mill](05-rumour-mill.md) |
| 6 | [Party, segregation without geography](06-party-segregation-without-geography.md) |
| 7 | [Market, supply and demand (after Primer)](07-market-supply-and-demand.md) |
| 8 | [Voter Model on a network](08-voter-model-on-a-network.md) |
| 9 | [Public Goods Game](09-public-goods-game.md) |
| 10 | [Iterated Prisoner's Dilemma with fixed partners](10-iterated-prisoner-s-dilemma-with-fixed-partners.md) |
| 11 | [Preferential Attachment (Wilensky & Rand, ch. 5)](11-preferential-attachment.md) |
| 12 | [Random Consumption Zakah, short form (custom)](12-random-consumption-zakah-short.md) |
| 13 | [Bank Reserves (Wilensky, NetLogo Social Science)](13-bank-reserves.md) |

### Part 2: three of those, corrected

Models 2, 4 and 12 run correctly and do not show the behaviour they are known for. In each case the description left out the mechanism that produces the result. Full workings in `vignette("corrections")`.

| # | model |
|---|---|
| 14 | [El Farol with inductive agents (Arthur 1994)](14-el-farol-with-inductive-agents.md) |
| 15 | [Ethnocentrism, Hammond & Axelrod (2006)](15-ethnocentrism-hammond-axelrod.md) |
| 16 | [Zakah with a risk process](16-zakah-with-a-risk-process.md) |

### Part 3: the first stress test

Chosen so that each one asks something of the grammar the earlier models did not. Five needed a package addition, five did not. These produced `abm_link()` / `abm_unlink()` / `abm_neighbours()`, the `one_of` pairing mode, `.scope = "population"` and `abm_birth(inherit =)`.

| # | model |
|---|---|
| 17 | [Giant Component (NetLogo Networks)](17-giant-component.md) |
| 18 | [Small Worlds (Watts & Strogatz; NetLogo Networks)](18-small-worlds.md) |
| 19 | [Fireflies (Buck 1988; NetLogo Biology)](19-fireflies.md) |
| 20 | [SIR on a network with recovery timers](20-sir-on-a-network-with-recovery-timers.md) |
| 21 | [Genetic Drift / Wright–Fisher (NetLogo GenDrift P Global)](21-genetic-drift-wright-fisher.md) |
| 22 | [Hawks and Doves (Maynard Smith & Price 1973)](22-hawks-and-doves.md) |
| 23 | [Divide the Cake (Skyrms / Harms; NetLogo)](23-divide-the-cake.md) |
| 24 | [Sex Ratio Equilibrium (Fisher; NetLogo Biology)](24-sex-ratio-equilibrium.md) |
| 25 | [Axelrod's cultural dissemination (Axelrod 1997)](25-axelrod-s-cultural-dissemination.md) |
| 26 | [PD N-Person Iterated (NetLogo Social Science)](26-pd-n-person-iterated.md) |

### Part 4: the second stress test

Chosen the same way. Three needed a package addition, seven did not. These produced `own_<col>` inside `abm_neighbours()`, `abm_network(type = "complete")` and clique linking. Two of them are among the sharpest quantitative checks in the corpus.

| # | model |
|---|---|
| 27 | [Threshold model of collective behaviour (Granovetter 1978)](27-threshold-model-of-collective-behaviour.md) |
| 28 | [Bounded confidence, pairwise (Deffuant, Neau, Amblard & Weisbuch 2000)](28-bounded-confidence-pairwise.md) |
| 29 | [Bounded confidence, all-neighbour (Hegselmann & Krause 2002)](29-bounded-confidence-all-neighbour.md) |
| 30 | [Norms and metanorms (Axelrod 1986)](30-norms-and-metanorms.md) |
| 31 | [Simple Birth Rates (Wilensky 1997, NetLogo Biology)](31-simple-birth-rates.md) |
| 32 | [Team Assembly (Guimerà, Uzzi, Spiro & Amaral 2005; NetLogo Networks)](32-team-assembly.md) |
| 33 | [Language Change (Troutman & Wilensky 2007, NetLogo Social Science)](33-language-change.md) |
| 34 | [epiDEM Basic (Yang & Wilensky 2011, NetLogo)](34-epidem-basic.md) |
| 35 | [Simple Genetic Algorithm (Wilensky 1998, NetLogo Computer Science)](35-simple-genetic-algorithm.md) |
| 36 | [Information cascade (Bikhchandani, Hirshleifer & Welch 1992)](36-information-cascade.md) |

### Part 5: the third stress test

Chosen against the *Open items* list rather than against the grammar: each one was picked because something on that list said it could not be written. Four needed a package addition, and two of the additions closed entries that had been open since Part 3. Two more entries came off the list without any code changing at all.

| # | model |
|---|---|
| 37 | [Virus on a Network (Stonedahl & Wilensky 2008, NetLogo Networks)](37-virus-on-a-network.md) |
| 38 | [Global cascades on random networks (Watts 2002)](38-global-cascades-on-random-networks.md) |
| 39 | [Sznajd model (Sznajd-Weron & Sznajd 2000)](39-sznajd-model.md) |
| 40 | [Naming Game (Baronchelli et al. 2006)](40-naming-game.md) |
| 41 | [Minority Game (Challet & Zhang 1997)](41-minority-game.md) |
| 42 | [Kirman's ants (Kirman 1993)](42-kirmans-ants.md) |
| 43 | [Zero-intelligence traders in a double auction (Gode & Sunder 1993)](43-zero-intelligence-traders.md) |
| 44 | [Fairness versus reason in the ultimatum game (Nowak, Page & Sigmund 2000)](44-ultimatum-game.md) |
| 45 | [Hotelling's Law (Hotelling 1929; NetLogo Social Science)](45-hotellings-law.md) |
| 46 | [The Beer Distribution Game (Sterman 1989)](46-beer-distribution-game.md) |

### Part 6: the fourth stress test

Chosen to leave the corpus's home ground. The first forty-six models are mostly
opinion dynamics, evolutionary games and network processes. These ten are
entomology, organization theory, industrial economics, organizational search,
behavioural epidemiology, banking, cultural evolution, the evolution of
cooperation, market design and ecology. Six needed a package addition, and two
of them found bugs rather than gaps.

| # | model |
|---|---|
| 47 | [Response thresholds and the division of labour (Bonabeau, Theraulaz & Deneubourg 1996)](47-response-thresholds-and-division-of-labour.md) |
| 48 | [A garbage can model of organizational choice (Cohen, March & Olsen 1972)](48-garbage-can-model.md) |
| 49 | [The emergence of firms (Axtell 1999)](49-emergence-of-firms.md) |
| 50 | [Adaptation on a rugged landscape (Kauffman 1993; Levinthal 1997)](50-rugged-landscapes.md) |
| 51 | [Imitation dynamics of vaccination (Fu, Rosenbloom, Wang & Nowak 2011)](51-vaccination-imitation.md) |
| 52 | [Bank runs and the sequential service constraint (Diamond & Dybvig 1983)](52-bank-runs.md) |
| 53 | [Random copying and the neutral model (Bentley, Hahn & Shennan 2004)](53-neutral-model.md) |
| 54 | [Indirect reciprocity by image scoring (Nowak & Sigmund 1998)](54-image-scoring.md) |
| 55 | [Deferred acceptance (Gale & Shapley 1962)](55-deferred-acceptance.md) |
| 56 | [Predator and prey without space (Lotka 1925; Volterra 1926)](56-predator-prey.md) |

## Reproducing the numbers

Every table in these files came from a script in
[`scripts/`](scripts/README.md), run at the size and seed the script names.
The `testthat` cases run the same models at reduced scale so the suite stays
quick. The scripts are what reproduce the published tables.

```
Rscript scripts/m43_zero_intelligence.R
```

Most of the scripts take a few minutes. The longest is about twenty. Sizes were
chosen so that each script finishes in one sitting, and where a run is too short
to settle, which happens once, with Kirman's ants, the file says so rather than
quoting a converged number it did not reach.
