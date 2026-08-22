# Changelog

## tidyABM 0.0.0.9000

First working version. The package implements the grammar developed
against thirteen reference models.

- [`abm_agents()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_agents.md),
  [`abm_network()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_network.md)
  and
  [`abm_setup()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_setup.md)
  declare the world.
- [`abm_go()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_go.md)
  takes an ordered sequence of steps, dispatched by type and position,
  and validates the sequence once at construction rather than on every
  tick.
- [`abm_match()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_match.md)
  supports four pairing modes (`"random"`, `"opposite_group"`,
  `"nearest"`, `"network"`), generalised to groups of `size` where that
  is well defined. Passing an argument a mode does not use is an error.
- [`abm_rules()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_rules.md)
  updates agents simultaneously;
  [`abm_sequential()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_sequential.md)
  updates them one at a time so that writes to globals are visible
  within the step;
  [`abm_global()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_global.md)
  updates population-level values;
  [`abm_birth()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_birth.md)
  and
  [`abm_death()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_death.md)
  change the size of the population.
- [`abm_run()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_run.md)
  takes a `seed` and restores the caller’s random state. It seeds the
  run, not the model: a randomly drawn starting population also needs
  `abm_setup(seed =)`, and both are documented together.

Three points where the implementation departs from the design notes:

- The role column is `.role`, not `role`, so that it cannot collide with
  a user’s own column. Package-managed columns are all dot-prefixed
  (`.id`, `.group`, `.role`).
- `abm_birth(cost =)` takes `column ~ expression` formulas rather than
  the string `"halve"`, since which column reproduction costs is
  model-specific. `cost = resource ~ resource / 2` reproduces the
  intended behaviour.
- `abm_match(resolve = "negotiate")` needs `positions` and `limits` to
  name the bid/ask and reservation columns. There is no way to infer
  them.

`abm_match(pair = "network", from = "parent")` links a newborn to the
agent it was cloned from, which is the only way to put offspring near
their kin. It exists because Hammond and Axelrod’s ethnocentrism model
does not work without it — see
[`vignette("corrections")`](https://rayhanalirachman.github.io/tidyabm/articles/corrections.md).

`abm_birth(when =)` and `abm_death(when =)` conditions are evaluated
through the same data-masking path as
[`abm_rules()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_rules.md),
so [`n()`](https://dplyr.tidyverse.org/reference/context.html),
[`if_else()`](https://dplyr.tidyverse.org/reference/if_else.html) and
friends mean the same thing in a condition as in a rule.

Two open questions from the design notes are now resolved:

- [`abm_death()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_death.md)
  prunes the removed agents’ network edges by default
  (`prune_edges = TRUE`), because leaving them would let
  `abm_match(pair = "network")` draw partners that no longer exist.
- `abm_match(pair = "nearest")` is defined for pairs only.
  Nearest-neighbour groups of three or more overlap, so they cannot
  serve as the grouping for
  [`abm_rules()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_rules.md).

### Steps added by the second round of models (Part 3)

Ten more models — Giant Component, Small Worlds, Fireflies, network SIR,
genetic drift, Hawks and Doves, Divide the Cake, Sex Ratio Equilibrium,
Axelrod’s cultural dissemination and PD N-Person Iterated — were ported
as a stress test. Five of them needed something the grammar did not
have:

- [`abm_link()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_link.md)
  and
  [`abm_unlink()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_unlink.md)
  turn a standing pairing into edges, or remove them. Until now a
  network could only grow through `abm_birth(attach_via =)`, so a graph
  process with a fixed population was inexpressible (Giant Component,
  Small Worlds).
- `abm_neighbours(col ~ aggregate)` summarises an agent’s whole network
  neighbourhood into a column. A match gives one partner; plenty of
  models need “how many of my neighbours are infected” (Fireflies, SIR).
- `abm_match(pair = "one_of")` draws each agent a partner from the whole
  population — NetLogo’s `one-of other turtles`. It is directional,
  unlike `"random"`, which partitions the eligible agents among
  themselves (Small Worlds).
- `abm_rules(..., .scope = "population")` evaluates a rule across every
  agent, ignoring the standing match. Without it, a fitness-proportional
  resampling step inherited the pair grouping and resampled within each
  pair (Hawks and Doves).
- `abm_birth(inherit =)` applies expressions to the newborn only — a
  reset age, a mutated trait, a sex drawn at birth. Evaluated in the
  parent’s row with the match standing, so `partner_*` is available and
  two-parent inheritance is a one-liner (Sex Ratio Equilibrium).

Three consistency fixes came with them:

- `abm_birth(when =)`, `abm_death(when =)` and `cost` see `partner_*`
  and `.role` when a match is standing, as
  [`abm_rules()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_rules.md)
  always has (Divide the Cake).
- A match that runs and pairs nobody now supplies `partner_*` columns
  full of `NA` instead of omitting them, so a tick where nobody meets
  anyone does not crash rules written for the ticks where they do.
- `pair = "opposite_group"` treats a population that is briefly all one
  kind as a step where nobody pairs up. Three or more distinct values is
  still an error.

### Steps added by the third round of models (Part 4)

Ten more — Granovetter’s threshold crowd, Deffuant and Hegselmann–Krause
bounded confidence, Axelrod’s norms and metanorms, Simple Birth Rates,
Team Assembly, Language Change, epiDEM Basic, the Simple Genetic
Algorithm and the Bikhchandani–Hirshleifer–Welch information cascade —
were ported as a second stress test. Three of them needed something new:

- [`abm_neighbours()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_neighbours.md)
  rules now also see the focal agent’s own columns, prefixed `own_`. A
  neighbourhood aggregate could previously only read the neighbours, so
  a *comparison* — `sum(wealth > own_wealth)`, “how many of my
  neighbours are richer than me” — was inexpressible. Axelrod’s norms
  game forced it: “how many of the others punished me” weighs my own
  chance of being seen against each neighbour’s vengefulness.
- `abm_network(type = "complete")` joins every pair of agents. That is
  the well-mixed population written as a graph, and it is what lets
  [`abm_neighbours()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_neighbours.md)
  mean “over everybody else” in a model with no structure at all
  (Axelrod).
- [`abm_link()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_link.md)
  after a match with `size > 2` links the group as a clique rather than
  erroring. A team, a committee or a coalition is a complete subgraph
  (Team Assembly).

Four idioms came out of the round that needed no code change, and are
written up in `MODELS.md` because none of them is obvious: drawing a
resampling index once and indexing every trait by it (a multi-trait
genome otherwise gets shuffled apart); a `parent` flag that makes
[`abm_birth()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_birth.md)
repeatable for fertility above one; building steps and rules
programmatically with [`rep()`](https://rdrr.io/r/base/rep.html),
[`do.call()`](https://rdrr.io/r/base/do.call.html) and
[`rlang::new_formula()`](https://rlang.r-lib.org/reference/new_formula.html);
and feeding
[`abm_edges()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_edges.md)
of one run into `abm_network(type = "manual")` of the next.

`models/` documents all forty-six models with their code, results and
sources, and `models/open-items.md` lists what is still out of reach —
notably a neighbourhood in attribute space and pairwise-consistent
randomness.

### Steps added by the fourth round of models (Part 5)

Ten more models — Virus on a Network, Watts’s global cascades, the
Sznajd model, the Naming Game, the Minority Game, Kirman’s ants, Gode &
Sunder’s zero-intelligence traders, the ultimatum game with reputation,
Hotelling’s Law and the Beer Distribution Game — were ported as a third
stress test. This round was chosen against the *Open items* list rather
than against the grammar: each model was picked because something on
that list said it could not be written.

Four of them needed something new:

- `abm_tell(col ~ expr, to =, when =, .resolve =)` writes into **another
  agent’s** row. The right-hand side is evaluated in the sender’s row
  and the value lands in the recipient’s column of that name.
  `to = "neighbours"` broadcasts along the network; `to = <expression>`
  names one recipient by `.id`, so `to = .partner` writes to a match
  partner and `to = best_bid_id` writes to whoever a global names.
  `.resolve` combines several senders addressing one recipient
  (`"last"`, `"first"`, `"sum"`, `"mean"`, `"max"`, `"min"`, or
  `"error"`). This closes three separate open items at once — no agent
  could write to another agent,
  [`abm_neighbours()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_neighbours.md)
  could read but not write, and the order book was the model the corpus
  could not reach (Sznajd, Gode & Sunder, the beer game).
- `abm_match(..., among =)` names the agents that may be *picked*, for
  the directional modes `"one_of"` and `"nearest"`. `eligible` says who
  takes part; `among` says who may be chosen, and the two only come
  apart when choosing is one-way. Without it, `pair = "nearest"` was
  unusable in a model with two agent groups, because a buyer’s nearest
  agent is another buyer (Hotelling).
- [`abm_network()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_network.md)
  gained `type = "poisson"` (Erdős–Rényi, mean degree `degree`),
  `type = "scale_free"` (Barabási–Albert) and `type = "ring"` (the
  one-dimensional lattice). `"random"` remains the `degree`-regular
  graph. The degree *distribution* turns out to be part of the model
  rather than part of the setup: on a regular graph, Watts’s cascade
  never starts (cascades, Sznajd).
- Rules inside
  [`abm_sequential()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_sequential.md)
  now **cascade** — the second rule sees what the first one wrote, to
  the agent’s own row and to the globals alike. This is what “one agent
  at a time” always implied, and it is what lets an agent draw a quote
  and then decide whether the quote crosses the book, in one step.
  [`abm_rules()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_rules.md)
  is unchanged and still simultaneous, so the difference between the two
  steps is now exactly the difference between the two update semantics
  (Gode & Sunder).

Two fixes came with them:

- [`n()`](https://dplyr.tidyverse.org/reference/context.html) works
  inside
  [`abm_global()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_global.md).
  Globals were evaluated outside dplyr’s data mask while rules were
  evaluated inside it, so `sum(x) < n() / 2` worked in a rule and failed
  in a global (Minority Game).
- A rule inside
  [`abm_sequential()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_sequential.md)
  whose target is a global is now routed by the columns on its
  right-hand side, like any other rule. It used to be applied to every
  agent regardless, so a rule written for one group was evaluated in
  another group’s row (Gode & Sunder).

Two entries came off *Open items* with no code change at all:

- **List columns already worked.** “No set-valued agent state” had been
  the first open item for two rounds and was simply wrong:
  [`abm_agents()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_agents.md)
  takes a list column, an
  [`abm_rules()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_rules.md)
  right-hand side may return one, `partner_<col>` carries one, and
  snapshots record one. The Naming Game holds a growing inventory of
  names per agent and the Minority Game a 2^m × S strategy table, both
  written directly. What was missing was the idiom, not the capability.
- **A push is often a pull.** NetLogo’s contagion — every infected node
  rolling a die at every neighbour — is exactly `1 - (1 - p)^k` for a
  susceptible node with `k` infected neighbours, so
  [`abm_neighbours()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_neighbours.md)
  plus one rule says it without any outward write.
  [`abm_tell()`](https://rayhanalirachman.github.io/tidyabm/reference/abm_tell.md)
  is for the cases where what is transmitted depends on *which* agent
  sent it.

The models reference is now a folder, `models/`, with one page per
model, an index, the grammar, the open items, the history of what each
stress test changed, the full source table, and the reproduction
scripts.

### Notes

`R CMD check` takes around nine minutes, almost all of it in
[`vignette("corrections")`](https://rayhanalirachman.github.io/tidyabm/articles/corrections.md),
which runs three full experiments. If that becomes awkward in CI,
precompute that vignette rather than shrinking the models — the
ethnocentrism result is drift-dominated below a few hundred agents and
stops being reliable.

### Known model issues, not package issues

Three of the reference models run correctly and do not reproduce the
behaviour they are known for, because the short form of each leaves out
the mechanism that produces it: El Farol needs agents that switch
between candidate predictors, ethnocentrism needs tag-conditional
strategies *and* local reproduction, and the zakah model needs a risk
process or nobody is ever poor.
[`vignette("corrections")`](https://rayhanalirachman.github.io/tidyabm/articles/corrections.md)
diagnoses and fixes all three.
