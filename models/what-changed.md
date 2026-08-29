# What the first stress test changed

Ten models, five additions. Each addition was forced by at least one model that
could not otherwise be written, and none of them is a special case for one model.

| addition | forced by | what it does |
|---|---|---|
| `abm_link()` / `abm_unlink()` | Giant Component, Small Worlds | network mutation with a fixed population |
| `abm_neighbours(col ~ agg)` | Fireflies, SIR | aggregate over the whole neighbourhood, not one partner |
| `pair = "one_of"` | Small Worlds | NetLogo's `one-of other turtles`: a directional draw from the whole population |
| `.scope = "population"` | Hawks and Doves | a rule that ignores the standing match |
| `abm_birth(inherit =)` | Sex Ratio Equilibrium | expressions for the newborn only, including `partner_*` |

Three consistency fixes came with them:

- `abm_birth(when =)` / `abm_death(when =)` and `cost` see `partner_*` and `.role`
  when a match is standing, as rules always have (Divide the Cake).
- A match that runs and pairs nobody supplies `partner_*` columns full of `NA`
  rather than omitting them, so a tick where nobody meets anyone does not crash
  rules written for the ticks where they do.
- `pair = "opposite_group"` treats a population that is briefly all one kind (every
  male dead, say) as a step where nobody pairs up. Three or more values is still
  an error.

---

# What the second stress test changed

Ten models, three additions.

| addition | forced by | what it does |
|---|---|---|
| `own_<col>` in `abm_neighbours()` | Axelrod's norms game | a neighbourhood aggregate can compare each neighbour to the focal agent |
| `abm_network(type = "complete")` | Axelrod's norms game | the well-mixed population, written as a graph, so a neighbourhood means "everybody else" |
| clique linking in `abm_link()` | Team Assembly | a matched group of three or more gains an edge between every pair inside it |

Four idioms came out of it that needed no code at all, and are worth writing down
because none of them is obvious:

- **Draw the index once, then index every trait by it.** A fixed-size generational
  resample of a genome with more than one trait cannot be written as two
  independent `sample()` calls, or the traits get shuffled apart. Instead:
  `abm_rules(pick ~ sample(n(), n(), replace = TRUE, prob = fitness))` and then
  `abm_rules(a ~ a[pick], b ~ b[pick])`. Used by both Axelrod and the genetic
  algorithm. Writing Axelrod's replication rule the obvious way, with `abm_birth()`
  above one SD and `abm_death()` below, collapsed the population from 20 to 2 over
  100 generations.
- **A `parent` flag makes `abm_birth()` repeatable.** Set it before the births,
  clear it on the newborns with `inherit =`, and `k` copies of the same birth step
  give each parent up to `k` offspring without any of them becoming grandparents
  mid-tick.
- **Steps are ordinary objects.** `rep(step_list, 4)` gives four rounds,
  `do.call(abm_go, steps)` assembles a block whose length depends on a parameter,
  and `rlang::new_formula()` builds a rule whose target is computed. Axelrod's
  four opportunities, the genetic algorithm's `L` bits and Team Assembly's
  member-by-member recruitment are all the same trick.
- **Models compose through the network.** `abm_edges()` of one run is a valid
  `edges =` for the next `abm_network(type = "manual")`, so Language Change runs
  on a graph the preferential-attachment model grew.

# What the third stress test changed

Ten models, four additions, and two entries that came off the *Open items* list
without any code changing at all.

| addition | forced by | what it does |
|---|---|---|
| `abm_tell()` | Sznajd (39), the double auction (43), the beer game (46) | an agent writes into another agent's row: its match partner, an agent a global names, or every one of its neighbours |
| `among =` on `one_of` and `nearest` | Hotelling (45) | separates who takes part in a match from who may be picked, which is what "the nearest *shop*" needs |
| `poisson`, `scale_free` and `ring` networks | Watts's cascades (38), Sznajd (39) | a degree *distribution* rather than a single degree |
| cascading rules in `abm_sequential()` | the double auction (43) | a rule sees what the rule above it wrote, so an agent can draw a quote and then act on it |

Plus one small fix: `n()` works inside `abm_global()`, which it did not, because
globals were evaluated outside dplyr's data mask while rules were evaluated
inside it. The minority game (41) is where that showed up.

`abm_tell()` is the one that matters. It closes three separate entries that had
been open since Part 3, namely "no agent can write to another agent", "`abm_neighbours()`
reads, it does not write", and the note that the order book was the model this
corpus could not reach, and it does it with one step and three arguments,
because those three entries were the same missing idea seen from three sides.
What was missing was not a mechanism for markets, or a mechanism for gossip, but
the *direction* of a write.

Two findings needed no code:

- **List columns already worked.** "No set-valued agent state" had been the
  first entry under *Open items* for two rounds, and it was wrong.
  `abm_agents()` takes a list column, `abm_rules()` may return one,
  `partner_<col>` carries one, and snapshots record one. The naming game (40)
  and the minority game (41) hold an inventory and a strategy table
  respectively, written directly. What the entry should have said is that the
  *idiom* is unobvious: rules over list columns are `Map()` and `lapply()` where
  you would otherwise write arithmetic, and nothing in the documentation
  suggested trying it.
- **A push is often a pull.** NetLogo's contagion, with every infected node
  rolling a die at every neighbour, is exactly `1 - (1 - p)^k` for a susceptible node
  with `k` infected neighbours, so `abm_neighbours()` plus one rule says it
  without any outward write at all. Model 37 is written that way. The models
  that genuinely need `abm_tell()` are the ones where what is transmitted
  depends on the sender (Sznajd's coincidence, the beer game's shipment), not
  merely on how many senders there were.

And two things that are worth recording as *modelling* traps rather than grammar
ones, both from Hotelling (45): a hill-climbing agent must compare against what
it is getting now rather than the best it ever got, or it freezes the moment a
rival moves in next door. And it must require a strict improvement, or a pair of
identical agents will random-walk together forever, since neutral moves look
acceptable to both. Neither is a limitation of the package. Both produce a
plausible wrong answer rather than an error.

# What the fourth stress test changed

Ten models, six additions, and two bugs that had been in the package since the
beginning.

| addition | forced by | what it does |
|---|---|---|
| `abm_repeat(..., until =, max =)` | vaccination (51), deferred acceptance (55) | replays a block of steps inside a tick until a condition holds |
| `abm_match(cost =)` | the garbage can (48), deferred acceptance (55) | `nearest` minimises a number you write rather than a distance |
| `abm_rules(.by =)` | the emergence of firms (49) | rules grouped by an ordinary agent column, so a firm can be added up |
| `abm_sequential(.order =)` | bank runs (52) | agents processed in a named order instead of a fresh shuffle |
| `abm_tell(to = <a set>)` | image scoring (54), the garbage can (48) | one sender, many chosen recipients |
| `abm_tell(.resolve = "collect")` | image scoring (54), the garbage can (48) | the recipient gets the list and decides for itself |

`abm_repeat()` is the one that changes what a model can be. Until now a tick was
the only loop, so a *phase* inside a tick, say an epidemic that must burn out before
anyone reconsiders vaccinating, a round of proposals that continues until nobody
is rejected, had to be written as a fixed number of repetitions with `rep()`.
That is either wrong, if the bound is too small, or wasteful, if it is safe. It
also closes the *no early stopping* entry from the other direction: a block
wrapped in `abm_repeat()` and run for one tick stops when the model reaches its
absorbing state, which is what Sznajd, the naming game and Watts's cascades all
wanted.

`cost =` and `.by =` are both answers to the same complaint, which is that the
grammar had exactly three ways of relating agents, the match, the network and
the whole population, and models keep wanting a fourth. `cost =` lets a
directional match be decided by anything the chooser can compute about a
candidate, which turns "the nearest shop" into "the accessible choice with the
smallest energy deficit" and "the best woman who has not yet rejected me".
`.by =` lets a rule be grouped by an ordinary column, which turns a firm, a
household or a team into something a model can add up, and because the column
is ordinary, the agents themselves decide which group they are in. That is the
only mutable grouping in the grammar.

The two `abm_tell()` additions turned out to be one idea seen twice, in the same
way `abm_tell()` itself was in Part 5. A relation that is many-to-one, like
problems attached to a choice or onlookers watching a donor, could be *counted* by
`.resolve = "sum"` and could not be *held*. `to = <a list column>` addresses a
set. `.resolve = "collect"` gathers one. Together they let an agent learn who
wrote to it and answer them.

Two bugs, both found by models rather than by tests:

- **A group of one is a group of one.** `sample(x)` reinterprets a length-1
  numeric `x` as `seq_len(x)`, which for a vector of agent ids means a pool
  holding a single agent silently became a pool of `.id` agents. Predator–prey
  (56) is the model that trips it, because the two populations cross constantly:
  `pair = "opposite_group"` with four wolves and one sheep paired the sheep with
  all four wolves, so every wolf ate every tick and the wolf population grew
  without bound. It had been there since the first release, in the matching
  pool, the with-replacement draw for `one_of`, and `abm_sequential()`'s
  shuffle. Every one of them now goes through a helper that does not have the
  behaviour.
- **A global need not be a scalar.** `abm_globals()` builds one row per tick and
  assumed every global was a single value. A matrix-valued global, whether an NK
  landscape (50), a payoff table or a vector of prices, produced one row per
  matrix row and a tick column that repeated, quietly. Non-scalar globals are
  now stored in a list column.

And three findings that needed no code:

- **A column written inside a match belongs to the encounter, not the agent.**
  `abm_rules()` after a match evaluates only for agents the match placed in a
  group, so an unmatched agent keeps whatever the column held last tick. In
  predator–prey a wolf that ate once and then went unpaired kept `caught = TRUE`
  and fed forever. The fix is the Hotelling idiom, resetting the column with a
  population-scope rule before the match, and it is worth stating as a rule
  rather than as a bug, because the alternative behaviour (clearing every column
  the rules mention) would break the models that rely on carrying state across a
  tick where nobody met anyone.
- **The pairing mode is the functional response.** In predator–prey,
  `pair = "opposite_group"` makes `min(S, W)` encounters, which is
  ratio-dependent predation. Filtering the hunters by prey density first makes
  `S·W/area` of them, which is mass action, and only the second gives Lotka and
  Volterra's cycles. The same is true wherever "who meets whom" is a rate rather
  than a rule, and it is an argument for the modes being named in the model
  rather than buried in a constant.
- **Novelty is a counter and a `cumsum()`.** A variant nobody has held before is
  a fresh identifier, and because `abm_rules()` is simultaneous the idiom is
  `variant ~ if_else(innovate, coined + cumsum(innovate), copied)` followed by
  `abm_global(coined ~ coined + sum(innovate))`. The neutral model (53) is
  written that way and needed nothing else.

Four of the ten needed no change at all: the division of labour (47), the NK
landscape (50), the neutral model (53) and predator–prey (56). Which is the
first round where the models that fit outnumbered the ones that did not by less
than in Part 4, and is what you would expect from a round chosen to leave the
corpus's home ground.

# What the maintenance round changed

Part 7 has no models in it. Every previous round was a set of ten papers chosen
to ask the grammar for something it might not have, and every one of them left
entries behind on *Open items*, nine of them by the end of Part 6, each with a model
naming the shape it wanted and none of them large enough to be worth stopping a
stress test for. This round went at that list instead, and closed seven of the
nine.

That is a different kind of evidence from the rounds before it. A stress test
tells you what is missing. It does not tell you whether the missing things are
*coherent*, whether the shapes the models asked for fit the grammar or merely
sit next to it. Five of the seven turned out to be shapes the package already
had somewhere else:

- **`abm_neighbours(within =)`** is `abm_match(cost =)`'s view with an aggregate
  on the end instead of an argmin. Both build one row per (focal, candidate) pair
  with `own_<col>` for the focal side. `cost` minimises over it and `within`
  filters it. Part 6 said this should be a small job, and it was, since the machinery
  was already written and tested.
- **`abm_global(.by =)`** is `abm_rules(.by =)`'s question asked on the other
  side of the population. The agent-side answer partitions the agents. The
  global-side one indexes a shared table. `.key` is the only thing that had to be
  invented, and only because a global has no rows to carry the key in.
- **`abm_tell(.order =)`** is `abm_sequential(.order =)`, down to `NA` sitting
  the agent out. Both are the same complaint, an arbitrary order where the model
  has a real one, arriving at two different steps two rounds apart.
- **`abm_birth(times =)`** is a count where there was a `1`.
- **`abm_run(record =)`** is not a grammar change at all. It is the scheduler
  admitting that "record everything" is a default rather than a law.

The two that were genuinely new are worth more than the five.

**`abm_draw()`** is the first step that writes to an **edge** rather than to an
agent. Everything else in the package computes something and puts it in a row.
`abm_rules()` in your row, `abm_tell()` in someone else's, `abm_global()` in the
shared value. The metanorms model (30) wanted something none of those can hold: a
number that belongs to a *relationship*, for the length of a step, readable
identically from both ends. Without it, "how many of my neighbours punished me"
and "how much punishing did I do" are two independent draws with the same
distribution, so Axelrod's enforcement costs balance on average and not agent by
agent. With it they are the same events, and the test changes from a comparison
of means to an identity. `.each = "endpoint"` came out of writing the model: the
symmetric draw covers *did we meet*, and asymmetric interactions need one coin
each, since whether I noticed you and whether you noticed me are different questions
about the same edge.

**`abm_sequential()`'s rewrite** is the one that says something about the design
rather than about a model. The step was a `dplyr::mutate()` on a one-row tibble,
per agent, per rule: a few hundred microseconds of grouping machinery for what is
usually one arithmetic operation on one number, plus a whole-column write per
assignment, which made the step quadratic in the population. Nothing in the
semantics needed any of that. An agent's row during that loop is a handful of
scalars, so the rules now evaluate against a plain data mask built from them and
the group's columns are held as bare vectors until the loop ends. The bank run
(52) went from 77.6 s to 1.75 s at 200 depositors × 50 days, bit-identical, and
the order book (43) is identical too. The lesson is narrow and worth writing
down: the tidy-data interface is what the *model* is written in, and it does not
have to be what the engine runs on.

Two entries stayed open, and both on purpose. A spatial primitive is a different
project. It means deciding what a neighbourhood, a boundary and a move are, and
`pair = "nearest"` with a cost expression plus `abm_neighbours(within =)` already
covers the similarity-space models that motivated it. And
`resolve = "negotiate"` stays narrow because bargaining protocols do not form a
family with a shared shape the way the pairing modes do: an argument general
enough to take a protocol would be an argument that takes a function, and at that
point the model may as well write the steps, which is what model 43 does.


---

# What re-checking the corrections changed

No new models. `vignette("corrections")` was read back against the corpus, on
the theory that a page about mechanisms compressed out of a description is the
page most likely to have compressed something itself.

| addition | forced by | what it does |
|---|---|---|
| `abm_birth(links =)` | Ethnocentrism (15) | how many edges a newborn gets, and with `from = "parent"` it takes the parent's neighbours rather than only the parent |

Two claims came off, and they are the more useful half.

**El Farol did not need anything.** It had been written as seventy scalar
columns and five blocks of `rlang::new_formula()` scaffolding, and cited three
times over as the model proving an agent could not hold a set. A list column
holds one. The rewrite is twelve lines, and it is a better demonstration than
the original: fixing `abm_setup(seed =)` as well as `abm_run(seed =)` made the
result reproducible, which it had not been, and a sweep over the size of the
predictor pool shows the fluctuation appearing as the pool grows rather than
being asserted from one run.

**The ethnocentrism result was resting on a broken network.** The model was
correct, the mechanism was real, and the graph it ran on had eroded to a mean
degree of 0.97 with a quarter of the population isolated, because deaths prune
four edges and each birth added one. Nothing in the run said so. The number the
write-up quoted, 0.955 of edges joining same-tag agents, was the artefact: on a
maintained network the figure is 0.85 and the strategy shares are the same. The
lesson is the one the vignette is about, applied to the vignette: a model that
runs, reports a plausible number, and is measuring something other than what the
sentence next to it claims.
