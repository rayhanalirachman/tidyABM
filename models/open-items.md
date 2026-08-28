# Open items

What the grammar still cannot say, and what came off the list when the seventh
round went at the list itself rather than at a new set of models.

## Closed by Part 7

Part 7 is the only round with no models in it. Every entry below had been sitting
under *Still open* since the round that found it, each with a model naming the
shape it wanted, and the seventh round was spent closing them rather than
collecting more. Seven of the nine went. The two that remain were never requests
for expressiveness in the first place.

- **~~No neighbourhood in attribute space.~~** `abm_neighbours(..., within =)`
  aggregates over everybody whose columns satisfy a condition instead of over the
  network. The condition is evaluated once per (focal, candidate) pair with the
  candidate's columns under their own names and the focal agent's under
  `own_<col>`, the same view `abm_match(cost =)` minimises over, which is why
  this was described as the other half of what `cost =` closed and why it turned
  out to be a small job. Hegselmann–Krause (29) is now
  `abm_neighbours(opinion ~ mean(opinion), within = abs(opinion - own_opinion) <= eps)`
  and produces the same run, agent for agent, as the hand-rolled O(n²)
  `vapply()` it replaces. One thing had to be decided rather than inherited: an
  agent **is** inside its own attribute neighbourhood, because "the mean opinion
  of everyone I take seriously" includes the agent's own, where it is never
  inside its own network neighbourhood. `within = ... & .id != own_.id` excludes
  it.
- **~~No global indexed by a category.~~** `abm_global(..., .by =)` makes the
  global a named vector and evaluates the rule once per key. `.key` is the key
  being written and the global's own name is bound to *that key's* current value,
  so the update reads exactly as the scalar version does. An ordinary rule reads
  the table back with `price[good]`. Each key still sees the whole population,
  because a colony-level stimulus balance is about the colony and not about the
  workers on one task. `.by` takes either a vector of keys, the index declared by
  the model, which is what the division of labour (47) wants, since a task nobody
  is working on still has to have its stimulus rise. Or an agent column, whose
  distinct values are the index, for categories that come and go. Model 47 loses
  its `rlang::new_formula()` scaffolding and reads as a specification again.
- **~~No pairwise-consistent randomness.~~** New step: `abm_draw()`. It attaches
  a value to every **edge**, and every later `abm_neighbours()` rule in the tick
  reads it under that name from either endpoint. Because both ends read the same
  number, two aggregates over it describe the same events rather than two
  independent draws with the same distribution. `.each = "edge"` is one value the
  pair shares: did we meet, what was it worth. `.each = "endpoint"` is one value
  each, read as `name` from the agent's own side and `name_back` from the other,
  which is what an asymmetric interaction needs: whether I noticed you and
  whether you noticed me are two coins. Axelrod's metanorms (30) is the model,
  and the test is an identity rather than a distribution. The punishments
  handed out and the punishments received now sum to the same number, agent by
  agent.
- **~~`abm_tell()` does not order its messages.~~** `.order =` is an expression
  evaluated in the sender's row whose ascending order the messages are considered
  in. It closes more than the entry asked for: `"first"` and `"last"` were
  arbitrary for the same reason `"collect"`'s list was, so all three become
  determinate at once. *The first person to reach the counter* is
  `.resolve = "first", .order = arrived_at`, and the recipient no longer has to
  reconstruct the queue from something the senders wrote down. `NA` sits a sender
  out, as it does in `abm_sequential(.order =)`.
- **~~`abm_birth()` gives one offspring per parent.~~** `times =` is an
  expression evaluated in the parent's row: a column, a draw
  (`rpois(n(), 2)`), a number. The flag column and the repeated step are gone.
  The part worth stating is what happens to `inherit`: each offspring is a row of
  its own before `inherit` is evaluated, so a mutation drawn there differs from
  sibling to sibling rather than being drawn once and copied. `cost` is still
  evaluated once, in the parent's row, so reproduction is paid for per parent, not
  per child, and both meanings were reachable but only one of them is what
  "cost" says.
- **~~`abm_run()` records everything.~~** `record =` says how much to keep:
  `"all"` (the default, and the old behaviour), a whole number for every *n*th
  tick plus the two ends, `"final"` for the last tick only, `"globals"` for none
  of the populations at all. Globals are recorded every tick whatever it says,
  since they are one row each, which is what makes `"globals"` useful rather
  than merely small. Nothing about the run changes. The same seed gives the same
  final state at any setting. Predator–prey (56) at the parameterisation that
  took the wolves to 47,000 now runs to the end.
- **~~`abm_sequential()` is slow.~~** It was a `dplyr::mutate()` on a one-row
  tibble per agent per rule, a few hundred microseconds of machinery for what is
  usually one arithmetic operation on one number, plus a whole-column write per
  assignment, which made the step quadratic in the population. It now evaluates
  its rules against a plain data mask built from the agent's scalars and holds
  the group's columns as bare vectors for the duration of the loop. The bank run
  (52) at 200 depositors × 50 days went from **77.6 s to 1.75 s**, and the result
  is bit-identical: run rate 0.507, the number in the model's own table. The
  order book (43), whose sequential step is twenty-six rules, is identical too.
  Nothing in the semantics moved. The cascade within an agent, the visibility of
  globals to the next agent, and the deliberate invisibility of other agents'
  columns are all as they were.

One fix came off with them. A value looked up out of a keyed global, as in
`price[good]`, arrives carrying the global's names, and an agent column is not a
lookup table, so the names are dropped on assignment rather than travelling
through the run and into the result.

## Still open

One entry, and it is not a request for expressiveness.

- **No spatial primitive.** Every model here is aspatial or network-based by
  design. A lattice or continuous space would be a different project, not an
  extension of this one. `pair = "nearest"` covers similarity-space models, and
  now that it takes a cost expression, and `abm_neighbours()` takes a `within`
  condition, it covers rather more of them than it did. That is as close as this
  grammar gets, and going further would mean deciding what a neighbourhood, a
  boundary and a move are, which is a design project of its own.
