# The grammar

A tidyABM model is three parts, written as three statements:

```r
world  <- abm_setup(...)                                # 1. who is in the model
go     <- abm_go(...)                                   # 2. what happens each tick
result <- abm_run(world, go, ticks = ..., seed = ...)   # 3. run it
```

`abm_setup()` declares the world: who the agents are, what network joins them,
what values everyone shares. `abm_go()` declares the tick, an ordered sequence
of typed steps. `abm_run()` takes both and produces a tidy tibble. The first two
follow NetLogo's `setup` / `go`. What is different is that they are data rather
than loops, so a model reads as a specification.

Everything else in this reference sits inside one of the three. `abm_agents()`
and `abm_network()` are specifications you hand to `abm_setup()`. Every other
`abm_*()` function is a step inside `abm_go()`.

Every model here is written that way, and none of them builds `abm_go()` inside
the call to `abm_run()`. The setup and the go block are objects: they are
validated once when you build them, they print, and either can be reused while
the other varies.

## 1. `abm_setup()`: the world

- `abm_setup(agents =, network =, globals =, seed =)` builds the model itself.
  `agents` is one `abm_agents()` or a named list of them. `globals` is a plain
  list, and `seed` fixes the draws that build the population.
- `abm_agents(n, col = value_or_formula, ...)` declares one group. A plain value
  recycles. A one-sided formula is evaluated once and sees `n` and the columns
  before it. A column may be a **list column**, which is how an agent holds a
  set, a vector or a matrix rather than a number.
- `abm_network(type =, degree =, edges =)` builds a persistent edge list.

| `type` | what it builds |
|---|---|
| `"random"` | a `degree`-regular random graph, so every agent has *exactly* `degree` neighbours |
| `"poisson"` | Erdős–Rényi `G(n, p)` with mean degree `degree`, giving a spread of degrees |
| `"scale_free"` | Barabási–Albert, `degree` edges per new agent, giving a heavy tail |
| `"ring"` | the one-dimensional lattice, `degree / 2` neighbours each side |
| `"complete"` | every pair, which is the well-mixed population written as a graph |
| `"manual"` | edges you supply |
| `"empty"` | no edges, for a network that grows during the run |

The degree *distribution* is part of the model, not part of the setup. A
threshold model on a regular graph behaves quite differently from the same model
on a graph with the same mean degree and a spread of degrees, because the
low-degree agents are the ones a cascade can start on and a regular graph has
none. Model 38 is that difference.

Globals are not required to be scalars. A lookup table, a payoff matrix or a
vector of prices is a perfectly good global. `abm_globals()` keeps it in a list
column so that the log stays one row per tick.

## 2. `abm_go()`: the tick

`abm_go(...)` takes steps in order, dispatched by type and position. Validated
once at construction: not empty, no two `abm_match()` adjacent, not ending on a
bare `abm_match()`.

| step | what it does |
|---|---|
| `abm_match(pair =, size =, by =, cost =, role =, eligible =, among =, ...)` | decides who meets whom |
| `abm_rules(col ~ expr, ..., .scope =, .by =)` | updates agent columns, simultaneously |
| `abm_sequential(col ~ expr, ..., .order =)` | updates them one agent at a time, cascading |
| `abm_neighbours(col ~ agg, within =)` | summarises each agent's neighbourhood, in the network or in attribute space |
| `abm_draw(name ~ expr, .each =)` | attaches a value to every edge, readable from both ends |
| `abm_tell(col ~ expr, to =, when =, .resolve =, .order =)` | writes into *another* agent's row |
| `abm_global(name ~ agg, .by =)` | updates a population-level value, or a table of them |
| `abm_birth(when =, n =, times =, cost =, inherit =, attach_via =)` | adds agents |
| `abm_death(when =, prune_edges =)` | removes them |
| `abm_link(when =)` / `abm_unlink(when =)` | adds / removes network edges |
| `abm_repeat(..., until =, max =)` | replays a block of steps inside the tick |

### Matching modes

Two are *mutual*. They partition the eligible agents, so being matched is
symmetric:

- `"random"` shuffles into groups of `size` (default 2)
- `"opposite_group"` pairs across a two-valued split named by `by`, and supports
  `resolve = "negotiate"` with `rounds`, `positions` and `limits`

Three are *directional*. Each agent gets a partner of its own, and your partner
need not have picked you:

- `"one_of"` draws a partner uniformly from the candidates (NetLogo's
  `one-of other turtles`)
- `"nearest"` takes the closest candidate in the space named by `by`, or the one
  that minimises the expression named by `cost`
- `"network"` takes a neighbour from the model's network. `from = "random_edge"`
  and `from = "parent"` work inside `abm_birth(attach_via =)`

Each mode uses a fixed set of the other arguments and errors on one it does not
use, rather than ignoring it.

Which one you want is not a matter of taste. A mutual mode guarantees exclusion,
so two wolves cannot eat the same sheep. A directional one gives no such
guarantee. Where matching stands in for a rate rather than a rule, the mode *is*
the model: `"opposite_group"` makes `min(A, B)` encounters and a directional
draw makes as many as there are choosers, and in a predator–prey model those are
ratio-dependent and mass-action predation respectively (model 56).

**`eligible` and `among`** ask different questions, and the difference only has
teeth in the directional modes. `eligible` says who *takes part*. `among` says
who may be *picked*. A buyer choosing the nearest shop wants
`among = .group == "shops"`, or it will find that the nearest agent to it is
another buyer.

**`by` and `cost`** are the two ways to say what "nearest" means. `by` names
coordinates and compares them by Euclidean distance. `cost` names a number the
chooser is minimising, evaluated once per (chooser, candidate) pair with the
candidate's columns under their own names and the chooser's under `own_<col>`,
`.id` and `.group` included. `NA` means the candidate is not acceptable to that
chooser, and a chooser with no acceptable candidate sits the step out. A
delivered price, an energy deficit and a position in a preference list are all
`cost`, and none of them is a distance.

### What a match gives a rule

For pairs, `partner_<col>` for every column of the partner, list columns
included. For groups, grouped evaluation, so `sum(x)` inside a rule means "across
this agent's group". Always `.role` when `role` was supplied and `.partner` (the
partner's `.id`, `NA` if unmatched). `.scope = "population"` on an `abm_rules()`
step ignores the standing match and evaluates across everybody.

Inside a `size = 2` match a rule is evaluated over a two-row group, so
`which(.role == "speaker")[[1]]` picks out one member's row and `rep(x, n())`
broadcasts a single value back to both. That is how a rule says "the pair agrees
on this" rather than "each of them decides separately".

A rule after a match is evaluated only for agents the match placed in a group,
so **an unmatched agent keeps whatever the column held before**. That is what
you want for state an agent carries across a tick where nobody met anyone, and
it is a trap for anything that describes the encounter rather than the agent:
reset such a column with a population-scope rule before the match, or it will
still be true next tick.

### A global with an index

`abm_global(name ~ agg)` writes one shared value. `.by` writes a shared *table*:
a stimulus per task, a price per good, a queue length per counter. The global
becomes a named vector, the rule is evaluated once per key, and an ordinary rule
reads it back with `price[good]`.

Two things are in scope during that evaluation and nowhere else. `.key` is the
key being written, so `sum(task == .key)` is "how many agents are on *this*
task". And the global's own name means **this key's** value, not the whole
vector, so the update reads exactly as the scalar version does. Each key still
sees the whole population, meaning `n()` is everybody, because a colony-level
balance is about the colony.

`.by = 1:2` declares the index, which is what you want when the categories are
fixed and a key with nobody in it still has to be updated. `.by = task` derives
it from an agent column, which is what you want when they come and go. Keys the
global already has stay in the index and are still evaluated.

### Three groupings

A rule can be grouped three ways, and they answer different questions:

| grouping | written | means |
|---|---|---|
| the standing match | the default | per pair, per group, or per agent |
| the whole population | `.scope = "population"` | everybody at once |
| a column | `.by = firm` | per distinct value of that column |

`.by` is the only one the agents themselves control. It partitions the whole
population by an ordinary agent column, evaluates the rules once per value, and
writes the answer back to every member. So `sum(effort)` means "my firm's total
effort", and an agent that writes a new value into `firm` has joined a different
one. Firms, households, teams and cohorts are all this shape (model 49).

### Simultaneous, sequential, and cascading

`abm_rules()` is **simultaneous**: every rule reads the state at the start of the
step, so `abm_rules(a ~ b, b ~ a)` swaps them. That is the synchronous update
agent-based models normally assume, and it is what makes a pipeline shuffle
(`ship_in1 ~ ship_in2, ship_in2 ~ 0`) come out right. It is also what makes the
novelty idiom work: `variant ~ if_else(new, counter + cumsum(new), copied)`
hands out distinct fresh identifiers because every innovator sees the same
counter.

`abm_sequential()` is the order-dependent sibling. Agents are processed one at a
time, each agent's writes to globals are visible to every agent after it, **and
rules cascade within the agent**, so the second rule sees what the first one
just wrote. That is what "one agent at a time" implies, and it is what lets an agent
draw a quote and then decide whether the quote crosses the book, in one step.

The order is a fresh shuffle unless you name one. `.order =` takes an expression
evaluated over the population and processes agents in its ascending order, which
is what a queue at a counter or a sequential-service constraint needs. `NA` sits
an agent out. The difference is not cosmetic. Run the same bank-run model under
the two orderings and you get different answers, because a fixed queue is
information an agent has about itself (model 52).

### Writing to other agents

`abm_neighbours()` reads: for every agent, an aggregate over the agents around
it, with the focal agent's own columns visible as `own_<col>` so that comparisons
like `sum(wealth > own_wealth)` are expressible.

The neighbourhood is the network by default. `within =` makes it a neighbourhood
in **attribute space** instead: everybody whose columns satisfy a condition,
network or no network, evaluated once per (focal, candidate) pair over the same
view `abm_match(cost =)` minimises over. `mean(opinion)` over
`within = abs(opinion - own_opinion) <= eps` is Hegselmann–Krause's confidence
set, written as a step rather than as a `vapply()` over the population. The two
differ in one respect beyond how they find the neighbours: an agent is inside its
own *attribute* neighbourhood whenever the condition holds of it, and never
inside its own *network* one. Write `within = ... & .id != own_.id` to exclude
it.

Two `abm_neighbours()` passes over the same network draw independently, which is
right when each is a separate event and wrong when it is one event seen from two
sides. `abm_draw(name ~ expr)` puts the draw on the **edge**: it is evaluated
once per edge, with `n()` being the number of edges, and every later
`abm_neighbours()` rule in the tick reads it under that name from either
endpoint. Because both ends read the same number, two aggregates over it describe
the same events, and "how many punished me" and "how much punishing did I do"
sum to the same total rather than agreeing in distribution. `.each = "edge"` is
one value the pair shares. `.each = "endpoint"` is one value each, read as `name`
from your own side and `name_back` from the other, which is what an asymmetric
interaction needs.

`abm_tell()` writes, and it is the only step that touches a row other than the
one it is evaluated on. The right-hand side is evaluated in the **sender's** row,
and the value lands in the **recipient's** column of that name. `to =
"neighbours"` broadcasts to everyone the sender is joined to. `to = <expression>`
names one recipient by `.id`, so `to = .partner` writes to your match partner and
`to = best_bid_holder` writes to whoever a global names. And if the expression
returns a **list column**, each element is a set of `.id`s and the sender writes
to all of them, which is how you address an audience that is neither its partner
nor its network.

`.resolve` says what happens when two senders address the same recipient:
`"last"`, `"first"`, `"sum"`, `"mean"`, `"max"`, `"min"`, `"collect"`, or
`"error"` to stop. `"collect"` hands the recipient the list of everything it was
told and lets its own rule decide. An agent nobody wrote to keeps what it had.

Three of those pick out a message rather than combining them all, so they only
mean something once the messages have an order. `.order =` gives them one: an
expression evaluated in the *sender's* row whose ascending order the messages are
considered in. `.resolve = "first", .order = arrived_at` is the first person to
reach the counter, and `"collect"` hands over a list already in that order. `NA`
sits a sender out.

### Repeating a block

`abm_repeat(..., until =, max =)` holds a block of steps and replays it until
`until` is true or `max` times, whichever comes first. `until` is evaluated the
way an `abm_global()` right-hand side is, over the whole population and with the
globals in scope, and must collapse to one logical value. It is checked *after*
each pass, so the block always runs at least once. `max` is required.

Use it for a phase that has to finish before the next one starts: an epidemic
that burns out before anyone reconsiders vaccinating (51), a round of proposals
that continues until nobody is rejected (55). A block wrapped in `abm_repeat()`
and run for a single tick is also how a whole model stops early at its absorbing
state.

## 3. `abm_run()`: the run

`abm_run(model, go, ticks, seed, record)` returns one long tibble of `tick`,
`.id`, `.group` and then the agent columns, with globals and the final network
attached, read with `abm_globals()` and `abm_edges()`. Tick 0 is the state
`abm_setup()` produced, so globals are `NA` on that row.

`record` says how much of that to keep: `"all"` (the default), a whole number for
every *n*th tick plus the two ends, `"final"` for the last tick only, or
`"globals"` for none of the populations. Globals are recorded every tick
whatever it says. A fixed population can ignore this. A growing one can't,
because recording every agent of every tick is what makes such a run die of
memory rather than merely take a while.

`abm_setup(seed =)` fixes *who the agents are*. `abm_run(seed =)` fixes *what
happens to them*. A model with randomly drawn starting columns needs both.

**Package-managed columns** are dot-prefixed: `.id`, `.group`, `.role`,
`.partner`, `.group_id`. Everything else is yours.

**Steps are ordinary objects.** `rep(step_list, 4)` gives four rounds,
`do.call(abm_go, steps)` assembles a block whose length depends on a parameter,
and `rlang::new_formula()` builds a rule whose target is computed. Models 30, 35,
44, 45 and 47 are all the same trick.
