# Match agents into pairs or groups

`abm_match()` is the step that decides *who interacts with whom* this
tick. It does not change any agent column; it only produces a partner
(for `size = 2`) or a group (for `size > 2`), which the
[`abm_rules()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_rules.md)
steps that follow it then use.

## Usage

``` r
abm_match(
  pair = c("random", "one_of", "opposite_group", "nearest", "network"),
  size = NULL,
  by = NULL,
  role = NULL,
  eligible = NULL,
  from = NULL,
  among = NULL,
  cost = NULL,
  weight = NULL,
  .by = NULL
)
```

## Arguments

- pair:

  The pairing mode. `"random"` shuffles agents into groups of `size`;
  `"one_of"` gives each agent one partner drawn uniformly from the whole
  population; `"opposite_group"` pairs each agent with one from the
  other group named by `by`; `"nearest"` gives each agent its closest
  other agent in the space defined by `by`; `"network"` draws the
  partner from the agent's neighbours in the model's
  [`abm_network()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_network.md).

- size:

  Group size. Defaults to 2 (a pair). Only `"random"` and `"nearest"`
  support `size > 2`; `"opposite_group"` is undefined above 2.

- by:

  Column(s) defining the space or the split. For `"opposite_group"` a
  single column that takes exactly two values (use `.group` for a
  multi-group model); for `"nearest"` one or more numeric columns,
  compared by Euclidean distance.

- role:

  A named list of two conditions, e.g.
  `list(giver = money > 0, receiver = TRUE)`. Within each pair, roles
  are assigned so that each member satisfies its own role's condition;
  if no assignment works the pair is dropped for this step. Rules then
  see `.role`.

- eligible:

  A condition. Agents for which it is `FALSE` sit the step out.

- from:

  For `"network"`, `"neighbour"` (the default) picks a random neighbour
  of the agent; `"random_edge"` picks a random edge of the whole network
  and then one of its endpoints, which selects agents in proportion to
  their degree (used by preferential attachment); `"parent"` is only
  meaningful inside
  [`abm_birth()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_birth.md)'s
  `attach_via` and links a newborn to the agent it was cloned from,
  which is what puts offspring next to their kin.

- among:

  A condition naming the agents that may be *chosen*, for the
  directional modes `"one_of"` and `"nearest"`. Defaults to everybody.
  An agent is never matched to itself.

  It is evaluated over the population, once per candidate, until it
  mentions an `own_<col>`. Then it is a question about the *pair* rather
  than about the candidate, and it is evaluated over the same (chooser,
  candidate) view `cost` minimises over, so every chooser gets a
  candidate set of its own. `among = .id %in% own_sellers` is "one of
  the firms I buy from", which no population condition can say.

- cost:

  For `"nearest"`, an expression naming what the chooser is minimising,
  used instead of `by`. It is evaluated once per (chooser, candidate)
  pair: the candidate's columns are visible under their own names and
  the chooser's under `own_<col>`, the same convention
  [`abm_neighbours()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_neighbours.md)
  uses. `.id` and `.group` are included, so a cost can be a lookup into
  the chooser's own preference list as easily as a price. `by` is the
  special case `cost = (x - own_x)^2`; anything else, a delivered price
  `price + travel * abs(x - own_x)`, an energy deficit, a position in a
  preference list, needs this. `NA` means the candidate is not
  acceptable to that chooser, and a chooser with no acceptable candidate
  sits the step out.

- weight:

  A draw probability for the candidates, for `"one_of"`. The default is
  a uniform draw. Evaluated like `among`: over the population unless it
  mentions an `own_<col>`, in which case it is per (chooser, candidate).
  Non-positive and `NA` weights make a candidate unpickable, and a
  chooser whose candidates all weigh nothing sits the step out. This is
  what "noticed in proportion to its size" and preferential attachment
  as a *step* need.

- .by:

  A column naming a partition the match is confined to: agents are only
  ever matched with agents sharing its value. This is
  [`abm_rules()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_rules.md)'s
  `.by` grouping applied to matching, and on a lattice `.by = .cell` is
  what makes every co-located interaction – predation, mating, combat –
  expressible:
  `abm_match(pair = "opposite_group", by = .group, .by = .cell)` gives
  each wolf one sheep from *its own cell*, and never pairs one sheep
  with two wolves. `NA` sits an agent out.

## Value

An `abm_match` step object.

## Details

After a match, every rule can see:

- `partner_<col>` for each of the partner's columns, when `size = 2`;

- grouped-mutate semantics when `size > 2`, so `sum(contribution)`
  inside a rule means "sum across this agent's group";

- `.role`, when `role` is supplied.

Each `pair` mode uses a fixed set of the other arguments, and passing
one that the chosen mode does not use is an error rather than being
ignored:

|                    |                                               |
|--------------------|-----------------------------------------------|
| mode               | uses                                          |
| `"random"`         | `size`, `role`, `eligible`                    |
| `"one_of"`         | `role`, `eligible`, `among`                   |
| `"opposite_group"` | `by`, `role`, `eligible`                      |
| `"nearest"`        | `by` *or* `cost`, `size`, `eligible`, `among` |
| `"network"`        | `from`, `eligible`                            |

`eligible` and `among` ask different questions, and the difference only
has teeth in the directional modes. `eligible` says who *takes part*;
`among` says who may be *picked*. A consumer choosing the nearest shop
wants `among = .group == "shops"`, or it will find that the nearest
agent to it is another consumer.

Two of the modes are *mutual*: `"random"` and `"opposite_group"`
partition the eligible agents into pairs or groups, so being matched is
symmetric and every agent appears once. The other three are
*directional*: `"one_of"`, `"nearest"` and `"network"` give each agent a
partner of its own, and your partner need not have picked you.
`"one_of"` is NetLogo's `one-of other turtles`; `"random"` is a fresh
shuffle of the whole population into couples. They are different models
of who meets whom, and which one you want depends on the source you are
porting.

## See also

[`abm_go()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_go.md),
which lists every step and fixes the order they run in.
[`abm_link()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_link.md),
[`abm_unlink()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_unlink.md)
and
[`abm_draw()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_draw.md)
also read the pairing a match leaves standing.

## Examples

``` r
abm_match(pair = "random")
#> <abm_match> pair = "random"
abm_match(pair = "one_of")
#> <abm_match> pair = "one_of"
abm_match(pair = "random", size = 4)
#> <abm_match> pair = "random"
#> • size = 4
abm_match(pair = "nearest", by = opinion)
#> <abm_match> pair = "nearest"
#> • by = `opinion`
abm_match(pair = "nearest", by = position, among = .group == "shops")
#> <abm_match> pair = "nearest"
#> • by = `position`
#> • among = `.group == "shops"`

# a firm I do not already buy from, noticed in proportion to its size
abm_match(pair = "one_of", among = .group == "firms" & !.id %in% own_sellers,
          weight = staff)
#> <abm_match> pair = "one_of"
#> • among = `.group == "firms" & !.id %in% own_sellers`
#> • weight = `staff`
abm_match(pair = "nearest", cost = price + abs(x - own_x),
          among = .group == "shops")
#> <abm_match> pair = "nearest"
#> • cost = `price + abs(x - own_x)`
#> • among = `.group == "shops"`
abm_match(pair = "random", role = list(giver = money > 0, receiver = TRUE))
#> <abm_match> pair = "random"
#> • roles = "giver" and "receiver"

# one prey per predator, per cell
abm_match(pair = "opposite_group", by = .group, .by = .cell,
          eligible = .group %in% c("wolves", "sheep"))
#> <abm_match> pair = "opposite_group"
#> • by = `.group`
#> • .by = `.cell`
#> • eligible = `.group %in% c("wolves", "sheep")`
```
