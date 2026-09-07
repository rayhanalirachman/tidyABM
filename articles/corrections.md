# Two models that need more than the sketch

``` r

library(tidyABM)
```

Two of the models in
[`vignette("models")`](https://rayhanalirachman.github.io/tidyABM/articles/models.md)
run correctly and still do not show the behaviour they are famous for.
That is not a bug in the package or in the translation. It is that the
short version of each model, the one that fits in a paragraph, leaves
out the mechanism that produces the result. This vignette shows what is
missing in each case and puts it back.

The distinction matters when you are porting models. A translation can
be faithful to the source you were given and still be scientifically
wrong, because the source was a sketch. The only way to catch it is to
check the output against what the model is supposed to do, which is why
every model in this package’s test suite asserts a behavioural claim
rather than just “it ran”.

## El Farol: one predictor is no predictor

Arthur’s bar problem is famous for never settling. A hundred people
decide independently whether to go to a bar that is only fun below 60.
If everyone expects it to be empty everyone goes, and the expectation
destroys itself.

The short version gives every agent the same forecast, last week’s
attendance, and differs only in the threshold each will tolerate:

``` r

short_pop <- abm_setup(
  agents  = abm_agents(n = 100, threshold = ~runif(n, 40, 80)),
  globals = list(last_attendance = 60))

short_go <- abm_go(
  abm_rules(go_today ~ last_attendance < threshold),
  abm_global(last_attendance ~ sum(go_today)))

short <- abm_run(short_pop, short_go, ticks = 40, seed = 2)

tail(abm_globals(short)$last_attendance, 12)
#>  [1]   0 100   0 100   0 100   0 100   0 100   0 100
```

Everybody goes, then nobody goes, forever. With a single shared forecast
the population is effectively one agent, and one agent chasing its own
tail produces a two-cycle. Giving the agents heterogeneous *fixed*
forecasts is not enough either. It moves the cycle, but the map is still
deterministic and still converges.

Arthur’s actual mechanism is **inductive**: each agent holds several
candidate predictors, scores them against what actually happened, and
acts on whichever has been working. Nobody’s forecast is stable, so the
population’s response to a given history keeps changing, and attendance
never settles.

Each agent therefore has to carry a *set* of predictors rather than a
number, and a column holds a set as readily as it holds a scalar. `w` is
a matrix of weights per agent, ten predictors over five lags and a
constant; `e` is that agent’s rolling error for each of the ten; `p` is
what each of them said this week. All three are ordinary list columns.

``` r

MEMORY <- 5    # weeks of attendance each predictor looks at
N_STRAT <- 10  # candidate predictors per agent
CAPACITY <- 60

farol <- abm_setup(
  agents = abm_agents(
    n = 100,
    w = ~lapply(seq_len(n), function(i)
          matrix(runif(N_STRAT * (MEMORY + 1), -1, 1), N_STRAT, MEMORY + 1)),
    e = ~lapply(seq_len(n), function(i) numeric(N_STRAT)),
    p = ~lapply(seq_len(n), function(i) numeric(N_STRAT)),
    go_today = FALSE),
  globals = as.list(setNames(rep(CAPACITY, MEMORY), paste0("att", 1:MEMORY))),
  seed = 1)
```

The tick is the four things the description names: forecast with every
predictor, act on one, observe what happened, re-score them all. There
is no fifth step for switching, because “act on whichever has been
working” is `which.min(e)` read at the moment of acting.

``` r

go <- abm_go(
  abm_rules(p ~ lapply(w, function(W) as.vector(W %*% c(100, att1, att2, att3, att4, att5)))),
  abm_rules(go_today ~ mapply(function(pi, ei) pi[which.min(ei)], p, e) < CAPACITY),
  abm_global(att5 ~ att4, att4 ~ att3, att3 ~ att2, att2 ~ att1,
             att1 ~ sum(go_today)),
  abm_rules(e ~ mapply(function(ei, pi) 0.8 * ei + 0.2 * abs(pi - att1),
                       e, p, SIMPLIFY = FALSE))
)

r <- abm_run(farol, go, ticks = 300, seed = 2)

attendance <- abm_globals(r)$att1[-(1:101)]
c(mean = round(mean(attendance), 1), sd = round(sd(attendance), 1),
  min = min(attendance), max = max(attendance),
  distinct = length(unique(attendance)))
#>     mean       sd      min      max distinct 
#>     57.2      3.4     50.0     65.0     16.0
```

Attendance sits at the capacity of 60 and keeps moving, which is
Arthur’s result. It is worth being precise about what “keeps moving”
means, because the short version did not merely fluctuate less, it
locked into a cycle. Over the last 200 ticks this run visits 16 distinct
attendance levels and matches no lag up to six, so there is no period to
find.

The mechanism is the size of the predictor pool, not the inductive
machinery on its own. Across four population draws, ten predictors per
agent never settles (sd 1.8 to 3.7, ten to seventeen distinct levels, no
period); three predictors locks into a cycle in two draws of four; one
predictor locks in all four, which is the degenerate case the first run
showed. Both seeds matter here: `abm_setup(seed =)` fixes which
predictors the agents are born with, and it changes the answer as much
as `abm_run(seed =)` does.

``` r

one_predictor <- abm_setup(
  agents = abm_agents(
    n = 100,
    w = ~lapply(seq_len(n), function(i) matrix(runif(MEMORY + 1, -1, 1), 1, MEMORY + 1)),
    e = ~lapply(seq_len(n), function(i) numeric(1)),
    p = ~lapply(seq_len(n), function(i) numeric(1)),
    go_today = FALSE),
  globals = as.list(setNames(rep(CAPACITY, MEMORY), paste0("att", 1:MEMORY))),
  seed = 1)

tail(abm_globals(abm_run(one_predictor, go, ticks = 300, seed = 2))$att1, 8)
#> [1] 71 71 71 71 71 71 71 71
```

**What this exposes about the grammar.** Nothing, as it turns out, and
that is worth recording. This model was first written as seventy scalar
columns and five blocks of
[`rlang::new_formula()`](https://rlang.r-lib.org/reference/new_formula.html)
scaffolding, and it was described here as the model that proved the
grammar could not give an agent a set. It can. What was missing was the
idiom, not the capability: rules over list columns are
[`lapply()`](https://rdrr.io/r/base/lapply.html) and
[`mapply()`](https://rdrr.io/r/base/mapply.html) where you would
otherwise write arithmetic, and nothing pointed that out. The minority
game and the naming game found it first.

## Ethnocentrism: cooperation has to be conditional, and neighbours have to be kin

Hammond and Axelrod’s model is famous for showing that in-group
favouritism can evolve from nothing. The short version gives agents a
tag and a strategy, but the strategy ignores the tag, so the tag does no
work, defection is simply the best move, and the population runs itself
down.

The real model gives every agent **two** strategy bits: whether to
cooperate with someone of your own tag, and whether to cooperate with
someone of a different one. That makes four types.

``` r

COST <- 0.01; BENEFIT <- 0.03; BASE_PTR <- 0.12; DEATH <- 0.10; CAPACITY <- 800

random_traits <- list(
  tag      ~ sample(c("red", "blue"), n(), replace = TRUE),
  coop_in  ~ sample(c(TRUE, FALSE),   n(), replace = TRUE),
  coop_out ~ sample(c(TRUE, FALSE),   n(), replace = TRUE)
)

start_pop <- function(network = NULL) {
  abm_setup(
    agents = abm_agents(
      n = 400,
      tag      = ~sample(c("red", "blue"), n, replace = TRUE),
      coop_in  = ~sample(c(TRUE, FALSE), n, replace = TRUE),
      coop_out = ~sample(c(TRUE, FALSE), n, replace = TRUE),
      ptr      = BASE_PTR),
    network = network,
    seed    = 1)
}

label <- function(r) {
  dplyr::mutate(r, type = case_when(
    coop_in & !coop_out ~ "ethnocentric",
    coop_in &  coop_out ~ "altruist",
   !coop_in & !coop_out ~ "egoist",
    TRUE                ~ "traitor"))
}
shares <- function(r, t) {
  x <- label(r); x <- x$type[x$tick == t]
  round(sort(table(x) / length(x), decreasing = TRUE), 3)
}
```

Even with conditional strategies, a well-mixed population does not
produce ethnocentrism. Everyone meets a random stranger, so the tag
carries no information, and the cheapest strategy wins:

``` r

well_mixed_go <- abm_go(
  abm_match(pair = "random"),
  abm_rules(give ~ if_else(partner_tag == tag, coop_in, coop_out)),
  abm_rules(ptr ~ BASE_PTR - COST * give + BENEFIT * partner_give),
  abm_birth(when = runif(n()) < ptr),
  abm_death(when = runif(n()) < DEATH + 0.25 * pmax(0, (n() - CAPACITY) / CAPACITY)),
  abm_birth(n = 8, inherit = random_traits)
)

mixed <- abm_run(start_pop(), well_mixed_go, ticks = 400, seed = 1)

shares(mixed, 400)
#> x
#>       egoist     altruist ethnocentric      traitor 
#>        0.514        0.215        0.159        0.111
```

Egoists, at half the population. This is not a failure. It is Hammond
and Axelrod’s own control condition, and it is the point of their paper:
ethnocentrism needs *local* structure. Offspring have to settle next to
their parents, so that your neighbours are disproportionately your kin,
so that “same tag” actually predicts “will cooperate with me”.

That is what the model needs, and it took two arguments the package did
not have. `attach_via` could put a newborn next to a random agent or a
well-connected one, but not next to its own parent, which
`from = "parent"` fixed. And a newborn got exactly one edge, which is
not a place in a neighbourhood but a leaf hanging off one; `links` gives
it the degree the model means. Both births below take four links, the
degree the population starts with.

``` r

local_go <- abm_go(
  abm_match(pair = "network"),
  abm_rules(give ~ if_else(partner_tag == tag, coop_in, coop_out)),
  abm_rules(ptr ~ BASE_PTR - COST * give + BENEFIT * partner_give),
  abm_birth(when = runif(n()) < ptr, links = 4,
            attach_via = abm_match(pair = "network", from = "parent")),
  abm_death(when = runif(n()) < DEATH + 0.25 * pmax(0, (n() - CAPACITY) / CAPACITY)),
  abm_birth(n = 8, links = 4, inherit = random_traits,
            attach_via = abm_match(pair = "network"))
)

local <- abm_run(start_pop(abm_network(type = "random", degree = 4)), local_go,
                 ticks = 400, seed = 1)

shares(local, 400)
#> x
#>     altruist ethnocentric      traitor       egoist 
#>        0.375        0.365        0.138        0.122
```

Egoists collapse, from about half the population to about an eighth, and
the two strategies that cooperate with their own kind take three
quarters of it between them. That is the paper’s result and it
replicates: over five population draws egoists average 0.37 well mixed
and 0.15 locally.

Which of the two in-group strategies leads is a weaker claim than the
vignette used to make. Ethnocentrics average 0.37 against altruists’
0.33 across those five draws, but they lead in only two of them, and
this seed is one where the altruists are marginally ahead. The reason is
visible in the network: clustering this strong means out-group
encounters are rare, and a strategy that pays a cost to refuse them
saves little by doing so.

``` r

edges <- abm_edges(local)
final <- local[local$tick == 400, ]
tags  <- setNames(final$tag, final$.id)
mean(tags[as.character(edges$from)] == tags[as.character(edges$to)], na.rm = TRUE)
#> [1] 0.8365911
```

Around 0.5 would mean neighbours are random with respect to tag. What
you get is around 0.84: local reproduction has sorted the population
into same-tag neighbourhoods, and that is the whole mechanism.

### What one edge per newborn was doing

`links` is not decoration, and the reason is worth showing, because the
model runs perfectly well without it and reports a *better* number.

With one edge per newborn, deaths prune four edges and births replace
one. The 4-regular graph the run starts on is gone within twenty-five
ticks. Over five draws it ends at a mean degree of 0.97, with 27% of the
population joined to nobody at all and sitting out every match. What is
left is a forest of parent-child pairs, and 0.955 of those edges join
agents of the same tag, which is close to 1 for the trivial reason that
most of them are a parent and its own child.

With `links = 4` the network is a network: mean degree 3.3, 5% isolated,
and a same-tag share of 0.85 that describes neighbourhoods rather than
kinship dyads. The strategy shares barely move, 0.39 ethnocentrics
against 0.37, which is the good outcome: the mechanism was real, and the
eroded network was overstating the evidence for it.

## What to take from this

The two failures have the same shape: a mechanism was compressed out of
the description, and the compressed version still runs.

- El Farol lost the *inductive* part, the agents that revise which
  forecast they trust. Without it the population is one agent.
- Ethnocentrism lost both the tag-conditional strategy and the local
  reproduction. Without the second one you reproduce the paper’s
  control, not its result.

El Farol needed no change to the package. Ethnocentrism needed two
arguments, and both were the same missing idea: a newborn has to arrive
*somewhere*. `from = "parent"` says where, and `links` says how much of
a neighbourhood it gets when it lands. Without the first there is no kin
structure. Without the second the kin structure eats the network.

There is a third thing to take from this, which is about reading a
result rather than writing a model. Each of these corrections was
checked by asking whether the number the model reports means what the
surrounding sentence claims. The uncorrected El Farol had a plausible
attendance series that was a two-cycle. The uncorrected ethnocentrism
had a same-tag edge share of 0.96 that was measuring a forest of
parent-child pairs. Both are the kind of number a model happily produces
and a reader happily accepts.
