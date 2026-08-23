# 56. Predator and prey without space (Lotka 1925; Volterra 1926)

**Concept**

- Setup: sheep and wolves, no grid; sheep reproduce, wolves burn energy and
  must eat
- Go: wolves search for sheep, how many find one is the model's *functional
  response*, a hunt succeeds with probability `catch`, a wolf that eats gains
  energy and one that runs out dies; both reproduce at a fixed rate
- Output: whether the two populations cycle, and by how much the predator lags
  the prey

**Package**

```r
response <- "mass_action"
n_sheep <- 250; n_wolves <- 175; area <- 300; K <- 1250
sheep_rep <- 0.03; wolf_rep <- 0.02; catch <- 0.04; gain <- 30

m <- abm_setup(
  agents  = list(
    sheep  = abm_agents(n = n_sheep,  energy = 0),
    wolves = abm_agents(n = n_wolves, energy = ~runif(n, 1, gain))),
  globals = list(n_sheep = n_sheep, n_wolves = n_wolves),
  seed    = 1)

# The pairing mode *is* the functional response. "opposite_group" makes
# min(S, W) encounters, which is ratio-dependent; filtering the hunters by
# sheep density first makes S*W/area of them, which is mass action.
hunt <- if (response == "mass_action") {
  abm_match(pair = "opposite_group", by = .group,
            eligible = .group == "sheep" | runif(n()) < n_sheep / area)
} else {
  abm_match(pair = "opposite_group", by = .group)
}

go <- abm_go(
  abm_global(n_sheep ~ sum(.group == "sheep"), n_wolves ~ sum(.group == "wolves")),
  abm_rules(caught ~ FALSE, .scope = "population"),
  hunt,
  abm_rules(caught ~ runif(1) < catch),
  abm_rules(energy ~ if_else(.group == "wolves" & caught, energy + gain,
                             energy - (.group == "wolves")), .scope = "population"),
  abm_death(when = (.group == "sheep" & caught) | (.group == "wolves" & energy <= 0)),
  abm_birth(when = .group == "sheep" & runif(n()) < sheep_rep * (1 - n_sheep / K)),
  abm_birth(when = .group == "wolves" & runif(n()) < wolf_rep, cost = energy ~ energy / 2)
)

# `record = "globals"` keeps the two counts and none of the agents, which is the
# difference between a run that finishes and one the kernel stops.
result <- abm_run(m, go, ticks = 2000, seed = 1, record = "globals")
```

**Result** (mass action. Catch 0.04, gain 30, r 0.03, K 1250, area 300, 2000 ticks)

| | mean | min | max | predicted |
|---|---|---|---|---|
| sheep | 605 | 65 | 1085 | 250 |
| wolves | 195 | 40 | 474 | 180 |

**Period 600 ticks. The predator peak lags the prey peak by 149 ticks. A
quarter cycle is 150.**

That last line is the model. Lotka and Volterra's cycles are neutrally stable
and the predator trails the prey by exactly a quarter period, the wolves peak
when the sheep are already crashing, because what drives wolf numbers is how
many sheep there *were*, not how many there are. Nothing in this
implementation knows that: it is five hundred agents eating each other, with a
delay that nobody wrote down. The mean wolf population sits close to the
equilibrium the parameters imply. The mean sheep population does not, and
should not, because the cycles are large and the average of a nonlinear
oscillation is not its fixed point.

Running the same code with the other pairing mode gives no cycle at all. Sheep
between 1 and 1083, wolves between 2 and 61, wandering near extinction.
That is not a bug, and it is the point of writing both. `pair =
"opposite_group"` makes `min(S, W)` encounters, so when sheep are plentiful
every wolf eats regardless of how plentiful, and predation is set by the number
of predators rather than by prey abundance. Filtering the hunters by sheep
density first, with `eligible = .group == "sheep" | runif(n()) < n_sheep / area`,
makes `S·W/area` encounters, which is mass action. The functional response is
a modelling decision on a par with the birth rate, and in this grammar it is
visible as the choice of matching mode rather than buried in a rate constant.

*Needed no package change, and shows what the mutual matching modes are for.
Two wolves must not eat the same sheep, and `"opposite_group"` guarantees it by
partitioning, the exclusion is structural rather than something the rules have
to police. Written with the directional `"one_of"`, every wolf would pick a
sheep independently, several would pick the same one, and the model would need
an ordering rule to decide who actually got it.*

*One trap, and it cost an afternoon. `abm_rules()` after a match evaluates only
for agents the match placed in a group, so an unmatched agent keeps whatever
that column held last tick. A wolf that ate once and then went unpaired kept
`caught = TRUE` and fed forever. The fix is the Hotelling idiom, reset the
column with a population-scope rule before the match, and it is worth stating
as a rule of thumb: a column written inside a match is a property of the
encounter, not of the agent, and has to be cleared like one.*

*The other thing this model exposed was not a grammar limitation but a real
one, and Part 7 fixed it: `abm_run()` kept every tick's snapshot, so a model
whose population grows ran out of memory rather than slowing down. An earlier
parameterisation took the wolves to 47,000 and was killed by the kernel.
`abm_run(record =)` now says how much to keep:* `"all"`, *every* n*th tick,*
`"final"`, *or* `"globals"` *for none of the populations at all. Globals are
recorded every tick regardless, which is what makes* `"globals"` *the right
setting here: the output of this model is two counts per tick, and the
populations themselves are only in the way.*

---

**Reproduce:** [`m56_predprey.R`](scripts/m56_predprey.R)

← [55. Deferred acceptance](55-deferred-acceptance.md) · [all models](README.md)
