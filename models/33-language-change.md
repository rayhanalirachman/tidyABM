# 33. Language Change (Troutman & Wilensky 2007, NetLogo Social Science)

**Concept**

- Setup: language users on a preferential-attachment network, each with a weight
  for grammar 1
- Go: everyone utters a form drawn from their own weight, then updates on what
  they heard from their neighbours
- Output: the update rule decides everything. A *threshold* rule amplifies
  whichever grammar is ahead; a *reward* rule averages everybody toward the middle
  and never locks in

**Package**

The network is produced by running the corpus's own preferential-attachment
model and handing its edges to the next `abm_setup()`:

```r
seed_net <- abm_setup(
  agents  = abm_agents(n = 2, dummy = 0),
  network = abm_network(type = "manual", edges = data.frame(from = 1, to = 2))
)

grow <- abm_go(
  abm_birth(n = 1,
            attach_via = abm_match(pair = "network", from = "random_edge")),
  abm_rules(dummy ~ dummy)
)

grown <- abm_run(seed_net, grow, ticks = n - 2, seed = 1)

pop <- abm_setup(
  agents  = abm_agents(n = n, w = ~as.numeric(runif(n) < start)),
  network = abm_network(type = "manual", edges = abm_edges(grown)),
  globals = list(alpha = 0.5, rate = 0.2)
)

go <- abm_go(
  abm_rules(utterance ~ as.numeric(runif(n()) < w)),   # speak
  abm_neighbours(heard ~ mean(utterance)),             # listen
  update                                               # threshold, or reward
)

threshold <- abm_rules(w ~ if_else(is.na(heard), w, as.numeric(heard >= alpha)))
reward    <- abm_rules(w ~ if_else(is.na(heard), w, (1 - rate) * w + rate * heard))

result <- abm_run(pop, go, ticks = 100, seed = 1)
```

**Result.** 200 users, 100 ticks, share using grammar 1:

| start | 0.14 | 0.37 | 0.49 | 0.58 | 0.82 |
|---|---|---|---|---|---|
| threshold → | 0.14 | 0.47 | 0.59 | 0.82 | 0.95 |
| reward → | 0.02 | 0.32 | 0.36 | 0.44 | 0.50 |

The threshold rule pushes a majority toward fixation and kills a minority; the
reward rule flattens every starting condition toward a middling weight. That is
the contrast the model is built to show.

*Needed nothing new.* Its contribution is compositional: `abm_edges()` of one run
is a legal `abm_network(type = "manual", edges =)` for the next, so a model can be
run on a network another model produced without any of it leaving the grammar.

---

**Reproduce:** [`m33_language.R`](scripts/m33_language.R)

← [32. Team Assembly](32-team-assembly.md) · [all models](README.md) · [34. epiDEM Basic](34-epidem-basic.md) →
