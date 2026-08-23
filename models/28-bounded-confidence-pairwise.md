# 28. Bounded confidence, pairwise (Deffuant, Neau, Amblard & Weisbuch 2000)

**Concept**

- Setup: 1000 agents with opinions drawn uniformly on [0, 1]
- Go: agents meet in pairs; if their opinions are within `d`, each moves a
  fraction `mu` of the way toward the other
- Output: the population fragments into clusters, and the number of them is set
  by `d` alone

**Package**

```r
deffuant <- function(d, mu = 0.5, n = 1000, ticks = 600) {
  pop <- abm_setup(
    agents  = abm_agents(n = n, opinion = ~runif(n)),
    globals = list(d = d, mu = mu),
    seed    = 42
  )
  go <- abm_go(
    abm_match(pair = "random"),
    abm_rules(opinion ~ if_else(abs(opinion - partner_opinion) < d,
                                opinion + mu * (partner_opinion - opinion),
                                opinion))
  )
  abm_run(pop, go, ticks = ticks, seed = 1)
}
```

**Result.** Counting clusters holding at least 5% of the population:

| `d` | 0.50 | 0.30 | 0.25 | 0.20 | 0.15 | 0.10 |
|---|---|---|---|---|---|---|
| clusters | 1 | 1 | 2 | 2 | 3 | 5 |
| `floor(1/(2d))` | 1 | 1 | 2 | 2 | 3 | 5 |

Six for six against the paper's estimate `n_max ≈ 1/(2d)`.

*Needed nothing new.* The whole model is one match and one conditional rule. It
is the cleanest demonstration in the corpus that `partner_<col>` plus
`if_else()` is enough for a pairwise interaction model.

---

**Reproduce:** [`m28_deffuant.R`](scripts/m28_deffuant.R), [`m28_deffuant_peaks.R`](scripts/m28_deffuant_peaks.R)

← [27. Threshold model of collective behaviour](27-threshold-model-of-collective-behaviour.md) · [all models](README.md) · [29. Bounded confidence, all-neighbour](29-bounded-confidence-all-neighbour.md) →
