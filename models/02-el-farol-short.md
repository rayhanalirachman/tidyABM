# 2. El Farol, short form (Arthur 1994)

**Concept**

- Setup: 100 agents with individual `threshold`; one global, `last_attendance`
- Go: go if last week's attendance was below your threshold; attendance updates
- Output: *supposed* to oscillate forever around the bar's capacity

**Package**

```r
elfarol <- abm_setup(
  agents  = abm_agents(n = 100, threshold = ~runif(n, 40, 80)),
  globals = list(last_attendance = 60)
)

go <- abm_go(
  abm_rules(go_today ~ last_attendance < threshold),
  abm_global(last_attendance ~ sum(go_today))
)

result <- abm_run(elfarol, go, ticks = 20, seed = 2)
```

*First model with no matching at all, and the first `abm_global()`. **It does not
work**. One shared forecast makes the population behave as a single agent, and it
locks into a 0/100 two-cycle. See model 14.*

---

← [1. Simple Economy](01-simple-economy.md) · [all models](README.md) · [3. PD Basic](03-pd-basic-well-mixed-prisoner-s-dilemma-with-imitation.md) →
