# 3. PD Basic — well-mixed prisoner's dilemma with imitation

**Concept**

- Setup: 100 agents, random `strategy`, `payoff = 0`
- Go: play a random partner, then pair again and copy whoever did better
- Output: cooperation collapses

**Package**

```r
pd <- abm_setup(agents = abm_agents(
  n = 100, strategy = ~sample(c("cooperate", "defect"), n, replace = TRUE), payoff = 0))

go <- abm_go(
  abm_match(pair = "random"),
  abm_rules(payoff ~ case_when(
    strategy == "cooperate" & partner_strategy == "cooperate" ~ 3,
    strategy == "defect"    & partner_strategy == "defect"    ~ 1,
    strategy == "defect"    & partner_strategy == "cooperate" ~ 5,
    strategy == "cooperate" & partner_strategy == "defect"    ~ 0)),
  abm_match(pair = "random"),
  abm_rules(strategy ~ if_else(partner_payoff > payoff, partner_strategy, strategy))
)

result <- abm_run(pd, go, ticks = 100, seed = 3)
```

*Introduced multiple match phases in one tick, written flat and in order.*

---

← [2. El Farol, short form](02-el-farol-short.md) · [all models](README.md) · [4. Ethnocentrism, short form](04-ethnocentrism-short.md) →
