# 9. Public Goods Game

**Concept**

- Setup: 100 agents with a 0/1 `contribution`
- Go: split into random groups of four; the pot is doubled and shared equally
- Output: contributing collapses

**Package**

```r
pgg <- abm_setup(agents = abm_agents(
  n = 100, contribution = ~sample(c(0, 1), n, replace = TRUE), payoff = 0))

go <- abm_go(
  abm_match(pair = "random", size = 4),
  abm_rules(payoff ~ sum(contribution) * 2 / 4)
)

result <- abm_run(pgg, go, ticks = 20, seed = 9)
```

*Introduced `size`. Rules reuse dplyr's grouped-mutate semantics rather than
adding group-aggregate functions, so `sum(contribution)` just means what you
expect.*

---

← [8. Voter Model on a network](08-voter-model-on-a-network.md) · [all models](README.md) · [10. Iterated Prisoner's Dilemma with fixed partners](10-iterated-prisoner-s-dilemma-with-fixed-partners.md) →
