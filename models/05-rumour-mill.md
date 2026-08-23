# 5. Rumour Mill

**Concept**

- Setup: 200 agents, one spreader, the rest ignorant
- Go: meet at random; ignorant + spreader → spreader; spreader + spreader or
  stifler → stifler
- Output: the rumour dies out before everyone hears it

**Package**

```r
rumour <- abm_setup(agents = abm_agents(
  n = 200, state = ~c("spreader", rep("ignorant", n - 1))))

go <- abm_go(
  abm_match(pair = "random"),
  abm_rules(state ~ case_when(
    state == "ignorant" & partner_state == "spreader" ~ "spreader",
    state == "spreader" & partner_state == "spreader" ~ "stifler",
    state == "spreader" & partner_state == "stifler"  ~ "stifler",
    TRUE ~ state))
)

result <- abm_run(rumour, go, ticks = 100, seed = 5)
```

*Introduced nothing, which was the point: symmetric state comparison through
`partner_*` is self-consistent and needs no `role`. Conserved-quantity transfers
need `role`; symmetric state changes do not.*

---

← [4. Ethnocentrism, short form](04-ethnocentrism-short.md) · [all models](README.md) · [6. Party](06-party-segregation-without-geography.md) →
