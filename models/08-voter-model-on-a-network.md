# 8. Voter Model on a network

**Concept**

- Setup: 100 agents with a 0/1 `opinion` and a fixed 4-regular network
- Go: adopt a random neighbour's opinion
- Output: consensus, at a speed that depends on the network

**Package**

```r
voter <- abm_setup(
  agents  = abm_agents(n = 100, opinion = ~sample(c(0, 1), n, replace = TRUE)),
  network = abm_network(type = "random", degree = 4))

go <- abm_go(abm_match(pair = "network"), abm_rules(opinion ~ partner_opinion))

result <- abm_run(voter, go, ticks = 200, seed = 8)
```

*Introduced `abm_network()`, stored as a separate edge list rather than a
list-column on the agent tibble.*

---

← [7. Market](07-market-supply-and-demand.md) · [all models](README.md) · [9. Public Goods Game](09-public-goods-game.md) →
