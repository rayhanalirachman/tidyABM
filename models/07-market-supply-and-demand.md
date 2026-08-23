# 7. Market, supply and demand (after Primer)

**Concept**

- Setup: 200 buyers with a private `wtp` and an opening `offer`; 200 sellers with
  a private `wta` and an opening `ask`
- Go: pair across the two groups, haggle for five rounds, adjust next tick's
  opening position depending on whether you traded
- Output: transaction prices converge on the clearing price

**Package**

```r
market <- abm_setup(agents = list(
  buyers  = abm_agents(n = 200, wtp = ~rnorm(n, 50, 10), offer = ~wtp * 0.8),
  sellers = abm_agents(n = 200, wta = ~rnorm(n, 40, 10), ask   = ~wta * 1.2)))

go <- abm_go(
  abm_match(pair = "opposite_group", by = .group, resolve = "negotiate",
            rounds = 5, positions = c(offer, ask), limits = c(wtp, wta)),
  abm_rules(offer ~ if_else(traded, offer * 0.98, offer * 1.02)),
  abm_rules(ask   ~ if_else(traded, ask * 1.02,   ask * 0.98))
)

result <- abm_run(market, go, ticks = 200, seed = 7)
```

*Introduced several agent groups and rule routing by column existence: the `offer`
rule mentions a column only buyers have, so it applies only to buyers. No
`type ==` test, no merging of the two tibbles. `resolve = "negotiate"` needs
`positions` and `limits` because there is no way to infer which columns are the
bid, the ask, and the two reservation prices.*

---

← [6. Party](06-party-segregation-without-geography.md) · [all models](README.md) · [8. Voter Model on a network](08-voter-model-on-a-network.md) →
