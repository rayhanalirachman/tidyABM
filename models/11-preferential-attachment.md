# 11. Preferential Attachment (Wilensky & Rand, ch. 5)

**Concept**

- Setup: two agents joined by one edge
- Go: one new agent joins, linking to an existing agent chosen in proportion to
  its degree
- Output: a scale-free network with hubs

**NetLogo**

```netlogo
to-report find-partner
  report [one-of both-ends] of one-of links
end
```

**Package**

```r
pa <- abm_setup(
  agents  = abm_agents(n = 2),
  network = abm_network(type = "manual", edges = data.frame(from = 1, to = 2)))

go <- abm_go(abm_birth(
  n = 1, attach_via = abm_match(pair = "network", from = "random_edge")))

result <- abm_run(pa, go, ticks = 300, seed = 11)
```

*Introduced network growth during a run. `from = "random_edge"` reuses NetLogo's
trick, pick an edge uniformly, then one of its endpoints, so selection is
degree-proportional without anyone storing a degree.*

---

← [10. Iterated Prisoner's Dilemma with fixed partners](10-iterated-prisoner-s-dilemma-with-fixed-partners.md) · [all models](README.md) · [12. Random Consumption Zakah, short form](12-random-consumption-zakah-short.md) →
