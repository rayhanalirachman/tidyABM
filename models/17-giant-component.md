# 17. Giant Component (NetLogo Networks)

**Concept**

- Setup: 400 isolated nodes
- Go: add edges at random
- Output: the largest connected component stays small until the mean degree
  passes 1, then suddenly contains almost everybody

**NetLogo**

```netlogo
to go
  ask one-of turtles [ create-link-with one-of other turtles ]
  find-all-components
  tick
end
```

**Package**

```r
gc_model <- abm_setup(agents = abm_agents(n = 400),
                      network = abm_network(type = "empty"))

go <- abm_go(
  abm_match(pair = "random", eligible = runif(n()) < 0.05),
  abm_link()
)

result <- abm_run(gc_model, go, ticks = 60, seed = 1)
```

**Result.** Mean degree 0.98 → largest component 28%; degree 2.0 → 80%; degree
3.0 → 95%. Textbook Erdős–Rényi percolation.

*Motivated `abm_link()`. Until this model the network could only grow through
`abm_birth(attach_via =)`, so a graph process with a fixed population was
inexpressible.*

---

← [16. Zakah with a risk process](16-zakah-with-a-risk-process.md) · [all models](README.md) · [18. Small Worlds](18-small-worlds.md) →
