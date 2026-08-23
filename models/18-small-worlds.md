# 18. Small Worlds (Watts & Strogatz; NetLogo Networks)

**Concept**

- Setup: 300 nodes in a ring lattice, each joined to its 4 nearest
- Go: with probability *p*, an agent drops one neighbour and links to a stranger
- Output: a little rewiring collapses the average path length while leaving
  clustering almost intact, the small-world regime

**NetLogo**

```netlogo
to rewire-one
  let victim one-of links
  ask victim [
    let node1 end1
    let node2 one-of nodes with [ self != node1 and not link-neighbor? node1 ]
    ask node1 [ create-link-with node2 ]
    die ]
end
```

**Package**

```r
ring <- do.call(rbind, lapply(1:2, function(d)
  data.frame(from = 1:300, to = ((1:300 + d - 1) %% 300) + 1)))

sw <- abm_setup(agents = abm_agents(n = 300, rewiring = FALSE),
                network = abm_network(type = "manual", edges = ring))

go <- function(p) abm_go(
  abm_rules(rewiring ~ runif(n()) < p, .scope = "population"),
  abm_match(pair = "network", eligible = rewiring), abm_unlink(),
  abm_match(pair = "one_of",  eligible = rewiring), abm_link()
)

result <- abm_run(sw, go(0.005), ticks = 20, seed = 2)
```

**Result.** Ring lattice: clustering 0.50, path length 37.9. After
`p = 0.005` for 20 ticks: clustering 0.43, path length 9.2, edge count unchanged.

*Motivated `abm_unlink()` and, more importantly, `pair = "one_of"`. The first
attempt used `pair = "random"` for the re-link, which **partitions the eligible
agents among themselves**, so R agents dropped R edges and gained only R/2. The
network bled edges every tick. `"one_of"` draws each agent a partner from the
whole population, which is NetLogo's `one-of other turtles` and what rewiring
actually means.*

---

← [17. Giant Component](17-giant-component.md) · [all models](README.md) · [19. Fireflies](19-fireflies.md) →
