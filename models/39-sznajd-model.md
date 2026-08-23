# 39. Sznajd model (Sznajd-Weron & Sznajd 2000)

**Concept**

- Setup: 16 agents on a ring, each holding an opinion of +1 or −1
- Go: pick one adjacent pair. If the two agree, they *together* persuade the
  agents on either side of them. If they disagree, nothing happens.
- Output: the ring always reaches consensus; which consensus depends on where
  it started, but not linearly

"United we stand, divided we fall", the rule is *outflow* dynamics, and it is
the mirror image of every other opinion model in this corpus, all of which are
inflow: an agent looks at its neighbours and updates itself.

**Package**

```r
sznajd <- function(n = 16, p_up = 0.5, ticks = 300, seed = 1) {
  m <- abm_setup(
    agents  = abm_agents(n = n,
                         spin     = ~ifelse(runif(n) < p_up, 1L, -1L),
                         speaking = FALSE),
    network = abm_network(type = "ring", degree = 2),
    seed    = seed
  )

  go <- abm_go(
    abm_match(pair = "network", eligible = seq_len(n()) == sample.int(n(), 1)),
    abm_rules(speaking ~ !is.na(.partner) & spin == partner_spin),
    abm_tell(speaking ~ TRUE, to = .partner, when = speaking),
    abm_tell(spin ~ spin, to = "neighbours", when = speaking),
    abm_rules(speaking ~ FALSE, .scope = "population"),
    abm_global(up ~ mean(spin > 0))
  )

  abm_globals(abm_run(m, go, ticks = ticks, seed = seed))$up
}
```

**Result** (30 runs at each starting density, n = 16, 300 ticks)

| initial density of +1 | consensus reached | P(consensus is +1) | mean ticks to consensus |
|---|---|---|---|
| 0.25 | 1.00 | 0.07 | 46 |
| 0.50 | 0.93 | 0.39 | 79 |
| 0.75 | 0.90 | 0.81 | 38 |

The voter model would put the middle column at 0.25, 0.50 and 0.75 exactly. Its
exit probability is the initial density, because a voter update is a fair coin. Outflow dynamics are steeper than that: a minority is wiped out more often
than its share, a majority runs away more often than its share. Whether the
curve becomes a true step function as the ring grows is still argued over in the
literature, and 16 agents is far too few to settle it. What the run does show
unambiguously is that the curve is not the straight line.

*Forced `abm_tell(to = "neighbours")`, and it is the model that shows why
`abm_neighbours()` was not enough. Whether you are persuaded here depends on a
coincidence between two* other *agents, your neighbour and its neighbour on the
far side, which is not visible from your own row at all, so there is no
neighbourhood aggregate that computes it. The pair has to reach out. The second
`abm_tell` in the block is a small trick worth noting: `pair = "network"` is
directional, so only the picked agent knows it is in a pair, and
`abm_tell(speaking ~ TRUE, to = .partner)` is how it brings its partner in so the
pair speaks from both ends.*

---

**Reproduce:** [`m39_sznajd.R`](scripts/m39_sznajd.R)

← [38. Global cascades on random networks](38-global-cascades-on-random-networks.md) · [all models](README.md) · [40. Naming Game](40-naming-game.md) →
