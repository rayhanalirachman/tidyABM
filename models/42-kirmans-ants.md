# 42. Kirman's ants (Kirman 1993)

**Concept**

- Setup: 60 ants, two identical food sources, half at each
- Go: an ant either switches source on its own, with small probability ε, or
  bumps into another ant at random and is converted with probability δ
- Output: the distribution of how many ants are at source A

Nothing distinguishes the two sources. Nothing about the ants changes. And yet
the colony does not sit at 50/50 — it spends long stretches almost entirely at
one source, flips, and spends a long stretch at the other. Kirman's point, in
an economics journal, was that herding and sustained majorities need no
information, no leaders and no memory.

**Package**

```r
kirman <- function(n = 60, epsilon = 0.0002, delta = 0.05, ticks = 4000,
                   seed = 1) {
  m <- abm_setup(
    agents = abm_agents(n = n, at_a = ~seq_len(n) <= n %/% 2),
    seed   = seed
  )

  go <- abm_go(
    abm_match(pair = "one_of"),
    abm_rules(at_a ~ dplyr::case_when(
      runif(n()) < epsilon ~ !at_a,          # switch on your own
      runif(n()) < delta   ~ partner_at_a,   # or be converted
      TRUE                 ~ at_a
    )),
    abm_global(share_a ~ mean(at_a))
  )

  abm_globals(abm_run(m, go, ticks = ticks, seed = seed))$share_a
}
```

**Result** (4000 ticks, first 1000 discarded, n = 60, δ = 0.05)

The stationary distribution of the share at source A is Beta(c, c) with
`c = ε(n − 1)/δ`: U-shaped and herding when c < 1, single-peaked and boring
when c > 1.

| | c = 0.24 (ε = 0.0002) | | c = 3.5 (ε = 0.003) | |
|---|---|---|---|---|
| | run | Beta(c, c) | run | Beta(c, c) |
| time with share < .1 or > .9 | 0.34 | 0.63 | 0.004 | 0.01 |
| time with .4 < share < .6 | 0.033 | 0.07 | 0.324 | 0.40 |
| s.d. of the share | 0.365 | 0.412 | 0.181 | 0.176 |

The c > 1 column matches the analytic distribution closely. The c < 1 column
matches it in shape and in spread but undersamples the extremes, because a
colony sitting at one source only leaves it when a spontaneous switch happens
to catch on, and at ε = 0.0002 that is roughly once every eighty ticks — three
thousand ticks is a handful of excursions, not a converged sample. What is
unambiguous is the contrast: the same colony, the same conversion rate, one
number changed, and the middle of the distribution goes from being where the
colony lives to being where it almost never is.

*Needed nothing new. One tick here is a full sweep — every ant meets somebody —
rather than the single meeting of Kirman's continuous-time process; with δ small
enough that only a few ants convert per sweep the two agree, and since the
stationary distribution depends on ε/δ and not on their scale, the scale is free
to be chosen for how fast the chain mixes. `pair = "one_of"` is doing the real
work: "bump into another ant at random" is NetLogo's `one-of other turtles` and
is not the same as `pair = "random"`, which would partition the colony into
couples and make being met exactly as common as meeting.*

---

**Reproduce:** [`m42_kirman_ants.R`](scripts/m42_kirman_ants.R)

← [41. Minority Game](41-minority-game.md) · [all models](README.md) · [43. Zero-intelligence traders in a double auction](43-zero-intelligence-traders.md) →
