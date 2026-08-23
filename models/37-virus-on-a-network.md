# 37. Virus on a Network (Stonedahl & Wilensky 2008, NetLogo Networks)

**Concept**

- Setup: 150 nodes on a 6-regular graph, 3 of them infected, each holding a
  private virus-check clock started at a random offset
- Go: infected nodes push the virus along their edges; a node whose clock comes
  round runs a check, and a check that succeeds either cures it back to
  susceptible or leaves it permanently resistant
- Output: whether the epidemic settles into an endemic equilibrium or burns out

**NetLogo**

```netlogo
to go
  if all? turtles [not infected?] [ stop ]
  ask turtles [
    set virus-check-timer virus-check-timer + 1
    if virus-check-timer >= virus-check-frequency [ set virus-check-timer 0 ] ]
  spread-virus
  do-virus-checks
  tick
end

to spread-virus
  ask turtles with [infected?]
    [ ask link-neighbors with [not resistant?]
        [ if random-float 100 < virus-spread-chance [ become-infected ] ] ]
end

to do-virus-checks
  ask turtles with [infected? and virus-check-timer = 0] [
    if random 100 < recovery-chance [
      ifelse random 100 < gain-resistance-chance
        [ become-resistant ] [ become-susceptible ] ] ]
end
```

**Package**

```r
virus_run <- function(gain_resistance, ticks = 2000, seed = 1,
                      n = 150, degree = 6, outbreak = 3,
                      spread = 0.025, check_freq = 20, recovery = 0.05) {
  m <- abm_setup(
    agents = abm_agents(
      n     = n,
      state = ~ifelse(seq_len(n) <= outbreak, "infected", "susceptible"),
      timer = ~sample.int(check_freq, n, replace = TRUE) - 1L
    ),
    network = abm_network(type = "random", degree = degree),
    seed    = seed
  )

  go <- abm_go(
    abm_rules(timer ~ (timer + 1L) %% check_freq),
    abm_neighbours(exposure ~ sum(state == "infected")),
    abm_rules(state ~ if_else(
      state == "susceptible" & runif(n()) < 1 - (1 - spread)^coalesce(exposure, 0L),
      "infected", state
    )),
    abm_rules(state ~ if_else(
      state == "infected" & timer == 0L & runif(n()) < recovery,
      if_else(runif(n()) < gain_resistance, "resistant", "susceptible"),
      state
    )),
    abm_global(infected  ~ mean(state == "infected"),
               resistant ~ mean(state == "resistant"))
  )

  abm_globals(abm_run(m, go, ticks = ticks, seed = seed))
}
```

**Result** (mean over the last 200 of 2000 ticks)

| `gain_resistance` | infected | resistant |
|---|---|---|
| 0, an SIS process | 0.977 | 0.000 |
| 1, an SIR process | 0.019 | 0.981 |

Which is the model's teaching point: the same virus, the same network and the
same spread rate give an endemic infection or a burnt-out one depending on
nothing but whether recovery confers immunity.

*Needed nothing new. Two things about it are worth writing down. The first is
that NetLogo's* push*, every infected node rolling a die at every neighbour,
is exactly a* pull *in distribution: a susceptible node with `k` infected
neighbours is infected with probability `1 - (1 - p)^k`, so
`abm_neighbours(exposure ~ sum(state == "infected"))` followed by one rule says
the same thing. NetLogo builds the infected agentset before asking it, so the
neighbour count is the one at the start of the tick either way. The second is
the private clock: `timer ~ (timer + 1L) %% check_freq` with a random offset per
agent is how you get NetLogo's staggered `virus-check-timer` without any
scheduling machinery, and it is the reason recovery is a slow trickle rather
than a synchronised pulse.*

---

**Reproduce:** [`m37_virus_network.R`](scripts/m37_virus_network.R)

← [36. Information cascade](36-information-cascade.md) · [all models](README.md) · [38. Global cascades on random networks](38-global-cascades-on-random-networks.md) →
