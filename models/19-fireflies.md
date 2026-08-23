# 19. Fireflies (Buck 1988; NetLogo Biology)

**Concept**

- Setup: 600 fireflies, each with a `clock` at a random phase of a shared 10-tick
  cycle; a firefly flashes while `clock < flash_length`
- Go: advance the clock; if you are dark and enough neighbours flashed, reset your
  clock so your next flash lines up with theirs
- Output: global synchrony from random phases, with no leader

**NetLogo**

```netlogo
to go
  ask turtles [ move  increment-clock
                if ((clock > window) and (clock >= threshold)) [ look ] ]
  ask turtles [ recolor ]     ;; second loop: everyone sees last tick's flashes
  tick
end
to look
  if count turtles in-radius 1 with [color = yellow] >= flashes-to-reset
    [ set clock reset-level ]
end
```

**Package**

```r
CYCLE <- 10; FLASH <- 1

ff <- abm_setup(
  agents  = abm_agents(n = 600, clock = ~sample(0:(CYCLE - 1), n, replace = TRUE),
                       flashing = FALSE),
  network = abm_network(type = "random", degree = 6))

go <- abm_go(
  abm_neighbours(seen ~ sum(flashing)),                       # last tick's flashes
  abm_rules(clock ~ (clock + 1) %% CYCLE),
  abm_rules(clock ~ if_else(clock >= FLASH & coalesce(seen, 0) >= 1, FLASH, clock)),
  abm_rules(flashing ~ clock < FLASH)                         # phase delay
)

result <- abm_run(ff, go, ticks = 120, seed = 1)
```

**Result.** Flashing count goes from scattered noise (peaks ~200 of 600) to a
clean sawtooth: 600 lit, then nine dark ticks, repeating.

*Motivated `abm_neighbours()`. A match gives an agent **one** partner; this model
needs a count over the **whole** neighbourhood. Reading the flashes before
advancing the clock reproduces NetLogo's two-loop structure, where everyone sees
the pattern as it stood at the end of the previous tick.*

---

← [18. Small Worlds](18-small-worlds.md) · [all models](README.md) · [20. SIR on a network with recovery timers](20-sir-on-a-network-with-recovery-timers.md) →
