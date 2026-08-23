# 23. Divide the Cake (Skyrms / Harms; NetLogo)

**Concept**

- Setup: 900 agents demanding 1/3, 1/2 or 2/3 of a cake worth 6
- Go: meet in pairs; if the two demands exceed the cake both die, otherwise each
  reproduces with probability `appetite/6`
- Output: the fair 1/2 demand takes over

**NetLogo**

```netlogo
to eat
  if (count turtles-here = 2 and turn?) [
    ifelse (6 >= sum [appetite] of turtles-here)
      [ ask turtles-here [ reproduce  set turn? false ] ]
      [ ask turtles-here [ die ] ] ]
end
to reproduce
  if (random 6) < appetite [ hatch 1 ]
end
```

**Package**

```r
cake <- abm_setup(agents = abm_agents(
  n = 900, appetite = ~sample(c(2, 3, 4), n, replace = TRUE)))

go <- abm_go(
  abm_match(pair = "random"),
  abm_death(when = appetite + partner_appetite > 6),
  abm_birth(when = runif(n()) < appetite / 6),
  abm_death(when = runif(n()) < pmax(0, (n() - 1200) / n()))
)

result <- abm_run(cake, go, ticks = 150, seed = 1)
```

**Result.** From an even three-way split: greedy gone by tick 20, modest gone by
tick 100, fair at 100% thereafter.

*Motivated **births and deaths seeing the standing match**. `abm_death(when =
appetite + partner_appetite > 6)` is the whole model, and until this it could not
be written: `when` was evaluated against the bare agent tibble, with no
`partner_*` columns. It is the same trade-off the greedy strategy faces —
higher fitness conditional on surviving, exactly offset by a higher chance of a
fatal encounter — and the fair split is what survives it.*

---

← [22. Hawks and Doves](22-hawks-and-doves.md) · [all models](README.md) · [24. Sex Ratio Equilibrium](24-sex-ratio-equilibrium.md) →
