# 27. Threshold model of collective behaviour (Granovetter 1978)

**Concept**

- Setup: 100 people, each with a threshold, the number of others already
  rioting it needs to see before joining
- Go: everybody looks at last tick's count and decides
- Output: the outcome is set by the *shape* of the threshold distribution, not
  by how radical the crowd is on average

**Package**

```r
riot <- function(thresholds, ticks = 120) {
  crowd <- abm_setup(
    agents  = abm_agents(n = length(thresholds), threshold = thresholds,
                         rioting = FALSE),
    globals = list(n_rioting = 0)
  )
  go <- abm_go(
    abm_rules(rioting ~ n_rioting >= threshold),
    abm_global(n_rioting ~ sum(rioting))
  )
  abm_run(crowd, go, ticks = ticks, seed = 1)
}

r <- riot(0:99)                                  # thresholds 0, 1, 2, ..., 99
bumped <- 0:99; bumped[2] <- 2                   # one person's 1 becomes a 2
```

**Result.** With thresholds `0:99` the cascade adds exactly one rioter per tick
and ends with all 100 rioting. Changing a single person's threshold from 1 to 2
breaks the chain at the first link and leaves **1** rioter. That is Granovetter's
own example, reproduced exactly: two crowds with almost identical
distributions, same mean, same median, one person different, end up 99 people
apart.

*Needed nothing new.* It is the smallest model in the corpus that uses no
matching at all: the entire interaction runs through one global.

---

**Reproduce:** [`m27_granovetter.R`](scripts/m27_granovetter.R)

← [26. PD N-Person Iterated](26-pd-n-person-iterated.md) · [all models](README.md) · [28. Bounded confidence, pairwise](28-bounded-confidence-pairwise.md) →
