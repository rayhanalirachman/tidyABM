# 25. Axelrod's cultural dissemination (Axelrod 1997)

**Concept**

- Setup: 400 agents on a 4-regular network, each with 5 cultural features taking
  one of *q* traits
- Go: meet a neighbour; interact with probability equal to your cultural
  similarity; if you interact, copy one feature you differ on
- Output: homophily produces *stable cultural regions*, and the number of them
  rises with *q*, more variety means less convergence, not more

**Package**

```r
q <- 5   # traits per feature

cols <- setNames(lapply(1:5, function(i)
  rlang::new_formula(NULL, rlang::expr(sample(1:!!q, n, replace = TRUE)))),
  paste0("f", 1:5))

pop <- abm_setup(agents  = do.call(abm_agents, c(list(n = 400), cols)),
                 network = abm_network(type = "random", degree = 4))

go <- abm_go(
  abm_match(pair = "network"),
  abm_rules(similarity ~ ((f1 == partner_f1) + (f2 == partner_f2) + (f3 == partner_f3) +
                          (f4 == partner_f4) + (f5 == partner_f5)) / 5),
  abm_rules(interact ~ similarity < 1 & runif(n()) < similarity),
  # choose uniformly among the features they differ on, by random key
  abm_rules(k1 ~ if_else(f1 != partner_f1, runif(n()), -1),
            k2 ~ if_else(f2 != partner_f2, runif(n()), -1),
            k3 ~ if_else(f3 != partner_f3, runif(n()), -1),
            k4 ~ if_else(f4 != partner_f4, runif(n()), -1),
            k5 ~ if_else(f5 != partner_f5, runif(n()), -1)),
  abm_rules(best ~ pmax(k1, k2, k3, k4, k5)),
  abm_rules(f1 ~ if_else(interact & k1 == best, partner_f1, f1),
            f2 ~ if_else(interact & k2 == best, partner_f2, f2),
            f3 ~ if_else(interact & k3 == best, partner_f3, f3),
            f4 ~ if_else(interact & k4 == best, partner_f4, f4),
            f5 ~ if_else(interact & k5 == best, partner_f5, f5))
)

result <- abm_run(pop, go, ticks = 400, seed = 3)
```

**Result.** After 400 ticks: q = 2 → 30 distinct cultures; q = 5 → 157;
q = 10 → 287 of 400 agents.

*Needed nothing new. The "pick one feature at random among those that differ"
rule is expressible exactly, give each differing feature a random key and copy
the one with the highest key, rather than approximated. Worth noting that the
random-key trick is a general pattern for "choose one of several options
uniformly" inside a vectorised rule.*

---

← [24. Sex Ratio Equilibrium](24-sex-ratio-equilibrium.md) · [all models](README.md) · [26. PD N-Person Iterated](26-pd-n-person-iterated.md) →
