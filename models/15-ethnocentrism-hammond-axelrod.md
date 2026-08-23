# 15. Ethnocentrism, Hammond & Axelrod (2006)

**What was missing.** Two things. Agents need *two* strategy bits — cooperate
with your own tag, cooperate with others — or the tag does no work. And that alone
is not enough: in a well-mixed population egoists still win, which is the paper's
own control condition. The result needs **local reproduction**, so that your
neighbours are disproportionately your kin and "same tag" starts predicting "will
cooperate with me".

**Package**

```r
COST <- 0.01; BENEFIT <- 0.03; BASE_PTR <- 0.12; DEATH <- 0.10; CAPACITY <- 800

random_traits <- list(
  tag      ~ sample(c("red", "blue"), n(), replace = TRUE),
  coop_in  ~ sample(c(TRUE, FALSE),   n(), replace = TRUE),
  coop_out ~ sample(c(TRUE, FALSE),   n(), replace = TRUE))

pop <- abm_setup(
  agents  = abm_agents(n = 400,
                       tag      = ~sample(c("red", "blue"), n, replace = TRUE),
                       coop_in  = ~sample(c(TRUE, FALSE), n, replace = TRUE),
                       coop_out = ~sample(c(TRUE, FALSE), n, replace = TRUE),
                       ptr      = BASE_PTR),
  network = abm_network(type = "random", degree = 4))

go <- abm_go(
  abm_match(pair = "network"),
  abm_rules(give ~ if_else(partner_tag == tag, coop_in, coop_out)),
  abm_rules(ptr ~ BASE_PTR - COST * give + BENEFIT * partner_give),
  abm_birth(when = runif(n()) < ptr,
            attach_via = abm_match(pair = "network", from = "parent")),
  abm_death(when = runif(n()) < DEATH + 0.25 * pmax(0, (n() - CAPACITY) / CAPACITY)),
  do.call(abm_birth, list(n = 8, attach_via = abm_match(pair = "network"),
                          cost = random_traits))
)

result <- abm_run(pop, go, ticks = 400, seed = 1)
```

**Result.** Ethnocentrics ≈ 0.40, egoists ≈ 0.14, and 96% of edges join same-tag
agents. Well-mixed, the same model gives egoists ≈ 0.35 and ethnocentrics ≈ 0.26.

*Motivated `from = "parent"` — there was no way to put an offspring next to its
parent, and a lot of kin-structured models depend on it.*

---

← [14. El Farol with inductive agents](14-el-farol-with-inductive-agents.md) · [all models](README.md) · [16. Zakah with a risk process](16-zakah-with-a-risk-process.md) →
