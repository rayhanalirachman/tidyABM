# 35. Simple Genetic Algorithm (Wilensky 1998, NetLogo Computer Science)

**Concept**

- Setup: a population of random bit strings
- Go: fitness is the number of ones; two parents are chosen by a tournament of
  three; single-point crossover with probability `crossover-rate`, otherwise a
  clone; then each bit mutates with probability `mutation-rate`
- Output: the population climbs to all-ones, unless mutation is high enough to
  destroy the answer faster than selection finds it

**Package**

The chromosome is `L` scalar columns here and the rules are built with
`do.call()`. A list column would hold the whole genome as one vector per agent,
the way model 41 holds a strategy table; this page keeps the version it was
written with, and the crossover rule is the one thing that reads better spread
across columns, since it is a different parent per bit:

```r
L <- 20; N <- 100                      # 20 bits, 100 individuals
mutation <- 0.03; crossover <- 0.7

start <- setNames(rep(list(~sample(0:1, n, replace = TRUE)), L),
                  paste0("b", seq_len(L)))

pop <- abm_setup(
  agents  = do.call(abm_agents, c(list(n = N), start)),
  globals = list(mut = mutation, xover = crossover, L = L),
  seed    = 1)

bs <- paste0("b", seq_len(L))

# one rule per bit: take the parent's bit, switching parents at the crossover
# point. All L rules live in one abm_rules() call, so every one reads the *old*
# generation.
child_rules <- lapply(bs, function(nm) {
  new_formula(sym(nm), expr(
    if_else(!sexual, (!!sym(nm))[p1],
            if_else(!!which(bs == nm) <= cross, (!!sym(nm))[p1], (!!sym(nm))[p2]))))
})

tournament <- function(out) list(
  abm_rules(t1 ~ sample(n(), n(), replace = TRUE),
            t2 ~ sample(n(), n(), replace = TRUE),
            t3 ~ sample(n(), n(), replace = TRUE)),
  abm_rules(new_formula(sym(out), expr(
    if_else(fitness[t1] >= fitness[t2] & fitness[t1] >= fitness[t3], t1,
            if_else(fitness[t2] >= fitness[t3], t2, t3)))))
)

go <- do.call(abm_go, c(
  list(abm_rules(fitness_rule)),
  tournament("p1"), tournament("p2"),
  list(abm_rules(cross  ~ sample(L, n(), replace = TRUE),
                 sexual ~ runif(n()) < xover)),
  list(do.call(abm_rules, child_rules)),
  list(do.call(abm_rules, mutate_rules)),
  list(abm_rules(fitness_rule))
))

result <- abm_run(pop, go, ticks = 100, seed = 1)
```

**Result.** 20 bits, 100 individuals, 100 generations. With mutation 0.03 the
optimum (fitness 20) is first reached at **generation 7** and held thereafter.
Across mutation rates:

| mutation | 0.01 | 0.03 | 0.10 | 0.30 |
|---|---|---|---|---|
| best | 20 | 20 | 18 | 16 |
| mean | 19.7 | 19.1 | 15.1 | 11.2 |

The error catastrophe, at the rate the NetLogo model puts it.

*Needed nothing new.* It was filed as the third model to hit the same wall as El
Farol's weights and PD N-Person's memory, a vector of per-agent state with only
scalar columns to put it in. That wall was not there: model 41 holds its
strategy table in a list column, and a 20-bit genome fits in one too. What did
work cleanly here: formula objects built with
`rlang::new_formula()` go straight into `abm_rules()` and `do.call(abm_go, ...)`,
so a model whose *shape* depends on a parameter is writable without any string
manipulation.

---

**Reproduce:** [`m35_ga.R`](scripts/m35_ga.R)

← [34. epiDEM Basic](34-epidem-basic.md) · [all models](README.md) · [36. Information cascade](36-information-cascade.md) →
