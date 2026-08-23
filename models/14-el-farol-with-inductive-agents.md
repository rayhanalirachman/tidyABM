# 14. El Farol with inductive agents (Arthur 1994)

**What was missing.** The short form gives every agent the same forecast, so the
population is one agent chasing its own tail. Heterogeneous *fixed* forecasts do
not help either, the map is still deterministic and still converges. Arthur's
mechanism is inductive: each agent holds several candidate predictors, scores them
against what actually happened, and acts on whichever has been working.

**Package**

```r
MEMORY <- 5; N_STRAT <- 10; CAPACITY <- 60

# one column per weight per strategy, built with do.call
strategy_columns <- function() {
  cols <- list()
  for (s in seq_len(N_STRAT)) {
    for (j in 0:MEMORY) cols[[sprintf("w%d_%d", s, j)]] <- ~runif(n, -1, 1)
    cols[[sprintf("e%d", s)]] <- 0
  }
  cols$active <- 1L
  cols
}

farol <- abm_setup(
  agents  = do.call(abm_agents, c(list(n = 100), strategy_columns())),
  globals = setNames(as.list(rep(CAPACITY, MEMORY)), paste0("att", 1:MEMORY)))

go <- abm_go(
  forecast,        # p1..p10 = w_s0*100 + sum_j w_sj * att_j
  act,             # go_today = (the active predictor's forecast) < 60
  observe,         # att5 ~ att4, ..., att1 ~ sum(go_today)
  rescore,         # e_s ~ 0.8*e_s + 0.2*abs(p_s - att1)
  switch_to_best   # active ~ argmin(e_1..e_10)
)

result <- abm_run(farol, go, ticks = 300, seed = 2)
```

**Result.** Attendance mean ≈ 59, sd ≈ 4.6, range 47–68, never repeating.

*Exposes a grammar gap: there is no compact way to give an agent a **set** of
anything. Ten predictors over five lags is seventy columns, only bearable with
`do.call()`. Sensitive too, three predictors instead of ten and it locks up
again.*

---

← [13. Bank Reserves](13-bank-reserves.md) · [all models](README.md) · [15. Ethnocentrism, Hammond & Axelrod](15-ethnocentrism-hammond-axelrod.md) →
