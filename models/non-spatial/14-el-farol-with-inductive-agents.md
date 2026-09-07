# 14. El Farol with inductive agents (Arthur 1994)

**What was missing.** The short form gives every agent the same forecast, so the
population is one agent chasing its own tail. Heterogeneous *fixed* forecasts do
not help either, the map is still deterministic and still converges. Arthur's
mechanism is inductive: each agent holds several candidate predictors, scores
each one by how well it would have predicted the recent past, and acts on
whichever currently has the smallest error. Nobody's forecast is stable, so the
population's response to a given history keeps changing, and attendance never
settles.

**Package**

Each agent carries a list column of `N_STRATEGIES` candidate predictors, each a
vector of `MEMORY + 1` random weights (a baseline plus one weight per week of
lag). Three named functions do the arithmetic; each is a direct transcription of
one NetLogo reporter or procedure, named the same way:

```r
MEMORY <- 5; N_STRATEGIES <- 10; THRESHOLD <- 60; N_AGENTS <- 100

# to-report random-strategy
random_strategy <- function() runif(MEMORY + 1, -1, 1)

# to-report predict-attendance [strategy subhistory]
predict_attendance <- function(strategy, subhistory) {
  100 * strategy[1] + sum(strategy[-1] * subhistory)
}

# how far off would ONE strategy have been, over the last MEMORY weeks?
score_strategy <- function(strategy, history) {
  total_error <- 0
  for (week in 1:MEMORY) {
    subhistory <- history[(week + 1):(week + MEMORY)]   # what this strategy could see, as of that week
    actual     <- history[week]                          # what really happened
    guess      <- predict_attendance(strategy, subhistory)
    miss       <- abs(actual - guess)
    total_error <- total_error + miss
  }
  total_error
}

# which of an agent's strategies has been least wrong lately? (to update-strategies)
best_strategy_of <- function(strategies, history) {
  scores <- numeric(length(strategies))
  for (s in seq_along(strategies)) {
    scores[s] <- score_strategy(strategies[[s]], history)
  }
  best_index <- which.min(scores)
  strategies[[best_index]]
}

farol <- abm_setup(
  agents = abm_agents(
    n = N_AGENTS,
    strategies = ~replicate(n, replicate(N_STRATEGIES, random_strategy(), simplify = FALSE),
                            simplify = FALSE),
    best       = ~lapply(strategies, function(s) s[[1]]),   # placeholder; tick 1 corrects it below
    prediction = 0,
    attend     = FALSE
  ),
  globals = list(history = list(sample(0:99, MEMORY * 2, replace = TRUE)), attendance = 0),
  seed = 1
)

go <- abm_go(
  abm_rules(best ~ {                      # update-strategies, run first so tick 1's placeholder
    out <- vector("list", n())            # never gets used to make a real decision
    for (i in seq_len(n())) out[[i]] <- best_strategy_of(strategies[[i]], history[[1]])
    out
  }),
  abm_rules(prediction ~ {
    out <- numeric(n())
    for (i in seq_len(n())) out[i] <- predict_attendance(best[[i]], history[[1]][1:MEMORY])
    out
  }),
  abm_rules(attend ~ prediction <= THRESHOLD),
  abm_global(attendance ~ sum(attend),
             history    ~ list(c(attendance, head(history[[1]], -1))))
)

result <- abm_run(farol, go, ticks = 300)
```

**Result.** Last 150 of 300 ticks: attendance mean 56.3, sd 7.6, range 41–71, 31
distinct levels, no repeating period out to lag 6. Attendance hovers around the
capacity of 60 and keeps moving rather than settling, which is Arthur's result.

*Needed nothing new, and the entry it was filed under was wrong.* This model
was first written as seventy scalar columns and five blocks of
`rlang::new_formula()` scaffolding, and cited as proof that an agent could not
hold a set. Model 41 found the list column and this one was rewritten around
it — first onto an exponentially-decayed running error per strategy, which is
not what NetLogo's `update-strategies` actually computes, and now onto the
version above, which re-scores every strategy from scratch each tick against a
fixed `MEMORY`-week backtest, with no memory of any earlier tick's scores. The
two scoring rules give quantitatively different attendance series (the decayed
version has less variance, since it never fully forgets a strategy's older
performance); the version above is the one that matches the source.

*Two simplifications from a literal NetLogo transcription, both checked to cost
nothing.* `best_strategy_of()` scores every strategy independently and takes
`which.min()`, rather than NetLogo's running "keep whichever is currently
lowest" comparison; the two differ only in which strategy wins an exact tie,
and continuous random weights make an exact tie a probability-zero event.
`best`'s placeholder at setup is never backtested against the true starting
history the way NetLogo's `create-turtles` does by calling `update-strategies`
once — instead the update step runs first in `go`, so the placeholder is
corrected before anything ever reads it for a real decision, which is
equivalent by construction and confirmed to reproduce the same output.

**Replication**

![14. El Farol with inductive agents (Arthur 1994)](figures/14-el-farol-with-inductive-agents.png)

**Reproduce:** [`14-el-farol-with-inductive-agents.R`](scripts/14-el-farol-with-inductive-agents.R)

---

← [13. Bank Reserves](13-bank-reserves.md) · [all models](README.md) · [15. Ethnocentrism, Hammond & Axelrod](15-ethnocentrism-hammond-axelrod.md) →
