# 41. Minority Game (Challet & Zhang 1997)

**Concept**

- Setup: 101 agents, each holding S = 2 strategies. A strategy is a lookup
  table from the last `m` winning sides — 2^m of them — to a prediction.
- Go: everyone plays their currently best-scoring strategy; whoever ends up on
  the smaller side wins; every strategy is then scored on what it *would* have
  predicted, whether or not it was the one played.
- Output: the volatility of attendance, as a function of how much memory the
  agents have relative to how many of them there are

There is no equilibrium to settle into. If everyone predicts the same thing,
everyone is wrong. The agents are inductive because they cannot be anything
else.

**Package**

```r
minority_game <- function(n = 101, m = 5, s = 2, ticks = 2000, seed = 1) {
  n_states <- 2^m

  mg <- abm_setup(
    agents = abm_agents(
      n = n,
      # s strategies per agent, each a 0/1 vector of length 2^m
      strategies = ~lapply(seq_len(n),
                           function(i) matrix(sample(0:1, n_states * s, TRUE),
                                              nrow = n_states, ncol = s)),
      score      = ~lapply(seq_len(n), function(i) numeric(s)),
      action     = 0L
    ),
    globals = list(history = 1L, attendance = 0L),
    seed    = seed
  )

  go <- abm_go(
    abm_rules(action ~ vapply(seq_along(strategies), function(i) {
      as.integer(strategies[[i]][history, which.max(score[[i]])])
    }, integer(1))),

    abm_global(attendance ~ sum(action)),

    abm_rules(score ~ {
      winner <- as.integer(attendance < n() / 2)
      lapply(seq_along(score),
             function(i) score[[i]] +
               ifelse(strategies[[i]][history, ] == winner, 1, -1))
    }),

    abm_global(history ~ (bitwAnd(history - 1L, 2L^(m - 1) - 1L) * 2L +
                          as.integer(attendance < n() / 2)) + 1L)
  )

  g <- abm_globals(abm_run(mg, go, ticks = ticks, seed = seed))
  a <- 2 * g$attendance[g$tick > ticks / 2] - n
  stats::var(a) / n
}
```

**Result** (n = 101, second half of 2000 ticks)

| m | α = 2^m / N | σ²/N |
|---|---|---|
| 2 | 0.040 | 5.62 |
| 3 | 0.079 | 2.49 |
| 4 | 0.158 | 1.94 |
| 5 | 0.317 | **0.22** |
| 6 | 0.634 | 0.27 |
| 8 | 2.535 | 0.70 |
| 10 | 10.139 | 0.82 |

The curve is the one the minority-game literature is built on. Coin-flipping
agents would give σ²/N = 1 exactly. At large α the agents have so much memory
relative to their number that they never see the same signal twice and the curve
returns to 1 from below. At small α they all read the same short history, crowd
onto the same prediction, and do **far worse than chance** — five and a half
times worse at m = 2. In between, at α ≈ 0.3, they coordinate *better* than
chance without ever intending to, and the minimum sits right where the reported
critical value α_c ≈ 0.34 says it should.

*Needed nothing new, for the same reason the naming game did not: a list column
holds each agent's 2^m × S strategy table and `abm_rules()` indexes into it.
Between this and the naming game, "no set-valued agent state" comes off the Open
items list — the El Farol and PD N-Person models that motivated the entry were
written as seventy and n² scalar columns because nobody had tried a list column,
not because the grammar refused one.*

*The other thing it exercises is `n()` inside `abm_global()`, which used not to
work: globals were evaluated outside dplyr's mask, so `attendance < n() / 2`
failed in a global while the identical expression worked in a rule. That is now
fixed, and the fix is why the history update can be written in one line.*

---

**Reproduce:** [`m41_minority_game.R`](scripts/m41_minority_game.R)

← [40. Naming Game](40-naming-game.md) · [all models](README.md) · [42. Kirman's ants](42-kirmans-ants.md) →
