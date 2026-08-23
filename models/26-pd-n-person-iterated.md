# 26. PD N-Person Iterated (NetLogo Social Science)

**Concept**

- Setup: N agents, each with one of six fixed strategies, random, cooperate,
  defect, tit-for-tat, unforgiving, unknown
- Go: pair off, play one round of PD (T=5, R=3, P=1, S=0), remember what **this
  particular opponent** did
- Output: defect beats unconditional cooperation; tit-for-tat ties with
  cooperators at 3; and against defectors tit-for-tat starts behind and overtakes
  as it learns each individual defector

**NetLogo**

```netlogo
to tit-for-tat
  set partner-defected? item ([who] of partner) partner-history
  ifelse partner-defected? [ set defect-now? true ] [ set defect-now? false ]
end
to unforgiving-history-update
  if partner-defected? [ set partner-history
    (replace-item ([who] of partner) partner-history true) ]   ;; latches forever
end
```

**Package**

```r
f <- function(lhs, rhs) rlang::new_formula(str2lang(lhs), str2lang(rhs))

# one memory column per possible opponent
hist <- setNames(rep(list(FALSE), N), paste0("h", 1:N))
pop <- abm_setup(agents = do.call(abm_agents, c(
  list(n = N, strategy = strategies, score = 0, games = 0, defect_now = FALSE), hist)))

recall <- f("remembered", paste0(
  "case_when(", paste(sprintf(".partner == %d ~ h%d", 1:N, 1:N), collapse = ", "),
  ", TRUE ~ FALSE)"))

update <- lapply(1:N, function(k) f(paste0("h", k), sprintf(
  "case_when(.partner != %d ~ h%d,
             strategy == 'unforgiving' ~ h%d | partner_defect_now,
             strategy == 'tit-for-tat' ~ partner_defect_now,
             TRUE ~ h%d)", k, k, k, k)))

go <- abm_go(
  abm_match(pair = "random"),
  abm_rules(recall),
  abm_rules(defect_now ~ case_when(
    strategy == "defect"    ~ TRUE,
    strategy == "cooperate" ~ FALSE,
    strategy == "random"    ~ runif(n()) < 0.5,
    TRUE                    ~ remembered)),
  abm_rules(payoff ~ case_when(
    !defect_now & !partner_defect_now ~ 3,
    !defect_now &  partner_defect_now ~ 0,
     defect_now & !partner_defect_now ~ 5,
    TRUE                              ~ 1)),
  abm_rules(score ~ score + payoff, games ~ games + 1),
  do.call(abm_rules, update)
)

result <- abm_run(pop, go, ticks = 400, seed = 5)
```

**Result** (N = 24, average payoff = cumulative score / cumulative games):

| matchup | outcome |
|---|---|
| cooperate vs defect | defect 3.07, cooperate 1.45 |
| tit-for-tat vs cooperate | both exactly 3.00 |
| tit-for-tat vs defect | tick 5: defect 2.33, TFT 1.87 · tick 20: 2.40 / 1.63 · tick 100: 1.48 / **1.83** · tick 400: 1.12 / **1.95** |

*The crossover is the model's signature curve and it comes out cleanly.*

*This is the model that shows where the grammar hurts. Per-opponent memory is a
**vector per agent**, and the only way to hold one is a column per element, N
columns for N possible opponents, so N² cells. It is workable at N = 24 with
`do.call()` and hopeless at NetLogo's default of 60. This is the same gap El Farol
hit from the other direction, and it is the one thing on this list that a future
version of the package should actually fix.*

*One honest difference from NetLogo: there, agents wander a 441-patch world and
meet sparsely, so opponents recur rarely and defect usually posts the best average.
Here everyone is paired every tick out of 24, so each pair meets roughly every
23 ticks and the retaliatory strategies get enough encounters to learn. In the
all-six run, unforgiving (2.64) and tit-for-tat (2.52) beat defect (2.28). That
difference **is** the game-theoretic point, retaliation pays when re-encounters
are frequent, but it means the two implementations are answering slightly
different questions.*

---

← [25. Axelrod's cultural dissemination](25-axelrod-s-cultural-dissemination.md) · [all models](README.md) · [27. Threshold model of collective behaviour](27-threshold-model-of-collective-behaviour.md) →
