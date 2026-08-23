# 47. Response thresholds and the division of labour (Bonabeau, Theraulaz & Deneubourg 1996)

**Concept**

- Setup: 100 workers, each idle or engaged in one of two tasks, each carrying a
  response threshold per task; each task carries a stimulus shared by the colony
- Go: a stimulus rises on its own and falls with the number of workers on that
  task; an idle worker takes on task *j* with probability
  `s_j² / (s_j² + θ_ij²)`; a busy worker drops what it is doing with
  probability `p`
- Output: how much of the colony works, and who does the working

**Package**

```r
n <- 100; delta <- 1; alpha <- 3; p <- 0.2
j <- 1:2                                   # two tasks
sym_th <- lapply(j, function(k) rlang::sym(paste0("theta_", k)))
sym_f  <- lapply(j, function(k) rlang::sym(paste0("fire_", k)))

# the stimuli are one global: a named vector with a key per task
pop <- abm_setup(
  agents  = do.call(abm_agents, c(
    list(n = n, task = 0L),
    setNames(lapply(j, function(k) ~sample(c(500, 500), n, replace = TRUE)),
             paste0("theta_", j)))),
  globals = list(s = c("1" = 0, "2" = 0)),
  seed    = 1)

# a worker carries one threshold per task, so the take-up rules are still
# assembled rather than written out
fire <- lapply(j, function(k) rlang::new_formula(sym_f[[k]], rlang::expr(
  task == 0L & runif(n()) < s[[!!as.character(k)]]^2 /
    (s[[!!as.character(k)]]^2 + (!!sym_th[[k]])^2))))

go <- abm_go(
  abm_rules(task ~ if_else(task > 0L & runif(n()) < p, 0L, task)),
  do.call(abm_rules, fire),
  abm_rules(rlang::new_formula(rlang::sym("task"), rlang::expr(
    if_else(task == 0L, choose_task(!!!sym_f), task)))),
  # one rule, whatever K is
  abm_global(s ~ max(0, s + delta - alpha * sum(task == .key) / n()), .by = j)
)

result <- abm_run(pop, go, ticks = 2000, seed = 1)
```

**Result** (δ = 1, α = 3, p = 0.2, N = 100, two tasks, 2000 ticks, 5 seeds)

| thresholds (θ drawn from) | on task 1 | on task 2 | stimulus s₁ |
|---|---|---|---|
| 500 / 500 | 0.332 | 0.333 | 212 |
| 50 / 5000 | 0.334 | 0.333 | 66 |

*Predicted active fraction per task: δ / α = 0.333.*

| θ on a task | fraction of time spent on it |
|---|---|
| 50 | 0.688 (task 1), 0.569 (task 2) |
| 5000 | 0.000, 0.000 |

The colony puts exactly δ/α of itself on each task, and it does so whether the
workers are identical or split into a responsive caste and a reluctant one. No
worker measures how much work there is. The stimulus does the measuring, and
the threshold rule reads it. That is the whole content of the model, and it is
why the fraction is the same in both rows: the *number* of workers on a task is
set by the stimulus balance, and the thresholds decide only *which* workers
they are.

Which they are is the second result. With one caste everybody works about a
third of the time. With two, the low-threshold workers are busy two thirds of
the time and the high-threshold ones essentially never, a division of labour
with no allocation, no signalling and no difference in the rule any worker is
following. The reserve caste is not idle by accident: it is what the colony
would call on if the stimulus ever rose far enough, and the equilibrium
stimulus is a third of the homogeneous colony's (66 against 212) precisely
because the responsive caste settles the work before it gets that high.

*Forced `abm_global(.by =)`, one round later.* This was the third model in a
row to want a global indexed by a category, a stimulus per task is a table, and
`abm_global()` wrote a name, and when it was first written the two tasks were
`s_1` and `s_2`, with the stimulus rules assembled by `rlang::new_formula()` and
`do.call()`. `.by` writes the table directly: the global is a named vector, the
rule is evaluated once per key with `.key` naming it, and `s` inside the rule
means *that key's* value, so the update reads exactly as the scalar version does.
The stimulus step is now one rule for any number of tasks, and the numbers above
are unchanged.

*Two details are worth recording. `.by = j` declares the index rather than
deriving it from an agent column, which matters here: a task nobody is working on
still has to have its stimulus rise, and it would not appear in an index derived
from the agents. And each key sees the* whole *population, `n()` is everybody,
not everybody on this task, because a colony-level stimulus balance is about the
colony.*

*What is left of the scaffolding is on the agent side. A worker carries one
threshold per task, so K tasks are still K columns and K take-up rules. That is a
different shape from the one `.by` closed, and it is the one the grammar still
does not say compactly.*

*Worth recording as a modelling trap rather than a grammar one: the take-up
rule has to be written so that a worker rolling against both tasks in the same
tick takes* one *of them, not the first one that fires. Testing the tasks in
order gives task 1 first refusal, and the asymmetry is invisible in the
aggregate, the total active fraction is still δ/α, but it shows up in the
per-task split, which is the number the model is about. `choose_task()` picks
uniformly among the tasks that fired.*

*And the stimulus converges slowly. The active fraction is at δ/α within a
couple of hundred ticks, but s₁ was still climbing at 2000 (212 against a fixed
point near 270) because ds/dt is proportional to the residual and the residual
is small long before the stimulus is settled. The table quotes what 2000 ticks
produced rather than the fixed point it is heading for.*

---

**Reproduce:** [`m47_thresholds.R`](scripts/m47_thresholds.R)

← [46. The Beer Distribution Game](46-beer-distribution-game.md) · [all models](README.md) · [48. A garbage can model of organizational choice](48-garbage-can-model.md) →
