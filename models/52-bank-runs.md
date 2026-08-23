# 52. Bank runs and the sequential service constraint (Diamond & Dybvig 1983)

**Concept**

- Setup: 200 depositors, each with a place in the queue and a private threshold
  for panicking; the bank holds enough liquid assets to pay 1.2 to 41.7% of
  them
- Go: each day a shock makes some fraction of depositors withdraw regardless;
  the rest withdraw if they believe the till will be empty when they reach it;
  depositors are served **in queue order** until the money runs out, and each
  of them sees how much was left when their turn came
- Output: who runs, and where in the queue they are

**Package**

```r
n <- 200; liquid <- 0.5; r1 <- 1.2; memory <- 0.5; impatient <- 0.1

m <- abm_setup(
  agents  = abm_agents(n = n, pos = ~sample(n), theta = ~runif(n),
                       belief = 0, ran = FALSE, paid = 0, dry = FALSE),
  globals = list(till = liquid * n, empty = 0),
  seed    = 1)

go <- abm_go(
  abm_global(till ~ liquid * n()),
  abm_sequential(
    ran    ~ runif(1) < impatient | belief > theta,
    paid   ~ if_else(ran, pmin(r1, till), 0),
    till   ~ till - paid,
    dry    ~ till < r1,
    belief ~ memory * belief + (1 - memory) * as.numeric(dry),
    .order = pos),
  abm_global(empty ~ mean(ran))
)

result <- abm_run(m, go, ticks = 50, seed = 1)
```

**Result** (200 depositors, 50 days, till = 0.5 N, r₁ = 1.2, 5 seeds)

| order | shock | run rate | front quarter | 2nd | 3rd | back quarter |
|---|---|---|---|---|---|---|
| queue | 0.30 | 0.306 | 0.308 | 0.294 | 0.324 | 0.297 |
| shuffled | 0.30 | 0.305 | 0.295 | 0.313 | 0.309 | 0.302 |
| queue | 0.36 | 0.370 | 0.366 | 0.353 | 0.395 | 0.367 |
| shuffled | 0.36 | 0.363 | 0.353 | 0.374 | 0.368 | 0.357 |
| queue | 0.44 | **0.507** | 0.435 | 0.427 | 0.462 | **0.705** |
| shuffled | 0.44 | **0.684** | 0.660 | 0.713 | 0.641 | 0.722 |
| queue | 0.52 | **0.633** | 0.511 | 0.514 | 0.548 | **0.961** |
| shuffled | 0.52 | **0.755** | 0.727 | 0.767 | 0.725 | 0.802 |

Below the critical withdrawal rate the two halves of the table are the same and
both are flat. The till never empties, so nobody ever sees it empty, so nobody
learns anything, and the run rate is just the shock. Above it they come apart.
With a fixed queue the panic localises: the back quarter runs at 0.705 while
the front quarter runs at 0.435, and at the larger shock the back quarter runs
almost every day. Being late in the line is the whole of what those depositors
know, and it is enough.

The shuffled column is the same model with the sequential service constraint
removed — everybody is served in a fresh random order each day. The panic is
then *general* rather than local, because everyone gets to the empty till
sometimes, and the total run rate is a third higher (0.684 against 0.507). A
bank that serves its customers in an unpredictable order does not spread the
risk out; it spreads the fear out, and ends up paying more of it. Diamond and
Dybvig's sequential service constraint is usually discussed as a technical
restriction on the contract. Here it is the thing that decides whether a run is
a property of some depositors or of all of them.

*Part 7 made this model forty-four times faster and changed nothing about it.*
`abm_sequential()` was a `dplyr::mutate()` on a one-row tibble per agent per
rule, plus a whole-column write per assignment. At 200 depositors × 50 days —
50,000 agent-rules — this script took 77.6 s per run. It now evaluates its rules
against a plain data mask built from the agent's scalars and holds the group's
columns as bare vectors for the duration of the loop, and the same run takes
1.75 s. The result is bit-identical: the run rate at the 0.44 shock is 0.507,
which is the number in the table above. The lesson is worth stating, because it
is about the design rather than about this model: the tidy-data interface is what
the *model* is written in, and it does not have to be what the engine runs on.

*Forced `.order =` on `abm_sequential()`. The step existed because order
sometimes matters and a shuffle is the honest way to say "in no particular
order". This is the model where the order is not arbitrary: a queue position is
a piece of the agent's state that determines what it can learn, and reshuffling
it every day is a different model with a different answer, as the table shows.
One argument, and the two rows underneath every shock are the comparison it
makes possible.*

*It did not need `abm_tell()`. A till that empties is a global that depletes,
which is what `abm_sequential()` was built for in model 13 — each depositor's
write is visible to everyone behind it. What is new is only that "behind" now
means something.*

*The model is Diamond and Dybvig's mechanism, not their contract. There is no
second period and no optimal deposit contract here; `r₁ = 1.2` and a liquid
reserve of half the deposits stand in for the demand-deposit contract, and the
belief rule — an exponential average of "was the till empty when I got there" —
is a learning story the original does not have, and is what makes the queue
position informative rather than merely unlucky.*

---

**Reproduce:** [`m52_bankrun.R`](scripts/m52_bankrun.R)

← [51. Imitation dynamics of vaccination](51-vaccination-imitation.md) · [all models](README.md) · [53. Random copying and the neutral model](53-neutral-model.md) →
