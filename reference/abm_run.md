# Run a model

`abm_run()` is the last of the three functions a model is made of –
[`abm_setup()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_setup.md),
then
[`abm_go()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_go.md),
then `abm_run()`. It is the scheduler: it takes the model and the
behavioural block, replays the block `ticks` times, and records the
whole population after every tick.

## Usage

``` r
abm_run(
  model,
  go,
  ticks,
  params = NULL,
  reps = 1,
  measures = NULL,
  checks = NULL,
  seed = NULL,
  record = "all",
  progress = NULL
)
```

## Arguments

- model:

  An `abm_model` from
  [`abm_setup()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_setup.md).

- go:

  An `abm_go` sequence.

- ticks:

  Number of ticks to run.

- params:

  Optional named list of values overriding the model's `globals`, one
  run per combination. An entry holding several values is swept; a
  global that is itself a vector is held fixed by wrapping it in a
  [`list()`](https://rdrr.io/r/base/list.html), the same way a
  non-scalar global is written into the log. See *Many runs* below.

- reps:

  Number of replicates of each parameter combination. Replicates differ
  in what the run draws, not in the population they start from.

- measures:

  Optional named list of one-sided formulas, each evaluated over the
  whole population once per tick and recorded with
  [`abm_measures()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_measures.md).
  A measure cannot be read by any step. See *Measures* below.

- checks:

  Optional named list of one-sided formulas, evaluated like `measures`
  at tick 0 and after every tick, each of which must return a single
  `TRUE`. The first that does not stops the run. See *Checks* below.

- seed:

  Optional integer seed. Set locally, so the caller's random state is
  left untouched. With more than one run this seeds the experiment and a
  seed per run is derived from it, so the whole thing reproduces at
  once. See the details above on why a random starting population also
  needs
  [`abm_setup()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_setup.md)'s
  `seed`.

- record:

  Which ticks' populations to keep. `"all"` (the default) keeps every
  one; a positive whole number keeps every `record`-th tick, plus tick 0
  and the last; `"final"` keeps only the last; `"globals"` keeps none.
  Globals are recorded every tick whatever this says, since they are one
  row each. A model whose population grows needs this: recording every
  agent of every tick is what makes such a run die of memory rather than
  merely take a while. It applies to each run, so its cost multiplies by
  how many there are.

- progress:

  Whether to show a progress bar with an ETA while the run is going.
  `NULL` (the default) follows the session: a bar at an interactive
  console once the run has been going long enough to be worth reporting,
  and nothing while knitr, pkgdown or `R CMD check` is running the code,
  so it never turns up in a rendered page. `TRUE` forces it on from the
  first tick, `FALSE` off. The bar counts ticks for a single run and
  runs for many.

## Value

An `abm_result`: a tibble of one row per agent per tick, carrying the
run's globals, measures and final network as attributes. With several
agent groups the tibble holds the union of their columns, so a column
one group does not have is `NA` on the other group's rows.

## Details

The result is one long tibble, `tick`, `.id`, `.group`, then every agent
column, which is what you want for plotting and summarising. Tick 0 is
the state produced by
[`abm_setup()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_setup.md),
before any step has run, so a run of `n` ticks returns `n + 1`
snapshots. Global values are recorded alongside and are available with
[`abm_globals()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_globals.md).

Agent-based models are stochastic, so `seed` is a first-class argument
rather than something to arrange yourself: it makes the run reproducible
without touching the global random state.

Every tick's whole population is recorded by default, which is right for
a fixed population and wrong for a growing one – a run that ends with
fifty thousand agents has been keeping every one of them, every tick,
since the start. `record` says how much to keep.

It fixes the run, though, not the model. If the agents' starting columns
were drawn at random, they were drawn when
[`abm_setup()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_setup.md)
was called, and this seed comes too late to affect them. Seed both for
an experiment that reproduces end to end:

    m <- abm_setup(agents = abm_agents(n = 100, x = ~runif(n)), seed = 1)
    r <- abm_run(m, go, ticks = 100, seed = 1)

## Many runs

`params` and `reps` turn one call into several runs. `params` is a named
list whose entries override the model's `globals` before each run; an
entry holding more than one value is swept, one run per value, and
several entries give every combination of them. `reps` repeats each of
those combinations.

    abm_run(m, go, ticks = 500, params = list(d = c(0.1, 0.2, 0.3)), reps = 20,
            seed = 1)

is sixty runs. Their results come back stacked into one `abm_result`,
with `.run`, `.rep` and one column per parameter in front of the
ordinary columns, and
[`abm_globals()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_globals.md),
[`abm_measures()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_measures.md)
and
[`abm_edges()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_edges.md)
carry the same identifying columns. A single run is left exactly as it
was: no `.run`, no `.rep`, and the same columns the function has always
returned.

A parameter is a global, and only a global. `n`,
[`abm_network()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_network.md)'s
`degree` and a lattice's `dims` are fixed when
[`abm_setup()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_setup.md)
builds the world, so sweeping one of those still means one world per
value.

`reps` replicates the *run*, not the population. `abm_run()` is handed a
world that has already been built, so a starting column drawn with
`~runif(n)` was drawn once, before this function saw it, and every
replicate begins from that same population. What varies across
replicates is everything the run itself draws – who is matched with
whom, which agent acts first, what a rule samples. To vary the starting
population too, build a world per replicate.

## Measures

`measures` records a summary of the population once per tick, without
keeping the population. Each entry is a one-sided formula evaluated the
way an
[`abm_global()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_global.md)
right-hand side is – the globals are in scope,
[`n()`](https://dplyr.tidyverse.org/reference/context.html) is the
population size – and must collapse to one value.

    abm_run(m, go, ticks = 500, record = "globals",
            measures = list(gini = ~gini(money), broke = ~mean(money == 0)))

A measure is an observation, not model state, and the difference is not
cosmetic: writing the same summary with
[`abm_global()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_global.md)
would put it where every rule in the model can read it. A measure is
written to the table
[`abm_measures()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_measures.md)
returns and to nowhere else, so nothing in the model can see it.
Together with `record = "globals"` this is what makes a sweep
affordable: the whole trajectory of every run, at a few thousand rows
rather than a few million.

## Checks

`checks` is the same shape as `measures`, one-sided formulas over the
whole population with the globals in scope, but each must return a
single `TRUE`. They are evaluated at tick 0 and after every tick, and
the first one that returns `FALSE` or `NA` stops the run with an error
of class `tidyABM_check_failed` naming the check and the tick (the
condition carries them as `check` and `tick`). That is what an
accounting identity wants: a stock-flow consistent model that leaks
money should stop where it leaks, not finish and leave the leak to be
found in a plot.

    abm_run(m, go, ticks = 500, seed = 1,
            checks = list(money = ~abs(sum(m) - total) < 1e-9 * total))

The tolerance is part of the formula, because what counts as zero
depends on the scale of the ledger. A check is never recorded and no
step can read it, so a run with checks returns exactly what the same run
without them does. In a sweep the error also names the run, with its
parameters and the seed it ran on: a single run with that seed and those
`params` reproduces it.

## Examples

``` r
economy <- abm_setup(agents = abm_agents(n = 50, money = 100))
go <- abm_go(
  abm_match(pair = "random", role = list(giver = money > 0, receiver = TRUE)),
  abm_rules(money ~ if_else(.role == "giver", money - 1, money + 1))
)
result <- abm_run(economy, go, ticks = 10, seed = 1)
result
#> <abm_result> 10 ticks, 50 distinct agents, 550 rows
#> # A tibble: 550 × 4
#>     tick   .id .group money
#>    <int> <int> <chr>  <dbl>
#>  1     0     1 agents   100
#>  2     0     2 agents   100
#>  3     0     3 agents   100
#>  4     0     4 agents   100
#>  5     0     5 agents   100
#>  6     0     6 agents   100
#>  7     0     7 agents   100
#>  8     0     8 agents   100
#>  9     0     9 agents   100
#> 10     0    10 agents   100
#> # ℹ 540 more rows

# ten replicates, summarised as they go and keeping no populations
many <- abm_run(economy, go, ticks = 10, reps = 10, seed = 1,
                record = "globals",
                measures = list(richest = ~max(money)))
abm_measures(many)
#> # A tibble: 110 × 4
#>     .run  .rep  tick richest
#>    <int> <int> <int>   <dbl>
#>  1     1     1     0     100
#>  2     1     1     1     101
#>  3     1     1     2     102
#>  4     1     1     3     103
#>  5     1     1     4     104
#>  6     1     1     5     105
#>  7     1     1     6     106
#>  8     1     1     7     107
#>  9     1     1     8     106
#> 10     1     1     9     107
#> # ℹ 100 more rows
```
