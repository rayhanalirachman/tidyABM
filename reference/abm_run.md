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
abm_run(model, go, ticks, seed = NULL, record = "all")
```

## Arguments

- model:

  An `abm_model` from
  [`abm_setup()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_setup.md).

- go:

  An `abm_go` sequence.

- ticks:

  Number of ticks to run.

- seed:

  Optional integer seed for the run. Set locally, so the caller's random
  state is left untouched. See the details above on why a random
  starting population also needs
  [`abm_setup()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_setup.md)'s
  `seed`.

- record:

  Which ticks' populations to keep. `"all"` (the default) keeps every
  one; a positive whole number keeps every `record`-th tick, plus tick 0
  and the last; `"final"` keeps only the last; `"globals"` keeps none.
  Globals are recorded every tick whatever this says, since they are one
  row each. A model whose population grows needs this: recording every
  agent of every tick is what makes such a run die of memory rather than
  merely take a while.

## Value

An `abm_result`: a tibble of one row per agent per tick, carrying the
run's globals and final network as attributes.

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

## Examples

``` r
economy <- abm_setup(agents = abm_agents(n = 50, money = 100))
go <- abm_go(
  abm_match(pair = "random", role = list(giver = money > 0, receiver = TRUE)),
  abm_rules(money ~ if_else(.role == "giver", money - 1, money + 1))
)
result <- abm_run(economy, go, ticks = 10, seed = 1)
result
#> <abm_result> 10 ticks, 50 agents seen, 550 rows
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
```
