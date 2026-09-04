# The tidyABM model corpus

Sixty-six published models, implemented in tidyABM, one page each, with the
concept, the working package code, the numbers the run produced and a figure of
the result. Every model here runs, every one has a reproduction script next to
it, and every one with a behavioural claim is pinned by a test in
`tests/testthat/`.

The corpus is in two halves, because the two halves ask different things of the
grammar.

- **[Non-spatial models](non-spatial/README.md)** — fifty-six models in
  attribute space or on an abstract network. This is the set the core grammar
  was designed against and then stress-tested with, and eighteen of the
  features in tidyABM exist because one of these models asked for it.
- **[Spatial models](spatial/README.md)** — ten models on a lattice. A lattice
  *is* a network here: `abm_network(type = "grid")` produces the same edge
  tibble every other network type produces, so there is no second medium and
  no patch-specific rule syntax.

Every model is written as the same three parts, `abm_setup()` then `abm_go()`
then `abm_run()`, kept as three separate statements, so the code on a page shows
the whole model rather than a fragment:

```r
world  <- abm_setup(...)                                # 1. who is in the model
go     <- abm_go(...)                                   # 2. what happens each tick
result <- abm_run(world, go, ticks = ..., seed = ...)   # 3. run it
```

## The shared documents

- **[The grammar](grammar.md)** — start here if you have not written a tidyABM
  model before.
- **[Open items](open-items.md)** — what the grammar still cannot say. The most
  useful page in here if you are extending the package rather than using it.
- **[What each stress test changed](what-changed.md)** — how the grammar got
  its current shape, round by round.
- **[Sources](sources.md)** — every citation in one table.

## Reproducing the numbers

Every table on every page came from a script under
[`non-spatial/scripts/`](non-spatial/scripts/README.md) or
[`spatial/scripts/`](spatial/scripts/README.md), run at the size and seed the
script names. Each script also writes the figure its page embeds.

```
Rscript models/non-spatial/scripts/43-zero-intelligence-traders.R
Rscript models/spatial/scripts/01-life.R
```

The scripts need `tidyABM` and `ggplot2` installed. The `testthat` cases run the
same models at reduced scale so the suite stays quick; the scripts are what
reproduce the published tables.
