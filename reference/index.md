# Package index

## The three parts

Every model is these three functions, written as three statements: the
world, the tick, the run.

- [`abm_setup()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_setup.md)
  : Set up a model
- [`abm_go()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_go.md)
  : Declare what happens each tick
- [`abm_run()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_run.md)
  : Run a model

## Declaring the world

The specifications you hand to
[`abm_setup()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_setup.md).

- [`abm_agents()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_agents.md)
  : Declare a group of agents
- [`abm_network()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_network.md)
  : Declare a persistent network between agents

## Steps — matching

Who interacts with whom this tick.

- [`abm_match()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_match.md)
  : Match agents into pairs or groups

## Steps — updating agents

- [`abm_rules()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_rules.md)
  : Update agent columns
- [`abm_sequential()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_sequential.md)
  : Update agent columns one agent at a time
- [`abm_global()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_global.md)
  : Update a shared, population-level value
- [`abm_neighbours()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_neighbours.md)
  : Summarise each agent's neighbourhood
- [`abm_tell()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_tell.md)
  : Write a value into another agent's row

## Steps — changing the population

- [`abm_birth()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_birth.md)
  : Add agents
- [`abm_death()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_death.md)
  : Remove agents

## Steps — changing the network

- [`abm_link()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_link.md)
  : Add edges between matched agents
- [`abm_unlink()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_unlink.md)
  : Remove edges between matched agents
- [`abm_draw()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_draw.md)
  : Attach a value to every edge, visible from both ends

## Steps — control flow

- [`abm_repeat()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_repeat.md)
  : Repeat a block of steps until a condition holds

## Working with results

- [`abm_globals()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_globals.md)
  : Global values recorded during a run
- [`abm_edges()`](https://rayhanalirachman.github.io/tidyABM/reference/abm_edges.md)
  : The network at the end of a run
- [`n_agents()`](https://rayhanalirachman.github.io/tidyABM/reference/n_agents.md)
  : Total number of agents in a model
