# tidyABM: Agent-Based Modelling with Tidy Data and a Declarative Grammar

A declarative grammar for writing agent-based models (ABMs) in R. Models
are described as data rather than as loops, in three parts:
'abm_setup()' declares the agent population, an optional network and
shared globals; 'abm_go()' declares an ordered sequence of typed steps
(matching, rules, births, deaths and global updates) replayed once per
tick; and 'abm_run()' takes the two and returns the result. Agents are
stored as tibbles and rules are written as 'dplyr'-style formulas, so a
model reads as a specification and its output is tidy data ready for
analysis. The design follows the block structure of 'NetLogo' (setup /
go) while keeping the interface idiomatic to the tidyverse.

## See also

Useful links:

- <https://github.com/rayhanalirachman/tidyABM>

- Report bugs at <https://github.com/rayhanalirachman/tidyABM/issues>

## Author

**Maintainer**: Rayhan Ali Rachman <rayhanalirachman@gmail.com>

Authors:

- Rayhan Ali Rachman <rayhanalirachman@gmail.com>
