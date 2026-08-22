# tidyABM: Agent-Based Modelling with Tidy Data and a Declarative Grammar

A declarative grammar for writing agent-based models (ABMs) in R. Models
are described as data rather than as loops: a setup block declares the
agent population, an optional network and shared globals, and a
behavioural block declares an ordered sequence of typed steps (matching,
rules, births, deaths and global updates). Agents are stored as tibbles
and rules are written as 'dplyr'-style formulas, so a model reads as a
specification and its output is tidy data ready for analysis. The design
follows the block structure of 'NetLogo' (setup / go) while keeping the
interface idiomatic to the tidyverse.

## See also

Useful links:

- <https://github.com/rayhanalirachman/tidyABM>

- Report bugs at <https://github.com/rayhanalirachman/tidyABM/issues>

## Author

**Maintainer**: Rayhan Ali Rachman <rayhanalirachman@gmail.com>

Authors:

- Rayhan Ali Rachman <rayhanalirachman@gmail.com>
