# abm_run() checks the engine option and runs the R engine

    Code
      abm_run(m, go, ticks = 3, seed = 2)
    Condition
      Error in `abm_run()`:
      ! The Rust engine cannot run models yet.
      i Use `options(tidyABM.engine = "r")` (the default) or "auto".

---

    Code
      abm_run(m, go, ticks = 3, seed = 2)
    Condition
      Error in `abm_run()`:
      ! `options(tidyABM.engine = )` must be one of "auto", "r", "rust", or "both".
      x It is "fast".

