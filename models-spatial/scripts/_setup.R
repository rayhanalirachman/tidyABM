# Shared by every script here. The package sources next door win over anything
# installed, so a script always exercises the working tree rather than a stale
# library copy.
local({
  root <- normalizePath(file.path("..", ".."), mustWork = FALSE)
  if (file.exists(file.path(root, "DESCRIPTION")) &&
      requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(root, quiet = TRUE)
  } else {
    library(tidyABM)
  }
})
