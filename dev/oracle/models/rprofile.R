library <- function(p, ...) { p <- as.character(substitute(p)); if (p == "tidyABM") suppressMessages(pkgload::load_all(Sys.getenv("PKG"), quiet = TRUE)) else suppressPackageStartupMessages(base::library(p, character.only = TRUE)) }
set.seed(20261117)
# TIDYABM_ENGINE=both (or rust, auto) runs the scripts under that engine (Rust migration)
if (nzchar(Sys.getenv("TIDYABM_ENGINE"))) options(tidyABM.engine = Sys.getenv("TIDYABM_ENGINE"))
