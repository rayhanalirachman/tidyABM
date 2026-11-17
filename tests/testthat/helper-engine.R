# Run the suite under the engine named by the TIDYABM_ENGINE environment variable
# (CI sets it; unset means the default, the R engine).
if (nzchar(Sys.getenv("TIDYABM_ENGINE"))) {
  options(tidyABM.engine = Sys.getenv("TIDYABM_ENGINE"))
}
