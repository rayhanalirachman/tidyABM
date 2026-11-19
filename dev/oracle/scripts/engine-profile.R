# R_PROFILE_USER for oracle runs under another engine: scripts/verify.sh and
# scripts/models.sh point R at this file when TIDYABM_ENGINE is set (r, auto, rust
# or both), so every Rscript they start runs its models under that engine.
if (nzchar(Sys.getenv("TIDYABM_ENGINE"))) options(tidyABM.engine = Sys.getenv("TIDYABM_ENGINE"))
