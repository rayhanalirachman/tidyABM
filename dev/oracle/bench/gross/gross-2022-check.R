# tidyABM model vs base-R transcription of the MATLAB, same seeds. Both draw the households'
# firm choices with the same sample.int() call, so the paths should agree to the last bit.
#   Rscript sandbox/gross-2022-beautiful-cycles/gross-2022-check.R [seeds=3] [T=1000] [F=200]

args  <- commandArgs(trailingOnly = TRUE)
seeds <- seq_len(as.integer(if (length(args) >= 1) args[1] else 3))
T_    <- as.integer(if (length(args) >= 2) args[2] else 1000)
F_    <- as.integer(if (length(args) >= 3) args[3] else 200)
dir_  <- {a <- grep("^--file=", commandArgs(), value = TRUE)
          if (length(a)) dirname(gsub("~+~", " ", sub("^--file=", "", a[1]), fixed = TRUE)) else "."}  # Rscript encodes spaces as ~+~
source(file.path(dir_, "gross-2022.R"))
source(file.path(dir_, "gross-2022-reference.R"))

burn <- 100
kurt <- function(x) mean((x - mean(x))^4) / mean((x - mean(x))^2)^2 - 3
# cycle length: strongest spectral peak of unemployment, 50-400 periods
cycle_len <- function(x) {
  s <- stats::spec.pgram(x - mean(x), taper = 0.1, plot = FALSE)
  k <- 1 / s$freq >= 50 & 1 / s$freq <= 400
  round(1 / s$freq[k][which.max(s$spec[k])])
}
stats_of <- function(o) {
  o <- o[o$tick > burn, ]
  c(unemp = mean(o$unemp), unemp_max = max(o$unemp), unemp_kurt = kurt(o$unemp),
    debt_gdp = mean(o$debt_gdp), profit_sh = mean(o$profit_share),
    def_per_t = mean(o$ndef), share_def_t = mean(o$ndef > 0), rate = mean(o$iL),
    cycle = cycle_len(o$unemp),
    cor_debt_unemp = cor(o$debt_gdp, o$unemp))
}
series <- c("gdp", "unemp", "bL", "bNW", "bDEP", "ndef", "iL", "div", "pm", "wm", "wages")

res <- list()
for (s in seeds) {
  t0 <- Sys.time()
  mine <- run_gross(s, T = T_, F = F_)
  t1 <- Sys.time()
  ref  <- ref_gross(s, T = T_, F = F_)
  same <- sapply(series, function(v) identical(mine[[v]], ref[[v]]))
  cat(sprintf("seed %d: tidyABM %.0fs, reference %.0fs, checks passed (largest SFC residual %.1e); paths identical in %d of %d series%s\n",
              s, as.numeric(t1 - t0, units = "secs"), as.numeric(Sys.time() - t1, units = "secs"),
              max(mine$sfc_bank, mine$sfc_ledger, mine$sfc_firm), sum(same), length(same),
              if (all(same)) "" else paste0(" (differ: ", paste(series[!same], collapse = ", "), ")")))
  res[[length(res) + 1]] <- data.frame(seed = s, impl = "tidyABM", t(stats_of(mine)))
  res[[length(res) + 1]] <- data.frame(seed = s, impl = "reference", t(stats_of(ref)))
}
res <- do.call(rbind, res)
print(res, digits = 3, row.names = FALSE)
cat("\nmean over seeds\n")
print(aggregate(. ~ impl, res[, -1], mean), digits = 3, row.names = FALSE)
