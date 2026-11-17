# usage: TIDYABM_PKG=<pkgdir> Rscript gross.R <out.rds>
dir_ <- dirname(gsub("~+~", " ", sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]), fixed = TRUE))  # Rscript encodes spaces as ~+~
source(file.path(dir_, "gross", "gross-2022.R"))
p <- proc.time()
o <- run_gross(seed = 1, T = 1000)
d <- proc.time() - p
cat(sprintf("tidyABM run_gross: real %.1f s, cpu %.1f s\n", d[["elapsed"]], d[["user.self"]] + d[["sys.self"]]))
saveRDS(as.data.frame(o), commandArgs(TRUE)[1])
