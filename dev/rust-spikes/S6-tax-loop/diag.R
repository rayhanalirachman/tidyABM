# S6 diagnosis: which neighbour order reproduces the engine?
args <- commandArgs(TRUE)
suppressMessages(pkgload::load_all(Sys.getenv("TIDYABM_PKG"), quiet = TRUE))
library(rspike, lib.loc = args[1])
src <- readLines("s6.R"); eval(parse(text = src[seq_len(grep("^cat\\(sprintf\\(\"runs", src) - 1)]))
try_order <- function(nbl, label) {
  off <- c(0L, cumsum(lengths(nbl))); idx <- as.integer(unlist(nbl)) - 1L
  got <- unlist(lapply(seq_len(nrow(runs)), function(r) { set.seed(seeds[[r]])
    rs_tax_run(typ, a$income, a$risk_aversion, off, idx, runs$audit_rate[r], 40L) }))
  d <- got != want
  cat(sprintf("%-28s identical %-5s differing values %4d / %d, max |diff| %.3g, first at run %s tick %s\n", label,
      identical(got, want), sum(d), length(d), max(abs(got - want)),
      if (any(d)) (which(d)[1] - 1) %/% 40 + 1 else NA, if (any(d)) (which(d)[1] - 1) %% 40 + 1 else NA))
}
try_order(nb, "lattice$nb as given")
try_order(lapply(nb, sort), "sorted ids")
try_order(lapply(nb, rev), "reversed")
e <- economy$edges; print(head(e, 3))
f <- c(e[[1]], e[[2]]); t <- c(e[[2]], e[[1]])
try_order(split(t, factor(f, levels = 1:441)), "edges (from,to) then (to,from)")
o <- order(f, seq_along(f)); try_order(split(t[o], factor(f[o], levels = 1:441)), "edges stable by from")
st <- tidyABM:::neighbour_table
cat("neighbour cache in run state? fields of model:", paste(names(economy), collapse = ", "), "\n")
