# middle path: same plain-R loop, but case_when / if_else swapped for lean base-R versions
fast_if_else <- function(condition, true, false) {
  n <- length(condition); out <- rep_len(false, n); i <- which(condition)
  out[i] <- if (length(true) == 1L) true else true[i]
  out[is.na(condition)] <- NA; out
}
fast_case_when <- function(...) {
  fs <- list(...); env <- parent.frame(); out <- NULL; done <- NULL
  for (f in fs) {
    cond <- eval(f[[2]], env); val <- eval(f[[3]], env)
    if (is.null(out)) { n <- length(cond); out <- rep(val[NA_integer_][1], n); done <- logical(n) }
    i <- which(cond & !done)
    if (length(i)) { out[i] <- if (length(val) == 1L) val else val[i]; done[i] <- TRUE }
  }
  out
}
src <- readLines("../rustq/exp-tax.R")
src <- src[seq_len(grep("^cpu <- ", src))]
eval(parse(text = src))
slow <- plainR
case_when <- fast_case_when; if_else <- fast_if_else
environment(plainR) <- globalenv()
for (i in 1:3) { a1 <- cpu(plainR); cat(sprintf("plain R, lean case_when/if_else: %.3f s\n", a1$s)) }
rm(case_when, if_else); b <- cpu(plainR); cat(sprintf("plain R, dplyr: %.3f s; identical results: %s\n", b$s, identical(a1$v, b$v)))
# does a different neighbour order explain the 1-ulp gap to the engine?
want <- ref$mean_gross_tax_rate[ref$tick > 0]
e <- economy$edges; f2 <- c(e[[1]], e[[2]]); t2 <- c(e[[2]], e[[1]]); o <- order(f2)
for (nm in c("edges", "edges-rev")) {
  tt <- if (nm == "edges") t2[o] else c(e[[1]], e[[2]])[order(c(e[[2]], e[[1]]))]
  nbm <- matrix(tt, 4) - 1L; storage.mode(nbm) <- "integer"
  cat(nm, "order: native identical to tidyABM:", identical(native(), want), "\n")
}
