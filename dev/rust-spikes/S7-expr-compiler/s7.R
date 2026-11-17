# Spike S7: expression compiler prototype. (1) compile the tax model's rules to IR and
# run the full 150-run sweep with the Rust evaluator for every rule; (2) cost per rule,
# Rust vs R; (3) property test: random expressions over random columns with
# NA/NaN/Inf, R vs Rust, identical() on values, warnings and .Random.seed.
# usage: TIDYABM_PKG=<pkgdir> Rscript s7.R <spikes-lib> <golden tax.rds> [cases=3000]
args <- commandArgs(TRUE)
suppressMessages(pkgload::load_all(Sys.getenv("TIDYABM_PKG"), quiet = TRUE))
library(rspike, lib.loc = args[1])
dir_ <- dirname(gsub("~+~", " ", sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]), fixed = TRUE))
source(file.path(dir_, "ir.R"))
ncases <- if (length(args) >= 3) as.integer(args[3]) else 3000L

# ---- (1) the tax model with every rule compiled ----------------------------------
src <- readLines(file.path(dir_, "..", "..", "oracle", "bench", "tax.R"))
eval(parse(text = src[grep("^economy <- abm_setup", src):(grep("^p <- proc.time", src) - 1)]))
rules <- list()
for (s in year$steps) for (r in s$rules) rules[[r$target]] <- r$quo
cat("rules in the go block:", paste(names(rules), collapse = ", "), "\n")
progs <- list(); for (nm in c("declared", "audited", "penalty_due", "believed_audit_chance", "declared_share")) {
  q <- rules[[nm]]
  progs[[nm]] <- Program$new(compile_ir(rlang::quo_get_expr(q), rlang::quo_get_env(q)))
}
for (nm in setdiff(names(rules), names(progs))) {
  r <- tryCatch(compile_ir(rlang::quo_get_expr(rules[[nm]]), rlang::quo_get_env(rules[[nm]])), ir_fallback = function(e) e)
  cat(sprintf("  %s: %s\n", nm, if (inherits(r, "ir_fallback")) paste("R path:", conditionMessage(r)) else "compiles"))
}
a <- economy$groups$agents; n <- nrow(a)
e <- economy$edges; f <- c(e[[1]], e[[2]]); t <- c(e[[2]], e[[1]])
nbm <- do.call(rbind, split(t, factor(f, levels = seq_len(n))))      # n x 4, edge order
nb_mean <- function(x) {                                            # nb_fold(): rowsum order + 2nd pass
  s <- 0; for (j in seq_len(ncol(nbm))) s <- s + x[nbm[, j]]
  out <- s / ncol(nbm); r <- 0; for (j in seq_len(ncol(nbm))) r <- r + (x[nbm[, j]] - out)
  out + r / ncol(nbm)
}
golden <- readRDS(args[2]); runs <- unique(golden[, c(".run", ".rep", "audit_rate")])
seeds <- unlist(tidyABM:::run_seeds(1, nrow(runs))); want <- golden$mean_gross_tax_rate[golden$tick > 0]
sweep <- function(eval_rule) unlist(lapply(seq_len(nrow(runs)), function(r) {
  set.seed(seeds[[r]])
  g <- list(tax_rate = 0.30, penalty_rate = 0.45, audit_rate = runs$audit_rate[r])
  v <- c(as.list(a[c("type", "income", "risk_aversion", "believed_audit_chance", "declared_share",
                     "declared", "penalty_due", "audited")]), g)
  out <- numeric(40)
  for (tk in 1:40) {
    v$neighbours_share <- nb_mean(v$declared_share)
    v$declared <- eval_rule("declared", v)
    v$audited <- eval_rule("audited", v)
    nw <- lapply(c("penalty_due", "believed_audit_chance", "declared_share"), eval_rule, v = v)  # start-of-step reads
    v$penalty_due <- nw[[1]]; v$believed_audit_chance <- nw[[2]]; v$declared_share <- nw[[3]]
    out[tk] <- v$tax_rate * sum(v$declared) / sum(v$income)
  }
  out
}))
rust_rule <- function(nm, v) progs[[nm]]$eval(v, n)
mask_env <- new.env(parent = asNamespace("dplyr")); assign("n", function() n, mask_env)
r_rule <- function(nm, v) eval(rlang::quo_get_expr(rules[[nm]]), v, mask_env)
got <- sweep(rust_rule)
cat("tax sweep, all five rules evaluated by the Rust IR evaluator, identical to engine:", identical(got, want), "\n")
got_r <- sweep(r_rule)
cat("same harness with the rules evaluated by R/dplyr (control), identical to engine:", identical(got_r, want), "\n")

# ---- (2) cost per rule at n = 441 (state of tick 1, run 1) -----------------------
set.seed(1)
v <- c(as.list(a), list(tax_rate = 0.3, penalty_rate = 0.45, audit_rate = 0.02)); v$neighbours_share <- nb_mean(v$declared_share)
v$audited <- runif(n) < 0.5
per <- function(fun, k = 2000) { t <- min(replicate(5, system.time(for (i in seq_len(k)) fun())[["user.self"]])); 1e6 * t / k }
vv <- v[c("type", "income", "risk_aversion", "believed_audit_chance", "declared_share", "declared", "penalty_due", "audited", "neighbours_share", "tax_rate", "penalty_rate", "audit_rate")]
cat(sprintf("%-24s %10s %10s %8s\n", "rule (n = 441)", "R us", "Rust us", "ratio"))
for (nm in names(progs)) {
  tr <- per(function() r_rule(nm, vv)); tu <- per(function() rust_rule(nm, vv))
  cat(sprintf("%-24s %10.1f %10.1f %8.1f\n", nm, tr, tu, tr / tu))
}

# ---- (3) property test ------------------------------------------------------------
set.seed(20261117)
N <- 40L
special <- c(NA, NaN, Inf, -Inf, 0, -0, 1e300, -1e-300, 1, -1)
mkd <- function() { x <- rnorm(N) * 10^sample(-3:3, N, TRUE); x[sample(N, 8)] <- sample(special, 8, TRUE); x }
gen_d <- function(d) {
  if (d <= 0 || runif(1) < 0.25) return(if (runif(1) < 0.7) as.name(sample(c("d1", "d2", "d3"), 1))
                                         else sample(c(0, 1, -1.5, 2, 1e-9, 100, 0.2), 1))
  switch(sample(c("arith", "arith", "neg", "exp", "log", "pm", "if_else", "case_when", "runif"), 1),
    arith = call(sample(c("+", "-", "*", "/"), 1), gen_d(d - 1), gen_d(d - 1)),
    neg = call("-", gen_d(d - 1)),
    exp = call("exp", gen_d(d - 1)),
    log = call("log", gen_d(d - 1)),
    pm = as.call(c(as.name(sample(c("pmin", "pmax"), 1)), lapply(seq_len(sample(2:3, 1)), function(i) gen_d(d - 1)))),
    if_else = call("if_else", gen_l(d - 1), gen_d(d - 1), gen_d(d - 1)),
    case_when = {
      k <- sample(1:3, 1); fs <- lapply(seq_len(k), function(i) call("~", gen_l(d - 1), gen_d(d - 1)))
      if (runif(1) < 0.5) fs <- c(fs, list(call("~", TRUE, gen_d(d - 1))))
      cl <- as.call(c(as.name("case_when"), fs))
      if (runif(1) < 0.3) cl$.default <- gen_d(d - 1)
      cl
    },
    runif = quote(runif(n())))
}
gen_l <- function(d) {
  if (d <= 0 || runif(1) < 0.25) return(switch(sample(c("col", "lit", "str"), 1, prob = c(.5, .2, .3)),
    col = quote(l1), lit = sample(list(TRUE, FALSE, NA), 1)[[1]], str = call(sample(c("==", "!="), 1), quote(s1), sample(c("a", "b"), 1))))
  switch(sample(c("cmp", "cmp", "and", "not"), 1),
    cmp = call(sample(c("<", ">", "<=", ">=", "==", "!="), 1), gen_d(d - 1), gen_d(d - 1)),
    and = call(sample(c("&", "|"), 1), gen_l(d - 1), gen_l(d - 1)),
    not = call("!", gen_l(d - 1)))
}
run_r <- function(ex, cols) {
  w <- character()
  val <- tryCatch(withCallingHandlers(eval(ex, cols, mask_env),
                    warning = function(c) { w <<- c(w, conditionMessage(c)); invokeRestart("muffleWarning") }),
                  error = function(e) structure(conditionMessage(e), class = "r_error"))
  list(val = val, warn = w)
}
tally <- c(identical = 0, both_fallback_or_error = 0, rust_fallback_r_ok = 0, compile_fallback = 0, MISMATCH = 0)
mism <- list(); n <- N
for (i in seq_len(ncases)) {
  ex <- if (runif(1) < 0.8) gen_d(4) else gen_l(4)
  cols <- list(d1 = mkd(), d2 = mkd(), d3 = mkd(), l1 = sample(c(TRUE, FALSE, NA), N, TRUE), s1 = sample(c("a", "b", NA), N, TRUE))
  ir <- tryCatch(compile_ir(ex, asNamespace("dplyr")), ir_fallback = function(e) NULL)
  if (is.null(ir)) { tally["compile_fallback"] <- tally["compile_fallback"] + 1; next }
  prog <- Program$new(ir)
  set.seed(i); r <- run_r(ex, cols); sr <- .Random.seed
  set.seed(i); u <- tryCatch(prog$eval(cols, N), error = function(e) structure(conditionMessage(e), class = "rust_error")); su <- .Random.seed
  r_bad <- inherits(r$val, "r_error") || length(r$warn) > 0
  u_bad <- inherits(u, "rust_error")
  if (!r_bad && !u_bad) {
    # R may return length 1 for a constant expression; the engine recycles to n
    rv <- if (length(r$val) == 1L) rep(r$val, N) else r$val
    if (identical(rv, u) && identical(sr, su)) tally["identical"] <- tally["identical"] + 1
    else { tally["MISMATCH"] <- tally["MISMATCH"] + 1; mism[[length(mism) + 1]] <- list(ex = ex, r = rv, u = u, seed_ok = identical(sr, su)) }
  } else if (r_bad && u_bad) tally["both_fallback_or_error"] <- tally["both_fallback_or_error"] + 1
  else if (!r_bad && u_bad) tally["rust_fallback_r_ok"] <- tally["rust_fallback_r_ok"] + 1
  else { tally["MISMATCH"] <- tally["MISMATCH"] + 1; mism[[length(mism) + 1]] <- list(ex = ex, r = r, u = u, note = "R warned/errored, Rust returned a value") }
}
cat("\nproperty test,", ncases, "random expressions over 40 rows with NA/NaN/Inf/-0:\n"); print(tally)
for (m in head(mism, 8)) { cat("MISMATCH:", deparse1(m$ex), "\n"); str(m[-1]) }
saveRDS(list(tally = tally, mismatches = mism), file.path(dir_, "results", "s7-property.rds"))
