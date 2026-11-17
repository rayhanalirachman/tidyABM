# Spike S3: boundary costs of an R-driven loop over a Rust store (savvy 0.11).
# usage: TIDYABM_PKG=<pkgdir> Rscript s3.R <spikes-lib> <outdir>
args <- commandArgs(TRUE)
library(rspike, lib.loc = args[1])
suppressMessages(pkgload::load_all(Sys.getenv("TIDYABM_PKG"), quiet = TRUE))
ns <- asNamespace("rspike")
call_impl <- function(name) get(name, envir = ns)   # the registered native symbol objects
noop_impl <- call_impl("savvy_rs_noop__impl"); col_impl <- call_impl("savvy_Store_col__impl")
set_impl <- call_impl("savvy_Store_set_col__impl"); step_impl <- call_impl("savvy_Store_step__impl")
chr_impl <- call_impl("savvy_Store_chr_col__impl")

# per-iteration time in microseconds, CPU, best of 5 repeats, empty loop subtracted
per <- function(expr, k) {
  e <- substitute(expr); env <- parent.frame()
  f <- eval(call("function", as.pairlist(alist()), call("{", call("for", quote(.i), call("seq_len", k), e))), env)
  f0 <- eval(call("function", as.pairlist(alist()), call("{", call("for", quote(.i), call("seq_len", k), NULL))), env)
  t <- min(replicate(5, system.time(f())[["user.self"]]))
  t0 <- min(replicate(5, system.time(f0())[["user.self"]]))
  1e6 * (t - t0) / k
}
rows <- list(); add <- function(what, n, us) { rows[[length(rows) + 1]] <<- data.frame(what = what, n = n, us = round(us, 3)); cat(sprintf("%-62s n=%-7s %9.3f us\n", what, n, us)) }

# 1. fixed cost of a call
rf <- function() NULL
add("R closure call (baseline)", NA, per(rf(), 1e6))
add("savvy no-op via generated R wrapper rs_noop()", NA, per(rs_noop(), 1e6))
add("savvy no-op via .Call(<native symbol>)", NA, per(.Call(noop_impl), 1e6))

# 2.-4. a 10-column store behind an external pointer
for (n in c(10L, 441L, 10000L, 100000L)) {
  st <- Store$new(n, 10L); p <- st$.ptr
  k <- if (n >= 1e5) 2e3 else 2e4
  x <- runif(n)
  add("copy-out 1 double column, method st$col(1L)", n, per(st$col(1L), k))
  add("copy-out 1 double column, .Call(col, ptr, 1L)", n, per(.Call(col_impl, p, 1L), k))
  add("copy-in 1 double column, .Call(set_col, ptr, 1L, x)", n, per(.Call(set_impl, p, 1L, x), k))
  add("character column materialised (3-string pool)", n, per(.Call(chr_impl, p), k))
  cache <- new.env(); cache$v <- .Call(chr_impl, p); cache$ver <- 1L; ver <- 1L
  add("character column from R-side mirror cache (version hit)", n, per(if (cache$ver == ver) cache$v else NULL, k))
  if (n %in% c(10L, 10000L)) {
    kk <- if (n == 10L) 2e4 else 2e3
    add("tibble round trip: 10 cols out -> new_tibble -> 10 cols in", n, per({
      tb <- tibble::new_tibble(lapply(1:10, function(j) .Call(col_impl, p, j)) |> stats::setNames(paste0("c", 1:10)), nrow = n)
      for (j in 1:10) .Call(set_impl, p, j, tb[[j]])
    }, kk))
  }
}

# 5. calling an R closure from Rust
t <- system.time(rs_call_closure(function() NULL, 1e5L))[["user.self"]]
add("R closure called from Rust (FunctionSexp$call), per call", NA, 1e6 * t / 1e5)

# 6. the R side of one step in a minimal R-driven loop:
# switch on the step class, a .Call into a Rust kernel holding the state, one
# withCallingHandlers for the run, per tick: tick bookkeeping, global_cells(), a counter.
steps <- list(structure(list(), class = "abm_rules"), structure(list(), class = "abm_global"),
              structure(list(), class = "abm_neighbours"), structure(list(), class = "abm_rules"))
loop <- function(p, ticks, rng) {
  state <- list(globals = list(tax_rate = 0.3, penalty_rate = 0.45, audit_rate = 0.02, mgtr = 0), tick = 0L)
  log <- vector("list", ticks + 1L); done <- 0L; at <- 0L
  withCallingHandlers({
    for (t in seq_len(ticks)) {
      state$tick <- t
      for (i in seq_along(steps)) {
        at <- i
        switch(class(steps[[i]])[[1]],
               abm_rules = .Call(step_impl, p, rng),
               abm_global = .Call(step_impl, p, rng),
               abm_neighbours = .Call(step_impl, p, rng),
               stop("unknown step"))
      }
      at <- 0L
      log[[t + 1L]] <- global_cells(state, t)
      done <- done + 1L
    }
  }, error = function(e) if (at != 0L) stop(e))
  log
}
for (n in c(10L, 441L)) {
  p <- Store$new(n, 10L)$.ptr
  ticks <- 20000L
  for (rng in c(FALSE, TRUE)) {
    gc(); g0 <- gc.time()[1]
    tt <- min(replicate(5, system.time(loop(p, ticks, rng))[["user.self"]]))
    g1 <- gc.time()[1]
    nsteps <- ticks * length(steps)
    # the kernel itself (inside Rust) for this n, measured on its own and subtracted
    kern <- per(.Call(step_impl, p, rng), 2e4) - per(.Call(noop_impl), 2e4)
    add(sprintf("R-driven loop: total per step (rng=%s, kernel incl.)", rng), n, 1e6 * tt / nsteps)
    add(sprintf("R-driven loop: R side per step excl. kernel work (rng=%s)", rng), n, 1e6 * tt / nsteps - kern)
    add(sprintf("   GC time during 5 loops (rng=%s), s", rng), n, g1 - g0)
  }
}
add("Get+PutRNGstate pair (difference rng=TRUE vs FALSE at n=10, per step)", 10,
    rows[[which(sapply(rows, function(r) r$what) == "R-driven loop: total per step (rng=TRUE, kernel incl.)" & sapply(rows, function(r) r$n) == 10)]]$us -
    rows[[which(sapply(rows, function(r) r$what) == "R-driven loop: total per step (rng=FALSE, kernel incl.)" & sapply(rows, function(r) r$n) == 10)]]$us)
gc.time(TRUE)

# 7. per-run fixed cost that stays in R in the R-driven design (tax sweep, 150 runs)
src <- readLines(file.path(Sys.getenv("ORACLE"), "bench/tax.R"))
eval(parse(text = src[grep("^economy <- abm_setup", src):(grep("^p <- proc.time", src) - 1)]))
for (tk in c(0L, 1L, 40L)) {
  t <- min(replicate(3, system.time(abm_run(economy, year, ticks = tk, reps = 25, seed = 1, record = "globals",
              params = list(audit_rate = c(0, 0.02, 0.04, 0.06, 0.08, 0.10))))[["user.self"]]))
  add(sprintf("abm_run tax sweep, ticks = %d: per run (150 runs), engine today", tk), 441, 1e6 * t / 150)
}
res <- do.call(rbind, rows)
saveRDS(res, file.path(args[2], "s3.rds"))
cat("load:", system("uptime", intern = TRUE), "\n")
