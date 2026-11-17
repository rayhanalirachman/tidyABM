# Spike S4: R fallback embedding and error transport across the Rust boundary.
# usage: TIDYABM_PKG=<pkgdir> Rscript s4.R <spikes-lib>
args <- commandArgs(TRUE); lib <- normalizePath(args[1])
library(rspike, lib.loc = lib)
suppressMessages(pkgload::load_all(Sys.getenv("TIDYABM_PKG"), quiet = TRUE))
ns <- asNamespace("rspike")
col <- get("savvy_Store_col__impl", ns); set <- get("savvy_Store_set_col__impl", ns); step <- get("savvy_Store_step__impl", ns)
ok <- function(label, cond) cat(sprintf("[%s] %s\n", if (isTRUE(cond)) "PASS" else "FAIL", label))
cond_of <- function(expr) tryCatch({ force(expr); NULL }, condition = function(c) c)

# 1. alternate Rust kernels and R-evaluated rules on one store, vs the same in plain R
st <- Store$new(441L, 3L); p <- st$.ptr
x1 <- .Call(col, p, 1L); x2 <- .Call(col, p, 2L)
set.seed(9)
for (t in 1:50) {
  .Call(step, p, TRUE)                                   # Rust: x1 <- 0.5 * x1 + runif
  a <- .Call(col, p, 1L); b <- .Call(col, p, 2L)
  .Call(set, p, 2L, dplyr::if_else(a > b, a - b, b * runif(441)))   # R fallback rule, draws too
}
got <- list(.Call(col, p, 1L), .Call(col, p, 2L))
set.seed(9)
for (t in 1:50) { x1 <- 0.5 * x1 + runif(441); x2 <- dplyr::if_else(x1 > x2, x1 - x2, x2 * runif(441)) }
ok("Rust kernel + R rule alternating on one store == plain R (values and one RNG stream)", identical(got, list(x1, x2)))

# 2. an R error raised inside R code that Rust called keeps its class, message and fields
f_abort <- function(x) rlang::abort("custom failure", class = "my_error", payload = 42)
e <- cond_of(rs_call_r(f_abort, 1))
e0 <- cond_of(f_abort(1))
ok("rlang::abort() inside an R call made from Rust: class preserved", identical(class(e), class(e0)))
ok("   message and custom field preserved", identical(conditionMessage(e), conditionMessage(e0)) && identical(e$payload, 42))
f_missing <- function(x) x + monney
e <- cond_of(rs_call_r(f_missing, 1)); e0 <- cond_of(f_missing(1))
ok("base error 'object not found' from Rust-called R: same class and message", identical(class(e), class(e0)) && identical(conditionMessage(e), conditionMessage(e0)))
# through the engine's own step-error rewriting, as a fallback rule would be
stp <- structure(list(rules = list(list(quo = rlang::quo(money + 1), target = "money", vars = "monney"))), class = "abm_rules")
state <- list(groups = list(agents = tibble::tibble(.id = 1:3, .group = "agents", money = 1)), globals = list(), tick = 3L)
via <- function(run) cond_of(withCallingHandlers(run(), error = function(e) {
  if (inherits(e, "tidyABM_error")) return()
  rethrow_step_error(e, stp, state, 1L)
}))
e <- via(function() rs_call_r(f_missing, 1)); e0 <- via(function() f_missing(1))
ok("rethrow_step_error() on an error from Rust-called R == on the same error in R (class)", identical(class(e), class(e0)))
ok("   ... and the full cli message", identical(conditionMessage(e), conditionMessage(e0)))
cat("   class:", paste(class(e), collapse = " / "), "\n")
# warnings and their restarts from R code that Rust called
w <- NULL
r <- withCallingHandlers(rs_call_r(function(x) { warning("careful"); x + 1 }, 1),
                         warning = function(c) { w <<- conditionMessage(c); invokeRestart("muffleWarning") })
ok("warning() in Rust-called R reaches the handler and muffleWarning resumes", identical(w, "careful") && identical(r, 2))
r <- withRestarts(rs_call_r(function(x) invokeRestart("skip_rule", "fallback"), 1), skip_rule = function(v) v)
ok("a restart established in R and invoked inside Rust-called R unwinds Rust frames", identical(r, "fallback"))

# 3. Rust errors and panics become R errors; the session survives
e <- cond_of(rs_error("typed error from Rust"))
ok("typed Rust error -> R error with that message", inherits(e, "error") && grepl("typed error from Rust", conditionMessage(e)))
cat("   class:", paste(class(e), collapse = " / "), "\n")
e <- cond_of(rs_panic("boom"))
ok("panic! -> R error (panic = 'unwind', savvy catch_unwind)", inherits(e, "error"))
cat("   class:", paste(class(e), collapse = " / "), " message:", conditionMessage(e), "\n")
e <- cond_of(rs_tax_sweep(1L, 1, 1, c(0L, 0L), integer(), rep(0.02, 4), 1L, 2L, 2L))
ok("panic inside a worker thread (index out of bounds) -> R error, no abort", inherits(e, "error"))
ok("session alive after panics: further calls work", identical(rs_call_r(function(x) x * 2, 21), 42))

# 4. Ctrl-C during a 10 s Rust loop (child R session, SIGINT after 1.5 s)
code <- sprintf('library(rspike, lib.loc = "%s"); t0 <- Sys.time()
r <- tryCatch(rs_spin(10), interrupt = function(e) "interrupted")
cat("RESULT", r, format(as.numeric(Sys.time() - t0, units = "secs")), "\\n"); cat("ALIVE", 1 + 1, "\\n")', lib)
px <- processx::process$new(file.path(R.home("bin"), "Rscript"), c("-e", code), stdout = "|", stderr = "|")
Sys.sleep(1.5); t_int <- Sys.time(); px$interrupt(); px$wait(15000); t_back <- as.numeric(Sys.time() - t_int, units = "secs")
out <- px$read_all_output_lines()
cat("  ", out, sep = "\n   ")
ok(sprintf("Ctrl-C honoured during Rust loop (child returned %.2f s after SIGINT)", t_back),
   any(grepl("RESULT interrupted", out)) && any(grepl("ALIVE 2", out)) && t_back < 0.5)

# 5. GC torture on the round trips (small n; every allocation triggers a GC)
st <- Store$new(5L, 2L); p <- st$.ptr
prog <- Program$new(list(op = "call", fn = "+", args = list(list(op = "var", name = "x"), list(op = "num", value = 1))))
run_all <- function() {
  v <- .Call(col, p, 1L); .Call(set, p, 2L, v * 2); s <- st$chr_col()
  cl <- rs_call_closure(function() paste("a", 1:3), 3L)
  ev <- prog$eval(list(x = c(1, NA, 3)), 3L)
  set.seed(1); tx <- rs_tax_run(c(1L, 2L, 3L), c(10, 20, 30), c(.5, .5, .5), c(0L, 1L, 2L, 3L), c(1L, 2L, 0L), 0.5, 3L)
  e1 <- cond_of(rs_call_r(f_abort, 1)); e2 <- cond_of(rs_panic("x"))
  list(v, .Call(col, p, 2L), s, cl, ev, tx, class(e1), class(e2))
}
ref <- run_all()
gctorture(TRUE); tort <- run_all(); gctorture(FALSE)
ok("gctorture(TRUE): all round trips give the same results, no crash", identical(ref, tort))
cat("rustc:", system("PATH=$HOME/.cargo/bin:$PATH rustc --version", intern = TRUE), " R:", R.version.string, "\n")
