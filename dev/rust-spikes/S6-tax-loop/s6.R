# Spike S6: the tax model's whole year loop in Rust vs the tidyABM engine.
# usage: TIDYABM_PKG=<pkgdir> Rscript s6.R <spikes-lib> <golden tax.rds>
args <- commandArgs(TRUE)
suppressMessages(pkgload::load_all(Sys.getenv("TIDYABM_PKG"), quiet = TRUE))
library(rspike, lib.loc = args[1])
cpu <- function(expr) { p <- proc.time(); force(expr); d <- proc.time() - p; c(real = d[["elapsed"]], cpu = d[["user.self"]] + d[["sys.self"]]) }

economy <- abm_setup(   # identical to oracle/bench/tax.R
  agents = abm_agents(n = 441,
    type = ~sample(rep(c("honest", "dishonest", "imitating"), c(50, 50, n - 100))),
    income = ~runif(n, 0, 100), risk_aversion = ~runif(n, 0, 1),
    believed_audit_chance = 0.02, declared_share = ~if_else(type == "honest", 1, 0),
    declared = 0, penalty_due = 0, audited = FALSE),
  network = abm_network("grid", dims = c(21, 21), diagonals = FALSE),
  globals = list(tax_rate = 0.30, penalty_rate = 0.45, audit_rate = 0.02, mean_gross_tax_rate = 0),
  seed = 1)
a <- economy$groups$agents
# neighbour order = the edge table's order, each edge seen from both ends, stable by
# focal agent (what the engine's padded index folds in); lattice$nb's order is NOT it
# (see diag.R: 347 of 6000 values then differ by 1 ulp)
e <- economy$edges; f <- c(e[[1]], e[[2]]); t <- c(e[[2]], e[[1]])
nb <- split(t, factor(f, levels = seq_len(nrow(economy$groups$agents))))
nb_off <- c(0L, cumsum(lengths(nb)))
nb_idx <- as.integer(unlist(nb)) - 1L
typ <- match(a$type, c("honest", "dishonest", "imitating"))
golden <- readRDS(args[2])
runs <- unique(golden[, c(".run", ".rep", "audit_rate")])
seeds <- unlist(tidyABM:::run_seeds(1, nrow(runs)))
want <- golden$mean_gross_tax_rate[golden$tick > 0]
cat(sprintf("runs %d, ticks 40, agents %d, neighbour degrees %s\n", nrow(runs), nrow(a),
            paste(names(table(lengths(nb))), collapse = "/")))

# A. R's generator through the C API (main thread), the design for P5
via_c <- function() unlist(lapply(seq_len(nrow(runs)), function(r) {
  set.seed(seeds[[r]])
  rs_tax_run(typ, a$income, a$risk_aversion, nb_off, nb_idx, runs$audit_rate[r], 40L)
}))
got <- via_c()
cat("A. Rust loop + R's C RNG, identical to engine:", identical(got, want), "\n")
# the loop must consume exactly one uniform per agent per tick, like runif(n())
set.seed(seeds[[1]]); invisible(rs_tax_run(typ, a$income, a$risk_aversion, nb_off, nb_idx, runs$audit_rate[1], 40L)); s1 <- .Random.seed
set.seed(seeds[[1]]); invisible(runif(441 * 40)); s2 <- .Random.seed
cat("   .Random.seed after one run identical to runif(441*40):", identical(s1, s2), "\n")
# whole result table rebuilt with Rust values
res <- golden; res$mean_gross_tax_rate[res$tick > 0] <- got
cat("   full globals table identical to golden:", identical(res, golden), "\n")

# B. pure-Rust RNG port (S1) on OS threads: what P6's rayon path would do
for (th in c(1L, 2L, 8L)) {
  g2 <- rs_tax_sweep(typ, a$income, a$risk_aversion, nb_off, nb_idx, runs$audit_rate, seeds, 40L, th)
  cat(sprintf("B. pure Rust RNG, %d thread(s), identical to engine: %s\n", th, identical(g2, want)))
}

# timings: min of 7 for the 150-run sweep, both paths, interleaved
tA <- tB1 <- tB8 <- NULL
for (i in 1:7) {
  tA <- rbind(tA, cpu(via_c()))
  tB1 <- rbind(tB1, cpu(rs_tax_sweep(typ, a$income, a$risk_aversion, nb_off, nb_idx, runs$audit_rate, seeds, 40L, 1L)))
  tB8 <- rbind(tB8, cpu(rs_tax_sweep(typ, a$income, a$risk_aversion, nb_off, nb_idx, runs$audit_rate, seeds, 40L, 8L)))
}
med <- function(m) sprintf("real %.4f s, cpu %.4f s (median of 7; min real %.4f)", median(m[, 1]), median(m[, 2]), min(m[, 1]))
cat("time A (R loop over 150 runs, Rust year loop, C RNG):", med(tA), "\n")
cat("time B (150 runs in Rust, 1 thread):                ", med(tB1), "\n")
cat("time B (150 runs in Rust, 8 threads):               ", med(tB8), "\n")
cat("load:", system("uptime", intern = TRUE), "\n")
