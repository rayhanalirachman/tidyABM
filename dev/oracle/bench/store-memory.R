# Memory of a record = "all" run with 100,000 agents (PLAN.md P3 exit: the Rust
# store within 1.5x of the R engine). usage, one engine per process so the peak
# resident size is that engine's:
#   /usr/bin/time -l Rscript bench/store-memory.R <pkgdir> <engine> <out.rds>
args <- commandArgs(TRUE)
suppressMessages(pkgload::load_all(args[1], quiet = TRUE))
options(tidyABM.engine = args[2])
m <- abm_setup(agents = abm_agents(n = 1e5, w = ~runif(n), k = 1L, fixed = ~runif(n),
                                   tag = ~sample(c("a", "b"), n, TRUE)), seed = 1)
go <- abm_go(abm_rules(w ~ w * 0.99 + runif(n()) * 0.01, k ~ k + 1L))
t <- system.time(r <- abm_run(m, go, ticks = 30, seed = 2))
cat(args[2], "rows", nrow(r), "cpu", t[["user.self"]] + t[["sys.self"]], "R max mem MB",
    sum(gc()[, 6]), "\n")
saveRDS(r, args[3])
