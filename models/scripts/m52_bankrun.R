# 52. Bank runs and the sequential service constraint
#     (Diamond & Dybvig 1983, J. Polit. Econ. 91: 401-419)
library(tidyABM)

bankrun <- function(n = 1000, days = 200, impatient = 0.1, r1 = 1.2,
                    liquid = 0.5, memory = 0.9, ordered = TRUE, seed = 1) {
  m <- abm_setup(
    agents = abm_agents(n = n, pos = ~sample(n), theta = ~runif(n),
                        belief = 0, ran = FALSE, paid = 0, dry = FALSE),
    globals = list(till = liquid * n, empty = 0),
    seed = seed
  )
  serve <- if (ordered) {
    abm_sequential(
      ran    ~ runif(1) < impatient | belief > theta,
      paid   ~ if_else(ran, pmin(r1, till), 0),
      till   ~ till - paid,
      dry    ~ till < r1,
      belief ~ memory * belief + (1 - memory) * as.numeric(dry),
      .order = pos)
  } else {
    abm_sequential(
      ran    ~ runif(1) < impatient | belief > theta,
      paid   ~ if_else(ran, pmin(r1, till), 0),
      till   ~ till - paid,
      dry    ~ till < r1,
      belief ~ memory * belief + (1 - memory) * as.numeric(dry))
  }
  go <- abm_go(
    abm_global(till ~ liquid * n()),
    serve,
    abm_global(empty ~ mean(ran))
  )
  abm_run(m, go, ticks = days, seed = seed)
}

if (sys.nframe() == 0L) {
  # takes the impatient-shock levels as command-line arguments, so the sweep
  # can be split across sittings: Rscript m52_bankrun.R 0.44
  args <- commandArgs(trailingOnly = TRUE)
  shocks <- if (length(args)) as.numeric(args) else c(0.30, 0.36, 0.44, 0.52)
  cat("N = 200 depositors, 50 days, till = 0.5 N, r1 = 1.2\n")
  cat("the till serves 0.5 / 1.2 = 41.7% of them, which is the critical\n")
  cat("withdrawal rate\n\n")
  cat(sprintf("%-9s %6s %9s %8s %8s %8s %8s\n", "order", "shock", "run rate",
              "front", "2nd", "3rd", "back"))
  for (t in shocks) {
    for (ord in c(TRUE, FALSE)) {
      r <- bankrun(n = 200, days = 50, impatient = t, memory = 0.8,
                   ordered = ord, seed = 1)
      f <- r[r$tick > 30, ]
      q <- cut(f$pos, stats::quantile(f$pos, 0:4 / 4), include.lowest = TRUE)
      o <- tapply(f$ran, q, mean)
      cat(sprintf("%-9s %6.2f %9.3f %8.3f %8.3f %8.3f %8.3f\n",
                  if (ord) "queue" else "shuffled", t, mean(f$ran),
                  o[1], o[2], o[3], o[4]))
    }
  }
}
