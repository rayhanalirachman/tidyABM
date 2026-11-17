# tax model three ways on identical worlds and seeds:
#   (1) tidyABM engine (timed by ../cmp.R), (2) plain vectorised R with the user's own dplyr
#   expressions and no engine, (3) whole tick loop in native code with R's RNG.
suppressMessages(pkgload::load_all("tidyABM", quiet = TRUE)); library(dplyr, warn.conflicts = FALSE)
dyn.load("../rustq/tax.so")
economy <- abm_setup(
  agents = abm_agents(n = 441,
    type = ~sample(rep(c("honest", "dishonest", "imitating"), c(50, 50, n - 100))),
    income = ~runif(n, 0, 100), risk_aversion = ~runif(n, 0, 1)),
  network = abm_network("grid", dims = c(21, 21), diagonals = FALSE), seed = 1)
a <- economy$groups$agents
ref <- readRDS("res-tidyABM.rds")
runs <- unique(ref[, c(".run", ".rep", "audit_rate")]); seeds <- tidyABM:::run_seeds(1, nrow(runs))
nb <- economy$lattice$nb; stopifnot(all(lengths(nb) == 4))
nbm <- matrix(unlist(nb), 4) - 1L; storage.mode(nbm) <- "integer"
from <- rep(seq_len(441), each = 4); to <- unlist(nb)
type_i <- match(a$type, c("honest", "dishonest", "imitating"))

native <- function() unlist(lapply(seq_len(nrow(runs)), function(r) {
  set.seed(seeds[[r]])
  .Call("tax_run", type_i, a$income, a$risk_aversion, nbm, runs$audit_rate[r], 40L)
}))

plainR <- function() unlist(lapply(seq_len(nrow(runs)), function(r) {
  set.seed(seeds[[r]])
  type <- a$type; income <- a$income; risk_aversion <- a$risk_aversion; n <- 441L
  tax_rate <- 0.30; penalty_rate <- 0.45; audit_rate <- runs$audit_rate[r]
  believed_audit_chance <- rep(0.02, n); declared_share <- if_else(type == "honest", 1, 0)
  declared <- penalty_due <- numeric(n); out <- numeric(40)
  for (t in 1:40) {
    v <- declared_share[to]; s <- as.vector(rowsum(v, from)) / 4
    neighbours_share <- s + as.vector(rowsum(v - s[from], from)) / 4
    declared <- case_when(
      penalty_due > 0   ~ penalty_due,
      type == "honest"  ~ income,
      type == "dishonest" ~ case_when(
        believed_audit_chance > tax_rate / penalty_rate ~ income,
        believed_audit_chance < tax_rate / (tax_rate + (penalty_rate - tax_rate) *
                                  exp(risk_aversion * penalty_rate * income)) ~ 0,
        TRUE ~ pmin(income, pmax(0, income - log((1 - believed_audit_chance) * tax_rate /
          (believed_audit_chance * (penalty_rate - tax_rate))) /
          (risk_aversion * penalty_rate)))),
      type == "imitating" ~ neighbours_share * income)
    audited <- declared < income - 1e-9 & runif(n) < audit_rate
    penalty_due <- if_else(audited, tax_rate * (income - declared) * (1 + penalty_rate * income), 0)
    believed_audit_chance <- case_when(
      audited ~ 1,
      believed_audit_chance > audit_rate ~
        if_else(believed_audit_chance - 0.2 < 0, audit_rate, believed_audit_chance - 0.2),
      TRUE ~ believed_audit_chance)
    declared_share <- declared / income
    out[t] <- tax_rate * sum(declared) / sum(income)
  }
  out
}))
cpu <- function(f, k = 1) { p <- proc.time(); for (i in seq_len(k)) v <- f(); d <- proc.time() - p; list(v = v, s = (d[[1]] + d[[2]]) / k) }
want <- ref$mean_gross_tax_rate[ref$tick > 0]
for (i in 1:3) {
  pr <- cpu(plainR); nt <- cpu(native, 20)
  cat(sprintf("plain R (dplyr exprs, no engine): %.3f s | native whole model: %.4f s | max|diff| vs tidyABM: plainR %.3g native %.3g | identical: %s %s\n",
              pr$s, nt$s, max(abs(pr$v - want)), max(abs(nt$v - want)), identical(pr$v, want), identical(nt$v, want)))
}
# design (i): the neighbour kernel alone, R's rowsum (already C) vs a native loop, 441 agents
v <- runif(441); k <- 20000
t1 <- system.time(for (i in 1:k) { x <- v[to]; s <- as.vector(rowsum(x, from)) / 4 })[[1]]
t2 <- system.time(for (i in 1:k) s2 <- .Call("nb_mean", v, nbm, 4L))[[1]]
t0 <- system.time(for (i in 1:k) NULL)[[1]]
cat(sprintf("neighbour mean kernel per call: R rowsum %.1f us, native %.2f us (all.equal %s)\n",
            1e6 * (t1 - t0) / k, 1e6 * (t2 - t0) / k, isTRUE(all.equal(s, s2))))
