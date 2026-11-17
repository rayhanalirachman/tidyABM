args <- commandArgs(TRUE); pkg <- args[1]
suppressMessages(pkgload::load_all(pkg, quiet = TRUE))
## 1. the world: 50 honest, 50 dishonest, the rest imitate -------------------
economy <- abm_setup(
  agents = abm_agents(
    n = 441,
    type = ~sample(rep(c("honest", "dishonest", "imitating"), c(50, 50, n - 100))),
    income        = ~runif(n, 0, 100),
    risk_aversion = ~runif(n, 0, 1),
    believed_audit_chance = 0.02,                   # ASSUMED: everyone starts out believing 2%
    # ASSUMED: last year's declared / income; all but the honest start at 0
    declared_share = ~if_else(type == "honest", 1, 0),
    declared = 0, penalty_due = 0, audited = FALSE),
  network = abm_network("grid", dims = c(21, 21), diagonals = FALSE),
  globals = list(tax_rate = 0.30, penalty_rate = 0.45, audit_rate = 0.02,
                 mean_gross_tax_rate = 0),
  seed = 1)

## 2. one year ---------------------------------------------------------------
year <- abm_go(
  # Eq. 5: what the four neighbours declared last year, as a share of their income
  abm_neighbours(neighbours_share ~ mean(declared_share)),

  # Eqs. 1-4: what each type declares. A penalty due this year replaces it (Eq. 7)
  abm_rules(
    declared ~ case_when(
      penalty_due > 0   ~ penalty_due,
      type == "honest"  ~ income,
      type == "dishonest" ~ case_when(
        # so afraid of being caught that it declares everything
        believed_audit_chance > tax_rate / penalty_rate ~ income,
        # so unafraid that it declares nothing
        believed_audit_chance < tax_rate / (tax_rate + (penalty_rate - tax_rate) *
                                  exp(risk_aversion * penalty_rate * income)) ~ 0,
        # in between: the amount that maximises its expected utility
        TRUE ~ pmin(income, pmax(0, income - log((1 - believed_audit_chance) * tax_rate /
          (believed_audit_chance * (penalty_rate - tax_rate))) /
          (risk_aversion * penalty_rate)))),
      # imitators declare the share their neighbours declared, of their own income
      type == "imitating" ~ neighbours_share * income)),

  # only someone who declared less than their income can be audited
  abm_rules(
    audited ~ declared < income - 1e-9 & runif(n()) < audit_rate),

  # a separate abm_rules(): rules in one call all see the start-of-step values,
  # and the three below need the `audited` just written
  abm_rules(
    # Eq. 7: the penalty falls due next year
    penalty_due ~ if_else(audited, tax_rate * (income - declared) * (1 + penalty_rate * income), 0),
    # Eq. 6: an audit makes the agent certain it will happen again; the fear fades 0.2 a year
    believed_audit_chance ~ case_when(
      audited ~ 1,
      believed_audit_chance > audit_rate ~
        if_else(believed_audit_chance - 0.2 < 0, audit_rate, believed_audit_chance - 0.2),
      TRUE ~ believed_audit_chance),
    declared_share ~ declared / income),

  # Eq. 9: taxes and penalties over total income, times the tax rate
  abm_global(mean_gross_tax_rate ~ tax_rate * sum(declared) / sum(income)))

Rprof("../rustq/tax.prof", interval = 0.005, gc.profiling = TRUE); p <- proc.time()
res <- abm_run(economy, year, ticks = 40, reps = 25, seed = 1, record = "globals",
               params = list(audit_rate = c(0, 0.02, 0.04, 0.06, 0.08, 0.10)))
d <- proc.time() - p; Rprof(NULL)
cat(sprintf("%s: real %.1f s, cpu %.1f s\n", basename(pkg), d[["elapsed"]], d[["user.self"]] + d[["sys.self"]]))
