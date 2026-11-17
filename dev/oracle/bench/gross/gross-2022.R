# Gross (2022), "Beautiful cycles", Economic Modelling 106, 105678, in tidyABM.
#   source("sandbox/gross-2022-beautiful-cycles/gross-2022.R")
#   o <- run_gross(seed = 1, T = 1000)          # F = 200 firms, N = 20,000 households

suppressMessages(pkgload::load_all(Sys.getenv("TIDYABM_PKG", "tidyABM"), quiet = TRUE))

# Parameters (globals, so abm_run(params = ) can sweep them); names follow matlab/SCRIPT_CONTROL.m
gross_defaults <- list(mu = 0.05, tar = 0.10, sf = 1, pricing = 1, smRP = 15,
                       dwrig = 1, floor_wg_at0 = 1, w_inf_agg = 0,
                       loangrant = 1, Dmu = 0, Dsig = 0, iD = 0, bCAPRmin = 0,
                       directed = 0)

# MATLAB arithmetic
mean_m    <- function(x) sum(x) / length(x)
nanmean_m <- function(x) { x <- x[!is.na(x)]; sum(x) / length(x) }
max_m     <- function(a, b) if (is.na(b)) a else max(a, b)
pow_m     <- function(x, y) -stats::pweibull(x, y, lower.tail = FALSE, log.p = TRUE)
annuity   <- function(P, D, i) {
  ifelse(D > 1, ifelse(i > 0, P * (i * pow_m(1 + i, pmax(D, 2))) / (pow_m(1 + i, pmax(D, 2)) - 1), pmin(P, P / D)),
         i * P + P)
}
ledger <- function(x0, keep, ...) sum(c(x0, rbind(...)[, keep]))
gross_randn <- function(draw, sig) {
  z <- numeric(length(draw))
  if (sig != 0 && any(draw)) z[draw] <- stats::rnorm(sum(draw))
  z
}

# World: firms, households in blocks by employer (matlab/distrib.m); the bank is globals
gross_world <- function(F = 200, N = 100 * F, seed = 1, params = list()) {
  nf <- c(rep(N %/% F, F - 1), N - (F - 1) * (N %/% F))
  emp <- rep(seq_len(F), nf)
  abm_setup(
    agents = list(
      firm = abm_agents(
        n = F, firm = ~seq_len(n), nf = as.numeric(nf),
        cash = 0, loan = 0, rate = 0, dur = 0, block = NA_real_, nw = 0, active = 1,
        act0 = 1, aI = 0, ann = 0, status = 0L,
        intpaid = 0, prin = 0, lossg = 0, seize = 0, icr = NA_real_, lw = NA_real_,
        act1 = NA_real_, act2 = NA_real_,
        cpe = NA_real_, cpe1 = NA_real_, cpe2 = NA_real_,
        p = NA_real_, p1 = NA_real_, p2 = NA_real_,
        both = FALSE, wgf = 0, pgf = 0, wg = 0, wbill = NA_real_,
        borrow = FALSE, need = 0, zD = 0, newD = 1, grant = FALSE, deny = FALSE, lent = 0,
        gtype = 0L, paid = 0, wage_out = 0, sales = 0, rev = 0, dint = 0),
      hh = abm_agents(n = N, firm = emp, dep = 0, inc = 0, spend = 0, dint = 0)),
    globals = c(modifyList(gross_defaults, params), list(
      nhh = N, t = 0,
      bL = 0, bNW = 0, bDEP = 0, capr = NA_real_,
      L0 = 0, npos = 0, lg = 0, ln = 0, ndef = 0, nrepay = 0, nroll = 0,
      newloans = 0, ninig = 0, ntopup = 0, nclose = 0, div = 0,
      PD = 0, LGD = 0, pdh = list(numeric(0)), lgdh = list(numeric(0)), sPD = 0, sLGD = 0,
      iDt = 0, spread = 0, iL = 0,
      mc1 = NA_real_, mc2 = NA_real_, mp1 = NA_real_, mp2 = NA_real_,
      anyboth = FALSE, wga = 0, pga = 0,
      gdp = 0, wages = 0, unemp = 0, pm = NA_real_, wm = NA_real_, nact = 0,
      icr_w = NA_real_, icr_med = NA_real_)),
    seed = seed)
}

# One period, in the order of matlab/Simulation.m
gross_go <- abm_go(
  abm_global(t    ~ t + 1,
             L0   ~ sum(loan[.group == "firm"]),
             npos ~ sum(loan[.group == "firm"] > 0)),

  ## Step 6: debt service (repay the annuity, roll over or default), compA.m
  abm_rules(
    act0 ~ case_when(block > 0 ~ 0, dur == 0 ~ 1, dur > 0 & block == 0 ~ 1,
                     TRUE ~ NA_real_),
    aI   ~ if_else(dur > 0, rate * loan, 0),
    ann  ~ if_else(dur > 0, annuity(loan, dur, rate), 0)),
  abm_rules(
    status ~ case_when(dur <= 0                    ~ 0L,
                       cash >= ann                 ~ 1L,
                       cash >= aI & act0 == 1      ~ 2L,
                       cash < aI                   ~ 3L,
                       TRUE                        ~ 4L),
    icr ~ if_else(aI > 0, cash / aI, NA_real_),
    lw  ~ if_else(aI > 0, loan, NA_real_)),
  abm_rules(
    intpaid ~ if_else(status %in% 1:2, aI, 0),
    prin    ~ if_else(status == 1L, ann - aI, 0),
    lossg   ~ if_else(status == 3L, loan, 0),
    seize   ~ if_else(status == 3L, pmin(cash, loan), 0)),
  abm_rules(
    cash   ~ case_when(status == 1L ~ cash - prin - intpaid,
                       status == 2L ~ cash - intpaid,
                       status == 3L ~ cash - seize,
                       TRUE         ~ cash),
    nw     ~ case_when(status %in% 1:2 ~ nw - intpaid,
                       status == 3L    ~ nw + loan - seize,
                       TRUE            ~ nw),
    loan   ~ case_when(status == 1L ~ loan - prin, status == 3L ~ 0, TRUE ~ loan),
    dur    ~ case_when(status == 1L ~ dur - 1, status == 3L ~ 0, TRUE ~ dur),
    rate   ~ if_else(status == 3L, 0, rate),
    block  ~ case_when(status == 1L & block > 0 ~ block - 1, status == 3L ~ 0,
                       TRUE ~ block),
    active ~ if_else(status == 3L, 0, act0)),
  abm_global(
    lg      ~ sum(lossg[.group == "firm"]),
    ln      ~ ledger(0, .group == "firm", lossg, -seize),
    ndef    ~ sum(status == 3L, na.rm = TRUE),
    nrepay  ~ sum(status == 1L, na.rm = TRUE),
    nroll   ~ sum(status == 2L, na.rm = TRUE),
    bL      ~ ledger(bL, .group == "firm", -(prin + lossg)),
    bNW     ~ ledger(bNW, .group == "firm", intpaid - lossg, seize),
    bDEP    ~ ledger(bDEP, .group == "firm", -(intpaid + seize), -prin),
    icr_w   ~ sum(icr * lw, na.rm = TRUE) / sum(lw, na.rm = TRUE),
    icr_med ~ median(icr, na.rm = TRUE)),

  ## Loan pricing, Eq.(2)-(3), compPDLGD.m
  abm_global(
    PD     ~ if (npos > 0) lg / L0 else 0,
    LGD    ~ if (isTRUE(lg > 0)) ln / lg else 0,
    pdh    ~ list(c(utils::tail(pdh[[1]], smRP - 1), PD)),
    lgdh   ~ list(c(utils::tail(lgdh[[1]], smRP - 1), LGD)),
    sPD    ~ mean_m(pdh[[1]]),
    sLGD   ~ mean_m(lgdh[[1]]),
    iDt    ~ iD,
    spread ~ if (pricing == 1) sPD * sLGD / (1 - sPD) else 0,
    iL     ~ max_m(0, iDt + spread + mu)),

  ## Step 1: wage setting, Eq.(1), wage_setting.m
  abm_global(
    mc1 ~ nanmean_m(cpe1), mc2 ~ nanmean_m(cpe2),
    mp1 ~ nanmean_m(p1),   mp2 ~ nanmean_m(p2),
    anyboth ~ sum(act1 == 1 & act2 == 1, na.rm = TRUE) > 0,
    wga ~ if (anyboth) log(mc1 / mc2) else 0,
    pga ~ if (anyboth) log(mp1 / mp2) else 0),
  abm_rules(both ~ coalesce(act1 == 1 & act2 == 1, FALSE)),
  abm_rules(
    wgf ~ if_else(both, log(cpe1 / cpe2), wga),
    pgf ~ if_else(both, log(p1 / p2), pga)),
  abm_rules(
    wg ~ if_else(coalesce(pgf > wgf, FALSE) | dwrig != 1,
                 w_inf_agg * pga + (1 - w_inf_agg) * pgf, 0)),
  abm_rules(
    wg ~ if_else(t < 3 | (floor_wg_at0 == 1 & coalesce(wg < 0, FALSE)), 0, wg)),
  abm_rules(
    cpe ~ case_when(active != 1                ~ NA_real_,
                    t == 1                     ~ 1,
                    coalesce(act1 == 1, FALSE) ~ exp(log(cpe1) + wg),
                    TRUE                       ~ exp(log(mc1) + wg))),
  abm_rules(
    wbill ~ case_when(active != 1                ~ NA_real_,
                      t == 1                     ~ cpe * nf,
                      coalesce(act1 == 1, FALSE) ~ exp(log(wbill) + wg),
                      TRUE                       ~ cpe * nf)),

  ## Step 2: loan granting
  abm_rules(borrow ~ coalesce(cash < wbill, FALSE)),
  abm_rules(
    need ~ if_else(borrow, wbill - cash, 0),
    zD   ~ gross_randn(borrow, Dsig)),
  abm_rules(
    grant ~ borrow & loangrant != 0,
    deny  ~ borrow & loangrant == 0,
    newD  ~ ceiling(exp(Dmu + Dsig * zD))),
  abm_rules(
    gtype  ~ case_when(grant & loan == 0 ~ 1L, grant & loan > 0 ~ 2L, TRUE ~ 0L),
    lent   ~ if_else(grant, need, 0),
    rate   ~ if_else(grant, (loan * rate + need * iL) / (loan + need), rate),
    dur    ~ if_else(grant, ceiling((loan * dur + need * newD) / (loan + need)), dur),
    loan   ~ if_else(grant, loan + need, loan),
    cash   ~ if_else(grant, cash + need, cash),
    block  ~ case_when(grant ~ 0, deny ~ dur, TRUE ~ block),
    active ~ if_else(deny, 0, active),
    cpe    ~ if_else(deny, NA_real_, cpe),
    wbill  ~ if_else(deny, NA_real_, wbill)),
  abm_global(
    newloans ~ sum(lent[.group == "firm"]),
    ninig    ~ sum(gtype == 1L, na.rm = TRUE),
    ntopup   ~ sum(gtype == 2L, na.rm = TRUE),
    nclose   ~ sum(deny, na.rm = TRUE),
    bL       ~ ledger(bL, .group == "firm", lent),
    bDEP     ~ ledger(bDEP, .group == "firm", lent)),

  ## Step 3: wages
  abm_rules(wage_out ~ if_else(active == 1, cpe, 0),
            paid     ~ coalesce(wbill, 0)),
  abm_rules(inc ~ if_else(.group == "hh", sum(wage_out[.group == "firm"]), 0),
            .by = firm),
  abm_rules(cash ~ cash - paid, nw ~ nw - paid),
  abm_rules(dep ~ dep + inc),

  ## Step 4: production, pricing, sales, Eq.(4)-(6)
  abm_rules(sales ~ 0),
  abm_match(pair = "one_of", eligible = .group == "hh",
            among = .group == "firm" & active == 1),
  abm_rules(spend ~ sf * dep, .scope = "population"),
  abm_tell(sales ~ spend, to = if (directed == 1) firm else .partner,
           when = .group == "hh", .resolve = "sum"),
  abm_rules(p   ~ if_else(active == 1, sales, NA_real_),
            rev ~ if_else(active == 1 & !is.na(sales), sales, 0), .scope = "population"),
  abm_rules(dep ~ dep - spend, cash ~ cash + rev, nw ~ nw + rev, .scope = "population"),

  ## Deposit interest
  abm_global(iDt ~ if (isTRUE(bNW - iDt * bDEP < bCAPRmin * bL))
                     max_m(0, (bNW - bCAPRmin * bL) / bDEP) else iDt),
  abm_rules(dint ~ if (iDt > 0) iDt * (cash - 0.5 * rev) else 0,
            dint ~ if (iDt > 0) iDt * (dep + 0.5 * spend) else 0, .scope = "population"),
  abm_rules(cash ~ cash + dint, nw ~ nw + dint, dep ~ dep + dint, .scope = "population"),
  abm_global(
    bDEP ~ bDEP + sum(dint[.group == "firm"]) + sum(dint[.group == "hh"]),
    bNW  ~ bNW - sum(dint[.group == "firm"]) - sum(dint[.group == "hh"])),

  ## Step 5: bank dividends, Eq.(7)
  abm_global(capr ~ bNW / bL,
             div  ~ if (isTRUE(capr > tar)) (capr - tar) * bL else 0),
  abm_rules(dep ~ dep + div / nhh, .scope = "population"),
  abm_global(bDEP ~ bDEP + div, bNW ~ bNW - div),

  ## Carry t-1 to t-2, record aggregates
  abm_rules(act2 ~ act1, act1 ~ active, cpe2 ~ cpe1, cpe1 ~ cpe, p2 ~ p1, p1 ~ p,
            .scope = "population"),
  abm_global(
    gdp   ~ sum(rev[.group == "firm"]),
    wages ~ sum(paid[.group == "firm"]),
    unemp ~ sum(nf[.group == "firm" & active == 0], na.rm = TRUE) / nhh,
    pm    ~ nanmean_m(p1),
    wm    ~ nanmean_m(cpe1),
    nact  ~ sum(active == 1, na.rm = TRUE))
)

# Stock-flow consistency: the run stops if any residual exceeds 1e-9
gross_residuals <- list(
  sfc_bank = ~abs(sum(loan[.group == "firm"]) -
                  (sum(cash[.group == "firm"]) + sum(dep[.group == "hh"]) + bNW)) /
    max(1, sum(loan[.group == "firm"])),
  sfc_ledger = ~max(abs(bL - sum(loan[.group == "firm"])),
                    abs(bDEP - sum(cash[.group == "firm"]) - sum(dep[.group == "hh"]))) /
    max(1, sum(loan[.group == "firm"])),
  sfc_firm = ~max(abs(nw - (cash - loan))[.group == "firm"]) /
    max(1, sum(loan[.group == "firm"])))
gross_checks <- lapply(gross_residuals, function(f) {
  rlang::new_formula(NULL, call("<", rlang::f_rhs(f), 1e-9), env = rlang::f_env(f))
})

# Run: one row per period
run_gross <- function(seed = 1, T = 1000, F = 200, N = 100 * F, params = list(),
                      checks = TRUE) {
  w <- gross_world(F, N, seed, params)
  r <- abm_run(w, gross_go, ticks = T, seed = seed, record = "globals",
               measures = gross_residuals, checks = if (checks) gross_checks)
  g <- merge(as.data.frame(abm_globals(r)), as.data.frame(abm_measures(r)), by = "tick")
  g <- g[g$tick >= 1, ]
  g$debt_gdp <- g$bL / g$gdp
  g$profit_share <- 1 - g$wages / g$gdp
  g$wage_share <- g$wages / g$gdp
  g$infl <- c(NA, diff(log(g$pm)))
  g$wage_infl <- c(NA, diff(log(g$wm)))
  g[, c("tick", "gdp", "unemp", "bL", "bNW", "bDEP", "debt_gdp", "profit_share",
        "wage_share", "infl", "wage_infl", "PD", "LGD", "sPD", "sLGD", "spread", "iL",
        "iDt", "ndef", "nrepay", "nroll", "ninig", "ntopup", "nclose", "newloans", "div",
        "capr", "wages", "pm", "wm", "nact", "icr_w", "icr_med",
        "sfc_bank", "sfc_ledger", "sfc_firm")]
}
