# Lengnick (2013), "Agent-based macroeconomics: A baseline model"
# Journal of Economic Behavior & Organization 86, 102-120
#
# STATUS: work in progress. This model does not run against the released
# package. It uses four pieces of grammar that are still uncommitted in this
# working tree (see `git diff`):
#
#   * `abm_sequential()` writing `partner_<col>` through a standing match,
#     which is what makes the goods market a real queue: the second buyer at a
#     shop sees the stock the first one took. `abm_tell()` cannot say it,
#     because it resolves every sender at once and the stock goes negative.
#   * `abm_match(weight = )`, a draw probability per candidate.
#   * `among` evaluated per (chooser, candidate) when it mentions `own_<col>`,
#     which is what "one of the firms I buy from" needs.
#   * a match made inside `abm_repeat()` not escaping the block.
#
# Two agent types and two relations. Neither relation is a network, because
# `abm_setup()` takes one and this model has two that both rewire: who I buy
# from (`sellers`, seven of them) and who I work for (`employer`). Both are
# ordinary agent columns, read across with `abm_neighbours(within = )`.
#
# NOTE ON `.scope = "population"`. Most rules here carry it and they are not
# decoration. A match stands until the next one, so a plain `abm_rules()`
# downstream of a match silently applies to the matched agents only. Dropping
# them from the month-end block once created 18,000 units of money out of
# nothing while every printed number still looked plausible.
#
# Run from the package root:  Rscript "work in progress/lengnick-2013.R"

suppressMessages(pkgload::load_all(".", quiet = TRUE))

## ---- parameters (Lengnick 2013, Table 1) ---------------------------------
# The paper uses H = 1000, F = 100 over 6000 months plus a 1000-month burn-in.
# That is out of reach here: the tick is ~150 nested passes and the goods
# market is a per-agent loop, so a month costs seconds rather than milliseconds.
H <- 200; F <- 20
months <- 120; burn <- 24
days   <- 21
nvisit <- 7      # n        sellers asked per day before demand lapses
nlink  <- 7      # type A connections
xi     <- 0.01   # price gap that makes a household switch seller
beta   <- 5      # firms an unemployed household visits
pi_s   <- 0.1    # search probability of a satisfied employee
alpha  <- 0.9    # consumption exponent
th_p   <- 0.25   # Theta_price
th_q   <- 0.25   # Theta_quant
gam    <- 24     # months of full employment before a wage cut
delta  <- 0.019  # wage adjustment support
phi_lo <- 0.25; phi_hi <- 1.0     # inventory bounds, times last month's demand
vp_lo  <- 1.025; vp_hi <- 1.15    # price bounds, times marginal cost
theta  <- 0.02   # price adjustment support
nu     <- 0.75   # Calvo probability
lambda <- 3      # units per worker per day
chi    <- 0.1    # liquidity buffer, times the wage bill

FID <- H + seq_len(F)               # the firms' .ids

## ---- helpers -------------------------------------------------------------
# how badly this candidate has failed to deliver to this chooser
unmet_at <- function(id, sellers, unmet) {
  j <- match(id, sellers)
  if (is.na(j)) 0 else unmet[[j]]
}
swap_seller <- function(s, out, into, do) {
  if (!do || is.na(out) || is.na(into)) s else replace(s, match(out, s), into)
}

## ---- 1. the world --------------------------------------------------------
economy <- abm_setup(
  agents = list(
    hh = abm_agents(
      n = H, mh = 100, wr = 63, wage = 63, income = 63, employed = TRUE,
      employer  = ~as.integer(rep_len(FID, n)),
      sellers   = ~lapply(seq_len(n), function(i) sample(FID, nlink)),
      unmet     = ~lapply(seq_len(n), function(i) numeric(nlink)),
      asked     = ~vector("list", n),
      PI = 1.05, cr = 63, daily = 3, want = 0, demand = 0, got = 0,
      searching = FALSE, visits_left = 0L, hired = FALSE,
      hunt_price = FALSE, hunt_quant = FALSE, rationed = FALSE,
      cand = NA_integer_, cand_price = NA_real_, drop_id = NA_integer_,
      swap = FALSE),
    firms = abm_agents(
      n = F, mf = 100, inv = ~rep(63 * H / F, n), price = 1.05, wage_f = 63,
      d_old = ~rep(63 * H / F, n), d_cur = 0, n_emp = ~rep(H / F, n),
      vac = 0L, fire_now = FALSE, filled_run = 0L,
      zeta = 0, eta = 0, calvo = 0, lo_stock = FALSE, hi_stock = FALSE,
      p_lo = 0, p_hi = 0, wagebill = 0, payout = 0)),
  globals = list(pool = 0, wealth = 0, excess_demand = 0, vacancy_rate = 0,
                 nonfinite = 0, lapsed = 0, planned = 0,
                 unemployment = 0, price_index = 0, output = 0),
  seed = 1)

## ---- 2. the month --------------------------------------------------------
month <- abm_go(

  ## --- the firm decides --------------------------------------------------
  abm_neighbours(n_emp ~ n(), within = employer == own_.id),
  abm_rules(n_emp ~ coalesce(n_emp, 0), .scope = "population"),
  abm_rules(zeta ~ runif(n(), 0, delta), eta ~ runif(n(), 0, theta),
            calvo ~ runif(n()), .scope = "population"),

  # wage: up if last month's vacancy went unfilled, down after gamma full months
  abm_rules(
    wage_f     ~ case_when(vac == 1L         ~ wage_f * (1 + zeta),
                           filled_run >= gam ~ wage_f * (1 - zeta),
                           TRUE              ~ wage_f),
    filled_run ~ case_when(vac == 1L         ~ 0L,
                           filled_run >= gam ~ 0L,
                           TRUE              ~ filled_run + 1L)),

  # quantity and price, both read off the same comparison of inventories with
  # last month's demand
  abm_rules(lo_stock ~ inv < phi_lo * d_old,
            hi_stock ~ inv > phi_hi * d_old,
            p_lo     ~ vp_lo * wage_f / (lambda * days),
            p_hi     ~ vp_hi * wage_f / (lambda * days)),
  abm_rules(
    vac   ~ if_else(lo_stock, 1L, 0L),
    price ~ case_when(calvo >= nu                          ~ price,
                      lo_stock & price * (1 + eta) <= p_hi ~ price * (1 + eta),
                      hi_stock & price * (1 - eta) >= p_lo ~ price * (1 - eta),
                      TRUE                                 ~ price)),

  # firing carries a month's lag: the match consumes last month's decision and
  # the rule underneath it takes this month's, so the lag is the order of two
  # steps. The firm picks one of its own workers and cuts them loose.
  abm_match(pair = "one_of", eligible = fire_now,
            among = .group == "hh" & employer == own_.id),
  abm_sequential(partner_employer ~ NA_integer_),
  abm_rules(fire_now ~ hi_stock & n_emp > 0, .scope = "population"),

  ## --- the household looks for a job -------------------------------------
  abm_neighbours(income ~ sum(wage_f), within = .id == own_employer),
  abm_rules(employed    ~ !is.na(income),
            wage        ~ coalesce(income, 0),
            searching   ~ !employed | wage < wr | runif(n()) < pi_s,
            visits_left ~ if_else(employed, 1L, beta),
            .scope = "population"),
  abm_repeat(
    abm_rules(hired ~ FALSE, .scope = "population"),
    abm_match(pair = "one_of", among = .group == "firms",
              eligible = searching & visits_left > 0L),
    # one position per firm, and the callers arrive one at a time, so the
    # vacancy the next caller finds is the one this caller left
    abm_sequential(
      hired       ~ partner_vac > 0L & partner_wage_f > wage,
      partner_vac ~ partner_vac - as.integer(hired),
      employer    ~ if_else(hired, .partner, employer),
      wage        ~ if_else(hired, partner_wage_f, wage),
      visits_left ~ if_else(hired, 0L, visits_left - 1L),
      searching   ~ searching & !hired),
    until = !any(searching & visits_left > 0L, na.rm = TRUE),
    max = beta),

  ## --- the household looks for better sellers ----------------------------
  abm_neighbours(n_emp ~ n(), within = employer == own_.id),
  abm_rules(n_emp ~ coalesce(n_emp, 0), .scope = "population"),
  abm_rules(rationed   ~ vapply(unmet, function(u) any(u > 0), logical(1)),
            hunt_price ~ runif(n()) < th_p,
            hunt_quant ~ runif(n()) < th_q,
            .scope = "population"),

  # cheaper: a firm I do not buy from, noticed in proportion to its size,
  # weighed against one of the sellers I have
  abm_match(pair = "one_of", eligible = hunt_price, weight = n_emp,
            among = .group == "firms" & !mapply(`%in%`, .id, own_sellers)),
  abm_rules(cand ~ .partner, cand_price ~ partner_price, .scope = "population"),
  abm_match(pair = "one_of", eligible = hunt_price,
            among = .group == "firms" & mapply(`%in%`, .id, own_sellers)),
  abm_rules(drop_id ~ .partner,
            swap    ~ !is.na(cand) & !is.na(.partner) &
                      cand_price < (1 - xi) * partner_price,
            .scope = "population"),
  abm_rules(sellers ~ Map(swap_seller, sellers, drop_id, cand, swap),
            .scope = "population"),

  # reliable: drop a seller that rationed me, chosen in proportion to how much
  abm_match(pair = "one_of", eligible = hunt_quant & rationed, weight = n_emp,
            among = .group == "firms" & !mapply(`%in%`, .id, own_sellers)),
  abm_rules(cand ~ .partner, .scope = "population"),
  abm_match(pair = "one_of", eligible = hunt_quant & rationed,
            among  = .group == "firms" & mapply(`%in%`, .id, own_sellers),
            weight = mapply(unmet_at, .id, own_sellers, own_unmet)),
  abm_rules(drop_id ~ .partner,
            swap    ~ !is.na(cand) & !is.na(.partner),
            .scope  = "population"),
  abm_rules(sellers ~ Map(swap_seller, sellers, drop_id, cand, swap),
            unmet   ~ lapply(sellers, function(s) numeric(length(s))),
            .scope  = "population"),

  ## --- and decides what to spend -----------------------------------------
  abm_neighbours(PI ~ mean(price), within = mapply(`%in%`, .id, own_sellers)),
  abm_rules(cr ~ pmin((mh / PI)^alpha, mh / PI), .scope = "population"),
  abm_rules(cr ~ if_else(is.finite(cr), cr, 0), .scope = "population"),
  abm_rules(daily ~ cr / days, .scope = "population"),

  ## --- 21 days ------------------------------------------------------------
  abm_global(lapsed ~ 0, planned ~ 0),
  abm_repeat(
    abm_rules(want ~ daily, asked ~ vector("list", n()), .scope = "population"),

    abm_repeat(
      abm_rules(demand ~ 0, got ~ 0, .scope = "population"),
      abm_match(pair = "one_of", eligible = want > 0.05 * daily,
                among = .group == "firms" &
                        mapply(`%in%`, .id, own_sellers) &
                        !mapply(`%in%`, .id, own_asked)),
      # the shop serves one customer at a time: what I carry away is what my
      # money buys and what the customers before me left on the shelf
      abm_sequential(
        demand        ~ pmin(want, mh / partner_price),
        got           ~ pmin(demand, partner_inv),
        mh            ~ pmax(0, mh - got * partner_price),
        want          ~ want - got,
        partner_inv   ~ partner_inv - got,
        partner_mf    ~ partner_mf + got * partner_price,
        partner_d_cur ~ partner_d_cur + demand),
      abm_rules(
        unmet ~ Map(function(u, s, p, x) {
                      j <- match(p, s)
                      if (!is.na(j) && isTRUE(x > 0)) u[[j]] <- u[[j]] + x
                      u
                    }, unmet, sellers, .partner, demand - got),
        asked ~ Map(function(a, p) if (is.na(p)) a else c(a, p), asked, .partner),
        .scope = "population"),
      until = !any(want > 0.05 * daily, na.rm = TRUE),
      max = nvisit),

    abm_global(lapsed  ~ lapsed + sum(want, na.rm = TRUE),
               planned ~ planned + sum(daily, na.rm = TRUE)),
    abm_rules(inv ~ inv + lambda * n_emp, .scope = "population"),
    max = days),

  ## --- the end of the month ------------------------------------------------
  abm_neighbours(n_emp ~ n(), within = employer == own_.id),
  abm_rules(n_emp ~ coalesce(n_emp, 0), .scope = "population"),
  abm_rules(wagebill ~ wage_f * n_emp, .scope = "population"),
  # the rare case: labour costs the firm cannot meet, and the workers take the cut
  abm_rules(wage_f ~ if_else(n_emp > 0 & mf < wagebill, mf / n_emp, wage_f),
            .scope = "population"),
  abm_neighbours(income ~ sum(wage_f), within = .id == own_employer),
  abm_rules(employed ~ !is.na(income), income ~ coalesce(income, 0),
            .scope = "population"),
  abm_rules(mh ~ mh + income, mf ~ mf - wage_f * n_emp, .scope = "population"),
  # what is left after the buffer is paid out, and the rich hold the larger claim
  abm_rules(payout ~ pmax(0, mf - chi * wage_f * n_emp), .scope = "population"),
  abm_rules(mf ~ mf - payout, .scope = "population"),
  abm_global(pool ~ sum(payout, na.rm = TRUE), wealth ~ sum(mh, na.rm = TRUE)),
  abm_rules(mh ~ mh + pool * mh / wealth, .scope = "population"),
  abm_rules(wr ~ case_when(!employed  ~ wr * 0.9,
                           income > wr ~ income,
                           TRUE        ~ wr),
            .scope = "population"),
  abm_global(nonfinite ~ sum(!is.finite(mh[.group == "hh"])) +
                          sum(!is.finite(daily[.group == "hh"])),
             excess_demand ~ lapsed / max(planned, 1e-9),
             vacancy_rate ~ sum(vac, na.rm = TRUE) / F),
  abm_rules(d_old ~ d_cur, d_cur ~ 0, .scope = "population"),

  abm_global(unemployment ~ mean(!employed, na.rm = TRUE),
             price_index  ~ sum(price * n_emp, na.rm = TRUE) /
                            sum(n_emp, na.rm = TRUE),
             output       ~ sum(lambda * days * n_emp, na.rm = TRUE))
)

## ---- 3. run --------------------------------------------------------------
result <- abm_run(economy, month, ticks = months, seed = 1, progress = FALSE)

g  <- abm_globals(result)[-1, ]
gg <- g[g$tick > burn, ]
u  <- gg$unemployment
# aligned to g$tick: the change since the previous month, NA for the first
infl <- c(NA, diff(log(g$price_index)))[g$tick > burn]
fm <- subset(result, .group == "firms")
hh <- subset(result, .group == "hh")
final_f <- subset(fm, tick == max(tick))
final_h <- subset(hh, tick == max(tick))
skew <- function(x) mean((x - mean(x))^3) / stats::sd(x)^3

print(as.data.frame(g[seq(1, nrow(g), by = 6),
                      c("tick", "unemployment", "price_index", "output",
                        "excess_demand")]), digits = 4)

cat(sprintf("\n-- months %d-%d, H = %d, F = %d ------------------------------\n",
            burn + 1, max(g$tick), H, F))
cat(sprintf("unemployment        mean %.3f  min %.3f  max %.3f   [paper 0 - 0.043]\n",
            mean(u), min(u), max(u)))
cat(sprintf("unsatisfied demand  95th pct %.5f                    [paper < 0.0003]\n",
            stats::quantile(gg$excess_demand, .95)))
cat(sprintf("output              sd/mean %.4f  AR(1) %+.3f\n",
            stats::sd(gg$output) / mean(gg$output),
            stats::cor(gg$output[-1], gg$output[-nrow(gg)])))
cat(sprintf("Phillips   cor(inflation, unemployment)  %+.3f       [paper negative]\n",
            stats::cor(infl, u)))
cat(sprintf("Beveridge  cor(vacancies, unemployment)  %+.3f       [paper negative]\n",
            stats::cor(gg$vacancy_rate, u)))
cat(sprintf("firm-size skewness                       %+.2f        [paper +1.88]\n",
            skew(final_f$n_emp)))
pc <- tapply(fm$price, fm$.id, function(p) mean(abs(diff(p)) > 1e-12))
cat(sprintf("price changes per firm-month   median %.2f  skew %+.2f  [paper 0.09]\n",
            stats::median(pc), skew(pc)))

# invariants. The paper is a pure exchange economy, so money only circulates,
# and inventories can never go negative -- that second one is the direct test
# of the sequential partner write.
cat(sprintf("\nmoney %.2f (should be %d)   min inventory %.2f   non-finite %d\n",
            sum(final_h$mh) + sum(final_f$mf), H * 100 + F * 100,
            min(fm$inv), sum(g$nonfinite)))
