# Base-R transcription of matlab/Simulation.m and its helpers, loop for loop; the oracle for gross-2022.R.
# (1 + i)^D is libm pow() through pweibull(), as in the model: R's ^ multiplies out D = 2, 3, 4.
#   ref_gross(seed, T, F, N, params = list(...))   parameter names as in gross_defaults
#   choose(cands, N) and randn() are the two random draws (randi and randn in the MATLAB), mean_m() its mean()

ref_gross <- function(seed = 1, T = 1000, F = 200, N = 100 * F, params = list(),
                      choose = function(cands, N) cands[sample.int(length(cands), N, replace = TRUE)],
                      randn = function() stats::rnorm(1),
                      mean_m = function(x) sum(x) / length(x)) {
  s <- modifyList(list(mu = 0.05, tar = 0.10, sf = 1, pricing = 1, smRP = 15, dwrig = 1,
                       floor_wg_at0 = 1, w_inf_agg = 0, loangrant = 1, Dmu = 0, Dsig = 0,
                       iD = 0, bCAPRmin = 0, directed = 0), params)
  if (!is.null(seed)) set.seed(seed)
  nanmean <- function(x) { x <- x[!is.na(x)]; sum(x) / length(x) }
  mmax <- function(a, b) if (is.na(b)) a else max(a, b)
  compA <- function(P, D, i) {
    aI <- i * P
    pw <- -stats::pweibull(1 + i, D, lower.tail = FALSE, log.p = TRUE)
    a <- if (D > 1) { if (i > 0) P * (i * pw) / (pw - 1) else min(P, P / D) } else aI + P
    c(a, aI, a - aI)
  }
  nf <- c(rep(N %/% F, F - 1), N - (F - 1) * (N %/% F))
  atFI <- rep(seq_len(F), nf)
  hDEP <- numeric(N); fDEP <- fL <- fNW <- numeric(F)
  bL <- bDEP <- bNW <- 0
  fi <- fd <- numeric(F); block <- rep(NA_real_, F)
  active <- cpe <- p <- payW <- matrix(NA_real_, T, F)
  PD <- LGD <- numeric(T)
  out <- matrix(NA_real_, T, 24, dimnames = list(NULL, c(
    "tick", "gdp", "unemp", "bL", "bNW", "bDEP", "ndef", "nrepay", "nroll", "ninig",
    "ntopup", "nclose", "iL", "spread", "PD", "LGD", "iDt", "wages", "pm", "wm", "div",
    "newloans", "capr", "nact")))
  for (t in 1:T) {
    ## Step 6: debt service
    L0 <- fL; D0 <- fDEP
    nrep <- nroll <- ndef <- 0; lossg <- lossn <- 0; aIrec <- numeric(F)
    for (f in 1:F) {
      if (isTRUE(fd[f] == 0)) active[t, f] <- 1
      if (isTRUE(fd[f] > 0) && isTRUE(block[f] == 0)) active[t, f] <- 1
      if (isTRUE(block[f] > 0)) active[t, f] <- 0
      if (isTRUE(fd[f] > 0)) {
        A <- compA(fL[f], fd[f], fi[f]); a <- A[1]; aI <- A[2]; aP <- A[3]; aIrec[f] <- aI
        if (isTRUE(fDEP[f] >= a)) {
          nrep <- nrep + 1
          fL[f] <- fL[f] - aP; fDEP[f] <- fDEP[f] - aP
          fDEP[f] <- fDEP[f] - aI; bDEP <- bDEP - aI
          bL <- bL - aP; bDEP <- bDEP - aP
          fNW[f] <- fNW[f] - aI; bNW <- bNW + aI
          fd[f] <- fd[f] - 1
          if (isTRUE(block[f] > 0)) block[f] <- block[f] - 1
        } else if (isTRUE(fDEP[f] >= aI) && isTRUE(active[t, f] == 1)) {
          nroll <- nroll + 1
          fDEP[f] <- fDEP[f] - aI; bDEP <- bDEP - aI
          fNW[f] <- fNW[f] - aI; bNW <- bNW + aI
        } else if (isTRUE(fDEP[f] < aI)) {
          active[t, f] <- 0; ndef <- ndef + 1
          seize <- min(fDEP[f], fL[f])
          lossg <- lossg + fL[f]; lossn <- lossn + fL[f] - seize
          bL <- bL - fL[f]; bNW <- bNW - fL[f]
          bDEP <- bDEP - seize; bNW <- bNW + seize
          fDEP[f] <- fDEP[f] - seize
          fNW[f] <- fNW[f] + fL[f] - seize
          fL[f] <- 0; fd[f] <- 0; fi[f] <- 0; block[f] <- 0
        }
      }
    }

    ## Loan pricing (compPDLGD.m)
    PD[t] <- if (sum(L0 > 0) > 0) lossg / sum(L0) else 0
    LGD[t] <- if (isTRUE(lossg > 0)) lossn / lossg else 0
    w <- (t - min(s$smRP, t) + 1):t
    sPD <- mean_m(PD[w]); sLGD <- mean_m(LGD[w])
    iDt <- s$iD
    spread <- if (s$pricing == 1) sPD * sLGD / (1 - sPD) else 0
    iL <- mmax(0, iDt + spread + s$mu)

    ## Step 1: wage setting (wage_setting.m)
    wg <- numeric(F)
    if (t >= 3) {
      both <- active[t - 1, ] %in% 1 & active[t - 2, ] %in% 1
      wga <- pga <- 0
      if (sum(both, na.rm = TRUE) > 0) {
        wga <- log(nanmean(cpe[t - 1, ]) / nanmean(cpe[t - 2, ]))
        pga <- log(nanmean(p[t - 1, ]) / nanmean(p[t - 2, ]))
      }
      wgf <- rep(wga, F); pgf <- rep(pga, F)
      wgf[both] <- log(cpe[t - 1, both] / cpe[t - 2, both])
      pgf[both] <- log(p[t - 1, both] / p[t - 2, both])
      for (f in 1:F) {
        if (isTRUE(pgf[f] > wgf[f]) || s$dwrig != 1) wg[f] <- s$w_inf_agg * pga + (1 - s$w_inf_agg) * pgf[f]
        if (s$floor_wg_at0 == 1 && isTRUE(wg[f] < 0)) wg[f] <- 0
      }
    }
    if (t == 1) {
      cpe[t, ] <- 1; payW[t, ] <- cpe[t, ] * nf
    } else {
      for (f in 1:F) {
        if (isTRUE(active[t, f] == 1) && isTRUE(active[t - 1, f] == 1)) {
          cpe[t, f] <- exp(log(cpe[t - 1, f]) + wg[f])
          payW[t, f] <- exp(log(payW[t - 1, f]) + wg[f])
        } else if (isTRUE(active[t, f] == 1) && isTRUE(active[t - 1, f] == 0)) {
          cpe[t, f] <- exp(log(nanmean(cpe[t - 1, ])) + wg[f])
          payW[t, f] <- cpe[t, f] * nf[f]
        }
      }
    }

    ## Step 2: loan granting
    newflow <- 0; ninig <- ntop <- nclose <- 0
    for (f in 1:F) {
      if (isTRUE(fDEP[f] < payW[t, f])) {
        delta <- payW[t, f] - fDEP[f]; lb <- fL[f]
        newD <- ceiling(exp(s$Dmu + s$Dsig * (if (s$Dsig != 0) randn() else 0)))
        newd <- ceiling((lb * fd[f] + delta * newD) / (lb + delta))
        newi <- (lb * fi[f] + delta * iL) / (lb + delta)
        if (s$loangrant != 0) {
          block[f] <- 0
          if (isTRUE(fL[f] == 0)) ninig <- ninig + 1 else if (isTRUE(fL[f] > 0)) ntop <- ntop + 1
          fDEP[f] <- fDEP[f] + delta; fL[f] <- fL[f] + delta
          bL <- bL + delta; bDEP <- bDEP + delta
          newflow <- newflow + delta
          fd[f] <- newd; fi[f] <- newi
        } else {
          active[t, f] <- 0; block[f] <- fd[f]; nclose <- nclose + 1
          payW[t, f] <- NA; cpe[t, f] <- NA
        }
      }
    }
    on <- active[t, ] %in% 1

    ## Step 3: wages
    payF <- payW[t, ]; payF[is.na(payF)] <- 0
    fDEP <- fDEP - payF; fNW <- fNW - payF
    hDEP <- hDEP + ifelse(on[atFI], cpe[t, atFI], 0)

    ## Step 4: production, pricing, sales
    payC <- s$sf * hDEP
    assign <- if (s$directed == 1) atFI else if (any(on)) choose(which(on), N) else integer()
    if (any(on)) p[t, on] <- vapply(split(payC, factor(assign, levels = seq_len(F))), sum, 0)[on]
    hDEP <- hDEP - payC
    rev <- p[t, ]; rev[is.na(rev)] <- 0
    fDEP <- fDEP + rev; fNW <- fNW + rev

    ## Deposit interest
    if (isTRUE(bNW - iDt * bDEP < s$bCAPRmin * bL)) iDt <- mmax(0, (bNW - s$bCAPRmin * bL) / bDEP)
    if (isTRUE(iDt > 0)) {
      iFI <- iDt * (fDEP - 0.5 * rev); fDEP <- fDEP + iFI; fNW <- fNW + iFI
      iHH <- iDt * (hDEP + 0.5 * payC); hDEP <- hDEP + iHH
      bDEP <- bDEP + sum(iFI) + sum(iHH); bNW <- bNW - sum(iFI) - sum(iHH)
    }

    ## Step 5: dividends
    capr <- bNW / bL; div <- 0
    if (isTRUE(capr > s$tar)) {
      div <- (capr - s$tar) * bL
      hDEP <- hDEP + div / N; bDEP <- bDEP + div; bNW <- bNW - div
    }

    pay <- payW[t, ]; pay[is.na(pay)] <- 0
    out[t, ] <- c(t, sum(rev), mean(active[t, atFI] %in% 0), bL, bNW, bDEP, ndef, nrep, nroll, ninig, ntop,
                  nclose, iL, spread, PD[t], LGD[t], iDt, sum(pay), nanmean(p[t, ]),
                  nanmean(cpe[t, ]), div, newflow, capr, sum(on))
  }
  out <- as.data.frame(out)
  out$debt_gdp <- out$bL / out$gdp
  out$profit_share <- 1 - out$wages / out$gdp
  out
}
