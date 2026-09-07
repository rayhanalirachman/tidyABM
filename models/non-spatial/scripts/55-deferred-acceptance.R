# 55. Deferred acceptance / the stable marriage problem
#     (Gale & Shapley 1962, American Mathematical Monthly 69: 9-15)
library(tidyABM)

# my rank of agent i, from a rank vector indexed by .id.  NB: not `pick()`,
# which dplyr 1.1 exports and the data mask would find first.
rank_of <- function(lst, i) {
  vapply(seq_along(i), function(k) {
    v <- lst[[k]]
    if (is.na(i[[k]]) || is.null(v) || length(v) < i[[k]]) NA_real_
    else as.numeric(v[[i[[k]]]])
  }, numeric(1))
}

deferred_acceptance <- function(n = 100, max_rounds = 5000, seed = 1) {
  ids_w <- (n + 1L):(2L * n)
  ids_m <- 1L:n
  rank_over <- function(k, targets) {
    lapply(seq_len(k), function(i) {
      r <- rep(NA_real_, 2 * n); r[sample(targets)] <- seq_along(targets); r
    })
  }
  m <- abm_setup(
    agents = list(
      men   = abm_agents(n = n, rank = ~rank_over(n, ids_w),
                         worst = 0, fiancee = NA_integer_, win = FALSE),
      women = abm_agents(n = n, rank = ~rank_over(n, ids_m),
                         holder = NA_integer_, best = Inf)
    ),
    seed = seed
  )

  round <- abm_repeat(
    # every woman starts the round holding nobody and listening
    abm_rules(best ~ Inf, holder ~ NA_integer_, .scope = "population"),
    # every man proposes to the best woman who has not yet rejected him --
    # including, for an engaged man, his fiancee, who is exactly that woman
    abm_match(pair = "nearest",
              cost = if_else(rank_of(own_rank, .id) <= own_worst,
                             NA_real_, rank_of(own_rank, .id)),
              eligible = .group == "men", among = .group == "women"),
    # she hears every proposal at once and keeps the best
    abm_tell(best ~ rank_of(partner_rank, .id), to = .partner,
             when = .group == "men" & !is.na(.partner), .resolve = "min"),
    abm_rules(win ~ !is.na(.partner) & rank_of(partner_rank, .id) == partner_best),
    abm_rules(fiancee ~ if_else(win, .partner, NA_integer_),
              worst   ~ if_else(!is.na(.partner) & !win,
                                rank_of(rank, .partner), worst)),
    abm_tell(holder ~ .id, to = .partner, when = win),
    until = sum(.group == "men" & is.na(fiancee)) == 0,
    max = max_rounds
  )
  go <- abm_go(round)
  abm_run(m, go, ticks = 1, seed = seed)
}

report <- function(n, seed) {
  r <- deferred_acceptance(n = n, seed = seed)
  f <- r[r$tick == 1, ]
  men   <- f[f$.group == "men", ]
  women <- f[f$.group == "women", ]
  mr <- mean(rank_of(men$rank, men$fiancee))
  wr <- mean(rank_of(women$rank, women$holder))
  stable <- sum(!is.na(men$fiancee)) == n
  c(n = n, men = mr, women = wr, ln_n = log(n), n_over_ln = n / log(n),
    matched = stable)
}

if (sys.nframe() == 0L) {
  cat("uniform random preferences, men proposing, 5 seeds each\n")
  cat("Pittel (1989): the proposing side averages ~ln n, the other ~n/ln n\n\n")
  cat(sprintf("%5s %8s %8s %10s %10s\n", "n", "men", "ln n", "women", "n/ln n"))
  sweep <- do.call(rbind, lapply(c(25, 50, 100, 200), function(n) {
    o <- colMeans(t(vapply(1:5, function(s) report(n, s), numeric(6))))
    cat(sprintf("%5d %8.2f %8.2f %10.1f %10.1f\n", n, o[["men"]], o[["ln_n"]],
                o[["women"]], o[["n_over_ln"]]))
    data.frame(n = n, men = o[["men"]], ln_n = o[["ln_n"]],
               women = o[["women"]], n_over_ln = o[["n_over_ln"]])
  }))

  # --- figure -------------------------------------------------------------
  # Parameter sweep on log axes: each side's mean rank of its partner against
  # Pittel's asymptotics. Proposing is worth a lot.
  library(ggplot2)

  fig_file <- function(name) {
    a <- grep("^--file=", commandArgs(), value = TRUE)
    d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
    dir.create(file.path(d, "..", "figures"), showWarnings = FALSE)
    file.path(d, "..", "figures", name)
  }

  d <- rbind(
    data.frame(n = sweep$n, series = "men (proposing)",   rank = sweep$men),
    data.frame(n = sweep$n, series = "ln n",              rank = sweep$ln_n),
    data.frame(n = sweep$n, series = "women (receiving)", rank = sweep$women),
    data.frame(n = sweep$n, series = "n / ln n",          rank = sweep$n_over_ln))

  fig <- ggplot(d, aes(n, rank, colour = series)) +
    geom_line(linewidth = 0.7) + geom_point() +
    scale_x_log10() + scale_y_log10() +
    theme_minimal() +
    labs(title = "Deferred acceptance: the proposing side wins, by a lot",
         subtitle = "uniform random preferences, 5 seeds per size",
         x = "market size n", y = "mean rank of one's partner", colour = NULL)
  print(fig)
  ggsave(fig_file("55-deferred-acceptance.png"), fig, width = 6, height = 4,
         dpi = 120)
}
