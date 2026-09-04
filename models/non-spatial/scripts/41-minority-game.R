# 41. Minority Game (Challet & Zhang 1997, Physica A 246: 407-418)
#
# An odd number of agents each pick one of two sides. Whoever ends up on the
# smaller side wins. There is no equilibrium to settle into -- if everyone
# predicts the same thing, everyone is wrong -- so agents use inductive rules
# instead: each holds S strategies, a strategy being a lookup table from the
# last m winning sides to a prediction, and plays whichever of its strategies
# has scored best so far.
#
# A strategy table is 2^m entries long and each agent holds S of them. That is
# an agent whose state is a matrix, and like the naming game it turns out to
# need no new grammar: a list column holds the tables and abm_rules() indexes
# into them.
#
# Reported: the volatility curve. sigma^2/N against alpha = 2^m/N is U-shaped
# with a minimum near alpha_c ~ 0.34, falling *below* the random-choice
# benchmark sigma^2/N = 1 there -- agents coordinate better than chance -- and
# rising back to 1 as alpha grows. Below alpha_c the crowd all reads the same
# signal and sigma^2/N blows up well above 1.

library(tidyABM)

minority_game <- function(n = 101, m = 5, s = 2, ticks = 2000, seed = 1) {
  n_states <- 2^m

  mg <- abm_setup(
    agents = abm_agents(
      n = n,
      # s strategies per agent, each a 0/1 vector of length 2^m, flattened
      strategies = ~lapply(seq_len(n),
                           function(i) matrix(sample(0:1, n_states * s, TRUE),
                                              nrow = n_states, ncol = s)),
      score      = ~lapply(seq_len(n), function(i) numeric(s)),
      action     = 0L
    ),
    globals = list(history = 1L, attendance = 0L),
    seed    = seed
  )

  go <- abm_go(
    # play the strategy that has done best so far, ties broken by the first
    abm_rules(action ~ vapply(seq_along(strategies), function(i) {
      best <- which.max(score[[i]])
      as.integer(strategies[[i]][history, best])
    }, integer(1))),

    # A = number choosing side 1; the minority side is the one with fewer
    abm_global(attendance ~ sum(action)),

    # every strategy is scored on what it *would* have predicted
    abm_rules(score ~ {
      winner <- as.integer(attendance < n() / 2)
      lapply(seq_along(score),
             function(i) score[[i]] +
               ifelse(strategies[[i]][history, ] == winner, 1, -1))
    }),

    # the shared memory: the last m winning sides, as a base-2 index
    abm_global(history ~ (bitwAnd(history - 1L, 2L^(m - 1) - 1L) * 2L +
                          as.integer(attendance < n() / 2)) + 1L)
  )

  g <- abm_globals(abm_run(mg, go, ticks = ticks, seed = seed))
  # A(t) is the sum of +/-1 actions, so it is 2 * (count on side 1) - N.
  # That normalisation is what makes random play sit at sigma^2/N = 1.
  a <- 2 * g$attendance[g$tick > ticks / 2] - n
  stats::var(a) / n
}

if (sys.nframe() == 0L) {
  n <- 101
  cat(sprintf("%4s %8s %12s  %s\n", "m", "alpha", "sigma^2/N", ""))
  sweep <- do.call(rbind, lapply(c(2, 3, 4, 5, 6, 8, 10), function(m) {
    v <- minority_game(n = n, m = m, seed = 1)
    a <- 2^m / n
    cat(sprintf("%4d %8.3f %12.2f  %s\n", m, a, v,
                strrep("#", max(1, round(min(v, 12) * 3)))))
    data.frame(m = m, alpha = a, volatility = v)
  }))
  cat("\nrandom-choice benchmark is sigma^2/N = 1; the minimum should sit near alpha = 0.34\n")

  # --- figure -------------------------------------------------------------
  # Parameter sweep: the volatility curve, on log axes, against the
  # random-choice benchmark and the critical alpha.
  library(ggplot2)

  fig_file <- function(name) {
    a <- grep("^--file=", commandArgs(), value = TRUE)
    d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
    dir.create(file.path(d, "..", "figures"), showWarnings = FALSE)
    file.path(d, "..", "figures", name)
  }

  fig <- ggplot(sweep, aes(alpha, volatility)) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
    geom_vline(xintercept = 0.34, linetype = "dotted", colour = "grey50") +
    geom_line(linewidth = 0.7) + geom_point() +
    scale_x_log10() + scale_y_log10() +
    theme_minimal() +
    labs(title = "Minority game: the volatility curve dips below chance",
         subtitle = "N = 101, 2 strategies each; dashed = random play, dotted = alpha_c",
         x = "alpha = 2^m / N", y = "sigma^2 / N")
  print(fig)
  ggsave(fig_file("41-minority-game.png"), fig, width = 6, height = 4,
         dpi = 120)
}
