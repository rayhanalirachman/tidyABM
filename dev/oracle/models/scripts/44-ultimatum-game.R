# 44. Fairness versus reason in the ultimatum game
#     (Nowak, Page & Sigmund 2000, Science 289: 1773-1775)
#
# A proposer splits a unit; a responder either accepts, in which case the split
# stands, or rejects, in which case neither gets anything. A strategy is a pair
# (p, q): offer p when proposing, accept nothing below q when responding.
# Reason says offer nothing and accept anything; people offer about a half.
#
# Nowak, Page & Sigmund's answer is reputation. A fraction w of the population
# finds out about any given interaction, so with probability w a proposer knows
# what its responder has been demanding and can offer just enough to clear it.
# That makes a high q pay -- it attracts higher offers rather than only
# rejections -- and p has to follow it up.
#
# Reported: with w = 0 the population converges near the rational solution
# S(0, 0); with w = 1 it converges near the fair solution S(1/2, 1/2), with q
# a little below p.
#
# Grammar note: the "draw the index once" idiom from Part 4. A genome with two
# traits cannot be resampled with two independent sample() calls or p and q get
# shuffled apart, so the index is drawn in a step of its own.

library(tidyABM)

ultimatum <- function(w = 1, n = 100, generations = 350, rounds = 8,
                      mu = 0.005, seed = 1) {
  m <- abm_setup(
    agents = abm_agents(
      n = n,
      p = ~runif(n, 0, 0.5),
      q = ~runif(n, 0, 0.5),
      payoff = 0,
      pick   = 1L
    ),
    seed = seed
  )

  # one encounter: proposer offers, responder takes it or leaves it
  play <- list(
    abm_match(pair = "random", role = list(proposer = TRUE, responder = TRUE)),
    abm_rules(payoff ~ {
      pr <- which(.role == "proposer")[[1]]
      re <- which(.role == "responder")[[1]]
      # with probability w the proposer knows this responder's demand
      informed <- runif(1) < w
      offer <- if (informed && q[[re]] > p[[pr]] && q[[re]] < 1) q[[re]] else p[[pr]]
      out <- payoff
      if (offer >= q[[re]]) {
        out[[pr]] <- out[[pr]] + (1 - offer)
        out[[re]] <- out[[re]] + offer
      }
      out
    })
  )

  go <- do.call(abm_go, c(
    rep(play, rounds),
    list(
      abm_global(mean_p ~ mean(p), mean_q ~ mean(q)),
      # reproduction proportional to payoff: the index is drawn once...
      abm_rules(pick ~ sample(n(), n(), replace = TRUE, prob = payoff + 1e-9),
                .scope = "population"),
      # ...and both traits of the surviving genome travel by it
      abm_rules(p ~ p[pick] + runif(n(), -mu, mu),
                q ~ q[pick] + runif(n(), -mu, mu),
                .scope = "population"),
      # strategies live in the simplex p >= 0, q >= 0, p + q <= 1
      abm_rules(p ~ pmax(0, pmin(1, p)), q ~ pmax(0, pmin(1, q)),
                .scope = "population"),
      abm_rules(p ~ pmin(p, 1 - q), .scope = "population"),
      abm_rules(payoff ~ 0, .scope = "population")
    )
  ))

  g <- abm_globals(abm_run(m, go, ticks = generations, seed = seed))
  g[g$tick > 0, ]
}

if (sys.nframe() == 0L) {
  cat(sprintf("%5s  %8s  %8s\n", "w", "mean p", "mean q"))
  sweep <- do.call(rbind, lapply(c(0, 0.25, 0.5, 0.75, 1), function(w) {
    out <- do.call(rbind, parallel::mclapply(1:3, function(s) {
      g <- ultimatum(w = w, seed = s)
      last <- g[g$tick > max(g$tick) - 50, ]
      c(p = mean(last$mean_p), q = mean(last$mean_q))
    }, mc.cores = 2))
    cat(sprintf("%5.1f  %8.3f  %8.3f\n", w, mean(out[, "p"]), mean(out[, "q"])))
    data.frame(w = w, p = mean(out[, "p"]), q = mean(out[, "q"]))
  }))
  cat("\nNowak, Page & Sigmund: w = 0 converges near the rational solution S(0,0);\ninformation moves the population towards the fair one, with q a little below p.\nAt w = 1 this implementation degenerates -- an informed proposer always meets\nthe demand, so p stops being selected on at all and only q is left.\n")

  # --- figure -------------------------------------------------------------
  # Parameter sweep: the two traits against the reputation parameter, with the
  # rational and fair solutions marked.
  library(ggplot2)

  fig_file <- function(name) {
    a <- grep("^--file=", commandArgs(), value = TRUE)
    d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
    dir.create(file.path(d, "..", "figures"), showWarnings = FALSE)
    file.path(d, "..", "figures", name)
  }

  d <- rbind(data.frame(w = sweep$w, trait = "offer p",  value = sweep$p),
             data.frame(w = sweep$w, trait = "demand q", value = sweep$q))

  fig <- ggplot(d, aes(w, value, colour = trait)) +
    geom_hline(yintercept = 0.5, linetype = "dashed", colour = "grey50") +
    geom_line(linewidth = 0.7) + geom_point() +
    ylim(0, 0.7) +
    theme_minimal() +
    labs(title = "Ultimatum game: reputation moves the population toward fairness",
         subtitle = "100 agents, 350 generations, 3 seeds; dashed line is the fair split",
         x = "w, the chance a proposer knows the responder's demand",
         y = "mean trait", colour = NULL)
  print(fig)
  ggsave(fig_file("44-ultimatum-game.png"), fig, width = 6, height = 4,
         dpi = 120)
}
