# 40. Naming Game (Baronchelli, Felici, Loreto, Caglioti & Steels 2006,
#     JSTAT P06014)
#
# Agents invent names for one object and negotiate their way to a shared one.
# Each tick a speaker and a hearer meet. The speaker utters a name from its
# inventory, inventing one if it is empty. If the hearer already knows that
# name the negotiation succeeds and *both* agents throw away everything else
# they knew, keeping only that name. If it does not, the hearer simply adds it.
#
# The agent state here is a set, not a number, and the set grows and collapses.
# It turns out abm_agents() takes a list column and abm_rules() will happily
# return one -- a rule is a dplyr expression and list columns are ordinary
# tibble columns -- so this needs no new grammar at all. What it needs is for
# the rules to be written over lists: Map() where you would have written
# arithmetic.
#
# Reported: the scaling laws. The total number of names held anywhere peaks at
# N_w max ~ N^1.5 and convergence to one shared name happens after t_conv ~
# N^1.5 *interactions*. abm_match(pair = "random") pairs the whole population
# at once, so one tick is n/2 interactions -- and because the pairs are
# disjoint, a tick is exactly n/2 independent interactions rather than an
# approximation of them.

library(tidyABM)

naming_game <- function(n = 100, ticks = 6000, seed = 1) {
  m <- abm_setup(
    agents = abm_agents(
      n         = n,
      inventory = ~vector("list", n),   # everyone starts knowing nothing
      utterance = NA_integer_
    ),
    seed    = seed
  )

  go <- abm_go(
    # a speaker and a hearer, drawn as a pair; roles are symmetric so the
    # first member of each pair speaks
    abm_match(pair = "random", role = list(speaker = TRUE, hearer = TRUE)),

    # the speaker picks a name it knows, or coins a fresh one. `.id * 10^6 + k`
    # keeps coined names unique without a shared counter.
    abm_rules(utterance ~ {
      s <- which(.role == "speaker")[[1]]
      inv <- inventory[[s]]
      rep(if (length(inv)) inv[sample.int(length(inv), 1L)]
          else .id[[s]] * 1000000L + sum(lengths(inventory)) + 1L,
          n())
    }),

    # success collapses both inventories to the uttered name; failure adds it
    # to the hearer's and leaves the speaker alone
    abm_rules(inventory ~ {
      u <- utterance[[1]]
      h <- which(.role == "hearer")[[1]]
      if (u %in% inventory[[h]]) {
        rep(list(u), n())
      } else {
        out <- inventory
        out[[h]] <- c(out[[h]], u)
        out
      }
    }),

    abm_global(words   ~ sum(lengths(inventory)),
               distinct_names ~ length(unique(unlist(inventory))),
               known   ~ mean(lengths(inventory) > 0))
  )

  abm_globals(abm_run(m, go, ticks = ticks, seed = seed))
}

if (sys.nframe() == 0L) {
  reps <- 3
  cat(sprintf("%5s %10s %14s %16s\n",
              "N", "N_w max", "t_conv (int.)", "t_conv / N^1.5"))
  fits <- list()
  for (n in c(40, 80, 160)) {
    out <- parallel::mclapply(seq_len(reps), function(s) {
      g <- naming_game(n = n, ticks = 30 * n, seed = s)
      conv <- which(g$distinct_names == 1 & g$known == 1)
      c(nw    = max(g$words, na.rm = TRUE),
        tconv = if (length(conv)) g$tick[conv[[1]]] * n / 2 else NA_real_)
    }, mc.cores = 2)
    out <- colMeans(do.call(rbind, out))
    cat(sprintf("%5d %10.0f %14.0f %16.3f\n",
                n, out[["nw"]], out[["tconv"]], out[["tconv"]] / n^1.5))
    fits[[as.character(n)]] <- c(n = n, out)
  }
  f <- do.call(rbind, fits)
  cat(sprintf("\nfitted exponents (Baronchelli et al. report 1.5): N_w max ~ N^%.2f, t_conv ~ N^%.2f\n",
              stats::coef(stats::lm(log(f[, "nw"]) ~ log(f[, "n"])))[[2]],
              stats::coef(stats::lm(log(f[, "tconv"]) ~ log(f[, "n"])))[[2]]))
}
