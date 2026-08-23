# 39. Sznajd model (Sznajd-Weron & Sznajd 2000, Int. J. Mod. Phys. C 11: 1157)
#
# "United we stand, divided we fall." Agents sit on a ring holding an opinion
# of +1 or -1. Each step picks one adjacent pair; if the two agree, they
# together persuade the agents on either side of them. If they disagree,
# nothing happens.
#
# The rule runs *outward*. Every other update step in the grammar is a pull --
# an agent reads its neighbours and writes to itself -- and this one cannot be
# written that way, because whether you are persuaded depends on a coincidence
# between two other agents, not on anything you can see from your own row. It
# is what abm_tell(to = "neighbours") is for.
#
# Reported: the ring always reaches consensus, and the exit probability -- the
# chance that the consensus is +1 -- rises with the initial density of +1 but
# is distinctly *steeper than linear*. That is what separates outflow dynamics
# from the voter model, where the exit probability is exactly the initial
# density; how much steeper, and whether it becomes a step function as the ring
# grows, is still argued over in the literature.

library(tidyABM)

sznajd <- function(n = 16, p_up = 0.5, ticks = 300, seed = 1) {
  m <- abm_setup(
    agents  = abm_agents(n = n,
                         spin     = ~ifelse(runif(n) < p_up, 1L, -1L),
                         speaking = FALSE),
    network = abm_network(type = "ring", degree = 2),
    seed    = seed
  )

  go <- abm_go(
    # one adjacent pair per step, NetLogo's `ask one-of turtles`
    abm_match(pair = "network", eligible = seq_len(n()) == sample.int(n(), 1)),

    # the picked agent speaks only if its neighbour agrees with it
    abm_rules(speaking ~ !is.na(.partner) & spin == partner_spin),

    # and it brings its partner in, so the pair speaks from both ends
    abm_tell(speaking ~ TRUE, to = .partner, when = speaking),

    # the persuasion itself: outward, to everyone the pair touches
    abm_tell(spin ~ spin, to = "neighbours", when = speaking),

    abm_rules(speaking ~ FALSE, .scope = "population"),
    abm_global(up ~ mean(spin > 0))
  )

  abm_globals(abm_run(m, go, ticks = ticks, seed = seed))$up
}

if (sys.nframe() == 0L) {
  reps <- 30
  cat(sprintf("%6s  %8s  %10s  %14s\n",
              "p_up", "reached", "P(all +1)", "mean t_consensus"))
  # P(all +1) is conditional on reaching consensus, so the runs still mixed at
  # the tick limit do not quietly count as "not +1".
  args <- commandArgs(trailingOnly = TRUE)
  ps <- if (length(args)) as.numeric(args) else c(0.25, 0.5, 0.75)
  for (p in ps) {
    out <- parallel::mclapply(seq_len(reps), function(s) {
      u <- sznajd(p_up = p, seed = s + 1000 * p)
      hit <- which(u %in% c(0, 1))
      c(done = length(hit) > 0, up = u[length(u)],
        t = if (length(hit)) hit[[1]] else NA_real_)
    }, mc.cores = 2)
    out <- do.call(rbind, out)
    done <- out[, "done"] == 1
    cat(sprintf("%6.2f  %8.2f  %10.2f  %14.0f\n",
                p, mean(done), mean(out[done, "up"] == 1),
                mean(out[, "t"], na.rm = TRUE)))
  }
  cat("\nThe voter model would give P(all +1) = p_up exactly. This is steeper:\nlow densities are wiped out and high ones run away. Binomial s.e. at 30 reps\nis about 0.09.\n")
}
