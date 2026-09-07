library(tidyABM)

# 30. Axelrod (1986) norms and metanorms game -------------------------------
# Boldness and vengefulness on Axelrod's 0-7 scale (3-bit strings). Every agent
# can see every other, so the population is a complete graph and
# `abm_neighbours()` means "over everybody else".
#
# The model is read from both sides -- what I did to you, and what was done to
# me -- and the two readings have to be the same events, or enforcement costs
# only balance on average. `abm_draw(.each = "endpoint")` puts the coins on the
# edge: each agent holds one draw per neighbour for whether it noticed that
# neighbour and one for whether it acted, and both endpoints read the same
# numbers. `saw`/`zeal` are the focal agent's own; `saw_back`/`zeal_back` are
# its neighbour's.

T_ <- 3; H_ <- -1; P_ <- -9; E_ <- -2

mutate3 <- function(x, rate = 0.01) {
  for (b in 0:2) {
    flip <- runif(length(x)) < rate
    x <- bitwXor(x, as.integer(flip) * bitwShiftL(1L, b))
  }
  as.integer(x)
}

opportunity <- function(metanorms) {
  steps <- list(
    abm_rules(seen     ~ runif(n())),
    abm_rules(defected ~ boldness / 7 > seen),
    abm_global(n_def ~ sum(defected)),
    # temptation to the defector, hurt to every other player
    abm_rules(payoff ~ payoff + if_else(defected, T_, 0) +
                        H_ * (n_def - as.integer(defected))),
    # one coin per (observer, observed) pair for noticing, one for acting
    abm_draw(saw ~ runif(n()), zeal ~ runif(n()), .each = "endpoint"),
    # what I saw, and how much of it I chose to punish
    abm_neighbours(witnessed   ~ sum(defected & saw < seen),
                   punish_acts ~ sum(defected & saw < seen &
                                     zeal < own_vengefulness / 7)),
    abm_rules(witnessed   ~ coalesce(witnessed, 0L),
              punish_acts ~ coalesce(punish_acts, 0L)),
    abm_rules(shirked ~ witnessed - punish_acts,
              payoff  ~ payoff + E_ * punish_acts),
    # how many others punished *me*: the same coins, read from the other end
    abm_neighbours(punishers ~ sum(own_defected & saw_back < own_seen &
                                   zeal_back < vengefulness / 7)),
    abm_rules(payoff ~ payoff + P_ * coalesce(punishers, 0L) * as.integer(defected))
  )
  if (!metanorms) return(steps)
  c(steps, list(
    # the metanorm: seeing a defection and letting it go is itself punishable.
    # Fresh coins, and again read from both ends.
    abm_draw(msaw ~ runif(n()), mzeal ~ runif(n()), .each = "endpoint"),
    abm_neighbours(meta_hits ~ sum(own_shirked > 0 & msaw_back < own_seen &
                                   mzeal_back < vengefulness / 7),
                   meta_acts ~ sum(shirked > 0 & msaw < seen &
                                   mzeal < own_vengefulness / 7)),
    abm_rules(payoff ~ payoff + P_ * coalesce(meta_hits, 0L) * shirked),
    abm_rules(payoff ~ payoff + E_ * coalesce(meta_acts, 0L))
  ))
}

# One generation of selection. Axelrod replicates a player twice if it scored a
# standard deviation above the mean and not at all if it scored one below; here
# that is written as a fixed-size resample. The index is drawn *once*, in its own
# step, so that both traits of a surviving genome travel together.
evolution <- list(
  abm_global(mu_p ~ mean(payoff), sd_p ~ stats::sd(payoff)),
  abm_rules(offspring ~ case_when(payoff > mu_p + sd_p ~ 2,
                                  payoff < mu_p - sd_p ~ 0,
                                  TRUE                 ~ 1)),
  abm_rules(pick ~ sample(n(), n(), replace = TRUE, prob = offspring + 1e-9)),
  abm_rules(boldness ~ boldness[pick], vengefulness ~ vengefulness[pick]),
  abm_rules(boldness ~ mutate3(boldness), vengefulness ~ mutate3(vengefulness)),
  abm_rules(payoff ~ 0)
)

axelrod <- function(metanorms, n = 20, generations = 100, seed = 1) {
  pop <- abm_setup(
    agents  = abm_agents(n = n,
                         boldness     = ~sample(0:7, n, replace = TRUE),
                         vengefulness = ~sample(0:7, n, replace = TRUE),
                         payoff = 0),
    network = abm_network(type = "complete"),
    globals = list(n_def = 0, mu_p = 0, sd_p = 0),
    seed    = seed
  )
  go <- do.call(abm_go, c(rep(opportunity(metanorms), 4), evolution))
  abm_run(pop, go, ticks = generations, seed = seed)
}

report <- function(label, res) {
  last <- res[res$tick >= max(res$tick) - 9, ]
  cat(sprintf("%-10s  boldness %.2f   vengefulness %.2f   n(final) = %d\n",
              label, mean(last$boldness), mean(last$vengefulness),
              sum(res$tick == max(res$tick))))
}

sweep <- function(metanorms, seeds = 1:20) {
  vapply(seeds, function(s) {
    r <- axelrod(metanorms, seed = s)
    last <- r[r$tick >= max(r$tick) - 9, ]
    c(boldness = mean(last$boldness), vengefulness = mean(last$vengefulness))
  }, numeric(2))
}

n_res <- sweep(FALSE); m_res <- sweep(TRUE)
cat(sprintf("norms      mean boldness %.2f  vengefulness %.2f  norm collapsed in %d/20\n",
            mean(n_res["boldness", ]), mean(n_res["vengefulness", ]),
            sum(n_res["vengefulness", ] < 1)))
cat(sprintf("metanorms  mean boldness %.2f  vengefulness %.2f  norm collapsed in %d/20\n",
            mean(m_res["boldness", ]), mean(m_res["vengefulness", ]),
            sum(m_res["vengefulness", ] < 1)))

# --- figure ---------------------------------------------------------------
# Final-state comparison: where boldness and vengefulness settle, with and
# without the metanorm. The norm alone does not hold; the metanorm does.
library(ggplot2)

fig_file <- function(name) {
  a <- grep("^--file=", commandArgs(), value = TRUE)
  d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
  dir.create(file.path(d, "..", "figures"), showWarnings = FALSE)
  file.path(d, "..", "figures", name)
}

d <- rbind(
  data.frame(game = "norms", trait = "boldness",
             value = n_res["boldness", ]),
  data.frame(game = "norms", trait = "vengefulness",
             value = n_res["vengefulness", ]),
  data.frame(game = "metanorms", trait = "boldness",
             value = m_res["boldness", ]),
  data.frame(game = "metanorms", trait = "vengefulness",
             value = m_res["vengefulness", ]))
d$game <- factor(d$game, levels = c("norms", "metanorms"))

fig <- ggplot(d, aes(trait, value, fill = game)) +
  stat_summary(fun = mean, geom = "col", position = "dodge") +
  theme_minimal() +
  labs(title = "Axelrod: the metanorm is what makes the norm hold",
       subtitle = "mean over the last 10 of 100 generations, 20 seeds; scale 0-7",
       x = NULL, y = "mean trait value", fill = NULL)
print(fig)
ggsave(fig_file("30-norms-and-metanorms.png"), fig, width = 6, height = 4,
       dpi = 120)
