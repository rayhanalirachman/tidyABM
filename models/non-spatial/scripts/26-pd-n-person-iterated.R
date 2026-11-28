# 26. PD N-Person Iterated  (NetLogo Social Science, PD N-Person Iterated)
#
# Per-opponent memory is a set per agent, the `.id`s of everyone who has
# defected on me, held in one list column and read with `.partner`.
#
# Reported: the three matchups, and in particular tit-for-tat's crossover
# against defect, which is the model's signature curve.
library(tidyABM)

pdn_run <- function(strategies, ticks = 400, seed = 5) {
  pop <- abm_setup(agents = abm_agents(
    n = length(strategies), strategy = strategies, score = 0, games = 0,
    defect_now = FALSE, grudges = ~vector("list", n)))

  go <- abm_go(
    abm_match(pair = "random"),
    abm_rules(remembered ~ mapply(function(g, p) p %in% g, grudges, .partner)),
    abm_rules(defect_now ~ case_when(
      strategy == "defect"    ~ TRUE,
      strategy == "cooperate" ~ FALSE,
      strategy == "random"    ~ runif(n()) < 0.5,
      TRUE                    ~ remembered)),
    abm_rules(payoff ~ case_when(
      !defect_now & !partner_defect_now ~ 3,
      !defect_now &  partner_defect_now ~ 0,
       defect_now & !partner_defect_now ~ 5,
      TRUE                              ~ 1)),
    abm_rules(score ~ score + payoff, games ~ games + 1),
    # unforgiving latches a grudge; tit-for-tat also forgives
    abm_rules(grudges ~ Map(function(g, p, d, s) {
      if (s == "unforgiving")      if (d) union(g, p) else g
      else if (s == "tit-for-tat") if (d) union(g, p) else setdiff(g, p)
      else g
    }, grudges, .partner, partner_defect_now, strategy))
  )
  abm_run(pop, go, ticks = ticks, seed = seed)
}

half <- function(a, b) rep(c(a, b), each = 12)

# average payoff = cumulative score / cumulative games
avg <- function(r, strat, t) {
  d <- r[r$tick == t & r$strategy == strat, ]
  sum(d$score) / sum(d$games)
}

cat("N = 24, T=5 R=3 P=1 S=0, 400 ticks; average payoff = score / games\n\n")

cd <- pdn_run(half("cooperate", "defect"))
cat(sprintf("cooperate vs defect      defect %.2f, cooperate %.2f\n",
            avg(cd, "defect", 400), avg(cd, "cooperate", 400)))

tc <- pdn_run(half("tit-for-tat", "cooperate"))
cat(sprintf("tit-for-tat vs cooperate TFT %.2f, cooperate %.2f\n",
            avg(tc, "tit-for-tat", 400), avg(tc, "cooperate", 400)))

td <- pdn_run(half("tit-for-tat", "defect"))
cat("tit-for-tat vs defect\n")
cat(sprintf("  %-8s %8s %8s\n", "tick", "defect", "TFT"))
for (t in c(5, 20, 100, 400)) {
  cat(sprintf("  %-8d %8.2f %8.2f\n", t, avg(td, "defect", t),
              avg(td, "tit-for-tat", t)))
}

all6 <- pdn_run(rep(c("random", "cooperate", "defect", "tit-for-tat",
                      "unforgiving", "unknown"), each = 4))
cat("\nall six strategies together, tick 400\n")
for (s in c("unforgiving", "tit-for-tat", "defect", "unknown", "random",
            "cooperate")) {
  cat(sprintf("  %-14s %6.2f\n", s, avg(all6, s, 400)))
}
cat("\nOne honest difference from NetLogo: there, agents wander a 441-patch world\n")
cat("and meet sparsely, so opponents recur rarely. Here everyone is paired\n")
cat("every tick out of 24, so each pair meets roughly every 23 ticks and the\n")
cat("retaliatory strategies get enough encounters to learn. The two\n")
cat("implementations are answering slightly different questions.\n")

# --- figure ---------------------------------------------------------------
# The crossover: TFT starts behind a defector and overtakes as it learns each
# individual one.
library(ggplot2)

.file <- grep("^--file=", commandArgs(), value = TRUE)
source(file.path(dirname(sub("^--file=", "", c(.file, "x")[1])), "../../fig_file.R"))

ticks <- sort(unique(td$tick)); ticks <- ticks[ticks > 0]
d <- do.call(rbind, lapply(c("defect", "tit-for-tat"), function(s)
  data.frame(strategy = s, tick = ticks,
             payoff = vapply(ticks, function(t) avg(td, s, t), numeric(1)))))

p <- ggplot(d, aes(tick, payoff, colour = strategy)) +
  geom_line(linewidth = 0.7) +
  theme_minimal() +
  labs(title = "PD N-person: tit-for-tat starts behind and overtakes",
       subtitle = "cumulative average payoff, 12 TFT against 12 defectors",
       x = "tick", y = "average payoff", colour = NULL)
print(p)
ggsave(fig_file("26-pd-n-person-iterated.png"), p, width = 6, height = 4,
       dpi = 120)
