# 26. PD N-Person Iterated  (NetLogo Social Science, PD N-Person Iterated)
#
# Per-opponent memory, held here as one column per possible opponent: N columns
# for N opponents, so N^2 cells. Workable at N = 24 with `do.call()` and
# hopeless at NetLogo's default of 60. A list column would hold the same grudges
# in one column, the way model 41 holds a strategy table; this is the N^2
# version the page was written with.
#
# Reported: the three matchups, and in particular tit-for-tat's crossover
# against defect, which is the model's signature curve.
library(tidyABM)

N <- 24
f <- function(lhs, rhs) rlang::new_formula(str2lang(lhs), str2lang(rhs))

pdn_run <- function(strategies, ticks = 400, seed = 5) {
  hist <- setNames(rep(list(FALSE), N), paste0("h", 1:N))
  pop <- abm_setup(agents = do.call(abm_agents, c(
    list(n = N, strategy = strategies, score = 0, games = 0,
         defect_now = FALSE), hist)))

  recall <- f("remembered", paste0(
    "case_when(", paste(sprintf(".partner == %d ~ h%d", 1:N, 1:N),
                        collapse = ", "),
    ", TRUE ~ FALSE)"))

  update <- lapply(1:N, function(k) f(paste0("h", k), sprintf(
    "case_when(.partner != %d ~ h%d,
               strategy == 'unforgiving' ~ h%d | partner_defect_now,
               strategy == 'tit-for-tat' ~ partner_defect_now,
               TRUE ~ h%d)", k, k, k, k)))

  go <- abm_go(
    abm_match(pair = "random"),
    abm_rules(recall),
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
    do.call(abm_rules, update)
  )
  abm_run(pop, go, ticks = ticks, seed = seed)
}

half <- function(a, b) rep(c(a, b), each = N / 2)

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

fig_file <- function(name) {
  a <- grep("^--file=", commandArgs(), value = TRUE)
  d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
  dir.create(file.path(d, "..", "figures"), showWarnings = FALSE)
  file.path(d, "..", "figures", name)
}

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
