# 16. Zakah with a risk process
#
# Two fixes to model 12. The poverty line is relative, and -- more importantly
# -- income follows an AR(1) in logs with occasional wealth shocks, so there is
# a persistent lower tail to redistribute against. Without the risk process the
# consumption rule converges every household to 20 * income and nobody is ever
# persistently poor, wherever the line is put.
#
# Reported: the share persistently below half the median, p10 and the Gini,
# with and without the transfer.
library(tidyABM)

NISAB <- 100

zakah_run <- function(transfer = TRUE, ticks = 300, seed = 12) {
  pop <- abm_setup(
    agents  = abm_agents(n = 500, wealth = ~rlnorm(n, 4, 0.5),
                         income = ~rlnorm(n, 3, 0.4)),
    globals = list(zakah_pool = 0, poverty_line = 30))

  shocks <- abm_rules(
    income ~ exp(0.9 * log(income) + 0.1 * 3 + rnorm(n(), 0, 0.15)),  # AR(1) in logs
    wealth ~ wealth - if_else(runif(n()) < 0.03, wealth * 0.6, 0))    # occasional hit

  steps <- list(
    shocks,
    abm_rules(wealth ~ pmax(0.01, wealth + income - (0.6 * income + 0.02 * wealth))),
    abm_global(poverty_line ~ 0.5 * median(wealth)))                  # relative
  if (transfer) {
    steps <- c(steps, list(
      abm_global(zakah_pool ~ sum(if_else(wealth > NISAB, wealth * 0.025, 0))),
      abm_rules(wealth ~ if_else(wealth > NISAB, wealth * 0.975, wealth)),
      abm_rules(wealth ~ if_else(wealth < poverty_line,
                                 wealth + zakah_pool /
                                   pmax(1, sum(wealth < poverty_line)),
                                 wealth))))
  }
  abm_run(pop, do.call(abm_go, steps), ticks = ticks, seed = seed)
}

gini <- function(x) {
  x <- sort(x); n <- length(x)
  sum((2 * seq_len(n) - n - 1) * x) / (n * sum(x))
}

summarise <- function(r) {
  late <- r[r$tick > 200, ]
  line <- tapply(late$wealth, late$tick, function(w) 0.5 * median(w))
  poor <- tapply(seq_len(nrow(late)), late$.id,
                 function(i) mean(late$wealth[i] <
                                    line[as.character(late$tick[i])]))
  final <- r$wealth[r$tick == 300]
  c(persistent = mean(poor > 0.25),
    p10 = unname(quantile(final, 0.10)),
    gini = gini(final))
}

cat("500 households, AR(1) income, 3% chance of a 60% wealth hit, 300 ticks\n")
cat("poverty line = half the median; persistently poor = below it in more\n")
cat("than a quarter of the last 100 ticks\n\n")

base <- summarise(zakah_run(FALSE))
with <- summarise(zakah_run(TRUE))

cat(sprintf("%-12s %14s %10s %8s\n", "", "persistently", "p10", "gini"))
cat(sprintf("%-12s %13.1f%% %10.0f %8.3f\n", "baseline",
            100 * base[["persistent"]], base[["p10"]], base[["gini"]]))
cat(sprintf("%-12s %13.1f%% %10.0f %8.3f\n", "with zakah",
            100 * with[["persistent"]], with[["p10"]], with[["gini"]]))
cat("\nCaveat on calibration: moving 2.5% of nearly everyone's wealth to the few\n")
cat("per cent below the line is an enormous per-head transfer, which is why\n")
cat("relative poverty is eliminated rather than merely reduced.\n")

# --- figure ---------------------------------------------------------------
# Time series of the Gini in both conditions, which is where the transfer's
# effect is visible tick by tick rather than only at the end.
library(ggplot2)

fig_file <- function(name) {
  a <- grep("^--file=", commandArgs(), value = TRUE)
  d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
  dir.create(file.path(d, "..", "figures"), showWarnings = FALSE)
  file.path(d, "..", "figures", name)
}

series <- function(r, lab) data.frame(
  tick = sort(unique(r$tick)), condition = lab,
  gini = as.numeric(tapply(r$wealth, r$tick, gini)))

d <- rbind(series(zakah_run(FALSE), "baseline"),
           series(zakah_run(TRUE),  "with zakah"))

p <- ggplot(d, aes(tick, gini, colour = condition)) +
  geom_line(linewidth = 0.6) +
  theme_minimal() +
  labs(title = "Zakah with a risk process: the transfer holds the Gini down",
       subtitle = "500 households, relative poverty line, 300 ticks",
       x = "tick", y = "Gini coefficient of wealth", colour = NULL)
print(p)
ggsave(fig_file("16-zakah-with-a-risk-process.png"), p,
       width = 6, height = 4, dpi = 120)
