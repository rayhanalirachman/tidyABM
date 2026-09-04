# 24. Sex Ratio Equilibrium  (Fisher 1930; NetLogo Biology)
#
# `abm_birth(inherit =)` is the addition: `cost` applies to parent and child
# alike, so a newborn with a reset age, a mutated trait and a sex drawn at birth
# had no expression before it. `inherit` is evaluated in the parent's row with
# the match standing, which is what makes two-parent inheritance a one-liner.
#
# The Fisherian feedback comes out of the matching, not out of any rule:
# `"opposite_group"` pairs min(n_male, n_female) couples, so when males are
# scarce each male fathers more children.
library(tidyABM)

LONGEVITY <- 8; MATURITY <- 2; MATING <- 0.6; CAP <- 1500

fisher_run <- function(p_male, ticks = 1200, seed = 2) {
  pop <- abm_setup(agents = abm_agents(
    n   = 1000,
    sex = ~sample(c("male", "female"), n, replace = TRUE,
                  prob = c(p_male, 1 - p_male)),
    mcc = ~pmin(0.95, pmax(0.05, rnorm(n, p_male, 0.08))),
    age = ~sample(0:LONGEVITY, n, replace = TRUE)))

  go <- abm_go(
    abm_rules(age ~ age + 1),
    abm_death(when = age > LONGEVITY),
    abm_death(when = runif(n()) < pmax(0, (n() - CAP) / n())),
    abm_match(pair = "opposite_group", by = sex, eligible = age >= MATURITY),
    abm_birth(
      when = sex == "female" & !is.na(.partner) & runif(n()) < MATING,
      inherit = list(
        age ~ 0,
        mcc ~ pmin(0.95, pmax(0.05, (mcc + partner_mcc) / 2 + rnorm(n(), 0, 0.05))),
        sex ~ if_else(runif(n()) < (mcc + partner_mcc) / 2, "male", "female")))
  )
  abm_run(pop, go, ticks = ticks, seed = seed)
}

series <- function(r, lab) data.frame(
  start = lab,
  tick  = sort(unique(r$tick)),
  male  = as.numeric(tapply(r$sex == "male", r$tick, mean)),
  mcc   = as.numeric(tapply(r$mcc, r$tick, mean)))

cat("1000 agents, longevity 8, maturity 2, cap 1500, 1200 ticks\n\n")
runs <- list()
for (p0 in c(0.25, 0.75)) {
  s <- series(fisher_run(p0), sprintf("%.0f%% male", 100 * p0))
  runs[[length(runs) + 1]] <- s
  cat(sprintf("starting at %.0f%% male\n", 100 * p0))
  cat(sprintf("%-6s %10s %10s\n", "tick", "male", "mean mcc"))
  for (t in c(0, 50, 200, 600, 1200)) {
    r <- s[s$tick == t, ]
    cat(sprintf("%-6d %10.2f %10.2f\n", r$tick, r$male, r$mcc))
  }
  cat("\n")
}
cat("from either side, the adult sex ratio converges on 1/2 and the inherited\n")
cat("male-child probability follows it: Fisher's principle\n")

# --- figure ---------------------------------------------------------------
library(ggplot2)

fig_file <- function(name) {
  a <- grep("^--file=", commandArgs(), value = TRUE)
  d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
  dir.create(file.path(d, "..", "figures"), showWarnings = FALSE)
  file.path(d, "..", "figures", name)
}

d <- do.call(rbind, runs)

p <- ggplot(d, aes(tick, male, colour = start)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", colour = "grey50") +
  geom_line(linewidth = 0.6) +
  ylim(0, 1) +
  theme_minimal() +
  labs(title = "Sex ratio: 1/2 from either side, with no rule that says so",
       subtitle = "adult sex ratio, two starting points, 1200 ticks",
       x = "tick", y = "share male", colour = "started at")
print(p)
ggsave(fig_file("24-sex-ratio-equilibrium.png"), p, width = 6, height = 4,
       dpi = 120)
