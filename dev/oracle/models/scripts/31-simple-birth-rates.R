library(tidyABM)

# 31. Simple Birth Rates (Wilensky 1997, NetLogo Biology) -------------------
# Two populations differing only in fertility, sharing one carrying capacity.
# A fertility of 3.4 means "three children, and a 40% chance of a fourth".

birth_rates <- function(red_fertility, blue_fertility, K = 300,
                        generations = 60, seed = 1) {
  pop <- abm_setup(
    agents = abm_agents(
      n      = K,
      colour = ~rep(c("red", "blue"), length.out = n),
      fert   = ~if_else(rep(c("red", "blue"), length.out = n) == "red",
                        red_fertility, blue_fertility),
      parent = FALSE
    ),
    globals = list(K = K),
    seed = seed
  )

  # one clone per whole unit of fertility, plus one more with the leftover
  # probability. `parent` keeps the newborns out of the later birth steps, so
  # nobody becomes a grandparent within a single generation.
  child <- abm_birth(when    = parent & runif(n()) < pmin(fert - born, 1),
                     cost    = born ~ born + 1,
                     inherit = list(parent ~ FALSE, born ~ 0))

  go <- do.call(abm_go, c(
    list(abm_rules(parent ~ TRUE, born ~ 0)),
    rep(list(child), ceiling(max(red_fertility, blue_fertility))),
    list(abm_death(when = rank(runif(n()), ties.method = "random") > K))
  ))

  abm_run(pop, go, ticks = generations, seed = seed)
}

trace <- function(res) {
  tapply(res$colour, res$tick, function(x) sum(x == "red"))
}

r <- birth_rates(3.4, 3.0)
tr <- trace(r)
cat("reds (fert 3.4) vs blues (fert 3.0), out of 300:\n")
print(tr[c(1, 6, 11, 21, 41, 61)])
cat("blue extinct at generation:",
    if (any(tr == 300)) names(tr)[which(tr == 300)[1]] else "not within 60", "\n")

# equal fertility: drift, not selection
tr2 <- trace(birth_rates(3.0, 3.0))
cat("equal fertility, reds after 60 generations:", tr2[["60"]], "\n")

# --- figure ---------------------------------------------------------------
# Time series: the red share under a fertility edge, against equal fertility,
# which is drift rather than selection.
library(ggplot2)

fig_file <- function(name) {
  a <- grep("^--file=", commandArgs(), value = TRUE)
  d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
  dir.create(file.path(d, "..", "figures"), showWarnings = FALSE)
  file.path(d, "..", "figures", name)
}

curve_of <- function(tr, lab) data.frame(
  case = lab, generation = as.integer(names(tr)), reds = as.numeric(tr) / 300)

d <- rbind(curve_of(tr,  "3.4 vs 3.0"),
           curve_of(tr2, "3.0 vs 3.0"))

fig <- ggplot(d, aes(generation, reds, colour = case)) +
  geom_line(linewidth = 0.7) +
  ylim(0, 1) +
  theme_minimal() +
  labs(title = "Simple birth rates: a 13% fertility edge is decisive",
       subtitle = "300 slots, shared carrying capacity, 60 generations",
       x = "generation", y = "share red", colour = "fertility")
print(fig)
ggsave(fig_file("31-simple-birth-rates.png"), fig, width = 6, height = 4,
       dpi = 120)
