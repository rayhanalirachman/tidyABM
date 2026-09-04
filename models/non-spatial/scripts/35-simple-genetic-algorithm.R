library(tidyABM)
library(rlang)

# 35. Simple Genetic Algorithm (Wilensky 1998, NetLogo Computer Science) ----
# Bit-string chromosomes evolve toward all-ones under tournament selection,
# single-point crossover and per-bit mutation. The package has no vector-valued
# agent column, so the chromosome is L columns and the rules are built with
# `do.call()`.

bits <- function(L) paste0("b", seq_len(L))

ga <- function(L = 20, N = 100, mutation = 0.03, crossover = 0.7,
               generations = 100, seed = 1) {
  bs <- bits(L)

  start <- setNames(
    lapply(bs, function(nm) new_formula(NULL, quote(sample(0:1, n, replace = TRUE)))),
    bs)
  pop <- abm_setup(
    agents  = do.call(abm_agents, c(list(n = N), start)),
    globals = list(mut = mutation, xover = crossover, L = L),
    seed    = seed
  )

  fitness_rule <- new_formula(sym("fitness"),
                              Reduce(function(a, b) call2("+", a, b), lapply(bs, sym)))

  # one rule per bit: take the parent's bit, switching parents at the crossover
  # point. All L rules live in one `abm_rules()` call, so every one of them
  # reads the *old* generation.
  child_rules <- lapply(bs, function(nm) {
    new_formula(sym(nm), expr(
      if_else(!sexual, (!!sym(nm))[p1],
              if_else(!!which(bs == nm) <= cross, (!!sym(nm))[p1], (!!sym(nm))[p2]))
    ))
  })
  mutate_rules <- lapply(bs, function(nm) {
    new_formula(sym(nm), expr(if_else(runif(n()) < mut, 1L - (!!sym(nm)), (!!sym(nm)))))
  })

  # tournament of three: the fittest of three random draws becomes a parent
  tournament <- function(out) {
    list(
      abm_rules(t1 ~ sample(n(), n(), replace = TRUE),
                t2 ~ sample(n(), n(), replace = TRUE),
                t3 ~ sample(n(), n(), replace = TRUE)),
      abm_rules(new_formula(sym(out), expr(
        if_else(fitness[t1] >= fitness[t2] & fitness[t1] >= fitness[t3], t1,
                if_else(fitness[t2] >= fitness[t3], t2, t3)))))
    )
  }

  go <- do.call(abm_go, c(
    list(abm_rules(fitness_rule)),
    tournament("p1"), tournament("p2"),
    list(abm_rules(cross  ~ sample(L, n(), replace = TRUE),
                   sexual ~ runif(n()) < xover)),
    list(do.call(abm_rules, child_rules)),
    list(do.call(abm_rules, mutate_rules)),
    list(abm_rules(fitness_rule))
  ))

  abm_run(pop, go, ticks = generations, seed = seed)
}

r <- ga()
best <- tapply(r$fitness, r$tick, max)
mean_fit <- tapply(r$fitness, r$tick, mean)
cat("best fitness by generation :", best[c(6, 11, 21, 51, 101)], "of 20\n")
cat("mean fitness by generation :", round(mean_fit[c(6, 11, 21, 51, 101)], 1), "\n")
cat("first generation at the optimum:",
    names(best)[which(best == 20)[1]], "\n")

for (mu in c(0.01, 0.03, 0.1, 0.3)) {
  rr <- ga(mutation = mu)
  fin <- rr[rr$tick == max(rr$tick), ]
  cat(sprintf("mutation %.2f  best %2d  mean %.1f\n",
              mu, max(fin$fitness), mean(fin$fitness)))
}

# --- figure ---------------------------------------------------------------
# Time series: best and mean fitness by generation, against the optimum of 20.
library(ggplot2)

fig_file <- function(name) {
  a <- grep("^--file=", commandArgs(), value = TRUE)
  d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
  dir.create(file.path(d, "..", "figures"), showWarnings = FALSE)
  file.path(d, "..", "figures", name)
}

d <- rbind(
  data.frame(generation = as.integer(names(best)),     what = "best",
             fitness = as.numeric(best)),
  data.frame(generation = as.integer(names(mean_fit)), what = "mean",
             fitness = as.numeric(mean_fit)))
d <- d[!is.na(d$fitness), ]

fig <- ggplot(d, aes(generation, fitness, colour = what)) +
  geom_hline(yintercept = 20, linetype = "dashed", colour = "grey50") +
  geom_line(linewidth = 0.7) +
  theme_minimal() +
  labs(title = "Simple GA: 20-bit chromosomes climb to all-ones",
       subtitle = "100 organisms, tournament of three, 3% per-bit mutation",
       x = "generation", y = "fitness (bits set)", colour = NULL)
print(fig)
ggsave(fig_file("35-simple-genetic-algorithm.png"), fig, width = 6, height = 4,
       dpi = 120)
