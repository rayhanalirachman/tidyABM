# 15. Ethnocentrism  (Hammond & Axelrod 2006, J. Confl. Resolut. 50: 926-936)
#
# Two strategy bits -- cooperate with your own tag, cooperate with others -- and
# local reproduction, which is what makes "same tag" start predicting "will
# cooperate with me". The well-mixed condition is the paper's own control, and
# egoists win it.
#
# Reported: the four strategy shares at tick 400, means over five population
# draws, in both conditions.
library(tidyABM)

COST <- 0.01; BENEFIT <- 0.03; BASE_PTR <- 0.12; DEATH <- 0.10; CAPACITY <- 800

random_traits <- list(
  tag      ~ sample(c("red", "blue"), n(), replace = TRUE),
  coop_in  ~ sample(c(TRUE, FALSE),   n(), replace = TRUE),
  coop_out ~ sample(c(TRUE, FALSE),   n(), replace = TRUE))

run_one <- function(local = TRUE, seed = 1, ticks = 400) {
  pop <- abm_setup(
    agents  = abm_agents(n = 400,
                         tag      = ~sample(c("red", "blue"), n, replace = TRUE),
                         coop_in  = ~sample(c(TRUE, FALSE), n, replace = TRUE),
                         coop_out = ~sample(c(TRUE, FALSE), n, replace = TRUE),
                         ptr      = BASE_PTR),
    network = abm_network(type = "random", degree = 4),
    seed    = seed)

  # the only difference between the two conditions: where a newborn is put
  birth_attach <- if (local) {
    abm_match(pair = "network", from = "parent")
  } else {
    abm_match(pair = "network")
  }

  go <- abm_go(
    abm_match(pair = "network"),
    abm_rules(give ~ if_else(partner_tag == tag, coop_in, coop_out)),
    abm_rules(ptr ~ BASE_PTR - COST * give + BENEFIT * partner_give),
    abm_birth(when = runif(n()) < ptr, links = 4, attach_via = birth_attach),
    abm_death(when = runif(n()) <
                DEATH + 0.25 * pmax(0, (n() - CAPACITY) / CAPACITY)),
    abm_birth(n = 8, links = 4, inherit = random_traits,
              attach_via = abm_match(pair = "network"))
  )
  abm_run(pop, go, ticks = ticks, seed = seed)
}

label <- function(ci, co) ifelse(ci & co, "altruist",
                          ifelse(ci & !co, "ethnocentric",
                          ifelse(!ci & co, "traitor", "egoist")))
kinds <- c("ethnocentric", "altruist", "egoist", "traitor")

shares_at <- function(r, t) {
  d <- r[r$tick == t, ]
  k <- label(d$coop_in, d$coop_out)
  vapply(kinds, function(x) mean(k == x), numeric(1))
}

cat("400 agents, 4-regular network, 400 ticks, five population draws\n\n")
tab <- do.call(rbind, lapply(c(FALSE, TRUE), function(loc) {
  s <- vapply(1:5, function(sd) shares_at(run_one(loc, sd), 400), numeric(4))
  data.frame(condition = if (loc) "local" else "well mixed",
             t(rowMeans(s)))
}))
cat(sprintf("%-12s %14s %10s %8s %9s\n", "", kinds[1], kinds[2], kinds[3],
            kinds[4]))
for (i in 1:2) {
  cat(sprintf("%-12s %14.3f %10.3f %8.3f %9.3f\n", tab$condition[i],
              tab$ethnocentric[i], tab$altruist[i], tab$egoist[i],
              tab$traitor[i]))
}
cat(sprintf("\nlocal reproduction takes egoism from %.2f to %.2f\n",
            tab$egoist[1], tab$egoist[2]))

# the network the run is actually on, which is what `links = 4` is for
r <- run_one(TRUE, 1)
e <- abm_edges(r)
last <- r[r$tick == 400, ]
deg <- table(factor(c(e$from, e$to), levels = last$.id))
tag <- setNames(last$tag, last$.id)
cat(sprintf("mean degree at tick 400: %.2f, isolated %.0f%%, same-tag edges %.0f%%\n",
            mean(deg), 100 * mean(deg == 0),
            100 * mean(tag[as.character(e$from)] == tag[as.character(e$to)],
                       na.rm = TRUE)))

# --- figure ---------------------------------------------------------------
# Final-state comparison: the four strategy shares in each condition.
library(ggplot2)

fig_file <- function(name) {
  a <- grep("^--file=", commandArgs(), value = TRUE)
  d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
  dir.create(file.path(d, "..", "figures"), showWarnings = FALSE)
  file.path(d, "..", "figures", name)
}

d <- data.frame(
  condition = factor(rep(tab$condition, each = 4),
                     levels = c("well mixed", "local")),
  strategy  = factor(rep(kinds, 2), levels = kinds),
  share     = c(unlist(tab[1, kinds]), unlist(tab[2, kinds])))

p <- ggplot(d, aes(strategy, share, fill = condition)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(title = "Ethnocentrism: local reproduction is what kills egoism",
       subtitle = "share of the population at tick 400, mean of five draws",
       x = NULL, y = "share of population", fill = NULL)
print(p)
ggsave(fig_file("15-ethnocentrism-hammond-axelrod.png"), p,
       width = 6, height = 4, dpi = 120)
