# 25. Axelrod's cultural dissemination  (Axelrod 1997, J. Confl. Resolut. 41:
#     203-226)
#
# Homophily produces stable cultural regions, and the number of them rises with
# q: more variety means *less* convergence, not more.
#
# The random-key trick is the reusable idea here. "Pick one feature at random
# among those that differ" is expressible exactly, rather than approximated:
# give each differing feature a random key and copy the one with the highest.
library(tidyABM)

axelrod_run <- function(q, n = 400, ticks = 400, seed = 3) {
  cols <- setNames(lapply(1:5, function(i)
    rlang::new_formula(NULL, rlang::expr(sample(1:!!q, n, replace = TRUE)))),
    paste0("f", 1:5))

  pop <- abm_setup(agents  = do.call(abm_agents, c(list(n = n), cols)),
                   network = abm_network(type = "random", degree = 4))

  go <- abm_go(
    abm_match(pair = "network"),
    abm_rules(similarity ~ ((f1 == partner_f1) + (f2 == partner_f2) +
                            (f3 == partner_f3) + (f4 == partner_f4) +
                            (f5 == partner_f5)) / 5),
    abm_rules(interact ~ similarity < 1 & runif(n()) < similarity),
    # choose uniformly among the features they differ on, by random key
    abm_rules(k1 ~ if_else(f1 != partner_f1, runif(n()), -1),
              k2 ~ if_else(f2 != partner_f2, runif(n()), -1),
              k3 ~ if_else(f3 != partner_f3, runif(n()), -1),
              k4 ~ if_else(f4 != partner_f4, runif(n()), -1),
              k5 ~ if_else(f5 != partner_f5, runif(n()), -1)),
    abm_rules(best ~ pmax(k1, k2, k3, k4, k5)),
    abm_rules(f1 ~ if_else(interact & k1 == best, partner_f1, f1),
              f2 ~ if_else(interact & k2 == best, partner_f2, f2),
              f3 ~ if_else(interact & k3 == best, partner_f3, f3),
              f4 ~ if_else(interact & k4 == best, partner_f4, f4),
              f5 ~ if_else(interact & k5 == best, partner_f5, f5))
  )
  abm_run(pop, go, ticks = ticks, seed = seed)
}

cultures <- function(r, t) {
  d <- r[r$tick == t, c("f1", "f2", "f3", "f4", "f5")]
  length(unique(do.call(paste, c(d, sep = "-"))))
}

cat("400 agents, 4-regular network, 5 features, 400 ticks\n\n")
cat(sprintf("%-6s %12s %12s\n", "q", "at tick 0", "at tick 400"))
sweep <- do.call(rbind, lapply(c(2, 5, 10), function(q) {
  r <- axelrod_run(q)
  row <- data.frame(q = q, start = cultures(r, 0), end = cultures(r, 400))
  cat(sprintf("%-6d %12d %12d\n", row$q, row$start, row$end))
  row
}))
cat("\nmore traits per feature means fewer chances to be similar enough to\n")
cat("interact, so variety survives: the count of distinct cultures rises with q\n")

# --- figure ---------------------------------------------------------------
# Parameter sweep: distinct cultures against q, before and after.
library(ggplot2)

fig_file <- function(name) {
  a <- grep("^--file=", commandArgs(), value = TRUE)
  d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
  dir.create(file.path(d, "..", "figures"), showWarnings = FALSE)
  file.path(d, "..", "figures", name)
}

d <- rbind(data.frame(q = sweep$q, when = "tick 0",   cultures = sweep$start),
           data.frame(q = sweep$q, when = "tick 400", cultures = sweep$end))
d$when <- factor(d$when, levels = c("tick 0", "tick 400"))

p <- ggplot(d, aes(q, cultures, colour = when)) +
  geom_line(linewidth = 0.7) + geom_point() +
  theme_minimal() +
  labs(title = "Cultural dissemination: more variety, less convergence",
       subtitle = "distinct culture vectors among 400 agents",
       x = "traits per feature (q)", y = "distinct cultures", colour = NULL)
print(p)
ggsave(fig_file("25-axelrod-s-cultural-dissemination.png"), p,
       width = 6, height = 4, dpi = 120)
