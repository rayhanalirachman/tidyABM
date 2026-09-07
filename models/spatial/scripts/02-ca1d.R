# 2. Elementary 1-D cellular automaton  (CA 1D Elementary, Wilensky 1998)
#
# The one model that needs L1. A 1-D lattice whose rule reads an *ordered*
# triple (left, self, right): the two neighbours are not interchangeable, so
# `.where` picks each one out by name.
#
# Reported: rule 90's row t against Pascal's triangle mod 2, exactly.

library(tidyABM)

ca <- function(rule_number, w = 121) {
  abm_setup(
    agents  = abm_agents(s = ~as.integer(seq_len(n) == n %/% 2 + 1)),
    network = abm_network(type = "line", dims = w, torus = TRUE),
    globals = list(rule = as.integer(intToBits(rule_number))[1:8])
  )
}
go <- abm_go(
  abm_neighbours(s_l ~ sum(s), .where = "west"),
  abm_neighbours(s_r ~ sum(s), .where = "east"),
  # note `rule[i]`, not `rule[[i]]`: `[[` does not vectorise over a population
  abm_rules(s ~ rule[4 * s_l + 2 * s + s_r + 1])
)

w <- 121; centre <- w %/% 2 + 1
r <- abm_run(ca(90, w), go, ticks = 24, seed = 1)

cat("Rule 90, first 16 rows (the space-time diagram is the result tibble)\n")
for (t in 0:15) {
  row <- r[r$tick == t, ]; row <- row[order(row$.x), ]
  cat("  ", paste(ifelse(row$s[(centre - 16):(centre + 16)] == 1, "#", "."),
                  collapse = ""), "\n", sep = "")
}

ok <- vapply(0:24, function(t) {
  row <- r[r$tick == t, ]; row <- row[order(row$.x), ]
  got  <- as.integer(row$.x[row$s == 1])
  want <- as.integer(sort((centre + 2 * (0:t) - t)[(choose(t, 0:t) %% 2) == 1]))
  identical(got, want)
}, logical(1))
cat(sprintf("\nrow t == choose(t, 0:t) %%%% 2 for every t in 0..24: %s\n", all(ok)))

r30 <- abm_run(ca(30, w), go, ticks = 200, seed = 1)
ctr <- r30$s[r30$.x == centre]
cat(sprintf("Rule 30 centre column, first 60 bits: %s\n",
            paste(ctr[1:60], collapse = "")))

# --- figure ---------------------------------------------------------------
# Time series: the number of live cells in row t, against the number of odd
# entries in Pascal's row t. Rule 90 is Sierpinski, and this is the check.
library(ggplot2)

fig_file <- function(name) {
  a <- grep("^--file=", commandArgs(), value = TRUE)
  d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
  dir.create(file.path(d, "..", "figures"), showWarnings = FALSE)
  file.path(d, "..", "figures", name)
}

rows <- data.frame(
  t = 0:24,
  observed = vapply(0:24, function(t) sum(r$s[r$tick == t]), numeric(1)),
  pascal   = vapply(0:24, function(t) sum(choose(t, 0:t) %% 2 == 1), numeric(1)))

d <- rbind(
  data.frame(t = rows$t, what = "live cells in row t", n = rows$observed),
  data.frame(t = rows$t, what = "odd entries of Pascal row t", n = rows$pascal))

p <- ggplot(d, aes(t, n, colour = what)) +
  geom_line(linewidth = 0.7) + geom_point(size = 1) +
  theme_minimal() +
  labs(title = "Rule 90 is Pascal's triangle mod 2, exactly",
       subtitle = "121-cell ring, single seeded cell, rows 0 to 24",
       x = "row t", y = "live cells", colour = NULL)
print(p)
ggsave(fig_file("02-ca1d.png"), p, width = 6, height = 4, dpi = 120)
