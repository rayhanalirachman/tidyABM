# 2. Elementary 1-D cellular automaton  (CA 1D Elementary, Wilensky 1998)
#
# The one model that needs L1. A 1-D lattice whose rule reads an *ordered*
# triple (left, self, right): the two neighbours are not interchangeable, so
# `.where` picks each one out by name.
#
# Reported: rule 90's row t against Pascal's triangle mod 2, exactly.

suppressPackageStartupMessages(source("_setup.R", chdir = TRUE))

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
