# 8. Voter Model on a network  (Clifford & Sudbury 1973; Holley & Liggett 1975)
#
# Adopt a random neighbour's opinion. Consensus is certain; how long it takes
# depends on the network, so this runs three degrees.
library(tidyABM)

voter_run <- function(degree, n = 100, ticks = 200, seed = 8) {
  m <- abm_setup(
    agents  = abm_agents(n = n, opinion = ~sample(c(0, 1), n, replace = TRUE)),
    network = abm_network(type = "random", degree = degree))
  go <- abm_go(abm_match(pair = "network"), abm_rules(opinion ~ partner_opinion))
  abm_run(m, go, ticks = ticks, seed = seed)
}

share <- function(r) {
  data.frame(tick = sort(unique(r$tick)),
             p    = as.numeric(tapply(r$opinion, r$tick, mean)))
}

result <- voter_run(4)
s <- share(result)

cat("100 agents, 0/1 opinion, adopt a random neighbour, 200 ticks\n\n")
cat(sprintf("%-6s %14s\n", "tick", "share holding 1"))
for (t in c(0, 5, 20, 50, 100, 200)) {
  cat(sprintf("%-6d %14.2f\n", t, s$p[s$tick == t]))
}
absorbed <- s$tick[s$p %in% c(0, 1)]
if (length(absorbed)) {
  cat(sprintf("\nconsensus on opinion %d, first reached at tick %d\n",
              round(s$p[nrow(s)]), min(absorbed)))
} else {
  cat(sprintf("\nno consensus inside 200 ticks; the share is still wandering (%.2f)\n",
              s$p[nrow(s)]))
}

cat("\nDegree sweep (first tick at consensus, 200-tick budget)\n\n")
sweep <- do.call(rbind, lapply(c(2, 4, 8, 16), function(d) {
  ss <- share(voter_run(d))
  hit <- ss$tick[ss$p %in% c(0, 1)]
  data.frame(degree = d, consensus = if (length(hit)) min(hit) else NA_integer_,
             final = ss$p[nrow(ss)])
}))
cat(sprintf("%-8s %12s %10s\n", "degree", "consensus", "final"))
for (i in seq_len(nrow(sweep))) {
  cat(sprintf("%-8d %12s %10.2f\n", sweep$degree[i],
              if (is.na(sweep$consensus[i])) ">200" else sweep$consensus[i],
              sweep$final[i]))
}

# --- figure ---------------------------------------------------------------
library(ggplot2)

fig_file <- function(name) {
  a <- grep("^--file=", commandArgs(), value = TRUE)
  d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
  dir.create(file.path(d, "..", "figures"), showWarnings = FALSE)
  file.path(d, "..", "figures", name)
}

p <- ggplot(s, aes(tick, p)) +
  geom_line() +
  ylim(0, 1) +
  theme_minimal() +
  labs(title = "Voter model: a random walk that ends in consensus",
       subtitle = "100 agents on a 4-regular network",
       x = "tick", y = "share holding opinion 1")
print(p)
ggsave(fig_file("08-voter-model-on-a-network.png"), p,
       width = 6, height = 4, dpi = 120)
