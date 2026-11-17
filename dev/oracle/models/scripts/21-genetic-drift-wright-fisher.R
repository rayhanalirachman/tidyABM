# 21. Genetic Drift / Wright-Fisher  (NetLogo GenDrift P Global)
#
# One rule. With no match standing, `abm_rules()` sees the whole population, so
# `sample(allele, n(), replace = TRUE)` is literally the Wright-Fisher operator.
#
# Reported: the five-allele run, and the 120-replicate check that an allele
# starting at frequency p fixes with probability p.
library(tidyABM)

go <- abm_go(abm_rules(allele ~ sample(allele, n(), replace = TRUE)))

drift <- abm_setup(agents = abm_agents(
  n = 200, allele = ~sample(1:5, n, replace = TRUE)))
result <- abm_run(drift, go, ticks = 800, seed = 3)

counts <- do.call(rbind, lapply(1:5, function(a)
  data.frame(allele = factor(a),
             tick = sort(unique(result$tick)),
             n = as.numeric(tapply(result$allele == a, result$tick, sum)))))

cat("200 agents, five alleles, 800 ticks\n\n")
cat(sprintf("%-6s %8s %s\n", "tick", "alive", "counts"))
for (t in c(0, 50, 100, 200, 400, 800)) {
  a <- result$allele[result$tick == t]
  tab <- table(factor(a, levels = 1:5))
  cat(sprintf("%-6d %8d %s\n", t, sum(tab > 0),
              paste(sprintf("%3d", as.integer(tab)), collapse = " ")))
}

cat("\nFixation probability: 120 replicates starting at 30/70\n\n")
fixed <- vapply(1:120, function(s) {
  m <- abm_setup(agents = abm_agents(
    n = 200, allele = ~as.integer(seq_len(n) <= 60)))   # 30% carry allele 1
  r <- abm_run(m, go, ticks = 800, seed = s, record = "final")
  mean(r$allele[r$tick == 800]) > 0.5
}, logical(1))
cat(sprintf("minority allele fixed in %d of 120 runs (%.0f%%); theory says 30%%\n",
            sum(fixed), 100 * mean(fixed)))

# --- figure ---------------------------------------------------------------
library(ggplot2)

fig_file <- function(name) {
  a <- grep("^--file=", commandArgs(), value = TRUE)
  d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
  dir.create(file.path(d, "..", "figures"), showWarnings = FALSE)
  file.path(d, "..", "figures", name)
}

p <- ggplot(counts, aes(tick, n, colour = allele)) +
  geom_line(linewidth = 0.5) +
  theme_minimal() +
  labs(title = "Genetic drift: fixation is certain, the winner is not",
       subtitle = "200 agents, five alleles, resampled every tick",
       x = "tick", y = "agents carrying the allele", colour = "allele")
print(p)
ggsave(fig_file("21-genetic-drift-wright-fisher.png"), p,
       width = 6, height = 4, dpi = 120)
