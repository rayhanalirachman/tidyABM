# 13. Bank Reserves  (Wilensky, NetLogo Social Science)
#
# Exchange cash pairwise, settle up, then borrow from the bank while it has
# lendable reserves. `abm_sequential()` is the point: lendable reserves are
# *depleted*, so the first borrower has to change what the second one sees.
library(tidyABM)

reserve_ratio <- 0.1

bankres <- abm_setup(
  agents  = abm_agents(n = 100, wallet = ~runif(n, 0, 50), savings = 0,
                       loan = 0, draw = 0),
  globals = list(bank_deposits = 0, bank_loans = 0, bank_reserves = 0))

go <- abm_go(
  abm_match(pair = "random", role = list(giver = TRUE, receiver = TRUE)),
  abm_rules(gift ~ sample(c(0, 2, 5), 1)),
  abm_rules(wallet ~ if_else(.role == "giver", wallet - gift,
                             wallet + partner_gift)),
  abm_rules(
    savings ~ if_else(wallet > 0, savings + wallet,
                      savings - pmin(savings, abs(wallet))),
    wallet  ~ if_else(wallet > 0, 0, wallet + pmin(savings, abs(wallet)))),
  abm_global(bank_deposits ~ sum(savings)),
  abm_sequential(
    draw          ~ if_else(wallet < 0 & bank_reserves > 0,
                            pmin(-wallet, bank_reserves), 0),
    loan          ~ loan + draw,
    wallet        ~ wallet + draw,
    bank_reserves ~ bank_reserves - draw),
  abm_global(bank_loans    ~ sum(loan)),
  abm_global(bank_reserves ~ bank_deposits * reserve_ratio - bank_loans)
)

result <- abm_run(bankres, go, ticks = 100, seed = 13)
g <- abm_globals(result)

cat("100 agents, reserve ratio 0.1, 100 ticks\n\n")
cat(sprintf("%-6s %14s %12s %14s\n", "tick", "deposits", "loans", "reserves"))
for (t in c(0, 1, 5, 10, 25, 50, 100)) {
  r <- g[g$tick == t, ]
  cat(sprintf("%-6d %14.0f %12.0f %14.0f\n", r$tick, r$bank_deposits,
              r$bank_loans, r$bank_reserves))
}
last <- g[g$tick == 100, ]
cat(sprintf("\nmoney multiplier: deposits / (deposits - loans) = %.2f\n",
            last$bank_deposits / (last$bank_deposits - last$bank_loans)))
cat(sprintf("the reserve ratio of %.2f caps lending at %.0f, and loans stand at %.0f\n",
            reserve_ratio, last$bank_deposits * reserve_ratio, last$bank_loans))

# --- figure ---------------------------------------------------------------
library(ggplot2)

fig_file <- function(name) {
  a <- grep("^--file=", commandArgs(), value = TRUE)
  d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
  dir.create(file.path(d, "..", "figures"), showWarnings = FALSE)
  file.path(d, "..", "figures", name)
}

d <- rbind(
  data.frame(tick = g$tick, series = "deposits", value = g$bank_deposits),
  data.frame(tick = g$tick, series = "loans",    value = g$bank_loans),
  data.frame(tick = g$tick, series = "reserves", value = g$bank_reserves))

p <- ggplot(d, aes(tick, value, colour = series)) +
  geom_line(linewidth = 0.7) +
  theme_minimal() +
  labs(title = "Bank Reserves: lending runs into the reserve ratio",
       subtitle = "sequential service is what makes depletion visible",
       x = "tick", y = "dollars", colour = NULL)
print(p)
ggsave(fig_file("13-bank-reserves.png"), p, width = 6, height = 4, dpi = 120)
