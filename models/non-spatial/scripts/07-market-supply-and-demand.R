# 7. Market, bilateral bargaining
#
# One seller, two buyers, private limit prices and a public expected price each.
# `opposite_group` matching forms exactly one pair per tick, so one buyer is
# always idle -- and the `.scope = "population"` step is what lets the idle one
# keep bidding itself up while it waits.
library(tidyABM)

market <- abm_setup(agents = list(
  seller = abm_agents(n = 1, limit_price = 20, expected_price = 20,
                      transacted = FALSE, deal_price = NA_real_),
  buyer  = abm_agents(n = 2, limit_price = c(40, 35), expected_price = c(30, 25),
                      transacted = FALSE, deal_price = NA_real_)
))

go <- abm_go(
  abm_rules(transacted ~ FALSE, deal_price ~ NA_real_),

  abm_match(pair = "opposite_group", by = .group),

  abm_rules(
    transacted ~ case_when(
      .group == "seller" & partner_expected_price >= expected_price ~ TRUE,
      .group == "buyer"  & expected_price >= partner_expected_price ~ TRUE,
      TRUE ~ FALSE
    ),
    deal_price ~ if_else(
      (.group == "seller" & partner_expected_price >= expected_price) |
      (.group == "buyer"  & expected_price >= partner_expected_price),
      (expected_price + partner_expected_price) / 2, NA_real_
    )
  ),

  abm_rules(
    expected_price ~ case_when(
      .group == "seller" &  transacted ~ expected_price + 2,
      .group == "seller" & !transacted ~ pmax(expected_price - 1, limit_price),
      .group == "buyer"  &  transacted ~ expected_price - 1,
      .group == "buyer"  & !transacted ~ pmin(expected_price + 1, limit_price),
      TRUE ~ expected_price
    ),
    .scope = "population"
  )
)

result <- abm_run(market, go, ticks = 50, seed = 123)

sell <- result[result$.group == "seller", c("tick", "deal_price",
                                            "expected_price", "transacted")]
sell <- sell[order(sell$tick), ]

cat("1 seller (limit 20), 2 buyers (limits 40 and 35), 50 ticks\n\n")
cat(sprintf("%-6s %12s %14s\n", "tick", "deal price", "seller ask"))
for (t in c(0, 5, 10, 20, 30, 40, 50)) {
  r <- sell[sell$tick == t, ]
  cat(sprintf("%-6d %12s %14.1f\n", t,
              if (is.na(r$deal_price)) "-" else sprintf("%.1f", r$deal_price),
              r$expected_price))
}
traded <- sell[!is.na(sell$deal_price), ]
cat(sprintf("\nticks with a trade: %d of 51\n", nrow(traded)))
cat(sprintf("deal price %.1f -> %.1f (it drifts up, it does not settle)\n",
            traded$deal_price[1], traded$deal_price[nrow(traded)]))
cat(sprintf("seller's ask %.1f -> %.1f, against a limit of 20\n",
            sell$expected_price[1], sell$expected_price[nrow(sell)]))

# --- figure ---------------------------------------------------------------
library(ggplot2)

fig_file <- function(name) {
  a <- grep("^--file=", commandArgs(), value = TRUE)
  d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
  dir.create(file.path(d, "..", "figures"), showWarnings = FALSE)
  file.path(d, "..", "figures", name)
}

p <- ggplot(sell, aes(tick, deal_price)) +
  geom_point(size = 1.2, na.rm = TRUE) +
  theme_minimal() +
  labs(title = "Market: the transaction price drifts up, it does not settle",
       subtitle = "one seller with two buyers concedes more slowly than it takes",
       x = "tick", y = "transaction price")
print(p)
ggsave(fig_file("07-market-supply-and-demand.png"), p,
       width = 6, height = 4, dpi = 120)
