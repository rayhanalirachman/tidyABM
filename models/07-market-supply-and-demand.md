# 7. Market, supply and demand (after Primer)

**Concept**

- Setup: 200 buyers with a private `wtp` and an opening `offer`; 200 sellers with
  a private `wta` and an opening `ask`
- Go: pair across the two groups, meet in the middle where both sides can live
  with the midpoint, adjust next tick's opening position depending on whether
  you traded
- Output: transaction prices converge on the clearing price

**Package**

```r
market <- abm_setup(agents = list(
  buyers  = abm_agents(n = 200, wtp = ~rnorm(n, 50, 10), offer = ~wtp * 0.8),
  sellers = abm_agents(n = 200, wta = ~rnorm(n, 40, 10), ask   = ~wta * 1.2)))

go <- abm_go(
  abm_match(pair = "opposite_group", by = .group),
  abm_rules(price  ~ (offer + partner_ask) / 2,
            price  ~ (partner_offer + ask) / 2),
  abm_rules(traded ~ price <= wtp & price >= partner_wta,
            traded ~ price >= wta & price <= partner_wtp),
  abm_rules(price  ~ if_else(traded, price, NA_real_)),
  abm_rules(offer  ~ pmin(if_else(traded, offer * 0.98, offer * 1.02), wtp)),
  abm_rules(ask    ~ pmax(if_else(traded, ask * 1.02,   ask * 0.98),   wta))
)

result <- abm_run(market, go, ticks = 200, seed = 7)
```

*Introduced several agent groups and rule routing by column existence. Every step
here relies on it: `price ~ (offer + partner_ask) / 2` mentions a column only
buyers have, so it applies only to buyers, and the rule beside it, with the same
target, applies only to sellers. No `type ==` test, no merging of the two
tibbles, and two rules writing one column is how a pair of groups say the same
thing from their own side.*

*A bilateral trade is the midpoint of the two opening positions, and it happens
when that midpoint is one both sides can live with: at or below the buyer's `wtp`
and at or above the seller's `wta`. The `pmin`/`pmax` on the last two rules are
the part worth keeping: without them a run of unlucky ticks walks an agent's
opening position past its own reservation value, and agents that bid above what
the good is worth to them push the mean price away from the clearing price
instead of onto it.*

---

← [6. Party](06-party-segregation-without-geography.md) · [all models](README.md) · [8. Voter Model on a network](08-voter-model-on-a-network.md) →
