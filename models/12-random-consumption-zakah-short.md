# 12. Random Consumption Zakah, short form (custom)

**Concept**

- Setup: 500 households with `wealth` and `income`; a `zakah_pool` global
- Go: consume, collect 2.5% from everyone above the *nisab*, share it among those
  below a poverty line
- Output: *supposed* to show redistribution changing the wealth distribution

**Package**

```r
NISAB <- 100; poverty_line <- 30

zakah <- abm_setup(
  agents  = abm_agents(n = 300, wealth = ~rlnorm(n, 4, 0.5),
                       income = ~rlnorm(n, 3, 0.4)),
  globals = list(zakah_pool = 0))

go <- abm_go(
  abm_rules(wealth ~ wealth + income - (0.6 * income + 0.02 * wealth)),
  abm_global(zakah_pool ~ sum(if_else(wealth > NISAB, wealth * 0.025, 0))),
  abm_rules(wealth ~ if_else(wealth > NISAB, wealth * 0.975, wealth)),
  abm_rules(wealth ~ if_else(wealth < poverty_line,
                             wealth + zakah_pool / sum(wealth < poverty_line), wealth))
)

result <- abm_run(zakah, go, ticks = 50, seed = 12)
```

*First model made only of individual and population-level steps, which confirmed
matching is optional machinery. **It stops working after about ten ticks** — see
model 16.*

---

← [11. Preferential Attachment](11-preferential-attachment.md) · [all models](README.md) · [13. Bank Reserves](13-bank-reserves.md) →
