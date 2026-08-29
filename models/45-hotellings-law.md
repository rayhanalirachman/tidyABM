# 45. Hotelling's Law (Hotelling 1929; NetLogo Social Science)

**Concept**

- Setup: shops somewhere on a stretch of beach, 400 buyers spread evenly along
  it
- Go: every buyer walks to the nearest shop; every shop tries a step to one
  side and keeps it if more buyers came, otherwise walks back
- Output: where the shops end up, and how far the buyers have to walk

**Package**

```r
step <- 1

m <- abm_setup(
  agents = list(
    shops  = abm_agents(n = 2, x = ~runif(n, 10, 90),
                        x_old = 0, step = step, customers = 0, base = 0),
    buyers = abm_agents(n = 400, x = ~seq(0, 100, length.out = n))),
  seed = 1)

count_buyers <- list(
  abm_rules(customers ~ 0, .scope = "population"),
  abm_match(pair = "nearest", by = x,
            eligible = .group == "buyers", among = .group == "shops"),
  abm_tell(customers ~ 1, to = .partner, when = .group == "buyers",
           .resolve = "sum")
)

go <- do.call(abm_go, c(
  count_buyers,
  list(
    abm_rules(base  ~ customers, .scope = "population"),
    abm_rules(x_old ~ x, .scope = "population"),
    abm_rules(x ~ pmin(100, pmax(0, x + sample(c(-1, 1), n(), TRUE) * step)),
              .scope = "population")
  ),
  count_buyers,
  list(abm_rules(x ~ if_else(customers > base, x, x_old), .scope = "population"))
))

result <- abm_run(m, go, ticks = 300, seed = 1)
```

**Result** (5 seeds, 300 ticks)

| shops | where they end up | mean walk | shortest possible walk |
|---|---|---|---|
| 2 | 50, 51 | 24.8 | 12.5 |
| 3 | 48, 49, 49 | 24.8 | 8.4 |
| 5 | 30, 31, 47, 53, 54 | 17.4 | 5.0 |

Two shops meet in the middle and stay there: Hotelling's *minimum
differentiation*, and the reason the two supermarkets are next door to each
other. Neither can do better anywhere else, and it is the worst arrangement
there is for the buyers, who walk exactly twice as far as they would if the
shops were placed to suit them. Adding shops does not fix it, five shops
cluster into a pair, a pair and a singleton and still leave the buyers walking
three and a half times the optimum.

*Forced `among =`. "The nearest shop" and "the nearest agent" are different
questions, and `abm_match(pair = "nearest")` used to answer only the second: in
a model with two groups a buyer's nearest agent is another buyer, so the mode
was unusable for exactly the models it was meant for. `eligible =` says who
takes part and `among =` says who may be picked, and the two only come apart in
the directional modes. `one_of` gained it at the same time and for the same
reason: "copy a random agent of the other group" was not sayable either.*

*Counting the buyers who chose you is then one line:
`abm_tell(customers ~ 1, to = .partner, .resolve = "sum")`, each buyer sending
its chosen shop a 1. A match gives an agent one partner. That step is what lets
the agent on the receiving end of many matches find out how many.*

*Two things did not work and are worth recording as much as the thing that did.
The shops must compare a step against what they are getting* now*, not against
the best they ever got, hence the block counting buyers twice per tick. A shop
comparing against its historical best freezes the moment a rival moves in next
door, because its old takings have become unreachable and every move looks bad.
And they must require a* strict *improvement: a pair of shops standing together
splits the beach evenly wherever it stands, so a shop that accepted neutral
moves would random-walk the pair into a wall. Neither is a limitation of the
grammar, but both are the kind of thing that makes a hill-climbing ABM quietly
produce a plausible wrong answer.*

*One real limitation showed up here and was closed later. `pair = "nearest"`
had a single fixed metric, Euclidean distance in the `by` columns, and the
price-competition version of this model, d'Aspremont, Gabszewicz & Thisse
(1979), where shops set prices too and the answer flips to* maximum
*differentiation, needs "nearest in price plus travel cost". That is
`cost = price + travel * abs(x - own_x)`, an expression the chooser minimises
instead of a distance, which the garbage can (48) forced and which this model
had asked for first.*

---

**Reproduce:** [`m45_hotelling.R`](scripts/m45_hotelling.R)

← [44. Fairness versus reason in the ultimatum game](44-ultimatum-game.md) · [all models](README.md) · [46. The Beer Distribution Game](46-beer-distribution-game.md) →
