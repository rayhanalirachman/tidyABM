# 54. Indirect reciprocity by image scoring (Nowak & Sigmund 1998)

**Concept**

- Setup: 100 players, each with a strategy `k` and a private opinion of
  everyone else's standing
- Go: players are paired as donor and recipient; the donor helps if it thinks
  the recipient's image score is at least `k`, paying `c` so the recipient
  gains `b`; the recipient and a random audience of `q·n` onlookers see what
  the donor did and adjust their own opinion of it
- Output: whether helping survives, as a function of how many people are
  watching

**Package**

```r
n <- 100; rounds <- 40; n_obs <- 10; b <- 1; cost <- 0.1; mu <- 0.01

m <- abm_setup(
  agents = abm_agents(n = n, k = ~sample(-5:6, n, replace = TRUE),
                      view  = ~lapply(seq_len(n), function(i) rep(0, n)),
                      inbox = ~vector("list", n), payoff = 0,
                      helped = FALSE, aud = ~vector("list", n)),
  seed   = 1)

round <- abm_repeat(
  abm_rules(inbox ~ vector("list", n()),
            aud ~ lapply(seq_len(n()), function(i) sample(n(), n_obs)),
            .scope = "population"),
  abm_match(pair = "random", role = list(donor = TRUE, recipient = TRUE)),
  abm_rules(helped ~ rep(img_of(view, .partner)[which(.role == "donor")[[1]]] >=
                         k[which(.role == "donor")[[1]]], n())),
  abm_rules(payoff ~ payoff + if_else(.role == "donor", -cost * helped, b * helped)),
  # the recipient always sees it; so does a random audience of q*n others
  abm_tell(inbox ~ Map(function(i, h) list(i, h), .id, helped),
           to = Map(function(a, p) unique(c(a, p)), aud, .partner),
           when = .role == "donor", .resolve = "collect"),
  abm_rules(view ~ Map(apply_obs, view, inbox), .scope = "population"),
  max = rounds
)

# a generation is one pass of `round`, then reproduction in proportion to payoff
go <- abm_go(
  round,
  abm_rules(pick ~ sample(n(), n(), replace = TRUE,
                          prob = payoff - min(payoff) + 1e-6), .scope = "population"),
  abm_rules(k ~ ifelse(runif(n()) < mu, sample(-5:6, n(), replace = TRUE), k[pick]),
            .scope = "population"),
  abm_rules(payoff ~ 0, view ~ lapply(seq_len(n()), function(i) rep(0, n())),
            .scope = "population"),
  abm_global(mean_k ~ mean(k), coop ~ mean(k <= 0))
)

result <- abm_run(m, go, ticks = 40, seed = 1)
```

**Result** (n = 100, 40 rounds per generation, 40 generations, 3 seeds, b = 1, c = 0.1)

| q | mean k | fraction with k ≤ 0 |
|---|---|---|
| 0.02 | 3.03 | 0.005 |
| 0.05 | 0.37 | 0.732 |
| 0.08 | −1.19 | 0.894 |
| 0.10 | −1.95 | 0.976 |
| 0.15 | −2.15 | 0.971 |
| 0.50 | −2.56 | 0.970 |

Nowak and Sigmund's condition for indirect reciprocity to pay is `q > c/b`,
which here is 0.1, and the table crosses over exactly there: with 2% of the
population watching, the strategy that survives is `k = 3`, which is to help
almost nobody. With 10% watching, 98% of the population will help anyone whose
standing is not actively bad. Above the threshold nothing much changes.
Watching harder than you need to buys very little.

What makes it indirect is that the donor is never repaid by the person it
helped. It is repaid by whoever happens to be its donor later, and that person
helps because it saw, or heard about, what this one did. The audience is the
entire mechanism, and `q` is a measure of how much of a community is a
community.

*Forced `abm_tell(to = <a set>)`. What a donor does has to reach a group of
agents that is neither its partner nor its neighbours, a fresh random audience
each round, and `to =` accepted exactly one recipient or the whole
neighbourhood. It now accepts a list column of ids, so
`to = Map(function(a, p) unique(c(a, p)), aud, .partner)` sends the same message
to the audience and the recipient. Reputation, gossip and broadcast media are
all this shape, and none of them is a network in the sense
`abm_network()` means: the audience is different every time.*

*Forced `.resolve = "collect"`. An onlooker may see several interactions in the
same round, and every existing `.resolve` throws all but one of them away or
adds them up. `"collect"` hands the recipient the list of what it was told and
lets its own rule decide, here, `Map(apply_obs, view, inbox)` walks the list
and moves the score of each donor it saw. That is more general than any fixed
collision policy, and it is the beginning of an answer to the Open items entry
about `abm_tell()` resolving collisions without ordering them: a recipient
holding all its messages can put them in whatever order it likes.*

*Each agent's `view` is a list column of length n, its private opinion of
everyone, which is the same "list columns already work" finding as models 40
and 41, at a larger size. There is no shared image score anywhere in the model.*

*This is the individual-image version rather than the public-score one. Nowak
and Sigmund's analytic condition `q > c/b` is derived for the variant where `q`
is the probability that a donor knows the recipient's true score. Here `q` is
the fraction of the population that witnesses each interaction, and scores are
built up privately from what each agent saw. The two are the same idea and not
the same parameter, so the agreement of the crossover with `c/b` is a good deal
better than this model is entitled to claim.*

---

**Reproduce:** [`m54_image_scoring.R`](scripts/m54_image_scoring.R)

← [53. Random copying and the neutral model](53-neutral-model.md) · [all models](README.md) · [55. Deferred acceptance](55-deferred-acceptance.md) →
