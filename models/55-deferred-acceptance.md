# 55. Deferred acceptance and the stable marriage problem (Gale & Shapley 1962)

**Concept**

- Setup: n men and n women, each with a strict ranking of everyone on the other
  side, drawn uniformly at random
- Go: every man proposes to the best woman who has not yet rejected him — for
  an engaged man, that is his fiancée; every woman hears all her proposals at
  once and keeps the best, rejecting the rest; repeat until nobody is rejected
- Output: how well each side does, measured by the average rank of the partner
  it ends up with

**Package**

```r
n <- 200

m <- abm_setup(
  agents = list(
    men   = abm_agents(n = n, rank = ~rank_over(n, ids_w),
                       worst = 0, fiancee = NA_integer_, win = FALSE),
    women = abm_agents(n = n, rank = ~rank_over(n, ids_m),
                       holder = NA_integer_, best = Inf)),
  seed   = 1)

round <- abm_repeat(
  # every woman starts the round holding nobody and listening
  abm_rules(best ~ Inf, holder ~ NA_integer_, .scope = "population"),
  # every man proposes to the best woman who has not yet rejected him
  abm_match(pair = "nearest",
            cost = if_else(rank_of(own_rank, .id) <= own_worst,
                           NA_real_, rank_of(own_rank, .id)),
            eligible = .group == "men", among = .group == "women"),
  # she hears every proposal at once and keeps the best
  abm_tell(best ~ rank_of(partner_rank, .id), to = .partner,
           when = .group == "men" & !is.na(.partner), .resolve = "min"),
  abm_rules(win ~ !is.na(.partner) & rank_of(partner_rank, .id) == partner_best),
  abm_rules(fiancee ~ if_else(win, .partner, NA_integer_),
            worst   ~ if_else(!is.na(.partner) & !win, rank_of(rank, .partner), worst)),
  abm_tell(holder ~ .id, to = .partner, when = win),
  until = sum(.group == "men" & is.na(fiancee)) == 0,
  max = 5000
)

# the whole algorithm runs to absorption inside a single tick
go <- abm_go(round)

result <- abm_run(m, go, ticks = 1, seed = 1)
```

**Result** (uniform random preferences, men proposing, 5 seeds each)

| n | men's average rank | ln n | women's average rank | n / ln n |
|---|---|---|---|---|
| 25 | 4.11 | 3.22 | 6.4 | 7.8 |
| 50 | 4.80 | 3.91 | 9.7 | 12.8 |
| 100 | 5.35 | 4.61 | 19.1 | 21.7 |
| 200 | 5.54 | 5.30 | 37.7 | 37.7 |

Everyone is matched in every run, and the matching is stable — no man and
woman both prefer each other to what they got, because a man only stops
proposing to a woman after she has turned him down for someone she likes
better. The numbers are Pittel's asymptotics: the proposing side averages
about `ln n` and the other side about `n / ln n`, and by n = 200 both are on
the nose. With two hundred candidates the men end up with roughly their sixth
choice and the women with roughly their thirty-eighth. Being the side that
asks is worth an order of magnitude.

*Forced `abm_repeat()`, and it is the second model in this round to want it for
a reason the first did not. The vaccination game (51) has a phase that runs to
absorption inside a tick; deferred acceptance is a whole algorithm whose
stopping rule is its definition — "until no one is rejected" is not a number of
rounds, and `n log n` is an expectation rather than a bound. Written with
`rep()` and a fixed count it is either wrong or wasteful.*

*Forced `cost =`, in a form the garbage can (48) did not. "The best woman who
has not yet rejected me" is a lookup into the chooser's own preference list,
indexed by the candidate's identity — `rank_of(own_rank, .id)`, with `NA` for
anyone already ruled out. So the cost expression has to see `.id` and `.group`,
not only the ordinary columns, and it does. A preference is not a position in
any space, and one metric was never going to cover it.*

*What made this writable at all is that the proposals are `abm_match()` and the
answers are `abm_tell()`, in that order, twice a round. `.resolve = "min"` is a
woman hearing every offer at once and keeping her best; the man then reads
`partner_best` — refreshed, because partner columns are recomputed at each
step — and knows whether he won without anyone telling him. Two steps do the
work the textbook description puts in a nested loop.*

*One trap, and it is a modelling one. Engaged men must keep proposing to their
fiancées every round. If only unmatched men propose, a woman who receives a
better offer has to actively free the man she was holding, and he has to be
told which woman rejected him so he does not go back — three extra writes and
an ordering problem. Letting everyone propose makes displacement fall out of
the same `min`, because the incumbent is just another offer she compares. It
is also how Gale and Shapley describe it.*

---

**Reproduce:** [`m55_deferred_acceptance.R`](scripts/m55_deferred_acceptance.R)

← [54. Indirect reciprocity by image scoring](54-image-scoring.md) · [all models](README.md) · [56. Predator and prey without space](56-predator-prey.md) →
