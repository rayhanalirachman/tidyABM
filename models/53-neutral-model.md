# 53. Random copying and the neutral model of cultural change (Bentley, Hahn & Shennan 2004)

**Concept**

- Setup: N people, each holding a cultural variant — a name, a pot decoration,
  a citation
- Go: everyone is replaced; each newcomer copies a variant from a random member
  of the previous generation, or with probability μ invents one nobody has used
- Output: how many variants are in circulation, and how their popularity is
  distributed

**Package**

```r
n <- 500; mu <- 0.01

m <- abm_setup(
  agents  = abm_agents(n = n, variant = ~seq_len(n)),
  globals = list(coined = n),
  seed    = 1)

go <- abm_go(
  abm_rules(copied   ~ sample(variant, n(), replace = TRUE),
            innovate ~ runif(n()) < mu,
            .scope = "population"),
  abm_rules(variant ~ if_else(innovate, coined + cumsum(innovate), copied),
            .scope = "population"),
  abm_global(coined ~ coined + sum(innovate))
)

result <- abm_run(m, go, ticks = 3000, seed = 1)
```

**Result** (3000 ticks, measured over the last 1000)

| N | μ | variants in use | Ewens prediction | ccdf slope |
|---|---|---|---|---|
| 500 | 0.0010 | 7.5 | 6.8 | 0.50 |
| 500 | 0.0050 | 25.1 | 23.6 | 0.72 |
| 500 | 0.0100 | 41.9 | 39.8 | 0.84 |
| 500 | 0.0500 | 131.2 | 120.4 | 1.35 |
| 2000 | 0.0025 | 55.9 | 53.5 | 0.61 |

The variant counts land on the Ewens sampling formula, which is the exact
result for the infinite-alleles model this is: `E[k] = Σ θ/(θ+i)` with
`θ = 2Nμ`. Nothing in the model is fitted to it, and the agreement is within a
few per cent across two orders of magnitude in μ — the sharpest analytic check
in the corpus alongside the quarter-cycle lag in model 56.

The popularity distribution is the reason anyone cares. Copying at random,
with no variant better than any other and nobody choosing anything, produces a
heavy right tail: a handful of variants held by many people, a long list held
by one or two. Converting the cumulative slopes to density exponents gives
α between 1.5 and 2.35, and the range Bentley, Hahn and Shennan measure in
twentieth-century baby names is 1.70 to 1.93. The moral of their paper is that
finding a power law in a popularity list is not evidence of anything: it is
what you get when nobody is deciding anything at all.

*Needed no package change, and is one of the few models here with no
interaction structure whatever — no network, no match, no partner. It is three
population-scope rules and a global.*

*The one thing it does need is a way to make something new. A variant nobody
has held before is a fresh identifier, and the idiom is a global counter plus
`cumsum()` over the innovators in the same step:
`variant ~ if_else(innovate, coined + cumsum(innovate), copied)`, then
`abm_global(coined ~ coined + sum(innovate))`. It works because `abm_rules()`
is simultaneous — every innovator sees the same `coined` — so the running sum
is what hands out distinct numbers. Written as an `abm_sequential()` step it
would also work and be a hundred times slower.*

*The ccdf slope is worth less than it looks. Bentley's argument is that Nμ
alone fixes the distribution, and the last row (N = 2000, μ = 0.0025) has the
same θ as the third (N = 500, μ = 0.01) and does not give the same slope — 0.61
against 0.84. The variant counts for those two rows differ too, and correctly
so: `E[k]` depends on the sample size as well as θ. What the slope is sensitive
to is the range of frequencies the fit covers, which grows with N. The number
is here to show a heavy tail of the right order, not to test the scaling claim,
which needs an estimator built for the job.*

---

**Reproduce:** [`m53_neutral.R`](scripts/m53_neutral.R)

← [52. Bank runs and the sequential service constraint](52-bank-runs.md) · [all models](README.md) · [54. Indirect reciprocity by image scoring](54-image-scoring.md) →
