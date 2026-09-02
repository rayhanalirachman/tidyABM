# Work in progress

Models and grammar that are not finished. Nothing here is part of the package,
and `.Rbuildignore` keeps the folder out of the build.

## `lengnick-2013.R`

Lengnick, M. (2013), "Agent-based macroeconomics: A baseline model",
*Journal of Economic Behavior & Organization* 86, 102–120.

A minimal ACE macro model: households and firms, no government or central bank,
consumption goods traded daily and labour monthly, all of it through trading
relationships between named individuals rather than through a market clearing
mechanism.

**It does not run against the released package.** It depends on four changes
that are uncommitted in this tree:

| change | why the model needs it |
|---|---|
| `abm_sequential()` writes `partner_<col>` through a standing match | the goods market is a queue: the second buyer at a shop sees the stock the first one took. `abm_tell()` resolves every sender at once, so inventories go negative |
| `abm_match(weight = )` | "a firm I do not buy from, noticed in proportion to its size" |
| per-chooser `among` | "one of the firms I buy from" is not a population condition |
| a match does not escape its `abm_repeat()` block | the day loop would otherwise leave a pairing standing over the month end |

### Where it stands

120 months, 200 households, 20 firms, one seed. The paper runs 6000 months at
1000 × 100 after a 1000-month burn-in, which is out of reach here.

| stylised fact | paper | this run |
|---|---|---|
| unemployment | 0 – 4.3% | 0 – 11.5%, mean 2.2% |
| unsatisfied demand | < 0.03% in 95% of months | 95th pct 0.013% |
| endogenous cycle | yes, unshocked | output sd/mean 2.7%, AR(1) +0.93 |
| Beveridge curve | negative | −0.61 |
| Phillips curve | negative | +0.05 — not reproduced |
| firm-size skewness | +1.88 | +0.50 |
| price changes per firm-month | median 9% | median 14% |

Two invariants hold exactly and matter more than any of the correlations:
money is conserved to the cent (it is a pure exchange economy, so money only
circulates), and inventory floors at zero without ever going below, which is
the direct test of the sequential partner write.

### Open

* The Phillips curve. 96 monthly observations against the paper's 6000 is the
  obvious explanation and is not demonstrated.
* A standing match reaches further than it looks. Most rules here carry
  `.scope = "population"` because a match stands until the next one; dropping
  them from the month-end block once created 18,000 units of money while every
  printed number stayed plausible. Either the semantics want tightening or
  `abm_go()` wants to complain.
* `abm_neighbours()` gives `NA` to an agent with no neighbours. Right for
  `mean(opinion)`, wrong for `n()`: a firm that lost its last worker got
  `n_emp = NA`, which ate its inventory and collapsed the economy 90 months
  later. Hence the `coalesce(n_emp, 0)` after every count.
