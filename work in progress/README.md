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

**It runs against the package as it stands.** Five pieces of grammar were
written to be stressed by this model and landed because of it; it is the only
model in the corpus that needs all five at once:

| grammar | why the model needs it |
|---|---|
| `abm_sequential()` writes `partner_<col>` through a standing match | the goods market is a queue: the second buyer at a shop sees the stock the first one took. `abm_tell()` resolves every sender at once, so inventories go negative |
| `abm_match(weight = )` | "a firm I do not buy from, noticed in proportion to its size" |
| per-chooser `among` | "one of the firms I buy from" is not a population condition |
| a match does not escape its `abm_repeat()` block | the day loop would otherwise leave a pairing standing over the month end |
| a **relation**, `sellers`, with `unmet` on the pair | how much *this* seller has rationed *this* household belongs to neither of them alone. Held as two list columns aligned by hand, it went out of step and shipped that way (below) |

The relation rewrite reproduces the list-column version **to the digit**: every
printed number of the 120-month run is identical, because `set.seed(1)` draws
the same sellers `abm_agents()` drew and the relation path consumes the RNG the
same way from there. What changed is that `unmet_at()`, `swap_seller()`,
`swap_unmet()` and a five-line `Map()` closure are gone, and every household
holds exactly seven sellers at the end -- the direct test that every unlink is
paired with a link, which the file now prints.

### Where it stands

120 months, 200 households, 20 firms, one seed. The paper runs 6000 months at
1000 × 100 after a 1000-month burn-in, which is out of reach here.

| stylised fact | paper | this run |
|---|---|---|
| unemployment | 0 – 4.3% | 0 – 10.0%, mean 2.6% |
| unsatisfied demand | < 0.03% in 95% of months | 95th pct 0.016% |
| endogenous cycle | yes, unshocked | output sd/mean 2.6%, AR(1) +0.94 |
| Beveridge curve | negative | −0.64 |
| Phillips curve | negative | −0.11 |
| firm-size skewness | +1.88 | +0.30 |
| price changes per firm-month | median 9% | median 14% |

Two invariants hold exactly and matter more than any of the correlations:
money is conserved to the cent (it is a pure exchange economy, so money only
circulates), and inventory floors at zero without ever going below, which is
the direct test of the sequential partner write. Money came out at exactly
22000.00 in all ten runs of the seed sweep below.

The table is one seed, and the two correlations in it are not equally solid.
Across seeds 1-5, before and after the `sellers`/`unmet` fix:

| | Beveridge | Phillips |
|---|---|---|
| pre-fix, 5 seeds | −0.62 mean, negative 5/5 | −0.05 mean, sd 0.20, negative 2/5 |
| fixed, 5 seeds | −0.59 mean, negative 5/5 | −0.14 mean, sd 0.20, negative 4/5 |

The Beveridge curve is reproduced. The Phillips curve is not: its sign flips
from seed to seed in both variants, and the paired difference the fix makes is
−0.09 with sd 0.32 over five seeds (t = −0.61, p = 0.57), which is nothing. The
−0.11 in the table is seed 1 being lucky.

### Open

* The Phillips curve is noise at this sample size, not a modelling failure that
  has been localised. 96 monthly observations against the paper's 6000 give a
  correlation whose sign is not stable across seeds, so nothing can be concluded
  from it either way until the run is long enough. It is the one stylised fact
  here that needs the paper's horizon rather than a better model.
* Firm-size skewness is +0.30 against the paper's +1.88, on one seed, and was
  +0.50 before the fix. Given how much the Phillips correlation moves on seed
  alone, this has not been checked across seeds either and should not be read as
  the fix making it worse.
* A standing match reaches further than it looks. Most rules here carry
  `.scope = "population"` because a match stands until the next one; dropping
  them from the month-end block once created 18,000 units of money while every
  printed number stayed plausible. Either the semantics want tightening or
  `abm_go()` wants to complain.
* **Closed, and worth keeping on the record.** `sellers` and `unmet` were two
  positionally-aligned list columns, and the price swap changed one without the
  other: `swap_seller()` wrote the incoming firm at the *dropped* firm's index,
  and `unmet` was not reset until after the quantity hunt had already read it as
  a draw weight, so a household that price-swapped weighted the firm it had
  just started buying from by the rationing history of the firm it had just
  dropped. It moved no stylised fact across five seeds, which is precisely why
  it survived: a misattributed draw weight changes who gets dropped without
  changing any aggregate enough to notice. A first fix added a `swap_unmet()`
  companion write. The real fix was to stop having two columns: `sellers` is
  now an `abm_relation()` with `unmet` on the pair, the swap is an
  `abm_unlink()` and an `abm_link()`, the shortfall is written on the pair in
  the same `abm_sequential()` as the sale, and there is no version of the file
  in which the two can disagree. This was the model that put *state that
  belongs to a pair* on `open-items.md`, and the one that took it off.
* `abm_neighbours()` gives `NA` to an agent with no neighbours. Right for
  `mean(opinion)`, wrong for `n()`: a firm that lost its last worker got
  `n_emp = NA`, which ate its inventory and collapsed the economy 90 months
  later. Hence the `coalesce(n_emp, 0)` after every count.
