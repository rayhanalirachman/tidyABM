# S7: expression compiler prototype (R AST -> IR -> Rust evaluator)

Date 2026-11-17. macOS arm64, R 4.6.1, dplyr 1.2.1, vctrs 0.7.3, rlang 1.3.0,
rustc/cargo 1.100.0, savvy 0.11.0. Files: `ir.R` (compiler, ~70 lines of R),
`../pkg/rspike/src/rust/src/expr.rs` (IR decoder + tree evaluator, ~560 lines, no
`unsafe`), `s7.R` (tests), log `results/s7.txt`, property-test detail
`results/s7-property.rds`.

## IR shape (frozen candidate for P5)

Nested R lists, one node per call: `list(op = "num"|"lgl"|"str", value)`,
`list(op = "var", name)`, `list(op = "call", fn, args = list(...))`,
`list(op = "case_when", pairs = list(list(cond, value), ...), default)`. The R side
resolves function names in the quosure's environment and accepts a name only if it
is `identical()` to the base/dplyr function (a masked `pmin` or a user function goes
to the R path with a recorded reason). Unary minus becomes `neg`, `(` disappears,
`runif` compiles only as `runif(n())`, `log` only with one argument, `NA_real_`
literals and named arguments fall back. The Rust side decodes the list once into an
enum tree (`Program$new`) and evaluates column at a time.

Semantics implemented from R's sources: R's relational NA rule, three-valued `&` /
`|` with both sides always evaluated, `pmin`/`pmax` NaN rule (summary.c `do_pmin`),
R's `exp`/`log` wrappers (S2), `if_else` (branches recycle to the condition's size,
NA condition -> NA), `case_when` (each condition then its value in written order;
size = common size of the conditions; NA condition = no match; `.default`).
Anything where R would warn ("NaNs produced") returns a fallback error so the rule is
re-evaluated in R (decision D9). Logical-valued `if_else`/`case_when` and integer
columns are not compiled in the spike (they fall back).

## Results

| check | result |
|---|---|
| tax model, all five per-agent rules (`declared` nested `case_when`, `audited` with `runif(n())`, `penalty_due`, `believed_audit_chance`, `declared_share`) evaluated by the Rust IR evaluator, full 150-run sweep | **identical** to the engine (control: same harness with R/dplyr evaluation also identical) |
| tax rules routed to R by the compiler | `neighbours_share ~ mean(...)` (a neighbour kernel, not a rule) and the global `sum(...)` (aggregates not in the spike) |
| property test: 5,000 random expressions (depth <= 4) over 40 rows with NA, NaN, +-Inf, -0, 1e300, character NA, mixing arithmetic, `exp`, `log`, `pmin`/`pmax`, comparisons, `&`/`|`/`!`, string `==`/`!=`, nested `if_else`/`case_when`, `runif(n())` | **3,630 identical** (values and `.Random.seed`), 1,329 where R errored or warned and Rust fell back too, 41 unnecessary fallbacks (spike gaps such as logical `if_else`), **0 mismatches** |

First property run (3,000 cases) had 109 mismatches, all of one kind: R/dplyr errors
when a size-1 condition meets size-n branches (`if_else(TRUE, d1, d2)`,
`case_when(FALSE ~ 2, .default = d1)`), while the prototype recycled. Fixed by
copying dplyr's size rules; the 5,000-case rerun is clean. This is exactly the class
of corner case the P5 property tests must keep catching (vctrs size/type rules).

Cost per rule at n = 441 (spike entry point, which copies the whole 12-column
environment from R on every call):

| rule | R/dplyr us | Rust us |
|---|---|---|
| `declared` (nested `case_when`, `exp`, `log`, `pmin`/`pmax`) | 148 | 35.5 |
| `believed_audit_chance` (`case_when` + `if_else`) | 60.5 | 13.5 |
| `audited` (`&`, `runif(n())`) | 7.0 | 15.0 |
| `penalty_due` (`if_else`) | 6.5 | 13.0 |
| `declared_share` (`/`) | 2.0 | 9.5 |
| `declared / income` with a 2-column env | 0.5 | 2.05 (`.Call`), of which `.Call` itself 0.15 |

The spike evaluator wins 4x where dplyr's `case_when`/`if_else` overhead dominates and
loses on one-liners because each call converts the environment (12 columns, a
441-string character column) into Rust values. In P5 the evaluator reads columns
straight from the store (no conversion), so the fixed cost per rule should fall to
the ~0.2-0.5 us of a `.Call` plus the arithmetic; S6 shows the compiled tax year
costs 0.27 ms per run in total.

## Verdict against PLAN.md

**PASS**: IR design and the fallback mechanism work; bit-identical on the tax model and
on 5,000 random expressions. **Plan inputs**: (1) the P5 evaluator must take its
inputs from the store, not from R lists (the spike's per-call conversion is the cost);
(2) P5's property test should run from day one, since dplyr's size/type rules (vctrs)
were the only source of mismatches; (3) aggregates (`sum`, `mean`) and logical/integer
results are the next coverage items for tax/Wolf-Sheep.
