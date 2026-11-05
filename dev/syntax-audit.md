# Syntax audit: does a tidyABM model read as a specification?

Scope: every exported function and argument, the formula mini-language, the
error messages and print methods, and the first-contact path (README,
`vignette("tidyABM")`, the reference pages, the 65 model pages: 54 in
`models/non-spatial/`, 11 in `models/spatial/`). Evidence comes from running
the package: every "before" below was re-run against 7a76c8a and every
"after" against this branch, an independent newcomer test (three models
written from the docs alone: rumour, predator-prey, Schelling), and the
corpus itself (corpus counts are pages under `models/*/[0-9]*.md`). Quoted
messages are what the package printed, with colour stripped.

Findings are ranked by how much they hurt a newcomer. Each has a before/after
from this package and a recommendation with its cost. "Fixed" means done in
this branch (non-breaking); "Decision" means the owner decides, see the list
at the end.

---

## 1. A failing rule said nothing about where it failed

**Hurt: highest.** Every model a newcomer writes fails this way at least
once, and the message gave no step, no tick, no column list and no hint.

Before (`abm_rules(money ~ monney - 1)`, run for 1 tick):

```
Error: object 'monney' not found
```

Not even a call: the fast evaluation path raises it from `eval_tidy()`
with `call = NULL`. The same bare line for a missing global (`stp`), for
`partner_money` with no match standing, for `partner_money` under a
`size = 4` match (where only grouped aggregates exist), for `role` instead
of `.role`, for `own_wealth` in `abm_neighbours()` when the column is `w`,
and for `runif(n)` where `n()` was meant, which does not even say "not
found":

```
Error: invalid arguments
```

After:

```
Error in `abm_run()`:
! Step 1 of the go block, `abm_rules()`, failed at tick 1.
x object 'monney' not found
i In rule `money ~ monney - 1`.
i Did you mean money?
i Agent columns: money.
i Globals: step.
```

and, for the shapes a newcomer actually hits:

```
i No `abm_match()` stands before this step, so there are no `partner_<col>` columns. Put the match earlier in the tick.
i The standing match groups 4 agents, and `partner_<col>` exists only for pairs (`size = 2`). In a group, aggregate instead: `sum(money)`.
i The columns tidyABM manages are dot-prefixed: did you mean `.role`?
i `own_<col>` exists only where a focal agent looks at candidates: `abm_neighbours(within =)`, `abm_match(cost =, among =, weight =)` and `abm_move(to = uphill())`. In a rule, an agent's own column is just `money`.
i A rule reads `n` without brackets. Inside a step the number of agents is `n()`; `n` is the plain number only in an `abm_agents()` setup formula.
```

A wrong-length rule, which used to print dplyr's internal `.abm_value must
be size 50 or 1, not 2` under `Error in dplyr::mutate(dat, ...)`, now carries the step and a sentence on what a rule
must return. Setup formulas get the same treatment: `abm_agents(n = 5,
b = ~a * 2, a = ~runif(n))` says *"`a` is declared after `b`. Formulas run
in order, so move it up."*, and `~runif(n())` says that `n` is a number
there.

**Fixed** (`run_step()` wraps every step; `materialise_agents()` wraps every
formula; the package's own `tidyABM_*` errors pass through untouched).
Non-breaking: only the message and class of errors that were already
errors changed. New classes `tidyABM_step_error`, `tidyABM_setup_error`,
plus `tidyABM_missing_column` when a name is the problem.

## 2. A match that pairs nobody writes everybody

**Hurt: highest, and silent.** The newcomer test's predator-prey model
reported foxes gaining energy with no rabbits left, and its Schelling
model's whole `type` column going `NA` on the first tick. Verified:

```r
w  <- abm_setup(agents = abm_agents(n = 6, x = 0, k = 1:6))
go <- abm_go(abm_match(pair = "one_of", eligible = k > 100), abm_rules(x ~ 1))
abm_run(w, go, ticks = 1)$x[-(1:6)]
#> 1 1 1 1 1 1        <- nobody was matched, everybody was written
go <- abm_go(abm_match(pair = "random", eligible = k <= 2), abm_rules(x ~ 1))
#> 1 1 0 0 0 0        <- one pair matched, the rest kept their value
```

`?abm_rules` said *"an agent the match left out keeps the value it had,
silently"*, which is true for a partial match and false for an empty one.
The engine (`run_rules()`) treats "no match" and "a match that paired
nobody" the same way (`active <- ... && nrow(match) > 0`), and
`what-changed.md` records the empty-match `NA` partner columns as a
deliberate choice so that `case_when(..., TRUE ~ state)` rules survive a
quiet tick. The two behaviours together mean a rule's reach depends on
whether *anyone* met, which no model can want. `abm_sequential()` already
does the other thing: `run_sequential()` returns at once on an empty match,
with a comment saying it does so "the way it leaves `abm_rules()` with no
rows to write". So the engine disagrees with itself, and with the help.

Reproduced from the newcomer's predator-prey model with `abm_rules(fed ~
TRUE)` after `abm_match(pair = "opposite_group", by = .group)`: once the
three rabbits are eaten (tick 1), mean fox energy goes 5, 7, 9, 11, 13 with
no rabbit alive. With the step narrowed to nobody it goes 5, 7, 6, 5, 4.
And `abm_rules(type ~ partner_type)` after a match nobody is eligible for
sets every `type` to `NA`.

**Verdict:** an engine inconsistency, not only a documentation gap. It is
**not fixed here**, because the fix changes what a valid model computes
on any tick where its match pairs nobody: every non-idempotent rule after
the match (`x ~ x + 1`, any rule that draws a random number) stops being
applied. That is the owner's call. **Done:** documented precisely in
`?abm_rules`, `?abm_match` and the cheat sheet, with the safe idiom
(`fed ~ !is.na(.partner)`).
**Decision 1:** make the empty-match case narrow the step to nobody (no
agent is written), like the partial case and like `abm_sequential()`. A
prototype (the early return in `run_rules()`, five lines) was run over the
whole corpus: no non-spatial script changed, and Daisyworld did (see
the decisions table).

## 3. A qualifier without its dot was read as a rule

**Hurt: high.** The rule steps take `...` first, so their qualifiers are
dot-prefixed (`.scope`, `.by`, `.order`, `.resolve`, `.where`, `.each`,
`.when`). Forget the dot and the argument lands in `...`:

```
abm_rules(x ~ 1, scope = "population")
#> ! Every argument to `abm_rules()` must be a two-sided formula.
#> x Argument 2 is <character>.
abm_rules(x ~ sum(y), by = firm)
#> Error: object 'firm' not found
abm_rules(money ~ money - 1, when = money > 0)      # the NetLogo reflex
#> Error: object 'money' not found
abm_rules(money = money - 1)                        # `=` for `~`
#> Error: object 'money' not found
```

After:

```
#> ! `scope` is not an argument of `abm_rules()`.
#> i Did you mean `.scope`? The qualifiers of a rule step are dot-prefixed, so they cannot collide with a column name.
#> ! `when` is not an argument of `abm_rules()`.
#> x Got `when = money > 0`. Everything in `...` must be a `column ~ expression` rule.
#> i A rule has no `when`: put the condition inside it, `col ~ if_else(<condition>, <new value>, col)`, or use `abm_birth(when =)` / `abm_death(when =)` / `abm_tell(when =)`.
#> ! `money` is not an argument of `abm_rules()`.
#> i A rule is written with `~`, not `=`: `money ~ money - 1`.
```

**Fixed** for every step that takes rules in `...` (`check_rule_dots()`,
inspects the unevaluated dots). It refuses only what cannot be a rule: a
qualifier missing its dot, a condition-like name (`when`, `if`, ...), a
constant, and `col = <expression using col>`. A name bound to a formula
(`r1 = f`) and a named call that may build one (`r1 = make_rule()`) are
evaluated as before. Functions without `...` (`abm_match`, `abm_birth`,
`abm_death`, `abm_unlink`, `abm_network`, `abm_setup`, `abm_run`,
`abm_move`) give R's own `unused argument (roles = c(a = TRUE))`, which at
least names the argument. R's partial matching also makes `abm_run(tick =)`,
`abm_network(dim =)`, `abm_setup(agent =, global =)` silently work.
**Decision 2:** whether to add a `...` catch-all to those six for a
"did you mean" (non-breaking for valid calls, but it turns a positional
surplus argument into a quieter failure). I recommend not.

## 4. One word, three ideas: `by`

**Hurt: high for reading, zero for writing once you know.**
`abm_match(by = .group)` names the two sides of an `"opposite_group"`
split; `abm_match(pair = "nearest", by = c(x, y))` names coordinates;
`abm_rules(.by = firm)` partitions the population; `abm_global(.by = task)`
indexes a table; and `abm_match(.by = .cell)` partitions a match. So

```r
abm_match(pair = "opposite_group", by = .group, .by = .cell)
```

is a real line in the wolf-sheep model, with `by` and `.by` meaning
different things two characters apart. A NetLogo reader sees "by" and
guesses "grouped by", which is right for `.by` and wrong for `by`.

**Decision 3:** rename `abm_match(by =)` to a word per idea, `sides =` for
`"opposite_group"` and `coords =` (or `space =`) for `"nearest"`, keeping
`by` as a deprecated alias. Breaking (soft): 7 of the 65 model pages use
`abm_match(by =)`.
If not renamed, the cheat sheet now states the three meanings side by side.

## 5. Dotted or plain, by accident of signature

**Hurt: medium.** The convention is "arguments after `...` are dotted",
which produces `abm_pairs(via, ..., .when)` next to `abm_tell(..., to,
when)` and `abm_link(..., when, via, to)`, and `abm_neighbours(..., within,
.where)` with one of each. A reader cannot predict the dot.

| idea | spelt | in |
|---|---|---|
| condition on the actor | `when` | `abm_birth`, `abm_death`, `abm_tell`, `abm_link`, `abm_unlink` |
| condition on the actor | `.when` | `abm_pairs` |
| condition on the actor | `eligible` | `abm_match` |
| condition on the candidate | `among` | `abm_match` |
| condition on the pair | `within` | `abm_neighbours` |
| which groups | `who` | `abm_move` |
| which group | `on` | `abm_network` |

**Decision 4:** (a) accept the mix and state the rule as "dotted =
*how the step evaluates* (`.scope`, `.by`, `.order`, `.resolve`, `.where`,
`.each`); plain = *what the step is about*", then rename the one outlier
`abm_pairs(.when)` to `when` with the old name kept as an alias (tiny,
soft-breaking, no corpus page uses it); or (b) dot everything after `...`
(`abm_tell(.to, .when)`, `abm_neighbours(.within)`), which is breaking
across the corpus. I recommend (a).

## 6. `to` means four things

`abm_tell(to = .partner)` is the recipient; `abm_tell(to = "neighbours")`
is a mode string in the same argument; `abm_move(to = "random_neighbour")`
and `to = uphill(sugar)` is how a destination is chosen; `abm_link(via =
"R", to = cand)` is the other end of a relation row; `from`/`to` are the
edge columns. The mixed string-or-expression argument (`to = "neighbours"`
vs `to = .partner`) is the one that costs: a newcomer who writes
`to = "partner"` gets a correct error, but the reason one is quoted and the
other not is a convention they have to be told.

**Decision 5:** none needed; the cheat sheet's rule 1 ("only a mode or a
name is a string") covers it. A `neighbours()` sentinel instead of the
string would make `to` one type, at a breaking cost (one corpus page, plus
`vignette("tidyABM")` and the help).

## 7. Who is "me": `partner_<col>` vs `own_<col>`

In a rule, *my* columns are plain and the partner's are `partner_`; in
every pair view (`within`, `cost`, `among`, `weight`, `uphill()`), the
*candidate's* columns are plain and mine are `own_`. Hegselmann-Krause reads
`within = abs(opinion - own_opinion) <= eps`, and a reader has to know
that the first `opinion` is somebody else's. It is consistent with "the
expression is evaluated over the rows being looked at", but that is a
second rule to learn, and `own_.cell` in the co-location idiom
`within = .group == "patches" & .id == own_.cell` is the first place most
spatial readers meet it.

**Done:** the cheat sheet's names table states both conventions on
adjacent rows, and the new error for `own_<col>` in a rule says where
`own_` exists. **Decision 6:** a rename (`their_<col>` in pair views, so
that `my` columns are always plain) would be clearer and is breaking for
the whole spatial corpus. I recommend keeping `own_`.

## 8. The left-hand side decides whose row is written, by prefix

`col ~ expr` writes the agent's own column in `abm_rules()`; in
`abm_sequential()` `partner_stock ~ partner_stock - got` writes the
*partner's* row; in either, `sellers_unmet ~ ...` writes a *relation* row
because `sellers_` is a declared relation; in `abm_tell()` the LHS is the
recipient's column but the RHS is evaluated in the sender's row; in
`abm_draw()` the LHS is a new edge value; in `abm_global()` it is a global.
None of this is wrong, but a reader cannot tell `x ~ 1` and `partner_x ~ 1`
apart without the prefix rule, and `sellers_unmet` looks exactly like an
agent column (setup refuses the collision, which is the right guard).

**Done:** the cheat sheet lists every prefix. **Decision 7:** none;
the alternative (explicit `abm_rules(partner = ...)`) is heavier than the
prefix.

## 9. `attach_via = abm_match(pair = "network", from = "parent")`

To say "a newborn settles next to its parent" a modeller writes a whole
match specification inside `abm_birth()`, with a `from` value (`"parent"`)
that is only legal there. The reference page explains it; the line itself
does not read as a specification.

**Decision 8:** add `abm_birth(attach = c("none", "parent", "random_edge",
"neighbour"))` as a plain string (non-breaking addition), keep `attach_via`
for the day a match with `eligible` is needed. Cost: one argument, 4 model
pages to simplify.

## 10. `type = "random"` is the regular graph

`abm_network(type = "random", degree = 4)` is a 4-regular random graph;
`type = "poisson"` is what the literature calls a random graph. Model 38's
page spends a paragraph on exactly this. A newcomer asking for "a random
network" gets a graph with no degree variance and will not know it.

**Decision 9:** add `"regular"` as the documented name for the k-regular
graph and keep `"random"` as an alias (non-breaking). Recommended.

## 11. Two seeds

`abm_run(seed =)` reproduces the run and not the world; a model with
`x = ~runif(n)` also needs `abm_setup(seed =)`. The vignette explained it,
the README did not; the newcomer test lost time here. **Fixed** in the
README and the cheat sheet (rule 4).

## 12. `.partner` was undocumented where it is needed

`.partner` (the partner's `.id`, `NA` if unmatched) is the idiom for "did I
meet anyone" (`!is.na(.partner)` is on 7 model pages, `.partner` on 11). The
help used it in six places (`?abm_tell`, `?abm_link`, `?abm_unlink`,
`?abm_sequential`, `?abm_relation`, `?abm_rules`) but never said what it is
on the page that creates it: `?abm_match`'s list of what a rule sees after a
match had `partner_<col>` and `.role` and not `.partner`. **Fixed** in
`?abm_match`, the README and the cheat sheet.

## 13. `eligible` did not exempt an agent from the two-value check

```r
abm_match(pair = "opposite_group", by = side, eligible = side != "none")
#> ! side must take at most two values for opposite-group matching.
#> x Across all agents it takes 3 values: "a", "b", and "none".
```

The documentation says `eligible` says who takes part; the check ignored
it. **Fixed:** the two values are counted among the participants, and the
message names both arguments. No valid model changes (a population with
two values has the same two among any subset).

## 14. Silent writes to engine columns

`abm_rules(.id ~ 99)` ran without complaint and gave every agent the same
`.id`, 99, from tick 1 on; `abm_rules(.group ~ "zzz")` relabelled the output
rows without moving the agent to another group. **Fixed:** `.id`, `.group`, `.group_id`, `.partner`
and `.role` are refused as a rule target with `tidyABM_reserved_column`
and a sentence on keeping an agent's kind in an ordinary column. `.cell`
stays writable by design. No corpus model or test wrote any of them.

## 15. A quoted condition died inside `&`

`abm_match(pair = "random", eligible = "money > 0")` was accepted at
construction and failed at run time with *"operations are possible only for
numeric, logical or complex types"*. **Fixed:** `tidyABM_bad_condition`,
with the unquoted form. Numeric conditions still coerce as before.

## 16. `pair = "nearest"` on a character column

Failed with `non-numeric argument to binary operator` from `a^2`.
**Fixed:** refused with a pointer to `cost` for a nearness that is not a
distance. A logical column, which measured as 0/1 before, is still
accepted.

## 17. `abm_agents(N = 10)`

R is case-sensitive, so `N` became a column and `abm_setup()` said "Group
agents needs an `n`" with no mention of the `N` it could see. **Fixed:**
the message says what happened.

## 18. The reference pages, rendered

Four pages had a code block written as one line inside backtick fences, so
pkgdown and the console help showed a garbled fragment: `?abm_global`
rendered as `n(), .by = 1:2) ```, `?abm_neighbours` as `own_opinion) <=
eps) ```, and `?abm_go` and `?abm_run` likewise. `?abm_neighbours` also
had a section heading ("One named lattice neighbour") inside the `...`
parameter's text. **Fixed** in the roxygen source and the Rd.

The two `\dontrun` examples (`abm_plot_lattice()`, `abm_view_lattice()`)
each run in under half a second; **fixed** to `@examplesIf` (ggplot2) and a
`tempfile()`. They were the only `\dontrun` blocks in `man/`.

`?abm_go`'s refusal of a non-step listed six constructors out of
fourteen; **fixed** to say what the offending thing is (the world from
`abm_setup()`, a bare formula, a function missing its parentheses).

## 19. `print()` as a teacher

`print(abm_go)` said `rules  2 rule(s)`, which tells a reader nothing
about the model. **Fixed:** it names what each step writes and its
qualifier. The wolf-sheep model's go block, before and after:

```
<abm_go> 13 steps, 1 match phase            <abm_go> 13 steps, 1 match phase
1. move along patches                       1. move along patches
2. rules 1 rule(s)                          2. rules energy
3. nbrs 1 rule(s)                           3. nbrs grass_here (within)
4. rules 1 rule(s)                          4. rules energy
5. tell 2 rule(s)                           5. tell grass, countdown to .cell
6. match opposite_group                     6. match opposite_group
7. rules 1 rule(s)                          7. rules energy
8. death                                    8. death when .group == "sheep" & !is.na(.partner)
9. death                                    9. death when .group %in% c("sheep", "wolves") & energy < 0
10. birth                                   10. birth when .group == "sheep" & runif(n()) < sheep_repro
11. birth                                   11. birth when .group == "wolves" & runif(n()) < wolf_repro
12. rules 2 rule(s)                         12. rules grass, countdown (population)
13. global 2 rule(s)                        13. global n_sheep, n_wolves
```

`print(abm_result)` said "N agents seen", which the newcomer test could
not interpret; it is the number of distinct `.id`s over the run and now
says "distinct agents" (and nothing under `record = "globals"`).
`print(abm_match)` labelled `role` as `roles`; fixed.

## 20. `n()` or `dplyr::n()`

The docs used both; both work (`n` is re-exported from dplyr). **Fixed:** the examples use bare `n()` and `?abm_rules`
says the two are the same thing and that setup formulas use the number `n`.

## 21. A cheat sheet

There was none: the grammar lived in `models/grammar.md` (not on the
pkgdown site, 379 lines), the README's tables, and `?abm_go`. **Fixed:**
`vignette("grammar")`, one page: the three statements, the world's
declarations, every step with its one-line meaning and key arguments, every
special name a rule can use, the five traps, and how to read the result.
Linked from the README and the pkgdown articles menu.

## 22. Things that are fine and should stay

- Mode-specific argument validation in `abm_match()` and `abm_network()`:
  an argument a mode does not use is an error, and `rlang::arg_match()`
  already gives "did you mean" for `pair = "one-of"` and `to =
  "random_neighbor"`.
- The three-statement shape and the step-per-line go block. The newcomer
  test's three models read as specifications once they ran; the problems
  were all in what the package said when they did not.
- `.scope = "population"`, `.resolve`, `record`, `cost`, `weight`,
  `times`, `links`: guessable, each one word, each one concept.
- The sequencing check in `abm_go()` (two matches in a row; ending on one).

## 23. Not changed, worth knowing

- `abm_tell()` requires the column to exist on the recipient; the result
  tibble carries the union of all groups' columns with `NA` elsewhere. Now
  documented in `?abm_run`'s value.
- `.role` with no match standing is `NA`, so `if_else(.role == "giver",
  ...)` yields `NA` silently. An error would be breaking for population-scope
  rules written after a directional match; left as is.
- Functions without `...` give base R's `unused argument` on a typo (see
  Decision 2).

## 24. How this branch was checked

- `testthat`: 1133 expectations pass, 0 fail (HEAD gives 1033 with the
  same `test_dir()` harness; the 100 new ones are `test-syntax.R`).
- All 11 `models/spatial/scripts/` print byte-identical output to the
  reference run, with empty stderr.
- All 57 `models/non-spatial/scripts/` print byte-identical output to HEAD
  when the session is seeded first (`set.seed(20261105); source(...)`).
  Unseeded, 16 of them differ from run to run *on HEAD too*: they call
  `abm_setup()` with random starting columns and no `seed =`, the trap of
  finding 11. See decision 12.
- `R CMD build` knits all four vignettes, `grammar.Rmd` included.

---

## Decisions for the owner

| # | decision | my recommendation | cost |
|---|---|---|---|
| 1 | A match that pairs nobody: narrow the following steps to nobody (as a partial match does) instead of writing everyone with `NA` partners | **Yes.** It is the inconsistency most likely to produce a wrong result that looks right, and `abm_sequential()` already behaves this way. Prototype run over the corpus: the 57 non-spatial scripts print the same numbers; Daisyworld (spatial 11) changes, because its seeding rule draws `runif(n())` after a match that pairs nobody once the lattice is full, and skipping it shifts the random stream. So it is breaking for at least one corpus model, and its page numbers would need regenerating. | engine: ~5 lines in `run_rules()`; changes results on empty-match ticks |
| 2 | Add a `...` catch-all with "did you mean" to the six functions without one | No; R's `unused argument (roles = ...)` names the argument | low, but makes a surplus positional argument quieter |
| 3 | Rename `abm_match(by =)`: `sides =` for `"opposite_group"`, `coords =` for `"nearest"`, `by` kept as alias | **Yes**, if any rename is on the table: it is the one argument with three meanings | soft-breaking; 7 model pages, docs |
| 4 | The dot convention: (a) state it as "dotted = evaluation qualifier", rename `abm_pairs(.when)` to `when` with alias; or (b) dot every post-`...` argument | **(a)** | (a): one alias, no corpus use; (b): breaking across the corpus |
| 5 | `abm_tell(to = "neighbours")` as a sentinel rather than a string | No; cheat-sheet rule 1 covers it | breaking; 1 corpus page, vignette, help |
| 6 | Rename `own_<col>` in pair views (e.g. `their_<col>` for the candidate so "me" is always plain) | No; keep `own_` | breaking; 11 model pages (6 of the 11 spatial ones) |
| 7 | Replace LHS prefix routing (`partner_x ~`, `R_v ~`) with explicit arguments | No | breaking, heavier to write |
| 8 | Add `abm_birth(attach = "parent" / "random_edge" / "neighbour")` as a plain string beside `attach_via` | **Yes** | additive; 4 model pages simplify |
| 9 | Add `abm_network(type = "regular")` as the documented name for the k-regular graph, `"random"` kept | **Yes** | additive; 10 pages use `"random"` and keep working |
| 10 | Make `.role` (and `.partner`) an error when read with no match standing in the tick | No; `NA` is defensible and an error would break population-scope rules after a directional match | breaking |
| 11 | Move `models/grammar.md` onto the pkgdown site, or retire it in favour of `vignette("grammar")` plus the README | Retire the duplicate sections; keep its "what a relation is" prose in `?abm_relation` | docs only |
| 12 | Give the 16 non-spatial scripts whose output varies run to run an `abm_setup(seed =)` | **Yes**: the corpus should model the two-seed advice | regenerates those pages' numbers once |
