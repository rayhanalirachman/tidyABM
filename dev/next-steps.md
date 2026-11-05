# Next steps

What is left from the syntax audit (full evidence and examples in
[`syntax-audit.md`](syntax-audit.md)). Each item is an owner decision; the
recommendation is from the audit. Tick the box when done.

## Do first (they regenerate model numbers, so do them together)

- [ ] **1. An empty match makes `abm_rules()` write nobody.**
  Today, when a match pairs nobody, a following `abm_rules()` writes every agent
  with `NA` partner columns (foxes keep gaining energy with no rabbits;
  `type ~ partner_type` turns every `type` into `NA`). `abm_sequential()`
  already writes nobody. A 5-line fix was tested: all 57 non-spatial scripts are
  unchanged, but **Daisyworld's numbers change** (one `runif(n())` draw is
  skipped). Breaking: regenerate the Daisyworld page and figure.
- [ ] **12. Seed the 16 non-spatial scripts that give different numbers on every
  run.** They draw random starting columns without `abm_setup(seed =)`. Add the
  seed and regenerate their page numbers once.

## Then

- [ ] **3. Rename `abm_match(by =)` to `sides =` / `coords =`, keep `by` as an
  alias.** Affects 7 model pages.
- [ ] **4. Dot convention.** State it as "dotted means how the step evaluates";
  rename `abm_pairs(.when)` to `when` with an alias. No model page uses
  `abm_pairs`. Do not dot everything.
- [ ] **8. Add `abm_birth(attach = "parent" | "random_edge" | "neighbour")`**
  beside `attach_via`. Additive; 4 pages simplify.
- [ ] **9. Add `abm_network(type = "regular")`, keep `"random"` as an alias.**
  Additive.
- [ ] **11. Retire `models/grammar.md`** in favour of `vignette("grammar")` and
  the README.

## Decided against (leave as is)

- 2. A `...` catch-all with "did you mean" on functions without `...`
  (R's "unused argument" error already names it).
- 5. A sentinel in place of the `to = "neighbours"` string.
- 6. Renaming `own_<col>`.
- 7. Explicit arguments in place of left-hand-side prefix routing
  (`partner_x ~`, `R_v ~`).
- 10. Making reading `.role` / `.partner` with no match standing an error.

## Other open ideas

- Spatial: `abm_death()` of the wired patch group leaves dangling `.cell`s
  (maybe an error); `along = "network"` on a non-lattice network can never move
  anything; `range = <column>` for per-agent vision; Rebellion's vision-radius
  `within =` is still an O(n^2) pair scan.
- Viewers: host `models/spatial/viewers/` on the pkgdown site (on GitHub they
  show as source, so they must be downloaded and opened).
- Viewer extras: a `trans =` for continuous fill, a globals sparkline under the
  canvas, a "download PNG of this frame" button.
- Check CI (`R-CMD-check`, all operating systems) after every push to `main`.
