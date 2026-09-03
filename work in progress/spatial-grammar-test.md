# Spatial models: does the grammar accommodate them?

A design probe, not a set of implemented models. Thirteen NetLogo spatial models,
each with a faithful sketch of its original and an attempt to write it in
tidyABM. The question for each is only: **what does the grammar need before this
model can be written the way the rest of the corpus is written** — as three
statements, typed steps, tidy output.

Models 1–8 span the difficulty axis; 9–13 were added to check the findings
generalise across domains (physics, political science) and edge cases (a single
deterministic agent, sequential dynamics, 1-D traffic, global feedback).

## What is being tested against

| tier | addition | one-line spec |
|---|---|---|
| **current** | — | the grammar as it stands in the repo today |
| **L0** | `abm_network(type = "grid" \| "line", dims, diagonals, torus)` | a lattice is a network type; see *Setup conventions* below |
| **L1** | named neighbours | `abm_neighbours(..., .where = "west")`, or direction-labelled lattice edges |
| **L2** | the turtle-≠-patch bundle | `abm_network(on = <group>)`, `.cell` as an engine-owned location column, `within = (a == b)` equijoin fast path, `abm_tell(to = <id column>)`, `abm_match(.by = <column>)`, `abm_move(along =, to =, who =, range =)` |

Verdict codes used below:

- 🟢 **current** — writable today, no additions (an idiom may be needed)
- 🔵 **L0** — needs only the lattice network type
- 🟣 **L0 + L1** — also needs named neighbours
- 🟠 **L2** — needs the turtle-≠-patch bundle
- 🔴 **L2+** — needs something beyond the proposed bundle
- ⛔ — not expressible even with everything proposed

## Setup conventions (settled in review)

These apply to every L0 / L2 model below and are why their setup blocks are short.

- **A lattice stays a network.** `abm_network(type = "grid", dims = c(w, h), on = <group>)`
  remains the primitive; `abm_neighbours()`, `abm_match(pair = "network")`,
  `abm_link()`, `abm_edges()` consume its edge list unchanged. `on =` names the
  group to wire (defaults to the whole population in a single-group model).
- **The wired group inherits its count.** A group a grid network wires omits
  `n` — it comes from `prod(dims)`. `n` is still usable inside that group's
  column formulas.
- **Coordinates are injected.** The grid constructor adds `.x`, `.y` (and `.cell`
  on non-wired groups). For a grid network `abm_setup()` builds the lattice
  *before* materialising agent columns, so `.x` / `.y` are in scope in setup
  formulas as well as in the go block. Non-grid networks keep the current order.
- **`diagonals = TRUE / FALSE`** replaces the `"moore"` / `"von_neumann"` eponyms.
  Default `TRUE` (8-neighbour, matches NetLogo's unmarked `neighbors` and Game of
  Life); Fire and Ising pass `FALSE`.
- **`abm_grid()` is optional sugar.** `abm_grid(dims, diagonals, torus, <cols>)`
  desugars to an `abm_agents()` group plus `abm_network(type = "grid", on = <it>)`
  — one implementation, so the go block is identical either way. Use it for
  patch-heavy models; keep the explicit two-line form when the network should be
  visible or sits alongside other networks.

---

## 1. Conway's Game of Life

**Source.** *Life Simple*, NetLogo Models Library (Computer Science → Cellular
Automata), Wilensky 1998.

**NetLogo sketch.**

```netlogo
patches-own [ living? live-neighbors ]

to setup
  clear-all
  ask patches [
    ifelse random-float 100 < initial-density [ cell-birth ] [ cell-death ]
  ]
  reset-ticks
end

to go
  ask patches [ set live-neighbors count neighbors with [living?] ]   ;; phase 1
  ask patches [                                                        ;; phase 2
    ifelse live-neighbors = 3 [ cell-birth ]
      [ if live-neighbors != 2 [ cell-death ] ]
  ]
  tick
end
```

**Spatial ingredients.** Patches only; Moore-8; torus; synchronous update.

**tidyABM.**

```r
w <- h <- 100

life <- abm_setup(
  agents  = abm_agents(alive = ~runif(n) < 0.10),        # n comes from the grid
  network = abm_network(type = "grid", dims = c(w, h))   # diagonals, torus default to TRUE
)

go <- abm_go(
  abm_neighbours(live_n ~ sum(alive)),
  abm_rules(alive ~ live_n == 3 | (alive & live_n == 2))
)

result <- abm_run(life, go, ticks = 500, seed = 1, record = 25)
```

**Verdict: 🔵 L0.** One new `type` string. `abm_neighbours()` is phase 1,
`abm_rules()` is phase 2, and `abm_rules()`'s simultaneity *is* the two-`ask`
structure — the synchronous update cannot be written wrongly. Bounded grids need
no boundary special-casing: an edge cell simply has 5 neighbours, a corner 3, and
`sum()` never sees `NA`.

**Friction.** None of note. A 100×100 grid is 10,000 rows per recorded tick, so
`record =` stops being optional — same rule as a growing population.

**Validation.** Blinker period 2; glider translates `(1,1)` every 4 ticks;
R-pentomino stabilises at tick 1103 leaving 116 live cells and 6 escaped gliders;
random-soup density relaxes to ≈ 0.0287.

---

## 2. Elementary 1-D cellular automaton

**Source.** *CA 1D Elementary*, NetLogo Models Library (Computer Science →
Cellular Automata), Wilensky 1998. Wolfram's 256 elementary rules.

**NetLogo sketch.**

```netlogo
patches-own [ on? ]
globals [ rule-list ]                      ;; 8 bits from the rule number

to go
  ask cells-in-next-row [
    let l  [on?] of cell-at (pxcor - 1) (pycor + 1)
    let c  [on?] of cell-at  pxcor      (pycor + 1)
    let r  [on?] of cell-at (pxcor + 1) (pycor + 1)
    set on? item (4 * bool l + 2 * bool c + bool r) rule-list
  ]
end
```

**Spatial ingredients.** 1-D lattice; the rule reads an **ordered** triple
(left, self, right) — the two neighbours are not interchangeable.

**tidyABM — option A (with L1).**

```r
w <- 401
rule_bits <- as.integer(intToBits(90L))[1:8]     # rule 90

ca <- abm_setup(
  agents  = abm_agents(n = w, s = ~as.integer(seq_len(n) == n %/% 2 + 1)),
  network = abm_network(type = "line", dims = w, torus = TRUE),
  globals = list(rule = rule_bits)
)

go <- abm_go(
  abm_neighbours(s_l ~ sum(s), .where = "west"),
  abm_neighbours(s_r ~ sum(s), .where = "east"),
  abm_rules(s ~ rule[[4 * s_l + 2 * s + s_r + 1]])
)

result <- abm_run(ca, go, ticks = 200)
```

**tidyABM — option B (no L1, hand-rolled gather, works with L0 only — in fact
needs no network at all).**

```r
ca <- abm_setup(agents = abm_agents(
  n  = w,
  x  = ~seq_len(n),
  li = ~((x - 2) %% n) + 1,
  ri = ~(x %% n) + 1,
  s  = ~as.integer(x == n %/% 2 + 1)
), globals = list(rule = rule_bits))

go <- abm_go(abm_rules(s ~ rule[[4 * s[li] + 2 * s + s[ri] + 1]]))
```

**Verdict: 🟣 L0 + L1** for the natural form; 🟢 **current** for option B.
`s[li]` inside a simultaneous rule reads start-of-tick `s`, which is correct for a
CA, so option B is genuinely expressible today. But the manual index gather is
exactly the kind of thing the package elsewhere turns into a step, and it does not
generalise (a 2-D "cell to my north" would need four such columns).

**Friction.** L1 is the one place the network framing needs a genuinely new idea:
a graph edge has no direction, so "left" vs "right" must come either from a
direction label on the lattice edges or from coordinates. Option A2 (the lattice
publishes `s_west` / `s_east` directly into `abm_rules`) is terser still but lets
`abm_rules()` read neighbour values, which today only match and `abm_neighbours()`
do.

**Validation.** Rule 90: row *t* equals `choose(t, 0:t) %% 2` exactly (Pascal's
triangle mod 2 / the Sierpiński pattern). Rule 30's centre column is aperiodic.
The result tibble, `tick × x × s`, is already the space-time diagram —
`ggplot(result, aes(x, tick, fill = s)) + geom_raster()`.

---

## 3. Forest fire

**Source.** *Fire*, NetLogo Models Library (Earth Science), Wilensky 1997.

**NetLogo sketch.**

```netlogo
to setup
  clear-all
  ask patches [ if random-float 100 < density [ set pcolor green ] ]     ;; tree
  ask patches with [ pxcor = min-pxcor and pcolor = green ]
    [ set pcolor red ]                                                    ;; ignite left edge
  reset-ticks
end

to go
  if all? patches [ pcolor != red ] [ stop ]
  ask patches with [ pcolor = red ] [
    ask neighbors4 with [ pcolor = green ] [ set pcolor red ]
    set pcolor red - 3.5                                                  ;; now burnt
  ]
  tick
end
```

(`ask` fixes its agentset at call time, so a patch lit this tick spreads only
next tick — an implicit one-tick state machine.)

**Spatial ingredients.** Patches only; von Neumann-4; state machine
empty → tree → burning → burnt; run to completion.

**tidyABM.**

```r
w <- h <- 250
density <- 0.60

# 1. the world: just the forest — no coordinates, no ignition
fire <- abm_setup(
  agents  = abm_agents(
    state = ~dplyr::if_else(runif(n) < density, "tree", "empty")
  ),
  network = abm_network(type = "grid", dims = c(w, h),
                        diagonals = FALSE, torus = FALSE)
)

# 2. the tick: ignite where you choose, then spread
go <- abm_go(
  abm_rules(state ~ dplyr::if_else(state == "tree" & .x == 1, "burning", state)),
  abm_neighbours(hot ~ any(state == "burning")),
  abm_rules(state ~ dplyr::case_when(
    state == "burning"    ~ "burnt",
    state == "tree" & hot ~ "burning",
    TRUE                  ~ state))
)

result <- abm_run(fire, go, ticks = 3 * w, seed = 1, record = 10)
```

**Verdict: 🔵 L0.** Only `diagonals = FALSE`. Setup is now a single random field;
ignition is its own `abm_rules` step, so the start pattern is one swappable line
(`.x == 1` for the left edge, `.x == w %/% 2 & .y == h %/% 2` for a centre point,
`.x %in% c(1, w) | .y %in% c(1, h)` for all edges).

**Friction.** The ignition step re-runs every tick; it is a silent no-op once the
target cells are no longer `"tree"`, which is fine for this absorbing model but
turns into a permanent fire source the moment regrowth is added. Placing ignition
*before* `abm_neighbours` lets the fire it lights be seen the same tick.

For the *fraction-burned-vs-density* study a run is an experiment, so wrap the
spread in `abm_repeat(until = !any(state == "burning"), max = w * h)` and run
`ticks = 1` — one run, one completed fire — then sweep `density` across worlds.
Bare `ticks = 3 * w` (shown) is the animation form: the front advances one ring
per tick and the model goes quiescent on its own.

**Validation.** Fraction of trees burned vs `density` shows a sharp transition
near the square-lattice site-percolation threshold, *p*c ≈ 0.5927. `diagonals =
FALSE` is load-bearing for that number — Moore spread drops it to ≈ 0.41.

---

## 4. Schelling segregation (with geography)

**Source.** *Segregation*, NetLogo Models Library (Social Science),
Wilensky 1997.

**NetLogo sketch.**

```netlogo
turtles-own [ happy? ]

to go
  if all? turtles [ happy? ] [ stop ]
  ask turtles with [ not happy? ] [ find-new-spot ]      ;; move to a random empty patch
  ask turtles [
    let near turtles-on neighbors
    let same count near with [ color = [color] of myself ]
    set happy? same >= %-similar-wanted / 100 * count near
  ]
  tick
end

to find-new-spot
  rt random-float 360  fd random-float 10
  if any? other turtles-here [ find-new-spot ]
  move-to patch-here
end
```

**Spatial ingredients.** One agent per patch; relocation to a random empty patch;
happiness from the *occupied* fraction of the Moore neighbourhood. Patches carry
no state — they are slots.

**tidyABM — cells as agents, occupancy as a column.**

```r
w <- h <- 51
vacancy <- 0.10; minority <- 0.30; tol <- 0.30
n_cells <- w * h; n_occ <- round(n_cells * (1 - vacancy))

schelling <- abm_setup(
  agents = abm_agents(
    n    = n_cells,
    occ  = ~sample(c(rep(TRUE, n_occ), rep(FALSE, n - n_occ))),
    type = ~dplyr::if_else(occ, sample(c("A", "B"), n, TRUE,
                                       c(1 - minority, minority)), NA_character_)
  ),
  network = abm_network(type = "grid", dims = c(w, h),
                        diagonals = TRUE, torus = TRUE),
  globals = list(tol = tol)
)

go <- abm_go(
  abm_neighbours(same ~ mean(type == own_type, na.rm = TRUE)),
  abm_rules(unhappy ~ occ & !is.na(same) & same < tol),

  abm_match(pair = "opposite_group", by = occ,
            eligible = unhappy | !occ),               # unhappy cells <-> empty cells

  abm_rules(
    type ~ dplyr::if_else(!occ & !is.na(partner_type), partner_type,
             dplyr::if_else(unhappy, NA_character_, type)),
    occ  ~ dplyr::case_when(!occ & !is.na(partner_type) ~ TRUE,
                            unhappy ~ FALSE, TRUE ~ occ))
)

result <- abm_run(schelling, go, ticks = 100, seed = 6)
```

**Verdict: 🔵 L0.** No new matching machinery. The relocation is a mutual
`opposite_group` match between unhappy cells and empty cells, and its exclusion
guarantee — two families cannot take one house — is the same one that stops two
wolves eating one sheep. The lattice never moves; a resident relocating is a data
write on two rows.

**Friction.** This is *synchronous* relocation (every unhappy resident moves at
once), which is what the NetLogo `move-unhappy-turtles` sweep also does. A
one-at-a-time variant would need `abm_sequential`, which cannot currently read
other agents' rows. Modelling choice: an occupied cell whose neighbourhood is
entirely empty comes out `NaN` and is treated as content here; making it unhappy
instead is a one-token change.

**Validation.** With `%-similar-wanted = 30`, the mean same-type neighbour share
rises from ≈ 0.5 to ≈ 0.72. Direct counterpart to model 6, *segregation without
geography* — the same result with the space put back.

---

## 5. Wolf–Sheep–Grass

**Source.** *Wolf Sheep Predation*, NetLogo Models Library (Biology),
Wilensky 1997.

**NetLogo sketch.**

```netlogo
breed [ sheep a-sheep ]
breed [ wolves wolf ]
turtles-own [ energy ]
patches-own [ countdown ]

to go
  ask sheep   [ move  set energy energy - 1  eat-grass   death  reproduce ]
  ask wolves  [ move  set energy energy - 1  catch-sheep death  reproduce ]
  ask patches [ grow-grass ]
  tick
end

to move        [ rt random 50  lt random 50  fd 1 ]
to eat-grass   [ if pcolor = green [ set pcolor brown  set energy energy + gain ] ]
to catch-sheep [ let p one-of sheep-here
                 if p != nobody [ ask p [ die ]  set energy energy + gain ] ]
to grow-grass  [ if pcolor = brown [ ifelse countdown <= 0
                   [ set pcolor green  set countdown regrow-time ]
                   [ set countdown countdown - 1 ] ] ]
```

**Spatial ingredients.** Two mobile breeds on a patch substrate; grass is
per-patch state; turtles read and write the patch they stand on; a wolf eats a
sheep sharing its patch; local movement; birth and death.

**tidyABM (lattice analogue of the continuous movement).**

```r
w <- h <- 50

world <- abm_setup(
  agents = list(
    patches = abm_agents(                                   # no n — comes from the grid
                grass     = ~sample(c(TRUE, FALSE), n, TRUE),
                countdown = ~sample.int(30, n, TRUE)),
    sheep   = abm_agents(n = 100, energy = ~runif(n, 4, 8)),   # .cell injected, random-placed
    wolves  = abm_agents(n = 50,  energy = ~runif(n, 4, 8))
  ),
  network = abm_network(type = "grid", dims = c(w, h), on = "patches",
                        diagonals = TRUE, torus = TRUE),
  globals = list(regrow = 30, sheep_gain = 4, wolf_gain = 20,
                 sheep_repro = 0.04, wolf_repro = 0.05)
)
# equivalently: patches = abm_grid(dims = c(w, h), grass = ~..., countdown = ~...)

go <- abm_go(
  # move, then pay the metabolic cost
  abm_move(along = "patches", to = "random_neighbour", who = c("sheep", "wolves")),
  abm_rules(energy ~ energy - 1),

  # sheep eat the grass on their cell
  abm_neighbours(grass_here ~ any(grass),
                 within = .group == "patches" & .id == own_.cell),
  abm_rules(energy ~ dplyr::if_else(grass_here, energy + sheep_gain, energy)),
  abm_tell(grass ~ FALSE, countdown ~ regrow, to = .cell, when = grass_here),

  # wolves eat a sheep sharing their cell
  abm_match(pair = "opposite_group", by = .group, .by = .cell,
            eligible = .group %in% c("wolves", "sheep")),
  abm_rules(energy ~ dplyr::if_else(.group == "wolves" & !is.na(.partner),
                                    energy + wolf_gain, energy)),
  abm_death(when = .group == "sheep" & !is.na(.partner)),

  # starve
  abm_death(when = .group %in% c("sheep", "wolves") & energy < 0),

  # reproduce — clone, split energy
  abm_birth(when = .group == "sheep"  & runif(dplyr::n()) < sheep_repro,
            cost = energy ~ energy / 2),
  abm_birth(when = .group == "wolves" & runif(dplyr::n()) < wolf_repro,
            cost = energy ~ energy / 2),

  # grass regrows
  abm_rules(
    grass     ~ grass | countdown == 0,
    countdown ~ dplyr::case_when(grass          ~ countdown,
                                 countdown <= 0 ~ regrow,
                                 TRUE           ~ countdown - 1),
    .scope = "population")
)

result <- abm_run(world, go, ticks = 500, seed = 1, record = 10)
```

**Verdict: 🟠 L2.** Forces the whole bundle:

- `abm_network(on = "patches")` — wire the patch group only, not the turtles.
- `.cell` — an engine-owned reserved column (`abm_agents()` rejects `.`-prefixed
  names, so it must be injected like `.id`), holding a patch `.id`.
- `within = .id == own_.cell` — a co-location join; expressible today but O(n²)
  without an equijoin fast path.
- `abm_tell(to = .cell)` — `to =` must accept an id-valued column (close to the
  existing "an agent a global names").
- `abm_match(.by = .cell)` — match *within* each patch. This is `.by`-grouping,
  which `abm_rules()` already has, applied to matching. One small, coherent
  option, and it is what makes every co-located turtle-turtle interaction
  (predation, mating, combat) expressible.
- `abm_move(along = "patches", to = "random_neighbour")` — the one genuinely new
  step. The mover needs the adjacency of the patch it stands on, and it is not on
  the patch network; `abm_move` is the step that knows both sides.

**Friction.** The NetLogo original moves continuously (`fd 1` with a random turn);
the lattice version drops the heading. That is a faithful discrete analogue, not
the same model — worth stating on the page the way the corpus states other
simplifications. `catch-sheep` is `one-of sheep-here`, i.e. one sheep per wolf per
tick; `opposite_group` with `.by = .cell` gives exactly that (a mutual match
inside each patch), including the case of several wolves and one sheep, which the
predator–prey lesson (model 56) showed must not pair the sheep with all of them.

**Validation.** In the standard parameter regime the two populations settle into
bounded out-of-phase oscillations (the classic predator–prey cycle).

---

## 6. Ants (pheromone foraging)

**Source.** *Ants*, NetLogo Models Library (Biology), Wilensky 1997.

**NetLogo sketch.**

```netlogo
patches-own [ chemical food nest? nest-scent ]

to go
  ask turtles [
    ifelse carrying-food? [ return-to-nest ] [ look-for-food ]
    wiggle  fd 1
  ]
  diffuse chemical (diffusion-rate / 100)
  ask patches [ set chemical chemical * (100 - evaporation-rate) / 100 ]
  tick
end

to look-for-food
  if food > 0 [ set carrying-food? true  rt 180  stop ]
  if chemical >= 0.05 [ uphill-chemical ]                 ;; follow the trail
end

to return-to-nest
  ifelse nest? [ set carrying-food? false  rt 180 ]
    [ set chemical chemical + 60  uphill-nest-scent ]     ;; lay trail, head home
end
```

**Spatial ingredients.** Everything in Wolf–Sheep, plus a diffusing/evaporating
scalar field on patches, plus **gradient-following movement** (step toward the
neighbour that maximises a patch variable), plus a static `nest-scent` gradient.

**tidyABM.**

```r
w <- h <- 71; cx <- 36; cy <- 36

world <- abm_setup(
  agents = list(
    patches = abm_agents(n = w * h,
                x = ~rep(seq_len(w), times = h), y = ~rep(seq_len(h), each = w),
                nest       = ~sqrt((x - cx)^2 + (y - cy)^2) < 5,
                nest_scent = ~200 - sqrt((x - cx)^2 + (y - cy)^2),
                food = ~as.integer(((x-cx-18)^2 + (y-cy)^2 < 9) |
                                   ((x-cx)^2 + (y-cy+22)^2 < 9)),
                chemical = 0),
    ants = abm_agents(n = 100, carrying = FALSE)     # .cell injected, starts at nest
  ),
  network = abm_network(type = "grid", dims = c(w, h), on = "patches",
                        diagonals = TRUE, torus = FALSE),
  globals = list(evap = 0.10, diff = 0.30)
)

go <- abm_go(
  # patch field: NetLogo `diffuse` + evaporation  (idiom over existing steps)
  abm_neighbours(inflow ~ sum(chemical)),
  abm_rules(chemical ~ (1 - evap) * ((1 - diff) * chemical + diff * inflow / 8),
            .scope = "population"),

  # ant senses its own patch
  abm_neighbours(here_food ~ sum(food), here_nest ~ any(nest),
                 here_chem ~ sum(chemical),
                 within = .id == own_.cell & .group == "patches"),

  # pick up / drop
  abm_rules(carrying ~ dplyr::case_when(!carrying & here_food > 0 ~ TRUE,
                                         carrying & here_nest      ~ FALSE,
                                         TRUE ~ carrying),
            .scope = "population"),

  # lay trail on the current patch while carrying
  abm_tell(chemical ~ chemical + 60, to = .cell, when = carrying, .resolve = "sum"),

  # step to the best adjacent patch: toward nest if carrying, up the trail if not
  abm_move(along = "patches", who = "ants",
           to = uphill(dplyr::if_else(carrying, nest_scent, chemical)))
)

result <- abm_run(world, go, ticks = 1000, seed = 1, record = 20)
```

**Verdict: 🟠 L2** (no tier beyond the bundle, but it exercises all of it). The
diffusion is a documented two-line idiom, not a new step. `abm_move(to = uphill(expr))`
is the argmax flavour of the same `abm_move` Wolf–Sheep needs — the step evaluates
`expr` over the current patch's neighbours and moves to the maximiser.

**Friction.** As with Wolf–Sheep, the continuous heading and `wiggle` are gone;
this is "lattice ants". Real ants sniff at ±45° from their current heading, which
gives trail-following its characteristic wandering; a pure lattice argmax makes
straighter, more brittle trails. Whether that reproduces the qualitative result
is exactly what building it would test. A dedicated `abm_diffuse(chemical, rate)`
would be sugar; only this model needs it here, so it stays an idiom until a second
does.

**Validation.** Trails form between the nest and each food pile; the nearest pile
is exploited first; all piles are eventually consumed and the trails to spent
piles evaporate.

---

## 7. Flocking (Boids)

**Source.** *Flocking*, NetLogo Models Library (Biology), Wilensky 1998.

**NetLogo sketch.**

```netlogo
turtles-own [ flockmates nearest-neighbor ]

to go
  ask turtles [ flock ]
  repeat 5 [ ask turtles [ fd 0.2 ] ]
  tick
end

to flock
  set flockmates other turtles in-radius vision
  if any? flockmates [
    set nearest-neighbor min-one-of flockmates [ distance myself ]
    ifelse distance nearest-neighbor < minimum-separation
      [ turn-away ([heading] of nearest-neighbor) max-separate-turn ]
      [ turn-towards average-flockmate-heading max-align-turn
        turn-towards average-heading-towards-flockmates max-cohere-turn ]
  ]
end
```

**Spatial ingredients.** Continuous space; **no patches**; neighbours are other
turtles within radius `vision`; three steering rules on `heading`.

**tidyABM.**

```r
n <- 300; vision <- 3; sep <- 1

flock <- abm_setup(agents = abm_agents(
  n = n,
  x = ~runif(n, 0, 100), y = ~runif(n, 0, 100),
  heading = ~runif(n, 0, 2 * pi)
))

go <- abm_go(
  # neighbourhood in attribute (here, physical) space
  abm_neighbours(
    n_mates ~ sum(TRUE),
    sin_h   ~ sum(sin(heading)),  cos_h ~ sum(cos(heading)),
    sin_b   ~ sum(sin(atan2(y - own_y, x - own_x))),
    cos_b   ~ sum(cos(atan2(y - own_y, x - own_x))),
    d_near  ~ min(sqrt((x - own_x)^2 + (y - own_y)^2)),
    within  = (x - own_x)^2 + (y - own_y)^2 < vision^2 & .id != own_.id
  ),
  abm_rules(
    heading ~ dplyr::case_when(
      is.na(n_mates)      ~ heading,
      d_near < sep        ~ heading + 0.3 * sin(heading - atan2(sin_b, cos_b) - pi),
      TRUE                ~ heading + 0.1 * sin(atan2(sin_h, cos_h) - heading)
                                    + 0.1 * sin(atan2(sin_b, cos_b) - heading))
  ),
  abm_rules(x ~ (x + cos(heading)) %% 100,
            y ~ (y + sin(heading)) %% 100)
)

result <- abm_run(flock, go, ticks = 500, seed = 1, record = 5)
```

**Verdict: 🟢 current.** No lattice, no new medium, no movement primitive.
Continuous-space flocking is columns (`x`, `y`, `heading`), an attribute-space
neighbourhood, and arithmetic — all of which exist. This is the useful
counterpoint: continuous space needs *less* than a grid does.

**Friction.**

- `abm_neighbours(within =)` is O(n²); at a few hundred birds that is fine, at
  tens of thousands it is not. A spatial-index fast path would help but is
  performance, not grammar.
- Circular means must be done as `atan2(sum(sin), sum(cos))` — expressible, shown
  above, but wordy. Sugar (`circ_mean(heading)`) would read better.
- Torus distance is `pmin(abs(dx), 100 - abs(dx))`; omitted above for brevity,
  which means birds near the edge do not see across it.
- The turn-angle limiters (`max-align-turn` etc.) are dropped here; adding them is
  more `case_when`, not new machinery.

**Validation.** An order parameter — the mean of the normalised velocity vectors —
rises from ≈ 0 to near 1 as coherent flocks form.

---

## 8. Sugarscape (immediate growback)

**Source.** *Sugarscape 1 Immediate Growback*, NetLogo Models Library (Social
Science), Li & Wilensky 2009, after Epstein & Axtell 1996.

**NetLogo sketch.**

```netlogo
patches-own [ psugar max-psugar ]
turtles-own [ sugar metabolism vision ]

to go
  ask turtles [
    move-to-max-sugar-in-vision            ;; look along the 4 axes, up to `vision` cells
    set sugar sugar - metabolism + [psugar] of patch-here
    ask patch-here [ set psugar 0 ]
    if sugar <= 0 [ die ]
  ]
  ask patches [ set psugar max-psugar ]     ;; immediate growback
  tick
end
```

**Spatial ingredients.** Wolf–Sheep's bundle, but movement is a **ranged**
directional scan: the turtle looks up to `vision` cells along each of the four
axes and jumps to the visible unoccupied patch with the most sugar.

**tidyABM.**

```r
w <- h <- 50

sugar <- abm_setup(
  agents = list(
    patches = abm_agents(n = w * h,
                x = ~rep(seq_len(w), times = h), y = ~rep(seq_len(h), each = w),
                max_psugar = ~pmax(0, 4 - round(sqrt((x - 15)^2 + (y - 35)^2) / 4)) +
                              pmax(0, 4 - round(sqrt((x - 35)^2 + (y - 15)^2) / 4)),
                psugar = ~max_psugar),
    people = abm_agents(n = 250,
                sugar = ~runif(n, 5, 25), metabolism = ~sample(1:4, n, TRUE),
                vision = ~sample(1:6, n, TRUE))
  ),
  network = abm_network(type = "grid", dims = c(w, h), on = "patches",
                        diagonals = FALSE, torus = TRUE)
)

go <- abm_go(
  abm_move(along = "patches", who = "people", range = vision, axes_only = TRUE,
           to = uphill(psugar), avoid_occupied = TRUE),
  abm_neighbours(here_sugar ~ sum(psugar),
                 within = .id == own_.cell & .group == "patches"),
  abm_rules(sugar ~ sugar - metabolism + here_sugar, .scope = "population"),
  abm_tell(psugar ~ 0, to = .cell),
  abm_death(when = .group == "people" & sugar <= 0),
  abm_rules(psugar ~ max_psugar, .scope = "population")   # immediate growback
)

result <- abm_run(sugar, go, ticks = 300, seed = 1, record = 10)
```

**Verdict: 🔴 L2+.** Everything in the L2 bundle, plus `abm_move` needs two
parameters the Wolf–Sheep/Ants forms do not: `range =` (scan more than one cell
out) and `axes_only =` / `avoid_occupied =`. These are not a new concept — they
are arguments on the step that already has to exist — but they are beyond the
minimal bundle and should be added only when a model forces them, which this one
does and the earlier two do not.

**Friction.** `avoid_occupied` re-introduces the occupancy question from Schelling
inside `abm_move`: the step has to know which target cells hold a `people` agent.
That is a reverse co-location lookup (`.cell` values currently held by the moving
group), which the step can compute, but it couples `abm_move` to the mover group's
own positions.

**Validation.** The wealth (`sugar`) distribution across surviving agents becomes
strongly right-skewed — the model's headline result — from an near-uniform start.

---

## 9. Langton's Ant

**Source.** *Turtle-based Langton's Ant*; NetLogo Models Library has this as a
*Turmites* variant (Computer Science → Cellular Automata).

**NetLogo sketch.**

```netlogo
patches-own [ white? ]

to setup
  clear-all
  ask patches [ set white? true ]
  create-turtles 1 [ setxy 0 0  set heading 0 ]
  reset-ticks
end

to go
  ask turtles [
    ifelse [white?] of patch-here [ rt 90 ] [ lt 90 ]      ;; turn by the cell's colour
    ask patch-here [ set white? not white? ]               ;; flip the cell
    fd 1                                                    ;; step in the new heading
  ]
  tick
end
```

**Spatial ingredients.** One mobile agent; a heading that is part of its state;
tight coupling — the agent reads the cell under it, flips it, then moves one cell
**in the direction it is now facing**.

**tidyABM.**

```r
w <- h <- 200

langton <- abm_setup(
  agents = list(
    patches = abm_agents(n = w * h, white = TRUE),
    ant     = abm_agents(n = 1, heading = 0L)     # 0 N, 1 E, 2 S, 3 W; .cell = centre
  ),
  network = abm_network(type = "grid", dims = c(w, h), on = "patches",
                        diagonals = FALSE, torus = TRUE)
)

go <- abm_go(
  abm_neighbours(here_white ~ all(white),
                 within = .id == own_.cell & .group == "patches"),
  abm_rules(heading ~ (heading + dplyr::if_else(here_white, 1L, 3L)) %% 4L,
            .scope = "population"),
  abm_tell(white ~ !white, to = .cell),
  abm_move(along = "patches", who = "ant", direction = heading)     # heading-relative step
)

result <- abm_run(langton, go, ticks = 12000, seed = 1, record = 200)
```

**Verdict: 🔴 L2+.** The L2 bundle covers reading the cell (`within` join), flipping
it (`abm_tell(to = .cell)`) and turning (`abm_rules` on `heading`). The new thing
is **`abm_move(direction = <column>)`**: move one cell along a per-agent heading.
That is L1 (the lattice knows which neighbour is north) *and* L2 (the mover is on
a cell but not on the lattice) at once, and it is the sharpest small requirement
in the whole set — every other `abm_move` flavour picks a neighbour by a value,
this one picks it by a stored compass direction.

**Friction.** `heading` as an integer 0–3 with `%% 4` arithmetic is the clean
encoding; NetLogo's continuous `heading` with `rt 90` is the same thing in
degrees. A single agent means every step here is over a one-row tibble — correct,
but the per-agent machinery is pure overhead for a model that is really one
pointer walking an array.

**Validation.** From an all-white field the trajectory is chaotic for ≈ 10,000
steps, then locks into a period-104 "highway" that translates diagonally forever —
a deterministic, exactly reproducible signature.

---

## 10. Ising model

**Source.** *Ising*, NetLogo Models Library (Chemistry & Physics),
Wilensky 2003.

**NetLogo sketch.**

```netlogo
patches-own [ spin ]

to go
  ask patches [                                   ;; random order; reads current spins
    let Ediff 2 * spin * sum [spin] of neighbors4
    if (Ediff <= 0) or (random-float 1 < exp (- Ediff / temperature))
      [ set spin (- spin) ]
  ]
  tick
end
```

`ask patches` visits sites in a fresh random order and each site reads its
neighbours' **current** spins, so a site updated early in the tick is seen by its
neighbours later in the same tick. This is single-site Glauber/Metropolis
dynamics.

**tidyABM — the checkerboard formulation.**

```r
w <- h <- 100

ising <- abm_setup(
  agents = abm_agents(
    n     = w * h,
    x     = ~rep(seq_len(w), times = h),
    y     = ~rep(seq_len(h), each = w),
    spin  = ~sample(c(1L, -1L), n, TRUE),
    black = ~(x + y) %% 2 == 0
  ),
  network = abm_network(type = "grid", dims = c(w, h),
                        diagonals = FALSE, torus = TRUE),
  globals = list(temp = 2.4)
)

sweep <- function(side) list(
  abm_neighbours(nbr ~ sum(spin)),
  abm_rules(spin ~ {
    Ediff  <- 2 * spin * nbr
    accept <- (Ediff <= 0) | (runif(dplyr::n()) < exp(-Ediff / temp))
    dplyr::if_else(black == side & accept, -spin, spin)
  })
)

go <- do.call(abm_go, c(sweep(TRUE), sweep(FALSE)))   # update one sublattice, then the other
result <- abm_run(ising, go, ticks = 2000, seed = 1, record = 50)
```

**Verdict: 🔵 L0 for the checkerboard form; 🔴 for random-sequential single-site.**
On a bipartite lattice every site's neighbours lie on the *other* sublattice, so
all black sites can be updated at once and then all white sites — a standard,
physically valid parallel scheme. That needs only L0.

The literal NetLogo dynamics — one site at a time, in random order, each reading
neighbours that may already have flipped this tick — is **not** expressible:
`abm_sequential()` processes agents one at a time but can read only its own row
and globals, never a neighbour. Making it neighbour-aware would be a real
addition, and this is the model that would force the question.

**Friction.** This belongs in the same category as the corpus's three *corrections*
— a model whose short form (`abm_rules` over the whole lattice, no sublattice
split) runs and is subtly wrong: simultaneous updates on a bipartite graph
oscillate rather than equilibrate. The page would have to say so.

**Validation.** Below the Onsager temperature *T*c = 2 / ln(1 + √2) ≈ 2.269 the
mean spin per site settles to a non-zero magnetisation; the susceptibility peaks
at *T*c. Both are quantitative checks the corpus would want.

---

## 11. Daisyworld

**Source.** *Daisyworld*, NetLogo Models Library (Biology), after Watson &
Lovelock 1983.

**NetLogo sketch.**

```netlogo
patches-own [ temperature daisy-type age ]     ;; "white" | "black" | "none"
globals    [ global-temperature ]

to go
  ask patches [ set temperature local-heating temperature ]   ;; from albedo + luminosity
  diffuse temperature 0.5
  ask patches with [ daisy-type != "none" ] [
    set age age + 1
    if age > max-age [ set daisy-type "none"  set age 0 ]
  ]
  ask patches with [ daisy-type = "none" ] [
    let seed one-of neighbors with [ daisy-type != "none" ]
    if seed != nobody and random-float 1 < seed-threshold temperature
      [ set daisy-type [daisy-type] of seed ]
  ]
  set global-temperature mean [temperature] of patches
  tick
end
```

**Spatial ingredients.** Patches only, but a two-way coupling to a global:
patch albedo sets local temperature, temperature diffuses, temperature gates
reproduction into empty neighbours, and the mean temperature is a global the
whole world reports.

**tidyABM.**

```r
w <- h <- 50

daisy <- abm_setup(
  agents = abm_agents(
    n    = w * h,
    kind = ~sample(c("white", "black", "none"), n, TRUE, c(0.2, 0.2, 0.6)),
    age  = 0L
  ),
  network = abm_network(type = "grid", dims = c(w, h),
                        diagonals = TRUE, torus = TRUE),
  globals = list(luminosity = 1.0, global_temp = 0)
)

go <- abm_go(
  abm_rules(
    albedo  ~ dplyr::case_when(kind == "white" ~ 0.75, kind == "black" ~ 0.25,
                               TRUE ~ 0.4),
    local_t ~ luminosity * (1 - albedo) * 40 - 5,
    .scope = "population"
  ),
  abm_neighbours(t_in ~ mean(local_t)),
  abm_rules(temperature ~ 0.5 * local_t + 0.5 * t_in),

  abm_rules(age  ~ dplyr::if_else(kind != "none", age + 1L, 0L),
            kind ~ dplyr::if_else(age > 25L, "none", kind), .scope = "population"),

  abm_match(pair = "network", eligible = kind == "none"),
  abm_rules(kind ~ dplyr::if_else(
    !is.na(partner_kind) & partner_kind != "none" &
      runif(dplyr::n()) < pmax(0, 1 - ((temperature - 22.5) / 17.5)^2),
    partner_kind, kind)),

  abm_global(global_temp ~ mean(temperature))
)

result <- abm_run(daisy, go, ticks = 400, seed = 1, record = 10)
```

**Verdict: 🔵 L0.** A rich model with a homeostatic feedback loop, and it needs
nothing past the lattice constructor. The global↔population coupling is the same
shape El Farol and the zakah models already use; seeding an empty patch from a
random daisy neighbour is `abm_match(pair = "network")` — a step that exists.

**Friction.** `diffuse temperature 0.5` is the two-line
`abm_neighbours` + `abm_rules` idiom again; the `0.5 * local_t + 0.5 * t_in`
weighting is an approximation of NetLogo's exact `diffuse` share. Age reset on
death is a plain `if_else`.

**Validation.** The headline Gaia result: equilibrium `global_temp` stays roughly
flat across a wide range of `luminosity`, held there by the shifting black/white
daisy mix — plot equilibrium temperature vs luminosity and the middle band is
level.

---

## 12. Traffic (car-following on a ring)

**Source.** *Traffic Basic*, NetLogo Models Library (Social Science),
Wilensky 1997.

**NetLogo sketch.**

```netlogo
turtles-own [ speed ]

to go
  ask turtles [
    let ahead one-of turtles-on patch-ahead 1
    ifelse ahead != nobody
      [ set speed [speed] of ahead  decelerate ]
      [ accelerate  if speed > speed-limit [ set speed speed-limit ] ]
    fd speed
  ]
  tick
end
```

Cars on a single-lane loop; each reacts to the nearest car ahead; phantom jams
emerge from a uniform start.

**Spatial ingredients.** A 1-D ring; **no patches needed**; the one relation is
"the nearest agent ahead of me along the line".

**tidyABM.**

```r
n_cars <- 40

traffic <- abm_setup(
  agents  = abm_agents(n = n_cars, pos = ~sort(runif(n, 0, 200)), speed = 0.1),
  globals = list(L = 200, vmax = 1.0, accel = 0.02, decel = 0.4, lookahead = 25)
)

go <- abm_go(
  abm_match(pair = "nearest",
            cost = { d <- (pos - own_pos) %% L
                     dplyr::if_else(d > 0 & d < lookahead, d, NA_real_) }),
  abm_rules(
    speed ~ dplyr::if_else(
      !is.na(partner_pos) & ((partner_pos - pos) %% L) < 2,
      pmax(0, partner_speed - decel),
      pmin(speed + accel, vmax)),
    .scope = "population"
  ),
  abm_rules(pos ~ (pos + speed) %% L, .scope = "population")
)

result <- abm_run(traffic, go, ticks = 2000, seed = 1, record = 5)
```

**Verdict: 🟢 current.** No lattice, no new step. "The next car ahead" is a
directional `pair = "nearest"` with a `cost` expression that is `NA` behind the
car and outside the look-ahead window — machinery that already exists for
Hotelling and the garbage-can model. `partner_speed` then carries the crude
"match the speed of the car ahead" rule verbatim.

**Friction.** The match is O(n²) for what is really a sort plus `lead()`; an
ordered windowed rule (`abm_rules(.order = pos)` that could see `lead(pos)`) would
be the natural form and does not exist. Ring wrap is the `%% L` idiom, as in
Flocking.

**Validation.** Stop-and-go waves form spontaneously and travel backward against
the direction of motion; mean speed collapses then oscillates around a value well
below `vmax`.

---

## 13. Rebellion (civil violence)

**Source.** *Rebellion*, NetLogo Models Library (Social Science),
Wilensky 2004, after Epstein 2002.

**NetLogo sketch.**

```netlogo
breed [ people person ]
breed [ cops cop ]
people-own [ risk-aversion perceived-hardship active? jail-term ]

to go
  ask turtles [ move ]                                   ;; to a random empty cell in vision
  ask people with [ jail-term = 0 ] [
    let C count cops in-radius vision
    let A 1 + count (people in-radius vision) with [ active? ]
    let arrest 1 - exp (- 2.3 * C / A)
    let grievance perceived-hardship * (1 - gov-legitimacy)
    set active? (grievance - risk-aversion * arrest > threshold)
  ]
  ask cops [
    let suspect one-of (people in-radius vision) with [ active? ]
    if suspect != nobody
      [ ask suspect [ set active? false  set jail-term random max-jail-term ]
        move-to suspect ]
  ]
  ask people [ if jail-term > 0 [ set jail-term jail-term - 1 ] ]
  tick
end
```

**Spatial ingredients.** Two mobile groups on a grid of bare slots;
**vision-radius counts among the turtle groups**; movement to a random empty cell
within vision; a targeted arrest (cop writes into one nearby active person's row).

**tidyABM.**

```r
w <- h <- 40

rebel <- abm_setup(
  agents = list(
    people = abm_agents(n = 1120, risk = ~runif(n), hardship = ~runif(n),
                        active = FALSE, jail = 0L),
    cops   = abm_agents(n = 64)
  ),
  network = abm_network(type = "grid", dims = c(w, h), on = "patches", torus = TRUE),
  globals = list(legitimacy = 0.82, threshold = 0.1, vision = 7, max_jail = 30)
)

go <- abm_go(
  abm_move(along = "patches", to = "random_empty_neighbour",
           who = c("people", "cops")),

  abm_neighbours(
    C ~ sum(.group == "cops"),
    A ~ 1 + sum(.group == "people" & active),
    within = .group != "patches" & .id != own_.id &
             abs(.x - own_.x) <= vision & abs(.y - own_.y) <= vision
  ),
  abm_rules(
    active ~ jail == 0L &
      (hardship * (1 - legitimacy) - risk * (1 - exp(-2.3 * C / A)) > threshold),
    .scope = "population"
  ),

  abm_match(pair = "one_of", eligible = .group == "cops",
            among = .group == "people" & active),
  abm_tell(active ~ FALSE, jail ~ sample.int(max_jail, 1L),
           to = .partner, when = !is.na(.partner)),

  abm_rules(jail ~ pmax(0L, jail - 1L), .scope = "population")
)

result <- abm_run(rebel, go, ticks = 300, seed = 1, record = 5)
```

**Verdict: 🟠 L2** (plus the occupancy-aware `abm_move` that Sugarscape also
wants). A canonical social-science model landing in the same bucket as
Wolf–Sheep is the useful signal here — the bundle is not biology-specific.

**Friction.** The vision count is `within` on `.x` / `.y`, which means turtles
need coordinates, not just a `.cell` id. The clean rule: **`.cell`, `.x`, `.y`
move together** — the engine keeps `.x` / `.y` equal to the coordinates of `.cell`
for any agent that has one. The arrest is `abm_match(pair = "one_of", among =)`
plus `abm_tell(to = .partner)` — both current — so the *targeted write* half of
the model needs nothing new.

**Validation.** Punctuated equilibrium: long quiet stretches broken by sudden
short bursts of widespread `active` rebellion; lowering `legitimacy` past a
threshold flips the system to sustained rebellion.

---

## Summary

| # | model | medium | verdict | primitives forced |
|---|---|---|---|---|
| 1 | Game of Life | patch-only | 🔵 L0 | grid network type |
| 2 | Elementary 1-D CA | patch-only, 1-D | 🟣 L0 + L1 | line network, named neighbours |
| 3 | Forest Fire | patch-only | 🔵 L0 | grid network (von Neumann) |
| 4 | Schelling (geo) | one turtle per patch | 🔵 L0 | grid network; relocation is an existing `opposite_group` match |
| 5 | Wolf–Sheep–Grass | turtle ≠ patch | 🟠 L2 | `on =`, `.cell`, co-location join, `abm_tell(to = col)`, `abm_match(.by =)`, `abm_move` |
| 6 | Ants | turtle ≠ patch | 🟠 L2 | L2 bundle + `abm_move(to = uphill())`; diffusion is an idiom |
| 7 | Flocking | continuous, no patch | 🟢 current | none (perf + circular-mean sugar would help) |
| 8 | Sugarscape | turtle ≠ patch | 🔴 L2+ | L2 bundle + `abm_move(range =, axes_only =, avoid_occupied =)` |
| 9 | Langton's Ant | turtle ≠ patch, 1 agent | 🔴 L2+ | L2 bundle + `abm_move(direction = <heading col>)` |
| 10 | Ising model | patch-only | 🔵 L0 (checkerboard) / 🔴 (random-sequential) | grid network; else neighbour-aware `abm_sequential` |
| 11 | Daisyworld | patch-only + global feedback | 🔵 L0 | grid network; global coupling and neighbour-seeding already exist |
| 12 | Traffic Basic | continuous 1-D ring | 🟢 current | none (`pair = "nearest"` + `cost =` gives "next car ahead") |
| 13 | Rebellion | turtle ≠ patch | 🟠 L2 | L2 bundle + occupancy-aware `abm_move`; `.x`/`.y` synced from `.cell` |

Tally: **5 need only L0** (1, 3, 4, 10, 11), **1 needs L0 + L1** (2), **2 work
today** (7, 12), **2 need the L2 bundle** (5, 13), **1 more is L2 with a named
argument** (6), **3 sit at the L2+ edge** (8, 9, and the strict form of 10).

## Primitive inventory

Ordered by how many of the thirteen force it, then by size.

| primitive | spec | forced by | size |
|---|---|---|---|
| `abm_network(type = "grid" \| "line")` | `dims`, `diagonals`, `torus`; injects `.x`, `.y`; the wired group inherits `n = prod(dims)`; lattice built before agent columns so `.x`/`.y` are in scope in setup | 1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 13 | one constructor branch; no new step |
| `abm_grid()` sugar | `abm_agents()` group + `abm_network(type = "grid", on = <it>)` in one call; desugars, so no new machinery | any patch model (optional) | thin wrapper |
| named neighbours (L1) | `abm_neighbours(..., .where = "west")` **or** direction-labelled lattice edges | 2, 9 | new idea (edge direction), small code |
| `abm_network(on = <group>)` | wire one named group instead of the whole population | 5, 6, 8, 9, 13 | one argument |
| `.cell` reserved column, with `.x`/`.y` kept in sync | engine-owned location column holding a patch `.id`; `.x`/`.y` track it; carve-out from the `.`-prefix ban | 5, 6, 8, 9, 13 | small |
| `within = (a == b)` equijoin | recognise an equality predicate and hash-join instead of building all pairs | 5, 6, 8, 9, 13 (perf); 7 (would help) | engine, not grammar |
| `abm_tell(to = <id column>)` | `to =` accepts a column of agent ids | 5, 6, 8, 9, 13 | small extension of existing `to =` |
| `abm_match(.by = <column>)` | partition the match within each value of a column (match "inside each patch") | 5, 8 | reuses `abm_rules(.by =)`; small |
| `abm_move(along =, to =, who =)` | reassign `.cell` to a chosen network-neighbour of the current cell; `to = "random_neighbour"` or `uphill(expr)` | 5, 6, 8, 9, 13 | **one new step** |
| `abm_move(direction = <column>)` | step one cell along a per-agent heading (needs L1's edge directions) | 9 | argument; couples L1 + L2 |
| `abm_move(range =, axes_only =, avoid_occupied =)` | ranged / axis-restricted / occupancy-aware scan | 8, 13 | arguments on the above; add when forced |
| neighbour-aware `abm_sequential()` | a one-at-a-time rule that can read a neighbour's row | strict Ising (10) only | real addition; deferred unless a second model asks |

## Reading of the result

- **Patch-only lattice models cost one constructor.** Five of the thirteen —
  Game of Life, Forest Fire, Schelling-with-geography, Ising (checkerboard),
  Daisyworld — need nothing but the `type` string. Daisyworld is the useful proof
  that "patch-only" is not "simple": a two-way global↔lattice feedback loop, age
  structure and neighbour-seeded reproduction all fall out of steps that exist.
- **The turtle-≠-patch bundle held across five very different models** — predator–
  prey, stigmergic foraging, a Turing-machine ant, wealth accumulation, and civil
  violence. Same six-item bill every time (`on =`, `.cell`+`.x`/`.y`, the equijoin,
  `abm_tell(to = col)`, `abm_move`, and — twice — `abm_match(.by =)`). That it is
  stable across biology, physics, CS and political science is the generalisability
  evidence the extra five models were added to get.
- **`abm_move` is the one new step, and its argument list is where the growth
  is.** `to = "random_neighbour"` (5, 13), `to = uphill(expr)` (6, 8),
  `direction = <heading>` (9), `range =` / `axes_only =` / `avoid_occupied =`
  (8, 13). Each argument is pulled in by a specific model; none should be built
  ahead of one.
- **Continuous space keeps needing nothing.** Flocking (2-D) and Traffic (1-D
  ring) are both 🟢 today — columns, `within =`, and for Traffic the existing
  `pair = "nearest"` with a directional `cost`. A release that shipped only L0
  plus the continuous-space idioms would already cover the CA family, the
  particle/flocking family, and 1-D car-following.
- **Two hard edges, both worth leaving open.** Langton's Ant needs a heading-
  relative `abm_move`, which fuses L1 and L2 and is the sharpest small ask in the
  set. Strict single-site Ising needs a neighbour-aware `abm_sequential`, which no
  other model here wants — so the checkerboard formulation is documented as the
  supported one, in the same spirit as the corpus's three *corrections*.

## Analysing the results (Game of Life as the worked example)

`abm_run()` returns a plain long tibble — one row per cell per recorded tick,
`tick, .id, .group, .x, .y, alive` — so every analysis is ordinary dplyr / ggplot.
Nothing about the API is spatial; you just have `.x` / `.y` to pivot on, and
`abm_edges()` for the adjacency.

### Decide first: the field, or a summary?

A `record =` choice made before the run.

| you want | keep it as | cost |
|---|---|---|
| the board itself (render, animate, cluster analysis) | populations — `record = "all"` or a thinned integer | ~10k rows/tick on a 100² grid |
| a scalar over time (density, activity) | `abm_global()` — recorded **every tick regardless of `record =`** | 1 row/tick |

So for just the density curve, add the global and thin the field:

```r
go <- abm_go(
  abm_neighbours(live_n ~ sum(alive)),
  abm_rules(alive ~ live_n == 3 | (alive & live_n == 2)),
  abm_global(density ~ mean(alive))
)
result <- abm_run(life, go, ticks = 500, seed = 1, record = "final")
abm_globals(result)                       # tick, density — all 500 ticks
```

### 1. Look at the field

```r
dplyr::filter(result, tick == 100) |>
  ggplot2::ggplot(ggplot2::aes(.x, .y, fill = alive)) +
  ggplot2::geom_raster() + ggplot2::coord_equal()
```

`facet_wrap(~ tick)` for a contact sheet; `gganimate::transition_manual(tick)` for
a movie. `geom_raster()` is why the grid constructor injects `.x` / `.y`.

### 2. Scalar time series (if not done as a global)

```r
result |>
  dplyr::group_by(tick) |>
  dplyr::summarise(density = mean(alive)) |>
  ggplot2::ggplot(ggplot2::aes(tick, density)) + ggplot2::geom_line()
```

Decays toward the random-soup asymptote ≈ 0.0287.

### 3. Structures — `abm_edges()` + igraph

The grid edge list is a spatial weights matrix. Keep edges between two live cells
and connected components are the live clusters:

```r
snap     <- dplyr::filter(result, tick == 500)
live_ids <- snap$.id[snap$alive]
e_live   <- dplyr::filter(abm_edges(result), from %in% live_ids, to %in% live_ids)

comp <- igraph::components(
  igraph::graph_from_data_frame(e_live, directed = FALSE,
    vertices = data.frame(name = live_ids)))

comp$no             # number of live clusters
table(comp$csize)   # 4 = block, 3 = blinker, 5 = glider, ...
```

Period / stasis detection needs consecutive ticks (`record = "all"`):

```r
result |>
  dplyr::arrange(.id, tick) |>
  dplyr::group_by(.id) |>
  dplyr::mutate(flipped = alive != dplyr::lag(alive)) |>
  dplyr::group_by(tick) |>
  dplyr::summarise(n_flipped = sum(flipped, na.rm = TRUE))
```

`n_flipped == 0` on consecutive ticks → a still life; a fixed value alternating →
an oscillator.

### The validation checks, concretely

| claim | analysis |
|---|---|
| blinker period 2 | seed 3 cells in a row, `record = "all"`, `alive` repeats every 2 ticks |
| glider translates `(1,1)` / 4 ticks | single glider, `filter(alive) \|> group_by(tick) \|> summarise(cx = mean(.x), cy = mean(.y))` — slopes ≈ 0.25 |
| soup density → 0.0287 | `abm_global(density ~ mean(alive))`, read at large tick, average over seeds |

The pattern generalises to every spatial model here: `group_by(tick)` for time
series, `filter(tick == t)` for a frame, `abm_edges()` for anything topological.
Turtle-≠-patch models add `filter(.group == "sheep")` before the `group_by`, and
`.cell` joins a turtle back to its patch.
