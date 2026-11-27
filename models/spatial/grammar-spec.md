# Spatial grammar — the end product

What a user types to write a spatial model in tidyABM, and what it means.
Implementation is out of scope here — this is the surface Opus builds *to*.

Companion to [`design-probe.md`](design-probe.md) (the 13-model probe that
produced it), and the surface the ten models in
[`README.md`](README.md) are written against.

---

## Mental model

Spatial geometry lives in `abm_space()` (`type = "grid"` for 1-D/2-D discrete
lattices, or `type = "continuous"` for 1-D/2-D coordinate boxes), separate from
undirected social connections in `abm_network()`. On a grid with patches,
patches are ordinary agents so `abm_neighbours()`, `abm_match(pair = "network")`,
and `abm_edges()` work unchanged (and `abm_network(type = "grid" | "line")`
remains supported as a deprecated alias). A mobile agent's location is a patch
`.id` held in `.cell`, it reads its patch's columns as read-only `cell_<col>`,
writes back via `abm_tell(..., to = .cell)`, and moves via `abm_move()`.

Three tiers, separable for scoping:

| tier | adds | unlocks |
|---|---|---|
| **L0** | the lattice as a network type | every patch-only model — CA, spread, reaction–diffusion, patch + global feedback |
| **L1** | reading one *named* neighbour | ordered-neighbour lattices (1-D CA) |
| **L2** | mobile agents on the lattice | turtle-≠-patch models |
| **L2+** | extra `abm_move()` arguments; neighbour-aware `abm_sequential()` | added per model, never ahead of one |

---

## L0 — the lattice

### `abm_network(type = , dims = , diagonals = , torus = , on = )`

| arg | applies to | default | meaning |
|---|---|---|---|
| `type = "grid"` | — | — | a 2-D lattice |
| `type = "line"` | — | — | a 1-D lattice |
| `dims` | grid: `c(w, h)` · line: `w` | required | the shape; cell count is `prod(dims)` |
| `diagonals` | grid only | `TRUE` | `TRUE` = 8-neighbour (orthogonal + diagonal); `FALSE` = 4-neighbour (orthogonal only). Error if passed with `type = "line"`. |
| `torus` | both | `TRUE` | `TRUE` = edges wrap; `FALSE` = bounded, border cells have fewer neighbours |
| `on` | both *(L2)* | whole population | name of the agent group to wire; the rest of the population is not on this network |

**Produces**

- the ordinary `from`/`to` edge tibble;
- `.x`, `.y` — integer cell coordinates on the **wired group**, 1-based.
  Convention: row-major, `.id = .x + (.y - 1) * w`, `.y` increases **upward**.
  `type = "line"` gives `.x` only.
- `.cell` — on **every other group**: an integer holding a wired-group `.id`
  ("the cell this agent is on").

`.x` / `.y` / `.cell` are readable in `abm_agents()` column formulas, in every
`abm_go()` step, and in the `abm_run()` output. They are reserved (dot-prefixed,
like `.id`) — a user cannot declare them. A rule may *write* `.cell` (that is a
move, and `.x` / `.y` follow); `.x` and `.y` themselves are read-only.

**Count inheritance.** The wired group omits `n` in `abm_agents()`; it is
`prod(dims)`. `n` is still usable inside that group's formulas. A matching `n`
is allowed; a mismatch is an error.

**Errors**

- `dims` missing or not positive integer(s)
- `diagonals` with `type = "line"`
- `on` naming a group not in `agents`
- wired group's explicit `n` ≠ `prod(dims)`

### `abm_grid(dims, diagonals, torus, ...)` — sugar

```r
patches = abm_grid(dims = c(w, h), diagonals = FALSE, torus = FALSE,
                   state = ~..., ...)
```

desugars exactly to

```r
agents = list(patches = abm_agents(state = ~..., ...))
space  = abm_space(type = "grid", dims = c(w, h),
                   diagonals = FALSE, torus = FALSE, on = "patches")
```

Identical model, identical go block. `...` are patch columns, same rules as
`abm_agents()`. Use it for patch-heavy models; use the explicit two-line form
when the network should be visible or another network sits alongside it.

### Reading the lattice — no new steps

| step | on a lattice, means |
|---|---|
| `abm_neighbours(k ~ sum(alive))` | aggregate over a cell's lattice neighbours |
| `abm_rules(...)` | simultaneous update — the synchronous CA step |
| `abm_match(pair = "network")` | a random lattice neighbour |
| `abm_global(m ~ mean(temp))` | a scalar over the whole lattice, recorded every tick |

Bounded grids need no boundary handling: a border cell has fewer neighbour rows,
`sum()` / `any()` see that directly; `abm_neighbours()` returns `NA` only for a
cell with *zero* neighbours, which a lattice never has.

---

## L1 — one named neighbour

### `abm_neighbours(col ~ agg, .where = )`

`.where` restricts the aggregate to the single neighbour in a named lattice
direction: `"north"` / `"south"` / `"east"` / `"west"` on a grid; `"left"` /
`"right"` (or `"west"` / `"east"`) on a line. The aggregate then runs over a
one-row set, so bare `col ~ s` or `col ~ sum(s)` both yield that neighbour's
value; a missing neighbour (bounded edge) yields `NA`.

```r
abm_neighbours(s_w ~ sum(s), .where = "west"),
abm_neighbours(s_e ~ sum(s), .where = "east"),
abm_rules(s ~ rule[[4 * s_w + 2 * s + s_e + 1]])
```

L1 is the one place the network framing needs a new idea — a lattice edge has to
carry a direction. Everything else is reuse.

---

## L2 — mobile agents on the lattice

### Placement

A non-wired group gets `.cell` automatically (uniform random). `at =` on
`abm_agents()` controls it — an expression evaluated once, like a column formula:

```r
sheep = abm_agents(n = 100, energy = ~runif(n, 4, 8),
                   at = ~sample(prod(dims), n, replace = TRUE))
ants  = abm_agents(n = 100, at = ~which(nest)[1])          # all at the nest
```

`.x` / `.y` on a mobile agent always mirror its current `.cell`.

### Read the cell an agent is on — `cell_<col>` (or `abm_neighbours(within = )`)

Mobile agents read the columns of the patch they stand on directly as read-only
`cell_<col>` inside `abm_rules()`, `abm_tell()`, `abm_sequential()`, etc.:

```r
abm_rules(energy ~ energy + sheep_gain, when = .group == "sheep" & cell_grass)
```

For multi-agent aggregates over co-located agents (e.g., a patch reading all
agents standing on it), `within = .id == own_.cell` remains hash-joined in
linear time.

### Write into another agent's row by id — `abm_tell(to = <expr>)`

`to =` accepts an expression yielding an agent id, typically `.cell`:

```r
abm_tell(grass ~ FALSE, countdown ~ regrow, to = .cell, when = .group == "sheep" & cell_grass)
abm_tell(chemical ~ 60, to = .cell, .resolve = "add", when = .group == "ants" & carrying)
```

Several senders to one target resolve with `.resolve =` (`"add"` increments the
recipient's existing value, `"sum"` replaces it with the sum of incoming messages).

### Interact with co-located agents — `abm_match(.by = <column>, ...)`

`.by =` partitions the match within each distinct value of the column. `.by =
.cell` confines a match to agents sharing a cell, and child steps in `...` scope
the match cleanly:

```r
abm_match(pair = "opposite_group", sides = .group, .by = .cell,
          eligible = .group %in% c("wolves", "sheep"),
          abm_rules(energy ~ energy + wolf_gain, when = .group == "wolves"),
          abm_death(when = .group == "sheep" & !is.na(.partner)))
```

Reuses `abm_rules(.by =)`'s grouping idea, applied to matching.

### Move — `abm_move()` (the one new step)

```r
abm_move(along = NULL, to = , who = , when = NULL)
```

| arg | meaning |
|---|---|
| `along` | optional: defaults to the model's space/lattice, or a group name / `"space"` / `"network"` |
| `to` | `"random_neighbour"`, `"random_empty_neighbour"`, `"random_empty"`, `"random_cell"`, `"stay"`, `uphill(expr)` / `downhill(expr)` (seeing `.dist`), or on continuous spaces `offset(dx, dy)` / `forward(by, heading)` |
| `who` | which agent groups move this step (defaults to all mobile groups) |
| `when` | optional condition selecting which agents move (others stay put and still occupy their cells) |

Effect: each mover's `.cell` is reassigned (or `.x` / `.y` on continuous space);
`.x` / `.y` follow. Ties random. A mover with no legal target stays put.

**L2+ arguments — add only when a model forces one:**

| arg | forced by | meaning |
|---|---|---|
| `direction = <column>` | Langton's Ant | step one cell along a stored heading (needs L1 edge directions) |
| `range = <n \| col>` | Sugarscape | look up to `n` (or per-agent column `vision`) cells away |
| `axes_only = TRUE` | Sugarscape | scan along the axes only |
| `avoid_occupied = TRUE` | Sugarscape, Rebellion | never land on a cell already holding a `who` mover |

### Demography on the lattice

- `abm_death()` of a mobile agent leaves the patch network untouched (it was
  never on it).
- `abm_birth()` of a mobile agent: the newborn inherits `.cell` (born on the
  parent's cell) unless `inherit =` overrides it.

---

## Results — no API change

The `abm_run()` output tibble carries `.x`, `.y` (wired group) and `.cell`
(mobile groups) alongside the agent columns. `abm_edges()` returns the lattice.
Analysis is ordinary dplyr / ggplot:

- `filter(tick == t)` + `geom_raster(aes(.x, .y, fill = ...))` — a frame
- `group_by(tick) |> summarise(...)` — a time series (or `abm_global()` for a
  scalar kept every tick regardless of `record =`)
- `abm_edges()` as a spatial weights matrix — clusters via `igraph::components()`
  on the live sub-graph, Moran's I, etc.

---

## Acceptance examples

### L0 — Game of Life

```r
w <- h <- 100

life <- abm_setup(
  agents = abm_agents(alive = ~runif(n) < 0.10),
  space  = abm_space(type = "grid", dims = c(w, h))        # diagonals, torus default TRUE
)

go <- abm_go(
  abm_neighbours(live_n ~ sum(alive)),
  abm_rules(alive ~ live_n == 3 | (alive & live_n == 2))
)

result <- abm_run(life, go, ticks = 500, seed = 1, record = 25)
```

Passes when: blinker has period 2; a lone glider's live-cell centroid gains
`(0.25, 0.25)` per tick; random-soup `mean(alive)` relaxes to ≈ 0.0287.

### L2 — Wolf–Sheep–Grass

```r
w <- h <- 50

world <- abm_setup(
  agents = list(
    patches = abm_agents(grass = ~sample(c(TRUE, FALSE), n, TRUE),
                         countdown = ~sample.int(30, n, TRUE)),
    sheep   = abm_agents(n = 100, energy = ~runif(n, 4, 8)),
    wolves  = abm_agents(n = 50,  energy = ~runif(n, 4, 8))
  ),
  space = abm_space(type = "grid", dims = c(w, h), on = "patches",
                    diagonals = TRUE, torus = TRUE),
  globals = list(regrow = 30, sheep_gain = 4, wolf_gain = 20,
                 sheep_repro = 0.04, wolf_repro = 0.05)
)

go <- abm_go(
  abm_move(to = "random_neighbour", who = c("sheep", "wolves")),
  abm_rules(energy ~ energy - 1),

  abm_rules(energy ~ energy + sheep_gain, when = .group == "sheep" & cell_grass),
  abm_tell(grass ~ FALSE, countdown ~ regrow, to = .cell,
           when = .group == "sheep" & cell_grass),

  abm_match(
    pair = "opposite_group", sides = .group, .by = .cell,
    eligible = .group %in% c("wolves", "sheep"),
    abm_rules(energy ~ energy + wolf_gain, when = .group == "wolves"),
    abm_death(when = .group == "sheep" & !is.na(.partner))
  ),

  abm_death(when = .group %in% c("sheep", "wolves") & energy < 0),

  abm_birth(when = .group == "sheep"  & runif(dplyr::n()) < sheep_repro,
            cost = energy ~ energy / 2),
  abm_birth(when = .group == "wolves" & runif(dplyr::n()) < wolf_repro,
            cost = energy ~ energy / 2),

  abm_rules(
    grass     ~ grass | countdown == 0,
    countdown ~ dplyr::case_when(grass          ~ countdown,
                                 countdown <= 0 ~ regrow,
                                 TRUE           ~ countdown - 1))
)

result <- abm_run(world, go, ticks = 500, seed = 1, record = 10)
```

Passes when: in the standard regime sheep and wolf counts settle into bounded,
out-of-phase oscillation; raising `wolf_gain` far enough collapses the system,
lowering it starves the wolves out.

---

## Notes for the implementer (not decisions — flags)

- The grammar is deliberately additive: no existing signature changes meaning.
  `abm_agents(n = )` still works everywhere; `n` becomes *optional* only for a
  grid-wired group.
- `uphill(expr)` / `downhill(expr)` are recognised forms inside `abm_move(to = )`,
  not general functions.
- `on =` is written here as general to `abm_network()` (any type). If L2 is out
  of the first build, `on =` and everything under L2 can be omitted entirely —
  L0 single-group models never need it.
- Coordinate origin and `.y` direction are a call the maintainer can flip; the
  only requirement is that the edge-wiring and the injected `.x` / `.y` agree.
