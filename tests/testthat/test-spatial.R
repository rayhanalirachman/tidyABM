# The spatial grammar: lattices, named neighbours, mobile agents, movement.
#
# Kept separate from the non-spatial tests. The model-level checks here are the
# validations named in `models/spatial/design-probe.md`, cut down to
# a size a test suite can afford.

cid <- function(x, y, w) x + (y - 1) * w

# L0 -- the lattice ------------------------------------------------------

test_that("a grid wires every cell to its Moore neighbourhood", {
  m <- abm_setup(agents = abm_agents(z = 0),
                 network = abm_network(type = "grid", dims = c(5, 4)))
  # 20 cells on a torus, 8 neighbours each, each edge counted once
  expect_equal(nrow(m$edges), 20 * 8 / 2)
  deg <- table(factor(c(m$edges$from, m$edges$to), levels = 1:20))
  expect_true(all(deg == 8))
  # the count is inherited from dims
  expect_equal(nrow(m$groups$agents), 20L)
})

test_that("diagonals = FALSE gives the von Neumann neighbourhood", {
  m <- abm_setup(agents = abm_agents(z = 0),
                 network = abm_network(type = "grid", dims = c(5, 4),
                                       diagonals = FALSE))
  expect_equal(nrow(m$edges), 20 * 4 / 2)
})

test_that("a bounded grid simply gives border cells fewer neighbours", {
  m <- abm_setup(agents = abm_agents(z = 0),
                 network = abm_network(type = "grid", dims = c(5, 5),
                                       diagonals = FALSE, torus = FALSE))
  deg <- table(factor(c(m$edges$from, m$edges$to), levels = 1:25))
  expect_equal(as.integer(deg[[cid(1, 1, 5)]]), 2L)   # corner
  expect_equal(as.integer(deg[[cid(3, 1, 5)]]), 3L)   # edge
  expect_equal(as.integer(deg[[cid(3, 3, 5)]]), 4L)   # interior
})

test_that("coordinates follow the documented convention and are in scope in setup", {
  w <- 5; h <- 4
  m <- abm_setup(agents = abm_agents(corner = ~.x == 1 & .y == 1,
                                     idx = ~.x + (.y - 1) * 5),
                 network = abm_network(type = "grid", dims = c(w, h)))
  g <- m$groups$agents
  expect_equal(g$.x, rep(1:5, times = 4))
  expect_equal(g$.y, rep(1:4, each = 5))
  expect_equal(g$idx, g$.id)                  # .id == .x + (.y - 1) * w
  expect_equal(sum(g$corner), 1L)             # `.x`/`.y` readable in a formula
})

test_that("a line lattice gives .x only, and wraps when asked", {
  m <- abm_setup(agents = abm_agents(z = 0),
                 network = abm_network(type = "line", dims = 6, torus = TRUE))
  expect_equal(nrow(m$edges), 6L)
  expect_true(".x" %in% names(m$groups$agents))
  expect_false(".y" %in% names(m$groups$agents))

  bounded <- abm_setup(agents = abm_agents(z = 0),
                       network = abm_network(type = "line", dims = 6,
                                             torus = FALSE))
  expect_equal(nrow(bounded$edges), 5L)
})

test_that("abm_grid() desugars to the same model as the two-line form", {
  a <- abm_setup(agents = abm_grid(dims = c(6, 6), diagonals = FALSE,
                                   torus = FALSE, z = ~.x))
  b <- abm_setup(agents = list(patches = abm_agents(z = ~.x)),
                 network = abm_network(type = "grid", dims = c(6, 6),
                                       diagonals = FALSE, torus = FALSE,
                                       on = "patches"))
  expect_equal(a$edges, b$edges)
  expect_equal(a$groups$patches, b$groups$patches)
})

test_that("the lattice constructor refuses arguments it cannot honour", {
  expect_error(abm_network(type = "grid"), class = "tidyABM_bad_dims")
  expect_error(abm_network(type = "grid", dims = 10), class = "tidyABM_bad_dims")
  expect_error(abm_network(type = "grid", dims = c(0, 5)), class = "tidyABM_bad_dims")
  expect_error(abm_network(type = "line", dims = 10, diagonals = TRUE),
               class = "tidyABM_irrelevant_arg")
  expect_error(abm_network(type = "grid", dims = c(4, 4), degree = 3),
               class = "tidyABM_irrelevant_arg")
  # and the non-lattice types still refuse the lattice arguments
  expect_error(abm_network(type = "random", degree = 2, dims = c(4, 4)),
               class = "tidyABM_irrelevant_arg")
})

test_that("the wired group inherits n, and a mismatch is an error", {
  ok <- abm_setup(agents = abm_agents(n = 16, z = 0),
                  network = abm_network(type = "grid", dims = c(4, 4)))
  expect_equal(nrow(ok$groups$agents), 16L)
  expect_error(
    abm_setup(agents = abm_agents(n = 15, z = 0),
              network = abm_network(type = "grid", dims = c(4, 4))),
    class = "tidyABM_bad_n"
  )
  # a group off the lattice still has to say how many there are
  expect_error(abm_setup(agents = abm_agents(z = 0)), class = "tidyABM_bad_n")
})

test_that("on = must name a group that exists", {
  expect_error(
    abm_setup(agents = list(patches = abm_agents(z = 0)),
              network = abm_network(type = "grid", dims = c(3, 3),
                                    on = "cells")),
    class = "tidyABM_bad_arg"
  )
  # ...and a multi-group model has to say which one is the lattice
  expect_error(
    abm_setup(agents = list(a = abm_agents(z = 0), b = abm_agents(n = 2)),
              network = abm_network(type = "grid", dims = c(3, 3))),
    class = "tidyABM_missing_arg"
  )
})

# L0 -- Game of Life -----------------------------------------------------

life_go <- function() {
  abm_go(abm_neighbours(live_n ~ sum(alive)),
         abm_rules(alive ~ live_n == 3 | (alive & live_n == 2)))
}

test_that("Game of Life: a blinker has period 2", {
  w <- h <- 12
  seed_ids <- c(5, 6, 7) + (6 - 1) * w
  m <- abm_setup(agents = abm_agents(alive = ~seq_len(n) %in% seed_ids),
                 network = abm_network(type = "grid", dims = c(w, h)))
  r <- abm_run(m, life_go(), ticks = 4, seed = 1)
  cells <- lapply(0:4, function(t) sort(r$.id[r$tick == t & r$alive]))
  expect_equal(lengths(cells), rep(3L, 5))          # always three cells
  expect_equal(cells[[1]], cells[[3]])              # period 2
  expect_equal(cells[[3]], cells[[5]])
  expect_false(identical(cells[[1]], cells[[2]]))   # ...and not period 1
})

test_that("Game of Life: a glider translates one cell diagonally every 4 ticks", {
  w <- h <- 20
  glider <- c(cid(3, 5, w), cid(4, 4, w), cid(2, 3, w), cid(3, 3, w), cid(4, 3, w))
  m <- abm_setup(agents = abm_agents(alive = ~seq_len(n) %in% glider),
                 network = abm_network(type = "grid", dims = c(w, h)))
  r <- abm_run(m, life_go(), ticks = 8, seed = 1)
  centre <- function(t) {
    s <- r[r$tick == t & r$alive, ]
    c(mean(s$.x), mean(s$.y), nrow(s))
  }
  c0 <- centre(0); c4 <- centre(4); c8 <- centre(8)
  expect_equal(c0[[3]], 5)                    # a glider is five cells, always
  expect_equal(c8[[3]], 5)
  # the centroid gains exactly (1, -1) every four ticks
  expect_equal(c4[[1]] - c0[[1]], 1)
  expect_equal(c4[[2]] - c0[[2]], -1)
  expect_equal(c8[[1]] - c4[[1]], 1)
  expect_equal(c8[[2]] - c4[[2]], -1)
})

test_that("Forest Fire: a bounded von Neumann grid burns across from one edge", {
  w <- h <- 40
  m <- abm_setup(
    agents = abm_agents(state = ~dplyr::if_else(runif(n) < 0.8, "tree", "empty")),
    network = abm_network(type = "grid", dims = c(w, h), diagonals = FALSE,
                          torus = FALSE),
    seed = 42)
  go <- abm_go(
    abm_rules(state ~ dplyr::if_else(state == "tree" & .x == 1, "burning", state)),
    abm_neighbours(hot ~ any(state == "burning")),
    abm_rules(state ~ dplyr::case_when(state == "burning"    ~ "burnt",
                                       state == "tree" & hot ~ "burning",
                                       TRUE                  ~ state)))
  r <- abm_run(m, go, ticks = 3 * w, seed = 1, record = "final")
  fin <- r[r$tick == max(r$tick), ]
  # well above the percolation threshold, so the fire spans the grid
  expect_true(any(fin$state == "burnt" & fin$.x == w))
  expect_gt(sum(fin$state == "burnt") / sum(fin$state != "empty"), 0.9)
})

# L1 -- one named neighbour ----------------------------------------------

test_that("Rule 90 reproduces Pascal's triangle mod 2 exactly", {
  w <- 81
  rule_bits <- as.integer(intToBits(90L))[1:8]
  m <- abm_setup(
    agents  = abm_agents(s = ~as.integer(seq_len(n) == n %/% 2 + 1)),
    network = abm_network(type = "line", dims = w, torus = TRUE),
    globals = list(rule = rule_bits))
  go <- abm_go(
    abm_neighbours(s_l ~ sum(s), .where = "west"),
    abm_neighbours(s_r ~ sum(s), .where = "east"),
    abm_rules(s ~ rule[4 * s_l + 2 * s + s_r + 1]))
  r <- abm_run(m, go, ticks = 20, seed = 1)

  centre <- w %/% 2 + 1
  for (t in 0:20) {
    row <- r[r$tick == t, ]
    row <- row[order(row$.x), ]
    got <- as.integer(row$.x[row$s == 1])
    want <- as.integer(sort((centre + 2 * (0:t) - t)[(choose(t, 0:t) %% 2) == 1]))
    expect_equal(got, want, info = paste("tick", t))
  }
})

test_that(".where reads exactly one neighbour, and NA past a bounded edge", {
  w <- 5
  m <- abm_setup(agents = abm_agents(v = ~.x * 10),
                 network = abm_network(type = "line", dims = w, torus = FALSE))
  r <- abm_run(m, abm_go(abm_neighbours(west_v ~ sum(v), .where = "west"),
                         abm_neighbours(east_v ~ sum(v), .where = "east")),
               ticks = 1, seed = 1)
  last <- r[r$tick == 1, ]
  last <- last[order(last$.x), ]
  expect_equal(last$west_v, c(NA, 10, 20, 30, 40))
  expect_equal(last$east_v, c(20, 30, 40, 50, NA))
  # "left"/"right" are the line's own names for the same two directions
  r2 <- abm_run(m, abm_go(abm_neighbours(l ~ sum(v), .where = "left")),
                ticks = 1, seed = 1)
  expect_equal(r2$l[r2$tick == 1][order(r2$.x[r2$tick == 1])],
               c(NA, 10, 20, 30, 40))
})

test_that(".where names the four compass directions on a grid", {
  w <- h <- 4
  m <- abm_setup(agents = abm_agents(v = ~.id),
                 network = abm_network(type = "grid", dims = c(w, h),
                                       torus = FALSE))
  r <- abm_run(m, abm_go(abm_neighbours(nv ~ sum(v), .where = "north"),
                         abm_neighbours(ev ~ sum(v), .where = "east")),
               ticks = 1, seed = 1)
  last <- r[r$tick == 1, ]
  # north is +1 in .y, which is +w in .id; the top row has none
  expect_equal(last$nv[last$.y < h], last$.id[last$.y < h] + w)
  expect_true(all(is.na(last$nv[last$.y == h])))
  expect_equal(last$ev[last$.x < w], last$.id[last$.x < w] + 1)
  expect_true(all(is.na(last$ev[last$.x == w])))
})

test_that(".where needs a lattice, refuses a bad direction, and excludes within", {
  m <- abm_setup(agents = abm_agents(n = 4, v = 1),
                 network = abm_network(type = "random", degree = 2))
  expect_error(abm_run(m, abm_go(abm_neighbours(k ~ sum(v), .where = "north")),
                       ticks = 1),
               class = "tidyABM_no_lattice")

  grid <- abm_setup(agents = abm_agents(v = 1),
                    network = abm_network(type = "grid", dims = c(3, 3)))
  expect_error(abm_run(grid, abm_go(abm_neighbours(k ~ sum(v), .where = "up")),
                       ticks = 1),
               class = "tidyABM_bad_where")
  expect_error(abm_neighbours(k ~ sum(v), .where = "north", within = v > 0),
               class = "tidyABM_conflicting_args")
})

# L2 -- mobile agents on the lattice -------------------------------------

test_that("a non-wired group gets .cell, and .x/.y mirror it", {
  w <- h <- 6
  m <- abm_setup(
    agents = list(patches = abm_agents(z = 0),
                  sheep = abm_agents(n = 4, at = ~c(1L, 2L, 8L, 36L))),
    network = abm_network(type = "grid", dims = c(w, h), on = "patches"))
  s <- m$groups$sheep
  expect_equal(s$.cell, c(1L, 2L, 8L, 36L))
  expect_equal(s$.x, c(1L, 2L, 2L, 6L))
  expect_equal(s$.y, c(1L, 1L, 2L, 6L))
  # the wired group is materialised first, so a cell id is a patch .id
  expect_equal(m$groups$patches$.id, 1:36)
  expect_false(".cell" %in% names(m$groups$patches))
})

test_that("placement is uniform at random by default, and `at` can read the patches", {
  withr::local_seed(11)
  w <- h <- 8
  m <- abm_setup(
    agents = list(patches = abm_agents(z = 0), bugs = abm_agents(n = 300)),
    network = abm_network(type = "grid", dims = c(w, h), on = "patches"))
  expect_true(all(m$groups$bugs$.cell %in% 1:64))
  expect_gt(length(unique(m$groups$bugs$.cell)), 30)

  nested <- abm_setup(
    agents = list(patches = abm_agents(nest = ~.x == 4 & .y == 4),
                  ants = abm_agents(n = 5, at = ~which(nest)[1])),
    network = abm_network(type = "grid", dims = c(w, h), on = "patches"))
  expect_equal(unique(nested$groups$ants$.cell), cid(4, 4, w))
})

test_that("at = without a lattice, and on the wired group, are both errors", {
  expect_error(abm_setup(agents = abm_agents(n = 3, at = ~1L)),
               class = "tidyABM_no_lattice")
  expect_error(
    abm_setup(agents = list(patches = abm_agents(z = 0, at = ~1L)),
              network = abm_network(type = "grid", dims = c(3, 3), on = "patches")),
    class = "tidyABM_irrelevant_arg")
})

test_that("within = .id == own_.cell reads the cell an agent is standing on", {
  w <- h <- 6
  m <- abm_setup(
    agents = list(patches = abm_agents(grass = ~seq_len(n) %% 2 == 0),
                  sheep = abm_agents(n = 5, at = ~1:5)),
    network = abm_network(type = "grid", dims = c(w, h), on = "patches"))
  r <- abm_run(m, abm_go(abm_neighbours(
    grass_here ~ any(grass),
    within = .group == "patches" & .id == own_.cell)), ticks = 1, seed = 1)
  s <- r[r$tick == 1 & r$.group == "sheep", ]
  s <- s[order(s$.cell), ]
  expect_equal(s$grass_here, (1:5) %% 2 == 0)
  # a patch has no `.cell`, so it joins to nothing
  expect_true(all(is.na(r$grass_here[r$tick == 1 & r$.group == "patches"])))
})

test_that("the equijoin fast path agrees with the full pair scan", {
  withr::local_seed(5)
  w <- h <- 5
  m <- abm_setup(
    agents = list(patches = abm_agents(v = ~.id * 2),
                  bugs = abm_agents(n = 10)),
    network = abm_network(type = "grid", dims = c(w, h), on = "patches"))
  combined <- bind_groups(m$groups)
  fast <- equijoin_view(
    abm_neighbours(k ~ sum(v), within = .group == "patches" & .id == own_.cell),
    combined, list())
  slow <- attribute_view(
    abm_neighbours(k ~ sum(v), within = .group == "patches" & .id == own_.cell),
    combined, list())
  expect_false(is.null(fast))
  key <- function(v) sort(paste(v$.of, v$.id))
  expect_equal(key(fast), key(slow))

  # a `within` with no equality falls back rather than misfiring
  expect_null(equijoin_view(abm_neighbours(k ~ sum(v), within = v > own_v),
                            combined, list()))
})

test_that("abm_tell(to = .cell) writes into the patch an agent stands on", {
  w <- h <- 4
  m <- abm_setup(
    agents = list(patches = abm_agents(hits = 0L),
                  bugs = abm_agents(n = 3, at = ~c(2L, 2L, 7L))),
    network = abm_network(type = "grid", dims = c(w, h), on = "patches"))
  r <- abm_run(m, abm_go(abm_tell(hits ~ 1L, to = .cell, .resolve = "sum",
                                  when = .group == "bugs")),
               ticks = 1, seed = 1)
  p <- r[r$tick == 1 & r$.group == "patches", ]
  expect_equal(p$hits[p$.id == 2L], 2L)     # two bugs on cell 2
  expect_equal(p$hits[p$.id == 7L], 1L)
  expect_equal(sum(p$hits), 3L)
})

test_that("abm_match(.by = .cell) confines a match to one cell", {
  withr::local_seed(3)
  w <- h <- 4
  m <- abm_setup(
    agents = list(
      patches = abm_agents(z = 0),
      wolves = abm_agents(n = 4, at = ~c(1L, 1L, 1L, 2L)),
      sheep  = abm_agents(n = 8, at = ~c(1L, 1L, 2L, 3L, 3L, 3L, 3L, 3L))),
    network = abm_network(type = "grid", dims = c(w, h), on = "patches"))
  go <- abm_go(
    abm_match(pair = "opposite_group", by = .group, .by = .cell,
              eligible = .group %in% c("wolves", "sheep")),
    abm_rules(caught ~ !is.na(.partner), .scope = "population"))
  r <- abm_run(m, go, ticks = 1, seed = 3)
  fin <- r[r$tick == 1 & r$.group != "patches", ]
  pairs <- tapply(fin$caught, fin$.cell, sum) / 2
  expect_equal(as.integer(pairs[["1"]]), 2L)   # 3 wolves, 2 sheep -> 2 pairs
  expect_equal(as.integer(pairs[["2"]]), 1L)   # 1 wolf,   1 sheep -> 1 pair
  expect_equal(as.integer(pairs[["3"]]), 0L)   # 0 wolves, 5 sheep -> none
  # and nobody is matched across cells
  caught <- fin[fin$caught, ]
  expect_true(all(caught$.cell %in% c(1L, 2L)))
})

test_that(".by refuses the one mode it makes no sense for", {
  expect_error(abm_match(pair = "network", .by = .cell),
               class = "tidyABM_irrelevant_arg")
})

# abm_move() -------------------------------------------------------------

test_that("random_neighbour lands on an adjacent cell, never the current one", {
  withr::local_seed(7)
  w <- h <- 9
  centre <- cid(5, 5, w)
  m <- abm_setup(
    agents = list(patches = abm_agents(z = 0),
                  bugs = abm_agents(n = 200, at = ~rep(centre, n))),
    network = abm_network(type = "grid", dims = c(w, h), on = "patches"))
  r <- abm_run(m, abm_go(abm_move(along = "patches", to = "random_neighbour",
                                  who = "bugs")), ticks = 1, seed = 7)
  b <- r[r$tick == 1 & r$.group == "bugs", ]
  expect_setequal(unique(b$.cell), m$lattice$nb[[centre]])
  expect_false(any(b$.cell == centre))
  expect_equal(b$.x, m$lattice$x[b$.cell])      # .x/.y follow .cell
  expect_equal(b$.y, m$lattice$y[b$.cell])
})

test_that("uphill climbs a gradient and then holds the summit", {
  w <- h <- 9
  m <- abm_setup(
    agents = list(patches = abm_agents(height = ~ -((.x - 5)^2 + (.y - 5)^2)),
                  walker = abm_agents(n = 1, at = ~1L)),
    network = abm_network(type = "grid", dims = c(w, h), on = "patches",
                          torus = FALSE))
  r <- abm_run(m, abm_go(abm_move(along = "patches", who = "walker",
                                  to = uphill(height))), ticks = 8, seed = 1)
  wk <- r[r$.group == "walker", ]
  wk <- wk[order(wk$tick), ]
  expect_equal(wk$.cell[wk$tick == 0], 1L)
  expect_equal(wk$.cell[wk$tick == 4], cid(5, 5, w))
  expect_true(all(wk$.cell[wk$tick >= 4] == cid(5, 5, w)))

  # downhill is the mirror image
  r2 <- abm_run(
    abm_setup(agents = list(patches = abm_agents(d = ~ (.x - 5)^2 + (.y - 5)^2),
                            walker = abm_agents(n = 1, at = ~1L)),
              network = abm_network(type = "grid", dims = c(w, h), on = "patches",
                                    torus = FALSE)),
    abm_go(abm_move(along = "patches", who = "walker", to = downhill(d))),
    ticks = 8, seed = 1)
  expect_equal(r2$.cell[r2$.group == "walker" & r2$tick == 8], cid(5, 5, w))
})

test_that("uphill sees the mover's own columns as well as the cell's", {
  w <- h <- 7
  m <- abm_setup(
    agents = list(
      patches = abm_agents(a = ~ -abs(.x - 1), b = ~ -abs(.x - 7)),
      walker  = abm_agents(n = 2, at = ~c(cid(4, 4, w), cid(4, 4, w)),
                           west = ~c(TRUE, FALSE))),
    network = abm_network(type = "grid", dims = c(w, h), on = "patches",
                          torus = FALSE))
  # each walker follows a different field, chosen by its own column
  r <- abm_run(m, abm_go(abm_move(along = "patches", who = "walker",
                                  to = uphill(dplyr::if_else(west, a, b)))),
               ticks = 6, seed = 1)
  fin <- r[r$tick == 6 & r$.group == "walker", ]
  fin <- fin[order(fin$.id), ]
  expect_equal(fin$.x, c(1L, 7L))
})

test_that("range and axes_only widen the scan the way Sugarscape needs", {
  w <- h <- 11
  m <- abm_setup(
    agents = list(patches = abm_agents(psugar = ~as.integer(.x == 1 & .y == 6)),
                  people = abm_agents(n = 1, at = ~cid(6, 6, w))),
    network = abm_network(type = "grid", dims = c(w, h), on = "patches",
                          diagonals = FALSE, torus = FALSE))
  r <- abm_run(m, abm_go(abm_move(along = "patches", who = "people",
                                  to = uphill(psugar), range = 5,
                                  axes_only = TRUE)), ticks = 1, seed = 1)
  # one jump, five cells west, straight to the sugar
  expect_equal(r$.cell[r$.group == "people" & r$tick == 1], cid(1, 6, w))

  # with range = 1 the same sugar is out of reach
  r2 <- abm_run(m, abm_go(abm_move(along = "patches", who = "people",
                                   to = uphill(psugar))), ticks = 1, seed = 1)
  expect_true(r2$.x[r2$.group == "people" & r2$tick == 1] > 1)
})

test_that("avoid_occupied never puts two movers on one cell", {
  withr::local_seed(5)
  w <- h <- 11
  centre <- cid(6, 6, w)
  m <- abm_setup(
    agents = list(patches = abm_agents(z = 0),
                  bugs = abm_agents(n = 9, at = ~rep(centre, n))),
    network = abm_network(type = "grid", dims = c(w, h), on = "patches"))
  r <- abm_run(m, abm_go(abm_move(along = "patches",
                                  to = "random_empty_neighbour", who = "bugs")),
               ticks = 1, seed = 5)
  b <- r[r$tick == 1 & r$.group == "bugs", ]
  expect_equal(anyDuplicated(b$.cell), 0L)
  # eight free neighbours for nine bugs, so exactly one has nowhere to go
  expect_equal(sum(b$.cell == centre), 1L)
})

test_that("avoid_occupied counts a shared cell rather than merely marking it", {
  # A cell several movers are stacked on must stay occupied until the *last*
  # of them leaves. Tracking occupancy as a set of cells frees it when the
  # first one goes, and a later mover can then land on top of a straggler.
  withr::local_seed(9)
  w <- h <- 11
  centre <- cid(6, 6, w)
  m <- abm_setup(
    agents = list(patches = abm_agents(z = 0),
                  bugs = abm_agents(n = 20, at = ~rep(centre, n))),
    network = abm_network(type = "grid", dims = c(w, h), on = "patches"))
  for (s in 1:8) {
    r <- abm_run(m, abm_go(abm_move(along = "patches",
                                    to = "random_empty_neighbour",
                                    who = "bugs")), ticks = 1, seed = s)
    b <- r[r$tick == 1 & r$.group == "bugs", ]
    # 20 bugs, 8 free neighbours: exactly 8 get out, one per cell, and the
    # other 12 stay stacked where they already were. What must not happen is a
    # mover landing on a cell that still holds a straggler -- which is what a
    # set-based occupancy does once the first bug leaves the centre.
    left <- b$.cell[b$.cell != centre]
    expect_equal(length(left), 8L)
    expect_equal(anyDuplicated(left), 0L)
    expect_setequal(left, m$lattice$nb[[centre]])
    expect_equal(sum(b$.cell == centre), 12L)
  }
})

test_that("Langton's Ant matches a reference implementation exactly", {
  w <- h <- 31
  start <- cid(16, 16, w)
  m <- abm_setup(
    agents = list(patches = abm_agents(white = TRUE),
                  ant = abm_agents(n = 1, heading = 0L, at = ~start)),
    network = abm_network(type = "grid", dims = c(w, h), on = "patches",
                          diagonals = FALSE, torus = TRUE))
  go <- abm_go(
    abm_neighbours(here_white ~ all(white),
                   within = .id == own_.cell & .group == "patches"),
    abm_rules(heading ~ (heading + dplyr::if_else(here_white, 1L, 3L)) %% 4L,
              .scope = "population"),
    abm_tell(white ~ !here_white, to = .cell),
    abm_move(along = "patches", who = "ant", direction = heading))
  N <- 300
  r <- abm_run(m, go, ticks = N, seed = 1, record = "final")

  white <- rep(TRUE, w * h); cell <- start; hd <- 0L
  for (i in seq_len(N)) {
    hw <- white[cell]
    hd <- (hd + if (hw) 1L else 3L) %% 4L
    white[cell] <- !hw
    x <- ((cell - 1) %% w) + 1; y <- ((cell - 1) %/% w) + 1
    if (hd == 0L) y <- y + 1 else if (hd == 1L) x <- x + 1 else
      if (hd == 2L) y <- y - 1 else x <- x - 1
    cell <- ((x - 1) %% w) + 1 + (((y - 1) %% h)) * w
  }
  fin <- r[r$tick == N, ]
  p <- fin[fin$.group == "patches", ]
  p <- p[order(p$.id), ]
  expect_equal(p$white, white)
  expect_equal(fin$.cell[fin$.group == "ant"], cell)
  expect_equal(fin$heading[fin$.group == "ant"], hd)
})

test_that("abm_move refuses arguments it cannot honour", {
  expect_error(abm_move(to = "random_neighbour"), class = "tidyABM_missing_arg")
  expect_error(abm_move(along = "p", to = "sideways"), "must be one of")
  expect_error(abm_move(along = "p", direction = heading, range = 3),
               class = "tidyABM_conflicting_args")

  w <- h <- 4
  m <- abm_setup(
    agents = list(patches = abm_agents(z = 0), bugs = abm_agents(n = 2)),
    network = abm_network(type = "grid", dims = c(w, h), on = "patches"))
  # the wired group *is* the lattice; it does not move across itself
  expect_error(
    abm_run(m, abm_go(abm_move(along = "patches", who = "patches")), ticks = 1),
    class = "tidyABM_bad_arg")
  expect_error(
    abm_run(m, abm_go(abm_move(along = "patches", who = "goats")), ticks = 1),
    class = "tidyABM_bad_arg")
  expect_error(
    abm_run(m, abm_go(abm_move(along = "cells", who = "bugs")), ticks = 1),
    class = "tidyABM_bad_arg")
  # a model with no lattice at all
  flat <- abm_setup(agents = abm_agents(n = 4, z = 0))
  expect_error(abm_run(flat, abm_go(abm_move(along = "patches")), ticks = 1),
               class = "tidyABM_no_lattice")
})

test_that("to = 'stay' is a no-op", {
  w <- h <- 5
  m <- abm_setup(
    agents = list(patches = abm_agents(z = 0), bugs = abm_agents(n = 5, at = ~1:5)),
    network = abm_network(type = "grid", dims = c(w, h), on = "patches"))
  r <- abm_run(m, abm_go(abm_move(along = "patches", to = "stay", who = "bugs"),
                         abm_rules(seen ~ TRUE, .scope = "population")),
               ticks = 3, seed = 1)
  expect_equal(r$.cell[r$tick == 3 & r$.group == "bugs"], 1:5)
})

# demography on the lattice ----------------------------------------------

test_that("a newborn is born on its parent's cell, and a death leaves the lattice alone", {
  withr::local_seed(2)
  w <- h <- 5
  m <- abm_setup(
    agents = list(patches = abm_agents(z = 0),
                  bugs = abm_agents(n = 4, at = ~c(3L, 3L, 9L, 20L), e = 5)),
    network = abm_network(type = "grid", dims = c(w, h), on = "patches"))
  edges_before <- nrow(m$edges)
  r <- abm_run(m, abm_go(abm_birth(when = .group == "bugs" & e == 5,
                                   cost = e ~ e / 2)), ticks = 1, seed = 2)
  b <- r[r$tick == 1 & r$.group == "bugs", ]
  expect_equal(nrow(b), 8L)
  expect_equal(sort(b$.cell), sort(rep(c(3L, 3L, 9L, 20L), 2)))
  expect_equal(b$.x, m$lattice$x[b$.cell])

  r2 <- abm_run(m, abm_go(abm_death(when = .group == "bugs" & .cell == 3L)),
                ticks = 1, seed = 2)
  expect_equal(sum(r2$tick == 1 & r2$.group == "bugs"), 2L)
  expect_equal(nrow(abm_edges(r2)), edges_before)   # patches untouched
})

# results ----------------------------------------------------------------

test_that("the result tibble carries the coordinates and the lattice edges", {
  w <- h <- 4
  m <- abm_setup(
    agents = list(patches = abm_agents(v = 1), bugs = abm_agents(n = 2, at = ~c(1L, 5L))),
    network = abm_network(type = "grid", dims = c(w, h), on = "patches"))
  r <- abm_run(m, abm_go(abm_rules(v ~ v + 1)), ticks = 2, seed = 1)
  expect_true(all(c(".x", ".y", ".cell") %in% names(r)))
  expect_equal(nrow(abm_edges(r)), nrow(m$edges))
  # a frame is a filter, which is the whole point of injecting .x / .y
  frame <- r[r$tick == 2 & r$.group == "patches", ]
  expect_equal(nrow(frame), 16L)
  expect_setequal(frame$.x, 1:4)
})
