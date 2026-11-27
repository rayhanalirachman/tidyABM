# Comprehensive tests for all 20 planned grammar changes (dev/syntax-changes.md),
# checked for bit-identical parity between the R and Rust engines.

agree <- function(model, go, ticks = 3, seed = 1, ...) {
  withr::local_options(tidyABM.engine = "r")
  r_out <- abm_run(model, go, ticks = ticks, seed = seed, progress = FALSE, ...)
  withr::local_options(tidyABM.engine = "auto")
  a_out <- abm_run(model, go, ticks = ticks, seed = seed, progress = FALSE, ...)
  withr::local_options(tidyABM.engine = "both")
  b_out <- abm_run(model, go, ticks = ticks, seed = seed, progress = FALSE, ...)
  expect_identical(a_out, r_out)
  expect_identical(b_out, r_out)
  r_out
}

test_that("#1: cell_<col> read access, guards, and optional abm_move(along =)", {
  # Reserved cell_ column guard and read-only LHS guard
  expect_error(abm_agents(n = 3, cell_grass = TRUE), class = "tidyABM_reserved_column")
  expect_error(abm_grid(dims = c(2, 2), cell_x = 1), class = "tidyABM_reserved_column")
  expect_error(abm_rules(cell_grass ~ FALSE), class = "tidyABM_reserved_column")

  m <- abm_setup(
    agents = list(
      patches = abm_agents(grass = ~seq_len(n) <= 2L),
      sheep   = abm_agents(n = 4, energy = 0, at = ~c(1L, 2L, 3L, 4L))
    ),
    space = abm_space(type = "grid", dims = c(2, 2), on = "patches"),
    seed = 1
  )
  go <- abm_go(
    abm_rules(energy ~ energy + if_else(cell_grass, 10, -1)),
    abm_tell(grass ~ FALSE, to = .cell, when = .group == "sheep" & cell_grass),
    abm_move(to = "random_neighbour", who = "sheep")
  )
  r <- agree(m, go, ticks = 2, seed = 7)
  t1 <- r[r$tick == 1, ]
  expect_equal(t1$energy[t1$.group == "sheep"], c(10, 10, -1, -1))
  expect_equal(t1$grass[t1$.group == "patches"], rep(FALSE, 4))
})

test_that("#2 & #20: when = on abm_rules() and abm_sequential(), and .scope on abm_sequential()", {
  m <- abm_setup(agents = abm_agents(n = 8, x = ~1:8, draw = 0, flag = FALSE),
                 globals = list(count = 0L), seed = 1)
  # n() and runif(n()) draw only for matching rows
  go <- abm_go(
    abm_rules(flag ~ TRUE, draw ~ runif(n()), when = x > 5),
    abm_match(pair = "random", eligible = x <= 4),
    abm_rules(x ~ x + 100L, when = x == 1L),
    abm_sequential(count ~ count + 1L, when = x > 5, .scope = "population")
  )
  r <- agree(m, go, ticks = 2, seed = 4)
  t1 <- r[r$tick == 1, ]
  expect_equal(t1$flag, c(rep(FALSE, 5), rep(TRUE, 3)))
  expect_equal(sum(t1$draw > 0), 3L)
  # agent 1 had x = 1 -> 101 (> 5), plus agents 6, 7, 8 -> 4 agents match when = x > 5
  expect_equal(abm_globals(r)$count[[2]], 4L)
  # Check RNG stream consumed exactly 3 draws in the first when = rule
  m0 <- abm_setup(agents = abm_agents(n = 8, x = ~1:8, draw = 0))
  r_when <- agree(m0, abm_go(abm_rules(draw ~ runif(n()), when = x > 5)), ticks = 1, seed = 42)
  expected_draws <- withr::with_seed(42, runif(3))
  expect_identical(r_when$draw[r_when$tick == 1 & r_when$x > 5], expected_draws)
})

test_that("#3: built-in read-only .tick counter across steps and inside abm_repeat()", {
  expect_error(abm_rules(.tick ~ 1L), class = "tidyABM_reserved_column")
  m <- abm_setup(agents = abm_agents(n = 4, stamped = 0L, rep_tick = 0L),
                 globals = list(g_tick = 0L))
  go <- abm_go(
    abm_rules(stamped ~ .tick, when = .tick >= 2L),
    abm_repeat(
      abm_rules(rep_tick ~ .tick),
      max = 2
    ),
    abm_global(g_tick ~ .tick)
  )
  r <- agree(m, go, ticks = 3, seed = 1, measures = list(mt = ~.tick))
  expect_equal(abm_globals(r)$g_tick, c(0L, 1L, 2L, 3L))
  expect_equal(abm_measures(r)$mt, c(0L, 1L, 2L, 3L))
  expect_equal(unique(r$stamped[r$tick == 1]), 0L)
  expect_equal(unique(r$stamped[r$tick == 2]), 2L)
  expect_equal(unique(r$rep_tick[r$tick == 3]), 3L)
})

test_that("#4: abm_space(grid + patchless + continuous), regular/on network, via, at, and .dist", {
  # 4a. Patchless grid + at = "unique" + torus .dist
  m_pl <- abm_setup(
    agents = abm_agents(n = 6, v = 1, near = 0L, at = "unique"),
    space  = abm_space(type = "grid", dims = c(5, 5), torus = TRUE),
    seed   = 1
  )
  expect_null(m_pl$lattice$on)
  expect_equal(length(unique(m_pl$groups$agents$.cell)), 6L)
  go_pl <- abm_go(
    abm_neighbours(near ~ n(), within = .dist <= 2 & .id != own_.id),
    abm_move(to = "random_empty")
  )
  r_pl <- agree(m_pl, go_pl, ticks = 2, seed = 2)
  expect_equal(length(unique(r_pl$.cell[r_pl$tick == 2])), 6L)

  # 4b. Combined space + network with `via = "space"` / `via = "network"`
  m_both <- abm_setup(
    agents  = list(patches = abm_agents(v = ~seq_len(n)),
                   people  = abm_agents(n = 4, s = ~seq_len(n), nb_net = 0, nb_sp = 0)),
    space   = abm_space(type = "grid", dims = c(3, 3), diagonals = FALSE, on = "patches"),
    network = abm_network(type = "regular", degree = 2, on = "people"),
    seed    = 1
  )
  expect_error(
    abm_run(m_both, abm_go(abm_neighbours(nb_net ~ sum(s))), ticks = 1),
    class = "tidyABM_ambiguous_topology"
  )
  go_both <- abm_go(
    abm_neighbours(nb_net ~ n(), via = "network"),
    abm_neighbours(nb_sp ~ n(), via = "space")
  )
  r_both <- agree(m_both, go_both, ticks = 1, seed = 1)
  t1 <- r_both[r_both$tick == 1, ]
  expect_equal(t1$nb_net[t1$.group == "people"], rep(2L, 4))
  expect_equal(t1$nb_sp[t1$.group == "patches"], rep(4L, 9))

  # 4c. Continuous 2-D space: at = list(x, y), offset(), forward(), torus .dist, AST cell bucketing
  m_cont <- abm_setup(
    agents = abm_agents(
      n = 80, heading = 0,
      at = list(x = ~runif(n(), 0, 10), y = ~runif(n(), 0, 10))
    ),
    space = abm_space(type = "continuous", dims = c(10, 10), torus = c(TRUE, FALSE)),
    seed = 5
  )
  # Verify torus on x (wraps across 0/10) and bounded on y (clamps at 0/10)
  go_c <- abm_go(
    abm_move(to = offset(9.5, 20)),
    abm_move(to = forward(0.2, heading)),
    abm_neighbours(k ~ n(), within = .dist <= 1.5 & .id != own_.id),
    abm_match(pair = "nearest"),
    abm_rules(d_near ~ .cost)
  )
  r_c <- agree(m_cont, go_c, ticks = 2, seed = 5)
  t1 <- r_c[r_c$tick == 1, ]
  expect_true(all(t1$.x >= 0 & t1$.x < 10))
  expect_true(all(t1$.y == 10))
  expect_true(all(t1$d_near >= 0 & is.finite(t1$d_near)))

  # Verify AST cell bucketing matches brute-force distance identically
  pts <- r_c[r_c$tick == 1, c(".id", ".x", ".y")]
  for (i in 1:5) {
    dx <- abs(pts$.x - pts$.x[[i]]); dx <- pmin(dx, 10 - dx)
    dy <- abs(pts$.y - pts$.y[[i]])
    d <- sqrt(dx^2 + dy^2)
    expect_equal(t1$k[[i]], sum(d <= 1.5) - 1L)
  }
})

test_that("#5 & #6: .cost from nearest, sides=/coords=, and empty-match narrowing", {
  expect_error(abm_rules(.cost ~ 0), class = "tidyABM_reserved_column")
  expect_error(abm_match(pair = "opposite_group", by = g, sides = g),
               class = "tidyABM_conflicting_args")
  expect_error(abm_match(pair = "nearest", by = x, coords = x),
               class = "tidyABM_conflicting_args")

  m <- abm_setup(agents = abm_agents(n = 4, x = ~c(0, 1, 5, 10), side = ~c("a", "b", "a", "b"),
                                     d = -1, cval = -1, touched = FALSE))
  go <- abm_go(
    abm_match(pair = "nearest", coords = x),
    abm_rules(d ~ .cost),
    abm_match(pair = "nearest", cost = abs(x - own_x) * 2),
    abm_rules(cval ~ .cost),
    abm_match(pair = "opposite_group", sides = side, eligible = FALSE),
    abm_rules(touched ~ TRUE)
  )
  r <- agree(m, go, ticks = 1, seed = 1)
  t1 <- r[r$tick == 1, ]
  expect_equal(t1$d, c(1, 1, 4, 5))
  expect_equal(t1$cval, c(2, 2, 8, 10))
  expect_equal(t1$touched, rep(FALSE, 4))
})

test_that("#7, #8, #10, #13, #17: abm_pairs(when=), birth(attach=/group=/guard), tell(add), neighbours(.default)", {
  # #7 abm_pairs(when=)
  expect_error(abm_pairs(via = "r", u ~ 1, when = TRUE, .when = TRUE),
               class = "tidyABM_conflicting_args")
  m_rel <- abm_setup(
    agents = abm_agents(n = 3, x = ~1:3),
    relations = list(r = abm_relation(data.frame(from = c(1L, 2L), to = c(2L, 3L)), w = 1))
  )
  r_rel <- agree(m_rel, abm_go(abm_pairs(via = "r", w ~ w + 10, when = from_x == 1L)),
                 ticks = 1, seed = 1)
  expect_equal(abm_relations(r_rel, "r")$w, c(11, 1))

  # #8 abm_birth(attach=) & #13 group= + patch guard
  expect_error(abm_birth(n = 1, attach = "parent", attach_via = abm_match(pair = "network")),
               class = "tidyABM_conflicting_args")
  m_net <- abm_setup(agents = abm_agents(n = 6, x = 1),
                     network = abm_network(type = "ring", degree = 2), seed = 1)
  r_att <- agree(m_net, abm_go(abm_birth(when = .id == 1L, attach = "parent", links = 2)),
                 ticks = 1, seed = 1)
  expect_equal(nrow(r_att[r_att$tick == 1, ]), 7L)
  expect_equal(nrow(abm_edges(r_att)), nrow(m_net$edges) + 2L)

  m_mg <- abm_setup(agents = list(a = abm_agents(n = 3, x = 1), b = abm_agents(n = 2, y = 2)))
  expect_error(abm_run(m_mg, abm_go(abm_birth(n = 1)), ticks = 1), class = "tidyABM_missing_arg")
  r_mg <- agree(m_mg, abm_go(abm_birth(n = 2, group = "b")), ticks = 1, seed = 1)
  expect_equal(sum(r_mg$tick == 1 & r_mg$.group == "b"), 4L)

  m_lat <- abm_setup(
    agents = list(patches = abm_agents(z = 1), bugs = abm_agents(n = 2, e = 1)),
    space = abm_space(type = "grid", dims = c(2, 2), on = "patches"), seed = 1
  )
  expect_error(abm_run(m_lat, abm_go(abm_death(when = z == 1)), ticks = 1),
               class = "tidyABM_bad_arg")
  expect_error(abm_run(m_lat, abm_go(abm_birth(when = z == 1)), ticks = 1),
               class = "tidyABM_bad_arg")
  # abm_birth(n = 1) on lattice defaults unambiguously to the lone non-patch group
  r_lat_b <- agree(m_lat, abm_go(abm_birth(n = 1)), ticks = 1, seed = 1)
  expect_equal(sum(r_lat_b$tick == 1 & r_lat_b$.group == "bugs"), 3L)

  # #10 abm_tell(.resolve = "add")
  m_t <- abm_setup(agents = abm_agents(n = 4, val = c(10, 20, 30, 40), to_id = c(2L, 2L, 4L, NA_integer_)))
  r_t <- agree(m_t, abm_go(abm_tell(val ~ 5, to = to_id, .resolve = "add")), ticks = 1, seed = 1)
  expect_equal(r_t$val[r_t$tick == 1], c(10, 30, 30, 45))

  # #17 abm_neighbours(.default =)
  m_iso <- abm_setup(agents = abm_agents(n = 4, x = ~1:4),
                     network = abm_network(type = "manual", edges = data.frame(from = 1L, to = 2L)))
  r_iso <- agree(m_iso, abm_go(abm_neighbours(s ~ sum(x), .default = 0L)), ticks = 1, seed = 1)
  expect_equal(r_iso$s[r_iso$tick == 1], c(2L, 1L, 0L, 0L))
})

test_that("#9, #11, #14: abm_move(range = col, when =, to = random_empty / random_cell)", {
  m <- abm_setup(
    agents = list(
      patches = abm_agents(sugar = ~seq_len(n)),
      people  = abm_agents(n = 3, vis = c(1L, 2L, 3L), jailed = c(TRUE, FALSE, FALSE),
                           at = ~c(1L, 2L, 3L))
    ),
    space = abm_space(type = "grid", dims = c(4, 4), diagonals = FALSE, on = "patches"),
    seed = 1
  )
  go <- abm_go(
    abm_move(to = uphill(sugar), range = vis, axes_only = TRUE,
             avoid_occupied = TRUE, when = !jailed)
  )
  r <- agree(m, go, ticks = 2, seed = 3)
  # Jailed person #1 never moves
  p1 <- r[r$.group == "people" & r$.id == 17L, ]
  expect_true(all(p1$.cell == 1L))
  # Unjailed movers moved to distinct cells
  fin_p <- r[r$tick == 2 & r$.group == "people", ]
  expect_equal(length(unique(fin_p$.cell)), 3L)

  # random_empty & random_cell
  r_re <- agree(m, abm_go(abm_move(to = "random_empty", when = !jailed)), ticks = 2, seed = 3)
  expect_equal(length(unique(r_re$.cell[r_re$tick == 2 & r_re$.group == "people"])), 3L)
  r_rc <- agree(m, abm_go(abm_move(to = "random_cell")), ticks = 1, seed = 3)
  expect_true(all(r_rc$.cell[r_rc$tick == 1 & r_rc$.group == "people"] %in% 1:16))
})

test_that("#12: scoped abm_match(..., <steps>) restores outer match cleanly", {
  m <- abm_setup(agents = abm_agents(n = 6, x = ~1:6, p1 = NA_integer_, p2 = NA_integer_,
                                     after = NA_integer_))
  go <- abm_go(
    abm_match(pair = "random"),
    abm_rules(p1 ~ .partner),
    abm_match(
      pair = "random",
      abm_rules(p2 ~ .partner)
    ),
    # Outer match is restored after the scoped block exits
    abm_rules(after ~ .partner)
  )
  r <- agree(m, go, ticks = 2, seed = 11)
  t1 <- r[r$tick == 1, ]
  expect_identical(t1$after, t1$p1)
  # Sequence ending on a scoped abm_match() is allowed
  go_end <- abm_go(abm_match(pair = "random", abm_rules(p1 ~ .partner)))
  expect_s3_class(go_end, "abm_go")
  r2 <- agree(m, go_end, ticks = 1, seed = 11)
  expect_true(all(!is.na(r2$p1[r2$tick == 1])))
})

test_that("#15, #16, #18, #19: n() in setup, formula globals, abm_run(until=), list splicing, replicate hint", {
  # #15 n() in setup + #18 formula-initialized globals
  m <- abm_setup(
    agents = abm_agents(n = 10, x = ~seq_len(n())),
    globals = list(total = ~sum(x), mean_x = ~total / n()),
    seed = 1
  )
  expect_equal(m$globals$total, 55)
  expect_equal(m$globals$mean_x, 5.5)

  # #19 step-list splicing in abm_go() and abm_repeat()
  phase <- list(abm_rules(x ~ x - 1L), abm_global(total ~ sum(x)))
  go <- abm_go(phase, abm_repeat(list(abm_rules(x ~ pmax(0L, x))), max = 1))
  expect_length(go$steps, 3L)

  # #16 abm_run(until = ~expr) early stopping (including record = "final")
  r_all <- agree(m, go, ticks = 20, seed = 1, until = ~sum(x) <= 25)
  expect_lt(max(r_all$tick), 20L)
  expect_true(abm_globals(r_all)$total[[nrow(abm_globals(r_all))]] <= 25)
  r_fin <- agree(m, go, ticks = 20, seed = 1, until = ~sum(x) <= 25, record = "final")
  expect_equal(unique(r_fin$tick), max(r_all$tick))

  # #19 once-per-session stochastic setup hint
  rm(list = ls(tidyABM:::.setup_hint), envir = tidyABM:::.setup_hint)
  m_st <- abm_setup(agents = abm_agents(n = 5, x = ~runif(n)), seed = 1)
  expect_message(
    abm_run(m_st, abm_go(abm_rules(x ~ x)), ticks = 1, reps = 2, seed = 1, progress = FALSE),
    class = "tidyABM_stochastic_setup_hint"
  )
  # Second time in the same session is silent
  expect_no_message(
    abm_run(m_st, abm_go(abm_rules(x ~ x)), ticks = 1, reps = 2, seed = 1, progress = FALSE)
  )
})

test_that("newly ported grammar options execute natively in Rust (0 declines, 0 R callbacks)", {
  check_native <- function(model, go, ticks = 3L, seed = 1L, ...) {
    ref <- agree(model, go, ticks = ticks, seed = seed, ...)
    old <- options(tidyABM.engine = "auto")
    on.exit(options(old), add = TRUE)
    got <- abm_run(model, go, ticks = ticks, seed = seed, ..., progress = FALSE)
    expect_identical(got, ref)
    st <- tidyABM:::.rl_last$stats
    expect_gt(st[[1]], 0L)
    expect_equal(st[[2]], 0L)
    expect_equal(st[[4]], 0L)
    got
  }

  # 1. Grid space + network, cell_<col>, when=, range=<col>, random_empty/cell/stay,
  #    tell(.resolve="add", .order=, via=), neighbours(.default=, via=), draw(via=),
  #    sequential(when=, .scope="population"), .tick, until=
  m_grid <- abm_setup(
    agents = list(
      patches = abm_agents(food = ~seq_len(n) %% 5L, chem = 0, e = 0),
      bugs = abm_agents(n = 6, e = ~seq_len(n) * 2.0, vis = c(1L, 2L, 1L, 2L, 1L, 2L),
                        jailed = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
                        last_t = 0L, seen_food = 0L, nb_chem = -1, sp_nb = -1,
                        pri = ~seq_len(n), msg = 0, d = 0),
      guards = abm_agents(n = 2, e = 5)
    ),
    space = abm_space(type = "grid", dims = c(4, 4), diagonals = FALSE, on = "patches"),
    network = abm_network(type = "manual", edges = data.frame(from = c(17L, 18L), to = c(18L, 19L))),
    globals = list(step_tick = 0L),
    seed = 7
  )
  go_grid <- abm_go(
    abm_rules(seen_food ~ cell_food, last_t ~ .tick + runif(n()) * 0, when = !jailed),
    abm_move(to = uphill(food), who = "bugs", range = vis, axes_only = TRUE,
             avoid_occupied = TRUE, when = !jailed),
    abm_move(to = "random_empty", who = c("bugs", "guards"),
             when = .group == "guards" | !jailed),
    abm_move(to = "random_cell", who = "guards"),
    abm_move(to = "stay", who = "bugs"),
    abm_tell(chem ~ 1.5, to = .cell, when = .group == "bugs" & cell_food > 0L, .resolve = "add"),
    abm_tell(msg ~ e, to = "neighbours", via = "network", .order = pri, .resolve = "first"),
    abm_draw(w ~ runif(n()), via = "space"),
    abm_neighbours(sp_nb ~ sum(food), via = "space", .default = -9L),
    abm_neighbours(nb_chem ~ sum(e), via = "network", .default = -1),
    abm_match(pair = "nearest", coords = e, eligible = .group == "bugs", among = .group == "bugs"),
    abm_rules(d ~ .cost),
    abm_sequential(e ~ e + cell_food * 0.1 + .tick * 0.01,
                   when = .group == "bugs" & !jailed, .scope = "population"),
    abm_global(step_tick ~ .tick)
  )
  res_g <- check_native(m_grid, go_grid, ticks = 5L, seed = 42L, until = ~.tick >= 3L)
  expect_equal(max(res_g$tick), 3L)

  # 2. Continuous space: offset() and forward() with when = and torus/clamp
  m_cont <- abm_setup(
    agents = abm_agents(n = 8, vx = 0.7, vy = -0.4, hd = ~seq(0, 2, length.out = n),
                        active = ~seq_len(n) %% 2L == 1L),
    space = abm_space(type = "continuous", dims = c(5, 5), torus = c(TRUE, FALSE)),
    seed = 3
  )
  go_cont <- abm_go(
    abm_move(to = offset(vx, vy), when = active),
    abm_move(to = forward(0.5, hd), when = !active)
  )
  check_native(m_cont, go_cont, ticks = 4L, seed = 5L)

  # 3. Birth with attach = "random_edge" and group =
  m_pa <- abm_setup(
    agents = list(a = abm_agents(n = 4, x = 1), b = abm_agents(n = 3, y = 2)),
    network = abm_network(type = "ring", degree = 2),
    seed = 9
  )
  go_pa <- abm_go(
    abm_birth(n = 2, group = "b", attach = "random_edge", links = 2)
  )
  check_native(m_pa, go_pa, ticks = 3L, seed = 11L)

  # 4. Double pair-view sum/mean, wired-patch birth/death guard, patchless
  #    gradient with .dist, and abm_tell() reading partner_<col>
  m_ws <- abm_setup(
    agents = list(
      patches = abm_agents(chem = ~runif(n), nb_chem = 0, deg = 0L),
      sheep = abm_agents(n = 5, energy = ~runif(n, 1, 6), gift = 0)
    ),
    space = abm_space(type = "grid", dims = c(4, 4), torus = TRUE, on = "patches"),
    seed = 12
  )
  go_ws <- abm_go(
    abm_neighbours(nb_chem ~ mean(chem), deg ~ n(), via = "space"),
    abm_birth(when = .group == "sheep" & energy > 3, cost = energy ~ energy / 2),
    abm_death(when = .group == "sheep" & energy < 2),
    abm_match(pair = "one_of", eligible = .group == "sheep", among = .group == "sheep"),
    abm_tell(gift ~ partner_energy * 0.1, to = .partner, when = !is.na(.partner), .resolve = "sum")
  )
  check_native(m_ws, go_ws, ticks = 3L, seed = 13L)

  m_pl <- abm_setup(
    agents = abm_agents(n = 4, target_x = 3),
    space = abm_space(type = "grid", dims = c(4, 4), torus = FALSE),
    seed = 14
  )
  go_pl <- abm_go(
    abm_move(to = uphill(-abs(.x - own_target_x) - 0.1 * .dist), range = 2)
  )
  check_native(m_pl, go_pl, ticks = 3L, seed = 15L)

  # 5. Spatial .dist with torus-aware cell bucketing (n > 64), continuous torus
  #    nearest, 2-D coords nearest, cost with .dist, and weighted one_of
  m_sp <- abm_setup(
    agents = abm_agents(n = 80, kind = ~rep(c(1L, 2L), 40),
                        w = ~seq_len(n) * 1.07, nb_n = 0L, nb_w = 0,
                        d_tor = 0, d_2d = 0, d_cost = 0, pick_w = 0),
    space = abm_space(type = "continuous", dims = c(20, 20), torus = TRUE),
    globals = list(rad = 3.5),
    seed = 16
  )
  go_sp <- abm_go(
    abm_neighbours(nb_n ~ n(), nb_w ~ mean(w),
                   within = .dist <= rad & kind == own_kind & .id != own_.id,
                   .default = 0),
    abm_match(pair = "nearest"),
    abm_rules(d_tor ~ .cost),
    abm_match(pair = "nearest", coords = c(.x, .y)),
    abm_rules(d_2d ~ .cost),
    abm_match(pair = "nearest", cost = .dist + 0.01 * w),
    abm_rules(d_cost ~ .cost),
    abm_match(pair = "one_of", weight = w),
    abm_rules(pick_w ~ partner_w)
  )
  check_native(m_sp, go_sp, ticks = 2L, seed = 17L)

  # Tied positive weights cleanly decline to R with exact parity
  m_tie <- abm_setup(agents = abm_agents(n = 8, w = rep(c(1, 2), 4), p = 0), seed = 18)
  go_tie <- abm_go(abm_match(pair = "one_of", weight = w), abm_rules(p ~ partner_w))
  old <- options(tidyABM.engine = "both")
  on.exit(options(old), add = TRUE)
  expect_identical(abm_run(m_tie, go_tie, ticks = 2L, seed = 19L),
                   { options(tidyABM.engine = "r"); abm_run(m_tie, go_tie, ticks = 2L, seed = 19L) })

  # 6. Relations: match(among = .rel), rules read/write, neighbours(within = .rel),
  #    link(via =), unlink(via =), and death() pruning relations
  m_rel <- abm_setup(
    agents = list(
      b = abm_agents(n = 6, w = ~seq_len(n), avg_u = 0, cnt = 0L,
                     tgt = c(7L, 8L, 7L, 8L, 7L, 8L)),
      s = abm_agents(n = 2, p = c(1.5, 2.5))
    ),
    relations = list(
      rel = abm_relation(data.frame(from = 1:4, to = c(7L, 7L, 8L, 8L),
                                    u = c(1, 2, 3, 4)), u = 0)
    ),
    seed = 20
  )
  go_rel <- abm_go(
    abm_match(pair = "one_of", eligible = .group == "b", among = .rel),
    abm_rules(rel_u ~ rel_u + 1, w ~ w + coalesce(rel_u, 0)),
    abm_neighbours(avg_u ~ mean(rel_u), cnt ~ n(), within = .rel, .default = 0),
    abm_link(via = "rel", to = tgt, when = .group == "b" & w > 4, u ~ 10),
    abm_unlink(via = "rel", when = rel_u > 10),
    abm_death(when = .group == "b" & .id == 1L)
  )
  check_native(m_rel, go_rel, ticks = 3L, seed = 21L)
})
