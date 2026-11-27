# More shortcuts that must be invisible: a network neighbourhood's sum() /
# mean() folded through a cached neighbour index, the per-step plan, and the
# common-case abm_rules(). Each is checked bit for bit against the path it
# replaces, including the cases that have to fall back to it.

# `ifelse()` with a vector test returns `x` unchanged, and takes the general
# rowsum() path rather than the cached-index one
slow_arg <- function(x) bquote(ifelse(TRUE | is.na(.(x)), .(x), 0))
# `target ~ fn(arg)`, the argument optionally routed to the general path
nb_rule <- function(target, fn, arg, slow = FALSE) {
  rlang::new_formula(as.name(target), call(fn, if (slow) slow_arg(arg) else arg))
}

test_that("the cached-index fold equals neighbour_aggregate() bit for bit", {
  withr::local_seed(11)
  for (spec in list(c(21, 21, 0, 0), c(10, 30, 1, 0), c(7, 7, 1, 1), c(9, 5, 0, 1))) {
    n <- spec[1] * spec[2]
    m <- abm_setup(agents = abm_agents(n = n, x = 0),
                   network = abm_network("grid", dims = spec[1:2],
                                         diagonals = as.logical(spec[3]),
                                         torus = as.logical(spec[4])))
    nb <- neighbour_table(m$edges)
    ix <- nb_index(nb, n)
    expect_true(ix$ok)
    for (trial in 1:60) {
      x <- rnorm(n, sd = 10^runif(1, -3, 8))
      if (trial %% 3 == 0) x[sample(n, 5)] <- NA
      if (trial %% 4 == 0) x[sample(n, 2)] <- NaN
      if (trial %% 7 == 0) x[sample(n, 2)] <- Inf
      if (trial %% 9 == 0) x[sample(n, 2)] <- -Inf
      if (trial %% 11 == 0) x[sample(n, 4)] <- -0
      if (trial %% 13 == 0) x <- x * 10^sample(-300:300, n, TRUE)
      view <- tibble::new_tibble(list(.id = nb$.neighbour, x = x[nb$.neighbour],
                                      .of = nb$.id), nrow = nrow(nb))
      for (fn in c("mean", "sum")) {
        a <- neighbour_aggregate(rlang::new_quosure(call(fn, quote(x)), globalenv()), view)
        expect_identical(nb_fold(fn, x, ix), a$.abm_value[match(seq_len(n), a$.of)])
      }
    }
  }
})

test_that("network sum()/mean() rules give the same run as the general path", {
  grid <- function(diag, torus) abm_setup(
    agents = abm_agents(n = 48, x = ~c(NA, Inf, -0, rnorm(n - 3, sd = 1e6)),
                        y = ~runif(n)),
    network = abm_network("grid", dims = c(6, 8), diagonals = diag, torus = torus),
    globals = list(k = 2), seed = 3)
  fast <- abm_go(
    abm_neighbours(a ~ mean(x), b ~ sum(y * k - x), c ~ mean(pmax(y, 0.5))),
    abm_rules(x ~ if_else(is.na(a), y, a / 2), y ~ runif(n())))
  slow <- abm_go(
    abm_neighbours(nb_rule("a", "mean", quote(x), TRUE),
                   nb_rule("b", "sum", quote(y * k - x), TRUE),
                   nb_rule("c", "mean", quote(pmax(y, 0.5)), TRUE)),
    abm_rules(x ~ if_else(is.na(a), y, a / 2), y ~ runif(n())))
  for (d in c(FALSE, TRUE)) for (t in c(FALSE, TRUE)) {
    m <- grid(d, t)
    expect_identical(abm_run(m, fast, ticks = 4, seed = 9),
                     abm_run(m, slow, ticks = 4, seed = 9))
  }
})

test_that("the rules of one neighbours step read the population as it was", {
  m <- abm_setup(agents = abm_agents(n = 9, x = as.numeric(1:9)),
                 network = abm_network("grid", dims = c(3, 3)))
  r <- abm_run(m, abm_go(abm_neighbours(x ~ sum(x), y ~ sum(x))), ticks = 1)
  t1 <- r[r$tick == 1, ]
  expect_identical(t1$x, t1$y)
})

test_that("the fast path steps aside where it cannot be exact", {
  m <- abm_setup(agents = abm_agents(n = 9, x = as.numeric(1:9)),
                 network = abm_network("grid", dims = c(3, 3)))
  st <- list(groups = m$groups, globals = m$globals, edges = m$edges,
             nb_cache = list(edges = m$edges, nb = neighbour_table(m$edges)))
  cmb <- bind_groups(st$groups)
  take <- function(...) !is.null(neighbour_fast(abm_neighbours(...), cmb, st))
  expect_true(take(s ~ sum(x)))
  expect_true(take(s ~ mean(x * 2 + 1)))
  expect_true(take(s ~ sum(as.integer(x))))       # integers fold exactly too
  expect_false(take(s ~ sum(as.character(x))))    # not a number
  expect_false(take(s ~ sum(x, na.rm = TRUE)))
  expect_false(take(s ~ sum(x > own_x)))          # reads the focal agent
  expect_false(take(s ~ sum(ifelse(x > 2, x, 0))))
  expect_false(take(s ~ any(x > 2)))
  expect_false(take(s ~ sum(x), t ~ n()))         # every rule or none
  expect_false(take(s ~ sum(log(x - 5))))         # warns: general path warns
  expect_warning(abm_run(m, abm_go(abm_neighbours(s ~ sum(log(x - 5)))), ticks = 1),
                 "NaN")
  # ids that are not the row numbers, and a relation, go the general way
  st2 <- st
  st2$relations <- list(r = 1)
  expect_null(neighbour_fast(abm_neighbours(s ~ sum(x)), cmb, st2))
  cmb2 <- cmb[-9, ]
  cmb2$.id <- c(1:7, 10L)
  expect_null(neighbour_fast(abm_neighbours(s ~ sum(x)), cmb2, st))
  # an edge to an agent beyond the rows: the general path's error, not the index's
  expect_false(nb_index(neighbour_table(edge_tbl(1L, 12L)), 9L)$ok)
})

test_that("the neighbour index follows births, deaths and new edges", {
  m <- abm_setup(agents = abm_agents(n = 30, x = ~runif(n), age = 0),
                 network = abm_network("ring", degree = 4), seed = 5)
  # without deaths the ids stay the row numbers as the population grows, so
  # the index is rebuilt for new agents and new edges; deaths send it the
  # general way
  mk <- function(slow, die = TRUE) abm_go(!!!c(list(
    abm_neighbours(nb_rule("s", "mean", quote(x), slow)),
    abm_rules(x ~ if_else(is.na(s), x, (x + s) / 2), age ~ age + 1),
    abm_birth(when = runif(n()) < 0.1,
              attach_via = abm_match(pair = "network", from = "parent"))),
    if (die) list(abm_death(when = runif(n()) < 0.1)),
    list(abm_match(pair = "random", eligible = runif(n()) < 0.2),
         abm_link())))
  for (die in c(FALSE, TRUE)) {
    expect_identical(abm_run(m, mk(FALSE, die), ticks = 15, seed = 2),
                     abm_run(m, mk(TRUE, die), ticks = 15, seed = 2))
  }
  # two groups stacked: the ids are still the row numbers
  m2 <- abm_setup(agents = list(a = abm_agents(n = 10, x = ~runif(n), age = 0),
                                b = abm_agents(n = 6, x = ~rnorm(n), age = 0)),
                  network = abm_network("ring", degree = 2), seed = 6)
  expect_identical(abm_run(m2, mk(FALSE), ticks = 6, seed = 1),
                   abm_run(m2, mk(TRUE), ticks = 6, seed = 1))
})

# The per-step plan ---------------------------------------------------------

test_that("routing follows a column that appears mid-run", {
  m <- abm_setup(agents = list(a = abm_agents(n = 3, x = 0, q = 1),
                               b = abm_agents(n = 2, x = 0, k = 1L)))
  go <- abm_go(
    abm_rules(x ~ x + q),            # a only, while b has no `q`
    abm_rules(q ~ 5, .by = k))       # writes `q` into b, which has `k`
  r <- abm_run(m, go, ticks = 3)
  expect_identical(r$x[r$tick == 1], c(1, 1, 1, 0, 0))
  expect_identical(r$x[r$tick == 3], c(3, 3, 3, 10, 10))
})

test_that("routing follows a global added mid-run", {
  m <- abm_setup(agents = list(a = abm_agents(n = 2, x = 0, w = 1),
                               b = abm_agents(n = 2, x = 0)))
  go <- abm_go(abm_rules(x ~ x + w), abm_global(w ~ 10))
  r <- abm_run(m, go, ticks = 3)
  # tick 1: `w` is a column of a only, so b is not written; from tick 2 the
  # global `w` is in scope and the rule applies to b too
  expect_identical(r$x[r$tick == 1], c(1, 1, 0, 0))
  expect_identical(r$x[r$tick == 3], c(3, 3, 20, 20))
})

test_that("a death condition is routed again when its column appears", {
  m <- abm_setup(agents = list(a = abm_agents(n = 3, x = 1),
                               b = abm_agents(n = 3, energy = 1)))
  go <- abm_go(abm_death(when = energy < 0),     # b only, at first
               abm_rules(energy ~ -1, .by = x))  # gives a an `energy`
  r <- abm_run(m, go, ticks = 2)
  expect_identical(r$.id[r$tick == 1], 1:6)
  expect_identical(r$.id[r$tick == 2], 4:6)
})

test_that("one go reused across worlds plans each run afresh", {
  w1 <- abm_setup(agents = list(a = abm_agents(n = 3, x = ~runif(n)),
                                b = abm_agents(n = 2, y = 1)), seed = 1)
  w2 <- abm_setup(agents = list(a = abm_agents(n = 2, y = 5),
                                b = abm_agents(n = 4, x = ~runif(n))), seed = 2)
  go <- abm_go(abm_rules(x ~ x * 2, y ~ y + 1),
               abm_death(when = x > 100),
               abm_global(s ~ sum(x, na.rm = TRUE)))
  r1 <- abm_run(w1, go, ticks = 4, seed = 3)
  r2 <- abm_run(w2, go, ticks = 4, seed = 3)
  expect_identical(abm_run(w1, go, ticks = 4, seed = 3), r1)
  expect_identical(r2$x[r2$tick == 4 & r2$.group == "b"],
                   w2$groups$b$x * 16)
  expect_identical(r2$y[r2$tick == 4 & r2$.group == "a"], c(9, 9))
  # the plan is the run's own: nothing is left on the user's steps
  expect_null(go$steps[[1]]$plan)
  # several runs at once: each its own plan
  rr <- abm_run(w1, go, ticks = 4, reps = 3, seed = 3)
  expect_identical(nrow(rr), 3L * nrow(r1))
})

test_that("the one-group rule path equals the general loop", {
  m <- abm_setup(agents = abm_agents(n = 12, x = ~runif(n), k = ~sample(3, n, TRUE)),
                 globals = list(g = 2), seed = 4)
  state <- list(groups = m$groups, globals = m$globals, edges = NULL,
                relations = NULL, match = NULL)
  # `.scope = "population"` with an empty standing match sends the step the
  # general way while still writing every agent
  general <- state
  general$match <- list(match = tidyABM:::empty_match(), size = 2L)
  steps <- list(
    abm_rules(x ~ x * g + runif(n()), y ~ sample(k), z ~ dplyr::row_number(),
              .scope = "population"),
    abm_rules(x ~ mean(x), .scope = "population"),
    abm_rules(w ~ x, x ~ x + 1, k ~ as.character(k), .scope = "population"),
    abm_rules(x ~ cumsum(x), .scope = "population"))
  for (st in steps) {
    a <- withr::with_seed(1, run_rules(st, state))
    b <- withr::with_seed(1, run_rules(st, general))
    expect_identical(a$groups, b$groups)
  }
  bad <- abm_rules(x ~ c(1, 2), y ~ 1, .scope = "population")
  expect_identical(tryCatch(run_rules(bad, state), error = conditionMessage),
                   tryCatch(run_rules(bad, general), error = conditionMessage))
  expect_true(is.character(tryCatch(run_rules(bad, state), error = conditionMessage)))
})

# Once per run, not per tick ----------------------------------------------

test_that("the global log is the tibble bind_rows() of one-row tibbles gives", {
  slow <- function(rows) dplyr::bind_rows(lapply(rows, tibble::new_tibble, nrow = 1L))
  cell <- function(globals, t) global_cells(list(globals = globals), t)
  logs <- list(
    lapply(0:3, function(t) cell(list(a = t * 1.5, b = TRUE, s = "x"), t)),
    lapply(0:2, function(t) cell(NULL, t)),
    # a global that appears mid-run
    lapply(0:3, function(t) cell(if (t < 2) list(a = 1) else list(a = 1, b = 2), t)),
    # one that changes type, a vector, a named scalar, a factor, a date
    lapply(0:2, function(t) cell(list(a = if (t) 1.5 else 1L), t)),
    lapply(0:2, function(t) cell(list(a = 1:3 + t), t)),
    lapply(0:2, function(t) cell(list(a = c(k = t)), t)),
    lapply(0:2, function(t) cell(list(a = factor("u")), t)),
    lapply(0:2, function(t) cell(list(a = as.Date("2020-01-01") + t), t)),
    lapply(0:2, function(t) cell(list(a = NA, b = NA_real_), t)))
  for (rows in logs) expect_identical(global_table(rows), slow(rows))
})

test_that("a run's identifying row is the one tibble() and bind_cols() build", {
  slow <- function(spec) {
    vals <- lapply(spec$values, function(v) if (is.null(dim(v)) && length(v) == 1L) v else list(v))
    meta <- tibble::tibble(.run = spec$.run, .rep = spec$.rep)
    if (length(vals)) meta <- dplyr::bind_cols(meta, tibble::as_tibble(vals))
    meta
  }
  specs <- list(
    list(.run = 1L, .rep = 1L, values = stats::setNames(list(), character())),
    list(.run = 2L, .rep = 3L, values = list(a = 0.5, b = "x", c = 2L)),
    list(.run = 3L, .rep = 1L, values = list(v = c(1, 2), m = matrix(1:4, 2))),
    list(.run = 4L, .rep = 1L, values = list(d = as.Date("2020-01-01"))))
  for (s in specs) expect_identical(run_meta(s), slow(s))
})

test_that("population columns come out as relocate() put them", {
  m <- abm_setup(agents = list(a = abm_agents(n = 2, x = 1), b = abm_agents(n = 2, y = "u")))
  go <- abm_go(abm_rules(x ~ x + 1))
  for (rec in list("all", "globals", "final")) {
    r <- abm_run(m, go, ticks = 2, record = rec)
    expect_identical(tibble::as_tibble(r),
                     dplyr::relocate(tibble::as_tibble(r), "tick", ".id", ".group"))
    expect_identical(names(r)[1:3], c("tick", ".id", ".group"))
  }
})

# Round 8: the defects found in round 7 -------------------------------------

test_that("a name that is neither a column nor a single value goes the general way", {
  m <- abm_setup(agents = abm_agents(n = 6, x = as.numeric(1:6)),
                 network = abm_network("ring", degree = 2),
                 globals = list(w = c(10, 20, 30, 40, 50, 60), k = 2))
  st <- list(groups = m$groups, globals = m$globals, edges = m$edges,
             nb_cache = list(edges = m$edges, nb = neighbour_table(m$edges)))
  cmb <- bind_groups(st$groups)
  take <- function(...) !is.null(neighbour_fast(abm_neighbours(...), cmb, st))
  expect_true(take(s ~ sum(x * k)))
  expect_false(take(s ~ sum(w)))                  # a global as long as the population
  expect_false(take(s ~ mean(x * w)))
  ev <- c(1, 2, 3, 4, 5, 6)                       # a vector in the formula's env
  expect_false(take(s ~ sum(x + ev)))
  mat <- matrix(1:6, 6, 1)
  expect_false(take(s ~ sum(x * mat)))
  one <- 3
  expect_true(take(s ~ sum(x * one)))
  # a user's function under an allowlisted name, or another package's
  abs <- function(z) rev(z)
  expect_false(take(s ~ sum(abs(x))))
  expect_false(take(s ~ sum(stats::abs(x))))
  expect_true(take(s ~ sum(base::sqrt(x))))
  # the old answer: evaluated over the view, the vector is summed whole
  r <- abm_run(m, abm_go(abm_neighbours(s ~ sum(w))), ticks = 1, seed = 1)
  expect_identical(r$s[r$tick == 1], rep(210, 6))
})

test_that("integer and logical neighbour sums fold as rowsum() does", {
  withr::local_seed(4)
  m <- abm_setup(agents = abm_agents(n = 35, x = 0),
                 network = abm_network("grid", dims = c(5, 7), diagonals = TRUE))
  nb <- neighbour_table(m$edges)
  ix <- nb_index(nb, 35L)
  big <- .Machine$integer.max
  xs <- list(sample(-5:5, 35, TRUE), replace(sample(0:1, 35, TRUE), c(3, 9), NA),
             rep(big %/% 3L, 35L),                     # overflows in every sum
             replace(rep(big - 1L, 35L), seq(2, 35, 2), -(big - 1L)), # partial sums leave the range
             replace(integer(35), 5, -big), c(big, rep(0L, 34)), integer(35))
  for (x in xs) {
    view <- tibble::new_tibble(list(.id = nb$.neighbour, x = x[nb$.neighbour],
                                    .of = nb$.id), nrow = nrow(nb))
    for (fn in c("sum", "mean")) {
      a <- neighbour_aggregate(rlang::new_quosure(call(fn, quote(x)), globalenv()), view)
      expect_identical(nb_fold_int(fn, x, ix), a$.abm_value[match(seq_len(35), a$.of)])
    }
  }
  # whole runs: logical and integer columns, NA, an isolated agent, overflow
  m2 <- abm_setup(
    agents = abm_agents(n = 30, b = ~c(NA, runif(n - 1) < 0.5), i = ~sample.int(9L, n, TRUE),
                        h = ~c(.Machine$integer.max, rep(1e9L, n - 1))),
    network = abm_network("ring", degree = 2), seed = 2)
  fast <- abm_go(abm_neighbours(s ~ sum(b), m ~ mean(b), t ~ sum(i), u ~ mean(i * 2L),
                                v ~ sum(h), w ~ sum(!b)),
                 abm_rules(b ~ runif(n()) < 0.5, i ~ i + 1L))
  slow <- abm_go(abm_neighbours(nb_rule("s", "sum", quote(b), TRUE),
                                nb_rule("m", "mean", quote(b), TRUE),
                                nb_rule("t", "sum", quote(i), TRUE),
                                nb_rule("u", "mean", quote(i * 2L), TRUE),
                                nb_rule("v", "sum", quote(h), TRUE),
                                nb_rule("w", "sum", quote(!b), TRUE)),
                 abm_rules(b ~ runif(n()) < 0.5, i ~ i + 1L))
  expect_identical(abm_run(m2, fast, ticks = 3, seed = 1), abm_run(m2, slow, ticks = 3, seed = 1))
  # an agent with no edges gets NA from sum() and mean(), 0 from n()
  m3 <- abm_setup(agents = abm_agents(n = 4, b = TRUE),
                  network = abm_network("manual", edges = data.frame(from = 1:2, to = 2:3)))
  r <- abm_run(m3, abm_go(abm_neighbours(s ~ sum(b))), ticks = 1)
  expect_identical(r$s[r$tick == 1], c(1L, 2L, 1L, NA))
})

test_that("a rule the fast path refuses costs no neighbour index, and a built one is kept", {
  m <- abm_setup(agents = abm_agents(n = 9, x = as.numeric(1:9), b = TRUE),
                 network = abm_network("grid", dims = c(3, 3)))
  st <- list(groups = m$groups, globals = m$globals, edges = m$edges,
             nb_cache = list(edges = m$edges, nb = neighbour_table(m$edges)))
  cmb <- bind_groups(st$groups)
  called <- 0L
  testthat::local_mocked_bindings(nb_index = function(nb, n) {
    called <<- called + 1L
    list(n = n, ok = FALSE)
  })
  expect_null(neighbour_fast(abm_neighbours(s ~ sum(as.character(x))), cmb, st))
  expect_identical(called, 0L)
  expect_null(neighbour_fast(abm_neighbours(s ~ sum(log(x - 5))), cmb, st))
  expect_identical(called, 0L)
})

test_that("the integer fold's index is cached across ticks and rebuilt with the edges", {
  m <- abm_setup(agents = abm_agents(n = 9, b = TRUE),
                 network = abm_network("grid", dims = c(3, 3)))
  st <- list(groups = m$groups, globals = m$globals, edges = m$edges,
             nb_cache = list(edges = m$edges, nb = neighbour_table(m$edges)))
  cmb <- bind_groups(st$groups)
  out <- neighbour_fast(abm_neighbours(s ~ sum(b)), cmb, st)
  expect_true(isTRUE(out$nb_cache$ix$ok))
  expect_identical(out$groups[[1]]$s, rep(8L, 9))
})

test_that("a global named tick keeps bind_rows()'s name repair", {
  m <- abm_setup(agents = abm_agents(n = 3, x = 1), globals = list(tick = 5))
  msgs <- testthat::capture_messages(
    r <- abm_run(m, abm_go(abm_rules(x ~ x + 1)), ticks = 2, seed = 1))
  expect_match(msgs[[1]], "New names")
  expect_named(abm_globals(r), c("tick...1", "tick...2"))
  slow <- function(rows) suppressMessages(
    dplyr::bind_rows(lapply(rows, tibble::new_tibble, nrow = 1L)))
  rows <- lapply(0:2, function(t) global_cells(list(globals = list(tick = 1, a = t)), t))
  expect_identical(suppressMessages(global_table(rows)), slow(rows))
})

# Round 8: group aggregates without the padded stack ------------------------

# a run's value, warnings and error, and the RNG state after it
outcome <- function(f) {
  w <- character()
  v <- withCallingHandlers(
    tryCatch(f(), error = function(e) list(error = conditionMessage(e))),
    warning = function(x) {
      w <<- c(w, conditionMessage(x))
      invokeRestart("muffleWarning")
    })
  list(value = v, warnings = w, seed = get0(".Random.seed", globalenv()))
}
# the same run with one shortcut switched off
without <- function(f, name, value) {
  testthat::local_mocked_bindings(!!name := value, .package = "tidyABM")
  outcome(f)
}

two_groups <- function(nf = 5, nh = 12, seed = 1) abm_setup(
  agents = list(
    firm = abm_agents(n = nf, loan = ~c(NA, runif(n) - 0.2)[seq_len(n)],
                      status = ~sample(0:3, n, TRUE), x = ~rnorm(n), on = ~runif(n) < 0.5),
    hh = abm_agents(n = nh, dep = ~c(runif(1), NaN, runif(n))[seq_len(n)], x = ~rnorm(n),
                    firm = ~sample.int(max(nf, 1L), n, TRUE))),
  globals = list(k = 2, v = c(1, 2), tot = 0, a = 0, b = 0, c = 0, d = 0, e = 0,
                 f = 0, g = 0, h = 0, i = 0, j = 0, l = 0, m = 0, o = 0, p = 0,
                 q = 0, r = 0, s = 0, u = 0, z = 0),
  seed = seed)

narrow_go <- function() {
  myfun <- function(x) rev(x)
  abm_go(
    abm_global(
      a ~ sum(loan[.group == "firm"]),
      b ~ sum(loan[.group == "firm"] > 0, na.rm = TRUE),
      c ~ max(abs(loan - x)[.group == "firm"], na.rm = TRUE),   # x in both: falls back
      d ~ sum(status == 3L, na.rm = TRUE),
      e ~ median(loan, na.rm = TRUE),
      f ~ mean(dep * k, na.rm = TRUE),
      g ~ sum(dep[.group == "hh" & dep > 0.5]),
      h ~ sum(x[.group == "hh"]) + sum(x[.group == "firm"]),       # shared, quiet
      i ~ sum(loan[.group == "firm" & NA]),
      j ~ length(loan[.group == "firm" & FALSE]),
      l ~ sum(is.na(loan), na.rm = TRUE),                         # not NA on padding
      m ~ sum(dep[.group == "hh"] * k),
      o ~ sum((dep * v)[.group == "hh"]),                          # a vector global
      p ~ a + sum(loan[.group == "firm"], na.rm = TRUE),           # reads `a` just set
      q ~ sum(myfun(loan)[.group == "firm"], na.rm = TRUE),        # user function
      r ~ sum(as.integer(status)[.group == "firm"]),
      s ~ sum(loan[.group == "nobody"]),
      u ~ if (isTRUE(a > 0)) sum(on[.group == "firm"]) else -1,
      z ~ sum(runif(1) + dep[.group == "hh"])),                    # draws: not rewritten
    abm_rules(loan ~ loan + runif(n()) - 0.5, dep ~ dep * 0.9, status ~ sample(0:3, n(), TRUE)))
}

test_that("group aggregates over one group give the run the padded stack gives", {
  for (spec in list(c(5, 12), c(1, 7), c(6, 1), c(0, 9), c(4, 0))) {
    m <- two_groups(spec[1], spec[2])
    go <- narrow_go()
    meas <- list(m1 = ~sum(loan[.group == "firm"], na.rm = TRUE) / max(1, sum(dep[.group == "hh"], na.rm = TRUE)),
                 m2 = ~max(abs(dep - 1)[.group == "hh"], na.rm = TRUE),
                 m3 = ~sum(status == 2L, na.rm = TRUE))
    chk <- list(c1 = ~sum(x[.group == "firm"]) < 1e9)
    run <- function() {
      r <- abm_run(m, go, ticks = 3, seed = 4, measures = meas, checks = chk, record = "globals")
      list(abm_globals(r), abm_measures(r))
    }
    expect_identical(outcome(run), without(run, "narrow_plan", function(...) NULL))
  }
})

test_that("a warning or an error comes from the general path, once", {
  m <- two_groups()
  for (go in list(
    abm_go(abm_global(a ~ sum(sqrt(loan - 2)[.group == "firm"]))),
    abm_go(abm_global(a ~ max(loan[.group == "firm" & FALSE]))),
    abm_go(abm_global(a ~ max(dep[.group == "hh"] > 2, na.rm = TRUE) + log(-1))),
    abm_go(abm_global(a ~ sum(loan[.group == "firm"]) + "x")),
    abm_go(abm_global(a ~ loan[.group == "firm"])))) {
    run <- function() abm_globals(abm_run(m, go, ticks = 2, seed = 1))
    expect_identical(outcome(run), without(run, "narrow_plan", function(...) NULL))
  }
})

test_that("the narrow plan takes exactly the shapes it can evaluate exactly", {
  m <- two_groups()
  plan <- function(...) {
    p <- narrow_plan(rlang::quos(...), m$groups)
    if (is.null(p)) return(FALSE)
    unname(!vapply(p$rules, is.null, NA))
  }
  expect_true(plan(sum(loan[.group == "firm"])))
  expect_true(plan(sum(status == 3L, na.rm = TRUE)))
  expect_true(plan(sum(x[.group == "hh"])))                  # shared but quiet
  expect_false(plan(sum(log(x)[.group == "hh"])))            # shared and could warn
  expect_false(plan(sum(x, na.rm = TRUE)))                    # several groups' column
  expect_false(plan(sum(loan[.group == "firm"]) + runif(1)))  # draws
  expect_false(plan(sum(sample(loan)[.group == "firm"])))
  expect_false(plan(sum(loan[.group == "firm" | dep > 0])))
  expect_false(plan(sum(loan[.id > 2 & .group == "firm"])))
  expect_false(plan(sum(.id[.group == "firm"])))
  expect_false(plan(sum(loan[.group == "firm"]) / n()))
  expect_false(plan(sum(dep[.group == "firm"])))              # not that group's column
  expect_false(plan(ledger(.group == "firm", loan)))
  expect_false(plan(sum(loan[.group == "firm", drop = TRUE])))
  expect_false(plan({ y <- 1; sum(loan[.group == "firm"]) }))
  expect_false(plan(sum(loan[.group == "firm"]), .data$x))
  expect_identical(plan(sum(loan[.group == "firm"]), mean(x)), c(TRUE, FALSE))
  expect_null(narrow_plan(rlang::quos(sum(x)), m$groups[1]))
})

test_that("an aggregate's names are checked when it is evaluated", {
  m <- two_groups()
  st <- list(groups = m$groups, globals = m$globals)
  ev <- function(q, globals = m$globals) {
    p <- narrow_plan(list(q), m$groups)$rules[[1]]
    narrow_eval(p, q, m$groups, NULL, abm_eval_env(q, globals, n = 17L), 17L)
  }
  expect_identical(ev(rlang::quo(sum(loan[.group == "firm"] * k))),
                   sum(m$groups$firm$loan * 2))
  expect_null(ev(rlang::quo(sum((loan * v)[.group == "firm"]))))   # a vector
  sum <- function(...) 0                                           # a user's sum()
  expect_null(ev(rlang::quo(sum(loan[.group == "firm"]))))
  rm(sum)
  # `is.na()` is TRUE on a padding row: the household rows would be counted
  expect_null(ev(rlang::quo(sum(is.na(loan), na.rm = TRUE))))
  # a column whose type differs between the groups
  g2 <- m$groups
  g2$hh$x <- as.integer(round(g2$hh$x))
  q <- rlang::quo(sum(x[.group == "firm"]))
  p <- narrow_plan(list(q), g2)$rules[[1]]
  expect_null(narrow_eval(p, q, g2, NULL, abm_eval_env(q, m$globals, n = 17L), 17L))
  # a group whose labels are not its name
  g3 <- m$groups
  g3$firm$.group[2] <- "hh"
  expect_false(narrow_group_labels(g3))
  expect_null(narrow_plan(list(q), g3))
  expect_true(narrow_group_labels(m$groups))
})

# `.by` aggregates folded once ------------------------------------------------

test_that(".by aggregates of a filtered column give the group-by-group answer", {
  m <- two_groups(6, 15)
  gos <- list(
    abm_go(abm_rules(x ~ if_else(.group == "hh", sum(loan[.group == "firm"]), 0), .by = firm)),
    abm_go(abm_rules(x ~ mean(x[x > 0]) + max(x), y ~ sum(x > k), .by = firm)),
    abm_go(abm_rules(x ~ length(x[NA]) + sum(x[FALSE]) + prod(x), .by = firm)),
    abm_go(abm_rules(x ~ any(x[x > 5]) + all(x[x < 5]) + min(x[.group == "hh"]), .by = firm)),
    abm_go(abm_rules(x ~ sum((x * v)[x > 0]), .by = firm)),               # vector global
    abm_go(abm_rules(x ~ sum(x[runif(n()) > 0.5]), .by = firm)),           # draws
    abm_go(abm_rules(x ~ sum(sqrt(x)[x > -1]), .by = firm)),               # warns
    abm_go(abm_rules(firm ~ firm %% 3, x ~ sum(x[x > 0]), .by = firm),      # key rewritten
           abm_rules(firm ~ firm + 1L, .by = firm),
           abm_rules(x ~ sum(as.integer(x > 0)[x < 1]), .by = firm)))
  for (go in gos) {
    run <- function() abm_run(m, go, ticks = 3, seed = 2)
    expect_identical(outcome(run), without(run, "aggregates_by_key", function(...) NULL))
  }
})

test_that("the .by group locations are kept only while the key stands", {
  plan <- new.env()
  aug <- tibble::tibble(.id = 1:6, k = c(2, 1, 2, NA, 1, 2), x = c(1, 2, 3, 4, 5, 6))
  q <- rlang::quo(sum(x[x > 1]))
  a <- eval_by_key(q, aug, list(), aug$k, plan)
  expect_identical(a, eval_by_key(q, aug, list(), aug$k))
  expect_identical(plan$by_loc$key, aug$k)
  aug$k <- c(1, 1, 2, 2, NA, 1)
  expect_identical(eval_by_key(q, aug, list(), aug$k, plan), eval_by_key(q, aug, list(), aug$k))
  expect_identical(plan$by_loc$key, aug$k)
  aug$k <- c(1, 1, 2, 2, NaN, 1)                 # NaN is not NA
  expect_identical(eval_by_key(q, aug, list(), aug$k, plan), eval_by_key(q, aug, list(), aug$k))
})

# A lighter abm_tell() and abm_match() -----------------------------------------

test_that("match_rows() is match()", {
  cases <- list(list(1:10, 6:10), list(1:10, 5:9), list(c(1L, 3L, 2L), 2L), list(1:5, integer()),
                list(c(1L, NA, 3L), 3L), list(c(1, 2, 3), c(2, 3)), list(1:3, 1:3),
                list(c(1L, 2L, 2L, 3L), c(2L, 3L)), list(1:3, 1:4), list(c(5L, 7L, 9L), c(7L, 9L)))
  for (cs in cases) expect_identical(match_rows(cs[[1]], cs[[2]]), match(cs[[1]], cs[[2]]))
})

test_that("abm_tell() with the light augmentation gives the full one's run", {
  m <- two_groups(6, 15)
  orig_aug <- augment_group
  gos <- list(
    abm_go(abm_match(pair = "one_of", eligible = .group == "hh", among = .group == "firm"),
           abm_tell(x ~ dep, to = .partner, when = .group == "hh", .resolve = "sum")),
    abm_go(abm_match(pair = "one_of", eligible = .group == "hh", among = .group == "firm"),
           abm_tell(x ~ if_else(is.na(.role), dep, 0) + .group_id, to = .partner,
                    when = .group == "hh", .resolve = "mean")),
    abm_go(abm_death(when = .group == "hh" & runif(n()) < 0.2),
           abm_match(pair = "one_of", eligible = .group == "hh", among = .group == "firm"),
           abm_tell(x ~ dep, to = .partner, when = .group == "hh", .resolve = "max")),
    abm_go(abm_tell(x ~ dep, to = firm, when = .group == "hh", .resolve = "sum")))
  for (go in gos) {
    run <- function() abm_run(m, go, ticks = 3, seed = 5)
    expect_identical(outcome(run), without(run, "augment_group",
      function(..., match_cols = NULL) orig_aug(...)))
    expect_identical(outcome(run), without(run, "match_rows", function(ids, mids) match(ids, mids)))
  }
  # a recipient out of range, with ids 1..N and after a death
  bad <- abm_go(abm_tell(x ~ dep, to = firm + 100L, when = .group == "hh"))
  expect_error(abm_run(m, bad, ticks = 1, seed = 1), "No agent has .*107|No agent has")
  e1 <- tryCatch(abm_run(m, bad, ticks = 1, seed = 1), error = function(e) conditionMessage(e))
  bad2 <- abm_go(abm_death(when = .id == 3L), abm_tell(x ~ dep, to = firm + 100L, when = .group == "hh"))
  e2 <- tryCatch(abm_run(m, bad2, ticks = 1, seed = 1), error = function(e) conditionMessage(e))
  expect_match(e1, "does not exist")
  expect_match(e2, "does not exist")
  expect_identical(sub(".*No agent", "", e1), sub(".*No agent", "", e2))
})

test_that("one_of draws exactly as before, with and without roles", {
  old_one_of <- function(spec, agents, globals, pool, ch = NULL) {
    ch <- ch %||% list(ids = agents$.id, ok = NULL, w = NULL)
    candidates <- ch$ids
    if (length(pool) == 0L || length(candidates) == 0L) return(empty_match())
    usable <- pool[!(pool %in% candidates) | length(candidates) > 1L]
    pool <- usable
    if (length(pool) == 0L) return(empty_match())
    draw <- draw_from(candidates, length(pool))
    clash <- draw == pool
    while (any(clash)) {
      draw[clash] <- draw_from(candidates, sum(clash))
      clash <- draw == pool
    }
    role_a <- rep(NA_character_, length(pool))
    keep <- rep(TRUE, length(pool))
    new_match(pool[keep], draw[keep], role_a[keep], seq_len(sum(keep)))
  }
  agents <- tibble::tibble(.id = 1:8, .group = "a")
  for (cs in list(list(1:8, 1:8), list(1:8, 3L), list(3L, 3L), list(c(2L, 5L), 5:6),
                  list(integer(), 1:3), list(1:4, integer()))) {
    ch <- list(ids = cs[[2]], ok = NULL, w = NULL)
    set.seed(9); a <- match_one_of(list(role = NULL), agents, list(), cs[[1]], ch); sa <- .Random.seed
    set.seed(9); b <- old_one_of(list(role = NULL), agents, list(), cs[[1]], ch); sb <- .Random.seed
    expect_identical(a, b)
    expect_identical(sa, sb)
  }
})

test_that("a step with no group aggregate is never planned", {
  expect_false(narrowable(rlang::quos(sum(.group == "sheep"), mean(x))))
  expect_true(narrowable(rlang::quos(sum(.group == "sheep"), sum(x[.group == "a"]))))
  expect_true(narrowable(rlang::quos(sum(x, na.rm = TRUE))))
  m <- two_groups()
  # whatever narrow_plan() would rewrite, narrowable() lets through
  for (q in rlang::quos(sum(loan[.group == "firm"]), sum(status == 3L, na.rm = TRUE),
                        if (TRUE) max(dep[.group == "hh" & dep > 0]) else 0)) {
    expect_true(narrowable(list(q)))
    expect_false(is.null(narrow_plan(list(q), m$groups)))
  }
})

test_that("abm_tell reads partner_<col> without naming .partner", {
  m <- abm_setup(agents = abm_agents(n = 6, x = as.numeric(1:6), z = 0), seed = 1)
  run <- function(...) abm_run(m, abm_go(abm_match(pair = "random"), abm_tell(...)),
                               ticks = 1, seed = 1)
  r <- run(z ~ partner_x, to = 1L, .resolve = "sum")
  expect_identical(r$z[r$tick == 1], c(21, 0, 0, 0, 0, 0))
  # x == .id, so partner_x as the target is .partner
  a <- run(z ~ x, to = as.integer(partner_x), .resolve = "sum")
  b <- run(z ~ x, to = .partner, .resolve = "sum")
  expect_identical(a, b)
  a <- run(z ~ x, to = 1L, when = partner_x > 3, .resolve = "sum")
  b <- run(z ~ x, to = 1L, when = .partner > 3, .resolve = "sum")
  expect_identical(a, b)
})

test_that("opposite-group matching under .by takes the same draws as the general loop", {
  # `by = "side"` (a string) is not a bare column, so it takes the general loop
  w <- function(seed) abm_setup(agents = abm_agents(n = 60,
    side = ~sample(c("a", "b", NA), n, TRUE, prob = c(.45, .45, .1)),
    cell = ~sample(c(1:8, NA), n, TRUE), x = ~runif(n), z = 0), seed = seed)
  for (seed in 1:5) {
    go <- function(by) abm_go(
      abm_match(pair = "opposite_group", by = !!by, .by = cell, eligible = x > 0.2),
      abm_rules(z ~ coalesce(as.numeric(.partner), -1) + .group_id / 100))
    a <- abm_run(w(seed), go(quote(side)), ticks = 3, seed = seed)
    b <- abm_run(w(seed), go("side"), ticks = 3, seed = seed)
    expect_identical(a, b)
  }
  # a third value among those taking part is still reported
  bad <- abm_setup(agents = abm_agents(n = 6, side = c("a", "b", "c", "a", "b", "c"),
                                       cell = 1L, z = 0L), seed = 1)
  expect_error(abm_run(bad, abm_go(abm_match(pair = "opposite_group", by = side, .by = cell),
                                    abm_rules(z ~ .partner)),
                       ticks = 1), class = "tidyABM_bad_by")
})

test_that("group_sum() matches rowsum() for distinct and repeated keys", {
  x <- c(-0, NA, NaN, 1.5, -2, 3)
  for (g in list(c(6L, 2L, 4L, 1L, 3L, 5L), c(2L, 2L, 1L, 3L, 1L, 2L))) {
    for (na_rm in c(FALSE, TRUE)) for (v in list(x, c(1L, NA, 3L, -4L, 5L, 0L))) {
      s <- rowsum(v, g, na.rm = na_rm)
      got <- group_sum(v, g, na_rm)
      expect_identical(got$keys, as.integer(rownames(s)))
      expect_identical(got$sums, as.vector(s))
      expect_identical(1 / got$sums, 1 / as.vector(s))   # the sign of a zero
    }
  }
})

test_that("eval_rule's base eval() gives what eval_tidy() gives, draws included", {
  g <- tibble::tibble(.id = 1:6, .group = "a", x = c(-1, 0, 2.5, NA, 4, -0),
                      k = c(1L, 1L, 2L, 2L, 3L, NA))
  gl <- list(p = 0.5, q = 2)
  exprs <- rlang::quos(x + q, x > 0 & !is.na(k), if_else(x > p, x * q, -x),
                       runif(n()) < p, sum(x[k == 1L], na.rm = TRUE) + 1, k %in% c(1L, 3L),
                       pmin(x, q), sqrt(abs(x)), max(x, na.rm = TRUE), TRUE, x)
  for (q in exprs) {
    expect_true(base_eval_ok(rlang::quo_get_expr(q)))
    set.seed(3); a <- eval_rule(list(quo = q), g, gl, grouped = FALSE); sa <- .Random.seed
    set.seed(3)
    b <- rlang::eval_tidy(rlang::quo_set_env(q, abm_eval_env(q, gl, n = 6L)), data = g)
    expect_identical(a, b)
    expect_identical(sa, .Random.seed)
  }
  # not taken: a mask helper, a pronoun, a formula, an assignment, a coercion
  for (q in rlang::quos(row_number(), .data$x, case_when(x > 0 ~ 1, TRUE ~ 0),
                        { y <- x; y }, as.integer(x), x <<- 1)) {
    expect_false(base_eval_ok(rlang::quo_get_expr(q)))
  }
  # a result of the wrong length goes to mutate(), drawing twice as before
  set.seed(5)
  expect_error(eval_rule(list(quo = rlang::quo(runif(3))), g, gl, grouped = FALSE))
  s1 <- .Random.seed
  set.seed(5); runif(3); runif(3)
  expect_identical(s1, .Random.seed)
})

test_that("a .by sum over groups of one matches dplyr's grouped sum", {
  d <- tibble::tibble(.id = 1:9, .group = rep(c("f", "h", "h"), 3),
                      k = c(1, 1, 1, 2, 2, NA, 3, 3, 3),
                      x = c(-0, 5, 6, NA, 1, 2, 1e308, 7, 8),
                      i = c(3L, 1L, 1L, NA, 2L, 2L, 5L, 1L, 1L))
  for (e in rlang::quos(sum(x[.group == "f"]), sum(i[.group == "f"]) + 0L,
                        if_else(.group == "h", sum(x[.group == "f"]), 0))) {
    got <- eval_rule(list(quo = e), d, list(), grouped = FALSE, by = "k",
                     cache = new.env())
    ref <- dplyr::pull(dplyr::mutate(dplyr::group_by(d, k), v = !!e), v)
    keep <- !is.na(d$k)
    expect_identical(got[keep], ref[keep])
    expect_identical(1 / got[keep], 1 / ref[keep])
  }
})

test_that("abm_tell's sum is sum() per recipient", {
  m <- abm_setup(agents = abm_agents(n = 40, v = ~rnorm(n) * 1e6, to = ~sample.int(5L, n, TRUE),
                                     s = 0), seed = 2)
  r <- abm_run(m, abm_go(abm_tell(s ~ v, to = to, .resolve = "sum")), ticks = 1, seed = 2)
  a <- r[r$tick == 0, ]
  b <- r[r$tick == 1, ]
  ref <- vapply(1:5, function(j) sum(a$v[a$to == j]), 0)
  expect_identical(b$s[1:5], ref)
})

test_that("a neighbourhood of one gives what the pair view gives", {
  w <- abm_setup(agents = list(
      p = abm_agents(n = 12, x = ~sample(c(TRUE, FALSE, NA), n, TRUE), v = ~c(-0, rnorm(n - 1)),
                     i = ~sample(c(1L, NA), n, TRUE)),
      m = abm_agents(n = 9, cell = ~sample(c(1:14, NA), n, TRUE), a = FALSE, s = 0, k = 0L,
                     c = 0L, t = 0)), seed = 4)
  go <- function(within) abm_go(abm_neighbours(a ~ any(x), t ~ sum(v), s ~ sum(is.na(i)),
                                               k ~ sum(i), c ~ n(), within = !!within))
  fast <- quote(.group == "p" & .id == own_cell & !is.na(x))
  slow <- quote(.group == "p" & .id == own_cell & !is.na(x) & abs(1) == 1)
  a <- abm_run(w, go(fast), ticks = 2, seed = 4)
  b <- abm_run(w, go(slow), ticks = 2, seed = 4)
  expect_identical(a, b)
  expect_identical(1 / a$t, 1 / b$t)
  # and the fast case is the one taken
  expect_false(is.null(nb_one_shape(go(fast)$steps[[1]], equijoin_key(fast))))
  expect_null(nb_one_shape(go(slow)$steps[[1]], equijoin_key(slow)))
})

test_that("append_rows() stacks offspring as vec_rbind() does", {
  g <- tibble::tibble(.id = 1:4, .group = "a", x = c(1.5, 2, NA, 4), ok = c(TRUE, NA, FALSE, TRUE))
  k <- vctrs::vec_slice(g, c(2L, 2L, 4L))
  k <- set_col(k, ".id", 5:7)
  expect_identical(append_rows(g, k), vctrs::vec_rbind(g, k))
  # a type that differs, or a classed column, is left to vec_rbind()
  k2 <- set_col(k, "x", 1:3)
  expect_identical(append_rows(g, k2), vctrs::vec_rbind(g, k2))
  g3 <- set_col(g, "d", as.Date("2020-01-01") + 0:3)
  expect_identical(append_rows(g3, vctrs::vec_slice(g3, 1L)), vctrs::vec_rbind(g3, vctrs::vec_slice(g3, 1L)))
})

test_that("the narrowing check is kept per environment and function globals", {
  rm(list = ls(.std_memo, all.names = TRUE), envir = .std_memo)
  e1 <- new.env(parent = baseenv())
  e2 <- new.env(parent = baseenv())
  e2$sum <- function(...) -1
  expect_true(standard_fns_in(c("sum", "["), e1, sig = list(e1, list())))
  expect_false(standard_fns_in(c("sum", "["), e2, sig = list(e2, list(sum = e2$sum))))
  # the first answer is still the answer for its own environment
  expect_true(standard_fns_in(c("sum", "["), e1, sig = list(e1, list())))
})
