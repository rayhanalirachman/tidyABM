# Relations: state that belongs to a pair of agents ------------------------

# three households (1:3) buying from four firms (4:7); unmet is what each
# seller has failed to deliver to that household
sellers_df <- function() {
  data.frame(from = c(1L, 1L, 2L, 2L, 3L, 3L),
             to   = c(4L, 5L, 5L, 6L, 6L, 7L),
             unmet = c(0, 5, 0, 0, 2, 0))
}
market <- function(...) {
  abm_setup(
    agents = list(hh = abm_agents(n = 3, pick = NA_integer_, pi = 0, rat = FALSE, got = 0, ...),
                  f  = abm_agents(n = 4, price = c(1, 2, 3, 4), n_buy = 0, inv = 3)),
    relations = list(sellers = abm_relation(edges = sellers_df(), unmet = 0)),
    seed = 1)
}

# --- declaration ---------------------------------------------------------

test_that("abm_relation() keeps its value columns and fills declared defaults", {
  r <- abm_relation(edges = data.frame(from = 1:2, to = 3:4), unmet = 0, seen = FALSE)
  expect_s3_class(r, "abm_relation")
  expect_equal(r$cols, c("unmet", "seen"))
  expect_equal(r$edges$unmet, c(0, 0))
  expect_equal(r$defaults, list(unmet = 0, seen = FALSE))
  # a column present in edges but not declared defaults to NA of its type
  r2 <- abm_relation(edges = data.frame(from = 1L, to = 2L, w = 3.5))
  expect_equal(r2$defaults$w, NA_real_)
  expect_snapshot(print(r))
})

test_that("abm_relation() refuses what cannot be a relation", {
  expect_error(abm_relation(edges = data.frame(a = 1, b = 2)), class = "tidyABM_bad_relation")
  expect_error(abm_relation(edges = data.frame(from = 1L, to = 1L)), class = "tidyABM_bad_relation")
  expect_error(abm_relation(edges = data.frame(from = c(1L, 1L), to = c(2L, 2L))),
               class = "tidyABM_bad_relation")
  expect_error(abm_relation(edges = data.frame(from = 1L, to = 2L), w = 1:2),
               class = "tidyABM_bad_relation")
  expect_error(abm_relation(edges = data.frame(from = 1L, to = 2L), 5),
               class = "tidyABM_bad_relation")
})

test_that("abm_setup() validates relations against the population", {
  bad_id <- data.frame(from = 1L, to = 99L)
  expect_error(abm_setup(agents = abm_agents(n = 3, x = 0),
                         relations = list(r = abm_relation(bad_id))),
               class = "tidyABM_bad_relation")
  ok <- data.frame(from = 1L, to = 2L)
  # reserved and clashing names
  for (nm in c("partner", "own", ".r")) {
    rels <- stats::setNames(list(abm_relation(ok)), nm)
    expect_error(abm_setup(agents = abm_agents(n = 3, x = 0), relations = rels),
                 class = "tidyABM_bad_relation")
  }
  expect_error(abm_setup(agents = abm_agents(n = 3, r_v = 0),
                         relations = list(r = abm_relation(ok, v = 0))),
               class = "tidyABM_bad_relation")
  expect_error(abm_setup(agents = abm_agents(n = 3, x = 0), globals = list(r_v = 1),
                         relations = list(r = abm_relation(ok, v = 0))),
               class = "tidyABM_bad_relation")
  expect_error(abm_setup(agents = abm_agents(n = 3, x = 0),
                         relations = list(abm_relation(ok))),
               class = "tidyABM_bad_relation")
  expect_error(abm_setup(agents = abm_agents(n = 3, x = 0),
                         relations = list(a = abm_relation(ok, b_c = 0),
                                          a_b = abm_relation(ok, c = 0))),
               class = "tidyABM_bad_relation")
  m <- market()
  expect_named(m$relations, "sellers")
  expect_snapshot(print(m))
})

# --- the pair view -------------------------------------------------------

test_that("among, weight and within see the pair's relation columns", {
  r <- abm_run(market(), abm_go(
    # a seller of mine, weighted by how much it rationed me: only rows with unmet > 0
    abm_match(pair = "one_of", eligible = .group == "hh",
              among = .group == "f" & .sellers, weight = sellers_unmet),
    abm_rules(pick ~ .partner, .scope = "population"),
    abm_neighbours(pi ~ mean(price), within = .sellers),
    abm_neighbours(rat ~ any(sellers_unmet > 0), within = .sellers),
    abm_neighbours(n_buy ~ n(), within = .sellers_back)
  ), ticks = 1, seed = 1)
  d <- r[r$tick == 1, ]
  expect_equal(d$pick[1:3], c(5L, NA, 6L))        # hh2's sellers all weigh 0: sits out
  expect_equal(d$pi[1:3], c(1.5, 2.5, 3.5))
  expect_equal(d$rat[1:3], c(TRUE, FALSE, TRUE))
  expect_equal(d$n_buy[4:7], c(1, 2, 2, 1))
})

test_that("!.R picks only non-sellers, and the fast path matches the cross product", {
  r <- abm_run(market(), abm_go(
    abm_match(pair = "one_of", eligible = .group == "hh", among = .group == "f" & !.sellers),
    abm_rules(pick ~ .partner, .scope = "population")), ticks = 1, seed = 3)
  d <- r[r$tick == 1, ]
  s <- sellers_df()
  for (i in 1:3) expect_false(d$pick[i] %in% s$to[s$from == i])

  # within = .sellers (relation rows) and within = .sellers & TRUE (pair view)
  # must agree exactly
  a <- abm_run(market(), abm_go(abm_neighbours(pi ~ mean(price), within = .sellers)),
               ticks = 1, seed = 1)
  b <- abm_run(market(), abm_go(abm_neighbours(pi ~ mean(price), within = .sellers & TRUE)),
               ticks = 1, seed = 1)
  expect_equal(a$pi, b$pi)
})

test_that("cost sees relation columns too", {
  r <- abm_run(market(), abm_go(
    abm_match(pair = "nearest", eligible = .group == "hh", among = .group == "f",
              cost = if_else(.sellers, price, NA_real_)),   # cheapest of MY sellers
    abm_rules(pick ~ .partner, .scope = "population")), ticks = 1, seed = 1)
  expect_equal(r$pick[r$tick == 1][1:3], c(4L, 5L, 6L))
})

# --- under a standing match ----------------------------------------------

test_that("a rule reads and writes the pair's value under a match", {
  r <- abm_run(market(), abm_go(
    abm_match(pair = "one_of", eligible = .group == "hh", among = .group == "f" & .sellers),
    abm_sequential(got ~ pmin(2, partner_inv), partner_inv ~ partner_inv - got,
                   sellers_unmet ~ sellers_unmet + (2 - got)),
    abm_rules(sellers_unmet ~ sellers_unmet + 100)          # same pair, whole-step write
  ), ticks = 1, seed = 2)
  rel <- abm_relations(r, "sellers")
  expect_equal(sum(rel$unmet >= 100), 3L)                    # exactly one row per household
  expect_true(all(rel$unmet < 100 | rel$unmet >= 100))
  # existence tests under a match
  r2 <- abm_run(market(), abm_go(
    abm_match(pair = "one_of", eligible = .group == "hh", among = .group == "f"),
    abm_rules(rat ~ .sellers, .scope = "population")), ticks = 1, seed = 5)
  d <- r2[r2$tick == 1, ]
  # .sellers under the match equals whether (hh -> partner) is a row
  s <- sellers_df()
  m <- abm_run(market(), abm_go(
    abm_match(pair = "one_of", eligible = .group == "hh", among = .group == "f"),
    abm_rules(pick ~ .partner, .scope = "population")), ticks = 1, seed = 5)
  pk <- m$pick[m$tick == 1][1:3]
  expect_equal(d$rat[1:3], mapply(function(i, p) any(s$from == i & s$to == p), 1:3, pk))
})

test_that("_back reads and writes the reverse row", {
  r <- abm_run(market(), abm_go(
    abm_pairs(via = "sellers", unmet ~ c(7, 8, 9, 10, 11, 12)),
    abm_match(pair = "one_of", eligible = .group == "f", among = .group == "hh" & .sellers_back),
    abm_rules(n_buy ~ sellers_unmet_back, .scope = "population"),
    abm_rules(sellers_unmet_back ~ 0)
  ), ticks = 1, seed = 3)
  d <- r[r$tick == 1, ]
  expect_true(all(d$n_buy[4:7] %in% c(7:12, NA)))
  rel <- abm_relations(r, "sellers")
  expect_equal(sum(rel$unmet == 0), sum(!is.na(d$n_buy[4:7]) & d$n_buy[4:7] > 0))
})

test_that("writes to a pair that is not there, or with no pairing, are errors", {
  expect_error(abm_run(market(), abm_go(
    abm_match(pair = "one_of", eligible = .group == "hh", among = .group == "f" & !.sellers),
    abm_rules(sellers_unmet ~ 1)), ticks = 1, seed = 1),
    class = "tidyABM_not_related")
  expect_error(abm_run(market(), abm_go(
    abm_match(pair = "one_of", eligible = .group == "hh", among = .group == "f" & !.sellers),
    abm_sequential(sellers_unmet ~ 1)), ticks = 1, seed = 1),
    class = "tidyABM_not_related")
  expect_error(abm_run(market(), abm_go(abm_rules(sellers_unmet ~ 1)), ticks = 1),
               class = "tidyABM_no_match")
  expect_error(abm_run(market(), abm_go(abm_sequential(sellers_unmet ~ 1)), ticks = 1),
               class = "tidyABM_no_match")
  expect_error(abm_run(market(), abm_go(
    abm_match(pair = "random", size = 3), abm_rules(sellers_unmet ~ 1)), ticks = 1),
    class = "tidyABM_no_match")
  expect_error(abm_run(market(), abm_go(
    abm_match(pair = "one_of", eligible = .group == "hh", among = .group == "f" & .sellers),
    abm_rules(.sellers ~ TRUE)), ticks = 1),
    class = "tidyABM_bad_target")
  expect_error(abm_run(market(), abm_go(
    abm_match(pair = "one_of", eligible = .group == "hh", among = .group == "f" & .sellers),
    abm_rules(sellers_unmet ~ 1, .by = .group)), ticks = 1),
    class = "tidyABM_bad_target")
  expect_error(abm_run(market(), abm_go(
    abm_neighbours(sellers_unmet ~ mean(price), within = .sellers)), ticks = 1),
    class = "tidyABM_bad_target")
})

test_that("under a mutual match a directed relation errors from the wrong side", {
  m <- abm_setup(agents = abm_agents(n = 2, x = 0),
                 relations = list(owes = abm_relation(data.frame(from = 1L, to = 2L), amt = 0)))
  # both agents have a row under a mutual match, and (2 -> 1) is not a pair
  expect_error(abm_run(m, abm_go(abm_match(pair = "random"), abm_rules(owes_amt ~ 1)), ticks = 1),
               class = "tidyABM_not_related")
  # the directional form writes from the one side that has the row
  r <- abm_run(m, abm_go(abm_match(pair = "one_of", eligible = .id == 1L),
                         abm_rules(owes_amt ~ 1)), ticks = 1, seed = 1)
  expect_equal(abm_relations(r, "owes")$amt, 1)
})

# --- link / unlink via ---------------------------------------------------

test_that("abm_link(via, to) and abm_unlink(via, to) rewire without a match or a network", {
  m <- abm_setup(
    agents = list(hh = abm_agents(n = 3, cand = c(6L, 7L, 4L), drop = c(4L, 5L, 6L),
                                  swap = c(TRUE, TRUE, FALSE)),
                  f  = abm_agents(n = 4, price = 1)),
    relations = list(sellers = abm_relation(sellers_df(), unmet = 0)))
  r <- abm_run(m, abm_go(
    abm_unlink(via = "sellers", to = drop, when = swap),
    abm_link(via = "sellers", to = cand, when = swap)), ticks = 1, seed = 1)
  rel <- abm_relations(r, "sellers")
  expect_equal(nrow(rel), 6L)
  expect_true(all(c("1 6", "2 7") %in% paste(rel$from, rel$to)))
  expect_false(any(c("1 4", "2 5") %in% paste(rel$from, rel$to)))
  expect_true(all(c("3 6", "3 7") %in% paste(rel$from, rel$to)))      # swap = FALSE untouched
  expect_equal(rel$unmet[paste(rel$from, rel$to) %in% c("1 6", "2 7")], c(0, 0))  # defaults
})

test_that("abm_link(via) under a match is directed, deduplicates, and takes value rules", {
  r <- abm_run(market(), abm_go(
    abm_match(pair = "one_of", eligible = .group == "hh", among = .group == "f"),
    abm_link(via = "sellers", unmet ~ 99),
    abm_rules(sellers_unmet ~ sellers_unmet + 1)
  ), ticks = 3, seed = 7)
  rel <- abm_relations(r, "sellers")
  expect_true(all(rel$from %in% 1:3 & rel$to %in% 4:7))                # hh -> firm only
  expect_false(anyDuplicated(paste(rel$from, rel$to)) > 0)
  new <- !paste(rel$from, rel$to) %in% paste(sellers_df()$from, sellers_df()$to)
  expect_true(all(rel$unmet[new] >= 100))                              # 99 at creation, then +1s
  # link-then-write accumulates across ticks on a repeated pair
  expect_true(any(rel$unmet[new] > 100))
})

test_that("via link steps refuse what they cannot do", {
  expect_error(abm_link(x ~ 1), class = "tidyABM_bad_link")
  expect_error(abm_link(to = cand), class = "tidyABM_bad_link")
  expect_error(abm_unlink(to = cand), class = "tidyABM_bad_link")
  expect_error(abm_link(via = 1), class = "tidyABM_no_relation")
  expect_error(abm_run(market(), abm_go(abm_link(via = "sellers")), ticks = 1),
               class = "tidyABM_no_match")
  expect_error(abm_run(market(), abm_go(abm_match(pair = "random", size = 3),
                                        abm_link(via = "sellers")), ticks = 1),
               class = "tidyABM_bad_link")
  expect_error(abm_run(market(), abm_go(abm_link(via = "nope", to = pick)), ticks = 1),
               class = "tidyABM_no_relation")
  m <- abm_setup(agents = abm_agents(n = 3, tgt = c(2L, 3L, 1L), self = 1:3),
                 relations = list(r = abm_relation(data.frame(from = 1L, to = 2L))))
  expect_error(abm_run(m, abm_go(abm_link(via = "r", to = self)), ticks = 1),
               class = "tidyABM_bad_link")
  expect_error(abm_run(m, abm_go(abm_link(via = "r", to = 99L)), ticks = 1),
               class = "tidyABM_bad_link")
})

test_that("network link/unlink are unchanged when via is absent", {
  m <- abm_setup(agents = abm_agents(n = 6, x = 0), network = abm_network(type = "empty"))
  r <- abm_run(m, abm_go(abm_match(pair = "random"), abm_link()), ticks = 1, seed = 1)
  expect_equal(nrow(abm_edges(r)), 3L)
  r <- abm_run(m, abm_go(abm_match(pair = "random"), abm_link(),
                         abm_match(pair = "network"), abm_unlink()), ticks = 1, seed = 1)
  expect_equal(nrow(abm_edges(r)), 0L)
})

# --- abm_pairs -----------------------------------------------------------

test_that("abm_pairs() updates every pair, sees both endpoints, and honours .when", {
  r <- abm_run(market(), abm_go(
    abm_pairs(via = "sellers", unmet ~ unmet * 2, seen_price ~ to_price),
    abm_pairs(via = "sellers", unmet ~ -1, .when = from_.id == 3L)
  ), ticks = 1, seed = 1)
  rel <- abm_relations(r, "sellers")
  expect_equal(rel$unmet, c(0, 10, 0, 0, -1, -1))
  expect_equal(rel$seen_price, c(1, 2, 2, 3, 3, 4))
  expect_error(abm_pairs(via = "sellers", from ~ 1), class = "tidyABM_bad_formula")
  expect_error(abm_pairs(via = "sellers", to_price ~ 1), class = "tidyABM_bad_formula")
  expect_error(abm_pairs(via = 1, v ~ 1), class = "tidyABM_no_relation")
  expect_error(abm_run(market(), abm_go(abm_pairs(via = "nope", v ~ 1)), ticks = 1),
               class = "tidyABM_no_relation")
  expect_snapshot(print(abm_pairs(via = "sellers", unmet ~ 0, .when = to_price > 1)))
})

# --- lifetime and results -------------------------------------------------

test_that("death prunes relation rows on either side", {
  m <- abm_setup(agents = abm_agents(n = 4, die = c(FALSE, FALSE, TRUE, FALSE)),
                 relations = list(r = abm_relation(data.frame(from = c(1L, 2L, 3L), to = c(2L, 3L, 4L)))))
  r <- abm_run(m, abm_go(abm_death(when = die)), ticks = 1, seed = 1)
  expect_equal(abm_relations(r, "r"), tibble::tibble(from = 1L, to = 2L))
})

test_that("abm_relations() returns the tables, and carries .rep under reps", {
  r <- abm_run(market(), abm_go(abm_pairs(via = "sellers", unmet ~ unmet + 1)), ticks = 2, seed = 1)
  expect_named(abm_relations(r), "sellers")
  expect_equal(abm_relations(r, "sellers")$unmet, sellers_df()$unmet + 2)
  expect_error(abm_relations(r, "nope"), class = "tidyABM_no_relation")
  m0 <- abm_setup(agents = abm_agents(n = 2, x = 0))
  expect_null(abm_relations(abm_run(m0, abm_go(abm_rules(x ~ x + 1)), ticks = 1)))
  rr <- abm_run(market(), abm_go(abm_pairs(via = "sellers", unmet ~ unmet + 1)),
                ticks = 1, reps = 2, seed = 1)
  tb <- abm_relations(rr, "sellers")
  expect_true(all(c(".run", ".rep") %in% names(tb)))
  expect_equal(nrow(tb), 12L)
})

test_that("relations survive a lattice setup and print", {
  m <- abm_setup(agents = abm_agents(n = 4, x = 0),
                 network = abm_network(type = "grid", dims = c(2, 2)),
                 relations = list(r = abm_relation(data.frame(from = 1L, to = 3L), w = 1)))
  expect_named(m$relations, "r")
  r <- abm_run(m, abm_go(abm_pairs(via = "r", w ~ w + 1)), ticks = 1, seed = 1)
  expect_equal(abm_relations(r, "r")$w, 2)
})

test_that("abm_odd() lists a relation as an entity and its steps under interaction", {
  m <- market()
  go <- abm_go(
    abm_match(pair = "one_of", eligible = .group == "hh", among = .group == "f" & .sellers),
    abm_rules(sellers_unmet ~ sellers_unmet + 1),
    abm_pairs(via = "sellers", unmet ~ 0))
  txt <- paste(capture.output(print(abm_odd(m, go, ticks = 3))), collapse = "\n")
  expect_match(txt, "sellers.*a relation between agents")
  expect_match(txt, "State held on pairs of agents")
  expect_match(txt, "State held on the pair it is looking at")
})
