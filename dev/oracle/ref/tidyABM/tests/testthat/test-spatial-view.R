# abm_view_lattice(): a self-contained HTML playback of a recorded run

view_model <- function() {
  abm_setup(
    agents = list(
      patches = abm_agents(alive = ~.x > 3, heat = ~.x + .y, kind = ~if_else(.y > 3, "n", "s")),
      bugs = abm_agents(n = 3, energy = ~c(1.5, 2.25, 3), at = ~c(1L, 2L, 36L))),
    network = abm_network(type = "grid", dims = c(6, 6), on = "patches"))
}
view_go <- abm_go(abm_rules(alive ~ !alive, heat ~ heat * 0.5),
                  abm_move(along = "patches", to = "random_neighbour", who = "bugs"),
                  abm_rules(energy ~ energy - 0.5))

test_that("abm_view_lattice() writes one self-contained file holding every frame", {
  r <- abm_run(view_model(), view_go, ticks = 4, seed = 1)
  f <- withr::local_tempfile(fileext = ".html")
  expect_invisible(out <- abm_view_lattice(r, fill = alive, agents = TRUE,
                                           hover = c("energy", "heat"), file = f,
                                           title = "Bugs & cells"))
  expect_identical(out, f)
  expect_true(file.exists(f))
  html <- paste(readLines(f, encoding = "UTF-8"), collapse = "\n")

  # one file: nothing fetched from anywhere
  expect_false(grepl("<script[^>]+src=", html))
  expect_false(grepl("<link[^>]+href=", html))
  expect_false(grepl("https?://", html))
  expect_match(html, "<canvas", fixed = TRUE)
  expect_match(html, "<title>Bugs &amp; cells</title>", fixed = TRUE)

  # the data: every recorded tick, the lattice's size, the layers asked for
  expect_match(html, '"ticks":[0,1,2,3,4]', fixed = TRUE)
  expect_match(html, '"w":6,"h":6', fixed = TRUE)
  expect_match(html, '"name":"alive","type":"discrete","levels":["FALSE","TRUE"]', fixed = TRUE)
  expect_match(html, '"name":"heat","type":"continuous"', fixed = TRUE)
  expect_match(html, '"groups":["bugs"]', fixed = TRUE)
  expect_match(html, '"name":"energy","type":"number"', fixed = TRUE)
  # five frames of cells, five of agents, three agents in each
  frames <- regmatches(html, regexpr('"frames":\\[\\[.*?\\]\\],"agents"', html))
  expect_length(gregexpr('\\["', frames)[[1]], 5L)
  agents <- sub('.*"agents":\\{.*"frames":(\\[.*\\])\\}\\}.*', "\\1", html)
  expect_equal(lengths(regmatches(agents, gregexpr("\\[[^][]*\\]", agents))), 5L)
})

test_that("abm_view_lattice() encodes cells as run lengths over 64 symbols", {
  # the symbols: no digits (those are run lengths), nothing that needs escaping
  expect_length(tidyABM:::code_symbols, 64L)
  expect_false(any(grepl("[0-9\"\\\\/<>.]", tidyABM:::code_symbols)))
  expect_identical(tidyABM:::rle_encode(c(0L, 0L, 0L, 1L, NA, NA, 2L)), "A3B.2C")
  expect_identical(tidyABM:::rle_encode(c(63L, 63L)), ":2")

  d <- data.frame(.x = rep(1:3, 2), .y = rep(1:2, each = 3), v = c(1, 1, 1, 2, 2, 3))
  f <- withr::local_tempfile(fileext = ".html")
  abm_view_lattice(d, fill = v, file = f)
  html <- paste(readLines(f), collapse = "\n")
  # whole numbers over a short range are kept exactly, from the minimum
  expect_match(html, '"min":1,"max":3,"step":1', fixed = TRUE)
  expect_match(html, '"frames":[["A3B2C"]]', fixed = TRUE)
  # a bare data frame is one frame, tick 0
  expect_match(html, '"ticks":[0]', fixed = TRUE)
})

test_that("abm_view_lattice() keeps a long run compact", {
  skip_on_cran()
  m <- abm_setup(agents = list(patches = abm_agents(alive = ~runif(n) < 0.3),
                               bugs = abm_agents(n = 50, at = ~sample(2500, n))),
                 network = abm_network(type = "grid", dims = c(50, 50), on = "patches"))
  go <- abm_go(abm_neighbours(k ~ sum(alive)),
               abm_rules(alive ~ k == 3 | (alive & k == 2)),
               abm_move(along = "patches", to = "random_neighbour", who = "bugs"))
  r <- abm_run(m, go, ticks = 200, seed = 1, progress = FALSE)
  f <- withr::local_tempfile(fileext = ".html")
  abm_view_lattice(r, fill = alive, agents = TRUE, file = f)
  expect_lt(file.size(f), 1.5e6)        # 50 x 50 x 201 frames, 50 agents
})

test_that("abm_view_lattice() names, colours and marks what it draws", {
  r <- abm_run(view_model(), view_go, ticks = 2, seed = 1)
  f <- withr::local_tempfile(fileext = ".html")
  abm_view_lattice(r, fill = heat, agents = TRUE, colour = energy,
                   mark = if_else(alive, "live", NA_character_), file = f,
                   labels = c(fill = "warmth", colour = "fuel", mark = "life"),
                   fill_colours = c("white", "grey10"),
                   mark_colours = c(live = "#D55E00"),
                   agent_colours = c("#DEEBF7", "#08306B"))
  html <- paste(readLines(f), collapse = "\n")
  expect_match(html, '"name":"warmth","type":"continuous"', fixed = TRUE)
  expect_match(html, '"colours":["white","#1A1A1A"]', fixed = TRUE)   # grey10 -> hex
  expect_match(html, '"name":"life","type":"discrete","levels":["live"],"colours":["#D55E00"],"role":"mark"', fixed = TRUE)
  expect_match(html, '"name":"fuel","type":"continuous"', fixed = TRUE)
  expect_match(html, '"title":"warmth"', fixed = TRUE)

  # no fill at all: grey cells under the agents, titled generically
  abm_view_lattice(r, agents = TRUE, file = f)
  html <- paste(readLines(f), collapse = "\n")
  expect_match(html, '"title":"A lattice run"', fixed = TRUE)
  expect_match(html, '"layers":[]', fixed = TRUE)
  expect_match(html, '"colours":["#0072B2"]', fixed = TRUE)            # Okabe-Ito
})

test_that("abm_view_lattice() refuses what it cannot play", {
  m <- abm_setup(agents = abm_agents(n = 3, z = 0))
  r <- abm_run(m, abm_go(abm_rules(z ~ z + 1)), ticks = 1, seed = 1)
  expect_error(abm_view_lattice(r, fill = z), class = "tidyABM_no_lattice")

  g <- abm_setup(agents = abm_grid(dims = c(3, 3), z = 0))
  go <- abm_go(abm_rules(z ~ z + 1))
  none <- abm_run(g, go, ticks = 4, seed = 1, record = "globals")
  expect_error(abm_view_lattice(none, fill = z), class = "tidyABM_bad_arg",
               regexp = "no recorded frames")
  some <- abm_run(g, go, ticks = 4, seed = 1, record = 2)
  expect_error(abm_view_lattice(some, fill = z, tick = c(2, 3)),
               class = "tidyABM_bad_arg", regexp = "not recorded")
  expect_error(abm_view_lattice(some, fill = z, agents = "yes"), class = "tidyABM_bad_arg")
  expect_error(abm_view_lattice(some, fill = z, hover = "nope"), class = "tidyABM_bad_arg")
  expect_error(abm_view_lattice(some, fill = z, labels = c(size = "x")), class = "tidyABM_bad_arg")
  expect_error(abm_view_lattice(some, fill = z, fps = 0), class = "tidyABM_bad_arg")
  expect_error(abm_view_lattice(some, fill = z, file = 1), class = "tidyABM_bad_arg")
  expect_error(abm_view_lattice(some, fill = z, mark = z), class = "tidyABM_bad_arg")
  expect_error(abm_view_lattice(some, fill = as.list(z)), class = "tidyABM_bad_arg")

  # several runs cannot share one slider
  many <- rbind(cbind(.run = 1L, some), cbind(.run = 2L, some))
  expect_error(abm_view_lattice(many, fill = z), class = "tidyABM_bad_arg",
               regexp = "2 runs")
})

test_that("to_json() writes what the viewer reads", {
  j <- tidyABM:::to_json
  expect_identical(j(NULL), "null")
  expect_identical(j(1L), "1")
  expect_identical(j(c(1.5, NA, Inf)), "[1.5,null,null]")
  expect_identical(j(c(TRUE, NA)), "[true,null]")
  expect_identical(j("a\"b\\c\n</script>"), "\"a\\\"b\\\\c\\n<\\/script>\"")
  expect_identical(j(list(a = 1, b = list("x", 2L), c = list())), '{"a":1,"b":["x",2],"c":[]}')
  expect_identical(j(as.list(1)), "[1]")
  expect_identical(j(integer()), "[]")
  expect_identical(j(factor(c("b", NA))), '["b",null]')
})
