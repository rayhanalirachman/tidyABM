# abm_plot_lattice(): a ggplot of a lattice frame

test_that("abm_plot_lattice() draws the cells of a tick, with the agents on top", {
  skip_if_not_installed("ggplot2")
  w <- h <- 6
  m <- abm_setup(
    agents = list(patches = abm_agents(alive = ~.x > 3),
                  bugs = abm_agents(n = 3, at = ~c(1L, 2L, 36L))),
    network = abm_network(type = "grid", dims = c(w, h), on = "patches"))
  r <- abm_run(m, abm_go(abm_rules(alive ~ !alive)), ticks = 2, seed = 1)

  p <- abm_plot_lattice(r, fill = alive)
  expect_s3_class(p, "ggplot")
  expect_equal(nrow(p$data), w * h)            # the last tick's cells only
  expect_true(all(p$data$tick == 2L))
  expect_false(any(!is.na(p$data$.cell)))
  expect_length(p$layers, 1L)

  p2 <- abm_plot_lattice(r, fill = alive, tick = c(0, 2), agents = TRUE)
  expect_equal(nrow(p2$data), 2 * w * h)
  expect_length(p2$layers, 2L)                  # raster + points
  expect_equal(nrow(p2$layers[[2]]$data), 6L)   # three bugs, two ticks
  expect_s3_class(p2$facet, "FacetWrap")

  # no fill: a plain grey field, which is what one draws agents over
  expect_length(abm_plot_lattice(r, agents = TRUE)$layers, 2L)
  # a bare data frame with coordinates works too
  expect_s3_class(abm_plot_lattice(data.frame(.x = 1:2, .y = 1L, z = 1:2), fill = z),
                  "ggplot")
})

test_that("abm_plot_lattice() leaves empty (NA) cells blank without a warning", {
  skip_if_not_installed("ggplot2")
  # a Schelling-style frame: vacant cells carry NA in the fill column
  d <- data.frame(.x = rep(1:4, 4), .y = rep(1:4, each = 4),
                  type = rep(c("A", "B", NA, "A"), 4))
  p <- abm_plot_lattice(d, fill = type)
  # the warning, when there is one, comes from the draw stage, not the build
  draw <- function(p) ggplot2::ggplot_gtable(ggplot2::ggplot_build(p))

  # a scale that declines to colour NA drops those cells: silently
  blank <- p + ggplot2::scale_fill_manual(values = c(A = "red", B = "blue"),
                                          na.translate = FALSE)
  expect_no_warning(draw(blank))
  expect_equal(sum(is.na(ggplot2::ggplot_build(blank)$data[[1]]$fill)), 4L)

  # the default scale still draws them in its na.value
  expect_no_warning(draw(p))
  expect_false(anyNA(ggplot2::ggplot_build(p)$data[[1]]$fill))
})

test_that("abm_plot_lattice() refuses what it cannot draw", {
  skip_if_not_installed("ggplot2")
  m <- abm_setup(agents = abm_agents(n = 3, z = 0))
  r <- abm_run(m, abm_go(abm_rules(z ~ z + 1)), ticks = 1, seed = 1)
  expect_error(abm_plot_lattice(r, fill = z), class = "tidyABM_no_lattice")

  g <- abm_setup(agents = abm_grid(dims = c(3, 3), z = 0))
  rg <- abm_run(g, abm_go(abm_rules(z ~ z + 1)), ticks = 4, seed = 1, record = 2)
  expect_error(abm_plot_lattice(rg, fill = z, tick = 3), class = "tidyABM_bad_arg")
  expect_error(abm_plot_lattice(rg, fill = z, agents = "yes"), class = "tidyABM_bad_arg")
})
