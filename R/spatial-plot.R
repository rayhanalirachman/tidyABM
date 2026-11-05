# Spatial grammar: drawing a lattice --------------------------------------

#' Draw the state of a lattice
#'
#' A frame of a lattice model is a filter on the result tibble plus
#' `geom_raster(aes(.x, .y, fill = ...))`, and every spatial model's figure
#' starts the same way: pick the tick, keep the cells, fix the aspect ratio,
#' drop the axes a grid of cells does not need. `abm_plot_lattice()` is that
#' start. It returns an ordinary ggplot, so scales, labels, titles and
#' facets are added to it as usual.
#'
#' The wired group's rows are the cells; a row with a `.cell` is an agent
#' standing on one. With `agents = TRUE` those are drawn over the cells as
#' points coloured by `.group`.
#'
#' @param result An [abm_run()] result, or any data frame carrying `.x` and
#'   `.y` (for example several final frames bound together with a column
#'   saying which run each came from, to facet on).
#' @param fill What colours a cell: a column or an expression over the cell's
#'   columns, as in `ggplot2::aes()`. Leave it out to draw the cells in a
#'   single neutral grey, for example under points. A cell whose value is `NA`
#'   (an empty cell) is drawn in the scale's `na.value`; give the scale
#'   `na.translate = FALSE` to leave it blank instead, which emits no warning.
#' @param tick Which tick(s) to draw. The last recorded tick by default; several
#'   ticks are drawn as one facet each.
#' @param agents Whether to draw the mobile agents over the cells.
#'
#' @return A ggplot object. Needs the ggplot2 package.
#' @seealso [abm_network()] for the lattice itself; [abm_move()] for the
#'   agents standing on it.
#' @export
#' @examples
#' \dontrun{
#' life <- abm_setup(agents = abm_grid(dims = c(30, 30), alive = ~runif(n) < 0.3))
#' go <- abm_go(abm_neighbours(k ~ sum(alive)),
#'              abm_rules(alive ~ k == 3 | (alive & k == 2)))
#' r <- abm_run(life, go, ticks = 20, seed = 1, record = 10)
#' abm_plot_lattice(r, fill = alive, tick = c(0, 10, 20))
#' }
abm_plot_lattice <- function(result, fill = NULL, tick = NULL, agents = FALSE) {
  rlang::check_installed("ggplot2", reason = "to draw a lattice.")
  fill <- rlang::enquo(fill)
  if (!is.data.frame(result) || !all(c(".x", ".y") %in% names(result))) {
    abm_abort(
      c("{.arg result} has no lattice coordinates.",
        "i" = "It needs the {.code .x} and {.code .y} columns an {.fn abm_run} result carries for a {.code type = \"grid\"} network."),
      class = "tidyABM_no_lattice"
    )
  }
  if (!rlang::is_bool(agents)) {
    abm_abort("{.arg agents} must be {.code TRUE} or {.code FALSE}.",
              class = "tidyABM_bad_arg")
  }
  d <- result
  if ("tick" %in% names(d)) {
    if (is.null(tick)) tick <- max(d$tick)
    missing <- setdiff(tick, d$tick)
    if (length(missing)) {
      abm_abort(
        c("Tick{?s} {missing} {?was/were} not recorded.",
          "i" = "Recorded ticks: {.val {sort(unique(d$tick))}}. See {.arg record} in {.fn abm_run}."),
        class = "tidyABM_bad_arg"
      )
    }
    d <- vctrs::vec_slice(d, d$tick %in% tick)
  }
  mobile <- if (".cell" %in% names(d)) !is.na(d$.cell) else rep(FALSE, nrow(d))
  cells <- vctrs::vec_slice(d, !mobile)
  movers <- vctrs::vec_slice(d, mobile)

  p <- ggplot2::ggplot(cells, ggplot2::aes(x = .data$.x, y = .data$.y))
  if (rlang::quo_is_null(fill)) {
    p <- p + ggplot2::geom_raster(fill = "grey88")
  } else {
    # A cell whose fill the scale maps to NA (an empty cell under
    # `na.translate = FALSE`) is left blank, quietly: it is not a missing value
    # but a cell with nothing on it.
    p <- p + ggplot2::geom_raster(ggplot2::aes(fill = !!fill), na.rm = TRUE)
  }
  if (agents && nrow(movers)) {
    p <- p + ggplot2::geom_point(data = movers,
                                 ggplot2::aes(colour = .data$.group),
                                 size = 1.2)
  }
  if (length(tick) > 1L) {
    p <- p + ggplot2::facet_wrap(~tick, labeller = ggplot2::label_both)
  }
  w <- max(d$.x, na.rm = TRUE); h <- max(d$.y, na.rm = TRUE)
  p +
    ggplot2::coord_fixed(xlim = c(0.5, w + 0.5), ylim = c(0.5, h + 0.5),
                         expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "right",
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(margin = ggplot2::margin(b = 4)),
      plot.subtitle = ggplot2::element_text(margin = ggplot2::margin(b = 10)),
      strip.text = ggplot2::element_text(face = "bold",
                                         margin = ggplot2::margin(4, 0, 6, 0)),
      panel.spacing = ggplot2::unit(10, "pt"),
      legend.key = ggplot2::element_rect(colour = "grey75", fill = NA),
      plot.margin = ggplot2::margin(8, 8, 8, 8))
}
