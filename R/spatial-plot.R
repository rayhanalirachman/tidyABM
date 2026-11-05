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
#' points, coloured by `.group` unless `colour` says otherwise, in the
#' Okabe-Ito palette (colour-blind safe) unless `colours` says otherwise.
#' The point layer comes with its colour scale, so to change the agents'
#' colours pass `colours` rather than adding a second `scale_colour_*()`,
#' which would replace it with a message.
#'
#' Several ticks are drawn as one facet each, in a single row when there are
#' six or fewer: a strip showing how the world evolved.
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
#' @param colour What colours a mobile agent: a column or an expression over
#'   the agent's columns. `.group` by default. A discrete value gets one
#'   colour per level; a numeric one gets a gradient.
#' @param shape What shapes a mobile agent: a column or an expression over the
#'   agent's columns, or `NULL` for a filled circle.
#' @param size The point size of a mobile agent, in ggplot2's units.
#' @param colours The agents' colours: for a discrete `colour`, a vector of
#'   colours named by level (an unnamed one is taken in level order); for a
#'   numeric `colour`, the two ends of the gradient. `NULL` picks Okabe-Ito
#'   colours, or a light-to-dark blue gradient.
#'
#' @return A ggplot object. Needs the ggplot2 package.
#' @seealso [abm_view_lattice()] for the same frames as an interactive
#'   playback; [abm_network()] for the lattice itself; [abm_move()] for the
#'   agents standing on it.
#' @export
#' @examplesIf rlang::is_installed("ggplot2")
#' life <- abm_setup(agents = abm_grid(dims = c(30, 30), alive = ~runif(n) < 0.3))
#' go <- abm_go(abm_neighbours(k ~ sum(alive)),
#'              abm_rules(alive ~ k == 3 | (alive & k == 2)))
#' r <- abm_run(life, go, ticks = 20, seed = 1, record = 10)
#' abm_plot_lattice(r, fill = alive, tick = c(0, 10, 20))
abm_plot_lattice <- function(result, fill = NULL, tick = NULL, agents = FALSE,
                             colour = NULL, shape = NULL, size = 1.2,
                             colours = NULL) {
  rlang::check_installed("ggplot2", reason = "to draw a lattice.")
  fill <- rlang::enquo(fill)
  colour <- rlang::enquo(colour)
  shape <- rlang::enquo(shape)
  frames <- lattice_frames(result, tick)
  if (!rlang::is_bool(agents)) {
    abm_abort("{.arg agents} must be {.code TRUE} or {.code FALSE}.",
              class = "tidyABM_bad_arg")
  }
  if (!rlang::is_scalar_double(size) && !rlang::is_scalar_integer(size) ||
      is.na(size) || size < 0) {
    abm_abort("{.arg size} must be a single non-negative number.",
              class = "tidyABM_bad_arg")
  }
  cells <- frames$cells
  movers <- frames$movers

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
    by_group <- rlang::quo_is_null(colour)
    if (by_group) colour <- rlang::quo(.data$.group)
    aes <- ggplot2::aes(colour = !!colour)
    if (!rlang::quo_is_null(shape)) aes$shape <- shape
    p <- p + ggplot2::geom_point(data = movers, aes, size = size)
    # The scale carries no name of its own, so `labs(colour = ...)` sets or
    # drops the legend's title as it would for any ggplot.
    values <- rlang::eval_tidy(colour, movers)
    if (is.numeric(values)) {
      ends <- colours %||% c("#DEEBF7", "#08306B")
      p <- p + ggplot2::scale_colour_gradient(low = ends[[1]],
                                              high = ends[[length(ends)]])
    } else {
      levels <- if (is.factor(values)) levels(values) else sort(unique(values))
      levels <- as.character(levels)
      p <- p + ggplot2::scale_colour_manual(
        values = level_colours(levels, colours), limits = levels,
        na.value = "grey50")
    }
    if (by_group) p <- p + ggplot2::labs(colour = NULL)
  }
  if (length(frames$tick) > 1L) {
    p <- p + ggplot2::facet_wrap(~tick, labeller = ggplot2::label_both,
                                 nrow = if (length(frames$tick) <= 6L) 1L)
  }
  p +
    ggplot2::coord_fixed(xlim = c(0.5, frames$w + 0.5),
                         ylim = c(0.5, frames$h + 0.5), expand = FALSE) +
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

#' The frames of a lattice result: its cells and its mobile agents, by tick
#'
#' Shared by [abm_plot_lattice()] and [abm_view_lattice()]. Checks that the
#' result has coordinates and that every requested tick was recorded, then
#' splits the rows into the cells (no `.cell`) and the agents standing on
#' them.
#'
#' @param tick The ticks wanted; `NULL` means the last one for a plot and
#'   every recorded one for a viewer (`all = TRUE`).
#' @return A list: `cells`, `movers`, `tick` (the ticks kept, sorted), `w`
#'   and `h`.
#' @noRd
lattice_frames <- function(result, tick = NULL, all = FALSE,
                           call = rlang::caller_env()) {
  if (!is.data.frame(result) || !all(c(".x", ".y") %in% names(result))) {
    abm_abort(
      c("{.arg result} has no lattice coordinates.",
        "i" = "It needs the {.code .x} and {.code .y} columns an {.fn abm_run} result carries for a {.code type = \"grid\"} network."),
      class = "tidyABM_no_lattice", call = call
    )
  }
  d <- result
  if ("tick" %in% names(d)) {
    if (is.null(tick)) tick <- if (all) sort(unique(d$tick)) else max(d$tick)
    missing <- setdiff(tick, d$tick)
    if (length(missing)) {
      abm_abort(
        c("Tick{?s} {missing} {?was/were} not recorded.",
          "i" = "Recorded ticks: {.val {sort(unique(d$tick))}}. See {.arg record} in {.fn abm_run}."),
        class = "tidyABM_bad_arg", call = call
      )
    }
    d <- vctrs::vec_slice(d, d$tick %in% tick)
    tick <- sort(unique(tick))
  } else {
    tick <- integer()
  }
  mobile <- if (".cell" %in% names(d)) !is.na(d$.cell) else rep(FALSE, nrow(d))
  list(cells = vctrs::vec_slice(d, !mobile),
       movers = vctrs::vec_slice(d, mobile),
       tick = tick,
       w = if (nrow(d)) max(d$.x, na.rm = TRUE) else NA_integer_,
       h = if (nrow(d)) max(d$.y, na.rm = TRUE) else NA_integer_)
}

#' The Okabe-Ito palette, colour-blind safe
#' @noRd
okabe_ito <- c("#0072B2", "#E69F00", "#009E73", "#D55E00", "#CC79A7",
               "#56B4E9", "#F0E442", "#000000")

#' One colour per level: the user's, by name or in order, or Okabe-Ito
#' @noRd
level_colours <- function(levels, colours = NULL, call = rlang::caller_env()) {
  levels <- as.character(levels)
  if (is.null(colours)) {
    return(stats::setNames(rep_len(okabe_ito, length(levels)), levels))
  }
  if (!is.character(colours)) {
    abm_abort("{.arg colours} must be a character vector of colours.",
              class = "tidyABM_bad_arg", call = call)
  }
  if (is.null(names(colours))) {
    return(stats::setNames(rep_len(colours, length(levels)), levels))
  }
  out <- stats::setNames(rep_len(okabe_ito, length(levels)), levels)
  known <- intersect(names(colours), levels)
  out[known] <- colours[known]
  out
}
