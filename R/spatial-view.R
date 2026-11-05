# Spatial grammar: playing a recorded run back in the browser ---------------

#' Play a recorded lattice run back in the browser
#'
#' [abm_plot_lattice()] draws one frame; `abm_view_lattice()` writes every
#' recorded frame into a single HTML file that plays them back: a canvas of
#' the lattice, play / pause, a tick slider, a speed control, a legend with
#' the counts at the current tick, and a readout of whatever cell or agent
#' the pointer is over. The file is self-contained (its script, style and
#' data are inline; nothing is fetched), so it opens from disk, attaches to
#' an email and works on a phone. It follows the browser's light or dark
#' theme and has a switch of its own.
#'
#' The frames are the ticks [abm_run()] recorded (`record = "all"` or a
#' number), so the file grows with the lattice, the number of frames and the
#' number of agents. Cell values are quantised to at most 64 levels and
#' run-length encoded, and an agent costs a few bytes a tick, so a 50 x 50
#' lattice over 200 frames is well under a megabyte. Thin a long run with
#' `record =` or `tick =` rather than recording every tick of it.
#'
#' @inheritParams abm_plot_lattice
#' @param fill What colours a cell: a column or an expression over the cell's
#'   columns. A logical, character or factor value gets one colour per level;
#'   a number gets a gradient (quantised to 64 steps). `NULL` draws the cells
#'   in a neutral grey, for agents to stand on.
#' @param file Where to write the HTML. `NULL` writes to a temporary file. The
#'   path is returned invisibly either way.
#' @param colour What colours a mobile agent: a column or an expression over
#'   the agent's columns. `.group` by default.
#' @param mark Cells to mark over the fill: a character or factor expression
#'   over the cell's columns whose levels are the marks and whose `NA` is no
#'   mark, for example `case_when(nest ~ "nest", food > 0 ~ "food")`.
#' @param hover Names of further columns to show in the readout: a cell's
#'   columns are stored as quantised layers, an agent's as its values at each
#'   tick. Keep it to a few; each one costs as much as the fill does.
#' @param tick Which ticks to play. Every recorded tick by default.
#' @param title,subtitle The page's heading. The title defaults to the
#'   `fill` expression.
#' @param labels What to call the `fill`, `colour` and `mark` values in the
#'   legend and the readout, as a character vector named by those three: for
#'   example `c(fill = "grass height")` when `fill` is an expression.
#' @param fill_colours,agent_colours,mark_colours The colours of the fill,
#'   the agents and the marks, as `colours` in [abm_plot_lattice()]: a vector
#'   named by level, or the stops of a gradient for a numeric value. `NULL`
#'   picks Okabe-Ito colours, or a light-to-blue gradient.
#' @param fps Frames per second at speed 1.
#'
#' @return The path of the file, invisibly.
#' @seealso [abm_plot_lattice()] for a static frame of the same result.
#' @export
#' @examples
#' \dontrun{
#' life <- abm_setup(agents = abm_grid(dims = c(30, 30), alive = ~runif(n) < 0.3))
#' go <- abm_go(abm_neighbours(k ~ sum(alive)),
#'              abm_rules(alive ~ k == 3 | (alive & k == 2)))
#' r <- abm_run(life, go, ticks = 100, seed = 1)
#' f <- abm_view_lattice(r, fill = alive, title = "Game of Life")
#' browseURL(f)
#' }
abm_view_lattice <- function(result, fill = NULL, file = NULL, agents = FALSE,
                             colour = NULL, mark = NULL, hover = NULL,
                             tick = NULL, title = NULL, subtitle = NULL,
                             labels = NULL, fill_colours = NULL,
                             agent_colours = NULL, mark_colours = NULL,
                             fps = 8) {
  fill <- rlang::enquo(fill)
  colour <- rlang::enquo(colour)
  mark <- rlang::enquo(mark)
  frames <- lattice_frames(result, tick, all = TRUE)
  if (!is.null(labels) &&
      (!is.character(labels) || is.null(names(labels)) ||
       !all(names(labels) %in% c("fill", "colour", "mark")))) {
    abm_abort("{.arg labels} must be a character vector named by {.val fill}, {.val colour} or {.val mark}.",
              class = "tidyABM_bad_arg")
  }
  label_of <- function(what, quo) {
    if (what %in% names(labels)) labels[[what]] else rlang::as_label(quo)
  }
  if (!rlang::is_bool(agents)) {
    abm_abort("{.arg agents} must be {.code TRUE} or {.code FALSE}.",
              class = "tidyABM_bad_arg")
  }
  if (!is.null(hover) && !is.character(hover)) {
    abm_abort("{.arg hover} must be a character vector of column names.",
              class = "tidyABM_bad_arg")
  }
  if (!is.null(file) && !rlang::is_string(file)) {
    abm_abort("{.arg file} must be a single path, or {.code NULL}.",
              class = "tidyABM_bad_arg")
  }
  if (!rlang::is_scalar_double(fps) && !rlang::is_scalar_integer(fps) ||
      is.na(fps) || fps <= 0) {
    abm_abort("{.arg fps} must be a single positive number.",
              class = "tidyABM_bad_arg")
  }
  if (".run" %in% names(result) && length(unique(result$.run)) > 1L) {
    abm_abort(
      c("{.arg result} holds {length(unique(result$.run))} runs.",
        "i" = "A viewer plays one run back; filter {.code .run} first."),
      class = "tidyABM_bad_arg"
    )
  }
  cells <- frames$cells
  movers <- frames$movers
  if (!nrow(cells)) {
    abm_abort(
      c("{.arg result} has no recorded frames to play.",
        "i" = "Record the population with {.code record = \"all\"} or a number in {.fn abm_run}; {.code \"globals\"} keeps none of it."),
      class = "tidyABM_bad_arg"
    )
  }
  ticks <- frames$tick
  if (!length(ticks)) {               # a bare data frame: one frame
    ticks <- 0L
    cells$tick <- 0L
    if (nrow(movers)) movers$tick <- 0L
  }
  w <- frames$w; h <- frames$h
  unknown <- setdiff(hover, union(names(cells), names(movers)))
  if (length(unknown)) {
    abm_abort("{.arg hover} names column{?s} {.val {unknown}}, which {?is/are} not in {.arg result}.",
              class = "tidyABM_bad_arg")
  }

  # the cells: each layer is one run-length string per tick ------------------
  cell_layers <- list()
  add_cell_layer <- function(values, role, name, colours) {
    enc <- encode_values(values, colours, name = name)
    idx <- (cells$.x - 1L) + (cells$.y - 1L) * w
    by_tick <- split(seq_len(nrow(cells)), factor(cells$tick, levels = ticks))
    strings <- vapply(by_tick, function(i) {
      frame <- rep(NA_integer_, w * h)
      frame[idx[i] + 1L] <- enc$codes[i]
      rle_encode(frame)
    }, "")
    cell_layers[[length(cell_layers) + 1L]] <<-
      list(spec = c(enc$spec, list(role = role)), frames = unname(as.list(strings)))
  }
  if (!rlang::quo_is_null(fill)) {
    add_cell_layer(eval_over(fill, cells, "fill"), "fill", label_of("fill", fill),
                   fill_colours)
  }
  if (!rlang::quo_is_null(mark)) {
    marks <- eval_over(mark, cells, "mark")
    if (is.numeric(marks) || is.logical(marks)) {
      abm_abort(
        c("{.arg mark} must name the marks: a character or factor value, {.code NA} where there is no mark.",
          "x" = "Got {.cls {class(marks)}}."),
        class = "tidyABM_bad_arg"
      )
    }
    # marks sit over a fill, so by default they skip the blue a fill starts on
    add_cell_layer(marks, "mark", label_of("mark", mark), mark_colours %||% okabe_ito[-1])
  }
  for (col in intersect(hover, names(cells))) {
    if (all(is.na(cells[[col]]))) next          # an agent column, not a cell's
    add_cell_layer(cells[[col]], "hover", col, NULL)
  }

  # the agents: a flat row per agent per tick ---------------------------------
  agent_data <- NULL
  if (agents && nrow(movers)) {
    by_group <- rlang::quo_is_null(colour)
    if (by_group) colour <- rlang::quo(.data$.group)
    group <- encode_values(movers$.group, NULL, name = ".group")
    col <- encode_values(eval_over(colour, movers, "colour"), agent_colours,
                         name = if (by_group) ".group" else label_of("colour", colour))
    hover_cols <- intersect(hover, names(movers))
    hover_cols <- hover_cols[vapply(hover_cols, function(x) !all(is.na(movers[[x]])), TRUE)]
    hov <- lapply(hover_cols, function(x) encode_values(movers[[x]], NULL, name = x,
                                                        exact = TRUE))
    m <- cbind((movers$.x - 1L) + (movers$.y - 1L) * w, movers$.id,
               group$codes, col$codes)
    for (hv in hov) m <- cbind(m, hv$codes)
    by_tick <- split(seq_len(nrow(movers)), factor(movers$tick, levels = ticks))
    agent_data <- list(
      groups = group$spec$levels,
      colour = col$spec,
      hover = lapply(hov, function(hv) hv$spec),
      stride = ncol(m),
      frames = unname(lapply(by_tick, function(i) as.vector(t(m[i, , drop = FALSE]))))
    )
  }

  data <- list(
    title = title %||% if (rlang::quo_is_null(fill)) "A lattice run" else label_of("fill", fill),
    subtitle = subtitle,
    w = w, h = h, ticks = as.list(ticks), fps = fps,
    layers = lapply(cell_layers, function(l) l$spec),
    frames = lapply(seq_along(ticks), function(t) {
      lapply(cell_layers, function(l) l$frames[[t]])
    }),
    agents = agent_data
  )
  file <- file %||% tempfile(fileext = ".html")
  template <- readLines(system.file("viewer", "lattice.html", package = "tidyABM"),
                        encoding = "UTF-8", warn = FALSE)
  html <- paste(template, collapse = "\n")
  html <- splice(html, "/*__DATA__*/null", to_json(data))
  html <- splice(html, "__TITLE__", html_escape(data$title))
  writeLines(html, file, useBytes = TRUE)
  invisible(file)
}

#' Evaluate a `fill`-like quosure over the rows of a frame
#' @noRd
eval_over <- function(quo, rows, arg, call = rlang::caller_env()) {
  values <- rlang::eval_tidy(quo, rows)
  if (length(values) == 1L) values <- rep_len(values, nrow(rows))
  if (length(values) != nrow(rows)) {
    abm_abort(
      "{.arg {arg}} must give one value per row: got {length(values)} for {nrow(rows)} rows.",
      class = "tidyABM_bad_arg", call = call
    )
  }
  values
}

#' Encode a column as small integer codes plus the key to read them back
#'
#' A logical, character or factor value gets one code per level, in level
#' order; a number is quantised onto at most 64 steps from its minimum (an
#' exact step of 1 when the values are whole and the range allows). With
#' `exact = TRUE` a number is kept as itself, rounded to four significant
#' figures, which is what an agent's readout wants.
#'
#' @return A list: `spec` (what the viewer needs to decode and colour the
#'   codes) and `codes` (an integer, or with `exact = TRUE` a double, per
#'   value; `NA` stays `NA`).
#' @noRd
encode_values <- function(values, colours = NULL, name, exact = FALSE,
                          call = rlang::caller_env()) {
  if (is.logical(values) || is.character(values) || is.factor(values)) {
    levels <- if (is.factor(values)) levels(values) else sort(unique(values[!is.na(values)]))
    levels <- as.character(levels)
    if (length(levels) > 64L) {
      abm_abort(
        c("{.code {name}} has {length(levels)} levels; the viewer draws at most 64.",
          "i" = "Group them, or draw a number instead."),
        class = "tidyABM_bad_arg", call = call
      )
    }
    cols <- css_colours(level_colours(levels, colours, call = call))
    return(list(
      spec = list(name = name, type = "discrete", levels = as.list(levels),
                  colours = as.list(unname(cols))),
      codes = match(as.character(values), levels) - 1L
    ))
  }
  if (!is.numeric(values)) {
    abm_abort(
      c("{.code {name}} is {.cls {class(values)}}, which the viewer cannot draw.",
        "i" = "It draws logical, character, factor and numeric values."),
      class = "tidyABM_bad_arg", call = call
    )
  }
  if (exact) {
    v <- signif(as.double(values), 4L)
    v[!is.finite(v)] <- NA_real_
    return(list(spec = list(name = name, type = "number"), codes = v))
  }
  finite <- values[is.finite(values)]
  rng <- if (length(finite)) range(finite) else c(0, 0)
  whole <- length(finite) && all(finite == round(finite)) && diff(rng) <= 63
  step <- if (whole) 1 else if (diff(rng) > 0) diff(rng) / 63 else 1
  codes <- as.integer(round((values - rng[[1]]) / step))
  codes[!is.finite(values)] <- NA_integer_
  if (!is.null(colours) && !is.character(colours)) {
    abm_abort("{.arg colours} must be a character vector of colours.",
              class = "tidyABM_bad_arg", call = call)
  }
  stops <- css_colours(colours %||% c("#F7FBFF", "#0072B2"))
  if (length(stops) < 2L) stops <- c("#F7FBFF", stops)
  list(
    spec = list(name = name, type = "continuous", min = rng[[1]], max = rng[[2]],
                step = step, colours = as.list(stops)),
    codes = codes
  )
}

#' Colours as the browser reads them
#'
#' A hex code or a CSS colour name passes through. R's grey levels
#' (`grey10`, `gray85`) are not CSS, and they are the one R-only spelling a
#' lattice figure habitually uses, so they are turned into hex here; any
#' other name the browser cannot read is drawn mid-grey by the viewer.
#' @noRd
css_colours <- function(x) {
  grey <- which(grepl("^gr[ae]y[0-9]{1,3}$", x))
  if (length(grey)) {
    level <- as.integer(sub("^gr[ae]y", "", x[grey]))
    grey <- grey[level <= 100L]; level <- level[level <= 100L]
    level <- round(level * 255 / 100)
    x[grey] <- sprintf("#%02X%02X%02X", level, level, level)
  }
  x
}

#' The 64 symbols a cell code is written as, and the run-length writer
#'
#' One printable character per level, none of them a digit, a quote, a
#' backslash, a slash or an angle bracket, so a frame can sit inside a JSON
#' string inside a `<script>` without escaping. `NA` is `.`. A run of a
#' symbol is the symbol followed by its length in decimal, which is why the
#' alphabet has no digits.
#' @noRd
code_symbols <- strsplit(
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz!#$%&()*+,-:", ""
)[[1]]

rle_encode <- function(codes) {
  sym <- code_symbols[codes + 1L]
  sym[is.na(codes)] <- "."
  r <- rle(sym)
  paste0(r$values, ifelse(r$lengths > 1L, r$lengths, ""), collapse = "")
}

#' A small JSON writer: enough for the viewer's data
#'
#' A named list is an object, an unnamed list an array, an atomic vector of
#' length one a scalar and any other atomic vector an array (so a one-element
#' array is written as `as.list(x)`). `NA`, `NaN` and infinities are `null`.
#' @noRd
to_json <- function(x) {
  if (is.null(x)) return("null")
  if (is.list(x)) {
    inner <- vapply(x, to_json, "", USE.NAMES = FALSE)
    if (!is.null(names(x)) && length(x)) {
      return(paste0("{", paste0(json_string(names(x)), ":", inner, collapse = ","), "}"))
    }
    return(paste0("[", paste0(inner, collapse = ","), "]"))
  }
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) {
    s <- json_string(x)
  } else if (is.logical(x)) {
    s <- ifelse(x, "true", "false")
  } else if (is.numeric(x)) {
    s <- formatC(x, digits = 15L, format = "g", width = 1L)
    s <- sub("^\\s+", "", s)
  } else {
    abm_abort("Cannot write {.cls {class(x)}} as JSON.", class = "tidyABM_internal",
              .internal = TRUE)
  }
  s[is.na(x) | (is.numeric(x) & !is.finite(x))] <- "null"
  if (length(x) == 1L) return(s)
  paste0("[", paste0(s, collapse = ","), "]")
}

json_string <- function(x) {
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub("\"", "\\\"", x, fixed = TRUE)
  x <- gsub("\n", "\\n", x, fixed = TRUE)
  x <- gsub("\r", "\\r", x, fixed = TRUE)
  x <- gsub("\t", "\\t", x, fixed = TRUE)
  x <- gsub("[[:cntrl:]]", " ", x)
  x <- gsub("</", "<\\/", x, fixed = TRUE)       # never close the <script>
  paste0("\"", x, "\"")
}

#' Put `value` where `marker` sits in the template, exactly once, verbatim
#' (`sub()` would read backslashes in the value as escapes)
#' @noRd
splice <- function(html, marker, value) {
  parts <- strsplit(html, marker, fixed = TRUE)[[1]]
  if (length(parts) != 2L) {
    abm_abort("The viewer template is damaged: {.val {marker}} is not in it once.",
              class = "tidyABM_internal", .internal = TRUE)
  }
  paste0(parts[[1]], value, parts[[2]])
}

html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}
