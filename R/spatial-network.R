# Spatial grammar: the lattice as a network type -----------------------
#
# A lattice *is a network*. `abm_network(type = "grid" | "line")` produces the
# same from/to edge tibble every other network type produces, so patches are
# ordinary agents and everything that already reads a network works on a grid
# with no change. This file holds every piece of the spatial grammar that is
# not the `abm_move()` step (that is in spatial-move.R); the core engine files
# only carry thin hooks that delegate here.
#
# Tiers (see `models/spatial/grammar-spec.md`):
#   L0  the lattice network type, `.x`/`.y` injection, count inheritance,
#       `abm_grid()` sugar
#   L1  `abm_neighbours(..., .where = "north")` -- one named lattice neighbour
#   L2  `.cell` on mobile groups, `at =` placement, the `within = .id == own_.cell`
#       equijoin fast path, `abm_match(.by = <col>)`
#   L2+ extra `abm_move()` arguments, added per model

# --- lattice metadata -------------------------------------------------------

#' Is this network spec a lattice?
#' @noRd
is_lattice_spec <- function(spec) {
  !is.null(spec) && inherits(spec, "abm_network") &&
    !is.null(spec$type) && spec$type %in% c("grid", "line")
}

#' Validate the lattice arguments of `abm_network()` and return a spec
#'
#' Called from [abm_network()] when `type` is `"grid"` or `"line"`. Keeps all
#' of the lattice-specific checking out of the core constructor.
#' @noRd
lattice_network_spec <- function(type, dims, diagonals, torus, on,
                                 call = rlang::caller_env()) {
  if (is.null(dims) || !is.numeric(dims) || anyNA(dims) ||
      any(dims != as.integer(dims)) || any(dims < 1)) {
    abm_abort(
      c("{.arg dims} must be positive whole numbers.",
        "i" = if (type == "grid") '{.code dims = c(width, height)} for a grid.'
              else '{.code dims = width} for a line.'),
      class = "tidyABM_bad_dims", call = call
    )
  }
  dims <- as.integer(dims)
  if (type == "grid" && length(dims) != 2L) {
    abm_abort(c('A grid needs {.code dims = c(width, height)}.',
                "x" = "Got {length(dims)} value{?s}."),
              class = "tidyABM_bad_dims", call = call)
  }
  if (type == "line" && length(dims) != 1L) {
    abm_abort(c('A line needs {.code dims = width}.',
                "x" = "Got {length(dims)} value{?s}."),
              class = "tidyABM_bad_dims", call = call)
  }
  if (type == "line" && !is.null(diagonals)) {
    abm_abort(
      c('{.arg diagonals} is a grid idea; a line has none.',
        "i" = 'Drop it, or use {.code type = "grid"}.'),
      class = "tidyABM_irrelevant_arg", call = call
    )
  }
  if (!is.null(diagonals) && !rlang::is_bool(diagonals)) {
    abm_abort("{.arg diagonals} must be {.code TRUE} or {.code FALSE}.",
              class = "tidyABM_bad_arg", call = call)
  }
  if (!is.null(torus) && !rlang::is_bool(torus)) {
    abm_abort("{.arg torus} must be {.code TRUE} or {.code FALSE}.",
              class = "tidyABM_bad_arg", call = call)
  }
  if (!is.null(on) && (!rlang::is_string(on))) {
    abm_abort("{.arg on} must be a single group name.",
              class = "tidyABM_bad_arg", call = call)
  }
  new_abm_network(
    type, degree = NULL, edges = NULL,
    dims = dims,
    diagonals = if (type == "grid") diagonals %||% TRUE else NULL,
    torus = torus %||% TRUE,
    on = on
  )
}

#' Turn a lattice spec into its coordinate/neighbour metadata
#'
#' Convention: 1-based, row-major, `.id = .x + (.y - 1) * w`, `.y` increases
#' upward. A line gives `.x` only. The returned list travels on the model and
#' the run state as `$lattice`.
#' @noRd
build_lattice <- function(spec, group_name) {
  type <- spec$type
  dims <- spec$dims
  torus <- spec$torus %||% TRUE
  diagonals <- if (type == "grid") spec$diagonals %||% TRUE else FALSE

  if (type == "line") {
    w <- dims[[1]]
    ncell <- w
    x <- seq_len(w)
    y <- NULL
    west <- shift_line(x, -1L, w, torus)
    east <- shift_line(x, 1L, w, torus)
    dir_nb <- list(west = west, east = east,
                   left = west, right = east)
    nb <- lapply(seq_len(ncell), function(i) {
      c(west[[i]], east[[i]])[!is.na(c(west[[i]], east[[i]]))]
    })
  } else {
    w <- dims[[1]]; h <- dims[[2]]
    ncell <- w * h
    x <- rep(seq_len(w), times = h)
    y <- rep(seq_len(h), each = w)
    north <- grid_shift(x, y, 0L,  1L, w, h, torus)
    south <- grid_shift(x, y, 0L, -1L, w, h, torus)
    east  <- grid_shift(x, y, 1L,  0L, w, h, torus)
    west  <- grid_shift(x, y, -1L, 0L, w, h, torus)
    dir_nb <- list(north = north, south = south, east = east, west = west)
    offs <- list(c(0L, 1L), c(0L, -1L), c(1L, 0L), c(-1L, 0L))
    if (diagonals) {
      offs <- c(offs, list(c(1L, 1L), c(1L, -1L), c(-1L, 1L), c(-1L, -1L)))
    }
    cols <- lapply(offs, function(o) grid_shift(x, y, o[[1]], o[[2]], w, h, torus))
    nb <- lapply(seq_len(ncell), function(i) {
      v <- vapply(cols, function(cc) cc[[i]], integer(1))
      v[!is.na(v)]
    })
  }

  list(
    type = type, dims = dims, torus = torus, diagonals = diagonals,
    on = group_name, ncell = ncell,
    x = as.integer(x), y = if (is.null(y)) NULL else as.integer(y),
    nb = nb, dir_nb = dir_nb
  )
}

#' 1-D neighbour index in direction `d` (+/-1), NA past a bounded edge
#' @noRd
shift_line <- function(x, d, w, torus) {
  nx <- x + d
  if (torus) return(as.integer(((nx - 1L) %% w) + 1L))
  nx[nx < 1L | nx > w] <- NA_integer_
  as.integer(nx)
}

#' Grid neighbour cell id after moving (dx, dy), NA past a bounded edge
#' @noRd
grid_shift <- function(x, y, dx, dy, w, h, torus) {
  nx <- x + dx; ny <- y + dy
  if (torus) {
    nx <- ((nx - 1L) %% w) + 1L
    ny <- ((ny - 1L) %% h) + 1L
    return(as.integer(nx + (ny - 1L) * w))
  }
  ok <- nx >= 1L & nx <= w & ny >= 1L & ny <= h
  out <- rep(NA_integer_, length(x))
  out[ok] <- as.integer(nx[ok] + (ny[ok] - 1L) * w)
  out
}

#' The lattice as a from/to edge tibble (undirected, each edge once)
#' @noRd
lattice_edges <- function(lat) {
  from <- rep(seq_len(lat$ncell), lengths(lat$nb))
  to <- unlist(lat$nb, use.names = FALSE)
  if (!length(to)) return(tibble::tibble(from = integer(), to = integer()))
  keep <- from < to
  tibble::tibble(from = as.integer(from[keep]), to = as.integer(to[keep]))
}

# --- setup: build the lattice before the agent columns --------------------

#' Set up a model whose network is a lattice
#'
#' Mirrors the tail of [abm_setup()], but the lattice is materialised *first*
#' so `.x` / `.y` are in scope in the wired group's column formulas, and the
#' wired group inherits its count from `prod(dims)`.
#' @noRd
setup_lattice <- function(specs, network, globals, call = rlang::caller_env()) {
  on <- network$on
  if (is.null(on)) {
    if (length(specs) != 1L) {
      abm_abort(
        c("A lattice network needs {.arg on} to say which group to wire.",
          "i" = 'For example {.code abm_network(type = "grid", dims = ..., on = "patches")}.'),
        class = "tidyABM_missing_arg", call = call
      )
    }
    on <- names(specs)[[1]]
  }
  if (!on %in% names(specs)) {
    abm_abort(
      c("{.arg on} names a group that is not in {.arg agents}.",
        "x" = "No group {.field {on}}.",
        "i" = "Groups: {.field {names(specs)}}."),
      class = "tidyABM_bad_arg", call = call
    )
  }

  lat <- build_lattice(network, on)

  wired_spec <- specs[[on]]
  if (!is.null(wired_spec$n) && wired_spec$n != lat$ncell) {
    abm_abort(
      c("The wired group's {.arg n} does not match the lattice.",
        "x" = "{.code n = {wired_spec$n}} but {.code prod(dims) = {lat$ncell}}.",
        "i" = "Omit {.arg n} for a grid-wired group; it is inherited."),
      class = "tidyABM_bad_n", call = call
    )
  }
  if (!is.null(wired_spec$at)) {
    abm_abort(
      c("{.arg at} places mobile agents on the lattice; the wired group *is* the lattice.",
        "i" = "Drop {.arg at} from group {.field {on}}."),
      class = "tidyABM_irrelevant_arg", call = call
    )
  }

  # wired group first, always, so cell ids are 1..prod(dims) == the group's .id
  wired <- materialise_lattice_group(wired_spec, on, lat)
  groups <- list()
  groups[[on]] <- wired
  offset <- lat$ncell

  for (nm in setdiff(names(specs), on)) {
    s <- specs[[nm]]
    if (is.null(s$n)) {
      abm_abort(
        c("Group {.field {nm}} needs an {.arg n}.",
          "i" = "Only the grid-wired group ({.field {on}}) inherits its count."),
        class = "tidyABM_bad_n", call = call
      )
    }
    g <- materialise_agents(s, nm, id_offset = offset)
    g <- place_on_lattice(g, s, nm, lat, wired, call)
    groups[[nm]] <- g
    offset <- offset + s$n
  }

  edges <- lattice_edges(lat)
  model <- new_abm_model(groups, as.list(globals), edges, network)
  model$lattice <- lat
  model
}

#' Materialise the wired group, with `.x` / `.y` visible to its formulas
#' @noRd
materialise_lattice_group <- function(spec, group_name, lat) {
  n <- lat$ncell
  out <- tibble::tibble(.id = seq_len(n))
  out$.x <- lat$x
  if (!is.null(lat$y)) out$.y <- lat$y
  mask_extra <- list(n = n, dims = lat$dims)

  for (nm in names(spec$cols)) {
    val <- spec$cols[[nm]]
    if (is_formula1(val)) {
      value <- rlang::eval_tidy(rlang::as_quosure(val),
                                data = c(as.list(out), mask_extra))
    } else {
      value <- val
    }
    if (length(value) == 1L) {
      value <- rep(value, n)
    } else if (length(value) != n) {
      abm_abort(
        c("Column {.field {nm}} has the wrong length.",
          "x" = "It returned {length(value)} value{?s}, but the lattice has {n} cell{?s}."),
        class = "tidyABM_bad_column_length"
      )
    }
    out[[nm]] <- value
  }
  out$.group <- group_name
  dplyr::relocate(out, ".id", ".group")
}

#' Give a mobile group its `.cell` (and the `.x` / `.y` that mirror it)
#' @noRd
place_on_lattice <- function(g, spec, group_name, lat, wired,
                             call = rlang::caller_env()) {
  n <- nrow(g)
  if (!is.null(spec$at)) {
    # evaluated once, like a column formula: sees the group's own columns, `n`,
    # `dims`, and the wired group's columns (so `at = ~which(nest)[1]` works).
    mask <- c(as.list(wired), list(n = n, dims = lat$dims), as.list(g))
    # both sides carry `.id` and `.group`; the moving group's win, so `at` reads
    # as a column formula of *this* group that happens to see the cells too
    mask <- mask[!duplicated(names(mask), fromLast = TRUE)]
    cell <- rlang::eval_tidy(spec$at, data = mask)
    if (length(cell) == 1L) cell <- rep(cell, n)
    if (length(cell) != n) {
      abm_abort(
        c("{.arg at} for group {.field {group_name}} has the wrong length.",
          "x" = "It returned {length(cell)} value{?s}, but there {?is/are} {n} agent{?s}."),
        class = "tidyABM_bad_column_length", call = call
      )
    }
  } else {
    cell <- sample.int(lat$ncell, n, replace = TRUE)
  }
  cell <- as.integer(cell)
  if (anyNA(cell) || any(cell < 1L | cell > lat$ncell)) {
    abm_abort(
      c("{.arg at} for group {.field {group_name}} must land on a cell.",
        "x" = "A value is missing or outside {.code 1:{lat$ncell}}."),
      class = "tidyABM_bad_arg", call = call
    )
  }
  g$.cell <- cell
  g$.x <- lat$x[cell]
  if (!is.null(lat$y)) g$.y <- lat$y[cell]
  g
}

#' Re-derive `.x` / `.y` from `.cell` for every group that has one
#'
#' Called after a step that can move an agent, so `.x` / `.y` always mirror the
#' current `.cell`.
#' @noRd
sync_cell_coords <- function(state) {
  lat <- state$lattice
  if (is.null(lat)) return(state)
  for (nm in names(state$groups)) {
    g <- state$groups[[nm]]
    if (!".cell" %in% names(g) || !nrow(g)) next
    g$.x <- lat$x[g$.cell]
    if (!is.null(lat$y)) g$.y <- lat$y[g$.cell]
    state$groups[[nm]] <- g
  }
  state
}

# --- abm_grid() sugar ----------------------------------------------------

#' A grid of patches, as a single declaration
#'
#' `abm_grid()` is sugar for a patch-heavy model. It desugars to an
#' [abm_agents()] group plus an [abm_network()] of `type = "grid"` wired to it,
#' so the [abm_go()] block is identical either way. Use the explicit two-line
#' form instead when the network should be visible in the setup, or when another
#' network sits alongside it.
#'
#' ```r
#' patches <- abm_grid(dims = c(w, h), diagonals = FALSE, torus = FALSE,
#'                     state = ~sample(c("tree", "empty"), n, TRUE))
#' ```
#'
#' desugars to
#'
#' ```r
#' agents  = list(patches = abm_agents(state = ~...)),
#' network = abm_network(type = "grid", dims = c(w, h),
#'                       diagonals = FALSE, torus = FALSE, on = "patches")
#' ```
#'
#' @param dims `c(width, height)`. The cell count is `prod(dims)`, and the
#'   wired group inherits it -- do not pass `n`.
#' @param diagonals `TRUE` (the default) for an 8-neighbour Moore lattice,
#'   `FALSE` for a 4-neighbour von Neumann one.
#' @param torus `TRUE` (the default) wraps the edges; `FALSE` gives a bounded
#'   grid whose border cells have fewer neighbours.
#' @param ... Patch column specifications, exactly as [abm_agents()] takes them
#'   (`n` is available inside a formula and equals `prod(dims)`).
#'
#' @return An `abm_grid` object, recognised by [abm_setup()].
#' @seealso [abm_network()], [abm_agents()].
#' @export
#' @examples
#' life <- abm_setup(
#'   agents = abm_grid(dims = c(20, 20), alive = ~runif(n) < 0.3)
#' )
abm_grid <- function(dims, diagonals = TRUE, torus = TRUE, ...) {
  cols <- rlang::list2(...)
  if (length(cols) && !rlang::is_named(cols)) {
    abm_abort("Every column passed to {.fn abm_grid} must be named.",
              class = "tidyABM_unnamed_column")
  }
  bad <- grep("^\\.", names(cols), value = TRUE)
  if (length(bad)) {
    abm_abort(
      c("Column names beginning with {.code .} are reserved by tidyABM.",
        "x" = "Rename {.field {bad}}."),
      class = "tidyABM_reserved_column"
    )
  }
  structure(
    list(dims = dims, diagonals = diagonals, torus = torus, cols = cols),
    class = "abm_grid"
  )
}

#' Expand any `abm_grid()` in an `agents` argument into a spec + network
#'
#' Returns `list(agents = <named list of abm_agents>, network = <abm_network>)`.
#' Called at the top of [abm_setup()].
#' @noRd
expand_abm_grid <- function(agents, network, call = rlang::caller_env()) {
  if (inherits(agents, "abm_grid")) {
    agents <- list(patches = agents)
  }
  if (!is.list(agents)) return(list(agents = agents, network = network))
  is_grid <- vapply(agents, inherits, logical(1), "abm_grid")
  if (!any(is_grid)) return(list(agents = agents, network = network))
  if (sum(is_grid) > 1L) {
    abm_abort(
      c("A model can have one {.fn abm_grid} group.",
        "x" = "Got {sum(is_grid)}: {.field {names(agents)[is_grid]}}."),
      class = "tidyABM_bad_agents", call = call
    )
  }
  if (!rlang::is_named(agents)) {
    abm_abort(
      c("An {.arg agents} list that contains an {.fn abm_grid} must be named.",
        "i" = 'For example {.code list(patches = abm_grid(...), sheep = abm_agents(...))}.'),
      class = "tidyABM_unnamed_group", call = call
    )
  }
  if (!is.null(network)) {
    abm_abort(
      c("{.fn abm_grid} builds its own grid network.",
        "i" = "Drop {.arg network}, or use {.fn abm_agents} + {.fn abm_network} throughout."),
      class = "tidyABM_conflicting_args", call = call
    )
  }
  gi <- which(is_grid)
  gname <- names(agents)[[gi]]
  gr <- agents[[gi]]
  agents[[gi]] <- new_abm_agents(n = NULL, cols = gr$cols)
  network <- abm_network(type = "grid", dims = gr$dims,
                         diagonals = gr$diagonals, torus = gr$torus,
                         on = gname)
  list(agents = agents, network = network)
}

#' @export
print.abm_grid <- function(x, ...) {
  cli::cli_text("{.cls abm_grid} {.strong {paste(x$dims, collapse = 'x')}} cells")
  cli::cli_bullets(c(
    "*" = "diagonals = {x$diagonals}",
    "*" = "torus = {x$torus}"
  ))
  for (nm in names(x$cols)) {
    val <- x$cols[[nm]]
    shown <- if (is_formula1(val)) paste0("~", deparse1(rlang::f_rhs(val)))
             else deparse1(val)
    cli::cli_bullets(c("*" = "{.field {nm}} = {.code {shown}}"))
  }
  invisible(x)
}

# --- L1: one named lattice neighbour ------------------------------------

#' The (focal, that-one-neighbour) view for `abm_neighbours(.where = )`
#'
#' Restricts the neighbourhood to the single lattice neighbour in a named
#' compass direction. A focal cell with no such neighbour (a bounded edge)
#' contributes no row, so the aggregate over it comes out `NA`.
#' @noRd
directional_view <- function(step, combined, state) {
  lat <- state$lattice
  if (is.null(lat)) {
    abm_abort(
      c("{.arg .where} needs a lattice network.",
        "i" = 'Add {.code abm_network(type = "grid", ...)} or {.code type = "line"}.'),
      class = "tidyABM_no_lattice"
    )
  }
  where <- tolower(step$where)
  valid <- names(lat$dir_nb)
  if (!where %in% valid) {
    abm_abort(
      c("{.arg .where} must be one of {.val {valid}}.",
        "x" = "Got {.val {step$where}}."),
      class = "tidyABM_bad_where"
    )
  }
  # Every agent that is *somewhere* has a direction: a wired cell is its own
  # `.id`, a mobile agent's is the `.cell` it stands on. The neighbour looked
  # up is always a cell, so the candidate row is always a wired-group row.
  focal_cell <- rep(NA_integer_, nrow(combined))
  is_wired <- combined$.group == lat$on
  focal_cell[is_wired] <- combined$.id[is_wired]
  if (".cell" %in% names(combined)) {
    mob <- !is_wired & !is.na(combined$.cell)
    focal_cell[mob] <- combined$.cell[mob]
  }
  nb_cell <- rep(NA_integer_, nrow(combined))
  has_cell <- !is.na(focal_cell)
  nb_cell[has_cell] <- lat$dir_nb[[where]][focal_cell[has_cell]]

  keep <- which(!is.na(nb_cell))
  cand_idx <- match(nb_cell[keep], combined$.id)
  ok <- !is.na(cand_idx)
  pair_view(combined, keep[ok], cand_idx[ok])
}

# --- L2: the `within = .id == own_.cell` equijoin fast path -------------

#' Split an `&`-chain of conditions into its conjuncts
#' @noRd
and_conjuncts <- function(expr) {
  if (rlang::is_call(expr, "&") && length(expr) == 3L) {
    return(c(and_conjuncts(expr[[2]]), and_conjuncts(expr[[3]])))
  }
  list(expr)
}

#' Find an `own_<col> == <col>` (or the reverse) equality among the conjuncts
#'
#' Returns `list(focal = "<col the own_ prefix named>", cand = "<other col>",
#' rest = <remaining conjuncts>)`, or `NULL` if there is no such equality.
#' @noRd
equijoin_key <- function(within_expr) {
  parts <- and_conjuncts(within_expr)
  for (i in seq_along(parts)) {
    p <- parts[[i]]
    if (!rlang::is_call(p, "==") || length(p) != 3L) next
    lhs <- p[[2]]; rhs <- p[[3]]
    if (!rlang::is_symbol(lhs) || !rlang::is_symbol(rhs)) next
    ln <- rlang::as_string(lhs); rn <- rlang::as_string(rhs)
    lo <- startsWith(ln, "own_"); ro <- startsWith(rn, "own_")
    if (lo && !ro) {
      return(list(focal = sub("^own_", "", ln), cand = rn,
                  rest = parts[-i]))
    }
    if (ro && !lo) {
      return(list(focal = sub("^own_", "", rn), cand = ln,
                  rest = parts[-i]))
    }
  }
  NULL
}

#' The (focal, candidate) view for `within =`, via a hash join when possible
#'
#' Recognises `within = <col> == own_<col> [& ...]` and joins on that equality
#' instead of building every (focal, candidate) pair, so a co-location lookup
#' over thousands of patches is linear rather than quadratic. Returns `NULL`
#' when the pattern is not present, so the caller can fall back to the full
#' [attribute_view()].
#' @noRd
equijoin_view <- function(step, combined, globals) {
  key <- equijoin_key(rlang::quo_get_expr(step$within))
  if (is.null(key)) return(NULL)
  if (!key$focal %in% names(combined) || !key$cand %in% names(combined)) {
    return(NULL)
  }
  n <- nrow(combined)
  focal_key <- combined[[key$focal]]
  cand_key <- combined[[key$cand]]

  # A sort plus a run-length table, rather than a per-key list: this runs on
  # every tick of every co-location model, so it stays vectorised.
  ord <- order(cand_key, na.last = NA)
  if (!length(ord)) return(pair_view(combined, integer(), integer()))
  sorted <- cand_key[ord]
  runs <- rle(sorted)
  starts <- cumsum(c(1L, runs$lengths))[seq_along(runs$lengths)]

  j <- match(focal_key, runs$values)
  reps <- ifelse(is.na(j), 0L, runs$lengths[j])
  hit <- reps > 0L
  if (!any(hit)) return(pair_view(combined, integer(), integer()))

  cand_idx <- ord[sequence(reps[hit], from = starts[j[hit]])]
  focal_idx <- rep(seq_len(n), reps)
  view <- pair_view(combined, focal_idx, cand_idx)
  if (length(key$rest)) {
    env <- rlang::quo_get_env(step$within)
    rest_expr <- Reduce(function(a, b) rlang::call2("&", a, b), key$rest)
    keep <- eval_over_view(rlang::new_quosure(rest_expr, env), view, globals)
    if (!is.logical(keep)) keep <- as.logical(keep)
    keep[is.na(keep)] <- FALSE
    view <- view[keep, , drop = FALSE]
  }
  view
}
