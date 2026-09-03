# Spatial grammar: the one new step --------------------------------------
#
# `abm_move()` reassigns a mobile agent's `.cell` to a cell reachable from the
# one it is standing on. It is the only genuinely new step the spatial grammar
# needs: the mover needs the adjacency of the patch it stands on, and it is not
# itself on the patch network, so `abm_move()` is the step that knows both
# sides. Everything else in the L2 bundle is an argument on a step that already
# exists.

new_abm_move <- function(along, to, mode, expr, who, direction, range,
                         axes_only, avoid_occupied) {
  structure(
    list(along = along, to = to, mode = mode, expr = expr, who = who,
         direction = direction, range = range, axes_only = axes_only,
         avoid_occupied = avoid_occupied),
    class = c("abm_move", "abm_step")
  )
}

#' Move agents across a lattice
#'
#' A mobile agent's location is a patch `.id` held in a reserved `.cell`
#' column, and moving is writing that column. `abm_move()` is the step that
#' writes it: each mover's `.cell` is reassigned to a cell reachable from the
#' one it is standing on, and `.x` / `.y` follow automatically.
#'
#' # Choosing where to go
#'
#' `to` says how the destination is picked among the candidate cells:
#'
#' * `"random_neighbour"` -- a uniform draw from the adjacent cells.
#' * `"random_empty_neighbour"` -- the same, but only cells that no mover is
#'   standing on. This is `"random_neighbour"` with `avoid_occupied = TRUE`.
#' * `uphill(<expr>)` / `downhill(<expr>)` -- the candidate cell maximising or
#'   minimising `expr`. The cell the agent is already standing on is itself a
#'   candidate, so nothing can push an agent off the best cell in reach.
#' * `"stay"` -- nobody moves. Useful as the inert branch of a model that
#'   switches movement on and off.
#'
#' Ties are broken at random, and a mover with no legal target stays where it
#' is rather than erroring. Because the current cell only ties rather than
#' wins, an agent on flat ground wanders instead of freezing, which is what
#' keeps a gradient-follower exploring before it finds the gradient. Write
#' `uphill()` over an expression that already prefers the status quo if you
#' want the strict "only move if it is strictly better" reading.
#'
#' # What `uphill()` sees
#'
#' `uphill()` and `downhill()` are recognised forms inside `to =`, not general
#' functions. The expression is evaluated once per (mover, candidate cell), and
#' it sees:
#'
#' * the **candidate cell's** patch columns under their own names, which is what
#'   makes `uphill(chemical)` "step towards more pheromone";
#' * the **mover's** own columns under their own names too, so
#'   `uphill(if_else(carrying, nest_scent, chemical))` reads as written. Where
#'   both sides have a column of the same name the mover wins, except for the
#'   engine-owned `.id`, `.group`, `.x`, `.y` and `.cell`, which stay the
#'   candidate cell's;
#' * every mover column again as `own_<col>`, so a clash can always be said
#'   explicitly;
#' * the globals.
#'
#' # Looking further than one cell
#'
#' `range`, `axes_only` and `avoid_occupied` widen or narrow the candidate set.
#' They exist because particular models force them and should be left alone
#' otherwise: Sugarscape's agents look `vision` cells along the four axes and
#' will not land on an occupied patch, which is
#' `range = vision, axes_only = TRUE, avoid_occupied = TRUE`.
#'
#' `direction` is the other flavour: instead of picking a neighbour by a value,
#' the mover steps one cell along a heading it is carrying. Langton's Ant is the
#' model that forces it. The heading column is an integer `0` north, `1` east,
#' `2` south, `3` west (character names work too), and a heading that points off
#' a bounded edge leaves the agent where it is.
#'
#' @param along Which lattice to move on: the name of the grid-wired group (for
#'   example `"patches"`), or `"network"` to use the model's edge list as the
#'   adjacency directly.
#' @param to How the destination is chosen. `"random_neighbour"`,
#'   `"random_empty_neighbour"`, `"stay"`, or `uphill(<expr>)` /
#'   `downhill(<expr>)`. Ignored when `direction` is given.
#' @param who Character vector of the agent groups that move this step; the
#'   rest of the population is untouched. Defaults to every group that has a
#'   `.cell`.
#' @param direction A column holding a per-agent heading. The mover steps one
#'   cell along it, rather than choosing a cell by value. Cannot be combined
#'   with `to`, `range` or `axes_only`.
#' @param range How many cells out to look. `1` (the default) is the adjacent
#'   cells only. Larger values reach further, following the lattice's own
#'   `diagonals` and `torus` settings.
#' @param axes_only Whether to scan only along the four axes (north, south,
#'   east, west) rather than over the whole neighbourhood. Only meaningful with
#'   `range > 1`.
#' @param avoid_occupied Whether to refuse a cell that another mover is already
#'   standing on. Movers are resolved in a random order and each one claims the
#'   cell it lands on, so two agents never end up on the same cell through this
#'   step.
#'
#' @return An `abm_move` step object.
#' @seealso [abm_go()], which lists every step and fixes the order they run in.
#'   [abm_network()] for the lattice itself.
#' @family agent update steps
#' @export
#' @examples
#' # a random walk on the patch lattice
#' abm_move(along = "patches", to = "random_neighbour", who = c("sheep", "wolves"))
#'
#' # follow the pheromone home, or up the trail
#' abm_move(along = "patches", who = "ants",
#'          to = uphill(dplyr::if_else(carrying, nest_scent, chemical)))
#'
#' # Sugarscape: look `vision` cells along the axes, skip occupied patches
#' abm_move(along = "patches", who = "people", to = uphill(psugar),
#'          range = 6, axes_only = TRUE, avoid_occupied = TRUE)
#'
#' # Langton's Ant: one cell along a stored heading
#' abm_move(along = "patches", who = "ant", direction = heading)
abm_move <- function(along, to = "random_neighbour", who = NULL,
                     direction = NULL, range = NULL, axes_only = FALSE,
                     avoid_occupied = FALSE) {
  if (missing(along) || !rlang::is_string(along)) {
    abm_abort(
      c("{.fn abm_move} needs an {.arg along}.",
        "i" = 'The name of the grid-wired group, e.g. {.code along = "patches"}, or {.val network}.'),
      class = "tidyABM_missing_arg"
    )
  }
  direction <- enquo_or_null(rlang::enquo(direction))
  to_quo <- rlang::enquo(to)
  to_given <- !identical(rlang::quo_get_expr(to_quo), rlang::expr("random_neighbour"))

  if (!is.null(direction)) {
    if (to_given || !is.null(range) || isTRUE(axes_only)) {
      abm_abort(
        c("{.arg direction} already says where the mover goes.",
          "i" = "Drop {.arg to}, {.arg range} and {.arg axes_only}, or drop {.arg direction}."),
        class = "tidyABM_conflicting_args"
      )
    }
    mode <- "direction"
    to <- NULL
    expr <- NULL
  } else {
    expr <- rlang::quo_get_expr(to_quo)
    if (rlang::is_call(expr, c("uphill", "downhill"))) {
      if (length(expr) != 2L) {
        abm_abort(
          c("{.fn uphill} and {.fn downhill} take one expression.",
            "x" = "Got {.code {deparse1(expr)}}."),
          class = "tidyABM_bad_to"
        )
      }
      mode <- rlang::as_string(expr[[1]])
      expr <- rlang::new_quosure(expr[[2]], rlang::quo_get_env(to_quo))
      to <- NULL
    } else {
      to <- rlang::eval_tidy(to_quo)
      if (!rlang::is_string(to)) {
        abm_abort(
          c("{.arg to} must be a string or {.fn uphill} / {.fn downhill}.",
            "x" = "Got {.code {deparse1(rlang::quo_get_expr(to_quo))}}."),
          class = "tidyABM_bad_to"
        )
      }
      to <- rlang::arg_match0(
        to, c("random_neighbour", "random_empty_neighbour", "stay"),
        arg_nm = "to"
      )
      mode <- to
      expr <- NULL
      if (to == "random_empty_neighbour") avoid_occupied <- TRUE
    }
  }

  if (!is.null(who) && !is.character(who)) {
    abm_abort("{.arg who} must be a character vector of group names.",
              class = "tidyABM_bad_arg")
  }
  if (!is.null(range)) {
    if (!rlang::is_scalar_integerish(range) || is.na(range) || range < 1) {
      abm_abort("{.arg range} must be a single whole number of at least 1.",
                class = "tidyABM_bad_arg")
    }
    range <- as.integer(range)
  }
  if (!rlang::is_bool(axes_only)) {
    abm_abort("{.arg axes_only} must be {.code TRUE} or {.code FALSE}.",
              class = "tidyABM_bad_arg")
  }
  if (!rlang::is_bool(avoid_occupied)) {
    abm_abort("{.arg avoid_occupied} must be {.code TRUE} or {.code FALSE}.",
              class = "tidyABM_bad_arg")
  }
  new_abm_move(along, to, mode, expr, who, direction, range %||% 1L,
               axes_only, avoid_occupied)
}

# --- execution ------------------------------------------------------------

#' Adjacency for the lattice `along` names
#'
#' Returns a list of integer vectors indexed by cell id, plus the lattice when
#' there is one (`along = "network"` has coordinates only if the model's network
#' happens to be a lattice).
#' @noRd
move_adjacency <- function(step, state) {
  lat <- state$lattice
  if (identical(step$along, "network")) {
    if (is.null(state$edges)) {
      abm_abort(
        c('{.code abm_move(along = "network")} needs a network.',
          "i" = "Add one with {.code abm_setup(..., network = abm_network(...))}."),
        class = "tidyABM_no_network"
      )
    }
    if (!is.null(lat)) return(lat$nb)
    ids <- sort(unique(c(state$edges$from, state$edges$to)))
    if (!length(ids)) return(list())
    nb <- vector("list", max(ids))
    split_to <- split(state$edges$to, state$edges$from)
    split_from <- split(state$edges$from, state$edges$to)
    for (i in ids) {
      nb[[i]] <- unique(c(split_to[[as.character(i)]],
                          split_from[[as.character(i)]]))
    }
    return(nb)
  }
  if (is.null(lat)) {
    abm_abort(
      c("{.fn abm_move} needs a lattice.",
        "i" = 'Add {.code abm_network(type = "grid", dims = ..., on = "{step$along}")}.'),
      class = "tidyABM_no_lattice"
    )
  }
  if (!identical(lat$on, step$along)) {
    abm_abort(
      c("{.arg along} does not name the grid-wired group.",
        "x" = "Got {.val {step$along}}; the lattice is wired to {.field {lat$on}}."),
      class = "tidyABM_bad_arg"
    )
  }
  lat$nb
}

#' The cells a mover on `cell` may consider
#'
#' Never includes the cell itself; `uphill` / `downhill` add it back, because an
#' agent already standing on the best cell should stay.
#' @noRd
candidate_cells <- function(cell, nb, lat, step) {
  # `along = "network"` can hand back an adjacency shorter than the cell id, for
  # a cell that has no edges at all; that is "nowhere to go", not an error
  at <- function(i) if (i >= 1L && i <= length(nb)) nb[[i]] %||% integer()
                    else integer()
  if (step$range <= 1L && !step$axes_only) {
    return(at(cell))
  }
  if (step$axes_only) {
    if (is.null(lat)) {
      abm_abort('{.arg axes_only} needs a lattice.', class = "tidyABM_no_lattice")
    }
    dirs <- intersect(c("north", "south", "east", "west"), names(lat$dir_nb))
    out <- integer()
    for (d in dirs) {
      cur <- cell
      for (k in seq_len(step$range)) {
        cur <- lat$dir_nb[[d]][cur]
        if (is.na(cur) || cur == cell) break
        out <- c(out, cur)
      }
    }
    return(unique(out))
  }
  # a ball of radius `range` in the lattice's own metric, by repeated expansion
  seen <- cell
  frontier <- cell
  for (k in seq_len(step$range)) {
    frontier <- setdiff(unique(unlist(lapply(frontier, at), use.names = FALSE)),
                        seen)
    if (!length(frontier)) break
    seen <- c(seen, frontier)
  }
  setdiff(seen, cell)
}

#' Heading column -> the compass name `dir_nb` is indexed by
#' @noRd
heading_names <- function(v) {
  if (is.character(v)) return(tolower(v))
  idx <- suppressWarnings(as.integer(round(as.numeric(v))))
  c("north", "east", "south", "west")[(idx %% 4L) + 1L]
}

#' Evaluate an `uphill()` / `downhill()` expression over candidate cells
#'
#' One row per candidate: the candidate cell's patch columns under their own
#' names, the mover's own columns over the top of them (except the engine-owned
#' ones), and every mover column again as `own_<col>`.
#' @noRd
eval_gradient <- function(quo, cells, wired, mover_row, globals) {
  patch <- wired[match(cells, wired$.id), , drop = FALSE]
  reserved <- c(".id", ".group", ".x", ".y", ".cell")
  own <- as.list(mover_row)
  view <- patch
  for (nm in names(own)) {
    if (nm %in% reserved) next
    view[[nm]] <- rep(own[[nm]], nrow(patch))
  }
  for (nm in names(own)) {
    view[[paste0("own_", nm)]] <- rep(own[[nm]], nrow(patch))
  }
  env <- rlang::quo_get_env(quo)
  if (length(globals)) env <- rlang::new_environment(globals, parent = env)
  val <- dplyr::pull(dplyr::mutate(view, .abm_value = !!rlang::quo_set_env(quo, env)),
                     ".abm_value")
  if (length(val) == 1L) val <- rep(val, nrow(patch))
  as.numeric(val)
}

#' @noRd
run_move <- function(step, state) {
  if (identical(step$mode, "stay")) return(state)
  lat <- state$lattice
  nb <- move_adjacency(step, state)

  which_groups <- names(state$groups)[
    vapply(state$groups, function(g) ".cell" %in% names(g), logical(1))
  ]
  if (!is.null(step$who)) {
    unknown <- setdiff(step$who, names(state$groups))
    if (length(unknown)) {
      abm_abort(
        c("{.arg who} names {length(unknown)} group{?s} that {?is/are} not in the model.",
          "x" = "No group {.field {unknown}}.",
          "i" = "Groups: {.field {names(state$groups)}}."),
        class = "tidyABM_bad_arg"
      )
    }
    no_cell <- setdiff(step$who, which_groups)
    if (length(no_cell)) {
      abm_abort(
        c("{.arg who} names {length(no_cell)} group{?s} with no {.code .cell}.",
          "x" = "{.field {no_cell}} {?is/are} not placed on the lattice.",
          "i" = "The grid-wired group is the lattice; only the other groups move."),
        class = "tidyABM_bad_arg"
      )
    }
    which_groups <- step$who
  }
  if (!length(which_groups)) return(state)

  if (identical(step$mode, "direction") && is.null(lat)) {
    abm_abort(
      c("{.arg direction} steps along a compass heading, which needs a lattice.",
        "i" = 'Add {.code abm_network(type = "grid", dims = ...)}.'),
      class = "tidyABM_no_lattice"
    )
  }
  if (step$axes_only && is.null(lat)) {
    abm_abort(
      c("{.arg axes_only} scans the lattice axes, which needs a lattice.",
        "i" = 'Add {.code abm_network(type = "grid", dims = ...)}.'),
      class = "tidyABM_no_lattice"
    )
  }

  wired <- if (!is.null(lat)) state$groups[[lat$on]] else NULL
  needs_wired <- step$mode %in% c("uphill", "downhill")
  if (needs_wired && (is.null(wired) || !nrow(wired))) {
    abm_abort(
      c("{.fn uphill} and {.fn downhill} read the cells being chosen between.",
        "i" = "The grid-wired group is empty."),
      class = "tidyABM_no_lattice"
    )
  }

  # one flat table of movers, so the random order and the occupancy claims are
  # shared across groups rather than being per-group
  order_rows <- do.call(rbind, lapply(which_groups, function(nm) {
    g <- state$groups[[nm]]
    if (!nrow(g)) return(NULL)
    cbind(group = match(nm, which_groups), row = seq_len(nrow(g)))
  }))
  if (is.null(order_rows) || !nrow(order_rows)) return(state)
  ord <- if (nrow(order_rows) > 1L) sample.int(nrow(order_rows)) else 1L
  order_rows <- order_rows[ord, , drop = FALSE]

  cells <- lapply(which_groups, function(nm) state$groups[[nm]]$.cell)
  # Occupancy is a *count* per cell, not a set of cells: several movers can
  # start stacked on one cell, and the first of them leaving must not mark that
  # cell free while the rest are still standing on it.
  occupied <- NULL
  if (step$avoid_occupied) {
    all_cells <- unlist(cells, use.names = FALSE)
    occupied <- tabulate(all_cells[!is.na(all_cells)],
                         nbins = max(length(nb), max(all_cells, 0L, na.rm = TRUE)))
  }
  taken <- function(i) !is.null(occupied) && i >= 1L &&
    i <= length(occupied) && occupied[[i]] > 0L

  headings <- NULL
  if (identical(step$mode, "direction")) {
    headings <- lapply(which_groups, function(nm) {
      g <- state$groups[[nm]]
      v <- eval_rule(list(quo = step$direction), g, state$globals,
                     grouped = FALSE)
      if (length(v) == 1L) v <- rep(v, nrow(g))
      heading_names(v)
    })
  }

  for (k in seq_len(nrow(order_rows))) {
    gi <- order_rows[k, "group"]
    ri <- order_rows[k, "row"]
    here <- cells[[gi]][[ri]]
    if (is.na(here)) next

    if (identical(step$mode, "direction")) {
      d <- headings[[gi]][[ri]]
      if (is.na(d) || !d %in% names(lat$dir_nb)) next
      target <- lat$dir_nb[[d]][here]
      if (is.na(target)) next
      if (step$avoid_occupied) {
        if (target != here && taken(target)) next
        occupied[[here]] <- occupied[[here]] - 1L
        occupied[[target]] <- occupied[[target]] + 1L
      }
      cells[[gi]][[ri]] <- target
      next
    }

    cand <- candidate_cells(here, nb, lat, step)
    if (step$mode %in% c("uphill", "downhill")) cand <- unique(c(here, cand))
    cand <- cand[!is.na(cand)]
    if (step$avoid_occupied && length(cand)) {
      cand <- cand[cand == here | !vapply(cand, taken, logical(1))]
    }
    if (!length(cand)) next

    if (step$mode %in% c("uphill", "downhill")) {
      g <- state$groups[[which_groups[[gi]]]]
      val <- eval_gradient(step$expr, cand, wired, g[ri, , drop = FALSE],
                           state$globals)
      val[is.na(val)] <- if (step$mode == "uphill") -Inf else Inf
      best <- if (step$mode == "uphill") which(val == max(val))
              else which(val == min(val))
      if (!length(best)) next
      target <- cand[[if (length(best) == 1L) best else sample(best, 1L)]]
    } else {
      target <- cand[[if (length(cand) == 1L) 1L else sample.int(length(cand), 1L)]]
    }
    if (step$avoid_occupied && target != here) {
      occupied[[here]] <- occupied[[here]] - 1L
      occupied[[target]] <- occupied[[target]] + 1L
    }
    cells[[gi]][[ri]] <- target
  }

  for (i in seq_along(which_groups)) {
    state$groups[[which_groups[[i]]]]$.cell <- as.integer(cells[[i]])
  }
  sync_cell_coords(state)
}

#' @export
print.abm_move <- function(x, ...) {
  dest <- if (!is.null(x$direction)) {
    paste0("direction = ", deparse1(rlang::quo_get_expr(x$direction)))
  } else if (!is.null(x$expr)) {
    paste0(x$mode, "(", deparse1(rlang::quo_get_expr(x$expr)), ")")
  } else {
    x$to
  }
  cli::cli_text("{.cls abm_move} along {.val {x$along}} to {.emph {dest}}")
  bits <- c(
    if (!is.null(x$who)) "who = {.val {x$who}}",
    if (x$range > 1L) "range = {x$range}",
    if (x$axes_only) "axes_only = TRUE",
    if (x$avoid_occupied) "avoid_occupied = TRUE"
  )
  if (length(bits)) cli::cli_bullets(stats::setNames(bits, rep("*", length(bits))))
  invisible(x)
}
