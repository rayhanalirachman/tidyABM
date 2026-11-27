# Spatial grammar: the one new step --------------------------------------
#
# `abm_move()` reassigns a mobile agent's `.cell` to a cell reachable from the
# one it is standing on. It is the only genuinely new step the spatial grammar
# needs: the mover needs the adjacency of the patch it stands on, and it is not
# itself on the patch network, so `abm_move()` is the step that knows both
# sides. Everything else in the L2 bundle is an argument on a step that already
# exists.

new_abm_move <- function(along, to, mode, expr, who, direction, range,
                         axes_only, avoid_occupied, when = NULL,
                         range_quo = NULL, move_args = NULL) {
  structure(
    list(along = along, to = to, mode = mode, expr = expr, who = who,
         direction = direction, range = range, range_quo = range_quo,
         axes_only = axes_only, avoid_occupied = avoid_occupied,
         when = when, move_args = move_args),
    class = c("abm_move", "abm_step")
  )
}

#' Move agents across a lattice
#'
#' A mobile agent's location is a patch `.id` held in a reserved `.cell`
#' column, and moving is writing that column. `abm_move()` is the step that
#' writes it: each mover's `.cell` is reassigned to a cell reachable from the
#' one it is standing on, and `.x` / `.y` follow automatically. A rule may also
#' write `.cell` directly (`abm_rules(.cell ~ which(nest)[1])` sends everyone
#' home); `.x` / `.y` follow that too, and are themselves read-only.
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
#' @param along Optional lattice to move on: the name of the grid-wired group
#'   (for example `"patches"`), `"space"`, or `"network"` to use the model's
#'   edge list directly. Defaults to the model's space/lattice when omitted.
#' @param to How the destination is chosen. `"random_neighbour"`,
#'   `"random_empty_neighbour"`, `"random_empty"`, `"random_cell"`, `"stay"`,
#'   `uphill(<expr>)` / `downhill(<expr>)`, or on continuous spaces
#'   `offset(...)` / `forward(...)`. Ignored when `direction` is given.
#' @param who Character vector of the agent groups that move this step; the
#'   rest of the population is untouched. Defaults to every group that has a
#'   `.cell` (or continuous coordinates).
#' @param when Optional condition selecting which agents move this step. Agents
#'   where `when` is `FALSE` stay put and still occupy their cells for
#'   `avoid_occupied`.
#' @param direction A column holding a per-agent heading. The mover steps one
#'   cell along it, rather than choosing a cell by value. Cannot be combined
#'   with `to`, `range` or `axes_only`.
#' @param range How many cells out to look. `1` (the default) is the adjacent
#'   cells only. Larger values reach further, following the lattice's own
#'   `diagonals` and `torus` settings.
#' @param axes_only Whether to scan only along the four axes (north, south,
#'   east, west) rather than over the whole neighbourhood. With `range = 1` on
#'   a Moore lattice that is the four orthogonal neighbours; with a larger
#'   `range` it is the four lines of sight a Sugarscape agent looks along.
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
abm_move <- function(along = NULL, to = "random_neighbour", who = NULL,
                     when = NULL, direction = NULL, range = NULL,
                     axes_only = FALSE, avoid_occupied = FALSE) {
  if (!is.null(along) && !rlang::is_string(along)) {
    abm_abort(
      c("{.arg along} must be a single string.",
        "i" = 'The name of the grid-wired group, e.g. {.code along = "patches"}, {.val space}, or {.val network}.'),
      class = "tidyABM_bad_arg"
    )
  }
  when <- enquo_or_null(rlang::enquo(when))
  direction <- enquo_or_null(rlang::enquo(direction))
  range_quo_raw <- enquo_or_null(rlang::enquo(range))
  to_quo <- rlang::enquo(to)
  to_given <- !identical(rlang::quo_get_expr(to_quo), rlang::expr("random_neighbour"))
  move_args <- NULL

  if (!is.null(direction)) {
    if (to_given || !is.null(range_quo_raw) || isTRUE(axes_only)) {
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
    } else if (rlang::is_call(expr, "offset")) {
      a <- as.list(expr)[-1]
      if (!length(a) %in% 1:2) {
        abm_abort(
          c("{.fn offset} takes one expression in 1-D ({.code offset(dx)}) or two in 2-D ({.code offset(dx, dy)}).",
            "x" = "Got {.code {deparse1(expr)}}."),
          class = "tidyABM_bad_to"
        )
      }
      mode <- "offset"
      move_args <- lapply(a, rlang::new_quosure, env = rlang::quo_get_env(to_quo))
      expr <- NULL
      to <- NULL
    } else if (rlang::is_call(expr, "forward")) {
      a <- as.list(expr)[-1]
      if (length(a) != 2L) {
        abm_abort(
          c("{.fn forward} takes two expressions: {.code forward(by, heading)}.",
            "x" = "Got {.code {deparse1(expr)}}."),
          class = "tidyABM_bad_to"
        )
      }
      nms <- names(a) %||% c("", "")
      if (any(nzchar(nms))) {
        if (!setequal(nms, c("by", "heading"))) {
          abm_abort("{.fn forward} arguments are {.arg by} and {.arg heading}.",
                    class = "tidyABM_bad_to")
        }
        a <- a[c("by", "heading")]
      }
      mode <- "forward"
      move_args <- list(
        by = rlang::new_quosure(a[[1]], rlang::quo_get_env(to_quo)),
        heading = rlang::new_quosure(a[[2]], rlang::quo_get_env(to_quo))
      )
      expr <- NULL
      to <- NULL
    } else {
      to <- rlang::eval_tidy(to_quo)
      if (!rlang::is_string(to)) {
        abm_abort(
          c("{.arg to} must be a string, {.fn uphill}/{.fn downhill}, or {.fn offset}/{.fn forward}.",
            "x" = "Got {.code {deparse1(rlang::quo_get_expr(to_quo))}}."),
          class = "tidyABM_bad_to"
        )
      }
      to <- rlang::arg_match0(
        to, c("random_neighbour", "random_empty_neighbour", "random_empty",
              "random_cell", "stay"),
        arg_nm = "to"
      )
      mode <- to
      expr <- NULL
      if (to %in% c("random_empty_neighbour", "random_empty")) avoid_occupied <- TRUE
    }
  }

  if (!is.null(who) && !is.character(who)) {
    abm_abort("{.arg who} must be a character vector of group names.",
              class = "tidyABM_bad_arg")
  }
  range_val <- 1L
  range_quo <- NULL
  if (!is.null(range_quo_raw)) {
    re <- rlang::quo_get_expr(range_quo_raw)
    if (is.numeric(re) || is.character(re) || is.logical(re) ||
        (rlang::is_call(re, c("-", "+", "c")) && !length(all.vars(re)))) {
      rv <- rlang::eval_tidy(range_quo_raw)
      if (!rlang::is_scalar_integerish(rv) || is.na(rv) || rv < 1) {
        abm_abort("{.arg range} must be a single whole number of at least 1, or an agent column/expression.",
                  class = "tidyABM_bad_arg")
      }
      range_val <- as.integer(rv)
    } else {
      # Could be a scalar variable in the caller's env (e.g. `r <- 3; range = r`)
      # or an agent column/expression (e.g. `range = vision`). Resolve scalar
      # numeric literals from env only if it exists and is a bare symbol NOT
      # shadowing; safest is to keep as `range_quo` and evaluate at run time
      # (agent columns win, then globals, then caller env)!
      range_val <- NULL
      range_quo <- range_quo_raw
    }
  }
  if (!rlang::is_bool(axes_only)) {
    abm_abort("{.arg axes_only} must be {.code TRUE} or {.code FALSE}.",
              class = "tidyABM_bad_arg")
  }
  if (!rlang::is_bool(avoid_occupied)) {
    abm_abort("{.arg avoid_occupied} must be {.code TRUE} or {.code FALSE}.",
              class = "tidyABM_bad_arg")
  }
  new_abm_move(along, to, mode, expr, who, direction, range_val,
               axes_only, avoid_occupied, when = when,
               range_quo = range_quo, move_args = move_args)
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
  if (is.null(step$along) || identical(step$along, "space")) {
    if (is.null(lat)) {
      abm_abort(
        c("{.fn abm_move} needs a grid space or lattice.",
          "i" = 'Add {.code space = abm_space(type = "grid", dims = ...)} or {.code along = "network"}.'),
        class = "tidyABM_no_lattice"
      )
    }
    return(lat$nb)
  }
  if (identical(step$along, "network")) {
    if (is.null(state$edges)) {
      abm_abort(
        c('{.code abm_move(along = "network")} needs a network.',
          "i" = "Add one with {.code abm_setup(..., network = abm_network(...))}."),
        class = "tidyABM_no_network"
      )
    }
    if (!is.null(lat) && is.null(state$space)) return(lat$nb)
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
        "i" = 'Add {.code abm_space(type = "grid", dims = ..., on = "{step$along}")}.'),
      class = "tidyABM_no_lattice"
    )
  }
  if (!identical(lat$on, step$along)) {
    abm_abort(
      c("{.arg along} does not name the grid-wired group.",
        "x" = "Got {.val {step$along}}; the lattice is wired to {.field {lat$on %||% '<none>'}}."),
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
candidate_cells <- function(cell, nb, lat, range, axes_only) {
  if (is.na(range) || range < 1L) return(integer())
  # `along = "network"` can hand back an adjacency shorter than the cell id, for
  # a cell that has no edges at all; that is "nowhere to go", not an error
  at <- function(i) if (i >= 1L && i <= length(nb)) nb[[i]] %||% integer()
                    else integer()
  if (range <= 1L && !axes_only) {
    return(at(cell))
  }
  if (axes_only) {
    if (is.null(lat)) {
      abm_abort('{.arg axes_only} needs a lattice.', class = "tidyABM_no_lattice")
    }
    dirs <- intersect(c("north", "south", "east", "west"), names(lat$dir_nb))
    out <- integer()
    for (d in dirs) {
      cur <- cell
      for (k in seq_len(range)) {
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
  for (k in seq_len(range)) {
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

#' The (candidate cell, mover) view an `uphill()` expression is evaluated in
#'
#' One row per candidate: the candidate cell's patch columns under their own
#' names, the mover's own columns over the top of them (except the engine-owned
#' ones), and every mover column again as `own_<col>`. `own` holds the movers'
#' columns, one value per mover; `lens` says how many candidate rows each
#' mover has, so a single mover is `lens = length(cells)`.
#' @noRd
gradient_view <- function(cells, wired, own, lens, space = NULL,
                          need_dist = FALSE) {
  view <- lapply(as.list(wired), `[`, match(cells, wired$.id))
  reserved <- c(".id", ".group", ".x", ".y", ".cell")
  for (nm in names(own)) {
    if (nm %in% reserved) next
    view[[nm]] <- rep(own[[nm]], lens)
  }
  for (nm in names(own)) {
    view[[paste0("own_", nm)]] <- rep(own[[nm]], lens)
  }
  if (need_dist && !is.null(space)) {
    view$.dist <- space_distance(view$.x, view$.y, view$own_.x, view$own_.y, space)
  }
  view
}

#' Evaluate an `uphill()` / `downhill()` expression over a view
#'
#' `ctx` is built once per step by [gradient_context()]: the quosure, whether
#' it needs a dplyr mask, and the environment the globals live in. The view
#' is a bare list: this can run once per mover per tick, and `mutate()` over
#' a tibble built with `[[<-` was nine tenths of what an `uphill()` model
#' cost. The same shortcut `eval_rule()` takes: `eval_tidy()` unless the
#' expression needs something only a dplyr mask supplies.
#' @noRd
eval_gradient <- function(ctx, view, np) {
  val <- NULL
  if (!ctx$mask) {
    env <- rlang::new_environment(list(n = function() np), parent = ctx$env)
    val <- rlang::eval_tidy(rlang::quo_get_expr(ctx$quo), data = view, env = env)
    if (length(val) != 1L && length(val) != np) val <- NULL
  }
  if (is.null(val)) {
    val <- dplyr::pull(
      dplyr::mutate(new_tbl(view, np),
                    .abm_value = !!rlang::quo_set_env(ctx$quo, ctx$env)),
      ".abm_value")
  }
  if (length(val) == 1L) val <- rep(val, np)
  as.numeric(val)
}

#' What an `uphill()` expression needs, worked out once per step
#'
#' `batch` says whether the expression can be evaluated over every mover's
#' candidates at once rather than once per mover. It can when every call in it
#' is elementwise: the value on a row then depends on that row alone, and
#' nothing in it draws, so the result is the same and so is the random stream
#' (ties are still broken mover by mover, in order). An expression that draws
#' or aggregates keeps the per-mover evaluation, which is what it was written
#' against.
#' @noRd
gradient_context <- function(step, globals) {
  quo <- step$expr
  expr <- rlang::quo_get_expr(quo)
  list(quo = quo, mask = needs_mask(expr), env = abm_eval_env(quo, globals),
       batch = is_elementwise(expr))
}

#' Evaluate `abm_move(when =)` across moving groups (binding multi-group columns)
#' @noRd
move_when_masks <- function(step, state, which_groups) {
  if (is.null(step$when)) return(NULL)
  when_cvars <- cell_vars(list(step$when))
  if (length(which_groups) == 1L) {
    g <- state$groups[[which_groups[[1]]]]
    if (!vctrs::vec_size(g)) return(list(logical()))
    aug <- if (length(when_cvars)) attach_cell_columns(g, state, when_cvars) else g
    return(list(eval_condition(step$when, aug, state$globals)))
  }
  combined <- bind_groups(state$groups[which_groups])
  if (!nrow(combined)) return(lapply(which_groups, function(nm) logical()))
  aug <- if (length(when_cvars)) attach_cell_columns(combined, state, when_cvars) else combined
  keep <- eval_condition(step$when, aug, state$globals)
  out <- vector("list", length(which_groups))
  off <- 0L
  for (i in seq_along(which_groups)) {
    k <- vctrs::vec_size(state$groups[[which_groups[[i]]]])
    out[[i]] <- if (k) keep[(off + 1L):(off + k)] else logical()
    off <- off + k
  }
  out
}

#' Continuous-space movement for `abm_move(to = offset(...) | forward(...))`
#' @noRd
run_move_continuous <- function(step, state) {
  sp <- state$space
  if (is.null(sp) || !identical(sp$type, "continuous")) {
    abm_abort(
      c("{.fn {step$mode}} requires a continuous space.",
        "i" = 'Add {.code space = abm_space(type = "continuous", dims = ...)} to {.fn abm_setup}.'),
      class = "tidyABM_bad_to"
    )
  }
  dims <- sp$dims
  torus <- sp$torus
  nd <- length(dims)
  if (step$mode == "offset" && length(step$move_args) != nd) {
    abm_abort(
      c("{.fn offset} needs {nd} expression{?s} for a {nd}-D space.",
        "x" = "Got {length(step$move_args)}."),
      class = "tidyABM_bad_to"
    )
  }
  which_groups <- names(state$groups)[
    vapply(state$groups, function(g) ".x" %in% names(g), logical(1))
  ]
  if (!is.null(step$who)) {
    unknown <- step$who[!step$who %in% names(state$groups)]
    if (length(unknown)) {
      abm_abort(
        c("{.arg who} names {length(unknown)} group{?s} that {?is/are} not in the model.",
          "x" = "No group {.field {unknown}}."),
        class = "tidyABM_bad_arg"
      )
    }
    which_groups <- step$who
  }
  wrap_or_clamp <- function(v, L, wrap) {
    if (wrap) v %% L else pmin(pmax(v, 0), L)
  }
  masks <- move_when_masks(step, state, which_groups)
  for (gi in seq_along(which_groups)) {
    nm <- which_groups[[gi]]
    g <- state$groups[[nm]]
    n <- vctrs::vec_size(g)
    if (!n) next
    rows <- if (is.null(masks)) rep(TRUE, n) else masks[[gi]]
    if (!any(rows)) next
    sub <- if (all(rows)) g else vctrs::vec_slice(g, rows)
    k <- vctrs::vec_size(sub)
    if (step$mode == "offset") {
      dx <- eval_rule(list(quo = step$move_args[[1]]), sub, state$globals, grouped = FALSE)
      if (length(dx) == 1L) dx <- rep(dx, k)
      dy <- if (nd == 2L) {
        v <- eval_rule(list(quo = step$move_args[[2]]), sub, state$globals, grouped = FALSE)
        if (length(v) == 1L) rep(v, k) else v
      }
    } else {
      by <- eval_rule(list(quo = step$move_args$by), sub, state$globals, grouped = FALSE)
      hd <- eval_rule(list(quo = step$move_args$heading), sub, state$globals, grouped = FALSE)
      if (length(by) == 1L) by <- rep(by, k)
      if (length(hd) == 1L) hd <- rep(hd, k)
      dx <- by * cos(hd)
      dy <- if (nd == 2L) by * sin(hd) else NULL
    }
    nx <- g$.x
    nx[rows] <- wrap_or_clamp(nx[rows] + as.double(dx), dims[[1]], torus[[1]])
    g <- set_col(g, ".x", nx)
    if (nd == 2L) {
      ny <- g$.y
      ny[rows] <- wrap_or_clamp(ny[rows] + as.double(dy), dims[[2]], torus[[2]])
      g <- set_col(g, ".y", ny)
    }
    state$groups[[nm]] <- g
  }
  state
}

#' @noRd
run_move <- function(step, state) {
  if (identical(step$mode, "stay")) return(state)
  if (step$mode %in% c("offset", "forward")) {
    return(run_move_continuous(step, state))
  }
  lat <- state$lattice
  nb <- move_adjacency(step, state)

  which_groups <- names(state$groups)[
    vapply(state$groups, function(g) ".cell" %in% names(g), logical(1))
  ]
  if (!is.null(step$who)) {
    unknown <- step$who[!step$who %in% names(state$groups)]
    if (length(unknown)) {
      abm_abort(
        c("{.arg who} names {length(unknown)} group{?s} that {?is/are} not in the model.",
          "x" = "No group {.field {unknown}}.",
          "i" = "Groups: {.field {names(state$groups)}}."),
        class = "tidyABM_bad_arg"
      )
    }
    no_cell <- step$who[!step$who %in% which_groups]
    if (length(no_cell)) {
      abm_abort(
        c("{.arg who} names {length(no_cell)} group{?s} with no {.code .cell}.",
          "x" = "{.field {no_cell}} {?is/are} not placed on the lattice.",
          "i" = "The grid-wired group is the lattice; only the other groups move."),
        class = "tidyABM_bad_arg"
      )
    }
    which_groups <- step$who
  } else if (!length(which_groups)) {
    abm_abort(
      c("{.fn abm_move} found nothing that can move.",
        "x" = "No group has a {.code .cell}: nothing is standing on a lattice.",
        "i" = "The grid-wired group is the lattice itself; a group that moves is another group, placed on it by {.code abm_network(on = )}."),
      class = "tidyABM_bad_arg"
    )
  }
  if (!length(which_groups)) return(state)

  if (identical(step$mode, "direction") && is.null(lat)) {
    abm_abort(
      c("{.arg direction} steps along a compass heading, which needs a lattice.",
        "i" = 'Add {.code abm_network(type = "grid", dims = ...)}.'),
      class = "tidyABM_no_lattice"
    )
  }
  if (step$mode %in% c("random_empty", "random_cell") && is.null(lat)) {
    abm_abort(
      c('{.code to = "{step$mode}"} needs a grid lattice.',
        "i" = 'Add {.code abm_space(type = "grid", dims = ...)}.'),
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

  wired <- if (!is.null(lat) && !is.null(lat$on)) state$groups[[lat$on]] else NULL
  gradient <- step$mode %in% c("uphill", "downhill")
  if (gradient && is.null(wired)) {
    if (!is.null(lat)) {
      # Patchless grid: synthetic cell table carrying `.id`, `.x` (and `.y`)
      wired <- if (is.null(lat$y)) {
        new_tbl(list(.id = seq_len(lat$ncell), .x = lat$x), lat$ncell)
      } else {
        new_tbl(list(.id = seq_len(lat$ncell), .x = lat$x, .y = lat$y), lat$ncell)
      }
    } else {
      abm_abort(
        c("{.fn uphill} and {.fn downhill} read the cells being chosen between.",
          "i" = "The grid-wired group is empty."),
        class = "tidyABM_no_lattice"
      )
    }
  }
  if (gradient && !nrow(wired)) {
    abm_abort(
      c("{.fn uphill} and {.fn downhill} read the cells being chosen between.",
        "i" = "The grid-wired group is empty."),
      class = "tidyABM_no_lattice"
    )
  }

  # Evaluate `when =` across moving groups to determine which rows actually move.
  # Non-moving agents still occupy their cells for `avoid_occupied`.
  cells <- lapply(which_groups, function(nm) state$groups[[nm]]$.cell)
  masks <- move_when_masks(step, state, which_groups)
  active_rows <- lapply(seq_along(which_groups), function(gi) {
    n <- vctrs::vec_size(state$groups[[which_groups[[gi]]]])
    if (!n) return(integer())
    if (is.null(masks)) seq_len(n) else which(masks[[gi]])
  })
  sizes <- lengths(active_rows)
  n_movers <- sum(sizes)
  if (!n_movers) return(state)
  ord <- if (n_movers > 1L) sample.int(n_movers) else 1L
  grp <- rep.int(seq_along(which_groups), sizes)[ord]
  row <- unlist(active_rows, use.names = FALSE)[ord]

  # each mover's own row is sliced out of these bare columns rather than with
  # `[.tbl_df`, once per mover
  gcols <- if (gradient) lapply(which_groups, function(nm) as.list(state$groups[[nm]]))
  # Occupancy is a *count* per cell across ALL agents in `which_groups`
  occupied <- NULL
  if (step$avoid_occupied) {
    all_cells <- unlist(cells, use.names = FALSE)
    nbins <- max(if (!is.null(lat)) lat$ncell else 0L,
                 length(nb), max(all_cells, 0L, na.rm = TRUE))
    occupied <- tabulate(all_cells[!is.na(all_cells)], nbins = nbins)
  }
  taken <- function(i) !is.null(occupied) && i >= 1L &&
    i <= length(occupied) && occupied[[i]] > 0L

  # Global relocation modes: `to = "random_empty"` or `"random_cell"`
  if (step$mode %in% c("random_empty", "random_cell")) {
    ncell <- lat$ncell
    for (k in seq_len(n_movers)) {
      gi <- grp[[k]]; ri <- row[[k]]
      here <- cells[[gi]][[ri]]
      if (is.na(here)) next
      if (step$avoid_occupied) {
        free <- which(occupied == 0L)
        if (!length(free)) next
        target <- free[[if (length(free) == 1L) 1L else sample.int(length(free), 1L)]]
        occupied[[here]] <- occupied[[here]] - 1L
        occupied[[target]] <- occupied[[target]] + 1L
      } else {
        target <- sample.int(ncell, 1L)
      }
      cells[[gi]][[ri]] <- target
    }
    for (i in seq_along(which_groups)) {
      state$groups[[which_groups[[i]]]] <-
        set_col(state$groups[[which_groups[[i]]]], ".cell", as.integer(cells[[i]]))
    }
    return(sync_cell_coords(state))
  }

  if (identical(step$mode, "direction")) {
    dir_cvars <- cell_vars(list(step$direction))
    headings <- lapply(which_groups, function(nm) {
      g <- state$groups[[nm]]
      aug <- if (length(dir_cvars)) attach_cell_columns(g, state, dir_cvars) else g
      v <- eval_rule(list(quo = step$direction), aug, state$globals,
                     grouped = FALSE)
      if (length(v) == 1L) v <- rep(v, nrow(g))
      d <- heading_names(v)
      bad <- unique(d[!is.na(d) & !d %in% names(lat$dir_nb)])
      if (length(bad)) {
        abm_abort(
          c("{.arg direction} holds a heading the lattice has no side for.",
            "x" = "Got {.val {bad}} in group {.field {nm}}.",
            "i" = "A heading is {.val {names(lat$dir_nb)}}, or {.code 0} north, {.code 1} east, {.code 2} south, {.code 3} west."),
          class = "tidyABM_bad_heading"
        )
      }
      d
    })
    for (k in seq_len(n_movers)) {
      gi <- grp[[k]]; ri <- row[[k]]
      here <- cells[[gi]][[ri]]
      if (is.na(here)) next
      d <- headings[[gi]][[ri]]
      if (is.na(d)) next
      target <- lat$dir_nb[[d]][here]
      if (is.na(target)) next
      if (step$avoid_occupied) {
        if (target != here && taken(target)) next
        occupied[[here]] <- occupied[[here]] - 1L
        occupied[[target]] <- occupied[[target]] + 1L
      }
      cells[[gi]][[ri]] <- target
    }
    for (i in seq_along(which_groups)) {
      state$groups[[which_groups[[i]]]] <-
        set_col(state$groups[[which_groups[[i]]]], ".cell", as.integer(cells[[i]]))
    }
    return(sync_cell_coords(state))
  }

  # Per-agent `range = <col/expr>` if supplied
  ranges <- NULL
  if (!is.null(step$range_quo)) {
    rng_cvars <- cell_vars(list(step$range_quo))
    ranges <- lapply(which_groups, function(nm) {
      g <- state$groups[[nm]]
      aug <- if (length(rng_cvars)) attach_cell_columns(g, state, rng_cvars) else g
      v <- eval_rule(list(quo = step$range_quo), aug, state$globals, grouped = FALSE)
      if (!is.numeric(v)) {
        abm_abort("{.arg range} must evaluate to whole numbers.", class = "tidyABM_bad_arg")
      }
      if (length(v) == 1L) v <- rep(v, nrow(g))
      as.integer(round(v))
    })
  }

  # The candidates of every mover, before any occupancy claim: adjacency is
  # fixed for the step, so this is done up front, and the common case (the
  # adjacent cells, nothing more) skips the general scan
  simple <- is.null(ranges) && step$range <= 1L && !step$axes_only
  n_nb <- length(nb)
  if (simple && !gradient && !step$avoid_occupied) {
    # Nobody's choice depends on anybody else's here, so when every mover has
    # the same number of cells to choose among they all draw in one call: the
    # same stream as one `sample.int()` per mover, without the loop.
    off <- cumsum(c(0L, lengths(cells)))
    at <- off[grp] + row
    flat <- unlist(cells, use.names = FALSE)
    here <- flat[at]
    if (!anyNA(here) && all(here >= 1L & here <= n_nb)) {
      cand <- nb[here]
      k <- length(cand[[1L]])
      to <- unlist(cand, use.names = FALSE)
      if (k > 1L && all(lengths(cand) == k) && !anyNA(to)) {
        flat[at] <- to[(seq_len(n_movers) - 1L) * k + sample.int(k, n_movers, replace = TRUE)]
        for (i in seq_along(which_groups)) {
          state$groups[[which_groups[[i]]]] <- set_col(
            state$groups[[which_groups[[i]]]], ".cell",
            as.integer(flat[off[[i]] + seq_len(off[[i + 1L]] - off[[i]])]))
        }
        return(sync_cell_coords(state))
      }
    }
  }
  cands <- vector("list", n_movers)
  for (k in seq_len(n_movers)) {
    gi <- grp[[k]]; ri <- row[[k]]
    here <- cells[[gi]][[ri]]
    if (is.na(here)) next
    rk <- if (is.null(ranges)) step$range else ranges[[gi]][[ri]]
    cand <- if (simple) {
      if (here >= 1L && here <= n_nb) nb[[here]] %||% integer() else integer()
    } else {
      candidate_cells(here, nb, lat, rk, step$axes_only)
    }
    if (gradient) cand <- unique(c(here, cand))
    cands[[k]] <- cand[!is.na(cand)]
  }

  # An elementwise `uphill()` expression is evaluated once over every mover's
  # candidates, in mover order; anything else once per mover, inside the loop
  ctx <- NULL
  vals <- NULL
  need_dist <- gradient && ".dist" %in% all.vars(rlang::quo_get_expr(step$expr))
  sp_dist <- state$space %||% lat
  if (gradient) {
    ctx <- gradient_context(step, state$globals)
    if (ctx$batch && length(which_groups) == 1L) {
      lens <- lengths(cands)
      if (sum(lens)) {
        own <- lapply(gcols[[1L]], `[`, row)
        view <- gradient_view(unlist(cands, use.names = FALSE), wired, own, lens,
                              space = sp_dist, need_dist = need_dist)
        flat <- eval_gradient(ctx, view, sum(lens))
        vals <- split(flat, factor(rep.int(seq_len(n_movers), lens),
                                 levels = seq_len(n_movers)))
      }
    }
  }

  for (k in seq_len(n_movers)) {
    gi <- grp[[k]]; ri <- row[[k]]
    here <- cells[[gi]][[ri]]
    if (is.na(here)) next
    cand <- cands[[k]]
    keep <- NULL
    if (step$avoid_occupied && length(cand)) {
      keep <- cand == here | !vapply(cand, taken, logical(1))
      cand <- cand[keep]
    }
    if (!length(cand)) next

    if (gradient) {
      val <- if (!is.null(vals)) {
        v <- vals[[k]]
        if (is.null(keep)) v else v[keep]
      } else {
        eval_gradient(ctx, gradient_view(cand, wired, lapply(gcols[[gi]], `[`, ri),
                                         length(cand), space = sp_dist,
                                         need_dist = need_dist), length(cand))
      }
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
    state$groups[[which_groups[[i]]]] <-
      set_col(state$groups[[which_groups[[i]]]], ".cell", as.integer(cells[[i]]))
  }
  sync_cell_coords(state)
}

#' @export
print.abm_move <- function(x, ...) {
  dest <- if (!is.null(x$direction)) {
    paste0("direction = ", deparse1(rlang::quo_get_expr(x$direction)))
  } else if (!is.null(x$expr)) {
    paste0(x$mode, "(", deparse1(rlang::quo_get_expr(x$expr)), ")")
  } else if (!is.null(x$move_args)) {
    paste0(x$mode, "(", paste(vapply(x$move_args, function(q) deparse1(rlang::quo_get_expr(q)), ""), collapse = ", "), ")")
  } else {
    x$to
  }
  along_txt <- if (is.null(x$along)) "" else cli::format_inline(" along {.val {x$along}}")
  cli::cli_text("{.cls abm_move}{along_txt} to {.emph {dest}}")
  bits <- c(
    if (!is.null(x$who)) "who = {.val {x$who}}",
    if (!is.null(x$when)) "when = {.code {deparse1(rlang::quo_get_expr(x$when))}}",
    if (!is.null(x$range_quo)) "range = {.code {deparse1(rlang::quo_get_expr(x$range_quo))}}",
    if (!is.null(x$range) && x$range > 1L) "range = {x$range}",
    if (x$axes_only) "axes_only = TRUE",
    if (x$avoid_occupied) "avoid_occupied = TRUE"
  )
  if (length(bits)) cli::cli_bullets(stats::setNames(bits, rep("*", length(bits))))
  invisible(x)
}
