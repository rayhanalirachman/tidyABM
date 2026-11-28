# Spatial geometry: discrete grids and continuous spaces ------------------

new_abm_space <- function(type, dims, diagonals = NULL, torus = TRUE, on = NULL) {
  structure(
    list(type = type, dims = dims, diagonals = diagonals, torus = torus, on = on),
    class = "abm_space"
  )
}

#' Declare a spatial geometry for a model
#'
#' `abm_space()` separates spatial geometry from social/graph connections
#' ([abm_network()]), so a model can have a discrete grid, a continuous 1-D or
#' 2-D world, or both a space and a network at the same time.
#'
#' @param type `"grid"` for a discrete 1-D or 2-D lattice of cells, or
#'   `"continuous"` for a continuous 1-D or 2-D coordinate box `[0, dims)`.
#' @param dims Numeric vector of length 1 (1-D) or 2 (2-D). Positive whole
#'   numbers for `"grid"`, positive numbers for `"continuous"`.
#' @param diagonals For 2-D `"grid"` only: `TRUE` (default) for 8-neighbour
#'   Moore adjacency, `FALSE` for 4-neighbour von Neumann.
#' @param torus Whether boundaries wrap. A single logical (applied to all axes)
#'   or a logical vector of length `length(dims)` (per-axis wrapping).
#' @param on For `"grid"` only: optional name of an agent group to wire as
#'   patches. When omitted, a lone group without `n` is wired automatically;
#'   when every group specifies `n`, the grid is patchless (cells exist as
#'   coordinate/occupancy slots without a patch group).
#'
#' @return An `abm_space` specification object.
#' @export
#' @examples
#' abm_space(type = "grid", dims = c(20, 20), diagonals = FALSE, torus = TRUE)
#' abm_space(type = "continuous", dims = c(100, 100), torus = c(TRUE, FALSE))
abm_space <- function(type = c("grid", "continuous"), dims, diagonals = NULL,
                      torus = TRUE, on = NULL) {
  type <- rlang::arg_match(type)
  if (missing(dims) || is.null(dims) || !is.numeric(dims) ||
      !length(dims) %in% 1:2 || anyNA(dims) || !all(is.finite(dims)) ||
      any(dims <= 0)) {
    abm_abort(
      c("{.arg dims} must be 1 or 2 positive numbers.",
        "i" = "For example {.code dims = 100} (1-D) or {.code dims = c(50, 50)} (2-D)."),
      class = "tidyABM_bad_dims"
    )
  }
  if (type == "grid") {
    if (any(dims != as.integer(dims)) || any(dims < 1)) {
      abm_abort("{.arg dims} must be positive whole numbers for {.code type = \"grid\"}.",
                class = "tidyABM_bad_dims")
    }
    dims <- as.integer(dims)
    if (length(dims) == 1L && !is.null(diagonals)) {
      abm_abort(
        c("{.arg diagonals} only applies to 2-D grids.",
          "i" = "Drop {.arg diagonals}, or pass two dimensions."),
        class = "tidyABM_irrelevant_arg"
      )
    }
    if (!is.null(diagonals) && !rlang::is_bool(diagonals)) {
      abm_abort("{.arg diagonals} must be {.code TRUE} or {.code FALSE}.",
                class = "tidyABM_bad_arg")
    }
    if (!is.null(on) && !rlang::is_string(on)) {
      abm_abort("{.arg on} must be a single group name.",
                class = "tidyABM_bad_arg")
    }
    diagonals <- if (length(dims) == 2L) diagonals %||% TRUE else NULL
  } else {
    dims <- as.double(dims)
    if (!is.null(diagonals)) {
      abm_abort('{.arg diagonals} is not used when {.code type = "continuous"}.',
                class = "tidyABM_irrelevant_arg")
    }
    if (!is.null(on)) {
      abm_abort('{.arg on} is not used when {.code type = "continuous"}.',
                class = "tidyABM_irrelevant_arg")
    }
  }
  if (!is.logical(torus) || anyNA(torus) || !length(torus) %in% c(1L, length(dims))) {
    abm_abort(
      c("{.arg torus} must be a single logical or one logical per dimension.",
        "x" = "Got length {length(torus)} for {length(dims)}-D space."),
      class = "tidyABM_bad_arg"
    )
  }
  torus <- rep_len(torus, length(dims))
  new_abm_space(type, dims, diagonals = diagonals, torus = torus, on = on)
}

#' @export
print.abm_space <- function(x, ...) {
  cli::cli_text('{.cls abm_space} type {.val {x$type}}')
  cli::cli_bullets(c("*" = "dims = {paste(x$dims, collapse = ' x ')}"))
  if (!is.null(x$diagonals)) cli::cli_bullets(c("*" = "diagonals = {x$diagonals}"))
  cli::cli_bullets(c("*" = "torus = {paste(x$torus, collapse = ', ')}"))
  if (!is.null(x$on)) cli::cli_bullets(c("*" = "on = {.field {x$on}}"))
  invisible(x)
}

# --- continuous space setup ----------------------------------------------

#' Set up a model in continuous 1-D or 2-D space
#' @noRd
setup_continuous <- function(specs, space, network, globals,
                             call = rlang::caller_env()) {
  groups <- list()
  offset <- 0L
  for (nm in names(specs)) {
    s <- specs[[nm]]
    if (is.null(s$n)) {
      abm_abort(
        c("Group {.field {nm}} needs an {.arg n}.",
          "i" = 'Continuous space has no patches, so every group specifies {.arg n}.'),
        class = "tidyABM_bad_n", call = call
      )
    }
    g <- materialise_agents(s, nm, id_offset = offset, globals = globals,
                            call = call)
    g <- place_in_continuous(g, s, nm, space, globals, call = call)
    groups[[nm]] <- g
    offset <- offset + s$n
  }
  edges <- materialise_network(network, n = offset, groups = groups, call = call)
  model <- new_abm_model(groups, as.list(globals), edges, network,
                         lattice = NULL, relations = NULL, space = space)
  model
}

#' Assign `.x` (and `.y` in 2-D) in `[0, dims)` to a group in continuous space
#' @noRd
place_in_continuous <- function(g, spec, group_name, space, globals,
                                call = rlang::caller_env()) {
  n <- nrow(g)
  dims <- space$dims
  nd <- length(dims)
  eval_coord <- function(val, env, axis_name, bound) {
    if (is_formula1(val)) {
      q <- rlang::as_quosure(val)
      q <- rlang::quo_set_env(q, setup_eval_env(q, n, globals))
      v <- rlang::eval_tidy(q, data = c(as.list(g), list(n = n, dims = dims)))
    } else if (rlang::is_quosure(val)) {
      q <- rlang::quo_set_env(val, setup_eval_env(val, n, globals))
      v <- rlang::eval_tidy(q, data = c(as.list(g), list(n = n, dims = dims)))
    } else {
      v <- val
    }
    if (length(v) == 1L) v <- rep(v, n)
    if (length(v) != n) {
      abm_abort(
        c("{.arg at} ({axis_name}) for group {.field {group_name}} has the wrong length.",
          "x" = "Returned {length(v)} value{?s}, expected {n}."),
        class = "tidyABM_bad_column_length", call = call
      )
    }
    if (!is.numeric(v) || anyNA(v) || !all(is.finite(v)) ||
        any(v < 0 | v > bound)) {
      abm_abort(
        c("{.arg at} ({axis_name}) for group {.field {group_name}} must lie in {.code [0, {bound}]}.",
          "x" = "Got values outside the space or {.val NA}."),
        class = "tidyABM_bad_arg", call = call
      )
    }
    as.double(v)
  }

  if (is.null(spec$at)) {
    g$.x <- stats::runif(n, 0, dims[[1]])
    if (nd == 2L) g$.y <- stats::runif(n, 0, dims[[2]])
    return(g)
  }
  at_expr <- rlang::quo_get_expr(spec$at)
  at_env <- rlang::quo_get_env(spec$at)
  if (nd == 2L) {
    val <- rlang::eval_tidy(
      rlang::quo_set_env(spec$at, setup_eval_env(spec$at, n, globals)),
      data = c(as.list(g), list(n = n, dims = dims))
    )
    if (!is.list(val) || !all(c("x", "y") %in% names(val))) {
      abm_abort(
        c("In a 2-D continuous space, {.arg at} must be {.code list(x = ~..., y = ~...)}.",
          "x" = "Got {.cls {class(val)[[1]]}}."),
        class = "tidyABM_bad_arg", call = call
      )
    }
    g$.x <- eval_coord(val$x, at_env, "x", dims[[1]])
    g$.y <- eval_coord(val$y, at_env, "y", dims[[2]])
  } else {
    g$.x <- eval_coord(spec$at, at_env, "x", dims[[1]])
  }
  g
}

# --- torus-aware Euclidean distance & AST cell bucketing -----------------

#' Euclidean distance between `(x1, y1)` and `(x2, y2)`, wrapping along torus axes
#' @noRd
space_distance <- function(x1, y1, x2, y2, space) {
  dims <- space$dims
  torus <- if (is.logical(space$torus)) rep_len(space$torus, length(dims)) else rep(FALSE, length(dims))
  dx <- abs(as.double(x1) - as.double(x2))
  if (torus[[1]]) dx <- pmin(dx, dims[[1]] - dx)
  if (length(dims) == 1L || is.null(y1) || is.null(y2)) {
    return(dx)
  }
  dy <- abs(as.double(y1) - as.double(y2))
  if (torus[[2]]) dy <- pmin(dy, dims[[2]] - dy)
  sqrt(dx * dx + dy * dy)
}

#' Recognise `.dist <= r` or `.dist < r` among `&`-conjuncts for cell bucketing
#'
#' Returns `list(radius_expr, strict, rest)` or `NULL`.
#' @noRd
dist_bound_key <- function(expr) {
  parts <- and_conjuncts(expr)
  for (i in seq_along(parts)) {
    p <- parts[[i]]
    if (!is.call(p) || length(p) != 3L || !is.symbol(p[[1]])) next
    op <- as.character(p[[1]])
    lhs <- p[[2]]; rhs <- p[[3]]
    if (op %in% c("<=", "<") && identical(lhs, as.name(".dist"))) {
      return(list(radius_expr = rhs, strict = op == "<", rest = parts[-i]))
    }
    if (op %in% c(">=", ">") && identical(rhs, as.name(".dist"))) {
      return(list(radius_expr = lhs, strict = op == ">", rest = parts[-i]))
    }
  }
  NULL
}

#' Spatial cell bucketing for `.dist <= r` / `.dist < r`
#'
#' Bins candidates into buckets of width `r` and only checks each focal agent
#' against its 3 (1-D) or 3x3 (2-D) neighbouring buckets (wrapping on torus
#' axes), returning `list(focal_idx, cand_idx)` of exact distance-passing pairs.
#' Returns `NULL` when bucketing is not applicable (caller uses full cross product).
#' @noRd
spatial_radius_pairs <- function(combined, focal_idx, cand_idx, radius_quo,
                                 strict, globals, space) {
  if (is.null(space) || !".x" %in% names(combined)) return(NULL)
  if (!length(focal_idx) || !length(cand_idx)) {
    return(list(focal_idx = integer(), cand_idx = integer()))
  }
  # Radius must be a scalar independent of candidate/focal columns
  rvars <- all.vars(rlang::quo_get_expr(radius_quo))
  if (any(rvars %in% names(combined)) || any(startsWith(rvars, "own_"))) return(NULL)
  r <- tryCatch(
    rlang::eval_tidy(rlang::quo_get_expr(radius_quo),
                     env = abm_eval_env(radius_quo, globals, n = nrow(combined))),
    error = function(e) NULL
  )
  if (!is.numeric(r) || length(r) != 1L || is.na(r) || !is.finite(r)) return(NULL)
  if (r < 0) return(list(focal_idx = integer(), cand_idx = integer()))

  dims <- as.double(space$dims)
  torus <- rep_len(as.logical(space$torus), length(dims))
  nd <- length(dims)
  # Only bucket when the radius is smaller than a third of every dimension
  cell_w <- max(r, 1e-9)
  nx_b <- as.integer(floor(dims[[1]] / cell_w))
  if (nx_b < 3L) return(NULL)
  ny_b <- if (nd == 2L) as.integer(floor(dims[[2]] / cell_w)) else 1L
  if (nd == 2L && ny_b < 3L) return(NULL)

  # Grid coords are 1..w; continuous are [0, w). Shift grid by -1 for [0, w).
  shift <- if (identical(space$type, "grid") || identical(space$type, "line")) 1 else 0
  x_all <- as.double(combined$.x) - shift
  y_all <- if (nd == 2L) as.double(combined$.y) - shift else NULL
  bx_w <- dims[[1]] / nx_b
  by_w <- if (nd == 2L) dims[[2]] / ny_b else 1

  bucket_1d <- function(v, w, nb) {
    b <- as.integer(floor(v / w))
    pmin(pmax(b, 0L), nb - 1L)
  }

  cx <- x_all[cand_idx]
  fx <- x_all[focal_idx]
  cbx <- bucket_1d(cx, bx_w, nx_b)
  fbx <- bucket_1d(fx, bx_w, nx_b)
  if (nd == 2L) {
    cy <- y_all[cand_idx]; fy <- y_all[focal_idx]
    cby <- bucket_1d(cy, by_w, ny_b)
    fby <- bucket_1d(fy, by_w, ny_b)
    c_key <- cbx + cby * nx_b + 1L
    n_b <- nx_b * ny_b
  } else {
    c_key <- cbx + 1L
    n_b <- nx_b
  }
  valid_c <- !is.na(c_key)
  if (!any(valid_c)) return(list(focal_idx = integer(), cand_idx = integer()))
  buckets <- split(cand_idx[valid_c], factor(c_key[valid_c], levels = seq_len(n_b)))

  # Neighbouring bucket offsets
  dx_off <- -1:1
  dy_off <- if (nd == 2L) -1:1 else 0L

  out_f <- vector("list", length(focal_idx))
  out_c <- vector("list", length(focal_idx))
  for (i in seq_along(focal_idx)) {
    if (is.na(fbx[[i]]) || (nd == 2L && is.na(fby[[i]]))) next
    bx <- fbx[[i]] + dx_off
    if (torus[[1]]) bx <- ((bx %% nx_b) + nx_b) %% nx_b
    else bx <- bx[bx >= 0L & bx < nx_b]
    if (!length(bx)) next

    if (nd == 2L) {
      by <- fby[[i]] + dy_off
      if (torus[[2]]) by <- ((by %% ny_b) + ny_b) %% ny_b
      else by <- by[by >= 0L & by < ny_b]
      if (!length(by)) next
      keys <- unique(as.integer(outer(bx, by * nx_b, "+")) + 1L)
    } else {
      keys <- unique(as.integer(bx) + 1L)
    }
    cands <- unlist(buckets[keys], use.names = FALSE)
    if (!length(cands)) next
    # Preserve candidate order (ascending index) to match cross product order
    cands <- sort(cands)
    fi <- focal_idx[[i]]
    d <- space_distance(
      combined$.x[cands], if (nd == 2L) combined$.y[cands] else NULL,
      combined$.x[[fi]], if (nd == 2L) combined$.y[[fi]] else NULL,
      space
    )
    hit <- if (strict) d < r else d <= r
    hit[is.na(hit)] <- FALSE
    if (any(hit)) {
      kept <- cands[hit]
      out_f[[i]] <- rep.int(fi, length(kept))
      out_c[[i]] <- kept
    }
  }
  fi_out <- unlist(out_f, use.names = FALSE)
  ci_out <- unlist(out_c, use.names = FALSE)
  list(focal_idx = fi_out %||% integer(), cand_idx = ci_out %||% integer())
}

#' Resolve which edge table (`state$edges` vs `state$space_edges`) a step reads
#' @noRd
resolve_topology_edges <- function(via, state, fn, call = rlang::caller_env()) {
  has_net <- !is.null(state$edges)
  has_sp <- !is.null(state$space_edges)
  if (!is.null(via)) {
    if (via == "space") {
      if (has_sp) return(list(edges = state$space_edges, cache = "space_nb_cache"))
      if (!is.null(state$lattice) && has_net) return(list(edges = state$edges, cache = "nb_cache"))
      abm_abort(
        c('{.code via = "space"} requires a grid space with wired patches.',
          "i" = 'Add {.code space = abm_space(type = "grid", dims = ..., on = "patches")}.'),
        class = "tidyABM_no_lattice", call = call
      )
    }
    # via == "network"
    if (has_net && (is.null(state$lattice) || has_sp)) {
      return(list(edges = state$edges, cache = "nb_cache"))
    }
    abm_abort(
      c('{.code via = "network"} requires a network.',
        "i" = "Add one with {.code abm_setup(..., network = abm_network(...))}."),
      class = "tidyABM_no_network", call = call
    )
  }
  if (has_net && has_sp) {
    abm_abort(
      c("{.fn {fn}} is ambiguous because this model has both a grid {.arg space} and a {.arg network}.",
        "i" = 'Specify {.code via = "space"} or {.code via = "network"}.'),
      class = "tidyABM_ambiguous_topology", call = call
    )
  }
  if (has_net) return(list(edges = state$edges, cache = "nb_cache"))
  if (has_sp) return(list(edges = state$space_edges, cache = "space_nb_cache"))
  NULL
}
