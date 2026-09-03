# Match execution --------------------------------------------------------

empty_match <- function() {
  tibble::tibble(.id = integer(), .partner = integer(),
                 .role = character(), .group_id = integer())
}

#' Evaluate a condition quosure against the agent tibble + globals
#' @noRd
eval_condition <- function(quo, agents, globals) {
  if (is.null(quo)) return(rep(TRUE, nrow(agents)))
  # Conditions go through the same data-masking path as rules, so that `n()`,
  # `if_else()` and friends mean the same thing in `when =` as in `abm_rules()`.
  out <- eval_rule(list(quo = quo), aug = agents, globals = globals,
                   grouped = FALSE)
  if (length(out) == 1L) out <- rep(out, nrow(agents))
  out & !is.na(out)
}

#' Split a vector of ids into consecutive groups of `size`
#' @noRd
chunk_ids <- function(ids, size) {
  n_full <- length(ids) %/% size
  if (n_full == 0L) return(list(ids = integer(), gid = integer()))
  keep <- ids[seq_len(n_full * size)]
  list(ids = keep, gid = rep(seq_len(n_full), each = size))
}

#' Assign two roles within pairs
#'
#' Returns a character vector aligned with `a`/`b`, or NA for pairs where no
#' assignment satisfies both conditions.
#' @noRd
assign_roles <- function(role_quo, agents, globals, a_idx, b_idx) {
  exprs <- as.list(rlang::quo_get_expr(role_quo))[-1]
  nms <- names(exprs)
  env <- rlang::quo_get_env(role_quo)
  conds <- lapply(exprs, function(e) {
    eval_condition(rlang::new_quosure(e, env), agents, globals)
  })
  c1 <- conds[[1]]; c2 <- conds[[2]]

  ab <- c1[a_idx] & c2[b_idx]   # a takes role 1
  ba <- c1[b_idx] & c2[a_idx]   # b takes role 1
  both <- ab & ba
  if (any(both)) {
    flip <- stats::runif(sum(both)) < 0.5
    ab[both][flip] <- FALSE
  }
  role_a <- ifelse(ab, nms[[1]], ifelse(ba, nms[[2]], NA_character_))
  role_b <- ifelse(ab, nms[[2]], ifelse(ba, nms[[1]], NA_character_))
  list(a = role_a, b = role_b, ok = !is.na(role_a))
}

#' Run one abm_match step
#'
#' Returns a tibble of .id/.partner/.role/.group_id, for participating agents
#' only. A match decides who meets whom; it never writes an agent column.
#' @noRd
run_match <- function(spec, agents, edges, globals, call = rlang::caller_env()) {
  if (!is.null(spec$dot_by)) return(run_match_by(spec, agents, edges, globals, call))
  eligible <- eval_condition(spec$eligible, agents, globals)
  pool <- agents$.id[eligible]
  # `eligible` says who takes part; `among` says who may be picked. They are
  # different questions for the directional modes, where choosing is one-way.
  ch <- choice_set(spec, agents, globals, pool, call)

  res <- switch(
    spec$pair,
    random         = match_random(spec, agents, globals, pool),
    one_of         = match_one_of(spec, agents, globals, pool, ch),
    opposite_group = match_opposite(spec, agents, globals, pool, call),
    nearest        = match_nearest(spec, agents, globals, pool, ch, call),
    network        = match_network(spec, agents, edges, pool)
  )
  res
}

#' Run a match once inside each partition of a `.by` column
#'
#' The whole match is replayed per partition -- eligibility, roles, the draw --
#' so an agent is only ever paired with an agent sharing the column's value.
#' `.group_id`s are re-based across partitions so that grouped rules still see
#' one group per pair. `NA` is not a partition: an agent with no value sits the
#' step out, the way it does in `abm_rules(.by =)`.
#' @noRd
run_match_by <- function(spec, agents, edges, globals, call) {
  by <- by_columns(spec$dot_by, call)
  if (length(by) != 1L || !by %in% names(agents)) {
    abm_abort(
      c("{.arg .by} must name one existing agent column.",
        "x" = "No column {.field {by}}."),
      class = "tidyABM_missing_column", call = call
    )
  }
  inner <- spec
  inner$dot_by <- NULL

  key <- agents[[by]]
  parts <- split(seq_len(nrow(agents)), key)          # `split()` drops NA keys
  out <- vector("list", length(parts))
  offset <- 0L
  for (i in seq_along(parts)) {
    sub <- agents[parts[[i]], , drop = FALSE]
    if (nrow(sub) == 0L) next
    m <- run_match(inner, sub, edges, globals, call)
    if (nrow(m) == 0L) next
    m$.group_id <- m$.group_id + offset
    offset <- max(m$.group_id)
    out[[i]] <- m
  }
  out <- Filter(Negate(is.null), out)
  if (!length(out)) return(empty_match())
  dplyr::bind_rows(out)
}

#' Does a condition ask about the pair rather than about the candidate?
#'
#' `among` and `weight` are ordinary population conditions until one of them
#' mentions an `own_<col>`. That is the signal that the question is "may *I*
#' pick this one", not "may anyone", and it is the same signal `cost` gives.
#' @noRd
mentions_own <- function(quo) {
  if (is.null(quo)) return(FALSE)
  any(startsWith(all.vars(rlang::quo_get_expr(quo)), "own_"))
}

#' Who each chooser may pick, and with what weight
#'
#' Returns `ids` (the candidates, in agent order), `ok` (`NULL` when every
#' chooser shares one candidate set, otherwise a chooser x candidate logical
#' matrix whose rows follow `pool`) and `w` (`NULL`, a vector over `ids`, or a
#' matrix shaped like `ok`).
#' @noRd
choice_set <- function(spec, agents, globals, pool, call = rlang::caller_env()) {
  if (!spec$pair %in% c("one_of", "nearest")) return(NULL)
  pairwise <- mentions_own(spec$among) || mentions_own(spec$weight)

  if (!pairwise) {
    ids <- agents$.id[eval_condition(spec$among, agents, globals)]
    w <- NULL
    if (!is.null(spec$weight)) {
      wv <- eval_rule(list(quo = spec$weight), aug = agents, globals = globals,
                      grouped = FALSE)
      if (length(wv) == 1L) wv <- rep(wv, nrow(agents))
      w <- check_weight(wv, spec, call)[match(ids, agents$.id)]
    }
    return(list(ids = ids, ok = NULL, w = w))
  }

  # one row per (chooser, candidate): the candidate's columns under their own
  # names, the chooser's under `own_`. The same view `cost` minimises over, so
  # a comparison written for one means the same thing in the other.
  focal <- which(agents$.id %in% pool)
  if (!length(focal)) return(list(ids = agents$.id, ok = NULL, w = NULL))
  ns <- length(focal); nc <- nrow(agents)
  view <- pair_view(agents,
                    focal_idx = rep(focal, each = nc),
                    cand_idx  = rep(seq_len(nc), times = ns))

  ok <- if (is.null(spec$among)) rep(TRUE, nrow(view))
        else eval_over_view(spec$among, view, globals)
  if (!is.logical(ok)) {
    abm_abort(
      c("{.arg among} must be a condition.",
        "x" = "{.code {deparse1(rlang::quo_get_expr(spec$among))}} returned {.cls {class(ok)[[1]]}}."),
      class = "tidyABM_bad_among", call = call
    )
  }
  if (length(ok) == 1L) ok <- rep(ok, nrow(view))
  ok[is.na(ok)] <- FALSE

  w <- NULL
  if (!is.null(spec$weight)) {
    wv <- eval_over_view(spec$weight, view, globals)
    if (length(wv) == 1L) wv <- rep(wv, nrow(view))
    w <- matrix(check_weight(wv, spec, call), nrow = ns, byrow = TRUE)
  }
  list(ids = agents$.id, ok = matrix(ok, nrow = ns, byrow = TRUE), w = w)
}

#' @noRd
check_weight <- function(w, spec, call) {
  if (!is.numeric(w)) {
    abm_abort(
      c("{.arg weight} must be numeric.",
        "x" = "{.code {deparse1(rlang::quo_get_expr(spec$weight))}} returned {.cls {class(w)[[1]]}}."),
      class = "tidyABM_bad_weight", call = call
    )
  }
  w[is.na(w)] <- 0
  pmax(w, 0)
}

#' Draw one candidate per chooser, honouring a per-chooser set and weights
#' @noRd
draw_choices <- function(pool, ch) {
  ids <- ch$ids
  out <- rep(NA_integer_, length(pool))
  for (i in seq_along(pool)) {
    keep <- if (is.null(ch$ok)) rep(TRUE, length(ids)) else ch$ok[i, ]
    keep <- keep & ids != pool[[i]]
    if (!any(keep)) next
    cand <- ids[keep]
    if (is.null(ch$w)) {
      out[[i]] <- cand[[sample.int(length(cand), 1L)]]
    } else {
      wi <- if (is.matrix(ch$w)) ch$w[i, keep] else ch$w[keep]
      if (sum(wi) <= 0) next
      out[[i]] <- cand[[sample.int(length(cand), 1L, prob = wi)]]
    }
  }
  out
}

match_random <- function(spec, agents, globals, pool) {
  pool <- shuffle(pool)
  ch <- chunk_ids(pool, spec$size)
  if (length(ch$ids) == 0L) return(empty_match())

  if (spec$size == 2L) {
    a <- ch$ids[c(TRUE, FALSE)]
    b <- ch$ids[c(FALSE, TRUE)]
    a_idx <- match(a, agents$.id); b_idx <- match(b, agents$.id)

    if (is.null(spec$role)) {
      m <- tibble::tibble(
        .id = c(a, b), .partner = c(b, a),
        .role = NA_character_, .group_id = rep(seq_along(a), 2)
      )
    } else {
      r <- assign_roles(spec$role, agents, globals, a_idx, b_idx)
      keep <- r$ok
      m <- tibble::tibble(
        .id = c(a[keep], b[keep]), .partner = c(b[keep], a[keep]),
        .role = c(r$a[keep], r$b[keep]),
        .group_id = rep(seq_len(sum(keep)), 2)
      )
    }
    return(m)
  }

  m <- tibble::tibble(.id = ch$ids, .partner = NA_integer_,
                      .role = NA_character_, .group_id = ch$gid)
  m
}

match_one_of <- function(spec, agents, globals, pool, ch = NULL) {
  ch <- ch %||% list(ids = agents$.id, ok = NULL, w = NULL)
  candidates <- ch$ids
  if (length(pool) == 0L || length(candidates) == 0L) {
    return(empty_match())
  }
  if (is.null(ch$ok) && is.null(ch$w)) {
    # NetLogo's `one-of other turtles`: each eligible agent draws a partner from
    # the candidate pool, itself excluded. Directional, so A may pick B while B
    # picks someone else entirely.
    # An agent that is its own only candidate has nobody to pick and sits out.
    usable <- pool[!(pool %in% candidates) | length(candidates) > 1L]
    pool <- usable
    if (length(pool) == 0L) return(empty_match())
    draw <- draw_from(candidates, length(pool))
    clash <- draw == pool
    while (any(clash)) {
      draw[clash] <- draw_from(candidates, sum(clash))
      clash <- draw == pool
    }
  } else {
    # a candidate set, or a weighting, that belongs to the chooser: it draws
    # one agent at a time rather than one draw for the whole pool
    draw <- draw_choices(pool, ch)
    got <- !is.na(draw)
    pool <- pool[got]; draw <- draw[got]
    if (length(pool) == 0L) return(empty_match())
  }
  idx <- match(pool, agents$.id)

  if (is.null(spec$role)) {
    role_a <- rep(NA_character_, length(pool))
    role_b <- role_a
    keep <- rep(TRUE, length(pool))
  } else {
    r <- assign_roles(spec$role, agents, globals, idx, match(draw, agents$.id))
    role_a <- r$a; role_b <- r$b; keep <- r$ok
  }

  m <- tibble::tibble(
    .id = pool[keep], .partner = draw[keep], .role = role_a[keep],
    .group_id = seq_len(sum(keep))
  )
  m
}

match_opposite <- function(spec, agents, globals, pool, call) {
  by <- by_columns(spec$by, call)
  if (length(by) != 1L) {
    abm_abort('{.arg by} must name one column when {.code pair = "opposite_group"}.',
              class = "tidyABM_bad_by", call = call)
  }
  if (!by %in% names(agents)) {
    abm_abort(c("Column {.field {by}} not found.",
                "i" = "For a multi-group model use {.code by = .group}."),
              class = "tidyABM_missing_column", call = call)
  }
  # the split is checked against the whole population, so that a mis-named
  # column is an error while a population that happens to be all one kind this
  # tick is simply a step where nobody pairs up
  all_vals <- sort(unique(agents[[by]]))
  if (length(all_vals) > 2L) {
    abm_abort(
      c("{.field {by}} must take at most two values for opposite-group matching.",
        "x" = "Across all agents it takes {length(all_vals)} values: {.val {all_vals}}."),
      class = "tidyABM_bad_by", call = call
    )
  }
  # a population that is all one kind this tick (every male dead, say) is a
  # step where nobody pairs up, not an error
  sub <- agents[agents$.id %in% pool, , drop = FALSE]
  vals <- all_vals
  if (length(all_vals) < 2L || length(unique(sub[[by]])) < 2L) {
    return(empty_match())
  }
  g1 <- shuffle(sub$.id[sub[[by]] == vals[[1]]])
  g2 <- shuffle(sub$.id[sub[[by]] == vals[[2]]])
  k <- min(length(g1), length(g2))
  if (k == 0L) return(empty_match())
  a <- g1[seq_len(k)]; b <- g2[seq_len(k)]
  a_idx <- match(a, agents$.id); b_idx <- match(b, agents$.id)

  if (is.null(spec$role)) {
    role_a <- rep(NA_character_, k); role_b <- role_a; keep <- rep(TRUE, k)
  } else {
    r <- assign_roles(spec$role, agents, globals, a_idx, b_idx)
    role_a <- r$a; role_b <- r$b; keep <- r$ok
  }

  m <- tibble::tibble(
    .id = c(a[keep], b[keep]), .partner = c(b[keep], a[keep]),
    .role = c(role_a[keep], role_b[keep]),
    .group_id = rep(seq_len(sum(keep)), 2)
  )

  m
}

match_nearest <- function(spec, agents, globals, pool, ch, call) {
  ch <- ch %||% list(ids = agents$.id, ok = NULL, w = NULL)
  if (!is.null(spec$cost)) {
    return(match_cheapest(spec, agents, globals, pool, ch, call))
  }
  candidates <- ch$ids
  by <- by_columns(spec$by, call)
  missing <- setdiff(by, names(agents))
  if (length(missing)) {
    abm_abort("Column{?s} {.field {missing}} not found.",
              class = "tidyABM_missing_column", call = call)
  }
  sub <- agents[agents$.id %in% pool, , drop = FALSE]
  cand <- agents[agents$.id %in% candidates, , drop = FALSE]
  if (nrow(sub) == 0L || nrow(cand) == 0L) {
    return(empty_match())
  }

  a <- as.matrix(sub[, by, drop = FALSE])
  b <- as.matrix(cand[, by, drop = FALSE])
  # squared Euclidean distance from every chooser to every candidate; only the
  # ordering matters, so the square root is not worth taking
  d <- outer(rowSums(a^2), rowSums(b^2), "+") - 2 * (a %*% t(b))
  d[outer(sub$.id, cand$.id, "==")] <- Inf   # never yourself
  # a candidate set of the chooser's own rules the rest out
  if (!is.null(ch$ok)) d[!ch$ok[, match(cand$.id, ch$ids), drop = FALSE]] <- Inf
  ok <- is.finite(matrixStats_rowMins(d))
  if (!any(ok)) return(empty_match())
  nearest <- max.col(-d, ties.method = "first")

  m <- tibble::tibble(
    .id = sub$.id[ok], .partner = cand$.id[nearest[ok]],
    .role = NA_character_, .group_id = seq_len(sum(ok))
  )
  m
}

#' `pair = "nearest"` with a cost expression rather than a coordinate
#'
#' One row per (chooser, candidate): the candidate's columns under their own
#' names, the chooser's under `own_<col>`. That is the same view
#' `abm_neighbours()` gives, and it is what lets the thing being minimised be a
#' delivered price or an energy deficit rather than a distance.
#' @noRd
match_cheapest <- function(spec, agents, globals, pool, ch, call) {
  candidates <- ch$ids
  sub  <- agents[agents$.id %in% pool, , drop = FALSE]
  cand <- agents[agents$.id %in% candidates, , drop = FALSE]
  if (nrow(sub) == 0L || nrow(cand) == 0L) {
    return(empty_match())
  }
  # every column, `.id` and `.group` included, so a cost can say "my rank of
  # this candidate" (`own_rank[[.]][.id]`) as well as "the price it charges"
  cols <- names(agents)
  ci <- rep(seq_len(nrow(cand)), times = nrow(sub))
  si <- rep(seq_len(nrow(sub)),  each  = nrow(cand))

  view <- cand[ci, cols, drop = FALSE]
  own  <- sub[si, cols, drop = FALSE]
  names(own) <- paste0("own_", cols)
  view <- dplyr::bind_cols(view, own)
  view$.chooser <- sub$.id[si]
  view$.candidate <- cand$.id[ci]

  quo <- spec$cost
  env <- rlang::quo_get_env(quo)
  if (length(globals)) env <- rlang::new_environment(globals, parent = env)
  quo <- rlang::quo_set_env(quo, env)
  val <- dplyr::pull(dplyr::mutate(view, .abm_cost = !!quo), ".abm_cost")
  if (length(val) == 1L) val <- rep(val, nrow(view))
  if (!is.numeric(val)) {
    abm_abort(
      c("{.arg cost} must be numeric.",
        "x" = "{.code {deparse1(rlang::quo_get_expr(spec$cost))}} returned {.cls {class(val)[[1]]}}."),
      class = "tidyABM_bad_cost", call = call
    )
  }
  val[view$.chooser == view$.candidate] <- NA_real_   # never yourself
  # a candidate set of the chooser's own rules the rest out
  if (!is.null(ch$ok)) val[!as.vector(t(ch$ok[, match(cand$.id, ch$ids), drop = FALSE]))] <- NA_real_
  keep <- !is.na(val)
  if (!any(keep)) return(empty_match())

  ord <- order(view$.chooser[keep], val[keep])
  ch  <- view$.chooser[keep][ord]
  cd  <- view$.candidate[keep][ord]
  first <- !duplicated(ch)
  m <- tibble::tibble(
    .id = ch[first], .partner = cd[first],
    .role = NA_character_, .group_id = seq_len(sum(first))
  )
  m
}

#' Row minima of a matrix, without a dependency
#' @noRd
matrixStats_rowMins <- function(x) {
  if (ncol(x) == 0L) return(rep(Inf, nrow(x)))
  do.call(pmin, c(lapply(seq_len(ncol(x)), function(j) x[, j]), list(na.rm = TRUE)))
}

match_network <- function(spec, agents, edges, pool) {
  nb <- neighbour_table(edges)
  nb <- nb[nb$.id %in% pool & nb$.neighbour %in% agents$.id, , drop = FALSE]
  if (nrow(nb) == 0L) return(empty_match())

  nb <- nb[sample.int(nrow(nb)), , drop = FALSE]
  pick <- !duplicated(nb$.id)
  chosen <- nb[pick, , drop = FALSE]

  m <- tibble::tibble(
    .id = chosen$.id, .partner = chosen$.neighbour,
    .role = NA_character_, .group_id = seq_len(nrow(chosen))
  )
  m
}

#' Pick `k` distinct target agents for a newborn
#'
#' One draw per target, rejecting repeats, which is what "m distinct edges per
#' new node" means in preferential attachment. `k = 1` makes exactly the one
#' draw `attach_target()` always made, so a model that does not ask for more
#' links sees the same random stream it always did.
#' @noRd
attach_targets <- function(spec, agents, edges, k = 1L) {
  out <- integer()
  attempts <- 0L
  cap <- 10L * k
  while (length(out) < k && attempts < cap) {
    attempts <- attempts + 1L
    hit <- attach_target(spec, agents[!agents$.id %in% out, , drop = FALSE],
                         edges)
    if (is.na(hit) || hit %in% out) next
    out <- c(out, hit)
  }
  out
}

#' The parent's own neighbours, which is what makes a newborn's place local
#'
#' `exclude` holds the newborn and its siblings: an offspring settles among the
#' agents already there, not among the ones born with it.
#' @noRd
parent_neighbours <- function(edges, parent, k, exclude = integer()) {
  if (k <= 0L || is.na(parent) || is.null(edges) || nrow(edges) == 0L) {
    return(integer())
  }
  nb <- c(edges$to[edges$from == parent], edges$from[edges$to == parent])
  nb <- unique(nb[!nb %in% c(parent, exclude)])
  if (!length(nb)) return(integer())
  nb[sample.int(length(nb), min(k, length(nb)))]
}

#' Pick a target agent for a newborn, given an `attach_via` match spec
#' @noRd
attach_target <- function(spec, agents, edges) {
  if (spec$from == "random_edge" && !is.null(edges) && nrow(edges) > 0L) {
    e <- sample.int(nrow(edges), 1L)
    return(if (stats::runif(1) < 0.5) edges$from[[e]] else edges$to[[e]])
  }
  if (nrow(agents) == 0L) return(NA_integer_)
  agents$.id[sample.int(nrow(agents), 1L)]
}
