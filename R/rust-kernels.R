# Rust step kernels (migration phase 4). A kernel is `function(step, state, store)`
# returning the new `state` -- the store written through `store$set_col()` and
# `state$groups` exported from it -- or `NULL` to decline before it has changed
# anything; [rust_step()] then runs the step through the adapter. A kernel follows
# the R engine's step function line for line where it does R work (D12: R first,
# Rust follows), so the state it returns is `identical()` to the R engine's, plan
# caches included. It must not print, and the R calls it makes are the engine's
# own, so an error or warning they raise is replayed on the R path.

#' `abm_neighbours()` over a network, the shapes [neighbour_fast()] takes
#'
#' [run_neighbours()]'s set-up and [neighbour_fast()], with the folds
#' (`nb_fold()`) in Rust (`rs_nb_fold()`) and the writes through the store. Any
#' other neighbourhood (lattice direction, `within`, relations, a rule the fast
#' path does not take) declines.
#' @noRd
rk_neighbours <- function(step, state, store) {
  if (!is.null(step$where) || !is.null(step$within) || is.null(state$edges) ||
      length(state$relations)) return(NULL)
  # run_neighbours()'s set-up, as there
  combined <- bind_groups(state$groups, planned(step, "reads",
    read_vars(c(lapply(step$rules, `[[`, "quo"), list(step$within)))))
  if (vctrs::vec_size(combined) == 0L) return(state)
  check_stale_draws(step, state)
  if (!identical(state$nb_cache$edges, state$edges)) {
    state$nb_cache <- list(edges = state$edges, nb = neighbour_table(state$edges))
  }
  # neighbour_fast(), as there
  ids <- combined$.id
  n <- length(ids)
  if (!identical(ids, seq_len(n))) return(NULL)
  shapes <- planned(step, "shapes", lapply(step$rules, function(r) nb_fast_shape(r$quo)))
  if (any(vapply(shapes, is.null, NA))) return(NULL)
  drawn <- drawn_names(state$edges)
  if (length(drawn)) {
    bad <- c(drawn, paste0(drawn, "_back"))
    if (any(vapply(shapes, function(s) any(s$vars %in% bad), NA))) return(NULL)
  }
  xs <- vector("list", length(shapes))
  for (i in seq_along(shapes)) {
    arg <- shapes[[i]]$arg
    # a bare column is the column: nb_fast_eval() returns it and
    # nb_fast_scalars() has nothing to check, so no evaluation scope is needed
    if (is.symbol(arg) && !is.null(x <- .subset2(combined, as.character(arg)))) {
      if (!(is.double(x) || is.integer(x) || is.logical(x)) || is.object(x)) return(NULL)
      xs[[i]] <- x
      next
    }
    env <- abm_eval_env(step$rules[[i]]$quo, state$globals)
    if (!nb_fast_scalars(shapes[[i]], names(combined), env)) return(NULL)
    x <- nb_fast_eval(shapes[[i]]$arg, env, combined)
    if (length(x) == 1L) x <- rep(x, n)
    if (is.object(x) || length(x) != n ||
        !(is.double(x) || is.integer(x) || is.logical(x))) return(NULL)
    xs[[i]] <- x
  }
  ix <- state$nb_cache$ix
  if (is.null(ix) || ix$n != n) {
    ix <- nb_index(state$nb_cache$nb, n)
    state$nb_cache$ix <- ix
  }
  if (!ix$ok) return(NULL)
  vals <- vector("list", length(shapes))
  for (i in seq_along(shapes)) {
    x <- xs[[i]]
    fn <- shapes[[i]]$fn
    vals[[i]] <- if (!is.double(x)) nb_fold_int(fn, as.integer(x), ix)
                 else rs_nb_fold(fn == "mean", x, ix$cols, ix$cnt) %||% nb_fold(fn, x, ix)
  }
  # assign_rule(g, target, v, TRUE) with `v` bare and of the group's length is
  # the column replaced by `v`: what set_col() does
  for (i in seq_along(shapes)) {
    for (nm in names(state$groups)) {
      g <- state$groups[[nm]]
      if (nrow(g) == 0L) next
      v <- vals[[i]]
      if (length(state$groups) > 1L) v <- v[g$.id]
      store$set_col(nm, step$rules[[i]]$target, v)
    }
  }
  state$groups <- store$export()
  state
}

#' Rust kernels by step class: `fun` the kernel, `auto` whether "auto" uses it
#' (proven in "both" mode and measured faster than the R engine)
#' @noRd
rust_kernels <- list(
  abm_neighbours = list(fun = rk_neighbours, auto = FALSE)
)
