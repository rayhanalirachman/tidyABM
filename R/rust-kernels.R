# Rust step kernels (migration phase 4). A kernel is `function(step, state, store)`
# returning the new `state` -- the store written through `store$set_col()` and
# `state$groups` exported from it -- or `NULL` to decline before it has changed
# anything; [rust_step()] then runs the step through the adapter. A kernel follows
# the R engine's step function line for line where it does R work (D12: R first,
# Rust follows), so the state it returns is `identical()` to the R engine's, plan
# caches included. It must not print, and the R calls it makes are the engine's
# own, so an error or warning they raise is replayed on the R path.

#' Rust kernels by step class: `fun` the kernel, `auto` whether "auto" uses it
#' (proven in "both" mode and measured faster than the R engine), `store` whether
#' it reads and writes the column store (`FALSE`: it works on `state$groups` like
#' an R step function, and the store is brought up to date when next read), and
#' `can(step)`, for "auto", whether a step's formulas can take the kernel at all
#' (a run whose steps cannot stays on the R engine). `native` (1 `abm_rules`, 2
#' `abm_global`, 3 network `abm_neighbours`): the step runs natively inside the Rust tick loop
#' ([run_once_loop()]); `fun` is then the same native executor, for "both" mode.
#' @noRd
rust_kernels <- list(
  abm_pairs      = list(fun = rk_native, auto = TRUE, store = FALSE, can = rl_pairs_ok,
                        native = 19L),
  abm_link       = list(fun = rk_native, auto = TRUE, store = FALSE, can = rl_link_ok,
                        native = 18L),
  abm_unlink     = list(fun = rk_native, auto = TRUE, store = FALSE, can = rl_link_ok,
                        native = 18L),
  abm_draw       = list(fun = rk_native, auto = TRUE, store = FALSE, can = rl_draw_ok,
                        native = 16L),
  abm_neighbours = list(fun = rk_native, auto = TRUE, store = FALSE,
                        can = function(step) rl_nb_ok(step) || rl_nb_view_ok(step) ||
                          rl_nb_within_ok(step) || rl_nb_cross_ok(step) ||
                          rl_nb_rel_ok(step) || rl_nb_where_ok(step),
                        native = 3L),
  abm_sequential = list(fun = rk_native, auto = TRUE, store = FALSE, can = rl_seq_ok,
                        native = 12L),
  abm_tell       = list(fun = rk_native, auto = TRUE, store = FALSE, can = rl_tell_ok,
                        native = 11L),
  abm_move       = list(fun = rk_native, auto = TRUE, store = FALSE, can = rl_move_ok,
                        native = 10L),
  abm_birth      = list(fun = rk_native, auto = TRUE, store = FALSE, can = rl_birth_ok,
                        native = 9L),
  abm_death      = list(fun = rk_native, auto = TRUE, store = FALSE, can = rl_death_ok,
                        native = 8L),
  abm_match      = list(fun = rk_native, auto = TRUE, store = FALSE, can = rl_match_ok,
                        native = 7L),
  abm_rules      = list(fun = rk_native, auto = TRUE, store = FALSE, can = rx_rules_mixed_ok,
                        native = 1L),
  abm_global     = list(fun = rk_native, auto = TRUE, store = FALSE,
                        can = function(step) rx_global_ok(step) || rl_global_by_ok(step),
                        native = 2L)
)
