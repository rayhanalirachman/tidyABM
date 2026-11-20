# The expression compiler of the Rust engine (migration phase 5). A rule's formula
# is walked once per run into an IR of nested lists, which `RxProgram$new()`
# decodes into a Rust tree (src/rust/src/expr.rs, tidyabm-core/src/expr.rs) and
# evaluates over the group's own column vectors. A formula the compiler does not
# cover is left to the R engine with a recorded reason (`rx_coverage()`); a
# formula it covers can still decline at run time (an integer overflow, `log()` of
# a negative number, a NaN reaching `pmin()`, ...), and the step then runs on the R
# path from the state and `.Random.seed` it started from (`rust_step()`'s replay),
# so whatever R says, it says itself.
#
# Covered (slice 1): literals, columns, globals and length-1 values from the
# formula's environment; `( )`, unary `-`, `+ - * /`, `< > <= >= == !=`, `& | !`,
# `is.na()`, dplyr's `if_else(c, t, f)` and `case_when(... , .default =)`,
# `pmin()`/`pmax()` (no `na.rm`), `exp()`, `log(x)`, `sqrt()` (R's own, called
# back), `sum(x)`, `mean(x)`, `n()`, `runif(n())`. Every function name must find
# the standard function from the formula's scope.

# The functions the IR takes, and what each name must find
rx_fns <- function() {
  list(
    "(" = base::`(`, "-" = base::`-`, "+" = base::`+`, "*" = base::`*`,
    "/" = base::`/`, "<" = base::`<`, ">" = base::`>`, "<=" = base::`<=`,
    ">=" = base::`>=`, "==" = base::`==`, "!=" = base::`!=`, "&" = base::`&`,
    "|" = base::`|`, "!" = base::`!`, is.na = base::is.na, pmin = base::pmin,
    pmax = base::pmax, exp = base::exp, log = base::log, sqrt = base::sqrt,
    sum = base::sum, mean = base::mean, runif = stats::runif,
    if_else = dplyr::if_else, case_when = dplyr::case_when)
}

# dplyr versions whose if_else()/case_when() semantics the evaluator was checked
# against (1.2.1); outside them those two take the R path (PLAN.md risk R3)
rx_dplyr_ok <- function() {
  if (is.null(.rx_memo$dplyr_ok)) {
    v <- utils::packageVersion("dplyr")
    .rx_memo$dplyr_ok <- v >= "1.2.0" && v < "1.3.0"
  }
  .rx_memo$dplyr_ok
}

# per session: the dplyr check, compiled formulas and decoded programs (keyed by
# the formulas' addresses, kept with the formulas so an address is not reused)
.rx_memo <- new.env(parent = emptyenv())

rx_no <- function(reason) structure(reason, class = "rx_reason")
is_rx_no <- function(x) inherits(x, "rx_reason")

#' A formula's IR, or the reason it is not compiled
#'
#' Returns `list(ir, syms, heads)`: `ir` with every name as `list("sym", name)`
#' (bound to an input slot by [rx_bind()]), the names it reads and the functions it
#' calls; or an `rx_reason` string.
#' @noRd
rx_compile <- function(expr) {
  syms <- character()
  heads <- character()
  no <- function(r) stop(structure(class = c("rx_stop", "error", "condition"),
                                   list(message = r, call = NULL)))
  walk <- function(e) {
    if (is.atomic(e) && length(e) == 1L && is.null(attributes(e)) &&
        typeof(e) %in% c("logical", "integer", "double", "character")) {
      return(list("const", e))
    }
    if (is.symbol(e)) {
      nm <- as.character(e)
      if (!nzchar(nm)) no("an empty argument")
      syms <<- c(syms, nm)
      return(list("sym", nm))
    }
    if (!is.call(e) || !is.symbol(e[[1]])) no("not a plain function call")
    fn <- as.character(e[[1]])
    args <- as.list(e)[-1]
    nms <- names(args) %||% rep("", length(args))
    heads <<- c(heads, fn)
    if (fn == "n" && !length(args)) return(list("n"))
    if (!fn %in% names(rx_fns())) no(paste0("`", fn, "()` is not compiled yet"))
    if (fn %in% c("if_else", "case_when") && !rx_dplyr_ok()) {
      no(paste0("dplyr ", utils::packageVersion("dplyr"), " is outside the verified range"))
    }
    if (fn == "case_when") {
      if (any(!nms %in% c("", ".default"))) no("`case_when()` argument other than `.default`")
      if (sum(nms == ".default") > 1L) no("two `.default`s")
      pairs <- lapply(args[nms == ""], function(f) {
        if (!(is.call(f) && identical(f[[1]], as.name("~")) && length(f) == 3L)) {
          no("`case_when()` argument that is not a two-sided formula")
        }
        list(walk(f[[2]]), walk(f[[3]]))
      })
      if (!length(pairs)) no("`case_when()` without cases")
      def <- if (any(nms == ".default")) walk(args[[which(nms == ".default")]])
      return(list("case_when", pairs, def))
    }
    if (fn == "if_else") {
      want <- c("condition", "true", "false")
      if (length(args) != 3L || any(nzchar(nms) & nms != want)) {
        no("`if_else()` with other than its three arguments")
      }
      return(c(list("if_else"), lapply(args, walk)))
    }
    if (any(nzchar(nms))) no(paste0("named arguments to `", fn, "()`"))
    if (fn == "runif") {
      if (!(length(args) == 1L && identical(args[[1]], quote(n())))) {
        no("`runif()` other than `runif(n())`")
      }
      heads <<- c(heads, "n")
      return(list("runif"))
    }
    k <- length(args)
    arity <- switch(fn, "(" = , "!" = , is.na = , exp = , log = , sqrt = , sum = ,
                    mean = k == 1L, "-" = k %in% 1:2, pmin = , pmax = k >= 2L, k == 2L)
    if (!arity) no(paste0("`", fn, "()` with ", k, " argument", if (k != 1L) "s"))
    if (fn == "(") return(walk(args[[1]]))
    op <- if (fn == "-" && k == 1L) "neg" else fn
    c(list(op), lapply(args, walk))
  }
  tryCatch(list(ir = walk(expr), syms = unique(syms), heads = unique(heads)),
           rx_stop = function(e) rx_no(conditionMessage(e)))
}

#' Does every function a formula calls find the standard one from `env`?
#' `n` finds the engine's own (`abm_eval_env()`), `==` may find [group_is()].
#' @noRd
rx_heads_ok <- function(heads, env) {
  fns <- rx_fns()
  for (f in setdiff(heads, "n")) {
    got <- get0(f, envir = env, mode = "function")
    if (!(identical(got, fns[[f]]) || (f == "==" && identical(got, group_is)))) {
      return(FALSE)
    }
  }
  TRUE
}

#' `ir` with each name replaced by its input slot (0-based) in `names`
#' @noRd
rx_bind <- function(ir, names) {
  if (identical(ir[[1]], "sym")) return(list("in", match(ir[[2]], names) - 1L))
  if (identical(ir[[1]], "const")) return(ir)
  if (identical(ir[[1]], "case_when")) {
    ir[[2]] <- lapply(ir[[2]], function(p) lapply(p, rx_bind, names = names))
    if (!is.null(ir[[3]])) ir[[3]] <- rx_bind(ir[[3]], names)
    return(ir)
  }
  if (length(ir) > 1L) ir[-1] <- lapply(ir[-1], rx_bind, names = names)
  ir
}

#' Each rule's compiled form, worked out once per run
#' @noRd
rx_rules <- function(step) {
  planned(step, "rx_rules", lapply(step$rules, function(r) rx_compile_memo(rlang::quo_get_expr(r$quo))))
}

rx_compile_memo <- function(e) {
  key <- paste0("c", rlang::obj_address(e))
  hit <- .rx_memo[[key]]
  if (is.null(hit) || !identical(hit$e, e)) {
    hit <- list(e = e, out = rx_compile(e))
    assign(key, hit, envir = .rx_memo)
  }
  hit$out
}

# `RxProgram$new()` decodes its IR node by node (~150 us for a large rule): kept
# for the session per formulas, names and writes
rx_new_program <- function(exprs, names, writes, irs) {
  key <- paste0("p", paste(vapply(exprs, rlang::obj_address, ""), collapse = ","),
                "|", paste(names, collapse = "\r"), "|", paste(writes, collapse = ","))
  hit <- .rx_memo[[key]]
  if (is.null(hit) || !identical(hit$exprs, exprs)) {
    hit <- list(exprs = exprs, prog = RxProgram$new(irs, writes))
    assign(key, hit, envir = .rx_memo)
  }
  hit$prog
}

#' Where each name a program reads comes from for a group with columns `cols`:
#' 1 a column, 2 a global, 3 the formula's scope; `NULL` when a name is found
#' nowhere (the R engine then raises its error)
#' @noRd
rx_sources <- function(names, cols, globals, env) {
  src <- ifelse(names %in% cols, 1L, ifelse(names %in% names(globals), 2L, 3L))
  for (nm in names[src == 3L]) {
    if (!exists(nm, envir = env)) return(NULL)
  }
  src
}

#' A program over some of a step's rules, bound for a group, or a reason
#' @noRd
rx_program <- function(step, idx, cols, globals, global_step = FALSE) {
  comp <- rx_rules(step)[idx]
  bad <- Filter(is_rx_no, comp)
  if (length(bad)) return(rx_no(bad[[1]]))
  rules <- step$rules[idx]
  envs <- lapply(rules, function(r) rlang::quo_get_env(r$quo))
  if (!all(vapply(envs, identical, NA, envs[[1]]))) return(rx_no("rules written in different environments"))
  # the scope the R engine evaluates in: the globals over the formula's environment
  scope <- abm_eval_env(rules[[1]]$quo, globals, n = 1L)
  heads <- unique(unlist(lapply(comp, `[[`, "heads")))
  if (!rx_heads_ok(heads, scope)) return(rx_no("a function name is masked"))
  if ("runif" %in% heads && !rust_rng_ok()) return(rx_no("an RNG kind the Rust path was not verified under"))
  names <- unique(unlist(lapply(comp, `[[`, "syms")))
  src <- rx_sources(names, cols, globals, envs[[1]])
  if (is.null(src)) return(rx_no("a name found nowhere"))
  # inputs in the order [rx_inputs()] gathers them: columns, globals, scope values
  o <- order(src)
  names <- names[o]
  src <- src[o]
  targets <- vapply(rules, `[[`, "", "target")
  # a global step's rules see the globals written before them; a column of the
  # same name hides the global, as in the R engine's data mask
  writes <- if (global_step) {
    w <- match(targets, names) - 1L
    w[is.na(w) | (src[w + 1L] %in% 1L)] <- -1L
    as.integer(w)
  } else rep(-1L, length(idx))
  prog <- rx_new_program(lapply(rules, function(r) rlang::quo_get_expr(r$quo)), names, writes,
                         lapply(comp, function(cp) rx_bind(cp$ir, names)))
  list(prog = prog, names = names, src = src, env = envs[[1]], targets = targets,
       cols = names[src == 1L], globs = names[src == 2L], scope = names[src == 3L])
}

# what a plan depends on beyond the formulas: the groups' and globals' names.
# Steps only ever add columns and globals (none removes or renames one), so
# their counts tell a changed schema; [schema_key()] builds the names and costs
# a few microseconds on every step
rx_key <- function(state) c(length(state$globals), lengths(state$groups, use.names = FALSE))

#' The inputs of a program for one group (or a list of groups, stacked by the
#' evaluator), or `NULL` when one is not a bare vector the evaluator can read
#' @noRd
rx_inputs <- function(p, cols, globals) {
  out <- c(cols, .subset(globals, p$globs),
           if (length(p$scope)) mget(p$scope, envir = p$env, inherits = TRUE))
  # bare vectors only (a factor, a Date, names or any attribute changes what R's
  # operators return); the evaluator declines other types and lengths itself
  stacked <- length(cols) && is.list(cols[[1]])
  flat <- if (stacked) c(unlist(cols, recursive = FALSE), out[-seq_along(cols)]) else out
  if (any(lengths(lapply(flat, attributes)))) return(NULL)
  out
}

# a step whose shape never compiles: marked in its plan, so later ticks go to the
# R engine without trying the kernel
rx_never <- function(step) {
  p <- .subset2(step, "plan")
  if (!is.null(p)) assign("rust_no", TRUE, envir = p)
  NULL
}

# a step left to the R path at run time: the reason is kept in the plan
rx_declined <- function(step, reason = rs_rx_reason()) {
  p <- .subset2(step, "plan")
  if (!is.null(p)) assign("rx_declined", c(p$rx_declined, reason), envir = p)
  NULL
}

#' `abm_rules()` with its formulas evaluated in Rust
#'
#' The shapes [run_rules()] evaluates over whole groups: no `.by`, no relation,
#' nothing read of a pairing, and no pairing that splits the rows (a standing
#' match under `.scope = "match"`). Values are computed group by group, every rule
#' of a group from the start-of-step columns, then written, as [run_rules()] does;
#' the groups stay R vectors (the store is not involved).
#' @noRd
rk_rules <- function(step, state, store) {
  if (length(state$relations)) return(NULL)
  # shapes that never compile, whatever the data: the step goes straight to the
  # R engine from now on ([rust_step()] reads `rust_no`). A `.cell` write moves
  # agents ([run_step()] re-derives `.x`/`.y`); a `partner_` target writes
  # another agent's row.
  if (!planned(step, "rx_shape", rx_rules_ok(step))) return(rx_never(step))
  if (!is.null(state$match) && nrow(state$match$match) > 0L &&
      (step$scope %||% "match") == "match") return(NULL)
  plan <- planned_for(step, "rx_plan", rx_key(state), {
    routes <- if (length(state$groups) > 1L) rule_routes(step, state)
    progs <- lapply(stats::setNames(nm = names(state$groups)), function(nm) {
      todo <- if (length(state$groups) == 1L) step$rules else routes[[nm]]
      if (!length(todo)) return(NULL)
      idx <- which(vapply(step$rules, function(r) any(vapply(todo, identical, NA, r)), NA))
      rx_program(step, idx, names(state$groups[[nm]]), state$globals)
    })
    if (any(vapply(progs, is_rx_no, NA))) NULL else Filter(Negate(is.null), progs)
  })
  if (is.null(plan)) return(NULL)
  applied <- FALSE
  for (nm in names(plan)) {
    p <- plan[[nm]]
    g <- state$groups[[nm]]
    n <- vctrs::vec_size(g)
    if (n == 0L) next
    applied <- TRUE
    ins <- rx_inputs(p, .subset(g, p$cols), state$globals)
    vals <- if (!is.null(ins)) p$prog$eval(ins, n, n, FALSE)
    if (is.null(vals)) return(rx_declined(step, if (is.null(ins)) "an input that is not a bare vector" else rs_rx_reason()))
    cls <- oldClass(g)
    g <- unclass(g)
    for (i in seq_along(vals)) g[[p$targets[[i]]]] <- vals[[i]]
    oldClass(g) <- cls
    state$groups[[nm]] <- g
  }
  # nobody to apply it to is the R engine's error to raise
  if (!applied && any(vapply(state$groups, vctrs::vec_size, 0L) > 0L)) return(NULL)
  state
}

#' `abm_global()` with its formulas evaluated in Rust
#'
#' Without `.by`: each rule over all groups stacked (every column it reads must
#' be in every group, so nothing is padded), one value per rule, the later rules
#' seeing the globals the earlier ones wrote, as [run_global()] does.
#' @noRd
rk_global <- function(step, state, store) {
  # one group: the R engine's own evaluation is a plain `eval()` and measured
  # cheaper than the kernel's set-up (tax: 28 vs 37 us a step)
  if (!planned(step, "rx_shape", rx_global_ok(step) && length(state$groups) > 1L)) {
    return(rx_never(step))
  }
  groups <- state$groups
  n <- sum(vapply(groups, vctrs::vec_size, 0L))
  if (n == 0L) return(NULL)
  p <- planned_for(step, "rx_plan", rx_key(state), {
    cols <- Reduce(intersect, lapply(groups, names))
    p <- rx_program(step, seq_along(step$rules), cols, state$globals, global_step = TRUE)
    # a column some group lacks is padded with NA by the R engine: not compiled
    if (!is_rx_no(p)) {
      everywhere <- model_columns(groups)
      if (any(p$names %in% setdiff(everywhere, cols))) p <- rx_no("a column some group lacks")
    }
    p
  })
  if (is_rx_no(p)) return(NULL)
  cols <- if (length(groups) == 1L) .subset(groups[[1]], p$cols) else
    lapply(stats::setNames(nm = p$cols), function(nm) lapply(groups, .subset2, nm))
  ins <- rx_inputs(p, cols, state$globals)
  vals <- if (!is.null(ins)) p$prog$eval(ins, n, 1L, length(groups) > 1L)
  if (is.null(vals)) return(rx_declined(step, if (is.null(ins)) "an input that is not a bare vector" else rs_rx_reason()))
  for (i in seq_along(vals)) state$globals[[p$targets[[i]]]] <- vals[[i]]
  state
}

# the parts of a step's shape that never change during a run, for the kernels
# and for "auto" to decide whether a run has anything to compile
rx_rules_ok <- function(step) {
  is.null(step$by) && !any(vapply(rx_rules(step), is_rx_no, NA)) &&
    !(step$uses_match %||% reads_match(lapply(step$rules, `[[`, "quo"))) &&
    !any(vapply(step$rules, function(r) r$target == ".cell" || startsWith(r$target, "partner_"), NA))
}
rx_global_ok <- function(step) is.null(step$by) && !any(vapply(rx_rules(step), is_rx_no, NA))

#' Which rules of a go block the Rust evaluator takes, and why the others not
#'
#' One row per rule of every `abm_rules()` / `abm_global()` step: `path` "rust" or
#' "r", and `reason`. Decided from the formulas alone (names and functions), so a
#' rule marked "rust" can still be declined at run time by its data.
#' @noRd
rx_coverage <- function(go) {
  rows <- list()
  walk_steps <- function(steps, prefix = "") {
    for (i in seq_along(steps)) {
      s <- steps[[i]]
      cls <- class(s)[[1]]
      if (cls == "abm_repeat") {
        walk_steps(s$steps, paste0(prefix, i, "."))
        next
      }
      if (!cls %in% c("abm_rules", "abm_global")) next
      shape <- if (!is.null(s$by)) "`.by` is not compiled yet"
      for (r in s$rules) {
        cp <- rx_compile(rlang::quo_get_expr(r$quo))
        why <- shape %||% if (is_rx_no(cp)) unclass(cp) else ""
        rows[[length(rows) + 1L]] <<- data.frame(
          step = paste0(prefix, i), class = cls, target = r$target,
          path = if (nzchar(why)) "r" else "rust", reason = why)
      }
    }
  }
  walk_steps(go$steps %||% go)
  if (!length(rows)) {
    return(data.frame(step = character(), class = character(), target = character(),
                      path = character(), reason = character()))
  }
  do.call(rbind, rows)
}
