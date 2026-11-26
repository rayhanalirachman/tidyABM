# The expression compiler of the Rust engine (migration phase 5). A rule's formula
# is walked once per run into an IR of nested lists, which `RxProgram$new()`
# decodes into a Rust tree (src/rust/src/expr.rs, tidyabm-core/src/expr.rs) and
# evaluates over the group's own column vectors. A formula the compiler does not
# cover is left to the R engine with a recorded reason (`rx_coverage()`); a
# formula it covers can still decline at run time (an integer overflow, `log()` of
# a negative number, a NaN reaching `pmin()`, ...), and the step then runs on the R
# path from the state and `.Random.seed` it started from (the loop's replay,
# R/run-loop.R), so whatever R says, it says itself. The compiled steps run
# natively inside the Rust tick loop (`rl_prep()`, src/rust/src/runloop.rs).
#
# Covered (slice 1): literals, columns, globals and length-1 values from the
# formula's environment; `( )`, unary `-`, `+ - * /`, `< > <= >= == !=`, `& | !`,
# `is.na()`, dplyr's `if_else(c, t, f)` and `case_when(... , .default =)`,
# `pmin()`/`pmax()` (no `na.rm`), `exp()`, `log(x)`, `sqrt()` (R's own, called
# back), `sum(x)`, `mean(x)`, `n()`, `runif(n())`. FR3: values from the
# formula's environment (any length), `c()` of literals, `sum`/`mean(x, na.rm =)`,
# dplyr's `coalesce()`, `%in%` (the engine's `in_rowwise()` with an atomic
# table), `sample(x, size, replace = TRUE)`, `runif(n(), a, b)`, `rnorm(n(), a,
# b)`, `x[i]` (positive indices), and the base functions in `rx_call_spec`
# (`^`, `abs`, `round`, `cumsum`, `min`/`max`, `%%`, ...), which R itself computes
# over the evaluated arguments. FR8: base `if (c) t else f` (lazy, one condition
# value), `runif(1, a, b)` / `rnorm(1, a, b)` (one draw), and `as.integer()` /
# `as.numeric()` / `as.double()` of numbers, computed natively. Every function name
# must find the standard function from the formula's scope.

# The functions the IR takes, and what each name must find (cached per session)
rx_fns <- function() {
  if (is.null(.rx_memo$fns)) {
    .rx_memo$fns <- c(list(
      "(" = base::`(`, "-" = base::`-`, "+" = base::`+`, "*" = base::`*`,
      "/" = base::`/`, "<" = base::`<`, ">" = base::`>`, "<=" = base::`<=`,
      ">=" = base::`>=`, "==" = base::`==`, "!=" = base::`!=`, "&" = base::`&`,
      "|" = base::`|`, "!" = base::`!`, is.na = base::is.na,
      is.finite = base::is.finite, is.infinite = base::is.infinite, is.nan = base::is.nan,
      pmin = base::pmin,
      pmax = base::pmax, exp = base::exp, log = base::log, sqrt = base::sqrt,
      sum = base::sum, mean = base::mean, any = base::any, all = base::all,
      runif = stats::runif,
      if_else = dplyr::if_else, case_when = dplyr::case_when,
      between = dplyr::between, ifelse = base::ifelse,
      # FR3
      c = base::c, coalesce = dplyr::coalesce, "%in%" = in_rowwise, sample = base::sample,
      sample.int = base::sample.int, seq_len = base::seq_len, seq_along = base::seq_along,
      rnorm = stats::rnorm, rbinom = stats::rbinom, rpois = stats::rpois,
      rexp = stats::rexp, rlnorm = stats::rlnorm, "[" = base::`[`,
      # FR8
      "if" = base::`if`), rx_call_fns())
  }
  .rx_memo$fns
}

rx_fn_names <- c(
  "(", "-", "+", "*", "/", "<", ">", "<=", ">=", "==", "!=", "&", "|", "!",
  "is.na", "is.finite", "is.infinite", "is.nan", "pmin", "pmax", "exp", "log",
  "sqrt", "sum", "mean", "any", "all", "runif", "if_else", "case_when",
  "between", "ifelse", "c", "coalesce", "%in%", "sample", "sample.int",
  "seq_len", "seq_along", "rnorm", "rbinom", "rpois", "rexp", "rlnorm", "[", "if",
  "^", "%%", "%/%", "abs", "sign", "floor", "ceiling", "trunc", "round",
  "signif", "cumsum", "cumprod", "cummax", "cummin", "as.integer", "as.numeric",
  "as.double", "min", "max", "log2", "log10", "log1p", "expm1"
)

rx_draw_heads <- c("runif", "rnorm", "sample", "sample.int", "rbinom", "rpois", "rexp", "rlnorm")
rx_rng_heads_ok <- function(heads) {
  if ("rbinom" %in% heads && !rust_rng_ok("rbinom")) return(FALSE)
  if (any(rx_draw_heads %in% heads) && !rust_rng_ok()) return(FALSE)
  TRUE
}

# Base functions R itself computes over the evaluated (numeric) arguments: their
# arity, and whether they can warn (then called through a guard that turns any
# condition into a decline, so the step is replayed on the R path)
rx_call_spec <- list(
  "^" = c(2, 2, 0), "%%" = c(2, 2, 1), "%/%" = c(2, 2, 1), abs = c(1, 1, 0),
  sign = c(1, 1, 0), floor = c(1, 1, 0), ceiling = c(1, 1, 0), trunc = c(1, 1, 0),
  round = c(1, 2, 0), signif = c(1, 2, 0), cumsum = c(1, 1, 1), cumprod = c(1, 1, 1),
  cummax = c(1, 1, 1), cummin = c(1, 1, 1), as.integer = c(1, 1, 1),
  as.numeric = c(1, 1, 1), as.double = c(1, 1, 1), min = c(1, Inf, 1), max = c(1, Inf, 1),
  log2 = c(1, 1, 1), log10 = c(1, 1, 1), log1p = c(1, 1, 1), expm1 = c(1, 1, 0))

rx_call_fns <- function() lapply(rlang::set_names(names(rx_call_spec)), get, envir = baseenv())

# the function a `call` node runs: R's own, or behind the guard
rx_call_fun <- function(fn) {
  f <- get(fn, envir = baseenv())
  if (!rx_call_spec[[fn]][[3]]) return(f)
  function(...) tryCatch(f(...), warning = function(w) NULL, message = function(m) NULL,
                         error = function(e) NULL)
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

# per session: the dplyr check and `rx_fns()` in `.rx_memo`; compiled formulas
# and decoded programs in `.rx_cache` (bounded so scripts that build fresh
# `abm_go()` blocks in a loop cannot grow unboundedly)
.rx_memo <- new.env(parent = emptyenv())
.rx_cache <- new.env(parent = emptyenv())
.rx_cache_max <- 4096L

rx_no <- function(reason) structure(reason, class = "rx_reason")
is_rx_no <- function(x) inherits(x, "rx_reason")

# Fast pre-check for unknown call heads before entering `tryCatch()` (avoids
# condition allocation when a script rebuilds uncompiled user-function formulas)
rx_bad_head <- function(e) {
  if (!is.call(e)) return(NULL)
  h <- e[[1]]
  if (is.symbol(h)) {
    fn <- as.character(h)
    if (fn == "::") return(NULL)
    if (fn != "n" && fn != "~" && !fn %in% rx_fn_names) return(fn)
  }
  for (k in seq_along(e)[-1L]) {
    b <- rx_bad_head(e[[k]])
    if (!is.null(b)) return(b)
  }
  NULL
}

#' A formula's IR, or the reason it is not compiled
#'
#' Returns `list(ir, syms, heads)`: `ir` with every name as `list("sym", name)`
#' (bound to an input slot by [rx_bind()]), the names it reads and the functions it
#' calls; or an `rx_reason` string.
#' @noRd
rx_compile <- function(expr) {
  if (!is.null(bad_fn <- rx_bad_head(expr))) {
    return(rx_no(paste0("`", bad_fn, "()` is not compiled yet")))
  }
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
    # `pkg::f` that is the very function the IR takes (FR11: model 42's
    # `dplyr::case_when()`): compiled as `f`, whatever `f` finds in the scope
    ns <- is.call(e) && rx_ns_head(e[[1]])
    if (ns) e[[1]] <- e[[1]][[3]]
    if (!is.call(e) || !is.symbol(e[[1]])) no("not a plain function call")
    fn <- as.character(e[[1]])
    args <- as.list(e)[-1]
    nms <- names(args) %||% rep("", length(args))
    if (!ns) heads <<- c(heads, fn)
    if (fn == "n" && !length(args)) return(list("n"))
    if (!fn %in% rx_fn_names) no(paste0("`", fn, "()` is not compiled yet"))
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
    if (fn == "if") {
      # base `if (c) t else f`: lazy, one condition value (FR8; R errs otherwise)
      if (length(args) != 3L || any(nzchar(nms))) no("`if` without `else`")
      return(c(list("if"), lapply(args, walk)))
    }
    if (fn %in% rx_fr3_fns) {
      r <- rx_walk_fr3(fn, e, args, nms, walk, no, function(h) heads <<- c(heads, h))
      if (!is.null(r)) return(r)
    }
    if (any(nzchar(nms))) no(paste0("named arguments to `", fn, "()`"))
    k <- length(args)
    arity <- switch(fn, "(" = , "!" = , is.na = , is.finite = , is.infinite = , is.nan = ,
                    seq_len = , seq_along = , exp = , log = , sqrt = , sum = , mean = ,
                    any = , all = k == 1L,
                    "-" = k %in% 1:2, pmin = , pmax = k >= 2L, k == 2L)
    # `"%in%"` = list("%in%", x, table); `"["` = list("[", x, i)
    if (!arity) no(paste0("`", fn, "()` with ", k, " argument", if (k != 1L) "s"))
    if (fn == "(") return(walk(args[[1]]))
    op <- if (fn == "-" && k == 1L) "neg" else fn
    c(list(op), lapply(args, walk))
  }
  tryCatch(list(ir = walk(expr), syms = unique(syms), heads = unique(heads)),
           rx_stop = function(e) rx_no(conditionMessage(e)))
}

# Is `h` a `pkg::f` call naming the function the IR takes for `f`?
rx_ns_head <- function(h) {
  if (!(is.call(h) && identical(h[[1]], as.name("::")) && length(h) == 3L &&
        is.symbol(h[[2]]) && is.symbol(h[[3]]))) return(FALSE)
  nm <- as.character(h[[3]])
  fns <- rx_fns()
  if (!nm %in% names(fns)) return(FALSE)
  f <- tryCatch(getExportedValue(as.character(h[[2]]), nm), error = function(err) NULL)
  identical(f, fns[[nm]])
}

# FR3's constructs, compiled apart from [rx_compile()] so their byte code is only
# built (R's JIT, first use) for a formula that has one: the tick loop's cold
# start stays as short as before
rx_fr3_fns <- c("coalesce", "sum", "mean", "any", "all", "between", "ifelse",
                "runif", "rnorm", "sample", "sample.int", "rbinom", "rpois", "rexp", "rlnorm",
                "c", names(rx_call_spec))
rx_walk_fr3 <- function(fn, e, args, nms, walk, no, add_head) {
  if (fn %in% c("coalesce", "between") && !rx_dplyr_ok()) {
    no(paste0("dplyr ", utils::packageVersion("dplyr"), " is outside the verified range"))
  }
  if (fn == "between") {
    want <- c("x", "left", "right")
    if (length(args) != 3L || any(nzchar(nms) & nms != want)) {
      no("`between()` with other than `(x, left, right)`")
    }
    return(c(list("between"), lapply(args, walk)))
  }
  if (fn == "ifelse") {
    want <- c("test", "yes", "no")
    if (length(args) != 3L || any(nzchar(nms) & nms != want)) {
      no("`ifelse()` with other than `(test, yes, no)`")
    }
    return(c(list("ifelse"), lapply(args, walk)))
  }
  if (fn %in% c("sum", "mean", "any", "all") && any(nzchar(nms))) {
    if (!identical(nms, c("", "na.rm"))) no(paste0("`", fn, "()` with other than `na.rm`"))
    narm <- args[[2]]
    if (!(is.logical(narm) && length(narm) == 1L && !is.na(narm))) no("`na.rm` that is not TRUE or FALSE")
    return(list(if (narm) paste0(fn, "_narm") else fn, walk(args[[1]])))
  }
  if (fn == "sample.int") {
    m <- tryCatch(as.list(match.call(rx_fns()[[fn]], e))[-1], error = function(err) NULL)
    if (is.null(m) || is.null(m$n) || !all(names(m) %in% c("n", "size", "replace"))) {
      no("`sample.int()` with unsupported arguments")
    }
    rep_flag <- if (is.null(m$replace)) FALSE else m$replace
    if (!(is.logical(rep_flag) && length(rep_flag) == 1L && !is.na(rep_flag))) {
      no("`sample.int()` with non-literal `replace`")
    }
    sz <- if (is.null(m$size)) NULL else walk(m$size)
    return(list("sample_int", walk(m$n), sz, rep_flag))
  }
  if (fn %in% c("rbinom", "rpois", "rexp", "rlnorm")) {
    m <- tryCatch(as.list(match.call(rx_fns()[[fn]], e))[-1], error = function(err) NULL)
    if (is.null(m) || is.null(m$n)) no(paste0("`", fn, "()` arguments that do not match"))
    one <- is.numeric(m$n) && length(m$n) == 1L && is.null(attributes(m$n)) &&
      isTRUE(m$n == 1)
    if (!one && !identical(m$n, quote(n()))) {
      no(paste0("`", fn, "()` whose first argument is not `n()` or 1"))
    }
    if (!one) add_head("n")
    kind <- match(fn, c("rbinom", "rpois", "rexp", "rlnorm")) - 1L
    if (fn == "rbinom") {
      if (!setequal(names(m), c("n", "size", "prob"))) no("`rbinom()` requires `(n, size, prob)`")
      return(list("rdist", kind, one, walk(m$size), walk(m$prob)))
    }
    if (fn == "rpois") {
      if (!setequal(names(m), c("n", "lambda"))) no("`rpois()` requires `(n, lambda)`")
      return(list("rdist", kind, one, walk(m$lambda), NULL))
    }
    if (fn == "rexp") {
      if (!all(names(m) %in% c("n", "rate"))) no("`rexp()` with unsupported arguments")
      a <- if (is.null(m$rate)) list("const", 1) else walk(m$rate)
      return(list("rdist", kind, one, a, NULL))
    }
    if (!all(names(m) %in% c("n", "meanlog", "sdlog"))) no("`rlnorm()` with unsupported arguments")
    a <- if (is.null(m$meanlog)) list("const", 0) else walk(m$meanlog)
    b <- if (is.null(m$sdlog)) list("const", 1) else walk(m$sdlog)
    return(list("rdist", kind, one, a, b))
  }
  if (fn %in% c("runif", "rnorm", "sample")) {
    m <- tryCatch(as.list(match.call(rx_fns()[[fn]], e))[-1], error = function(err) NULL)
    if (is.null(m)) no(paste0("`", fn, "()` arguments that do not match"))
    first <- names(m)[[1]]
    if (fn == "sample") {
      if (!setequal(names(m), c("x", "size", "replace")) || !isTRUE(m$replace)) {
        no("`sample()` other than `sample(x, size, replace = TRUE)`")
      }
      return(list("sample", walk(m$x), walk(m$size)))
    }
    # `n()` draws one per row; a literal 1 (FR8) draws one value
    one <- is.numeric(m$n) && length(m$n) == 1L && is.null(attributes(m$n)) &&
      isTRUE(m$n == 1)
    if (!one && !identical(m$n, quote(n()))) {
      no(paste0("`", fn, "()` whose first argument is not `n()` or 1"))
    }
    if (!one) add_head("n")
    p <- if (fn == "runif") c("min", "max") else c("mean", "sd")
    if (!all(names(m) %in% c("n", p))) no(paste0("`", fn, "()` with other arguments"))
    if (one) {
      a <- if (is.null(m[[p[1]]])) list("const", 0) else walk(m[[p[1]]])
      b <- if (is.null(m[[p[2]]])) list("const", 1) else walk(m[[p[2]]])
      return(list(if (fn == "runif") "runif_ab1" else "rnorm_ab1", a, b))
    }
    if (fn == "runif" && length(m) == 1L) return(list("runif"))
    a <- if (is.null(m[[p[1]]])) list("const", if (fn == "runif") 0 else 0) else walk(m[[p[1]]])
    b <- if (is.null(m[[p[2]]])) list("const", 1) else walk(m[[p[2]]])
    return(list(if (fn == "runif") "runif_ab" else "rnorm_ab", a, b))
  }
  if (any(nzchar(nms))) no(paste0("named arguments to `", fn, "()`"))
  if (fn == "c") {
    if (!length(args) || !all(vapply(args, function(a) is.atomic(a) && length(a) == 1L &&
                                       is.null(attributes(a)) &&
                                       typeof(a) %in% c("logical", "integer", "double", "character"), NA))) {
      no("`c()` of other than literal values")
    }
    return(list("constv", do.call(base::c, args)))
  }
  if (fn == "abs" && length(args) == 1L) return(list("abs", walk(args[[1]])))
  if (fn %in% c("min", "max") && length(args) >= 1L) {
    return(c(list(fn), lapply(args, walk)))
  }
  if (fn %in% c("as.numeric", "as.double", "as.integer") && length(args) == 1L) {
    # numbers coerced natively (FR8): no R call per evaluation
    return(list(if (fn == "as.integer") "as_int" else "as_dbl", walk(args[[1]])))
  }
  if (fn == "coalesce") {
    if (!length(args)) no("`coalesce()` of nothing")
    return(c(list("coalesce"), lapply(args, walk)))
  }
  if (fn %in% names(rx_call_spec)) {
    ar <- rx_call_spec[[fn]]
    if (length(args) < ar[[1]] || length(args) > ar[[2]]) {
      no(paste0("`", fn, "()` with ", length(args), " arguments"))
    }
    return(c(list("call", fn), lapply(args, walk)))
  }
  NULL
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
  if (!is.list(ir)) return(ir)
  if (identical(ir[[1]], "sym")) return(list("in", match(ir[[2]], names) - 1L))
  if (identical(ir[[1]], "const") || identical(ir[[1]], "constv")) return(ir)
  if (identical(ir[[1]], "call")) {
    ir[-(1:2)] <- lapply(ir[-(1:2)], rx_bind, names = names)
    return(ir)
  }
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
  hit <- .rx_cache[[key]]
  if (is.null(hit) || !identical(hit$e, e)) {
    if (length(.rx_cache) >= .rx_cache_max) rm(list = ls(.rx_cache), envir = .rx_cache)
    hit <- list(e = e, out = rx_compile(e))
    assign(key, hit, envir = .rx_cache)
  }
  hit$out
}

# `RxProgram$new()` decodes its IR node by node (~150 us for a large rule): kept
# for the session per formulas, names and writes
rx_new_program <- function(exprs, names, writes, irs) {
  key <- paste0("p", paste(vapply(exprs, rlang::obj_address, ""), collapse = ","),
                "|", paste(names, collapse = "\r"), "|", paste(writes, collapse = ","))
  hit <- .rx_cache[[key]]
  if (is.null(hit) || !identical(hit$exprs, exprs)) {
    if (length(.rx_cache) >= .rx_cache_max) rm(list = ls(.rx_cache), envir = .rx_cache)
    # (numbering only a program that calls R functions: its byte code is built on
    # first use)
    nb <- if ("call" %in% unlist(irs, use.names = FALSE)) rx_number_calls(irs)
          else list(irs = irs, fns = list())
    hit <- list(exprs = exprs, prog = RxProgram$new(nb$irs, writes, nb$fns))
    assign(key, hit, envir = .rx_cache)
  }
  hit$prog
}

#' The `call` nodes of a program's IRs numbered: `irs` with each function name
#' replaced by its 0-based place in `fns`, the functions `RxProgram$new()` calls
#' @noRd
rx_number_calls <- function(irs) {
  fns <- character()
  number <- function(ir) {
    if (!is.list(ir)) return(ir)
    if (identical(ir[[1]], "call")) {
      if (!ir[[2]] %in% fns) fns <<- c(fns, ir[[2]])
      ir[[2]] <- match(ir[[2]], fns) - 1L
      ir[-(1:2)] <- lapply(ir[-(1:2)], number)
      return(ir)
    }
    if (identical(ir[[1]], "const") || identical(ir[[1]], "constv")) return(ir)
    # a node (its operator first) or a list of nodes (case_when()'s pairs)
    if (is.character(ir[[1]])) ir[-1] <- lapply(ir[-1], number) else ir <- lapply(ir, number)
    ir
  }
  irs <- lapply(irs, number)
  list(irs = irs, fns = lapply(fns, rx_call_fun))
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
rx_program <- function(step, idx, cols, globals, global_step = FALSE,
                       match_names = character()) {
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
  if (!rx_rng_heads_ok(heads)) return(rx_no("an RNG kind the Rust path was not verified under"))
  names <- unique(unlist(lapply(comp, `[[`, "syms")))
  # 4: a column a standing match adds (`.partner`, `.role`, `.group_id`,
  # `partner_<col>`), built by the native step from `state$match`
  src <- rep(4L, length(names))
  own <- !names %in% match_names
  if (any(own)) {
    s1 <- rx_sources(names[own], cols, globals, envs[[1]])
    if (is.null(s1)) return(rx_no("a name found nowhere"))
    src[own] <- s1
  }
  # inputs in the order the native step gathers them: columns, globals, scope
  # values, match columns
  o <- order(src)
  names <- names[o]
  src <- src[o]
  targets <- vapply(rules, `[[`, "", "target")
  # a global step's rules see the globals written before them; a column of the
  # same name hides the global, as in the R engine's data mask
  writes <- if (identical(global_step, "by")) {
    # `.by` rules: a later rule reads the column an earlier one wrote
    w <- match(targets, names) - 1L
    w[is.na(w) | !(src[w + 1L] %in% 1L)] <- -1L
    as.integer(w)
  } else if (isTRUE(global_step)) {
    w <- match(targets, names) - 1L
    w[is.na(w) | (src[w + 1L] %in% 1L)] <- -1L
    as.integer(w)
  } else rep(-1L, length(idx))
  prog <- rx_new_program(lapply(rules, function(r) rlang::quo_get_expr(r$quo)), names, writes,
                         lapply(comp, function(cp) rx_bind(cp$ir, names)))
  list(prog = prog, names = names, src = src, env = envs[[1]], targets = targets,
       cols = names[src == 1L], globs = names[src == 2L], scope = names[src == 3L],
       mcols = names[src == 4L])
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

# the parts of a step's shape that never change during a run, for the kernels
# and for "auto" to decide whether a run has anything to compile
rx_rules_ok <- function(step) {
  is.null(step$by) && !any(vapply(rx_rules(step), is_rx_no, NA)) &&
    !(step$uses_match %||% reads_match(lapply(step$rules, `[[`, "quo"))) &&
    !any(vapply(step$rules, function(r) r$target == ".cell" || startsWith(r$target, "partner_"), NA))
}
# at least one rule compiled (the others are evaluated by R inside the native step)
rx_rules_mixed_ok <- function(step) {
  if (!is.null(step$by)) return(rx_by_ok(step))
  is.null(step$by) && any(!vapply(rx_rules(step), is_rx_no, NA)) &&
    (!(step$uses_match %||% reads_match(lapply(step$rules, `[[`, "quo"))) ||
       !is.null(rx_match_names(step))) &&
    !any(vapply(step$rules, function(r) r$target == ".cell" || startsWith(r$target, "partner_"), NA))
}
# The match columns a step's rules read (`.partner`, `.role`, `.group_id`,
# `partner_<col>`), when every rule compiled (an R-evaluated rule would not see
# them) and the names are known; `NULL` otherwise (the R path)
rx_match_names <- function(step) {
  quos <- lapply(step$rules, `[[`, "quo")
  reads <- read_vars(quos)
  if (is.null(reads) || any(vapply(rx_rules(step), is_rx_no, NA))) return(NULL)
  reads[reads %in% c(".partner", ".role", ".group_id") | startsWith(reads, "partner_")]
}

# `.by` rules: every rule compiled, one `.by` column, nothing read of a pairing
rx_by_ok <- function(step) {
  !is.null(step$by) && length(by_columns(step$by)) == 1L &&
    !any(vapply(rx_rules(step), is_rx_no, NA)) &&
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
      shape <- if (!is.null(s$by) && cls == "abm_global" &&
                   !rlang::is_symbol(rlang::quo_get_expr(s$by))) "`.by` other than a name"
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
