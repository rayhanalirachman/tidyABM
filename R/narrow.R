# Aggregates over one group, without the padded stack ----------------------
#
# A model with several groups writes an aggregate over one of them as
# `sum(x[.group == "firm"])` or `sum(status == 3L, na.rm = TRUE)`. Evaluated as
# written, both stack every group, pad the firm-only columns with `NA` for the
# households, and throw the padding away again. The elements that survive are
# the firm group's own, in the firm group's order, so the same expression
# evaluated over the firm rows alone gives the same vector, and every
# reduction of it the same bits.
#
# A rule is rewritten only when the whole of it is built from deterministic
# base functions (nothing draws, nothing assigns, no user function), so when
# anything at all is unexpected -- a warning, an error, a type, a length -- the
# rule is simply evaluated again the general way, which then says what it
# always said.

# The allowlists it uses, `narrow_outer_fns` and friends, are in utils.R.

#' Is `fun`, found under the name `f`, the function the allowlists mean?
#' @noRd
standard_fn <- function(f, fun) {
  identical(fun, get0(f, envir = baseenv(), mode = "function")) ||
    (f == "==" && identical(fun, group_is)) ||
    (f == "median" && identical(fun, stats::median)) ||
    (f %in% c("if_else", "case_when", "coalesce", "between") &&
       identical(fun, getExportedValue("dplyr", f)))
}

#' Every function an expression calls resolves, in `env`, to the standard one
#' @noRd
standard_fns_in <- function(fns, env, sig = NULL) {
  # `sig` is what the answer depends on beyond the run's own operators -- the
  # formula's environment and any global that is a function -- and with it
  # the answer is kept for the rest of the run
  if (!is.null(sig)) {
    key <- paste0("k", paste(fns, collapse = "\r"))
    hit <- .std_memo[[key]]
    if (!is.null(hit) && identical(hit$sig, sig)) return(hit$res)
  }
  res <- TRUE
  for (f in fns) {
    if (!standard_fn(f, get0(f, envir = env, mode = "function"))) {
      res <- FALSE
      break
    }
  }
  if (!is.null(sig)) assign(key, list(sig = sig, res = res), envir = .std_memo)
  res
}

.std_memo <- new.env(parent = emptyenv())

#' Could [narrow_plan()] rewrite any of these formulas? A look at their text
#' alone, so a model with no group aggregate never pays for a plan
#' @noRd
narrowable <- function(quos) {
  any(vapply(quos, function(q) {
    e <- rlang::quo_get_expr(q)
    nms <- all.names(e)
    (".group" %in% nms && "[" %in% nms) ||
      any(grepl("na.rm", deparse(e), fixed = TRUE))
  }, NA))
}

#' Each formula's plan, or `NULL` for a formula left as it is
#'
#' `list(rules, rest)`: per formula `list(expr, pre, rest, fns)` -- the formula
#' with each group aggregate replaced by a `.abm_nar<k>` symbol, the aggregates
#' (see [narrow_spec()]), the columns the rewritten formula still reads from the
#' stack, and the functions it calls -- and the columns every formula left
#' over reads. `NULL` when nothing is rewritten.
#' @noRd
narrow_plan <- function(quos, groups) {
  # `.group == "g"` must pick out exactly group `g`'s rows. The engine labels
  # every agent with its group's name when it is made (set-up, birth) and no
  # rule may write `.group`, so this holds for the run once it holds here.
  if (length(groups) < 2L || !narrow_group_labels(groups)) return(NULL)
  gcols <- lapply(groups, names)
  allc <- unique(unlist(gcols, use.names = FALSE))
  rules <- lapply(quos, function(q) {
    tryCatch(narrow_rule(rlang::quo_get_expr(q), gcols, allc), error = function(e) NULL)
  })
  if (all(vapply(rules, is.null, NA))) return(NULL)
  rest <- lapply(seq_along(quos), function(i) {
    if (is.null(rules[[i]])) read_vars(quos[i]) else rules[[i]]$rest
  })
  # a formula left over that could read any column needs them all
  if (any(vapply(seq_along(quos), function(i) is.null(rest[[i]]) && is.null(rules[[i]]), NA))) {
    return(NULL)
  }
  rest <- intersect(unlist(rest, use.names = FALSE), allc)
  list(rules = rules, rest = rest)
}

#' One formula's plan, or `NULL`
#' @noRd
narrow_rule <- function(expr, gcols, allc) {
  nms <- all.names(expr)
  if (any(c(mask_only_fns, ".data", ".env", "get", "mget", "eval", "evalq",
            "<-", "<<-", "=", "assign") %in% nms)) return(NULL)
  pre <- list()
  walk <- function(e, path) {
    if (!is.call(e)) return(e)
    head <- e[[1]]
    if (!rlang::is_symbol(head)) return(e)
    spec <- narrow_spec(e, gcols, allc)
    if (!is.null(spec)) {
      sym <- paste0(".abm_nar", length(pre) + 1L)
      pre[[sym]] <<- spec
      return(as.symbol(sym))
    }
    if (!rlang::as_string(head) %in% narrow_outer_fns) return(e)
    for (i in seq_along(e)[-1]) {
      if (!identical(e[[i]], quote(expr = ))) e[[i]] <- walk(e[[i]])
    }
    e
  }
  out <- walk(expr)
  if (!length(pre)) return(NULL)
  fns <- unique(call_heads(out))
  # the whole rule must be deterministic and standard
  if (anyNA(fns) || !all(fns %in% narrow_outer_fns)) return(NULL)
  rest <- intersect(all.names(out), allc)
  list(expr = out, pre = pre, rest = rest, fns = fns)
}

#' A group aggregate's plan, or `NULL`
#'
#' * `E[.group == "g"]` and `E[.group == "g" & C]`: `E` and `C` evaluated over
#'   group `g` alone, then `E[C]`.
#' * `f(E, na.rm = TRUE)`, every column `E` reads `g`'s alone: `f()` of `E`
#'   over group `g` alone, provided `E` is missing on a padding row, which is
#'   checked when it is evaluated.
#'
#' Columns `E` and `C` read must all be `g`'s, and any other name is checked to
#' be a single value when evaluated. A column another group also has is
#' allowed only in an expression that cannot warn.
#' @noRd
narrow_spec <- function(e, gcols, allc) {
  fn <- rlang::as_string(e[[1]])
  if (fn == "[") {
    if (length(e) != 3L || !is.null(names(e))) return(NULL)
    idx <- e[[3]]
    lit <- function(x) {
      if (rlang::is_call(x, "==", n = 2) && identical(x[[2]], quote(.group)) &&
          is.character(x[[3]]) && length(x[[3]]) == 1L && !is.na(x[[3]])) x[[3]]
    }
    g <- lit(idx)
    C <- NULL
    if (is.null(g) && rlang::is_call(idx, "&", n = 2)) {
      g <- lit(idx[[2]])
      C <- idx[[3]]
    }
    if (is.null(g)) return(NULL)
    E <- e[[2]]
    return(narrow_finish("sub", g, E, C, c("[", "==", if (!is.null(C)) "&"), gcols, allc))
  }
  if (fn %in% narrow_na_fns && length(e) == 3L &&
      identical(names(e), c("", "", "na.rm")) && isTRUE(e[[3]])) {
    E <- e[[2]]
    used <- intersect(all.vars(E), allc)
    owners <- names(gcols)[vapply(gcols, function(cc) any(used %in% cc), NA)]
    if (length(owners) != 1L) return(NULL)
    spec <- narrow_finish("narm", owners, E, NULL, fn, gcols, allc)
    # every column read is that group's alone
    if (!is.null(spec) && length(spec$shared)) return(NULL)
    return(spec)
  }
  NULL
}

#' @noRd
narrow_finish <- function(kind, g, E, C, own_fns, gcols, allc) {
  if (!g %in% names(gcols)) return(NULL)
  parts <- c(list(E), if (!is.null(C)) list(C))
  if (!all(vapply(parts, is_elementwise, NA, fns = nb_fast_fns))) return(NULL)
  vars <- unique(unlist(lapply(parts, all.vars), use.names = FALSE))
  if (any(c(".group", ".id") %in% vars)) return(NULL)
  cols <- intersect(vars, allc)
  if (!length(intersect(all.vars(E), allc)) || !all(cols %in% gcols[[g]])) return(NULL)
  others <- unlist(gcols[names(gcols) != g], use.names = FALSE)
  shared <- intersect(cols, others)
  heads <- unlist(lapply(parts, call_heads), use.names = FALSE)
  if (anyNA(heads)) return(NULL)
  if (length(shared) && !all(heads %in% narrow_quiet_fns)) return(NULL)
  list(kind = kind, g = g, E = E, C = C, f = if (kind == "narm") own_fns,
       cols = cols, shared = shared, scalars = setdiff(vars, allc),
       fns = unique(c(own_fns, heads)))
}

#' A rewritten formula's value, or `NULL` for "evaluate it the general way"
#'
#' `rest` is the stack of the columns the rewritten formula still reads, `env`
#' the formula's evaluation environment (globals and `n()` bound), `n` the
#' number of agents.
#' @noRd
narrow_eval <- function(plan, quo, groups, rest, env, n, sig = NULL) {
  if (!standard_fns_in(plan$fns, env, sig)) return(NULL)
  tryCatch({
    vals <- list()
    for (sym in names(plan$pre)) {
      v <- narrow_pre(plan$pre[[sym]], groups, env, sig)
      if (is.null(v)) return(NULL)
      vals[[sym]] <- v
    }
    data <- c(if (!is.null(rest)) unclass(rest), vals)
    rlang::eval_tidy(rlang::new_quosure(plan$expr, env), data = data)
  }, warning = function(w) NULL, error = function(e) NULL)
}

#' One group aggregate over its group alone, or `NULL`
#' @noRd
narrow_pre <- function(s, groups, env, sig = NULL) {
  g <- groups[[s$g]]
  ng <- vctrs::vec_size(g)
  if (!ng) return(NULL)
  if (!standard_fns_in(s$fns, env, sig)) return(NULL)
  for (v in s$scalars) {
    val <- get0(v, envir = env, inherits = TRUE)
    if (is.null(val) || !is.atomic(val) || length(val) != 1L ||
        !is.null(attributes(val))) return(NULL)
  }
  d <- unclass(g)[s$cols]
  for (v in s$cols) {
    col <- d[[v]]
    if (!is.atomic(col) || !is.null(attributes(col))) return(NULL)
    for (o in groups[names(groups) != s$g]) {
      oc <- .subset2(o, v)
      if (!is.null(oc) && (!is.atomic(oc) || !is.null(attributes(oc)) ||
                           typeof(oc) != typeof(col))) return(NULL)
    }
  }
  x <- rlang::eval_tidy(rlang::new_quosure(s$E, env), data = d)
  if (length(x) != ng || !is.atomic(x) || !is.null(attributes(x))) return(NULL)
  if (s$kind == "sub") {
    if (is.null(s$C)) return(x)
    keep <- rlang::eval_tidy(rlang::new_quosure(s$C, env), data = d)
    if (!is.logical(keep) || !is.null(attributes(keep)) ||
        !length(keep) %in% c(1L, ng)) return(NULL)
    return(x[keep])
  }
  # the padding rows: every column `NA`; their value must be one `na.rm` drops
  pad <- lapply(d, function(col) col[NA_integer_])
  if (any(vapply(groups[names(groups) != s$g], vctrs::vec_size, 0L) > 0L)) {
    p <- rlang::eval_tidy(rlang::new_quosure(s$E, env), data = pad)
    if (length(p) != 1L || !is.na(p)) return(NULL)
  }
  fun <- get0(s$f, envir = env, mode = "function")
  out <- fun(x, na.rm = TRUE)
  if (length(out) != 1L || !is.null(attributes(out))) return(NULL)
  out
}

#' Is every group's `.group` its own name, so that `.group == "g"` on the
#' stack picks out group `g`'s rows exactly?
#' @noRd
narrow_group_labels <- function(groups) {
  all(vapply(names(groups), function(nm) {
    lab <- .subset2(groups[[nm]], ".group")
    is.character(lab) && is.null(attributes(lab)) && !anyNA(lab) && all(lab == nm)
  }, NA))
}
