# Internal helpers -------------------------------------------------------

#' Abort with a tidyABM error class
#' @noRd
abm_abort <- function(message, class = NULL, call = rlang::caller_env(),
                      .envir = parent.frame(), ...) {
  cli::cli_abort(
    message,
    class = c(class, "tidyABM_error"),
    call = call,
    .envir = .envir,
    ...
  )
}

#' Is `x` a one-sided or two-sided formula?
#' @noRd
is_formula2 <- function(x) rlang::is_formula(x) && length(x) == 3L

#' Is `x` a one-sided formula (`~expr`)?
#' @noRd
is_formula1 <- function(x) rlang::is_formula(x) && length(x) == 2L

#' The left-hand side of a two-sided formula, as a single name
#' @noRd
f_lhs_name <- function(x, arg = "rule", call = rlang::caller_env()) {
  lhs <- rlang::f_lhs(x)
  if (!rlang::is_symbol(lhs)) {
    abm_abort(
      c("The left-hand side of a {arg} must be a single column name.",
        "x" = "Got {.code {deparse1(lhs)}}."),
      class = "tidyABM_bad_formula", call = call
    )
  }
  name <- rlang::as_string(lhs)
  if (name %in% c(".x", ".y")) {
    abm_abort(
      c("{.code {name}} is derived from {.code .cell} and cannot be written.",
        "i" = "Write {.code .cell} instead, or move with {.fn abm_move}; {.code .x} and {.code .y} follow."),
      class = "tidyABM_reserved_column", call = call
    )
  }
  # the engine owns these: `.id` and `.group` say who an agent is, and the
  # other three are rewritten by every match. A rule that targets one used to
  # write it silently and have the write ignored or, for `.group`, mislabel
  # the output without moving the agent.
  if (name %in% c(".id", ".group", ".group_id", ".partner", ".role")) {
    abm_abort(
      c("{.code {name}} is managed by tidyABM and cannot be written by a {arg}.",
        "i" = "{.code .id} and {.code .group} identify an agent; {.code .role}, {.code .partner} and {.code .group_id} are set by {.fn abm_match}.",
        "i" = "To move an agent between kinds, keep the kind in an ordinary column (e.g. {.code state ~ \"infected\"})."),
      class = "tidyABM_reserved_column", call = call
    )
  }
  name
}

#' Catch a named non-rule argument in a step's `...` before it is evaluated
#'
#' `exprs` are the unevaluated dots (`rlang::enexprs(...)`). The rule steps
#' take `...` first, so a qualifier written without its dot (`scope =
#' "population"`, `by = firm`) lands in `...` and used to be evaluated as if it
#' were a rule: "object 'firm' not found", or "argument 2 is <character>",
#' neither of which names the mistake. Nothing is evaluated here, so a
#' condition such as `when = money > 0` is reported as a misplaced `when`
#' rather than as a missing object.
#'
#' Only entries that cannot be a rule are refused: a qualifier missing its dot,
#' a condition-like name, a constant, or `col = <expression of col>` (a rule
#' written with `=`). A named call that might build a formula (`r1 =
#' make_rule()`) is left to be evaluated, as it always was.
#' @noRd
check_rule_dots <- function(exprs, fn, call = rlang::caller_env()) {
  nms <- names(exprs)
  if (is.null(nms)) return(invisible(NULL))
  # found through the namespace this helper lives in
  formals_fn <- names(formals(get(fn, mode = "function")))
  conditionish <- c("when", "if", "where", "cond", "condition")
  for (i in which(nzchar(nms))) {
    e <- exprs[[i]]
    if (rlang::is_formula(e) || rlang::is_call(e, "~")) next
    nm <- nms[[i]]
    dotted <- paste0(".", nm)
    if (dotted %in% formals_fn) {
      abm_abort(
        c("{.arg {nm}} is not an argument of {.fn {fn}}.",
          "i" = "Did you mean {.arg {dotted}}? The qualifiers of a rule step are dot-prefixed, so they cannot collide with a column name.",
          "x" = "Got {.code {nm} = {deparse1(e)}}, which was read as a rule."),
        class = "tidyABM_bad_arg", call = call
      )
    }
    # a name bound to a formula (`r1 = f`) passes, and so does a call that
    # may return one
    is_cond <- nm %in% conditionish
    self_ref <- rlang::is_call(e) && nm %in% all.vars(e)
    if (rlang::is_symbol(e) && !is_cond) next
    if (rlang::is_call(e) && !is_cond && !self_ref) next
    hint <- if (is_cond) {
      c("i" = "A rule has no {.arg {nm}}: put the condition inside it, {.code col ~ if_else(<condition>, <new value>, col)}, or use {.code abm_birth(when =)} / {.code abm_death(when =)} / {.code abm_tell(when =)}.")
    } else if (self_ref) {
      c("i" = "A rule is written with {.code ~}, not {.code =}: {.code {nm} ~ {deparse1(e)}}.")
    } else {
      character()
    }
    named <- setdiff(formals_fn, "...")
    abm_abort(
      c("{.arg {nm}} is not an argument of {.fn {fn}}.",
        "x" = "Got {.code {nm} = {deparse1(e)}}. Everything in {.code ...} must be a {.code column ~ expression} rule.",
        hint,
        if (length(named)) c("i" = "{.fn {fn}}'s named arguments are {.arg {named}}.")),
      class = "tidyABM_bad_arg", call = call
    )
  }
  invisible(NULL)
}

#' The candidates in `pool` that look like a typo of `name`
#'
#' Edit distance at most 1 for short names and at most 2 otherwise, with a
#' case-insensitive match always counted. Used by the "did you mean" hints.
#' @noRd
near_names <- function(name, pool) {
  pool <- setdiff(unique(pool[!is.na(pool)]), name)
  if (!length(pool) || !nzchar(name)) return(character())
  d <- utils::adist(name, pool, ignore.case = TRUE)[1, ]
  limit <- if (nchar(name) <= 4L) 1L else 2L
  out <- pool[d <= limit]
  out[order(d[d <= limit])]
}

#' Another error's message, safe to hand to cli as a bullet
#'
#' cli interpolates `{}`; a message quoting user code may contain them.
#' @noRd
cli_escape <- function(x) {
  x <- cli::ansi_strip(x)
  gsub("}", "}}", gsub("{", "{{", x, fixed = TRUE), fixed = TRUE)
}

#' Another error's message as cli bullets: the first line marked `x`, the rest
#' indented, with rlang's own `i`/`!` markers taken off
#' @noRd
error_lines <- function(e) {
  lines <- unlist(strsplit(cli_escape(conditionMessage(e)), "\n", fixed = TRUE))
  lines <- sub("^[!ix*] ", "", trimws(lines))
  lines <- lines[nzchar(lines)]
  if (!length(lines)) return(character())
  stats::setNames(lines, c("x", rep(" ", length(lines) - 1L)))
}

#' The name a "not found" error complains about, or `NULL`
#' @noRd
missing_object_name <- function(cnd) {
  msgs <- character()
  c0 <- cnd
  while (!is.null(c0)) {
    msgs <- c(msgs, conditionMessage(c0))
    c0 <- c0$parent
  }
  m <- regmatches(msgs, regexec("object '([^']+)' not found", msgs))
  hit <- Filter(length, m)
  if (!length(hit)) {
    m <- regmatches(msgs, regexec("could not find function \"([^\"]+)\"", msgs))
    hit <- Filter(length, m)
    if (!length(hit)) return(NULL)
    return(list(name = hit[[1]][[2]], fn = TRUE))
  }
  list(name = hit[[1]][[2]], fn = FALSE)
}

#' Variables referenced on the right-hand side of a formula
#' @noRd
f_rhs_vars <- function(x) {
  all.vars(rlang::f_rhs(x))
}

#' Capture an expression argument as a quosure, or return NULL if absent
#' @noRd
enquo_or_null <- function(quo) {
  if (rlang::quo_is_null(quo) || rlang::quo_is_missing(quo)) NULL else quo
}

#' Column names named by a tidyselect-ish `by =` argument
#'
#' `by` is captured as a quosure. It may be a single symbol (`opinion`),
#' a call to `c()` (`c(x, y)`), or a character vector.
#' @noRd
by_columns <- function(by_quo, call = rlang::caller_env()) {
  if (is.null(by_quo)) return(NULL)
  expr <- rlang::quo_get_expr(by_quo)
  if (rlang::is_symbol(expr)) return(rlang::as_string(expr))
  if (rlang::is_call(expr, "c")) {
    parts <- as.list(expr)[-1]
    return(vapply(parts, function(p) {
      if (rlang::is_symbol(p)) rlang::as_string(p) else as.character(p)
    }, character(1)))
  }
  val <- rlang::eval_tidy(by_quo)
  if (is.character(val)) return(val)
  abm_abort(
    "{.arg by} must be a column name, {.code c(col1, col2)}, or a character vector.",
    class = "tidyABM_bad_by", call = call
  )
}

#' Seed the random number generator for the calling function only
#'
#' The caller's random state is put back when it returns: restored if there
#' was one, removed if there was none, so a seeded run in a fresh session does
#' not leave the session on a fixed stream.
#' @noRd
local_seed <- function(seed, frame = parent.frame()) {
  had <- exists(".Random.seed", .GlobalEnv, inherits = FALSE)
  old <- if (had) get(".Random.seed", .GlobalEnv, inherits = FALSE) else NULL
  set.seed(seed)
  restore <- function() {
    if (had) {
      assign(".Random.seed", old, envir = .GlobalEnv)
    } else if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }
  # registered on the caller's exit; the call holds the closure itself, so it
  # needs no name to be visible from there
  do.call(on.exit, list(as.call(list(restore)), TRUE), envir = frame)
  invisible(seed)
}

#' Shuffle a vector, safely
#'
#' `sample(x)` reinterprets a length-1 numeric `x` as `seq_len(x)`, which for a
#' vector of agent ids means a population of one silently becomes a population
#' of `.id` agents. Every shuffle in the package goes through here.
#' @noRd
shuffle <- function(x) if (length(x) <= 1L) x else sample(x)

#' Draw `k` values from `x` with replacement, safely
#'
#' Same trap as [shuffle()]: `sample(x, k, replace = TRUE)` with a length-1 `x`
#' draws from `seq_len(x)` instead of from `x`.
#' @noRd
draw_from <- function(x, k) x[sample.int(length(x), k, replace = TRUE)]

#' Bind a list of tibbles with differing schemas into one long tibble
#' @noRd
bind_groups <- function(groups) {
  if (length(groups) == 1L) {
    g <- groups[[1]]
    # a group is already a tibble; `as_tibble()` would re-validate its names
    # on every step of every tick
    if (inherits(g, "tbl_df")) return(g)
    return(tibble::as_tibble(g))
  }
  # what `bind_rows()` does, without its argument handling: this runs on
  # every step of every tick
  do.call(vctrs::vec_rbind, unname(groups))
}

#' The columns a set of quosures reads through `partner_<col>`
#' @noRd
partner_vars <- function(quos) {
  vars <- unlist(lapply(Filter(Negate(is.null), quos), function(q) {
    all.vars(rlang::quo_get_expr(q))
  }), use.names = FALSE)
  sub("^partner_", "", grep("^partner_", vars, value = TRUE))
}

#' Edges without the internal columns `abm_draw()` attaches to them
#' @noRd
strip_draws <- function(edges) {
  if (is.null(edges)) return(NULL)
  keep <- !startsWith(names(edges), ".draw_") & !startsWith(names(edges), ".back_")
  edges[, keep, drop = FALSE]
}

#' Membership that understands a set-valued column
#'
#' A set-valued agent column is a list column, so in a pair view `own_sellers`
#' arrives as a list with one element per (chooser, candidate) row. Base `%in%`
#' coerces that side with `as.character()`, compares `"4"` against `"4:5"`, and
#' quietly returns `FALSE` for every row -- which turns `among = .id %in%
#' own_sellers` into a match that pairs nobody, with no error to say so. When
#' the right-hand side is a list the question is asked row by row instead,
#' which is what the formula plainly means.
#'
#' With an atomic right-hand side this is `match(x, table, nomatch = 0) > 0`,
#' base's own definition, so every formula that already worked is unaffected.
#'
#' The list case runs on a pair view, so it is called with (choosers x
#' candidates) rows and sits on the hot path of every `among`. It is done as
#' one comparison over the unlisted sets rather than a `match()` per row: the
#' loop cost 22% of a Lengnick month. `NA` matches `NA`, as it does in base.
#' @noRd
in_rowwise <- function(x, table) {
  if (!is.list(table) || is.data.frame(table)) {
    return(match(x, table, nomatch = 0L) > 0L)
  }
  n <- max(length(x), length(table))
  if (n == 0L) return(logical(0))
  table <- rep_len(table, n)
  len <- lengths(table)

  if (!is.list(x)) {
    if (!sum(len)) return(rep(FALSE, n))
    flat <- unlist(table, use.names = FALSE)
    xi <- rep(rep_len(x, n), len)
    hit <- (flat == xi) | (is.na(flat) & is.na(xi))
    hit[is.na(hit)] <- FALSE
    out <- logical(n)
    out[rep(seq_len(n), len)[hit]] <- TRUE
    return(out)
  }
  x <- rep_len(x, n)
  vapply(seq_len(n),
         function(i) any(match(x[[i]], table[[i]], nomatch = 0L) > 0L),
         logical(1))
}

#' The environment one formula is evaluated in
#'
#' The globals go in scope, and the grammar's own operators go innermost so a
#' formula means the same thing wherever it is written: in `abm_rules()`, in
#' `abm_match(among =)`, in `abm_neighbours(within =)`.
#' @noRd
abm_eval_env <- function(quo, globals, n = NULL) {
  env <- rlang::quo_get_env(quo)
  if (length(globals)) env <- rlang::new_environment(globals, parent = env)
  # `n` is bound when the expression is evaluated outside a dplyr mask, so
  # `n()` still means the number of rows
  ops <- list(`%in%` = in_rowwise)
  if (!is.null(n)) ops$n <- function() n
  rlang::new_environment(ops, parent = env)
}

# dplyr's context helpers: the only things an ungrouped `mutate()` supplies
# that `eval_tidy()` over the same tibble does not. `n()` is bound by
# [abm_eval_env()] instead, so it is not listed.
mask_only_fns <- c(
  "row_number", "cur_group", "cur_group_id", "cur_group_rows", "cur_column",
  "cur_data", "cur_data_all", "across", "pick", "if_any", "if_all", "c_across"
)

#' Does an expression call anything only a dplyr mask can answer?
#'
#' A qualified `dplyr::n()` counts too: it reaches past the bound `n`.
#' @noRd
needs_mask <- function(expr) {
  if (!is.call(expr)) return(FALSE)
  head <- expr[[1]]
  if (rlang::is_call(head, "::")) {
    fn <- rlang::as_string(head[[3]])
    if (fn == "n" || fn %in% mask_only_fns) return(TRUE)
  } else if (rlang::is_symbol(head) && rlang::as_string(head) %in% mask_only_fns) {
    return(TRUE)
  }
  any(vapply(as.list(expr)[-1], needs_mask, logical(1)))
}

# Functions whose value on a row depends on that row alone. A rule built only
# from these means the same thing evaluated per pair as over the population,
# so the per-group evaluation a standing match normally forces can be skipped.
# Nothing here draws, counts, indexes or aggregates: `n()`, `sample()`, `[`,
# `sum()` and any function not listed keep the grouped path.
# ponytail: an allowlist; a rule calling anything else is merely slow, not wrong
elementwise_fns <- c(
  "+", "-", "*", "/", "^", "%%", "%/%", "==", "!=", "<", ">", "<=", ">=",
  "&", "|", "!", "(", "~", "if_else", "ifelse", "case_when", "coalesce",
  "between", "is.na", "pmin", "pmax", "abs", "sqrt", "exp", "log", "log1p",
  "round", "floor", "ceiling", "trunc", "sign", "%in%", "xor", "as.integer",
  "as.numeric", "as.double", "as.logical", "as.character", "paste", "paste0",
  "nchar", "tolower", "toupper"
)

#' Is every call in an expression elementwise?
#' @noRd
is_elementwise <- function(expr) {
  if (!is.call(expr)) return(TRUE)
  head <- expr[[1]]
  if (rlang::is_call(head, "::")) head <- head[[3]]
  if (!rlang::is_symbol(head) || !rlang::as_string(head) %in% elementwise_fns) {
    return(FALSE)
  }
  all(vapply(as.list(expr)[-1], is_elementwise, logical(1)))
}
