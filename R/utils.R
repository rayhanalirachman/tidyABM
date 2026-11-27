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
  if (startsWith(name, "cell_")) {
    abm_abort(
      c("{.code {name}} is read-only and cannot be written by a {arg}.",
        "i" = "{.code cell_<col>} reads the column of the patch an agent is standing on.",
        "i" = "To write to the patch, use {.code abm_tell(..., to = .cell)}."),
      class = "tidyABM_reserved_column", call = call
    )
  }
  # the engine owns these: `.id` and `.group` say who an agent is, `.tick` is
  # the current tick, `.dist` is spatial distance, and the rest are set by match
  if (name %in% c(".id", ".group", ".group_id", ".partner", ".role",
                  ".cost", ".tick", ".dist")) {
    abm_abort(
      c("{.code {name}} is managed by tidyABM and cannot be written by a {arg}.",
        "i" = "{.code .id} and {.code .group} identify an agent; {.code .tick} is the current tick; {.code .role}, {.code .partner}, {.code .cost} and {.code .group_id} are set by {.fn abm_match}.",
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
  conditionish <- setdiff(c("when", "if", "where", "cond", "condition"), formals_fn)
  for (i in which(nzchar(nms))) {
    e <- exprs[[i]]
    if (rlang::is_formula(e) || rlang::is_call(e, "~")) next
    nm <- nms[[i]]
    dotted <- paste0(".", nm)
    if (dotted %in% formals_fn && !nm %in% formals_fn) {
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

#' Does `rowsum()` give, group by group, what `sum()` gives for `x`?
#'
#' Both add in order from zero, `sum()` in a `long double`; where that is a
#' plain `double` (`.Machine$sizeof.longdouble` 8, or 0 when R was built
#' without one, as on arm64) the two make the same bits for a bare double
#' vector. Elsewhere `sum()` keeps the wider accumulator, so no.
#' @noRd
sum_is_rowsum <- function(x) {
  .Machine$sizeof.longdouble <= 8L && is.double(x) && is.null(attributes(x))
}

#' `tibble::new_tibble(x, nrow = n)` for a named list, without its argument
#' handling: the same object, built several times per step
#' @noRd
new_tbl <- function(x, n) {
  vctrs::new_data_frame(x, n = as.integer(n), class = c("tbl_df", "tbl"))
}

#' Set one whole column of a group tibble
#'
#' With the class off: `$<-.tbl_df` re-checks the whole tibble, and the engine
#' writes a column of the right length several times per step.
#' @noRd
set_col <- function(g, name, value) {
  cls <- oldClass(g)
  g <- unclass(g)
  g[[name]] <- value
  oldClass(g) <- cls
  g
}

#' Bind a list of tibbles with differing schemas into one long tibble
#'
#' `cols` names the columns the caller will read, from [read_vars()]; `.id`
#' and `.group` come too unless `ids = FALSE`, for a caller that only evaluates
#' a formula over the result. Binding every column of every group on every step
#' was three quarters of a tick in a model with a small and a large group.
#' @noRd
bind_groups <- function(groups, cols = NULL, ids = TRUE) {
  if (length(groups) == 1L) {
    g <- groups[[1]]
    # a group is already a tibble; `as_tibble()` would re-validate its names
    # on every step of every tick
    if (inherits(g, "tbl_df")) return(g)
    return(tibble::as_tibble(g))
  }
  # what `bind_rows()` does, without its argument handling: this runs on
  # every step of every tick. A group is cut down to the wanted columns as a
  # bare data frame; a tibble built per group cost more than stacking them.
  want <- if (!is.null(cols)) c(if (ids) c(".id", ".group"), cols)
  narrow <- function(g, keep) {
    vctrs::new_data_frame(unclass(g)[keep], n = vctrs::vec_size(g))
  }
  as_tbl <- function(df) {
    class(df) <- c("tbl_df", "tbl", "data.frame")
    df
  }
  if (!isTRUE(.bound$on)) {
    if (is.null(want)) return(do.call(vctrs::vec_rbind, unname(groups)))
    return(as_tbl(do.call(vctrs::vec_rbind, unname(lapply(groups, function(g) {
      narrow(g, names(g) %in% want)
    })))))
  }
  # In a large population (`.bound$on`, set by `run_once()`) `.id` and `.group`
  # are not stacked again while they stand: nearly every aggregate reads
  # `.group`, and the two change only at a birth or a death. What is kept is
  # checked against the groups' own vectors on each use.
  parts <- list(.id = lapply(groups, .subset2, ".id"),
                .group = lapply(groups, .subset2, ".group"))
  nms <- model_columns(groups)
  if (!is.null(want)) nms <- nms[nms %in% want]
  hit <- identical(.bound$parts, parts)
  out <- do.call(vctrs::vec_rbind, unname(lapply(groups, function(g) {
    narrow(g, names(g) %in% nms & !(hit & names(g) %in% c(".id", ".group")))
  })))
  if (hit) {
    out <- tibble::new_tibble(c(.bound$val, unclass(out))[nms], nrow = nrow(out))
  } else if (all(c(".id", ".group") %in% nms)) {
    .bound$parts <- parts
    .bound$val <- unclass(out)[c(".id", ".group")]
    .bound$is <- list()
  }
  as_tbl(out)
}

# `.id` and `.group` as [bind_groups()] last stacked them, the vectors they
# were stacked from, and what [group_is()] has been asked of that `.group`
.bound <- new.env()

#' `==`, remembering `.group == "<name>"` over the stacked population
#'
#' An aggregate over one kind of agent is written `sum(x[.group == "firm"])`,
#' so a model asks this of the same `.group` vector dozens of times a tick.
#' The answer is kept with the stacked `.group` it was asked of and dropped
#' when that is stacked again; anything else is base `==`.
#' @noRd
group_is <- function(e1, e2) {
  if (length(e1) == length(.bound$val$.group) && is.character(e2) &&
      length(e2) == 1L && !is.na(e2) && identical(e1, .bound$val$.group)) {
    if (is.null(.bound$is[[e2]])) .bound$is[[e2]] <- e1 == e2
    return(.bound$is[[e2]])
  }
  e1 == e2
}

#' The names a set of formulas mentions, for [bind_groups()] to bind only those
#'
#' `partner_<col>` and `own_<col>` read `<col>`. `NULL`, which binds everything, when a
#' formula could reach a column without naming it.
#' @noRd
read_vars <- function(quos) {
  nms <- as.character(unlist(lapply(Filter(Negate(is.null), quos), function(q) {
    all.names(rlang::quo_get_expr(q))
  }), use.names = FALSE))
  if (any(c(mask_only_fns, ".data", "get", "mget", "eval") %in% nms)) return(NULL)
  pre <- startsWith(nms, "partner_") | startsWith(nms, "own_")
  out <- if (any(pre)) c(nms, sub("^(partner|own)_", "", nms[pre])) else nms
  if (any(startsWith(nms, "cell_"))) out <- c(out, ".cell")
  if (".dist" %in% nms) out <- c(out, ".x", ".y", ".cell")
  out
}

#' Could these formulas read what a match adds: `.partner`, `.role`,
#' `.group_id`, `.cost` or a `partner_<col>`? If not, [augment_group()] is skipped.
#' @noRd
reads_match <- function(quos) {
  reads <- read_vars(quos)
  is.null(reads) || any(startsWith(reads, "partner_")) ||
    any(c(".group_id", ".role", ".partner", ".cost") %in% reads)
}

#' Which of the match columns a set of formulas reads, for [augment_group()]
#' to build only those; `NULL` (all of them) when that cannot be known
#' @noRd
match_cols_read <- function(quos, state) {
  reads <- read_vars(quos)
  if (is.null(reads) || length(state$relations)) return(NULL)
  intersect(c(".group_id", ".role", ".partner", ".cost"), reads)
}

#' The patch columns a set of quosures reads through `cell_<col>`
#' @noRd
cell_vars <- function(quos) {
  vars <- unlist(lapply(Filter(Negate(is.null), quos), function(q) {
    all.vars(rlang::quo_get_expr(q))
  }), use.names = FALSE)
  unique(sub("^cell_", "", grep("^cell_", vars, value = TRUE)))
}

#' Attach read-only `cell_<col>` columns for a mobile group or stacked tibble
#' @noRd
attach_cell_columns <- function(df, state, cvars) {
  if (!length(cvars)) return(df)
  lat <- state$lattice
  if (is.null(lat) || is.null(lat$on) || !lat$on %in% names(state$groups)) {
    return(df)
  }
  wired <- state$groups[[lat$on]]
  cvars <- cvars[cvars %in% names(wired)]
  if (!length(cvars) || !".cell" %in% names(df)) return(df)
  n <- vctrs::vec_size(df)
  idx <- if (identical(wired$.id, seq_len(nrow(wired)))) {
    df$.cell
  } else {
    match(df$.cell, wired$.id)
  }
  cols <- as.list(df)
  for (nm in cvars) {
    cols[[paste0("cell_", nm)]] <- wired[[nm]][idx]
  }
  new_tbl(cols, n)
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

.run_ctx <- new.env(parent = emptyenv())
.run_ctx$tick <- 0L

#' Bind read-only `.tick` into an evaluation environment via active binding
#' @noRd
bind_tick <- function(env) {
  makeActiveBinding(".tick", function() .run_ctx$tick, env)
  env
}

#' Environment holding `globals` plus active `.tick` (for dplyr `reframe`/`mutate`)
#' @noRd
with_tick_env <- function(globals, parent) {
  env <- if (length(globals)) list2env(globals, parent = parent) else new.env(parent = parent)
  bind_tick(env)
}

#' The environment one formula is evaluated in
#'
#' The globals go in scope, and the grammar's own operators go innermost so a
#' formula means the same thing wherever it is written: in `abm_rules()`, in
#' `abm_match(among =)`, in `abm_neighbours(within =)`.
#' @noRd
abm_eval_env <- function(quo, globals, n = NULL) {
  env <- rlang::quo_get_env(quo)
  # base `list2env()`: built once per rule per group per tick
  if (length(globals)) env <- list2env(globals, parent = env)
  # `n` is bound when the expression is evaluated outside a dplyr mask, so
  # `n()` still means the number of rows
  ops <- list(`%in%` = in_rowwise)
  # only in a large population: see [bind_groups()]
  if (isTRUE(.bound$on)) ops$`==` <- group_is
  if (!is.null(n)) ops$n <- function() n
  bind_tick(list2env(ops, parent = env))
}

#' [abm_eval_env()], kept while the formula's environment, the globals and `n`
#' stay the same
#'
#' Only for [eval_rule()], of an expression that cannot write into the scope
#' it is handed (see `scope_writers`): building the scope
#' was a third of what a short rule cost. Emptied at the start of every run.
#' @noRd
eval_env_cached <- function(quo, globals, n) {
  of <- rlang::quo_get_env(quo)
  c <- .eval_env
  if (!identical(c$of, of) || !identical(c$globals, globals)) {
    c$of <- of
    c$globals <- globals
    c$genv <- if (length(globals)) list2env(globals, parent = of) else of
    c$by_n <- list()
  }
  key <- as.character(n)
  env <- c$by_n[[key]]
  if (is.null(env)) {
    ops <- list(`%in%` = in_rowwise)
    if (isTRUE(.bound$on)) ops$`==` <- group_is
    ops$n <- local({ m <- n; function() m })
    env <- bind_tick(list2env(ops, parent = c$genv))
    # ponytail: one scope per population size seen; reset past 64 sizes
    if (length(c$by_n) >= 64L) c$by_n <- list()
    c$by_n[[key]] <- env
  }
  env
}

.eval_env <- new.env(parent = emptyenv())

# Names that keep an expression off [eval_rule()]'s base `eval()`: dplyr's
# context helpers and `::` (see [needs_mask()]), rlang's pronouns and `~`
# (a quosure or a formula: `eval_tidy()` gives those their own scope), and
# anything that could write into, or reach, the scope it is evaluated in,
# which [eval_env_cached()] keeps from one rule to the next
# what could write into, or reach, the scope an expression is evaluated in
scope_writers <- c(
  "<<-", "<-", "=", "->", "->>",
  "assign", "delayedAssign", "makeActiveBinding", "rm", "remove", "eval",
  "evalq", "local", "environment", "parent.frame", "parent.env", "sys.frame",
  "sys.call", "sys.function", "match.call", "function", "new.env", "list2env",
  "attach", "with", "within", "get", "get0", "mget", "exists", "on.exit",
  "return", "missing", "quote", "bquote", "substitute", "Recall"
)
base_eval_blocked <- c(
  "row_number", "cur_group", "cur_group_id", "cur_group_rows", "cur_column",
  "cur_data", "cur_data_all", "across", "pick", "if_any", "if_all", "c_across",
  "::", ":::", ".data", ".env", ".top_env", "~", scope_writers
)

# Calls whose own warnings name themselves, not the frame evaluating them
# (`as.integer()` names the frame, so it is not here): base `eval()` and
# `eval_tidy()` report them alike
base_eval_ops <- c(
  "+", "-", "*", "/", "^", "%%", "%/%", "==", "!=", "<", ">", "<=", ">=",
  "&", "|", "!", "(", "is.na", "[", "[[", "max", "min", "any", "all",
  "sqrt", "log", "exp", "abs", "floor", "ceiling", "trunc", "sign", "length",
  "c", "sum"
)
# Closures: whatever they warn about, they warn about from their own frames
base_eval_closures <- c(
  "if_else", "coalesce", "pmin", "pmax", "ifelse", "between", "mean", "n",
  "runif", "rnorm", "rbinom", "rpois", "rexp", "sample", "%in%"
)

#' May [eval_rule()] evaluate `expr` with base `eval()`? Only if it names
#' nothing in `base_eval_blocked` and every call in it is reached through
#' `base_eval_ops`, down to a closure from `base_eval_closures` or a leaf
#' @noRd
base_eval_ok <- function(expr) {
  !any(match(all.names(expr), base_eval_blocked, 0L)) && base_eval_safe(expr)
}

base_eval_safe <- function(expr) {
  if (!is.call(expr)) return(TRUE)
  head <- expr[[1]]
  if (!is.symbol(head)) return(FALSE)
  head <- as.character(head)
  if (head %in% base_eval_closures) return(TRUE)
  if (!head %in% base_eval_ops) return(FALSE)
  for (a in as.list(expr)[-1]) if (!base_eval_safe(a)) return(FALSE)
  TRUE
}

.mark_memo <- new.env(parent = emptyenv())

#' [mark_quosures()] over a go block's steps, into `abm_repeat()`'s
#' @noRd
mark_steps <- function(steps) {
  lapply(steps, function(s) {
    if (inherits(s, "abm_repeat") || (inherits(s, "abm_match") && !is.null(s$steps))) {
      s$steps <- mark_steps(s$steps)
      mark_quosures(s)
    } else {
      mark_quosures(s)
    }
  })
}

#' `x` with every quosure in it marked with [base_eval_ok()], for
#' [eval_rule()] to read rather than work out on every tick
#' @noRd
mark_quosures <- function(x) {
  if (rlang::is_quosure(x)) {
    e <- rlang::quo_get_expr(x)
    # worked out once per expression for the session; the memo holds the
    # expression, so its address is not reused while the entry stands
    key <- rlang::obj_address(e)
    hit <- .mark_memo[[key]]
    if (is.null(hit) || !identical(hit$e, e)) {
      # a scope it cannot write into can be kept for the next evaluation
      hit <- list(e = e, base = base_eval_ok(e),
                  scope = !any(match(all.names(e), scope_writers, 0L)))
      assign(key, hit, envir = .mark_memo)
    }
    attr(x, "abm_base") <- hit$base
    attr(x, "abm_scope") <- hit$scope
    return(x)
  }
  if (is.list(x) && !is.data.frame(x)) {
    # only lists and quosures can hold a quosure
    for (i in seq_along(x)) {
      xi <- .subset2(x, i)
      if (typeof(xi) == "language" || (is.list(xi) && !is.data.frame(xi))) {
        x[[i]] <- mark_quosures(xi)
      }
    }
  }
  x
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
  # asked of every rule on every tick, and nearly always answered here
  if (!any(c(mask_only_fns, "::") %in% all.names(expr))) return(FALSE)
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

# elementwise and blind to which rows they are handed: not `ifelse()` (a
# one-value test), `%in%` (a column as the table) or `paste()` (`collapse =`)
nb_fast_fns <- setdiff(elementwise_fns, c("ifelse", "%in%", "paste", "paste0"))

# For [narrow_plan()]:
# Calls a rewritten rule may make around and between the group aggregates
narrow_outer_fns <- c(
  setdiff(nb_fast_fns, c("~", "case_when")),
  "sum", "mean", "min", "max", "prod", "any", "all", "median", "length",
  "if", "{", "isTRUE", "isFALSE", "&&", "||"
)

# Calls that cannot warn: an argument that reads a column other groups also
# have is evaluated over those groups' real rows on the stack, so it must not
# be able to say anything about them
narrow_quiet_fns <- c("(", "==", "!=", "<", ">", "<=", ">=", "&", "|", "!", "is.na")

narrow_na_fns <- c("sum", "min", "max", "mean", "median", "any", "all", "prod")

#' Is every call in an expression elementwise?
#' @noRd
is_elementwise <- function(expr, fns = elementwise_fns) {
  if (!is.call(expr)) return(TRUE)
  head <- expr[[1]]
  if (rlang::is_call(head, "::")) head <- head[[3]]
  if (!rlang::is_symbol(head) || !rlang::as_string(head) %in% fns) {
    return(FALSE)
  }
  all(vapply(as.list(expr)[-1], is_elementwise, logical(1), fns = fns))
}

# Calls that fold a group to one value. Such a call whose arguments only
# subset and compute elementwise draws nothing, so [eval_by_key()] can take it
# out of a grouped rule, evaluate it per group, and evaluate what is left of
# the rule once over every row.
aggregate_fns <- c("sum", "mean", "min", "max", "any", "all", "prod", "length", "n")

#' A grouped rule as `list(outer, aggs)`: the rule with each aggregate call
#' replaced by a placeholder `.agg<k>`, and the calls taken out
#' @noRd
split_aggregates <- function(expr) {
  aggs <- list()
  walk <- function(e) {
    if (!is.call(e)) return(e)
    if (is.symbol(e[[1]]) && as.character(e[[1]]) %in% aggregate_fns) {
      aggs[[length(aggs) + 1L]] <<- e
      return(as.symbol(paste0(".agg", length(aggs))))
    }
    as.call(lapply(as.list(e), walk))
  }
  list(outer = walk(expr), aggs = aggs)
}
