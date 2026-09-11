# The ODD protocol -------------------------------------------------------
#
# ODD (Grimm et al. 2006; 2010; 2020, JASSS 23(2):7) is the template a paper
# describes an ABM in. A tidyABM model is data rather than code -- `abm_setup()`
# returns a population and `abm_go()` returns an ordered list of typed steps --
# so the *mechanical* half of that template can be read straight off the two
# objects: what the entities are, what runs in what order, what each process
# reads and writes, where the randomness enters.
#
# The interpretive half cannot be, and nothing here guesses at it. Two rules
# hold throughout. Expressions are deparsed, never paraphrased into English:
# element 7 wants the equations, and an equation restated in prose is a
# different claim about the model. And no derived text ever asserts that an
# interpretive concept is *absent* -- "no adaptation" is a statement about what
# the modeller meant, and only the author can make it, so every such element is
# emitted as a marked hole.
#
# The walk over `go$steps` mirrors `run_step()` in `run.R`: the same switch over
# the same closed set of step classes, threading a `ctx` the way the scheduler
# threads its `state`. If a step class is added to the grammar it must be added
# to both switches, which is what `test-odd.R`'s completeness test checks.

odd_hole <- "**[author input required]**"

# The draws the grammar knows how to spot inside an expression: base R's two
# samplers and every random-generation function in `stats`. The list is
# enumerated rather than matched on a `^r[a-z]+$` pattern because that pattern
# also catches `reformulate`, `resid`, `reorder` and `runmed`, and a false
# positive here invents a source of randomness the model does not have.
#
# A miss is the worse direction, though. Element 4 is the one derived element
# whose failure mode is a false *negative* -- a stochasticity inventory that
# omits a real draw is a wrong claim about the model, not merely a thin one --
# which is why this errs towards listing generators no model in the corpus uses.
odd_rng_fns <- c(
  "sample", "sample.int",
  "rbeta", "rbinom", "rcauchy", "rchisq", "rexp", "rf", "rgamma", "rgeom",
  "rhyper", "rlnorm", "rlogis", "rmultinom", "rnbinom", "rnorm", "rpois",
  "rsignrank", "rsmirnov", "rt", "runif", "rweibull", "rwilcox"
)

# Which network types are drawn rather than constructed. `ring`, `complete`,
# `grid`, `line`, `manual` and `empty` are all determined by their arguments.
odd_random_networks <- c(
  random      = "each pair is joined with a fixed probability",
  poisson     = "each agent's degree is drawn from a Poisson distribution",
  scale_free  = "edges are attached preferentially, so the degree sequence is drawn"
)

# Which pairing modes draw. `"nearest"` minimises a distance or a cost, so it
# is the one mode that is not a draw.
odd_random_pairing <- c(
  random         = "agents are shuffled into groups",
  one_of         = "each agent draws a partner from the population",
  opposite_group = "both sides are shuffled before being paired",
  network        = "each agent draws among its network neighbours"
)

# Reading the pieces of a step -------------------------------------------

#' Deparse a quosure (or an expression) to a single line, or `NULL`
#' @noRd
odd_expr <- function(x) {
  if (is.null(x)) return(NULL)
  deparse1(if (rlang::is_quosure(x)) rlang::quo_get_expr(x) else x)
}

#' `target ~ expr` for each rule of a rule-bearing step
#'
#' One line per rule. R's own line breaking splits an expression wherever the
#' width runs out, mid-condition and mid-argument, which is verbatim and
#' unreadable; a long line at least breaks where the reader's editor chooses to.
#' @noRd
odd_rule_exprs <- function(rules) {
  vapply(rules, function(r) {
    paste0(r$target, " ~ ", deparse1(rlang::quo_get_expr(r$quo)))
  }, character(1))
}

#' @noRd
odd_targets <- function(rules) vapply(rules, function(r) r$target, character(1))

#' @noRd
odd_quos <- function(rules) lapply(rules, function(r) r$quo)

#' Every symbol read by a set of quosures
#' @noRd
odd_vars <- function(quos) {
  quos <- Filter(Negate(is.null), quos)
  if (!length(quos)) return(character())
  unique(unlist(lapply(quos, function(q) {
    all.vars(if (rlang::is_quosure(q)) rlang::quo_get_expr(q) else q)
  })))
}

#' Which random-number functions a set of quosures calls
#'
#' `all.names()` rather than `all.vars()`, because the thing being looked for is
#' the function being called, not a column being read.
#' @noRd
odd_rng <- function(quos) {
  quos <- Filter(Negate(is.null), quos)
  if (!length(quos)) return(character())
  nms <- unlist(lapply(quos, function(q) {
    all.names(if (rlang::is_quosure(q)) rlang::quo_get_expr(q) else q)
  }))
  intersect(odd_rng_fns, unique(nms))
}

# Describing one step ----------------------------------------------------

#' The context a step is described in
#'
#' The counterpart of `run_once()`'s `state`: what a step's description needs to
#' know about everything before it. `match` is the standing pairing, set by an
#' `abm_match` and starting empty, since a tick begins with none.
#' @noRd
new_odd_ctx <- function(model) {
  list(
    sizes   = vapply(model$groups, nrow, integer(1)),
    columns = lapply(model$groups, function(g) setdiff(names(g), c(".id", ".group"))),
    globals = names(model$globals),
    match   = NULL
  )
}

#' Describe one step, returning its fields and the context it leaves behind
#'
#' The same switch as [run_step()], over the same closed set of classes. Both
#' switches must list every step class in the grammar.
#' @noRd
describe_step <- function(step, ctx) {
  switch(
    class(step)[[1]],
    abm_match = {
      d <- describe_match(step, ctx)
      # the pairing stands until the tick ends, which is what makes the steps
      # after it about pairs rather than about agents
      ctx$match <- d
      list(desc = d, ctx = ctx)
    },
    abm_repeat = {
      inner <- describe_block(step$steps, ctx)
      list(desc = describe_repeat(step, inner$descs), ctx = inner$ctx)
    },
    abm_rules      = list(desc = describe_rules(step, ctx), ctx = ctx),
    abm_sequential = list(desc = describe_sequential(step, ctx), ctx = ctx),
    abm_global     = list(desc = describe_global(step, ctx), ctx = ctx),
    abm_neighbours = list(desc = describe_neighbours(step, ctx), ctx = ctx),
    abm_move       = list(desc = describe_move(step, ctx), ctx = ctx),
    abm_draw       = list(desc = describe_draw(step, ctx), ctx = ctx),
    abm_tell       = list(desc = describe_tell(step, ctx), ctx = ctx),
    abm_birth      = list(desc = describe_birth(step, ctx), ctx = ctx),
    abm_death      = list(desc = describe_death(step, ctx), ctx = ctx),
    abm_link       = list(desc = describe_link(step, ctx), ctx = ctx),
    abm_unlink     = list(desc = describe_link(step, ctx), ctx = ctx),
    abm_abort("Unknown step type {.cls {class(step)[[1]]}}.",
              class = "tidyABM_unknown_step")
  )
}

#' Describe an ordered block of steps, threading the context through it
#' @noRd
describe_block <- function(steps, ctx) {
  descs <- vector("list", length(steps))
  for (i in seq_along(steps)) {
    out <- describe_step(steps[[i]], ctx)
    descs[[i]] <- out$desc
    ctx <- out$ctx
  }
  list(descs = descs, ctx = ctx)
}

#' @noRd
describe_match <- function(step, ctx) {
  # every `abm_match` carries every argument whether or not its mode looks at
  # one: `from` is set on all of them and means nothing outside `"network"`.
  # Reporting a dead field would describe a model the run does not have.
  used <- match_relevant_args[[step$pair]]
  keep <- function(nm, x) if (nm %in% used) x else NULL
  role_expr <- if ("role" %in% used) step$role else NULL
  quos <- list(keep("eligible", step$eligible), keep("among", step$among),
               keep("weight", step$weight), keep("cost", step$cost), role_expr)
  args <- c(
    sprintf('pair = "%s"', step$pair),
    if ("size" %in% used && step$size != 2L) paste0("size = ", step$size),
    if (!is.null(keep("by", step$by))) paste0("by = ", odd_expr(step$by)),
    if (!is.null(role_expr)) paste0("role = ", odd_expr(role_expr)),
    if (!is.null(keep("eligible", step$eligible))) paste0("eligible = ", odd_expr(step$eligible)),
    if (!is.null(keep("among", step$among))) paste0("among = ", odd_expr(step$among)),
    if (!is.null(keep("weight", step$weight))) paste0("weight = ", odd_expr(step$weight)),
    if (!is.null(keep("cost", step$cost))) paste0("cost = ", odd_expr(step$cost)),
    if (!is.null(step$dot_by)) paste0(".by = ", odd_expr(step$dot_by)),
    if (step$pair == "network") sprintf('from = "%s"', step$from)
  )
  list(
    type = "abm_match", kind = "interaction",
    pair = step$pair,
    size = if ("size" %in% used) step$size else 2L,
    mutual = step$pair %in% c("random", "opposite_group"),
    roles = if (is.null(role_expr)) NULL
            else names(as.list(rlang::quo_get_expr(role_expr))[-1]),
    by = keep("by", odd_expr(step$by)),
    partition = odd_expr(step$dot_by),
    from = if (step$pair == "network") step$from else NULL,
    random = step$pair %in% names(odd_random_pairing),
    reads = odd_vars(quos), rng = odd_rng(quos), exprs = args
  )
}

#' Which agents a rule step is evaluated over
#' @noRd
odd_rules_who <- function(step, ctx) {
  if (identical(step$scope, "population")) return("population")
  by <- odd_expr(step$by)
  if (!is.null(by)) return(paste0("partition ", by))
  if (!is.null(ctx$match)) return("matched")
  "each agent"
}

#' @noRd
describe_rules <- function(step, ctx) {
  list(
    type = "abm_rules", kind = "update",
    who = odd_rules_who(step, ctx),
    simultaneous = TRUE,
    writes = odd_targets(step$rules),
    reads = odd_vars(odd_quos(step$rules)),
    rng = odd_rng(odd_quos(step$rules)),
    exprs = odd_rule_exprs(step$rules)
  )
}

#' @noRd
describe_sequential <- function(step, ctx) {
  quos <- c(odd_quos(step$rules), list(step$order))
  list(
    type = "abm_sequential", kind = "update",
    who = if (is.null(ctx$match)) "each agent in turn" else "each matched agent in turn",
    simultaneous = FALSE,
    order = odd_expr(step$order),
    shuffled = is.null(step$order),
    writes = odd_targets(step$rules),
    reads = odd_vars(quos), rng = odd_rng(quos),
    exprs = c(odd_rule_exprs(step$rules),
              if (!is.null(step$order)) paste0(".order = ", odd_expr(step$order)))
  )
}

#' @noRd
describe_global <- function(step, ctx) {
  list(
    type = "abm_global", kind = "global",
    who = "the whole population",
    indexed_by = odd_expr(step$by),
    writes = odd_targets(step$rules),
    reads = odd_vars(odd_quos(step$rules)),
    rng = odd_rng(odd_quos(step$rules)),
    exprs = c(odd_rule_exprs(step$rules),
              if (!is.null(step$by)) paste0(".by = ", odd_expr(step$by)))
  )
}

#' @noRd
describe_neighbours <- function(step, ctx) {
  quos <- c(odd_quos(step$rules), list(step$within))
  where <- if (is.null(step$within)) {
    if (is.null(step$where)) "network neighbours"
    else paste0("the lattice cell to the ", step$where)
  } else {
    paste0("agents satisfying ", odd_expr(step$within))
  }
  list(
    type = "abm_neighbours", kind = "aggregate",
    who = "each agent", neighbourhood = where,
    writes = odd_targets(step$rules),
    reads = odd_vars(quos), rng = odd_rng(quos),
    exprs = c(odd_rule_exprs(step$rules),
              if (!is.null(step$within)) paste0("within = ", odd_expr(step$within)),
              if (!is.null(step$where)) sprintf('.where = "%s"', step$where))
  )
}

#' @noRd
describe_move <- function(step, ctx) {
  quos <- list(step$expr, step$direction)
  dest <- switch(
    step$mode,
    random_neighbour       = "a cell drawn from those adjacent",
    random_empty_neighbour = "an unoccupied cell drawn from those adjacent",
    stay                   = "the cell it is already on",
    direction              = paste0("one cell along ", odd_expr(step$direction)),
    uphill                 = paste0("the reachable cell maximising ", odd_expr(step$expr)),
    downhill               = paste0("the reachable cell minimising ", odd_expr(step$expr))
  )
  list(
    type = "abm_move", kind = "movement",
    who = step$who %||% "every group not wired to the lattice",
    along = step$along, mode = step$mode, destination = dest,
    range = step$range, axes_only = step$axes_only,
    avoid_occupied = step$avoid_occupied,
    random = step$mode %in% c("random_neighbour", "random_empty_neighbour"),
    writes = ".cell", reads = odd_vars(quos), rng = odd_rng(quos),
    exprs = c(sprintf('along = "%s"', step$along),
              if (!is.null(step$to)) sprintf('to = "%s"', step$to),
              if (!is.null(step$expr)) paste0("to = ", step$mode, "(", odd_expr(step$expr), ")"),
              if (!is.null(step$direction)) paste0("direction = ", odd_expr(step$direction)),
              if (step$range != 1L) paste0("range = ", step$range),
              if (isTRUE(step$axes_only)) "axes_only = TRUE",
              if (isTRUE(step$avoid_occupied)) "avoid_occupied = TRUE")
  )
}

#' @noRd
describe_draw <- function(step, ctx) {
  list(
    type = "abm_draw", kind = "edge value",
    who = "each edge of the network",
    each = step$each,
    writes = odd_targets(step$rules),
    reads = odd_vars(odd_quos(step$rules)),
    rng = odd_rng(odd_quos(step$rules)),
    exprs = c(odd_rule_exprs(step$rules), sprintf('.each = "%s"', step$each))
  )
}

#' @noRd
describe_tell <- function(step, ctx) {
  quos <- c(odd_quos(step$rules), list(step$to_quo, step$when, step$order))
  to <- if (identical(step$to, "neighbours")) "its network neighbours"
        else paste0("the agent named by ", odd_expr(step$to_quo))
  list(
    type = "abm_tell", kind = "message",
    who = "each sender", recipients = to,
    when = odd_expr(step$when), resolve = step$resolve,
    order = odd_expr(step$order),
    writes = odd_targets(step$rules),
    reads = odd_vars(quos), rng = odd_rng(quos),
    exprs = c(odd_rule_exprs(step$rules),
              if (identical(step$to, "neighbours")) 'to = "neighbours"'
              else paste0("to = ", odd_expr(step$to_quo)),
              if (!is.null(step$when)) paste0("when = ", odd_expr(step$when)),
              sprintf('.resolve = "%s"', step$resolve),
              if (!is.null(step$order)) paste0(".order = ", odd_expr(step$order)))
  )
}

#' @noRd
describe_birth <- function(step, ctx) {
  quos <- c(list(step$when, step$times), odd_quos(step$cost %||% list()),
            odd_quos(step$inherit %||% list()))
  list(
    type = "abm_birth", kind = "demography",
    who = if (is.null(step$when)) "the model" else "each agent satisfying a condition",
    mode = if (is.null(step$when)) "count" else "condition",
    n = step$n, when = odd_expr(step$when), times = odd_expr(step$times),
    attach_via = if (is.null(step$attach_via)) NULL else step$attach_via$from,
    links = step$links,
    writes = c(if (!is.null(step$cost)) odd_targets(step$cost),
               if (!is.null(step$inherit)) odd_targets(step$inherit)),
    reads = odd_vars(quos), rng = odd_rng(quos),
    exprs = c(if (!is.null(step$when)) paste0("when = ", odd_expr(step$when)),
              if (!is.null(step$n)) paste0("n = ", step$n),
              if (!is.null(step$times)) paste0("times = ", odd_expr(step$times)),
              if (!is.null(step$cost)) paste0("cost: ", odd_rule_exprs(step$cost)),
              if (!is.null(step$inherit)) paste0("inherit: ", odd_rule_exprs(step$inherit)),
              if (!is.null(step$attach_via)) sprintf('attach_via = network, from = "%s"',
                                                     step$attach_via$from),
              if (!is.null(step$links)) paste0("links = ", step$links))
  )
}

#' @noRd
describe_death <- function(step, ctx) {
  list(
    type = "abm_death", kind = "demography",
    who = "each agent satisfying a condition",
    when = odd_expr(step$when), prune_edges = step$prune_edges,
    writes = character(), reads = odd_vars(list(step$when)),
    rng = odd_rng(list(step$when)),
    exprs = c(paste0("when = ", odd_expr(step$when)),
              paste0("prune_edges = ", step$prune_edges))
  )
}

#' @noRd
describe_link <- function(step, ctx) {
  drop <- isTRUE(step$drop)
  list(
    type = if (drop) "abm_unlink" else "abm_link", kind = "topology",
    who = "each matched pair", drop = drop,
    when = odd_expr(step$when),
    writes = character(), reads = odd_vars(list(step$when)),
    rng = odd_rng(list(step$when)),
    exprs = if (is.null(step$when)) "when = (every matched pair)"
            else paste0("when = ", odd_expr(step$when))
  )
}

#' @noRd
describe_repeat <- function(step, inner) {
  list(
    type = "abm_repeat", kind = "control flow",
    who = "the whole population", max = step$max,
    until = odd_expr(step$until), steps = inner,
    writes = character(), reads = odd_vars(list(step$until)),
    rng = odd_rng(list(step$until)),
    exprs = c(paste0("max = ", step$max),
              if (!is.null(step$until)) paste0("until = ", odd_expr(step$until)))
  )
}

# Walking the block ------------------------------------------------------

#' Number the processes, `1`, `2`, and `3.1` inside a repeat block
#' @noRd
odd_number <- function(descs, prefix = "") {
  for (i in seq_along(descs)) {
    descs[[i]]$index <- paste0(prefix, i)
    if (!is.null(descs[[i]]$steps)) {
      descs[[i]]$steps <- odd_number(descs[[i]]$steps, paste0(prefix, i, "."))
    }
  }
  descs
}

#' Every process, nested blocks included, in one flat list
#' @noRd
odd_flatten <- function(descs) {
  out <- list()
  for (d in descs) {
    out <- c(out, list(d))
    if (!is.null(d$steps)) out <- c(out, odd_flatten(d$steps))
  }
  out
}

# The elements -----------------------------------------------------------

#' @noRd
odd_entities <- function(model, ticks) {
  types <- lapply(names(model$groups), function(nm) {
    g <- model$groups[[nm]]
    cols <- setdiff(names(g), c(".id", ".group"))
    list(name = nm, n = nrow(g), variables = cols,
         classes = vapply(cols, function(cc) class(g[[cc]])[[1]], character(1),
                          USE.NAMES = FALSE))
  })
  names(types) <- names(model$groups)
  list(
    agent_types = types,
    globals = names(model$globals),
    global_classes = vapply(model$globals, function(v) class(v)[[1]], character(1),
                            USE.NAMES = FALSE),
    space = odd_space(model),
    ticks = ticks
  )
}

#' What space, if any, the model represents
#' @noRd
odd_space <- function(model) {
  n_edges <- if (is.null(model$edges)) 0L else nrow(model$edges)
  lat <- model$lattice
  if (!is.null(lat)) {
    return(list(kind = "lattice", type = lat$type, dims = lat$dims,
                cells = lat$ncell, torus = lat$torus, diagonals = lat$diagonals,
                on = lat$on, edges = n_edges))
  }
  spec <- model$network_spec
  if (!is.null(spec)) {
    return(list(kind = "network", type = spec$type, degree = spec$degree,
                edges = n_edges))
  }
  list(kind = "none")
}

#' How each starting column was declared
#'
#' Three declarations and one fallback. ODD's initialisation element is about
#' what the model *says*, not about the numbers one draw happened to produce,
#' so a formula is reported as the formula rather than as its result. The
#' fallback covers the reserved columns a lattice injects (`.cell`, `.x`, `.y`),
#' which no `abm_agents()` spec declares.
#' @noRd
odd_start_value <- function(col, spec, group) {
  val <- if (is.null(spec)) NULL else spec$cols[[col]]
  if (is_formula1(val)) {
    return(list(mode = "formula", value = paste0("~", deparse1(rlang::f_rhs(val)))))
  }
  if (!is.null(val)) {
    return(list(mode = if (length(val) == 1L) "constant" else "values",
                value = odd_literal(val)))
  }
  x <- group[[col]]
  if (length(unique(x)) <= 1L) {
    list(mode = "constant", value = odd_literal(x[1]), derived = TRUE)
  } else {
    list(mode = "values", value = odd_literal(x), derived = TRUE)
  }
}

#' A literal starting value, truncated when it is a long vector
#' @noRd
odd_literal <- function(x, max = 8L) {
  if (is.list(x)) return(paste0("<", class(x)[[1]], ">"))
  if (length(x) > max) {
    return(paste0(deparse1(utils::head(x, max)), " ... (", length(x), " values)"))
  }
  deparse1(x)
}

#' @noRd
odd_initialisation <- function(model) {
  specs <- model$agent_specs %||% list()
  out <- lapply(names(model$groups), function(nm) {
    g <- model$groups[[nm]]
    spec <- specs[[nm]]
    cols <- setdiff(names(g), c(".id", ".group"))
    vals <- lapply(cols, odd_start_value, spec = spec, group = g)
    names(vals) <- cols
    list(name = nm, n = nrow(g),
         at = if (is.null(spec)) NULL else odd_expr(spec$at),
         columns = vals)
  })
  names(out) <- names(model$groups)
  list(groups = out,
       globals = lapply(model$globals, odd_literal))
}

#' Every symbol the model's rules read, sorted into where it comes from
#' @noRd
odd_sensing <- function(flat, ctx) {
  vars <- unique(unlist(lapply(flat, function(d) d$reads)))
  vars <- if (is.null(vars)) character() else sort(vars)
  # `ctx$columns` is the population as `abm_setup()` left it, and a great many
  # models declare only their trait columns there and build the rest with a
  # rule. Such a column is model state as much as a declared one is, so the
  # columns every step writes are counted too -- without this a model's own
  # bookkeeping is reported as "resolved outside the model", which is both
  # wrong and the opposite of reassuring.
  all_cols <- unique(c(unlist(ctx$columns),
                       unlist(lapply(flat, function(d) d$writes))))
  partner <- grep("^partner_", vars, value = TRUE)
  pair_view <- grep("^own_", vars, value = TRUE)
  rest <- setdiff(vars, c(partner, pair_view))
  own <- intersect(rest, all_cols)
  globals <- intersect(rest, ctx$globals)
  left <- setdiff(rest, c(own, globals))
  list(
    own_columns = own,
    globals = globals,
    partner = partner,
    reads_partner = length(partner) > 0L,
    pair_view = pair_view,
    reserved = grep("^\\.", left, value = TRUE),
    other = setdiff(left, grep("^\\.", left, value = TRUE))
  )
}

#' The steps through which one agent's state reaches another's
#' @noRd
odd_interaction <- function(flat, model) {
  pick <- function(types) Filter(function(d) d$type %in% types, flat)
  list(
    matching = pick("abm_match"),
    neighbourhood = pick("abm_neighbours"),
    messaging = pick("abm_tell"),
    topology = pick(c("abm_link", "abm_unlink", "abm_draw")),
    space = odd_space(model)
  )
}

#' Where the randomness enters
#'
#' Four mechanical sources, each read off a field rather than inferred: a
#' pairing mode that draws, a sequential step with no declared order, a move to
#' a cell drawn at random, and a call to one of the random-number functions in
#' any expression -- the go block's and `abm_agents()`'s alike.
#' @noRd
odd_stochasticity <- function(flat, model) {
  out <- list()
  add <- function(where, source, detail) {
    out[[length(out) + 1L]] <<- list(where = where, source = source,
                                     detail = detail)
  }
  for (d in flat) {
    at <- paste0("process ", d$index)
    if (identical(d$type, "abm_match") && isTRUE(d$random)) {
      add(at, "pairing", unname(odd_random_pairing[[d$pair]]))
    }
    if (identical(d$type, "abm_sequential") && isTRUE(d$shuffled)) {
      add(at, "ordering", "agents are processed in a fresh random order each step")
    }
    if (identical(d$type, "abm_move") && isTRUE(d$random)) {
      add(at, "movement", d$destination)
    }
    if (length(d$rng)) {
      add(at, "draw", paste0("calls ", paste0("`", d$rng, "()`", collapse = ", ")))
    }
  }
  for (nm in names(model$agent_specs %||% list())) {
    spec <- model$agent_specs[[nm]]
    quos <- c(lapply(Filter(is_formula1, spec$cols), rlang::as_quosure),
              list(spec$at))
    fns <- odd_rng(quos)
    if (length(fns)) {
      add(paste0("setup, group ", nm), "initialisation",
          paste0("calls ", paste0("`", fns, "()`", collapse = ", ")))
    }
  }
  # The topology is drawn too, and element 2 already says so. Leaving it out
  # here let a model whose whole subject is a random graph report that nothing
  # in it is random -- the one contradiction the two sections could produce.
  ntype <- model$network_spec$type
  if (!is.null(ntype) && ntype %in% names(odd_random_networks)) {
    add("setup, network", "topology",
        paste0("a `", ntype, "` network -- ",
               unname(odd_random_networks[[ntype]])))
  }
  out
}

# The generator ----------------------------------------------------------

#' Generate an ODD protocol skeleton
#'
#' The ODD protocol (Overview, Design concepts, Details -- Grimm et al. 2006;
#' 2010; 2020, *JASSS* 23(2):7) is the template a paper describes an ABM in.
#' `abm_odd()` fills in the half of it that the model objects already contain
#' and marks the other half for you.
#'
#' It can do that because a tidyABM model is data: [abm_setup()] returns the
#' population and [abm_go()] returns an ordered list of typed steps, so the
#' entities and their state variables (element 2), the schedule (element 3), the
#' initialisation (element 5) and the submodels' equations (element 7) can be
#' read off them rather than remembered. Three of element 4's design concepts --
#' interaction, sensing and stochasticity -- are mechanical too, and are
#' likewise derived.
#'
#' Everything else is left as `[author input required]`. Purpose, emergence,
#' adaptation, objectives, learning, prediction, collectives, observation and
#' the *justification* for each submodel are statements about what the modeller
#' meant, and no object in the package records intent. In particular this
#' function never writes that a concept is absent: "no adaptation" is a claim,
#' and it is yours to make.
#'
#' Two things follow from that and are worth knowing before you paste the
#' output into a paper. Expressions are printed verbatim, never restated in
#' English, because element 7 wants the equations. And nothing here describes
#' *behaviour*: ODD is the specification, not the results, so no derived text
#' says what a run does.
#'
#' @param model An `abm_model` from [abm_setup()].
#' @param go An `abm_go` sequence.
#' @param ticks Optional number of ticks, used for element 2's temporal scale.
#'   Pass the same number you pass [abm_run()].
#'
#' @return An `abm_odd` object: a named list with one entry per ODD element,
#'   each holding structured fields. `print()` renders it as markdown and
#'   [format()] returns that markdown as a character vector.
#' @seealso [abm_setup()] and [abm_go()], the two objects this reads.
#' @export
#' @examples
#' economy <- abm_setup(agents = abm_agents(n = 500, money = 100))
#' go <- abm_go(
#'   abm_match(pair = "random", role = list(giver = money > 0, receiver = TRUE)),
#'   abm_rules(money ~ if_else(.role == "giver", money - 1, money + 1))
#' )
#' abm_odd(economy, go, ticks = 1000)
abm_odd <- function(model, go, ticks = NULL) {
  if (!inherits(model, "abm_model")) {
    abm_abort("{.arg model} must come from {.fn abm_setup}.",
              class = "tidyABM_bad_model")
  }
  if (!inherits(go, "abm_go")) {
    abm_abort("{.arg go} must come from {.fn abm_go}.",
              class = "tidyABM_bad_go")
  }
  if (!is.null(ticks) && (!rlang::is_scalar_integerish(ticks) || ticks < 0)) {
    abm_abort("{.arg ticks} must be a single non-negative whole number.",
              class = "tidyABM_bad_ticks")
  }

  ctx <- new_odd_ctx(model)
  processes <- odd_number(describe_block(go$steps, ctx)$descs)
  flat <- odd_flatten(processes)

  structure(
    list(
      purpose = list(purpose = odd_hole, patterns = odd_hole),
      entities = odd_entities(model, ticks),
      schedule = list(ticks = ticks, processes = processes),
      design_concepts = list(
        basic_principles = odd_hole,
        emergence = odd_hole,
        adaptation = odd_hole,
        objectives = odd_hole,
        learning = odd_hole,
        prediction = odd_hole,
        sensing = odd_sensing(flat, ctx),
        interaction = odd_interaction(flat, model),
        stochasticity = odd_stochasticity(flat, model),
        collectives = odd_hole,
        observation = odd_hole
      ),
      initialisation = odd_initialisation(model),
      input_data = list(note = odd_hole),
      submodels = lapply(flat, function(d) {
        list(index = d$index, type = d$type, kind = d$kind,
             exprs = d$exprs, justification = odd_hole)
      })
    ),
    class = "abm_odd"
  )
}

# Rendering --------------------------------------------------------------

#' @noRd
odd_code <- function(x) paste0("`", x, "`")

#' @noRd
odd_title <- function(x) sub("^(.)", "\\U\\1", x, perl = TRUE)

#' @noRd
odd_list <- function(x) paste(odd_code(x), collapse = ", ")

#' One process, as a heading and a handful of derived facts
#' @noRd
odd_md_process <- function(d, depth = 0L) {
  bullets <- switch(
    d$type,
    abm_match = c(
      sprintf("mode: `%s`, %s, in groups of %d", d$pair,
              if (d$mutual) "mutual" else "directional", d$size),
      if (!is.null(d$by)) paste0("space or split: ", odd_code(d$by)),
      if (!is.null(d$roles)) paste0("roles: ", odd_list(d$roles)),
      if (!is.null(d$partition)) paste0("confined to partitions of ", odd_code(d$partition)),
      if (!is.null(d$from)) sprintf("partner taken from: `%s`", d$from)
    ),
    abm_repeat = c(
      paste0("repeated at most ", d$max, " times within the tick"),
      if (!is.null(d$until)) paste0("until ", odd_code(d$until))
      else "with no stopping condition"
    ),
    c(
      paste0("evaluated over: ", d$who),
      if (!is.null(d$neighbourhood)) paste0("neighbourhood: ", d$neighbourhood),
      if (!is.null(d$recipients)) paste0("written into: ", d$recipients),
      if (!is.null(d$destination)) paste0("destination: ", d$destination),
      if (!is.null(d$indexed_by)) paste0("one value per key of ", odd_code(d$indexed_by)),
      if (length(d$writes)) paste0("writes: ", odd_list(d$writes)),
      if (length(d$reads)) paste0("reads: ", odd_list(d$reads)),
      if (isTRUE(d$simultaneous))
        "all rules are evaluated against the state at the start of the step",
      if (identical(d$simultaneous, FALSE))
        paste0("one agent at a time, ",
               if (isTRUE(d$shuffled)) "in a fresh random order"
               else paste0("in ascending order of ", odd_code(d$order))),
      if (!is.null(d$when)) paste0("condition: ", odd_code(d$when)),
      if (!is.null(d$resolve)) sprintf("collisions resolved by `%s`", d$resolve)
    )
  )
  pad <- strrep("   ", depth)
  c(sprintf("%s%s. **%s** (`%s`)", pad, d$index, odd_title(d$kind), d$type),
    paste0(pad, "   - ", bullets),
    if (!is.null(d$steps)) unlist(lapply(d$steps, odd_md_process, depth = depth + 1L)))
}

#' @noRd
odd_md_entities <- function(e) {
  out <- c("## 2. Entities, state variables and scales", "",
           "### Entities and state variables", "")
  for (t in e$agent_types) {
    out <- c(out, sprintf("**%s** -- %d agent%s.", t$name, t$n,
                          if (t$n == 1L) "" else "s"), "")
    if (length(t$variables)) {
      out <- c(out, "| state variable | type |", "| --- | --- |",
               sprintf("| `%s` | %s |", t$variables, t$classes), "")
    } else {
      out <- c(out, "This entity has no state variables of its own.", "")
    }
  }
  out <- c(out, "### Global variables", "")
  out <- c(out, if (length(e$globals)) {
    sprintf("- `%s` (%s)", e$globals, e$global_classes)
  } else {
    "The model declares no global variables."
  }, "")

  s <- e$space
  out <- c(out, "### Spatial extent", "", switch(
    s$kind,
    none = "The model declares neither a network nor a lattice, so it represents no space.",
    lattice = sprintf(paste0("A `%s` lattice of %s = %d cells, wired to the **%s** ",
                             "group, %s, %s. The lattice is the model's network ",
                             "(%d edges)."),
                      s$type, paste(s$dims, collapse = " x "), s$cells, s$on,
                      if (isTRUE(s$torus)) "wrapping at the edges (a torus)" else "bounded",
                      if (identical(s$type, "line")) "each cell joined to its two neighbours"
                      else if (isTRUE(s$diagonals)) "Moore neighbourhood (diagonals included)"
                      else "von Neumann neighbourhood (no diagonals)",
                      s$edges),
    network = sprintf(paste0("No lattice. Agents are joined by a `%s` network%s, ",
                             "%d edges, built once at setup."),
                      s$type,
                      if (is.null(s$degree)) "" else paste0(" with degree ", s$degree),
                      s$edges)
  ), "", "### Temporal extent", "",
  if (is.null(e$ticks)) paste0("Number of ticks: ", odd_hole, ".")
  else sprintf("One simulation is %d ticks.", e$ticks),
  paste0("What one tick represents in the modelled system: ", odd_hole, "."), "")
  out
}

#' @noRd
odd_md_concepts <- function(dc) {
  hole <- function(title) c(paste0("### ", title), "", dc[[
    tolower(gsub("[^A-Za-z]+", "_", title))]], "")
  s <- dc$sensing
  sensing <- c(
    "### Sensing", "",
    "Derived from every expression in the go block.", "",
    if (length(s$own_columns)) paste0("- Own state variables: ", odd_list(s$own_columns)),
    if (length(s$globals)) paste0("- Global variables: ", odd_list(s$globals)),
    if (length(s$partner)) paste0("- The partner's state, via ", odd_list(s$partner)),
    if (length(s$pair_view)) paste0("- Its own state seen from a candidate's row, via ",
                                    odd_list(s$pair_view)),
    if (length(s$reserved)) paste0("- Reserved columns: ", odd_list(s$reserved)),
    if (length(s$other)) paste0("- Symbols resolved outside the model: ", odd_list(s$other)),
    "",
    if (!s$reads_partner)
      c(paste0("No rule reads a `partner_` column, so no agent's expression ",
               "depends on the state of the agent it is matched with."), "")
  )
  i <- dc$interaction
  interaction <- c(
    "### Interaction", "", if (length(i$matching)) {
      c("Direct interaction is mediated by matching:",
        sprintf("- process %s: `%s` pairing, %s, in groups of %d%s",
                vapply(i$matching, function(d) d$index, character(1)),
                vapply(i$matching, function(d) d$pair, character(1)),
                vapply(i$matching, function(d) if (d$mutual) "mutual" else "directional",
                       character(1)),
                vapply(i$matching, function(d) d$size, integer(1)),
                vapply(i$matching, function(d)
                  if (is.null(d$roles)) "" else paste0(", roles ", odd_list(d$roles)),
                  character(1))))
    },
    if (length(i$neighbourhood))
      c("", "Indirect interaction, through a neighbourhood aggregate:",
        sprintf("- process %s reads %s",
                vapply(i$neighbourhood, function(d) d$index, character(1)),
                vapply(i$neighbourhood, function(d) d$neighbourhood, character(1)))),
    if (length(i$messaging))
      c("", "One agent writing into another's row:",
        sprintf("- process %s writes into %s",
                vapply(i$messaging, function(d) d$index, character(1)),
                vapply(i$messaging, function(d) d$recipients, character(1)))),
    if (length(i$topology))
      c("", "The interaction structure itself changes during the run:",
        sprintf("- process %s (`%s`)",
                vapply(i$topology, function(d) d$index, character(1)),
                vapply(i$topology, function(d) d$type, character(1)))),
    "", paste0("What the interaction represents: ", odd_hole, "."), ""
  )
  st <- dc$stochasticity
  stoch <- c(
    "### Stochasticity", "",
    if (length(st)) {
      c("The model draws in the following places:",
        sprintf("- %s -- %s (%s)",
                vapply(st, function(z) z$where, character(1)),
                vapply(st, function(z) z$detail, character(1)),
                vapply(st, function(z) z$source, character(1))))
    } else {
      "No pairing, ordering, movement or expression in the model names a draw."
    },
    "",
    # The detection is syntactic: it reads the expressions the grammar holds and
    # nothing else. A draw inside a function a rule calls is invisible to it, so
    # the list is a floor rather than an inventory, and says so instead of
    # letting a reader take it for one.
    paste("Draws are found by reading the declared expressions, so a draw made",
          "inside a function called from a rule is not listed here. Check this",
          "list against any helper the model calls."),
    "", paste0("Why each is modelled as random: ", odd_hole, "."), ""
  )
  c("## 4. Design concepts", "",
    hole("Basic principles"), hole("Emergence"), hole("Adaptation"),
    hole("Objectives"), hole("Learning"), hole("Prediction"),
    sensing, interaction, stoch,
    hole("Collectives"), hole("Observation"))
}

#' @noRd
odd_md_initialisation <- function(init) {
  out <- c("## 5. Initialisation", "")
  for (g in init$groups) {
    out <- c(out, sprintf("**%s** -- %d agent%s at tick 0.", g$name, g$n,
                          if (g$n == 1L) "" else "s"), "")
    if (!is.null(g$at)) {
      out <- c(out, sprintf("Placed on the lattice by `%s`.", g$at), "")
    }
    for (nm in names(g$columns)) {
      v <- g$columns[[nm]]
      note <- if (isTRUE(v$derived)) " (set by the lattice, not declared)" else ""
      out <- c(out, switch(
        v$mode,
        constant = sprintf("- `%s`: every agent starts at `%s`%s", nm, v$value, note),
        values   = sprintf("- `%s`: `%s`%s", nm, v$value, note),
        formula  = sprintf("- `%s`: drawn once at setup from `%s`", nm, v$value)
      ))
    }
    out <- c(out, "")
  }
  if (length(init$globals)) {
    out <- c(out, "**Globals** at tick 0.", "",
             sprintf("- `%s`: `%s`", names(init$globals), unlist(init$globals)), "")
  }
  c(out, paste0("Whether these values are arbitrary or taken from data: ",
                odd_hole, "."), "")
}

#' @noRd
odd_md_submodels <- function(sm) {
  out <- c("## 7. Submodels", "",
           paste0("Each process of element 3, with the expressions it is ",
                  "declared by, verbatim."), "")
  for (s in sm) {
    out <- c(out, sprintf("### %s. `%s`", s$index, s$type), "",
             "```r", s$exprs, "```", "",
             paste0("Justification and parameter provenance: ", s$justification, "."),
             "")
  }
  out
}

#' @export
format.abm_odd <- function(x, ...) {
  n <- length(x$schedule$processes)
  c("# ODD protocol", "",
    paste0("Generated by `abm_odd()` from the model and its `abm_go()` block. ",
           "Everything marked ", odd_hole, " is a statement about intent, which ",
           "no object in the package records; fill those in yourself."), "",
    "## 1. Purpose and patterns", "",
    paste0("The purpose of this model: ", x$purpose$purpose, "."), "",
    paste0("The patterns that make it useful for that purpose: ",
           x$purpose$patterns, "."), "",
    odd_md_entities(x$entities),
    "## 3. Process overview and scheduling", "",
    sprintf("Each tick replays %d process%s, in this order.", n,
            if (n == 1L) "" else "es"), "",
    unlist(lapply(x$schedule$processes, odd_md_process)), "",
    odd_md_concepts(x$design_concepts),
    odd_md_initialisation(x$initialisation),
    "## 6. Input data", "",
    paste0("Whether the model uses external data as input: ", x$input_data$note,
           "."), "",
    odd_md_submodels(x$submodels))
}

#' @export
print.abm_odd <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}
