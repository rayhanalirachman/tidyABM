# abm_odd(): the derived half of the ODD protocol
#
# These tests assert on the structure of the `abm_odd` object -- the fields each
# element holds -- and never on the rendered English. The prose is a rendering
# of those fields and is free to change; what must not change is that the right
# facts are read off the model.

# The step classes the grammar dispatches ---------------------------------

# Every `switch(class(step)[[1]], ...)` branch name in a function body.
odd_switch_branches <- function(f) {
  found <- character()
  walk <- function(e) {
    if (!is.call(e)) return(invisible(NULL))
    if (identical(e[[1]], quote(switch))) {
      nms <- names(as.list(e))
      found <<- c(found, nms[nzchar(nms)])
    }
    for (i in seq_along(e)) {
      part <- tryCatch(e[[i]], error = function(...) NULL)
      if (!is.null(part)) walk(part)
    }
  }
  walk(body(f))
  sort(unique(found))
}

test_that("every step class the scheduler dispatches can also be described", {
  withr::local_seed(4001)
  # The two switches are the same closed set: `run_step()` runs a step and
  # `describe_step()` describes it, so a class added to one and not the other is
  # a step the ODD generator would abort on. Both lists are read out of the
  # installed sources rather than restated here, so this cannot pass by being
  # updated alongside the code it checks.
  run <- odd_switch_branches(tidyABM:::run_step)
  odd <- odd_switch_branches(tidyABM:::describe_step)

  expect_equal(setdiff(run, odd), character())   # dispatched but not described
  expect_equal(setdiff(odd, run), character())   # described but never run
  # and the set is the grammar as documented, so a class dropped from *both*
  # switches is caught too
  expect_equal(run, sort(c("abm_match", "abm_rules", "abm_sequential",
                           "abm_global", "abm_neighbours", "abm_move",
                           "abm_draw", "abm_tell", "abm_birth", "abm_death",
                           "abm_link", "abm_unlink", "abm_pairs", "abm_repeat")))
})

test_that("one step of every class is described without error", {
  withr::local_seed(4002)
  # the switch names agreeing is necessary but not sufficient: each branch must
  # actually cope with a real step object of its class
  m <- abm_setup(agents = abm_agents(n = 6, x = 1, h = 0),
                 network = abm_network(type = "grid", dims = c(3, 2)))
  ctx <- tidyABM:::new_odd_ctx(m)
  steps <- list(
    abm_match      = abm_match(pair = "random"),
    abm_rules      = abm_rules(x ~ x + 1),
    abm_sequential = abm_sequential(x ~ x + 1),
    abm_global     = abm_global(x ~ sum(x)),
    abm_neighbours = abm_neighbours(x ~ mean(x)),
    abm_move       = abm_move(along = "agents", to = "random_neighbour"),
    abm_draw       = abm_draw(w ~ runif(1)),
    abm_tell       = abm_tell(h ~ h + 1, to = "neighbours"),
    abm_birth      = abm_birth(n = 1),
    abm_death      = abm_death(when = x > 100),
    abm_link       = abm_link(),
    abm_unlink     = abm_unlink(),
    abm_pairs      = abm_pairs(via = "ties", w ~ w + 1),
    abm_repeat     = abm_repeat(abm_rules(x ~ x + 1), max = 2)
  )
  # the list below must cover the dispatched set, or the loop proves nothing
  expect_setequal(names(steps), odd_switch_branches(tidyABM:::run_step))

  for (nm in names(steps)) {
    d <- tidyABM:::describe_step(steps[[nm]], ctx)$desc
    expect_equal(d$type, nm)                      # `abm_unlink` is not `abm_link`
    expect_true(is.character(d$kind) && nzchar(d$kind))
    expect_true(is.character(d$exprs) && length(d$exprs) > 0L)
  }
})

test_that("an unknown step class is refused rather than silently skipped", {
  withr::local_seed(4003)
  m <- abm_setup(agents = abm_agents(n = 3, x = 1))
  bogus <- structure(list(), class = c("abm_teleport", "abm_step"))
  expect_error(tidyABM:::describe_step(bogus, tidyABM:::new_odd_ctx(m)),
               class = "tidyABM_unknown_step")
})

# Simple economy ---------------------------------------------------------

economy_model <- function() abm_setup(agents = abm_agents(n = 500, money = 100))
economy_go <- function() {
  abm_go(
    abm_match(pair = "random", role = list(giver = money > 0, receiver = TRUE)),
    abm_rules(money ~ if_else(.role == "giver", money - 1, money + 1))
  )
}

test_that("element 2 reads the one entity type and its one state variable", {
  withr::local_seed(4004)
  o <- abm_odd(economy_model(), economy_go(), ticks = 1000)
  types <- o$entities$agent_types
  expect_named(types, "agents")
  expect_equal(types$agents$name, "agents")
  expect_equal(types$agents$n, 500L)
  expect_equal(types$agents$variables, "money")
  expect_equal(types$agents$classes, "numeric")
  expect_null(o$entities$globals)
  expect_equal(o$entities$space$kind, "none")
  expect_equal(o$entities$ticks, 1000)
})

test_that("element 3 is the two processes, match then update, in order", {
  withr::local_seed(4005)
  o <- abm_odd(economy_model(), economy_go(), ticks = 1000)
  p <- o$schedule$processes
  expect_equal(length(p), 2L)
  expect_equal(vapply(p, function(d) d$type, character(1)),
               c("abm_match", "abm_rules"))
  expect_equal(vapply(p, function(d) d$index, character(1)), c("1", "2"))
  expect_equal(p[[1]]$pair, "random")
  expect_equal(p[[1]]$size, 2L)
  expect_true(p[[1]]$mutual)
  expect_equal(p[[1]]$roles, c("giver", "receiver"))
  # the pairing stands for the rest of the tick, so the update after it is
  # about pairs and not about lone agents
  expect_equal(p[[2]]$who, "matched")
  expect_true(p[[2]]$simultaneous)
  expect_equal(p[[2]]$writes, "money")
  expect_true(all(c("money", ".role") %in% p[[2]]$reads))
})

test_that("sensing reports no partner read when no rule names one", {
  withr::local_seed(4006)
  s <- abm_odd(economy_model(), economy_go())$design_concepts$sensing
  expect_false(s$reads_partner)
  expect_equal(s$partner, character())
  expect_equal(s$own_columns, "money")
  expect_equal(s$reserved, ".role")   # `.role` is not a state variable
  expect_equal(s$other, character())
})

test_that("stochasticity finds the pairing and nothing else", {
  withr::local_seed(4007)
  st <- abm_odd(economy_model(), economy_go())$design_concepts$stochasticity
  expect_equal(length(st), 1L)
  expect_equal(st[[1]]$source, "pairing")
  expect_equal(st[[1]]$where, "process 1")
})

# Market: two entity types, four processes, a population step ------------

market_model <- function() {
  abm_setup(agents = list(
    seller = abm_agents(n = 1, limit_price = 20, expected_price = 20,
                        transacted = FALSE, deal_price = NA_real_),
    buyer  = abm_agents(n = 2, limit_price = c(40, 35), expected_price = c(30, 25),
                        transacted = FALSE, deal_price = NA_real_)
  ))
}

market_go <- function() {
  abm_go(
    abm_rules(transacted ~ FALSE, deal_price ~ NA_real_),

    abm_match(pair = "opposite_group", by = .group),

    abm_rules(
      transacted ~ case_when(
        .group == "seller" & partner_expected_price >= expected_price ~ TRUE,
        .group == "buyer"  & expected_price >= partner_expected_price ~ TRUE,
        TRUE ~ FALSE
      ),
      deal_price ~ if_else(
        (.group == "seller" & partner_expected_price >= expected_price) |
        (.group == "buyer"  & expected_price >= partner_expected_price),
        (expected_price + partner_expected_price) / 2, NA_real_
      )
    ),

    abm_rules(
      expected_price ~ case_when(
        .group == "seller" &  transacted ~ expected_price + 2,
        .group == "seller" & !transacted ~ pmax(expected_price - 1, limit_price),
        .group == "buyer"  &  transacted ~ expected_price - 1,
        .group == "buyer"  & !transacted ~ pmin(expected_price + 1, limit_price),
        TRUE ~ expected_price
      ),
      .scope = "population"
    )
  )
}

test_that("element 2 keeps the two entity types apart, with their own sizes", {
  withr::local_seed(4008)
  o <- abm_odd(market_model(), market_go(), ticks = 50)
  types <- o$entities$agent_types
  expect_named(types, c("seller", "buyer"))
  expect_equal(vapply(types, function(t) t$n, integer(1)),
               c(seller = 1L, buyer = 2L))
  expect_setequal(types$seller$variables,
                  c("limit_price", "expected_price", "transacted", "deal_price"))
  # the two groups declare the same columns, which is what makes them one
  # population the rules can range over
  expect_setequal(types$seller$variables, types$buyer$variables)
})

test_that("element 3 tells the three scopes of a rule step apart", {
  withr::local_seed(4009)
  p <- abm_odd(market_model(), market_go(), ticks = 50)$schedule$processes
  expect_equal(length(p), 4L)
  expect_equal(vapply(p, function(d) d$type, character(1)),
               c("abm_rules", "abm_match", "abm_rules", "abm_rules"))
  # 1 runs before any pairing stands, so it is over agents...
  expect_equal(p[[1]]$who, "each agent")
  # ...3 runs after `abm_match()`, so the same default scope now means pairs...
  expect_equal(p[[3]]$who, "matched")
  # ...and 4 opts out of the pairing entirely
  expect_equal(p[[4]]$who, "population")
  expect_equal(p[[2]]$pair, "opposite_group")
  expect_equal(p[[2]]$by, ".group")
  expect_null(p[[2]]$roles)
  expect_equal(p[[4]]$writes, "expected_price")
})

test_that("sensing reports the partner column the bargaining reads", {
  withr::local_seed(4010)
  s <- abm_odd(market_model(), market_go())$design_concepts$sensing
  expect_true(s$reads_partner)
  expect_equal(s$partner, "partner_expected_price")
  # the partner column is not mistaken for a state variable of the agent's own
  expect_false("partner_expected_price" %in% s$own_columns)
  expect_true(all(c("expected_price", "limit_price") %in% s$own_columns))
  expect_equal(s$reserved, ".group")
})

# A lattice model with movement ------------------------------------------

lattice_model <- function() {
  abm_setup(
    agents = list(
      patches = abm_agents(height = ~ -((.x - 5)^2 + (.y - 5)^2)),
      walker  = abm_agents(n = 30, at = ~sample(64, 30, TRUE), energy = 10)
    ),
    network = abm_network(type = "grid", dims = c(8, 8), on = "patches",
                          torus = FALSE)
  )
}

lattice_go <- function() {
  abm_go(abm_move(along = "patches", who = "walker", to = uphill(height)),
         abm_rules(energy ~ energy - 1))
}

test_that("element 2 reads the lattice off the model", {
  withr::local_seed(4011)
  o <- abm_odd(lattice_model(), lattice_go(), ticks = 10)
  s <- o$entities$space
  expect_equal(s$kind, "lattice")
  expect_equal(s$type, "grid")
  expect_equal(s$dims, c(8L, 8L))
  expect_equal(s$cells, 64L)
  expect_false(s$torus)
  expect_true(s$diagonals)
  expect_equal(s$on, "patches")
  expect_gt(s$edges, 0L)

  types <- o$entities$agent_types
  expect_named(types, c("patches", "walker"))
  expect_equal(types$patches$n, 64L)      # the cell count, inherited from dims
  expect_equal(types$walker$n, 30L)
  # the mover carries `.cell`; the group the lattice is wired to does not
  expect_true(".cell" %in% types$walker$variables)
  expect_false(".cell" %in% types$patches$variables)
  expect_true(all(c(".x", ".y") %in% types$patches$variables))
})

test_that("a move is described by who moves, along what, and where to", {
  withr::local_seed(4012)
  p <- abm_odd(lattice_model(), lattice_go(), ticks = 10)$schedule$processes
  mv <- p[[1]]
  expect_equal(mv$type, "abm_move")
  expect_equal(mv$kind, "movement")
  expect_equal(mv$who, "walker")          # the group name, not an expression
  expect_equal(mv$along, "patches")
  expect_equal(mv$mode, "uphill")
  expect_equal(mv$writes, ".cell")
  expect_equal(mv$reads, "height")        # the cell field it climbs
  expect_false(mv$random)                 # `uphill` maximises, it does not draw
  expect_equal(mv$range, 1L)
  expect_false(mv$axes_only)

  # a drawn destination is a source of stochasticity, a climbed one is not
  rnd <- abm_odd(lattice_model(),
                 abm_go(abm_move(along = "patches", who = "walker",
                                 to = "random_neighbour")))
  expect_true(rnd$schedule$processes[[1]]$random)
  sources <- vapply(rnd$design_concepts$stochasticity,
                    function(z) z$source, character(1))
  expect_true("movement" %in% sources)
})

test_that("a draw in abm_agents() is stochasticity attributed to setup", {
  withr::local_seed(4013)
  o <- abm_odd(lattice_model(), lattice_go(), ticks = 10)
  st <- o$design_concepts$stochasticity
  # nothing in the go block draws, so the only source is the placement formula
  expect_equal(length(st), 1L)
  expect_equal(st[[1]]$source, "initialisation")
  expect_equal(st[[1]]$where, "setup, group walker")
  # and element 5 records the placement as the formula, not as one draw's result
  expect_equal(o$initialisation$groups$walker$at, "sample(64, 30, TRUE)")
  expect_null(o$initialisation$groups$patches$at)
})

# abm_repeat: the recursion path -----------------------------------------

test_that("a repeat block is described, numbered and flattened recursively", {
  withr::local_seed(4014)
  m <- abm_setup(agents = abm_agents(n = 10, x = 0, moved = FALSE),
                 globals = list(k = 0))
  go <- abm_go(
    abm_rules(moved ~ FALSE),
    abm_repeat(abm_rules(x ~ x + 1), abm_global(k ~ sum(x)),
               until = all(x >= 3), max = 5),
    abm_rules(moved ~ TRUE)
  )
  o <- abm_odd(m, go, ticks = 4)
  p <- o$schedule$processes
  expect_equal(length(p), 3L)             # the block is one process at the top
  rep <- p[[2]]
  expect_equal(rep$type, "abm_repeat")
  expect_equal(rep$kind, "control flow")
  expect_equal(rep$max, 5L)
  expect_equal(rep$until, "all(x >= 3)")
  expect_equal(length(rep$steps), 2L)
  expect_equal(vapply(rep$steps, function(d) d$type, character(1)),
               c("abm_rules", "abm_global"))
  # nested processes are numbered under their block
  expect_equal(vapply(rep$steps, function(d) d$index, character(1)),
               c("2.1", "2.2"))
  # ...and element 7 lists the block and its contents, in order
  expect_equal(vapply(o$submodels, function(s) s$index, character(1)),
               c("1", "2", "2.1", "2.2", "3"))
  expect_equal(vapply(o$submodels, function(s) s$type, character(1)),
               c("abm_rules", "abm_repeat", "abm_rules", "abm_global", "abm_rules"))
  # an expression inside the block is still read for element 4
  expect_true("x" %in% o$design_concepts$sensing$own_columns)
})

test_that("a draw inside a repeat block is attributed to the nested process", {
  withr::local_seed(4015)
  m <- abm_setup(agents = abm_agents(n = 8, x = 0))
  o <- abm_odd(m, abm_go(abm_rules(x ~ x),
                         abm_repeat(abm_rules(x ~ x + runif(1)), max = 3)))
  st <- o$design_concepts$stochasticity
  expect_equal(length(st), 1L)
  expect_equal(st[[1]]$where, "process 2.1")
  expect_equal(st[[1]]$source, "draw")
})

# Element 5: how a column was declared -----------------------------------

test_that("initialisation keeps a constant, a vector and a formula apart", {
  withr::local_seed(4016)
  m <- abm_setup(
    agents = abm_agents(n = 3, money = 100, kind = c("a", "b", "c"),
                        w = ~runif(n)),
    globals = list(rate = 0.5))
  cols <- abm_odd(m, abm_go(abm_rules(money ~ money * rate)))$initialisation$groups$agents$columns
  expect_named(cols, c("money", "kind", "w"))
  # one value repeated for everyone
  expect_equal(cols$money$mode, "constant")
  expect_equal(cols$money$value, "100")
  # a literal vector, one value per agent
  expect_equal(cols$kind$mode, "values")
  expect_equal(cols$kind$value, deparse1(c("a", "b", "c")))
  # a formula: the declaration, not the numbers this draw happened to give
  expect_equal(cols$w$mode, "formula")
  expect_equal(cols$w$value, "~runif(n)")
  expect_null(cols$money$derived)
  expect_null(cols$w$derived)
})

test_that("columns the lattice injects are marked as derived, not declared", {
  withr::local_seed(4017)
  o <- abm_odd(lattice_model(), lattice_go())
  patches <- o$initialisation$groups$patches$columns
  expect_true(isTRUE(patches$.x$derived))
  expect_true(isTRUE(patches$.y$derived))
  expect_null(patches$height$derived)     # this one `abm_agents()` did declare
  expect_equal(patches$height$mode, "formula")
  # a long derived vector is truncated rather than printed in full
  expect_match(patches$.x$value, "\\(64 values\\)")
})

test_that("globals are recorded at their starting values", {
  withr::local_seed(4018)
  m <- abm_setup(agents = abm_agents(n = 4, x = 1),
                 globals = list(rate = 0.5, label = "base"))
  o <- abm_odd(m, abm_go(abm_rules(x ~ x * rate)))
  expect_equal(o$initialisation$globals, list(rate = "0.5", label = "\"base\""))
  expect_equal(o$entities$globals, c("rate", "label"))
  expect_equal(o$design_concepts$sensing$globals, "rate")
})

# Holes and arguments -----------------------------------------------------

test_that("the interpretive elements are left as marked holes", {
  withr::local_seed(4019)
  o <- abm_odd(economy_model(), economy_go(), ticks = 100)
  hole <- tidyABM:::odd_hole
  # nothing here guesses at intent, and nothing asserts a concept is *absent*
  for (nm in c("basic_principles", "emergence", "adaptation", "objectives",
               "learning", "prediction", "collectives", "observation")) {
    expect_equal(o$design_concepts[[nm]], hole)
  }
  expect_equal(o$purpose$purpose, hole)
  expect_equal(o$purpose$patterns, hole)
  expect_equal(o$input_data$note, hole)
  expect_true(all(vapply(o$submodels, function(s) s$justification,
                         character(1)) == hole))
})

test_that("ticks is optional and carried through when given", {
  withr::local_seed(4020)
  no_ticks <- abm_odd(economy_model(), economy_go())
  expect_null(no_ticks$entities$ticks)
  expect_null(no_ticks$schedule$ticks)
  expect_equal(abm_odd(economy_model(), economy_go(), ticks = 7)$schedule$ticks, 7)
})

test_that("arguments are checked the way abm_run() checks them", {
  withr::local_seed(4021)
  m <- economy_model()
  go <- economy_go()
  expect_error(abm_odd("nope", go), class = "tidyABM_bad_model")
  expect_error(abm_odd(m, "nope"), class = "tidyABM_bad_go")
  expect_error(abm_odd(m, go, ticks = -1), class = "tidyABM_bad_ticks")
  expect_error(abm_odd(m, go, ticks = c(1, 2)), class = "tidyABM_bad_ticks")
  expect_error(abm_odd(m, abm_rules(money ~ money)), class = "tidyABM_bad_go")
})

# Rendering ---------------------------------------------------------------

test_that("format() renders every element, for every model here", {
  withr::local_seed(4022)
  models <- list(
    list(economy_model(), economy_go(), 1000),
    list(market_model(), market_go(), 50),
    list(lattice_model(), lattice_go(), 10)
  )
  for (spec in models) {
    o <- abm_odd(spec[[1]], spec[[2]], ticks = spec[[3]])
    md <- format(o)
    expect_true(is.character(md))
    expect_false(anyNA(md))
    # the seven element headings, in order, and nothing dropped
    headings <- grep("^## ", md, value = TRUE)
    expect_equal(length(headings), 7L)
    expect_equal(substr(headings, 1, 5),
                 paste0("## ", 1:7, "."))
    expect_equal(md[[1]], "# ODD protocol")
    expect_output(returned <- print(o))
    expect_identical(returned, o)  # print returns its input invisibly
  }
})

test_that("element 7 prints the declaring expressions verbatim", {
  withr::local_seed(4023)
  o <- abm_odd(economy_model(), economy_go(), ticks = 10)
  sm <- o$submodels
  # an equation restated in prose is a different claim about the model, so the
  # deparsed expression is what element 7 carries
  expect_equal(sm[[2]]$exprs,
               "money ~ if_else(.role == \"giver\", money - 1, money + 1)")
  expect_true('pair = "random"' %in% sm[[1]]$exprs)
  expect_true(any(grepl("^role = list\\(giver = money > 0", sm[[1]]$exprs)))
})

# Regressions found by running the generator over the whole model corpus ----

test_that("a drawn network topology is a source of stochasticity", {
  withr::local_seed(4024)
  # Element 2 reports the network type, so leaving it out of element 4 let a
  # model whose entire subject is a random graph state that nothing in it is
  # random -- the two elements contradicting each other in one document.
  for (type in c("random", "poisson", "scale_free")) {
    m <- abm_setup(agents = abm_agents(n = 40, state = 0),
                   network = abm_network(type = type, degree = 2))
    o <- abm_odd(m, abm_go(abm_neighbours(k ~ sum(state))), ticks = 5)
    st <- o$design_concepts$stochasticity
    expect_true(any(vapply(st, function(z) identical(z$source, "topology"),
                           logical(1))),
                info = type)
  }
  # a network fixed by its arguments is not a draw
  m <- abm_setup(agents = abm_agents(n = 40, state = 0),
                 network = abm_network(type = "complete"))
  o <- abm_odd(m, abm_go(abm_neighbours(k ~ sum(state))), ticks = 5)
  expect_false(any(vapply(o$design_concepts$stochasticity,
                          function(z) identical(z$source, "topology"),
                          logical(1))))
})

test_that("a column a rule creates is the model's own state, not an outside symbol", {
  withr::local_seed(4025)
  # Declaring only the trait columns in `abm_agents()` and building the rest
  # with a rule is a common idiom; the population `abm_setup()` returns does
  # not have those columns yet, so reading it alone reports the model's own
  # bookkeeping as external.
  m <- abm_setup(agents = abm_agents(n = 30, trait = 1), globals = list(rate = 2))
  go <- abm_go(
    abm_rules(fitness ~ trait * rate),
    abm_rules(trait ~ if_else(fitness > 1, trait + 1, trait))
  )
  sens <- abm_odd(m, go, ticks = 5)$design_concepts$sensing
  expect_true("fitness" %in% sens$own_columns)
  expect_true("trait" %in% sens$own_columns)
  expect_equal(sens$globals, "rate")
  expect_false("fitness" %in% sens$other)
  expect_length(sens$other, 0L)
})

test_that("stochasticity spots every generator in stats, not a favoured few", {
  withr::local_seed(4026)
  # A missed draw is the one failure mode that makes the document assert
  # something false, so the list errs towards generators no model here uses.
  for (fn in c("rgamma", "rbeta", "rweibull", "rnbinom")) {
    call <- str2lang(paste0("x + ", fn, "(n(), 1, 1)"))
    go <- abm_go(abm_rules(rlang::new_formula(quote(x), call)))
    m <- abm_setup(agents = abm_agents(n = 10, x = 1))
    st <- abm_odd(m, go, ticks = 2)$design_concepts$stochasticity
    expect_true(any(grepl(fn, vapply(st, function(z) z$detail, character(1)))),
                info = fn)
  }
})
