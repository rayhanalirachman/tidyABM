## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")


## ----setup--------------------------------------------------------------------
library(tidyABM)


## -----------------------------------------------------------------------------
short_pop <- abm_setup(
  agents  = abm_agents(n = 100, threshold = ~runif(n, 40, 80)),
  globals = list(last_attendance = 60))

short_go <- abm_go(
  abm_rules(go_today ~ last_attendance < threshold),
  abm_global(last_attendance ~ sum(go_today)))

short <- abm_run(short_pop, short_go, ticks = 40, seed = 2)

tail(abm_globals(short)$last_attendance, 12)


## -----------------------------------------------------------------------------
MEMORY <- 5    # weeks of attendance each predictor looks at
N_STRAT <- 10  # candidate predictors per agent
CAPACITY <- 60

farol <- abm_setup(
  agents = abm_agents(
    n = 100,
    w = ~lapply(seq_len(n), function(i)
          matrix(runif(N_STRAT * (MEMORY + 1), -1, 1), N_STRAT, MEMORY + 1)),
    e = ~lapply(seq_len(n), function(i) numeric(N_STRAT)),
    p = ~lapply(seq_len(n), function(i) numeric(N_STRAT)),
    go_today = FALSE),
  globals = as.list(setNames(rep(CAPACITY, MEMORY), paste0("att", 1:MEMORY))),
  seed = 1)


## -----------------------------------------------------------------------------
go <- abm_go(
  abm_rules(p ~ lapply(w, function(W) as.vector(W %*% c(100, att1, att2, att3, att4, att5)))),
  abm_rules(go_today ~ mapply(function(pi, ei) pi[which.min(ei)], p, e) < CAPACITY),
  abm_global(att5 ~ att4, att4 ~ att3, att3 ~ att2, att2 ~ att1,
             att1 ~ sum(go_today)),
  abm_rules(e ~ mapply(function(ei, pi) 0.8 * ei + 0.2 * abs(pi - att1),
                       e, p, SIMPLIFY = FALSE))
)

r <- abm_run(farol, go, ticks = 300, seed = 2)

attendance <- abm_globals(r)$att1[-(1:101)]
c(mean = round(mean(attendance), 1), sd = round(sd(attendance), 1),
  min = min(attendance), max = max(attendance),
  distinct = length(unique(attendance)))


## -----------------------------------------------------------------------------
one_predictor <- abm_setup(
  agents = abm_agents(
    n = 100,
    w = ~lapply(seq_len(n), function(i) matrix(runif(MEMORY + 1, -1, 1), 1, MEMORY + 1)),
    e = ~lapply(seq_len(n), function(i) numeric(1)),
    p = ~lapply(seq_len(n), function(i) numeric(1)),
    go_today = FALSE),
  globals = as.list(setNames(rep(CAPACITY, MEMORY), paste0("att", 1:MEMORY))),
  seed = 1)

tail(abm_globals(abm_run(one_predictor, go, ticks = 300, seed = 2))$att1, 8)


## -----------------------------------------------------------------------------
COST <- 0.01; BENEFIT <- 0.03; BASE_PTR <- 0.12; DEATH <- 0.10; CAPACITY <- 800

random_traits <- list(
  tag      ~ sample(c("red", "blue"), n(), replace = TRUE),
  coop_in  ~ sample(c(TRUE, FALSE),   n(), replace = TRUE),
  coop_out ~ sample(c(TRUE, FALSE),   n(), replace = TRUE)
)

start_pop <- function(network = NULL) {
  abm_setup(
    agents = abm_agents(
      n = 400,
      tag      = ~sample(c("red", "blue"), n, replace = TRUE),
      coop_in  = ~sample(c(TRUE, FALSE), n, replace = TRUE),
      coop_out = ~sample(c(TRUE, FALSE), n, replace = TRUE),
      ptr      = BASE_PTR),
    network = network,
    seed    = 1)
}

label <- function(r) {
  dplyr::mutate(r, type = case_when(
    coop_in & !coop_out ~ "ethnocentric",
    coop_in &  coop_out ~ "altruist",
   !coop_in & !coop_out ~ "egoist",
    TRUE                ~ "traitor"))
}
shares <- function(r, t) {
  x <- label(r); x <- x$type[x$tick == t]
  round(sort(table(x) / length(x), decreasing = TRUE), 3)
}


## -----------------------------------------------------------------------------
well_mixed_go <- abm_go(
  abm_match(pair = "random"),
  abm_rules(give ~ if_else(partner_tag == tag, coop_in, coop_out)),
  abm_rules(ptr ~ BASE_PTR - COST * give + BENEFIT * partner_give),
  abm_birth(when = runif(n()) < ptr),
  abm_death(when = runif(n()) < DEATH + 0.25 * pmax(0, (n() - CAPACITY) / CAPACITY)),
  abm_birth(n = 8, inherit = random_traits)
)

mixed <- abm_run(start_pop(), well_mixed_go, ticks = 400, seed = 1)

shares(mixed, 400)


## -----------------------------------------------------------------------------
local_go <- abm_go(
  abm_match(pair = "network"),
  abm_rules(give ~ if_else(partner_tag == tag, coop_in, coop_out)),
  abm_rules(ptr ~ BASE_PTR - COST * give + BENEFIT * partner_give),
  abm_birth(when = runif(n()) < ptr, links = 4,
            attach_via = abm_match(pair = "network", from = "parent")),
  abm_death(when = runif(n()) < DEATH + 0.25 * pmax(0, (n() - CAPACITY) / CAPACITY)),
  abm_birth(n = 8, links = 4, inherit = random_traits,
            attach_via = abm_match(pair = "network"))
)

local <- abm_run(start_pop(abm_network(type = "random", degree = 4)), local_go,
                 ticks = 400, seed = 1)

shares(local, 400)


## -----------------------------------------------------------------------------
edges <- abm_edges(local)
final <- local[local$tick == 400, ]
tags  <- setNames(final$tag, final$.id)
mean(tags[as.character(edges$from)] == tags[as.character(edges$to)], na.rm = TRUE)

