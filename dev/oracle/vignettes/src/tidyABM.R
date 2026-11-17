## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")


## ----setup--------------------------------------------------------------------
library(tidyABM)


## ----eval = FALSE-------------------------------------------------------------
# world  <- abm_setup(...)                                # 1
# go     <- abm_go(...)                                   # 2
# result <- abm_run(world, go, ticks = ..., seed = ...)   # 3


## -----------------------------------------------------------------------------
# 1. the world
economy <- abm_setup(agents = abm_agents(n = 500, money = 100))
economy


## -----------------------------------------------------------------------------
# 2. the tick
go <- abm_go(
  abm_match(pair = "random", role = list(giver = money > 0, receiver = TRUE)),
  abm_rules(money ~ if_else(.role == "giver", money - 1, money + 1))
)
go


## -----------------------------------------------------------------------------
# 3. the run
result <- abm_run(economy, go, ticks = 500, seed = 1)
result


## -----------------------------------------------------------------------------
start <- result$money[result$tick == 0]
end   <- result$money[result$tick == 500]

c(total_start = sum(start), total_end = sum(end))
round(summary(end))


## ----wealth-histogram, eval = requireNamespace("ggplot2", quietly = TRUE), fig.width = 6, fig.height = 3.2, fig.alt = "Histogram of agent wealth after 500 ticks: a broad, roughly symmetric spread centred near 100 dollars."----
library(ggplot2)

ggplot(data.frame(money = end), aes(money)) +
  geom_histogram(binwidth = 10, boundary = 0) +
  labs(x = "money at tick 500", y = "agents")


## -----------------------------------------------------------------------------
abm_agents(
  n = 200,
  wtp   = ~rnorm(n, 50, 10),
  offer = ~wtp * 0.8
)


## -----------------------------------------------------------------------------
market <- abm_setup(agents = list(
  buyers  = abm_agents(n = 5, wtp = ~rnorm(n, 50, 10), offer = ~wtp * 0.8),
  sellers = abm_agents(n = 5, wta = ~rnorm(n, 40, 10), ask   = ~wta * 1.2)
))
market


## -----------------------------------------------------------------------------
elfarol <- abm_setup(
  agents  = abm_agents(n = 100, threshold = ~runif(n, 40, 80)),
  globals = list(last_attendance = 60)
)


## -----------------------------------------------------------------------------
abm_network(type = "random", degree = 4)


## -----------------------------------------------------------------------------
pd_go <- abm_go(
  # phase 1: play
  abm_match(pair = "random"),
  abm_rules(payoff ~ case_when(
    strategy == "cooperate" & partner_strategy == "cooperate" ~ 3,
    strategy == "defect"    & partner_strategy == "defect"    ~ 1,
    strategy == "defect"    & partner_strategy == "cooperate" ~ 5,
    strategy == "cooperate" & partner_strategy == "defect"    ~ 0
  )),
  # phase 2: imitate whoever did better
  abm_match(pair = "random"),
  abm_rules(strategy ~ if_else(partner_payoff > payoff, partner_strategy, strategy))
)
pd_go


## ----error = TRUE-------------------------------------------------------------
try({
abm_go(
  abm_match(pair = "random"),
  abm_match(pair = "random"),
  abm_rules(x ~ 1)
)
})


## -----------------------------------------------------------------------------
farol_go <- abm_go(
  abm_rules(go_today ~ last_attendance < threshold),
  abm_global(last_attendance ~ sum(go_today))
)
abm_run(elfarol, farol_go, ticks = 5, seed = 1) |> abm_globals()


## -----------------------------------------------------------------------------
rumour <- abm_setup(
  agents = abm_agents(n = 100, state = ~c("spreader", rep("ignorant", n - 1)))
)

rumour_go <- abm_go(
  abm_match(pair = "random"),
  abm_rules(state ~ case_when(
    state == "ignorant" & partner_state == "spreader" ~ "spreader",
    state == "spreader" & partner_state == "spreader" ~ "stifler",
    state == "spreader" & partner_state == "stifler"  ~ "stifler",
    TRUE ~ state
  ))
)

r <- abm_run(rumour, rumour_go, ticks = 100, seed = 1)

table(r$state[r$tick == 100])


## -----------------------------------------------------------------------------
pgg <- abm_setup(agents = abm_agents(
  n = 100, contribution = ~sample(c(0, 1), n, replace = TRUE), payoff = 0
))

pgg_go <- abm_go(
  abm_match(pair = "random", size = 4),
  abm_rules(payoff ~ sum(contribution) * 2 / 4)
)

r <- abm_run(pgg, pgg_go, ticks = 10, seed = 1)

table(r$payoff[r$tick == 10])


## -----------------------------------------------------------------------------
swap <- abm_setup(agents = abm_agents(n = 3, a = 1, b = 2))
swap_go <- abm_go(abm_rules(a ~ b, b ~ a))

abm_run(swap, swap_go, ticks = 1)


## -----------------------------------------------------------------------------
pot <- abm_setup(agents = abm_agents(n = 20, got = 0), globals = list(pot = 10))

pot_go <- abm_go(abm_sequential(
  got ~ if_else(pot > 0, 1, 0),
  pot ~ if_else(pot > 0, pot - 1, pot)
))

seq_run <- abm_run(pot, pot_go, ticks = 1, seed = 1)

sum(seq_run$got[seq_run$tick == 1])  # exactly the 10 units that existed


## -----------------------------------------------------------------------------
queue <- abm_setup(agents = abm_agents(n = 20, place = ~sample(n), got = 0),
                   globals = list(pot = 10))

queue_go <- abm_go(abm_sequential(
  got ~ if_else(pot > 0, 1, 0),
  pot ~ if_else(pot > 0, pot - 1, pot),
  .order = place
))

q <- abm_run(queue, queue_go, ticks = 1, seed = 1)
sum(q$got[q$tick == 1 & q$place <= 10])   # the front of the queue, every time


## -----------------------------------------------------------------------------
grow <- abm_setup(agents = abm_agents(n = 5, x = 0))
grow_go <- abm_go(
  abm_repeat(abm_rules(x ~ x + 1), until = mean(x) >= 4, max = 100)
)

r <- abm_run(grow, grow_go, ticks = 1, seed = 1)
unique(r$x[r$tick == 1])


## -----------------------------------------------------------------------------
town <- abm_setup(
  agents  = abm_agents(n = 40, wealth = ~round(runif(n, 0, 100))),
  network = abm_network(type = "poisson", degree = 4),
  seed    = 1
)
town_go <- abm_go(
  abm_neighbours(richer ~ sum(wealth > own_wealth))
)

r <- abm_run(town, town_go, ticks = 1, seed = 1)
table(r$richer[r$tick == 1], useNA = "ifany")


## -----------------------------------------------------------------------------
crowd <- abm_setup(agents = abm_agents(n = 200, opinion = ~runif(n)),
                   globals = list(eps = 0.15), seed = 42)
crowd_go <- abm_go(
  abm_neighbours(opinion ~ mean(opinion),
                 within = abs(opinion - own_opinion) <= eps)
)

hk <- abm_run(crowd, crowd_go, ticks = 30)
round(sort(unique(round(hk$opinion[hk$tick == 30], 4))), 3)


## -----------------------------------------------------------------------------
pairs <- abm_setup(agents = abm_agents(n = 30, met = 0L),
                   network = abm_network(type = "poisson", degree = 4),
                   seed = 2)
pairs_go <- abm_go(
  abm_draw(happened ~ runif(n()) < 0.5),
  abm_neighbours(met ~ sum(happened))
)

r <- abm_run(pairs, pairs_go, ticks = 1, seed = 1)
# every meeting is counted by both of the agents in it, so the total is even
sum(r$met[r$tick == 1], na.rm = TRUE)


## -----------------------------------------------------------------------------
ethno <- abm_setup(agents = abm_agents(
  n = 100,
  strategy = ~sample(c("cooperate", "defect"), n, replace = TRUE),
  resource = 10
))

ethno_go <- abm_go(
  abm_match(pair = "random"),
  abm_rules(resource ~ case_when(
    strategy == "cooperate" & partner_strategy == "cooperate" ~ resource + 2,
    strategy == "defect"    & partner_strategy == "cooperate" ~ resource + 4,
    strategy == "cooperate" & partner_strategy == "defect"    ~ resource - 1,
    TRUE ~ resource
  ) - 1),
  abm_birth(when = resource > 20, cost = resource ~ resource / 2),
  abm_death(when = resource <= 0)
)

r <- abm_run(ethno, ethno_go, ticks = 20, seed = 1)

table(r$tick)[c(1, 11, 21)]


## -----------------------------------------------------------------------------
seeds <- abm_setup(agents = abm_agents(n = 10, age = 5), seed = 1)
seeds_go <- abm_go(
  abm_birth(when = age > 0, times = rpois(n(), 2), inherit = age ~ 0)
)

r <- abm_run(seeds, seeds_go, ticks = 1, seed = 1)
table(r$tick)


## -----------------------------------------------------------------------------
life <- abm_setup(
  agents  = abm_agents(alive = ~seq_len(n) %in% (c(5, 6, 7) + 5 * 12)),
  network = abm_network(type = "grid", dims = c(12, 12))
)

life_go <- abm_go(
  abm_neighbours(live_n ~ sum(alive)),
  abm_rules(alive ~ live_n == 3 | (alive & live_n == 2))
)

blinker <- abm_run(life, life_go, ticks = 2, seed = 1)
subset(blinker, tick == 1 & alive, select = c(.x, .y))


## -----------------------------------------------------------------------------
world <- abm_setup(
  agents = list(
    patches = abm_agents(grass = ~runif(n) < 0.5),
    sheep   = abm_agents(n = 20, energy = 10)
  ),
  network = abm_network(type = "grid", dims = c(10, 10), on = "patches"),
  seed = 1
)

graze_go <- abm_go(
  abm_move(along = "patches", to = "random_neighbour", who = "sheep"),
  # what is on the cell I am standing on
  abm_neighbours(grass_here ~ any(grass),
                 within = .group == "patches" & .id == own_.cell),
  abm_rules(energy ~ if_else(grass_here, energy + 1, energy - 1)),
  # eat it
  abm_tell(grass ~ FALSE, to = .cell, when = .group == "sheep" & grass_here)
)

grazed <- abm_run(world, graze_go, ticks = 10, seed = 1)
subset(grazed, tick == 10 & .group == "sheep", select = c(.id, .cell, energy))[1:5, ]


## -----------------------------------------------------------------------------
set.seed(1); before <- runif(1)
invisible(abm_run(economy, go, ticks = 5, seed = 99))
set.seed(1); after <- runif(1)
identical(before, after)


## -----------------------------------------------------------------------------
a <- abm_run(economy, go, ticks = 20, seed = 42)
b <- abm_run(economy, go, ticks = 20, seed = 42)
identical(as.data.frame(a), as.data.frame(b))


## -----------------------------------------------------------------------------
random_pop <- function() abm_setup(agents = abm_agents(n = 50, x = ~runif(n)))
noop_go <- abm_go(abm_rules(x ~ x))

set.seed(1); one <- abm_run(random_pop(), noop_go, ticks = 1, seed = 5)
set.seed(2); two <- abm_run(random_pop(), noop_go, ticks = 1, seed = 5)
identical(one$x, two$x)


## -----------------------------------------------------------------------------
experiment <- function() {
  m <- abm_setup(agents = abm_agents(n = 50, x = ~runif(n)), seed = 4)
  noop_go <- abm_go(abm_rules(x ~ x))
  abm_run(m, noop_go, ticks = 1, seed = 5)
}
set.seed(1); one <- experiment()
set.seed(2); two <- experiment()
identical(one$x, two$x)


## -----------------------------------------------------------------------------
sizes <- c(
  all      = nrow(abm_run(economy, go, ticks = 20, record = "all")),
  every_5  = nrow(abm_run(economy, go, ticks = 20, record = 5)),
  final    = nrow(abm_run(economy, go, ticks = 20, record = "final")),
  globals  = nrow(abm_run(economy, go, ticks = 20, record = "globals"))
)
sizes


## -----------------------------------------------------------------------------
economy2 <- abm_setup(agents  = abm_agents(n = 200, money = 100),
                      globals = list(step = 1))
go2 <- abm_go(
  abm_match(pair = "random", role = list(giver = money >= step, receiver = TRUE)),
  abm_rules(money ~ if_else(.role == "giver", money - step, money + step))
)

many <- abm_run(economy2, go2, ticks = 200, seed = 1, record = "final",
                params = list(step = c(1, 5, 10)))
many


## -----------------------------------------------------------------------------
swept <- abm_run(economy2, go2, ticks = 200, seed = 1,
                 params   = list(step = c(1, 5, 10)),
                 reps     = 5,
                 record   = "globals",
                 measures = list(spread = ~sd(money), broke = ~mean(money == 0)))

final <- subset(abm_measures(swept), tick == 200)
aggregate(cbind(spread, broke) ~ step, data = final, FUN = mean)

