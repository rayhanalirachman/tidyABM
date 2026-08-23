# 45. Hotelling's Law (Hotelling 1929, Economic Journal 39: 41-57;
#     NetLogo Sample Models / Social Science / Hotelling's Law)
#
# Shops sit on a stretch of beach, buyers are spread evenly along it, and every
# buyer walks to the nearest shop. Each shop tries a step to one side and keeps
# it if more buyers came, otherwise walks back.
#
# Hotelling's answer is *minimum differentiation*: two shops end up back to
# back in the middle. Neither can do better anywhere else, and yet it is the
# arrangement that makes the buyers walk furthest -- the social optimum is a
# quarter and three quarters of the way along. It is the reason the two
# supermarkets are next door to each other.
#
# Grammar note. "The nearest shop" is not the same question as "the nearest
# agent", and abm_match(pair = "nearest") used to answer only the second: in a
# model with buyers and shops, a buyer's nearest agent is another buyer. That
# is what `among =` is for. Counting the buyers who chose you is then one
# abm_tell() with .resolve = "sum" -- every buyer sends its chosen shop a 1.

library(tidyABM)

hotelling <- function(n_shops = 2, n_buyers = 400, ticks = 300, step = 1,
                      seed = 1) {
  m <- abm_setup(
    agents = list(
      shops  = abm_agents(n = n_shops, x = ~runif(n, 10, 90),
                          x_old = 0, step = step, customers = 0, base = 0),
      buyers = abm_agents(n = n_buyers, x = ~seq(0, 100, length.out = n))
    ),
    seed = seed
  )

  # every buyer walks to the nearest shop and the shop counts who turned up
  count_buyers <- list(
    abm_rules(customers ~ 0, .scope = "population"),
    abm_match(pair = "nearest", by = x,
              eligible = .group == "buyers", among = .group == "shops"),
    abm_tell(customers ~ 1, to = .partner, when = .group == "buyers",
             .resolve = "sum")
  )

  # A shop compares a step against what it is getting *now*, not against the
  # best it ever got. The difference matters: once a rival moves in next door
  # your old takings are unreachable, and a shop that kept comparing against
  # them would never move again.
  go <- do.call(abm_go, c(
    count_buyers,
    list(
      abm_rules(base  ~ customers, .scope = "population"),
      abm_rules(x_old ~ x, .scope = "population"),
      abm_rules(x ~ pmin(100, pmax(0, x + sample(c(-1, 1), n(), TRUE) * step)),
                .scope = "population")
    ),
    count_buyers,
    list(abm_rules(x ~ if_else(customers > base, x, x_old),
                   .scope = "population"))
  ))

  r <- abm_run(m, go, ticks = ticks, seed = seed)
  f <- r[r$tick == ticks, ]
  shops <- sort(f$x[f$.group == "shops"])
  buyers <- f$x[f$.group == "buyers"]
  list(shops = shops,
       walk = mean(vapply(buyers, function(b) min(abs(b - shops)), numeric(1))))
}

# how far buyers would walk if the shops were placed to suit them
optimal_walk <- function(n_shops, n_buyers = 400) {
  best <- (seq_len(n_shops) - 0.5) / n_shops * 100
  buyers <- seq(0, 100, length.out = n_buyers)
  mean(vapply(buyers, function(b) min(abs(b - best)), numeric(1)))
}

if (sys.nframe() == 0L) {
  cat(sprintf("%7s  %-28s %10s %10s\n",
              "shops", "where they end up", "mean walk", "best walk"))
  for (k in c(2, 3, 5)) {
    out <- lapply(1:5, function(s) hotelling(n_shops = k, seed = s))
    pos <- rowMeans(vapply(out, function(o) o$shops, numeric(k)))
    cat(sprintf("%7d  %-28s %10.1f %10.1f\n", k,
                paste(sprintf("%.0f", pos), collapse = ", "),
                mean(vapply(out, function(o) o$walk, numeric(1))),
                optimal_walk(k)))
  }
  cat("\nTwo shops meet in the middle (Hotelling's minimum differentiation) and\nthe buyers walk about twice as far as they would if the shops were placed\nto suit them.\n")
}
