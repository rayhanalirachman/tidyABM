# fixed cost of one engine step, by step type, at tiny n (so vector work ~ 0)
suppressMessages(pkgload::load_all("tidyABM", quiet = TRUE))
per <- function(go, n = 10, ticks = 3000, groups = 1) {
  ag <- if (groups == 1) abm_agents(n = n, x = ~runif(n), y = 0)
        else list(a = abm_agents(n = n, x = ~runif(n), y = 0), b = abm_agents(n = n, x = ~runif(n), y = 0))
  w <- abm_setup(agents = ag, network = abm_network("grid", dims = c(n %/% 5, 5), on = if (groups == 1) NULL else "a"),
                 globals = list(g = 0), seed = 1)
  t <- min(replicate(3, { p <- proc.time(); abm_run(w, go, ticks = ticks, seed = 1, record = "globals", progress = FALSE); sum((proc.time() - p)[1:2]) }))
  1e6 * t / ticks
}
base <- per(abm_go(abm_global(g ~ g + 1)))
r1 <- per(abm_go(abm_global(g ~ g + 1), abm_rules(y ~ x + 1)))
r5 <- per(abm_go(abm_global(g ~ g + 1), abm_rules(y ~ x + 1), abm_rules(y ~ x + 1), abm_rules(y ~ x + 1), abm_rules(y ~ x + 1), abm_rules(y ~ x + 1)))
nb <- per(abm_go(abm_global(g ~ g + 1), abm_neighbours(y ~ mean(x))))
r2g <- per(abm_go(abm_global(g ~ g + 1), abm_rules(y ~ x + 1)), groups = 2)
big <- per(abm_go(abm_global(g ~ g + 1), abm_rules(y ~ x + 1)), n = 100000, ticks = 300)
x <- runif(10); q <- rlang::quo(x + 1); d <- tibble::tibble(x = x)
ev <- system.time(for (i in 1:20000) rlang::eval_tidy(q, d))[[1]] / 20000 * 1e6
x <- runif(1e5); vec <- system.time(for (i in 1:2000) y <- x + 1)[[1]] / 2000 * 1e6
cat(sprintf("us per tick: 1 global %.0f | +1 rules %.0f | +5 rules %.0f (%.0f per rules step) | +1 neighbours %.0f | +1 rules, 2 groups %.0f\n", base, r1, r5, (r5 - base) / 5, nb, r2g))
cat(sprintf("rules step at n=1e5: %.0f us added; bare x+1 at 1e5: %.0f us; bare eval_tidy at n=10: %.1f us\n", big - base, vec, ev))
