# 47. Fixed response thresholds and the division of labour
#     (Bonabeau, Theraulaz & Deneubourg 1996, Proc. R. Soc. B 263: 1565-1569;
#      Theraulaz, Bonabeau & Deneubourg 1998, SFI 98-01-006)
#
# Workers are idle or engaged in one of K tasks. Each task has a stimulus that
# rises on its own and falls with the number of workers doing it; a worker
# takes on task j with probability s_j^2 / (s_j^2 + theta_ij^2), and abandons
# whatever it is doing with probability p. Nobody is told what to do.
#
# Two things come out of it. The fraction of the colony working on a task
# settles at delta / alpha whatever the thresholds are -- the colony matches
# the work to be done without measuring it. And when the thresholds differ,
# the same rule produces specialists: low-threshold workers do most of the
# work and high-threshold workers become a reserve that is called on only when
# the stimulus is high.
#
# Grammar note. A stimulus per task is a *global indexed by a category*, which
# is what `abm_global(.by =)` writes: the global `s` is a named vector, the rule
# is evaluated once per key with `.key` naming it, and `s` inside the rule means
# that key's value. The whole stimulus update is one rule for any number of
# tasks. `.by = j` declares the index rather than deriving it from the agents,
# which matters here: a task nobody is working on still has to have its stimulus
# rise, and it would not appear in a column-derived index.
#
# What is left of the scaffolding is on the agent side. A worker carries one
# threshold per task, so K tasks are still K columns and K take-up rules
# assembled with `rlang::new_formula()`. That is a different shape from the one
# `.by` closed, and it is the one the grammar still does not say compactly.

library(tidyABM)

# among the tasks whose threshold fired, take one at random
choose_task <- function(...) {
  f <- cbind(...)
  vapply(seq_len(nrow(f)), function(i) {
    w <- which(f[i, ])
    if (length(w) == 0L) 0L else if (length(w) == 1L) w else sample(w, 1L)
  }, integer(1))
}

dol <- function(n = 100, castes = c(500, 500), n_tasks = 2, delta = 1,
                alpha = 3, p = 0.2, ticks = 2000, seed = 1) {
  j <- seq_len(n_tasks)
  sym_th <- lapply(j, function(k) rlang::sym(paste0("theta_", k)))
  sym_f  <- lapply(j, function(k) rlang::sym(paste0("fire_", k)))

  m <- abm_setup(
    agents = do.call(abm_agents, c(
      list(n = n, task = 0L),
      stats::setNames(lapply(j, function(k) {
        rlang::new_formula(NULL, rlang::expr(sample(!!castes, n, replace = TRUE)))
      }), paste0("theta_", j)))),
    globals = list(s = stats::setNames(rep(0, n_tasks), j)),
    seed = seed
  )

  fire <- lapply(j, function(k) {
    key <- as.character(k)
    rlang::new_formula(sym_f[[k]], rlang::expr(
      task == 0L & runif(n()) < s[[!!key]]^2 /
        (s[[!!key]]^2 + (!!sym_th[[k]])^2)))
  })

  go <- abm_go(
    abm_rules(task ~ if_else(task > 0L & runif(n()) < p, 0L, task)),
    do.call(abm_rules, fire),
    abm_rules(rlang::new_formula(rlang::sym("task"), rlang::expr(
      if_else(task == 0L, choose_task(!!!sym_f), task)))),
    # one rule, whatever K is: the stimulus of every task, in one place
    abm_global(s ~ max(0, s + delta - alpha * sum(task == .key) / n()),
               .by = j)
  )
  abm_run(m, go, ticks = ticks, seed = seed)
}

if (sys.nframe() == 0L) {
  cat("delta = 1, alpha = 3, p = 0.2, N = 100, 2 tasks, 2000 ticks\n")
  cat("predicted active fraction per task = delta / alpha = 0.333\n")
  # at the fixed point a = delta/alpha per task, so idle = 1 - 2a and the
  # per-task take-up probability T solves a*p = (1 - 2a) * T * (1 - T/2)
  a <- 1 / 3
  Tstar <- uniroot(function(t) (1 - 2 * a) * t * (1 - t / 2) - a * 0.2,
                   c(1e-6, 1))$root
  cat("implied take-up probability:", round(Tstar, 3),
      "-> stimulus for theta = 500:",
      round(500 * sqrt(Tstar / (1 - Tstar))), "\n\n")

  cat(sprintf("%-24s %9s %9s %9s\n", "thresholds", "task 1", "task 2", "s_1"))
  for (cs in list(c(500, 500), c(50, 5000))) {
    out <- lapply(1:5, function(s) {
      r <- dol(castes = cs, seed = s)
      late <- r[r$tick > 1500, ]
      g <- abm_globals(r); g <- g[g$tick > 1500, ]
      c(mean(late$task == 1), mean(late$task == 2),
        mean(vapply(g$s, function(v) v[["1"]], numeric(1))))
    })
    o <- rowMeans(vapply(out, identity, numeric(3)))
    cat(sprintf("%-24s %9.3f %9.3f %9.0f\n",
                paste(cs, collapse = " / "), o[1], o[2], o[3]))
  }

  cat("\nwho does the work, two castes (theta = 50 or 5000 per task)\n")
  cat(sprintf("%-14s %10s %10s\n", "theta on task", "1", "2"))
  r <- dol(castes = c(50, 5000), seed = 2)
  late <- r[r$tick > 1500, ]
  for (th in c(50, 5000)) {
    cat(sprintf("%-14d %10.3f %10.3f\n", th,
                mean(late$task[late$theta_1 == th] == 1),
                mean(late$task[late$theta_2 == th] == 2)))
  }

  # --- figure -------------------------------------------------------------
  # Time series: the fraction of the colony on each task, against the
  # delta / alpha the colony is supposed to find without measuring it.
  library(ggplot2)

  fig_file <- function(name) {
    a <- grep("^--file=", commandArgs(), value = TRUE)
    d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
    dir.create(file.path(d, "..", "figures"), showWarnings = FALSE)
    file.path(d, "..", "figures", name)
  }

  r_fig <- dol(castes = c(500, 500), seed = 1)
  d <- do.call(rbind, lapply(1:2, function(k) data.frame(
    task = paste("task", k),
    tick = sort(unique(r_fig$tick)),
    share = as.numeric(tapply(r_fig$task == k, r_fig$tick, mean)))))

  fig <- ggplot(d, aes(tick, share, colour = task)) +
    geom_hline(yintercept = 1 / 3, linetype = "dashed", colour = "grey50") +
    geom_line(linewidth = 0.5) +
    theme_minimal() +
    labs(title = "Response thresholds: the colony finds delta / alpha on its own",
         subtitle = "100 workers, 2 tasks, equal thresholds; dashed line is 1/3",
         x = "tick", y = "share of the colony on the task", colour = NULL)
  print(fig)
  ggsave(fig_file("47-response-thresholds-and-division-of-labour.png"), fig,
         width = 6, height = 4, dpi = 120)
}
