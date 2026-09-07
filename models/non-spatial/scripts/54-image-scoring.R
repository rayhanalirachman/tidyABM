# 54. Evolution of indirect reciprocity by image scoring
#     (Nowak & Sigmund 1998, Nature 393: 573-577; JTB 194: 561-574)
library(tidyABM)

img_of <- function(lst, i) vapply(seq_along(i), function(k) {
  v <- lst[[k]]
  if (is.na(i[[k]]) || is.null(v) || length(v) < i[[k]]) NA_real_ else v[[i[[k]]]]
}, numeric(1))

apply_obs <- function(view, msgs) {
  for (msg in msgs) {
    d <- msg[[1]]
    view[d] <- max(-5, min(5, view[d] + if (msg[[2]] > 0) 1 else -1))
  }
  view
}

image_scoring <- function(n = 100, q = 0.5, rounds = 60, gens = 60,
                          b = 1, cost = 0.1, mu = 0.01, seed = 1) {
  n_obs <- max(0L, round(q * n))
  m <- abm_setup(
    agents = abm_agents(n = n, k = ~sample(-5:6, n, replace = TRUE),
                        view = ~lapply(seq_len(n), function(i) rep(0, n)),
                        inbox = ~vector("list", n), payoff = 0,
                        helped = FALSE, aud = ~vector("list", n)),
    seed = seed
  )
  round <- abm_repeat(
    abm_rules(inbox ~ vector("list", n()),
              aud ~ lapply(seq_len(n()), function(i) sample(n(), n_obs)),
              .scope = "population"),
    abm_match(pair = "random", role = list(donor = TRUE, recipient = TRUE)),
    # the donor decides on its own image of this recipient
    abm_rules(helped ~ rep(img_of(view, .partner)[which(.role == "donor")[[1]]] >=
                           k[which(.role == "donor")[[1]]], n())),
    abm_rules(payoff ~ payoff + if_else(.role == "donor",
                                        -cost * helped, b * helped)),
    # the recipient always sees it; so does a random audience of q*n others
    abm_tell(inbox ~ Map(function(i, h) list(i, h), .id, helped),
             to = Map(function(a, p) unique(c(a, p)), aud, .partner),
             when = .role == "donor", .resolve = "collect"),
    abm_rules(view ~ Map(apply_obs, view, inbox), .scope = "population"),
    max = rounds
  )
  go <- abm_go(
    round,
    # a generation ends: reproduce in proportion to payoff, then start clean
    abm_rules(pick ~ sample(n(), n(), replace = TRUE,
                            prob = payoff - min(payoff) + 1e-6),
              .scope = "population"),
    abm_rules(k ~ ifelse(runif(n()) < mu, sample(-5:6, n(), replace = TRUE),
                         k[pick]), .scope = "population"),
    abm_rules(payoff ~ 0, view ~ lapply(seq_len(n()), function(i) rep(0, n())),
              .scope = "population"),
    abm_global(mean_k ~ mean(k), coop ~ mean(k <= 0))
  )
  abm_run(m, go, ticks = gens, seed = seed)
}
if (sys.nframe() == 0L) {
  # takes the observation probabilities as command-line arguments, so the sweep
  # can be split across sittings: Rscript m54_image_scoring.R 0.1 0.15
  args <- commandArgs(trailingOnly = TRUE)
  qs <- if (length(args)) as.numeric(args) else
    c(0.02, 0.05, 0.08, 0.10, 0.15, 0.50)
  cat("n = 100, 40 rounds per generation, 40 generations, 3 seeds, b = 1, c = 0.1\n")
  cat("Nowak & Sigmund's condition for cooperation is q > c/b = 0.1\n\n")
  cat(sprintf("%6s %10s %14s\n", "q", "mean k", "discriminators"))
  sweep <- do.call(rbind, lapply(qs, function(q) {
    o <- rowMeans(vapply(1:3, function(s) {
      r <- image_scoring(q = q, rounds = 40, gens = 40, seed = s)
      g <- abm_globals(r); g <- g[g$tick > 15, ]
      c(mean(g$mean_k), mean(g$coop))
    }, numeric(2)))
    cat(sprintf("%6.2f %10.2f %14.3f\n", q, o[1], o[2]))
    data.frame(q = q, mean_k = o[1], discriminators = o[2])
  }))

  # --- figure -------------------------------------------------------------
  # Parameter sweep: the share of the population willing to help against how
  # visible its behaviour is, with Nowak and Sigmund's q > c/b threshold.
  library(ggplot2)

  fig_file <- function(name) {
    a <- grep("^--file=", commandArgs(), value = TRUE)
    d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
    dir.create(file.path(d, "..", "figures"), showWarnings = FALSE)
    file.path(d, "..", "figures", name)
  }

  fig <- ggplot(sweep, aes(q, discriminators)) +
    geom_vline(xintercept = 0.1, linetype = "dashed", colour = "grey50") +
    geom_line(linewidth = 0.7) + geom_point() +
    ylim(0, 1) +
    theme_minimal() +
    labs(title = "Image scoring: cooperation needs to be seen",
         subtitle = "100 agents, 40 rounds per generation, 3 seeds; dashed line is q = c/b",
         x = "q, the chance a third party observes an interaction",
         y = "share with k <= 0 (willing to help)")
  print(fig)
  ggsave(fig_file("54-image-scoring.png"), fig, width = 6, height = 4,
         dpi = 120)
}
