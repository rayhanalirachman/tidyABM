# 48. A garbage can model of organizational choice
#     (Cohen, March & Olsen 1972, Admin. Sci. Q. 17: 1-25)
library(tidyABM)

garbage_can <- function(load = 1.1, access = c("unsegmented", "specialized"),
                        n_choices = 10, n_problems = 20, n_kinds = 3,
                        n_dms = 10, periods = 20, power = 0.55, seed = 1) {
  access <- match.arg(access)
  need <- load * n_dms * power * periods / n_problems
  open <- access == "unsegmented"
  base <- 1L
  m <- abm_setup(
    agents = list(
      choices = abm_agents(
        n = n_choices, entry = ~seq_len(n), energy = 0, added = 0,
        kind = ~rep(seq_len(n_kinds), length.out = n),
        n_att = 0L, ever = 0L, deficit = need, made = FALSE, fresh = FALSE,
        members = ~vector("list", n), style = NA_character_, made_at = NA_integer_),
      problems = abm_agents(
        n = n_problems, entry = ~rep(seq_len(n_choices), length.out = n),
        kind = ~sample(seq_len(n_kinds), n, replace = TRUE),
        solved = FALSE, att = NA_integer_),
      dms = abm_agents(n = n_dms, power = power,
                       kind = ~rep(seq_len(n_kinds), length.out = n))
    ),
    globals = list(now = 0),
    seed = seed
  )
  live_choice  <- rlang::expr(.group == "choices"  & !made   & entry <= now)
  live_problem <- rlang::expr(.group == "problems" & !solved & entry <= now)

  go <- abm_go(
    abm_global(now ~ now + 1),
    abm_rules(n_att ~ 0L, added ~ 0, fresh ~ FALSE,
              members ~ vector("list", n()), .scope = "population"),

    # every live problem attaches to the accessible choice that is closest to
    # being made -- the cost is an energy deficit, not a distance
    abm_match(pair = "nearest",
              cost = if_else(open | kind == own_kind, deficit, NA_real_),
              eligible = !!live_problem, among = !!live_choice),
    abm_rules(att ~ .partner),
    abm_tell(n_att ~ 1L, to = .partner, when = !!live_problem & !is.na(.partner),
             .resolve = "sum"),
    # ...and tells the choice who it is, so the choice can answer back later
    abm_tell(members ~ .id, to = .partner,
             when = !!live_problem & !is.na(.partner), .resolve = "collect"),

    # every decision maker puts its energy where it will make most difference
    abm_match(pair = "nearest",
              cost = if_else(open | kind == own_kind, deficit, NA_real_),
              eligible = .group == "dms", among = !!live_choice),
    abm_tell(added ~ power, to = .partner,
             when = .group == "dms" & !is.na(.partner), .resolve = "sum"),
    abm_rules(energy ~ energy + added, ever ~ ever + n_att, .scope = "population"),

    abm_rules(fresh ~ !made & energy >= need * (base + n_att),
              .scope = "population"),
    abm_rules(
      made ~ made | fresh,
      made_at ~ if_else(fresh, as.integer(now), made_at),
      style ~ dplyr::case_when(
        !fresh      ~ style,
        n_att > 0L  ~ "resolution",
        ever > 0L   ~ "flight",
        TRUE        ~ "oversight"),
      .scope = "population"),
    # a choice that has just been made solves everything still attached to it
    abm_tell(solved ~ TRUE, to = lapply(members, function(v) unlist(v)),
             when = .group == "choices" & fresh),
    abm_rules(deficit ~ pmax(0, need * (base + n_att) - energy),
              .scope = "population"),
    abm_global(n_made ~ sum(made, na.rm = TRUE),
               n_solved ~ sum(solved, na.rm = TRUE))
  )
  abm_run(m, go, ticks = periods, seed = seed)
}

cat(sprintf("%-12s %6s %8s %11s %8s %10s %10s\n", "access", "load", "decided",
            "resolution", "flight", "oversight", "solved"))
sweep <- list()
for (acc in c("unsegmented", "specialized"))
for (load in c(0.5, 1.1, 2.2, 3.3)) {
  out <- t(sapply(1:20, function(s) {
    r <- garbage_can(load = load, access = acc, seed = s)
    f <- r[r$tick == max(r$tick), ]
    ch <- f[f$.group == "choices", ]; pr <- f[f$.group == "problems", ]
    c(made = sum(ch$made), res = sum(ch$style == "resolution", na.rm = TRUE),
      fli = sum(ch$style == "flight", na.rm = TRUE),
      ove = sum(ch$style == "oversight", na.rm = TRUE),
      sol = sum(pr$solved))
  }))
  o <- colMeans(out)
  cat(sprintf("%-12s %6.1f %8.1f %11.1f %8.1f %10.1f %10.1f\n", acc, load,
              o[["made"]], o[["res"]], o[["fli"]], o[["ove"]], o[["sol"]]))
  sweep[[length(sweep) + 1]] <- data.frame(
    access = acc, load = load,
    style = factor(c("resolution", "flight", "oversight"),
                   levels = c("resolution", "flight", "oversight")),
    n = c(o[["res"]], o[["fli"]], o[["ove"]]))
}
sweep <- do.call(rbind, sweep)

# --- figure ---------------------------------------------------------------
# Parameter sweep: how the ten choices get made, by load and access structure.
# Cohen, March and Olsen's headline is that resolution is the minority route.
library(ggplot2)

fig_file <- function(name) {
  a <- grep("^--file=", commandArgs(), value = TRUE)
  d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
  dir.create(file.path(d, "..", "figures"), showWarnings = FALSE)
  file.path(d, "..", "figures", name)
}

fig <- ggplot(sweep, aes(factor(load), n, fill = style)) +
  geom_col() +
  facet_wrap(~access) +
  theme_minimal() +
  labs(title = "Garbage can: most choices are not made by resolving anything",
       subtitle = "10 choices, 20 problems, 20 periods, 20 seeds",
       x = "problem load", y = "choices made", fill = NULL)
print(fig)
ggsave(fig_file("48-garbage-can-model.png"), fig, width = 6, height = 4,
       dpi = 120)
