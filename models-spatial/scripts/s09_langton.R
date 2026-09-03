# 9. Langton's Ant  (a Turmites variant, NetLogo Models Library)
#
# One mobile agent whose heading is part of its state. The L2 bundle covers
# reading the cell (`within =`), flipping it (`abm_tell(to = .cell)`) and
# turning (`abm_rules` on `heading`); the new thing is
# `abm_move(direction = <column>)` -- step one cell along a stored compass
# heading, which is L1 (the lattice knows which neighbour is north) and L2 (the
# mover is on a cell but not on the lattice) at once.
#
# Deterministic, so this is checked cell-for-cell against a plain-R reference.

suppressPackageStartupMessages(source("_setup.R", chdir = TRUE))

w <- h <- 121
start <- 61 + (61 - 1) * w

langton <- abm_setup(
  agents = list(patches = abm_agents(white = TRUE),
                ant     = abm_agents(n = 1, heading = 0L, at = ~start)),
  network = abm_network(type = "grid", dims = c(w, h), on = "patches",
                        diagonals = FALSE, torus = TRUE))

go <- abm_go(
  abm_neighbours(here_white ~ all(white),
                 within = .id == own_.cell & .group == "patches"),
  abm_rules(heading ~ (heading + if_else(here_white, 1L, 3L)) %% 4L,
            .scope = "population"),
  # the ant already knows the cell's colour, so it writes back what it read
  abm_tell(white ~ !here_white, to = .cell),
  abm_move(along = "patches", who = "ant", direction = heading)
)

reference <- function(n_steps) {
  white <- rep(TRUE, w * h); cell <- start; hd <- 0L
  for (i in seq_len(n_steps)) {
    hw <- white[cell]
    hd <- (hd + if (hw) 1L else 3L) %% 4L
    white[cell] <- !hw
    x <- ((cell - 1) %% w) + 1; y <- ((cell - 1) %/% w) + 1
    if (hd == 0L) y <- y + 1 else if (hd == 1L) x <- x + 1 else
      if (hd == 2L) y <- y - 1 else x <- x - 1
    cell <- ((x - 1) %% w) + 1 + ((y - 1) %% h) * w
  }
  list(white = white, cell = cell, heading = hd)
}

cat("121x121 torus, one ant, heading 0 = north\n\n")
for (N in c(100, 500, 2000)) {
  r <- abm_run(langton, go, ticks = N, seed = 1, record = "final",
               progress = FALSE)
  fin <- r[r$tick == N, ]
  p <- fin[fin$.group == "patches", ]; p <- p[order(p$.id), ]
  a <- fin[fin$.group == "ant", ]
  ref <- reference(N)
  cat(sprintf("%5d steps: %4d black cells; matches reference: %s\n",
              N, sum(!p$white),
              identical(p$white, ref$white) && a$.cell == ref$cell &&
                a$heading == ref$heading))
}
cat("\n  From an all-white field the trajectory is chaotic for about 10,000\n")
cat("  steps and then locks into a period-104 highway. Raise the tick count\n")
cat("  above 11,000 to see it.\n")
