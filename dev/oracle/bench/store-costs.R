# Boundary costs of the phase-3 column store (compare spike S3, sandbox/rust-migration/
# spikes/S3-boundary/RESULT.md). usage: Rscript bench/store-costs.R <pkgdir>
# CPU (user + system) per operation, best of 5 batches of at least 0.2 s each.
pkg <- commandArgs(TRUE)[1]
suppressMessages(pkgload::load_all(pkg, quiet = TRUE, export_all = TRUE))

us <- function(f) {
  reps <- 1L
  repeat {  # batch size for >= 0.2 s
    t <- system.time(for (i in seq_len(reps)) f())[["elapsed"]]
    if (t >= 0.2) break
    reps <- reps * 4L
  }
  best <- Inf
  for (b in 1:5) {
    t <- system.time(for (i in seq_len(reps)) f())
    best <- min(best, (t[["user.self"]] + t[["sys.self"]]) / reps * 1e6)
  }
  best
}
new_store <- function(g, mirror = TRUE) AgentStore$new(g, store_special, mirror)
tbl <- function(cols, n) list(a = tibble::new_tibble(cols, nrow = n))

cat(R.version.string, "|", R.version$platform, "|", system("uptime", intern = TRUE), "\n\n")
rows <- list()
add <- function(what, n, v, unit = "us") {
  rows[[length(rows) + 1L]] <<- data.frame(what = what, n = n, value = signif(v, 3), unit = unit)
}
for (n in c(10L, 441L, 1e4L, 1e5L)) {
  id <- list(.id = seq_len(n) + 0L)
  dbl <- stats::setNames(lapply(1:10, function(j) stats::runif(n)), paste0("x", 1:10))
  chr <- list(s = sample(c("red", "green", "blue"), n, TRUE))
  g_id <- tbl(id, n); g_dbl <- tbl(c(id, dbl), n); g_chr <- tbl(c(id, chr), n)
  base_new <- us(function() new_store(g_id))
  add("copy-in, per double column (store creation)", n, (us(function() new_store(g_dbl)) - base_new) / 10)
  add("copy-in, character column, per element (pool lookup)", n,
      (us(function() new_store(g_chr)) - base_new) / n * 1e3, "ns")
  # recorded tick 0 is never cached: every call builds its vectors
  rec <- function(g) { s <- new_store(g, FALSE); s$snapshot(0L, FALSE); s }
  s_id <- rec(g_id); s_dbl <- rec(g_dbl); s_chr <- rec(g_chr)
  base_out <- us(function() s_id$recorded_groups(-1L))
  add("copy-out, per double column (built from Rust)", n,
      (us(function() s_dbl$recorded_groups(-1L)) - base_out) / 10)
  add("copy-out, character column, per element (pool)", n,
      (us(function() s_chr$recorded_groups(-1L)) - base_out) / n * 1e3, "ns")
  st <- new_store(g_dbl)
  add("export, mirror hit (the step adapter's steady state)", n, us(function() st$export()))
  add("load, nothing changed (list recognised)", n, us(function() st$load(g_dbl)))
  g1 <- g_dbl
  g1$a <- tibble::new_tibble(c(unclass(g_dbl$a)[1:10], list(x10 = dbl$x10 + 0)), nrow = n)
  flip <- FALSE
  add("load, one of 11 columns changed", n, us(function() {
    flip <<- !flip
    st$load(if (flip) g1 else g_dbl)
  }))
  ten <- tbl(c(id, list(.group = rep("a", n)), dbl[1:8]), n)
  s10 <- rec(ten)
  st10 <- new_store(ten)
  add("round trip, 10 columns built out + 10 copied in", n, us(function() {
    x <- s10$recorded_groups(-1L)
    st10$load(x)
  }))
}
# stacking 41 recorded ticks of 441 agents x 12 columns, store vs the R engine's code
g <- tbl(c(list(.id = 1:441, .group = rep("a", 441)),
           stats::setNames(lapply(1:10, function(j) stats::runif(441)), paste0("x", 1:10))), 441)
st <- new_store(g)
for (t in 0:40) {
  g$a$x1 <- g$a$x1 + 1
  st$load(g)
  st$snapshot(t, TRUE)
}
add("stack 41 ticks x 441 x 12 columns, in Rust", 441, us(function() store_population(st)))
snaps <- lapply(0:40, function(k) snapshot(list(groups = st$recorded_groups(k)), k))
add("stack the same with the R engine's bind_rows()", 441, us(function() {
  p <- dplyr::bind_rows(snaps)
  tibble::new_tibble(unclass(p)[c("tick", ".id", ".group", paste0("x", 1:10))], nrow = nrow(p))
}))
print(do.call(rbind, rows), row.names = FALSE)
