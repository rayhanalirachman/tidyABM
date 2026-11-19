# The Rust column store (migration phase 3): groups go in and come back bit for
# bit, the mirror cache copies only what changed, and the store stacks recorded
# ticks into exactly the table the R engine builds. Failures name the platform
# (helper-rust.R) and the first differing element.

store_of <- function(groups, mirror = FALSE) {
  rust_call(AgentStore$new(groups, store_special, mirror))
}

# every column bit for bit (same_bits), then the whole object with identical()
expect_round_trip <- function(groups, what) {
  # copied in at once ("both"), and kept lazily then copied in by a snapshot ("rust")
  lazy <- store_of(groups, mirror = TRUE)
  lazy$snapshot(0L, TRUE)
  outs <- list(eager = store_of(groups)$export(), lazy = lazy$recorded_groups(0L))
  for (how in names(outs)) {
    out <- outs[[how]]
    for (g in names(groups)) for (col in names(groups[[g]])) {
      expect_same_as_r(out[[g]][[col]], groups[[g]][[col]], paste(what, how, g, col))
    }
    expect(identical(out, groups),
           paste0(what, " (", how, "): the groups differ after the round trip on ",
                  rust_platform(), "; ", diff_where(groups, out) %||% "?"))
  }
}

test_that("every kind of column crosses the store unchanged", {
  n <- 6L
  nan_payload <- unlist(lapply(c("7ff8000000000000", "fff8000000000000",
                                 "7ff0000000000001", "7ff00000000007a2"), function(h)
    readBin(as.raw(strtoi(substring(h, seq(15, 1, -2), seq(16, 2, -2)), 16L)), "double")))
  latin <- "caf\xe9"
  Encoding(latin) <- "latin1"
  plain <- tibble::tibble(
    .id    = 1:n,                          # an ALTREP compact sequence
    .group = "a",
    dbl    = c(0, -0, NA, NaN, Inf, 1 / 3),
    nan    = c(nan_payload, -Inf, .Machine$double.xmin),
    int    = c(1L, NA, -2147483647L, 0L, 2147483647L, 5L),
    lgl    = c(TRUE, FALSE, NA, TRUE, NA, FALSE),
    chr    = c("x", NA, "", "été", latin, "x"),
    alt    = as.character(seq_len(n)),     # an ALTREP deferred string
    fct    = factor(c("b", "a", NA, "b", "c", "a"), levels = c("c", "b", "a")),
    ord    = factor(c(1, 2, 3, 1, 2, 3), ordered = TRUE),
    date   = as.Date("2024-01-01") + c(0:4, NA),
    time   = as.POSIXct("2024-01-01 12:00", tz = "UTC") + 1:6,
    named  = stats::setNames(as.double(1:6), letters[1:6]),
    lst    = list(1, "a", NULL, 1:3, list(x = 1), NA),
    cpx    = complex(real = 1:6, imaginary = -1),
    raw    = as.raw(1:6),
    df     = tibble::tibble(u = 1:6, v = letters[1:6]),
    mat    = matrix(as.double(1:12), ncol = 2)
  )
  groups <- list(a = plain, b = plain[2:1, c(".id", ".group", "int")],
                 empty = plain[0, ])
  expect_round_trip(groups, "mixed columns")

  s <- store_of(groups)$stats()
  # list, complex, raw and data-frame columns are held as R objects; the typed
  # columns with attributes (factor, Date, POSIXct, matrix, ...) keep them
  typed <- vapply(plain, typeof, "") %in% c("double", "integer", "logical", "character")
  classed <- typed & !vapply(lapply(plain, attributes), is.null, NA)
  expect_identical(s[["opaque"]], 2L * sum(!typed))  # in groups a and empty
  expect_identical(s[["attrs"]], 2L * sum(classed))
  expect_gte(sum(classed), 5L)
})

test_that("a group with no rows or one column, and an empty run, survive", {
  one <- list(g = tibble::tibble(.id = integer()))
  expect_round_trip(one, "zero rows")
  expect_round_trip(list(g = tibble::tibble(.id = 1L, .group = NA_character_)), "NA group")
  expect_round_trip(stats::setNames(list(), character()), "no groups")
})

test_that("the mirror cache hands back what R gave and copies only changed columns", {
  g <- list(a = tibble::tibble(.id = 1:3, .group = "a", x = c(1, 2, 3), y = c("p", "q", "r")),
            b = tibble::tibble(.id = 4:5, .group = "b", z = c(TRUE, NA)))
  st <- store_of(g, mirror = TRUE)
  s0 <- st$stats()
  expect_identical(s0[["cols_copied"]], 7L)
  expect_identical(st$export(), g)

  st$load(st$export())  # nothing changed: the same list comes back
  expect_identical(st$stats()[["lists_reused"]], 1L)
  expect_identical(st$stats()[["cols_copied"]], 7L)

  g2 <- g
  g2$a$x <- g2$a$x * 2   # one column of one group
  st$load(g2)
  s2 <- st$stats()
  expect_identical(s2[["cols_copied"]], 8L)
  expect_identical(s2[["cols_reused"]], 3L)
  expect_identical(s2[["groups_reused"]], 1L)
  expect_identical(st$export(), g2)
  # with mirrors a typed column is kept as R's vector until a snapshot may reach
  # the result; then it is copied in, and strings are pooled once each: the
  # column names, group names and values
  nms <- unique(c(store_special, names(g2), names(g2$a), names(g2$b)))
  expect_identical(s2[["pool"]], length(nms))
  expect_identical(s2[["typed"]], 0L)
  st$snapshot(0L, TRUE)
  s3 <- st$stats()
  expect_identical(s3[["pool"]], length(unique(c(nms, "p", "q", "r"))))
  expect_identical(s3[["typed"]], 7L)
  expect_identical(st$export(), g2)                  # still R's own objects
  expect_identical(store_population(st)$x, c(2, 4, 6, NA, NA))
})

test_that("set_col() replaces or appends a column and drops the stale mirrors", {
  g <- list(a = tibble::tibble(.id = 1:3, .group = "a", x = c(1, 2, 3)),
            b = tibble::tibble(.id = 4:5, .group = "b", x = c(7, 8)))
  for (mirror in c(TRUE, FALSE)) {
    st <- store_of(g, mirror = mirror)
    e0 <- st$export()
    st$set_col("a", "x", c(10, 20, 30))
    st$set_col("b", "w", c(TRUE, NA))
    want <- g
    want$a <- set_col(want$a, "x", c(10, 20, 30))
    want$b <- set_col(want$b, "w", c(TRUE, NA))
    e1 <- st$export()
    expect_identical(e1, want, label = paste("mirror", mirror))
    expect_identical(e0, g)                           # what R held is untouched
    expect_identical(names(e1$b), c(".id", ".group", "x", "w"))
    st$snapshot(0L, TRUE)
    expect_identical(store_population(st)$w, c(NA, NA, NA, TRUE, NA))
    expect_error(st$set_col("a", "x", 1), "one value per row")
    expect_error(st$set_col("zz", "x", 1:3), "no such group")
    expect_identical(st$export(), want)               # a refused write changes nothing
  }
})

test_that("a store without mirrors exports fresh copies of its own values", {
  g <- list(a = tibble::tibble(.id = 1:3, .group = "a", x = c(1, NA, 3)))
  st <- store_of(g, mirror = FALSE)
  out <- st$export()
  expect_identical(out, g)
  # a fresh vector: changing the copy R holds cannot reach the store
  out$a$x[1] <- 99
  expect_identical(st$export(), g)
})

test_that("without import mirrors, what R reads back is built from the Rust values once", {
  g <- list(a = tibble::tibble(.id = 1:3, .group = "a", x = c(1, 2, 3), y = c(TRUE, NA, FALSE)))
  st <- store_of(g, mirror = FALSE)
  e1 <- st$export()
  expect_identical(e1, g)
  expect_identical(st$export(), e1)      # built once, then handed back
  st$load(e1)                            # R hands back what it was given
  expect_identical(st$stats()[["lists_reused"]], 1L)
  e2 <- e1
  e2$a$x <- e2$a$x + 1
  st$load(e2)
  s <- st$stats()
  expect_identical(s[["cols_copied"]], 5L)  # the 4 first loaded + x
  expect_identical(s[["cols_reused"]], 3L)  # .id, .group and y: the vectors built above
  e3 <- st$export()
  expect_identical(e3, e2)
  expect_false(identical(rlang::obj_address(e3$a$x), rlang::obj_address(e2$a$x)))
  expect_identical(rlang::obj_address(e3$a$y), rlang::obj_address(e1$a$y))
})

test_that("bad input is a classed error, not a crash", {
  expect_error(store_of(list(1:3)), class = "tidyABM_rust_error")
  expect_error(store_of(list(a = 1:3)), class = "tidyABM_rust_error")
  expect_error(store_of(list(a = list(1))), class = "tidyABM_rust_error")
  st <- store_of(list(a = tibble::tibble(.id = 1L)))
  expect_error(rust_call(st$recorded_groups(3L)), class = "tidyABM_rust_error")
})

# What run_once() makes of a list of recorded (tick, groups) pairs
r_population <- function(recorded, first) {
  snaps <- Map(function(t, g) snapshot(list(groups = g), t), recorded$ticks, recorded$groups)
  pop <- if (length(snaps)) dplyr::bind_rows(snaps) else snapshot(list(groups = first), 0L)[0, , drop = FALSE]
  front <- c("tick", ".id", ".group")
  cols <- names(pop)
  tibble::new_tibble(unclass(pop)[c(front, cols[!cols %in% front])], nrow = vctrs::vec_size(pop))
}

expect_same_stack <- function(seq, keep, rust_stacks, what) {
  st <- store_of(seq[[1]])
  st$snapshot(0L, keep[[1]])
  for (t in seq_along(seq)[-1]) {
    st$load(seq[[t]])
    st$snapshot(t - 1L, keep[[t]])
  }
  rec <- list(ticks = which(keep) - 1L, groups = seq[keep])
  ref <- r_population(rec, seq[[1]])
  got <- store_population(st)
  expect(identical(got, ref), paste0(what, ": the stacked results differ on ",
                                     rust_platform(), "; ", diff_where(ref, got) %||% "?"))
  for (col in names(ref)) expect_same_as_r(got[[col]], ref[[col]], paste(what, col))
  expect_identical(!is.null(st$assemble()), rust_stacks, label = paste(what, "stacked in Rust"))
}

test_that("the store stacks recorded ticks exactly as the R engine does", {
  a <- tibble::tibble(.id = 1:3, .group = "a", x = c(1, NA, 3), k = c(1L, NA, 2L))
  b <- tibble::tibble(.id = 4:5, .group = "b", flag = c(TRUE, NA), x = c(-0, NaN))
  two <- list(a = a, b = b)
  later <- list(a = dplyr::mutate(a, new = c("u", NA, "w")), b = b)
  fewer <- list(a = a[2, ], b = b[0, ])
  seq <- list(two, two, later, fewer, two)
  for (keep in list(rep(TRUE, 5), c(TRUE, FALSE, TRUE, FALSE, TRUE),
                    c(FALSE, FALSE, FALSE, FALSE, TRUE), rep(FALSE, 5))) {
    expect_same_stack(seq, keep, TRUE, paste("typed", paste(keep, collapse = "")))
  }
  expect_same_stack(list(list(a = a)), TRUE, TRUE, "one group, tick 0 only")

  # what R must stack: a classed column, a list column, a type change, `tick`
  fct <- list(a = dplyr::mutate(a, f = factor(c("p", "q", "p"))), b = b)
  lst <- list(a = dplyr::mutate(a, l = list(1, 2, 3)), b = b)
  dbl <- list(a = dplyr::mutate(a, k = as.double(k)), b = b)
  tck <- list(a = dplyr::mutate(a, tick = 9L), b = b)
  for (case in list(list(fct, "factor"), list(lst, "list column"),
                    list(dbl, "integer then double"), list(tck, "a column called tick"))) {
    expect_same_stack(list(two, case[[1]], two), rep(TRUE, 3), FALSE, case[[2]])
  }
})

test_that("a column one group lacks is R's own NA in the stacked result", {
  a <- tibble::tibble(.id = 1L, .group = "a")
  b <- tibble::tibble(.id = 2L, .group = "b", x = 2.5, i = 3L, l = TRUE, s = "z")
  st <- store_of(list(a = a, b = b))
  st$snapshot(0L, TRUE)
  got <- store_population(st)
  ref <- vctrs::vec_rbind(a, b)
  for (col in c("x", "i", "l", "s")) expect_same_as_r(got[[col]], ref[[col]], paste("NA fill", col))
})
