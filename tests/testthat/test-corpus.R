# The corpus rule (models/README.md): before the first run, a model defines only
# functions and the results of abm_setup() / abm_go(). `models/` is not part of
# the built package, so this runs from the source tree only.

core <- c("abm_setup", "abm_go", "abm_run", "abm_grid")

loose_definitions <- function(exprs) {
  head_of <- function(e) if (is.call(e) && is.symbol(e[[1]])) as.character(e[[1]]) else ""
  out <- character()
  for (e in exprs) {
    if ("abm_run" %in% all.names(e)) break
    if (head_of(e) != "<-") next
    value <- e[[3]]
    if (head_of(value) != "function" && !any(core %in% all.names(value))) {
      out <- c(out, deparse1(e[[2]]))
    }
  }
  out
}

models <- test_path("..", "..", "models")

test_that("model scripts define nothing loose before the first run", {
  skip_if_not(dir.exists(models), "models/ is not in this tree")
  scripts <- Sys.glob(file.path(models, "*", "scripts", "*.R"))
  expect_gt(length(scripts), 60)
  for (f in scripts) {
    expect_identical(loose_definitions(parse(f)), character(), label = basename(f))
  }
})

test_that("model pages define nothing loose before the first run", {
  skip_if_not(dir.exists(models), "models/ is not in this tree")
  pages <- Sys.glob(file.path(models, "*", "[0-9]*.md"))
  for (f in pages) {
    x <- readLines(f, encoding = "UTF-8")
    open <- which(x == "```r")[1]
    if (is.na(open)) next
    close <- which(x == "```")
    code <- x[seq(open + 1, close[close > open][1] - 1)]
    expect_identical(loose_definitions(parse(text = code)), character(),
                     label = basename(f))
  }
})
