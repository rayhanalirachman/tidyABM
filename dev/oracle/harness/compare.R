args <- commandArgs(TRUE)
a <- readRDS(args[1]); b <- readRDS(args[2])
stopifnot(identical(names(a), names(b)))
for (nm in names(a)) {
  x <- a[[nm]]; y <- b[[nm]]
  status <- if (identical(x, y)) "IDENTICAL" else "DIFFER"
  kind <- if (!is.null(x$error)) paste0("error: ", substr(gsub("\n", " ", x$error), 1, 90))
          else paste0("ok, ", length(x$warnings), " warning(s)")
  cat(sprintf("%-32s %-9s old=%s\n", nm, status, kind))
  if (status == "DIFFER") {
    for (k in union(names(x), names(y))) if (!identical(x[[k]], y[[k]]))
      cat("    component differs:", k, "\n")
    if (!is.null(y$error)) cat("    new error:", substr(gsub("\n", " ", y$error), 1, 200), "\n")
    if (!is.null(x$value) && !is.null(y$value)) print(all.equal(x$value, y$value))
  }
}
