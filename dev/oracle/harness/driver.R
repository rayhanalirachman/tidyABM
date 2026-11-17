args <- commandArgs(TRUE)
suppressMessages(pkgload::load_all(args[1], quiet = TRUE, export_all = TRUE))
source(args[2], local = TRUE)
out <- list()
for (nm in names(cases)) {
  warns <- list()
  set.seed(424242)
  res <- tryCatch(
    withCallingHandlers(list(value = cases[[nm]]()),
      warning = function(w) {
        warns[[length(warns) + 1L]] <<- list(msg = conditionMessage(w), class = class(w))
        invokeRestart("muffleWarning")
      },
      message = function(m) { warns[[length(warns) + 1L]] <<- list(msg = conditionMessage(m), class = class(m)); invokeRestart("muffleMessage") }),
    error = function(e) list(error = conditionMessage(e), class = class(e),
                             parent = if (!is.null(e$parent)) conditionMessage(e$parent)))
  res$warnings <- warns
  res$seed_after <- .Random.seed
  out[[nm]] <- res
}
saveRDS(out, args[3])
