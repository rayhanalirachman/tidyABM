# Where a script's figure goes: `../<dir>/<name>`, seen from the script's own
# folder. Every script that draws sources this instead of repeating it.
fig_file <- function(name, dir = "figures") {
  a <- grep("^--file=", commandArgs(), value = TRUE)
  d <- if (length(a)) dirname(sub("^--file=", "", a[1])) else "."
  dir.create(file.path(d, "..", dir), showWarnings = FALSE)
  file.path(d, "..", dir, name)
}
