# classify Rprof samples: user-expression vs engine vs GC
args <- commandArgs(TRUE)
suppressMessages(pkgload::load_all("tidyABM", quiet = TRUE))
eng <- ls(asNamespace("tidyABM"), all.names = TRUE)
L <- readLines(args[1])[-1]
st <- lapply(strsplit(L, " ", fixed = TRUE), function(x) gsub('"', "", x))
st <- lapply(st, function(x) sub("^.*::", "", x))
n <- length(st)
cat1 <- character(n); leaf <- character(n); engfn <- character(n); userfn <- character(n)
for (i in seq_len(n)) {
  s <- st[[i]]; gc <- s[1] == "<GC>"; if (gc) s <- s[-1]
  # innermost point where engine hands over to a user expression
  k <- which(s %in% c("eval_tidy", "eval") & c(s[-1], "") %in% eng)
  m <- which(s == "eval_tidy"); k <- c(k, m)
  ie <- which(s %in% eng)
  first_eng <- if (length(ie)) min(ie) else NA
  user <- length(k) && (is.na(first_eng) || min(k) < first_eng)
  leaf[i] <- s[1]
  engfn[i] <- if (!is.na(first_eng)) s[first_eng] else "(outside)"
  if (user) {
    kk <- min(k)
    if (kk > 1 && s[kk - 1] == ".External2") kk <- kk - 1
    userfn[i] <- if (kk == 1) "(C primitives)" else s[kk - 1]
    cat1[i] <- if (gc) "a_gc" else if (kk == 1) "a_prim" else "a_Rfun"
  } else cat1[i] <- if (gc) "b_gc" else "b_engine"
}
pc <- function(t) round(100 * sort(t, decreasing = TRUE) / n, 1)
cat("samples", n, "\n"); print(round(100 * table(cat1) / n, 1))
cat("\n-- user-expr: outermost fn called by the expression\n"); print(head(pc(table(userfn[startsWith(cat1, "a")])), 14))
cat("\n-- user-expr leaf fns\n"); print(head(pc(table(leaf[startsWith(cat1, "a")])), 14))
cat("\n-- engine (non-expression) by nearest tidyABM fn\n"); print(head(pc(table(engfn[startsWith(cat1, "b")])), 25))
cat("\n-- engine leaf fns\n"); print(head(pc(table(leaf[cat1 == "b_engine"])), 25))
