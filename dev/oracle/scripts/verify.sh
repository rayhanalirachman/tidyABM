#!/bin/bash
# Full oracle check of one tidyABM source tree (everything except models/, see models.sh).
# usage: scripts/verify.sh [pkgdir=../.. (the package this oracle lives in)] [outdir=results/verify-<date>]
# Paths may contain spaces. Prints a summary to stdout and to <outdir>/verify.log.
set -u
O="$(cd "$(dirname "$0")/.." && pwd)"
PKG="$(cd "${1:-$O/../..}" && pwd)"
OUT="${2:-$O/results/verify-$(date +%Y%m%d-%H%M%S)}"; mkdir -p "$OUT"; OUT="$(cd "$OUT" && pwd)"
L="$OUT/verify.log"; : > "$L"
log() { echo "$*" | tee -a "$L"; }
export TIDYABM_PKG="$PKG"
log "== oracle verify  pkg=$PKG  $(date)"
log "   $(Rscript -e 'cat(R.version.string, "dplyr", format(packageVersion("dplyr")), "vctrs", format(packageVersion("vctrs")), "rlang", format(packageVersion("rlang")), "igraph", format(packageVersion("igraph")), R.version$platform)')"

log "== 1. testthat (NOT_CRAN=true); expect 379 blocks / 2020 expectations, 0 failed, 0 errors"
(cd "$PKG" && NOT_CRAN=true Rscript -e 'suppressMessages(pkgload::load_all(".", quiet=TRUE)); library(testthat); r <- as.data.frame(testthat::test_dir("tests/testthat", reporter="silent", stop_on_failure=FALSE, load_package="none")); cat("test blocks:", nrow(r), " expectations:", sum(r$nb), " failed:", sum(r$failed), " errors:", sum(r$error), " warnings:", sum(r$warning), " skipped:", sum(r$skipped), "\n")') 2>&1 | grep "test blocks" | tee -a "$L"

log "== 2. harness: 291 cases vs pkg-r6 goldens; expect 291 IDENTICAL, 0 DIFFER"
for c in cases cases2 cases3 cases4 cases5 cases6 cases7 cases8; do
  Rscript "$O/harness/driver.R" "$PKG" "$O/harness/cases/$c.R" "$OUT/$c.rds" > /dev/null 2>&1
  Rscript "$O/harness/compare.R" "$O/harness/golden/$c-old.rds" "$OUT/$c.rds" > "$OUT/harness-$c.txt" 2>&1
done
cat "$OUT"/harness-*.txt > "$OUT/harness.txt"
log "   IDENTICAL: $(grep -c ' IDENTICAL ' "$OUT/harness.txt")  DIFFER: $(grep -c ' DIFFER ' "$OUT/harness.txt")"

log "== 3. vignettes vs original outputs; expect 0 diff lines each"
mkdir -p "$OUT/vig" "$OUT/vig2"
for f in "$O"/vignettes/src/*.R; do b=$(basename "$f" .R)
  (cd "$PKG/vignettes" && timeout 280 Rscript -e "suppressMessages(pkgload::load_all(Sys.getenv('TIDYABM_PKG'), quiet=TRUE)); set.seed(1); source('$f', echo=FALSE, print.eval=TRUE)" > "$OUT/vig/$b.out" 2>&1)
  log "   $b: $(diff "$O/vignettes/golden-strict/$b.out" "$OUT/vig/$b.out" | wc -l | tr -d ' ')"
done
for b in grammar tidyABM; do
  (cd "$PKG/vignettes" && timeout 280 Rscript -e "suppressMessages(pkgload::load_all(Sys.getenv('TIDYABM_PKG'), quiet=TRUE)); set.seed(1); for (e in parse('$O/vignettes/src/$b.R', keep.source=FALSE)) try(print(withVisible(eval(e, globalenv()))\$value), silent=FALSE)" > "$OUT/vig2/$b.out" 2>&1)
  log "   (error-tolerant) $b: $(diff "$O/vignettes/golden-tolerant/$b.out" "$OUT/vig2/$b.out" | wc -l | tr -d ' ')"
done

log "== 4. benchmarks identical to goldens (single timing runs here are NOT the speed number; use pairs.sh)"
Rscript "$O/bench/tax.R" "$PKG" "$OUT/tax.rds" 2>&1 | grep "real" | tee -a "$L"
Rscript "$O/bench/ws.R" "$OUT/ws.rds" 2>&1 | grep "real" | tee -a "$L"
Rscript "$O/bench/gross.R" "$OUT/gross.rds" 2>&1 | grep "real" | tee -a "$L"
Rscript -e 'o <- commandArgs(TRUE); for (b in c("tax","ws","gross")) cat("  ", b, "identical to golden:", identical(readRDS(file.path(o[1], "bench/golden", paste0(b, ".rds"))), readRDS(file.path(o[2], paste0(b, ".rds")))), "\n")' "$O" "$OUT" 2>&1 | tee -a "$L"

log "== 5. Gross SFC check vs base-R MATLAB transcription; expect 'paths identical' 11/11"
Rscript "$O/bench/gross/gross-2022-check.R" 3 1000 200 2>&1 | grep "paths identical" | tee -a "$L"
log "load: $(uptime)"
log "DONE"
