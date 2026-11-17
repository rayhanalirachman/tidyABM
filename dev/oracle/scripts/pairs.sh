#!/bin/bash
# Interleaved A/B timing of the three benchmarks (one Rscript at a time, A then B, N times).
# usage: scripts/pairs.sh <pkgA> <pkgB> [npairs=3] [outdir=results/pairs-<date>]
# A == B is allowed (measures noise). Each line: bench label real/cpu. Also checks A and B
# outputs are identical to each other and to the goldens.
set -u
O="$(cd "$(dirname "$0")/.." && pwd)"
A="$(cd "$1" && pwd)"; B="$(cd "$2" && pwd)"; N="${3:-3}"
OUT="${4:-$O/results/pairs-$(date +%Y%m%d-%H%M%S)}"; mkdir -p "$OUT"; OUT="$(cd "$OUT" && pwd)"
L="$OUT/pairs.log"; : > "$L"
echo "A=$A" >> "$L"; echo "B=$B" >> "$L"; echo "start load: $(uptime)" >> "$L"
for i in $(seq "$N"); do for v in A B; do
  if [ $v = A ]; then P="$A"; else P="$B"; fi
  echo "tax $v $(Rscript "$O/bench/tax.R" "$P" "$OUT/tax-$v.rds" 2>&1 | grep -o 'real.*')" >> "$L"
  echo "ws $v $(TIDYABM_PKG="$P" Rscript "$O/bench/ws.R" "$OUT/ws-$v.rds" 2>&1 | grep -o 'real.*')" >> "$L"
  echo "gross $v $(TIDYABM_PKG="$P" Rscript "$O/bench/gross.R" "$OUT/gross-$v.rds" 2>&1 | grep -o 'real.*')" >> "$L"
done; done
Rscript -e 'o <- commandArgs(TRUE); for (b in c("tax","ws","gross")) { a <- readRDS(file.path(o[2], paste0(b,"-A.rds"))); bb <- readRDS(file.path(o[2], paste0(b,"-B.rds"))); g <- readRDS(file.path(o[1], "bench/golden", paste0(b,".rds"))); cat(b, "A==B:", identical(a, bb), " A==golden:", identical(a, g), "\n") }' "$O" "$OUT" >> "$L"
# median CPU per bench and side
Rscript "$O/scripts/median.R" "$L" >> "$L"
echo "end load: $(uptime)" >> "$L"; echo DONE >> "$L"
cat "$L"
