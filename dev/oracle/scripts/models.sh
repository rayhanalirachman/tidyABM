#!/bin/bash
# Run the 68 models/ scripts (frozen copies) on one tidyABM tree and diff their printed
# output against the pkg-r6 goldens. ~17 min CPU on this Mac. Expect 68/68 exit=0 diff=0.
# usage: scripts/models.sh [pkgdir=../.. (the package this oracle lives in)] [outdir=results/models-<date>]
set -u
O="$(cd "$(dirname "$0")/.." && pwd)"
PKG="$(cd "${1:-$O/../..}" && pwd)"
OUT="${2:-$O/results/models-$(date +%Y%m%d-%H%M%S)}"; mkdir -p "$OUT"; OUT="$(cd "$OUT" && pwd)"
L="$OUT/models.log"; : > "$L"
# run from a copy: scripts write figures/, *.s.out and <scripts>/../../../pkgdown/assets/viewers/*.html
mkdir -p "$OUT/run" "$OUT/../pkgdown/assets/viewers"
cp -R "$O/models/scripts" "$OUT/run/scripts"; cd "$OUT/run/scripts"
ONLY="${3:-}"   # optional: space-separated script names to run (default all)
for f in *.R; do b=$(basename "$f" .R)
  if [ -n "$ONLY" ] && [[ " $ONLY " != *" $b "* ]]; then continue; fi
  R_PROFILE_USER="$O/models/rprofile.R" PKG="$PKG" /usr/bin/time -p -o "$OUT/$b.time" timeout 600 Rscript "$f" > "$OUT/$b.out" 2>&1; rc=$?
  echo "$b real=$(awk '/real/{print $2}' "$OUT/$b.time") user=$(awk '/user/{print $2}' "$OUT/$b.time") exit=$rc diff=$(diff "$O/models/golden/$b.out" "$OUT/$b.out" | wc -l | tr -d ' ')" >> "$L"
done
awk '{for(i=1;i<=NF;i++){split($i,a,"="); if(a[1]=="exit"&&a[2]!=0)e++; if(a[1]=="diff"&&a[2]!=0)d++; if(a[1]=="user")u+=a[2]}} END{printf "SUMMARY scripts=%d nonzero_exit=%d diff=%d user_total=%.1f s\n", NR, e, d, u}' "$L" | tee -a "$L"
echo "load: $(uptime)" >> "$L"; echo DONE >> "$L"
