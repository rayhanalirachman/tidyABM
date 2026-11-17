#!/bin/bash
# usage: runvig.sh <pkgdir> <outdir>
mkdir -p $2
for f in /private/tmp/claude-501/-Users-rayhanalirachman--claude-spares-spare-I0elJW/9409e65c-885e-4db3-acb0-2dc090ad97a6/scratchpad/vig/*.R; do b=$(basename $f .R)
  (cd "$1/vignettes" && timeout 280 Rscript -e "suppressMessages(pkgload::load_all('$1', quiet=TRUE)); set.seed(1); source('$f', echo=FALSE, print.eval=TRUE)" > $2/$b.out 2>&1)
done
