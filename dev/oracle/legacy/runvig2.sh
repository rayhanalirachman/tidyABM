#!/bin/bash
mkdir -p $2
for b in grammar tidyABM; do
  (cd "$1/vignettes" && timeout 280 Rscript -e "suppressMessages(pkgload::load_all('$1', quiet=TRUE)); set.seed(1); for (e in parse('/private/tmp/claude-501/-Users-rayhanalirachman--claude-spares-spare-I0elJW/9409e65c-885e-4db3-acb0-2dc090ad97a6/scratchpad/vig/$b.R', keep.source=FALSE)) try(print(withVisible(eval(e, globalenv()))\$value) , silent=FALSE)" > $2/$b.out 2>&1)
done
