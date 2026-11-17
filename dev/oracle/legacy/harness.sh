#!/bin/bash
# usage: harness.sh <newpkg> [cases...] ; compares against v8/*-old.rds (pkg-r6)
SP=/private/tmp/claude-501/-Users-rayhanalirachman--claude-spares-spare-I0elJW/9409e65c-885e-4db3-acb0-2dc090ad97a6/scratchpad
cd $SP/gnew
pkg=$1; shift
list="${@:-cases cases2 cases3 cases4 cases5 cases6 cases7 cases8}"
for c in $list; do
  f=$SP/v7/$c.R; [ -f $f ] || f=$SP/r8/h/$c.R; [ -f $f ] || f=$SP/v8/$c.R; [ -f $f ] || f=$SP/r9/$c.R
  old=$SP/v8/$c-old.rds; [ -f $old ] || old=$SP/r9/$c-old.rds
  [ -f $old ] || Rscript $SP/v7/driver.R $SP/gr6/tidyABM $f $old >/dev/null 2>&1
  Rscript $SP/v7/driver.R $pkg $f $SP/r9/$c-new.rds >/dev/null 2>&1
  echo "=== $c"; Rscript $SP/v7/compare.R $old $SP/r9/$c-new.rds
done
