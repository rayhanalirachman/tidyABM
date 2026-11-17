#!/bin/bash
# usage: runmodels-new.sh <pkgdir> <log> : rerun models/ scripts on <pkgdir>, diff against the r6 outputs in v8/mr/old
SP=/private/tmp/claude-501/-Users-rayhanalirachman--claude-spares-spare-I0elJW/9409e65c-885e-4db3-acb0-2dc090ad97a6/scratchpad
P=$1; L=$2; : > $L; O=$SP/r9b/mr-out; mkdir -p $O
for f in $SP/v8/mr/new/scripts/*.R; do
  b=$(basename $f .R)
  cd $SP/v8/mr/new/scripts
  R_PROFILE_USER=$SP/v7/prof2.R PKG=$P /usr/bin/time -p -o $O/$b.time timeout 600 Rscript $b.R > $O/$b.out 2>&1; rc=$?
  echo "$b real=$(awk '/real/{print $2}' $O/$b.time) user=$(awk '/user/{print $2}' $O/$b.time) exit=$rc diff=$(diff $SP/v8/mr/old/$b.out $O/$b.out | wc -l)" >> $L
done
echo DONE >> $L
