#!/bin/bash
# usage: pairsN.sh <old-dir containing tidyABM link, e.g. /private/tmp/claude-501/-Users-rayhanalirachman--claude-spares-spare-I0elJW/9409e65c-885e-4db3-acb0-2dc090ad97a6/scratchpad/gr6> [npairs=4] ; interleaved old/new, log pairsN.log
SP=/private/tmp/claude-501/-Users-rayhanalirachman--claude-spares-spare-I0elJW/9409e65c-885e-4db3-acb0-2dc090ad97a6/scratchpad; OLD=${1:-$SP/gr6}; N=${2:-4}; L=$SP/pairsN.log; : > $L
for i in $(seq $N); do for v in old new; do
  if [ $v = old ]; then d=$OLD; else d=$SP/gnew; fi
  echo "tax $v $(cd $d && Rscript $SP/cmp.R $d/tidyABM 2>&1 | grep -o 'real.*')" >> $L
  echo "ws $v $(cd $d && Rscript $SP/ws-r.R $SP/ws-pair-$v.rds 2>&1 | grep -o 'real.*')" >> $L
  echo "gross $v $(cd $d && Rscript $SP/gross-r.R $SP/gross-pair-$v.rds 2>&1 | grep -o 'real.*')" >> $L
done; done
Rscript -e 'for (b in c("ws","gross")) cat(b, "old==new:", identical(readRDS(sprintf("/private/tmp/claude-501/-Users-rayhanalirachman--claude-spares-spare-I0elJW/9409e65c-885e-4db3-acb0-2dc090ad97a6/scratchpad/%s-pair-old.rds",b)), readRDS(sprintf("/private/tmp/claude-501/-Users-rayhanalirachman--claude-spares-spare-I0elJW/9409e65c-885e-4db3-acb0-2dc090ad97a6/scratchpad/%s-pair-new.rds",b))), "\n")' >> $L
echo "load: $(uptime)" >> $L; echo DONE >> $L
