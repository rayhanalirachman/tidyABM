#!/bin/bash
L=/private/tmp/claude-501/-Users-rayhanalirachman--claude-spares-spare-I0elJW/9409e65c-885e-4db3-acb0-2dc090ad97a6/scratchpad/pairs.log; : > $L
for i in 1 2 3 4; do
  for v in r4 new; do
    if [ $v = r4 ]; then d=/private/tmp/claude-501/-Users-rayhanalirachman--claude-spares-spare-I0elJW/9409e65c-885e-4db3-acb0-2dc090ad97a6/scratchpad/gr4; p=/private/tmp/claude-501/-Users-rayhanalirachman--claude-spares-spare-I0elJW/9409e65c-885e-4db3-acb0-2dc090ad97a6/scratchpad/pkg-r4; else d=/private/tmp/claude-501/-Users-rayhanalirachman--claude-spares-spare-I0elJW/9409e65c-885e-4db3-acb0-2dc090ad97a6/scratchpad/gnew; p=/private/tmp/claude-501/-Users-rayhanalirachman--claude-spares-spare-I0elJW/9409e65c-885e-4db3-acb0-2dc090ad97a6/scratchpad/gnew/tidyABM; fi
    echo "tax $v $(cd $d && Rscript /private/tmp/claude-501/-Users-rayhanalirachman--claude-spares-spare-I0elJW/9409e65c-885e-4db3-acb0-2dc090ad97a6/scratchpad/cmp.R $p 2>&1 | grep -o 'cpu [0-9.]*')" >> $L
    echo "ws $v $(cd $d && Rscript /private/tmp/claude-501/-Users-rayhanalirachman--claude-spares-spare-I0elJW/9409e65c-885e-4db3-acb0-2dc090ad97a6/scratchpad/ws-r.R /private/tmp/claude-501/-Users-rayhanalirachman--claude-spares-spare-I0elJW/9409e65c-885e-4db3-acb0-2dc090ad97a6/scratchpad/ws-pair.rds 2>&1 | grep -o 'cpu [0-9.]*')" >> $L
  done
done
for i in 1 2; do
  for v in r4 new; do
    if [ $v = r4 ]; then d=/private/tmp/claude-501/-Users-rayhanalirachman--claude-spares-spare-I0elJW/9409e65c-885e-4db3-acb0-2dc090ad97a6/scratchpad/gr4; else d=/private/tmp/claude-501/-Users-rayhanalirachman--claude-spares-spare-I0elJW/9409e65c-885e-4db3-acb0-2dc090ad97a6/scratchpad/gnew; fi
    echo "gross $v $(cd $d && Rscript /private/tmp/claude-501/-Users-rayhanalirachman--claude-spares-spare-I0elJW/9409e65c-885e-4db3-acb0-2dc090ad97a6/scratchpad/gross-r.R /private/tmp/claude-501/-Users-rayhanalirachman--claude-spares-spare-I0elJW/9409e65c-885e-4db3-acb0-2dc090ad97a6/scratchpad/gross-pair.rds 2>&1 | grep -o 'cpu [0-9.]*')" >> $L
  done
done
echo "load: $(uptime)" >> $L; echo DONE >> $L
