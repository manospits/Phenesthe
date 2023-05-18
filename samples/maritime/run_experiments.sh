#!/bin/bash
START=1443650401
END=1459461588
WINDOWS="172800 345600 691200 1382400"
STEP=7200
for WINDOW in $WINDOWS; do 
    swipl -O -s ./init_f.prolog -g "queries_on_fstream('BREST_phenesthe.input','./logs_future/log$WINDOW.csv','/dev/null',$START,$END,$STEP,$WINDOW)." -g "halt."
    swipl -O -s ./init_p.prolog -g "queries_on_fstream('BREST_phenesthe.input','./logs_past/log$WINDOW.csv','/dev/null',$START,$END,$STEP,$WINDOW)." -g "halt."
done

