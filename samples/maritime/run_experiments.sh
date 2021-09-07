#!/bin/bash
START=1443650401
END=1459461588
WINDOWS="7200 14400 28800 57600 115200"
STEP=7200
for WINDOW in $WINDOWS; do 
    swipl -s ./run.prolog -g "queries_on_fstream('BREST_phenesthe_input_sample.csv','log$WINDOW.csv','results.csv',$START,$END,$STEP,$WINDOW)." -g "halt."
done
