#!/bin/bash
START=1443650401
END=1459461588
WINDOWS="7200 14400 28800 57600 115200"
STEP=7200
for WINDOW in $WINDOWS; do 
    swipl -O -s ./init.prolog -g "queries_on_fstream('BREST_phenesthe.input','logs/log$WINDOW.csv','results/results$WINDOW.out',$START,$END,$STEP,$WINDOW)." -g "halt."
    swipl -O -s ./init_serial.prolog -g "queries_on_fstream('BREST_phenesthe.input','logs/log_serial$WINDOW.csv','results/results_serial$WINDOW.out',$START,$END,$STEP,$WINDOW)." -g "halt."
done

