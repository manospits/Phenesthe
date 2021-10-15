#!/bin/bash
START=1443650401
END=1459461588
WINDOWS="7200 14400 28800 57600 115200"
STEP=7200
for WINDOW in $WINDOWS; do 
    #ionice -c 2 -n 0 nice -n -20 swipl -O -s ./run.prolog -g "queries_on_fstream('BREST_phenesthe_input.csv','log$WINDOW.csv','results_serial$WINDOW.csv',$START,$END,$STEP,$WINDOW)." -g "halt."
    #taskpolicy -d important swipl -O -s ./run.prolog -g "queries_on_fstream('BREST_phenesthe_input.csv','log_serial$WINDOW.csv','results_serial$WINDOW.csv',$START,$END,$STEP,$WINDOW)." -g "halt."
    taskpolicy -d important swipl -O -s ./run.prolog -g "queries_on_fstream('BREST_phenesthe_input.csv','log$WINDOW.csv','results$WINDOW.csv',$START,$END,$STEP,$WINDOW)." -g "halt."
done
