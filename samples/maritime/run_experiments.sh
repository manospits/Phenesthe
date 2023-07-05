#!/bin/bash
START=1443650401
END=1459461588

WINDOWS="172800 345600 691200 1382400 0"
STEPS="7200 86400"


for STEP in $STEPS; do
	for WINDOW in $WINDOWS; do 
        echo mult:future $STEP --- $WINDOW
        swipl -O -s ./init_f.prolog -g "queries_on_fstream('BREST_phenesthe.input','./logs_future/logW${WINDOW}S$STEP.csv','/dev/null',$START,$END,$STEP,$WINDOW)." -g "halt."
        echo mult:past $STEP --- $WINDOW
        swipl -O -s ./init_p.prolog -g "queries_on_fstream('BREST_phenesthe.input','./logs_past/logW${WINDOW}S$STEP.csv','/dev/null',$START,$END,$STEP,$WINDOW)." -g "halt."
        echo ser:future $STEP --- $WINDOW
        swipl -O -s ./init_f.prolog -g "phe_setval(multithreading,0), queries_on_fstream('BREST_phenesthe.input','./logs_future_serial/logW${WINDOW}S$STEP.csv','/dev/null',$START,$END,$STEP,$WINDOW)." -g "halt."
        echo ser:past $STEP --- $WINDOW
        swipl -O -s ./init_p.prolog -g "phe_setval(multithreading,0), queries_on_fstream('BREST_phenesthe.input','./logs_past_serial/logW${WINDOW}S$STEP.csv','/dev/null',$START,$END,$STEP,$WINDOW)." -g "halt."
	done
done

