#!/bin/bash
TEST_FOLDERS='events states dynamic_temporal_phenomena'
for f in $TEST_FOLDERS; do
    echo "------- Running tests in folder \"$f\" -------"
    swipl -s $f/tests.prolog -g 'run_tests' -g 'halt.'
    echo ""
done
