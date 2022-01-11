Phenesthe - Maritime use case example

!!!Note Git LFS is required to download the compressed dataset file.!!!
https://github.com/git-lfs/git-lfs/blob/main/docs/man/git-lfs-fetch.1.ronn


1. Perform all experiments for window sizes 2,4,8,16 and 32 hours and step 2 hours.
    - open a terminal
    - run the ./run_experiments.sh

2. Perform a single experiment
    - open SWI-prolog
    - load the init.file
        ?- ['init.prolog'].
    - call the queries_on_fstream predicate with appropriate arguments e.g.,
        ?- queries_on_fstream('BREST_phenesthe.input','logs/log7200.csv','results/results7200.out',1443650401,1443650401,7200,7200).
    - once processing completes of the input phenomena is finished you can close prolog.
