% load Phenesthe
:-['../../phenesthe.prolog'].
% load the maritime definitions
:-['definitions.prolog'].
% preprocess phenomena definitions (transform them, find evaluation order, etc.)
:-preprocess_phenomena_definitions.
:-set_prolog_flag(stack_limit, 2_147_483_648).

:-queries_on_fstream('BREST_phenesthe.input','logs/log_86400.csv','results/results_86400.out',1443650401,1443909601,259200,259200).

%:-queries_on_fstream('BREST_phenesthe.input','logs/log_86400.csv','results/results_86400.out',1443650401,1444255201,604800,604800).

%:-queries_on_fstream('BREST_phenesthe.input','logs/log_86400.csv','results/results_86400.out',1443650401,1444860001,1209600,1209600).

%:-queries_on_fstream('BREST_phenesthe.input','logs/log_86400.csv','results/results_86400.out',1443650401,1446069601,2419200,2419200).

:-phe_setval(verboselearning, 0).


:- findall(N, (dynamic_phenomenon_intervals(fishing_trip(_V,_PA,_FA,_PB),L), length(L,N)), NL),
   sumlist(NL,SNL), nl,nl, write("Number of GT intervals: "), writeln(SNL), nl,nl.


:-time(exp_learn_phenomenon(fishing_trip(V,PA,FA,PB),[V,PA,FA,PB],[start(moored(V,PB)), end(moored(V,PA)),underway(V),in_fishing_area(V,FA)],0,_Z)).

:-halt.
