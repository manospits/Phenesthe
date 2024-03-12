%gp_learn_phenomenon(PhenomenaList, StartingPhenomena, Ending)
%initialise_population([start(moored(Vessel,PB)),end(moored(Vessel,PA)),underway(Vessel)],[end(moored(Vessel,PA))],[start(moored(Vessel,PB))], 2, 4, 1000, Formulae), tournament(Formulae,[Vessel,PA,PB],trip(Vessel,PA,PB),10,(F,S)), mutate_formula(F,[start(moored(Vessel,PB)),end(moored(Vessel,PA)),underway(Vessel)],[end(moored(Vessel,PA))],[start(moored(Vessel,PB))],MutatedFormula,1).
% check transformations file
%allowed_formulae(before,[instant,dinterval,ndinterval],[instant,dinterval,ndinterval]).
%allowed_formulae(meets,[dinterval,ndinterval],[dinterval,ndinterval]).
%allowed_formulae(overlaps,[dinterval,ndinterval],[dinterval,ndinterval]).
%allowed_formulae(finishes,[instant,dinterval,ndinterval],[dinterval,ndinterval]).
%allowed_formulae(starts,[instant,dinterval,ndinterval],[dinterval,ndinterval]).
%allowed_formulae(equals,[dinterval,ndinterval],[dinterval,ndinterval]).
%allowed_formulae(contains,[dinterval,ndinterval],[instant,dinterval,ndinterval]).

gp_learn_phenomenon(Phenomenon, VarList, PhenomenaList, StartingPhenomena,
                    EndingPhenomena, Min, Max, PopulationSize,
                    TournamentSize, MutationD, FitnessThr, WinningFormula):-
    initialise_population(PhenomenaList, StartingPhenomena, EndingPhenomena, Min, Max, PopulationSize, Population),
    generation(0,Population, Phenomenon, VarList, PhenomenaList, StartingPhenomena, EndingPhenomena, TournamentSize, MutationD, FitnessThr, WinningFormula).

generation(N, Population, Phenomenon, VarList, PhenomenaList, StartingPhenomena, EndingPhenomena, TournamentSize, MutationD, FitnessThr, WinningFormula):-
    write('Generation: '),writeln(N),
    length(Population, L),
    write('Population size:'), writeln(L),
    N1 is N+1,
    random(0,3,R),
    (
        (R=0,
         tournament(Population, VarList, Phenomenon, TournamentSize, (WinnerF1, Fitness1)),
         tournament(Population, VarList, Phenomenon, TournamentSize, (WinnerF2, Fitness2)),
         write('Fitness 1:'),writeln(Fitness1),
         write('Fitness 2:'),writeln(Fitness2),
         (
            (
                Fitness1 >= FitnessThr,!,
                WinningFormula=WinnerF1
            );
            (
                Fitness2 >= FitnessThr,!,
                WinningFormula=WinnerF2
            );
            (
                crossover_formulae(WinnerF1,WinnerF2,NewFormula),
                nl,nl,generation(N1, [NewFormula|Population],Phenomenon, VarList, PhenomenaList, StartingPhenomena, EndingPhenomena, TournamentSize, MutationD, FitnessThr,WinningFormula)
            )
         )
        );
        (R=1,
         tournament(Population, VarList, Phenomenon, TournamentSize, (WinnerF1, Fitness1)),
         write('Fitness 1:'),writeln(Fitness1),
            (
                (
                    Fitness1 >= FitnessThr,!,
                    WinningFormula=WinnerF1
                );
                (
                    mutate_formula(WinnerF1,PhenomenaList, StartingPhenomena, EndingPhenomena, NewFormula, MutationD),
                    nl,nl,generation(N1, [NewFormula|Population],Phenomenon, VarList, PhenomenaList, StartingPhenomena, EndingPhenomena, TournamentSize, MutationD, FitnessThr,WinningFormula)
                )
            )
        );
        (R=2,
         tournament(Population, VarList, Phenomenon, TournamentSize, (WinnerF1, Fitness1)),
         write('Fitness 1:'),writeln(Fitness1),
            (
                (
                    Fitness1 >= FitnessThr,!,
                    WinningFormula=WinnerF1
                );
                (
                    nl,nl,generation(N1, [WinnerF1|Population],Phenomenon, VarList, PhenomenaList, StartingPhenomena, EndingPhenomena, TournamentSize, MutationD, FitnessThr,WinningFormula)
                )
            )
        )
        
    ).




create_random_formula(PhenomenaList, StartingPhenomena, EndingPhenomena, Formula, Algorithm, Depth):-
    create_random_formula(PhenomenaList, StartingPhenomena, EndingPhenomena, 0, 0, Formula, Algorithm, Depth, ndinterval).

create_random_formula(_PhenomenaList, StartingPhenomena, _EndingPhenomena, 1, 0, Formula, Algorithm, Depth, Type):-
    ((Depth=0 ; check_grow(Algorithm,StartingPhenomena))),!,
    random_member(Formula,StartingPhenomena),
    formula_type(Formula,Type).

create_random_formula(_PhenomenaList, _StartingPhenomena, EndingPhenomena, 0, 1, Formula, Algorithm, Depth, Type):-
    ((Depth=0 ; check_grow(Algorithm,EndingPhenomena))),!,
    random_member(Formula,EndingPhenomena),
    formula_type(Formula,Type).

create_random_formula(PhenomenaList, _StartingPhenomena, _EndingPhenomena, 1, 1, Formula, Algorithm, Depth, Type):-
    ((Depth=0 ; check_grow(Algorithm,PhenomenaList))),!,
    random_member(Formula,PhenomenaList),
    formula_type(Formula,Type).

create_random_formula(PhenomenaList, StartingPhenomena, EndingPhenomena, LPath, RPath, Formula, Algorithm, Depth, ndinterval):-
    Depth>0,
    temporal_relation_list(TRels),
    repeat,random_member(Relation, TRels),
    Depth1 is Depth-1,
    allowed_formulae(Relation, AllowedLeft, AllowedRight),
    create_random_formula(PhenomenaList, StartingPhenomena, EndingPhenomena, 1, RPath, LFormula, Algorithm, Depth1, TypeLeft),
    create_random_formula(PhenomenaList, StartingPhenomena, EndingPhenomena, LPath, 1, RFormula, Algorithm, Depth1, TypeRight),
    member(TypeLeft, AllowedLeft),
    member(TypeRight, AllowedRight),!,
    Formula =.. [Relation,LFormula,RFormula].

check_grow(grow, Phenomena):-
        random(R),
        temporal_relation_list(TRels),
        length(Phenomena, L),
        length(TRels, F),
        LF is L+F,
        S is div(L,LF),
        R < S.


formula_type(Formula, instant):-
    term_variables(Formula,Vars),
    transform_instant_formula(Formula, Vars, _TF, _T).
formula_type(Formula, dinterval):-
    term_variables(Formula,Vars),
    transform_dinterval_formula(Formula, Vars, _TF, _T).
formula_type(Formula, ndinterval):-
    term_variables(Formula,Vars),
    transform_ndinterval_formula(Formula, Vars, _TF, _T).

initialise_population(PhenomenaList, StartingPhenomena, EndingPhenomena, Min, Max, TotalPopulation, Formulae):-
    TotalDepths is (Max - Min)+1,
    TreesPerDepth is div(TotalPopulation,TotalDepths),
    initialise_population2(PhenomenaList, StartingPhenomena, EndingPhenomena, Min, Max, TreesPerDepth, Formulae).

initialise_population2( _, _, _, Cur, Max, _, []):-
    Cur>Max,!.

initialise_population2(PhenomenaList, StartingPhenomena, EndingPhenomena, Cur, Max, TreesPerDepth, Formulae):-
    initialise_population_for_depth(PhenomenaList, StartingPhenomena, EndingPhenomena, TreesPerDepth, Cur, CFormulae),
    Cur1 is Cur+1,
    initialise_population2(PhenomenaList, StartingPhenomena, EndingPhenomena, Cur1, Max, TreesPerDepth, C1Formulae),
    append(CFormulae, C1Formulae, Formulae).

initialise_population_for_depth(PhenomenaList, StartingPhenomena, EndingPhenomena, NTrees, Depth, Formulae):-
    HTrees is div(NTrees,2),
    create_n_random_formulae(PhenomenaList, StartingPhenomena, EndingPhenomena, grow, Depth, HTrees, GFormulae),
    create_n_random_formulae(PhenomenaList, StartingPhenomena, EndingPhenomena, full, Depth, HTrees, FFormulae),
    append(GFormulae,FFormulae,Formulae).

create_n_random_formulae(_,_,_,_,_,0,[]).
create_n_random_formulae(PhenomenaList,StartingPhenomena,EndingPhenomena, Algorithm,Depth,N,[Formula|Rest]):-
    N>0,
    create_random_formula(PhenomenaList,StartingPhenomena,EndingPhenomena,Formula,Algorithm,Depth),
    N1 is N-1,
    create_n_random_formulae(PhenomenaList,StartingPhenomena,EndingPhenomena,Algorithm,Depth,N1,Rest).

% initialise_population([start(moored(Vessel,PB)),end(moored(Vessel,PA)),underway(Vessel)],[end(moored(Vessel,PA))],[start(moored(Vessel,PB))],
%   2,
%   4,=
%   100,
%   Formulae),
%   member(X,Formulae),
%   transform_ndinterval_formula(X,[Vessel,PortA,Area,PortB],TF,IL),
%   dynamic_phenomenon_intervals_internal(trip(V,PA,PB),I),
%   TF,intersection(I,IL,CI),
%   length(CI,LE),
%   LE>2.

tournament(Formulae, PheVars, GtPhenomenon, K, Winner):-
    random_member(Formula, Formulae),
    transform_ndinterval_formula(Formula, PheVars, TF, IL),
    findall((PheVars,GIL),(dynamic_phenomenon_intervals_internal(GtPhenomenon,GIL)),AllGIL),
    findall((PheVars,IL),TF,AllIL),
    fitness(AllGIL,AllIL,Fitness),
    K1 is K-1,
    tournament(Formulae, PheVars, AllGIL, K1, (Formula, Fitness), Winner).

tournament(_Formulae, _PheVars, _AllGIL, 0, Winner, Winner):-!.
tournament(Formulae, PheVars, AllGIL, K, (CWFormula,CWFitness), Winner):-
    random_member(Formula, Formulae),
    transform_ndinterval_formula(Formula, PheVars, TF, IL),
    findall((PheVars,IL),TF,AllIL),
    fitness(AllGIL,AllIL,Fitness),
    ((Fitness>CWFitness,
      NewCurrentWinner=(Formula,Fitness)
     );
     (
      Fitness=<CWFitness,
      NewCurrentWinner=(CWFormula,CWFitness)
     )
    ),
    K1 is K-1,
    tournament(Formulae, PheVars, AllGIL, K1, NewCurrentWinner, Winner).

%------------------------------------------------------------------------
% + mutate_formula(Formula, PhenomenaList, StartingPhenomena, EndingPhenomena, Formula, Algorithm, Depth)
% | headless chicken crossover, i.e., choose random
% | node in tree replace node with a newly generated tree.
mutate_formula(Formula, PhenomenaList, StartingPhenomena, EndingPhenomena, MutatedFormula, Depth):-
    repeat,
    random(R),
    random(0,Depth,NDepth),
    (
        (R < 0.5,
         get_relation_number(Formula,N),
         N1 is N+1,
         random(1,N1,Point),
         replace_subformula_at_relation_point(Formula, PhenomenaList, StartingPhenomena, EndingPhenomena, MutatedFormula, Point, NDepth),!
        );
        (R>=0.5,
         get_leaf_number(Formula,N),
         N1 is N+1,
         random(1,N1,Point),
         replace_subformula_at_leaf_point(Formula, PhenomenaList, StartingPhenomena, EndingPhenomena, MutatedFormula, Point, NDepth),!
        )
    ),!.

%------------------------------------------------------------------------
% + crossover_formulae(FormulaA,FormulaB)
% | choose node 90% of times relation 10% leave,
% | replace node with acceptable subtree from tree 2
%
crossover_formulae(FormulaA,FormulaB, CrossoverFormula):-
    repeat,
    random(R),
    (
        (R < 0.5,
         get_relation_number(FormulaA,N),
         N1 is N+1,
         random(1,N1,Point),
         get_relation_point_type(FormulaA, Point, Left, Right, Allowed),
         get_random_subformula_of_type(FormulaB, Left, Right, Allowed, SubFormulaB),!,
         replace_subformula_at_relation_point_with_formula(FormulaA, SubFormulaB, Point, CrossoverFormula)
        );
        (R>=0.5,
         get_leaf_number(FormulaA,N),
         N1 is N+1,
         random(1,N1,Point),
         get_leaf_point_type(FormulaA, Point, Left, Right, Allowed),
         get_random_subformula_of_type(FormulaB, Left, Right, Allowed, SubFormulaB),!,
         replace_subformula_at_leaf_point_with_formula(FormulaA, SubFormulaB, Point, CrossoverFormula)
        )
    ),!.

get_random_subformula_of_type(Formula, Left, Right, Allowed, SubFormula):-
     random(R),
    (
        (R < 0.5,
         %no need to check for allowed types here as it's always ndinterval
         get_relation_of_path_number(Formula,Left,Right,N),
         N>0,
         N1 is N+1,
         random(1,N1,Point),
         get_relation_of_path(Formula, Point, Left, Right, SubFormula)
        );
        (R>=0.5,
         % must check for allowed types
         get_leaf_of_path_and_type_number(Formula, Left, Right, Allowed,N),
         N > 0,
         N1 is N+1,
         random(1,N1,Point),
         get_leaf_of_path_and_type(Formula, Left, Right, Allowed, Point, SubFormula)
        )
    ).

%------------------------------------------------------------------------
% + Replace subformula at Relation node with new random formula
replace_subformula_at_relation_point(Formula, PhenomenaList, StartingPhenomena, EndingPhenomena, MutatedFormula, Point, NDepth):-
    replace_subformula_at_relation_point(Formula, PhenomenaList, StartingPhenomena, EndingPhenomena, MutatedFormula, Point, 0, _ProcessedPoints, [ndinterval], NDepth,0,0).

replace_subformula_at_relation_point(Formula, _PhenomenaList, _StartingPhenomena, _EndingPhenomena, Formula, _Point, PrevPoint, PrevPoint, _PrevAllowed, _NDepth,_Left,_Right):-
    Formula=..[Relation|_R],
    temporal_relation_list(TRels),
    \+member(Relation,TRels).
 

replace_subformula_at_relation_point(Formula, PhenomenaList, StartingPhenomena, EndingPhenomena, MutatedFormula, Point, PrevPoint, ProcessedPoints, PrevAllowed, NDepth,Left,Right):-
    Formula=..[Relation,L,R],
    temporal_relation_list(TRels),
    member(Relation,TRels),
    allowed_formulae(Relation,AllowedLeft,AllowedRight),
    CurPoint is PrevPoint+1,
    (
        ( 
            CurPoint = Point,!,
            ProcessedPoints = CurPoint,
            create_random_formula(PhenomenaList, StartingPhenomena, EndingPhenomena, Left, Right, MutatedFormula, grow, NDepth, Type),
            member(Type, PrevAllowed)
        );
        (
            CurPoint \= Point,
            replace_subformula_at_relation_point(L,PhenomenaList, StartingPhenomena, EndingPhenomena, MutatedLeft, Point, CurPoint, ProcessedPointsL, AllowedLeft, NDepth, 1, Right),
            replace_subformula_at_relation_point(R,PhenomenaList, StartingPhenomena, EndingPhenomena, MutatedRight, Point, ProcessedPointsL, ProcessedPoints, AllowedRight, NDepth, Left, 1),
            MutatedFormula=..[Relation,MutatedLeft,MutatedRight]
        )
    ).

%------------------------------------------------------------------------
% + Replace subformula at Relation node with a subformula formula
replace_subformula_at_relation_point_with_formula(FormulaA, SubFormulaB, Point, CrossoverFormula):-
    replace_subformula_at_relation_point_with_formula(FormulaA, SubFormulaB, Point, 0, _ProcessedPoints, CrossoverFormula).

replace_subformula_at_relation_point_with_formula(Formula, _SubFormulaB, _Point, Prev, Prev, Formula):-
    Formula=..[Relation|_R],
    temporal_relation_list(TRels),
    \+member(Relation,TRels),!.

replace_subformula_at_relation_point_with_formula(Formula, SubFormulaB, Point, Prev, ProcessedPoints, CrossoverFormula):-
    Formula=..[Relation, L, R],
    temporal_relation_list(TRels),
    member(Relation,TRels),
    CurPoint is Prev+1,
    (
        (
           CurPoint = Point,!,
            CrossoverFormula = SubFormulaB,
            ProcessedPoints = CurPoint
        );
        (
            replace_subformula_at_relation_point_with_formula(L, SubFormulaB, Point, CurPoint, ProcessedPointsLeft, FormulaL),
            replace_subformula_at_relation_point_with_formula(R, SubFormulaB, Point, ProcessedPointsLeft, ProcessedPoints, FormulaR),
            CrossoverFormula=..[Relation, FormulaL, FormulaR]
        )
    ).

%------------------------------------------------------------------------
%% + Replace subformula at leaf
replace_subformula_at_leaf_point(Formula, PhenomenaList, StartingPhenomena, EndingPhenomena, MutatedFormula, Point, NDepth):-
    replace_subformula_at_leaf_point(Formula, PhenomenaList, StartingPhenomena, EndingPhenomena, MutatedFormula, Point, 0, _ProcessedPoints, [ndinterval], NDepth,0,0).

replace_subformula_at_leaf_point(Formula, _PhenomenaList, _StartingPhenomena, _EndingPhenomena, Formula, Point, PrevPoint, CurPoint, _PrevAllowed, _NDepth,_Left,_Right):-
    Formula=..[Relation|_R],
    temporal_relation_list(TRels),
    \+member(Relation,TRels),
    CurPoint is PrevPoint + 1,
    CurPoint \= Point.

replace_subformula_at_leaf_point(Formula, PhenomenaList, StartingPhenomena, EndingPhenomena, MutatedFormula, Point, PrevPoint, CurPoint, PrevAllowed, NDepth, Left, Right):-
    Formula=..[Relation|_R],
    temporal_relation_list(TRels),
    \+member(Relation,TRels),
    CurPoint is PrevPoint + 1,
    CurPoint = Point,
    create_random_formula(PhenomenaList, StartingPhenomena, EndingPhenomena, Left, Right, MutatedFormula, grow, NDepth, Type), member(Type, PrevAllowed).

replace_subformula_at_leaf_point(Formula, PhenomenaList, StartingPhenomena, EndingPhenomena, MutatedFormula, Point, PrevPoint, ProcessedPoints, _PrevAllowed, NDepth,Left,Right):-
    Formula=..[Relation,L,R],
    temporal_relation_list(TRels),
    member(Relation,TRels),
    allowed_formulae(Relation,AllowedLeft,AllowedRight),
    replace_subformula_at_leaf_point(L,PhenomenaList, StartingPhenomena, EndingPhenomena, MutatedLeft, Point, PrevPoint, ProcessedPointsL, AllowedLeft, NDepth, 1, Right),
    replace_subformula_at_leaf_point(R,PhenomenaList, StartingPhenomena, EndingPhenomena, MutatedRight, Point, ProcessedPointsL, ProcessedPoints, AllowedRight, NDepth, Left, 1),
    MutatedFormula=..[Relation,MutatedLeft,MutatedRight].


%------------------------------------------------------------------------
% replace_subformula_at_leaf_point_with_formula(FormulaA, SubFormulaB, Point, CrossoverFormula)
replace_subformula_at_leaf_point_with_formula(FormulaA, SubFormulaB, Point, CrossoverFormula):-
    replace_subformula_at_leaf_point_with_formula(FormulaA, SubFormulaB, Point, 0, _Processed, CrossoverFormula).


replace_subformula_at_leaf_point_with_formula(Formula, SubFormulaB, Point, Prev, CurPoint, CrossoverFormula):-
    Formula=..[Relation|_R],
    temporal_relation_list(TRels),
    \+member(Relation,TRels),
    CurPoint is Prev + 1,
    (
        (
            CurPoint=Point,!,
            CrossoverFormula=SubFormulaB
        );
        (
            CurPoint\=Point,
            CrossoverFormula=Formula
        )
    ).

replace_subformula_at_leaf_point_with_formula(Formula, SubFormulaB, Point, Prev, Processed, CrossoverFormula):-
    Formula=..[Relation,L,R],
    temporal_relation_list(TRels),
    member(Relation,TRels),
    replace_subformula_at_leaf_point_with_formula(L, SubFormulaB, Point, Prev, ProcessedPointsL, CrossoverFormulaL),
    replace_subformula_at_leaf_point_with_formula(R, SubFormulaB, Point, ProcessedPointsL, Processed, CrossoverFormulaR),
    CrossoverFormula=..[Relation,CrossoverFormulaL,CrossoverFormulaR].

%------------------------------------------------------------------------
% initialise_population([start(moored(Vessel,PB)),end(moored(Vessel,PA)),underway(Vessel)],[end(moored(Vessel,PA))],[start(moored(Vessel,PB))], 2, 4, 1000, Formulae), tournament(Formulae,[Vessel,PA,PB],trip(Vessel,PA,PB),10,Winner).
%
fitness(AllGIL,AllIL,Score):-
    find_tp_fp_fn(AllGIL,AllIL,TP,FP,FN),
    precision(TP,FP,PR),
    recall(TP,FN,RE),
    f1score(PR,RE,Score).

precision(0,0,0):-!.
precision(TP,FP,PR):- PR is TP/(TP+FP).

recall(0,0,0):-!.
recall(TP,FN,RE):- RE is TP/(TP+FN).

f1score(0,0,0):-!.
f1score(PR,RE,Score):- Score is 2*PR*RE/(PR+RE).

find_tp_fp_fn([],NAllIL,0,FP,0):-count_intervals(NAllIL,FP).
find_tp_fp_fn([(AV,GTI)|Rest],AllIL,TP,FP,FN):-
    (
        (member((AV,IL),AllIL),
         delete(AllIL,(AV,IL),NAllIL),
         intersection(IL,GTI,II),
         length(II,CTP),
         length(IL,NIL),
         length(GTI,NGIL),
         CFP is NIL-CTP,
         CFN is NGIL-CTP
        )
     ;
         (\+member((AV,_IL),AllIL),
             NAllIL=AllIL,
            CTP=0,CFP=0, length(GTI, CFN))
    ),
    find_tp_fp_fn(Rest,NAllIL,RestTP,RestFP,RestFN),
    TP is RestTP+CTP,
    FP is RestFP+CFP,
    FN is RestFN+CFN.

count_intervals([],0).
count_intervals([(_A,B)|R],C):-
    length(B,C1),
    count_intervals(R,C2),
    C is C1+C2.


%------------------------------------------------------------------------
% get_relation_point_type(Formula,Point,Left,Right,Allowed):-
% get the type and the allowed set for the relation at point 
%
get_relation_point_type(Formula,Point,Left,Right,Allowed):-
    get_relation_point_type(Formula,0,Point,_Processed,[ndinterval],0,0,Left,Right,Allowed).

get_relation_point_type(Formula,Prev,_Point,Prev,_PrevAllowed,_PLeft,_PRight,_Left,_Right,_Allowed):-
    Formula=..[Relation|_],
    temporal_relation_list(TRels),
    \+member(Relation,TRels).

get_relation_point_type(Formula,Prev,Point,Processed, PrevAllowed, PLeft, PRight, Left, Right, Allowed):-
    Formula=..[Relation,L,R],
    temporal_relation_list(TRels),
    member(Relation,TRels),
    Cur is Prev+1,
    (
        (
        Cur=Point,!,
        Processed = Cur,
        Allowed=PrevAllowed, PLeft=Left, PRight=Right
        );
        (
          Cur \= Point,
          allowed_formulae(Relation,AllowedLeft,AllowedRight),
          get_relation_point_type(L,Cur,Point,ProcessedLeft,AllowedLeft,1,PRight,Left,Right,Allowed),
          get_relation_point_type(R,ProcessedLeft,Point,Processed,AllowedRight,PLeft,1,Left,Right,Allowed)
        )
    ).

%------------------------------------------------------------------------
% get_leaf_point_type(Formula,Point,Left,Right,Allowed):-
% get the type and the allowed set for the relation at point 
%
get_leaf_point_type(Formula,Point,Left,Right,Allowed):-
    get_leaf_point_type(Formula,0,Point,_Processed,[ndinterval],0,0,Left,Right,Allowed).

get_leaf_point_type(Formula, Prev, Point, Processed, PrevAllowed, PLeft, PRight, Left, Right, Allowed):-
    Formula=..[Relation|_],
    temporal_relation_list(TRels),
    \+member(Relation,TRels),
    Cur is Prev+1,
    (
        (
            Cur=Point,!,
            Processed = Cur,
            Allowed=PrevAllowed, PLeft=Left, PRight=Right
        );
        (
            Cur \= Point,
            Processed = Cur,
            Allowed=_, Left=_, Right=_
        )
    ).

get_leaf_point_type(Formula,Prev,Point,Processed, _PrevAllowed, PLeft, PRight, Left, Right, Allowed):-
    Formula=..[Relation,L,R],
    temporal_relation_list(TRels),
    member(Relation,TRels),
    allowed_formulae(Relation,AllowedLeft,AllowedRight),
    get_leaf_point_type(L,Prev,Point,ProcessedLeft,AllowedLeft,1,PRight,Left,Right,Allowed),
    get_leaf_point_type(R,ProcessedLeft,Point,Processed,AllowedRight,PLeft,1,Left,Right,Allowed).



%------------------------------------------------------------------------
% get the number of relation nodes included in a formula
%
get_relation_number(Formula,0):-
    Formula =.. [OP|_],
    temporal_relation_list(TRels),
    \+member(OP,TRels),!.

get_relation_number(Formula,N):-
    Formula =.. [OP,L,R],
    temporal_relation_list(TRels),
    member(OP,TRels),
    get_relation_number(L,NL),
    get_relation_number(R,NR),
    N is NL + NR + 1.

get_leaf_number(Formula,1):-
    Formula =.. [OP|_],
    temporal_relation_list(TRels),
    \+member(OP,TRels),!.

get_leaf_number(Formula,N):-
    Formula =.. [OP,L,R],
    temporal_relation_list(TRels),
    member(OP,TRels),
    get_leaf_number(L,NL),
    get_leaf_number(R,NR),
    N is NL + NR.



%------------------------------------------------------------------------
%  get_relation_of_path_number(Formula,Left,Right,0)
%  returns the number of relation nodes that have type (left,right)

get_relation_of_path_number(Formula,CLeft,CRight,N):-
    get_relation_of_path_number(Formula,0,0,CLeft,CRight,N).

get_relation_of_path_number(Formula,_,_,_,_,0):-
    Formula=..[Relation|_],
    temporal_relation_list(TRels),
    \+member(Relation,TRels),!.

get_relation_of_path_number(Formula,Left,Right,CLeft,CRight,N):-
    Formula=..[Relation,L,R],
    temporal_relation_list(TRels),
    member(Relation,TRels),
    get_relation_of_path_number(L,1,Right,CLeft,CRight,N1),
    get_relation_of_path_number(R,Left,1,CLeft,CRight,N2),
    (((Left,Right)=(CLeft,CRight),N3=1,!);(N3=0)),
    N is N1+N2+N3.

%------------------------------------------------------------------------
% get_relation_of_path(Formula, Point, Left, Right,SubFormula).
% returns the relation node of type left right at position point

get_relation_of_path(Formula, Point, CLeft, CRight, SubFormula):-
    get_relation_of_path(Formula, Point, 0, 0, CLeft, CRight,  0, _Processed, SubFormula).

get_relation_of_path(Formula, _Point, _Left, _Right, _CLeft, _CRight, Prev, Prev, _SubFormula):-
    Formula=..[Relation|_],
    temporal_relation_list(TRels),
    \+member(Relation,TRels),!.

get_relation_of_path(Formula, Point, Left, Right, CLeft, CRight, Prev, Processed, SubFormula):-
    Formula=..[Relation,L,R],
    temporal_relation_list(TRels),
    member(Relation,TRels),
    (
        (
            (Left,Right)=(CLeft,CRight),!,
            Cur is Prev+1,
            (
                Cur=Point,!,
                Formula=SubFormula, Processed=Point
            );
            (
                Cur\=Point,
                get_relation_of_path(L, Point, 1, Right, CLeft, CRight, Cur, ProcessedLeft, SubFormula),
                get_relation_of_path(R, Point, Left, 1, CLeft, CRight, ProcessedLeft, Processed, SubFormula)
            )
        );
        (
                get_relation_of_path(L, Point, 1, Right, CLeft, CRight, Prev, ProcessedLeft, SubFormula),
                get_relation_of_path(R, Point, Left, 1, CLeft, CRight, ProcessedLeft, Processed, SubFormula)
        )
    ).


%----------------------------------------------------------------
%  get_leaf_of_path_and_type_number(Formula,Left,Right,Allowed,1)
% get number of leafs that have the specific path (left, right)
% and their type is included in allowed set
get_leaf_of_path_and_type_number(Formula,CLeft,CRight,Allowed,N):-
    get_leaf_of_path_and_type_number(Formula,0,0,CLeft,CRight,Allowed,N).

get_leaf_of_path_and_type_number(Formula,Left,Right,CLeft,CRight,Allowed,N):-
    Formula=..[Relation|_],
    temporal_relation_list(TRels),
    \+member(Relation,TRels),
    formula_type(Formula,Type),
    (
        (
            member(Type,Allowed),
            (Left,Right)=(CLeft,CRight),!,N=1
        );
        (
            N=0
        )
    ).

get_leaf_of_path_and_type_number(Formula,Left,Right,CLeft,CRight,Allowed,N):-
    Formula=..[Relation,L,R],
    temporal_relation_list(TRels),
    member(Relation,TRels),
    get_leaf_of_path_and_type_number(L,1,Right,CLeft,CRight,Allowed,N1),
    get_leaf_of_path_and_type_number(R,Left,1,CLeft,CRight,Allowed,N2),
    N is N1+N2.

%----------------------------------------------------------------
%  get_leaf_of_path_and_type(Formula,Left,Right,Allowed,1)
% get leaf with left,right and type at point
get_leaf_of_path_and_type(Formula, CLeft, CRight, Allowed, Point, SubFormula):-
    get_leaf_of_path_and_type(Formula, 0, 0, CLeft, CRight, Allowed, Point, 0, _Processed, SubFormula).

get_leaf_of_path_and_type(Formula, Left, Right, CLeft, CRight, Allowed, Point, Prev, Processed, SubFormula):-
    Formula=..[Relation|_],
    temporal_relation_list(TRels),
    \+member(Relation,TRels),
    formula_type(Formula,Type),
    (
        (
             (CLeft,CRight)=(Left,Right),
             member(Type,Allowed),!,
             Cur is Prev + 1,Processed=Cur,
             (
                (
                    Cur=Point,!,
                    SubFormula=Formula
                );
                (
                    Cur\=Point,
                    SubFormula=_
                )
             )
        );
        (
         Processed=Prev
        )
    ).

get_leaf_of_path_and_type(Formula,Left,Right,CLeft,CRight,Allowed,Point, Prev, Processed, SubFormula):-
    Formula=..[Relation,L,R],
    temporal_relation_list(TRels),
    member(Relation,TRels),
    get_leaf_of_path_and_type(L,1,Right,CLeft,CRight,Allowed, Point, Prev, ProcessedL, SubFormula),
    get_leaf_of_path_and_type(R,Left,1,CLeft,CRight,Allowed, Point, ProcessedL, Processed, SubFormula).


