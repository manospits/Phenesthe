:-use_module(library(lists)).
:-use_module(library(ordsets)).

:-dynamic dependencies/2,
          atemporal/1,
          input_phenomenon/2,
          user_phenomenon/2,
          phenomenon_conditions/2,
          phenomenon_transformed_conditions/3,
          event_instant/2,
          event_instants/2,
          state_intervals/2,
          retained_starting_formula/4,
          retained_tset_formula_intervals/5,
          retained_relation_formula_intervals/5,
          dynamic_phenomenon_intervals_internal/2,
          level/2.

:-multifile input_phenomenon/2.

:-['./src/operators.prolog'].
:-['./src/utilities.prolog'].
:-['./src/transformations.prolog'].
:-['./src/temporal_operators.prolog'].
:-['./src/temporal_relations.prolog'].

%Definitions loading/pre-processing/storing 
term_expansion(input_phenomenon(Phenomenon,Type),input_phenomenon(Phenomenon,Type)).
term_expansion(:=(event_phenomenon(X),Y), phenomenon_conditions(X,Y)):-assert(user_phenomenon(X,event)).
term_expansion(:=(state_phenomenon(X),Y), phenomenon_conditions(X,Y)):-assert(user_phenomenon(X,state)).
term_expansion(:=(dynamic_phenomenon(X),Y), phenomenon_conditions(X,Y)):-assert(user_phenomenon(X,dynamic_phenomenon)).

% Predicate type handling
% event state dynamic phenomenon predicates
% input or user defined
phenomenon_type(X,PType,DType):-
    user_phenomenon(X,PType), DType = user ; input_phenomenon(X,PType), DType = input.

% Initialisations
% Load the definitions in memory
% Find and assert dependencies
% Find evaluation order
preprocess_phenomena_definitions:-
    findall(_,(
        phenomenon_type(X,_,_),
        phenomenon_conditions(X,Conditions),
        find_dependencies(Conditions,D),
        assert(dependencies(X,D))
        )
    ,_),
    my_setval(formula_id,0),
    findall(_,(
        phenomenon_type(X,_,PType),
        preprocess_phenomenon_definition(X,PType),
        compute_topological_place(X,P),
        assert(level(X,P))
    ),_).

dependencies(X,[]):-
    input_phenomenon(X,_).

% operators with lr side
find_dependencies(Condition,D):-
    Condition=..[OP,A,B],
    op_list(OP_list),
    member(OP,OP_list),!,
    find_dependencies(A,AD),
    find_dependencies(B,BD),
    append(AD,BD,D).

% start end ops or tnot
find_dependencies(Condition,D):-
    Condition=..[OP,StateExpr],
    member(OP,[start,end,tnot]),!,
    find_dependencies(StateExpr,D).

% reached a phenomenon
find_dependencies(Condition,[Condition]):-
    phenomenon_type(Condition,_,_),!.

% atemporal
find_dependencies(Condition,[]):-
    Condition=..[OP|_],
    op_list(OP_list),
    \+member(OP,OP_list),!.

compute_topological_place(X,P):-
    dependencies(X,D),D\=[],
    findall(PE,(
        member(E,D),
        compute_topological_place(E,PE)),
        DPE),
    mmax_member(P1,DPE),
    P is P1+1,!.

compute_topological_place(X,0):-
    dependencies(X,[]).

preprocess_phenomenon_definition(_,input).
preprocess_phenomenon_definition(X,user):-
    transform_formula(X,P,TI),
    assert(phenomenon_transformed_conditions(X,P,TI)).


%%%
%%% Recognition (processing) of an entity
%%%
recognition_query(WindowSize,Step,Tq):-
    Tcrit is Tq+Step-WindowSize,
    Tqmw is Tq-WindowSize,
    discard_redundant(Tqmw),
    Tqmw1 is Tqmw+1,
    create_window_instants(Tqmw1,Tq,WindowInstants),
    my_setval(tcrit,Tcrit),
    my_setval(tqmw,Tqmw),
    my_setval(tq,Tq),
    my_setval(current_window_instants,WindowInstants),
    process_level(1,WindowSize,Step,Tq).
    %export results
    %discard_redundant.

process_level(Level,_W,_S,_Tq):-
    \+level(_,Level),!.

process_level(Level, WindowSize, Step, Tq):-
    level(_,Level),!,
    %findall(_,(level(X,Level),process_phenomenon(X)),_),
    findall(X,(level(X,Level)),Phenomena),
    concurrent_maplist(process_phenomenon,Phenomena),
    NextLevel is Level + 1,
    process_level(NextLevel, WindowSize, Step, Tq).


discard_redundant(TsCurrentW):-
    %retract all recognised
    findall(_,(
        phenomenon_type(X,event,user),
        retractall(event_instants(X,_))
        ),_),
    findall(_,(
        phenomenon_type(X,state,user),
        retractall(state_intervals(X,_))
        ),_),
    findall(_,(
        phenomenon_type(X,dynamic_phenomenon,user),
        retractall(dynamic_phenomenon_intervals_internal(X,_))
        ),_),
    %retract input entities
    findall(_,(
        phenomenon_type(X,event,input),
        event_instant(X,T),
        T=<TsCurrentW,
        retract((event_instant(X,T)))),
        _),
    %retract input states
    findall(_,(
        phenomenon_type(X,state,input),
        state_intervals(X,IL),
        remaining(IL,TsCurrentW,ILR),
        retract((state_intervals(X,IL))),
        assert((state_intervals(X,ILR)))
    ),_),
    %retract dynamic phenomena intervals
    findall(_,(
        retained_starting_formula(A,B,C,T),
        T\=TsCurrentW,
        retract(retained_starting_formula(A,B,C,T))
    ),_),
    findall(_,(
        retained_tset_formula_intervals(A,B,C,T,D),
        T\=TsCurrentW,
        retract(retained_tset_formula_intervals(A,B,C,T,D))
    ),_),
findall(_,(
        retained_relation_formula_intervals(A,B,C,T,D),
        T\=TsCurrentW,
        retract(retained_relation_formula_intervals(A,B,C,T,D))
    ),_)
    .

remaining([],_,[]).
remaining([[TS,TE]|IL],T,[[TS,TE]|RIL]):-
    TE > T,
    remaining(IL,T,RIL).
remaining([[_TS,TE]|IL],T,RIL):-
    TE =< T,
    remaining(IL,T,RIL).



process_phenomenon(Phenomenon):-
    phenomenon_type(Phenomenon,PType,_),
    ((
        PType='event',!,
        process_event(Phenomenon)
    );
    (
        PType='state',!,
        process_state(Phenomenon)
    );
    (
        PType='dynamic_phenomenon',!,
        process_dynamic_phenomenon(Phenomenon)
    )).

process_event(Phenomenon):-
    %find valid groundings
    phenomenon_transformed_conditions(Phenomenon,ProcessedFormula,InstantList),
    term_variables(Phenomenon, Variables),
    findall((Variables,InstantList),
    (ProcessedFormula,
     ground(Phenomenon),
     InstantList\=[],
     assert_if_not_exists(event_instants(Phenomenon,InstantList))
    ),_),!.

process_state(Phenomenon):-
    term_variables(Phenomenon, Variables),
    phenomenon_transformed_conditions(Phenomenon,ProcessedFormula,IL),
    findall((Variables,IL),
        (
        ProcessedFormula,
        IL\=[],
        assert(state_intervals(Phenomenon,IL))
        )
    ,_).

process_dynamic_phenomenon(Phenomenon):-
    term_variables(Phenomenon, Variables),
    phenomenon_transformed_conditions(Phenomenon,ProcessedFormula,IL),
    findall((Variables,IL),
        (
        ProcessedFormula,
        IL\=[],
        assert(dynamic_phenomenon_intervals_internal(Phenomenon,IL))
        )
    ,_).

dynamic_phenomenon_intervals(Phenomenon,IL):-
    dynamic_phenomenon_intervals_internal(Phenomenon,IIL),
    clean_from_unk(IIL,IL),IL\=[].
