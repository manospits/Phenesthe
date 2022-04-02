% Phenesthe is a system for the representation and processing of temporal phenomena.
%
% Copyright (C) 2021 Manolis Pitsikalis
%
% This program is free software: you can redistribute it and/or modify it under
% the terms of the GNU General Public License as published by the Free Software
% Foundation, either version 3 of the License, or (at your option) any later
% version.
%
% This program is distributed in the hope that it will be useful, but WITHOUT
% ANY WARRANTY; without even the implied warranty of  MERCHANTABILITY or FITNESS
% FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License along with
% this program.  If not, see <http://www.gnu.org/licenses/>.

:-use_module(library(lists)).
:-use_module(library(ordsets)).
:-set_prolog_flag(optimise, true).

:-dynamic dependencies/2,
          atemporal/1,
          input_phenomenon/2,
          user_phenomenon/2,
          phenomenon_conditions/2,
          phenomenon_transformed_conditions/3,
          input_event_instant/2,
          event_instants/2,
          input_state_interval/2,
          state_intervals/2,
          input_dynamic_phenomenon_interval/2,
          dynamic_phenomenon_intervals/2,
          dynamic_phenomenon_intervals_internal/2,
          retained_starting_formula/4,
          retained_iteration_formula_points/4,
          retained_iteration_formula_intervals/4,
          retained_tset_formula_intervals/5,
          retained_relation_formula_temp_info/5,
          level/2.

:-multifile input_phenomenon/2.
:-discontiguous atemporal_preprocess/3.

:-['./src/operators.prolog'].
:-['./src/utilities.prolog'].
:-['./src/filters.prolog'].
:-['./src/transformations.prolog'].
:-['./src/temporal_connectives.prolog'].
:-['./src/temporal_operators.prolog'].
:-['./src/temporal_relations.prolog'].
:-['./src/stream_processing.prolog'].
:-['./src/multithreading.prolog'].

% Multithreading is by default on.
:-phe_setval(multithreading,1).
:-phe_setval(preprocessing,0).

%Definitions loading/pre-processing/storing
term_expansion(input_phenomenon(Phenomenon,Type),input_phenomenon(Phenomenon,Type)).
term_expansion(:=(event_phenomenon(X),Y), phenomenon_conditions(X,Y)):-assertz(user_phenomenon(X,event)).
term_expansion(:=(state_phenomenon(X),Y), phenomenon_conditions(X,Y)):-assertz(user_phenomenon(X,state)).
term_expansion(:=(dynamic_phenomenon(X),Y), phenomenon_conditions(X,Y)):-assertz(user_phenomenon(X,dynamic_phenomenon)).

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
        assertz(dependencies(X,D))
        )
    ,_),
    phe_setval(formula_id,0),
    findall(_,(
        phenomenon_type(X,_,PType),
        preprocess_phenomenon_definition(X,PType),
        compute_topological_place(X,P),
        assertz(level(X,P))
    ),_),
    findall(_,(
        phenomenon_type(X,_,user),
        \+(phenomenon_transformed_conditions(X,_C,_T)),
        format('ERROR: Definition transformation of phenomenon ~w failed',[X])
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

% filter op
find_dependencies(Condition, D):-
    Condition=..[filter,Expr,_],!,
    find_dependencies(Expr,D).

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
    assertz(phenomenon_transformed_conditions(X,P,TI)).


%%%
%%% Recognition (processing) of an entity
%%%
recognition_query(WindowSize,Step,Tq):-
    Tcrit is Tq+Step-WindowSize,
    Tqmw is Tq-WindowSize,
    Tqmws is Tq-WindowSize-Step,
    discard_redundant(Tqmw,Tqmws),
    Tqmw1 is Tqmw+1,
    create_window_instants(Tqmw1,Tq,WindowInstants),
    phe_setval(tcrit,Tcrit),
    phe_setval(tqmw,Tqmw),
    phe_setval(tq,Tq),
    phe_setval(current_window_instants,WindowInstants),
    findall(process_phenomenon(X), phenomenon_type(X,_,user), Phenomena),
    (phe_getval(multithreading, 1) ->
        dependency_aware_parallel_execution(Phenomena,[])
        ;
        process_level(1,WindowSize,Step,Tq)
    ).

process_level(Level,_W,_S,_Tq):-
    \+level(_,Level),!.

process_level(Level, WindowSize, Step, Tq):-
    level(_,Level),!,
    findall(_,(level(X,Level),process_phenomenon(X)),_),
    NextLevel is Level + 1,
    process_level(NextLevel, WindowSize, Step, Tq).


discard_redundant(Tqmw,Tqmws):-
    %retract all recognised
    forall(phenomenon_type(X,event,user),retractall(event_instants(X,_))),
    forall(phenomenon_type(X,state,user),retractall(state_intervals(X,_))),
    forall(phenomenon_type(X,dynamic_phenomenon,user),retractall(dynamic_phenomenon_intervals_internal(X,_))),
    %retract input entities
    forall((phenomenon_type(X,event,input),
            input_event_instant(X,T),
            T=<Tqmw),
           retract(input_event_instant(X,T))),
    %retract input states
    forall((phenomenon_type(X,state,input),
        input_state_interval(X,[Ts,Te]),
        Te=<Tqmw),
        retract(input_state_interval(X,[Ts,Te]))),
    %retract dynamic phenomena intervals
    forall((phenomenon_type(X,dynamic_phenomenon,input),
        input_dynamic_phenomenon_interval(X,[Ts,Te]),
        Te=<Tqmw),
        retract(input_dynamic_phenomenon_interval(X,[Ts,Te]))),
    %retract old retained information
    retractall(retained_starting_formula(_,_,_,Tqmws)),
    retractall(retained_iteration_formula_points(_,_,_,Tqmws)),
    retractall(retained_iteration_formula_intervals(_,_,_,Tqmws)),
    retractall(retained_tset_formula_intervals(_,_,_,Tqmws,_)),
    retractall(retained_relation_formula_temp_info(_,_,_,Tqmws,_)).


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
     %thread_self(ID),
     %format('Thread ~w asserts ~w\n',[ID,event_instants(Phenomenon,InstantList)]),
     assert_if_not_exists(event_instants(Phenomenon,InstantList))
    ),_),!.

process_state(Phenomenon):-
    term_variables(Phenomenon, Variables),
    phenomenon_transformed_conditions(Phenomenon,ProcessedFormula,IL),
    findall((Variables,IL),
        (
        ProcessedFormula,
        IL\=[],
        assert_if_not_exists(state_intervals(Phenomenon,IL))
        )
    ,_).

process_dynamic_phenomenon(Phenomenon):-
    term_variables(Phenomenon, Variables),
    phenomenon_transformed_conditions(Phenomenon,ProcessedFormula,IL),
    findall((Variables,IL),
        (
        ProcessedFormula,
        IL\=[],
        assert_if_not_exists(dynamic_phenomenon_intervals_internal(Phenomenon,IL))
        )
    ,_).

dynamic_phenomenon_intervals(Phenomenon,IL):-
    phenomenon_type(Phenomenon,dynamic_phenomenon,user),
    dynamic_phenomenon_intervals_internal(Phenomenon,IIL),
    clean_from_unk(IIL,IL),IL\=[].
