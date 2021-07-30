:-begin_tests(events).
:-['../../loader.prolog'].
:-['../definitions.prolog'].
:-['./narrative.prolog'].

:-preprocess_phenomena_definitions.

test(query0):-
    assert_input_phenomena(5),
    recognition_query(5,5,5).

%simple or case
test(case0):-
    findall(_,event_instants(action(bob),[1,3]),[_]),
    findall(_,event_instants(action(alice),[4]),[_]).

%simple and case no negation
test(case1):-
    event_instants(unchanged(bob),[3]),
    \+(event_instants(unchanged(alice),_)).

%non negated phenomenon w/ in conjunction with negated one
test(case2):-
   findall(_,event_instants(gain(bob),[1]),[_]),
   findall(_,event_instants(gain(alice),[4]),[_]),
   \+(event_instants(loss(bob),_)),
   \+(event_instants(loss(alice),_)).

%conjunction of two negated phenomena
test(case3):-
  findall(_,event_instants(no_action(bob),[2,4,5]),[_]),
  findall(_,event_instants(no_action(alice),[1,2,3,5]),[_]).

%check discarding mechanism
test(query1):-
    assert_input_phenomena(10),
    recognition_query(5,5,10),
    findall(_,(event_instant(_,T),T=<5),[]).

%same as case0
test(case4):-
    findall(_,event_instants(action(bob),[8]),[_]),
    findall(_,event_instants(action(alice),[6,9]),[_]).

%simple and case no negation
test(case5):-
    \+(event_instants(unchanged(bob),_)),
    \+(event_instants(unchanged(alice),_)).

%non negated phenomenon w/ in conjunction with negated one
test(case6):-
   findall(_,event_instants(loss(bob),[8]),[_]),
   findall(_,event_instants(loss(alice),[9]),[_]),
   findall(_,event_instants(gain(alice),[6]),[_]),
   \+(event_instants(gain(bob),_)).

test(case8):-
    findall(_,event_instants(ate_and_dropped(alice),[9]),[_]).
%conjunction of two negated phenomena
test(case7):-
   findall(_,event_instants(no_action(bob),[6,7,9,10]),[_]),
   findall(_,event_instants(no_action(alice),[7,8,10]),[_]).

:-end_tests(events).
