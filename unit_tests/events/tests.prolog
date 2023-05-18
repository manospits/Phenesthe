:-begin_tests(events).
:-['../../phenesthe.prolog'].
:-['../definitions.prolog'].
:-['./narrative.prolog'].

:-preprocess_phenomena_definitions.

test(query0):-
    assert_input_phenomena(5),
    recognition_query(5,5,5).

%test(case_debug1):-
    %nl,
    %findall(_,(event_instants(X,T),writeln(event_instants(X,T))),_).

%simple or case
test(case0):-
    findall(I,event_instants(action(bob),I),A),
    assertion(A==[[(1,t),(3,t)]]),
    findall(I,event_instants(action(alice),I),B),
    assertion(B==[[(4,t)]]).

%simple and case no negation
test(case1):-
    assertion(findall(_,event_instants(unchanged(bob),[(3,t)]),[_])),
    \+(event_instants(unchanged(alice),_)).

%non negated phenomenon w/ in conjunction with negated one
test(case2):-
   assertion(findall(_,event_instants(gain(bob),[(1,t)]),[_])),
   assertion(findall(_,event_instants(gain(alice),[(4,t)]),[_])),
   \+(event_instants(loss(bob),_)),
   \+(event_instants(loss(alice),_)).

%conjunction of two negated phenomena
test(case3):-
  assertion(findall(_,event_instants(no_action(bob),[(2,t),(4,t),(5,t)]),[_])),
  assertion(findall(_,event_instants(no_action(alice),[(1,t),(2,t),(3,t),(5,t)]),[_])).

%in connective
test(case4):-
  assertion(findall(_,event_instants(ate_while_happy(bob,food),[(3,t)]),[_])).

%check discarding mechanism
test(query1):-
    assert_input_phenomena(10),
    recognition_query(5,5,10),
    assertion(findall(_,(input_event_instant(_,T),T=<5),[])).

%same as case0
test(case5):-
    assertion(findall(_,event_instants(action(bob),[(8,t)]),[_])),
    assertion(findall(_,event_instants(action(alice),[(6,t),(9,t)]),[_])).

%simple and case no negation
test(case6):-
    \+(event_instants(unchanged(bob),_)),
    \+(event_instants(unchanged(alice),_)).

%non negated phenomenon w/ in conjunction with negated one
test(case7):-
   assertion(findall(_,event_instants(loss(bob),[(8,t)]),[_])),
   assertion(findall(_,event_instants(loss(alice),[(9,t)]),[_])),
   assertion(findall(_,event_instants(gain(alice),[(6,t)]),[_])),
   \+(event_instants(gain(bob),_)).

test(case8):-
    assertion(findall(_,event_instants(ate_and_dropped(alice),[(9,t)]),[_])).
%conjunction of two negated phenomena
test(case9):-
   assertion(findall(_,event_instants(no_action(bob),[(6,t),(7,t),(9,t),(10,t)]),[_])),
   assertion(findall(_,event_instants(no_action(alice),[(7,t),(8,t),(10,t)]),[_])).

test(case10):-
   assertion(findall(_,event_instants(ate_while_happy(alice,bread),[(9,t)]),[_])).
%test(case_debug2):-
    %assertion(findall(_,(event_instants(X,T),writeln(event_instants(X,T))),_)).

:-end_tests(events).
