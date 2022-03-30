:-begin_tests(states).
:-['../../phenesthe.prolog'].
:-['../definitions.prolog'].
:-['./narrative.prolog'].

:-preprocess_phenomena_definitions.

test(query0):-
    assert_input_phenomena(5),
    recognition_query(5,5,5).

%maximal interval operator depends on input events
test(case0):-
    findall(L,state_intervals(possess(bob,torch),L),[[[1,5]]]),
    findall(L,state_intervals(possess(bob,coin),L),[[[3,inf]]]),
    findall(L,state_intervals(possess(alice,wallet),L),[[[4,inf]]]).

%maximal interval operator depends on user events
test(case1):-
    findall(L,state_intervals(happy(bob),L),[[[1,5]]]),
    findall(L,state_intervals(happy(alice),L),[[[4,inf]]]),
    findall(L,state_intervals(sad(bob),L),[[[5,inf]]]).

%union
test(case2):-
    findall(L,state_intervals(happy_or_angry(bob),L),[[[1,5]]]),
    findall(L,state_intervals(happy_or_angry(alice),L),[[[1,2],[4,inf]]]).

%intersection-
test(case3):-
    findall(L,state_intervals(happy_with_money(bob),L),[]),
    findall(L,state_intervals(happy_with_money(alice),L),[[[4,inf]]]).

%iteration
test(case4):-
    findall(L,state_intervals(keep_dropping(bob),L),[[[3,5]]]).

test(query1):-
    assert_input_phenomena(10),
    recognition_query(5,5,10),
    findall(_,(input_event_instant(_,T),T=<5),[]),
    findall(_,(input_state_interval(_,[_,Te]),Te=<5),[]).

%iteration
test(case5):-
    findall(L,state_intervals(keep_dropping(bob),L),[[[3,8]]]).


test(case6):-
    findall(L,state_intervals(possess(bob,torch),L),[]),
    findall(L,state_intervals(possess(bob,coin),L),[[[3,8]]]),
    findall(L,state_intervals(possess(bob,wallet),L),[[[6,inf]]]),
    findall(L,state_intervals(possess(alice,wallet),L),[[[4,9]]]),
    findall(L,state_intervals(possess(alice,flower),L),[[[6,inf]]]),
    findall(L,state_intervals(possess(alice,coin),L),[[[9,inf]]]),
    findall(L,state_intervals(possess(john, wallet),L),[[[10,inf]]]).

test(case7):-
    findall(L,state_intervals(happy(bob),L),[[[6,8]]]),
    findall(L,state_intervals(happy(alice),L),[[[4,inf]]]),
    findall(L,state_intervals(happy(john),L),[[[10,inf]]]).

test(case8):-
    findall(L,state_intervals(sad(bob),L),[[[5,6],[8,inf]]]),
    findall(L,state_intervals(sad(alice),L),[]).

test(case9):-
    findall(L,state_intervals(happy_with_money(bob),L),[[[6,8]]]),
    findall(L,state_intervals(happy_with_money(alice),L),[[[4,9]]]),
    findall(L,state_intervals(happy_with_money(john),L),[[[10,inf]]]).

test(case10):-
    findall(L,state_intervals(happy_without_money(bob),L),[]),
    findall(L,state_intervals(happy_without_money(alice),L),[[[9,inf]]]).

test(case11):-
    findall(L,state_intervals(can_eat(bob),L),[[[6,inf]]]).

test(case12):-
    findall(L,state_intervals(hungry_and_angry(bob),L),[[[6,7],[9,10]]]).

:-end_tests(states).
