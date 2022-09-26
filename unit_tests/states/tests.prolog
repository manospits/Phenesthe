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
    assertion(findall(L,state_intervals(possess(bob,torch),L),[[[1,5]]])),
    assertion(findall(L,state_intervals(possess(bob,coin),L),[[[3,inf]]])),
    assertion(findall(L,state_intervals(possess(alice,wallet),L),[[[4,inf]]])).

%maximal interval operator depends on user events
test(case1):-
    assertion(findall(L,state_intervals(happy(bob),L),[[[1,5]]])),
    assertion(findall(L,state_intervals(happy(alice),L),[[[4,inf]]])),
    assertion(findall(L,state_intervals(sad(bob),L),[[[5,inf]]])).

%union
test(case2):-
    assertion(findall(L,state_intervals(happy_or_angry(bob),L),[[[1,5]]])),
    assertion(findall(L,state_intervals(happy_or_angry(alice),L),[[[1,2],[4,inf]]])).

%intersection-
test(case3):-
    assertion(findall(L,state_intervals(happy_with_money(bob),L),[])),
    assertion(findall(L,state_intervals(happy_with_money(alice),L),[[[4,inf]]])).

%iteration <
test(case4):-
    assertion(findall(L,state_intervals(keep_dropping(bob),L),[[[3,5]]])).

%iteration =
test(case5):-
    assertion(findall(L,state_intervals(regular_stare(bob,alice),L),[[[1,5]]])).

%iteration <=
test(case6):-
    assertion(findall(L,state_intervals(not_so_regular_stare(alice,bob),L),[])),
    assertion(findall(L,state_intervals(not_so_regular_stare(bob,alice),L),[[[1,5]]])).

test(query1):-
    assert_input_phenomena(10),
    recognition_query(5,5,10),
    assertion(findall(_,(input_event_instant(_,T),T=<5),[])),
    assertion(findall(_,(input_state_interval(_,[_,Te]),Te=<5),[])).

%iteration
test(case7):-
    assertion(findall(L,state_intervals(keep_dropping(bob),L),[[[3,8]]])).


test(case8):-
    assertion(findall(L,state_intervals(possess(bob,torch),L),[])),
    assertion(findall(L,state_intervals(possess(bob,coin),L),[[[3,8]]])),
    assertion(findall(L,state_intervals(possess(bob,wallet),L),[[[6,inf]]])),
    assertion(findall(L,state_intervals(possess(alice,wallet),L),[[[4,9]]])),
    assertion(findall(L,state_intervals(possess(alice,flower),L),[[[6,inf]]])),
    assertion(findall(L,state_intervals(possess(alice,coin),L),[[[9,inf]]])),
    assertion(findall(L,state_intervals(possess(john, wallet),L),[[[10,inf]]])).

test(case9):-
    assertion(findall(L,state_intervals(happy(bob),L),[[[6,8]]])),
    assertion(findall(L,state_intervals(happy(alice),L),[[[4,inf]]])),
    assertion(findall(L,state_intervals(happy(john),L),[[[10,inf]]])).

test(case10):-
    assertion(findall(L,state_intervals(sad(bob),L),[[[5,6],[8,inf]]])),
    assertion(findall(L,state_intervals(sad(alice),L),[])).

test(case11):-
    assertion(findall(L,state_intervals(happy_with_money(bob),L),[[[6,8]]])),
    assertion(findall(L,state_intervals(happy_with_money(alice),L),[[[4,9]]])),
    assertion(findall(L,state_intervals(happy_with_money(john),L),[[[10,inf]]])).

test(case12):-
    assertion(findall(L,state_intervals(happy_without_money(bob),L),[])),
    assertion(findall(L,state_intervals(happy_without_money(alice),L),[[[10,inf]]])).

test(case13):-
    assertion(findall(L,state_intervals(can_eat(bob),L),[[[6,inf]]])).

test(case14):-
    assertion(findall(L,state_intervals(hungry_and_angry(bob),L),[[[6,7],[9,10]]])).

%iteration =
test(case15):-
    assertion(findall(L,state_intervals(regular_stare(bob,alice),L),[[[1,9]]])).

%iteration =
test(case16):-
    assertion(findall(L,state_intervals(not_so_regular_stare(bob,alice),L),[[[1,9]]])),
    assertion(findall(L,state_intervals(not_so_regular_stare(alice,bob),L),[[[1,10]]])).

:-end_tests(states).
