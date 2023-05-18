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
    phe_getval(tq,Tq),
    atom_concat(Tq,'+',Tqp),
    assertion(findall(L,state_intervals(possess(bob,torch),L),[[([1,5],t)]])),
    assertion(findall(L,state_intervals(possess(bob,coin),L),[[([3,Tqp],t)]])),
    assertion(findall(L,state_intervals(possess(alice,wallet),L),[[([4,Tqp],t)]])).

%maximal interval operator depends on user events
test(case1):-
    phe_getval(tq,Tq),
    atom_concat(Tq,'+',Tqp),
    assertion(findall(L,state_intervals(happy(bob),L),[[([1,5],t)]])),
    assertion(findall(L,state_intervals(happy(alice),L),[[([4,Tqp],t)]])),
    assertion(findall(L,state_intervals(sad(bob),L),[[([5,Tqp],t)]])).

%union
test(case2):-
    phe_getval(tq,Tq),
    atom_concat(Tq,'+',Tqp),
    assertion(findall(L,state_intervals(happy_or_angry(bob),L),[[([1,5],t)]])),
    assertion(findall(L,state_intervals(happy_or_angry(alice),L),[[([1,2],t),([4,Tqp],t)]])).

%intersection-
test(case3):-
    phe_getval(tq,Tq),
    atom_concat(Tq,'+',Tqp),
    assertion(findall(L,state_intervals(happy_with_money(bob),L),[])),
    assertion(findall(L,state_intervals(happy_with_money(alice),L),[[([4,Tqp],t)]])).


test(query1):-
    assert_input_phenomena(10),
    recognition_query(5,5,10),
    assertion(findall(_,(input_event_instant(_,T),T=<5),[])),
    assertion(findall(_,(input_state_interval(_,[_,Te]),Te=<5),[])).


test(case8):-
    phe_getval(tq,Tq),
    atom_concat(Tq,'+',Tqp),
    assertion(findall(L,state_intervals(possess(bob,torch),L),[])),
    assertion(findall(L,state_intervals(possess(bob,coin),L),[[([3,8],t)]])),
    assertion(findall(L,state_intervals(possess(bob,wallet),L),[[([6,Tqp],t)]])),
    assertion(findall(L,state_intervals(possess(alice,wallet),L),[[([4,9],t)]])),
    assertion(findall(L,state_intervals(possess(alice,flower),L),[[([6,Tqp],t)]])),
    assertion(findall(L,state_intervals(possess(alice,coin),L),[[([9,Tqp],t)]])),
    assertion(findall(L,state_intervals(possess(john, wallet),L),[[([10,Tqp],t)]])).

test(case9):-
    phe_getval(tq,Tq),
    atom_concat(Tq,'+',Tqp),
    assertion(findall(L,state_intervals(happy(bob),L),[[([6,8],t)]])),
    assertion(findall(L,state_intervals(happy(alice),L),[[([4,Tqp],t)]])),
    assertion(findall(L,state_intervals(happy(john),L),[[([10,Tqp],t)]])).

test(case10):-
    phe_getval(tq,Tq),
    atom_concat(Tq,'+',Tqp),
    assertion(findall(L,state_intervals(sad(bob),L),[[([5,6],t),([8,Tqp],t)]])),
    assertion(findall(L,state_intervals(sad(alice),L),[])).

test(case11):-
    phe_getval(tq,Tq),
    atom_concat(Tq,'+',Tqp),
    assertion(findall(L,state_intervals(happy_with_money(bob),L),[[([6,8],t)]])),
    assertion(findall(L,state_intervals(happy_with_money(alice),L),[[([4,9],t)]])),
    assertion(findall(L,state_intervals(happy_with_money(john),L),[[([10,Tqp],t)]])).

test(case12):-
    phe_getval(tq,Tq),
    atom_concat(Tq,'+',Tqp),
    assertion(findall(L,state_intervals(happy_without_money(bob),L),[])),
    assertion(findall(L,state_intervals(happy_without_money(alice),L),[[([10,Tqp],t)]])).

test(case13):-
    phe_getval(tq,Tq),
    atom_concat(Tq,'+',Tqp),
    assertion(findall(L,state_intervals(can_eat(bob),L),[[([6,Tqp],t)]])).

test(case14):-
    assertion(findall(L,state_intervals(hungry_and_angry(bob),L),[[([6,7],t),([9,10],t)]])).

:-end_tests(states).
