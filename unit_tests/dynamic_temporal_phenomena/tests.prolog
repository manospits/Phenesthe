:-begin_tests(dynamic_temporal_phenomena).
:-['../../phenesthe.prolog'].
:-['../definitions.prolog'].
:-['./narrative.prolog'].

:-preprocess_phenomena_definitions.

test(query0):-
    assert_input_phenomena(5),
    recognition_query(0,5,5).

% contains d-t
test(case1):-
    phe_getval(tq,Tq),
    atom_concat(Tq,'+',Tqp),
    assertion(findall(L,dynamic_phenomenon_intervals(drops_objects_when_hungry(bob),L),[[([1,Tqp],t)]])).

% starts
test(case2):-
    assertion(findall(L,dynamic_phenomenon_intervals(hunger_starts_hungry(bob),L),Z1)),
    assertion(findall(L,state_intervals(hungry(bob),L),Z2)),
    Z1=Z2.

% ends
test(case3):-
    assertion(findall(L,dynamic_phenomenon_intervals(ate_ends_hungry(_X),L),[])).

test(query1):-
    assert_input_phenomena(10),
    recognition_query(0,5,10).

% contains d-t window
test(case4):-
    phe_getval(tq,Tq),
    atom_concat(Tq,'+',Tqp),
    assertion(findall(L,dynamic_phenomenon_intervals(drops_objects_when_hungry(bob),L),[[([1,8],t)]])),
    assertion(findall(L,dynamic_phenomenon_intervals(drops_objects_when_hungry(alice),L),[[([1,Tqp],t)]])).

% meets d-d
test(case5):-
    phe_getval(tq,Tq),
    atom_concat(Tq,'+',Tqp),
    assertion(findall(L,dynamic_phenomenon_intervals(happy_then_sad(bob),L),[[([6,10],t)]])),
    assertion(findall(L,dynamic_phenomenon_intervals(sad_then_happy(bob),L),[[([1,8],t),([8,Tqp],t)]])).

%starts
test(case6):-
    assertion(findall(L,dynamic_phenomenon_intervals(hunger_starts_hungry(bob),L),Z1)),
    assertion(findall(L,state_intervals(hungry(bob),L),Z2)),
    Z1=Z2.

% ends
test(case7):-
    assertion(findall(L,dynamic_phenomenon_intervals(ate_ends_hungry(bob),L),[[([1,8],t)]])).

% equals & intersection
test(case8):-
    assertion(findall(L, dynamic_phenomenon_intervals(happy_equals_feels_inter_happy(bob),L),[[([6,8],t)]])).

%overlaps
test(case9):-
    phe_getval(tq,Tq),
    atom_concat(Tq,'+',Tqp),
    assertion(findall(L, dynamic_phenomenon_intervals(possess_overlaps(bob,coin,wallet),L),[[([3,Tqp],t)]])),
    assertion(findall(L, dynamic_phenomenon_intervals(possess_overlaps(alice,wallet,flower),L),[[([4,Tqp],t)]])).


test(query2):-
    assert_input_phenomena(15),
    recognition_query(0,5,15).

% meets d-d window
test(case10):-
    assertion(findall(L,dynamic_phenomenon_intervals(sad_then_happy(bob),L),[[([8,15],t)]])).

% before d-d (retained from last window)
test(case11):-
    phe_getval(tq,Tq),
    atom_concat(Tq,'+',Tqp),
    assertion(findall(L,dynamic_phenomenon_intervals(sad_before_hungry(bob),L),[[([8,Tqp],t)]])).

% contains d-t
test(case12):-
    assertion(findall(L,dynamic_phenomenon_intervals(drops_objects_when_hungry(alice),L),[[([1,14],t)]])).

%starts
test(case13):-
    assertion(findall(L,dynamic_phenomenon_intervals(hunger_starts_hungry(bob),L),Z1)),
    assertion(findall(L,state_intervals(hungry(bob),L),Z2)),
    Z1=Z2.

test(case14):-
    assertion(findall(L,dynamic_phenomenon_intervals(ate_ends_hungry(alice),L),[[([1,14],t)]])),
    assertion(findall(L,dynamic_phenomenon_intervals(ate_ends_hungry(bob),L),[])).

:-end_tests(dynamic_temporal_phenomena).
