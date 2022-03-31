:-begin_tests(dynamic_temporal_phenomena).
:-['../../phenesthe.prolog'].
:-['../definitions.prolog'].
:-['./narrative.prolog'].

:-preprocess_phenomena_definitions.

test(query0):-
    assert_input_phenomena(5),
    recognition_query(5,5,5).

% contains d-t
test(case1):-
    assertion(findall(L,dynamic_phenomenon_intervals(drops_objects_when_hungry(bob),L),[[[1,inf]]])).

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
    recognition_query(5,5,10).

% contains d-t window
test(case4):-
    assertion(findall(L,dynamic_phenomenon_intervals(drops_objects_when_hungry(bob),L),[[[1,8]]])),
    assertion(findall(L,dynamic_phenomenon_intervals(drops_objects_when_hungry(alice),L),[[[1,inf]]])).

% meets d-d
test(case5):-
    assertion(findall(L,dynamic_phenomenon_intervals(happy_then_sad(bob),L),[[[6,10]]])),
    assertion(findall(L,dynamic_phenomenon_intervals(sad_then_happy(bob),L),[[[1,8],[8,inf]]])).

%starts
test(case6):-
    assertion(findall(L,dynamic_phenomenon_intervals(hunger_starts_hungry(bob),L),Z1)),
    assertion(findall(L,state_intervals(hungry(bob),L),Z2)),
    Z1=Z2.

% ends
test(case7):-
    assertion(findall(L,dynamic_phenomenon_intervals(ate_ends_hungry(bob),L),[[[1,8]]])).

% equals & intersection
test(case8):-
    assertion(findall(L, dynamic_phenomenon_intervals(happy_equals_feels_inter_happy(bob),L),[[[6,8]]])).

%overlaps
test(case9):-
    assertion(findall(L, dynamic_phenomenon_intervals(possess_overlaps(bob,coin,wallet),L),[[[3,inf]]])),
    assertion(findall(L, dynamic_phenomenon_intervals(possess_overlaps(alice,wallet,flower),L),[[[4,inf]]])).


test(query2):-
    assert_input_phenomena(15),
    recognition_query(5,5,15).

% meets d-d window
test(case10):-
    assertion(findall(L,dynamic_phenomenon_intervals(sad_then_happy(bob),L),[[[8,15]]])).

% before d-d (retained from last window)
test(case11):-
    assertion(findall(L,dynamic_phenomenon_intervals(sad_before_hungry(bob),L),[[[8,inf]]])).

% contains d-t
test(case12):-
    assertion(findall(L,dynamic_phenomenon_intervals(drops_objects_when_hungry(alice),L),[[[1,14]]])).

%starts
test(case13):-
    assertion(findall(L,dynamic_phenomenon_intervals(hunger_starts_hungry(bob),L),Z1)),
    assertion(findall(L,state_intervals(hungry(bob),L),Z2)),
    Z1=Z2.

test(case14):-
    assertion(findall(L,dynamic_phenomenon_intervals(ate_ends_hungry(alice),L),[[[1,14]]])),
    assertion(findall(L,dynamic_phenomenon_intervals(ate_ends_hungry(bob),L),[])).

:-end_tests(dynamic_temporal_phenomena).
