:-begin_tests(events).
:-['../../loader.prolog'].
:-['../definitions.prolog'].
:-['./narrative.prolog'].

:-preprocess_phenomena_definitions.

test(query0):-
    assert_input_phenomena(5),
    recognition_query(5,5,5).

test(case1):-
    findall(L,dynamic_phenomenon_intervals(drops_objects_when_hungry(bob),L),[[[1,inf]]]).

test(query1):-
    assert_input_phenomena(10),
    recognition_query(5,5,10).

test(case2):-
    findall(L,dynamic_phenomenon_intervals(drops_objects_when_hungry(bob),L),[[[1,8]]]),
    findall(L,dynamic_phenomenon_intervals(drops_objects_when_hungry(alice),L),[[[1,inf]]]).

test(case3):-
    findall(L,dynamic_phenomenon_intervals(happy_then_sad(bob),L),[[[6,10]]]),
    findall(L,dynamic_phenomenon_intervals(sad_then_happy(bob),L),[[[1,8],[8,inf]]]).

test(query2):-
    assert_input_phenomena(15),
    recognition_query(5,5,15).

test(case4):-
    findall(L,dynamic_phenomenon_intervals(sad_then_happy(bob),L),[[[8,inf]]]).

test(case5):-
    findall(L,dynamic_phenomenon_intervals(sad_before_hungry(bob),L),[[[8,inf]]]).

test(case6):-
    findall(L,dynamic_phenomenon_intervals(drops_objects_when_hungry(alice),L),[[[1,14]]]).


:-end_tests(events).
