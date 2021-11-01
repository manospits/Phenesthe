assert_input_phenomena(5):-
    assert(input_event_instant(pickup(bob,torch),1)),
    assert(input_event_instant(pickup(bob,coin),3)),
    assert(input_event_instant(drop(bob,wallet),3)),
    assert(input_event_instant(ate(bob,food),3)),
    assert(input_event_instant(pickup(alice,wallet),4)).
assert_input_phenomena(10):-
    assert(input_event_instant(pickup(alice,flower),6)),
    assert(input_event_instant(drop(bob,torch),8)),
    assert(input_event_instant(ate(alice,bread),9)),
    assert(input_event_instant(drop(alice,bread),9)).

