assert_input_phenomena(5):-
    assert(input_event_instant(born(bob),1)),
    assert(input_event_instant(load(gun),2)),
    assert(input_event_instant(shoot(gun,bob),3)).

