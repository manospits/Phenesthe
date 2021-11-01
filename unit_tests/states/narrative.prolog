assert_input_phenomena(5):-
    assert(input_event_instant(pickup(bob,torch),1)),
    assert(input_event_instant(pickup(bob,coin),3)),
    assert(input_event_instant(drop(bob,wallet),3)),
    assert(input_event_instant(pickup(alice,wallet),4)),
    assert(input_event_instant(drop(bob,torch),5)),
    assert(input_state_interval(angry(alice),[1,2])).
assert_input_phenomena(10):-
    assert(input_event_instant(hunger(bob),6)),
    assert(input_state_interval(angry(bob),[6,7])),
    assert(input_event_instant(pickup(alice,flower),6)),
    assert(input_event_instant(pickup(bob,wallet),6)),
    assert(input_event_instant(drop(bob,coin),8)),
    assert(input_state_interval(angry(bob),[9,10])),
    assert(input_event_instant(drop(alice,wallet),9)),
    assert(input_event_instant(pickup(alice,coin),9)),
    assert(input_event_instant(pickup(john,wallet),10)).

