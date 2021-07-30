assert_input_phenomena(5):-
    assert(event_instant(pickup(bob,torch),1)),
    assert(event_instant(pickup(bob,coin),3)),
    assert(event_instant(drop(bob,wallet),3)),
    assert(event_instant(pickup(alice,wallet),4)),
    assert(event_instant(drop(bob,torch),5)).
assert_input_phenomena(10):-
    assert(event_instant(pickup(alice,flower),6)),
    assert(event_instant(pickup(bob,wallet),6)),
    assert(event_instant(drop(bob,coin),8)),
    assert(event_instant(drop(alice,wallet),9)),
    assert(event_instant(pickup(alice,coin),9)),
    assert(event_instant(pickup(john,wallet),10)).

