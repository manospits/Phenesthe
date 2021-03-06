:-multifile input_phenomenon/2.

input_phenomenon(drop(_Person,_Object),event).
input_phenomenon(pickup(_Person,_Object),event).
input_phenomenon(ate(_Person,_Food),event).
input_phenomenon(hunger(_Person),event).

%gain when picking up sth and not dropping sth
event_phenomenon gain(Person) :=
    pickup(Person, _Object) and
    tnot drop(Person,_).

%loss when dropping sth and not picking up sth
event_phenomenon loss(Person) :=
    drop(Person, _Object1) and
    tnot pickup(Person, _Object2).

%no effect when loss and gain
event_phenomenon unchanged(Person) :=
    pickup(Person, _Object1) and
    drop(Person, _Object2).

%true when dropping or picking up something
event_phenomenon action(Person) :=
    drop(Person, _Object1) or
    pickup(Person, _Object3).

%true when neither dropping or picking up happens
event_phenomenon no_action(Person) :=
    tnot drop(Person,_) and
    tnot pickup(Person,_).

event_phenomenon ate_and_dropped(Person) :=
    ate(Person,X) and drop(Person,X).

%a person posses something when he picks it up
%stops possesing it if it drops it
state_phenomenon possess(Person, Object) :=
    pickup(Person, Object) ~> drop(Person, Object).

%a person is happy when there is gain
%stops being happy when there is loss
state_phenomenon happy(Person) :=
    gain(Person) ~> loss(Person).

state_phenomenon sad(Person) :=
    loss(Person) ~> gain(Person).

state_phenomenon hungry(Person):=
    hunger(Person) ~> ate(Person,_Food).

state_phenomenon happy_with_money(Person) :=
    happy(Person) intersection possess(Person,wallet).

state_phenomenon happy_without_money(Person) :=
    happy(Person) complement possess(Person,wallet).

state_phenomenon can_eat(Person):=
    (pickup(Person,food) ~>
        drop(Person,food) or ate(Person,food))
    union possess(Person, wallet).

state_phenomenon feels(Person) :=
    hungry(Person) union sad(Person) union happy(Person).

dynamic_phenomenon sad_then_happy(Person) :=
    sad(Person) meets happy(Person).

dynamic_phenomenon happy_then_sad(Person) :=
    happy(Person) meets sad(Person).

dynamic_phenomenon drops_objects_when_hungry(Person) :=
    hungry(Person) contains drop(Person,_Object).

dynamic_phenomenon sad_before_hungry(Person) :=
    sad(Person) before hungry(Person).


