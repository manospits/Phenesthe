:-multifile input_phenomenon/2.

input_phenomenon(ais(_,_),event).
input_phenomenon(in_port(_,_),state).
input_phenomenon(in_fishing_area(_,_),state).

event_phenomenon stop_start(V) := ais(V,S) aand S =< 0.5.
event_phenomenon stop_end(V) := ais(V,S) aand S > 0.5.

state_phenomenon stopped(V) := stop_start(V) ~> stop_end(V).
state_phenomenon underway(V) := (ais(V,S) aand S > 1) ~> (ais(V,S) aand S<1).
state_phenomenon moored(V,P) := stopped(V) intersection in_port(V,P).


dynamic_phenomenon fishing_trip(V,PA,FA,PB):=
    (end(moored(V,PA)) before
        (underway(V) contains in_fishing_area(V,FA)))
            before start(moored(V,PB)).
