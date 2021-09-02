:-multifile input_phenomenon/2.

input_phenomenon(ais(_MMSI,_Speed,_CoG,_TrHeading),event).
input_phenomenon(entersPort(_MMSI,_Port),event).
input_phenomenon(leavesPort(_MMSI,_Port),event).
input_phenomenon(entersFishingArea(_MMSI,_FArea),event).
input_phenomenon(leavesFishingArea(_MMSI,_FArea),event).

event_phenomenon stop_start(V) := ais(V,S,_,_) aand S =< 0.5.
event_phenomenon stop_end(V) := ais(V,S,_,_) aand S > 0.5.

state_phenomenon in_port(V,P) := entersPort(V,P) ~> leavesPort(V,P).
state_phenomenon in_fishing_area(V,F) := entersFishingArea(V,F) ~> leavesFishingArea(V,F).
state_phenomenon stopped(V) := stop_start(V) ~> stop_end(V).
state_phenomenon underway(V) := (ais(V,S,_,_) aand S > 1) ~> (ais(V,S,_,_) aand S<1).
state_phenomenon moored(V,P) := stopped(V) intersection in_port(V,P).


dynamic_phenomenon fishing_trip(V,PA,FA,PB):=
    end(moored(V,PA)) before 
        ((underway(V) contains in_fishing_area(V,FA))
            before start(moored(V,PB))).

