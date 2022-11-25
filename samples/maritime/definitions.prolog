:-['../../src/operators.prolog'].
:-multifile input_phenomenon/2.

input_phenomenon(ais(_MMSI,_Speed,_CoG,_TrHeading),event).
input_phenomenon(entersPort(_MMSI,_Port),event).
input_phenomenon(leavesPort(_MMSI,_Port),event).
input_phenomenon(entersFishingArea(_MMSI,_FArea),event).
input_phenomenon(leavesFishingArea(_MMSI,_FArea),event).

event_phenomenon stop_start(V) := ais(V,S,_,_) aand S =< 0.5.
event_phenomenon stop_end(V) := ais(V,S,_,_) aand S > 0.5.


%state_phenomenon no_major_speed_changes(V) :=
    %ais(V,S1,_,_) ~> (ais(V,S2,_,_) aand ((S3 is abs(S2-S1), S3 >6))).

state_phenomenon no_major_speed_changes(V) :=
    ais(V,S,_,_) <@ collector(600,[S],speed_diff_check).

speed_diff_check([PrevSpeed],[CurSpeed]):- 
    D is abs(CurSpeed-PrevSpeed), D < 6.

state_phenomenon in_port(V,P) := entersPort(V,P) ~> leavesPort(V,P).
state_phenomenon in_fishing_area(V,F) := entersFishingArea(V,F) ~> leavesFishingArea(V,F).
state_phenomenon stopped(V) := stop_start(V) ~> stop_end(V).
state_phenomenon underway(V) := (ais(V,S1,_,_) aand S1 >= 2.7) ~> (ais(V,S2,_,_) aand S2<2.7).
state_phenomenon moored(V,P) := stopped(V) intersection in_port(V,P).


dynamic_phenomenon trip(V,PA,PB):=
    end(moored(V,PA)) before
     (underway(V) before start(moored(V,PB))).

dynamic_phenomenon fishing_trip(V,PA,FA,PB):=
    (end(moored(V,PA)) aand vessel_type(V,fishing)) before
        ((underway(V) contains in_fishing_area(V,FA))
            before start(moored(V,PB))).
