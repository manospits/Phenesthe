:-multifile input_phenomenon/2.

input_phenomenon(turn(_P,_A,_S),event).
input_phenomenon(place(_P,_A,_F),event).
input_phenomenon(remove(_P,_A,_F),event).
input_phenomenon(stir(_F),event).


state_phenomenon powered(A) := turn(_P1,A,on) ~> turn(_P2,A,off).
state_phenomenon placed(P,A,F) := place(P,A,F) ~> remove(P,A,F).
state_phenomenon cooking(P,A,F) := powered(A) intersection placed(P,A,F).

state_phenomenon stirfryPrep(P) := filter((filter(((stir(vegetables) @ 30) intersection cooking(P,stove,vegetables)),greater(300))),less(360)).
dynamic_phenomenon steaksPrep(P) :=  filter((filter((cooking(P,stove,steaks)),greater(120))),less(180)) before filter((filter((cooking(P,oven,steaks)),greater(480))),less(600)).
dynamic_phenomenon mealPrep(P) := stirfryPrep(P) finishes steaksPrep(P).



