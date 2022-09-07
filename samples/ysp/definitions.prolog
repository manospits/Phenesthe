:-multifile input_phenomenon/2.

input_phenomenon(load(_G),event).
input_phenomenon(born(_P),event).
input_phenomenon(shoot(_G,_P),event).


state_phenomenon loaded(G) := load(G) ~> shoot(G,_P).

state_phenomenon alive(P) := born(P) ~> dies(P).

event_phenomenon dies(P) := shoot(G,P) in loaded(G).
