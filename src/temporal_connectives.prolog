
compute_instants_in_intervals([],_,[]):-!.
compute_instants_in_intervals(_,[],[]):-!.
compute_instants_in_intervals([A|AR],[[TS,TE]|BR],[A|TL]):-
    A >= TS,
    A =< TE,
    compute_instants_in_intervals(AR,[[TS,TE]|BR],TL).

compute_instants_in_intervals([A|AR],[[_TS,TE]|BR],TL):-
    A > TE,
    compute_instants_in_intervals([A|AR],BR,TL).

compute_instants_in_intervals([A|AR],[[TS,TE]|BR],TL):-
    A < TS,
    compute_instants_in_intervals(AR,[[TS,TE]|BR],TL).

