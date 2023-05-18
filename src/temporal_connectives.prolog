% Author: Manolis Pitsikalis
%
% - Connectives operations -
%

%find points included in an interval #
compute_instants_in_intervals([],_,[]):-!.
compute_instants_in_intervals(_,[],[]):-!.
compute_instants_in_intervals([(A,AV)|AR],[([TS,TE],V)|BR],[(A,Vt)|TL]):-
    A >= TS,
    A =< TE,
    (
        (AV=t,V=t,Vt=t,!);
        (AV=u,Vt=u,!);
        (V=u,Vt=u,!)
    ),
    compute_instants_in_intervals(AR,[([TS,TE],V)|BR],TL).

compute_instants_in_intervals([(A,AV)|AR],[([_TS,TE],_V)|BR],TL):-
    A > TE,
    compute_instants_in_intervals([(A,AV)|AR],BR,TL).

compute_instants_in_intervals([(A,_AV)|AR],[([TS,TE],V)|BR],TL):-
    A < TS,
    compute_instants_in_intervals(AR,[([TS,TE],V)|BR],TL).

compute_disjunction([],[],[]):-!.
compute_disjunction(A,[],A):-!.
compute_disjunction([],B,B):-!.
compute_disjunction([(T,Va)|Ra],[(T,Vb)|Rb],[(T,Vt)|Rc]):-
    member(t,[Va,Vb]),!,
    Vt=t,
    compute_disjunction(Ra,Rb,Rc).
compute_disjunction([(T,u)|Ra],[(T,u)|Rb],[(T,u)|Rc]):-
    compute_disjunction(Ra,Rb,Rc).
compute_disjunction([(T1,Va)|Ra],[(T2,Vb)|Rb],[(T1,Va)|Rc]):-
    T1 < T2,
    compute_disjunction(Ra,[(T2,Vb)|Rb],Rc).
 compute_disjunction([(T1,Va)|Ra],[(T2,Vb)|Rb],[(T2,Vb)|Rc]):-
    T1 > T2,
    compute_disjunction([(T1,Va)|Ra],Rb,Rc).
 

compute_conjunction([],[],[]):-!.
compute_conjunction(_A,[],[]):-!.
compute_conjunction([],_B,[]):-!.
compute_conjunction([(T,Va)|Ra],[(T,Vb)|Rb],[(T,Vt)|Rc]):-
    e_and(Va,Vb,Vt),
    compute_conjunction(Ra,Rb,Rc).
compute_conjunction([(T1,_)|Ra],[(T2,Vb)|Rb],Rc):-
    T1 < T2,
    compute_conjunction(Ra,[(T2,Vb)|Rb],Rc).

