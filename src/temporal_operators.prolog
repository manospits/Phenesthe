% Author: Manolis Pitsikalis
%
% Temporal operations 
% maximal range, iteration, temp. union/intersection/complement


compute_maxmin_intervals(max,L,I):-
    compute_maximal_intervals(L,(_,n),I).
compute_maxmin_intervals(min,L,I):-
    compute_minimal_intervals(L, n, n, n, I).

%maximal interval cmputation from se list #
compute_maximal_intervals([],(_,n),[]).
compute_maximal_intervals([],(Ts:[V],y),[([Ts,inf],V)]).
compute_maximal_intervals([(Te,0:[],1:[t])|SE],(Ts:[Va],y),[([Ts,Te],Va)|R]):-
    compute_maximal_intervals(SE,(_,n),R).
compute_maximal_intervals([(_Te,0:[],1:[u])|SE],(Ts:[_],y),R):-
    compute_maximal_intervals(SE,(Ts:[u],y),R).
compute_maximal_intervals([(Ts,1:[Va],_)|SE],(_,n),R):-
    compute_maximal_intervals(SE,(Ts:[Va],y),R).
compute_maximal_intervals([(_,1:[_],_)|SE],(Ts:[V],y),R):-
    compute_maximal_intervals(SE,(Ts:[V],y),R).
compute_maximal_intervals([(_,0:[],_)|SE],(_,n),R):-
    compute_maximal_intervals(SE,(_,n),R).


% empty list
% no Tstart set
compute_minimal_intervals([], n, _, _, []).
% Tstart set ->  create interval open with u 
compute_minimal_intervals([], T, _, _, [([T,inf],u)]):- T\=n.

%---- irrelevant t ----
% Tstart not set, and current t not start 
compute_minimal_intervals([(_,0:[],_:_)|SE], n, n, n, R):-
    compute_minimal_intervals(SE, n, n, n, R).

%---- encounter Tend -----
% Tstart set Tend is true -> create interval with value V
compute_minimal_intervals([(TE,0:[],1:[t])|SE], TS, _, V, [([TS,TE],V)|R]):-
    TS \= n,
    compute_minimal_intervals(SE, n, n, n, R).
% Tstart set but Tend is u -> store tend and update V
compute_minimal_intervals([(TE, _:Z, 1:[u])|SE], TS, _, _V, R):-
    TS \= n,
    member(Z,[[],[u]]),
    compute_minimal_intervals(SE, TS, TE, u, R).

%---- encounter Tstart ----
%case a: no prev start, store appropriate values
compute_minimal_intervals([(TS, 1:[V], _:_)|SE], n, n, n, R):-
    compute_minimal_intervals(SE, TS, n, V, R).

%case b: encounter true and previous end is u. Create interval with u value
compute_minimal_intervals([(T, 1:[t], _:_)|SE], TS, TE, V, [([TS,TE],V)|R]):-
    TS\=n, TE\=n, 
    compute_minimal_intervals(SE, T, n, t, R).
%case c: encounter true and no previous end is u. Update t start
compute_minimal_intervals([(T, 1:[t], _:_)|SE], TS, n, _V, R):-
    TS\=n, 
    compute_minimal_intervals(SE, T, n, t, R).
%case d: encounter u, no update at t start but change V to u
compute_minimal_intervals([(_T, 1:[u], _:_)|SE], TS, _, _V, R):-
    TS\=n, 
    compute_minimal_intervals(SE, TS, n, u, R).



%single scan temporal union # 
compute_union_intervals([],_,_,_,_,[]).
compute_union_intervals([(TS,(SC:SV,EC:EV))|R],0,E,SU,_,IL):-
    SC>0,
    EN is E+EC,
    count_unknown(SV,SUc),
    count_unknown(EV,EUc),
    length(SV,L),
    (SUc < L -> V=t;V=u),
    SUn is SU+SUc - EUc,!,
    compute_union_intervals(R,SC,EN,SUn, (TS,V),IL).

compute_union_intervals([(TE,(SC:_SV,EC:EV))|R],S,E,SU,(TS,V1),[([TS,TE],V)|IL]):-
    S\=0,
    count_unknown(EV,EUc),
    SUn is SU-EUc,
    ((SUn > 0,EC > 0) -> V=u ; V=V1),
    SN is S+SC,
    EN is E+EC,
    SN>0, SN=EN,!,
    compute_union_intervals(R,0,0,0,_,IL).

compute_union_intervals([(_,(SC:SV,EC:EV))|R],S,E,SU,(TS,V1),IL):-
    S\=0,
    count_unknown(SV,SUc),
    count_unknown(EV,EUc),
    SUn is SU+SUc-EUc,
    ((SUn > 0, EC > 0) -> V = u ; V1=V),
    SN is S+SC,
    EN is E+EC,
    SN\=EN,!,
    compute_union_intervals(R,SN,EN,SUn,(TS,V),IL).

count_unknown([],0).
count_unknown([(_,u)|R],S):-
    count_unknown(R,S1),
    S is S1+1,!.
count_unknown([(_,t)|R],S):-
    count_unknown(R,S),!.

%single scan temporal intersection #
compute_intersection_intervals([],_,_,_,[]).
compute_intersection_intervals([(TE,_:_,_:B)|R],1:Va,1:Vb,TS,[([TS,TE],V)|IL]):-
    e_and(Va,Vb,V),
    ((
        member((a,_),B),member((b,_),B),
        compute_intersection_intervals(R,0:f,0:f,_,IL)
    );
    (
        member((a,_),B),\+member((b,_),B),
        compute_intersection_intervals(R,0:f,1:Vb,_,IL)
    );
    (
        member((b,_),B),\+member((a,_),B),
        compute_intersection_intervals(R,1:Va,0:f,_,IL)
    )).

compute_intersection_intervals([(T,_:A,_:B)|R],AIN:VA,BIN:VB,_,IL):-
    BOTHIN is AIN /\ BIN, BOTHIN = 0,
    check_in(a,VSa,A,AAIN),
    check_in(b,VSb,A,ABIN),
    check_in(a,_VEa,B,RAIN),
    check_in(b,_VEb,B,RBIN),
    neg(RAIN,NeRAIN),
    neg(RBIN,NeRBIN),
    NAIN is (AIN \/ AAIN) /\ NeRAIN,
    NBIN is (BIN \/ ABIN) /\ NeRBIN,
    update_values(NAIN,AAIN,VSa,VA,VAn),
    update_values(NBIN,ABIN,VSb,VB,VBn),
    compute_intersection_intervals(R,NAIN:VAn,NBIN:VBn,T,IL).

update_values(1,1,V,_,V).
update_values(1,0,_,V,V).
update_values(0,_,_,_,f).


%single scan temporal complement
compute_complement_intervals([],_,_,_,[]).

compute_complement_intervals([(TE,_:A,_:B)|R],1:VA,0:_VB,TS,RES):-
    (
        %case interval of a ends
        member((a,_),B),!,
        check_in(b,VSb,A,BIN),
        TE1=TE,
        (TS<TE -> RES=[([TS,TE1],VA)|IL] ; RES=IL),
        compute_complement_intervals(R,0:f,BIN:VSb,_,IL)
    );
    (
        %case interval of b starts
        member((b,VSb),A),
        %check_in(a,VEa,B,AIN),
        %neg(AIN,NAIN),
        %update_values(NAIN,AIN,f,VA,VAn),
        (
            (
             VSb=t,
             TE1 is TE-1,
             (TS<TE1 -> RES = [([TS,TE1],VA)|IL] ; RES=IL), 
             compute_complement_intervals(R,1:VA,1:VSb,_,IL)
            )
            ;
            (
             VSb=u,
             compute_complement_intervals(R,1:VSb,1:VSb,TS,RES)
            )
        )
    ).

compute_complement_intervals([(TE,_:_A,_:B)|R],1:VA,1:u,TS,RES):-!,
    ((
        %case interval of a ends
        member((a,_),B),!,
        check_in(b,_VEb,B,RBIN), % end of interval of second list at t
        (RBIN = 1 -> NBIN=0,VB=f ; NBIN=1,VB=u),
        TE1=TE,
        (TS<TE -> RES=[([TS,TE1],VA)|IL] ; RES=IL),
        compute_complement_intervals(R,0:f,NBIN:VB,_,IL)
    );
    (
        %case interval of a not ends but interval of b ends
        compute_complement_intervals(R,1:u,0:f,TS,RES)
    )).

compute_complement_intervals([(T,_:A,_:B)|R],AIN:VA,BIN:VB,To,IL):-
    \+((AIN=1,BIN=0)),
    \+((AIN=1,BIN=1,VB=u)),
    check_in(a,VSa,A,AAIN), % start of interval of first list at t
    check_in(b,VSb,A,ABIN), % start of interval of second list at t
    check_in(a,_VEa,B,RAIN), % end of interval of first list at t 
    check_in(b,VEb,B,RBIN), % end of interval of second list at t
    neg(RAIN,NeRAIN), % not (end of interval of first list at t)
    neg(RBIN,NeRBIN), % not (end of interval of second list at t)
    %writeln((NAIN is (AIN \/ AAIN) /\ NeRAIN)),
    NAIN is (AIN \/ AAIN) /\ NeRAIN, % interval of a already opened or opened now, and did not close now
    NBIN is (BIN \/ ABIN) /\ NeRBIN, % interval of b already opened or opened now, and did not close now
    update_values(NAIN,AAIN,VSa,VA,VAn),
    update_values(NBIN,ABIN,VSb,VB,VBn),
    (
        (
            NAIN=1,!,
            (
                (ABIN=0,RBIN=0) -> 
                 compute_complement_intervals(R,NAIN:VAn,NBIN:VBn,T,IL)
                 ; 
                 (
                    ( (VEb=u -> T1 = T ; T1 is T+1),
                        compute_complement_intervals(R,NAIN:VAn,NBIN:VBn,T1,IL)
                    )
                 ) 
            )
        )   
    ;
        (
             NAIN\=1,
             compute_complement_intervals(R,NAIN:VAn,NBIN:VBn,To,IL)
        )
    ).


check_in(X,V,A,1):-
    member((X,V),A).
check_in(X,f,A,0):-
    \+member((X,_),A).

neg(1,0).
neg(0,1).
