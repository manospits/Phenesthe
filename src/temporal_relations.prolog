:-discontiguous compute_before_intervals/6.

compute_relation_intervals(meets,FT,A,B,IL,RA,Tc):-compute_meets_intervals(FT,A,B,IL,RA,Tc).
compute_relation_intervals(overlaps,FT,A,B,IL,RA,Tc):-compute_overlaps_intervals(FT,A,B,IL,RA,Tc).
compute_relation_intervals(starts,FT,A,B,IL,RA,Tc):-compute_starts_intervals(FT,A,B,IL,RA,Tc).
compute_relation_intervals(contains,FT,A,B,IL,RB,Tc):-compute_contains_intervals(FT,A,B,IL,RB,Tc).
compute_relation_intervals(finishes,FT,A,B,IL,Tc):-compute_finishes_intervals(FT,A,B,IL,Tc).
compute_relation_intervals(equals,FT,A,B,IL):-compute_equals_intervals(FT,A,B,IL).


%------------------------------------------------------------
%------------------------------------------------------------
%computation of before intervals
%------------------------------------------------------------
%------------------------------------------------------------
compute_before_intervals(_,[],_,[],[],_).
compute_before_intervals(_,[A|R],[],IL,NonRedundantA,Tcrit):-
    last_ending_temporal_entities([A|R],ALast),
    create_intervals(keepnot,[[unk,unk]],[[unk,unk]],ALast, IL, NonRedundantA,Tcrit).

%------------------------------------------------------------
%participating phenomena are either states or events
compute_before_intervals(d,[A|RL],[B|RR],[[TS,TE]|IL],NonRedundantA,Tcrit):-
    temporal_information(A,_TSA,TEA,_),
    temporal_information(B,TSB,_TEB,_),
    lt(TEA,TSB),
    compute_before_intervals2(d,[A|RL],B,NRL,[TS,TE],AP),
    add_if_not_redundant(AP,[TS,TE],Tcrit,NonRedundantAL,NonRedundantA),
    compute_before_intervals(d,NRL,RR,IL,NonRedundantAL,Tcrit).

compute_before_intervals(d,[A|RL],[B|RR],IL,NonRedundantA,Tcrit):-
    temporal_information(A,_TSA,TEA,_),
    temporal_information(B,TSB,_TEB,_),
    geq(TEA,TSB),
    compute_before_intervals(d,[A|RL],RR,IL,NonRedundantA,Tcrit).

compute_before_intervals2(d,[_|RL],B,NIL,I,AP):-
    RL=[A2|_],
    temporal_information(A2,_TSA2,TEA2,_),
    temporal_information(B,TSB,_TEB,_),
    lt(TEA2,TSB),
    compute_before_intervals2(d,RL,B,NIL,I,AP).

compute_before_intervals2(d,[A|RL],B,RL,[TSA,TEB],A):-
    temporal_information(A,TSA,_TEA,_),
    temporal_information(B,TSB,TEB,_),
    (
        (
         RL=[A2|_],
         temporal_information(A2,_TSA2,TEA2,_),
         geq(TEA2,TSB)
        );
        (
         RL=[]
        )
    ).

compute_before_intervals2(d,[A|[]],B,[],[TSA,TEB],A):-
    temporal_information(A,TSA,_TEA,_),
    temporal_information(B,_TSB,TEB,_).


%-----------------------------------------------------------
%participating phenomena involve dynamic phenomena
compute_before_intervals(nd,[A|RL],[B|RR],I,NonRedundantA,T):-
    get_same_ts_sublist(B,RR,BS,NRR),
    compute_before_intervals2(nd,[A|RL],[B|BS],[],NRL,II,NonRedundantAII,T),
    ((%if there is only one interval of b starting at t and ending unk
      %then A intervals should also be kept for other detections since that interval
      %may not happen. If it does happen results will be updated.
      [B|BS]=[BI],temporal_information(BI,_,unk,_),!,
      compute_before_intervals(nd,[A|RL],NRR,IL,NonRedundantAIL,T)
      )
      ;
      (
      %if not in the above case then we can safely use ramaining A intervals
      compute_before_intervals(nd,NRL,NRR,IL,NonRedundantAIL,T)
      )
    ),
    ord_merge(II,IL,I),
    ord_merge(NonRedundantAII,NonRedundantAIL,NonRedundantA).



%all time entities of A were iterated; create intervals
compute_before_intervals2(nd,[],[B|BI],CLOSR,[],I,NonRedundantA,T):-
    create_intervals(keep,[B|BI],[B|BI],CLOSR,I,NonRedundantA,T).


compute_before_intervals2(nd,[A|RL],[B|RR],CLOS,NRL,I,NonRedundantA,T):-
    temporal_information(A,_TSA,TEA,V), %V contains the TSBs that this interval was matched in a previous query
    temporal_information(B,TSB,_TEB,_),
    V\=[],
    member(TSB,V),!, 
    ((CLOS=[C|_],
      temporal_information(C,_TSC,TEC,_),
      TEC=TEA);
      (CLOS=[])),!,
    append(CLOS,[A],CLOSN),
    compute_before_intervals2(nd,RL,[B|RR],CLOSN,NRL,I,NonRedundantA,T).
compute_before_intervals2(nd,[A|RL],[B|RR],CLOS,NRL,I,NonRedundantA,T):-
    temporal_information(A,_TSA,_TEA,V),
    temporal_information(B,TSB,_TEB,_),
    V\=[],
    \+((member(TSB,V))),!, %if B not member of V A
    compute_before_intervals2(nd,RL,[B|RR],CLOS,NRL,I,NonRedundantA,T).

%current interval of A satisfies the conditions and is closest
compute_before_intervals2(nd,[A|RL],[B|RR],CLOS,NRL,I,NonRedundantA,T):-
    temporal_information(A,_TSA,TEA,_),
    temporal_information(B,TSB,_TEB,_),
    lt(TEA,TSB),
    ((CLOS=[C|_],
      temporal_information(C,_TSC,TEC,_),
      TEC=TEA);
      (CLOS=[])),!,
    append(CLOS,[A],CLOSN),
    compute_before_intervals2(nd,RL,[B|RR],CLOSN,NRL,I,NonRedundantA,T).

%current interval of A satisfies the constraints and is NEW closest
compute_before_intervals2(nd,[A|RL],[B|RR],CLOS,NRL,I,NonRedundantA,T):-
    temporal_information(A,_TSA,TEA,_),
    temporal_information(B,TSB,_TEB,_),
    lt(TEA,TSB),
    CLOS=[C|_],
    temporal_information(C,_TSC,TEC,_),
    lt(TEC,TEA),!,
    compute_before_intervals2(nd,RL,[B|RR],[A],NRL,I,NonRedundantA,T).

%current interval of A satisfies the constraints and but IS NOT closest
compute_before_intervals2(nd,[A|RL],[B|RR],CLOS,NRL,I,NonRedundantA,T):-
    temporal_information(A,_TSA,TEA,_),
    temporal_information(B,TSB,_TEB,_),
    lt(TEA,TSB),
    CLOS=[C|_],
    temporal_information(C,_TSC,TEC,_),
    gt(TEC,TEA),!,
    compute_before_intervals2(nd,RL,[B|RR],CLOS,NRL,I,NonRedundantA,T).

%current interval of A does not satisfy the constraints
compute_before_intervals2(nd,[A|RL],[B|RR],CLOS,[A|NRL],I,NonRedundantA,T):-
    temporal_information(A,_TSA,TEA,_),
    temporal_information(B,TSB,_TEB,_),
    geq(TEA,TSB),!,
    compute_before_intervals2(nd,RL,[B|RR],CLOS,NRL,I,NonRedundantA,T).


%------------------------------------------------------------
%------------------------------------------------------------
%computation of the intervals of a meets relation
%------------------------------------------------------------
%------------------------------------------------------------
compute_meets_intervals(_,[],_,[],[],_):-!.
compute_meets_intervals(_,[A|R],[],IL,NonRedundantA,Tcrit):-
    unk_or_inf_temporal_entities([A|R],AUI),
    create_intervals(keepnot,[[unk,unk]],[[unk,unk]],AUI, IL, NonRedundantA,Tcrit).

%------------------------------------------------------------
%participating phenomena are states
%satisfied conditions
compute_meets_intervals(d,[A|RL],[B|RR],[[TSA,TEB]|IL],NonRedundantA,Tcrit):-
    temporal_information(A,TSA,TEA,_V1),
    temporal_information(B,TSB,TEB,_V2),
    TEA=TSB,!,
    add_if_not_redundant(A,[TSA,TEB],Tcrit,NonRedundantAL,NonRedundantA),
    compute_meets_intervals(d,RL,RR,IL,NonRedundantAL,Tcrit).
%skip b
compute_meets_intervals(d,[A|RL],[B|RR],IL,NonRedundantA,Tcrit):-
    temporal_information(A,_TSA,TEA,_V1),
    temporal_information(B,TSB,_TEB,_V2),
    gt(TEA,TSB),!,
    compute_meets_intervals(d,[A|RL],RR,IL,NonRedundantA,Tcrit).
%skip a
compute_meets_intervals(d,[A|RL],[B|RR],IL,NonRedundantA,Tcrit):-
    temporal_information(A,_TSA,TEA,_V1),
    temporal_information(B,TSB,_TEB,_V2),
    lt(TEA,TSB),!,
    compute_meets_intervals(d,RL,[B|RR],IL,NonRedundantA,Tcrit).

%participating phenomena may be dynamic phenomenona
compute_meets_intervals(nd,[A|RL],[B|RR],IL,NonRedundantA,Tcrit):-
    get_same_ts_sublist(B, RR, SBL, NRR),
    compute_meets_intervals2(nd,[A|RL],[B|SBL],NRL,[],IX,NonRedundantAII,Tcrit),
    compute_meets_intervals(nd,NRL,NRR,IY,NonRedundantAIL,Tcrit),
    ord_merge(IX, IY, IL),
    ord_merge(NonRedundantAII, NonRedundantAIL, NonRedundantA).

compute_meets_intervals2(nd, [A|RL], [B|RR], NRL, CORS, IL, NonRedundantA, Tcrit):-
    temporal_information(A,_TSA,T,_V1),
    temporal_information(B,T,_TEB,_V2),
    append(CORS,[A],NCORS),
    compute_meets_intervals2(nd, RL, [B|RR], NRL, NCORS, IL,NonRedundantA,Tcrit).

%skip a
compute_meets_intervals2(nd, [A|RL], [B|RR], NRL, CORS, IL, NonRedundantA, Tcrit):-
    temporal_information(A,TSA,TEA,_V1),
    temporal_information(B,TSB,_TEB,_V2),
    TSBM1 is TSB-1,
    leq(TSA,TSBM1),
    lt(TEA,TSB),!,
    compute_meets_intervals2(nd, RL, [B|RR], NRL, CORS, IL, NonRedundantA, Tcrit).

%keep a
compute_meets_intervals2(nd,[A|RL],[B|RR], [A|NRL], CORS, IL, NonRedundantA, Tcrit):-
    temporal_information(A,TSA,TEA,_V1),
    temporal_information(B,TSB,_TEB,_V2),
    TSBM1 is TSB-1,
    leq(TSA,TSBM1),
    gt(TEA, TSB),
    compute_meets_intervals2(nd, RL, [B|RR], NRL, CORS, IL, NonRedundantA, Tcrit).

%create intervals
compute_meets_intervals2(nd,[A|RL],[B|RR], [A|RL], CORS, IL,NonRedundantA,Tcrit):-
    temporal_information(A,TSA,_TEA,_V1),
    temporal_information(B,TSB,_TEB,_V2),
    TSBM1 is TSB-1,
    gt(TSA,TSBM1),
    create_intervals(keepnot,[B|RR], [B|RR], CORS, IL, NonRedundantA, Tcrit).

compute_meets_intervals2(nd,[],[B|RR], [], CORS, IL, NonRedundantA, Tcrit):-
    create_intervals(keepnot,[B|RR], [B|RR], CORS, IL, NonRedundantA, Tcrit).

%------------------------------------------------------------
%------------------------------------------------------------
%computation of the intervals of an overlaps  relation
%------------------------------------------------------------
%------------------------------------------------------------
compute_overlaps_intervals(_,[],_,[],[],_):-!.
compute_overlaps_intervals(_,[A|R],[],IL,NonRedundantA,Tcrit):-
    unk_or_inf_temporal_entities([A|R],AUI),
    create_intervals(keepnot,[[unk,unk]],[[unk,unk]],AUI, IL, NonRedundantA,Tcrit).


%------------------------------------------------------------
%participating phenomena are states
%partially satisfied conditions
compute_overlaps_intervals(d,[A|RL],[B|RR],[[TSA,unk]|IL],NonRedundantA,Tcrit):-
    temporal_information(A,TSA,TEA,_V1),
    temporal_information(B,TSB,TEB,_V2),
    is_inf_unk(TEA),
    is_inf_unk(TEB),
    lt(TSA,TSB),gt(TEA,TSB),!,
    % no need to retain information here; either unk or inf
    compute_overlaps_intervals(d,RL,RR,IL,NonRedundantA,Tcrit).
%satisfied partially and completely **** in the case the ending instant
%of b is unk, the code works as it is
compute_overlaps_intervals(d,[A|RL],[B|RR],[[TSA,TEB]|IL],NonRedundantA,Tcrit):-
    temporal_information(A,TSA,TEA,_V1),
    temporal_information(B,TSB,TEB,_V2),
    lt(TSA,TSB),gt(TEA,TSB),lt(TEA,TEB),!,
    add_if_not_redundant(A,[TSA,TEB],Tcrit,NonRedundantAL,NonRedundantA),
    compute_overlaps_intervals(d,RL,RR,IL,NonRedundantAL,Tcrit).
%skip a
compute_overlaps_intervals(d,[A|RL],[B|RR],IL,NonRedundantA,Tcrit):-
    temporal_information(A,TSA,TEA,_V1),
    temporal_information(B,TSB,TEB,_V2),
    (leq(TEA,TSB) ; (gt(TSA,TSB),lt(TEA,TEB))),!,
    compute_overlaps_intervals(d,RL,[B|RR],IL,NonRedundantA,Tcrit).
%skip b
compute_overlaps_intervals(d,[A|RL],[B|RR],IL,NonRedundantA,Tcrit):-
    temporal_information(A,_TSA,TEA,_V1),
    temporal_information(B,_TSB,TEB,_V2),
    geq(TEA,TEB),!,
    compute_overlaps_intervals(d,[A|RL],RR,IL,NonRedundantA,Tcrit).

%participating phenomena may be dynamic phenomenona
compute_overlaps_intervals(nd,[A|RL],[B|RR],IL,NonRedundantA,Tcrit):-
    compute_overlaps_intervals2(nd,[A|RL],B,NRL,[],IX,NonRedundantAII,Tcrit),
    compute_overlaps_intervals(nd,NRL,RR,IY,NonRedundantAIL,Tcrit),
    ord_merge(IX,IY,IL),
    ord_merge(NonRedundantAII,NonRedundantAIL,NonRedundantA).

%partial satisfaction **** both unk/inf
compute_overlaps_intervals2(nd, [A|RL], B, [A|NRL], CORS, IL, NonRedundantA,Tcrit):-
    temporal_information(A,TSA,TEA,_V1),
    temporal_information(B,TSB,TEB,_V2),
    is_inf_unk(TEA),
    is_inf_unk(TEB),
    lt(TSA,TSB),gt(TEA,TSB),!,
    append(CORS,[(u,A)],NCORS), %remember that this is A was in an incomplete detection
    compute_overlaps_intervals2(nd, RL, B, NRL, NCORS, IL, NonRedundantA, Tcrit).

%partial and complete satisfaction **** same as in d case
compute_overlaps_intervals2(nd, [A|RL], B, [A|NRL], CORS, IL, NonRedundantA,Tcrit):-
    temporal_information(A,TSA,TEA,_V1),
    temporal_information(B,TSB,TEB,_V2),
    lt(TSA,TSB),gt(TEA,TSB),lt(TEA,TEB),!,
    append(CORS,[A],NCORS),
    compute_overlaps_intervals2(nd, RL, B, NRL, NCORS, IL, NonRedundantA, Tcrit).

%skip a for future b
compute_overlaps_intervals2(nd, [A|RL], B, NRL, CORS, IL, NonRedundantA, Tcrit):-
    temporal_information(A,TSA,TEA,_V1),
    temporal_information(B,TSB,_TEB,_V2),
    TSBM1 is TSB-1,
    leq(TSA, TSBM1),
    leq(TEA, TSB),!,
    compute_overlaps_intervals2(nd, RL, B, NRL, CORS, IL, NonRedundantA, Tcrit).

%keep a for future b
compute_overlaps_intervals2(nd,[A|RL], B, [A|NRL], CORS, IL, NonRedundantA, Tcrit):-
    temporal_information(A,TSA,TEA,_V1),
    temporal_information(B,TSB,_TEB,_V2),
    TSBM1 is TSB-1,
    leq(TSA,TSBM1),
    gt(TEA,TSB),!,% TEA>=TEB,!,
    compute_overlaps_intervals2(nd, RL, B, NRL, CORS, IL, NonRedundantA, Tcrit).

compute_overlaps_intervals2(nd,[A|RL],B, [A|RL], CORS, IL, NonRedundantA, Tcrit):-
    temporal_information(A,TSA, _TEA,_V1),
    temporal_information(B,TSB, _TEB,_V2),
    TSBM1 is TSB-1,
    gt(TSA, TSBM1),!,
    create_intervals(keepnot,[B], [B], CORS, IL, NonRedundantA, Tcrit).

compute_overlaps_intervals2(nd,[],B, [], CORS, IL, NonRedundantA, Tcrit):-
    create_intervals(keepnot,[B], [B], CORS, IL, NonRedundantA, Tcrit).

%------------------------------------------------------------
%------------------------------------------------------------
%computation of the intervals of a finishes  relation
%------------------------------------------------------------
%------------------------------------------------------------
compute_finishes_intervals(_,[],[],[],_):-!.
compute_finishes_intervals(_,[],[B|R],IL,Tcrit):-!,
    unk_or_inf_temporal_entities([B|R],BUI),
    create_intervals(keepnot,[[unk,unk]],[[unk,unk]],BUI, IL, _,Tcrit).
compute_finishes_intervals(_,[_|_],[],[],_):-!.


%------------------------------------------------------------
%participating phenomena are states
%
%partially satisfied conditions
compute_finishes_intervals(d,[A|RL],[B|RR],[[TSB,unk]|IL], Tcrit):-
    temporal_information(A,TSA, TEA,_V1),
    temporal_information(B,TSB, TEB,_V2),
    is_inf_unk(TEA),
    is_inf_unk(TEB),
    gt(TSA,TSB),!,
    compute_finishes_intervals(d,RL,RR,IL, Tcrit).
%satisfaction
compute_finishes_intervals(d,[A|RL],[B|RR],[B|IL], Tcrit):-
    temporal_information(A,TSA, TEA,_V1),
    temporal_information(B,TSB, TEB,_V2),
    ((lt(TSA,TEA),
      gt(TSA,TSB),TEA=TEB);
      (TSA=TEA,TEA=TEB)),!,
    compute_finishes_intervals(d,RL,RR,IL, Tcrit).

%skip a
compute_finishes_intervals(d,[A|RL],[B|RR],IL, Tcrit):-
    temporal_information(A,_TSA, TEA,_V1),
    temporal_information(B,_TSB, TEB,_V2),
    leq(TEA,TEB),!,
    compute_finishes_intervals(d,RL,[B|RR],IL, Tcrit).

%skip b
compute_finishes_intervals(d,[A|RL],[B|RR],IL, Tcrit):-
    temporal_information(A,_TSA, TEA,_V1),
    temporal_information(B,_TSB, TEB,_V2),
    gt(TEA,TEB),
    compute_finishes_intervals(d,[A|RL],RR,IL, Tcrit).

%participating phenomena may be dynamic phenomenona
compute_finishes_intervals(nd,[A|RL],[B|RR],IL, Tcrit):-
    compute_finishes_intervals2(nd,[A|RL],B,NRL,[],IX, Tcrit),
    compute_finishes_intervals(nd,NRL,RR,IY, Tcrit),
    ord_merge(IX,IY,IL).

%satisfaction
compute_finishes_intervals2(nd, [A|RL], B, [A|NRL], CORS, IL, Tcrit):-
    temporal_information(A, TSA, TEA,_V1),
    temporal_information(B, TSB, TEB,_V2),
    is_inf_unk(TEA),
    is_inf_unk(TEB),
    gt(TSA,TSB),!,
    append(CORS,[(u,A)],NCORS),
    compute_finishes_intervals2(nd, RL, B, NRL, NCORS,  IL, Tcrit).

compute_finishes_intervals2(nd, [A|RL], B, [A|NRL], CORS, IL, Tcrit):-
    temporal_information(A,TSA, TEA,_V1),
    temporal_information(B,TSB, TEB,_V2),
    ((lt(TSA,TEA),
      gt(TSA,TSB),TEA=TEB);
      (TSA=TEA,TEA=TEB)),!,
    append(CORS,[A],NCORS),
    compute_finishes_intervals2(nd, RL, B, NRL, NCORS, IL, Tcrit).

%skip a for future b
compute_finishes_intervals2(nd, [A|RL], B, NRL, CORS, IL, Tcrit):-
    temporal_information(A,TSA, TEA, _V1),
    temporal_information(B,TSB, _TEB, _V2),
    TSBM1 is TSB-1,
    leq(TSA,TSBM1),
    leq(TEA,TSB),!,
    compute_finishes_intervals2(nd, RL, B, NRL, CORS, IL, Tcrit).

%keep a for future b
compute_finishes_intervals2(nd,[A|RL], B, [A|NRL], CORS, IL, Tcrit):-
    temporal_information(A,TSA, TEA, _V1),
    temporal_information(B,TSB, _TEB, _V2),
    TSBM1 is TSB-1,
    leq(TSA,TSBM1),
    gt(TEA,TSB),!,
    compute_finishes_intervals2(nd, RL, B, NRL, CORS, IL, Tcrit).

compute_finishes_intervals2(nd,[A|RL], B, [A|RL], CORS, IL, Tcrit):-
    temporal_information(A,TSA, _TEA, _V1),
    temporal_information(B,TSB, _TEB, _V2),
    TSBM1 is TSB-1,
    gt(TSA, TSBM1),!,
    create_intervals(keepnot,[B], [B], CORS, IL, _, Tcrit).
    %((CORS\=[],!,IL=[[TSB,TEB]]);
     %(CORS=[],IL=[])).

compute_finishes_intervals2(nd,[],B, [], CORS, IL, Tcrit):-
    create_intervals(keepnot,[B], [B], CORS, IL, _, Tcrit).


%------------------------------------------------------------
%------------------------------------------------------------
%computation of the intervals of a starts relation
%------------------------------------------------------------
%------------------------------------------------------------
compute_starts_intervals(_,[],_,[],[],_):-!.
compute_starts_intervals(_,[_|_],[],[],[],_):-!.

%------------------------------------------------------------
%participating phenomena are states
%partially satisfied conditions
compute_starts_intervals(d,[A|RL],[B|RR],[[TSA,unk]|IL],NonRedundantA,Tcrit):-
    temporal_information(A,TSA,TEA,_V1),
    temporal_information(B,TSB,TEB,_V2),
    TSA=TSB,
    is_inf_unk(TEA),
    is_inf_unk(TEB),!,
    compute_starts_intervals(d,RL,RR,IL,NonRedundantA,Tcrit).

%satisfied conditions
compute_starts_intervals(d,[A|RL],[B|RR],[B|IL],NonRedundantA,Tcrit):-
    temporal_information(A,TSA,TEA,_V1),
    temporal_information(B,TSB,TEB,_V2),
    ((lt(TSA,TEA),
      TSA=TSB,lt(TEA,TEB));
      (TSA=TEA,TEA=TSB)),!,
    add_if_not_redundant(A,[TSB,TEB],Tcrit,NonRedundantAL,NonRedundantA),
    compute_starts_intervals(d,RL,RR,IL,NonRedundantAL,Tcrit).

%skip a
compute_starts_intervals(d,[A|RL],[B|RR],IL,NonRedundantA,Tcrit):-
    temporal_information(A,_TSA,TEA,_V1),
    temporal_information(B,_TSB,TEB,_V2),
    leq(TEA,TEB),!,
    compute_starts_intervals(d,RL,[B|RR],IL,NonRedundantA,Tcrit).

%skip b
compute_starts_intervals(d,[A|RL],[B|RR],IL,NonRedundantA,Tcrit):-
    temporal_information(A,_TSA,TEA,_V1),
    temporal_information(B,_TSB,TEB,_V2),
    gt(TEA, TEB),!,
    compute_starts_intervals(d,[A|RL],RR,IL,NonRedundantA,Tcrit).

%participating phenomena may be dynamic phenomenona
compute_starts_intervals(nd,[A|RL],[B|RR],IL,NonRedundantA,Tcrit):-
    compute_starts_intervals2(nd,[A|RL],B,NRL,[],IX,NonRedundantAII,Tcrit),
    compute_starts_intervals(nd,NRL,RR,IY,NonRedundantAIL,Tcrit),
    ord_merge(IX,IY,IL),
    ord_merge(NonRedundantAII,NonRedundantAIL,NonRedundantA).

%partial satisfaction
compute_starts_intervals2(nd, [A|RL], B, [A|NRL], CORS, IL, NonRedundantA,Tcrit):-
    temporal_information(A,TSA,TEA,_V1),
    temporal_information(B,TSB,TEB,_V2),
    TSA=TSB,
    is_inf_unk(TEA),
    is_inf_unk(TEB),!,
    append(CORS,[(u,A)],NCORS),
    compute_starts_intervals2(nd, RL, B, NRL, NCORS, IL, NonRedundantA,Tcrit).

%satisfaction
compute_starts_intervals2(nd, [A|RL], B, [A|NRL], CORS, IL, NonRedundantA,Tcrit):-
    temporal_information(A,TSA,TEA,_V1),
    temporal_information(B,TSB,TEB,_V2),
    ((lt(TSA,TEA),
      TSA=TSB,lt(TEA,TEB));
      (TSA=TEA,TEA=TSB)),!,
    append(CORS,[A],NCORS),
    compute_starts_intervals2(nd, RL, B, NRL, NCORS,  IL, NonRedundantA,Tcrit).

%skip a for future b
compute_starts_intervals2(nd, [A|RL], B, NRL, CORS, IL, NonRedundantA,Tcrit):-
    temporal_information(A,TSA,TEA,_V1),
    temporal_information(B,TSB,_TEB,_V2),
    TSBM1 is TSB-1,
    leq(TSA,TSBM1),
    leq(TEA,TSB),!,
    compute_starts_intervals2(nd, RL, B, NRL, CORS, IL, NonRedundantA,Tcrit).

%keep a for future b
compute_starts_intervals2(nd,[A|RL],B, [A|NRL], CORS, IL, NonRedundantA,Tcrit):-
    temporal_information(A,TSA,TEA,_V1),
    temporal_information(B,TSB,_TEB,_V2),
    TSBM1 is TSB-1,
    leq(TSA, TSBM1),
    gt(TEA, TSB),!,
    compute_starts_intervals2(nd, RL, B, NRL, CORS, IL, NonRedundantA,Tcrit).

compute_starts_intervals2(nd,[A|RL], B, [A|RL], CORS,  IL,  NonRedundantA,Tcrit):-
    temporal_information(A,TSA,_TEA,_V1),
    temporal_information(B,TSB,_TEB,_V2),
    TSBM1 is TSB-1,
    gt(TSA,TSBM1),!,
    create_intervals(keepnot,[B], [B], CORS, IL, NonRedundantA, Tcrit).

compute_starts_intervals2(nd,[], B, [], CORS,  IL, NonRedundantA,Tcrit):-
    create_intervals(keepnot,[B], [B], CORS, IL, NonRedundantA, Tcrit).

%------------------------------------------------------------
%------------------------------------------------------------
%computation of the intervals of a equals relation
%------------------------------------------------------------
%------------------------------------------------------------
compute_equals_intervals(_,[],_,[]):-!.
compute_equals_intervals(_,_,[],[]):-!.

%------------------------------------------------------------
%participating phenomena are states
%partially satisfied conditions
compute_equals_intervals(d,[I1|RL],[I2|RR],[[TS,unk]|IL]):-
    temporal_information(I1,TS,TE1,_V1),
    temporal_information(I2,TS,TE2,_V2),
    is_inf_unk(TE1),
    is_inf_unk(TE2),!,
    compute_equals_intervals(d,RL,RR,IL).

%satisfied conditions
compute_equals_intervals(d,[I1|RL],[I2|RR],[[TS,TE]|IL]):-
    temporal_information(I1,TS,TE,_V1),
    temporal_information(I2,TS,TE,_V2),
    \+is_inf_unk(TE),!,
    compute_equals_intervals(d,RL,RR,IL).

%skip a
compute_equals_intervals(d,[I1|RL],[I2|RR],IL):-
    temporal_information(I1,TSA,TEA,_V1),
    temporal_information(I2,TSB,TEB,_V2),
    \+((TSA=TSB,TEA=TEB)),
    leq(TEA,TEB),!,
    compute_equals_intervals(d,RL,[I2|RR],IL).

%skip b
compute_equals_intervals(d,[I1|RL],[I2|RR],IL):-
    temporal_information(I1,_TSA,TEA,_V1),
    temporal_information(I2,_TSB,TEB,_V2),
    gt(TEA,TEB),
    compute_equals_intervals(d,[I1|RL],RR,IL).

%participating phenomena may be dynamic phenomenona
compute_equals_intervals(nd,[A|RL],[B|RR],IL):-
    compute_equals_intervals2(nd,[A|RL],B,NRL,[],IX),
    compute_equals_intervals(nd,NRL,RR,IY),
    ord_merge(IX,IY,IL).

%partial satisfaction validity cannot be determined
compute_equals_intervals2(nd, [I1|RL], I2, RL, _CORS, [[TS,unk]]):-
    temporal_information(I1,TS,TE1,_V1),
    temporal_information(I2,TS,TE2,_V2),
    is_inf_unk(TE1),is_inf_unk(TE2),!.

%satisfaction ---validity can be determined
compute_equals_intervals2(nd, [I1|RL], I2, RL, _CORS, [[TS,TE]]):-
    temporal_information(I1,TS,TE,_V1),
    temporal_information(I2,TS,TE,_V2),
    \+is_inf_unk(TE),!.

%skip a for future b
compute_equals_intervals2(nd, [I1|RL], I2, NRL, CORS, IL):-
    temporal_information(I1,TSA,TEA,_V1),
    temporal_information(I2,TSB,_TEB,_V2),
    TSBM1 is TSB-1,
    leq(TSA,TSBM1),
    leq(TEA, TSB),!,
    compute_equals_intervals2(nd, RL, I2, NRL, CORS, IL).

%keep a for future b
% ---------
% -----
compute_equals_intervals2(nd,[I1|RL], I2, [I1|NRL], CORS, IL):-
    temporal_information(I1,TSA,TEA,_V1),
    temporal_information(I2,TSB,_TEB,_V2),
    TSBM1 is TSB-1,
    leq(TSA,TSBM1),
    gt(TEA, TSB),!,
    compute_equals_intervals2(nd, RL, I2, NRL, CORS, IL).

compute_equals_intervals2(nd,[I1|RL], I2, [I1|RL], _CORS, []):-
    temporal_information(I1,TSA,_TEA,_V1),
    temporal_information(I2,TSB,_TEB,_V2),
    TSBM1 is TSB-1,
    gt(TSA,TSBM1),!.

compute_equals_intervals2(nd,[],[_|_RR], [], _CORS, []).


%------------------------------------------------------------
%------------------------------------------------------------
%computation of the intervals of a contains relation
%------------------------------------------------------------
%------------------------------------------------------------
compute_contains_intervals(_,[],_,[],[],_):-!.
compute_contains_intervals(_,[A|R],[],IL,NonRedundantA,Tcrit):-!,
    unk_or_inf_temporal_entities([A|R],AUI),
    create_intervals(keepnot,[[unk,unk]],[[unk,unk]],AUI, IL, NonRedundantA,Tcrit).

%------------------------------------------------------------
%participating phenomena are states/events (b)
%satisfied conditions
compute_contains_intervals(d,[A|RL],[B|RR],[[TSA,unk]|IL],NonRedundantB,Tcrit):-
    temporal_information(A,TSA,TEA,_V1),
    temporal_information(B,TSB,TEB,_V2),
    is_inf_unk(TEA),
    is_inf_unk(TEB),
    lt(TSA,TSB),!,
    %no need to add them in non redundant
    compute_contains_intervals(d,RL,RR,IL,NonRedundantB,Tcrit).

compute_contains_intervals(d,[A|RL],[B|RR],[A|IL],NonRedundantB,Tcrit):-
    temporal_information(A,TSA,TEA,_V1),
    temporal_information(B,TSB,TEB,_V2),
    lt(TSA,TSB),gt(TEA,TEB),!,
    add_if_not_redundant(B,A,Tcrit,NonRedundantBL,NonRedundantB),
    compute_contains_intervals(d,RL,RR,IL,NonRedundantBL,Tcrit).


%skip a
compute_contains_intervals(d,[A|RL],[B|RR],IL,NonRedundantB,Tcrit):-
    temporal_information(A,TSA,TEA,_V1),
    temporal_information(B,TSB,TEB,_V2),
    \+((lt(TSA,TSB),gt(TEA,TEB))),
    leq(TEA,TEB),!,
    compute_contains_intervals(d,RL,[B|RR],IL,NonRedundantB,Tcrit).

%skip b
compute_contains_intervals(d,[A|RL],[B|RR],IL,NonRedundantB,Tcrit):-
    temporal_information(A,TSA,TEA,_V1),
    temporal_information(B,TSB,TEB,_V2),
    \+((lt(TSA,TSB),gt(TEA,TEB))),
    gt(TEA,TEB),!,
    compute_contains_intervals(d,[A|RL],RR,IL,NonRedundantB,Tcrit).

%participating phenomena may be dynamic phenomenona
compute_contains_intervals(nd,[A|RL],[B|RR],IL,NonRedundantB,Tcrit):-
    compute_contains_intervals2(nd,[A|RL],B,NRL,[],IX,NonRedundantBII,Tcrit),
    compute_contains_intervals(nd,NRL,RR,IY,NonRedundantBIL,Tcrit),
    ord_merge(IX,IY,IL),
    ord_merge(NonRedundantBII,NonRedundantBIL,NonRedundantB).

%partial satisfaction case a
compute_contains_intervals2(nd, [A|RL], B, NRL, CORS, IL, NonRedundantB, Tcrit):-
    temporal_information(A,TSA,TEA,_V1),
    temporal_information(B,TSB,TEB,_V2),
    is_inf_unk(TEA),
    is_inf_unk(TEB),
    lt(TSA,TSB),!,
    append(CORS,[[TSA,unk]],NCORS),
    compute_contains_intervals2(nd,RL,B, NRL, NCORS, IL, NonRedundantB, Tcrit).
%partial satisfaction case b
compute_contains_intervals2(nd, [A|RL], B, NRL, CORS, IL, NonRedundantB, Tcrit):-
    temporal_information(A,TSA,unk,_V1),
    temporal_information(B,TSB,TEB,_V2),
    \+is_inf_unk(TEB),
    lt(TSA,TSB),!,
    append(CORS,[[TSA,unk]],NCORS),
    compute_contains_intervals2(nd,RL,B, NRL, NCORS, IL, NonRedundantBL, Tcrit),
    add_if_not_redundant(B,[TSA,unk],Tcrit,NonRedundantBL,NonRedundantBZ),
    ((NonRedundantBZ=[NRB|_],!,NonRedundantB=[NRB]);
     (NonRedundantBZ=[],NonRedundantB=[])).

%satisfaction
compute_contains_intervals2(nd, [A|RL], B, NRL, CORS, IL, NonRedundantB, Tcrit):-
    temporal_information(A,TSA,TEA,_V1),
    temporal_information(B,TSB,TEB,_V2),
    lt(TSA,TSB),gt(TEA,TEB),!,
    append(CORS,[A],NCORS),
    compute_contains_intervals2(nd,RL,B, NRL, NCORS, IL, NonRedundantBL, Tcrit),
    add_if_not_redundant(B,A,Tcrit,NonRedundantBL,NonRedundantBZ),
    ((NonRedundantBZ=[NRB|_],!,NonRedundantB=[NRB]);
     (NonRedundantBZ=[],NonRedundantB=[])).


%skip a for future b
compute_contains_intervals2(nd, [A|RL], B, NRL, CORS, IL, NonRedundantB, Tcrit):-
    temporal_information(A,TSA,TEA,_V1),
    temporal_information(B,TSB,_TEB,_V2),
    TSBM1 is TSB-1,
    leq(TSA,TSBM1),
    leq(TEA,TSB),!,
    compute_contains_intervals2(nd, RL, B, NRL, CORS, IL, NonRedundantB, Tcrit).

%keep a for future b
compute_contains_intervals2(nd,[A|RL],B, [A|NRL], CORS, IL, NonRedundantB, Tcrit):-
    temporal_information(A,TSA,TEA,_V1),
    temporal_information(B,TSB,_TEB,_V2),
    TSBM1 is TSB-1,
    leq(TSA,TSBM1),
    gt(TEA,TSB),!,
    compute_contains_intervals2(nd, RL, B, NRL, CORS, IL, NonRedundantB, Tcrit).

compute_contains_intervals2(nd,[A|RL],B, [A|RL], CORS, CORS,  [], _):-
    temporal_information(B,TSA,_TEA,_V1),
    temporal_information(B,TSB,_TEB,_V2),
    TSBM1 is TSB-1,
    gt(TSA,TSBM1),!.

compute_contains_intervals2(nd,[],[_|_], [], CORS, CORS, [], _).
