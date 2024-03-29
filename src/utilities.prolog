% Author: Manolis Pitsikalis
%
% Utilities used in different parts
%
:-dynamic val/2.

% merge two interval lists into one that has instants and information
% regarding if that instant is a start or an end of an interval
merge_ilse([],[],[]).
merge_ilse([],[[B1,B2]|BL],[(B1,(1:[b],0:[]))|RIL]):-
    merge_ilse([],[[B2]|BL],RIL).
merge_ilse([],[[B2]|BL],[(B2,(0:[],1:[b]))|RIL]):-
    merge_ilse([],BL,RIL).
merge_ilse([[A1,A2]|AL],[],[(A1,(1:[a],0:[]))|RIL]):-
    merge_ilse([[A2]|AL],[],RIL).
merge_ilse([[A2]|AL],[],[(A2,(0:[],1:[a]))|RIL]):-
    merge_ilse(AL,[],RIL).
merge_ilse([A|AL],[B|BL],[(FA,(XC1N,XC2N))|RIL]):-
    getf(A,FA,AR,(XA1,XA2)),
    getf(B,FB,BR,(XB1,XB2)),
    FA=FB,
    update_xc((XA1,XA2),(XB1,XB2),XC1N,XC2N),
    append(BR,BL,BN),
    append(AR,AL,AN),
    merge_ilse(AN,BN,RIL).

merge_ilse([A|AL],[B|BL],[(FA,XA)|RIL]):-
    getf(A,FA,a,AR,XA),
    getf(B,FB,b,_,_),
    FA<FB,
    append(AR,AL,AN),
    merge_ilse(AN,[B|BL],RIL).

merge_ilse([A|AL],[B|BL],[(FB,XB)|RIL]):-
    getf(A,FA,a,_,_),
    getf(B,FB,b,BR,XB),
    FB<FA,
    append(BR,BL,BN),
    merge_ilse([A|AL],BN,RIL).

update_xc((1,0),(0,1),1:[a],1:[b]).
update_xc((0,1),(1,0),1:[b],1:[a]).
update_xc((1,0),(1,0),2:[a,b],0:[]).
update_xc((0,1),(0,1),0:[],2:[a,b]).

%get the first or the last item of an interval (used above)
getf([A,B],A,[[B]],(1,0)).
getf([A],A,[],(0,1)).
getf([A,B],A,N,[[B]],(1:[N],0:[])).
getf([A],A,N,[],(0:[],1:[N])).


%merge se, from two instant lists create a single one that has
%triples (instant,membershipinfirst, membershipinsecond)
merge_se([A|SP],[B|EP],[(A,1,1)|R]):-
    B=A,merge_se(SP,EP,R).
merge_se([A|SP],[B|EP],[(A,1,0)|R]):-
    A<B,!,merge_se(SP,[B|EP],R).
merge_se([A|SP],[B|EP],[(B,0,1)|R]):-
    B<A,!,merge_se([A|SP],EP,R).
merge_se([],[B|EP],[(B,0,1)|R]):-merge_se([],EP,R).
merge_se([A|SP],[],[(A,1,0)|R]):-merge_se(SP,[],R).
merge_se([],[],[]).

%returns the variables of the first list that are not in the second
variable_list_diff([A|T],BL,[A|Td]):- \+vmember(A,BL),variable_list_diff(T,BL,Td).
variable_list_diff([A|T],BL,Td):- vmember(A,BL),variable_list_diff(T,BL,Td).
variable_list_diff([],_,[]).

variable_list_intersection([A|T],BL,[A|Td]):- vmember(A,BL),variable_list_intersection(T,BL,Td).
variable_list_intersection([A|T],BL,Td):- \+vmember(A,BL),variable_list_intersection(T,BL,Td).
variable_list_intersection([],_,[]).

%checks if a variable is in the list of variables (%TODO CAUTION)
vmember(X,[A|_]):-X==A.
vmember(X,[A|Tail]):- \+(X==A),vmember(X,Tail).

%finds the maximum of a list
mmax_member(A,[A]).
mmax_member(A,[A|R]):-
    mmax_member(RM,R),
    A>=RM.
mmax_member(RM,[A|R]):-
    mmax_member(RM,R),
    A<RM.

%useful for treating instants and intervals the same way in
%temporal relations computation
temporal_information([TS,TE],TS,TE,[]).
temporal_information(([TS,TE],BTSL),TS,TE,BTSL).
temporal_information(T,T,T,[]):- \+is_list(T).


%used in before computation
get_same_ts_sublist(_,[],[],[]):-!.
get_same_ts_sublist(B1,[B2|R],[B2|I],RI):-
    temporal_information(B1,T,_,_),
    temporal_information(B2,T,_,_),!,
    get_same_ts_sublist(B1,R,I,RI).

get_same_ts_sublist(B1,[B2|R],[],[B2|R]):-
    temporal_information(B1,T1,_,_),
    temporal_information(B2,T2,_,_),
    T1\=T2.

%merges ordered lists of nd intervals
ord_merge([], Merge, Merge).
ord_merge([H1|T1], L2, Merge):-
    ord_merge2(L2, H1, T1, Merge).

ord_merge2([], H1, T1, [H1|T1]).
ord_merge2([H2|T2], H1, T1, Merge):-
    interval_compare(Order, H1, H2),
    ord_merge3(Order, H1, T1, H2, T2, Merge).

ord_merge3(<, H1, T1, H2, T2, [H1|Merge]) :-
    ord_merge2(T1, H2, T2, Merge).
ord_merge3(=, H1, T1, _H2, T2, [H1|Merge]) :-
    ord_merge(T1, T2, Merge).
ord_merge3(>, H1, T1, H2, T2, [H2|Merge]) :-
    ord_merge2(T2, H1, T1, Merge).


interval_compare(<,I1,I2):-
    temporal_information(I1,TS1,_TE1,_),
    temporal_information(I2,TS2,_TE2,_),
    lt(TS1,TS2).
interval_compare(<,I1,I2):-
    temporal_information(I1,TS,TE1,_),
    temporal_information(I2,TS,TE2,_),
    lt(TE1,TE2).
interval_compare(>,I1,I2):-
    temporal_information(I1,TS1,_TE1,_),
    temporal_information(I2,TS2,_TE2,_),
    gt(TS1,TS2).
interval_compare(>,I1,I2):-
    temporal_information(I1,TS,TE1,_),
    temporal_information(I2,TS,TE2,_),
    gt(TE1,TE2).
interval_compare(=,I1,I2):-
    temporal_information(I1,TS,TE,_),
    temporal_information(I2,TS,TE,_).

min(A,B,A):-leq(A,B),!.
min(A,B,B):-lt(B,A).

%create the intervals for current B interval
create_intervals(_,_,_,[],[],[],_):-!.
create_intervals(Type,[B|BI],BALL,[(u,A)|AI],I,RemainingA,Tcrit):-
    !,temporal_information(A,ATS,_,_),
    temporal_information(B,BTS,_,_),
    min(ATS,BTS,TS),
    create_intervals(Type,BI,BALL,[A|AI],II,RemainingA,Tcrit),
    ord_merge([[TS,unk]],II,I).
create_intervals(Type,[B|BI],BALL,[A|AI],I,RemainingA,Tcrit):-
    temporal_information(B,BTS,BTE,_V),
    temporal_information(A,ATS,_,_),
    min(ATS,BTS,TS),
    create_intervals(Type,BI,BALL,[A|AI],II,RemainingAL,Tcrit),
    (
      (
        Type=keep,!,
        add_if_not_redundant((A,[BTS]),[TS,BTE],Tcrit,RemainingAL,RemainingA)
      )
      ;
      (
        Type\=keep,
        add_if_not_redundant(A,[TS,BTE],Tcrit,RemainingAL,RemainingA)
      )
    ),
    ord_merge([[TS,BTE]],II,I).
create_intervals(Type,[],BALL,[_|AI],I,RemainingA,Tcrit):-
    create_intervals(Type,BALL,BALL,AI,I,RemainingA,Tcrit).

formula_is(instant,d).
formula_is(dinterval,d).
formula_is(ndinterval,nd).

formulae_are(d,d,d).
formulae_are(nd,d,nd).
formulae_are(d,nd,nd).
formulae_are(nd,nd,nd).

formulae_ints_type(X,Y,Type):-
    formula_is(X,XI),
    formula_is(Y,YI),
    formulae_are(XI,YI,Type).

setof_empty(V,F,L):-
    setof(V,F,L) *-> true; L = [].

split_on_t(_,[],[],[]).
split_on_t(Tqmw,[[TS,TE]|TailLIL],[[TS,Tqmw]|TailLILbeforeTqmw],[[Tqmw,TE]|TailLILafterTqmw]):-
    Tqmw>TS,Tqmw<TE,
    split_on_t(Tqmw,TailLIL,TailLILbeforeTqmw,TailLILafterTqmw).
split_on_t(Tqmw,[[TS,TE]|TailLIL],[[TS,TE]|TailLILbeforeTqmw],TailLILafterTqmw):-
    TE=<Tqmw,
    split_on_t(Tqmw,TailLIL,TailLILbeforeTqmw,TailLILafterTqmw).
split_on_t(Tqmw,[[TS,TE]|TailLIL],TailLILbeforeTqmw,[[TS,TE]|TailLILafterTqmw]):-
    TS>=Tqmw,
    split_on_t(Tqmw,TailLIL,TailLILbeforeTqmw,TailLILafterTqmw).

splice_interval_sets([],[],[]).
splice_interval_sets([A|B],[],[A|B]).
splice_interval_sets([],[A|B],[A|B]).
splice_interval_sets([[ATS,T]|_],[[T,BTE]|IL],[[ATS,BTE]|IL]).
splice_interval_sets([[ATS,ATE]|_],[[BTS,BTE]|IL],[[ATS,ATE],[BTS,BTE]|IL]):-ATE\=BTS.

splice_interval_sets_drop([],[],[]).
splice_interval_sets_drop([_A|_B],[],[]).
splice_interval_sets_drop([],[A|B],[A|B]).
splice_interval_sets_drop([[ATS,T]|_],[[T,BTE]|IL],[[ATS,BTE]|IL]).
splice_interval_sets_drop([[ATS,ATE]|_],[[BTS,BTE]|IL],[[ATS,ATE],[BTS,BTE]|IL]):-ATE\=BTS.

splice_data_interval_sets_drop([],[],[]).
splice_data_interval_sets_drop([_A|_B],[],[]).
splice_data_interval_sets_drop([],[A|B],[A|B]).
splice_data_interval_sets_drop([[ATS,(T,_)]|_],[[(T,_),BTE]|IL],[[ATS,BTE]|IL]).
splice_data_interval_sets_drop([[ATS,(ATE,ATED)]|_],[[(BTS,BTSD),BTE]|IL],[[ATS,(ATE,ATED)],[(BTS,BTSD),BTE]|IL]):-ATE\=BTS.

strip_data_from_intervals([],[]).
strip_data_from_intervals([[(T1,_),(T2,_)]|R],[[T1,T2]|Z]):-strip_data_from_intervals(R,Z).


%returns the last ending interval(s) with an instant 
%not equal to inf or unk, plus all the intervals that end with an
%inf or unk
%e.g., for an input [[1,2],[1,3],[1,inf],[2,3]]
%it will return [[1,3],[1,inf],[2,3]]
last_ending_temporal_entities([],[]).
last_ending_temporal_entities([A|R],LastEntities):-
    temporal_information(A,_TSA,TEA,_),
    is_inf_unk(TEA),
    last_ending_temporal_entities(R,LastEntities).
last_ending_temporal_entities([A|R],LastEntities):-
    temporal_information(A,_TSA,TEA,_),
    \+is_inf_unk(TEA),
    last_ending_temporal_entities(R,LastEntitiesI),
    (
        (LastEntitiesI=[],LastEntities=[A])
        ;
        (
            LastEntitiesI=[AI|_],
            temporal_information(AI,_TSAI,TEAI,_),
            is_inf_unk(TEAI),
            LastEntities=[A]
        )
        ;
        (
            LastEntitiesI=[AI|AItail],
            temporal_information(AI,_TSAI,TEAI,_),
            \+is_inf_unk(TEAI),
            unk_or_inf_temporal_entities(AItail,AItailUI),
            gt(TEA,TEAI),
            LastEntities=[A|AItailUI]
        )
        ;
        (
            LastEntitiesI=[AI|_],
            temporal_information(AI,_TSAI,TEAI,_),
            \+is_inf_unk(TEAI),
            lt(TEA,TEAI),
            LastEntities=LastEntitiesI
        )
        ;
        (
            LastEntitiesI=[AI|_],
            temporal_information(AI,_TSAI,TEAI,_),
            \+is_inf_unk(TEAI),
            TEA=TEAI,
            LastEntities=[A|LastEntitiesI]
        )
    ).

unk_or_inf_temporal_entities(L,Linfunk):-
    include(temporal_entity_has_inf_unk,L,Linfunk).

add_if_not_redundant(AP,[_,TE],Tcrit,RemainingAL,[AP|RemainingAL]):-
    gt(TE,Tcrit),
    temporal_information(AP,_,TAPE,_),
    leq(TAPE,Tcrit),!.
add_if_not_redundant(_AP,[_,_TE],_Tcrit,RemainingAL,RemainingAL).
    %\+((gt(TE,Tcrit),get_end(AP,TAPE),leq(TAPE,Tcrit))).

is_inf_unk(inf).
is_inf_unk(unk).
interval_has_unk([_,unk]).
temporal_entity_has_inf_unk(A):-
    temporal_information(A,_TSA,TEA,_),
    is_inf_unk(TEA).

lt(A,unk):-number(A),!.
lt(unk,inf):-!.
lt(A,inf):-number(A),!.
lt(A,B):-number(A),number(B),A<B.

gt(inf,unk):-!.
gt(inf,B):-number(B),!.
gt(unk,B):-number(B),!.
gt(A,B):-number(A),number(B),A>B.

leq(A,B):-A=B,!.
leq(A,B):-lt(A,B).

geq(A,B):-A=B,!.
geq(A,B):-gt(A,B).


instant_complement(StartTime,EndTime,Formula,T):-
    StartTime=<EndTime,
    T=StartTime,
    \+Formula.
instant_complement(StartTime,EndTime,Formula,T):-
    StartTime=<EndTime,
    StartTime1 is StartTime+1,
    instant_complement(StartTime1,EndTime,Formula,T).

create_window_instants(StartTime,EndTime,[]):-
    StartTime>EndTime,!.
create_window_instants(StartTime,EndTime,[StartTime|R]):-
    StartTime1 is StartTime+1,
    create_window_instants(StartTime1,EndTime,R).


assert_if_not_exists(X):-
    X-> true ; assertz(X).

merge_temporal_information_lists(instant, InputList, OutputList):-
    merge_instant_lists(InputList,OutputList).
merge_temporal_information_lists(dinterval, InputList, OutputList):-
    merge_disjoint_interval_lists(InputList, OutputList).
merge_temporal_information_lists(ndinterval, InputList, OutputList):-
    merge_non_disjoint_interval_lists(InputList, OutputList).


merge_instant_lists([],[]).
merge_instant_lists([A|Tail],Merged):-
    is_list(A),
    merge_instant_lists(Tail,TailMerged),
    ord_union(A,TailMerged,Merged).
merge_instant_lists([A|Tail],[A|Tail]):-
    \+is_list(A).

merge_disjoint_interval_lists([],[]).
merge_disjoint_interval_lists([A|Tail],Union):-
    merge_disjoint_interval_lists(Tail,TailUnion),
    merge_ilse(A,TailUnion,SEL),
    compute_union_intervals(SEL,0,0,_,Union).

merge_non_disjoint_interval_lists([],[]).
merge_non_disjoint_interval_lists([A|Tail],Merged):-
    merge_non_disjoint_interval_lists(Tail,TailMerged),
    ord_merge(A,TailMerged,Merged).

ground_check(A,yes):-
    ground(A),!.
ground_check(A,no):-
    \+ground(A).


phe_setval(X,C):-
    ((val(X,_),!,retract(val(X,_)));
     (\+val(X,_))),
    assertz(val(X,C)).
phe_getval(X,C):-
    val(X,C).

clean_from_unk(L,Lcleaned):-
    exclude(interval_has_unk,L,Lcleaned).

remaining([],_,[]).
remaining([[TS,TE]|IL],T,[[TS,TE]|RIL]):-
    TE > T,
    remaining(IL,T,RIL).
remaining([[_TS,TE]|IL],T,RIL):-
    TE =< T,
    remaining(IL,T,RIL).

%old_code
%ord_merge([],[],[]):-!.
%ord_merge(I,[],I):-I\=[],!.
%ord_merge([],I,I):-I\=[],!.

%ord_merge([A|AR],[B|BR],[A|ABR]):-
    %temporal_information(A,TSA,_TEA,_),
    %temporal_information(B,TSB,_TEB,_),
    %lt(TSA,TSB),!,
    %ord_merge(AR,[B|BR],ABR).
%ord_merge([A|AR],[B|BR],[B|ABR]):-
    %temporal_information(A,TSA,_TEA,_),
    %temporal_information(B,TSB,_TEB,_),
    %gt(TSA,TSB),!,
    %ord_merge([A|AR],BR,ABR).
%ord_merge([A|AR],[B|BR],[A|ABR]):-
    %temporal_information(A,TSA,TEA,_),
    %temporal_information(B,TSB,TEB,_),
    %TSA=TSB,
    %lt(TEA,TEB),!,
    %ord_merge(AR,[B|BR],ABR).
%ord_merge([A|AR],[B|BR],[B|ABR]):-
    %temporal_information(A,TSA,TEA,_),
    %temporal_information(B,TSB,TEB,_),
    %TSA=TSB,
    %gt(TEA,TEB),!,
    %ord_merge([A|AR],BR,ABR).
%ord_merge([A|AR],[B|BR],[B|ABR]):-
    %temporal_information(A,TSA,TEA,[]),
    %temporal_information(B,TSB,TEB,[]),
    %TSA=TSB,
    %TEA=TEB,!,
    %ord_merge(AR,BR,ABR).
%ord_merge([A|AR],[B|BR],[([TSA,TEA],V3)|ABR]):-
    %temporal_information(A,TSA,TEA,V1),
    %temporal_information(B,TSB,TEB,V2),
    %append(V1,V2,V3),
    %TSA=TSB,
    %TEA=TEB,!,
    %ord_merge(AR,BR,ABR).


