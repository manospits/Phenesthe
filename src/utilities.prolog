% Author: Manolis Pitsikalis
%
% Utilities used in different parts
%
:-dynamic val/2.

% merge two interval lists into one that has instants and information
% regarding if that instant is a start or an end of an interval
merge_ilse([],[],[]).
merge_ilse([],[([B1,B2],V)|BL],[(B1,(1:[(b,V)],0:[]))|RIL]):-
    merge_ilse([],[([B2],V)|BL],RIL).
merge_ilse([],[([B2],V)|BL],[(B2,(0:[],1:[(b,V)]))|RIL]):-
    merge_ilse([],BL,RIL).
merge_ilse([([A1,A2],V)|AL],[],[(A1,(1:[(a,V)],0:[]))|RIL]):-
    merge_ilse([([A2],V)|AL],[],RIL).
merge_ilse([([A2],V)|AL],[],[(A2,(0:[],1:[(a,V)]))|RIL]):-
    merge_ilse(AL,[],RIL).
merge_ilse([(A,Va)|AL],[(B,Vb)|BL],[(FA,(XC1N,XC2N))|RIL]):-
    getf(A,Va,FA,AR,(XA1,XA2)),
    getf(B,Vb,FB,BR,(XB1,XB2)),
    FA=FB,
    update_xc((XA1,XA2),(XB1,XB2),Va,Vb,XC1N,XC2N),
    append(BR,BL,BN),
    append(AR,AL,AN),
    merge_ilse(AN,BN,RIL).

merge_ilse([(A,Va)|AL],[(B,Vb)|BL],[(FA,XA)|RIL]):-
    getf(A,Va,FA,a,AR,XA),
    getf(B,Vb,FB,b,_,_),
    FA<FB,
    append(AR,AL,AN),
    merge_ilse(AN,[(B,Vb)|BL],RIL).

merge_ilse([(A,Va)|AL],[(B,Vb)|BL],[(FB,XB)|RIL]):-
    getf(A,Va,FA,a,_,_),
    getf(B,Vb,FB,b,BR,XB),
    FB<FA,
    append(BR,BL,BN),
    merge_ilse([(A,Va)|AL],BN,RIL).

%merge se, from two instant lists create a single one that has
%triples (instant,membershipinfirst, membershipinsecond)
merge_se([(A,Va)|SP],[(B,Vb)|EP],[(A,1:[Va],1:[Vb])|R]):-
    B=A,merge_se(SP,EP,R).
merge_se([(A,Va)|SP],[(B,Vb)|EP],[(A,1:[Va],0:[])|R]):-
    A<B,!,merge_se(SP,[(B,Vb)|EP],R).
merge_se([(A,Va)|SP],[(B,Vb)|EP],[(B,0:[],1:[Vb])|R]):-
    B<A,!,merge_se([(A,Va)|SP],EP,R).
merge_se([],[(B,Vb)|EP],[(B,0:[],1:[Vb])|R]):-merge_se([],EP,R).
merge_se([(A,Va)|SP],[],[(A,1:[Va],0:[])|R]):-merge_se(SP,[],R).
merge_se([],[],[]).


update_xc((1,0),(0,1),Va,Vb,1:[(a,Va)],1:[(b,Vb)]).
update_xc((0,1),(1,0),Va,Vb,1:[(b,Vb)],1:[(a,Va)]).
update_xc((1,0),(1,0),Va,Vb,2:[(a,Va),(b,Vb)],0:[]).
update_xc((0,1),(0,1),Va,Vb,0:[],2:[(a,Va),(b,Vb)]).

%get the first or the last item of an interval (used above)
getf([A,B],V,A,[([B],V)],(1,0)).
getf([A],_V,A,[],(0,1)).

getf([A,B],V,A,N,[([B],V)],(1:[(N,V)],0:[])).
getf([A],V,A,N,[],(0:[],1:[(N,V)])).



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

member_rem(A, [A|R], R).
member_rem(A, [_|R], R1):- member_rem(A,R,R1). 


%merges ordered lists of nd intervals
ord_merge([], Merge, Merge).
ord_merge([H1|T1], L2, Merge):-
    ord_merge2(L2, H1, T1, Merge).

ord_merge2([], H1, T1, [H1|T1]).
ord_merge2([H2|T2], H1, T1, Merge):-
    interval_compare(Order, H1, H2, H3),
    ord_merge3(Order, H1, T1, H2, T2, H3, Merge).

ord_merge3(<, H1, T1, H2, T2, _, [H1|Merge]) :-
    ord_merge2(T1, H2, T2, Merge).
ord_merge3(=, _H1, T1, _H2, T2, H3, [H3|Merge]) :-
    ord_merge(T1, T2, Merge).
ord_merge3(>, H1, T1, H2, T2, _, [H2|Merge]) :-
    ord_merge2(T2, H1, T1, Merge).


interval_compare(<,([TSa,_TEa],_V1),([TSb,_TEb],_V2),_V3):-
    TSa < TSb,!.
interval_compare(<,([TS,TEa],_V1),([TS,TEb],_V2),_V3):-
    TEa < TEb,!.
interval_compare(>,([TSa,_TEa],_V1),([TSb,_TEb],_V2),_V3):-
    TSa > TSb,!.
interval_compare(>,([TS,TEa],_V1),([TS,TEb],_V2),_V3):-
    TEa > TEb,!.
interval_compare(=,([TS,TE],V1),([TS,TE],V2),([TS,TE],V3)):-
    (member(t,[V1,V2]) -> V3 = t ; V3=u),!.

interval_compare(<,([TSa,_TEa],_V1,_),([TSb,_TEb],_V2,_),_V3):-
    TSa < TSb,!.
interval_compare(<,([TS,TEa],_V1,_),([TS,TEb],_V2,_),_V3):-
    TEa < TEb,!.
interval_compare(>,([TSa,_TEa],_V1,_),([TSb,_TEb],_V2,_),_V3):-
    TSa > TSb,!.
interval_compare(>,([TS,TEa],_V1,_),([TS,TEb],_V2,_),_V3):-
    TEa > TEb,!.
interval_compare(=,([TS,TE],V1,Z1),([TS,TE],V2,Z2),([TS,TE],V3,Z)):-
    (member(t,[V1,V2]) -> (V3 = t, Z=Z1 ); (V3=u, Z=Z2)),!.

min(A,B,A):-leq(A,B),!.
min(A,B,B):-lt(B,A).

max(A,B,B):-leq(A,B),!.
max(A,B,A):-lt(B,A).


get_same_ts_sublist(_,[],[]).
get_same_ts_sublist(TS,[([TS,TE],V)|R],[([TS,TE],V)|R1]):-
    get_same_ts_sublist(TS,R,R1).
get_same_ts_sublist(TS,[([TS1,_],_)|_],[]):-TS1\=TS.



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




is_inf(inf).


lt(A,inf):-number(A),!.
lt(A,B):-number(A),number(B),A<B.

gt(inf,B):-number(B),!.
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

create_window_instants(StartTime,EndTime,_,[]):-
    StartTime>EndTime,!.
create_window_instants(StartTime,EndTime,V,[(StartTime,V)|R]):-
    StartTime1 is StartTime+1,
    create_window_instants(StartTime1,EndTime,V,R).


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
    compute_disjunction(A,TailMerged,Merged).
merge_instant_lists([A|Tail],[A|Tail]):-
    \+is_list(A).

merge_disjoint_interval_lists([],[]).
merge_disjoint_interval_lists([A|Tail],Union):-
    merge_disjoint_interval_lists(Tail,TailUnion),
    merge_ilse(A,TailUnion,SEL),
    compute_union_intervals(SEL,0,0,0,_,Union).

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

merge_wt_with_unk([],[],[]):-!.
merge_wt_with_unk([],I,I):-!.
merge_wt_with_unk(I,[],I):-!.
merge_wt_with_unk([(T,t)|R1],[(T,u)|R2],[(T,u)|R]):-
    merge_wt_with_unk(R1,R2,R),!.
merge_wt_with_unk([(T,t)|R1],[(T1,u)|R2],[(T1,u)|R]):-
    T1 < T,
    merge_wt_with_unk([(T,t)|R1],R2,R),!.
merge_wt_with_unk([(T,t)|R1],[(T1,u)|R2],[(T,t)|R]):-
    T1 > T,
    merge_wt_with_unk(R1,[(T1,u)|R2],R),!.

%conjunction for true
%and unknown
e_and(t,t,t).
e_and(u,u,u).
e_and(u,t,u).
e_and(t,u,u).
    
% relations utilities
start_same([TS,_],[([TS,_],_)|_]).

split_true_unknown([],[],[]).
split_true_unknown([(V,t)|R],[(V,t)|Rt],Ru):-!,
    split_true_unknown(R,Rt,Ru).
split_true_unknown([(V,u)|R],Rt,[[(V,u)]|Ru]):-!,
    split_true_unknown(R,Rt,Ru).
split_true_unknown([(V,t,Z)|R],[(V,t,Z)|Rt],Ru):-!,
    split_true_unknown(R,Rt,Ru).
split_true_unknown([(V,u,_Z)|R],Rt,[[(V,u)]|Ru]):-!,
    split_true_unknown(R,Rt,Ru).

remove_source([],[]).
remove_source([(I,V,_S)|R],[(I,V)|R1]):-
    remove_source(R,R1).
remove_source([(I,V)|R],[(I,V)|R1]):-
    remove_source(R,R1).

split_paired_list([],[],[]).
split_paired_list([(A,B)|R],[A|RA],[B|RB]):-
    split_paired_list(R,RA,RB).

int_member([TSa,TEa], [[TSa,TEa]|_R], Tc):- TSa < Tc.
int_member(_, [[TSa,_TEa]|_R], Tc):- TSa >= Tc,!,fail.
int_member(Z, [_|R], Tc):- int_member(Z, R, Tc).

sublist_intervals_starting([],_,[]).
sublist_intervals_starting([[Z,_B]|_RA],A,[]):-Z\=A.
sublist_intervals_starting([[A,B]|RA],A,[[A,B]|R]):-
    sublist_intervals_starting(RA,A,R).

tunion(A, I):- merge_disjoint_interval_lists(A,I).

convert_points_to_pis([],[]).
convert_points_to_pis([([A,B],V)|R],[([A,B],V)|R]):-!.
convert_points_to_pis([(A,V)|R1],[([A,A],V)|R2]):-
    convert_points_to_pis(R1,R2).

find_tcrit_from_results([],Z,Z).
find_tcrit_from_results([([TS,_TE],u)|R],Z1,Z):-!,
    %V = unknown
    TS < Z1 -> find_tcrit_from_results(R,TS,Z);
    find_tcrit_from_results(R,Z1,Z).
find_tcrit_from_results([([TS,inf],t)|R],Z1,Z):-!,
    %V = t 
    TS < Z1 -> find_tcrit_from_results(R,TS,Z);
    find_tcrit_from_results(R,Z1,Z).
find_tcrit_from_results([([_TS,TE],t)|R],Z1,Z):-!,
    %V = t 
    TE\=inf,
    find_tcrit_from_results(R,Z1,Z).
find_tcrit_from_results([(T,u)|R],Z1,Z):-!,
    %V = unknown
    T < Z1 -> find_tcrit_from_results(R,T,Z);
    find_tcrit_from_results(R,Z1,Z).
find_tcrit_from_results([(_T,t)|R],Z1,Z):-!,
    %V = t 
    find_tcrit_from_results(R,Z1,Z).

