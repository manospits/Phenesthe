% Author: Manolis Pitsikalis
%
% Temporal relations computation

:-discontiguous compute_before_intervals/6.

compute_relation_intervals(before,A,B,Tq,IL):-compute_before_intervals(A,B,Tq,IL). %
compute_relation_intervals(meets,A,B,Tq,IL):-compute_arelation_intervals(meets,A,B,Tq,IL). %
compute_relation_intervals(overlaps,A,B,Tq,IL):-compute_arelation_intervals(overlaps,A,B,Tq,IL). %
compute_relation_intervals(starts,A,B,Tq,IL):-compute_arelation_intervals(starts,A,B,Tq,IL). %
compute_relation_intervals(contains,A,B,Tq,IL):-compute_brelation_intervals(contains,A,B,Tq,IL). %
compute_relation_intervals(finishes,A,B,Tq,IL):-compute_arelation_intervals(finishes,A,B,Tq,IL). %
compute_relation_intervals(equals,A,B,Tq,IL):-compute_arelation_intervals(equals,A,B,Tq,IL). %


compute_before_intervals(A,B,Tq,I):-
    compute_before_intervals(A,[],B,Tq,I1),
    split_true_unknown(I1,It,Iu),
    tunion(Iu,Iuu),
    ord_merge(It,Iuu,I).

%----------- OUTER ----------------
compute_before_intervals([],_,[],_,[]).
compute_before_intervals(A,_UBS,[],Tq,I):-
    Tq2 is Tq + 2,
    Tq1 is Tq + 1,
    compute_before_intervals(A,[Tq1],[Tq2,inf],_Ar,Tq,I).

compute_before_intervals(A,UBS,[(B,t)|Rb], Tq, I):-
    compute_before_intervals(A, UBS, B, Ar, Tq, Ic),
    (start_same(B,Rb) -> compute_before_intervals(A, UBS, Rb, Tq, Ir) ; compute_before_intervals(Ar, UBS, Rb, Tq, Ir)),
    append(Ic,Ir,Is),
    sort(Is,I).
compute_before_intervals(A,UBS,[([TS,_],u)|Rb], Tq, I):-
    compute_before_intervals(A, [TS|UBS], Rb, Tq , I).

%-------------- INNER ----------------------

% assuming b true
compute_before_intervals(A,UBS,[TSb,TEb], ARemaining, Tq, I):-
    tur_before(A,TSb,Tq,RSBeforeTrueI,SBeforeUnknownI,ARemaining),
    % if RSBeforeTrueI = RSBeforeUnknownI = [] -> use next Bi
    % if RSBeforeTrueI = [], RSBeforeUnknownI /= [] -> create unknown intervals 
    % if RSBeforeTrueI /= [], RSBeforeUnknownI = [] -> create true intervals 
    % if RSBeforeTrueI /= [], RSBeforeUnknownI /= [] -> check whether true or unknown status  
    ((RSBeforeTrueI = [],SBeforeUnknownI = []) -> I=[] ;(
    (RSBeforeTrueI \= [] -> 
        (
            sort(0,@>,RSBeforeTrueI,BeforeTrueI),
            % find the one 
            BeforeTrueI=[[TEx,TSx]|_]
        );
        (
            BeforeTrueI = [],
            TEx=0,TSx=TSb
        )
    ),
    TSb1 is TSb-1,
    ((member(Tu, UBS),Tu > TEx, Tu < TSb) -> Vt = u ; Vt = t),
    sublist_intervals_starting(BeforeTrueI,TEx,LBeforeTrueI),
    %if exists an interval that starts earlier and finishes later (possibly) than TEx
    %then unknown interval should be created
    (
        setof([TSa,TEa],(int_member([TSa,TEa], SBeforeUnknownI, TSb1), TEa >= TEx),BeforeUnknownI) ->  
        (
            (
                %if below true create an interval that has unknown status and starts
                % at the earliest TSau that satisfies constraints below and above
                (member([TS,_],BeforeUnknownI),TS < TSx, TEx \= TSb1) ->            
                (
                    I = [([TS,TEb],u)],!
                )
                ;
                (                 
                % if not create Vt (check also if there are unknown from b) intervals,
                % with the true intervals of A and unknown intervals
                % with the unknown intervals of A that satisfy the before constraints
                    findall(([TS,TEb],Vt), (member([_,TS],LBeforeTrueI)),I1),
                    setof(([TS,TEb],u), (member([TS,_],BeforeUnknownI)),[Iu|_]),
                    append(I1,[Iu],Ius),
                    sort(Ius,I),!
                )
            )
         )
         ;
         (
            % if no unknown interval create true intervals
            findall(([TS,TEb],Vt), member([TEx,TS],LBeforeTrueI),I1),
            sort(I1,I)
         )
    ))).

% a relation -> for each b process a
compute_arelation_intervals(Relation, A, B, Tq, I):-
    compute_arelation_intervals1(Relation, A,B,Tq,I1),
    split_true_unknown(I1,It,Iu),
    tunion(Iu,Iuu),
    ord_merge(It,Iuu,I).

%----------- OUTER ----------------
compute_arelation_intervals1(_,[],[],_,[]):-!.
compute_arelation_intervals1(_,[],_,_,[]):-!.
compute_arelation_intervals1(Relation, A, [], Tq, I):-!,
    Tq1 is Tq + 1,
    compute_arelation_intervals1(Relation,A,([Tq1,inf],u),_Ar,Tq,I).

compute_arelation_intervals1(Relation, A,[(B,V)|Rb], Tq, I):-
    compute_arelation_intervals1(Relation, A, (B,V), Ar, Tq, Ic),
    compute_arelation_intervals1(Relation, Ar, Rb, Tq, Ir),
    ord_merge(Ic,Ir,I).

%-------------- INNER ----------------------
% assuming b true
compute_arelation_intervals1(Relation, A, ([TSb,TEb],V), ARemaining, Tq, I):-
    tur(Relation, A,([TSb,TEb],V),Tq,ARelationTrueI,ARelationUnknownI,ARemaining),
    create_intervals(Relation, ARelationTrueI, ARelationUnknownI, ([TSb,TEb],V), TI, UI),
    ord_merge(TI,UI,IS),
    sort(IS,I).

% b relation -> for each a process b
compute_brelation_intervals(Relation, A,B,Tq,I):-
    compute_brelation_intervals1(Relation, B,A,Tq,I1),
    split_true_unknown(I1,It,Iu),
    tunion(Iu,Iuu),
    ord_merge(It,Iuu,I).

%----------- OUTER ----------------
compute_brelation_intervals1(_,[],[],_,[]):-!.
compute_brelation_intervals1(_,_,[],_,[]):-!.
compute_brelation_intervals1(Relation,[],[(B,V)|Rb],Tq,I):-!,
    Tq1 is Tq + 1,
    compute_brelation_intervals1(Relation,[([Tq1,inf],u)],[(B,V)|Rb],Tq,I).


compute_brelation_intervals1(Relation,A,[(B,V)|Rb], Tq, I):-
    compute_brelation_intervals1(Relation,A, (B,V), Ar, Tq, Ic),
    compute_brelation_intervals1(Relation,Ar, Rb, Tq, Ir),
    ord_merge(Ic,Ir,I).

%-------------- INNER ----------------------

% assuming b true
compute_brelation_intervals1(Relation,A,([TSb,TEb],V), ARemaining, Tq, I):-
    tur(Relation, A, ([TSb,TEb],V),Tq,BRelationTrueI,BRelationUnknownI,ARemaining),
    create_intervals(Relation,BRelationTrueI,BRelationUnknownI,([TSb,TEb],V),TI,UI),
    ord_merge(TI,UI,IS),
    sort(IS,I).

create_intervals(_, [], [], _, [],[]).
%-------------------------------- equals
create_intervals(equals, True, _Unk, ([TSb,TEb],t), [([TSb,TEb],t)],[]):-
    True \= [].
create_intervals(equals, [], Unk, ([TSb,TEb],t), [], [([TSb,TEb],u)]):-
    Unk \= [].
create_intervals(equals, [], Unk, ([TSb,TEb],u), [], UI):-
    Unk \= [],
    findall(([TSo,TEo],u),(member([TSx,TEx],Unk), max(TSx,TSb,TSo), min(TEx,TEb,TEo)),UIs),sort(UIs,UI).

%-------------------------------- starts finishes
create_intervals(R, True, _Unk, ([TSb,TEb],t), [([TSb,TEb],t)],[]):-
    member(R,[starts,finishes]),
    True \= [],!.
create_intervals(R, [], Unk, ([TSb,TEb],t), [], [([TSb,TEb],u)]):-
    member(R,[starts,finishes]),
    Unk \= [],!.
create_intervals(starts, [], Unk, ([TSb,TEb],u), [], UI):-
    Unk \= [],
    findall(([TSo,TEo],u),(member([TSx,_TEx],Unk),
                           max(TSx,TSb,TSo),
                           TEo=TEb),UIs),sort(UIs,UI).
create_intervals(finishes, [], Unk, ([TSb,TEb],u), [], UI):-
    Unk \= [],
    findall(([TSo,TEo],u),(member([_TSx,TEx],Unk),
                           min(TEx,TEb,TEo),
                           TSo=TSb),UIs),sort(UIs,UI).

%------------------------------- meets/overlaps 
create_intervals(R, True, _Unk, ([_TSb,TEb],t), TI,[]):-
    member(R,[meets,overlaps]),
    True\=[],!,
    findall(([TSx,TEb],t),member([TSx,_],True),TI).
create_intervals(R, [], [[TSa,_TEa]|_], ([_TSb,TEb],t), [], [([TSa,TEb],u)]):-
    member(R,[meets,overlaps]),!.
create_intervals(R, [], Unk, ([_TSb,TEb],u), [], UI):-
    member(R,[meets,overlaps]),
    Unk \= [],
    findall(([TSo,TEo],u),(member([TSx,TEx],Unk),
                           (TEx\=inf -> TEx1 is TEx+1; TEx1 = TEx),
                           min(TEx1,TEb,TEo),
                           TSo=TSx),UIs),sort(UIs,UI).

%------------------------------- contains 
create_intervals(contains, True, _Unk, ([TSb,TEb],t), [([TSb,TEb],t)],[]):-
    True\=[],!.
create_intervals(contains, [], [[_TSa,_TEa]|_], ([TSb,TEb],t), [], [([TSb,TEb],u)]):-!.
create_intervals(contains, [], Unk, ([_TSb,TEb],u), [], UI):-
    Unk \= [],
    findall( ([TSo,TEo],u),(member([TSo,TEx],Unk),
                           (TEx\=inf -> TEx1 is TEx+1; TEx1 = TEx),
                            min(TEb, TEx,TEo)),UI).


find_max_end([],Z,Z).
find_max_end([[_,Te]|R],Z,O):-
    Te  > Z -> find_max_end(R,Te,O) ; find_max_end(R,Z,O).
% ------- tur before ---------
% first list all true intervals before TSb --- [ts,te] is [te,ts]
% second list all unknown intervals before (relaxed) TSb --- [ts,te] remains
% third all intervals that can contribute to next TSb --- as original
tur_before([],_,_,[],[],[]).
tur_before([([TSa,TEa],t)|R],TSb,Tq,[[TEa,TSa]|TB],UB,RA):- TEa < TSb,
    tur_before(R,TSb,Tq,TB,UB,RA).
tur_before([([TSa,TEa],t)|R],TSb,Tq,TB,UB,[([TSa,TEa],t)|RA]):- TEa >= TSb, TEa \= inf,
    tur_before(R,TSb,Tq,TB,UB,RA).
tur_before([([TSa,TEa],t)|R],TSb,Tq,TB,UB,RA):- TEa >= TSb, TEa = inf,
    tur_before([([TSa,Tq],t)|R],TSb,Tq,TB,UB,RA).
tur_before([([TSa,TEa],u)|R],TSb,Tq,TB,[[TSa,TEa]|UB],RA):-TSb1 is TSb-1, TSa < TSb1,
    (TEa < TSb -> (RA = RA1) ; (RA = [([TSa,TEa],u)|RA1])),
    tur_before(R,TSb,Tq,TB,UB,RA1).
tur_before([([TSa,TEa],u)|R],TSb,Tq,TB,UB,[([TSa,TEa],u)|RA]):- TSb1 is TSb-1,TSa >= TSb1,
    tur_before(R,TSb,Tq,TB,UB,RA).

% ------- tur meets --------
% first list all true intervals meets TSb --- [ts,te] is [te,ts]
% second list all unknown intervals meets (relaxed) TSb --- [ts,te] remains
% third all intervals that can contribute to next TSb --- as original
tur(meets,[],_,_,[],[],[]).
tur(meets,[([TSa,TEa],t)|R],([TSb,TEb],t),Tq,[[TSa,TEa]|TB],UB,[([TSa,TEa],t)|RA]):-  TEa = TSb,!,
    tur(meets,R,([TSb,TEb],t),Tq,TB,UB,RA).
tur(meets,[([TSa,TEa],t)|R],([TSb,TEb],u),Tq,TB,[[TSa,TEa]|UB],[([TSa,TEa],t)|RA]):- TSa < TSb, TEa >= TSb,!,
    tur(meets,R,([TSb,TEb],u),Tq,TB,UB,RA).
tur(meets,[([_TSa,TEa],t)|R],([TSb,TEb],Vb),Tq,TB,UB,RA):- TEa < TSb,!,
    tur(meets,R,([TSb,TEb],Vb),Tq,TB,UB,RA).
tur(meets,[([TSa,TEa],t)|R],([TSb,TEb],Vb),Tq,TB,UB,[([TSa,TEa],t)|RA]):- TEa \= inf,!,
    tur(meets,R,([TSb,TEb],Vb),Tq,TB,UB,RA).
tur(meets,[([TSa,TEa],t)|R],([TSb,TEb],Vb),Tq,TB,UB,RA):- TEa = inf,!,Tq2 is Tq+2,
    tur(meets,[([TSa,Tq2],u)|R],([TSb,TEb],Vb),Tq,TB,UB,RA).
tur(meets,[([TSa,TEa],u)|R],([TSb,TEb],t),Tq,TB,UB,RA):- TSa < TSb,!,
    (TEa < TSb -> (UB = UB1, RA=RA1) ; (UB = [[TSa,TEa]|UB1] , RA = [([TSa,TEa],u)|RA1])),
    tur(meets,R,([TSb,TEb],t),Tq,TB,UB1,RA1).
tur(meets,[([TSa,TEa],u)|R],([TSb,TEb],u),Tq,TB,[[TSa,TEa]|UB],[([TSa,TEa],u)|RA]):- TSa < TEb, TSb =< TEa,!,
    tur(meets,R,([TSb,TEb],u),Tq,TB,UB,RA).
tur(meets,[([TSa,TEa],u)|R],([TSb,TEb],V),Tq,TB,UB,RA):-
    (TEa < TSb -> (RA=RA1) ; (RA = [([TSa,TEa],u)|RA1])),
    tur(meets,R,([TSb,TEb],V),Tq,TB,UB,RA1).

% ---------------- tur overlaps ------------------
% first list all true intervals overlaps TSb --- [ts,te] is [te,ts]
% second list all unknown intervals overlaps (relaxed) TSb --- [ts,te] remains
% third all intervals that can contribute to next TSb --- as original
tur(overlaps,[],_,_,[],[],[]).
tur(overlaps,[([TSa,TEa],t)|R],([TSb,TEb],t),Tq,[[TSa,TEa]|TB],UB,[([TSa,TEa],t)|RA]):- TSa < TSb, TEa > TSb, TEa < TEb,!,
    tur(overlaps,R,([TSb,TEb],t),Tq,TB,UB,RA).
tur(overlaps,[([TSa,TEa],t)|R],([TSb,TEb],u),Tq,TB,[[TSa,TEa]|UB],[([TSa,TEa],t)|RA]):- TEa > TSb, TEa < TEb,!,
    tur(overlaps,R,([TSb,TEb],u),Tq,TB,UB,RA).
tur(overlaps,[([_TSa,TEa],t)|R],([TSb,TEb],Vb),Tq,TB,UB,RA):- TEa =< TSb,!,
    tur(overlaps,R,([TSb,TEb],Vb),Tq,TB,UB,RA).
tur(overlaps,[([TSa,TEa],t)|R],([TSb,TEb],Vb),Tq,TB,UB,[([TSa,TEa],t)|RA]):- TEa \= inf,!,
    tur(overlaps,R,([TSb,TEb],Vb),Tq,TB,UB,RA).
tur(overlaps,[([TSa,TEa],t)|R],([TSb,TEb],Vb),Tq,TB,UB,RA):- TEa = inf,!,Tq2 is Tq+2,
    tur(overlaps,[([TSa,Tq2],u)|R],([TSb,TEb],Vb),Tq,TB,UB,RA).
tur(overlaps,[([TSa,TEa],t)|R],([TSb,TEb],Vb),Tq,TB,UB,RA):- TEa = inf,!,Tq2 is Tq+2,
    tur(overlaps,[([TSa,Tq2],u)|R],([TSb,TEb],Vb),Tq,TB,UB,RA).

tur(overlaps,[([TSa,TEa],u)|R],([TSb,TEb],t),Tq,TB,UB,RA):- TSa < TSb,!,
    (TEa =< TSb -> (UB = UB1, RA=RA1) ; (UB = [[TSa,TEa]|UB1] , RA = [([TSa,TEa],u)|RA1])),
    tur(overlaps,R,([TSb,TEb],t),Tq,TB,UB1,RA1).
tur(overlaps,[([TSa,TEa],u)|R],([TSb,TEb],u),Tq,TB,[[TSa,TEa]|UB],[([TSa,TEa],u)|RA]):- TSa < TEb, TSb < TEa,!,
    tur(overlaps,R,([TSb,TEb],u),Tq,TB,UB,RA).
tur(overlaps,[([TSa,TEa],u)|R],([TSb,TEb],V),Tq,TB,UB,RA):-
    (TEa =< TSb -> (RA=RA1) ; (RA = [([TSa,TEa],u)|RA1])),
    tur(overlaps,R,([TSb,TEb],V),Tq,TB,UB,RA1).

%----- tur starts
% first list all true intervals starts TSb --- [ts,te] is [te,ts]
% second list all unknown intervals starts (relaxed) TSb --- [ts,te] remains
% third all intervals that can contribute to next TSb --- as original
tur(starts,[],_,_,[],[],[]).
tur(starts,[([TSa,TEa],t)|R],([TSb,TEb],t),Tq,[[TSa,TEa]|TB],UB,[([TSa,TEa],t)|RA]):- TEa\=inf,  TSa = TSb, TEa < TEb,!,
    tur(starts,R,([TSb,TEb],t),Tq,TB,UB,RA).
tur(starts,[([TSa,TEa],t)|R],([TSb,TEb],u),Tq,TB,[[TSa,TEa]|UB],[([TSa,TEa],t)|RA]):- TEa\=inf, TSa >= TSb, TEa < TEb,!,
    tur(starts,R,([TSb,TEb],u),Tq,TB,UB,RA).
tur(starts,[([TSa,TEa],t)|R],([TSb,TEb],t),Tq,TB,[[TSa,TEa]|UB],[([TSa,TEa],t)|RA]):- TEa=inf,  TSa = TSb, TEa = TEb,!,
    tur(starts,R,([TSb,TEb],u),Tq,TB,UB,RA).
tur(starts,[([TSa,TEa],t)|R],([TSb,TEb],u),Tq,TB,[[TSa,TEa]|UB],[([TSa,TEa],t)|RA]):- TEa=inf, TSa >= TSb, TEa = TEb,!,
    tur(starts,R,([TSb,TEb],u),Tq,TB,UB,RA).
tur(starts,[([_TSa,TEa],t)|R],([TSb,TEb],Vb),Tq,TB,UB,RA):- TEa < TSb,!,
    tur(starts,R,([TSb,TEb],Vb),Tq,TB,UB,RA).
tur(starts,[([TSa,TEa],t)|R],([TSb,TEb],Vb),Tq,TB,UB,[([TSa,TEa],t)|RA]):-
    tur(starts,R,([TSb,TEb],Vb),Tq,TB,UB,RA).
tur(starts,[([TSa,TEa],u)|R],([TSb,TEb],t),Tq,TB,[[TSa,TEa]|UB],[([TSa,TEa],u)|RA]):- TSa < TEa,TSa =< TSb,TEa >= TSb,!,
    tur(starts,R,([TSb,TEb],t),Tq,TB,UB,RA).
tur(starts,[([Ta,Ta],u)|R],([TSb,TEb],t),Tq,TB,[[Ta,Ta]|UB],[([Ta,Ta],u)|RA]):- Ta = TSb,!,
    tur(starts,R,([TSb,TEb],t),Tq,TB,UB,RA).
tur(starts,[([TSa,TEa],u)|R],([TSb,TEb],u),Tq,TB,[[TSa,TEa]|UB],[([TSa,TEa],u)|RA]):- TSa < TEb, TSb =< TEa,!,
    tur(starts,R,([TSb,TEb],u),Tq,TB,UB,RA).
tur(starts,[([TSa,TEa],u)|R],([TSb,TEb],Vb),Tq,TB,UB,RA):-
    (TEa < TSb -> (RA=RA1) ; (RA = [([TSa,TEa],u)|RA1])),
    tur(starts,R,([TSb,TEb],Vb),Tq,TB,UB,RA1).

%------ tur equals 
% first list all true intervals equals TSb --- [ts,te] is [te,ts]
% second list all unknown intervals equals (relaxed) TSb --- [ts,te] remains
% third all intervals that can contribute to next TSb --- as original
tur(equals,[],_,_,[],[],[]).
tur(equals,[([TSa,TEa],t)|R],([TSb,TEb],t),Tq,[[TSa,TEa]|TB],UB,[([TSa,TEa],t)|RA]):- TEa\=inf,  TSa = TSb, TEa = TEb,!,
    tur(equals,R,([TSb,TEb],t),Tq,TB,UB,RA).
tur(equals,[([TSa,TEa],t)|R],([TSb,TEb],u),Tq,TB,[[TSa,TEa]|UB],[([TSa,TEa],t)|RA]):- TEa\=inf, TSa >= TSb, TEa =< TEb,!,
    tur(equals,R,([TSb,TEb],u),Tq,TB,UB,RA).
tur(equals,[([TSa,TEa],t)|R],([TSb,TEb],t),Tq,TB,[[TSa,TEa]|UB],[([TSa,TEa],t)|RA]):- TEa=inf,  TSa = TSb, TEa = TEb,!,
    tur(equals,R,([TSb,TEb],u),Tq,TB,UB,RA).
tur(equals,[([TSa,TEa],t)|R],([TSb,TEb],u),Tq,TB,[[TSa,TEa]|UB],[([TSa,TEa],t)|RA]):- TEa=inf, TSa >= TSb, TEa = TEb,!,
    tur(equals,R,([TSb,TEb],u),Tq,TB,UB,RA).
tur(equals,[([_TSa,TEa],t)|R],([TSb,TEb],Vb),Tq,TB,UB,RA):- TEa < TSb,!,
    tur(equals,R,([TSb,TEb],Vb),Tq,TB,UB,RA).
tur(equals,[([TSa,TEa],t)|R],([TSb,TEb],Vb),Tq,TB,UB,[([TSa,TEa],t)|RA]):-!,
    tur(equals,R,([TSb,TEb],Vb),Tq,TB,UB,RA).

tur(equals,[([TSa,TEa],u)|R],([TSb,TEb],t),Tq,TB,[[TSa,TEa]|UB],[([TSa,TEa],u)|RA]):- TSa =< TSb,TEa >= TEb,!,
    tur(equals,R,([TSb,TEb],t),Tq,TB,UB,RA).
tur(equals,[([TSa,TEa],u)|R],([TSb,TEb],u),Tq,TB,[[TSa,TEa]|UB],[([TSa,TEa],u)|RA]):- TSa =< TEb, TSb =< TEa,!,
    tur(equals,R,([TSb,TEb],u),Tq,TB,UB,RA).
tur(equals,[([TSa,TEa],u)|R],([TSb,TEb],Vb),Tq,TB,UB,RA):-
    (TEa < TSb -> (RA=RA1) ; (RA = [([TSa,TEa],u)|RA1])),!,
    tur(equals,R,([TSb,TEb],Vb),Tq,TB,UB,RA1).


%------ tur contains
% first list all true intervals contains TSb --- [ts,te] is [te,ts]
% second list all unknown intervals contains (relaxed) TSb --- [ts,te] remains
% third all intervals that can contribute to next TSb --- as original
tur(contains,[],_,_,[],[],[]).
tur(contains,[([TSa,TEa],t)|R],([TSb,TEb],V),Tq,TB,UB,[([TSa,TEa],t)|RA]):- TEa\=inf,  TSa > TSb, TEa < TEb,!,
    e_and(t,V,Vt),
    (Vt = t -> (TB = [[TSa,TEa]|TB1], UB=UB1); (UB = [[TSa,TEa]|UB1],TB=TB1)),
    tur(contains,R,([TSb,TEb],V),Tq,TB1,UB1,RA).
tur(contains,[([TSa,TEa],t)|R],([TSb,TEb],_V),Tq,TB,[[TSa,TEa]|UB],[([TSa,TEa],t)|RA]):- TEa=inf,  TSa > TSb, TEa = TEb,!,
    tur(contains,R,([TSb,TEb],u),Tq,TB,UB,RA).
tur(contains,[([_TSa,TEa],t)|R],([TSb,TEb],Vb),Tq,TB,UB,RA):- TEa < TSb,!,
    tur(contains,R,([TSb,TEb],Vb),Tq,TB,UB,RA).
tur(contains,[([TSa,TEa],t)|R],([TSb,TEb],Vb),Tq,TB,UB,[([TSa,TEa],t)|RA]):-
    tur(contains,R,([TSb,TEb],Vb),Tq,TB,UB,RA).
tur(contains,[([TSa,TEa],u)|R],([TSb,TEb],Vb),Tq,TB,[[TSa,TEa]|UB],[([TSa,TEa],u)|RA]):- TSa < TEb,TSb < TEa,!,
    tur(contains,R,([TSb,TEb],Vb),Tq,TB,UB,RA).
tur(contains,[([TSa,TEa],u)|R],([TSb,TEb],Vb),Tq,TB,UB,RA):-
    (TEa < TSb -> (RA=RA1) ; (RA = [([TSa,TEa],u)|RA1])),
    tur(contains,R,([TSb,TEb],Vb),Tq,TB,UB,RA1).


% ---- tur finishes
% first list all true intervals finishes TSb --- [ts,te] is [te,ts]
% second list all unknown intervals finishes (relaxed) TSb --- [ts,te] remains
% third all intervals that can contribute to next TSb --- as original
tur(finishes,[],_,_,[],[],[]).
% T T i i
tur(finishes,[([TSa,TEa],t)|R],([TSb,TEb],t),Tq,[[TSa,TEa]|TB],UB,[([TSa,TEa],t)|RA]):- TEb\=inf, TEa = TEb,  TSb < TSa,!,
    tur(finishes,R,([TSb,TEb],t),Tq,TB,UB,RA).
% T u i i
tur(finishes,[([TSa,TEa],t)|R],([TSb,TEb],u),Tq,TB,[[TSa,TEa]|UB],[([TSa,TEa],t)|RA]):- TSa < TEa, TEb\=inf, TEb >= TEa, TSa < TEb, TSa > TSb,!,
    tur(finishes,R,([TSb,TEb],u),Tq,TB,UB,RA).
% T V t t
tur(finishes,[([Ta,Ta],t)|R],([TSb,TEb],V),Tq,TB,UB,[([Ta,Ta],t)|RA]):- Ta\=inf, Ta = TEb,!,
    e_and(t,V,Vt),
    (Vt = t -> (TB = [[Ta,Ta]|TB1], UB=UB1); (UB = [[Ta,Ta]|UB1],TB=TB1)),
    tur(finishes,R,([TSb,TEb],V),Tq,TB1,UB1,RA).
% inf
tur(finishes,[([TSa,TEa],t)|R],([TSb,TEb],_V),Tq,TB,[[TSa,TEa]|UB],[([TSa,TEa],t)|RA]):- TEa=inf,  TSb < TSa, TEa = TEb,!,
    tur(finishes,R,([TSb,TEb],u),Tq,TB,UB,RA).
% no - remove A
tur(finishes,[([_TSa,TEa],t)|R],([TSb,TEb],Vb),Tq,TB,UB,RA):- TEa < TSb,!,
    tur(finishes,R,([TSb,TEb],Vb),Tq,TB,UB,RA).
% no - keep A
tur(finishes,[([TSa,TEa],t)|R],([TSb,TEb],Vb),Tq,TB,UB,[([TSa,TEa],t)|RA]):-
    tur(finishes,R,([TSb,TEb],Vb),Tq,TB,UB,RA).
% u T i i 
% % if |a|=1 then no interval of b exists, a point exists however
tur(finishes,[([TSa,TEa],u)|R],([TSb,TEb],t),Tq,TB,[[TSa,TEa]|UB],[([TSa,TEa],u)|RA]):- TSa<TEa, TSa < TEb, TEa >= TEb,!,
    tur(finishes,R,([TSb,TEb],u),Tq,TB,UB,RA).
% u T t i  
tur(finishes,[([Ta,Ta],u)|R],([TSb,TEb],t),Tq,TB,[[Ta,Ta]|UB],[([Ta,Ta],u)|RA]):- Ta = TEb,!,
    tur(finishes,R,([TSb,TEb],u),Tq,TB,UB,RA).
% u u i i
tur(finishes,[([TSa,TEa],u)|R],([TSb,TEb],u),Tq,TB,[[TSa,TEa]|UB],[([TSa,TEa],u)|RA]):- TSa < TEa,  TSb < TEa, TSa < TEb,!,
    tur(finishes,R,([TSb,TEb],u),Tq,TB,UB,RA).
% u u t i
tur(finishes,[([Ta,Ta],u)|R],([TSb,TEb],u),Tq,TB,[[Ta,Ta]|UB],[([Ta,Ta],u)|RA]):-  TSb < Ta, Ta =< TEb,!,
    tur(finishes,R,([TSb,TEb],u),Tq,TB,UB,RA).
tur(finishes,[([TSa,TEa],u)|R],([TSb,TEb],Vb),Tq,TB,UB,RA):-
    (TEa < TSb -> (RA=RA1) ; (RA = [([TSa,TEa],u)|RA1])),
    tur(finishes,R,([TSb,TEb],Vb),Tq,TB,UB,RA1).
