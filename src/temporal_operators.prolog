% Author: Manolis Pitsikalis
%
% Temporal operations 
% maximal range, iteration, temp. union/intersection/complement

%maximal interval cmputation from se list
compute_maximal_intervals([],(_,n),[]).
compute_maximal_intervals([],(Ts,y),[[Ts,inf]]).
compute_maximal_intervals([(Te,0,1)|SE],(Ts,y),[[Ts,Te]|R]):-
    compute_maximal_intervals(SE,(_,n),R).
compute_maximal_intervals([(Ts,1,_)|SE],(_,n),R):-
    compute_maximal_intervals(SE,(Ts,y),R).
compute_maximal_intervals([(_,1,_)|SE],(Ts,y),R):-
    compute_maximal_intervals(SE,(Ts,y),R).
compute_maximal_intervals([(_,0,_)|SE],(_,n),R):-
    compute_maximal_intervals(SE,(_,n),R).

%iteration operator (temporal constraints only)
compute_iteration_intervals(OP,S,D,IL,TR):-
    compute_iteration_intervals(OP,S,no_collection,D,IL,TR).

    
%iteration operator (temporal + atemporal contiguous pair constraints)
compute_iteration_intervals(OP,[A|R],PredicateName,D,IL,TR):-
    compute_iteration_intervals(OP,R,PredicateName,D,A,A,IL,TR).

compute_iteration_intervals(_,[],_,_,_,[],[]):-!.
compute_iteration_intervals(OP,[AA|R], PredicateName, D, PPrev, SStart, Z,TR):-
    (
        (
         PredicateName=no_collection,
         A=AA,
         Prev=PPrev,
         Start=SStart
        );
        (
         PredicateName\=no_collection,
         AA=(A,CVals),
         PPrev=(Prev,PVals),
         SStart=(Start,_SVals)
        )
    ),
    Diff is A-Prev,
    (
        % temporal constraints unsatisfied
        ( 
            (
                OP = <@,
                Diff >= D
            );
            (
                OP = =@,
                Diff =\= D
            );
            (
                OP = >=@,
                Diff < D
            )
        );
        % or atemporal constraints unsatisfied
        (
            PredicateName\=no_collection,
            Prev > Start,
            UserPredicate=..[PredicateName,PVals,CVals],
            \+call(UserPredicate)
        )
    )    
    ,!,
    (
        % if not the start of interval create one
        (
            Prev > Start,!,
            Z=[[SStart,PPrev]|IL]
        )
        ;
        (
        % if start of interval store start and continue
            Prev = Start,
            Z=IL
        )
    ),
    compute_iteration_intervals(OP,R,PredicateName,D,AA,AA,IL,TR).

compute_iteration_intervals(OP,[AA|R],PredicateName,D,PPrev,SStart,IL,TR):-
    (
        (
         PredicateName=no_collection,
         A=AA,
         Prev=PPrev
        );
        (
         PredicateName\=no_collection,
         AA=(A,CVals),
         PPrev=(Prev,PVals)
        )
    ),
    Diff is A-Prev,
    ( 
        (   
            % temporal constraints satisfied
            (
            OP = <@,
            Diff < D
            );
            (
                OP = =@,
                Diff =:= D
            );
            (
                OP = >=@,
                Diff >= D
            )
        ),
        (
            % atemporal constraints if exist
            % and if A > Prev (not start of interva)
            % satisfied
            (
                PredicateName=no_collection,
                true
            );
            (   
                PredicateName\=no_collection,
                Prev < A,
                UserPredicate=..[PredicateName,PVals,CVals],
                call(UserPredicate)
            );
            (
                PredicateName\=no_collection,
                Prev=A,
                true
            )
        )
    ),!,
    (
        (
            R=[],
            IL=[[SStart,AA]],
	        TR=[]
        )
        ;
        (
            R\=[],
            compute_iteration_intervals(OP,R,PredicateName,D,AA,SStart,IL,TR)
        )
    ).

compute_iteration_intervals(_,[],_,_,Prev,_,[],[Prev]).


%single scan temporal union
compute_union_intervals([],_,_,_,[]).
compute_union_intervals([(TS,(SC:_,EC:_))|R],0,E,_,IL):-
    SC>0,
    EN is E+EC,
    compute_union_intervals(R,SC,EN,TS,IL).
compute_union_intervals([(TE,(SC:_,EC:_))|R],S,E,TS,[[TS,TE]|IL]):-
    S\=0,
    SN is S+SC,
    EN is E+EC,
    SN>0, SN=EN,
    compute_union_intervals(R,0,0,_,IL).
compute_union_intervals([(_,(SC:_,EC:_))|R],S,E,TS,IL):-
    S\=0,
    SN is S+SC,
    EN is E+EC,
    SN\=EN,
    compute_union_intervals(R,SN,EN,TS,IL).

%single scan temporal intersection
compute_intersection_intervals([],_,_,_,[]).
compute_intersection_intervals([(TE,_:_,_:B)|R],1,1,TS,[[TS,TE]|IL]):-
    (
        member(a,B),member(b,B),
        compute_intersection_intervals(R,0,0,_,IL)
    );
    (
        member(a,B),\+member(b,B),
        compute_intersection_intervals(R,0,1,_,IL)
    );
    (
        member(b,B),\+member(a,B),
        compute_intersection_intervals(R,1,0,_,IL)
    ).
compute_intersection_intervals([(T,_:A,_:B)|R],AIN,BIN,_,IL):-
    BOTHIN is AIN /\ BIN, BOTHIN = 0,
    check_in(a,A,AAIN),
    check_in(b,A,ABIN),
    check_in(a,B,RAIN),
    check_in(b,B,RBIN),
    neg(RAIN,NeRAIN),
    neg(RBIN,NeRBIN),
    NAIN is (AIN \/ AAIN) /\ NeRAIN,
    NBIN is (BIN \/ ABIN) /\ NeRBIN,
    compute_intersection_intervals(R,NAIN,NBIN,T,IL).

%single scan temporal complement
compute_complement_intervals([],_,_,_,[]).
compute_complement_intervals([(TE,_:A,_:B)|R],1,0,TS,RES):-
    (
        %case interval of a ends
        member(a,B),!,
        check_in(b,A,BIN),
        TE1=TE,
        (TS<TE -> RES=[[TS,TE1]|IL] ; RES=IL),
        compute_complement_intervals(R,0,BIN,_,IL)
    );
    (
        %case interval of b starts
        member(b,A),
        check_in(a,B,AIN),
        neg(AIN,NAIN),
        TE1 is TE-1,
        (TS<TE1 -> RES = [[TS,TE1]|IL] ; RES=IL), 
        compute_complement_intervals(R,NAIN,1,_,IL)
    ).

compute_complement_intervals([(T,_:A,_:B)|R],AIN,BIN,To,IL):-
    \+((AIN=1,BIN=0)),
    check_in(a,A,AAIN), % start of interval of first list at t
    check_in(b,A,ABIN), % start of interval of second list at t
    check_in(a,B,RAIN), % end of interval of first list at t 
    check_in(b,B,RBIN), % end of interval of second list at t
    neg(RAIN,NeRAIN), % not (end of interval of first list at t)
    neg(RBIN,NeRBIN), % not (end of interval of second list at t)
    %writeln((NAIN is (AIN \/ AAIN) /\ NeRAIN)),
    NAIN is (AIN \/ AAIN) /\ NeRAIN, % interval of a already opened or opened now, and did not close now
    NBIN is (BIN \/ ABIN) /\ NeRBIN, % interval of b already opened or opened now, and did not close now
    ((
     NAIN=1,!,
     ((ABIN=0,RBIN=0) -> compute_complement_intervals(R,NAIN,NBIN,T,IL); (T1 is T+1,compute_complement_intervals(R,NAIN,NBIN,T1,IL))) 
    );
    (
     NAIN\=1,
     compute_complement_intervals(R,NAIN,NBIN,To,IL)
    )).


check_in(X,A,1):-
    member(X,A).
check_in(X,A,0):-
    \+member(X,A).

neg(1,0).
neg(0,1).
