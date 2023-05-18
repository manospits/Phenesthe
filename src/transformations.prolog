% Author: Manolis Pitsikalis
%
% Transform user formulae into internal 
% representation
%

%%
%%------------------------ Phi handling --------------------------
%%
%% Takes a formula of Phi and transforms it accordingly for
%% prolog
transform_formula(Phenomenon, TransformedFormula, TemporalInformationList):-
    phenomenon_type(Phenomenon,PhenomenonType,user),
    phenomenon_conditions(Phenomenon, Formula),
    term_variables(Phenomenon, Variables),
    transform_formula2(PhenomenonType, Formula, Variables, TransformedFormula, TemporalInformationList),!.

transform_formula(Phenomenon, _TransformedFormula, _TemporalInformationList):-
    phenomenon_type(Phenomenon,_PhenomenonType,user),
    format(user_error,'ERROR: Definition transformation of user phenomenon ~w failed.\n',[Phenomenon]),
    halt(1).


transform_formula2(event,Formula,Variables,TransformedFormula, InstantList):-
    transform_instant_formula(Formula, Variables, ProcessedFormula, PInstantList),
    term_variables(ProcessedFormula,PFVars),
    variable_list_diff(PFVars, [PInstantList|Variables], PFStVars),
    TransformedFormula=(
        setof_empty(PInstantList,PFStVars^ProcessedFormula, InstantList)).

transform_formula2(state,Formula,Variables,TransformedFormula, IntervalList):-
    transform_dinterval_formula(Formula, Variables, ProcessedFormula, PIL),
    term_variables(ProcessedFormula,PFVars),
    variable_list_diff(PFVars, [PIL|Variables], PFStVars),
    TransformedFormula=(
        setof_empty(PIL,PFStVars^ProcessedFormula,ZIL),
        merge_disjoint_interval_lists(ZIL,IntervalList)
    ).

transform_formula2(dynamic_phenomenon,Formula,Variables,TransformedFormula,IntervalList):-
    transform_ndinterval_formula(Formula, Variables, ProcessedFormula, PIL),
    term_variables(ProcessedFormula,PFVars),
    variable_list_diff(PFVars, [PIL|Variables], PFStVars),
    TransformedFormula=(
        setof_empty(PIL,PFStVars^ProcessedFormula,ZIL),
        merge_non_disjoint_interval_lists(ZIL,IntervalList)
    ).

%%
%%------------------------ Phi^. handling --------------------------
%%
%% Takes a formula of Phi^. and transforms it accordingly for
%% prolog #
transform_instant_formula(aand(L,R), PheVars, ProcessedFormula, T):-!,
    term_variables([PheVars,R],RPheVars),
    transform_instant_formula(L, RPheVars, LAt,  T),
    ProcessedFormula=(
        LAt,R
    ).

% conjunction #
% setof((X,T),
%  ((b(X,T),a(X,T),T=t);
%  (b(X,t),a(X,T),T=u);
%  (b(X,u),a(X,t),T=u);
%  (b(X,u),a(X,u),T=u)),L).
transform_instant_formula(and(L,R), PheVars, ProcessedFormula, (T,V)):-!,
    phe_getval(formula_id,FormulaId),
    term_variables(L,LFormulaVars),
    term_variables(R,RFormulaVars),
    term_variables([PheVars,L],LPheVars),
    term_variables([PheVars,R],RPheVars),
    term_variables([LFormulaVars,RFormulaVars],LRVars),
    variable_list_intersection(LRVars,PheVars,LRVarsRelatedWithT),
    variable_list_diff(LRVarsRelatedWithT,[T],LRVarsRelated),
    transform_instant_formula(L, RPheVars, LAt, (T,V1)),
    transform_instant_formula(R, LPheVars, RAt, (T,V2)),
    ProcessedFormula=(
        phe_getval(tq,Tq),
        phe_getval(tqmw,Tqmw),
        phe_getval(tqms,Tqms),
        (
            (
                
                (instant_left_retained(FormulaId, Tqms, LRVarsRelated,T,V1);LAt),
                (instant_right_retained(FormulaId, Tqms,LRVarsRelated,T,V2);RAt),
                e_and(V1,V2,V)
            );
            (
                (instant_right_retained(FormulaId, Tqms,LRVarsRelated,T,V2);RAt),
                (instant_left_retained(FormulaId, Tqms,LRVarsRelated,T,V1);LAt),
                e_and(V1,V2,V)
            )
        ),
        ((V=u, T>Tqmw, \+formula_retaining_time(FormulaId,LRVarsRelated,Tq,_)) -> assert_if_not_exists(formula_retaining_time(FormulaId, LRVarsRelated, Tq, T)) ; true),
        (formula_retaining_time(FormulaId,LRVarsRelated,Tq,Tc) -> 
            (T >= Tc, retain_subformulae_instant(l,FormulaId, LRVarsRelated, Tq, T, V1),
                      retain_subformulae_instant(r,FormulaId, LRVarsRelated, Tq, T, V2)
            ); true
        )
    ),
    FormulaIdp1 is FormulaId+1,phe_setval(formula_id,FormulaIdp1).
    


% disjunction #
% setof((X,T), 
% ((a(X,T),T=t);
% (b(X,T),T=t);
% (a(X,T),b(X,T),T=u);
% (a(X,T),\+b(X,_),T=u);
% (b(X,T),a(X,T),T=u);
% (b(X,T),\+a(X,_),T=u)),L).
transform_instant_formula(or(L,R), PheVars, ProcessedFormula, (T,V)):-!,
    phe_getval(formula_id,FormulaId),
    term_variables(L,LFormulaVars),
    term_variables(R,RFormulaVars),
    term_variables([PheVars,L],LPheVars),
    term_variables([PheVars,R],RPheVars),
    term_variables([LFormulaVars,RFormulaVars],LRVars),
    variable_list_intersection(LRVars,PheVars,LRVarsRelatedWithT),
    variable_list_diff(LRVarsRelatedWithT,[T],LRVarsRelated),
    transform_instant_formula(L, RPheVars, LAt, (T,V1)),
    transform_instant_formula(R, LPheVars, RAt, (T,V2)),
    ProcessedFormula=( 
        (
            phe_getval(tq,Tq),
            phe_getval(tqmw,Tqmw),
            phe_getval(tqmw,Tqms),
            ((instant_left_retained(FormulaId, Tqms, LRVarsRelated,T,V1);LAt) , V1=t,V1=V,Case=a);
            ((instant_right_retained(FormulaId, Tqms, LRVarsRelated,T,V2);RAt), V2=t,V2=V, Case=b);
            (V1=u,V2=V1,LAt,RAt, V1=V2,V1=u,V=V1, Case = c);
            (V1=u,LAt,\+(instant_right_retained(FormulaId,Tqms,LRVarsRelated,T,V2);RAt),V=u, Case = d);
            (V1=u,V2=V1,RAt,LAt,V=V1, Case = d);
            (V2=u,RAt,V2=u,\+(instant_left_retained(FormulaId,Tqms,LRVarsRelated,T,V1);LAt),V=u, Case = d)
        ),
        ((V=u, T>Tqmw, \+formula_retaining_time(FormulaId,LRVarsRelated,_,_)) -> assert_if_not_exists(formula_retaining_time(FormulaId, LRVarsRelated, Tq, T)) ; true),
        (formula_retaining_time(FormulaId,LRVarsRelated, Tq, Tc) -> 
            (   
                (
                    T >= Tc, 
                    (
                      (Case = a, retain_subformulae_instant(l,FormulaId, LRVarsRelated, Tq, T, V1));
                      (Case = b, retain_subformulae_instant(r,FormulaId, LRVarsRelated, Tq, T, V2));
                      (Case = c);
                      (Case = d)
                    )
                );
                (T < Tc)
            )
            ; true
        )
    ),
    FormulaIdp1 is FormulaId+1,phe_setval(formula_id,FormulaIdp1).

%returns the instants in the window where the negated formula
%does not hold #
%transform_instant_formula(tnot(R), PheVars, ProcessedFormula, T):-!,
    %transform_instant_formula(R, PheVars, RAt, (Ti,Vi)),
    %term_variables(RAt,RAtVars),
    %term_variables( [(Ti,Vi),PheVars],TVPheVars),
    %variable_list_diff(RAtVars,TVPheVars,RAtVarsUnrelated),
    %term_variables( [(Ti,Vi),RAtVarsUnrelated],TVRAtVarsU),
    %variable_list_diff(RAtVars,TVRAtVarsU,RAtVarsRelated),
    %ProcessedFormula=(
        %ground(RAtVarsRelated) -> 
            %(
                %(\+(RAt),T=(Ti,t));
                %(Vi=u,RAt,T=(Ti,u))
            %);
            %(\+ground(RAtVarsRelated),
            %setof_empty((Ti,t),RAtVarsUnrelated^(RAt,Vi=t),TRInstantList),
            %setof_empty((Ti,u),RAtVarsUnrelated^(RAt,Vi=u),URInstantList),
            %phe_getval(tqmw,Tqmw),
            %(
                %(URInstantList\=[],
                %member((Ti,u), URInstantList),
                %Ti > Tqmw) -> 
                    %( Ti=Tc,assertz(formula_retaining_time(FormulaId, RAtVarsRelated, Tc))); true
            %),
            %phe_getval(current_window_instants,InstantListW),
            %( 
              %retained_formula_instant(FormulaId,RAtVarsRelated,RetainedTTi);
              %(\+retained_formula_instant(FormulaId,RAtVarsRelated,RetainedTTi),RetainedTTi=[]) 
            %),
            %merge_instant_lists([RetainedTTi,TRInstantList],RTRInstantList),
            %(formula_retaining_time(FormulaId, RAtVarsRelated, Tc) -> 
                %(
                    %findall((Ti,t), (member((Ti,t),RTRInstantList),Ti>Tc), InstantsToRetain),
                    %(InstantsToRetain\=[] -> assertz(retained_right_instants(FormulaId,RAtVarsRelated,InstantsToRetain));true)
                %);
                %true
            %),
            %ord_subtract(InstantListW,RTRInstantList,TInstantList),
            %merge_wt_with_unk(TInstantList,URInstantList,InstantList),
            %member(T,InstantList))
    %).

%returns the instants in the window where the negated formula
%does not hold
%nothing to retain (check)
transform_instant_formula(tnot(R), PheVars, ProcessedFormula, T):-!,
    transform_instant_formula(R, PheVars, RAt, (Ti,Vi)),
    term_variables(RAt,RAtVars),
    term_variables( [(Ti,Vi),PheVars],TVPheVars),
    variable_list_diff(RAtVars,TVPheVars,RAtVarsUnrelated),
    term_variables( [(Ti,Vi),RAtVarsUnrelated],TVRAtVarsU),
    variable_list_diff(RAtVars,TVRAtVarsU,RAtVarsRelated),
    ProcessedFormula=(
        ground(RAtVarsRelated) -> 
            (
                (
                    (T=(Ti,V),Vi=t,\+(RAt),V=t);
                    (T=(Ti,V),Vi=u,RAt,V=u)
                )
            );
            (
                \+ground(RAtVarsRelated),
                setof_empty((Ti,t),RAtVarsUnrelated^(RAt,Vi=t),TRInstantList),
                setof_empty((Ti,u),RAtVarsUnrelated^(RAt,Vi=u),URInstantList),
                phe_getval(current_window_instants,InstantListW),
                ord_subtract(InstantListW,TRInstantList,TInstantList),
                merge_wt_with_unk(TInstantList,URInstantList,InstantList),
                member(T,InstantList)
            )
    ).

%is true when the grounded formula does not gold on T #
%nothing to retain (check)
transform_instant_formula(gtnot(R), PheVars, ProcessedFormula, (T,V)):-!,
    transform_instant_formula(R, PheVars, RAt, (T,V)),
    term_variables(RAt,RAtVars),
    term_variables([PheVars,(T,V)],TVPheVars),
    variable_list_diff(RAtVars,TVPheVars,RAtVarsUnrelated),
    variable_list_diff(RAtVars,[V|RAtVarsUnrelated],RAtVarsRelated),
    ProcessedFormula=(
        (ground(RAtVarsRelated),  \+(RAt),V=t);
        (ground(RAtVarsRelated), RAt, V=u)
    ).

% checks whether an instant formula occurs during a disjoint
% interval formula (A in B is is true on instants at which A is 
% true and are included in an interval at which B is true) #
transform_instant_formula(in(L,R), PheVars, ProcessedFormula, T):-!,
    phe_getval(formula_id,FormulaId),
    term_variables([PheVars,L],LPheVars),
    term_variables([PheVars,R],RPheVars),
    transform_instant_formula(L, RPheVars, Lt, Ti),
    transform_dinterval_formula(R, LPheVars, Rt, RIL),
    term_variables(Lt, LFormulaVars),
    term_variables(Rt, RFormulaVars),
    term_variables([LFormulaVars,PheVars],LPheVarsF),
    term_variables([RFormulaVars,PheVars],RPheVarsF),
    variable_list_diff(LFormulaVars,[Ti|RPheVarsF],LVarsUnrelated),
    variable_list_diff(RFormulaVars,[RIL|LPheVarsF],RVarsUnrelated),
    term_variables([LFormulaVars,RFormulaVars],LRVars),
    variable_list_intersection(LRVars,PheVars,LRVarsRelatedWithT),
    variable_list_diff(LRVarsRelatedWithT,[Ti,RIL],LRVarsRelated),
    ProcessedFormula=(
        phe_getval(tq,Tq),
        phe_getval(tqmw,Tqmw),
        phe_getval(tqms,Tqms),
        %compute all instants of left formula
        ( 
            (
                retained_left_instants(FormulaId, Tqms, LRVarsRelated, RetainedInstants),
                setof_empty(Ti,LVarsUnrelated^Lt,TiL)
            );
            (
                setof_empty(Ti,LVarsUnrelated^Lt,TiL),
                \+retained_left_instants(FormulaId, Tqms, LRVarsRelated, RetainedInstants),
                RetainedInstants=[]
            )
        ),
        merge_instant_lists([TiL,RetainedInstants],LInstantList),
        %compute intervals of right formula
        (
            (
                retained_right_intervals(FormulaId, Tqms, LRVarsRelated, RetainedIntervals),
                setof_empty(RIL,RVarsUnrelated^Rt,RILists)
            )
            ;
            (
                \+retained_right_intervals(FormulaId, Tqms, LRVarsRelated, RetainedIntervals),
                setof_empty(RIL,RVarsUnrelated^Rt,RILists),
                RetainedIntervals=[]
            )
        ),
        merge_disjoint_interval_lists([RetainedIntervals|RILists],RILmerged),
        compute_instants_in_intervals(LInstantList,RILmerged,IL),
        (
            (member((Tx,u), IL),Tx > Tqmw, \+formula_retaining_time(FormulaId,RAtVarsRelated, Tq,_)) -> 
                ( Tx=Tc,assertz(formula_retaining_time(FormulaId, RAtVarsRelated, Tq, Tc))); true
        ),
        (formula_retaining_time(FormulaId, RAtVarsRelated, Tq, Tc) -> 
            (
                findall((Tx,t), (member((Tx,t),IL),Tx>Tc), InstantsToRetain),
                (InstantsToRetain\=[] -> assert_if_not_exists(retained_left_instants(FormulaId,Tq,LRVarsRelated,InstantsToRetain));true),
                findall(([Ts,Te],t), (member(([Ts,Te],t),IL),Ts>Tc,Te\=inf), IntervalsToRetain),
                (InstantsToRetain\=[] -> assert_if_not_exists(retained_right_intervals(FormulaId,Tq,LRVarsRelated,IntervalsToRetain));true)
            );
            true
        ),
        member(T,IL)
    ),
    FormulaIdp1 is FormulaId+1,phe_setval(formula_id,FormulaIdp1).

%input event # 
transform_instant_formula(Formula, _PheVars, ProcessedFormula, T):-
    phenomenon_type(Formula,event,input),!,
    ProcessedFormula=(input_event_instant(Formula,T1),T=(T1,t)).

%user event (NOTE! User events use instant lists) 
transform_instant_formula(Formula, _PheVars, ProcessedFormula,  T):-
    phenomenon_type(Formula,event,user),!,
    ProcessedFormula=(event_instants_internal(Formula,InstantList),member(T,InstantList)).

% start operator 
% nothing to retain (check)
transform_instant_formula(start(DFormula), PheVars, ProcessedFormula, T):-!,
    transform_dinterval_formula(DFormula, PheVars, DPFormula, PIL),
    term_variables(DPFormula,PFVars),
    variable_list_diff(PFVars, [PIL|PheVars], PFVarsUnrelated),
    ProcessedFormula=(
        setof_empty(PIL,PFVarsUnrelated^DPFormula,ZIL),
        merge_disjoint_interval_lists(ZIL,IL),
        member(([TS,TE],V),IL),
        phe_getval(tq,Tq),
        (
            (V=t -> 
                T=(TS,t) ; 
                (
                    (TE\=inf -> (TE1 is TE-1) ; TE1 = Tq), create_window_instants(TS,TE1,u,LTU),
                member(T,LTU))
            )
        )
    ).

% end operator 
% nothing to retain (check)
transform_instant_formula(end(DFormula), PheVars, ProcessedFormula, T):-!,
    transform_dinterval_formula(DFormula, PheVars, DPFormula, PIL),
    term_variables(DPFormula,PFVars),
    variable_list_diff(PFVars, [PIL|PheVars], PFVarsUnrelated),
    ProcessedFormula=(
        setof_empty(PIL,PFVarsUnrelated^DPFormula,ZIL),
        merge_disjoint_interval_lists(ZIL,IL),
        member(([TS,TE],V),IL),
        (
            (V=t -> 
                (
                    TE\=inf, T=(TE,t)
                )  ;
                (TS1 is TS+1, create_window_instants(TS1,TE,u,LTU),
                member(T,LTU))
            )
        )
    ).
% atemporal formula
transform_instant_formula(Formula, _PheVars, Formula, _):-
    Formula=..[OP|_],
    \+phenomenon_type(Formula,_,_),
    op_list(OPList),
    \+(member(OP,OPList)).
%------------------------------------------------------------------



%%
%%------------------------ Phi^- handling --------------------------
%%
%% Takes a formula of Phi^- and transforms it accordingly for
%% prolog
transform_dinterval_formula(~>(L,R),PheVars, ProcessedFormula, IL):-!,
    term_variables([L,PheVars],LPheVars),
    term_variables([R,PheVars],RPheVars),
    transform_instant_formula(L, RPheVars, Lt, LTs),
    transform_instant_formula(R, LPheVars, Rt, LTe),
    maxmin_interval_computation_formula(max, Lt, Rt, LTs, LTe, IL, PheVars, ProcessedFormula).

transform_dinterval_formula(~>>(L,R),PheVars, ProcessedFormula, IL):-!,
    term_variables([L,PheVars],LPheVars),
    term_variables([R,PheVars],RPheVars),
    transform_instant_formula(L, RPheVars, Lt, LTs),
    transform_instant_formula(R, LPheVars, Rt, LTe),
    maxmin_interval_computation_formula(min, Lt, Rt, LTs, LTe, IL, PheVars, ProcessedFormula).


% temporal union/intersection/complement
transform_dinterval_formula(Formula, PheVars, ProcessedFormula, IL):-
    Formula=..[OP,L,R],
    member(OP,[union,intersection,complement]),!,
    term_variables([L,PheVars],LPheVars),
    term_variables([R,PheVars],RPheVars),
    transform_dinterval_formula(L,RPheVars,Lt,LIL),
    transform_dinterval_formula(R,LPheVars,Rt,RIL),
    tset_computation_formula(OP,Lt,Rt,LIL,RIL,IL,PheVars,ProcessedFormula).

% interval filtering
transform_dinterval_formula(filter(Formula, Operation), PheVars, ProcessedFormula, IL):-
    transform_dinterval_formula(Formula, PheVars, DPFormula, PIL),
    term_variables(DPFormula,PFVars),
    variable_list_diff(PFVars, [PIL|PheVars], PFVarsUnrelated),
    ProcessedFormula=(
        setof_empty(PIL,PFVarsUnrelated^DPFormula,ZIL),
        merge_disjoint_interval_lists(ZIL,ILNF),
        apply_filter(Operation,ILNF,IL)
    ).

% atemporal conjuntion
transform_dinterval_formula(aand(L,R), PheVars, ProcessedFormula, IL):-
    term_variables([R,PheVars],RPheVars),
    transform_dinterval_formula(L,RPheVars,LAt,IL),
    ProcessedFormula=(
        LAt,R
    ).

% user defined state intervals
transform_dinterval_formula(Formula, _PheVars, ProcessedFormula, IL):-
    phenomenon_type(Formula,state,user),
    ProcessedFormula=(state_intervals_internal(Formula,IL)).

% input intervals #
transform_dinterval_formula(Formula, _PheVars, ProcessedFormula, IL):-
    phenomenon_type(Formula,state,input),
    ProcessedFormula=(input_state_interval(Formula,I),IL=[(I,t)]).

% formula that computes temporal union intersection and complement 
tset_computation_formula(OP,LFormula,RFormula,LIL,RIL,IL,PheVars,ProcessedFormula):-
    phe_getval(formula_id,FormulaId),
    term_variables(LFormula,LFormulaVars),
    term_variables(RFormula,RFormulaVars),
    term_variables([LFormulaVars,PheVars],LPheVarsF),
    term_variables([RFormulaVars,PheVars],RPheVarsF),
    variable_list_diff(LFormulaVars,[LIL|RPheVarsF],LVarsUnrelated),
    variable_list_diff(RFormulaVars,[RIL|LPheVarsF],RVarsUnrelated),
    term_variables([LIL|LVarsUnrelated],LVarsUnrelatedLIL),
    term_variables([RIL|RVarsUnrelated],RVarsUnrelatedRIL),
    variable_list_diff(LPheVarsF,LVarsUnrelatedLIL,LVarsRelated),
    variable_list_diff(RPheVarsF,RVarsUnrelatedRIL,RVarsRelated),
    term_variables([LVarsRelated,RVarsRelated],LRVarsRelated),
    ProcessedFormula=(
        phe_getval(tq,Tq),
        phe_getval(tqms,Tqms),
          (
           (
               (
                   (
                    setof(ILRetained,retained_left_intervals(FormulaId, Tqms, LRVarsRelated, ILRetained), ILRetainedLists),
                    setof_empty(LIL,LVarsUnrelated^LFormula,LILists)
                   );
                   (
                    setof(LIL,LVarsUnrelated^LFormula,LILists),
                    \+setof(ILRetained,retained_left_intervals(FormulaId, Tqms, LRVarsRelated, ILRetained), ILRetainedLists),
                    ILRetainedLists=[]
                   )
               ),
               (
                   (
                    setof(IRRetained,retained_right_intervals(FormulaId, Tqms, LRVarsRelated, IRRetained), IRRetainedLists),
                    setof_empty(RIL,RVarsUnrelated^RFormula,RILists)
                   );
                   (
                    setof_empty(RIL,RVarsUnrelated^RFormula,RILists),
                    \+setof(IRRetained,retained_right_intervals(FormulaId, Tqms, LRVarsRelated, IRRetained), IRRetainedLists),
                    IRRetainedLists=[]
                   )
               )
           );
           (
               (
                   (
                    setof(IRRetained,retained_right_intervals(FormulaId, Tqms, LRVarsRelated, IRRetained), IRRetainedLists),
                    setof_empty(RIL,RVarsUnrelated^RFormula,RILists)
                   );
                   (
                    setof(RIL,RVarsUnrelated^RFormula,RILists),
                    \+setof(IRRetained,retained_right_intervals(FormulaId, Tqms, LRVarsRelated, IRRetained), IRRetainedLists),
                    IRRetainedLists=[]
                   )
               ),
               \+((
                   (
                    setof(ILRetained,retained_left_intervals(FormulaId, Tqms, LRVarsRelated, ILRetained), ILRetainedLists),
                    setof_empty(LIL,LVarsUnrelated^LFormula,LILists)
                   );
                   (
                    setof(LIL,LVarsUnrelated^LFormula,LILists),
                    \+setof(ILRetained,retained_left_intervals(FormulaId, Tqms, LRVarsRelated, ILRetained), ILRetainedLists)
                   )
                  )
                ),
               LILists=[],
               ILRetainedLists=[]
           )
          ),
          merge_disjoint_interval_lists(LILists,LIListsUnion),
          merge_disjoint_interval_lists(ILRetainedLists,RLIListsUnion),
          merge_disjoint_interval_lists(RILists,RIListsUnion),
          merge_disjoint_interval_lists(IRRetainedLists,RRIListsUnion),
          merge_disjoint_interval_lists([LIListsUnion,RLIListsUnion],RNLIListsUnion),
          merge_disjoint_interval_lists([RIListsUnion,RRIListsUnion],RNRIListsUnion),
          merge_ilse(RNLIListsUnion,RNRIListsUnion,SEL),
          compute_tset_intervals(OP,SEL,IL),
          retain_left_right_intervals(FormulaId, IL, RNLIListsUnion, RNRIListsUnion, LRVarsRelated, LRVarsRelated, Tq)
    ),
    FormulaIdp1 is FormulaId+1,phe_setval(formula_id,FormulaIdp1).

% temporal union/intersection/complement helpers
compute_tset_intervals(union,SEL,IL):-compute_union_intervals(SEL,0,0,0,_,IL).
compute_tset_intervals(intersection,SEL,IL):-compute_intersection_intervals(SEL,0:f,0:f,_,IL).
compute_tset_intervals(complement,SEL,IL):-compute_complement_intervals(SEL,0:f,0:f,_,IL).

% maximal interval computation formula transformation
maxmin_interval_computation_formula(MAXMIN,StartingFormula,EndingFormula,(Ts,VTs),(Te,VTe),IL,PheVars,ProcessedFormula):-
    phe_getval(formula_id,FormulaId),
    term_variables(StartingFormula,SVars),
    term_variables(EndingFormula,EVars),
    % we need to pass variables of left formula to right
    % and the reverse to make sure we don't skip in 
    % set of any unifications
    term_variables([SVars,PheVars],LPheVarsF),
    term_variables([EVars,PheVars],RPheVarsF),
    term_variables([(Ts,VTs)|RPheVarsF],RPheVarsFTSv),
    term_variables([(Te,VTe)|LPheVarsF],LPheVarsFTEv),
    variable_list_diff(SVars,RPheVarsFTSv,SVarsUnrelated),
    variable_list_diff(EVars,LPheVarsFTEv,EVarsUnrelated),
    %term_variables([SVars,EVars],SEVars),
    term_variables([(Ts,VTs)|SVarsUnrelated],SVarsUnrelatedTSv),
    term_variables([(Te,VTe)|EVarsUnrelated],EVarsUnrelatedTEv),
    %term_variables([SVarsUnrelatedTSv,EVarsUnrelatedTEv],SEVarsUnrelatedTv),
    variable_list_diff(SVars,SVarsUnrelatedTSv,SVarsRelated),
    variable_list_diff(EVars,EVarsUnrelatedTEv,EVarsRelated),
    term_variables([SVarsRelated,EVarsRelated],SEVarsRelated),
    ProcessedFormula=(
        phe_getval(tq,Tq),
        phe_getval(tqms,Tqms),
        (
          % check if there are any retained results from before
          (
           setof(TsRetained,retained_left_instants(FormulaId, Tqms, SEVarsRelated, TsRetained), StartingPointsRetainedL),
           setof_empty((Ts,VTs),SVarsUnrelated^(StartingFormula),StartingPointsNew)
          );
          (
          % if there aren't proceed as usual. Extra care must be take to avoid duplicates.
           setof_empty((Ts,VTs),SVarsUnrelated^(StartingFormula),StartingPointsNew),
           \+((ground(SEVarsRelated), setof(TsRetained,retained_left_instants(FormulaId, Tqms, SEVarsRelated, TsRetained), StartingPointsRetainedL))),
           StartingPointsRetainedL=[]
          )
        ),
        %merge old starting points with retained
        merge_instant_lists([StartingPointsNew|StartingPointsRetainedL],StartingPoints),
        %find ending points
        (
            (
                setof(TeRetained, retained_right_instants(FormulaId,Tqms,SEVarsRelated, TeRetained),EndingPointsRetainedL),
                setof_empty((Te,VTe),EVarsUnrelated^EndingFormula,EndingPoints)
            )
            ;
            (
                setof_empty((Te,VTe),EVarsUnrelated^EndingFormula,EndingPoints),
                \+setof(TeRetained, retained_right_instants(FormulaId,Tqms,SEVarsRelated, TeRetained),EndingPointsRetainedL),
                EndingPointsRetainedL=[]
            )
        ),
        merge_instant_lists([EndingPoints|EndingPointsRetainedL],EndingPoints),
        %create the appropriate se list
        merge_se(StartingPoints,EndingPoints,SEList),
        %compute the maximal intervals
        compute_maxmin_intervals(MAXMIN,SEList,IL),
        %check if you need to retain anything for next query and retain it
        retain_starting_ending_points(FormulaId, IL, StartingPoints, EndingPoints, SEVarsRelated, SEVarsRelated, Tq)
    ),
    FormulaIdp1 is FormulaId+1,phe_setval(formula_id,FormulaIdp1).

%%
%%------------------------ Phi^= handling --------------------------
%%
%% Takes a formula of Phi^= and transforms it accordingly for
%% prolog
allowed_formulae(before,[instant,dinterval,ndinterval],[instant,dinterval,ndinterval]).
allowed_formulae(meets,[dinterval,ndinterval],[dinterval,ndinterval]).
allowed_formulae(overlaps,[dinterval,ndinterval],[dinterval,ndinterval]).
allowed_formulae(finishes,[instant,dinterval,ndinterval],[dinterval,ndinterval]).
allowed_formulae(starts,[instant,dinterval,ndinterval],[dinterval,ndinterval]).
allowed_formulae(equals,[dinterval,ndinterval],[dinterval,ndinterval]).
allowed_formulae(contains,[dinterval,ndinterval],[instant,dinterval,ndinterval]).

transform_ndinterval_formula_internal(Formula, Allowed, OtherVars, TransformedFormula, TemporalInformationList, FormulaType):-
    ((member(instant,Allowed), transform_instant_formula(Formula, OtherVars, TransformedFormula, TemporalInformationList), FormulaType=instant);
     (member(dinterval,Allowed), transform_dinterval_formula(Formula, OtherVars, TransformedFormula, TemporalInformationList), FormulaType=dinterval);
     (member(dinterval,Allowed), transform_ndinterval_formula(Formula, OtherVars, TransformedFormula, TemporalInformationList), FormulaType=ndinterval)).


% relations
transform_ndinterval_formula(Formula, PheVars, ProcessedFormula, IL):-
    Formula=..[Relation, L, R],
    allowed_formulae(Relation, AllowedL, AllowedR),!,
    term_variables([L,PheVars],LPheVars),
    term_variables([R,PheVars],RPheVars),
    %check which formulae are allowed left and right based on the relation
    transform_ndinterval_formula_internal(L, AllowedL, RPheVars, LTransformed, LTIL, LType),
    transform_ndinterval_formula_internal(R, AllowedR, LPheVars, RTransformed, RTIL, RType),
    formulae_ints_type(LType,RType,FType),
    relation_intervals_formula(Relation, FType, LType, RType, LTransformed, RTransformed, LTIL, RTIL, IL, PheVars, ProcessedFormula).

% atemporal conjunction
transform_ndinterval_formula(aand(L,R), PheVars, ProcessedFormula, IL):-
    term_variables([R,PheVars],RPheVars),
    transform_ndinterval_formula(L,RPheVars,LAt,IL),
    ProcessedFormula=(
        LAt,R
    ).

% user defined dynamic temporal phenomenon
transform_ndinterval_formula(Formula, _PheVars, ProcessedFormula, IL):-
    phenomenon_type(Formula,dynamic_phenomenon,user),
    ProcessedFormula=(dynamic_phenomenon_intervals_internal(Formula,IL); \+dynamic_phenomenon_intervals_internal(Formula,_),IL=[]).

% input temporal phenomenon
transform_ndinterval_formula(Formula, _PheVars, ProcessedFormula, IL):-
    phenomenon_type(Formula,dynamic_phenomenon,input),
    ProcessedFormula=(input_dynamic_phenomenon_interval(Formula,I),IL=[I]).

% before relation ** not the usual allen relations before ** intervals must be contiguous
relation_intervals_formula(Relation, _FT, LType, RType, LFormula, RFormula, LIL, RIL, IL, PheVars, ProcessedFormula):-
    !,phe_getval(formula_id,FormulaId),
    term_variables(LFormula,LFormulaVars),
    term_variables(RFormula,RFormulaVars),
    term_variables([LFormulaVars,PheVars],LPheVarsF),
    term_variables([RFormulaVars,PheVars],RPheVarsF),
    variable_list_diff(LFormulaVars, [LIL|RPheVarsF], LVarsUnrelated),
    variable_list_diff(RFormulaVars, [RIL|LPheVarsF], RVarsUnrelated),
    term_variables([LFormulaVars,RFormulaVars],LRVars),
    variable_list_intersection(LRVars,PheVars,LRVarsRelatedWithT),
    variable_list_diff(LRVarsRelatedWithT,[LIL,RIL],LRVarsRelated),
    ProcessedFormula=(
        phe_getval(tq,Tq),
        phe_getval(tqms,Tqms),
        (
              (
                setof(ILRetained,retained_left_intervals(FormulaId, Tqms, LRVarsRelated, ILRetained), ILRetainedLists),
                setof_empty(LIL,LVarsUnrelated^LFormula,LILists)
              );
              (
                setof(LIL,LVarsUnrelated^LFormula,LILists),
                \+((ground(LRVarsRelated),setof(ILRetained,retained_left_intervals(FormulaId, Tqms, LRVarsRelated, ILRetained), ILRetainedLists))),
                ILRetainedLists=[]
              )
        ),
        (
            (
                setof(IRRetained,retained_right_intervals(FormulaId, Tqms, LRVarsRelated, IRRetained), IRRetainedLists),
                setof_empty(RIL,RVarsUnrelated^RFormula,RILists)
            );
            (
                setof_empty(RIL,RVarsUnrelated^RFormula,RILists),
                \+setof(IRRetained,retained_right_intervals(FormulaId, Tqms, LRVarsRelated, IRRetained), IRRetainedLists),
                IRRetainedLists=[]
            )
        ),
        merge_temporal_information_lists(LType, LILists, MergedLIList),
        merge_temporal_information_lists(RType, RILists, MergedRIList),
        merge_temporal_information_lists(LType, ILRetainedLists,RetainedLIL),
        merge_temporal_information_lists(RType, IRRetainedLists,RetainedRIL),
        %merge retained temporal information list for L formula with current temporal
        %information list of L formula
        merge_temporal_information_lists(LType,[RetainedLIL,MergedLIList],LILcombined),
        merge_temporal_information_lists(RType,[RetainedRIL,MergedRIList],RILcombined),
        convert_points_to_pis(LILcombined,LFI),
        convert_points_to_pis(RILcombined,RFI),
        compute_relation_intervals(Relation,LFI,RFI,Tq,IL),
        %retain intervals that end before tcrit and participate in recognised intervals that overlap tcrit
        %retain_relation_formula_temp_info(before,LRVarsRelated,FormulaId,Tcrit,AP)
        retain_left_right_relation_intervals(FormulaId, IL, LType, RType, LILcombined, RILcombined, LRVarsRelated, LRVarsRelated, Tq)
    ),
    FormulaIdp1 is FormulaId+1,phe_setval(formula_id,FormulaIdp1).


%Retained information access
%--------------------------- Phi^.--------------------------------
retain_subformulae_instant(l, FormulaId, LRVarsRelated, Tq, T, t):-
    assert_if_not_exists(instant_left_retained(FormulaId, Tq, LRVarsRelated, T,t)).
retain_subformulae_instant(r, FormulaId, LRVarsRelated, Tq, T, t):-
    assert_if_not_exists(instant_right_retained(FormulaId, Tq, LRVarsRelated, T,t)).
retain_subformulae_instant(_, _FormulaId,  _LRVarsRelated, _Tq, _T, u).
%--------------------------- Phi^- -------------------------------
retain_starting_ending_points(FormulaId, I, SPoints, EPoints, SVarsRelated, EVarsRelated, Tq):-
   phe_getval(tqmw,Tqmw),
   (
    (
       member(([Ts,Te],V),I),
       (
           (V=u, Ts > Tqmw , Tc = Ts)
           ;
           (V=t, Te = inf, Tc = Ts)
       ),!,
    
       (V\=t -> findall((T,t), (member((T,t), SPoints),T >=Tc), SPointsToRetain) ;
                findall((T,t), (member((T,t), SPoints),T =Tc), SPointsToRetain)),
       (V\=t -> findall((T,t), (member((T,t), EPoints), T >= Tc) , EPointsToRetain) ;
           EPointsToRetain=[] ),
      (SPointsToRetain \= [] -> 
        assert_if_not_exists(retained_left_instants(FormulaId, Tq, SVarsRelated, SPointsToRetain)) ; true),
      (EPointsToRetain \= [] -> 
        assert_if_not_exists(retained_right_instants(FormulaId, Tq, EVarsRelated, EPointsToRetain)) ; true)
    );
    (
        true
    )
   ).

retain_left_right_intervals(FormulaId, IL, LILists, RILists, LVarsRelated, RVarsRelated, Tq):-
    phe_getval(tqmw,Tqmw),
       (
        (
           member(([Ts,Te],V),IL),
           (
               (V=u, Ts > Tqmw, Tc = Ts)
               ;
               (V=t, Te = inf, Tc is Ts-1)
           ),!, 
           findall(([Tsi,Te1],t), (member(([Tsi,Tei],t), LILists), (Tei>=Tc), ((Tei\=inf,Te1=Tei);(Tei=inf,Te1=Tq))), LIToRetain),
           findall(([Tsi,Te1],t), (member(([Tsi,Tei],t), RILists), (Tei>=Tc), ((Tei\=inf,Te1=Tei);(Tei=inf,Te1=Tq))) , RIToRetain),
          (LIToRetain \= [] -> 
            assert_if_not_exists(retained_left_intervals(FormulaId, Tq, LVarsRelated, LIToRetain)) ; true),
          (RIToRetain \= [] -> 
            assert_if_not_exists(retained_right_intervals(FormulaId, Tq, RVarsRelated, RIToRetain)) ; true)
        );
        (
            true
        )
       ).

retain_left_right_relation_intervals(FormulaId, IL, LType, RType, LILists, RILists, LVarsRelated, RVarsRelated, Tq):-
    phe_getval(tqmw,Tqmw),
       (
        (
           member(([Ts,Te],V),IL),
           (
               (V=u, Ts > Tqmw, Tc = Ts)
               ;
               (V=t, Te = inf, Tc = Ts)
           ),!, 
            (
              (LType = dinterval, findall(([Tsi,Te1],t), (member(([Tsi,Tei],t), LILists),Tsi >=Tc, ((Tei\=inf,Te1=Tei);(Tei=inf,Te1=Tq))), LIToRetain));
              (LType = ndinterval, findall(([Tsi,Tei],t), (member(([Tsi,Tei],t), LILists),Tsi >=Tc), LIToRetain));
              (LType = instant, findall((T,t), (member((T,t), LILists),T >=Tc), LIToRetain))
            ),
            (
              (RType = dinterval, findall(([Tsi,Te1],t), (member(([Tsi,Tei],t), RILists), Tsi >= Tc, ((Tei\=inf,Te1=Tei);(Tei=inf,Te1=Tq))) , RIToRetain));
              (RType = ndinterval, findall(([Tsi,Tei],t), (member(([Tsi,Tei],t), RILists), Tsi >= Tc) , RIToRetain));
              (RType = instant, findall((T,t), (member((T,t), RILists), T >= Tc) , RIToRetain))
            ),
            (
                LIToRetain \= [] -> 
                    assert_if_not_exists(retained_left_intervals(FormulaId, Tq, LVarsRelated, LIToRetain)) ; true
            ),
            (
                RIToRetain \= [] -> 
                    assert_if_not_exists(retained_right_intervals(FormulaId, Tq, RVarsRelated, RIToRetain)) ; true
            )
        );
        (
            true
        )
       ).

