%%
%%------------------------ Phi handling --------------------------
%%
%% Takes a formula of Phi and transforms it accordingly for
%% prolog
transform_formula(Phenomenon, TransformedFormula, TemporalInformationList):-
    phenomenon_type(Phenomenon,PhenomenonType,user),
    phenomenon_conditions(Phenomenon, Formula),
    term_variables(Phenomenon, Variables),
    transform_formula2(PhenomenonType, Formula, Variables, TransformedFormula, TemporalInformationList).

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
%% prolog
transform_instant_formula(aand(L,R), PheVars, ProcessedFormula, T):-!,
    transform_instant_formula(L, PheVars, LAt,  T),
    ProcessedFormula=(
        LAt,R
    ).

% conjunction
transform_instant_formula(and(L,R), PheVars, ProcessedFormula, T):-!,
    term_variables([PheVars,L],LPheVars),
    term_variables([PheVars,R],RPheVars),
    transform_instant_formula(L, RPheVars, LAt, T),
    transform_instant_formula(R, LPheVars, RAt, T),
    ProcessedFormula=( (LAt,RAt); (RAt,LAt )).

% disjunction
transform_instant_formula(or(L,R), PheVars, ProcessedFormula, T):-!,
    term_variables([PheVars,L],LPheVars),
    term_variables([PheVars,R],RPheVars),
    transform_instant_formula(L, RPheVars, LAt, T),
    transform_instant_formula(R, LPheVars, RAt, T),
    ProcessedFormula=( LAt;RAt ).

%returns the instants in the window where the negated formula
%does not hold
transform_instant_formula(tnot(R), PheVars, ProcessedFormula, T):-!,
    transform_instant_formula(R, PheVars, RAt, Ti),
    term_variables(RAt,RAtVars),
    variable_list_diff(RAtVars,[Ti|PheVars],RAtVarsUnrelated),
    ProcessedFormula=(
        setof_empty(Ti,RAtVarsUnrelated^RAt,RInstantList),
        phe_getval(current_window_instants,InstantListW),
        ord_subtract(InstantListW,RInstantList,InstantList),
        member(T,InstantList)
    ).

%input event
transform_instant_formula(Formula, _PheVars, ProcessedFormula, T):-
    phenomenon_type(Formula,event,input),!,
    ProcessedFormula=(input_event_instant(Formula,T)).

%user event (NOTE! User events use instant lists)
transform_instant_formula(Formula, _PheVars, ProcessedFormula,  T):-
    phenomenon_type(Formula,event,user),!,
    ProcessedFormula=(event_instants(Formula,InstantList),member(T,InstantList)).

% start operator
transform_instant_formula(start(DFormula), PheVars, ProcessedFormula, T):-!,
    transform_dinterval_formula(DFormula, PheVars, DPFormula, PIL),
    term_variables(DPFormula,PFVars),
    variable_list_diff(PFVars, [PIL|PheVars], PFVarsUnrelated),
    ProcessedFormula=(
        setof_empty(PIL,PFVarsUnrelated^DPFormula,ZIL),
        merge_disjoint_interval_lists(ZIL,IL),
        member([T,_],IL)
    ).

% end operator
transform_instant_formula(end(DFormula), PheVars, ProcessedFormula, T):-!,
    transform_dinterval_formula(DFormula, PheVars, DPFormula, PIL),
    term_variables(DPFormula,PFVars),
    variable_list_diff(PFVars, [PIL|PheVars], PFVarsUnrelated),
    ProcessedFormula=(
        setof_empty(PIL,PFVarsUnrelated^DPFormula,ZIL),
        merge_disjoint_interval_lists(ZIL,IL),
        member([_,T],IL)
    ).

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
    maximal_interval_computation_formula(Lt, Rt, LTs, LTe, IL, PheVars, ProcessedFormula).

transform_dinterval_formula(Formula, PheVars, ProcessedFormula, IL):-
    Formula=..[OP,L,R],
    member(OP,[union,intersection,complement]),!,
    term_variables([L,PheVars],LPheVars),
    term_variables([R,PheVars],RPheVars),
    transform_dinterval_formula(L,RPheVars,Lt,LIL),
    transform_dinterval_formula(R,LPheVars,Rt,RIL),
    tset_computation_formula(OP,Lt,Rt,LIL,RIL,IL,PheVars,ProcessedFormula).

transform_dinterval_formula(Formula, _PheVars, ProcessedFormula, IL):-
    phenomenon_type(Formula,state,user),
    ProcessedFormula=(state_intervals(Formula,IL)).

transform_dinterval_formula(Formula, _PheVars, ProcessedFormula, IL):-
    phenomenon_type(Formula,state,input),
    ProcessedFormula=(input_state_interval(Formula,I),IL=[I]).


tset_computation_formula(OP,LFormula,RFormula,LIL,RIL,IL,PheVars,ProcessedFormula):-
    phe_getval(formula_id,FormulaId),
    term_variables(LFormula,LFormulaVars),
    term_variables(RFormula,RFormulaVars),
    variable_list_diff(LFormulaVars,[LIL|PheVars],LVarsUnrelated),
    variable_list_diff(RFormulaVars,[RIL|PheVars],RVarsUnrelated),
    term_variables([LFormulaVars,RFormulaVars],LRVars),
    variable_list_intersection(LRVars,PheVars,LRVarsRelatedWithT),
    variable_list_diff(LRVarsRelatedWithT,[LIL,RIL],LRVarsRelated),
    ProcessedFormula=(
          (
           (
               setof(LIL,LVarsUnrelated^LFormula,LILists),
               setof_empty(RIL,RVarsUnrelated^RFormula,RILists)
           );
           (
               setof(RIL,RVarsUnrelated^RFormula,RILists),
               \+setof(LIL,LVarsUnrelated^LFormula,_),
               LILists=[]
           )
          ),
          merge_disjoint_interval_lists(LILists,LIListsUnion),
          merge_disjoint_interval_lists(RILists,RIListsUnion),
          phe_getval(tqmw,Tqmw),phe_getval(tcrit,Tcrit),
          %split information on window start to avoid rewriting history
          split_on_t(Tqmw,LIListsUnion,_LILbeforeTqmw,LILafterTqmw),
          split_on_t(Tqmw,RIListsUnion,_RILbeforeTqmw,RILafterTqmw),
          merge_ilse(LILafterTqmw,RILafterTqmw,SEL),
          compute_tset_intervals(OP,SEL,CurrentWIL),
          %MERGE WITH PREVIOUS
          get_retained_tset_formula(OP,LRVarsRelated,FormulaId,Tqmw,RetainedIL),
          split_on_t(Tqmw,RetainedIL,RetainedILbt,_RetainedILat),
          splice_interval_sets(RetainedILbt,CurrentWIL,IL),
          retain_tset_formula(IL,OP,LRVarsRelated,FormulaId,Tcrit)
    ),
    FormulaIdp1 is FormulaId+1,phe_setval(formula_id,FormulaIdp1).


compute_tset_intervals(union,SEL,IL):-compute_union_intervals(SEL,0,0,_,IL).
compute_tset_intervals(intersection,SEL,IL):-compute_intersection_intervals(SEL,0,0,_,IL).
compute_tset_intervals(complement,SEL,IL):-compute_complement_intervals(SEL,0,0,_,IL).

maximal_interval_computation_formula(StartingFormula,EndingFormula,Ts,Te,IL,PheVars,ProcessedFormula):-
    phe_getval(formula_id,FormulaId),
    term_variables(StartingFormula,SVars),
    term_variables(EndingFormula,EVars),
    variable_list_diff(SVars,[Ts|PheVars],SVarsUnrelated),
    variable_list_diff(EVars,[Te|PheVars],EVarsUnrelated),
    variable_list_diff(SVars,SVarsUnrelated,SVarsRelated),
    ProcessedFormula=(
        phe_getval(tqmw,Tqmw),phe_getval(tcrit,Tcrit),
        (
          (
           setof(TsRetained,retained_starting_formula(SVarsRelated,TsRetained,FormulaId,Tqmw),StartingPointsRetained),
           setof_empty(Ts,SVarsUnrelated^StartingFormula,StartingPointsNew)
          );
          (
           setof_empty(Ts,SVarsUnrelated^StartingFormula,StartingPointsNew),
           \+setof(TsRetained,retained_starting_formula(SVarsRelated,TsRetained,FormulaId,Tqmw),_),
           StartingPointsRetained=[]
          )
        ),
        ord_union(StartingPointsNew,StartingPointsRetained,StartingPoints),
        setof_empty(Te,EVarsUnrelated^EndingFormula,EndingPoints),
        merge_se(StartingPoints,EndingPoints,SEList),
        compute_maximal_intervals(SEList,(_,n),IL),
        retain_starting_formula(IL, SVarsRelated, FormulaId, Tcrit)
    ),
    FormulaIdp1 is FormulaId+1,phe_setval(formula_id,FormulaIdp1).


%--------------------------------------------------------------


%%
%%------------------------ Phi^= handling --------------------------
%%
%% Takes a formula of Phi^= and transforms it accordingly for
%% prolog
allowed_formulae(before,[instant,dinterval,ndinterval],[instant,dinterval,ndinterval]).
allowed_formulae(meets,[dinterval,ndinterval],[dinterval,ndinterval]).
allowed_formulae(overlaps,[instant,dinterval,ndinterval],[instant,dinterval,ndinterval]).
allowed_formulae(finishes,[instant,dinterval,ndinterval],[dinterval,ndinterval]).
allowed_formulae(starts,[instant,dinterval,ndinterval],[dinterval,ndinterval]).
allowed_formulae(equals,[dinterval,ndinterval],[dinterval,ndinterval]).
allowed_formulae(contains,[dinterval,ndinterval],[instant,dinterval,ndinterval]).

transform_ndinterval_formula_internal(Formula, Allowed, OtherVars, TransformedFormula, TemporalInformationList, FormulaType):-
    ((member(instant,Allowed), transform_instant_formula(Formula, OtherVars, TransformedFormula, TemporalInformationList), FormulaType=instant);
     (member(dinterval,Allowed), transform_dinterval_formula(Formula, OtherVars, TransformedFormula, TemporalInformationList), FormulaType=dinterval);
     (member(dinterval,Allowed), transform_ndinterval_formula(Formula, OtherVars, TransformedFormula, TemporalInformationList), FormulaType=ndinterval)).


transform_ndinterval_formula(Formula, PheVars, ProcessedFormula, IL):-
    Formula=..[Relation, L, R],!,
    allowed_formulae(Relation, AllowedL, AllowedR),
    term_variables([L,PheVars],LPheVars),
    term_variables([R,PheVars],RPheVars),
    transform_ndinterval_formula_internal(L, AllowedL, RPheVars, LTransformed, LTIL, LType),
    transform_ndinterval_formula_internal(R, AllowedR, LPheVars, RTransformed, RTIL, RType),
    formulae_ints_type(LType,RType,FType),
    relation_intervals_formula(Relation, FType, LType, RType, LTransformed, RTransformed, LTIL, RTIL, IL, PheVars, ProcessedFormula).

transform_ndinterval_formula(Formula, _PheVars, ProcessedFormula, IL):-
    phenomenon_type(Formula,dynamic_phenomenon,user),
    ProcessedFormula=(dynamic_phenomenon_intervals_internal(Formula,IL); \+dynamic_phenomenon_intervals_internal(Formula,_),IL=[]).

transform_ndinterval_formula(Formula, _PheVars, ProcessedFormula, IL):-
    phenomenon_type(Formula,dynamic_phenomenon,input),
    ProcessedFormula=(input_dynamic_phenomenon_interval(Formula,I),IL=[I]).

relation_intervals_formula(before, FT, LType, RType, LFormula, RFormula, LIL, RIL, IL, PheVars, ProcessedFormula):-
    !,phe_getval(formula_id,FormulaId),
    term_variables(LFormula,LFormulaVars),
    term_variables(RFormula,RFormulaVars),
    variable_list_diff(LFormulaVars, [LIL|PheVars], LVarsUnrelated),
    variable_list_diff(RFormulaVars, [RIL|PheVars], RVarsUnrelated),
    variable_list_intersection(LFormulaVars,PheVars,LVarsRelatedWithT),
    variable_list_diff(LVarsRelatedWithT,[LIL],LVarsRelated),
    ProcessedFormula=(
        phe_getval(tqmw,Tqmw),phe_getval(tcrit,Tcrit),
        (
          (
            setof(RetainedLILi,retained_relation_formula_temp_info(before,LVarsRelated,FormulaId,Tqmw,RetainedLILi),RetainedLILs),
            setof_empty(LIL,LVarsUnrelated^LFormula,LILists)
          );
          (
            setof_empty(LIL,LVarsUnrelated^LFormula,LILists),
            \+setof(RetainedLILi,retained_relation_formula_temp_info(before,LVarsRelated,FormulaId,Tqmw,RetainedLILi),_),
            RetainedLILs=[]
          )
        ),
        setof_empty(RIL,RVarsUnrelated^RFormula,RILists),
        merge_temporal_information_lists(LType, LILists,MergedLIList),
        merge_temporal_information_lists(RType, RILists,MergedRIList),
        merge_temporal_information_lists(LType, RetainedLILs,RetainedLIL),
        %merge retained temporal information list for L formula with current temporal
        %information list of L formula
        merge_temporal_information_lists(LType,[RetainedLIL,MergedLIList],LILcombined),
        compute_before_intervals(FT,LILcombined,MergedRIList,IL,AP,Tcrit),
        %retain intervals that end before tcrit and participate in recognised intervals that overlap tcrit
        retain_relation_formula_temp_info(before,LVarsRelated,FormulaId,Tcrit,AP)
    ),
    FormulaIdp1 is FormulaId+1,phe_setval(formula_id,FormulaIdp1).


relation_intervals_formula(Relation, FT, LType, RType,LFormula, RFormula, LIL, RIL, IL, PheVars, ProcessedFormula):-
    member(Relation,[meets,overlaps,starts]),!,
    phe_getval(formula_id,FormulaId),
    term_variables(LFormula,LFormulaVars),
    term_variables(RFormula,RFormulaVars),
    variable_list_diff(LFormulaVars, [LIL|PheVars], LVarsUnrelated),
    variable_list_diff(RFormulaVars, [RIL|PheVars], RVarsUnrelated),
    variable_list_intersection(LFormulaVars,PheVars,LVarsRelatedWithT),
    variable_list_diff(LVarsRelatedWithT,[LIL],LVarsRelated),
    ProcessedFormula=(
        phe_getval(tqmw,Tqmw),phe_getval(tcrit,Tcrit),
        (
          (
            setof(RetainedLILi,retained_relation_formula_temp_info(Relation,LVarsRelated,FormulaId,Tqmw,RetainedLILi),RetainedLILs),
            setof_empty(LIL,LVarsUnrelated^LFormula,LILists)
          );
          (
            setof_empty(LIL,LVarsUnrelated^LFormula,LILists),
            \+setof(RetainedLILi,retained_relation_formula_temp_info(Relation,LVarsRelated,FormulaId,Tqmw,RetainedLILi),_),
            RetainedLILs=[]
          )
        ),
        setof_empty(RIL,RVarsUnrelated^RFormula,RILists),
        merge_temporal_information_lists(LType, LILists,MergedLIList),
        merge_temporal_information_lists(RType, RILists,MergedRIList),
        merge_temporal_information_lists(LType, RetainedLILs,RetainedLIL),
        ord_merge(RetainedLIL,MergedLIList,LILcombined),
        compute_relation_intervals(Relation, FT, LILcombined, MergedRIList, IL, NonRedundantA, Tcrit),
        retain_relation_formula_temp_info(Relation,LVarsRelated,FormulaId,Tcrit,NonRedundantA)
    ),
    FormulaIdp1 is FormulaId+1,phe_setval(formula_id,FormulaIdp1).

relation_intervals_formula(contains, FT, LType, RType, LFormula, RFormula, LIL, RIL, IL, PheVars, ProcessedFormula):-
    !,phe_getval(formula_id,FormulaId),
    term_variables(LFormula,LFormulaVars),
    term_variables(RFormula,RFormulaVars),
    variable_list_diff(LFormulaVars, [LIL|PheVars], LVarsUnrelated),
    variable_list_diff(RFormulaVars, [RIL|PheVars], RVarsUnrelated),
    term_variables([LFormulaVars,RFormulaVars],LRVars),
    variable_list_intersection(LRVars,PheVars,LRVarsRelatedWithT),
    variable_list_diff(LRVarsRelatedWithT,[LIL,RIL],LRVarsRelated),
    ProcessedFormula=(
        phe_getval(tqmw,Tqmw),phe_getval(tcrit,Tcrit),
        setof_empty(LIL,LVarsUnrelated^LFormula,LILists),
        setof_empty(RIL,RVarsUnrelated^RFormula,RILists),
        merge_temporal_information_lists(LType, LILists,MergedLIList),
        merge_temporal_information_lists(RType, RILists,MergedRIList),
        get_retained_relation_formula_temp_info(contains,LRVarsRelated,FormulaId,Tqmw,RetainedRIL),
        merge_temporal_information_lists(RType, [RetainedRIL,MergedRIList], RILcombined),
        compute_relation_intervals(contains, FT, MergedLIList, RILcombined, IL, NonRedundantB, Tcrit),
        retain_relation_formula_temp_info(contains,LRVarsRelated,FormulaId,Tcrit,NonRedundantB)
    ),
    FormulaIdp1 is FormulaId+1,phe_setval(formula_id,FormulaIdp1).

relation_intervals_formula(finishes, FT, LType, RType, LFormula, RFormula, LIL, RIL, IL, PheVars, ProcessedFormula):-
    !,term_variables(LFormula,LFormulaVars),
    term_variables(RFormula,RFormulaVars),
    variable_list_diff(LFormulaVars, [LIL|PheVars], LVarsUnrelated),
    variable_list_diff(RFormulaVars, [RIL|PheVars], RVarsUnrelated),
    ProcessedFormula=(
        setof_empty(LIL,LVarsUnrelated^LFormula,LILists),
        setof_empty(RIL,RVarsUnrelated^RFormula,RILists),
        merge_temporal_information_lists(LType, LILists,MergedLIList),
        merge_temporal_information_lists(RType, RILists,MergedRIList),
        phe_getval(tcrit,Tcrit),
        compute_relation_intervals(finishes, FT, MergedLIList, MergedRIList, IL, Tcrit)
    ).

relation_intervals_formula(Relation, FT, LType, RType, LFormula, RFormula, LIL, RIL, IL, PheVars, ProcessedFormula):-
    term_variables(LFormula,LFormulaVars),
    term_variables(RFormula,RFormulaVars),
    variable_list_diff(LFormulaVars, [LIL|PheVars], LVarsUnrelated),
    variable_list_diff(RFormulaVars, [RIL|PheVars], RVarsUnrelated),
    ProcessedFormula=(
        setof_empty(LIL,LVarsUnrelated^LFormula,LILists),
        setof_empty(RIL,RVarsUnrelated^RFormula,RILists),
        merge_temporal_information_lists(LType, LILists,MergedLIList),
        merge_temporal_information_lists(RType, RILists,MergedRIList),
        compute_relation_intervals(Relation, FT, MergedLIList, MergedRIList, IL)
    ).



%Retained information access
%--------------------------- Phi^- -------------------------------
retain_starting_formula([],_,_,_).
retain_starting_formula([[Ts,Te]|R], StartingFormula, FormulaId, Tcrit):-
    \+((Te > Tcrit,Ts =< Tcrit)),
    retain_starting_formula(R,StartingFormula,FormulaId, Tcrit).
retain_starting_formula([[Ts,Te]|_R], StartingFormula, FormulaId, Tcrit):-
    Te > Tcrit,
    Ts =< Tcrit,
    retained_starting_formula((StartingFormula),Ts,FormulaId,Tcrit),!.
retain_starting_formula([[Ts,Te]|_R], StartingFormula, FormulaId, Tcrit):-
    Te > Tcrit,
    Ts =< Tcrit,
    assertz(retained_starting_formula((StartingFormula),Ts,FormulaId,Tcrit)).

get_retained_tset_formula(OP,PheVars,FormulaId,Tqmw,RetainedIL):-
    retained_tset_formula_intervals(OP,PheVars,FormulaId,Tqmw,RetainedIL).
get_retained_tset_formula(OP,PheVars,FormulaId,Tqmw,[]):-
    \+(retained_tset_formula_intervals(OP,PheVars,FormulaId,Tqmw,_)).

retain_tset_formula(IL,_OP,_PheVars,_FormulaId,Tcrit):-
    retain_tset_formula_intervals(IL, [], Tcrit),!.
retain_tset_formula(IL,OP,PheVars,FormulaId,Tcrit):-
    retain_tset_formula_intervals(IL, ILR, Tcrit),
    retained_tset_formula_intervals(OP,PheVars,FormulaId,Tcrit,ILR),!.
retain_tset_formula(IL,OP,PheVars,FormulaId,Tcrit):-
    retain_tset_formula_intervals(IL, ILR, Tcrit),
    assertz(retained_tset_formula_intervals(OP,PheVars,FormulaId,Tcrit,ILR)).

retain_tset_formula_intervals([], [], _).
retain_tset_formula_intervals([[Ts,Te]|ILTail], ILR, Tcrit):-
    \+((Te > Tcrit,Ts =< Tcrit)),
    retain_tset_formula_intervals(ILTail, ILR, Tcrit).
retain_tset_formula_intervals([[Ts,Te]|ILTail], [[Ts,Te]|ILR], Tcrit):-
    Te > Tcrit, Ts =< Tcrit,
    retain_tset_formula_intervals(ILTail, ILR, Tcrit).


%--------------------------- Phi^= -------------------------------
get_retained_relation_formula_temp_info(OP,PheVars,FormulaId,Tqmw,RetainedLIL):-
    retained_relation_formula_temp_info(OP,PheVars,FormulaId,Tqmw,RetainedLIL).
get_retained_relation_formula_temp_info(OP,PheVars,FormulaId,Tqmw,[]):-
    \+retained_relation_formula_temp_info(OP,PheVars,FormulaId,Tqmw,_).

retain_relation_formula_temp_info(_,_,_,_,[]):-!.
retain_relation_formula_temp_info(Relation,PheVars,FormulaId,Tcrit,AP):-
    retained_relation_formula_temp_info(Relation,PheVars,FormulaId,Tcrit,AP),!.
retain_relation_formula_temp_info(Relation,PheVars,FormulaId,Tcrit,AP):-
    assertz(retained_relation_formula_temp_info(Relation,PheVars,FormulaId,Tcrit,AP)).

