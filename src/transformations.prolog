%%
%%------------------------ Phi handling --------------------------
%%
%% Takes a formula of Phi and transforms it accordingly for
%% prolog
transform_formula(Phenomenon,TransformedFormula,TL):-
    phenomenon_type(Phenomenon,event,user),
    phenomenon_conditions(Phenomenon,Formula),
    term_variables(Phenomenon, Variables),
    transform_instant_formula(Formula, Variables, ProcessedFormula, PTL),
    term_variables(ProcessedFormula,PFVars),
    variable_list_diff(PFVars, [PTL|Variables], PFStVars),
    TransformedFormula=(
        setof_empty(PTL,PFStVars^ProcessedFormula,TL)).

transform_formula(Phenomenon,TransformedFormula,IL):-
    phenomenon_type(Phenomenon,state,user),
    phenomenon_conditions(Phenomenon,Formula),
    term_variables(Phenomenon, Variables),
    transform_dinterval_formula(Formula, Variables, ProcessedFormula, PIL),
    term_variables(ProcessedFormula,PFVars),
    variable_list_diff(PFVars, [PIL|Variables], PFStVars),
    TransformedFormula=(
        setof_empty(PIL,PFStVars^ProcessedFormula,ZIL),
        merge_disjoint_interval_lists(ZIL,IL)
    ).

transform_formula(Phenomenon,TransformedFormula,IL):-
    phenomenon_type(Phenomenon,dynamic_phenomenon,user),
    phenomenon_conditions(Phenomenon,Formula),
    term_variables(Phenomenon, Variables),
    transform_ndinterval_formula(Formula, Variables, ProcessedFormula, PIL),
    term_variables(ProcessedFormula,PFVars),
    variable_list_diff(PFVars, [PIL|Variables], PFStVars),
    TransformedFormula=(
        setof_empty(PIL,PFStVars^ProcessedFormula,ZIL),
        merge_non_disjoint_interval_lists(ZIL,IL)
    ).

%%
%%------------------------ Phi^. handling --------------------------
%%
%% Takes a formula of Phi^. and transforms it accordingly for
%% prolog
transform_instant_formula(Formula, PheVars, ProcessedFormula, T):-
    Formula=..[aand,L,R],
    transform_instant_formula(L, PheVars, LAt,  T),
    ProcessedFormula=(
        LAt,R
    ).


transform_instant_formula(Formula, PheVars, ProcessedFormula, T):-
    Formula=..[and,L,R],
    term_variables([PheVars,L],LPheVars),
    term_variables([PheVars,R],RPheVars),
    transform_instant_formula(L, RPheVars, LAt, T),
    transform_instant_formula(R, LPheVars, RAt, T),
    ProcessedFormula=( (LAt,RAt); (RAt,LAt )).

transform_instant_formula(Formula, PheVars, ProcessedFormula, T):-
    Formula=..[or,L,R],
    term_variables([PheVars,L],LPheVars),
    term_variables([PheVars,R],RPheVars),
    transform_instant_formula(L, RPheVars, LAt, T),
    transform_instant_formula(R, LPheVars, RAt, T),
    ProcessedFormula=( LAt;RAt ).

%returns the instants in the window where the negated formula
%does not hold
transform_instant_formula(Formula, PheVars, ProcessedFormula, T):-
    Formula=..[tnot,R],!,
    transform_instant_formula(R, PheVars, RAt, Ti),
    term_variables(RAt,RAtVars),
    variable_list_diff(RAtVars,[Ti|PheVars],RAtVarsUnrelated),
    ProcessedFormula=(
        setof_empty(Ti,RAtVarsUnrelated^RAt,TL),
        my_getval(current_window_instants,InstantListW),
        ord_subtract(InstantListW,TL,InstantList),
        member(T,InstantList)
    ).

transform_instant_formula(Formula, _PheVars, ProcessedFormula, T):-
    phenomenon_type(Formula,event,input),!,
    ProcessedFormula=(event_instant(Formula,T)).

transform_instant_formula(Formula, _PheVars, ProcessedFormula,  T):-
    phenomenon_type(Formula,event,user),!,
    ProcessedFormula=(event_instants(Formula,TL),member(T,TL)).

transform_instant_formula(Formula, PheVars, ProcessedFormula, T):-
    Formula=start(DFormula),!,
    transform_dinterval_formula(DFormula, PheVars, DPFormula, PIL),
    term_variables(DPFormula,PFVars),
    variable_list_diff(PFVars, [PIL|PheVars], PFVarsUnrelated),
    ProcessedFormula=(
        setof_empty(PIL,PFVarsUnrelated^DPFormula,ZIL),
        merge_disjoint_interval_lists(ZIL,IL),
        member([T,_],IL)
    ).

transform_instant_formula(Formula, PheVars, ProcessedFormula, T):-
    Formula=end(DFormula),!,
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
transform_dinterval_formula(Formula,PheVars, ProcessedFormula, IL):-
    Formula=..[~>,L,R],!,
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
    phenomenon_type(Formula,state,_),
    ProcessedFormula=(state_intervals(Formula,IL)).


tset_computation_formula(OP,LFormula,RFormula,LIL,RIL,IL,PheVars,ProcessedFormula):-
    my_getval(formula_id,FormulaId),
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
          my_getval(tqmw,Tqmw),my_getval(tcrit,Tcrit),
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
    FormulaIdp1 is FormulaId+1,my_setval(formula_id,FormulaIdp1).

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
    assert(retained_tset_formula_intervals(OP,PheVars,FormulaId,Tcrit,ILR)).

retain_tset_formula_intervals([], [], _).
retain_tset_formula_intervals([[Ts,Te]|ILTail], ILR, Tcrit):-
    \+((Te > Tcrit,Ts =< Tcrit)),
    retain_tset_formula_intervals(ILTail, ILR, Tcrit).
retain_tset_formula_intervals([[Ts,Te]|ILTail], [[Ts,Te]|ILR], Tcrit):-
    Te > Tcrit, Ts =< Tcrit,
    retain_tset_formula_intervals(ILTail, ILR, Tcrit).

compute_tset_intervals(union,SEL,IL):-compute_union_intervals(SEL,0,0,_,IL).
compute_tset_intervals(intersection,SEL,IL):-compute_intersection_intervals(SEL,0,0,_,IL).
compute_tset_intervals(complement,SEL,IL):-compute_complement_intervals(SEL,0,0,_,IL).

maximal_interval_computation_formula(StartingFormula,EndingFormula,Ts,Te,IL,PheVars,ProcessedFormula):-
    my_getval(formula_id,FormulaId),
    term_variables(StartingFormula,SVars),
    term_variables(EndingFormula,EVars),
    variable_list_diff(SVars,[Ts|PheVars],SVarsUnrelated),
    variable_list_diff(EVars,[Te|PheVars],EVarsUnrelated),
    variable_list_diff(SVars,SVarsUnrelated,SVarsRelated),
    ProcessedFormula=(
        my_getval(tqmw,Tqmw),my_getval(tcrit,Tcrit),
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
    FormulaIdp1 is FormulaId+1,my_setval(formula_id,FormulaIdp1).


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
    assert(retained_starting_formula((StartingFormula),Ts,FormulaId,Tcrit)).
%--------------------------------------------------------------


%%
%%------------------------ Phi^= handling --------------------------
%%
%% Takes a formula of Phi^= and transforms it accordingly for
%% prolog
transform_ndinterval_formula(Formula, PheVars, ProcessedFormula, IL):-
    Formula=..[before, L, R],!,
    term_variables([L,PheVars],LPheVars),
    term_variables([R,PheVars],RPheVars),
    (
     (transform_dinterval_formula(L, RPheVars, Lt, LIL),LTYPE=d);
     (transform_instant_formula(L, RPheVars, Lt,LIL), LTYPE=d);
     (transform_ndinterval_formula(L, RPheVars, Lt, LIL),LTYPE=nd)
    ),
    (
     (transform_dinterval_formula(R, LPheVars, Rt, RIL),RTYPE=d);
     (transform_instant_formula(R, LPheVars, Rt, RIL),RTYPE=d);
     (transform_ndinterval_formula(R, LPheVars,Rt,RIL),RTYPE=nd)
    ),
    formulae_ints_type(LTYPE,RTYPE,FTYPE),
    before_intervals_formula(FTYPE, Lt, Rt, LIL, RIL, IL, PheVars, ProcessedFormula).

transform_ndinterval_formula(Formula, PheVars, ProcessedFormula, IL):-
    Formula=..[meets, L, R],!,
    term_variables([L,PheVars],LPheVars),
    term_variables([R,PheVars],RPheVars),
    (
     (transform_dinterval_formula(L, RPheVars, Lt, LIL),LTYPE=d);
     (transform_ndinterval_formula(L, RPheVars, Lt, LIL),LTYPE=nd)
    ),
    (
     (transform_dinterval_formula(R, LPheVars, Rt, RIL),RTYPE=d);
     (transform_ndinterval_formula(R, LPheVars,Rt,RIL),RTYPE=nd)
    ),
    formulae_ints_type(LTYPE,RTYPE,FTYPE),
    relation_intervals_formula(meets, FTYPE, Lt, Rt, LIL, RIL, IL, PheVars, ProcessedFormula).

transform_ndinterval_formula(Formula, PheVars, ProcessedFormula, IL):-
    Formula=..[overlaps, L, R],!,
    term_variables([L,PheVars],LPheVars),
    term_variables([R,PheVars],RPheVars),
    (
     (transform_dinterval_formula(L, RPheVars, Lt, LIL),LTYPE=d);
     (transform_ndinterval_formula(L, RPheVars, Lt, LIL),LTYPE=nd)
    ),
    (
     (transform_dinterval_formula(R, LPheVars, Rt, RIL),RTYPE=d);
     (transform_ndinterval_formula(R, LPheVars,Rt,RIL),RTYPE=nd)
    ),
    formulae_ints_type(LTYPE,RTYPE,FTYPE),
    relation_intervals_formula(overlaps, FTYPE, Lt, Rt, LIL, RIL, IL, PheVars, ProcessedFormula).

transform_ndinterval_formula(Formula, PheVars, ProcessedFormula, IL):-
    Formula=..[finishes, L, R],!,
    term_variables([L,PheVars],LPheVars),
    term_variables([R,PheVars],RPheVars),
    (
     (transform_dinterval_formula(L, RPheVars, Lt, LIL),LTYPE=d);
     (transform_instant_formula(L, RPheVars, Lt,LIL),LTYPE=d);
     (transform_ndinterval_formula(L, RPheVars, Lt, LIL),LTYPE=nd)
    ),
    (
     (transform_dinterval_formula(R, LPheVars, Rt, RIL),RTYPE=d);
     (transform_ndinterval_formula(R, LPheVars,Rt,RIL),RTYPE=nd)
    ),
    formulae_ints_type(LTYPE,RTYPE,FTYPE),
    relation_intervals_formula(finishes, FTYPE, Lt, Rt, LIL, RIL, IL, PheVars, ProcessedFormula).

transform_ndinterval_formula(Formula, PheVars, ProcessedFormula, IL):-
    Formula=..[starts, L, R],!,
    term_variables([L,PheVars],LPheVars),
    term_variables([R,PheVars],RPheVars),
    (
     (transform_dinterval_formula(L, RPheVars, Lt, LIL),LTYPE=d);
     (transform_instant_formula(L, RPheVars, Lt,LIL),LTYPE=d);
     (transform_ndinterval_formula(L, RPheVars, Lt, LIL),LTYPE=nd)
    ),
    (
     (transform_dinterval_formula(R, LPheVars, Rt, RIL),RTYPE=d);
     (transform_ndinterval_formula(R, LPheVars,Rt,RIL),RTYPE=nd)
    ),
    formulae_ints_type(LTYPE,RTYPE,FTYPE),
    relation_intervals_formula(starts, FTYPE, Lt, Rt, LIL, RIL, IL, PheVars, ProcessedFormula).

transform_ndinterval_formula(Formula, PheVars, ProcessedFormula, IL):-
    Formula=..[equals, L, R],!,
    term_variables([L,PheVars],LPheVars),
    term_variables([R,PheVars],RPheVars),
    (
     (transform_dinterval_formula(L, RPheVars, Lt, LIL),LTYPE=d);
     (transform_ndinterval_formula(L, RPheVars, Lt, LIL),LTYPE=nd)
    ),
    (
     (transform_dinterval_formula(R, LPheVars, Rt, RIL),RTYPE=d);
     (transform_ndinterval_formula(R, LPheVars, Rt, RIL),RTYPE=nd)
    ),
    formulae_ints_type(LTYPE,RTYPE,FTYPE),
    relation_intervals_formula(equals, FTYPE, Lt, Rt, LIL, RIL, IL, PheVars, ProcessedFormula).

transform_ndinterval_formula(Formula, PheVars, ProcessedFormula, IL):-
    Formula=..[contains, L, R],!,
    term_variables([L,PheVars],LPheVars),
    term_variables([R,PheVars],RPheVars),
    (
     (transform_dinterval_formula(L, RPheVars, Lt, LIL),LTYPE=d);
     (transform_ndinterval_formula(L, RPheVars, Lt, LIL),LTYPE=nd)
    ),
    (
     (transform_dinterval_formula(R, LPheVars, Rt, RIL),RTYPE=d);
     (transform_instant_formula(R, LPheVars, Rt, RIL),RTYPE=d);
     (transform_ndinterval_formula(R, LPheVars,Rt,RIL),RTYPE=nd)
    ),
    formulae_ints_type(LTYPE,RTYPE,FTYPE),
    relation_intervals_formula(contains, FTYPE, Lt, Rt, LIL, RIL, IL, PheVars, ProcessedFormula).

transform_ndinterval_formula(Formula, _PheVars, ProcessedFormula, IL):-
    phenomenon_type(Formula,dynamic_phenomenon,_),
    ProcessedFormula=(dynamic_phenomenon_intervals_internal(Formula,IL); \+dynamic_phenomenon_intervals_internal(Formula,_),IL=[]).

before_intervals_formula(FT, LFormula, RFormula, LIL, RIL, IL, PheVars, ProcessedFormula):-
    my_getval(formula_id,FormulaId),
    term_variables(LFormula,LFormulaVars),
    term_variables(RFormula,RFormulaVars),
    variable_list_diff(LFormulaVars, [LIL|PheVars], LVarsUnrelated),
    variable_list_diff(RFormulaVars, [RIL|PheVars], RVarsUnrelated),
    term_variables([LFormulaVars,RFormulaVars],LRVars),
    variable_list_intersection(LRVars,PheVars,LRVarsRelatedWithT),
    variable_list_diff(LRVarsRelatedWithT,[LIL,RIL],LRVarsRelated),
    ProcessedFormula=(
        my_getval(tqmw,Tqmw),my_getval(tcrit,Tcrit),
        (
          (
            setof(RetainedLILi,retained_relation_formula_intervals(before,LRVarsRelated,FormulaId,Tqmw,RetainedLILi),RetainedLILs),
            setof_empty(LIL,LVarsUnrelated^LFormula,LILists)
          );
          (
            setof_empty(LIL,LVarsUnrelated^LFormula,LILists),
            \+setof(RetainedLILi,retained_relation_formula_intervals(before,LRVarsRelated,FormulaId,Tqmw,RetainedLILi),_),
            RetainedLILs=[]
          )
        ),
        setof_empty(RIL,RVarsUnrelated^RFormula,RILists),
        merge_non_disjoint_interval_lists(LILists,MergedLIList),
        merge_non_disjoint_interval_lists(RILists,MergedRIList),
        merge_non_disjoint_interval_lists(RetainedLILs,RetainedLIL),
        %merge LIL_RETAINED with LIL cur
        ord_merge(RetainedLIL,MergedLIList,LILcombined),
        compute_before_intervals(FT,LILcombined,MergedRIList,IL,AP,Tcrit),
        %retain intervals that end before tcrit and participate in recognised intervals that overlap tcrit
        retain_relation_formula_intervals(before,LRVarsRelated,FormulaId,Tcrit,AP)
    ),
    FormulaIdp1 is FormulaId+1,my_setval(formula_id,FormulaIdp1).

get_retained_relation_intervals(OP,PheVars,FormulaId,Tqmw,RetainedLIL):-
    retained_relation_formula_intervals(OP,PheVars,FormulaId,Tqmw,RetainedLIL).
get_retained_relation_intervals(OP,PheVars,FormulaId,Tqmw,[]):-
    \+retained_relation_formula_intervals(OP,PheVars,FormulaId,Tqmw,_).

relation_intervals_formula(Relation, FT, LFormula, RFormula, LIL, RIL, IL, PheVars, ProcessedFormula):-
    member(Relation,[meets,overlaps,starts]),!,
    my_getval(formula_id,FormulaId),
    term_variables(LFormula,LFormulaVars),
    term_variables(RFormula,RFormulaVars),
    variable_list_diff(LFormulaVars, [LIL|PheVars], LVarsUnrelated),
    variable_list_diff(RFormulaVars, [RIL|PheVars], RVarsUnrelated),
    term_variables([LFormulaVars,RFormulaVars],LRVars),
    variable_list_intersection(LRVars,PheVars,LRVarsRelatedWithT),
    variable_list_diff(LRVarsRelatedWithT,[LIL,RIL],LRVarsRelated),
    ProcessedFormula=(
        my_getval(tqmw,Tqmw),my_getval(tcrit,Tcrit),
        (
          (
            setof(RetainedLILi,retained_relation_formula_intervals(Relation,LRVarsRelated,FormulaId,Tqmw,RetainedLILi),RetainedLILs),
            setof_empty(LIL,LVarsUnrelated^LFormula,LILists)
          );
          (
            setof_empty(LIL,LVarsUnrelated^LFormula,LILists),
            \+setof(RetainedLILi,retained_relation_formula_intervals(Relation,LRVarsRelated,FormulaId,Tqmw,RetainedLILi),_),
            RetainedLILs=[]
          )
        ),
        setof_empty(RIL,RVarsUnrelated^RFormula,RILists),
        merge_non_disjoint_interval_lists(LILists,MergedLIList),
        merge_non_disjoint_interval_lists(RILists,MergedRIList),
        merge_non_disjoint_interval_lists(RetainedLILs,RetainedLIL),
        ord_merge(RetainedLIL,MergedLIList,LILcombined),
        compute_relation_intervals(Relation, FT, LILcombined, MergedRIList, IL, NonRedundantA, Tcrit),
        retain_relation_formula_intervals(Relation,LRVarsRelated,FormulaId,Tcrit,NonRedundantA)
    ),
    FormulaIdp1 is FormulaId+1,my_setval(formula_id,FormulaIdp1).

relation_intervals_formula(contains, FT, LFormula, RFormula, LIL, RIL, IL, PheVars, ProcessedFormula):-
    !,my_getval(formula_id,FormulaId),
    term_variables(LFormula,LFormulaVars),
    term_variables(RFormula,RFormulaVars),
    variable_list_diff(LFormulaVars, [LIL|PheVars], LVarsUnrelated),
    variable_list_diff(RFormulaVars, [RIL|PheVars], RVarsUnrelated),
    term_variables([LFormulaVars,RFormulaVars],LRVars),
    variable_list_intersection(LRVars,PheVars,LRVarsRelatedWithT),
    variable_list_diff(LRVarsRelatedWithT,[LIL,RIL],LRVarsRelated),
    ProcessedFormula=(
        my_getval(tqmw,Tqmw),my_getval(tcrit,Tcrit),
        setof_empty(LIL,LVarsUnrelated^LFormula,LILists),
        setof_empty(RIL,RVarsUnrelated^RFormula,RILists),
        merge_non_disjoint_interval_lists(LILists,MergedLIList),
        merge_non_disjoint_interval_lists(RILists,MergedRIList),
        get_retained_relation_intervals(contains,LRVarsRelated,FormulaId,Tqmw,RetainedRIL),
        ground(PheVars),
        ord_merge(RetainedRIL,MergedRIList,RILcombined),
        compute_relation_intervals(contains, FT, MergedLIList, RILcombined, IL, NonRedundantB, Tcrit),
        retain_relation_formula_intervals(contains,LRVarsRelated,FormulaId,Tcrit,NonRedundantB)
    ),
    FormulaIdp1 is FormulaId+1,my_setval(formula_id,FormulaIdp1).

relation_intervals_formula(finishes, FT, LFormula, RFormula, LIL, RIL, IL, PheVars, ProcessedFormula):-
    term_variables(LFormula,LFormulaVars),
    term_variables(RFormula,RFormulaVars),
    variable_list_diff(LFormulaVars, [LIL|PheVars], LVarsUnrelated),
    variable_list_diff(RFormulaVars, [RIL|PheVars], RVarsUnrelated),
    !,ProcessedFormula=(
        setof_empty(LIL,LVarsUnrelated^LFormula,LILists),
        setof_empty(RIL,RVarsUnrelated^RFormula,RILists),
        merge_non_disjoint_interval_lists(LILists,MergedLIList),
        merge_non_disjoint_interval_lists(RILists,MergedRIList),
        my_getval(tcrit,Tcrit),
        compute_relation_intervals(finishes, FT, MergedLIList, MergedRIList, IL, Tcrit)
    ).

relation_intervals_formula(Relation, FT, LFormula, RFormula, LIL, RIL, IL, PheVars, ProcessedFormula):-
    term_variables(LFormula,LFormulaVars),
    term_variables(RFormula,RFormulaVars),
    variable_list_diff(LFormulaVars, [LIL|PheVars], LVarsUnrelated),
    variable_list_diff(RFormulaVars, [RIL|PheVars], RVarsUnrelated),
    ProcessedFormula=(
        setof_empty(LIL,LVarsUnrelated^LFormula,LILists),
        setof_empty(RIL,RVarsUnrelated^RFormula,RILists),
        merge_non_disjoint_interval_lists(LILists,MergedLIList),
        merge_non_disjoint_interval_lists(RILists,MergedRIList),
        compute_relation_intervals(Relation, FT, MergedLIList, MergedRIList, IL)
    ).

retain_relation_formula_intervals(_,_,_,_,[]):-!.
retain_relation_formula_intervals(Relation,PheVars,FormulaId,Tcrit,AP):-
    %writeln((before-PheVars)),
    retained_relation_formula_intervals(Relation,PheVars,FormulaId,Tcrit,AP),!.
    %writeln((after-PheVars)).
retain_relation_formula_intervals(Relation,PheVars,FormulaId,Tcrit,AP):-
    assert(retained_relation_formula_intervals(Relation,PheVars,FormulaId,Tcrit,AP)).

