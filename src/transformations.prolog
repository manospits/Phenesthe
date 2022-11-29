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
%% prolog
transform_instant_formula(aand(L,R), PheVars, ProcessedFormula, T):-!,
    term_variables([PheVars,R],RPheVars),
    transform_instant_formula(L, RPheVars, LAt,  T),
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
    variable_list_diff(RAtVars,[T|RAtVarsUnrelated],RAtVarsRelated),
    ProcessedFormula=(
        ground(RAtVarsRelated) -> 
            (Ti=T,\+(RAt))
            ;
            (\+ground(RAtVarsRelated),
            setof_empty(Ti,RAtVarsUnrelated^RAt,RInstantList),
            phe_getval(current_window_instants,InstantListW),
            ord_subtract(InstantListW,RInstantList,InstantList),
            member(T,InstantList))
    ).

%is true when the grounded formula does not gold on T
transform_instant_formula(gtnot(R), PheVars, ProcessedFormula, T):-!,
    transform_instant_formula(R, PheVars, RAt, T),
    term_variables(RAt,RAtVars),
    variable_list_diff(RAtVars,[T|PheVars],RAtVarsUnrelated),
    variable_list_diff(RAtVars,RAtVarsUnrelated,RAtVarsRelated),
    ProcessedFormula=(
        ground(RAtVarsRelated), \+(RAt)
    ).

% checks whether an instant formula occurs during a disjoint
% interval formula (A in B is is true on instants at which A is 
% true and are included in an interval at which B is true)
transform_instant_formula(in(L,R), PheVars, ProcessedFormula, T):-!,
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
    ProcessedFormula=(
        %compute all instants of left formula
        setof_empty(Ti,LVarsUnrelated^Lt,TiL),
        %compute intervals of right formula
        setof_empty(RIL,RVarsUnrelated^Rt,RILists),
        merge_disjoint_interval_lists(RILists,RILmerged),
        compute_instants_in_intervals(TiL,RILmerged,IL),
        member(T,IL)
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

% constrained iteration
transform_dinterval_formula(Formula, PheVars, ProcessedFormula, IL):-
    Formula=..[OP,L,D],
    member(OP,[<@,>=@,=@]),!,
    transform_instant_formula(L, PheVars, Lt, LTs),
    iteration_interval_computation_formula(OP,Lt, D, LTs, IL, PheVars, ProcessedFormula).

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
    ProcessedFormula=(state_intervals(Formula,IL)).

% input intervals
transform_dinterval_formula(Formula, _PheVars, ProcessedFormula, IL):-
    phenomenon_type(Formula,state,input),
    ProcessedFormula=(input_state_interval(Formula,I),IL=[I]).

% formula that computes temporal union intersection and complement 
tset_computation_formula(OP,LFormula,RFormula,LIL,RIL,IL,PheVars,ProcessedFormula):-
    phe_getval(formula_id,FormulaId),
    term_variables(LFormula,LFormulaVars),
    term_variables(RFormula,RFormulaVars),
    term_variables([LFormulaVars,PheVars],LPheVarsF),
    term_variables([RFormulaVars,PheVars],RPheVarsF),
    variable_list_diff(LFormulaVars,[LIL|RPheVarsF],LVarsUnrelated),
    variable_list_diff(RFormulaVars,[RIL|LPheVarsF],RVarsUnrelated),
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

% temporal union/intersection/complement helpers
compute_tset_intervals(union,SEL,IL):-compute_union_intervals(SEL,0,0,_,IL).
compute_tset_intervals(intersection,SEL,IL):-compute_intersection_intervals(SEL,0,0,_,IL).
compute_tset_intervals(complement,SEL,IL):-compute_complement_intervals(SEL,0,0,_,IL).

% maximal interval computation formula transformation
maximal_interval_computation_formula(StartingFormula,EndingFormula,Ts,Te,IL,PheVars,ProcessedFormula):-
    phe_getval(formula_id,FormulaId),
    term_variables(StartingFormula,SVars),
    term_variables(EndingFormula,EVars),
    % we need to pass variables of left formula to right
    % and the reverse to make sure we don't skip in 
    % set of any unifications
    term_variables([SVars,PheVars],LPheVarsF),
    term_variables([EVars,PheVars],RPheVarsF),
    variable_list_diff(SVars,[Ts|RPheVarsF],SVarsUnrelated),
    variable_list_diff(EVars,[Te|LPheVarsF],EVarsUnrelated),
    variable_list_diff(SVars,[Ts|SVarsUnrelated],SVarsRelated),
    ProcessedFormula=(
        phe_getval(tqmw,Tqmw),phe_getval(tcrit,Tcrit),
        (
          % check if there are any retained results from before
          (
           setof(TsRetained,retained_starting_formula(SVarsRelated,TsRetained,FormulaId,Tqmw),StartingPointsRetained),
           setof_empty(Ts,SVarsUnrelated^StartingFormula,StartingPointsNew)
          );
          (
          % if there aren't proceed as usual. Extra care must be take to avoid duplicates.
           setof_empty(Ts,SVarsUnrelated^StartingFormula,StartingPointsNew),
           \+setof(TsRetained,retained_starting_formula(SVarsRelated,TsRetained,FormulaId,Tqmw),_),
           StartingPointsRetained=[]
          )
        ),
        %merge old starting points with retained
        ord_union(StartingPointsNew,StartingPointsRetained,StartingPoints),
        %find ending points
        setof_empty(Te,EVarsUnrelated^EndingFormula,EndingPoints),
        %create the appropriate se list
        merge_se(StartingPoints,EndingPoints,SEList),
        %compute the maximal intervals
        compute_maximal_intervals(SEList,(_,n),IL),
        %check if you need to retain anything for next query and retain it
        retain_starting_formula(IL, SVarsRelated, FormulaId, Tcrit)
    ),
    FormulaIdp1 is FormulaId+1,phe_setval(formula_id,FormulaIdp1).

% iteration computation formula with temporal only constraints
iteration_interval_computation_formula(OP,Formula, D, Ts, IL, PheVars, ProcessedFormula):-
    D\=collector(_,_,_),!,
    phe_getval(formula_id,FormulaId),
    term_variables(Formula,SVars),
    variable_list_diff(SVars, [Ts|PheVars], SVarsUnrelated),
    variable_list_diff(SVars,SVarsUnrelated,SVarsRelated),
    ProcessedFormula=(
        phe_getval(tqmw,Tqmw),phe_getval(tcrit,Tcrit),
        (
          (
           setof(TsRetained,retained_iteration_formula_points(SVarsRelated,TsRetained,FormulaId,Tqmw),StartingPointsRetained),
           setof_empty(Ts,SVarsUnrelated^Formula,StartingPointsNew)
          );
          (
           setof_empty(Ts,SVarsUnrelated^Formula,StartingPointsNew),
           \+setof(TsRetained,retained_iteration_formula_points(SVarsRelated,TsRetained,FormulaId,Tqmw),_),
           StartingPointsRetained=[]
          )
        ),
        ord_union(StartingPointsNew,StartingPointsRetained,StartingPoints),
        compute_iteration_intervals(OP,StartingPoints,D,IIL,LastPoinToRetain),
        get_retained_iteration_formula_intervals(SVarsRelated,RI,FormulaId,Tqmw),
        splice_interval_sets_drop(RI,IIL,IL),
        retain_iteration_formula(OP,IL, LastPoinToRetain, D, SVarsRelated, FormulaId, Tcrit)
    ),
    FormulaIdp1 is FormulaId+1,phe_setval(formula_id,FormulaIdp1).

% iteration computation formula with temporal constraints and atemporal constraints on consecutive pairs
% collector(    
%               D,                : Same as above (some temporal threshold)
%               VarsToCollect,    : A list of the variables of the left formula 
%                                   you want to use in the atemporal constraints
%               PredicateName     : The predicate to call for comparing prev with current.
%                                   For example for speed_check(Prev,Cur), PredicateName
%                                   should be speed_check.
%           ) 
iteration_interval_computation_formula(OP,Formula, Collector, Ts, IL, PheVars, ProcessedFormula):-
    Collector = collector(D,VarsToCollect,PredicateName),
    phe_getval(formula_id,FormulaId),
    term_variables(Formula,SVars),
    term_variables([PheVars,VarsToCollect],PheVarsC),
    variable_list_diff(SVars, [Ts|PheVarsC], SVarsUnrelated),
    variable_list_diff(SVars,SVarsUnrelated,SVarsRelated),
    ProcessedFormula=(
        phe_getval(tqmw,Tqmw),phe_getval(tcrit,Tcrit),
        (
          (
           setof((TsRetained,VarsToCollect),retained_iteration_formula_points(SVarsRelated,(TsRetained,VarsToCollect),FormulaId,Tqmw),StartingPointsRetained),
           setof_empty((Ts,VarsToCollect),SVarsUnrelated^Formula,StartingPointsNew)
          );
          (
           setof_empty((Ts,VarsToCollect),SVarsUnrelated^Formula,StartingPointsNew),
           \+setof((TsRetained,VarsToCollect),retained_iteration_formula_points(SVarsRelated,(TsRetained,VarsToCollect),FormulaId,Tqmw),_),
           StartingPointsRetained=[]
          )
        ),
        ord_union(StartingPointsNew,StartingPointsRetained,StartingPoints),
        compute_iteration_intervals(OP,StartingPoints,PredicateName,D,IIL,LastPoinToRetain),
        get_retained_iteration_formula_intervals(SVarsRelated,RI,FormulaId,Tqmw),
        splice_data_interval_sets_drop(RI,IIL,ILD),
        retain_iteration_formula(OP, ILD, LastPoinToRetain, D, SVarsRelated, FormulaId, Tcrit),
        strip_data_from_intervals(ILD,IL)
    ),
    FormulaIdp1 is FormulaId+1,phe_setval(formula_id,FormulaIdp1).
%-------------------------------------------------------------------


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
relation_intervals_formula(before, FT, LType, RType, LFormula, RFormula, LIL, RIL, IL, PheVars, ProcessedFormula):-
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
        phe_getval(tqmw,Tqmw),phe_getval(tcrit,Tcrit),
        (
          (
            setof(RetainedLILi,retained_relation_formula_temp_info(before,LRVarsRelated,FormulaId,Tqmw,RetainedLILi),RetainedLILs),
            setof_empty(LIL,LVarsUnrelated^LFormula,LILists),
            setof_empty(RIL,RVarsUnrelated^RFormula,RILists)
          );
          (
            setof_empty(LIL,LVarsUnrelated^LFormula,LILists),
            setof_empty(RIL,RVarsUnrelated^RFormula,RILists),
            %No need to check with set of here
            \+(retained_relation_formula_temp_info(before,LRVarsRelated,FormulaId,Tqmw,RetainedLILi)),
            RetainedLILs=[]
          )
        ),
        merge_temporal_information_lists(LType, LILists,MergedLIList),
        merge_temporal_information_lists(RType, RILists,MergedRIList),
        merge_temporal_information_lists(LType, RetainedLILs,RetainedLIL),
        %merge retained temporal information list for L formula with current temporal
        %information list of L formula
        merge_temporal_information_lists(LType,[RetainedLIL,MergedLIList],LILcombined),
        compute_before_intervals(FT,LILcombined,MergedRIList,IL,AP,Tcrit),
        %retain intervals that end before tcrit and participate in recognised intervals that overlap tcrit
        retain_relation_formula_temp_info(before,LRVarsRelated,FormulaId,Tcrit,AP)
    ),
    FormulaIdp1 is FormulaId+1,phe_setval(formula_id,FormulaIdp1).


% meets, overlaps and starts are handled similarly
relation_intervals_formula(Relation, FT, LType, RType,LFormula, RFormula, LIL, RIL, IL, PheVars, ProcessedFormula):-
    member(Relation,[meets,overlaps,starts]),!,
    phe_getval(formula_id,FormulaId),
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
        phe_getval(tqmw,Tqmw),phe_getval(tcrit,Tcrit),
        (
          (
            setof(RetainedLILi,retained_relation_formula_temp_info(Relation,LRVarsRelated,FormulaId,Tqmw,RetainedLILi),RetainedLILs),
            setof_empty(LIL,LVarsUnrelated^LFormula,LILists),
            setof_empty(RIL,RVarsUnrelated^RFormula,RILists)
          );
          (
            setof_empty(LIL,LVarsUnrelated^LFormula,LILists),
            setof_empty(RIL,RVarsUnrelated^RFormula,RILists),
            %No need to check with set of here
            \+(retained_relation_formula_temp_info(Relation,LRVarsRelated,FormulaId,Tqmw,RetainedLILi)),
            RetainedLILs=[]
          )
        ),
        merge_temporal_information_lists(LType, LILists,MergedLIList),
        merge_temporal_information_lists(RType, RILists,MergedRIList),
        merge_temporal_information_lists(LType, RetainedLILs,RetainedLIL),
        ord_merge(RetainedLIL,MergedLIList,LILcombined),
        compute_relation_intervals(Relation, FT, LILcombined, MergedRIList, IL, NonRedundantA, Tcrit),
        retain_relation_formula_temp_info(Relation,LRVarsRelated,FormulaId,Tcrit,NonRedundantA)
    ),
    FormulaIdp1 is FormulaId+1,phe_setval(formula_id,FormulaIdp1).

% contains relation treatment
relation_intervals_formula(contains, FT, LType, RType, LFormula, RFormula, LIL, RIL, IL, PheVars, ProcessedFormula):-
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

% finishes relation treatment
relation_intervals_formula(finishes, FT, LType, RType, LFormula, RFormula, LIL, RIL, IL, PheVars, ProcessedFormula):-
    !,term_variables(LFormula,LFormulaVars),
    term_variables(RFormula,RFormulaVars),
    term_variables([LFormulaVars,PheVars],LPheVarsF),
    term_variables([RFormulaVars,PheVars],RPheVarsF),
    variable_list_diff(LFormulaVars, [LIL|RPheVarsF], LVarsUnrelated),
    variable_list_diff(RFormulaVars, [RIL|LPheVarsF], RVarsUnrelated),
    ProcessedFormula=(
        setof_empty(LIL,LVarsUnrelated^LFormula,LILists),
        setof_empty(RIL,RVarsUnrelated^RFormula,RILists),
        merge_temporal_information_lists(LType, LILists,MergedLIList),
        merge_temporal_information_lists(RType, RILists,MergedRIList),
        phe_getval(tcrit,Tcrit),
        compute_relation_intervals(finishes, FT, MergedLIList, MergedRIList, IL, Tcrit)
    ).

% remaining relations treatments
relation_intervals_formula(Relation, FT, LType, RType, LFormula, RFormula, LIL, RIL, IL, PheVars, ProcessedFormula):-
    term_variables(LFormula,LFormulaVars),
    term_variables(RFormula,RFormulaVars),
    term_variables([LFormulaVars,PheVars],LPheVarsF),
    term_variables([RFormulaVars,PheVars],RPheVarsF),
    variable_list_diff(LFormulaVars, [LIL|RPheVarsF], LVarsUnrelated),
    variable_list_diff(RFormulaVars, [RIL|LPheVarsF], RVarsUnrelated),
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
    assert_if_not_exists(retained_starting_formula((StartingFormula),Ts,FormulaId,Tcrit)).

get_retained_iteration_formula_intervals(IterationFormula,A,FormulaId,Tcrit):-
    retained_iteration_formula_intervals((IterationFormula),A,FormulaId,Tcrit).
get_retained_iteration_formula_intervals(IterationFormula,[],FormulaId,Tcrit):-
    \+(retained_iteration_formula_intervals((IterationFormula),_,FormulaId,Tcrit)).


retain_iteration_formula(_,[],[], _,_,_,_).
%no intervals only point A that doesn't matter
retain_iteration_formula(OP, [], [AA], D, _, _, Tcrit):-
    (   
        (
            AA\=(_,_),
            AA=A
        );
        (
            AA=(A,_)
        )
    ),
    C is Tcrit+1-A,
    (OP \= >=@ -> (\+((A =< Tcrit, D >= C)));(\+((A =< Tcrit)))).
%no intervals but point A matters
retain_iteration_formula(OP, [], [AA], D, IterationFormula, FormulaId, Tcrit):-
    (   
        (
            AA\=(_,_),
            AA=A
        );
        (
            AA=(A,_)
        )
    ),
	A =< Tcrit, C is Tcrit+1-A, 
    (OP \= >=@ -> (D >= C);(true)),
    assert_if_not_exists(retained_iteration_formula_points((IterationFormula),AA,FormulaId,Tcrit)).
%intervals but current interval doesn't matter
retain_iteration_formula(OP,[[TTs,TTe]|R], A, D, IterationFormula, FormulaId, Tcrit):-
    (   
        (
            TTs\=(_,_),
            TTs=Ts,TTe=Te
        );
        (
            TTs=(Ts,_),
            TTe=(Te,_)
        )
    ),
    C is Tcrit+1-Te,
    (OP \= >=@ -> \+((Ts =< Tcrit, D >= C)) ; \+(Ts =< Tcrit)),
    retain_iteration_formula(OP, R, A, D, IterationFormula,FormulaId, Tcrit).
%intervals but current interval does matter
retain_iteration_formula(OP, [[TTs,TTe]|_R], _A, D, IterationFormula, FormulaId, Tcrit):-
    (   
        (
            TTs\=(_,_),
            TTs=Ts,TTe=Te
        );
        (
            TTs=(Ts,_),
            TTe=(Te,_)
        )
    ),
    Ts =< Tcrit, C is Tcrit+1-Te,
    (OP \= >=@ -> (D >= C); (true)),
    assert_if_not_exists(retained_iteration_formula_intervals((IterationFormula),[[TTs,TTe]],FormulaId,Tcrit)),
    Te =< Tcrit -> assert_if_not_exists(retained_iteration_formula_points((IterationFormula),TTe,FormulaId,Tcrit)) ; true. 


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
    assert_if_not_exists(retained_tset_formula_intervals(OP,PheVars,FormulaId,Tcrit,ILR)).

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
    assert_if_not_exists(retained_relation_formula_temp_info(Relation,PheVars,FormulaId,Tcrit,AP)).

