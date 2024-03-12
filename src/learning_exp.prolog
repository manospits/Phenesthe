 %exp_learn_phenomenon(fishing_trip(V,PA,FA,PB),[V,PA,FA,PB],[start(moored(V,PB)), end(moored(V,PA)),underway(V),in_fishing_area(V,FA)],[end(moored(V,PA))],[start(moored(V,PB))],Z).
% check transformations file
%allowed_formulae(before,[instant,dinterval,ndinterval],[instant,dinterval,ndinterval]).
%allowed_formulae(meets,[dinterval,ndinterval],[dinterval,ndinterval]).
%allowed_formulae(overlaps,[dinterval,ndinterval],[dinterval,ndinterval]).
%allowed_formulae(finishes,[instant,dinterval,ndinterval],[dinterval,ndinterval]).
%allowed_formulae(starts,[instant,dinterval,ndinterval],[dinterval,ndinterval]).
%allowed_formulae(equals,[dinterval,ndinterval],[dinterval,ndinterval]).
%allowed_formulae(contains,[dinterval,ndinterval],[instant,dinterval,ndinterval]).
:-dynamic candidate_formula_chronons/2,last_candidate_formula_id/1, candidate_formula_statistics/2.


exp_learn_phenomenon(Phenomenon, VarList, PhenomenaList, DistThreshold, Z):-
    prepare_phenomena_list(Phenomenon,VarList,PhenomenaList,PrPhenomenaList),
    compute_max_distance(Phenomenon,MaxD),
    iteration(Phenomenon, VarList, [], PrPhenomenaList, DistThreshold, MaxD, 0, Z).

compute_max_distance(Phenomenon,MaxD):-
    findall((D,S),(
                dynamic_phenomenon_intervals(Phenomenon,IGT),
                phe_getval(tqmw,Tqmw),
                phe_getval(tq,Tq),
                crop_intervals(IGT,[Tqmw,Tq],IGTC),
                isets_l1_distance_jsim(Tqmw,Tq,IGTC,[],D,S)
            ),L),
    pair_sumlist(L,(MaxD,_MaxS)).


prepare_phenomena_list(Phenomenon,VarList,PhenomenaList,PrPhenomenaList):-
    prepare_phenomena_list1(PhenomenaList,PhenomenaList1),
    generate_formulae1(VarList, Phenomenon, PhenomenaList1,Formulae1,0,_N),
    prepare_phenomena_list2(PhenomenaList,Formulae1,PrPhenomenaList).

prepare_phenomena_list1([],[]).
prepare_phenomena_list1([A|R],[(A,(ne,ne))|R1]):-prepare_phenomena_list1(R,R1).


prepare_phenomena_list2([],[],[]).
prepare_phenomena_list2([Phe|PR],[A|R],[(Phe,(Dist,Sim))|R1]):-
    A=(Phenomenon, _ID, _Name, _Formula,(TF,IL), Type, _Flag, _DistA, _DistB),
    findall((D,S),(
                    dynamic_phenomenon_intervals(Phenomenon,IGT),
                    TF, % candidate formula evaluation
                    %(dynamic_phenomenon_intervals(Phenomenon,IGT) -> true ; IGT=[]),
                    clean_from_unk(IL,ILL),
                    phe_getval(tqmw,Tqmw),
                    phe_getval(tq,Tq),
                    crop_intervals(IGT,[Tqmw,Tq],IGTC),
                    (
                        (
                            Type\=instant,
                            crop_intervals(ILL,[Tqmw,Tq],ILLC),
                            isets_l1_distance_jsim(Tqmw,Tq,IGTC,ILLC,D,_),
                            intervalsA_containing_intervalsB(IGT,ILL,S)
                        )
                        ;
                        (
                            Type=instant,
                            ILLC=ILL,
                            iset_instset_l1_distance_jsim(Tqmw,Tq,IGTC,ILLC,D,_),
                            intervalsA_containing_pointsB(IGT,ILL,S)
                        )
                )
            )
    ,L),
    pair_sumlist(L,(Dist,Sim)),
    prepare_phenomena_list2(PR,R,R1).


iteration(_, _, [], _, _, _, N, []):-N>0,!.
iteration(Phenomenon, VarList, PhenomenaListG, PhenomenaListB, DistThreshold, MaxD, ItN, Z1):-
    write('------------ Iteration: '),write(ItN),writeln(' ------------'),
    generate_all_formulae(ItN,VarList, Phenomenon, PhenomenaListG, PhenomenaListB, Formulae),!,
    length(Formulae,NF),
    write(' - Number of generated formulae: '),writeln(NF),
    concurrent_maplist(check_candidate_formula,Formulae),
    findall(N, (dynamic_phenomenon_intervals(Phenomenon,IL), length(IL,N)), NTS),
    sumlist(NTS,NGT),
    writeln(NGT),
    remove_bad_formulae(NGT,Formulae,NewPhenomenaListG,DistThreshold,MaxD,Solutions,BestD),
    length(NewPhenomenaListG,NKEPT),
    DISC is NF - NKEPT,
    write(' - Number of discarded formulae: '),writeln(DISC),
    write(' - Number of kept formulae: '),writeln(NKEPT),
    write(' - Current best distance: '),writeln(BestD),nl,nl,
    retractall(candidate_formula_chronons(_,_)),
    retractall(candidate_formula_statistics(_,_)),
    (
        (
            Solutions = [],
            ItN1 is ItN+1,!,
            %writeln(PhenomenaListB),
            %writeln(PhenomenaListG),
            %append(PhenomenaListG,PhenomenaListB,NewPhenomenaListB),!,
            %append(NewPhenomenaListG,PhenomenaListB,NewPhenomenaListB),
            iteration(Phenomenon, VarList, NewPhenomenaListG, PhenomenaListB, DistThreshold, MaxD, ItN1, Z1)
        );
        (
            Solutions \= [],!,
            writeln('Found suitable formula(e): '),
            findall(_,(
                        member(X,Solutions),
                        write("\t"),
                        X= (H,B),
                        write(H), write(' := '), writeln(B)
                      ),_),
            Z1=Solutions
        )
    ).

check_candidate_formula(A):-
    A=(Phenomenon, ID, Name, _Formula,(TF,IL), Type, Flag, _DistA, _DistB),
    findall((D,S),(
                    dynamic_phenomenon_intervals(Phenomenon,IGT),
                    TF, % candidate formula evaluation
                    %(dynamic_phenomenon_intervals(Phenomenon,IGT) -> true ; IGT=[]),
                    ((Flag=yes,assert_if_not_exists(candidate_formula_chronons(Name,IL)));
                    (Flag=no)),
                    clean_from_unk(IL,ILL),
                    phe_getval(tqmw,Tqmw),
                    phe_getval(tq,Tq),
                    crop_intervals(IGT,[Tqmw,Tq],IGTC),
                    (
                        (
                            Type\=instant,
                            crop_intervals(ILL,[Tqmw,Tq],ILLC),
                            isets_l1_distance_jsim(Tqmw,Tq,IGTC,ILLC,D,_),
                            intervalsA_containing_intervalsB(IGT,ILL,S)
                        )
                        ;
                        (
                            Type=instant,
                            ILLC=ILL,
                            iset_instset_l1_distance_jsim(Tqmw,Tq,IGTC,ILLC,D,_),
                            intervalsA_containing_pointsB(IGT,ILL,S)
                        )
                    )
                )
    ,L),
    pair_sumlist(L,S),
    assertz(candidate_formula_statistics(ID,S)).


unfold_candidate_formula(Formula, Formula):-
    phenomenon_type(Formula,_,user);
    phenomenon_type(Formula,_,input),!.
unfold_candidate_formula(Formula, UnfoldedFormula):-
    phenomenon_type(Formula,_,candidate),!,
    phenomenon_conditions(Formula,Conditions),
    unfold_candidate_formula(Conditions, UnfoldedFormula).
unfold_candidate_formula(Formula, UnfoldedFormula):-
    Formula=..[OP|LR],
    infix_operators(I),
    prefix_operators(P),
    (
        (
            member(OP, I),!,
            LR=[L,R],
            unfold_candidate_formula(L,LU),
            unfold_candidate_formula(R,RU),
            UnfoldedFormula =.. [OP,LU,RU]
        );
        (
            member(OP,P),!,
            LR=[R],
            unfold_candidate_formula(R,RU),
            UnfoldedFormula =.. [OP,RU]
        )
    ).


remove_bad_formulae(NGT, PL, RPLG, DT, MaxD, S, B):-
    remove_bad_formulae(NGT, PL, RPLG, DT, MaxD, S, inf, B).

remove_bad_formulae(_,[],[],_DistThreshold,_MaxD,[],BestD,BestD).
remove_bad_formulae(NGT,[(_Phenomenon,ID,Name,A,(TF,IL), Type, Flag,(DistA,SimA),(DistB,SimB),FT)|AR],[(Name,(Dist,Sim))|RNG],DistThreshold, MaxD, Solutions, PBestD, BestD):-
    candidate_formula_statistics(ID,(Dist,Sim)),
    unfold_candidate_formula(A,AU),
    FT = type2,
    Sim=:=NGT,!,
    phe_getval(verboselearning,Verbose),
    (Verbose == 1 -> (
    write(' + '),
    write(Name),
    write(' := '),
    write(AU),
    write(' | '),
    write((Dist,Sim)),
    write(' | prA: '),
    write((DistA,SimA)),
    write(' | prB: '),
    writeln((DistB,SimB))) ; true),
    assert_new_phenomenon(Name,A,(TF,IL),Type,Flag),
    (Dist < PBestD -> NBestD = Dist ; NBestD = PBestD),!,
    remove_bad_formulae(NGT, AR, RNG,  DistThreshold, MaxD, Solutions1, NBestD, BestD),
    (Dist =< DistThreshold -> Solutions = [(Name,AU,Dist)|Solutions1] ; Solutions=Solutions1).

remove_bad_formulae(NGT,[(_Phenomenon, ID, Name, A,(_TF,_IL),_Type,_Flag,(DistA,SimA),(DistB,SimB),FT)|AR], RNG, DistThreshold, MaxD, Solutions, PBestD, BestD):-
    FT= type2,
    candidate_formula_statistics(ID,(Dist,Sim)),
    unfold_candidate_formula(A,AU),
    phe_getval(verboselearning,Verbose),
    (Verbose == 1 -> (
    write(' - '),
    write(Name),
    write(' := '),
    write(AU),
    write(' | '),
    write((Dist,Sim)),
    write(' | prA: '),
    write((DistA,SimA)),
    write(' | prB: '),
    writeln((DistB,SimB))); true),
    remove_bad_formulae(NGT,AR,RNG, DistThreshold, MaxD, Solutions, PBestD, BestD).

remove_bad_formulae(NGT,[(_Phenomenon,ID,Name,A,(TF,IL),Type,Flag,(DistA,SimA),(DistB,SimB),FT)|AR], RNG,DistThreshold, MaxD, Solutions, PBestD, BestD):-
    candidate_formula_statistics(ID,(Dist,Sim)),
    unfold_candidate_formula(A,AU),
    FT=type1,
    phe_getval(verboselearning,Verbose),
    (Verbose == 1 -> (
    write(Name),
    write(' := '),
    write(AU),
    write(' | '),
    write((Dist,Sim)),
    write(' | prA: '),
    write((DistA,SimA)),
    write(' | prB: '),
    writeln((DistB,SimB))); true),
    assert_new_phenomenon(Name,A,(TF,IL),Type,Flag),
    (Dist < PBestD -> NBestD = Dist ; NBestD = PBestD),
    remove_bad_formulae(NGT, AR, RNG, DistThreshold, MaxD, Solutions1, NBestD, BestD),!,
    (Dist =< DistThreshold -> Solutions = [(Name,AU,Dist)|Solutions1] ; Solutions=Solutions1).

assert_new_phenomenon(_Name,_Formula,(_TF,_IL),_Type,no).
assert_new_phenomenon(Name,Formula,(TF,IL),_Type,yes):-
    assertz(candidate_phenomenon(Name,dynamic_phenomenon)),
    assertz(phenomenon_conditions(Name,Formula)),
    assertz(phenomenon_transformed_conditions(Name,TF,IL)),
    assert_computed_intervals(Name).

assert_computed_intervals(Name):-
    findall(_,
        (candidate_formula_chronons(Name,IL),
        assertz(dynamic_phenomenon_intervals_internal(Name,IL))),
    _).

create_formula(FormulaA, TypeA, FormulaB, TypeB, Formula, ndinterval):-
    allowed_formulae(Relation, AllowedA, AllowedB),
    \+((Relation=equals,FormulaA=FormulaB)),
    member(TypeA,AllowedA),
    member(TypeB,AllowedB),
    Formula =..[Relation,FormulaA,FormulaB].


% The new formulae set is the union of the following:
% G-1_removed: filtered combined formulae of previous iteration
% B-1: base used in previous iteration
%
% G-1_removed U
% G-1 x (G-1_removed U B-1) U
% B-1 x G-1_removed
% 

generate_all_formulae(0, VarList, Phenomenon, _PhenomenaListG, PhenomenaListB, Formulae):-
    findall((Relation,AllowedA,AllowedB),allowed_formulae(Relation,AllowedA,AllowedB),Relations),
    generate_formulae1(VarList, Phenomenon, PhenomenaListB, Formulae1, 0, N),
    generate_formulae2(VarList, Phenomenon, PhenomenaListB, PhenomenaListB, Relations, Formulae2, N, _),
    append(Formulae1, Formulae2, Formulae).

%generate_all_formulae(It, VarList, Phenomenon, PhenomenaListG, PhenomenaListB, Formulae):-
    %It > 0,
    %% create new formulae by combining those in PhenomenaList
    %findall((Relation,AllowedA,AllowedB),allowed_formulae(Relation,AllowedA,AllowedB),Relations),
    %%(G-1_removed U B-1)
    %append(PhenomenaListG,PhenomenaListB,PhenomenaListGB),    
    %% G-1_removed
    %generate_formulae1(VarList, Phenomenon, PhenomenaListG,Formulae1,0,N1),
    %% G-1 x (G-1_removed U B-1)
    %generate_formulae2(VarList, Phenomenon, PhenomenaListG, PhenomenaListGB, Relations, Formulae2, N1, N2),
    %% B-1 x G-1_removed
    %generate_formulae2(VarList, Phenomenon, PhenomenaListB, PhenomenaListG, Relations, Formulae3, N2, _N),
    %append(Formulae1,Formulae2,Formulae12),
    %append(Formulae12,Formulae3, Formulae).

generate_all_formulae(It, VarList, Phenomenon, PhenomenaListG, PhenomenaListB, Formulae):-
    It > 0,
    % create new formulae by combining those in PhenomenaList
    findall((Relation,AllowedA,AllowedB),allowed_formulae(Relation,AllowedA,AllowedB),Relations),
    generate_formulae1(VarList, Phenomenon, PhenomenaListG, _Formulae1,0,N1),
    generate_formulae2(VarList, Phenomenon, PhenomenaListB, PhenomenaListG, Relations, Formulae2, N1, N2),
    generate_formulae2(VarList, Phenomenon, PhenomenaListG, PhenomenaListB, Relations, Formulae3, N2, N3),
    generate_formulae2(VarList, Phenomenon, PhenomenaListG, PhenomenaListG, Relations, Formulae4, N3, _N),
    append(Formulae2,Formulae3, Formulae23),
    append(Formulae23, Formulae4, Formulae).


generate_formulae1(_,_,[],[],N,N).
generate_formulae1(VarList,Phenomenon,[(Formula,_Dist)|R],[(Phenomenon,Current,Formula,Formula,(TF,IL),Type,no,(ne,ne),(ne,ne),type1)|Rest],Prev,All):-
    formula_type(Formula,Type),
    term_variables(Formula,PheVars),
    variable_list_intersection(VarList,PheVars,FVars),
    Current is Prev+1,
    (
      (
        Type=instant,
        transform_instant_formula(Formula,FVars,TFi,ILi),
        term_variables(TFi,PFVars),
        variable_list_diff(PFVars, [ILi|PheVars], PFStVars),
        TF=(setof_empty(ILi,PFStVars^TFi,IL))
      );
      (
        Type=dinterval,
        transform_dinterval_formula(Formula,FVars,ProcessedFormula,PIL), 
        term_variables(ProcessedFormula,PFVars),
        variable_list_diff(PFVars, [PIL|PheVars], PFStVars),
        TF=(
            setof_empty(PIL, PFStVars^ProcessedFormula, ZIL),
            merge_disjoint_interval_lists(ZIL,IL)
        )
      );
      (
        Type=ndinterval,
        transform_ndinterval_formula(Formula,FVars,ProcessedFormula,PIL),
        term_variables(ProcessedFormula,PFVars),
        variable_list_diff(PFVars, [PIL|PheVars], PFStVars),
        TF=(
            setof_empty(PIL, PFStVars^ProcessedFormula, ZIL),
            merge_non_disjoint_interval_lists(ZIL,IL)
        )
      )
    ),!,
    generate_formulae1(VarList, Phenomenon, R, Rest, Current, All).

generate_formulae2(_VarList, _, [],_AllPhe,_Relations,[], All, All).
generate_formulae2(VarList, Phenomenon, [FormulaA|R],AllPhe,Relations,Formulae,Prev,All):-
    generate_formulae3(VarList, Phenomenon, FormulaA,AllPhe,Relations,Formulae1,Prev,Some),!,
    generate_formulae2(VarList, Phenomenon,R,AllPhe,Relations,Formulae2, Some, All),
    append(Formulae1,Formulae2,Formulae).

generate_formulae3(_VarList, _Phenomenon,  _FormulaA, [], _Relations, [], N, N).
generate_formulae3(VarList, Phenomenon,  FormulaA, [FormulaB|RFB], Relations, Formulae, Prev, All):-
    % pair of formulae - creation of relations
    generate_formulae4(VarList, Phenomenon,  FormulaA, FormulaB,Relations,Formulae1, Prev, Some),!,
    % get next formula of B
    generate_formulae3(VarList, Phenomenon,  FormulaA, RFB, Relations, Formulae2, Some, All),
    append(Formulae1,Formulae2,Formulae).

generate_formulae4(_VarList, _Phenomenon,  _FormulaA,_FormulaB,[],[], N, N).
generate_formulae4(VarList, Phenomenon,  (FormulaA,DistA),(FormulaB,DistB),[(Relation,AllowedA,AllowedB)|R],Formulae, Prev, All):-
    formula_type(FormulaA,TypeA),
    formula_type(FormulaB,TypeB),
    (
        (
            FormulaA=FormulaB,
            Relation=equals,!,
            Formulae=RF,
            Current = Prev
        )
        ;
        (
            \+((member(TypeA,AllowedA),
            member(TypeB,AllowedB))),!,
            Formulae=RF,
            Current = Prev
        )
        ;
        (
            member(TypeA,AllowedA),
            member(TypeB,AllowedB),!,
            Formula =..[Relation,FormulaA,FormulaB],
            get_candidate_formula_name(Formula,Name),
            term_variables(Formula,PheVars),
            variable_list_intersection(VarList,PheVars,FVars),
            transform_ndinterval_formula(Formula,FVars,TF,IL),
            Current is Prev+1,
            Formulae=[(Phenomenon, Current, Name, Formula, (TF,IL), ndinterval, yes, DistA, DistB, type2)|RF]
        )
    ),!,
    generate_formulae4(VarList, Phenomenon, (FormulaA,DistA), (FormulaB,DistB), R, RF, Current, All).

get_candidate_formula_name(Formula,Phe):-
    \+last_candidate_formula_id(_),
    assert(last_candidate_formula_id(0)),
    atom_concat(candidate_formula,0,PheN),
    term_variables(Formula,Vars),
    Phe=..[PheN|Vars],!.

get_candidate_formula_name(Formula,Phe):-
    last_candidate_formula_id(N),
    A is N+1,
    retract(last_candidate_formula_id(N)),
    assert(last_candidate_formula_id(A)),
    atom_concat(candidate_formula,A,PheN),
    term_variables(Formula,Vars),
    Phe=..[PheN|Vars],!.


generate_formulae(PhenomenaList, Formula, Type):-
    member(Formula,PhenomenaList),
    formula_type(Formula,Type).
generate_formulae(PhenomenaList, Formula, Type):-
    member(A,PhenomenaList),
    member(B,PhenomenaList),
    formula_type(A,TypeA),
    formula_type(B,TypeB),
    create_formula(A,TypeA,B,TypeB, Formula,Type).

instants_in_intervals(A,B):-
    instants_in_intervals(A,0,B).
instants_in_intervals([], A, A).
instants_in_intervals([[A,inf]|R], Prev, All):-!,
    phe_getval(tq, Tq),
    NPrev is Prev + Tq-A+1,
    instants_in_intervals(R,NPrev,All).
instants_in_intervals([[A,B]|R], Prev, All):-
    NPrev is Prev + B-A+1,!,
    instants_in_intervals(R,NPrev,All).


chronon_in([T1,T2],[T3,T4]):-geq(T1,T3),leq(T2,T4).
chronon_in(T1,[T3,T4]):-T1\=[_,_],geq(T1,T3),leq(T1,T4).

chronons_in_interval(_,[],[],[]).
chronons_in_interval(A,[I|B],C,D):-
    (
        (
            chronon_in(I,A),!,
            C=[I|C1],D=D1
        );
        (
            C=C1,D=[I|D1]
        )
    ),!,
    chronons_in_interval(A,B,C1,D1).
