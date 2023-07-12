% Author: Manolis Pitsikalis
%
% Perform Complex Event Processing using a file stream
%
% -- USAGE --
% ?- queries_on_stream(+InputFile, +LogFile, +ResultsFile, +End, +Step, +Window).
%
% -- VARIABLES --
%   InputFile: Name of the input file.
%   LogFile: Name of the log file (stattistics for each temporal query).
%   ResultsFile: Printed instants and intervals at which user defined phenomena are true hold.
%   Start: Timestamp to start processing.
%   End: Timestampt to end processing.
%   Step: Window sliding step.
%   Window: Window size.
%
% NOTE: initialisation of Phenesthe must happen before calling the queries_on_stream predicate.
:-multifile atemporal_preprocess/3.

queries_on_stream(InputFile,LogFile,ResultsFile,Step,Window):-
    open(InputFile,read,IFd,[alias(input)]),
    open(LogFile,write,LFd,[alias(log)]),
    open(ResultsFile,write,RFd,[alias(results)]),
    prepare_log_file(LFd),
    read_first_line(IFd, Retained, Start),
    FirstQueryTime is Start+Step,
    stream_perform_query((IFd,LFd,RFd),Retained,Step,Window,FirstQueryTime),
    close(IFd),close(LFd),close(RFd).

stream_perform_query((IFd,LFd,RFd),Retained,Step,Window,QueryTime):-
    writeln('===================================================='),
    write('            '),get_time(Time),rfc1123_timestamp(Time,FTime),
    writeln(FTime),
    writeln('----------------------------------------------------'),
    write('Loading data until t='),writeln(QueryTime),
    assert_from_stream(IFd,Retained,RetainedNew,QueryTime, StreamFinished),
    phe_getval(preprocessing, PreProcessFlag),
    preprocess_check(PreProcessFlag, Window, Step, QueryTime),
    writeln('----------------------------------------------------'),
    write('Performing query... S='),write(Step),
    write(' W='),write(Window),write(' Tq='),writeln(QueryTime),
    nl,
    statistics(walltime, [_TimeSinceStart | [_TimeSinceLastCall]]),
    recognition_query(Window,Step,QueryTime),
    statistics(walltime, [_NewTimeSinceStart | [ExecutionTime]]),
    write("Processing time: "), write(ExecutionTime), write(' ms.'),nl,
    count_input(IEi,ISi,IDi),
    count_results((Ea,Ei,EIa,EIi),(Sa,Si,SIa,SIi),(Da,Di,DIa,DIi)),
    count_retained(TotalPRetained,TotalIRetained),
    log_stats(LFd,QueryTime,ExecutionTime,IEi,ISi,IDi,(Ea,Ei,EIa,EIi),(Sa,Si,SIa,SIi),(Da,Di,DIa,DIi),TotalPRetained,TotalIRetained),
    print_stats(IEi,ISi,IDi,(Ea,Ei),(Sa,Si),(Da,Di),TotalPRetained,TotalIRetained),
    print_results(RFd,QueryTime,Window),
    writeln("\n\n"),
    NewQueryTime is QueryTime+Step,
    garbage_collect, trim_stacks,
    !,
    (
        (
        StreamFinished = 0,
         stream_perform_query((IFd,LFd,RFd),RetainedNew,Step,Window,NewQueryTime)
        );
        (StreamFinished = 1 )
    ).

preprocess_check(0,_,_,_):-!.
preprocess_check(1, Window, Step, QueryTime):-
    atemporal_preprocess(Window, Step, QueryTime).


queries_on_fstream(InputFile,LogFile,ResultsFile,Start,End,Step,Window):-
    open(InputFile,read,IFd,[alias(input)]),
    open(LogFile,write,LFd,[alias(log)]),
    open(ResultsFile,write,RFd,[alias(results)]),
    prepare_log_file(LFd),
    FirstQueryTime is Start+Step,
    fstream_perform_query((IFd,LFd,RFd),[],End,Step,Window,FirstQueryTime),!,
    close(IFd),close(LFd),close(RFd).

fstream_perform_query(_Fd,_Retained,End,_Step,_Window,QueryTime):-
    QueryTime > End.

fstream_perform_query((IFd,LFd,RFd),Retained,End,Step,Window,QueryTime):-
    QueryTime =< End,
    writeln('===================================================='),
    write('            '),get_time(Time),rfc1123_timestamp(Time,FTime),
    writeln(FTime),
    writeln('----------------------------------------------------'),
    write('Loading data until t='),writeln(QueryTime),
    assert_from_stream(IFd,Retained,RetainedNew,End,QueryTime,StreamFinished),
    write('Performing query... S='),write(Step),
    write(' W='),write(Window),write(' Tq='),writeln(QueryTime),
    nl,
    statistics(walltime, [_TimeSinceStart | [_TimeSinceLastCall]]),
    recognition_query(Window,Step,QueryTime),
    statistics(walltime, [_NewTimeSinceStart | [ExecutionTime]]),
    write("Processing time: "), write(ExecutionTime), write(' ms.'),nl,
    count_input(IEi,ISi,IDi),
    count_results((Ea,Ei,EIa,EIi),(Sa,Si,SIa,SIi),(Da,Di,DIa,DIi)),
    count_retained(TotalPRetained,TotalIRetained),
    log_stats(LFd,QueryTime,ExecutionTime,IEi,ISi,IDi,(Ea,Ei,EIa,EIi),(Sa,Si,SIa,SIi),(Da,Di,DIa,DIi),TotalPRetained,TotalIRetained),
    print_stats(IEi,ISi,IDi,(Ea,Ei),(Sa,Si),(Da,Di),TotalPRetained,TotalIRetained),
    print_results(RFd,QueryTime,Window),
    writeln("\n\n"),
    NewQueryTime is QueryTime+Step,
    garbage_collect, trim_stacks,
    !,
    (
        (StreamFinished = 0,!,
         fstream_perform_query((IFd,LFd,RFd),RetainedNew,End,Step,Window,NewQueryTime)
        );
        (StreamFinished = 1 )
    ).

assert_from_stream(Fd, Retained, RetainedNew, End, QueryTime, StreamFinished):-
    findall(_,(member(X,Retained),assertz(X)),_),
    read_lines(Fd,End,QueryTime,RetainedNew, StreamFinished).

assert_from_stream(Fd,Retained,RetainedNew,QueryTime, StreamFinished):-
    findall(_,(member(X,Retained),assertz(X)),_),
    read_lines(Fd,QueryTime,QueryTime,RetainedNew, StreamFinished).



read_first_line(Fd,Retained,T):-
    read_string(Fd, "\n", "\r", Sep, String),
    term_string(InputPhe,String),
    (
        (
            Sep \= -1,
            (
                (InputPhe=input_event_instant(_X,T),!)
                ;
                (InputPhe=input_state_interval(_X,[_,T]),!)
                ;
                (InputPhe=input_dynamic_phenomenon_interval(_X,[_,T]),!)
            ),
            Retained=[InputPhe]
        );
        (
            Sep = -1,
            Retained=[],
            T = -1
        )
    ).

read_lines(Fd,End,QueryTime,Retained, StreamFinished):-
    read_string(Fd, "\n", "\r", Sep, String),
    read_lines2(Fd,String,Sep,End,QueryTime, Retained, StreamFinished).

read_lines2(_Fd,_String,-1,_End,_QueryTime,[], 1).

read_lines2(_Fd,String,_Sep,End,QueryTime,[InputPhe], 0):-
    term_string(InputPhe,String),
    (
        (InputPhe=input_event_instant(_X,T),
            ((T>End);(T>QueryTime)),!)
        ;
        (InputPhe=input_state_interval(_X,[_,Te]),
            ((Te>End);(Te>QueryTime)),!)
        ;
        (InputPhe=input_dynamic_phenomenon_interval(_X,[_,Te]),
            ((Te>End);(Te>QueryTime)),!)
    ).

read_lines2(Fd,String,_Sep,End,QueryTime,Retained, StreamFinished):-
    term_string(InputPhe,String),
    (
        (InputPhe=input_event_instant(_X,T),
         T=<End,T=<QueryTime,
         assertz(InputPhe))
        ;
        (InputPhe=input_state_interval(_X,[_,Te]),
         Te=<End,Te=<QueryTime,
         assertz(InputPhe))
        ;
        (InputPhe=input_dynamic_phenomenon_interval(_X,[_,Te]),
         Te=<End,Te=<QueryTime,
         assertz(InputPhe))
    ),
    read_lines(Fd,End,QueryTime,Retained, StreamFinished).


count_input(Ei,Si,Di):-
    findall(T,(input_event_instant(X,T)),E_Info),
    length(E_Info,Ei),
    findall(L,(input_state_interval(X,I)),S_Info),
    length(S_Info,Si),
    findall(L,(input_dynamic_phenomenon_interval(X,I)),D_Info),
    length(D_Info,Di).

count_results((Ea,Ei,EIa,EIi),(Sa,Si,SIa,SIi),(Da,Di,DIa,DIi)):-
    findall(L,(event_instants(X,TL),length(TL,L)),E_Info),
    length(E_Info,Ea),
    sumlist(E_Info,Ei),
    findall(L,(event_instants_internal(X,TL),length(TL,L)),EI_Info),
    length(EI_Info,EIa),
    sumlist(EI_Info,EIi),
    findall(L,(state_intervals(X,TL),phenomenon_type(X,_,user),length(TL,L)),S_Info),
    length(S_Info,Sa),
    sumlist(S_Info,Si),
    findall(L,(state_intervals_internal(X,TL),phenomenon_type(X,_,user),length(TL,L)),SI_Info),
    length(SI_Info,SIa),
    sumlist(SI_Info,SIi),
    findall(L,(dynamic_phenomenon_intervals(X,TL),phenomenon_type(X,_,user),length(TL,L)),D_Info),
    length(D_Info,Da),
    sumlist(D_Info,Di),
    findall(L,(dynamic_phenomenon_intervals_internal(X,TL),phenomenon_type(X,_,user),length(TL,L)),DI_Info),
    length(DI_Info,DIa),
    sumlist(DI_Info,DIi).

count_results((Ea,Ei),(Sa,Si),(Da,Di)):-
    findall(L,(event_instants(X,TL),length(TL,L)),E_Info),
    length(E_Info,Ea),
    sumlist(E_Info,Ei),
    findall(L,(state_intervals(X,TL),phenomenon_type(X,_,user),length(TL,L)),S_Info),
    length(S_Info,Sa),
    sumlist(S_Info,Si),
    findall(L,(dynamic_phenomenon_intervals(X,TL),phenomenon_type(X,_,user),length(TL,L)),D_Info),
    length(D_Info,Da),
    sumlist(D_Info,Di).

count_retained(TotalInstants,TotalIntervals):-
    findall(_,instant_left_retained(_, Tqmws,_,_,_),A),
    findall(_,instant_right_retained(_, Tqmws,_,_,_),B),
    findall(T,retained_left_instants(_, Tqmws,_,T),C),
    findall(T,retained_right_instants(_, Tqmws,_,T),D),
    findall(I,retained_left_intervals(_, Tqmws,_,I),E),
    findall(I,retained_right_intervals(_, Tqmws,_,I),F),
    length(A,LI),
    length(B,RI),
    findall(L,(member(X,C),length(X,L)),CL),
    findall(L,(member(X,D),length(X,L)),DL),
    findall(L,(member(X,E),X=[([_,_],_)|_],length(X,L)),EL),
    findall(L,(member(X,E),X=[(A,_)|_],A\=[_,_],length(X,L)),ELT),
    findall(L,(member(X,F),X=[([_,_],_)|_],length(X,L)),FL),
    findall(L,(member(X,F),X=[(A,_)|_],A\=[_,_],length(X,L)),FLT),
    sumlist(CL,CLS),
    sumlist(DL,DLS),
    sumlist(EL,ELS),
    sumlist(ELT,ELTS),
    sumlist(FL,FLS),
    sumlist(FLT,FLTS),
    TotalInstants is LI+RI+CLS+DLS+ELTS+FLTS,
    TotalIntervals is ELS+FLS.


    %findall(_,retained_starting_formula(_,_,_,_),A),
    %findall(_,retained_iteration_formula_points(_,_,_,_),B),
    %findall(_,retained_iteration_formula_intervals(_,_,_,_),C),
    %findall(_,retained_tset_formula_intervals(_,_,_,_,_),D),
    %findall(_,retained_relation_formula_temp_info(_,_,_,_,_),E),
    %length(A,MrOPRetained),
    %length(B,ItOPRetainedP),
    %length(C,ItOPRetainedI),
    %length(D,TSetOpRetained),
    %length(E,TRelRetained).


print_stats(IEi,ISi,IDi,(Ea,Ei),(Sa,Si),(Da,Di), TotalP, TotalI):-
    write('Input event instants: '),  writeln(IEi),
    write('Input intervals: '), writeln(ISi),
    write('Input dynamic phe. intervals: '), writeln(IDi),nl,
    write('User event instances/instants: '), write(Ea), write('/'), writeln(Ei),
    write('User state instances/intervals: '), write(Sa), write('/'), writeln(Si),
    write('User dynamic phe. instances/intervals: '), write(Da), write('/'), writeln(Di),nl,
    write('Retained instants: '), writeln(TotalP),
    write('Retained intervals: '), writeln(TotalI).

log_stats(LFd,Tq,Rt,IEi,ISi,IDi,(Ea,Ei,EIa,EIi),(Sa,Si,SIa,SIi),(Da,Di,DIa,DIi),RP,RI):-
    write_comma_separated(LFd,[Tq,Rt,IEi,ISi,IDi,Ea,Ei,EIa,EIi,Sa,Si,SIa,SIi,Da,Di,DIa,DIi,RP,RI]),nl(LFd).
prepare_log_file(LFd):-
    write_comma_separated(LFd,['query time','processing time','input event instants','input state intervals',
                               'input dynamic phe. intervals', 'user event instances', 'user event instants','user event instances internal', 'user event instants internal',
                               'user state instances','user state intervals','user state instances internal','user state intervals internal', 
                               'user dynamic phe. instances', 'user dynamic phe. intervals','user dynamic phe. instances internal', 'user dynamic phe. intervals internal',
                               'retained instants','retained intervals']), nl(LFd).

print_results(RFd, QueryTime, Window):-
    write(RFd,"Tq = "),write(RFd,QueryTime),write(RFd,", W = "),write(RFd,Window),nl(RFd),
    forall(event_instants(Phenomenon,Instants),writeln(RFd,event_instants(Phenomenon,Instants))),
    forall((state_intervals(Phenomenon,Intervals),phenomenon_type(Phenomenon,_,user)),writeln(RFd,state_intervals(Phenomenon,Intervals))),
    forall((dynamic_phenomenon_intervals(Phenomenon,Intervals),phenomenon_type(Phenomenon,_,user)),writeln(RFd,dynamic_phenomenon_intervals(Phenomenon,Intervals))),
    nl(RFd).


write_comma_separated(_Fd,[]).
write_comma_separated(Fd,[A]):-
    write(Fd,A).
write_comma_separated(Fd,[A|B]):-
    write(Fd,A),write(Fd,','),
    write_comma_separated(Fd,B).

convert_numbers(X,X):-
    number(X),!.
convert_numbers(X,Z):-
    X=..[A],
    atom_number(A,Z),!.
convert_numbers(X,A):-
    X=..[A],
    \+atom_number(A,_),!.
convert_numbers(X,Converted):-
    X=..[A|Tail],
    atom_number(A,Z),
    findall(C,(member(TX,Tail),convert_numbers(TX,C)),CTail),
    Converted=..[Z|CTail].

convert_numbers(X,Converted):-
    X=..[A|Tail],
    \+atom_number(A,_),
    findall(C,(member(TX,Tail),convert_numbers(TX,C)),CTail),
    Converted=..[A|CTail].

rfc1123_timestamp(Time,Atom):-
    stamp_date_time(Time, Date, 'UTC'),
    format_time(atom(Atom),'%a, %d %b %Y %T GMT',Date, posix).
