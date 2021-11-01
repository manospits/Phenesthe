% Author: Manolis Pitsikalis
%
% Perform Complex Event Processing using a file stream
%
% -- USAGE --
% ?- queries_on_fstream(+InputFile, +LogFile, +ResultsFile, +Start, +End, +Step, +Window).
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
% NOTE: initialisation of Phenesthe must happen before calling the queries_on_fstream predicate.

queries_on_fstream(InputFile,LogFile,ResultsFile,Start,End,Step,Window):-
    open(InputFile,read,IFd,[alias(input)]),
    open(LogFile,write,LFd,[alias(log)]),
    open(ResultsFile,write,RFd,[alias(results)]),
    prepare_log_file(LFd),
    FirstQueryTime is Start+Step,
    fstream_perform_query((IFd,LFd,RFd),[],End,Step,Window,FirstQueryTime),
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
    assert_from_stream(IFd,Retained,RetainedNew,End,QueryTime),
    write('Performing query... S='),write(Step),
    write(' W='),write(Window),write(' Tq='),writeln(QueryTime),
    nl,
    statistics(walltime, [_TimeSinceStart | [_TimeSinceLastCall]]),
    recognition_query(Window,Step,QueryTime),
    statistics(walltime, [_NewTimeSinceStart | [ExecutionTime]]),
    write("Processing time: "), write(ExecutionTime), write(' ms.'),nl,
    count_input(IEi,(ISa,ISi),(IDa,IDi)),
    count_results((Ea,Ei),(Sa,Si),(Da,Di)),
    log_stats(LFd,QueryTime,ExecutionTime,IEi,(ISa,ISi),(IDa,IDi),(Ea,Ei),(Sa,Si),(Da,Di)),
    print_stats(IEi,(ISa,ISi),(IDa,IDi),(Ea,Ei),(Sa,Si),(Da,Di)),
    print_results(RFd,QueryTime,Window),
    writeln("\n\n"),
    NewQueryTime is QueryTime+Step,!,
    fstream_perform_query((IFd,LFd,RFd),RetainedNew,End,Step,Window,NewQueryTime).

assert_from_stream(Fd,Retained,RetainedNew,End,QueryTime):-
    findall(_,(member(X,Retained),assert(X)),_),
    read_lines(Fd,End,QueryTime,RetainedNew).


read_lines(Fd,End,QueryTime,Retained):-
    read_string(Fd, "\n", "\r", Sep, String),
    read_lines2(Fd,String,Sep,End,QueryTime,Retained).

read_lines2(_Fd,_String,-1,_End,_QueryTime,[]).

read_lines2(_Fd,String,_Sep,End,QueryTime,[InputPhe]):-
    term_string(InputPhe,String),
    InputPhe=event_instant(_X,T),
    ((T>End);(T>QueryTime)),!.
read_lines2(Fd,String,_Sep,End,QueryTime,Retained):-
    term_string(InputPhe,String),
    InputPhe=event_instant(_X,T),
    T=<End,T=<QueryTime,
    assert(InputPhe),
    read_lines(Fd,End,QueryTime,Retained).


count_input(Ei,(Sa,Si),(Da,Di)):-
    findall(T,(event_instant(X,T)),E_Info),
    length(E_Info,Ei),
    findall(L,(state_intervals(X,TL),phenomenon_type(X,_,input),length(TL,L)),S_Info),
    length(S_Info,Sa),
    sumlist(S_Info,Si),
    findall(L,(dynamic_phenomenon_intervals(X,TL),phenomenon_type(X,_,input),length(TL,L)),D_Info),
    length(D_Info,Da),
    sumlist(D_Info,Di).


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

print_stats(IEi,(ISa,ISi),(IDa,IDi),(Ea,Ei),(Sa,Si),(Da,Di)):-
    write('Input event instants: '),  writeln(IEi),
    write('Input state instances/intervals: '), write(ISa), write('/'), writeln(ISi),
    write('Input dynamic phe. instances/intervals: '), write(IDa), write('/'), writeln(IDi),nl,
    write('User event instances/instants: '), write(Ea), write('/'), writeln(Ei),
    write('User state instances/intervals: '), write(Sa), write('/'), writeln(Si),
    write('User dynamic phe. instances/intervals: '), write(Da), write('/'), writeln(Di).

log_stats(LFd,Tq,Rt,IEi,(ISa,ISi),(IDa,IDi),(Ea,Ei),(Sa,Si),(Da,Di)):-
    write_comma_separated(LFd,[Tq,Rt,IEi,ISa,ISi,IDa,IDi,Ea,Ei,Sa,Si,Da,Di]),nl(LFd).
prepare_log_file(LFd):-
    write_comma_separated(LFd,['query time','processing time','input event instants',
                               'input state instances', 'input state intervals', 'input dynamic phe. instances',
                               'input dynamic phe. intervals', 'user event instances', 'user event instants', 'user state instances',
                               'user state intervals', 'user dynamic phe. instances', 'user dynamic phe. intervals']),
    nl(LFd).

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
