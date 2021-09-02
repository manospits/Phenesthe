:-['../../phenesthe.prolog'].
:-['definitions.prolog'].
:-preprocess_phenomena_definitions.
%
% BREST
% one week 
% queries_on_fstream('BREST_phenesthe_input.csv',1443650401,1444255201,86400,86400).

queries_on_fstream(File,Start,End,Step,Window):-
    open(File,read,Fd,[alias(input)]),
    FirstQueryTime is Start+Step,
    fstream_perform_query(Fd,[],End,Step,Window,FirstQueryTime).

fstream_perform_query(_Fd,_Retained,End,_Step,_Window,QueryTime):-
    QueryTime > End.

fstream_perform_query(Fd,Retained,End,Step,Window,QueryTime):-
    QueryTime =< End,
    writeln('===================================================='),
    write('            '),get_time(Time),rfc1123_timestamp(Time,FTime),
    writeln(FTime),
    writeln('----------------------------------------------------'),
    write('Loading data until t='),writeln(QueryTime),
    assert_from_stream(Fd,Retained,RetainedNew,End,QueryTime),
    write('Performing query... S='),write(Step),
    write(' W='),write(Window),write(' Tq='),writeln(QueryTime),
    nl,
    statistics(walltime, [_TimeSinceStart | [_TimeSinceLastCall]]),
    recognition_query(Window,Step,QueryTime),
    statistics(walltime, [_NewTimeSinceStart | [ExecutionTime]]),
    write("Processing time: "), write(ExecutionTime), write(' ms.'),nl,
    garbage_collect_clauses,
    count_results((Ea,Ei),(Sa,Si),(Da,Di)),
    write('Event instances/instants: '), write(Ea), write('/'), writeln(Ei),
    write('State instances/intervals: '), write(Sa), write('/'), writeln(Si),
    write('Dynamic Phe. instances/intervals: '), write(Da), write('/'), writeln(Di),
    writeln("\n\n"),
    NewQueryTime is QueryTime+Step,
    fstream_perform_query(Fd,RetainedNew,End,Step,Window,NewQueryTime).

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
