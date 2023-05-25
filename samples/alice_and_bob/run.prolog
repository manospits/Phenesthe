:-['../../phenesthe.prolog'].
:-['definitions.prolog'].
:-['narrative.prolog'].
:-preprocess_phenomena_definitions.

% queries can happen only at equally spaced times
% if the first query is at 5 and the step is 5 then
% the next query should happen at t=10.
%
% current narrative supports queries at times t={5,10,15}
query(X):-assert_input_phenomena(X),recognition_query(0,5,X),write('\n\nQuery at tq='),writeln(X),write_results.
write_results:-
        writeln('-----Events:'),
        findall(_,(
            event_instants(X,T),
            write('    '),write(X:T),nl),_),nl,
        writeln('-----States:'),
        findall(_,(
            state_intervals(S,I),
            write('    '),write(S:I),nl),_),nl,
        writeln('-----Dynamic temporal phenomena:'),
        findall(_,(
            dynamic_phenomenon_intervals(S,I),
            write('    '),write(S:I),nl),_).

:-query(5).
:-query(10).
:-query(15).
