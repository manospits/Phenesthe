:-['../../phenesthe.prolog'].
:-['definitions.prolog'].
:-['narrative.prolog'].
:-preprocess_phenomena_definitions.

% queries can happen only at equally spaced times
% if the first query is at 5 and the step is 5 then
% the next query should happen at t=10.
%
% current narrative supports queries at times t={5,10,15}
query:-assert_input_phenomena(1000),recognition_query(1000,1000,1000).

:-query, writeln('-----Events:'),
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

