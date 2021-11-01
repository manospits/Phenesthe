:-['../../phenesthe.prolog'].
:-['definitions.prolog'].
:-['narrative.prolog'].
:-preprocess_phenomena_definitions.

% queries can happen only at equally spaced times
% if the first query is at 5 and the step is 5 then
% the next query should happen at t=10.
%
% current narrative supports queries at times t={5,10,15}
query(X):-assert_input_phenomena(X),recognition_query(5,5,X).

