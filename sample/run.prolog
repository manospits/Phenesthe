:-['../phenesthe.prolog'].
:-['definitions.prolog'].
:-['narrative.prolog'].
:-preprocess_phenomena_definitions.

query(X):-assert_input_phenomena(X),recognition_query(5,5,X).

