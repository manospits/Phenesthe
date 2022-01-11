% load Phenesthe
:-['../../phenesthe.prolog'].
% load the maritime definitions
:-['./definitions.prolog'].
% load vessel types
:-['./vessel_types.prolog'].
% preprocess phenomena definitions (transform them, find evaluation order, etc.)
:-preprocess_phenomena_definitions.
