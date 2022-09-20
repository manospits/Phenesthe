% Author: Manolis Pitsikalis
%
% - Interval filtering -
%
% apply different kind of filters on
% interval formulae e.g. keep intervals greater/smaller than a value

apply_filter(greater(X),IL1,IL2):-
    findall([TS,TE],
            (member([TS,TE],IL1),
             (
                 (is_inf_unk(TE))
                 ;
                 (\+is_inf_unk(TE),D is TE - TS, D>X)
             )
            ), IL2).

apply_filter(less(X),IL1,IL2):-
    findall([TS,TE],
            (member([TS,TE],IL1),
             (
                 (is_inf_unk(TE),fail)
                 ;
                 (\+is_inf_unk(TE),D is TE - TS, D<X)
             )
            ), IL2).

apply_filter(equal(X),IL1,IL2):-
    findall([TS,TE],
            (member([TS,TE],IL1),
             (
                 (is_inf_unk(TE),fail)
                 ;
                 (\+is_inf_unk(TE),D is TE - TS, D=X)
             )
            ), IL2).

