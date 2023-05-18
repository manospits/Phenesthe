% Author: Manolis Pitsikalis
%
% - Interval filtering -
%
% apply different kind of filters on
% interval formulae e.g. keep intervals greater/smaller than a value

apply_filter(greater(X),IL1,IL2):-
    findall(([TS,TE],V),
            (member(([TS,TE],V),IL1),
             (
                 (
                     %TODO
                     is_inf_unk(TE)
                 
                 )
                 ;
                 (\+is_inf_unk(TE),D is TE - TS, D>X)
             )
            ), IL2).

apply_filter(less(X),IL1,IL2):-
    findall(([TS,TE],V1),
            (member(([TS,TE],V),IL1),
             (
                 (
                     %TODO
                     is_inf_unk(TE),
                     V1=u
                 )
                 ;
                 (
                     \+is_inf_unk(TE),
                     (
                         (
                             V=t,
                             D is TE - TS, D<X,
                             V1=t
                         );
                         (
                             V=u,V1=u
                         )
                     )   
                 )
             )
            ), IL2).

apply_filter(equal(X),IL1,IL2):-
    findall([TS,TE],
            (member([TS,TE],IL1),
             (
                 (is_inf_unk(TE),fail)
                 ;
                 (
                     %TODO
                     \+is_inf_unk(TE), D is TE - TS,
                    (
                         (
                             V=t,
                             D=X
                         );
                         (
                             V=u,D>=X
                         )
                     )   
                 )
             )
            ), IL2).

