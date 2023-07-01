% Author: Manolis Pitsikalis
%
% - Interval filtering -
%
% apply different kind of filters on
% interval formulae e.g. keep intervals greater/smaller than a value

apply_filter(greater(X),IL1,IL2):-
    findall(([TS,TE],V1),
            (member(([TS,TE],V),IL1),
             (
                 (
                     
                     is_inf(TE),
                     phe_getval(tq,Tq),
                     (
                         (
                             V=t,
                             D is Tq - TS,
                             (D > X -> V1=t ; V1 = u)
                         )
                         ;
                         (
                             V=u,
                             V1=u
                         )
                        
                     )
                 )
                 ;
                 (
                     \+is_inf(TE),
                     D is TE - TS, D>X,
                     V1=V
                 )
             )
            ), IL2).

apply_filter(less(X),IL1,IL2):-
    findall(([TS,TE],V1),
            (member(([TS,TE],V),IL1),
             (
                 (
                     
                     is_inf(TE),
                     phe_getval(tq,Tq),
                     (
                         (
                             V=t,
                             D is Tq - TS,
                             D < X, V1=u
                         )
                         ;
                         (
                             V=u,V1=u
                         )
                     )
                 )
                 ;
                 (
                     \+is_inf(TE),
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
    findall(([TS,TE],V1),
            (member(([TS,TE],V),IL1),
             (
                 (
                     is_inf(TE),
                     phe_getval(tq,Tq),
                     D is Tq - TS,
                     D > X, V1=u
                 )
                 ;
                 (
                    \+is_inf(TE), D is TE - TS,
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

