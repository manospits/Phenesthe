% Author: Manolis Pitsikalis
%
% - Operators of the language -
%

:-op(1149,fx,event_phenomenon).
:-op(1149,fx,state_phenomenon).
:-op(1149,fx,dynamic_phenomenon).
:-op(1199,xfx,:=).

:-op(1048,xfy,before).
:-op(1047,xfy,overlaps).
:-op(1046,xfy,meets).
:-op(1045,xfy,contains).
:-op(1044,xfy,starts).
:-op(1043,xfy,finishes).
:-op(1042,xfy,equals).


:-op(1041,xfy,complement).
:-op(1040,xfy,intersection).
:-op(1039,xfy,union).
:-op(1038,xfy,~>).
:-op(1037,xfy,<@).
:-op(1037,xfy,>=@).
:-op(1037,xfy,=@).

:-op(1036,xfy,aand).
:-op(1035,xfy,or).
:-op(1034,xfy,and).
:-op(1033,xfy,in).
:-op(1032,fy,tnot).
:-op(1032,fy,gtnot).

op_list([aand,tnot,gtnot,and,or,in,
        <@,>=@,=@,~>,union,intersection,complement,
        before,overlaps,meets,contains,starts,finishes,equals,filter]).

instant_op_list([and,or,tnot,start,end]).
