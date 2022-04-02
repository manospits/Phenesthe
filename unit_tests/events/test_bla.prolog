
cforall:-
    concurrent_forall(member(X,[1,2,3,4,5]),assertz(a(X))),
    concurrent_forall(member(X,[1,2,3,4,5]),assertz(b(X))),
    concurrent_forall(member(X,[1,2,3,4,5]),assertz(c(X))),
    concurrent_forall(member(X,[1,2,3,4,5]),assertz(d(X))),
    concurrent_forall(member(X,[1,2,3,4,5]),assertz(e(X))).

:-begin_tests(findall).

test(assert):-cforall.

test(findall):-
    assertion(findall(_,a(_),[_,_,_,_,_])),
    assertion(findall(_,b(_),[_,_,_,_,_])),
    assertion(findall(_,c(_),[_,_,_,_,_])),
    assertion(findall(_,d(_),[_,_,_,_,_])),
    assertion(findall(_,e(_),[_,_,_,_,_])).

:-end_tests(findall).
