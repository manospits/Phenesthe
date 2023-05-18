% ------  A greater B ------
% case A N - N
gt(A,B):-numinf(A),numinf(B), A > B,!.

% case B1 U - N: U2 > B
gt([A1-A2],B,[A1-A2]):-
    numinf(B), A1 > B,!.
% case B2 U - N:  A1 <= B < A2
gt([A1-A2],B,[B1-A2]):-
    numinf(B), A1 =< B, B < A2,
    B1 is B+1,!.

% case C1  N - U: N > U2
gt(A, [_B1-B2],[A-A]):-
    numinf(A), A > B2,!.
% case C2  N - U: U1 < N < U2
gt(A, [B1-B2],[A-A]):-
    numinf(A), A =< B2, A > B1,!.

% case D1 U1 - U2: U11 > U22 
gt([A1-A2],[_B1-B2],[A1-A2]):-
    A1 > B2,!.
% case D2 U1 - U2: U11 > U21 > U12  
gt([A1-A2],[B1-_B2],[B11-A2]):-
    B1 >= A1, B1 < A2,
    B11 is B1+1,!.
% case D3 U1 - U2: U21 > U11 > U22
gt([A1-A2],[B1-B2],[A1-A2]):-
    A1>B1, A1 < B2,!.

% ----- A less B ------
% case A N - N
lt(A,B):-numinf(A),numinf(B), A < B,!.

% case B1 U - N: U2 < B
lt([A1-A2],B,[A1-A2]):-
    numinf(B), A2 < B,!.
% case B2 U - N:  A1 < B < A2
lt([A1-A2],B,[A1-B1]):-
    numinf(B), A1 < B, B =< A2,
    B1 is B-1,!.

% case C1  N - U: N > U2
lt(A, [_B1-B2],[A-A]):-
    numinf(A), A > B2,!.
% case C1  N - U: U1 < N < U2
lt(A, [B1-B2],[A-A]):-
    numinf(A), A =< B2, A > B1,!.

% case D1 U1 - U2: U12 < U21 
lt([A1-A2],[B1-_B2],[A1-A2]):-
    A2 < B1,!.
% case D2 U1 - U2: U11 > U22 > U12  
lt([A1-A2],[_B1-B2],[A1-B21]):-
    B2 > A1, B2 =< A2,
    B21 is B2-1,!.
% case D3 U1 - U2: U21 > U12 > U22
lt([A1-A2],[B1-B2],[A1-A2]):-
    A2>=B1, A2 =< B2,!.

% ------  A greater or equal B ------
% case A N - N
geq(A,B):-numinf(A),numinf(B), A >= B,!.

% case B1 U - N: U2 >= B
geq([A1-A2],B,[A1-A2]):-
    numinf(B), A1 >= B,!.
% case B2 U - N:  A1 <= B < A2
geq([A1-A2],B,[B-A2]):-
    numinf(B), A1 =< B, B =< A2,!.

% case C1  N - U: N >= U2
geq(A, [_B1-B2],[A-A]):-
    numinf(A), A >= B2,!.
% case C2  N - U: U1 < N < U2
geq(A, [B1-B2],[A-A]):-
    numinf(A), A =< B2, A >= B1,!.

% case D1 U1 - U2: U11 > U22 
geq([A1-A2],[_B1-B2],[A1-A2]):-
    A1 >= B2,!.
% case D2 U1 - U2: U11 >= U21 >= U12  
geq([A1-A2],[B1-_B2],[B1-A2]):-
    B1 >= A1, B1 =< A2,!.
% case D3 U1 - U2: U21 > U11 > U22
geq([A1-A2],[B1-B2],[A1-A2]):-
    A1>=B1, A1 < B2,!.

% ----- A less or equal B ------
% case A N - N
leq(A,B):-numinf(A),numinf(B), A =< B,!.

% case B1 U - N: U2 < B
leq([A1-A2],B,[A1-A2]):-
    numinf(B), A2 =< B,!.
% case B2 U - N:  A1 < B < A2
leq([A1-A2],B,[A1-B]):-
    numinf(B), A1 =< B, B =< A2,!.

% case C1  N - U: N > U2
leq(A, [_B1-B2],[A-A]):-
    numinf(A), A >= B2,!.
% case C1  N - U: U1 < N < U2
leq(A, [B1-B2],[A-A]):-
    numinf(A), A =< B2, A >= B1,!.

% case D1 U1 - U2: U12 < U21 
leq([A1-A2],[B1-_B2],[A1-A2]):-
    A2 =< B1,!.
% case D2 U1 - U2: U11 > U22 > U12  
leq([A1-A2],[_B1-B2],[A1-B2]):-
    B2 >= A1, B2 =< A2,!.
% case D3 U1 - U2: U21 > U12 > U22
leq([A1-A2],[B1-B2],[A1-A2]):-
    A2>=B1, A2 =< B2,!.

% ----- A equals B ------
% case A N - N
eq(A,B):-number(A),number(B),A=B,!.

% case B1 U - N
eq([A1-A2],B,[B-B]):-number(B),
    B>=A1,B=<A2,!.

% case B1 N - U
eq(A,[B1-B2],[A-A]):-number(A),
    A >= B1, A =< B2.

%
eq([A1-A2],[B1-B2],[MIN-MAX]):-
    max_list([A1,B1],MIN),
    min_list([A2,B2],MAX),
    MIN =< MAX,!.
        
numinf(A):-number(A),!.
numinf(inf).
