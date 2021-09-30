% member(?E, +L), with {Π A: Universe u}, X: A, L: List(A)
% true if E ∈ L
member(X, [X|_]).
member(X, [_|R]) :- member(X, R).

% member_strict(?E, +L), with {Π A: Universe u}, X: A, L: List(A)
% true if E ∈ L, does not return multiple times E if E is not set
member_strict(X, [X|R]) :- not(member(X, R)).
member_strict(X, [_|R]) :- member_strict(X, R).

% first(?X, +L), with {Π A: Universe u}, X: A, L: List(A)
% true if L = [X | ...]
first(X, [X|_]).

% last(?X, +L), with {Π A: Universe u}, X: A, L: List(A)
% true if L = [..., X]
last(X, [X]).
last(X, [_ | R]) :- last(X, R).

% verif_list_pnd(+N, +L), with X: ℕ, L: List(ℕ)
% true if L = [N, N-1, ...]
verif_list_pnd(0, []).
verif_list_pnd(N, [N | R]) :- N1 is N-1, N > 0, verif_list_pnd(N1, R).

% verif_list_pnc2(X, L, Max), with X: ℕ, Max: ℕ, L: List(ℕ)
% true if L = [1, 2, ..., X] and X <= Max
verif_list_pnc2(_, [], _).
verif_list_pnc2(X, [X | R], Max) :- X1 is X+1, X =< Max, verif_list_pnc2(X1, R, Max).

% verif_list_pnc(N, L), with N: ℕ, L: List(ℕ)
% true if L = [1, 2, ..., N]
verif_list_pnc(0, []).
verif_list_pnc(N, L) :-
    N > 0,
    verif_list_pnc2(1, L, N),
    last(N, L).

% occurence(+Elem, +L, ?N), with {Π A: Universe u}, Elem: A, L: List(A), N: ℕ
% true if Elem appears N times in L
occurence(_, [], 0).
occurence(Elem, [Elem | R], N) :- N #> 0, O #= N-1, occurence(Elem, R, O).
occurence(Elem, [NotElem | R], N) :- NotElem \= Elem, occurence(Elem, R, N).

% remove(+Elem, +L1, ?L2), with {Π A: Universe u}, Elem: A, L1: List(A), L2: List(A)
% true if L2 corresponds to L1, with all occurences of Elem removed
remove(_, [], []).
remove(Elem, [Elem | R1], R2) :- remove(Elem, R1, R2).
remove(Elem, [NotElem | R1], [NotElem | R2]) :- NotElem \= Elem, remove(Elem, R1, R2).
