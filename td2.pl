:- use_module(library(clpfd)).

% adjacent(?E1, ?E2, +L), with {Π A : Universe u}, E1: A, E2: A, L: List(A)
% Returns true if [E1, E2] ⊂ L
adjacent(E1, E2, [E1, E2 | _]).
adjacent(E1, E2, [_ | R]) :- adjacent(E1, E2, R).

% traduit_mot(?C, ?M), C ∈ [[0, 9]], M: string
% Returns true if M is the written-out version of C
traduit_mot(0, "zero").
traduit_mot(1, "un").
traduit_mot(2, "deux").
traduit_mot(3, "trois").
traduit_mot(4, "quatre").
traduit_mot(5, "cinq").
traduit_mot(6, "six").
traduit_mot(7, "sept").
traduit_mot(8, "huit").
traduit_mot(9, "neuf").

% traduit(?Lc, ?Lm), Lc: List(ℕ), Lm: List(string)
% Returns true if Lm = traduit_mot(Lc)
traduit([], []).
traduit([C | Rc], [M | Rm]) :- traduit_mot(C, M), traduit(Rc, Rm).

% insere(+E, ?N, +Lo, ?Lr), {Π A: Universe u}, E : A, N : ℕ, Lo : List(A), Lr : List(A).
% True if Lr is the list Lo with E inserted at the nth position
insere(E, N, [X | Ro], [X | Rr]) :- N #> 0, N1 #= N - 1, insere(E, N1, Ro, Rr).
insere(E, 0, Lo, [E | Lo]).
% insere(E, N, [], [E]). % Uncomment if you wish to insert the elemen at the end of the list if N > length(Lr); causes weird values to be outputted if N isn't defined in that case

% concat(L1, L2, L3), {Π A: Universe u}, L1: List(A), L2: List(A), L3: List(A).
% True if L3 = L1 ++ L2
concat([], [], []).
concat([X | R1], L2, [X | R3]) :- concat(R1, L2, R3).
concat([], L2, L2).

% equi(L1, L2), {Π A: Universe u, '*': A}, L1: List(A), L2: List(A).
% True if L1 is equal to L2, ignoring occurences of "*".
equi(L, L).
equi(['*' | R1], L2) :- equi(R1, L2).
equi(L1, ['*' | R2]) :- equi(L1, R2).
equi([X | R1], [X | R2]) :- R1 \= R2, equi(R1, R2).

% cons_liste_ordonnee(L1, L2, L3), L1: List(ℤ), L2: List(ℤ), L3: List(ℤ)
% True if L3 is the sorted version of L1 and L2
cons_liste_ordonnee(L1, L2, L3) :- concat(L1, L2, L), sort(L, L3).

% selectionner(?X, ?L1, ?L2), with {Π A: Universe u}, L1: List(A), L2: List(A)
% True if L2 is L1, missing an occurence of X
selectionner(X, [X | R], R).
selectionner(X, [Y | L1], [Y | L2]) :- selectionner(X, L1, L2).

% permut(+L1, +L2), with {Π A: Universe u}, L1: List(A), L2: List(A)
% True if L1 is a permutation of L2
permut([], []).
permut([X | R], L) :- permut(R, L1), selectionner(X, L, L1).
