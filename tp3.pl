:- use_module(library(clpfd)).

:- begin_tests(assortis).
test(no_cut) :- findall(X, (
    member(L, [[a, b], [c, d, e]]), member(X, L)
), XL), XL = [a, b, c, d, e].
test(cut_last) :- findall(X, (
    member(L, [[a, b], [c, d, e]]), member(X, L), !
), XL), XL = [a].
test(cut_middle) :- findall(X, (
    member(L, [[a, b], [c, d, e]]), !, member(X, L)
), XL), XL = [a, b].
test(cut_start) :- findall(X, (
    !, member(L, [[a, b], [c, d, e]]), member(X, L)
), XL), XL = [a, b, c, d, e].
:- end_tests(assortis).
% From these tests, we can restrict the predicate chain to only return the first candidate by putting the ! at the end
% ! also prevents other variants of the predicate to be called.

not2(A) :- call(A) -> fail ; !.
:- begin_tests(not2).
test(not) :- not2(member(a, [b])).
test(notnot) :- not2(not2(member(a, [a]))).
:- end_tests(not2).

sas(C, A, _B) :- call(C), !, call(A).
sas(_C, _A, B) :- !, call(B).


premier(X, [X | _]).

dernier(X, [X]).
dernier(X, [_ | R]) :- dernier(X, R).

ajout_debut(X, L, [X | L]).

ajout_fin(X, [Y | L], [Y | R]) :- ajout_fin(X, L, R).
ajout_fin(X, [], [X]).

inclus([], _M).
inclus([X | L], M) :- member(X, M), !, inclus(L, M).

% cons_liste_npairs(L, M) :- premier(X, L), X1 #= X - 1, cons_liste_npairs(L, M, X1).
% cons_liste_npairs(L, [X | M], Y) :- member(X, L), X #> Y, X #= 2 * _, !, cons_liste_npairs(L,)

cons_liste_npairs([], []).
cons_liste_npairs([X | L], [X | M]) :- X #= 2 * _, !, cons_liste_npairs(L, M).
cons_liste_npairs([X | L], M) :- !, X #\= 2 * _, cons_liste_npairs(L, M).

cons_liste_pn(0, []).
cons_liste_pn(N, L) :- cons_liste_pn(N, L, 1).
cons_liste_pn(N, [N], N).
cons_liste_pn(N, [X | L], X) :- X #=< N, X1 #= X + 1, cons_liste_pn(N, L, X1), !.

cons_liste_ordonnee(L1, L2, L3) :- cons_liste_ordonnee2(L1, L2, L3u), sort(L3u, L3).
cons_liste_ordonnee2([X | L1], L2, [X | L3]) :- cons_liste_ordonnee2(L1, L2, L3).
cons_liste_ordonnee2([], [X | L2], [X | L3]) :- cons_liste_ordonnee2([], L2, L3).
cons_liste_ordonnee2([], [], []).

:- begin_tests(cons_liste_ordonnee).
test(cons_liste_ordonnee) :- cons_liste_ordonnee([1, 4, 8], [2, 3, 6], L), !, L = [1, 2, 3, 4, 6, 8].
:- end_tests(cons_liste_ordonnee).
