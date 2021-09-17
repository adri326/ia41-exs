% Encode the tree:

% man(?X), with X: atom; true if X is a man
:- discontiguous man/1.
% woman(?X), with X: atom; true if X is a woman
:- discontiguous woman/1.
% couple(?N, ?X, ?Y), with N: ℕ, X: atom, Y: atom; true if X and Y are the N-th couple
:- discontiguous couple/3.
% child(?X, ?N), with N: ℕ, X: atom; true if X is the child of the N-th couple
:- discontiguous child/2.

man(odilon).
woman(genevieve).
couple(1, odilon, genevieve).
child(melanie, 1).
woman(melanie).
child(pauline, 1).
woman(pauline).
child(edouard, 1).
man(edouard).

man(roger).
couple(2, roger, melanie).
child(roseline, 2).
woman(roseline).
child(marcel, 2).
man(marcel).

woman(tatiana).
couple(3, edouard, tatiana).
child(marius, 3).
man(marius).
child(prisca, 3).
woman(prisca).

man(vincent).
couple(4, vincent, roseline).
child(agnes, 4).
woman(agnes).
child(arnaud, 4).
man(arnaud).

woman(paule).
couple(5, marcel, paule).
child(martine, 5).
woman(martine).
child(blaise, 5).
man(blaise).

% father(?X, ?Y), true if X is the father of Y
father(X, Y) :- child(Y, N), couple(N, X, _).

% mother(?X, ?Y), true if X is the mother of Y
mother(X, Y) :- child(Y, N), couple(N, _, X).

% parent(?X, ?Y), true if X is the parent of Y
parent(X, Y) :- father(X, Y); mother(X, Y).

% brother(?X, ?Y), true if X is the brother of Y
brother(X, Y) :- child(X, N), child(Y, N), man(X), X \= Y.
% sister(?X, ?Y), true if X is the sister of Y
sister(X, Y) :- child(X, N), child(Y, N), woman(X), X \= Y.

% brother_or_sister(?X, ?Y), X: atom, Y: atom
brother_or_sister(X, Y) :- brother(X, Y); sister(X, Y).

% uncle(?X, ?Y), true if X is the uncle of Y
uncle(X, Y) :- parent(Z, Y), brother(X, Z).

% aunt(?X, ?Y), true if X is the aunt of Y
aunt(X, Y) :- parent(Z, Y), sister(X, Z).

% grand-parent(?X, ?Y), true if X is the grand-parent of Y
grand-parent(X, Y) :- parent(X, Z), parent(Z, Y).

% cousin(?X, ?Y), true if X is the cousin of Y
cousin(X, Y) :- parent(Xp, X), brother_or_sister(Xp, Yp), parent(Yp, Y).
