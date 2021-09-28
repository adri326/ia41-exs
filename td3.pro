% is_set(+L) is true if L does not contain any duplicates
is_set([]).
is_set([X | R]) :- not(member(X, R)), is_set(R).

% list_to_set(L, S), true if S is a de-duplicated version of L
list_to_set([], []).
list_to_set([X | RL], [X | RS]) :- list_to_set(RL, RS), not(member(X, RS)).
list_to_set([X | RL], S) :- list_to_set(RL, S), member(X, S).

% TODO
intersection(_, _, []).
intersection(S1, S2, [X | R]) :- member(X, S1), member(X, S2), intersection(S1, S2, R), not(member(X, R)), is_set(S1), is_set(S2).
