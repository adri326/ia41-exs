% is_set(+L) is true if L does not contain any duplicates
is_set([]).
is_set([X | R]) :- not(member(X, R)), is_set(R).

% list_to_set(L, S), true if S is a de-duplicated version of L.
% Only the first occurence of a duplicate is kept.
list_to_set(L, S) :- list_to_set(L, S, []).
list_to_set([], [], _).
list_to_set([X | RL], [X | RS], E) :- not(member(X, E)), list_to_set(RL, RS, [X | E]).
list_to_set([X | RL], S, E) :- member(X, E), list_to_set(RL, S, E).

% intersection(+S1, +S2, ?SI), true if SI is the intersection of S1 and S2. Note that SI's order will be that of S1
% S1 and S2 must be sets
intersection(S1, S2, SI) :- is_set(S1), is_set(S2), intersection_(S1, S2, SI).
intersection_([], _, []).
intersection_(_, [], []).
intersection_([X | R1], S2, [X | R]) :- member(X, S2), intersection_(R1, S2, R), not(member(X, R)).

% union(+S1, +S2, ?SU), true if SU is the union of S1 and S2.
% S1 and S2 must be sets
union(S1, S2, SU) :- is_set(S1), is_set(S2), append(S1, S2, LU), list_to_set(LU, SU).

% subtract(+S, +D, +R), true if R is S \ D
% S must be a set
subtract(S, D, R) :- is_set(S), subtract_(S, D, R).
subtract_([], _, []).
subtract_([X | RS], D, [X | RR]) :- not(member(X, D)), subtract_(RS, D, RR).
subtract_([X | RS], D, R) :- member(X, D), subtract_(RS, D, R).

% subset(S1, S2), true if S1 ⊂ S2
% S1 and S2 must be sets
subset(S1, S2) :- is_set(S1), is_set(S2), subset_(S1, S2).
subset_([], _).
subset_([X | R], S) :- member(X, S), subset(R, S).
