% 1.1 == substitute(+X, +Y, +L1, ?L2); returns true if L2 is equal to L1 with X substituted by Y

% R1: Both lists are empty
substitute(_X, _Y, [], []).

% R2: A symbol different from X is encountered, do not substitute it
substitute(X, Y, [Z | R1], [Z | R2]) :- Z \= X, substitute(X, Y, R1, R2).

% R3: X is encountered, substitute it with Y in L2
substitute(X, Y, [X | R1], [Y | R2]) :- substitute(X, Y, R1, R2).

:- begin_tests(substitute).

test("Empty list") :-
    substitute(a, b, [], L), !,
    L = [].

test("Simple list") :-
    substitute(a, b, [a], L), !,
    L = [b].

test("Simple list, no substitution") :-
    substitute(a, b, [c], L), !,
    L = [c].

test("Long list") :-
    substitute(1, 2, [0, 1, 2, 0, 1, 2, 3, 1], L), !,
    L = [0, 2, 2, 0, 2, 2, 3, 2].

test("Long list, exhaustive") :-
    findall(L, (
        member(X, [0, 1, 2]), substitute(X, x, [2, 1, 0, 1, 2, 1], L)
    ), L2),
    L2 = [[2, 1, x, 1, 2, 1], [2, x, 0, x, 2, x], [x, 1, 0, 1, x, 1]].

:- end_tests(substitute).

% 1.2 == reverse(+L1, ?L2), true if L2 is the inverted version of L1

% Required predicate: length(+L, ?N). This predicate exists in SWI Prolog's built-in list operations.
% length([], 0).
% length([_ | R], N) :- length(R, N1), N is N1+1.

% Required predicate: nth_element(+N, +L, ?X), returns true if X is the Nth element, false otherwise
% NOTE: this is different from nth0, which raises an error if N is out of bound
nth_element(1, [X | _], X) :- !.
nth_element(N, [_ | R], X) :- N > 1, N1 is N-1, nth_element(N1, R, X).

% R0: set default 3rd parameter to the length of L1
reverse(L1, L2) :- length(L1, N), reverse(L1, L2, N), !.

% R1: if L1 is empty, L2 should be empty
reverse([], [], _).

% R2: if N is less than 0, L2 should be empty
reverse(_, [], N) :- N =< 0, !.

% R3: ...otherwise, verify that the head element of L2 is the Nth element of L1 and recurse with N+1
reverse(L1, [X | R2], N) :- nth_element(N, L1, X), N1 is N-1, reverse(L1, R2, N1).

:- begin_tests(reverse).

test("nth_element: returns false for empty list") :-
    not(nth_element(_, [], _)).
test("nth_element: returns false for out-of-bound index") :-
    not(nth_element(2, [a], _)), not(nth_element(0, [a], _)).
test("nth_element: simple list") :-
    findall(X, (
        member(N, [0, 1, 2, 3, 4]), nth_element(N, [a, b, c], X)
    ), L),
    L = [a, b, c].

test("reverse: empty list") :-
    reverse([], []).

test("reverse: simple lists") :-
    reverse([a], [a]),
    reverse([a, b], [b, a]),
    reverse([a, b, c], [c, b, a]).

test("reverse: no leftover choicepoints") :-
    findall(L, reverse([a, b, c, d], L), L2),
    L2 = [[d, c, b, a]].

:- end_tests(reverse).

% 1.3 == insert(+X, +L1, ?L2), true if L2 corresponds to L1 with X inserted at the right index, supposes that L1 is sorted

% R1: If L1 is empty, set L2 to [X]
insert(X, [], [X]) :- !.

% R2: If the head element of L1 is less than X, recurse
insert(X, [Y | R1], [Y | R2]) :- Y < X, !, insert(X, R1, R2), !.

% R3: If the head element of L1 is greater than X, insert X before Y and set the rest of L2 to be the rest of L1
insert(X, [Y | R], [X, Y | R]) :- Y >= X.

:- begin_tests(insert).

test("insert: empty list") :-
    insert(1, [], [1]).

test("insert: simple lists") :-
    insert(3, [2, 4], [2, 3, 4]),
    insert(6, [5, 6, 7], [5, 6, 6, 7]),
    insert(0, [-1, 1], [-1, 0, 1]),
    insert(5, [0, 2, 3], [0, 2, 3, 5]),
    insert(1, [7, 9, 10], [1, 7, 9, 10]).

test("insert: guess L2") :-
    findall(L, (
        member(X, [1, 2, 3, 9]),
        insert(X, [0, 2, 4], L)
    ), L2),
    L2 = [[0, 1, 2, 4], [0, 2, 2, 4], [0, 2, 3, 4], [0, 2, 4, 9]].

:- end_tests(insert).

% 1.4 == insert_sort(+L1, ?L2), true if L2 is the sorted version of L1

% R1: if L1 is empty, L2 should be empty
insert_sort([], []) :- !.

% R2: otherwise, call insert_sort recursively and insert the head of L1 in L2
insert_sort([X | R], L2) :- insert_sort(R, R2), insert(X, R2, L2), !.


:- begin_tests(insert_sort).

test("insert_sort: empty list") :-
    insert_sort([], L), L = [].

test("insert_sort: simple lists") :-
    insert_sort([1, 2, 3], L1), L1 = [1, 2, 3],
    insert_sort([3, 2, 1], L2), L2 = [1, 2, 3],
    insert_sort([3, 1, 2], L3), L3 = [1, 2, 3].

:- use_module(library(random)).

generate_n_random_numbers(N, X) :-
    random_between(0, 1000, X);
    (N > 0, N1 is N-1, generate_n_random_numbers(N1, X)).

is_sorted([]) :- !.
is_sorted([_]) :- !.
is_sorted([X, Y | R]) :- is_sorted([Y | R]), X =< Y.

test("insert_sort: long list") :-
    findall(X, generate_n_random_numbers(100, X), L),
    insert_sort(L, S),
    is_sorted(S), % S should be sorted
    forall(member(X, L), member(X, S)), % S should contain every number of L
    forall(member(X, S), member(X, L)). % S shouldn't contain numbers outside of L's

:- end_tests(insert_sort).

% 1.5 ==

% 1.5b -- diagonal(+M, ?L); true if L contains the diagonal values of M

% R0: set default 3rd parameter to 1
diagonal(M, D) :- diagonal(M, D, 1), !.

% R1: if M is empty, set D to []
diagonal([], [], _).

% R2: otherwise, append the nth of element of the head row of M to D
diagonal([Row | M], [X | D], N) :- nth_element(N, Row, X), N1 is N+1, diagonal(M, D, N1).

:- begin_tests(diagonal).

test("diagonal: empty matrix") :-
    diagonal([], L), L = [].

test("diagonal: simple matrices") :-
    diagonal([[a]], L1), L1 = [a],
    diagonal([[1, 2, 3], [4, 5, 6], [7, 8, 9]], L2), L2 = [1, 5, 9].

:- end_tests(diagonal).

% 1.5c -- trace(+M, ?S); true if S is the sum of of the diagonal values in M

% sum(+L, ?S), computes the sum of the elements in L. Present in the "lists" library as "sum_list"
sum([], 0).
sum([X | R], S) :- sum(R, S1), S is S1 + X.

% R0: computes the diagonal vector of M and computes its sum
trace(M, S) :- diagonal(M, L), sum(L, S).

:- begin_tests(trace).

test("trace: empty matrix") :-
    trace([], S), S = 0.

test("trace: simple matrices") :-
    trace([[8]], S1), S1 = 8,
    trace([[1, 2, 3], [4, 5, 6], [7, 8, 9]], S2), S2 = 15.

:- end_tests(trace).
