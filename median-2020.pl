% 2.1 == cons_mat_diag(+D, -M), constructs M, the diagonal matrix, with diagonal terms D

% Required predicate: zeroes(-L, +N); fills L with a list of N zeroes
zeroes([], 0) :- !.
zeroes([0 | R], N) :- N > 0, N1 is N-1, zeroes(R, N1).

% Required predicate: cons_row(+D, -L, +N); constructcs a row of the diagonal matrix
cons_row([_ | D], [0 | L], N) :- N > 0, N1 is N-1, cons_row(D, L, N1), !.
cons_row([X | D], [X | L], 0) :- length(D, N), zeroes(L, N).

% R0: set default 3rd parameter to the length of D
cons_mat_diag(D, M) :- cons_mat_diag(D, M, 0), !.

% R1: if N is zero, set M to []
cons_mat_diag(D, [], N) :- length(D, N), !.

% R2: otherwise, create a row of the matrix:
cons_mat_diag(D, [Row | Mat], N) :- N1 is N+1, cons_mat_diag(D, Mat, N1), cons_row(D, Row, N).


:- begin_tests(cons_mat_diag).

test("zeroes") :-
    zeroes(L1, 0), L1 = [],
    zeroes(L2, 1), L2 = [0],
    zeroes(L3, 5), L3 = [0, 0, 0, 0, 0].

test("cons_row") :-
    findall(L, (member(X, [0, 1, 2]), cons_row([1, 2, 3], L, X)), L1),
    L1 = [[1, 0, 0], [0, 2, 0], [0, 0, 3]].

test("cons_mat_diag") :-
    cons_mat_diag([1], M1),
    M1 = [[1]],
    cons_mat_diag([2, 4, 6], M2),
    M2 = [[2, 0, 0], [0, 4, 0], [0, 0, 6]].

:- end_tests(cons_mat_diag).


% 2.2 == elem_non_nul(+L, -X); sets X to the first non-zero element of L

% R1: Zero found, recurse
elem_non_nul([0 | R], X) :- elem_non_nul(R, X), !.

% R2: Non-zero found, assign X to that value
elem_non_nul([X | _], X) :- X \= 0.


:- begin_tests(elem_non_nul).

test("elem_non_nul on zero list") :-
    not(elem_non_nul([0, 0, 0], _)),
    not(elem_non_nul([], _)).

test("elem_non_nul") :-
    elem_non_nul([0, 0, 1], X), X = 1,
    elem_non_nul([4, 0, 5], Y), Y = 4,
    elem_non_nul([3], Z), Z = 3.

:- end_tests(elem_non_nul).


% 2.3 == diagonal(+M, -D); puts in D the diagonal elements of M, supposing that M is a diagonal matrix.

% R1: calculate the head of the diagonal list using `elem_non_nul` and recurse
diagonal([Row | M], [X | D]) :- elem_non_nul(Row, X), diagonal(M, D), !.

% R2: empty matrix, empty diagonal
diagonal([], []).


:- begin_tests(diagonal).

test("diagonal") :-
    forall(member(L, [[], [1], [2, 3], [4, 5, 6]]), (
        cons_mat_diag(L, M), diagonal(M, D), !, D = L
    )).

:- end_tests(diagonal).


% 2.4 == mult_matrix_diag(+M1, +M2, -M3); puts in M3 the result of the multiplication of M1 and M2, assuming that they are diagonal matrices of the same size

% mult_diag(+L1, +L2, -L3); multiplies L1 and L2 such that ∀i≤n, L3[i] = L1[i] * L2[i]
mult_diag([X | R1], [Y | R2], [Z | R3]) :- mult_diag(R1, R2, R3), !, Z is X * Y.
mult_diag([], [], []).

mult_matrix_diag(M1, M2, Res) :-
    length(M1, N), length(M2, N), % Verify that matrices have the same size
    diagonal(M1, D1), diagonal(M2, D2), % Grab the diagonal vectors of both matrices
    mult_diag(D1, D2, D3), % Multiply these two vectors together coordinate-wise
    cons_mat_diag(D3, Res).


:- begin_tests(mult_matrix_diag).

test("mult_diag") :-
    mult_diag([1, 2], [3, 5], L1), L1 = [3, 10],
    mult_diag([5], [7], L2), L2 = [35],
    mult_diag([], [], []),
    not(mult_diag([1, 1, 1, 1], [2, 2, 2, 2], [3, 3, 3, 3])).

test("mult_matrix_diag") :-
    D1 = [3, 5, 7],
    D2 = [11, 13, 17],
    mult_diag(D1, D2, D3),
    cons_mat_diag(D1, M1), cons_mat_diag(D2, M2),
    mult_matrix_diag(M1, M2, M3),
    diagonal(M3, D3).

:- end_tests(mult_matrix_diag).
