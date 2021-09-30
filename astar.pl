% target = [4, 4] % [x, y]
% source = [2, 1]

source([2, 1]).
target([4, 4]).

joined_([1, 1], [2, 1]).
joined_([1, 1], [1, 2]).
joined_([1, 2], [1, 3]).
joined_([1, 3], [1, 4]).
joined_([1, 4], [2, 4]).
joined_([2, 4], [3, 4]).
joined_([3, 4], [4, 4]).
joined_([2, 1], [3, 1]).
joined_([3, 1], [4, 1]).
joined_([4, 1], [4, 2]).
joined_([4, 2], [3, 2]).
joined_([3, 2], [2, 2]).
joined_([2, 2], [2, 3]).
joined_([2, 3], [3, 3]).
joined_([3, 3], [3, 4]).
joined_([3, 4], [4, 4]).

joined(A, B) :- joined_(A, B) ; joined_(B, A).

g([X1, Y1], [X2, Y2], G) :- G is sqrt((X1 - X2) * (X1 - X2) + (Y1 - Y2) * (Y1 - Y2)).

h([P | Path], H) :- target(Target), g(P, Target, G), length(Path, L), H is L + 1 + G.
h_([], L) :- L = [].
h_([X | R], L) :- h(X, H), h_(R, L2), L = [[H, X] | L2].

guesses_([P | Path], N) :- joined(P, N1), h([N1, P | Path], _H), N = [N1, P | Path].
guesses(P, L) :- findall(N, guesses_(P, N), L).

exclude_guesses([], _, []).
exclude_guesses([N | R], E, [N | R2]) :- [N1 | _] = N, not(member(N1, E)), exclude_guesses(R, E, R2).
exclude_guesses([N | R], E, R2) :- [N1 | _] = N, member(N1, E), exclude_guesses(R, E, R2).

get_best_guess(Guesses, Best) :-
  h_(Guesses, H),
  sort(1, @=<, H, H2),
  H2 = [Best | _].

a_star(Source, Target, Path) :- a_star([[Source]], Target, Path, []).
a_star(Open, Target, Path, Closed) :-
  get_best_guess(Open, Best),
  [_Heuristic, Head] = Best,
  (
    (
      Head = [Target | _],
      reverse(Head, Path),
      writeln(Path)
    );
    [Node | _] = Head,
    Closed2 = [Node | Closed],
    guesses(Head, Open2),
    exclude_guesses(Open2, Closed2, Open3),
    % writeln(Path),
    a_star(Open3, Target, Path, Closed2)
  ).

:- writeln("Solution is: ").
:- source(Source), target(Target), a_star(Source, Target, _Path).
:- writeln("").
