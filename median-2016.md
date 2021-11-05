# Médian de 2016

Le code source de tous les prédicats Prolog ainsi que des tests unitaires se trouve dans le fichier [median-2016.pl](./median-2016.pl).

## Exercice 1

### 1. substitute

`substitute(+X, +Y, +L1, ?L2)`.

Retourne vrai si L2 est égal à L1, avec X remplacé par Y

```prolog
% R1: Both lists are empty
substitute(_X, _Y, [], []).

% R2: A symbol different from X is encountered, do not substitute it
substitute(X, Y, [Z | R1], [Z | R2]) :- Z \= X, substitute(X, Y, R1, R2).

% R3: X is encountered, substitute it with Y in L2
substitute(X, Y, [X | R1], [Y | R2]) :- substitute(X, Y, R1, R2).
```

### 2. reverse

`reverse(+L1, ?L2)`.

Retourne vrai si L2 est égal à l'inverse de L1.

Pour définir ce prédicat, il nous faut définir `length(+L, ?N)`, qui donne la longueur de `L` dans `N`, et `nth_element(+N, +L, ?X)`, qui vérifie que `X` est le `N`-ième élément de `L`.

```prolog
length([], 0).
length([_ | R], N) :- length(R, N1), N is N1+1.

nth_element(1, [X | _], X) :- !.
nth_element(N, [_ | R], X) :- N > 1, N1 is N-1, nth_element(N1, R, X).
```

```prolog
% R0: set default 3rd parameter to the length of L1
reverse(L1, L2) :- length(L1, N), reverse(L1, L2, N), !.

% R1: if L1 is empty, L2 should be empty
reverse([], [], _).

% R2: if N is less than 0, L2 should be empty
reverse(_, [], N) :- N =< 0, !.

% R3: ...otherwise, verify that the head element of L2 is the Nth element of L1 and recurse with N+1
reverse(L1, [X | R2], N) :- nth_element(N, L1, X), N1 is N-1, reverse(L1, R2, N1).
```

### 3. insert

`insert(+X, +L1, ?L2)`.

Retourne vrai si `L2` correspond à `L1` avec `X` inséré au bon indice. Suppose que `L1` est trié.

```prolog
% R1: If L1 is empty, set L2 to [X]
insert(X, [], [X]) :- !.

% R2: If the head element of L1 is less than X, recurse
insert(X, [Y | R1], [Y | R2]) :- Y < X, !, insert(X, R1, R2), !.

% R3: If the head element of L1 is greater than X, insert X before Y and set the rest of L2 to be the rest of L1
insert(X, [Y | R], [X, Y | R]) :- Y >= X.
```

### 4. insert\_sort

`insert_sort(+L1, ?L2)`.

Retourne vrai si `L2` correspond à la version triée de `L1`.

```prolog
% R1: if L1 is empty, L2 should be empty
insert_sort([], []) :- !.

% R2: otherwise, call insert_sort recursively and insert the head of L1 in L2
insert_sort([X | R], L2) :- insert_sort(R, R2), insert(X, R2, L2), !.
```

### 5. trace

Pour représenter une matrice sous forme d'une liste, nous pouvons encoder la matrice comme une liste de rangées, qui elles-même sont des listes de nombres.
Par exemple:

```
    ⎡ 1 2 3 ⎤
M = ⎢ 4 5 6 ⎥ -> [[1, 2, 3], [4, 5, 6], [7, 8 9]]
    ⎣ 7 8 9 ⎦
```

`diagonal(+M, ?D)`.

Retourne la liste correspondant aux valeurs de la diagonale de la matrice `M`:

```
    ⎡ 1 2 3 ⎤
M = ⎢ 4 5 6 ⎥
    ⎣ 7 8 9 ⎦

D = [1, 5, 9]
```

```prolog
% R0: set default 3rd parameter to 1
diagonal(M, D) :- diagonal(M, D, 1), !.

% R1: if M is empty, set D to []
diagonal([], [], _).

% R2: otherwise, append the nth of element of the head row of M to D
diagonal([Row | M], [X | D], N) :- nth_element(N, Row, X), N1 is N+1, diagonal(M, D, N1).
```

`trace(+M, ?S)`.

Retourne la somme des valeurs de la diagonale de la matrice `M`.

```
% R0: computes the diagonal vector of M and computes its sum
trace(M, S) :- diagonal(M, L), sum(L, S).
```

## Exercice 2

```
(1) S'il fait beau, je vais en mer.
(2) Si la marée est haute, l'écluse est ouverte.
(3) Si l'écluse est ouverte, je peux aller en mer.
(4) La marée est haute.
(C) Je vais en mer.
```

Soit:

```
H1: Beau => Mer
H2: Maree => Ecluse
H3: Ecluse => Mer'
H4: Maree
```

### Dérivation par résolution

Nous réécrivons `H1-4` en forme standard:

```
H1 = ¬Beau ∨ Mer
H2 = ¬Maree ∨ Ecluse
H3 = ¬Ecluse ∨ Mer'
H4 = Maree
```

Soit `Γ := {H1, H2, H3, H4}`.

#### C₁ = H4, C₂ = H2:

- Soit `σ = []`, `θ = []`, `I₁ = Maree` et `I₂ = Maree`.
- On a `R1 = σ(θ(H4 \ {Maree}) ∪ H2 \ {¬Maree}) = σ(θ({}) ∪ {Ecluse}) = Ecluse`,
- Et `Γ := {H1, H2, H3, H4, {Ecluse}}`.

#### C₁ = R1, C₂ = H3:

- Soit `σ = []`, `θ = []`, `I₁ = Ecluse` et `I₂ = Ecluse`.
- On a `R2 = σ(θ(R1 \ {Maree}) ∪ H3 \ {¬Maree}) = σ(θ({}) ∪ {Mer'}) = Mer'`,
- Et `Γ := {H1, H2, H3, H4, {Ecluse}, {Mer'}}`.

#### Conclusion

Il est trivial de remarquer qu'aucune nouvelle conséquence de `Γ` ne peut être trouvée: seules les deux résolutions précédentes ne pouvaient être appliquées.

Ni `Mer`, ni `¬Mer` n'appartiennent à `Γ`: il ne nous est pas possible de savoir si l'on va en mer ou non, nous savons uniquement qu'il est possible d'y aller.

### Dérivation en logique propositionelle

Nous avons:

```
S1: Ecluse :=
    H2  H4
    ------ (modus ponens)
    Ecluse

S2: Mer' :=
    H3  S1
    ------ (modus ponens)
    Mer'
```

Il nous est donc possible d'aller en mer. En revanche, aucun axiome ne nous permet de déduire `Mer` ("je vais en mer"): `H1` ne peut pas être utilisé car l'atome `Beau` n'est pas défini et aucun autre axiome ne contient `Mer`.
