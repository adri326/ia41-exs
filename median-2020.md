# Médian de 2020

## Exercice 1

### 1. Quels sont les éléments qui permettent de définir une logique formelle?

Pour définir une logique formelle, il faut:

- une grammaire `G = (N, Σ, P, S)`, composée d'un ensemble de symboles non-terminaux `N`, d'un ensemble de symboles terminaux `Σ`, d'une règle de production `P` et d'un symbole initial `S ∈ N`.
- un langage `ℒ(G) ⊆ Σ*`, défini étend l'ensemble des phrases pouvant être construites par application répétée de `P`.
- un ensembles d'axiomes `(Hₙ) ∈ ℒ(G)*`
- un ensemble de règles d'inférences `(Aₙ) ∈ (ℒ(G)* -> ℒ(G))`, prenant en entrée un ensemble de conditions et donnant en sortie une formule vraie dans la logique formelle en question

Les règles d'inférence sont indispensables pour réaliser une démonstration dans la logique formelle: sans celles-cis, il serait impossible de dériver des formulles vraies à partir des axiomes de départ.

### 2. Considérons qu'un facteur a 15 villages à visiter, il voudrait déterminer une tournée qui est la plus courte possible

Ce problème est le problème du "Travelling Salesman", qui fait partie des problèmes `NP-complets` et est appliqué à un grand nombre de domaines.

Ce problème est un problème de planification.

On considère le graphe des villages: les noeuds correspondent aux villages à visiter et les arrêtes aux chemins entre les villages.
Les arrêtes contiennent une valeur, correspondant à la distance entre les villages.

Nous pouvons encoder ce problème sous forme d'un arbre d'exploration: chaque noeud correspond à un chemin et ses enfants aux chemins possibles qui étendent le noeud parent.

Un état aurait alors la forme `(Nᵢ, Nⱼ, ...)`, avec `Nᵢ`, `Nⱼ`, ..., des noeuds du graphe des villages. L'on peut calculer la distance parcourue `d = d(Nᵢ, Nⱼ) + d(Nⱼ, Nₖ) + ...`

Un noeud est terminal lorsqu'il contient tous les villages.
Le noeud recherché est le noeud pour lequel la distance parcourue `d` est minimale.

## Exercice 2

### 1. `cons_mat_diag(+D, -M)`.

Construit la matrice diagonale `M`, avec comme termes `D`.

#### Prérequis: `zeroes(-L, +N)`

Remplit la liste `L` avec `N` zéros.

```prolog
zeroes([], 0) :- !.
zeroes([0 | R], N) :- N > 0, N1 is N-1, zeroes(R, N1).
```

#### Prérequis: `cons_row(+D, -L, +N)`

Remplit la liste `L` de `length(D)` zéros, à l'exception du `N`-ième terme, qui prend la valeur de `D[N]`.

```prolog
% Fill in the beginning of L with zeroes
cons_row([_ | D], [0 | L], N) :- N > 0, N1 is N-1, cons_row(D, L, N1), !.

% Set the N-th element of L to the N-th element of D, filling the remainder with zeroes
cons_row([X | D], [X | L], 0) :- length(D, N), zeroes(L, N).
```

#### Définition de `cons_mat_diag`

```prolog
% R0: set default 3rd parameter to the length of D
cons_mat_diag(D, M) :- cons_mat_diag(D, M, 0), !.

% R1: if N is zero, set M to []
cons_mat_diag(D, [], N) :- length(D, N), !.

% R2: otherwise, create a row of the matrix:
cons_mat_diag(D, [Row | Mat], N) :- N1 is N+1, cons_mat_diag(D, Mat, N1), cons_row(D, Row, N).
```

### 2. `elem_non_nul(+L, -X)`

Assigne à `X` le premier élément non-nul de `L`.

```prolog
% R1: Zero found, recurse
elem_non_nul([0 | R], X) :- elem_non_nul(R, X), !.

% R2: Non-zero found, assign X to that value
elem_non_nul([X | _], X) :- X \= 0.
```

### 3. `diagonal(+M, -D)`

Assigne à `D` la listes des termes de la diagonale de la matrice diagonale `M`.

```prolog
% R1: calculate the head of the diagonal list using `elem_non_nul` and recurse
diagonal([Row | M], [X | D]) :- elem_non_nul(Row, X), diagonal(M, D), !.

% R2: empty matrix, empty diagonal
diagonal([], []).
```

### 4. `mult_matrix_diag(+M1, +M2, -M3)`

Calcule `M3 = M1 * M2`, avec `M1` et `M2` des matrices diagonales de même taille.

#### Prérequis: `mult_diag(+L1, +L2, -L3)`

Multiplie `L1` et `L2`, tel que `∀i≤n, L3[i] = L1[i] * L2[i]`.

```prolog
mult_diag([X | R1], [Y | R2], [Z | R3]) :- mult_diag(R1, R2, R3), !, Z is X * Y.
mult_diag([], [], []).
```

#### Construction de `mult_matrix_diag`

```prolog
mult_matrix_diag(M1, M2, Res) :-
    length(M1, N), length(M2, N), % Verify that matrices have the same size
    diagonal(M1, D1), diagonal(M2, D2), % Grab the diagonal vectors of both matrices
    mult_diag(D1, D2, D3), % Multiply these two vectors together coordinate-wise
    cons_mat_diag(D3, Res).
```

## Exercice 3

```
(1) Lorsqu'il y a grève, il n'y a pas école.
(2) Lorsqu'il n'y a pas école, si la bibliothèque est ouverte, je me rends à la bibliothèque; sinon, je reste à la maison.
(3) Il y a grève et je ne vais pas à la bibliothèque.
(C) Je reste à la maison.
```

### Dérivation de `(C)` par la méthode de résolution

Nous réecrivons les formules de l'énoncé en forme standard:

```
H1: ¬G ∨ ¬E
H2: E ∨ (BO ∧ B) ∨ (¬BO ∧ M) = (E ∨ BO ∨ ¬BO) ∧ (E ∨ BO ∨ M) ∧ (E ∨ B ∨ ¬BO) ∧ (E ∨ B ∨ M)
H3: G ∧ ¬B
```

`Γ := H1 ∪ H2 ∪ H3`.

#### C₁ = {G}, C₂ = H1

- Soit `σ = []`, `θ = []`, `I₁ = I₂ = G`
- `R1 = σ(θ({G} \ {G}) ∪ (H1 \ {¬G})) = σ({} ∪ {¬E}) = {¬E}`
- `Γ := H1 ∪ H2 ∪ H3 ∪ {¬E}`

#### C₁ = {E ∨ B ∨ ¬BO}, C₂ = R1

- Soit `σ = []`, `θ = []`, `I₁ = I₂ = E`
- `R2 = σ(θ({E ∨ B ∨ ¬BO} \ {E}) ∪ (R1 \ {¬E})) = σ({B ∨ ¬BO} ∪ {}) = {B ∨ ¬BO}`
- `Γ := H1 ∪ H2 ∪ H3 ∪ {¬E, B ∨ ¬BO}`

#### C₁ = {E ∨ BO ∨ M}, C₂ = R1

- Soit `σ = []`, `θ = []`, `I₁ = I₂ = E`
- `R2' = σ(θ({E ∨ BO ∨ M} \ {E}) ∪ (R1 \ {¬E})) = σ({BO ∨ M} ∪ {}) = {BO ∨ M}`
- `Γ := H1 ∪ H2 ∪ H3 ∪ {¬E, B ∨ ¬BO, BO ∨ M}`

#### C₁ = {B ∨ ¬BO}, C₂ = {¬B}

- Soit `σ = []`, `θ = []`, `I₁ = I₂ = B`
- `R3 = σ(θ({B ∨ ¬BO} \ {B}) ∪ ({¬B} \ {¬B})) = {¬BO}`
- `Γ := H1 ∪ H2 ∪ H3 ∪ {¬E, B ∨ ¬BO, BO ∨ M, ¬BO}`

#### C₁ = {BO ∨ M}, C₂ = {¬BO}

- Soit `σ = []`, `θ = []`, `I₁ = I₂ = BO`
- `R3 = σ(θ({BO ∨ M} \ {BO}) ∪ ({¬BO} \ {¬BO})) = {M}`
- `Γ := H1 ∪ H2 ∪ H3 ∪ {¬E, B ∨ ¬BO, BO ∨ M, ¬BO, M}`

#### Conclusion

`{M} ∈ Γ`, donc on reste en effet à la maison.

### Dérivation de `(C)` en logique propositionelle

Soit les constantes:
- `G` ("il y a grève")
- `E` ("il y a école")
- `BO` ("la bibliothèque est ouverte")
- `B` ("je me rends à la bibliothèque")
- `M` ("je reste à la maison")

On peut écrire les axiomes suivants, correspondant aux énoncés de la question:

```
H1: G => ¬E
H2: ¬E => (BO ∧ B) ∨ (¬BO ∧ M)
H3: G ∧ ¬B
```

On applique alors les inférences suivantes:

```
R1: ¬E :=
    G => ¬E (H1)    G (H3)
    ---------------------- (modus ponens)
    ¬E
```

```
R2: (BO ∧ B) ∨ (¬BO ∧ M) :=
        H2     ¬E R1
    -------------------- (modus ponens)
    (BO ∧ B) ∨ (¬BO ∧ M)
```

```
R3: ¬(BO ∧ B) :=
                                            ¬B (H3)
                                           --------- (or-introduction)
                                           ¬B ∨ ¬BO
    -------------------- (exluded middle)  --------- (neg-distribution)
    (BO ∧ B) ∨ ¬(BO ∧ B)                   ¬(B ∧ BO)
    ------------------------------------------------ (or-distribution)
    ((BO ∧ B) ∧ ¬(B ∧ BO)) ∨ (¬(BO ∧ B) ∧ ¬(B ∧ BO))
    let (BO ∧ B) ∧ ¬(B ∧ BO) => ¬(BO ∧ B), from (x ∧ ¬x => false, false-elimination, transitivity)
    let (¬(BO ∧ B) ∧ ¬(B ∧ BO)) => ¬(B ∧ BO), from (and-elimination)
    ------------------------------------------------ (or-elimination)
    ¬(BO ∧ B)
```

```

R4: ¬BO ∧ M :=
    ¬(BO ∧ B) (R3)    (BO ∧ B) ∨ (¬BO ∧ M) (R2)
    ------------------------------------------- (or-distribution)
    (¬(BO ∧ B) ∧ (BO ∧ B)) ∨ (¬(BO ∧ B) ∧ (¬BO ∧ M))
    let (¬(BO ∧ B) ∧ (BO ∧ B)) => ¬BO ∧ M, from (x ∧ ¬x => false, false-elimination, transitivity)
    let (¬(BO ∧ B) ∧ (¬BO ∧ M)) => ¬BO ∧ M, from (and-elimination)
    ------------------------------------------------ (or-elimination)
    ¬BO ∧ M
```

D'où `{H1, H2, H3} ⊦ M`: on reste bien à la maison, car la bibliothèque n'est pas ouverte.
