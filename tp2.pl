:- use_module(library(clpfd)).

% Exercice 1

:- discontiguous homme/4.
:- discontiguous femme/4.
:- discontiguous passion/4.
:- discontiguous cherche/4.

sport(natation, 0).
sport(tennis, 1).
sport(jogging, 2).

homme(paul, grand, brun, mur).
passion(paul, classique, aventure, 0). % natation
cherche(paul, grand, roux, jeune).

homme(pierre, moyen, blond, jeune).
passion(pierre, rock, sf, 1). % tennis
cherche(pierre, X, blond, jeune) :- X = petit ; X = moyen.

homme(jean, petit, brun, mur).
passion(jean, jazz, policier, 1). % tennis
cherche(jean, petit, blond, moyen).

femme(marie, moyen, blond, moyen).
passion(marie, _, aventure, 0). % natation
cherche(marie, grand, brun, moyen).

femme(eva, petit, blond, jeune).
passion(eva, rock, sf, X) :- X #\= 2. % tout sauf jogging
cherche(eva, moyen, blond, jeune).

femme(lea, petit, brun, mur).
passion(lea, classique, aventure, 0). % natation
cherche(lea, moyen, brun, mur).

% On suppose que tout le monde est hétérosexuel
convient_physiquement(X, Y) :-
    homme(X, Xt, Xc, Xa),
    femme(Y, Yt, Yc, Ya),
    cherche(X, Yt, Yc, Ya),
    cherche(Y, Xt, Xc, Xa).

ont_meme_gout(X, Y) :-
    passion(X, M, L, S),
    passion(Y, M, L, S),
    X \= Y.

assortis(X, Y) :-
    convient_physiquement(X, Y),
    ont_meme_gout(X, Y).
assortis([X, Y]) :- assortis(X, Y).

:- begin_tests(assortis).
test(assortis_pierre_eva) :- findall(N, assortis(N), L), L = [[pierre, eva]].
:- end_tests(assortis).

% Exercice 2

entrees([foie_gras, salade_gourmande, crudites, tomates_mozzarella]).
% entree(foie_gras).
% entree(salade_gourmande).
% entree(crudites).
% entree(tomates_mozzarella).
entree(X) :- entrees(L), member(X, L).
viandes([entrecote, magret_de_canard, dinde_fermiere]).
% viande(entrecote).
% viande(magret_de_canard).
% viande(dinde_fermiere).
viande(X) :- viandes(L), member(X, L).
poissons([truite_meuniere, brochet_de_loire, cube_de_bar_en_dret]).
% poisson(truite_meuniere).
% poisson(brochet_de_loire).
% poisson(cube_de_bar_en_dret).
poisson(X) :- poissons(L), member(X, L).
desserts([mousse_au_chocolat, sorbet, ile_flottante, poire_belle_helene]).
% dessert(mousse_au_chocolat).
% dessert(sorbet).
% dessert(ile_flottante).
% dessert(poire_belle_helene).
dessert(X) :- desserts(L), member(X, L).

plat(X) :- viande(X) ; poisson(X).

menu(Entree, Plat, Dessert) :- entree(Entree), plat(Plat), dessert(Dessert).

calories(foie_gras, 208).
calories(dinde_fermiere, 382).
calories(mousse_au_chocolat, 136).
calories(salade_gourmande, 154).
calories(magret_de_canard, 405).
calories(sorbet, 60).
calories(crudites, 81).
calories(truite_meuniere, 260).
calories(ile_flottante, 95).
calories(tomates_mozzarella, 109).
calories(brochet_de_loire, 256).
calories(poire_belle_helene, 114).
calories(entrecote, 537).
calories(cube_de_bar_en_dret, 292).

menu_kc(Entree, Plat, Dessert, KC) :- menu(Entree, Plat, Dessert), calories(Entree, Ke), calories(Plat, Kp), calories(Dessert, Kd), KC #= Ke + Kp + Kd.

:- begin_tests(menu).
test(viande_poisson_cal) :- findall(X, ((viande(X) ; poisson(X)), calories(X, C), C #=< 400), L), length(L, Ll), Ll = 4.
:- end_tests(menu).
