%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PUISSANCE 4
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Initialiser le plateau de jeu
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
initialisation(L):- 
	L = [0,0,0,0,0,0,0, 0,0,0,0,0,0,0, 0,0,0,0,0,0,0, 0,0,0,0,0,0,0, 0,0,0,0,0,0,0, 0,0,0,0,0,0,0].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Menu
% Choix du jeu
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
jouer :-
	initialisation(I), 
	write('******************** PUISSANCE 4 ********************\n*********** Choisissez votre mode de jeu ************\n*********** 1 : Humain VS Humain         ************\n*********** 2 : Humain VS IA             ************\n*****************************************************\nVotre choix : '),
	read(C),
	choix(C, I, 1).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Mode de jeu choix(<mode de jeu>, ListeCourante,Joueur)
% <mode de jeu>
% 1 : Joueur VS Joueur
% 2 : Joueur VS IA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
choix(1, L, Joueur):-
	write('Joueur '), write(Joueur), write(' entrez la colonne à remplir : '), read(C), joueur(L,Joueur,C).

choix(2, L, _):-
	test(L,1), write('Joueur 1 a gagné').
choix(2, L, _):-
	test(L,2), write('Joueur 2 a gagné').
choix(2, L, _):-
	write('A Vous ! '), read(C), joueurIA(L,2,C).	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Vérifier la position du jeu
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test(L, _):-
	not(memberchk(0,L)), 
	write('Fin du jeu').
% check si le jeu n'est pas terminé (colonne)
test(L, Joueur):- 
	between(1,21,P),  
	nth1(P,L,Joueur), 
	P2 is P+7, nth1(P2,L,Joueur),
	P3 is P2+7, nth1(P3,L,Joueur),
	P4 is P3+7, nth1(P4,L,Joueur).
% check si le jeu n'est pas terminé (ligne)
test(L, Joueur):-  
	between(1,42,P),
	P mod 7 < 5, P mod 7 \=0, nth1(P,L,Joueur),
	P2 is P+1, nth1(P2,L,Joueur),
	P3 is P2+1, nth1(P3,L,Joueur),
	P4 is P3+1, nth1(P4,L,Joueur).
% check si le jeu n'est pas terminé (diagonale descendante)
test(L, Joueur):- 
	between(1,21,P),
	P mod 7 < 5, P mod 7 \=0,  nth1(P,L,Joueur),
	P2 is P+8, nth1(P2,L,Joueur),
	P3 is P2+8, nth1(P3,L,Joueur), 
	P4 is P3+8, nth1(P4,L,Joueur).
% check si le jeu n'est pas terminé (diagonale montante)
test(L, Joueur):- 
	between(1,22,P),
	P mod 7 > 3,  P mod 7 \=0, nth1(P,L,Joueur), 
	P2 is P+6, nth1(P2,L,Joueur),
	P3 is P2+6, nth1(P3,L,Joueur),
	P4 is P3+6, nth1(P4,L,Joueur).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Change de joueur
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
secondJoueur(1,2).
secondJoueur(2,1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Vérifier la position du jeu de façon à miniser les chances pour l'adversaire
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Vérification pour évaluation X Y Y Y(colonne)
testD(L, Joueur):- between(1,21,P), secondJoueur(Joueur, Autre), nth1(P,L,Autre), P2 is P+7, nth1(P2,L,Joueur),P3 is P2+7, nth1(P3,L,Joueur), P4 is P3+7, nth1(P4,L,Joueur).
% Vérification pour évaluation Y X Y Y(colonne)
testD(L, Joueur):- between(1,21,P), secondJoueur(Joueur, Autre), nth1(P,L,Joueur), P2 is P+7, nth1(P2,L,Autre),P3 is P2+7, nth1(P3,L,Joueur), P4 is P3+7, nth1(P4,L,Joueur).
% Vérification pour évaluation Y Y X Y(colonne)
testD(L, Joueur):- between(1,21,P), secondJoueur(Joueur, Autre), nth1(P,L,Joueur), P2 is P+7, nth1(P2,L,Joueur),P3 is P2+7, nth1(P3,L,Autre), P4 is P3+7, nth1(P4,L,Joueur).
% Vérification pour évaluation Y Y Y X(colonne)
testD(L, Joueur):- between(1,21,P), secondJoueur(Joueur, Autre), nth1(P,L,Joueur), P2 is P+7, nth1(P2,L,Joueur),P3 is P2+7, nth1(P3,L,Joueur), P4 is P3+7, nth1(P4,L,Autre).

% Vérification pour évaluation X Y Y Y (ligne)
testD(L, Joueur):-  between(1,42,P), P mod 7 < 5, Z is P mod 7, Z \=0,secondJoueur(Joueur, Autre), nth1(P,L,Autre), P2 is P+1, nth1(P2,L,Joueur),P3 is P2+1, nth1(P3,L,Joueur), P4 is P3+1, nth1(P4,L,Joueur).
% Vérification pour évaluation Y X Y Y (ligne)
testD(L, Joueur):-  between(1,42,P), P mod 7 < 5, Z is P mod 7, Z \=0,secondJoueur(Joueur, Autre), nth1(P,L,Joueur), P2 is P+1, nth1(P2,L,Autre),P3 is P2+1, nth1(P3,L,Joueur), P4 is P3+1, nth1(P4,L,Joueur).
% Vérification pour évaluation Y Y X Y (ligne)
testD(L, Joueur):-  between(1,42,P), P mod 7 < 5, Z is P mod 7, Z \=0,secondJoueur(Joueur, Autre), nth1(P,L,Joueur), P2 is P+1, nth1(P2,L,Joueur),P3 is P2+1, nth1(P3,L,Autre), P4 is P3+1, nth1(P4,L,Joueur).
% Vérification pour évaluation Y Y Y X (ligne)
testD(L, Joueur):-  between(1,42,P), P mod 7 < 5, Z is P mod 7, Z \=0,secondJoueur(Joueur, Autre), nth1(P,L,Joueur), P2 is P+1, nth1(P2,L,Joueur),P3 is P2+1, nth1(P3,L,Joueur), P4 is P3+1, nth1(P4,L,Autre).

% Vérification pour évaluation (diagonale descendante)
testD(L, Joueur):- between(1,21,P), P mod 7 < 5, P mod 7 \=0, secondJoueur(Joueur, Autre), nth1(P,L,Autre), P2 is P+8, nth1(P2,L,Joueur),P3 is P2+8, nth1(P3,L,Joueur), P4 is P3+8, nth1(P4,L,Joueur).
% Vérification pour évaluation (diagonale descendante)
testD(L, Joueur):- between(1,21,P), P mod 7 < 5, P mod 7 \=0, secondJoueur(Joueur, Autre), nth1(P,L,Joueur), P2 is P+8, nth1(P2,L,Autre),P3 is P2+8, nth1(P3,L,Joueur), P4 is P3+8, nth1(P4,L,Joueur).
% Vérification pour évaluation (diagonale descendante)
testD(L, Joueur):- between(1,21,P), P mod 7 < 5, P mod 7 \=0, secondJoueur(Joueur, Autre), nth1(P,L,Joueur), P2 is P+8, nth1(P2,L,Joueur),P3 is P2+8, nth1(P3,L,Autre), P4 is P3+8, nth1(P4,L,Joueur).
% Vérification pour évaluation (diagonale descendante)
testD(L, Joueur):- between(1,21,P), P mod 7 < 5, P mod 7 \=0, secondJoueur(Joueur, Autre), nth1(P,L,Joueur), P2 is P+8, nth1(P2,L,Joueur),P3 is P2+8, nth1(P3,L,Joueur), P4 is P3+8, nth1(P4,L,Autre).

% Vérification pour évaluation (diagonale montante)
testD(L, Joueur):- between(1,21,P), P mod 7 > 3, secondJoueur(Joueur, Autre), nth1(P,L,Autre), P2 is P+6, nth1(P2,L,Joueur),P3 is P2+6, nth1(P3,L,Joueur), P4 is P3+6, nth1(P4,L,Joueur).
% Vérification pour évaluation (diagonale montante)
testD(L, Joueur):- between(1,21,P), P mod 7 > 3, secondJoueur(Joueur, Autre), nth1(P,L,Joueur), P2 is P+6, nth1(P2,L,Autre),P3 is P2+6, nth1(P3,L,Joueur), P4 is P3+6, nth1(P4,L,Joueur).
% Vérification pour évaluation (diagonale montante)
testD(L, Joueur):- between(1,21,P), P mod 7 > 3, secondJoueur(Joueur, Autre), nth1(P,L,Joueur), P2 is P+6, nth1(P2,L,Joueur),P3 is P2+6, nth1(P3,L,Autre), P4 is P3+6, nth1(P4,L,Joueur).
% Vérification pour évaluation (diagonale montante)
testD(L, Joueur):- between(1,21,P), P mod 7 > 3, secondJoueur(Joueur, Autre), nth1(P,L,Joueur), P2 is P+6, nth1(P2,L,Joueur),P3 is P2+6, nth1(P3,L,Joueur), P4 is P3+6, nth1(P4,L,Autre).

% Vérification pour évaluation Y Y Y 0 (ligne)
testOpo(L, Joueur):-  between(1,42,P), P mod 7 < 5, Z is P mod 7, Z \=0, nth1(P,L,Joueur), P2 is P+1, nth1(P2,L,Joueur),P3 is P2+1, nth1(P3,L,Joueur), P4 is P3+1, nth1(P4,L,0).
% Vérification pour évaluation Y Y Y 0 Y (ligne)
testOpo(L, Joueur):-  between(1,42,P), P mod 7 < 5, Z is P mod 7, Z \=0, nth1(P,L,Joueur), P2 is P+1, nth1(P2,L,Joueur),P3 is P2+1, nth1(P3,L,0), P4 is P3+1, nth1(P4,L,Joueur).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Connaître la terminaison du jeu
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
testGagnant(S) :- test(S, 1).
testGagnant(S) :- test(S, 2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Joueur humain vs humain
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
joueur(L,Joueur,C):- not(between(1,7,C)), write('\nErreur, recommencez ... \n'), choix(1, L, Joueur).
joueur(L,Joueur,C):- nth1(C, L, E), E\=0, write('\nErreur, colonne déjà pleine, recommencez ... \n'), choix(1, L, Joueur).
joueur(L,Joueur,C):- placer(L,C,Joueur,S), afficherPlateau(S), not(testGagnant(S)), secondJoueur(Joueur, Autre), choix(1, S, Autre) .
joueur(_,Joueur,_):- write('Joueur '),write(Joueur), write(' a gagné!').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Humain vs IA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
joueurIA(L,Joueur,C):- not(between(1,7,C)), write('\nErreur, recommencez ... \n'), choix(2, L, Joueur).
joueurIA(L,Joueur,C):- nth1(C, L, E), E\=0, write('\nErreur, colonne déjà pleine, recommencez ... \n'), choix(2, L, Joueur).
joueurIA(L,Joueur,C):- placer(L,C,Joueur,S), not(testGagnant(S)), afficherPlateau(S), minmax(S,Joueur,1,BestPosition), afficherPlateau(BestPosition),choix(2, BestPosition, Joueur) .
joueurIA(L,Joueur,_):- testGagnant(L),write('Joueur '),write(Joueur), write(' a gagné!').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Place un jeton dans le jeu
% C : colonne
% S : Position du jeu après insertion
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
placer(L,C,_,_):- between(1,7,C), nth1(C,L,E), E\=0, fail.
placer(L,C,Joueur,S):- nth1(C,L,E), E\=0, not(between(1,7,C)), C3 is C-7, replace(C3,Joueur,L,S).
placer(L,C,Joueur,S):- between(43, 50, C), C3 is C-7, replace(C3,Joueur,L,S).
placer(L,C,Joueur,S):- nth1(C,L,0), C2 is C+7, placer(L,C2,Joueur,S).

replace(1,X,[_|L],[X|L]).
replace(Idx,X,[Y|L],[Y|S]):- Idx2 is Idx-1, replace(Idx2,X,L,S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Afficher le jeu
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
afficherPlateau(L) :- write('\n'), ecrire(L,1), write('\n').

ecrire([],_):-!.
ecrire([X|L],7):- write(X), write('\n'), ecrire(L,1).
ecrire([X|L],C):- C\=7, write(X), write(' '), C2 is C+1, ecrire(L,C2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implémentation IA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Evaluation de la position
% Regarde d'abord si IA a une chance de gagner !
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
evaluation(L, Valeur):- test(L,1), Valeur is 1000.
evaluation(L, Valeur):- test(L,2), Valeur is -1000.
evaluation(L, Valeur):- testOpo(L,2), Valeur is -700.
evaluation(L, Valeur):- testD(L,1), Valeur is -500.
evaluation(L, Valeur):- testD(L,2), Valeur is 500.
evaluation(_, Valeur):- Valeur is 0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Récupérer tous les successeurs à partir d'une position
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
placer2(_,_,0,_).

placer2(L,Joueur,C,Liste):-
	C \= 1,
	nth1(C,L,E),
	E\=0,
	C1 is C-1, 
	placer2(L,Joueur,C1,Liste).

placer2(L,_,1,_):-
	nth1(1,L,E), E \= 0.
	
placer2(L,Joueur,1,[T|[]]):-
	nth1(1,L,E), E == 0,
	placer(L,1,Joueur,T).

placer2(L,Joueur,C,[T|Q]):-
	C\=1,
	placer(L,C,Joueur,T),
	C1 is C-1, 
	placer2(L,Joueur,C1,Q).
	 
succ(P, Joueur, L) :-
	secondJoueur(Joueur, AutreJoueur), findall(M,placer2(P,AutreJoueur,7,M),Tmp), nth0(0,Tmp,L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Garder le min ou le max
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
max(X,X,E,_,E,X, C,_,C).
max(X,Y,E,_,E,X, C,_,C) :- X>Y.
max(X,Y,_,E,E,Y, _,C,C) :- Y>X.
max(X,_,E,_,E,X, C,_,C).

min(X,X,E,_,E,X, C,_,C).
min(X,Y,_,E,E,Y, _,C,C) :- Y<X.
min(X,Y,E,_,E,X, C,_,C) :- X<Y.
min(X,_,E,_,E,X, C,_,C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Décrémente la profondeur
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dec(P,P2) :- P2 is P-1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% si la position possède des possibilités de jeu
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
vide([]).
testRacine(P,E,J) :- P =:= 0 ; succ(E,J,X), vide(X).

minmax(Position,Joueur,Profondeur,BestPosition) :-
    succ(Position,Joueur,X), length(X, Longueur), Lon is Longueur +1,
    secondJoueur(Joueur,AutreJoueur),
    m(X,AutreJoueur,Profondeur,_,_,_,_, Lon, BestCoup),
	reverse(X,ListeTemp),
	nth1(BestCoup, ListeTemp, BestPosition).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Fin de parcours
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
m([],_,_,_,_,_,_,_,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A partir de la position, choisir le max pour l'IA en prenant compte des coups de l'adversaire (en le minimisant)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
m([E|L],1,P,Meilleur,V,_,_, Best,BestCoup) :- 
	succ(E,1,X), length(X, Longueur), dec(P,P2),
    m(X,2,P2,MeilleurX,ValX, Longueur,_, Best, BestCoup1),	
	write(' '),
    m(L,1,P,MeilleurL,ValM, Longueur,_, BestCoup1, BestCoup2),
    max(ValX,ValM,MeilleurX,MeilleurL,Meilleur,V, BestCoup1, BestCoup2, BestCoup).

m([E|L],2,P,Meilleur,V, Id, Coup,Best,BestCoup) :- 
	BestCoup is Best - 1,
	testRacine(P,E,2),
    evaluation(E, ValE),!,
	Id1 is Id -1,
    m(L,2,P,MeilleurL,ValL,Id1,Coup1,Best,BestCoup),
    min(ValE,ValL,E,MeilleurL,Meilleur,V,Id1,Coup1,Coup).