parent(jack, monica).
female(rachel).
male(ross).
female(phoebe).
female(monica).
male(chandler).
male(joey).
male(gunther).
female(emma).
male(jack).
parent(jack, ross).
gparent(X,Y) :- parent(X,Z), parent(Z,Y).
sibling(ross, monica).
parent(ross, ben).
parent(ross, emma).
parent(rachel, emma).
