/* 0.651908 0.000000 */	daughter(B,A); son(B,A) :- parent(A,B).
/* 0.442777 0.000000 */	daughter(B,A) :- parent(A,B), female(B).
/* 0.433374 0.000000 */	daughter(B,A); male(B) :- parent(A,B).
/* 0.411354 0.000000 */	parent(A,B) :- daughter(B,A).
/* 0.394479 0.000000 */	son(B,A) :- parent(A,B), male(B).
/* 0.362536 0.017544 */	daughter(B,A); male(A) :- parent(A,B).
/* 0.362520 0.000000 */	parent(A,B) :- son(B,A).
/* 0.360299 0.017544 */	daughter(B,A) :- parent(A,B), female(A).
/* 0.349779 0.087719 */	daughter(B,A) :- parent(A,B).
/* 0.339816 0.000000 */	son(B,A); female(B) :- parent(A,B).
