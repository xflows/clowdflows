/* 0.000000 0.000000 train(A) */	class(A,eastbound); class(A,westbound).
/* 0.000000 0.000000 train(A) */	 :- class(A,eastbound), class(A,westbound).
/* 0.000000 0.050000 train(A) */	 :- car(A,B), shape(B,ellipse).
/* 0.000000 0.050000 train(A) */	 :- car(A,B), roof(B,arc).
/* 0.000000 0.100000 train(A) */	 :- car(A,B), roof(B,peaked).
/* 0.000000 0.100000 train(A) */	 :- car(A,B), shape(B,hexa).
/* 0.000000 0.200000 train(A) */	 :- car(A,B), roof(B,jagged).
/* 0.000000 0.200000 train(A) */	 :- car(A,B), not wall(B,single).
/* 0.000000 0.200000 train(A) */	 :- car(A,B), wall(B,double).
/* 0.000000 0.300000 train(A) */	 :- car(A,B), shape(B,bucket).
/* 0.000000 0.450000 train(A) */	 :- car(A,B), shape(B,u_shaped).
/* 0.000000 0.500000 train(A) */	 :- class(A,eastbound).
/* 0.000000 0.500000 train(A) */	class(A,westbound).
/* 0.000000 0.500000 train(A) */	 :- class(A,westbound).
/* 0.000000 0.500000 train(A) */	class(A,eastbound).
/* 0.000000 0.500000 train(A) */	 :- car(A,B), wheels(B,3).
/* 0.000000 0.500000 train(A) */	 :- car(A,B), not wheels(B,2).
/* 0.000000 0.550000 train(A) */	 :- car(A,B), roof(B,flat).
/* 0.000000 0.750000 train(A) */	 :- car(A,B), not shape(B,rectangle).
/* 0.000000 0.800000 train(A) */	 :- car(A,B), not roof(B,none).
