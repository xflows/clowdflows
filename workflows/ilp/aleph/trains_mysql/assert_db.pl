:- use_module(library(myddas)).
:- db_open(mysql, 'localhost'/'test', 'root', '').
:- db_import(cars, tmp_cars).
:- db_import(trains, tmp_trains).

:- repeat, tmp_trains(A,B), (trains(A,B), !, fail ; assertz(trains(A,B)), fail).
