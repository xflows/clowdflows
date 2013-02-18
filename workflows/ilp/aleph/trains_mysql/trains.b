:- use_module(library(myddas)).
:- db_open(mysql, 'localhost'/'test', 'root', '').
:- db_import(cars, cars).
:- db_import(trains, trains).

:- modeh(1, east(+trains)).
:- modeb(*, has_cars(+trains,-cars)).
:- modeb(*, cars_position(+cars,#position)).
:- modeb(*, cars_shape(+cars,#shape)).
:- modeb(*, cars_len(+cars,#len)).
:- modeb(*, cars_sides(+cars,#sides)).
:- modeb(*, cars_roof(+cars,#roof)).
:- modeb(*, cars_wheels(+cars,#wheels)).
:- modeb(*, cars_load_shape(+cars,#load_shape)).
:- modeb(*, cars_load_num(+cars,#load_num)).

:- determination(east/1, has_cars/2).
:- determination(east/1, cars_position/2).
:- determination(east/1, cars_shape/2).
:- determination(east/1, cars_len/2).
:- determination(east/1, cars_sides/2).
:- determination(east/1, cars_roof/2).
:- determination(east/1, cars_wheels/2).
:- determination(east/1, cars_load_shape/2).
:- determination(east/1, cars_load_num/2).

trains(Id) :-
    trains(Id,_).
cars(Id) :-
    cars(Id,_,_,_,_,_,_,_,_,_).
position(Position) :-
    cars(_,_,Position,_,_,_,_,_,_,_).
shape(Shape) :-
    cars(_,_,_,Shape,_,_,_,_,_,_).
len(Len) :-
    cars(_,_,_,_,Len,_,_,_,_,_).
sides(Sides) :-
    cars(_,_,_,_,_,Sides,_,_,_,_).
roof(Roof) :-
    cars(_,_,_,_,_,_,Roof,_,_,_).
wheels(Wheels) :-
    cars(_,_,_,_,_,_,_,Wheels,_,_).
load_shape(Load_shape) :-
    cars(_,_,_,_,_,_,_,_,Load_shape,_).
load_num(Load_num) :-
    cars(_,_,_,_,_,_,_,_,_,Load_num).
has_cars(Trains, Cars) :-
    trains(Trains,_),
    cars(Cars,Trains,_,_,_,_,_,_,_,_).
cars_position(Cars, Position) :-
    cars(Cars,_,Position,_,_,_,_,_,_,_).
cars_shape(Cars, Shape) :-
    cars(Cars,_,_,Shape,_,_,_,_,_,_).
cars_len(Cars, Len) :-
    cars(Cars,_,_,_,Len,_,_,_,_,_).
cars_sides(Cars, Sides) :-
    cars(Cars,_,_,_,_,Sides,_,_,_,_).
cars_roof(Cars, Roof) :-
    cars(Cars,_,_,_,_,_,Roof,_,_,_).
cars_wheels(Cars, Wheels) :-
    cars(Cars,_,_,_,_,_,_,Wheels,_,_).
cars_load_shape(Cars, Load_shape) :-
    cars(Cars,_,_,_,_,_,_,_,Load_shape,_).
cars_load_num(Cars, Load_num) :-
    cars(Cars,_,_,_,_,_,_,_,_,Load_num).