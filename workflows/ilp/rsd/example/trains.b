:-modeh(1,train(+train)).   % main key
:-modeb(1,hasCar(+train,-car)).
:-modeb(1,carshape(+car,-car_shape)).
:-modeb(1,instantiate(+car_shape)). % a constant value should be considered for this var when generating features
:-modeb(1,carlength(+car,-car_length)).
:-modeb(1,instantiate(+car_length)). % a constant value should be considered for this var when generating features
:-modeb(1,has_sides(+car,-sides)).
:-modeb(1,instantiate(+sides)). % a constant value should be considered for this var when generating features
:-modeb(1,has_roof(+car,-roof)).
:-modeb(1,instantiate(+roof)). % a constant value should be considered for this var when generating features
:-modeb(1,has_wheels(+car,-wheels)).
:-modeb(1,instantiate(+wheels)). % a constant value should be considered for this var when generating features
:-modeb(1,has_load(+car,-load)).
:-modeb(1,loadshape(+load,-load_shape)).
:-modeb(1,instantiate(+load_shape)).  % a constant value should be considered for this var when generating features
:-modeb(1,loadnum(+load,-load_num)).
:-modeb(1,instantiate(+load_num)).  % a constant value should be considered for this var when generating features

%:-modeb(1,notSame(+car,+car)).

% settings considered by "featurize.pl":
% ... none - all default

% settings considered by "process.pl"
% ... none - all default

% settings considered by "rules.pl"
:-set(eval_threshold,0.01).
:-set(sig_threshold,0).  % chi^2 value threshold does not make sense with 20 train instances -> therefore 0

hasCar(Train,Car):-
    my_member(Car,Train).

carshape(c(_,S,_,_,_,_,_),S).

carlength(c(_,_,LE,_,_,_,_),LE).

has_sides(c(_,_,_,SD,_,_,_),SD).

has_roof(c(_,_,_,_,R,_,_),R).

has_wheels(c(_,_,_,_,_,W,_),W).

has_load(c(_,_,_,_,_,_,L),L).

loadshape(l(LS,_),LS).

loadnum(l(_,LN),LN).
