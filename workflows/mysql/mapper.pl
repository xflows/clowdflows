#!/usr/local/bin/yap -L --
#.

eval_features(MainPred):-
    ExamplePred =.. [MainPred, A, B],
    all(example(A, B), (ExamplePred, testExampleIDs(TestExampleIDs), memberchk(B, TestExampleIDs)), Examples),
    featureIDs(Features),
    map_examples(Examples, Features),
    !.

map_examples([Example], Features):-
    write_example(Example, Features), 
    write_class(Example),
    nl,
    !.

map_examples([Example|ExampleTail], Features):-
    write_example(Example, Features), 
    write_class(Example),
    nl,
    map_examples(ExampleTail, Features),
    !.

write_example(Example, []) :-
    !.

write_example(Example, [Feature]) :-
    write_example_feature(Example, Feature),
    !.

write_example(Example, [Feature|FeatureTail]):-
    write_example_feature(Example, Feature), !,
    write_example(Example, FeatureTail),
    !.

write_example_feature(example(Class, Id), Feature):-
    ( f(Feature, Id), !, write('+')
      ;
      write('-')
    ),
    !,
    write(' '),
    !.

write_class(example(Class, _)):-
    write(Class).

main([KBFile, MainPred]):- 
    consult(KBFile),
    eval_features(MainPred),
    !.

main(_):-
    write('Call with: <kbfile> <mainpred>'), nl.

:- use_module(library(lists)).
:- unix(argv(AllArgs)), main(AllArgs).
