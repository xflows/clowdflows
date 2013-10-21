% [aleph], read_all(train), induce_features, [show_features], save_features('ficrji'), save_dataset('dataset').

:- set(portray_examples, true).

save_features(File):-
    open(File, write, Stream),
    set_output(Stream),
    show(features),
    close(Stream).

save_dataset(File):-
    open(File, write, Stream),
    set_output(Stream),
    show(train_pos),
    show(train_neg),
    close(Stream).

aleph_portray(train_pos):-
    setting(train_pos,File),
    show_features(File,positive).

aleph_portray(train_neg):-
    setting(train_neg,File),
    show_features(File,negative).

show_features(File,Class):-
    open(File,read,Stream),
    repeat,
    read(Stream,Example),
    (Example = end_of_file -> close(Stream);
        Example =.. [_|[Example_id]],
        write('example('),
        write(Example_id),
        write_features(Example, Class),
        fail).

feature_holds(Feature_id, Example):-
    feature(Feature_id, (Example:- Body)),
    Body.

write_features(Example,_):-
    write(','),
    (all(Feature_id, feature_holds(Feature_id, Example), Features)
        -> write(Features)
        ;  write('[]')
    ).

write_features(_,Class):-
    write(','), write(Class), write(').'), nl.
