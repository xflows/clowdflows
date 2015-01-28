% The code below has been written by 
% Filip Zelezny (CVUT Prague, zelezny@fel.cvut.cz)

% Date of release: May 23, 2004


:-dynamic f/2.
:-dynamic def_head/5.
:-dynamic def_property/3.
:-dynamic def_structural/5.
:-dynamic count/1.
:-dynamic setting/2.
:-dynamic var_name/2.
:-dynamic dec_name/1.
:-dynamic last_noinst_feature/1.
:-dynamic last_example/1.


%%%%%%%%%%%%% Unified Control %%%%%%%%%%%%%%%%%%%%%%%%

r(Name):-
    cat([Name,'.b'],BckFile,_),
    cat([Name,'_frs_noinst.pl'],NoinstFrsFile,_),
    cat([Name,'_frs.smb'],FrsSmbFile,_),
    (files_ok([BckFile,NoinstFrsFile,FrsSmbFile]) ->
        fail
        ;
        !,
        fail
    ).
    
files_ok([]).
files_ok([File|Files]):-
    exists(File),
    !,
    files_ok(Files).
files_ok([File|_]):-
    nl,
    write('File '),
    write(File),
    write(' is missing.'),
    fail.

r(Name):-
    pos_neg_files_exist(Name),
    class_file_exists(Name),
    !,
    nl,
    write('Both pos-neg (.f, .n) and class (.pl) data exist. Don''t know what to read. Use read_posneg/1 (rpn/1) or read_with_classes/1 (rwc/1) to specify.').
r(Name):-
    class_file_exists(Name),
    rwc(Name),
    good_start,
    !.
r(Name):-
    pos_neg_files_exist(Name),
    rpn(Name),
    good_start,
    !.
r(_):-
    nl,
    write('Example file(s) not found.'),
    nl.
    
pos_neg_files_exist(Name):-
    cat([Name,'.f'],PosFileName,_),
    cat([Name,'.n'],NegFileName,_),
    exists(PosFileName),
    exists(NegFileName).

class_file_exists(Name):-
    cat([Name,'.pl'],ClassFileName,_),
    exists(ClassFileName).

good_start:-
    nl,
    write('Congratulations! You succeeded loading all neccessary files.').

w:-
    ee.
    
w(Name):-
    retractall(file_name(_)),
    assert(file_name(Name)),  
    ee.
    
w(System,Name):-
    cif(System,Name).
    
s:-
    ea.
   
show:-
    ea.
     
%%%%%%%%%%%%%% Expand features with instantiations to constants %%%%%%%%

expand_export:-
    % clean(feature_table),
    file_name(FileName),
    cat([FileName,'_frs.pl'],FileName1,_),
    fcreate(output,FileName1,1),
    get_time(Start),
    output(output),
    last_noinst_feature(LF),
    init_meter(LF),
    expand_all,
    output(0),
    get_time(Stop),
    fclose(output),
    Time is Stop-Start,
    nl,
    write(Time),
    write(' seconds taken to expand features.'),
    try_to_be_funny(Time),
    nl.
    
ee:-
    expand_export.

expand_all:-
    reset_count,
    eraseall(feature_table),
    fail.
    
expand_all:-
    smb_feat(FeatNum,Head,BodyList),
    progress,
    expand_with_const(FeatNum,Head,BodyList),
    fail.
expand_all.

ea:-
    expand_all.

expand_with_const(FeatNum,Head,BodyList):-
    find_instances(FeatNum,Instantiations),
    expand_all(FeatNum,Head,BodyList,Instantiations),
    !.

expand_all(_,_,_,[]).
expand_all(FeatNum,Head,BodyList,[(Instances,ExNums,NewFeatNum,Negation)|OtherIns]):-
    write_with_const(FeatNum,Head,BodyList,Instances,NewFeatNum,Negation),
    expand_all(FeatNum,Head,BodyList,OtherIns).

min_cov(MinCov):-
    setting(min_coverage,MinCov),
    !.
min_cov(1).

write_with_const(FeatNum,Head,BodyList,Instances,C,Negation):-
    all_subs(Instances,BodyList,SubsBodyList,Substitutions,0),
    write_with_vars(Head,SubsBodyList,Substitutions,C,Negation).

all_subs([],Body,Body,[],_).

all_subs([Inst|Rest],Body1,Body3,[Sub|Subs],Offset):-
    subs(Inst,Body1,Body2,Sub,Offset),
    Offset1 is Offset + 1,
    all_subs(Rest,Body2,Body3,Subs,Offset1).

subs((Place,Const),Body1,Body2,[Var,Const],Offset):-
    Place1 is Place - Offset,
    remove_ins_lit(Place1,Body1,Body2,instantiate(Var)).

remove_ins_lit(1,[Lit|Rest],Rest,Lit).
remove_ins_lit(N,[Lit|Rest],[Lit|RmRest],RmLit):-
    N1 is N-1,
    remove_ins_lit(N1,Rest,RmRest,RmLit).


%%%%%%%% Find instantiations from data %%%%%%%%%%%%%%%%

find_instances(FeatNum,Instances):-
    clean(inst),
    find_instances(FeatNum),
    min_cov(MinCov),
    last_example(LastEx),
    findall((Inst,ExNums,NewFeatNum,Negation),
                (
                   recorded(inst,(Occ,Inst,PosExNums),_),
                   (  
                        (                   % for a non-negated feature
                            (setting(negation,later) ->
                                (Occ >= MinCov ; LastEx - Occ >= MinCov)
                                ;
                                Occ >= MinCov 
                             ),
                        ExNums = PosExNums,
                        Negation = pos
                        )
                        ;
                        (                       % for a negated feature
                            setting(negation,now),
                            LastEx - Occ >= MinCov,
                            findall(ExNum,example(ExNum,_,_,_),AllEx),
                            my_del_list(PosExNums,AllEx,ExNums),
                            Negation = neg
                        )
                    ),
             % Below: (a) don't record feature if another with same coverage
             % already in and filtering on. Don't need to check set equivalence as
             % ExNums are sorted. THIS IS IRRESPECTIVE OF CLASSES. (b) don't record 
             % feature with complete coverage on examples if filtering on. THIS IS 
             % IRRESPECTIVE OF CLASSES.
                    (setting(filtering,false) ->
                        true
                        ;
                        not recorded(feature_table,(_,ExNums),_), % (a)
                        length(ExNums,LExNums),
                        not LExNums = LastEx                      % (b)
                    ),
                   get_count(NewFeatNum),
                   recordz(feature_table,(NewFeatNum,ExNums),_)
                ),
                Instances).

fi(FeatNum,Instances):-
    find_instances(FeatNum,Instances).

find_instances(FeatNum):-
    clause(f(FeatNum,Key),Body),
    example(ExNum,Class,Ex,_),
    Ex =.. [_,Key],
    prove_body(Body),
    add_instances(ExNum,Class),
    fail.
find_instances(_).
        
add_instances(ExNo,ExClass):-
        findall(I,recorded(curr_inst,I,_),Instances),
        (recorded(inst,(Occs,Instances,ExNos),DBRef) ->
            (my_member(ExNo,ExNos) ->
                true
                ;
                Occs1 is Occs+1,
                erase(DBRef),
                recorda(inst,(Occs1,Instances,[ExNo|ExNos]),_)
            )
            ;
            recorda(inst,(1,Instances,[ExNo]),_)
        ),
        !.

prove_body(B):-
    clean(curr_inst),
    prove_body(1,B).

clean(Key):-
    recorded(Key,I,DBRef),
    erase(DBRef),
    fail.
clean(Key).

prove_lit(Place,instantiate(Var)):- 
    (recorded(curr_inst,(Place,_),DBRef) ->
              erase(DBRef)
              ;
              true
    ),
        recordz(curr_inst,(Place,Var),_),
    !.

prove_lit(_,B):-
    B.

prove_body(Place,(B1,B2)):-
    !,
    prove_lit(Place,B1),
    Place1 is Place + 1,
    prove_body(Place1,B2).

prove_body(Place,B):-
    prove_lit(Place,B).

%%%%%%%%%%% Test/Train and XValidation Example Splitting %%%%%%%%%%%%%%

% stratified cross-validation & train/test splits

split:-
        splitting(S),
        !,
        setof(C,X^Y^Z^DbRef^recorded(example,(X,C,Y,Z),DbRef),Classes),
        (S > 1 ->
            split_classes(Classes,S)
            ;
            split_classes(Classes,2)
        ).
split:-
        recorded(example,(X,C,Y,Fold),DbRef),
        not Fold = train,
        erase(DbRef),
        recordz(example,(X,C,Y,train),_),
        fail.
split.

split_classes(Classes,Folds):-
        my_member(Class,Classes),
        split_class(Class,Folds),
        fail.
split_classes(_,_).
        
split_class(Class,Folds):-
        findall(I,recorded(example,(I,Class,_,_),_),Inst),
        length(Inst,NumInst),
        Coef is Folds/NumInst,
        reset_count,
        assign_folds(Inst,Coef),
        !.
        
assign_folds(Inst,Coef):-
        my_member(I,Inst),
        get_count(N),
        assign_fold_to_instance(N,I,Coef),
        fail.
assign_folds(_,_).

assign_fold_to_instance(N,I,Coef):-
        splitting(Ratio),
        Ratio < 1,
        !,
        recorded(example,(I,Class,Ex,_),DbRef),
        erase(DbRef),
        (N * Coef =< 2 * (1 - Ratio) ->
            recordz(example,(I,Class,Ex,train),_)
            ;
            recordz(example,(I,Class,Ex,test),_)
        ),
        !.
        
assign_fold_to_instance(N,I,Coef):-
        recorded(example,(I,Class,Ex,_),DbRef),
        erase(DbRef),
        Fold1 is integer(round(N * Coef + 0.5)),
        splitting(MaxFold),
        (MaxFold < Fold1 ->     % Makes sure that Fold number does not overflow
            Fold = MaxFold      % This could happen for small MaxFold, when
            ;                   % the real number Coef underflows.
            Fold = Fold1
        ),
        recordz(example,(I,Class,Ex,Fold),_),
        !.

%%%%%%%%%%%%%%% Write Attribute Table %%%%%%%%%%%%

write_attribute_table:-
        get_time(Start),
        write_attribute_table1,
        get_time(Stop),
        Time is Stop-Start,
        output(CurrOut),
        output(0),
        nl,
        write(Time),
        write(' seconds taken write attribute table.'),
        try_to_be_funny(Time),
        nl,
        output(CurrOut).

wat:-
        write_attribute_table.

write_attribute_table1:-
        last_example(LastEx),
        init_meter(LastEx),
        fail.
        
write_attribute_table1:-
        example(ExNum,ExClass,_,_),
        output(output),
        flip_output,     % obsolete
        progress,
        write_columns(ExNum),
        write_class(ExClass),
        terminator(Terminator),
        write(Terminator),
        nl,
        fail.
write_attribute_table1.

flip_output:-           % obsolete
        splitting(Split),
        A is random,
        A < Split,
        !,
        output(test).
flip_output.

write_columns(ExNum):-
        sign(true,TrueSign),
        sign(false,FalseSign),
        separator(Separator),
        recorded(feature_table,(FeatNum,ExNums),_),
        (my_member(ExNum,ExNums) ->
               write(TrueSign)
               ;
               write(FalseSign)
        ),
        write(Separator),
        fail.
write_columns(_).

write_class(Class):-
        setting(posneg,true),
        !,
        (Class = pos ->
                sign(true,Sign)
                ;
                sign(false,Sign)
    ),
        write(Sign).

write_class(Class):-
        write(Class).


%%%%%%%%%%%%%% Writing coverage file (used by RSD rule inducer) %%%%%%%%%%%

write_coverage_file(FileName):-
        get_time(Start),
        write_coverage_file1(FileName),
        get_time(Stop),
        Time is Stop-Start,
        %output(0),
        nl,
        write(Time),
        write(' seconds taken to write coverage file(s).'),
        try_to_be_funny(Time),
        nl.

write_coverage_file1(FileName):-
        split,              % do xval or train/test splitting if requested
        setof(C,X^Y^Z^DbRef^recorded(example,(X,C,Y,Z),DbRef),Classes),
        length(Classes,NumClasses),
        setof(Z,C^X^Y^DbRef^recorded(example,(X,C,Y,Z),DbRef),Folds), 
        (my_member(test,Folds) ->
            NumFolds is 1
            ;
            length(Folds,NumFolds)
        ),
        findall(FNo,recorded(feature_table,(FNo,_),_),Feats),
        length(Feats,NumFeats),
        Num is NumFeats * NumClasses * NumFolds, 
        init_meter(Num),
        (my_member(train,Folds) ->
            write_test_train(FileName,Folds,Classes)
            ;
            write_folds(FileName,Folds,Classes)
        ).

write_test_train(FileName,[train],Classes):-
    !, 
    cat([FileName,'.cov'],TrainName,_),       
    fcreate(train_file,TrainName,1),
    write_train_test(Classes,none),
    fclose(train_file).

write_test_train(FileName,_,Classes):-
    cat([FileName,'.cov'],TrainName,_),       
    cat([FileName,'_test.cov'],TestName,_),
    fcreate(train_file,TrainName,1),
    fcreate(test_file,TestName,1),
    write_train_test(Classes,test),
    fclose(train_file),
    fclose(test_file).

write_folds(FileName,Folds,Classes):-
    my_member(Fold,Folds),
    write_one_fold(FileName,Fold,Classes),
    fail.
write_folds(_,_,_).

write_one_fold(FileName,Fold,Classes):-
    number_atom(Fold,SFold),
    cat([FileName,SFold],NameNum,_),       
    cat([NameNum,'.cov'],TrainName,_),       
    cat([NameNum,'_test.cov'],TestName,_),
    fcreate(train_file,TrainName,1),
    fcreate(test_file,TestName,1),
    write_train_test(Classes,Fold),
    fclose(train_file),
    fclose(test_file),
    !.
       
write_train_test(Classes,TestFold):-
        my_member(Class,Classes),
        recorded(feature_table,(FeatNum,Inst),_),
        get_train_instances(Inst,Class,TestFold,TrainInst),
        output(train_file),
        write_cov(Class,FeatNum,TrainInst),
        progress,
        not TestFold = none,      % either fails here and backtracks for another class
        get_test_instances(Inst,Class,TestFold,TestInst),
        output(test_file),
        write_cov(Class,FeatNum,TestInst),
        fail.                     % or fails here and backtracks for another class
write_train_test(_,_):-
        !.

get_train_instances(Inst,Class,TestFold,TrainInst):-
        findall(I,
                (
                my_member(I,Inst),
                recorded(example,(I,Class,_,Fold),_),
                not Fold = TestFold
                ),
                TrainInst),
        !.
                
get_test_instances(Inst,Class,TestFold,TrainInst):-
        findall(I,
                (
                my_member(I,Inst),
                recorded(example,(I,Class,_,TestFold),_)
                ),
                TrainInst),
        !.

write_cov(_,_,[]):-
        !.
write_cov(Class,FeatNum,Inst):-
        write('cov('),
        write(Class),
        write(','),
        write(FeatNum),
        write(',['),
        write_inst(Inst),
        write(']).'),
        nl,
        !.
        
write_inst([H]):-
        write(H),
        !.
write_inst([H|B]):-
        write(H),
        write(','),
        write_inst(B).


% ************ special characters **********


sign(true,Sign):-
        setting(true_sign,Sign), 
        !.
sign(false,Sign):-
        setting(false_sign,Sign), 
        !.
sign(true,+).
sign(false,-).

separator(X):-
    setting(separator,X),
    !.
separator(' ').

terminator(X):-
    setting(terminator,X),
    !.
terminator('').
       
%%%%%%%%%%%%%% Create Input Files %%%%%%%%%

%% for RSD

cif(rsd,FileName):- 
    write_coverage_file(FileName).
    
%% for Weka

cif(weka,FileName):-
    set(separator,','),
    set(terminator,''),
    cat([FileName,'.arff'],Name,_),
    fcreate(output,Name,1),
    output(output),
    write('@relation '),
    write(FileName),
    nl,
    nl,
    write_weka_attributes,
    nl,
    write('@data'),
    nl,
    write_weka_examples,
    fclose(output).
 
write_weka_attributes:-
    write_weka_feature_decs,
     write('@attribute class {'),
     setof(C,X^Y^Z^example(X,C,Y,Z),Classes),
     write_weka_classes(Classes), 
     write('}'),
     nl.

write_weka_classes(_):-
        setting(posneg,true),
        !,
        sign(true,TrueSign),
        sign(false,FalseSign),
        write(FalseSign),
        write(', '),
        write(TrueSign).

write_weka_classes(Classes):-
        my_member(Class,Classes),
        write(Class),
        write(', '),
        fail.
write_weka_classes(Classes).

write_weka_feature_decs:-
       recorded(feature_table,(FeatNum,_),_),
       write('@attribute f'),
       write(FeatNum),
       write(' '),
       write('{'),
       sign(true,TrueSign),
       write(TrueSign),
       write(', '),
       sign(false,FalseSign),
       write(FalseSign),
       write('}'),
       nl,
       fail.   
write_weka_feature_decs. 

write_weka_examples:-
    write_attribute_table.  

%% for CN2

cif(cn2,FileName):-
    set(true_sign,'_1'),
    set(false_sign,'_0'),
    set(separator,' '),
    set(terminator,';'),
    create_cn2_header(FileName),
    create_cn2_examples(FileName).

create_cn2_header(FileName):-
    cat([FileName,'.att'],Name,_),
    fcreate(cn2head,Name,1),
    output(cn2head),
    write('**ATTRIBUTE FILE**'),
    nl,
    nl,
    write_cn2_feature_decs,
    write('class: '),
        setof(C,X^Y^Z^example(X,C,Y,Z),Classes),
        write_classes(Classes),
        write(';'),
        fclose(cn2head),
    nl.

write_classes(_):-
        setting(posneg,true),
        !,
        sign(true,TrueSign),
        sign(false,FalseSign),
        write(FalseSign),
        write(', '),
        write(TrueSign).

write_classes(Classes):-
        my_member(Class,Classes),
        write(Class),
        write(' '),
        fail.
write_classes(Classes).

write_cn2_feature_decs:-
        recorded(feature_table,(FeatNum,_),_),
    write(f),
    write(FeatNum),
    write(': _0 _1;'),
    nl,
    fail.   
write_cn2_feature_decs.
       
create_cn2_examples(FileName):-
        (splitting(_) ->            % obsolete but functional way of train/test splitting
            cat([FileName,'_test.exs'],TestName,_),
            fcreate(test,TestName,1),
            output(test),
            write_cn2_ex_header,
            cat([FileName,'_train.exs'],Name,_)
            ;
            cat([FileName,'.exs'],Name,_)
    ),
        fcreate(output,Name,1),
        output(output),
        write_cn2_ex_header,
    write_attribute_table,
        fclose(output),
        (splitting(_) ->
            fclose(test)
            ;
            true
        ).

write_cn2_ex_header:-
    write('**EXAMPLE FILE**'),
    nl,
        nl.

splitting(X):-
        setting(splitting,X),
        not my_member(X,[false,0,1]).

%%%%%%%%%%%%%%% Display Progress Meter %%%%%%%%%

progress:-
        output(CurOut),
        CurOut = 0,
        !.
progress:-
        progress(Point),
        display_endpoint(EndPoint),
        EndPoint =:= Point, 
        !,
        %display_point(DisplayPoint),
        %display_progress(DisplayPoint,100).  
        display_progress(100). 
progress:-
        progress(Point),
        display_endpoint(EndPoint),
        EndPoint > Point,
        Point1 is Point+1,
        retractall(progress(_)),
        assert(progress(Point1)),
        ProgressPoint is round(Point/EndPoint * 100),
        display_point(DisplayPoint), 
        (ProgressPoint >= DisplayPoint ->
            retractall(display_point(_)),
            display_step(DisplayStep),
            DisplayPoint1 is ProgressPoint + DisplayStep,
            display_progress(DisplayPoint1),
            assert(display_point(DisplayPoint1))
            ;
            true
        ),
        !.

%init_meter(_):- % keep this outcommented
%        output(CurOut),
%        CurOut = 0,
%        !.
init_meter(EndPoint):-  
        retractall(display_endpoint(_)),
        assert(display_endpoint(EndPoint)),
        retractall(progress(_)),
        assert(progress(1)),
        retractall(display_point(_)),
        assert(display_point(0)),
        eraseall(progress),
        recorda(progress,asterisks(0),_),
        fail.
init_meter(_):-
        setting(display_progress,numbers),
        !.                      
init_meter(_):-
        output(CurOut),
        output(0),
        very_funny_message,
        meter_width(Width),
        write('0%|'),
        Spaces is Width - 2,
        display_spaces(Spaces),
        write('|100%'),
        nl,
        write('  '),
        output(CurOut),
        !.

very_funny_message:-
        recorded(hasbeenfunny,enough,_),
        !.
very_funny_message:-    
        nl,
        %write('Warning: progress meter is approximate and takes no legal responsibility. :-)'),
        nl,
        nl,
        recorda(hasbeenfunny,enough,_).

display_spaces(N):-
        N<1.
display_spaces(N):-
        write('-'),
        N1 is N-1,
        display_spaces(N1).
                       
display_progress(ProgressPoint):-
        setting(display_progress,numbers),
        !,                    
        nl,
        round(ProgressPoint,Number), 
        write(Number),
        write('%').

display_progress(ProgressPoint):-
        meter_width(MeterWidth),
        recorded(progress,asterisks(AsterisksPrev),DbRef),
        AsterisksNew is round(ProgressPoint * MeterWidth / 100 - AsterisksPrev),
        erase(DbRef),
        TotAst is AsterisksPrev + AsterisksNew,
        (TotAst > MeterWidth ->
            Asterisks is MeterWidth-AsterisksPrev,
            recorda(progress,asterisks(MeterWidth),_)
            ;
            Asterisks is AsterisksNew,
            recorda(progress,asterisks(TotAst),_)
        ),
        output(CurrOut),
        output(0),
        asterisks(Asterisks),
        flush,
        output(CurrOut),
        !.           

asterisks(Number):-
        Number =< 0.        
asterisks(Number):-
        write('*'), 
        Number1 is Number - 1,
        asterisks(Number1).       

meter_width(Width):-
    setting(meter_width,Width),
    !.
meter_width(50).

%outcommented below: unneccessary complication?
%better keep it      
%           
%display_step(DisplayStep):-
%        display_endpoint(EndPoint),
%        EndPoint < 100,
%        !,
%        RDisplayStep is 100/EndPoint-1,
%        round(RDisplayStep,DisplayStep).
%display_step(DisplayStep):-
%        setting(display_step,DisplayStep),
%        !.
%display_step(DisplayStep):-
%        display_endpoint(EndPoint),
%        RDisplayStep is EndPoint/30,
%        round(RDisplayStep,DisplayStep),
%        !.
display_step(1).

%%%%%%%%%%%%%%% Read declarations and features %%%

read_dec_feat(Name):-
    retractall(file_name(_)),
    assert(file_name(Name)),
    cat([Name,'.b'],Filename,_),
    reconsult(Filename),
    retractall(dec_name(_)),
    assert(dec_name(Name)),
    cat([Name,'_frs_noinst.pl'],Filename1,_),
    reconsult(Filename1),
    cat([Name,'_frs.smb'],Filename2,_),
    fopen(input,Filename2,0),
    input(input),
    reset_count,
    read_smb_feat,
    output(0),
    fclose(input).

read_smb_feat:-
    repeat,
    get_count(N),
    read(Line),
    (Line = end_of_file ->
        retractall(last_noinst_feature(_)),
            N1 is N-1,
            assert(last_noinst_feature(N1))
        ;
        Line = (Head:-BodyList),
        assert(smb_feat(N,Head,BodyList)),
        fail
    ).


%%%%%%%%%%%%%%% Accept declarations %%%%%%%%%%%%%%
% This part is copied from featurize.pl and simplified

cleanup:-
    retractall(def_head(_,_,_,_,_)),
    retractall(def_property(_,_,_)),
    retractall(def_structural(_,_,_,_,_)),
    retractall(order(_,_)),
    retractall(setting(_,_)),
    retractall(example(_,_,_,_)),
    retractall(smb_feat(_,_,_)),
    retractall(display_endpoint(_)),
    reset_count.

set(Parameter,Value):-
    retractall(setting(Parameter,_)),
    assert(setting(Parameter,Value)).

unset(Parameter):-
    retractall(setting(Parameter,_)).

settings:-
    setting(Parameter,Value),
    nl,
    write(Parameter),
    write(' = '),
    write(Value),
    fail.
settings.

st:-
    settings.
    
modeh(Recall,Def):-
    Def =.. [Name|Args],
    length(Args,Arity),
    inout(Args,In,Out,InPlaces,1),
    assert(def_head(Recall,Name/Arity,In,Out,InPlaces)).

modeb(Recall,Def):-
    Def =.. [Name|Args],
    length(Args,Arity),
    get_count(C),
    inout(Args,In,Out,InPlaces,1),
    (Out = [] ->
        assert(def_property(Recall,Name/Arity,In)),
        Order = C
        ;
        assert(def_structural(Recall,Name/Arity,In,Out,InPlaces)),
        Order is C+10000        % Properties should appear before structures
    ),              % in given variable depth
    assert(order(Name/Arity,Order)).

inout([],[],[],[],_).
inout([+X|B],[X|In],Out,[Place|InPlaces],Place):-
    NextPlace is Place+1,
    inout(B,In,Out,InPlaces,NextPlace).
inout([-X|B],In,[X|Out],InPlaces,Place):-
    NextPlace is Place+1,
    inout(B,In,Out,InPlaces,NextPlace).

%%%%%%%%% Read and register examples %%%%%%%%%%%%%

read_posneg(Name):-
    cleanup,
    read_in(Name,'.f',pos),
    read_in(Name,'.n',neg),
    register_last,
    record_examples,  %  Puts example/4 facts into Yap internal database, example/4 should not be used (slow).
    read_dec_feat(Name),
    assert(setting(posneg,true)).      

rpn(Name):-
    read_posneg(Name).

read_with_classes(Name):-
    cleanup,
    read_in(Name,'.pl',all),
    register_last,
    record_examples,  %  Puts example/4 facts into Yap internal database, example/4 should not be used (slow).
    read_dec_feat(Name).

rwc(Name):-
    read_with_classes(Name).

register_last:-
        count(Last),
        retractall(last_example(_)),
        assert(last_example(Last)).
    
read_in(Name,Suffix,Class):-
    cat([Name,Suffix],Filename,_),
    fopen(input,Filename,0),
    input(input),
    collect(Class),
    count_down(Number),
    nl,
    write(Number),
    write(' examples read.'),   
    nl,
    %input(0),
    fclose(input).

collect(Class):-
    repeat,
    read(Example),
    get_count(C),
    (Example = end_of_file ->
        true
        ;
        register_example(C,Example,Class),
        fail
    ).

register_example(C,Example,all):-
    !,
    Example =.. [Name,Class|Args],
    ExNoClass =.. [Name|Args],
    assert(example(C,Class,ExNoClass,1)).

register_example(C,Example,Class):-
    assert(example(C,Class,Example,train)).

record_examples:-
        eraseall(example),
        fail.
record_examples:-        
        example(X,C,Y,Z),
        recordz(example,(X,C,Y,train),_),
        fail.
%record_examples:-  % this is now done before exporting tables
%        xval(Folds),
%        !,
%        setof(C,X^Y^Z^DbRef^recorded(example,(X,C,Y,Z),DbRef),Classes),
%        split(Classes,Folds).        
record_examples.

%%%%%%%%%%%%% Export Features %%%%%%%%%%%%%%%%

write_with_vars(Head,BodyList,Substitutions,C,Negation):-
    retractall(var_name(_,_)),
    %get_count(C),
    Head =.. [_|Args],
    (setting(head_name,Name) ->
        write(Name)
        ;
        write('f')
    ),
    (setting(feature_num,end_of_name) ->
        write(C),
        write('('),
        write_with_vars1(Args,Substitutions)
        ;
        write('('),
        write(C),
        write(','),
        write_with_vars1(Args,Substitutions)
    ),
    write(')'),
    write(':-'),
    (Negation = neg ->
        write('not(')
        ;
        true
    ),
    write_with_vars1(BodyList,Substitutions),
    (Negation = neg ->
        write(')')
        ;
        true
    ),
    write('.'),
    nl,
    !.

%%
%% Escape atoms that contain particular characters that would
%% cause syntax errors when loaded.
%%
%% Added 20.1.2015
%% anze.vavpetic@ijs.si
must_escape(Atom):-
    atom(Atom),
    not number(Atom).
    %atom_chars(Atom, Chars),
    %my_member(Char, Chars),
    %my_member(Char, ['[', ']', '(', ')', ' ', ',', '.', '-', '<', '>', '=']).

write_with_vars1([],_).

write_with_vars1([A|B],Substitutions):-
    my_member([A,Sub],Substitutions),
    !,
    (must_escape(Sub) ->
        writeq(Sub)
        ;
        write(Sub)
    ),
    !,
    (B = [_|_] ->
        write(',')
        ;
        true
    ),
    write_with_vars1(B,Substitutions).

write_with_vars1([A|B],Substitutions):-
    A =.. [Name|Arg],
    (type(Name) ->
        variabilize(Name,Arg,VName),
        write(VName)
        ;
        (Arg = [_|_] ->
            write(Name),
            write('('),
            write_with_vars1(Arg,Substitutions),
            write(')')
            ;
            true    
        ) 
    ),
    (B = [_|_] ->
        write(',')
        ;
        true
    ),
    write_with_vars1(B,Substitutions).

variabilize(Name,[Number,_,_],VName):-
    setting(format_vars,all_cap_type),
    !,
    atom_string(Name,SName),
    lwrupr(SName,SVar),
    number_string(Number,SNumber),
    cat([SVar,SNumber],VName,_).

variabilize(Name,[Number,_,_],VName):-
    setting(format_vars,first_cap_type),
    !,
    atom_chars(Name,[First|Rest]),
    string_chars(SFirst,[First]), 
    lwrupr(SFirst,SCFIrst), 
    string_chars(SRest,Rest),
    number_string(Number,SNumber),
    cat([SCFIrst,SRest,SNumber],VName,_).

variabilize(Name,[Number,_,_],VName):-
    var_name((Name,Number),VName),
    !.
variabilize(Name,[Number,_,_],VName):-
    var_chars(VName1),
    cat(VName1,VName,_),
    (var_name(_,VName) -> fail ; true),
    assert(var_name((Name,Number),VName)),
    !.

var_chars([Char]):-
    var_names(VarNames),
    my_member(Char,VarNames).

var_chars(Chars):-
    var_chars(Chars1),
    var_names(VarNames),
    my_member(Char,VarNames),
    my_append(Chars1,[Char],Chars).

var_names(['A','B','C','D','E','F','G','H','I',
    'J','K','L','M','N','O','P','Q','R','S',
    'T','U','V','W','X','Y','Z']).

type(A):-
    def_property(_,_,In),
    my_member(A,In).
type(A):-
    def_structural(_,_,In,Out,_),
    (my_member(A,In) ; my_member(A,Out)).
    
%%%%%%%%%%%%%%%%%%% feature number reporting %%%%%%%%%%%%%%%%%%%%%%%

test(Max):-
    set(reporting,on),
    test(0,Max).

test(Max,Max).
test(N,Max):- 
    set(min_coverage,N),
    s,
    report,
    N1 is N + 20,
    test(N1,Max).
    
report:-
    setting(reporting,on),
    !,
    setting(min_coverage,MC),
    get_count(C),
    FeatNum is C - 1,
    (exists('p-featnum.txt') ->
        open('p-featnum.txt','append',ReportFile1)
        ;
        open('p-featnum.txt','write',ReportFile1)
    ),
    set_output(ReportFile1),
    write(MC),
    write(' '),
    write(FeatNum),
    nl,
    flush_output,
    close(ReportFile1).
report.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%% GENERIC PREDICATES %%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reset_count:-
    retractall(count(_)),
    assert(count(0)),
        !.

get_count(C):-
    count(X),
    retractall(count(X)),
    C is X+1,
    assert(count(C)),
        !.

count_down(C):-
        count(X),
    retractall(count(X)),
    C is X-1,
    assert(count(C)),
        !.

my_subset([],_).
my_subset([A|B],X):-
    my_member(A,X),
    my_subset(B,X).

my_member(A,[A|_]).
my_member(A,[X|B]):-
    my_member(A,B).

my_del_list([],A,A).
my_del_list([A|B],C,DC):-
    my_del(A,C,D),
    my_del_list(B,D,DC).

my_del(_,[],[]).
my_del(A,[A|B],DB):-
    my_del(A,B,DB),
    !.
my_del(A,[X|B],[X|DB]):-
    my_del(A,B,DB).

my_append([],A,A).
my_append([A|B],C,[A|BC]):-
    my_append(B,C,BC).

try_to_be_funny(RunTime):-
    nl,
    (RunTime > 60 ->
        write('How was your coffee?')
        ;
        write('Hmmm, that was a quick one!')
    ).

%%%%%%% Yap wrapping %%%%%%%%%%%

flush:-
    flush_output.

round(X,Y):-
    Y is integer(X)+1.

del(FileName):-
        cat(['rm ',FileName],DelCommand,_),
        system(DelCommand).

get_time(Time):-
    Time is cputime.

ren(F1,F2):-
    rename(F1,F2).

reg(Direction,LpaIO,Stream):-
    retractall(ios(Direction,LpaIO,_)),
    assert(ios(Direction,LpaIO,Stream)).

:-   current_output(Output_Stream),
     reg(output,0,Output_Stream),
     current_input(Input_Stream),
     reg(input,0,Input_Stream).

cat([A],A,_):-
    !.

cat([A,B],C,_):-
    atom_chars(A,SA),
    atom_chars(B,SB),
    my_append(SA,SB,SAB),
    atom_chars(C,SAB),
    !.

fclose(Output):-
    ios(_,Output,Stream),
    flush_output,
    close(Stream).

fopen(Output,Name,_):-
    open(Name,'read',Stream),
    reg(input,Output,Stream).

fcreate(Output,Name,_):-
    open(Name,'write',Stream),
    reg(output,Output,Stream).

input(Input):-
    ground(Input),
    ios(input,Input,Stream),
    set_input(Stream),
    !.
   
input(Input):-
    current_input(Stream),
    ios(input,Input,Stream).

output(Output):-
    ground(Output),
    ios(output,Output,Stream),
    set_output(Stream),
    !.
   
output(Output):-
    current_output(Stream),
    ios(output,Output,Stream).
