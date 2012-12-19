% The code below has been written by 
% Filip Zelezny (CVUT Prague, zelezny@fel.cvut.cz)

% Date of release: May 23, 2004

%%%%%%%%%%%%%%% Parameters %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

search_strategy(beam). % don't change; nothing else supported yet

beam_width(BeamWidth):-
    setting(beam_width,BeamWidth),
    !.
beam_width(3).

max_length(MaxLength):-
    setting(max_length,MaxLength),
    !.
max_length(3).

evalfn(EvalFn):-
    setting(evalfn,EvalFn),
    !.
evalfn(wracc).

acc_est(AccEst):-
    setting(acc_est,AccEst),
    !.
acc_est(freq).
    
weight_threshold(WeightThreshold):-  % 0 ~ 1 : fraction of initial total weight
    setting(weight_threshold,WeightThreshold),
    !.
weight_threshold(0.1). % 10% of instance weight volume can be left uncovered
                        % if search=cover, it is 10% of instances

sig_threshold(SigThreshold):-
    setting(sig_threshold,SigThreshold),
    !.
sig_threshold(0).  

eval_threshold(EvalThreshold):-
    setting(eval_threshold,NegEvalThreshold),
    !,
    EvalThreshold is 0 - NegEvalThreshold.  % Evals stored as negatives
eval_threshold(-0.0001).                     % for easier sorting

max_rules_for_class(MaxRulesForClass):-
    setting(max_rules_for_class,MaxRulesForClass),
    !.
max_rules_for_class(10).

gamma(Gamma):-
    setting(gamma,Gamma),
    !.
gamma(1).

cover_search:-
    setting(search,cover).

stop_on_sig:-                   % if yes, then rule generation for current class is stopped once minimum 
    setting(stop_on_sig,yes).   % significance is reached

stop_on_eval:-                   % if yes, then rule generation for current class is stopped once minimum 
    setting(stop_on_eval,yes).   % evaluation is reached
    
calc_weight(0,1):-
    cover_search,
    !.
calc_weight(_,0):-
    cover_search.
calc_weight(Cov_Num,Weight):-
    gamma(Gamma),
    Weight is 1 / (1 + Gamma * Cov_Num).

%%%%%%% reading inputs %%%%%%%%%%%%%%%%%%%%%%%%%

r(Name):-
    eraseall(filename),
    recorda(filename,Name,_),
    cat([Name,'.cov'],CovName,_),        
    consult(CovName),
    cat([Name,'.b'],DecName,_),     
    (exists(DecName) ->
        consult(DecName),
        nl,
        write(DecName),
        write(' file loaded, settings applied.'),
        nl
        ;
        nl,
        write(DecName),
        write(' file not found, no settings changed.'),
        nl
    ),
    record_coverage,
    retractall(cov(_,_,_)).

%%%%% unified control %%%%%%%%%%%%%%%%%%%%%%%%%%%

i:-
    induce.
    
s:-
    write_rules.
    
show:-
    write_rules.
    
w:-
    recorded(filename,Name,_),
    !,
    write_rules(Name).
w:-
    nl,
    write('Error: no data loaded.').
        
w(Name):-
    write_rules(Name).
    
rr(Name):-
    read_rules(Name).

rr:-
    recorded(filename,Name,_),
    !,
    read_rules(Name).
rr:-
    nl,
    write('Error: no data loaded.').
    
%%%%%% setting a setting :-) %%%%%%%%%%%%%%%%%%%%%

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

modeh(_,_). % void

modeb(_,_). % void
    
%%%%%%% Writing and Reading Rules %%%%%%%%%%%%%%%%%%

write_rules(FileName):-
    cat([FileName,'.rules'],Name,_),
    fcreate(output,Name,1),
    output(output), 
    write_rules,
    fclose(output).
    
write_rules:-
    recorded(rule,Rule,_),
    output(Out),
    (Out = output ->
        write('('),
        write_term(Rule,[max_depth(100000)]),
        %write(Rule),
        write(').')
        ;
        write_nice(Rule)
    ),
    nl,
    fail.
write_rules.

write_nice((Class,Lits,Distrib,default)):-
    !,
    write('Default Class = '),
    write(Class),
    write(' Class distribution: '),
    write(Distrib),
    nl.
write_nice((Class,Lits,Distrib,Eval)):-
    write('IF '),
    write_lits(Lits),
    %nl,
    write(' THEN Class = '),
    write(Class),
    nl,
    write('Evaluation: '),
    NegEval is round(-1000 * Eval)/1000,
    write(NegEval),
    get_distribution([],PriorDist),
    significance(PriorDist,Distrib,Sig),
    RSig is round(Sig),
    write(' Significance: '),
    write(RSig),
    write(' Class distribution: '),
    write(Distrib),
    nl.
    
write_lits([Lit]):-
    write(Lit),
    !.
write_lits([Lit|Lits]):-
    write(Lit),
    write(' AND '),
    write_lits(Lits).

read_rules(FileName):-
    eraseall(rule),
    cat([FileName,'.rules'],Name,_),
    fopen(input,Name,0),
    input(input), 
    read_rules,
    fclose(input).
    
read_rules:-
    repeat,
    read(Rule),
    (Rule = end_of_file ->
        true
        ;
        recordz(rule,Rule,_),
        fail).

%%%%%%% Writing ROC data for GNU plot %%%%%%%%%%%%%%%%%

wroc(System,Class):-
    recorded(filename,FileName,_),
    !,
    wroc(System,Class,FileName).
wroc(_,_):-
    nl,
    write('Nothing to export.'),
    nl.
    
wroc(gnu,Class,FileName1):-
    get_roc_diag(Class,AllPoints,ConvHull),
    cat([FileName1,'-'],FileName2,_),
    cat([FileName2,Class],FileName,_),
    cat([FileName,'_roc_ch.dat'],ConvHullFile,_),      
    cat([FileName,'_roc_all.dat'],AllPointsFile,_),   
    cat([FileName,'_roc.plt'],PlotFile,_),
    fcreate(output,ConvHullFile,1),
    output(output), 
    write_points(ConvHull),
    fclose(output),
    fcreate(output,AllPointsFile,1),
    output(output), 
    write_points(AllPoints),
    fclose(output),
    fcreate(output,PlotFile,1),
    output(output), 
    write_gnu_plot_commands(ConvHullFile,AllPointsFile),
    fclose(output).

write_points([]).
write_points([(X,Y,_)|Rest]):-
    nl,
    write(X),
    write('  '),
    write(Y),
    write_points(Rest).
    
write_gnu_plot_commands(ConvHullFile,AllPointsFile):-
    get_title(Title),
    write('set xlabel "False Positive Rate"'), 
    nl,
    write('set ylabel "True Positive Rate"'),
    nl,
    write('set grid'),
    nl,
    write('set size 0.6,0.6'),
    nl,
    write('set key right bottom'),
    nl,
    write('plot  "'),
    write(ConvHullFile),
    write('" title "'),
    write(Title),
    write('" with linespoints pointtype 1'),
    %nl,
    write(', "'),
    write(AllPointsFile),
    write('" title "" with points pointtype 1').
    
get_title(Title):-
    evalfn(EvalFn),
    (EvalFn = wracc ->
        Heur = 'WRAcc - '
        ;
        Heur = 'Acc - '
    ),
    (cover_search ->
        Search = 'Cover'
        ;
        Search = 'Weight'
    ),
    cat([Heur,Search],Title,_).

get_roc_diag(Class,_,_):-
    not recorded(roc,(Class/_,_,_),_),
    !,
    write('Please generate ROC points first (e.g. sg_roc/1)'),
    fail.
get_roc_diag(Class,Roc,CvHull):-
    findall((FPr,TPr,RuleLits),recorded(roc,(Class/RuleLits,FPr,TPr),_),UnsortedRoc1),
    (my_member((0.0,0.0,_),UnsortedRoc1) ->       % adding (0,0) and (1,1) points if not present
        UnsortedRoc2 = UnsortedRoc1
        ;
        UnsortedRoc2 = [(0.0,0.0,added)|UnsortedRoc1]
    ),
        (my_member((1.0,1.0,_),UnsortedRoc2) ->
        UnsortedRoc3 = UnsortedRoc2
        ;
        UnsortedRoc3 = [(1.0,1.0,added)|UnsortedRoc2]
    ),
    sort(UnsortedRoc3,Roc),         
    extract_conv_hull(Roc,CvHull),
    findall(Lits,(
        recorded(roc,(Class/Lits,_,_),_),
        my_member((_,_,Lits),CvHull)
        ),
        CHRules),
    (recorded(chrules,Class/_,DbRef) ->
        erase(DbRef)
        ;
        true
    ),
    recorda(chrules,Class/CHRules,_).
    
extract_conv_hull([],[]):-
    !.
extract_conv_hull([Point|P],[Point|Ch]):-
    max_angle(Point,P,MaxPoint),
    del_concave_points(MaxPoint,P,P1),
    extract_conv_hull(P1,Ch).
    
max_angle(Point,[],none).
max_angle(Point,[(1.0,1.0,_)],(1.0,1.0,_)).
max_angle(Point,[Point1|P],MaxPoint):-
    max_angle(Point,P,MaxPoint1),
    angle(Point,MaxPoint1,AngleSoFar),
    angle(Point,Point1,AngleNow),
    (AngleSoFar > AngleNow ->    % substitute > with >= if points included in
        MaxPoint = MaxPoint1    % an existing convex hull line should be excluded from the partially linear CH
        ;                       % i.e. those that are not the limiting points of one line part
        MaxPoint = Point1
    ).
    
angle(_,none,0):-
    !.
angle((X1,Y1,_),(X2,Y2,_),Angle):-
    Angle is (Y2 - Y1) / (X2 - X1).

del_concave_points(none,_,[]):-
    !.
del_concave_points(MaxPoint,[MaxPoint|Rest],[MaxPoint|Rest]):-
    !.
del_concave_points(MaxPoint,[(1.0,1.0,_)],[(1.0,1.0,_)]):-
    !. 
del_concave_points(MaxPoint,[Point|Rest],Rest2):-
    del_concave_points(MaxPoint,Rest,Rest2).   
    
%%%%%%% Write (Append to File) a Quality Report %%%%%%%%%%%%%%%

sq:-
    sg_quality((NumRules,AvgLength,AvgSig,AvgCov,AvgAuc)),
    recorded(filename,FileName,_),
    nl,
    write('Data file = '),
    write(FileName),
    nl,
    write('Number of rules = '),
    write(NumRules),
    nl,
    write('Average Length of Rule = '),
    write(AvgLength),
    nl,
    write('Average Significance = '),
    write(AvgSig),
    nl,
    write('Average Coverage = '),
    write(AvgCov),
    write('% of instances'),
    nl,
    write('Average (over classes) Area Under Roc = '),
    write(AvgAuc),
    nl.

wq:-
    working_with_test_file,
    !,
    wq('report-test.txt').    

wq:-
    wq('report.txt').
    
working_with_test_file:-         % 'test' word in the end of 
    atom_chars('test',CTest),    % currently loaded data name
    my_reverse(CTest,RCTest),
    recorded(filename,FileName,_),
    atom_chars(FileName,CFileName), 
    my_reverse(CFileName,[D,U,M,Y|_]),  % VERY dummy, my_reverse(CFileName,[RCTest|_]) did not work
    RCTest = [D,U,M,Y].                 % Yap unification problem? 
    
wq(FileName):-
    sg_quality((NumRules,AvgLength,AvgSig,AvgCov,AvgAuc)),
    recorded(filename,DataFileName,_),
    (exists(FileName) ->
        open(FileName,'append',Stream),
        Header = none
        ;
        open(FileName,'write',Stream),
        Header = 'Data file ; Avg Rule Length ; No of rules ;  Avg significance ; Avg Coverage[%] ; Avg AUC'
    ),
    current_output(CurrOut),
    set_output(Stream),
    (Header = none ->
        true
        ;
        write(Header),
        nl
    ),
    write(DataFileName),
    write(' ; '),
    write(AvgLength),
    write(' ; '),
    write(NumRules),
    write(' ; '),
    write(AvgSig),
    write(' ; '),
    write(AvgCov),
    write(' ; '),
    write(AvgAuc),
    nl,
    flush_output,
    close(Stream),
    set_output(CurrOut).

%%%%%%% Drop rules below ROC convex hull %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dr:-    
    drop_sub_ch_rules.
    
drop_sub_ch_rules:-
    not recorded(chrules,_,_),
    !,
    nl,
    write('No convex hull entry created yet.').
drop_sub_ch_rules:-
    recorded(chrules,Class/CHRulesLits,_),
    recorded(rule,(Class,CHRuleLits,_,_),DbRef),
    (my_member(CHRuleLits,CHRulesLits) ->
        true
        ;
        erase(DbRef)
    ),
    fail.
drop_sub_ch_rules.
    
%%%%%%% Test subgroup interestingness on currently loaded data %%%%%%%%%%%%%%%%%%%%

sg_avg_auc(AvgAuc):-
    all_classes(Classes),
    sum_aucs(Classes,SumAuc),
    length(Classes,NumClasses),
    AvgAuc is SumAuc / NumClasses.
    
sum_aucs([],0). 
sum_aucs([Class|Rest],SumAuc):-
    sum_aucs(Rest,SumAuc1),
    sg_auc(Class,Auc),
    SumAuc is SumAuc1 + Auc.

sg_auc(Class,Auc):-
    sg_roc(Class),
    get_roc_diag(Class,_,ConvHull),
    calc_area(ConvHull,Auc).
    
calc_area([_],0). 
calc_area([(X1,Y1,_),(X2,Y2,_)|Points],Auc):-
    calc_area([(X2,Y2,_)|Points],Auc1),
    Auc2 is (Y2 + Y1) / 2 * (X2 - X1),
    Auc is Auc1 + Auc2.

sg_quality((Num,AvgLength,AvgSig,AvgCov,AvgAuc)):-
    sg_sum_values((Num,SumLength,SumSig,SumCov)),
    AvgLength is round(100 * SumLength / Num) / 100,
    AvgSig is round(100 * SumSig / Num) / 100,
    AvgCov is round(100 * SumCov / Num) / 100,
    sg_avg_auc(AvgAuc).

sg_sum_values(_):-
    eraseall(current_sums),
    recorda(current_sums,(0,0,0,0),_),
    fail.
sg_sum_values(_):-
    get_distribution([],PriorDist),
    recorded(rule,(_,Lits,_,Eval),_),       % posterior distrib of a rule is not taken from the rule database
    not Eval = default,
    get_distribution(Lits,Distrib),         % but from here, as new (testing) data may have been loaded (-> must recalculate)
    significance(PriorDist,Distrib,Sig),
    sum_list(Distrib,Cov),
    length(Lits,Length),
    recorded(current_sums,(Num1,SumLength1,SumSig1,SumCov1),DbRef),
    erase(DbRef),
    Num is Num1 + 1,
    SumLength is SumLength1 + Length,
    SumSig is SumSig1 + Sig,
    SumCov is SumCov1 + Cov,
    recorda(current_sums,(Num,SumLength,SumSig,SumCov),_),
    fail.
sg_sum_values((Num,SumLength,SumSig,RelSumCov)):- % Last arg was SumCov (wrongly), corrected Feb 27, 03 (FZ)
    recorded(current_sums,(Num,SumLength,SumSig,SumCov),DbRef),
    erase(DbRef),
    get_distribution([],PriorDist),
    sum_list(PriorDist,NumInst),
    RelSumCov is integer(SumCov / NumInst * 10000)/100.

sg_roc(Class):-
    not recorded(rule,(Class,Lits,_,_),_), % if there is no rule for the class
    !,
    eraseall(roc),
    recordz(roc,(Class/added,0.0,0.0),_),  % then only 0,0 - 1,1 points are on the ROC
    recordz(roc,(Class/added,1.0,1.0),_).   

sg_roc(Class):-
    eraseall(roc),
    get_distribution([],PriorDistrib),
    all_classes(Classes),
    pos_neg_distr(Class,Classes,PriorDistrib,Pos,Neg),
    recorded(rule,(Class,Lits,_,Eval),_), % posterior distribution in not taken from the rule database
    not Eval = default,
    get_distribution(Lits,Distrib),    % as a new (testing) data file may have been loaded (-> must recalculate)
    pos_neg_distr(Class,Classes,Distrib,PostPos,PostNeg),
    TPr is round(PostPos / Pos * 100) / 100,
    FPr is round(PostNeg / Neg * 100) / 100,
    recordz(roc,(Class/Lits,FPr,TPr),_),
    fail.
%sg_roc(Class):-                        % Also include negative subgroups ???? %    
%    get_distribution([],PriorDistrib), % Probably not a good idea -> report separately
%    all_classes(Classes),
%    pos_neg_distr(Class,Classes,PriorDistrib,Pos,Neg),
%    recorded(rule,(OtherClass,Lits,Distrib,_),_),
%    not Class = OtherClass,
%    pos_neg_distr(Class,Classes,Distrib,PostPos,PostNeg),
%    FPr is 1 - round(PostPos / Pos * 100) / 100, % here reverted
%    TPr is 1 - round(PostNeg / Neg * 100) / 100, % here reverted
%    recordz(roc,(Lits,FPr,TPr),_),
%    fail.
sg_roc(_).
    
pos_neg_distr(_,[],[],0,0).
pos_neg_distr(Class,[Class|Cs],[PriorDistrib|P],Pos,Neg):-
    !,
    pos_neg_distr(Class,Cs,P,Pos1,Neg),
    Pos is Pos1 + PriorDistrib.
pos_neg_distr(Class,[OtherClass|Cs],[PriorDistrib|P],Pos,Neg):-
    pos_neg_distr(Class,Cs,P,Pos,Neg1),
    Neg is Neg1 + PriorDistrib.
       
%%%%%%% Test ruleset accuracy on currently loaded data %%%%%%%%%%%

%%% prob_roc/1 has to be changed accoring to a ROC-generation methodology due to Peter Flach
%%% this implementation is slow and gives many identical points
%%% Also, method should not be limited to 2-class problems, user will specify the target class
%%% all others being negative (like sg_roc/1). assign_classes/4 will not then be needed

prob_roc(NumPoints):-
    eraseall(roc),
    Step is 1/NumPoints,
    bin_tests(0,Step).
    
bin_tests(FPPoint,_):-
    FPPoint >= 1.
bin_tests(FPPoint,Step):-
    write('Checking ROC values for Pos threshold '),
    write(FPPoint),
    nl,
    bin_test(FPPoint,Rates),
    recordz(roc,Rates,_),
    FPPoint1 is FPPoint + Step,
    bin_tests(FPPoint1,Step).
    
bin_test(Rates):-
    all_classes([_,_]),
    !,
    bin_test(0.5,Rates).
    
bin_test(PosThr,(FPr,TPr)):-
    all_classes([First,Second]),
    assign_classes(First,Second,Pos,Neg),
    !,
    all_cov(Inst),
    test_bin(Inst,(Tp,Tn,Fp,Fn),Pos,Neg,PosThr),
    FPr is Fp / (Tn + Fp),
    TPr is Tp / (Fn + Tp).

test(Result):-
    all_cov(Inst),
    % length(Inst,All),
    test_classes(Inst,Corr,All),
    Result is Corr/All.

% Recognizes which class is meant as positive/negative 
% This will become unneded (see above)

assign_classes(pos,neg,pos,neg).
assign_classes(neg,pos,pos,neg).
assign_classes(+,-,+,-).
assign_classes(-,+,+,-).
assign_classes(0,1,1,0).
assign_classes(1,0,1,0).
assign_classes(First,Second,First,Second):-
    !.
    
test_bin([],(0,0,0,0),Pos,Neg,_).
test_bin([I|Inst],(Tp1,Tn1,Fp1,Fn1),Pos,Neg,PosThr):-
    test_bin(Inst,(Tp,Tn,Fp,Fn),Pos,Neg,PosThr),
    class(I,Class),
    prob_classify(I,PosThr,PredClass),
    update_rates(Pos,Neg,Class,PredClass,(Tp,Tn,Fp,Fn),(Tp1,Tn1,Fp1,Fn1)).
    %write(I),
    %write(', Class / Predicted: '),
    %write(Class),
    %write(' / '),
    %write(PredClass),
    %write(', Tp,Tn,Fp,Fn so far: '),
    %write((Tp1,Tn1,Fp1,Fn1)),
    %nl.

update_rates(Pos,Neg,Pos,Pos,(Tp,Tn,Fp,Fn),(Tp1,Tn,Fp,Fn)):-
    Tp1 is Tp + 1,
    !.  
update_rates(Pos,Neg,Pos,Neg,(Tp,Tn,Fp,Fn),(Tp,Tn,Fp,Fn1)):-
    Fn1 is Fn + 1,
    !. 
update_rates(Pos,Neg,Neg,Neg,(Tp,Tn,Fp,Fn),(Tp,Tn1,Fp,Fn)):-
    Tn1 is Tn + 1,
    !. 
update_rates(Pos,Neg,Neg,Pos,(Tp,Tn,Fp,Fn),(Tp,Tn,Fp1,Fn)):-
    Fp1 is Fp + 1,
    !. 
          
test_classes([],0,0).
test_classes([I|Inst],Corr2,InstNum):-
    test_classes(Inst,Corr1,InstNum1),
    InstNum is InstNum1 + 1,
    classify(I,PredClass/_,_),
    class(I,Class),
    (PredClass = Class ->
        Corr2 is Corr1 + 1
        ;
        Corr2 is Corr1
    ),
    CurrAcc is round(Corr2/InstNum*100)/100,
    nl,
    write(I),
    write(', Class / Predicted: '),
    write(Class),
    write(' / '),
    write(PredClass),
    write(', Accuracy so far: '),
    write(CurrAcc).

class(I,Class):-
    recorded(cov,(Class,_,Inst),_),
    my_member(I,Inst),
    !.

%%%%%%% Classify an instance by an unordered ruleset %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

classify(_,_,_):-
    eraseall(current_distrib),
    %Distrib = [0,0],
    recorda(current_distrib,Distrib,_),
    fail.
classify(Inst,_,_):-
    recorded(rule,(_,Lits,Distrib,Eval),_),
    not Eval = default,
    consistent(Lits,Inst),
    recorded(current_distrib,CurDis,DbRef),     
    sum_elements(Distrib,CurDis,NewDis),
    erase(DbRef),
    recorda(current_distrib,NewDis,_),
    fail.
classify(Inst,MajClassFreq,Distrib):-
    recorded(current_distrib,Distrib,_),
    all_classes(Classes),
    majority_vote(Classes,Distrib,MajClassFreq).
    
prob_classify(Inst,PosThreshold,Class):-
    classify(Inst,_,DUMMY),   % can't unify at once (YAP bug)
    DUMMY = [DFirst,DSecond],
    all_classes([First,Second]),
    assign_classes(First,Second,Pos,Neg),
    (Pos = First ->
        DPos = DFirst,
        DNeg = DSecond
        ;
        DPos = DSecond,
        DNeg = DFirst
    ),
    PosFraction is DPos / (DPos + DNeg),
    (PosFraction > PosThreshold ->
        Class = Pos
        ;
        Class = Neg
    ).
    
consistent([],_).
consistent([L|Lits],I):-
    recorded(cov,(_,L,Cov),_),
    my_member(I,Cov),
    consistent(Lits,I).
    
sum_elements([],[],[]):-
    !.
sum_elements([E1|R1],[E2|R2],[E3|R3]):- 
    E3 is E1 + E2,
    sum_elements(R1,R2,R3).

sum_list([],0).
sum_list([E|R],Sum):-
    sum_list(R,Sum1),
    Sum is E + Sum1.
    
%%%%%%% Finding rules for all classes %%%%%%%%%%%%%%%

induce:-
    not recorded(cov,_,_),
    !,
    nl,
    write('Please read some data first.'),
    nl.

induce:-
    Start is cputime,
    induce1,
    Stop is cputime,
    Time is Stop - Start,
    nl,
    write(Time),
    write(' seconds taken to induce rules.').

induce1:-
    nl,
    write('Inducing rules.'),
    nl,
    recorded(classes,(_,Classes),_),
    my_member(Class,Classes),           
    nl,
    write('Class: '),
    write(Class),
    write('  '),
    eraseall(current_class),
    recorda(current_class,Class,_),
    rules,
    fail.
induce1:-
    default_rule.
    
default_rule:-
    (recorded(rule,(_,_,_,default),DbRef) ->
        erase(DbRef)
        ;
        true),
    get_distribution([],Distrib),
    all_classes(Classes),
    majority_vote(Classes,Distrib,MajClass/_),
    recordz(rule,(MajClass,[],Distrib,default),_).
    
majority_vote([Class],[Freq],Class/Freq).
majority_vote([Class|C],[Freq|F],Class2/Freq2):-
    majority_vote(C,F,Class1/Freq1),
    (Freq > Freq1 ->
        Class2 = Class,
        Freq2 = Freq
        ;
        Class2 = Class1,
        Freq2 = Freq1
    ).

%%%%%%% Finding a set of rules for the current class %%%%%%%%%%%%

rules:-                         
    recorded(current_class,Class,_),
    clean_class_rules(Class),
    init_cov_nums,
    total_weight(Tw),
    eraseall(initweight),
    recorda(initweight,Tw,_),
    calculate_prior(Class),
    repeat,
    rule(Eval-Lits-PosCov/_),      
    (Lits = [true] ->           
        !,
        why_stopping(exhausted)
        ;               
        get_distribution(Lits,Distrib),       
        Rule = (Class,Lits,Distrib,Eval),   
        recordz(rule,Rule,_),  
        write_nice(Rule),
        update_positives(PosCov),                   
        stop_adding_rules(Eval,Distrib),
        !                         
    ).
      
calculate_prior(Class):-      
    all_cov(Cov),  
    all_pos(Class,PosCov),
    length(Cov,LCov),          
    length(PosCov,LPosCov),     
    eraseall(current_prior),
    Prior is LPosCov/LCov,
    recorda(current_prior,Prior,_),
    write('(prior = '),
    RPrior is integer(Prior * 100) / 100,
    write(RPrior),
    write(')'),
    nl,
    nl,
    !.
       
why_stopping(Reason):-
    get_excuse(Reason,Message),
    nl,
    write(Message),
    write('No more rules for this class. '),
    nl.
    
get_excuse(exhausted,'No more rules left in the search space. ').
get_excuse(sigthr,'Significance threshold reached. ').
get_excuse(evalthr,'Evaluation threshold reached. ').
get_excuse(maxrulenum,'Maximum number of rules per class reached. ').
get_excuse(weightthr,'Sufficient weight / number of instaces covered. ').
    
stop_adding_rules(CurrEval,_):-   
    stop_on_eval,
    eval_threshold(EvThr),  
    CurrEval > EvThr,
    why_stopping(evalthr).  
    
stop_adding_rules(_,Distrib):-
    stop_on_sig,
    sig_threshold(SigThr),
    get_distribution([],PriorDist),
    significance(PriorDist,Distrib,Sig),
    Sig < SigThr,
    why_stopping(sigthr).
    
stop_adding_rules(_,_):-
    recorded(current_class,Class,_), 
    findall(Rule,recorded(rule,(Class,_,_,_),_),Rules),
    length(Rules,NumRules),
    max_rules_for_class(MaxRules),
    NumRules = MaxRules,
    why_stopping(maxrulenum).
      
stop_adding_rules(_,_):-    
    total_weight(Tw),             
    recorded(initweight,Iw,_),      
    weight_threshold(Wt),           
    Prop is Tw/Iw, 
    write('Remaining % of initial total weight = '),
    RProp is round(100 * Prop),
    write(RProp),
    nl,
    nl,
    Prop < Wt,
    why_stopping(weightthr).
    
total_weight(Tw):-
    all_cov(Ac), 
    weight(Ac,Tw). 
    
clean_class_rules(Class):-
    recorded(rule,(Class,_,_,_),DbRef),
    erase(DbRef),
    fail.
clean_class_rules(_).

init_cov_nums:-
    eraseall(covnum),
    all_cov(Inst),
    my_member(I,Inst),
    recordz(covnum,(I,0),_),
    fail.
init_cov_nums.    

update_positives(PosCov):-
    my_member(I,PosCov),
    inc_cov_num(I),
    fail.
update_positives(_).

inc_cov_num(I):-
    recorded(covnum,(I,Num),DbRef),
    Num1 is Num+1,
    erase(DbRef),
    recordz(covnum,(I,Num1),_),
    !.
    
get_distribution(Lits,Distr):-
    all_classes(Classes),
    get_distr(Classes,Lits,Distr),
    !.
    
get_distr([],_,[]).
get_distr([C|Classes],Lits,[N|Nums]):-
    rule_cov(C,Lits,Cov),
    length(Cov,N),
    get_distr(Classes,Lits,Nums).
    
%%%%%% Finding one rule %%%%%%%%%%%%

rule(Rule):-
    search_strategy(beam),
    all_cov(Cov),
    calculate_weight(Cov),
    rule_beam([1-[true]-Cov/Cov],1-[true]-Cov/Cov,Rule).

calculate_weight(Cov):-      % For UNORDERED rulesets
    weight(Cov,WCov), 
    eraseall(current_weight),
    recorda(current_weight,WCov,_).

calculate_weight_and_prior2(Cov):-      % This would be for ORDERED rulesets
    recorded(current_class,Class,_),    % only for future use.
    all_pos(Class,PosCov),              % for UNORDED, prior calculated by ruleset/0.
    weight(Cov,WCov),           % change to length/2 if weights should 
    weight(PosCov,WPosCov),     % not be used to calculate prior for wracc
    eraseall(current_prior),
    Prior is WPosCov/WCov,
    recorda(current_prior,Prior,_),
    eraseall(current_weight),
    recorda(current_weight,WCov,_).
    
rule_beam([],B,B).
rule_beam([Rule|Open1],BestRule1,BestRule3):-  
    check_acceptability(Rule,Acceptable), 
    check_best(Rule,BestRule1,BestRule2,Acceptable),  
    n_best_refs(Rule,NBest,Acceptable),     
    my_append(Open1,NBest,Open2),
    sort(Open2,SOpen2),         
    rule_beam(SOpen2,BestRule2,BestRule3). 

check_acceptability(E-L-_/_,yes):-
    acceptable(E,L),
    !.
check_acceptability(E-L-_/_,no).

check_best(E-L-C,BE1-BL1-BC1,E-L-C,yes):- 
    E < BE1,
    !.    
check_best(E-L-C,BE1-BL1-BC1,BE1-BL1-BC1,_).

acceptable(E,L):-
    good_enough(E),
    significant(L),
    not inacceptable(L).
    
good_enough(E):-
    eval_threshold(EvalThr),
    E =< EvalThr. % both numbers are stored as negatives

significant(L):-
    get_distribution([],PriorDist),
    get_distribution(L,PosteriorDist),
    significance(PriorDist,PosteriorDist,Sig),
    sig_threshold(SigThr),
    Sig >= SigThr.

significance(Prior,Post,Sig):-
    sum_list(Prior,PriorCov),
    sum_list(Post,PostCov),
    Coef is PostCov / PriorCov,
    sig_sum(Prior,Post,Coef,Sum),
    Sig is 2 * Sum.
    
sig_sum([],[],_,0).
sig_sum([E1|E],[F1|F],Coef,Sig):-
    sig_sum(E,F,Coef,Sig1),
    P1 is Coef * E1,
    (F1 =:= 0 ->
        Summand is 0
        ;
        Summand is F1 * log(F1/P1)
    ),
    Sig is Sig1 + Summand.

inacceptable(L):-                     % may be changed to avoid similar rules
    recorded(current_class,Class,_),  % currently avoids duplicit rules       
    recorded(rule,(Class,Lits,_,_),_),
    same_sets(L,Lits).

n_best_refs(E-L1-PosCov/NegCov,NBest,Acceptable):- 
    continue_refinement(E-L1-PosCov/NegCov,Acceptable),          
    !, 
    (L1 = [true] ->
        L = []
        ;
        L = L1
    ),
    recorded(current_class,Class,_),  
    findall(RE-[RL|L]-RPosCov/RNegCov,
        (                          
        get_lit(Class,L,PosCov,RL,RPosCov),  %nl,write('inspected lit: '),write(L),write(':'),write(RL),
        neg_cov(Class,NegCov,RL,RNegCov),
        weight(RPosCov,RWPos),
        weight(RNegCov,RWNeg),
        evaluate([RL|L],RWPos-RWNeg,RE)
        ),
        Refs),          
    sort(Refs,SRefs),
    beam_width(N),
    first_n(N,SRefs,NBest).
n_best_refs(_,[],_).
    
% pruning takes place here
continue_refinement(E-L-PosCov/NegCov,Acceptable):-
     not prune_on_max_length(L),
     not prune_on_no_negcov(E,L,NegCov,Acceptable),
     not prune_on_wracc_threshold(PosCov,NegCov).

prune_on_max_length(L):-
    length(L,LL),
    max_length(ML),
    LL = ML.
    
prune_on_no_negcov(E,L,NegCov,yes):-  % If L acceptable, then only refine L if negcov nonzero (otherwise L 'cannot be better') 
    length(NegCov,0).

prune_on_wracc_threshold(PosCov,NegCov):-
    evalfn(EvalFn),
    EvalFn = wracc,
    eval_threshold(NegEvalThreshold), % Eval is stored as a negative number for sorting
    EvalThreshold is 0 - NegEvalThreshold,
    weight(NegCov,WNeg),
    weight(PosCov,WPos),
    recorded(current_prior,Prior,_),
    recorded(current_weight,CurWeight,_),
    Gen is (WPos+WNeg)/CurWeight,
    MaxPossWracc is Gen * (1 - Prior), % maximum possible wracc value of a descendant of the investigated literal                               
    MaxPossWracc < EvalThreshold.
    
get_lit(Class,L,PosCov,RL,RPosCov):-
    %cov(Class,RL,RCov),    % yap use record
    recorded(cov,(Class,RL,RCov),_),
    not my_member(RL,L), 
    bagof(Ex,
        (
        my_member(Ex,PosCov),
        my_member(Ex,RCov)
        ),
        RPosCov).      
      
neg_cov(Class,NegCov,RL,RNegCov):-
    findall(Ex,
        (
        % cov(OtherClass,RL,RCov), % yap use record
        recorded(cov,(OtherClass,RL,RCov),_), 
        not OtherClass = Class,
        my_member(Ex,NegCov),
        my_member(Ex,RCov)
        ),
        RNegCov),
    !.
        
weight([],0).
weight([I|Cov],W):-
    weight(Cov,W1),
    recorded(covnum,(I,Num),_),
    calc_weight(Num,W2),
    W is W1+W2,
    !.

evaluate(Lits,WPos-WNeg,E):- 
    length(Lits,L),
    recorded(current_prior,Prior,_),
    recorded(current_weight,CurWeight,_),
    recorded(classes,(NumClasses,_),_),
    LaplaceAcc is (WPos+1)/(WPos+WNeg+NumClasses),
    FreqAcc is (WPos)/(WPos+WNeg),
    Gen is (WPos+WNeg)/CurWeight,
    acc_est(AccEst),
    (AccEst = laplace ->
        EstAcc is LaplaceAcc
        ;
        EstAcc is FreqAcc
    ),
    WRAcc is Gen * (EstAcc - Prior),
    evalfn(EvalFn),
    (EvalFn = wracc ->
        E is -WRAcc        % minus for easier sorting
        ;
        E is -EstAcc
    ),
    !.

first_n(0,_,[]):-
    !.
first_n(_,[],[]).
first_n(N,[H|B],[H|B1]):-
    N1 is N-1,
    first_n(N1,B,B1).

all_pos(Class,PosCov):-
    setof(I,
        F^Inst^DbRef^(
        recorded(cov,(Class,F,Inst),DbRef),   
        my_member(I,Inst)
        ),
        PosCov
        ),
    !.

all_neg(Class,NegCov):-
    setof(I,
        F^Inst^OtherClass^DbRef^(
        recorded(cov,(OtherClass,F,Inst),DbRef),
        not Class = OtherClass,
        my_member(I,Inst)
        ),
        NegCov
        ),
    !.
        
all_cov(Cov):-
    setof(I,
        F^Inst^C^DbRef^(
        recorded(cov,(C,F,Inst),DbRef),
        my_member(I,Inst)
        ),
        Cov
        ),
    !.

rule_cov(Class,[],AllCov):-
    all_pos(Class,AllCov).
rule_cov(Class,[L|Lits],Cov):-
    (recorded(cov,(Class,L,Cov2),_) ->       
        rule_cov(Class,Lits,Cov1),
        findall(I,
                (
                my_member(I,Cov1),
                my_member(I,Cov2)
                ),
                Cov)
        ;
        Cov = []).
        
all_classes(Classes):-
    setof(C,F^Inst^DbRef^recorded(cov,(C,F,Inst),DbRef),Classes),
    !.           

%%%%%%%% REPEATED INDUCTION & VALIDATION %%%%%%%%%%%%%%%%

validate(FileName):-
    cat([FileName,'1.cov'],NumFileName,_),
    cat([FileName,'1_test.cov'],NumFileTestName,_),
    exists(NumFileName),
    exists(NumFileTestName),
    !,
    nl,
    write('Cross-validating ...'),
    nl,
    crossvalidate(1,FileName).
validate(FileName):-  
    cat([FileName,'.cov'],CompleteFileName,_),
    cat([FileName,'_test.cov'],CompleteTestFileName,_),
    exists(CompleteFileName),
    exists(CompleteTestFileName),
    !,
    test_train_validate(FileName).
validate(FileName):-
    nl,
    write('Missing .cov or _test.cov files'),
    nl.
    
crossvalidate(Fold,FileName):- 
    number_atom(Fold,AFold),
    cat([FileName,AFold],TrainName,_),
    cat([AFold,'_test'],TestSuffix,_),
    cat([FileName,TestSuffix],TestName,_),  
    cat([TrainName,'.cov'],CompleteTrainName,_),
    cat([TestName,'.cov'],CompleteTestName,_),
    exists(CompleteTrainName),
    exists(CompleteTestName),
    !,
    nl,
    write('Processing fold '),
    write(Fold),
    nl,
    nl,
    assess(TrainName,TestName),
    % assess2(TrainName,TestName), % (only if rulesets exist already)
    NewFold is Fold + 1,
    crossvalidate(NewFold,FileName).
crossvalidate(Fold,FileName):-
    PrevFold is Fold - 1,
    nl,
    write('Cross-validation completed with '),
    write(PrevFold),
    write(' folds.'),
    message_for_dummies.
    
train_test_validate(TrainName):- 
    cat([TrainName,'_test'],TestName,_),  
    !,
    nl,
    write('Inducing and testing ...'),
    nl,
    nl,
    assess(TrainName,TestName),
    %assess2(TrainName,TestName), (only if rulesets exist already)
    message_for_dummies.

assess(TrainName,TestName):-
    r(TrainName),                   % read train fold / split
    i,                              % induce rules
    w,                              % write rules
    wq,                             % test all rules on TRAINING data and write quality report
    dr,                             % drop rules below convex hull
    cat([TrainName,'-ch'],CHName,_),
    w(CHName),                      % write rules above convex hull   
    wq('report-ch.txt'),            % test filtered rules on TRAINING data and write quality report         
    rr,                             % read back all rules
    r(TestName),                    % read test fold / split
    wq,                             % test all rules on TESTING data and write quality report
    rr(CHName),                     % read back filtered rules
    wq('report-ch-test.txt').       % test filtered rules on TESTING data and write quality report 
   
assess2(TrainName,TestName):-    % SPECIAL: when rule files already exist
    r(TrainName),                   % read train fold / split
    rr,                             % read (don't induce) rules
    % w,                              % write rules ... no
    wq,                             % test all rules on TRAINING data and write quality report
    % dr,                             % drop rules below convex hull .. no, instead:
    cat([TrainName,'-ch'],CHName,_),
    rr(CHName),                     % read filtered rules
    % w(CHName),                      % write rules above convex hull .. no  
    wq('report-ch.txt'),            % test filtered rules on TRAINING data and write quality report         
    rr,                             % read back all rules
    r(TestName),                    % read test fold / split
    wq,                             % test all rules on TESTING data and write quality report
    rr(CHName),                     % read back filtered rules
    wq('report-ch-test.txt').       % test filtered rules on TESTING data and write quality report 
    
message_for_dummies:-  
    nl,   
    write('Results stored in files: ''report.txt'', ''report-test.txt'', ''report-ch.txt'' and ''report-ch-test.txt''.').

%%%%%%%%%%%%%%% Generic Predicates %%%%%%%%

same_sets(A,B):-
    subset(A,B),
    subset(B,A).

subset([],A).
subset([A|B],C):-
    my_member(A,C),
    subset(B,C).
    
my_member(A,[A|_]).
my_member(A,[X|B]):-
    my_member(A,B).

my_append([],A,A).
my_append([A|B],C,[A|BC]):-
    my_append(B,C,BC).

my_reverse([],[]).
my_reverse([A|B],RBA):-
    my_reverse(B,RB),
    my_append(RB,[A],RBA).

record_coverage:-
    eraseall(cov),
    fail.
record_coverage:-
    cov(C,F,Inst),
    recordz(cov,(C,F,Inst),_),
    fail.
record_coverage:-
    eraseall(classes),
    all_classes(Classes),
    length(Classes,NumClasses),
    recorda(classes,(NumClasses,Classes),_).

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
    atom_chars(C,SAB).

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

:-dynamic cov/3.
