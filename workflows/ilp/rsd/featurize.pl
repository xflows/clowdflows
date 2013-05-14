% The code below has been written by 
% Filip Zelezny (CVUT Prague, zelezny@fel.cvut.cz)

% Date of release: May 23, 2004


%%%%%%%%%%%%%%%%%%%%%%%%%%
% Known bug: 
% 	max_occ cannot be set for more than 1 predicate
%
% New features (not described in manual):
%	 predicates can be specified as
%	 commutative, antireflexive, antisymmetric
%	 helps to reduce # of features obtained
%
%%%%%%%%%%%%%%%%

:-dynamic def_head/5.
:-dynamic def_property/3.
:-dynamic def_structural/5.
:-dynamic count/1.
:-dynamic setting/2.
:-dynamic var_name/2.
:-dynamic dec_name/1.
:-dynamic max_input_arity/1.
:-dynamic instantiating/0.
:-dynamic commutative_pred/1.
:-dynamic antireflexive_pred/1.
:-dynamic antisymmetric_pred/1.

%%%%%%%%%%%%%%% Unified control %%%%%%%%%%%%%%%%%

r(Name):-
    read_dec(Name).
    
s:-
    wf.
    
show:-
    wf.
    
w:-
    cff.
    
w(Name):-
    set(feature_file,Name),
    cff.
    
%%%%%%%%%%%%%%% Read declarations %%%%%%%%%%%%%%%%

read_dec(Name):-
    cleanup,
    cat([Name,'.b'],Filename,_),
    (exists(Filename) ->
        reconsult(Filename),
        retractall(dec_name(_)),
        assert(dec_name(Name))
        ;
        nl,
        write('Cannot find '),
        write(Filename),
        write(' file.')
    ).

%%%%%%%%%%%%%%% Accept declarations %%%%%%%%%%%%%%
 
cleanup:-
    retractall(def_head(_,_,_,_,_)),
    retractall(def_property(_,_,_)),
    retractall(def_structural(_,_,_,_,_)),
    retractall(order(_,_)),
    retractall(setting(_,_)),
    retractall(instantiating),
    retractall(max_input_arity(_)),
    retractall(commutative_pred(_)),
    retractall(antireflexive_pred(_)),
    reset_count,
    assert(max_input_arity(0)).

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
    Def =.. [Name1|Args],
    length(Args,Arity),
    get_count(C),
    Num is C + 1000,
    number_atom(Num,NameNum),
    cat([Name1,NameNum],Name,_),
    inout(Args,In,Out,InPlaces,1),
    length(In,InputArity),
    (Out = [] ->
        assert(def_property(Recall,Name/Arity,In)),
        (Name1 = 'instantiate' ->
            assert(instantiating)
            ;
            true
        ),
        Order = C,
        max_input_arity(MaxArity),
        (Arity > MaxArity ->
            retractall(max_input_arity(_)),
            assert(max_input_arity(Arity))
            ;
            true
        )
        ;
        assert(def_structural(Recall,Name/Arity,In,Out,InPlaces)),
        max_input_arity(MaxArity),
        (InputArity > MaxArity ->
            retractall(max_input_arity(_)),
            assert(max_input_arity(InputArity))
            ;
            true
        ),
        Order is C+10000        % Properties should appear before structures
    ),                          % in given variable depth
    assert(order(Name/Arity,Order)).

inout([],[],[],[],_).
inout([+X|B],[X|In],Out,[Place|InPlaces],Place):-
    NextPlace is Place+1,
    inout(B,In,Out,InPlaces,NextPlace).
inout([-X|B],In,[X|Out],InPlaces,Place):-
    NextPlace is Place+1,
    inout(B,In,Out,InPlaces,NextPlace).

commutative(SymLit):-
    assert(commutative_pred(SymLit)).
    
antireflexive(SymLit):-
    assert(antireflexive_pred(SymLit)).

antisymmetric(SymLit):-
    assert(antisymmetric_pred(SymLit)).
             
%%%%%%%%%%%%%%% Create Features %%%%%%%%%%%%%%

feature_clause(Head,BodyList):-
    head_and_vars(Head,Out),
    !,
    feature([],BodyList,Out/Out,_,[],_).
    
head_and_vars(Head,Out):-
    def_head(_,Name/_,DIn,_,InPlaces),
    new_outvars([],DIn,0,-1,Out),
    place_args(Out,[],InPlaces,Args,Place),
    Head =.. [Name|Args],
    !.

feature(_,[],Vars/[],Vars/[],[],[_]).
feature(_,[],Vars/[],Vars/[],[_],[_]).
feature(SoFar,[Lit|Rest],Vars1,Vars3,EqClasses1,EqClasses3):-
    continue_adding_lits(SoFar),
    length(SoFar,CurrLength),
    literal(Lit,CurrLength,Vars1,Vars2,EqClasses1,EqClasses2),
    allowed(Lit,SoFar),
    feature([Lit|SoFar],Rest,Vars2,Vars3,EqClasses2,EqClasses3).

continue_adding_lits(Body):-
    not too_long(Body),
    not only_primary_property(Body).
    
only_primary_property([Lit]):- % (Pruning rule 4/2)
    primary_property(Lit).

primary_property(Lit):-
    Lit =.. [_,Var],
    Var =.. [PrimKey|_],
    def_head(_,_,[PrimKey],_,_).
 
too_long(Body):-
    max_length(MaxLength),
    length(Body,MaxLength),
    !.

max_length(MaxLength):-
    setting(clauselength,MaxLength),
    !.
max_length(8).

allowed(Lit,SoFar):-
    depth_of_literal(Lit,Depth),
    not my_member(Lit,SoFar),  % remove (handled by ordering)?
    % not too_long(SoFar), % handled elsewhere
    not too_deep(Depth),
    not too_many_occurences(Lit,SoFar),
    not too_many_recalls(Lit,SoFar),
    not wrong_ordering(Lit,SoFar,Depth),
    not wrong_args(Lit,SoFar),
    !.

too_deep(Depth):-
    setting(depth,MaxDepth),
    !,
    Depth > MaxDepth,
        !.

too_deep(Depth):-
    Depth > 4,
        !.

too_many_occurences(Lit,SoFar):-
    symbolic(Lit,Symb),
    setting(max_occ,(Symb,MOcc)),
    findall(Occ,occurence(Occ,SoFar,Symb),Occs),
    length(Occs,LOccs),
    LOccs >= MOcc,
        !.

occurence(Lit,Body,Symb):-
    my_member(Lit,Body),
    symbolic(Lit,Symb),
        !.

too_many_recalls(Lit,SoFar):-
    symbolic(Lit,Symb),
    def_structural(Recall,Symb,_,_,_),
    get_in_out_vars(Lit,InVars,_),
    findall(Rec,recall(Rec,SoFar,Symb,InVars),Recs),
    length(Recs,LRecs),
    LRecs >= Recall,
    !.

recall(Lit2,Body,Symb,InVars):-
    my_member(Lit2,Body),
    symbolic(Lit2,Symb),
    get_in_out_vars(Lit2,InVars,_).
    
wrong_ordering(Lit,SoFar,Depth):-
    my_member(Lit2,SoFar),
    depth_of_literal(Lit2,Depth1),
    Depth1 > Depth,
        !.
wrong_ordering(Lit,SoFar,Depth):-
    symbolic(Lit,Symb),
    order(Symb,OLit),
    my_member(Lit2,SoFar),
    depth_of_literal(Lit2,Depth),
    symbolic(Lit2,Symb2),
    order(Symb2,OLit2),
    OLit2 > OLit,
    !.
wrong_ordering(Lit,SoFar,Depth):-
    symbolic(Lit,Symb),
    my_member(Lit2,SoFar),
    depth_of_literal(Lit2,Depth),
    symbolic(Lit2,Symb),
    compare(<,Lit,Lit2),
    !.

wrong_args(Lit,_):-  % must be changed  
    symbolic(Lit,NameNum/Arity), 
    denumber(NameNum,Name), % must be changed to use "origin" in the newer version
    commutative_pred(Name/Arity), 
    get_in_out_vars(Lit,InVars,_),
    sort(InVars,SInVars),
    length(SInVars,Len),
    length(InVars,Len),
    not SInVars = InVars.

wrong_args(Lit,_):-  % must be changed  
    symbolic(Lit,NameNum/Arity), 
    denumber(NameNum,Name), % must be changed to use "origin" in the newer version
    antireflexive_pred(Name/Arity), 
    get_in_out_vars(Lit,InVars,_),
    sort(InVars,[_]). 
    
wrong_args(Lit,SoFar):-  % must be changed  
    symbolic(Lit,NameNum/Arity), 
    denumber(NameNum,Name), % must be changed to use "origin" in the newer version
    antisymmetric_pred(Name/Arity), 
    get_in_out_vars(Lit,[InVar1,InVar2],_),
    my_member(Lit2,SoFar),
    get_in_out_vars(Lit2,[InVar2,InVar1],_).
     
symbolic(Lit,Name/Arity):-
    Lit =.. [Name|Args],
    length(Args,Arity),
    !.

depth_of_literal(Lit,Depth):-
    get_in_out_vars(Lit,InVars,_),
    find_amg_vars(max_depth,_,InVars,Depth),
        !.

get_in_out_vars(Lit,InVars,OutVars):-
    symbolic(Lit,Symb),
    Lit =.. [_|Vars],
    def_structural(_,Symb,_,_,InPlaces),
    !,
    get_io(Vars,InVars,OutVars,InPlaces,1).

get_in_out_vars(Lit,InVars,[]):-
    Lit =.. [_|InVars].

get_io([],[],[],_,_).
get_io([V|Vars],[V|InVars],OutVars,InPlaces,Place):-
    my_member(Place,InPlaces),
    NextPlace is Place+1,
    get_io(Vars,InVars,OutVars,InPlaces,NextPlace),
    !.

get_io([V|Vars],InVars,[V|OutVars],InPlaces,Place):-
    NextPlace is Place+1,
    get_io(Vars,InVars,OutVars,InPlaces,NextPlace).

literal(Lit,_,All1/Free1,All2/Free2,EqClasses1,EqClasses2):-
        % not Free1 = [],
        def_property(_,Name/Arity,DIn),
        assign_vars(DIn,All1,In,_),
        eq_class_check(In,EqClasses1,EqClasses2,_),
        Lit =.. [Name|In],
        my_del_list(In,Free1,Free2),
        rmv_inst_var(Name,In,All1,All2).

rmv_inst_var(Name,In,All1,All2):- 
    atom_chars(Name,[105,110,115,116,97,110,116,105,97,116,101|_]),
    !,
    my_del_list(In,All1,All2). 
rmv_inst_var(_,_,All,All).

literal(Lit,CurrLength,All1/Free1,All2/Free2,EqClasses,NewEqClasses):-
    add_structural_literal(Free1,CurrLength),
    !,
    def_structural(_,Name/Arity,DIn,DOut,InPlaces),
    assign_vars(DIn,All1,In,Depth),
    Depth1 is Depth+1,
    eq_class_check(In,EqClasses,EqClasses1,EqClRepresent),
    (EqClRepresent = 0 ->
        find_amg_vars(last_eqclass,_,All1,LastEqClass),
        EqClass is LastEqClass + 1,
        NewEqClasses = [[EqClass]|EqClasses1]
        ;
        NewEqClasses = EqClasses1
        ),
    new_outvars(All1,DOut,Depth1,EqClRepresent,Out),
    place_args(In,Out,InPlaces,Args,1),
    Lit =.. [Name|Args],
    my_del_list(In,Free1,Free2a),
    max_input_arity(MaxPropArity),
    (MaxPropArity = 1 ->
        remove_primary_var(All1,All3)
        ;
        All3 = All1
    ),
    my_append(Out,Free2a,Free2),
    my_append(Out,All3,All2).

add_structural_literal(_,_):-
    setting(pruning,off),
    !.
add_structural_literal(FreeVars,CurrLength):- % (pruning rule 2)
    length(FreeVars,NumFreeVars),
    max_length(MaxLength),
    max_input_arity(MaxPropArity),
    div_int_uppr(NumFreeVars,MaxPropArity,LitsNeeded),
    CurrLength < MaxLength - LitsNeeded,
    !.

% (pruning rule 4/2)
% if maximum input arity = 1, then primary key
% var (KV) can be removed from the set of available vars after first 
% usage of a strucural predicate taking KV as input, since all relevant
% properties have been applied on KV (due to ordering) and further 
% chains of vars arising from other structures could not be joined
% due to only unary inputs

remove_primary_var(Vars1,Vars1):-
    setting(pruning,off),
    !.
remove_primary_var(Vars1,Vars2):-
    def_head(_,_,[DKeyVar],_,_), 
    KeyVar =.. [DKeyVar,_,_,_],
    my_del(KeyVar,Vars1,Vars2).

assign_vars([],_,[],0).
assign_vars([T|Types],InVars,[Var|AssVars],Depth):-
    Var =.. [T,_,VarDepth,EqClass],
    my_member(Var,InVars),
    assign_vars(Types,InVars,AssVars,Depth1),
    (VarDepth > Depth1 ->
        Depth = VarDepth
        ;
        Depth = Depth1).

eq_class_check(_,[1],[1],1):-   % (pruning rule 5)
    max_input_arity(1),
    not setting(pruning,off),
    !.
eq_class_check(InVars,EqClasses1,EqClasses2,EqClRepresent):-
    setof(InvolvedClass,
            Na^No^De^Var^InVars^EqClass^(
            my_member(Var,InVars),
            Var =.. [Na,No,De,EqClass],
            my_member(InvolvedClass,EqClasses1),
            my_member(EqClass,InvolvedClass)
            ),
        InvolvedClasses),
    InvolvedClasses = [[EqClRepresent|_]|_],
    merge(InvolvedClasses,EqClasses1,EqClasses2),
    !.
eq_class_check(InVars,EqClasses,EqClasses,0).

merge([_],EqClass,EqClass):-
    !.
merge([A,B|Rest],Classes,Merged):-
    my_append(A,B,AB),
    my_del(A,Classes,Classes1),
    my_del(B,Classes1,Classes2),
    merge([AB|Rest],[AB|Classes2],Merged).

new_outvars(_,[],_,_,[]).
new_outvars(All,[T|Types],Depth,EqClRepresent,[Var|OutVars]):-
    Var =.. [T,Number,Depth,EqClass],
    (EqClRepresent = 0 ->
        find_amg_vars(last_eqclass,_,All,LastEqClass),
        EqClass is LastEqClass + 1
        ;
        (EqClRepresent = -1 ->
            EqClass = 0
            ;
            EqClass = EqClRepresent
        )
    ),
    find_amg_vars(last_varnum,T,All,Last),
    Number is Last+1,
    new_outvars([Var|All],Types,Depth,EqClRepresent,OutVars).

place_args([],[],_,[],_).
place_args([In|InArgs],OutArgs,InPlaces,[In|Args],Place):-
    my_member(Place,InPlaces),
    NextPlace is Place+1,
    place_args(InArgs,OutArgs,InPlaces,Args,NextPlace),
    !.
place_args(InArgs,[Out|OutArgs],InPlaces,[Out|Args],Place):-
    NextPlace is Place+1,
    place_args(InArgs,OutArgs,InPlaces,Args,NextPlace). 

find_amg_vars(_,_,[],0).
find_amg_vars(ToFind,Type,[V|Vars],Max):-
    (var(Type) ->
        V =.. [_,Num,Depth,EqClass]
        ;
        V =.. [Type,Num,Depth,EqClass]
    ),
    (ToFind = last_varnum ->
        Max = Num
        ;
        true
    ),
    (ToFind = last_eqclass ->
        Max = EqClass
        ;
        true
    ),
    (ToFind = max_depth ->
        Max = Depth
        ;
        true
    ),
    find_amg_vars(ToFind,Type,Vars,Max1),
    Max > Max1,
    !.
find_amg_vars(ToFind,Type,[V|Vars],Max):-
    find_amg_vars(ToFind,Type,Vars,Max).

%%%%%%%%%%%%% Export Features %%%%%%%%%%%%%%%%

write_with_vars(Head,BodyList):-
    retractall(var_name(_,_)),
    get_count(C),
    Head =.. [_|Args],
    (setting(head_name,Name) ->
        write(Name)
        ;
        write('f')
    ),
    (setting(feature_num,end_of_name) ->
        write(C),
        write('('),
        write_with_vars1(Args)
        ;
        write('('),
        write(C),
        write(','),
        write_with_vars1(Args)
    ),
    write(')'),
    write(':-'),
    write_with_vars1(BodyList),
    write('.'),
    nl,
    !.

write_with_vars1([]).
write_with_vars1([A|B]):-
    A =.. [Name|Arg],
    (type(Name) ->
        variabilize(Name,Arg,VName),
        write(VName)
        ;
        (Arg = [_|_] ->
            denumber(Name,DName),
            write(DName),
            write('('),
            write_with_vars1(Arg),
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
    write_with_vars1(B).

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

% correction below May 23 2004

variabilize(Name,[Number,Depth,_],VName):-
    var_name((Name,Number/Depth),VName),
    !.
variabilize(Name,[Number,Depth,_],VName):-
    var_chars(VName1),
    cat(VName1,VName,_),
    (var_name(_,VName) -> fail ; true),
    assert(var_name((Name,Number/Depth),VName)),
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

denumber(Name,DName):-
    atom_chars(Name,CName),
    my_reverse(CName,[_,_,_,_|RDCName]),
    my_reverse(RDCName,DCName),
    atom_chars(DName,DCName).

write_features:-
    reset_count, % for feature counting
    get_time(Start),
    write_features1,
    get_time(Stop),
    Time is Stop-Start,
    output(CurOut),
    output(0),
    nl,
    write(Time),
    write(' seconds taken to construct features.'),
    nl,
    output(CurOut),
    report(Time),
    !.
    
report(Time):-
    setting(reporting,on),
    !,
    max_length(MaxLength),
    get_count(C),
    FeatNum is C - 1,
    (exists('featnum.txt') ->
        open('featnum.txt','append',ReportFile1)
        ;
        open('featnum.txt','write',ReportFile1)
    ),
    set_output(ReportFile1),
    write(MaxLength),
    write(' '),
    write(FeatNum),
    nl,
    flush_output,
    close(ReportFile1),
     (exists('time.txt') ->
        open('time.txt','append',ReportFile2)
        ;
        open('time.txt','write',ReportFile2)
    ),
    set_output(ReportFile2),
    write(MaxLength),
    write(' '),       
    RTime is round(Time * 100)/100,
    write(Time),
    nl,
    flush_output,
    close(ReportFile2).
report(_).

write_features1:-
    (dec_name(Name) ->
        feature_clause(Head,NBodyList),
        denumber_bodylist(NBodyList,BodyList),
        output(Curr_output),
        (Curr_output = 0 ->
            true
            ;
                output(smb_output), 
            write((Head:-BodyList)),
            write('.'),
            nl,
            output(Curr_output)
        ),
        write_with_vars(Head,NBodyList),
        fail
        ;
        write('Error: No declarations loaded.'),
        nl,
        !,
        fail
    ).
write_features1.

denumber_bodylist([],[]).
denumber_bodylist([NLit|NBodyList],[Lit|BodyList]):-
    NLit =.. [NName|Args],
    denumber(NName,Name),
    Lit =.. [Name|Args],
    denumber_bodylist(NBodyList,BodyList).

wf:-
    write_features.

create_feature_file:-
    (setting(feature_file,Name) ->
        ff_extension(FfExt),
        cat([Name,FfExt],FileName,_)
        ;
        (dec_name(Name) ->
            ff_extension(FfExt),
            cat([Name,FfExt],FileName,_)
            ;
            write('Error: No declarations loaded.'),
            nl,
            fail
        )
    ),
    cat([Name,'_frs.smb'],SmbFileName,_),
    (fcreate(output,FileName,1) ->  % Yap change
        output(output), 
        fcreate(smb_output,SmbFileName,1),
        write_features,
        output(0),
        fclose(output),
        fclose(smb_output)
        ;
        write('Error creating file.'),
        nl,
        fail
    ).

cff:-
    create_feature_file.

%outcommented lines below not neccessary
%ff_extension('_frs_noinst.pl'):-
%        instantiating,
%        !.
%ff_extension('_frs.pl').

ff_extension('_frs_noinst.pl').

%%%%%%%%%%%% Testing %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test(Max):-
    set(reporting,on),
    test(1,Max).

test(Max,Max).
test(N,Max):- 
    set(clauselength,N),
    s,
    N1 is N + 1,
    test(N1,Max).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%% GENERIC PREDICATES %%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reset_count:-
    retractall(count(_)),
    assert(count(0)).

get_count(C):-
    count(X),
    retractall(count(X)),
    C is X+1,
    assert(count(C)).

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

my_reverse([],[]).
my_reverse([A|B],RBA):-
    my_reverse(B,RB),
    my_append(RB,[A],RBA).

%%%%%%% Yap wrapping %%%%%%%%%%%

div_int_uppr(A,B,C):-
    A/B =:= integer(A/B),
    !,
    C is A/B.
div_int_uppr(A,B,C):-
    C is integer(A/B) + 1.
    
get_time(Time):-
    Time is cputime.

reg(Output,Stream):-
    retractall(outputs(Output,_)),
    assert(outputs(Output,Stream)).

:-   current_output(Stream),
     reg(0,Stream).

cat([A],A,_):-
    !.

cat([A,B],C,_):-
    atom_chars(A,SA),
    atom_chars(B,SB),
    my_append(SA,SB,SAB),
    atom_chars(C,SAB).

fclose(Output):-
    outputs(Output,Stream),
    flush_output,
    close(Stream).

fcreate(Output,Name,_):-
    open(Name,'write',Stream),
    reg(Output,Stream).

output(Output):-
    ground(Output),
    outputs(Output,Stream),
    set_output(Stream),
    !.
   
output(Output):-
    current_output(Stream),
    outputs(Output,Stream).
