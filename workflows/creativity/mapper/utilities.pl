:-dynamic cache/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	This is the file that collects miscelaneous predicates used throughout the project. Since Divago has
%	had many version in three different prolog compilers (one - XSB - gave many problems), there are certainly many 
%	predicates that are not currently being used. 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%-------------------------------------------------------------------
%%              OPERACOES COM LISTAS
%%%-------------------------------------------------------------------

list_of_atoms([A]):-
	atomic(A).

list_of_atoms([A|Rest]):-
	atomic(A),
	list_of_atoms(Rest).

list([_|_]).            %%E' uma lista?

list([]).

generate_list([]).      %%Gera uma lista com tamanho crescente (comeca com zero)

generate_list([_|Rest]):-
    generate_list(Rest).



pick_1(List, Element, ListWithoutElement):-
    length(List, N),
    random(0,N, ElementIndex),
    removeIndex(ElementIndex, List, Element, ListWithoutElement).



removeIndex(0, [A|Rest],A,Rest).

removeIndex(N, [A|List], Element, [A|ListWithoutElement]):-
    N1 is N - 1,
    removeIndex(N1, List, Element, ListWithoutElement).
    



%random(0.5).           

%random(Z):-
%   cputime(CT),
%   R is CT mod 100,
%   Z is R/100.


modulo(X, Y, Z):-
    X > Y,!,
    Z is X - Y.

modulo(X, Y, Z):-
    Z is Y - X.


round(AReal, ATmp):-
    ATmp is floor(AReal),
    Eval is AReal - ATmp,
    Eval < 0.5,!.

round(AReal, ATmp):-
    ATmp is floor(AReal)+1.


remove(_, [], []):-!, fail.       %% remover um elemento de uma lista. Falha se nao remove nenhum

remove(X, [X|L], L):-!.

remove(X, [Y|L], [Y|L2]):-
    remove(X, L, L2),!.

removeall(_,[],[]).         %%remover todos os elementos X de uma lista

removeall(X,[X|L],L1):-!,
    removeall(X,L,L1).

removeall(X,[Y|L],[Y|L1]):-
    removeall(X,L,L1).

/*
length([],0).               %%comprimento de uma lista
length([_|L], N):-
    length(L, M),
    N is M + 1.


append([],L,L).             %%concatenacao de listas

append([X|Rest],L,[X|L1]):-
    append(Rest,L,L1).


member(X,[X|_]).                %%X e' membro de uma lista?

member(X,[_|Rest]):-
    member(X,Rest).
*/
%%Ordenacao de listas


ordena([],[]).              %%ordenacao quicksort. Cada elemento e' do formato A/B, com B inteiro (a chave de ordenacao)
ordena(L,L2):-
    ordena2(L,L2).

ordena2([],[]).

ordena2([X/N],[X/N]).

ordena2([X/N|L],Lfinal):-
    parte_ao_meio(X/N,L, L1, L2),
    ordena2(L1,L1t),
    ordena2(L2,L2t),
    append(L1t,[X/N|L2t],Lfinal).

parte_ao_meio(_/_,[],[],[]).

parte_ao_meio(X/N,[Y/M|Rest],L1,[Y/M|L2]):-
    M>N,
    parte_ao_meio(X/N,Rest,L1,L2).

parte_ao_meio(X/N,[Y/M|Rest],L1b,L2):-
    parte_ao_meio(X/N,Rest,L1,L2),
    append(L1,[Y/M],L1b).

ordenadesc([],[]).          %%Ordena descendentemente (nao utiliza inverse para poupar tempo de processamento)

ordenadesc([X/N],[X/N]).

ordenadesc([X/N|L],Lfinal):-
    parte_ao_meiodesc(X/N,L, L1, L2),
    ordenadesc(L1,L1t),
    ordenadesc(L2,L2t),
    append(L1t,[X/N|L2t],Lfinal).

parte_ao_meiodesc(_/_,[],[],[]).

parte_ao_meiodesc(X/N,[Y/M|Rest],L1,[Y/M|L2]):-
    M<N,
    parte_ao_meiodesc(X/N,Rest,L1,L2).

parte_ao_meiodesc(X/N,[Y/M|Rest],L1b,L2):-
    parte_ao_meiodesc(X/N,Rest,L1,L2),
    append(L1,[Y/M],L1b).

/*
reverse([],[]).         %%Inverte lista

reverse([X|Rest],L):-
    reverse(Rest,L1),
    append(L1,[X],L).
*/
inv_ordena(L,Li):-      %%Ordenacao descendente com base em inversao (mais lenta...)
    ordena(L,Lo),
    reverse(Lo,Li).


elimina_negs([],[]).    %% retira de uma lista todos predicados que foram dados como exemplos negativos

elimina_negs([X|Rest],L):-
    neg(_,X),
    elimina_negs(Rest,L).

elimina_negs([X|Rest],[X|L]):-
    elimina_negs(Rest,L).



elimina_reps([],[]).    %%elimina elementos repetidos numa lista

elimina_reps([X|Rest],L):-
    member(X,Rest),
    elimina_reps(Rest,L).

elimina_reps([X|Rest],[X|L]):-
    elimina_reps(Rest,L).


%%elimina_intercepcao(A,B,L).   
%%elimina da lista B todos os elementos que tambem pertencem a' lista A

elimina_intercepcao([],L,L).    

elimina_intercepcao([A/_|Rest],L,L1):-
    removeall(A,L,L2),
    elimina_intercepcao(Rest,L2,L1).

elimina_intercepcao([[A/_,B/_]|Rest],L,L1):-
    removeallA(A,L,L2),
    removeallB(B,L2,L3),
    elimina_intercepcao(Rest,L3,L1).

elimina_intercepcao([X|Rest],L,L1):-
    removeall(X,L,L2),
    elimina_intercepcao(Rest,L2,L1).

elimina_intercepcao(A,L,L1):-
    removeall(A,L,L2),
    elimina_intercepcao(A,L2,L1).

%%obtem_interseccao(A,B,C).
%%C e' a interseccai de A com B

obtem_interseccao([],_,[]):-!.

obtem_interseccao([A|Rest],L1,[A|L]):-
    member(A,L1),!,
    obtem_interseccao(Rest,L1,L).

obtem_interseccao([_|Rest],L1,L):-
    obtem_interseccao(Rest,L1,L),!.


%%remove_sublist(A,B,C).            
%%remove a sublista A da lista B

remove_sublist([],L,L).         

remove_sublist([X|L1],L2,L):-
    removeall(X,L2,L3),
    remove_sublist(L1, L3, L).



order_sublist_by_size_reverse(L,L3):-           %%ordena as sublistas de L por ordem crescente de tamanho (quicksort)
    order_sublist_by_size(L,L2),
    reverse(L2,L3). 

order_sublist_by_size([],[]):-!.

order_sublist_by_size([X/B],[X/B]):-!.

order_sublist_by_size([X/B|L],Lfinal):-
    parte_ao_meio_by_size(X/B,L, L1, L2),
    order_sublist_by_size(L1,L1t),
    order_sublist_by_size(L2,L2t),
    append(L1t,[X/B|L2t],Lfinal),!.

parte_ao_meio_by_size(_/_,[],[],[]).

parte_ao_meio_by_size(X/B,[Y/BY|Rest],L1,[Y/BY|L2]):-
    length(B,N), length(BY,M),
    M>N,
    parte_ao_meio_by_size(X/B,Rest,L1,L2).

parte_ao_meio_by_size(X/B,[Y/BY|Rest],L1b,L2):-
    parte_ao_meio_by_size(X/B,Rest,L1,L2),
    append(L1,[Y/BY],L1b).

    
%%%Atencao: Lista L e' do tipo [A/B, C/D, ...]
    
tira_todos([],L,L):-!.       %%remove da lista todos os elementos cujos segundos parametros (formato A/B) seja iguais a elementos da lista dada

tira_todos([F|Rest],L,L1):-
    removeall(_/F,L,L2),
    tira_todos(Rest,L2,L1).

retira_proibidos([],L,L):-!.    %%Igual a anterior para formato A/B/C

retira_proibidos([F|Rest],L,L1):-
    removeall(_/F/_,L,L2),
    retira_proibidos(Rest,L2,L1).


%%------------- Specific utilities ------------------
%% removeAllA retira os elementos (formato X/Y) em que X=A
%% removeAllB retira os elementos (formato X/Y) em que Y=B
%% Do mapper

max(A,B,A):-
	A>B,!.

max(_,B,B).


assertrel(A):-
	A,!.

assertrel(A):-
	assert(A).

load_dyn(A):-
	consult(A).

str_cat(A,B,C):-
	atom_concat(A,B,C).

removeallA(_,[],[]).

removeallA(A,[A/_|Rest],RestR):-!,
    removeallA(A,Rest,RestR).


removeallA(A,[[A/_,_/_]|Rest],RestR):-!,
    removeallA(A,Rest,RestR).

removeallA(A,[X|Rest],[X|RestR]):-
    removeallA(A,Rest,RestR).

removeallB(_,[],[]).

removeallB(B,[_/B|Rest],RestR):-!,
    removeallB(B,Rest,RestR).

removeallB(B,[[_/_,B/_]|Rest],RestR):-!,
    removeallB(B,Rest,RestR).

removeallB(B,[X|Rest],[X|RestR]):-
    removeallB(B,Rest,RestR).

obtem_n_primeiros(X, _,[]):-
    X =< 0.

obtem_n_primeiros(N, [X|Rest],[X|Rest2]):-
    N1 is N - 1,
    obtem_n_primeiros(N1, Rest, Rest2).



%%%-------------------------------------------------------------------
%%              OPERACOES DE OUTPUT
%%%-------------------------------------------------------------------

show_domain(Domain):-
    nl,nl,write(' %%%%%%%%%%% Projections %%%%%%%%%%%%%%%%% '),nl,
    findall(projection(D, A, B), projection(Domain, D, A, B), L0),
    mostra_listanl(L0),
    nl,nl,write(' %%%%%%%%%%% Concept Map   %%%%%%%%%%%%%%%%% '),nl,
    findall(rel(Domain, A, R, B), rel(Domain, A, R, B), L1),
    mostra_listanl(L1),
    nl,nl,write(' %%%%%%%%%%% relations %%%%%%%%%%%%%%%%% '),nl,
    findall(arc(Domain, A), arc(Domain, A, _, _, _), L2),
    mostra_listanl(L2),
    nl,nl,write(' %%%%%%%%%%% rules %%%%%%%%%%%%%%%%% '),nl,
    findall(rule(Domain, A, B, C, D, E), rule(Domain, A, B, C, D, E), L3),
    mostra_listanl(L3),
    nl,nl,write(' %%%%%%%%%%% frames %%%%%%%%%%%%%%%%% '),nl,
    findall(frame(Domain, A, B, C, D, E), frame(Domain, A, B, C, D, E), L4),
    mostra_listanl(L4),
    nl,nl,write(' %%%%%%%%%%% integrity constraints %%%%%%%%%%%%%%%%% '),nl,
    findall(integrity(Domain, A, R), integrity(Domain, A, R), L5),
    mostra_listanl(L5),
    nl,nl,write(' %%%%%%%%%%%% optimality measures  %%%%%%%%%%%%%%%%% '),nl,
    (optimality_pressure(Domain, integration, Value1); Value1=0), write('Integration='), write(Value1),
    (optimality_pressure(Domain, pattern_completion, Value2); Value2=0), tab(8), write('PCompletion='), write(Value2),
    (optimality_pressure(Domain, topology, Value4); Value4=0),  tab(8), write('Topology='), write(Value4),
    (optimality_pressure(Domain, maximization_vr, Value5); Value5=0),tab(8), write('Max_VR='), write(Value5),
    (optimality_pressure(Domain, intensification_vr, Value6); Value6=0), tab(8), write('Int_VR='), write(Value6), 
    (optimality_pressure(Domain, unpacking, Value7); Value7=0),tab(8), write('Unpacking='), write(Value7),
    (optimality_pressure(Domain, web, Value8); Value8=0), tab(8), write('Web='), write(Value8),
    (optimality_pressure(Domain, relevance, Value3); Value3=0),  tab(8), write('Relevance='), write(Value3).



transfere_rels(ID):-
    write('.'),
    retract(rel(A,B,C,D)),
    write(ID, rel(A,B,C,D)),
    write(ID, '.'),
    nl(ID),
    transfere_rels(ID).

transfere_rels(_).

grava_factos([],_).

grava_factos([F|Rest],ID):-
    write(ID,F),write(ID,'.'),nl(ID),
    grava_factos(Rest,ID).


save_blend:-      %% para gravar o novo dominio num ficheiro "blend.dat"
    open('blend.dat',write,IDB),
    transfere_rels(IDB),
    close(IDB).

println([]):-
    nl.
    

println([C|Rest]):-
    write(C),
    write(' '),
    println(Rest).

mostra_lista([]).       %%mostra lista no ecran

mostra_lista([X]):-
    write(X).

mostra_lista([X|L]):-
    write(X), write(','),
    mostra_lista(L).

mostra_listanl([]). %%mostra lista, com cada elemento em cada linha

mostra_listanl([X]):-
    write(X).

mostra_listanl([X|L]):-
    write(X), nl,
    mostra_listanl(L).

%%Mostra todas as regras aprendiddas pelo ILP


learned:-
    findall(Arc, arc(Arc,_,_,_),L),
    print_relations(L).

print_relations([]).                

print_relations([Arc|Rest]):-
    findall(F/Body,( F=..[Arc, _, _], clause(F,Body), Body\=true, Body\='!'),Relacoes),
    mostra_relacoes(Relacoes),
    print_relations(Rest).

mostra_relacoes([]).

mostra_relacoes([Head/Body|Rest]):-
    write(Head),
    write(":-"),nl,
    split_predicates(Body, Splitted),
    mostra_conjuncao(Splitted),
    mostra_relacoes(Rest).

mostra_conjuncao([A]):-         
    write(A),
    write('.'),
    nl,
    nl.

mostra_conjuncao([A|Rest]):-
    write(A),
    write(','),
    nl,
    mostra_conjuncao(Rest). 


%% Lista os predicados positivos que sao dedutiveis a partir das regras aprendidas

suspeito:-
    findall(F,(arc(A,_,_,_),(F=..[A,X,Y],fact(F);F=..[A,X,Y],fact(F))),L1),
    findall(F2, (arc(A,_,_,_), (F2=..[A,X,Y],clausula(F2);F2=..[A,Y,X],clausula(F2))), L2),
    elimina_intercepcao(L1,L2,L3),
    elimina_reps(L3,L3a),
    present('I suspect that:',L3a).



%%escreve_lista_log([]).

%%escreve_lista_log([A|Rest]):-
%%  nllog,
%%  writelog(A),
%%  escreve_lista_log(Rest).

%%escreve_lista(ID,[]):-        
%%  write(ID,'.').

%%escreve_lista(ID,[X]):-
%%  write(ID, X),
%%  write(ID, '.').

%%escreve_lista(ID, [X|L]):-
%%  write(ID, X), write(ID, ','),
%%  escreve_lista(ID, L).


depura(Pred):-
    Pred=..L,
    mostra_lista(L),
    nl.


%%%-------------------------------------------------------------------
%%              OPERACOES DE ACESSO E TRANSFORMACAO DE MEMORIA DINAMICA
%%%-------------------------------------------------------------------

retractAllMembers([]).

retractAllMembers([A|Rest]):-
	(retract(A);true),
	retractAllMembers(Rest).


    
cleanDomains([]).


cleanDomains([D|Rest]):-
    cleanDomain(D),
    cleanDomains(Rest).


cleanDomain(D):-
    retractall(rel(D,_,_,_)),
    retractall(arc(D,_,_,_,_)),
    retractall(frame(D,_,_,_,_,_)),
    retractall(rule(D,_,_,_,_,_)),
    retractall(integrity(D,_,_)),
    retractall(frame_relations(D, _,_,_)),
    retractall(projection(D,_,_,_)),
    retractall(optimality_pressure(D,_,_)),
    retractall(origin(D,_)),
    retractall(violated_integrity(D,_,_)),
    retractall(value(pattern_completion(_,_,_),_)).




fact(F):-                   %%F e' facto se corresponder a uma regra com true no corpo ex: foo(X):-true <=> foo(X).
    clause(F,true).

clausula(F,NRef):-          %%Obtem a referencia de uma clausula
    db_ref(F,Body,NRef), Body\=true,
    split_predicates(Body,Corpo),
    instancia_corpo(Corpo).   %%ATENCAO! Sera' que este pode ser?...

clausula(F):-               %%Se F e' clausula, instancia o seu corpo
    clause(F,Body), 
    split_predicates(Body,Corpo),
    instancia_corpo(Corpo).

instancia_lista(Lista, HG,BG):-     
    findall(F,(member(F,Lista),F=HG,instancia_corpo(BG)),L),!,
    L\=[].
    

instancia_corpo(_,T,S,[]):-
    length(S,N),
    N > T + 1,!.

instancia_corpo([],_,S,S).

instancia_corpo(['!'],_,_,[]).

instancia_corpo([true],_,_,[]).


instancia_corpo([F|Rest],T,L,Suspects):-
    fact(F),!,
    instancia_corpo(Rest,T,L,Suspects).

instancia_corpo([F|Rest],T,L,Suspects):-
    instancia_corpo(Rest,T,[F|L],Suspects).

instancia_corpo([]).

instancia_corpo([F|Rest]):-
    fact(F),
    instancia_corpo(Rest).

assertall(L):-
	elimina_reps(L, L1),
	assertall2(L1).

assertall2([]).

assertall2([A|R]):-
    call(A),!,
    assertall2(R).


assertall2([A|R]):-
    assert(A),
    assertall2(R).

assertrelall(L):-
	elimina_reps(L,L1),
	assertrelall2(L1).

assertrelall2([]).

assertrelall2([A|R]):-
    assertrel(A),
    assertrelall2(R).


derive_rules(ID, Mode):-
    write(ID, ':-multifile rel/4, neg/4, arc/5.'),nl(ID),
    write('I am going to try do derive rules...'),nl,
    findall([Domain, N,A,B,C,D], rule(Domain, N,A,B,C,D), L),
    apply_rules([],ID, Mode,L),
    close(ID).

apply_rules(_,_,_,[]).

apply_rules(InArcs, ID, Mode, [R1|Rest]):-   %%Arcs - Any new relations demands discrimination in an arc/4 clause...
    apply_rule(InArcs, OutArcs, ID, Mode, R1),
    apply_rules(OutArcs, ID, Mode, Rest).

apply_rules(Arcs, ID, Mode, [_|Rest]):-
    apply_rules(Arcs, ID, Mode, Rest).



apply_rule(InArcs, OutArcs, ID, single, [Domain, _Name, Conds, Neg, Add, Del]):-
    satisfies_conds(Domain, Conds, Neg, _),
    save_addlist(InArcs, OutArcs, ID, Domain, Add),
    apply_dellist(Domain, Del).


apply_rule(InArcs, OutArcs, ID, any, [_, _, Conds, Neg, Add, Del]):-
    satisfies_conds(_, Conds, Neg, _),
    save_addlist(InArcs, OutArcs, ID,_, Add),
    apply_dellist(_, Del).

apply_rule(InArcs, OutArcs, ID, Blend, [_Domain, _Name, Conds, Neg, Add, Del]):-
    satisfies_conds(Blend, Conds, Neg, _),
    save_addlist(InArcs, OutArcs, ID, Blend, Add),
    apply_dellist(Blend, Del).


save_addlist(Arcs,Arcs,_ID, _Domain, []).

save_addlist(InArcs, OutArcs, ID, Domain, [Fact|Rest]):-
    Fact=..[Rel, A, B], 
    member(Domain/Rel, InArcs), !,
    write(' New fact: '), write(rel(Domain, A, Rel, B)), nl,
    write(ID, rel(Domain, A, Rel, B)),write(ID, '.'), nl(ID),
    save_addlist(InArcs, OutArcs, ID, Domain, Rest).

save_addlist(InArcs, OutArcs, ID, Domain, [Fact|Rest]):-
    Fact=..[Rel, A, B], 
    write(' New fact: '), write(rel(Domain, A, Rel, B)), nl,
    write(ID, arc(Domain, Rel,[],[],[])), write(ID, '.'), nl(ID),
    write(ID, rel(Domain, A, Rel, B)),write(ID, '.'), nl(ID),
    save_addlist([Domain/Rel|InArcs], OutArcs, ID, Domain, Rest).




add_stats(Var, N):-
    retract(stats(Var, N1)),!,
    N2 is N + N1,
    assert(stats(Var, N2)).

add_stats(Var, N):-
    assert(stats(Var, N)).


retractall_nr(P):-
    retract(P),
    fail.

retractall_nr(_P).




%%%-------------------------------------------------------------------
%%              OUTRAS OPERACOES
%%%-------------------------------------------------------------------

descendant(Rel, Rel).

descendant(RelA, RelB):-
	cache(descendant(RelA, RelB)).

descendant(RelA, RelB):-
	cache(neg(descendant(RelA, RelB))),!, fail.

descendant(RelA, RelB):-
	not(cache(descendant(RelA, RelB))),
	arc(_,RelA, H,_,_),
	member(RelC, H),
	descendant(RelC, RelB),
	assertrel(cache(descendant(RelA, RelB))).

descendant(RelA, RelB):-
	not(cache(descendant(RelA, RelB))),
	arc(_,_, H,Syns,_),
	member(RelA, Syns),
	member(RelC, H),
	descendant(RelC, RelB),
	assertrel(cache(descendant(RelA, RelB))).

descendant(RelA, RelB):-
	not(cache(descendant(RelA, RelB))),
	assert(cache(neg(descendant(RelA, RelB)))),!,fail.


join_words(List, R):-
    var(R), list_of_atoms(List),
    concat_atom(List, R),!.

join_words(List, R):-
    name(R, RN),
    join_words_passive(RN, List),!.

/*join_words_active(L,_):-
	member(X,L), 
	var(X),!,fail.
       
join_words_active([N],AN):-
    number(N),!,
    number_codes(N,NC),
    atom_codes(AN,NC).
    

join_words_active([A],A).

join_words_active([A|Rest],R2):-
    number(A),!,
    number_codes(A,AC),
    atom_codes(AA,AC),
    join_words_active(Rest, R1),
    str_cat(AA, R1, R2).


join_words_active([A|Rest],R2):-
    join_words_active(Rest, R1),
    str_cat(A, R1, R2).
*/

join_words_passive([],[]):-!.

join_words_passive(RN, [A|List]):-
    atom(A),!,
    name(A, AN),
    append(AN, NewRN, RN),
    join_words_passive(NewRN, List).

join_words_passive(RN, [A|List]):-
    append(AN, NewRN, RN),
    name(A,AN),
    join_words_passive(NewRN, List).

check_repeat(N):-
    X is N + 1, 
    retract(cycle(X)),!, fail.

check_repeat(N):-
    retract(cycle(X)),!,
    N>X,
    NX is X + 1,
    assert(cycle(NX)).

check_repeat(_N):-
    assert(cycle(0)).
    

number_to_atom(N,A):-
    number_codes(N,NC),
    atom_codes(A,NC).

exp(_,0,1).

exp(X, Y, Z):-
    Y <0,
    Y2 is -Y,
    exp(X,Y2,Z2),
    Z is 1 / Z2.
    
exp(X,Y,Z):-        %% Z=X^Y  --> Sera' que o XSB nao tem uma funcao de potenciacao?????...
    Y2 is Y - 1,
    exp(X, Y2, Z1),
    Z is X * Z1.
/*
atom_number(Atomo, Number):-        %%transforma numero em atomo
    string_integer(S,Number),
    string_atom(S,Atomo).

*/
predicate(K):-
    list(K),!,fail.

predicate(K):-
    K=..L,
    L=[_],!,fail.

predicate(_).


%%processa lista de predicado prolog contidos em {}

process_curly_brackets({H}):-
    process_brackets(H).
    


%% Cria novo dominio "vazio", apagando o anterior

novo:-
    fcopy('toplevelcpts.pro', 'concepts.pro'),
    fcopy('defaultlngmm.pro','longmem.pro'),
    fopen(ID2,  'shortmem.pro', w),
    write(ID2, 'shortmem(term, all).'),nl(ID2),
    fclose(ID2),
    fopen(ID3, 'negs.pro', w),
    write(ID3, 'neg(void,void(void, void)).'), nl(ID3),
    fclose(ID3),
    fopen(ID4, 'rels.pro', w),
    write(ID4, 'arc(isa,[],[],[]).'),nl(ID4),
    fclose(ID4).

%%Simples funcao para copia de ficheiros

fcopy(A,B):-
    fopen(IDA,A,r),
    fopen(IDB,B,w),
    see(IDA),
    tell(IDB),
    fcopy2,
    seen,
    told,
    fclose(IDA),
    fclose(IDB).    


fcopy2:-
    get0(X),
    trata_char(X).

trata_char('!EOF').

trata_char(X):-
    put(X),
    fcopy2.






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
%             Frame Operators
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5



substitute_all(Blend, A, B):-
			findall(rel(Blend, B, R, T), (rel(Blend, A, R, T), not(member(R, [same_as, icon, isa, represents])), retract(rel(Blend, A, R, T))), L1),
			findall(rel(Blend, T, R, B), (rel(Blend, T, R, A), not(member(R, [same_as, icon, isa, represents])), retract(rel(Blend, T, R, A))), L2),
			append(L1, L2, L3),
			assertrelall(L3).	




other_input_domain(D1, D2):-
	(stats(domain1,D2);
	 stats(domain2, D2)),
	D1 \= D2.


%relationArc(Domain, Action):-
%	test_condition_trace(Domain, isaN(Action, action), _).

relationArc(Domain, Action):-
	rel(Domain, Action, actor,_);rel(Domain, Action, actee,_);rel(Domain, Action, result,_).


differentiating_features(A, B, L):-
	cache(differentiating_features(A,B,L)),!.

differentiating_features(A, B, DF):-
	findall(FA/FB, (member(R, [color, shape, made_of, found, property]), relation(A, A, R, FA), relation(B, B, R, FB)), L),
	remove_matching(L, L, DF1),
	simplify(DF1, DF1, DF),!,
	assert(cache(differentiating_features(A,B,DF))).
	
relation(Domain, O, R, D):-
	rel(Domain, O, R, D).

relation(Domain, O, R, nil):-
	not(rel(Domain, O, R, _s)).


simplify([], L, L).

simplify([_/nil|Rest], L, LNew):-
	simplify(Rest, L, LNew).

simplify([nil/_|Rest], L, LNew):-
	simplify(Rest, L, LNew).

simplify([A/B|Rest], L, LNew):-
	remove(A/nil, L, L1), remove(nil/B, L1, L2),
	simplify(Rest, L2, LNew).

simplify([_|Rest], L, LNew):-
	simplify(Rest, L, LNew).


remove_matching([], L, L).

remove_matching([FA/FA|Rest], List, ListFinal):-
	substitute_by_nil(FA, List, List1),
	remove_matching(Rest, List1, ListFinal).

remove_matching([_|Rest],List, ListFinal):-
	remove_matching(Rest, List, ListFinal).

substitute_by_nil(_, [], []).

substitute_by_nil(C, [C/C|Rest], Rest1):-
	substitute_by_nil(C, Rest, Rest1).

substitute_by_nil(C, [C/nil|Rest], Rest1):-
	substitute_by_nil(C, Rest, Rest1).
	
substitute_by_nil(C, [nil/C|Rest], Rest1):-
	substitute_by_nil(C, Rest, Rest1).

substitute_by_nil(C, [C/D|Rest], [nil/D|Rest1]):-
	substitute_by_nil(C, Rest, Rest1).
	
substitute_by_nil(C, [D/C|Rest], [D/nil|Rest1]):-
	substitute_by_nil(C, Rest, Rest1).

substitute_by_nil(C, [D|Rest], [D|Rest1]):-
	substitute_by_nil(C, Rest, Rest1).
