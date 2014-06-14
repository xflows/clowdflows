:-dynamic db/2, af/4, used/2, m/3, rel/4, stats/2, cycle/1, r/4.
:- ensure_loaded(utilities).
:- ensure_loaded(config).
:- ensure_loaded(blender).
:- ensure_loaded(constraints).


run:-
	metaphors(dragon,horse,1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MAPPER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Establishes a mapping between Vehicle and Tenor domains
%%
%%  This mapping is achieved in two phases:
%%	1 - Placement of dormant bridges:predicates db/2.
%%	2 - Choice of correct correspondences: predicates m/1.
%%
%%% Estabelece um mapeamento entre os dominios Vehicle e Tenor
%% Esse mapeamento sera' feito em duas fases:
%% 1- colocacao de dormant bridges: predicados db/2.
%% 2- Escolha das correspondencias correctas: predicados m/1.


%% List of defined predicates:
%%
%% metaphors/3
%% metaphor/2
%% clean_garbage/0 - clean previous mappings
%% readUserMappings/0 - load from file or read from keyboard user mappings
%% mapper/4 - load files and starts generating dbs
%% generate_db/4 - Generation of dormant bridges using the triangulation rule (still not the squaring rule...
%% 	 triangulate/3
%% 	 squaring/3
%%   join_homonyms/3 - not being used
%% 	 vital_relations_mapping/3
%% 		change_mapping(Vehicle, Tenor, LChange),
%% 		identity_mapping(Vehicle, Tenor, LIdentity),
%% 		time_mapping(Vehicle, Tenor, LTime),
%% 		space_mapping(Vehicle, Tenor, LSpace),
%% 		cause_effect_mapping(Vehicle, Tenor, LCause_Effect),
%% 		part_whole_mapping(Vehicle, Tenor, LPart_Whole),
%% 		representation_mapping(Vehicle, Tenor, LRepresentation),
%% 		role_mapping(Vehicle, Tenor, LRole),
%% 		analogy_mapping(Vehicle, Tenor, LAnalogy),
%% 		disanalogy_mapping(Vehicle, Tenor, LDisanalogy),
%% 		property_mapping(Vehicle, Tenor, LProperty),
%% 		similarity_mapping(Vehicle, Tenor, LSimilarity),
%% 		category_mapping(Vehicle, Tenor, LCategory),
%% 		intentionality_mapping(Vehicle, Tenor, LIntentionality),
%% 		uniqueness_mapping(Vehicle, Tenor, LUniqueness),
%% floodfill/2 - floodfill algorithm
%% 	 reflow_bridges/6
%% 	 spread_flow/5
%% choose_mapping/3 - awakening dormant bridges
%% rank_cpts/3 - concept count: [a,b,c,d,a,s,a,c,a]=> [a/4,b/1,c/2,d/1]
%% choose_initial/3 - choose initial concept from a list
%% assertM/1 - record metaphorical connections
%% assert_dbs/1 - record dormant_bridges
%% square/4 - squaring rule (not used)
%% eliminate_redundancy/2 - eliminate redundancies in relations.


metaphors(_,_,0).

metaphors(Vehicle, Tenor, N):-
	N1 is N - 1,
	metaphor(Vehicle, Tenor),
	%stats(mapping_size,M),	write('Mapping size='), write(M), read(_),
	!,
	metaphors(Vehicle, Tenor, N1).

max_metaphor(_, _, 0, 0).

max_metaphor(Vehicle, Tenor, Attempts, N):-
	metaphor(Vehicle, Tenor),
	stats(mapping_size, M1),!,
	Next is Attempts - 1,
	max_metaphor(Vehicle, Tenor, Next, M2),
	max(M1,M2,N).

metaphor(_, _):-
	exists_file('custom.map'),
	consult('custom.map'),
	write('USER DEFINED MAPPING...'),nl,
	findall(m(A,B), m(_,A,B),Ms),
	mostra_listanl(Ms),
	length(Ms, Msl),
	retractall(stats(mapping_size,_)),
	assert(stats(mapping_size, Msl)),
	saveM('mappings.tmp').

metaphor(Vehicle, Tenor):-
	config(mapping_type, all),!,
	clean_garbage,
	mapper(Vehicle, Tenor, LT, LS),
	elimina_reps(LT,LT1),
	elimina_reps(LS,LS1),
	write('.'), % 7,
	assertM(LT1),
	assertM(LS1),
 	findall(A/B, m(_,A,B),Ms),
	nl,mostra_listanl(Ms),
 	length(Ms, Msl),
	retractall(stats(mapping_size,_)),
	assert(stats(mapping_size, Msl)),
	saveM('mappings.tmp'),!.

metaphor(Vehicle, Tenor):-
%	config(mapping_type, alignment),!,
	clean_garbage,
	mapper(Vehicle, Tenor, LT, LS),
	elimina_reps(LT,LT1),
	elimina_reps(LS,LS1),
	write('.'), % 7,
 	assert_dbs(LT1),
	assert_dbs(LS1),
	%%readUserMappings,
	floodfill(Vehicle, Tenor),	
	choose_mapping(Vehicle, Tenor,M1),
	delete(M1, m(mseed, mseed),M),
	retractall(rel(_,_,isa,mseed)),
	assertM(M),
 	findall(m, m(_,_,_),Ms),
 	length(Ms, Msl),
	retractall(stats(mapping_size,_)),
	assert(stats(mapping_size, Msl)),
	saveM('mappings.tmp'),!.


metaphor(Vehicle, Tenor, V/T):-
	config(mapping_type, all),
	clean_garbage,
	mapper(Vehicle, Tenor, LT, LS, V/T),
	elimina_reps(LT,LT1),
	elimina_reps(LS,LS1),
	write('.'), % 7
	assertM(LT1),
	assertM(LS1),
	findall(m, m(_,_,_),Ms),
	length(Ms, Msl),
	retractall(stats(mapping_size,_)),
	assert(stats(mapping_size, Msl)),
	saveM('mappings.tmp').

metaphor(Vehicle, Tenor, V/T):-
	config(mapping_type, all),
	clean_garbage,
	mapper(Vehicle, Tenor, LT, LS, V/T),
	elimina_reps(LT,LT1),
	elimina_reps(LS,LS1),
	write('.'), % 7
	assert_dbs(LT1),
	assert_dbs(LS1),
	%%readUserMappings,
	floodfill(Vehicle, Tenor, V/T),	
	choose_mapping(Vehicle, Tenor,M1),
	delete(M1, mseed/mseed, M),
	retractall(rel(_,_,isa,mseed)),
	assertM(M),
	findall(m, m(_,_,_),Ms),
	length(Ms, Msl),
	retractall(stats(mapping_size,_)),
	assert(stats(mapping_size, Msl)),
	saveM('mappings.tmp').


%%Limpeza de mapeamentos anteriores

clean_garbage:-
	retractall(rel(_,_,_,_)),
	retractall(db(_,_)),
	retractall(af(_,_,_,_)),
	retractall(used(_,_)),
	retractall(m(_,_,_)),
	retractall(stats(_,_)),
	retractall(cycle(_)).

%%------------------------------------------------------------------------------------
%% Carrega ou le do teclado mapeamentos do utilizador


readUserMappings:-
	write('Seeded correspondences:'),nl,
	write('|-'),
	read(X), processResponse(X).

processResponse(n):-!.

processResponse(Mapping):-
	Mapping=..[m,A,B],
	assert(m(analogy,A,B)),
	write('|-'),read(X),
	processResponse(X).

processResponse(File):-
	load_dyn(File),
	write('|-'),read(X),
	processResponse(X).

processResponse(_):-
	write('Error: filename not recognized (n=exit; m(analogy,A,B)=seed a correspondence; <filename>=load a file)'),
	write('|-'),read(X),
	processResponse(X).

%%------------------------------------------------------------------------------------
%% Carrega os ficheiros e comeca a gerar db's

mapper(Vehicle, Tenor, LT, LS):-
	initialize_mapper,
	readDomain(generic),
	write('TTTTTTTT'),nl, %%%%%%%%
	readDomainsAbstraction([Vehicle, Tenor],0),
	(config(metablends, no),retractall(rel(_,_,blended_with,_));true),
	(config(mapping_mode, intensional), derive_rules('newfacts.dt', single); true),
	(readDomains([newfacts]);true),
	write('.'),   % 1
	(config(both, yes),eliminate_redundancy(Vehicle, Tenor);true),
	write('.'),   % 2
	generate_db(Vehicle, Tenor, LT,LS),
	write('.'),   % 5
	listing(db/2),
	write('.').   % 6

mapper(Vehicle, Tenor, LT, LS, V/T):-
	initialize_mapper,
	readDomain(generic),
	readDomainsAbstraction([Vehicle, Tenor],0),
	assert(rel(Vehicle, V, isa, mseed)),
	assert(rel(Tenor, T, isa, mseed)),
	(config(metablends, no),retractall(rel(_,_,blended_with,_));true),
	(config(mapping_mode, intensional), derive_rules('newfacts.dt', single); true),
	(readDomains([newfacts]);true),
	write('.'),   % 1
	(config(both, yes),eliminate_redundancy(Vehicle, Tenor);true),
	write('.'),   % 2
	generate_db(Vehicle, Tenor, LT,LS),
	write('.'),   % 5
	listing(db/2),
	write('.').   % 6

initialize_mapper:-
	retractall(m(_,_,_)),
	retractall(stats(unmapped1, _)),
	retractall(stats(unmapped2, _)),
	retractall(case(_,_,_,_,_)),
	retractall(case(_,_,_,_)),
	retractall(integrity(_,_,_)),
	retractall(rule(_,_,_,_,_,_)),
	retractall(rel(_,_,_,_)),
	retractall(frame(_,_,_,_,_,_)),
	assert(case(_,_,_,_)),
	retractall(case(_,_,_,_)),
	retractall(case(_,_,_,_,_)).

%%------------------------------------------------------------------------------------

generate_db(Vehicle, Tenor,LT,LS):-   %%Generation of dormant bridges using the triangulation rule (still not the squaring rule...
	write('.'),  % 3
	triangulate(Vehicle, Tenor,LT1),
	write('.'),  % 4
	squaring(Vehicle, Tenor, LS), write('.'),
%	join_homonyms(Vehicle, Tenor, LT2),
	vital_relations_mapping(Vehicle, Tenor, LT2),
	append(LT1,LT2,LT).

%%------------------------------------------------------------------------------------
%%triangulacao: se existe um conceito Z em ambos os dominios e se esta' ligado a X num e a Y noutro, estabelece uma db entre X e Y

triangulate(Vehicle, Tenor, L):-
	findall(alignment/X/Y,(rel(Vehicle,X,F,Z),rel(Tenor, Y,F,Z)),L1),
	findall(alignment/X/Y,(rel(Vehicle,Z,F,X),rel(Tenor, Z,F,Y)),L2),
	append(L1,L2,L).

%%------------------------------------------------------------------------------------
% %quadrangulacao (minha versao): se existe uma relacao igual a montante e justante entre X num dominio e Y noutro, estabelece uma db entre X e Y

squaring(Vehicle, Tenor, L):-
	findall(alignment/X/Y, (rel(Vehicle, _, R, X), rel(Tenor, _, R, Y), not(member(R,[isa, ako, can_be]))), L1),
	findall(alignment/X/Y, (rel(Vehicle, X, R, _), rel(Tenor, Y, R, _), not(member(R,[isa, ako, can_be]))), L2),
	append(L1, L2, L).

%%------------------------------------------------------------------------------------
%% Junta todos os conceitos representados pela mesma palavra
%join_homonyms(Vehicle, Tenor, L):-
%	findall(X/X, (rel(Vehicle, X, _,_), (rel(Tenor, X, _, _); rel(Tenor, _,_,X)) ), L1),
%	findall(X/X, (rel(Vehicle, _, _,X), (rel(Tenor, X, _, _); rel(Tenor, _,_,X))), L2),
%	append(L1,L2,L).



%%------------------------------------------------------------------------------------
%% Mapeamentos baseados nas vital relations


vital_relations_mapping(Vehicle, Tenor, LT):-
	change_mapping(Vehicle, Tenor, LChange),
	identity_mapping(Vehicle, Tenor, LIdentity),
	append(LChange, LIdentity, L1),
	time_mapping(Vehicle, Tenor, LTime),
	append(L1, LTime, L2),
	space_mapping(Vehicle, Tenor, LSpace),
	append(L2, LSpace, L3),
	cause_effect_mapping(Vehicle, Tenor, LCause_Effect),
	append(L3, LCause_Effect, L4),
	part_whole_mapping(Vehicle, Tenor, LPart_Whole),
	append(L4, LPart_Whole, L5),
	representation_mapping(Vehicle, Tenor, LRepresentation),
	append(L5, LRepresentation, L6),
	role_mapping(Vehicle, Tenor, LRole),
	append(L6, LRole, L7),
	analogy_mapping(Vehicle, Tenor, LAnalogy),
	append(L7, LAnalogy, L8),
	disanalogy_mapping(Vehicle, Tenor, LDisanalogy),
	append(L8, LDisanalogy, L9),
	property_mapping(Vehicle, Tenor, LProperty),
	append(L9, LProperty, L10),
	similarity_mapping(Vehicle, Tenor, LSimilarity),
	append(L10, LSimilarity, L11),
	category_mapping(Vehicle, Tenor, LCategory),
	append(L11, LCategory, L12),
	intentionality_mapping(Vehicle, Tenor, LIntentionality),
	append(L12, LIntentionality, L13),
	uniqueness_mapping(Vehicle, Tenor, LUniqueness),
	append(L13, LUniqueness, LT).


%%%%%%%%% Codigo das vital relations  %%%%%%%%%%%%%5

change_mapping(_Vehicle, _Tenor, []).
	

identity_mapping(Vehicle, Tenor, LIdentity):-
	findall(identity/X/X, (rel(Vehicle, X, _,_), (rel(Tenor, X, _, _); rel(Tenor, _,_,X)) ), L1),
	findall(identity/X/X, (rel(Vehicle, _, _,X), (rel(Tenor, X, _, _); rel(Tenor, _,_,X))), L2),
	append(L1,L2,LIdentity).
	


time_mapping(Vehicle, Tenor, LTime):-
	findall(time/X/Y,((rel(Vehicle, X, R1,_);rel(Vehicle, _, R1,X)), R1\=isa, 
	test_condition(Vehicle, isaN(X, temporal_entity)), 	(rel(Tenor, Y, R2, _);rel(Tenor, _, R2, Y)), test_condition(Tenor, isaN(Y, temporal_entity)), R2\=isa), LTime).
	

space_mapping(Vehicle, Tenor, LSpace):-
	findall(space/X/Y,((rel(Vehicle, X, R1,_);rel(Vehicle, _, R1,X)), R1\=isa, 
	test_condition(Vehicle, isaN(X, spatial_entity)), 	(rel(Tenor, Y, R2, _);rel(Tenor, _, R2, Y)), test_condition(Tenor, isaN(Y, spatial_entity)), R2\=isa), LSpace).


cause_effect_mapping(_Vehicle, _Tenor, []).
	
part_whole_mapping(_Vehicle, _Tenor, []).

representation_mapping(_Vehicle, _Tenor, []).

role_mapping(Vehicle, Tenor, LRole):-
	findall(role/X/Type, 
       ((rel(Vehicle, X, _, _); rel(Vehicle, _, _, X)),(rel(Tenor, Type, _, _); rel(Tenor, _, _, Type)), 
        test_condition(Vehicle, isaN(X, Type))), LLeft), 
	findall(role/Type/X, 
       ((rel(Tenor, X, _, _); rel(Tenor, _, _, X)), (rel(Vehicle, Type, _, _); rel(Vehicle, _, _,Type)), 
	test_condition(Tenor, isaN(X, Type))), LRight), 
	append(LRight, LLeft, LRole).

	

analogy_mapping(_Vehicle, _Tenor, []).

disanalogy_mapping(_Vehicle, _Tenor, []).

property_mapping(_Vehicle, _Tenor, []).

similarity_mapping(_Vehicle, _Tenor, []).

category_mapping(_Vehicle, _Tenor, []).

intentionality_mapping(_Vehicle, _Tenor, []).

uniqueness_mapping(_Vehicle, _Tenor, []).


%%------------------------------Os predicados acima nao estao a ser utilizados--------------------------------



%% Algoritmo de floodfill: comeca num par de conceitos ligados por uma db, depois espalha uma 
%% onda de activacao, que vai decrescendo (a menos que seja reforcada por outras db's)



floodfill(Vehicle, Tenor):-
	findall(X, db(X,_),L),
	rank_cpts([],L,LR),
	write('.'), % 9
	length(L,N),  %%para obter o numero maximo para o random
	choose_initial(N,LR,C),
	(C=void,!, write('no mapping'),nl, write('.');   %%caso nao haja candidatos
	findall(db(A,B),db(A,B),DBS),
	(remove(db(C,Y),DBS,DBS2);DBS2=[]),
	write('.'), % 10
	reflow_bridges(C/Y,Vehicle,Tenor, db(C,Y),DBS2,100)),
	write('.'). %11


floodfill(Vehicle, Tenor, V/T):-
	C=V, Y=T,
	findall(db(A,B),db(A,B),DBS),
	(remove(db(C,Y),DBS,DBS2);DBS2=[]),
	write('.'), % 10
	reflow_bridges(C/Y,Vehicle,Tenor, db(C,Y),DBS2,100),
	write('.'). %11


%%------------------------------------------------------------------------------------
%%Espalha o fluxo pelas dormant bridges. Comeca com uma db entre um C e um Y e espalha pelas ligacoes

reflow_bridges(C/Y, Vehicle, Tenor, db(X,Z),DBS,N):-
%	nl,write('vou espalhar o fluxo, em '),write(db(X,Z)),
	spread_flow(C/Y, Vehicle, Tenor, db(X,Z),N),
	NovoN is 100,
	reflow_bridges2(C/Y,Vehicle,Tenor,DBS,NovoN).

reflow_bridges(_, _, _, _,[],_).  %%Para que?


reflow_bridges2(C/Y,Vehicle,Tenor, DBS,N):-
	findall(db(X,Z),(member(db(X,Z),DBS),af(X,V1,_,_),af(Z,V2,_,_),V1>20,V2>20),L),
	L=[];(L=[db(A,B)|_],
	remove(db(A,B),DBS,DBS2),
	reflow_bridges(C/Y, Vehicle, Tenor, db(A,B),DBS2,N)).


%% Espalha o fluxo por outros conceitos. A activacao vai decrescendo em 90% de cada vez ate' parar quando
%% fica inferior a 20; é dividida igualmente pelos conceitos que a recebem.

spread_flow(Initial, Vc, Tn, db(X,Y), N):-
	findall(Zo,rel(Vc,X,_,Zo),LVo),
	findall(Zi,rel(Vc,Zi,_,X),LVi),
	append(LVi,LVo,LV),
	findall(To,rel(Tn,Y,_,To),LTo),
	findall(Ti,rel(Tn,Ti,_,Y),LTi),
	append(LTo,LTi,LT),
	length(LV,CompLV),
	length(LT,CompLT),
	NovoNTenor is N * 0.9/ CompLT,
	NovoNVeiculo is N * 0.9/CompLV,
%%	nl,write('No veiculo:'),write(LV),
	spread_flow_list(Initial, Vc,LV,NovoNVeiculo),
%%	nl,write('No tenor:'),write(LT),
	spread_flow_list(Initial, Tn,LT,NovoNTenor).

spread_flow_list(_, _, [],_).

spread_flow_list(_,_,_,N):-
	N<3.

spread_flow_list(Initial,Domain, L, N):-
	spread_flow_list2(Initial,Domain, L, N, NewList),
	NovoN is N * 0.9,
	spread_flow_list(Initial, Domain, NewList, NovoN).


spread_flow_list2(_, _, [],_,[]).

spread_flow_list2(_, Domain,[C|_], N, [C]):-
	af(C, V, _, Domain),
	NovoV is (V+N)/2, NovoV=V.


spread_flow_list2(Initial, Domain,[C|Rest], N, NewList):-
	retract(af(C, V, LInitial, Domain)),
	NovoV is (V+N)/2, 	
	(remove(Initial/Vezes,LInitial,LInitial2);LInitial2=LInitial, Vezes=0),
	NovoVezes is Vezes + 1,
	assert(af(C,NovoV,[Initial/NovoVezes|LInitial2],Domain)),
	spread_flow_list2(Initial, Domain, Rest, N, LN),
	findall(Z,rel(Domain,C,_,Z),LD),
	append(LD,LN, NewList).

spread_flow_list2(Initial, Domain,[C|Rest], N, NewList):-
	assert(af(C,N,[Initial/1],Domain)),	
	spread_flow_list2(Initial, Domain, Rest, N, LN),
	findall(Z,rel(Domain,C,_,Z),LD),
	append(LD,LN, NewList).



%%------------------------------------------------------------------------------------
%%Escolha do mapeamento. "Acordamento" (awakening) das dormant bridges

choose_mapping(Vehicle, Tenor,M):-
	findall([A/AA,B/BA],(db(A,B),af(A,AA,_,Vehicle),af(B,BA,_,Tenor)),DBs),
	(DBs=[];
	purge_dbs(DBs,PureDBs),
	write('waking up bridges!...'),nl,
	af(_,_,[X1/Y1/_|_],_),
	awake(Vehicle, Tenor,[[X1/50,Y1/50]|PureDBs],_, M)).

purge_dbs([],[]).

purge_dbs([[_/AA,_/BA]|Rest],M):-	%%db eliminada se
	AA<20;			%%um dos elementos teve activacao media menor que 20
	BA<20;
	(V is AA - BA,
	(V<0,Dif is -V; Dif = V),
	Dif>20),		%%a diferenca de activacao e' maior do que 20
	purge_dbs(Rest,M).

purge_dbs([[A/_,B/_]|Rest],M):-      %%db eliminada se ja' existe uma ligacao M entre os conceitos
	m(analogy,A,B),				%%definida pelo utilizador.
	purge_dbs(Rest,M).


purge_dbs([DB|Rest],[DB|M]):-
	purge_dbs(Rest,M).

%% Para acordar as db's, comeca-se num par de conceitos ligados com uma db e acordam-se os 
%% vizinhos ate' nao haver mais db's ligadas. Entao, salta-se para outro conjunto de db's (ate'
%% estas acabarem).

awake(_,_,[],[],[]).

awake(Vehicle, Tenor,[[mseed/_,mseed/_]|Rest],_U2, M):-  %% Problema com U e U2...
	assert(used(Vehicle,mseed)),
	assert(used(Tenor, mseed)),
	removeallA(mseed,Rest,R1),
	removeallB(mseed,R1,R2),
	awake_near(Vehicle, Tenor,mseed/mseed,R2,_U,M1),
	M=M1.     %%Assim, so' ha' um M1 !!!!!!


awake(Vehicle, Tenor, [[A/_,B/_]|Rest], _U2, [A/B|M]):-  %% Problema com U e U2...
	write(A),write('<--M1-->'),write(B),nl,
	assert(used(Vehicle,A)),
	assert(used(Tenor, B)),
	removeallA(A,Rest,R1),
	removeallB(B,R1,R2),
	awake_near(Vehicle, Tenor,A/B,R2,_U,M1),
	M=M1.     %%Assim, so' ha' um M1 !!!!!!
%%	awake(Vehicle, Tenor,U, U2,M2),
%%	append(M1,M2,M).

%%awake_near(_,[],[],[]).

%%awake_near(A/B,R2,U,M):-
%%	findall(X/Y,(member([X/_,Y/_],R2),rel(_,A,_,X),rel(_,B,_,Y)),L1),
%%	findall(X/Y,(member([X/_,Y/_],R2),rel(_,X,_,A),rel(_,Y,_,B)),L2),
%%	append(L1,L2,L),
%%	write('vizinhos de '),write(A),write(' <--> '),write(B),write(':'),nl,
%%	write(L),
%%	awake_neighbours(L,R2,U,M).

awake_near(V, T,A/B,R2,U,M):-   %%squaring rule!!!
 	findall(X/Y,(rel(V,A,Rel,X),rel(T,B,Rel,Y), not(used(V,X)), not(used(T,Y))),L1),
	findall(X/Y,(rel(V,X,Rel,A),rel(T,Y,Rel,B), not(used(V,X)), not(used(T,Y))),L2),
	append(L1,L2,L),
%%	write('vizinhos de '),write(A),write(' <--> '),write(B),write(':'),nl,
%%	write(L),
	awake_neighbours(V, T,L,R2,U,M).

awake_neighbours(_, _,[],R,R,[]).

awake_neighbours(Vehicle, Tenor, [X/Y|Rest], R2, U, M):-
	(used(Vehicle,X);used(Tenor,Y)),!, 
	awake_neighbours(Vehicle, Tenor,Rest,R2,U,M).

awake_neighbours(Vehicle, Tenor,[mseed/mseed|Rest],R2,U,M):-
	assert(used(Vehicle,mseed)),
	assert(used(Tenor, mseed)),
	removeallA(mseed,Rest,Rest1),
	removeallB(mseed,Rest1,Rest2),
	removeallA(mseed,R2,R21),
	removeallB(mseed,R21,R22),
	awake_near(Vehicle, Tenor, mseed/mseed, R22, R23, M2),
	awake_neighbours(Vehicle, Tenor,Rest2,R23,U,M1),	
	append(M1,M2,M).


awake_neighbours(Vehicle, Tenor,[X/Y|Rest],R2,U,[X/Y|M]):-
	write(X),write('<--M2-->'),write(Y),nl,
	assert(used(Vehicle,X)),
	assert(used(Tenor, Y)),
	removeallA(X,Rest,Rest1),
	removeallB(Y,Rest1,Rest2),
	removeallA(X,R2,R21),
	removeallB(Y,R21,R22),
	awake_near(Vehicle, Tenor, X/Y, R22, R23, M2),
	awake_neighbours(Vehicle, Tenor,Rest2,R23,U,M1),	
	append(M1,M2,M).
	


%%------------------------------------------------------------------------------------
%%Conta conceitos numa lista. Ex: [a,b,c,d,a,s,a,c,a]=> [a/4,b/1,c/2,d/1]


rank_cpts(_,[],[]).

rank_cpts(P,[C|L], Cs):-
	member(C,P),!,
	rank_cpts(P,L,Cs2),
	join([C/10],Cs2, Cs).

rank_cpts(P,[C|L], Cs):-
	rank_cpts(P,L,Cs2),
	join([C/1],Cs2, Cs).

join([],L,L):-!.

join([X/N|L], L1, [X/T|L2]):-
	remove(X/M, L1, L3),!,
	T is N+M,
	join(L,L3,L2).

join([X|L], L1, [X|L2]):-
	join(L,L1,L2).
	


%%Escolhe conceito inicial, de uma lista.

choose_initial(0,_,void).

choose_initial(N,LR,C):-
	write('num de escolhas possiveis:'),write(N),nl,
	repeat,
	random(0,N,Agulha1),
	statistics(cputime,F), X is floor(F), Y is X * 1, Z is Y mod N,
	write('cputime='),write(F),nl,
	write('Z ='),write(Z),nl,
	write('Agulha1='),write(Agulha1),nl,
	Agulha2 is Agulha1+Z,
	Agulha is Agulha2//2,
	verify_choice(Agulha),
%	Agulha is Z*N,
%%	write(LR),
	write('escolha='),write(Agulha),nl,
	extract_concept(0,Agulha, LR, C).


verify_choice(Agulha):-
	stats(choice, Agulha),
	write(' rejeitado...'),nl,
	check_repeat(10), !, fail.

verify_choice(Agulha):-
	assert(stats(choice, Agulha)).


extract_concept(_, _, [], _):-
	write('error extracting concept!'),nl,
	!,fail.

extract_concept(N,Agulha, [C/V|_],C):-
	Top is N+V,
	Agulha < Top.

extract_concept(N,Agulha, [_/V|R],C):-
	Nnovo is N + V,
	extract_concept(Nnovo, Agulha, R, C).
	
saveM(File):-
	%%fopen(ID2,  File, w),
	open(File,write,ID2),
	saveM2(ID2),
	close(ID2).

saveM2(ID2):-
	retract(m(VR, A,B)),
	write(ID2,m(VR, A,B)),write(ID2,'.'),nl(ID2),
	saveM2(ID2).

saveM2(_).



%% Gravacao de ligacoes metaforicas

assertM([]).


assertM([Type/A/B|Rest]):-
	m(Type, A, B),!,
	assertM(Rest).

assertM([Type / A / B|Rest]):-
	assert(m(Type,A,B)),!,
	assertM(Rest).

assertM([A/B|Rest]):-
	m(alignment, A, B),
	assertM(Rest).

assertM([A / B|Rest]):-
	assert(m(alignment,A,B)),
	assertM(Rest).

	
%%Gravacao de dormant_bridges

assert_dbs([]).

assert_dbs([_/X/Y|R]):-
	db(X,Y),
	assert_dbs(R).

assert_dbs([_/X/Y|R]):-
	assert(db(X,Y)),
	assert_dbs(R).

%%------------------------------------------------------------------------------------
%%Squaring rule (nao utilizada)

square(_, _,[],[]).

square(Vehicle, Tenor,[X/Y|Rest],L):-   %%Resolver este caos...		
	findall(Z/T, (rel(Vehicle, X, R, Z),rel(Tenor, Y,R,T),Z\=T),L1),
	findall(Z/T, (rel(Vehicle, Z, R, X),rel(Tenor, T,R,Y),Z\=T),L2),
	append(L1,L2,L3),
	square(Vehicle, Tenor, Rest,L4),
	append(L3,L4,L).

square_all([],_,_,_,[]).

square_all([X/Y|Rest],Vehicle, Tenor, LS, [X/Y|L]):-
	findall(Z/T, (rel(Vehicle, X, R, Z),rel(Tenor, Y,R,T),Z\=T,not(member(Z/T, LS))),L1),
	findall(Z/T, (rel(Vehicle, Z, R, X),rel(Tenor, T,R,Y),Z\=T,not(member(Z/T, LS))),L2),
	append(L1,L2,L3),
	append(L3,LS,L3S),
	append(Rest,L3,RestL3),
	square_all(RestL3,Vehicle, Tenor, L3S, L).

%%------------------------------------------------------------------------------------
%%Eliminar redundancia nas relacoes. Quando existe uma relacao exactamente igual em ambos os dominios, cria-se o dominio "both"

eliminate_redundancy(Vehicle, Tenor):-
	rel(Vehicle,A,B,C),
	retract(rel(Tenor,A,B,C)),
	retract(rel(Vehicle,A,B,C)),
	(rel(both, A, B, C);assert(rel(both,A,B,C))),
	eliminate_redundancy(Vehicle, Tenor).

eliminate_redundancy(_,_).

