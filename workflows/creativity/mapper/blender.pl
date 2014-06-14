:-dynamic rel/4, stats/2,case/4, case/5, projection/3, projection/4, r/4, current_blendoid/1, arc/5,rule/6, frame/6, integrity/3.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	This file is an auxiliary module for the preparation of the blending generation (that is done by the 	
%	Factory). It initializes several variables and dynamic predicates, as well as calculating several things
%	such as the blendoid statistics. It has much code that is not used anymore, it was the actual 
%	implementation of the first version of Divago's blender (as presented in "Knowledge Integration...", 2001)
%	
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

blends(_,_,_,_,0):-
	listing(stats/2).


blends(D1,D2,I1,I2,N):-
	N1 is N - 1,
	retractall(stats(run,_)),
	assert(stats(run, N1)),
	write('run='), write(N1),nl,
	blend(D1,D2,I1,I2, void),
	blends(D1, D2, I1,I2,N1).

%%simple blend -> just for the domain theory
blend(D1,D2):- 
	blend(D1,D2,void,void, void).

blend(D1, D2, V/T):-
	blend(D1, D2, void, void, V/T).
   
%%Dado um d1 + d2 + mapeamento + constraints -> criar blend

blend(D1,D2,Cases1,Cases2, V/T):-
	metaphor(D1,D2, V/T),!,
	blender(D1,D2,Cases1,Cases2,'generic.dt','mappings.tmp').

blend(D1,D2,Cases1,Cases2, void):-
	metaphor(D1,D2),!,
	blender(D1,D2,Cases1,Cases2,'generic.dt','mappings.tmp').


blender(D1,D2,Cases1,Cases2,GenericSpace, M):-
	initialize_blender,
	cleanDomain(tmp),
	load_dyn(GenericSpace),
	retractall(stats(domain1,_)),
	retractall(stats(domain2,_)),
	assert(stats(domain1, D1)),
	assert(stats(domain2, D2)),
	readMapping(M),   
	(config(both, yes),eliminate_redundancy(D1, D2);true),
	readCases([Cases1,Cases2]),
	str_cat(D1,'_',F1),
	str_cat(F1,D2,F2Raw),
	assert(stats(blend_name, F2Raw)),
	version(F2Raw, F2a),
	joinMappingStat(F2a, F2),
	str_cat(F2,'.dt',F),
	(str_cat(F2,'.di',Fcases),(Cases1\=void; Cases2\=void);Fcases=void),
	open(F, write, ID),
	generate_blendoid(D1, D2, BlendID, ID),
	write(ID,'%%	        '),write(ID, 'statistics....'),nl(ID), nl(ID),
	gravaStats(ID),
	close(ID),!,
	prepare_blend_generation,
	generate_blends(BlendID, M, Fcases),
	write('Blends already generated...'),nl,
	write('Going to verify pressures...'),nl,
	mostraStats,
	write('blend finished...'),!.

initialize_blend_generation:-  
	stats(domain1, D1),
	stats(domain2, D2),
	clean_intersection(generic,D1),
	clean_intersection(generic,D2),
	retractall(integrates(_,_)),
	retractall(value(_,_)),
	current_blendoid(TMP),
	retractall(rel(TMP,_,_,_)),
	findall(projection(TMP, A,B,C), projection(TMP, A, B, C), L),
	readProjection(L, tmp),
	list_of_concepts(TMP, LConcepts),	
	list_of_concepts(D1, LConceptsD1),	
	list_of_concepts(D2, LConceptsD2),	
	purge_unuseful_projections(TMP,D1, D2, LConceptsD1,LConceptsD2),
	assert(value(blendoid_concepts, LConcepts)),
	assert(value(d1_concepts, LConceptsD1)),
	assert(value(d2_concepts, LConceptsD2)),
	findall(A/Rel/B, rel(TMP,A,Rel,B),LARels),
	count_potential_newrels(TMP, LARels,CRels),
	findall(a, frame(TMP, _,_,_,_,_), LAFrames),
	obtain_vital_relations(V),
	assert(current_vital_relations(V)),
	findall(A/Rel/B, (rel(TMP, A, Rel,B), member(Rel, V)), LBVR),
	count_potential_newrels(TMP, LBVR,NVR),
	assert(value(num_blendoid_vr, NVR)),
	assert(value(num_blendoid_rels,CRels)),
	length(LConcepts, CC), assert(value(num_blendoid_concepts,CC)),
	length(LAFrames, CFrames), assert(value(num_blendoid_frames, CFrames)),	retractall(stats(frame, _)),
	findall(Do/Na, projection(TMP, Do, Na,_),IS),
	elimina_reps(IS, IS2),
	assert(value(individual_specification,IS2)).


expand_domain(Domain):-
	findall(C, rel(Domain, C, isa,_), L1),
	findall(C, rel(Domain, _, isa, C), L2),
	append(L1, L2, L3),
	elimina_reps(L3, L4),
	expand_concepts(Domain, L4).


expand_concepts(Domain, ConceptList):-
	top_level_concept(Domain, ConceptList, C),
	expand_concept(Domain, C),
	remove(C, ConceptList, ConceptList2),
	expand_concepts(Domain, ConceptList2).

expand_concepts(_, _).


expand_concept(Domain,C):-
	findall(rel(Domain, C, R, B), (rel(Domain, C, isa, A), rel(Domain, A, R, B), R\=isa), L1),
	assertrelall(L1), mostra_listanl(L1),nl,
	findall(rel(Domain, B, R, C), (rel(Domain, C, isa, A), rel(Domain, B, R, A), R\=isa), L2),
	assertrelall(L2), mostra_listanl(L2).


top_level_concept(Domain, ConceptList, C):-
	findall(A/B, (member(A, ConceptList), rel(Domain, A, isa, B)), L),
	get_top_level_concept(L, C).


get_top_level_concept(IsaList, C):-
	findall(A/B, (member(A/B, IsaList), not(member(_/A, IsaList))), RemoveList),
	RemoveList\=[],
	remove_sublist(RemoveList, IsaList, IsaList2),
	get_top_level_concept(IsaList2, C).

get_top_level_concept([C/_|_], C).
	

initialize_blender:-
	retractall(frame_relations(_,_,_,_)),
	retractall(projection(_,_,_)),
	retractall(projection(_,_,_,_)),
	retractall(stats(unmapped1, _)),
	retractall(stats(unmapped2, _)),
	retractall(case(_,_,_,_,_)),
	retractall(case(_,_,_,_)),
	retractall(rel(_,_,_,_)),
	retractall(optimality_pressure(_,_,_)).

has_repeated_projections([projection(_, D,R,_)|Rest], [D/R|Rest2]):-
	member(projection(_,D, R, _), Rest),
	has_repeated_projections(Rest, Rest2).

has_repeated_projections([_|Rest], Rest2):-
	has_repeated_projections(Rest, Rest2).




readProjection(ProjList, New):-
	%length(ProjList, DF),
	findall(rel(_, A, R,B), rel(both, A,R, B), Both1),
     	stats(domain1,D1),
	findall(rel(D1, A, R, B), rel(D1, A, R, B), L1),
	remove_sublist(Both1, L1, L1b),
	projectDomain(L1b, ProjList, New, Result1),
	findall(rel(_, A, R,B), rel(both, A,R, B), Both2),
	stats(domain2,D2),
	findall(rel(D2, A, R, B), rel(D2, A, R, B), L2),
	remove_sublist(Both2, L2, L2b),
	projectDomain(L2b, ProjList, New, Result2),
	append(Result1, Result2, NewDomain),
	elimina_reps(NewDomain, NewDomain2),
	assertall(NewDomain2).

projectDomain([], _,_,[] ).

projectDomain([rel(Domain, A, _, B)|CM], ProjList, New, Result):-
	(member(projection(New, Domain, A, nil),ProjList);
	member(projection(New, Domain, B, nil),ProjList)),!,
	projectDomain(CM, ProjList, New, Result).


projectDomain([rel(Domain, A, R, B)|CM], ProjList, New, [rel(New, AN, R, BN)|Result]):-
	(member(projection(New, Domain, A, AN),ProjList);A=AN),
	(member(projection(New, Domain, B, BN),ProjList);B=BN),
	projectDomain(CM, ProjList, New, Result).


readDomainsAbstraction(Domains,Depth):-
	readDomainsAbstraction2(Domains, Depth),
	get_used_relations(Domains, UsedRels),
	assert_synonyms(Domains,UsedRels).


readDomainsAbstraction2([],_).

readDomainsAbstraction2([D|Rest], Depth):-
	readDomainAbstraction(D, Depth),
	readDomainsAbstraction2(Rest, Depth).


readDomainAbstraction(D1, Depth):-
	str_cat(D1, '.dt',F),
	load_dyn(F),	
	convert_to_rel(D1, Depth),
	spread_transitive_rels(D1).

convert_to_rel(Domain, Depth):-
	retract(r(A,B,C,D)),
	assert_arc_abstractions(A,B,C,D, Depth), 
	convert_to_rel(Domain, Depth).
	

convert_to_rel(_,_).


get_used_relations([], []).

get_used_relations([D1|Rest], UsedRels):-
	findall(Rel, rel(D1, _, Rel,_), L1),
	elimina_reps(L1, LD1), 
	get_used_relations(Rest, LD2),
	append(LD1, LD2, UsedRels).

assert_synonyms([],_).


assert_synonyms([D1|Rest],UsedRels):-
	assert_synonyms2(D1, UsedRels),
	assert_synonyms(Rest, UsedRels).

assert_synonyms2(D1, UsedRels):-
	findall(rel(D1, A, Rel, C), rel(D1, A, Rel, C), Rels),
	get_synonym_rels(Rels, SynRels, UsedRels),
	elimina_reps(SynRels, List),
	assertrelall(List).

get_synonym_rels([], [],_).

get_synonym_rels([rel(D, A, Rel, B)|Rels], SynRels, UsedRels):-
	findall(rel(D, A, Rel2, B), (arc(_, Rel, _, Syns,_), member(Rel2, Syns), memberchk(Rel2, UsedRels)), L1),
	findall(rel(D, A, Rel2, B), (arc(_, Rel3, _, Syns,_), memberchk(Rel, Syns),delete(Syns, Rel, Syns2), member(Rel2, [Rel3|Syns2]), memberchk(Rel2, UsedRels)), L2),
	append(L1, L2, L3),
	get_synonym_rels(Rels, SynRels1, UsedRels),
	append(L3, SynRels1, SynRels).

assert_arc_abstractions(A,B,C,D, 0):-
	rel(A,B,C,D);
	assertrel(rel(A,B,C,D)).

assert_arc_abstractions(A,B,C,D, Depth):-
	get_arc_abstraction(C, NewC, Depth),
	(rel(A,B,NewC,D);
	assertrel(rel(A,B,NewC,D))),
	NewD is Depth - 1,
	assert_arc_abstractions(A,B,C,D, NewD).


get_arc_abstraction(Arc, Arc, 0).

get_arc_abstraction(Arc, NewArc, N):-
	arc(_, Arc, [Arc2|_],_,_),
	N1 is N - 1, 
	get_arc_abstraction(Arc2, NewArc, N1).

get_arc_abstraction(Arc, NewArc, N):-
	arc(_, _, [Arc2|_],Arcs,_),
	member(Arc, Arcs),
	N1 is N - 1, 
	get_arc_abstraction(Arc2, NewArc, N1).

get_arc_abstraction(Arc, Arc, _).

spread_transitive_rels(Domain):-
	findall(T, transitive(T), TList),
	flatten(TList, TransitiveRels),
	get_new_transitive_rels(Domain, TransitiveRels,	NewRels1),
	elimina_reps(NewRels1, NewRels),
	assertrelall(NewRels).

get_new_transitive_rels(_Domain, [], []).

get_new_transitive_rels(Domain, [Rel|Rest], NewRels):-
	findall(rel(Domain, A, Rel, B), rel(Domain, A, Rel, B), L),
	spread_transitive_rel(L, L, NewR1),
	get_new_transitive_rels(Domain, Rest, NewR2),
	append(NewR1, NewR2, NewRels).


spread_transitive_rel([],_,[]).

spread_transitive_rel([rel(Domain, A, Rel, B)|Rest], Rels, NewRels):-
	spread_down(B, B, Domain, Rel, Rels, NewR1),
	spread_up(A, A, Domain, Rel, Rels, NewR2),
	spread_transitive_rel(Rest, Rels, NewR3),
	append(NewR1, NewR2, N1),
	append(N1, NewR3, NewRels).

spread_down(_, _, _, _, [], []).
		
spread_down(A, C, Domain, Rel, Rest, [rel(Domain, X, Rel, A)|Rels]):-
	remove(rel(Domain, X, Rel, C), Rest, Rest2),!,
	spread_down(A, C, Domain, Rel, Rest2, R1),
	spread_down(A, X, Domain, Rel, Rest2, R2),
	append(R1, R2, Rels).

spread_down(_, _, _, _, _, []).

spread_up(_, _, _, _, [], []).
		
spread_up(B, C, Domain, Rel, Rest, [rel(Domain, B, Rel, X)|Rels]):-
	remove(rel(Domain, C, Rel, X), Rest, Rest2),!,
	spread_up(B, C, Domain, Rel, Rest2, R1),
	spread_up(B, X, Domain, Rel, Rest2, R2),
	append(R1, R2, Rels).
	
spread_up(_, _, _, _, _, []).
	

%%Leitura dos dois ficheiros
readDomains([]).

readDomains([D|Rest]):-
	readDomain(D),
	readDomains(Rest).

readDomain(D1):-
	str_cat(D1, '.dt',F),
	load_dyn(F),	
	convert_to_rel(D1).

convert_to_rel(Domain):-
	retract(r(A,B,C,D)),
	(rel(A,B,C,D);
	assertrel(rel(A,B,C,D))),
	convert_to_rel(Domain).
	

convert_to_rel(_).

%%Leitura das bases de casos
readCases([]).

readCases([void,void]).

readCases([D|Rest]):-
	readCaseBase(D),
	readCases(Rest).

readCaseBase(D1):-
	load_dyn(D1).

readCaseBase(_).

%%Leitura do mapeamento

readMapping(M):-
	retractall(m(_,_,_)),
	load_dyn(M).

%%Criacao do novo mapa conceptual
generate_blendoid(D1, D2, Blend, ID):-
	str_cat(D1,'_',Bl1),	
	str_cat(Bl1,D2, BlendRaw),
	version(BlendRaw, Blend),
	write('generating blendoid now...'),nl,
	makeHeader(Blend, ID,D1,D2),
	write(ID, '%%%% compound concepts %%%%'),nl(ID),
	blend_concept_map(Blend, ID),
%	verify_correctness(Blend, ID),
	%%check_generic_frames(Blend),
	write('Transferring unmapped relations...'),nl,
	write(ID, '%%%% both space concepts %%%%'),nl(ID),
	transfer_both(Blend,ID),
	write('"both" transferred'),nl,
	write(ID, '%%%% tenor concepts %%%%'),nl(ID),
	transfer_unmapped(Blend,D1, ID),
	write('tenor concepts transferred'),nl,
	write(ID, '%%%% vehicle concepts %%%%'),nl(ID),
	transfer_unmapped(Blend,D2, ID),
	write('vehicle concepts transferred'),nl,
	write(ID, '%%%% tenor rules %%%%'),nl(ID),
	transfer_rules(Blend, D1, ID),
	write('tenor rules transferred'),nl,
	write(ID, '%%%% vehicle rules %%%%'),nl(ID),
	transfer_rules(Blend, D2, ID),
	write('vehicle rules transferred'),nl,
	readDomains([D1,D2]),
%	write(ID, '%%%% generic frames %%%%'),nl(ID),
%	transfer_frames(Blend, generic, ID),
%	write('generic frames transferred'),nl,
	write(ID, '%%%% tenor frames %%%%'),nl(ID),
	transfer_frames(Blend, D1, ID),
	write('tenor frames transferred'),nl,
	write(ID, '%%%% vehicle frames %%%%'),nl(ID),
	transfer_frames(Blend, D2, ID),
	write('vehicle frames transferred'),nl,
	write(ID, '%%%% tenor integrity constraints %%%%'),nl(ID),
	transfer_integrity_constraints(Blend, D1, ID),
	write('tenor integrity constraints transferred'),nl,
	write(ID, '%%%% vehicle integrity constraints %%%%'),nl(ID),
	transfer_integrity_constraints(Blend, D2, ID),
	write('vehicle integrity constraints transferred'),nl,
	make_generic(Blend, D1, D2).
	
make_generic(_, D1, D2):-
	findall(rel(generic, A, isa, B), rel(D1, A, isa, B),L1),
	findall(rel(generic, A, isa, B), rel(D2, A, isa, B),L2),
	append(L1, L2, L),
	assertrelall(L).

prepare_blend_generation:-
	prepare_generic_space.

prepare_generic_space:-
	load_dyn('generic.dt').
	

generate_blends(Blendoid, M, _IDCases):-				
	readMapping(M),			%% reads cases according to blendoid
	%% blend_cases(Blendoid,IDCases),
	stats(domain1, _D1),stats(domain2, _D2),
	findall(rel(tmp,B,C,D), rel(Blendoid, B, C, D), LR),
	findall(arc(tmp,B,C,D,E), arc(Blendoid, B, C, D, E), LA),
	findall(frame(tmp, A, B, C, D, E), frame(Blendoid, A, B, C, D, E), LF),
	findall(rule(tmp, A, B, C, D, E), rule(Blendoid, A, B, C, D, E), LRL),
	findall(integrity(tmp, A, B), integrity(Blendoid, A, B), LI),
	findall(projection(tmp, D, A, B), projection(D, A, B), LPR),
	other_projections(LPR2),
	assertall(LR),
	assertall(LA),
	assertall(LRL),
	assertall(LF),
	assertall(LI),
	assertall(LPR),
	assertall(LPR2),
	assert(blendID(Blendoid, 0)),
	retractall(current_blendoid(_)),
	assert(current_blendoid(tmp)).

other_projections(L):-
	stats(domain1, D1),
	list_of_concepts(D1, LD1),
	stats(domain2, D2),
	list_of_concepts(D2, LD2),
	list_of_concepts(both, Both),
	retractall(domain_concepts(_,_)),
	remove_sublist(Both, LD1, LD1b),
	remove_sublist(Both, LD2, LD2b),
	findall(projection(tmp, D1, A, A), (member(A,LD1b), not(projection(D1,A,_))), L1),
	findall(projection(tmp, D2, A, A), (member(A,LD2b), not(projection(D2,A,_))), L2),
	append(L1, L2, L3),
	elimina_reps(L3,L).
	

version(BlendRaw,Blend):-
	stats(run, N),
	number_codes(N,NC),
	atom_codes(BlendRaw,BRC),
	append(BRC,NC,B),
	atom_codes(Blend,B).

version(Blend, Blend).

makeHeader(Blend,ID, D1, D2):-
	write(ID, ':-multifile r/4, neg/4, arc/5, rule/6, frame/6, integrity/3.'),nl(ID),
	(arc(_,same_as,_,_,_);
	write(ID, arc(Blend,blended_with,[],[],[])),write(ID,'.'),nl(ID)),
	gravaArcs(Blend,ID,D1),
	gravaArcs(Blend,ID,D2).

gravaArcs(Blend,ID,Domain):-
	retract(arc(Domain,A,B,C,D)),
	retractall(arc(_,A,_,_,_)),
	write(ID, arc(Blend, A,B,C,D)), write(ID, '.'), nl(ID),
	assert(arc(Blend, A, B, C, D)),
	gravaArcs(Blend,ID,Domain).

gravaArcs(_,_,_).

joinMappingStat(F2a, F2):-
	stats(mapping_size,MS),
	number_codes(MS,MSC),
	atom_codes(s,S),
	append(S,MSC,MTMP),
	atom_codes(FTMP,MTMP),
	str_cat(F2a, FTMP, F2).


joinMappingStat(F, F).

gravaStats(ID):-
	findall(Atribute/Value, stats(Atribute, Value),L),
	gravaEmComentario(L,ID),
	nl(ID), nl(ID), nl(ID).

mostraStats:-
	findall(Atribute/Value, stats(Atribute, Value),L),
	mostra_listanl(L).


gravaEmComentario([],_).

gravaEmComentario([A/V|Rest],ID):-
	write(ID, '%%            '), write(ID, A),write(ID, '='), write(ID, V),nl(ID),
	gravaEmComentario(Rest,ID).


blend_concept_map(Blend,ID):-
	retract(m(_VR, C1,C2)),	
	blend_concepts(Blend,C1,C2,ID),
	blend_concept_map(Blend,ID).

blend_concept_map(_,_):-
	findall(A/B/C, projection(A, B, C), L),
	elimina_projeccoes_duplas(L,L1),
	retractAllMembers(L1).

elimina_projeccoes_duplas([],[]).

elimina_projeccoes_duplas([A/B/C|Rest], [projection(A,B,C)|Rest2]):-
	member(A/B/_, Rest),
	elimina_projeccoes_duplas(Rest, Rest2).

elimina_projeccoes_duplas([_|Rest], Rest2):-
	elimina_projeccoes_duplas(Rest, Rest2).


transfer_both(Blend,ID):-
	findall(rel(Blend,B,C,D), rel(both,B,C,D),L),
	elimina_reps(L,L1),
	grava_factos(L1,ID).


transfer_unmapped(Blend, D, ID):-
	findall(rel(D, A, Pred, B), rel(D, A, Pred, B),L),
	transfer_unmappedL(Blend, L,ID).

transfer_unmapped(_,_,_).

transfer_unmappedL(_,[], _).

transfer_unmappedL(Blend, [rel(_D, A, Pred, B)|Rest],ID):-
	write(ID, r(Blend, A, Pred, B)), write(ID, '.'), nl(ID),
	assertrel(rel(Blend, A, Pred, B)),
	transfer_unmappedL(Blend, Rest,ID).


	

blend_concepts(Blend, C1,C2,ID):-
	write('blending concepts '),write(C1),write(' and '), write(C2),nl,
	write(ID, r(Blend, C1, blended_with, C2)),write(ID, '.'),
	stats(domain1,D1),stats(domain2,D2),
	(config(compound, yes), C1\=C2, str_cat(C1,'_',C1tmp), str_cat(C1tmp,C2, NovoC);NovoC=C1),!,
	assert(projection(D1, C1, NovoC)),
	assert(projection(D2, C2, NovoC)),
	assertrel(rel(Blend, C1, blended_with, C2)),
	write(ID, '  %%  '), write(ID, C1), write(ID, '<-M->'), write(ID, C2),nl(ID),
	generate_relations_d1(Blend, C1, C2, ID),
	generate_relations_d2(Blend, C1, C2, ID).

generate_relations_d1(Blend,C1,C2, ID):-
	stats(domain1,D),
	(rel(D,C1,_,_); rel(D, _,_,C1)),
	transfer_relations(D,Blend,C1, C2, ID).

generate_relations_d1(_,_,_,_).

transfer_relations(D1,Blend,C1, C2,ID):-
	retract(rel(D1,C1,Rel,C3)),
	projection(D1, C1, NovoC),
	(config(compound, yes), (rel(Blend,C3,blended_with,C3A);m(_VR, C3,C3A)),C3A\=C3, str_cat(C3,'_',C3Atmp), str_cat(C3Atmp,C3A, NovoC3);NovoC3=C3),
	assertrel(rel(Blend,NovoC,Rel,NovoC3)),
	write(ID, r(Blend,NovoC,Rel,NovoC3)),write(ID, '.'),nl(ID),
	transfer_relations(D1,Blend,C1,C2,ID).


transfer_relations(D1,Blend,C1,C2,ID):-
	retract(rel(D1,C3,Rel,C1)),
	(config(compound, yes), C1\=C2, str_cat(C1,'_',C1tmp), str_cat(C1tmp,C2, NovoC);NovoC=C1),
	(config(compound, yes), (rel(Blend,C3,blended_with,C3A);m(_VR,C3,C3A)),C3A\=C3, str_cat(C3,'_',C3Atmp), str_cat(C3Atmp,C3A, NovoC3);NovoC3=C3),
	assertrel(rel(Blend,NovoC3,Rel,NovoC)),
	write(ID, r(Blend, NovoC3, Rel, NovoC)),write(ID, '.'),nl(ID),
	transfer_relations(D1,Blend,C1,C2,ID).

transfer_relations(_,_,_,_,_).

generate_relations_d2(Blend, C1,C2,ID):-
	stats(domain2,D),
	transfer_unmapped_relations(D,Blend,C1,C2,ID).

generate_relations_d2(_,_,_,_).

transfer_unmapped_relations(D,Blend,C1,C2,ID):-
	findall([C2,Rel,B],rel(D,C2,Rel,B),L1),
	findall([A,Rel,C2],rel(D,A,Rel,C2),L2),
	(config(compound, yes),C1\=C2, str_cat(C1,'_',C1tmp), str_cat(C1tmp,C2, NovoC);NovoC=C1),
	only_unmapped1(NovoC,D,Blend,L1,ID),
	only_unmapped2(NovoC,D,Blend,L2,ID).

transfer_unmapped_relations(A,B,C,D,_):-	
	write('did not transfer anything!'),write('   values:'),write(A),write(','),write(B),write(','),write(C),write(','),write(D),nl.

only_unmapped1(_,_,_,[],_).

only_unmapped1(C1,D,Blend,[[_,_,B]|Rest],ID):-
	(m(VR,B,_);m(VR,_,B)),
	only_unmapped1(C1,D,Blend,Rest,ID).

only_unmapped1(C1,D,Blend,[[C,Rel,B]|Rest],ID):-
	retract(rel(D,C,Rel,B)),
	(config(compound, yes), rel(Blend,A,blended_with,B),B\=A, str_cat(A,'_',Atmp), str_cat(Atmp,B, NovoB);NovoB=B),
	assertrel(rel(Blend,C1,Rel,NovoB)),
	write(ID, r(Blend,C1,Rel,NovoB)),write(ID, '.'),write(ID, '  %% *'), nl(ID),
	sum_stats(unmapped1,1),
	only_unmapped1(C1,D,Blend,Rest,ID).

sum_stats(Type, N):-
	retract(stats(Type, X)),
	NX is X + N,
	assert(stats(Type, NX)).

sum_stats(Type, N):-
	assert(stats(Type, N)).



only_unmapped2(_,_,_,[],_).

only_unmapped2(C1,D,Blend,[[A,_,_]|Rest],ID):-
	(m(VR,A,_);m(VR,_,A)),
	only_unmapped2(C1,D,Blend,Rest,ID).

only_unmapped2(C1,D,Blend,[[A,Rel,C]|Rest],ID):-
	retract(rel(D,A,Rel,C)),
	(config(compound, yes), rel(Blend,B,blended_with, A), A\=B, str_cat(B,'_',Btmp), str_cat(Btmp,A, NovoA);NovoA=A),
	assertrel(rel(Blend,A,Rel,C1)),
	write(ID, r(Blend,NovoA,Rel,C1)),write(ID,'.'),write(ID, '  %% **'), nl(ID),
	sum_stats(unmapped2,1),
	only_unmapped2(C1,D,Blend,Rest,ID).



%%Criacao da nova lista de regras
transfer_rules(Blend, Domain, ID):-
	retract(rule(Domain, Name, Conds, Negs, Add, Del)),
	transform(Domain, Blend, Name, Namet),write('.'),
	transform(Domain, Blend, Conds, Condst),write('.'),
	transform(Domain, Blend, Negs, Negst),write('.'),
	transform(Domain, Blend, Add, Addt),write('.'),
	transform(Domain, Blend, Del, Delt),write('.'),
	assert(rule(Blend, Namet, Condst, Negst, Addt, Delt)),write('.'),
	write(ID,rule(Blend, Namet, Condst, Negst, Addt, Delt)),write(ID,'.    %'), write(ID,'from domain '), write(ID, Domain), nl(ID),write('.'),
	transfer_rules(Blend, Domain, ID),write('!'),nl.

transfer_rules(_,_,_).

transfer_frames(Blend, Domain, ID):-
	frame(Domain, _,_,_,_,_), %%because of a silly bug! It just crashes with a retract of an "absent" fact
	retract(frame(Domain, Name, Conds, Negs, Add, Del)),
%	write('Transfering frame '), write(frame(Domain, Name, Conds, Negs, Add, Del)),
	transform(Domain, Blend, Name, Namet),write('.'),
	transform(Domain, Blend, Conds, Condst),write('.'),
	transform(Domain, Blend, Negs, Negst),write('.'),
	transform(Domain, Blend, Add, Addt),write('.'),
	transform(Domain, Blend, Del, Delt),write('.'),
	assert(frame(Blend, Namet, Condst, Negst, Addt, Delt)),write('.'),
	write(ID,frame(Blend, Namet, Condst, Negst, Addt, Delt)),write(ID,'.    %'), write(ID,'from domain '), write(ID, Domain), nl(ID),write('.'),	
	transfer_frames(Blend, Domain, ID),write('!'),nl.

transfer_frames(_,_,_).

transfer_integrity_constraints(Blend, Domain, ID):-
	integrity(Domain, _,_), %%because of a silly bug! It just crashes with a retract of an "absent" fact
	retract(integrity(Domain, Pos, Negs)),
%	write('Transfering integrity '), write(integrity(Domain, Pos, Negs)),
	transform(Domain, Blend, Pos, Post),write('.'),
	transform(Domain, Blend, Negs, Negst),write('.'),
	assert(integrity(Blend, Post, Negst)),write('.'),
	write(ID,integrity(Blend, Post, Negst)),write(ID,'.    %'), write(ID,'from domain '), write(ID, Domain), nl(ID),write('.'),	
	transfer_integrity_constraints(Blend, Domain, ID),write('!'),nl.

transfer_integrity_constraints(_,_,_).




%%Faltam os separadores: ':', '/', '=', etc.











