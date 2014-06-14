:-dynamic violated_integrity/3, value/2, frame_relations/4, frame_elaboration/4, current_blendoid/1, current_blend/1, integrates/2, domain_concepts/2, hiden/1.


%%	This file contains the approach to the optimality constraints of Conceptual Blending, as published in our 
%%	papers. It also deals with other aspects of the evaluation of a blend, such as Integrity constraint 
%%	violation or instantion of frames.
%%



verify_pressures(NewB):-
	retractall(current_blend(_)),
	retractall(domain_concepts(_,_)),
	assert(current_blend(NewB)),
	initial_calculations(NewB),
	(config(integration_weight, 0), assert(optimality_pressure(NewB, integration, 0));integration(NewB)),
	(config(pattern_completion_weight, 0), assert(optimality_pressure(NewB, pattern_completion, 0));pattern_completion(NewB)),
	(config(topology_weight, 0), config(web, 0), assert(optimality_pressure(NewB, topology, 0));topology(NewB)),
	(config(maximization_vr, 0), assert(optimality_pressure(NewB, maximization_vr, 0));maximization_VR(NewB)),
	(config(intensification_vr, 0), assert(optimality_pressure(NewB, intensification_vr, 0));intensification_VR(NewB)),	
	(config(unpacking, 0), config(web, 0), assert(optimality_pressure(NewB, unpacking, 0));unpacking(NewB)),
	(config(web, 0), assert(optimality_pressure(NewB, web, 0));web(NewB)),
	(config(relevance_weight, 0), assert(optimality_pressure(NewB, relevance, 0));relevance(NewB)),!.


purge_unuseful_projections(TMP, D1, D2, LD1, LD2):-
	findall(projection(TMP, D1, A, A), (projection(TMP, D1, A, A), not(member(A, LD1))),L1),
	findall(projection(TMP, D2, A, A), (projection(TMP, D2, A, A), not(member(A, LD2))),L2),
	 append(L1, L2, L),
	 retractlist(L).

clean_intersection(Domain, D):-
	findall(rel(D, A, Rel, B), rel(Domain, A, Rel, B),L),
	retractlist(L).

retractlist([]).

retractlist([A|Rest]):-
	retract(A),
	retractlist(Rest).

retractlist([A|Rest]):-
	retractall(A),
	retractlist(Rest).
	

count_potential_newrels(_,[],0).

count_potential_newrels(Blendoid, [A/Rel/B|Rest], N1):-
	count_new_pot_rel(Blendoid, A/Rel/B, F),
	count_potential_newrels(Blendoid, Rest, N),
	N1 is N + F.

count_new_pot_rel(Blendoid, A/Rel/B, 2):-
	stats(domain1, D1), stats(domain2, D2),
	projection(Blendoid, D1, XA, A),
	projection(Blendoid, D1, XB, B),
	rel(D1, XA, Rel, XB),
	projection(Blendoid, D2, YA, A),
	projection(Blendoid, D2, YB, B),
	rel(D2, YA, Rel, YB).

count_new_pot_rel(_,_,1).
	

initial_calculations(NewB):- 	    %% let's see what will go in here...
        retractall(optimality_pressure(NewB,_,_)),
        retractall(integrates(_,_)),
	retractall(value(num_rels,_)),
	retractall(value(num_concepts,_)),
	findall(a, rel(NewB,_,_,_),LARels),
	list_of_concepts(NewB, LConcepts),	
	length(LARels, CRels), assert(value(num_rels,CRels)),
	length(LConcepts, CConcepts), assert(value(num_concepts,CConcepts)),
	retractall(stats(frame, _)),
	apply_frames_fix_point(NewB),
	apply_integrity(NewB).

list_of_concepts(Domain, L):-
	domain_concepts(Domain, L).

list_of_concepts(Domain, L):-
	findall(C, rel(Domain, C, _,_), L1),
	findall(C, rel(Domain, _, _,C), L2),
	append(L1, L2, L3),
	elimina_reps(L3, L),!,
	assert(domain_concepts(Domain, L)).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%INTEGRATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


integration(Blend):-
	findall(F, (integrates(Blend, F), F\=query), Fs),
	length(Fs, I), 
	(I=\=0,
	calculate_frames_integration(Blend, Fs, Intersection, XUnion, Sum_of_If),
	config(desintegration_factor, Alpha), 
	calculate_frame_integration(Blend, Intersection, [], Iintersection),
	length(Intersection,VI), 
	length(XUnion, VX), 
	I1 is I - 1, 
	exp(Alpha, I1, VAlpha),
	Divisor is VX + VI,
	(Divisor =\=0, Uncoverage is VX / (VX + VI); Uncoverage=0),  
	Integration is Iintersection + VAlpha * Uncoverage * Sum_of_If;
	Integration=0),
	assert(optimality_pressure(Blend, integration, Integration)).


calculate_frames_integration(_, [], [], [], 0).

calculate_frames_integration(Blend, [Frame], Conds, [], If):-
	frame_relations(Blend, Frame, Conds, Negs),
	calculate_frame_integration(Blend, Conds, Negs, If).

calculate_frames_integration(Blend, [Frame|Fs], Intersection, XUnion, Sum_of_If):-
	calculate_frames_integration(Blend, Fs, IntersectionFs, XUnionFs, Sum_of_IfFs),
	frame_relations(Blend, Frame, Conds, Negs),
	calculate_frame_integration(Blend, Conds, Negs, If),
	Sum_of_If is Sum_of_IfFs + If,
	findall(Rel, (member(Rel, Conds), Rel\=projection(_,_,_)), CondsClean),
	intersection(CondsClean, IntersectionFs, Intersection),
	remove_sublist(Intersection, CondsClean, XU1),
	remove_sublist(Intersection, IntersectionFs, XU2),
	append(XU1, XU2, XU),
	append(XU, XUnionFs, XUnion).


calculate_frame_integration(Blend, Conds, Negs, If):-
	elimina_reps(Conds, Conds2),
	findall(A, (member(A, Conds2), A\=projection(_,_,_)), Conds3),
	length(Conds3, LC),
	value(num_rels, NRels),
	findall(i, (violated_integrity(Blend, IConds, INegs),(member(X, Conds2),member(X, IConds);member(X, Negs),member(X, INegs))), L),
	length(L, I),
	config(integrity_penalty, IP),
	value(num_blendoid_rels,Total),
	Coverage is NRels/Total,
	Penalty is 1 - IP,
	exp(Penalty, I, Discount),
	(NRels=0, If=0;
	If is (LC / NRels) * Discount * (1 + Coverage)/2).

	

	
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PATTERN COMPLETION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pattern_completion(Blend):-
	config(abductive_pattern_completion, Abductive),
	config(integrity_penalty, IP),
	get_uncompleted_frames(Blend, F),
	normal_pattern_completion(Blend, F, NewRelsN1, SatisfiedCondsN, ViolatedIntegrityN),
	elimina_reps(NewRelsN1, NewRelsN),
	union(NewRelsN, SatisfiedCondsN, VerifiedCondsN),
	length(SatisfiedCondsN, SCN),
       	length(NewRelsN, NRN),length(VerifiedCondsN, _VCNBAK), length(ViolatedIntegrityN, VIN),
	VCN is NRN + SCN,
	Penalty is 1 - IP,
	exp(Penalty, VIN, DiscountN),
	(VCN=0, PCN=0; PCN is (1 - (NRN / VCN)) * DiscountN),
	abductive_pattern_completion(Blend, F, NewRelsA, SatisfiedCondsA, ViolatedIntegrityA),
	union(NewRelsA, SatisfiedCondsA, VerifiedCondsA),
	length(NewRelsA, NRA),length(VerifiedCondsA, VCA), length(ViolatedIntegrityA, VIA),
	exp(Penalty, VIA, DiscountA),
	(VCA=0, PCA=0; PCA is (1 - (NRA / VCA)) * DiscountA),
	PatternCompletion is (1 - Abductive) * PCN+ Abductive * PCA,
	assert(optimality_pressure(Blend, pattern_completion, PatternCompletion)).

get_uncompleted_frames(Blend, F3):-
	current_blendoid(TMP),
	findall(frame(Blend, Frame, Conds, [], Add, []), (frame(TMP, Frame, Conds, _Negs, Add, _Del), not(integrates(Blend, Frame))),F1),
	findall(frame(Blend, Frame, Conds, [], Add, []),  (frame(generic, Frame, Conds, _Negs, Add, _Del), not(integrates(Blend, Frame))), F2),
	append(F1, F2, F3).


abductive_pattern_completion(_,[], [], [], []).

abductive_pattern_completion(Blend, [frame(_,_, Conds, _, Add, _)|Fs], NewRels, VerifiedConds, ViolatedIC):-
	abduction_satisfaction(Blend, Add, Conds, Sat, NonSat),!,
	abductive_pattern_completion(Blend, Fs, NewRelsFs, VerifiedCondsFs, ViolatedICFs),
	check_integrity(Blend, Conds, ViolatedIC1),
	union(NewRelsFs, NonSat, NewRels),
	union(VerifiedCondsFs, Sat, VerifiedConds),
	union(ViolatedIC1, ViolatedICFs, ViolatedIC).

abductive_pattern_completion(Blend, [_|Fs], NewRels, VerifiedConds, ViolatedIC):-
	abductive_pattern_completion(Blend, Fs, NewRels, VerifiedConds, ViolatedIC).


abduction_satisfaction(Blend, Add, Conds, Sat, NonSat):-
	one_true(Blend, Add),
	max_satisfaction(Blend, Conds, Sat, NonSat).

	

one_true(_, []):- !, fail.

one_true(Blend, [Add|_]):-
	test_condition(Blend, Add).

one_true(Blend, [_|Rest]):-
	one_true(Blend, Rest).

normal_pattern_completion(_,[], [], [], []).

normal_pattern_completion(Blend, [frame(_, Name, Conds, _, _, _l)|Fs], NewRels, VerifiedConds, ViolatedIC):-
	normal_pattern_completion(Blend, Fs, NewRelsFs, VerifiedCondsFs, ViolatedICFs),
      	(max_satisfaction(Blend, Conds, Sat, NonSat);Sat=[], NonSat=Conds),!,
	check_integrity(Blend, Conds, ViolatedIC1),
	union(NewRelsFs, NonSat, NewRels),
	union(VerifiedCondsFs, Sat, VerifiedConds),
	union(ViolatedIC1, ViolatedICFs, ViolatedIC),
	length(Conds, LengthConds),
	length(NewRels, LengthNewRels),
	(LengthConds=0, Evidence_threshold=0; Evidence_threshold is LengthNewRels/LengthConds),
	assert(value(pattern_completion(Blend, Name, Evidence_threshold), NewRels)).


max_satisfaction(Blend, Conds, Sat, NonSat):-
	member(op(_), Conds),!,
	expand_conds(Blend, Conds, CondsExpanded),
	max_satisfaction(Blend, CondsExpanded, Sat, NonSat),!.

max_satisfaction(Blend, Conds, Sat, NonSat):-
	core_conds(Conds, Core, Conds2),
	satisfies_core(Blend, Core, Sat1, NonSat1),
	%length(Sat1, N),
	max_satisfaction2(0, Blend, Conds2, Sat2, NonSat2),
	append(Sat1, Sat2, Sat),
	append(NonSat1, NonSat2, NonSat).

core_conds([], [], []).

core_conds([Cond|Conds], [Cond|Core], Conds2):-
	Cond=..[_|L],
	list_of_atoms(L),!,
	core_conds(Conds, Core, Conds2).

core_conds([Cond|Conds], Core, [Cond|Conds2]):-
	core_conds(Conds, Core, Conds2).

satisfies_core(_,[],[],[]).

satisfies_core(Blend, [Cond|Rest],[Cond|Sat],NonSat):-
	test_condition(Blend, Cond),!,
	satisfies_core(Blend, Rest, Sat, NonSat).

satisfies_core(Blend, [Cond|Rest],Sat,[Cond|NonSat]):-
       	satisfies_core(Blend, Rest, Sat, NonSat).


max_satisfaction2(_,_,[],[],[]).

max_satisfaction2(N, Blend, Conds,Sat,NonSat):-
	once(max_satisfaction3(N,Blend, Conds, _, _)),
	length(Conds, M),
	N1 is N + 1, N1=<M, 
	max_satisfaction2(N1,Blend, Conds, Sat, NonSat).

max_satisfaction2(N, Blend, Conds,Sat,NonSat):-
	max_satisfaction3(N,Blend, Conds, Sat, NonSat).


	
% max_satisfaction3(_,_,[],[],[]).

max_satisfaction3(0,_,L,[],L).

max_satisfaction3(N, Blend, [Cond|Conds],[Cond|Rest],NonSat):- 
       	test_condition(Blend,Cond),
	N1 is N - 1,
	max_satisfaction3(N1, Blend, Conds, Rest, NonSat).

%max_satisfaction3(N, Blend, [Cond|Conds],Sat,[Cond|NonSat]):-
%	max_satisfaction3(N, Blend, Conds, Sat, NonSat).

check_integrity(_,[],[]).

check_integrity(Blend, [Cond|Conds], [Cond|ViolatedIC1]):-
	findall(LC/LN, (integrity(Blend, Pos, LN), remove(Cond, Pos, LC)), L),
	violates_1_integrityL(Blend, L),
	check_integrity(Blend, Conds, ViolatedIC1).

check_integrity(Blend, [_|Conds], ViolatedIC1):-
	check_integrity(Blend, Conds, ViolatedIC1).



violates_1_integrityL(_,[]):-!,fail.

violates_1_integrityL(Blend, [Pos/Neg|_]):-
	satisfies_pos(Blend, Pos),
	satisfies_negs(Blend, Neg).

violates_1_integrityL(Blend, [_|Rest]):-
	violates_1_integrityL(Blend, Rest).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TOPOLOGY %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

topology(Blend):-
%	findall(a,rel(Blend,_,_,_),L1),
	value(num_blendoid_rels, NRels),
%	length(L1, NRels),
	(NRels=0, Topology=0;
	stats(domain1, D1),
	stats(domain2, D2),
	findall(rel(Blend, A, R, B), (rel(Blend, A, R, B), (rel(D1, A, R, B); rel(D2, A, R, B))), L),
	elimina_reps(L, LE),
	length(LE, U1),
	Topology is U1 / NRels),
	assert(optimality_pressure(Blend, topology, Topology)).

unpack(Domain, A, AD):-
	once(projection(Domain, AD, A)),!.

unpack(_,A,A):-!.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MAXIMIZATION OF VITAL RELATIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


	
maximization_VR(Blend):-
	findall(rel(A,Rel, B), rel(Blend, A, Rel, B), L1),
	current_vital_relations(V),	
	value(num_blendoid_vr,NVR),
	%length(L1, CMB),
	findall(rel(A, Vital, B), (member(Vital, V), member(rel(A,Vital, B), L1)), LV),
%	write(LV),nl,
	length(LV, VI),
	(NVR=0, Maximization_VR=0;Maximization_VR is VI / NVR),
	assert(optimality_pressure(Blend, maximization_vr, Maximization_VR)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% INTENSIFICATION OF VIRTUAL RELATIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


intensification_VR(Blend):-
%	current_blendoid(TMP),
	findall(p(A,B), projection(Blend, _, A, B), Ps),
	config(vital_relations, V),
	rank_vital_relations(V, Ps, RankedV),
	%sum_rank_values(RankedV, SumV1),
	length(Ps, SumV),
	sum_ranked_squares(RankedV, SumV2),
	(SumV=0, Intensification_VR=0;Intensification_VR is SumV2 / (SumV*SumV)),
	assert(optimality_pressure(Blend, intensification_vr, Intensification_VR)).

rank_vital_relations([],_,[]).

rank_vital_relations([V|Vs],Ps, [V/Value|Rest]):-
	findall(v, (member(p(A,B), Ps), m(V, A, B)), List),
	length(List, Value),
	rank_vital_relations(Vs,Ps, Rest).

sum_rank_values([],0).

sum_rank_values([_/Value|Rest],N):-
	sum_rank_values(Rest,N1),
	N is N1 + Value.

sum_ranked_squares([],0).

sum_ranked_squares([_/Value|Rest],N):-
	sum_ranked_squares(Rest,N1),
	N is N1 + Value*Value.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% UNPACKING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unpacking(Blend):-
	list_of_concepts(Blend, LConcepts),
	get_unpacking_sum(Blend, LConcepts, U),
	length(LConcepts, N),
	(N=0, Unpacking is 0;
	Unpacking is U/N),
	assert(optimality_pressure(Blend, unpacking, Unpacking)).	

get_unpacking_sum(_Blend, [], 0).


get_unpacking_sum(Blend, [Concept|Rest], V):-
	stats(domain1, D1),
	stats(domain2, D2),
	projection(Blend, D1, Origin1, Concept),
	projection(Blend, D2, Origin2, Concept),!,
	unpacking_value(Blend, D1, Origin1, Concept, Va),
	unpacking_value(Blend, D2, Origin2, Concept, Vb),	
	(Va=0, Vb=0, Vc is 0;
	VU is (Va+Vb)/2,
	config(unpacking_effort, UE),
	Vc is (1-UE)*VU),
	get_unpacking_sum(Blend, Rest, V1),
	V is V1+Vc.

get_unpacking_sum(Blend, [Concept|Rest], V):-
	stats(domain1, D1),
	stats(domain2, D2),
	projection(Blend, D1, Origin1, Concept),
	not(projection(Blend, D2, _, Concept)),!,
	unpacking_value(Blend, D1, Origin1, Concept, Va),
	get_unpacking_sum(Blend, Rest, V1),
	V is V1 + Va.


get_unpacking_sum(Blend, [Concept|Rest], V):-
	projection(Blend, D2, Origin2, Concept),!,
	unpacking_value(Blend, D2, Origin2, Concept, Vb),	
	get_unpacking_sum(Blend, Rest, V1),
	V is V1+Vb.

get_unpacking_sum(Blend, [_|Rest], V):-
	get_unpacking_sum(Blend, Rest, V).



unpacking_value(Blend, Domain, Origin, Concept, V):-
	findall(Concept/R/B, rel(Domain, Origin, R, B), L1), 	%%% Original concept frame
	findall(B/R/Concept, rel(Domain, B, R, Origin), L2),
	append(L1, L2, LA),
	findall(Concept/R/B, rel(Blend, Concept, R, B), L3),	%%% Blend concept explanation
	findall(B/R/Concept, rel(Blend, B, R, Concept), L4),
	append(L3,L4,LB),
	intersection(LA,LB,LI),
	length(LI, N1),
	length(LA, N2),
	(N2=0, V is 0;
	V is N1/N2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% WEB %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

web(Blend):-
	config(web_alpha, Alpha),
	config(web_beta, Beta),
	optimality_pressure(Blend, topology, Topology),
	optimality_pressure(Blend, unpacking, Unpacking),
	Web is (Alpha * Topology + Beta * Unpacking) / (Alpha + Beta),
	assert(optimality_pressure(Blend, web, Web)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% RELEVANCE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

relevance(Blend):-
	goal_frames([]),
	assert(optimality_pressure(Blend, relevance, 0)).


relevance(Blend):-
	goal_frames(FAll),
	get_goal_commands(FAll, Commands, F),
	findall(Name, integrates(Blend, Name), LRep),
	once(elimina_reps(LRep,L)),
	instanciate(L, F, Uncompleted),!,
%	remove_sublist(L,F, Uncompleted),
	commands_ok(Blend, Commands),
	get_uncompleted_goal_frames(Blend, Uncompleted, UF),
	goal_frames_pattern_completion(Blend, UF, PCN),
	length(UF, U),
	length(F, Size),
	N is Size - U,
	findall(a, violated_integrity(Blend,_,_),VI),
	length(VI,I),
	config(integrity_penalty,IP),
	Penalty is 1 - IP,
	exp(Penalty, I, Discount),		
	Relevance is Discount*(N + 0.7*U*PCN)/Size,
	assert(optimality_pressure(Blend, relevance, Relevance)).

instanciate([], Uncompleted, Uncompleted).

instanciate([FI|FIs], Fs, Uncompleted):-
	remove(FI, Fs, Fsnew),
	instanciate(FIs, Fsnew, Uncompleted).

instanciate([_|FIs], Fs, Uncompleted):-
	instanciate(FIs, Fs, Uncompleted).


commands_ok(_, []).

commands_ok(Blend, [C|Rest]):-
	call(C),
	commands_ok(Blend, Rest).

commands_ok(Blend, [C|Rest]):-
	(violated_integrity(Blend, [goal_command(C)], []);
	assert(violated_integrity(Blend, [goal_command(C)], []))),
	commands_ok(Blend, Rest).	

get_goal_commands([], [], []).


get_goal_commands([{C}|Rest], [C|Commands], Fs):-
	get_goal_commands(Rest, Commands, Fs).


get_goal_commands([F|Rest], Commands, [F|Fs]):-
	get_goal_commands(Rest, Commands, Fs).


goal_frames_pattern_completion(Blend, Uncompleted, PCN):-
	config(integrity_penalty, IP),
	normal_pattern_completion(Blend, Uncompleted, NewRelsN, SatisfiedCondsN, ViolatedIntegrityN),
	union(NewRelsN, SatisfiedCondsN, VerifiedCondsN),
	length(NewRelsN, NRN),length(VerifiedCondsN, VCN), length(ViolatedIntegrityN, VIN),
	Penalty is 1 - IP,
	exp(Penalty, VIN, DiscountN),
	(VCN=0, PCN=0; PCN is (1 - (NRN / VCN)) * DiscountN),
	assert(value(pattern_completion(Blend, all, PCN),NewRelsN)).

get_uncompleted_goal_frames(Blend, Uncompleted, F3):-
	current_blendoid(TMP),
	findall(frame(Blend, Frame, Conds, [], Add, []), (member(Frame, Uncompleted), frame(TMP, Frame, Conds, _Negs, Add, _Del)),F1),
	findall(frame(Blend, Frame, Conds, [], Add, []), (member(Frame, Uncompleted), frame(generic, Frame, Conds, _Negs, Add, _Del)), F2),
	findall(a, (member(Frame, Uncompleted), not(frame(generic, Frame, _Conds, _Negs, Add, _Del))), Fault),!,
	append(F1, F2, F3),
%	length(Uncompleted, V1), 
	(Fault=[]; write('CANNOT FIND GOAL FRAMES:'), nl, nl, write('ASKED:'), write(Uncompleted), nl, write('Found:'), mostra_listanl(F3), read(_)).




goal_frames(F):-
	query(frames, F).  	%The frames are supposed to be designated by a query



goal_frames(F):-		% Just to test... using all the generic space frames...
	findall(Name,frame(generic, Name, _Conds, _Negs, _Add, _Del),F).









%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%frames:
%%Set of features that, satisfied together, achieve a given property/function/class/concept, ...

apply_frames_fix_point(Blend):-
	apply_frames(Blend),
	findall(Add/Del, (frame_elaboration(Blend, _, Add, Del), Add\=[], Del\=[]), Fs),!,
	(unprocessed_knowledge(Blend, Fs),
	elaborate_frames(Blend, Fs,N),!,
	N=1, apply_frames_fix_point(Blend); true).


unprocessed_knowledge(_, [Add/Del|_]):-
	Add\=[]; Del\=[].

unprocessed_knowledge(Blend, [_|Rest]):-
	unprocessed_knowledge(Blend, Rest).
	


apply_frames(Blend):-
	current_blendoid(TMP),
	findall(frame(Blend, Name, Conds, Negs, Add, Del),frame(generic, Name, Conds, Negs, Add, Del),LG),
	findall(frame(Blend, Name, Conds, Negs, Add, Del),frame(TMP, Name, Conds, Negs, Add, Del),LB),
	findall(frame(Blend, Name, Conds, Negs, Add, Del), (goal_frames(L), member(Name, L),
	frame(generic, Name, Conds, Negs, Add, Del)),LGoal),
	append(LG, LB, LS1),
	append(LS1, LGoal, LS),
	apply_framesL(LS, Blend).

apply_frames(_):-
	write('Problem applying frames or no frames found...').

apply_framesL([],_).

apply_framesL([frame(Domain, Name, Conds, Negs, _Add, _Del)|Rest], Blend):-  
	test_frame(Domain, Name, Blend, Conds, Negs),
	apply_framesL(Rest,Blend).

apply_framesL([_|Rest], Blend):-   %%Nao escolhe a solucao optima
	apply_framesL(Rest,Blend).

test_frame(Domain, Name, Blend, _Conds, _Negs):-
	frame(_, Name, Conds, Negs, Add, Del),
	copy_term(Name, NewName),
	satisfies_conds(Blend, Conds, Negs, CE, NE), 
	(CE\=[];NE\=[];!,fail),
	(integrates(Domain, Name);assert(integrates(Domain, Name)),
	assertrel(frame_relations(Domain, Name, CE,NE)),
	assertrel(frame_elaboration(Domain, Name, Add, Del))),
	retry_frame(Domain, NewName, Blend, Conds, Negs, CE).


retry_frame(_, _, _, _, _, []).

retry_frame(Domain, Name, Blend, Conds, Negs, CE):-
	hide_facts(Blend, CE),
	(test_frame(Domain, Name, Blend, Conds, Negs);true),
	unhide_facts.

hide_facts(_, []).

hide_facts(Blend, [Fact|Rest]):-
	Fact=..[A,B,C],
	retract(rel(Blend, B, A, C)),
	assert(hiden(rel(Blend, B, A, C))),
	hide_facts(Blend, Rest).

hide_facts(Blend, [projection(B,C,D)|Rest]):-
	retract(projection(Blend, B, C, D)),
	assert(hiden(projection(Blend, B, C, D))),
	hide_facts(Blend, Rest).

hide_facts(Blend, [_|Rest]):-
	hide_facts(Blend, Rest).

unhide_facts:-
	findall(F, retract(hiden(F)),L),
	assertall(L).

satisfies_conds(Blend, Conds, Negs, CondsExpanded, NegsExpanded):-
	expand_conds(Blend, Conds, CondsExpanded),
	expand_conds_neg(Blend, Negs, NegsExpanded),
	satisfies_pos(Blend, CondsExpanded),   %REDUNDâNCIA...
	satisfies_negs(Blend, NegsExpanded).

expand_conds(Blend, [C|Conds], [C|CondsExpanded]):-
	(C=..[_,_,_]; C= projection(_,_,_)),
	test_condition(Blend, C),
	expand_conds(Blend, Conds, CondsExpanded).

expand_conds(Blend, [{C}|Conds], CondsExpanded):-
	call(C),
	expand_conds(Blend, Conds, CondsExpanded).

expand_conds(Blend, [op(C)|Conds], CondsExpanded2):-
	execute_frame_operator(C, L),
	expand_conds(Blend, Conds, CondsExpanded1),
	append(L, CondsExpanded1, CondsExpanded2).

expand_conds(_,[],[]).

expand_conds_neg(Blend, [C|Conds], [C|CondsExpanded]):-
	(C=..[_,_,_];C=projection(_,_,_)),
	expand_conds_neg(Blend, Conds, CondsExpanded).

expand_conds_neg(Blend, [{C}|Conds], CondsExpanded):-
	call(C),!,
	expand_conds_neg(Blend, Conds, CondsExpanded).

expand_conds_neg(Blend, [{C}|Conds], [{C}|CondsExpanded]):-
	expand_conds_neg(Blend, Conds, CondsExpanded).


expand_conds_neg(Blend, [op(C)|Conds], CondsExpanded2):-
	execute_frame_operator(C, L),!,
	expand_conds_neg(Blend, Conds, CondsExpanded1),
	append(L, CondsExpanded1, CondsExpanded2).



expand_conds_neg(_,[],[]).


satisfies_pos(_,[]).

satisfies_pos(Blend, [C|Conds]):-
	test_condition(Blend, C),
	satisfies_pos(Blend, Conds).

satisfies_negs(_,[]).   

satisfies_negs(Blend, [N|Negs]):-
	not(test_condition(Blend, N)),	
	satisfies_negs(Blend, Negs).	


%%Tests whether the condition C is true. Returns 1 if true , 0 otherwise
%%if condition has the form f([a,b,c], d), its semantics is f(a,d) v f(b,d) v f(c,d) 

test_condition(_, {C}):-
	call(C).

test_condition(Blend, op(F)):-
	execute_frame_operator(F, L),
	satisfies_pos(Blend, L).

test_condition(Blend, projection(D,A,B)):-
	C=..[projection,Blend,D,A,B],
	call(C).
	


test_condition(Blend, C):-
	C=..[R,A,B],
	C2=..[rel,Blend,A,R,B],
	list_of_conds(C2, L),
	make_calls(L).

%%test_condition(_,_, 0).

execute_frame_operator(exists(List), L):-
	convert_to_rels(List, L),!.

%execute_frame_operator(_,[]).



convert_to_rels([],[]).

convert_to_rels([projection/D/X/Y| List], [F|L]):-
	F=..[projection,D,X,Y],
	convert_to_rels(List,L).


convert_to_rels([R/X/Y| List], [F|L]):-
	F=..[R,X,Y],
	convert_to_rels(List,L).


list_of_conds(rel(Blend, A, R, B), L):-
	is_list(A), is_list(B),
	fill_left([rel(Blend,A,R,B)],L1),
	fill_right(L1,L).


list_of_conds(rel(Blend, A, R, B), L):-
	is_list(A),
	fill_left([rel(Blend,A,R,B)],L).
	

list_of_conds(rel(Blend, A, R, B), L):-
	is_list(B),
	fill_right([rel(Blend, A, R, B)], L).

list_of_conds(rel(Blend, A, R, B),[rel(Blend, A, R, B)]):-!.




fill_right([],[]).

fill_right([rel(Blend, A, R, B)|Rest],L):-
	fill_rightL(rel(Blend, A, R, B), L1),
	fill_right(Rest,L2),
	append(L1,L2,L).

fill_rightL(rel(_,_,_,[]),[]).

fill_rightL(rel(Blend, A, R, [Fact|Rest]), [rel(Blend, A, R, Fact)|RestRels]):-
	fill_rightL(rel(Blend, A, R, Rest), RestRels).


fill_left([],[]).

fill_left([rel(Blend, A, R, B)|Rest],L):-
	fill_leftL(rel(Blend, A, R, B), L1),
	fill_left(Rest,L2),
	append(L1,L2,L).

fill_leftL(rel(_,[],_,_),[]).

fill_leftL(rel(Blend, [Fact|Rest], R, B), [rel(Blend, Fact, R, B)|RestRels]):-
	fill_leftL(rel(Blend, Rest, R, B), RestRels).


make_calls([rel(_, A, isaN, A)|_]).

make_calls([rel(_, _, isaN, something)|_]).

make_calls([rel(Blend, A, isaN, B)|_]):-
	atomic(A), atomic(B),			%% Risky...
	make_call(rel(Blend, A,isa, B));
	(make_call(rel(Blend, A, isa, X)),
	make_calls([rel(Blend, X,isaN, B)])).

make_calls([C|_]):-
	make_call(C).

make_calls([_|Rest]):-
	make_calls(Rest).


make_call(rel(G, A, isa, B)):-
	G\=generic,!,
	(call(rel(G, A, isa, B)),!;
	call(rel(generic, A, isa, B))).

make_call(C):-
	call(C).


%Integrity constraints: Each set of constraints is mutually inconsistent. I.e., it consists on a false model.
apply_integrity(Blend):-
	stats(domain1, D1), stats(domain2, D2),
	findall(integrity(Conds, Negs), integrity(D1, Conds, Negs), LB1),
	findall(integrity(Conds, Negs), integrity(D2, Conds, Negs), LB2),
	findall(integrity(Conds, Negs), integrity(generic, Conds, Negs), LG),
	append(LB1, LB2, LB),
	append(LB,LG, LC1),
	elimina_reps(LC1, LC),
	apply_integrityL(LC, Blend).

apply_integrity(_):-
	write('Problem checking integrity or no constraints found...').

apply_integrityL([],_).

apply_integrityL([integrity(Conds, Negs)|LC], Blend):-
	once(satisfies_pos(Blend, Conds)),
	satisfies_negs(Blend, Negs),!,
	(violated_integrity(Blend, Conds, Negs);
	assert(violated_integrity(Blend, Conds, Negs))),
%	write('VIOLATED INTEGRITY '), write(violated_integrity(Blend, Conds, Negs)), 
	apply_integrityL(LC, Blend).


apply_integrityL([_|LC], Blend):- 
	apply_integrityL(LC, Blend).




%%Falta contar com restições estruturais(1to1, 1to*, etc.)



obtain_vital_relations(VFinal):-
	config(vital_relations,L),
	expand_vital_relations(L, V),
	elimina_reps(V, VFinal).

expand_vital_relations([],[]).

expand_vital_relations([V|Vs], L):-
	expand_vital_relation(V, VExpanded),
	expand_vital_relations(Vs, L1),
	append(VExpanded, L1, L).

expand_vital_relation(V, VExpanded):-
	arc(_, V, _, Syns, _),!,
	get_descs(L1, V),
	expand_vital_relations(L1, VExpanded1),
	append([V|Syns],VExpanded1, VExpanded).

expand_vital_relation(V, [V|VExpanded]):-
	get_descs(L1, V),
	expand_vital_relations(L1, VExpanded).


get_descs(L, V):-
	findall([Name|Syns], (arc(_, Name, List, Syns, _), member(V, List)), L1),
	flatten(L1, L).



