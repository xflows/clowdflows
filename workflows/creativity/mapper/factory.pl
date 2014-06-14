:-dynamic counter/1, current_population/1, origin/2, class/1, monitor/2, query/2, save_name/1, marked/1, s/4, creature_name/1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	This file contains the "factory" module of Divago. Here, we implemented a genetic algorithm (GA) based search 
%	engine that allows the finding of a "selective projection". An individual is a set of projections, each one 
%	corresponding to one concept (one single node in the concept map) of each of the input domains. Each concept 
%	can be projected either to a copy of itself, its counterpart (in the mapping to the other input) and nil (no 
%	projection). The fitness function is based on the eight optimality principles of Conceptual Blending and is 
%	coded in the "contraints" module.
%	The configuration of the GA is specified in the "config.pl" file. 
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


ga(Frames, []):-
	ga(Frames).

ga(Frames, Query):-
	ga(Frames, Query, []).

ga(Frames, QueryPos, QueryNegs):-
	retractall(frame(generic, query,_,_,_,_)),
	retractall(query(facts,_)),
	assert(frame(generic, query, QueryPos, QueryNegs, [query] ,[])),
	assert(query(facts, [QueryPos,QueryNegs])),
	ga([query|Frames]).


ga(Frames):-
	list(Frames),
	retractall(query(frames,_)),
	assert(query(frames, Frames)),
	once(ga).


ga:-
	retractall(cache(_)),
	initialize_blend_generation,
	load_dyn('generic.dt'),
	retractall(optimality_pressure(_,_,_)),!,
	retractall(monitor(generation,_)),
	retractall(monitor(emergentExtra(_),_)),
	retractall(monitor(emergentMissing(_),_)),
	config(popsize, Popsize), 
	config(generations, Generations), 
	evolve_blends(tmp, Popsize, Generations, ind).


evolve_blends(Initial, Popsize, Number, Name):-
	generate_initial_population(Initial, Popsize, Name),
	loop_ga(Initial, Name, Popsize, Number, 0),
	current_population(L),!,
	evaluate(L,LValues),
	msort(LValues, LV1),
	reverse(LV1, [V/D|_]),
	write(V),nl,
	(retract(cache(evaluate_individual(D,_)));true),
	evaluate_individual(D,_),
	show_domain(D),!,nl,
	compare_to_solution(D, Dist, Missing),
	stats(domain1, D1),
	compare_to_input(D, D1, DistD1, ExtraD1, MissingD1),
	stats(domain2, D2),
	compare_to_input(D, D2, DistD2, ExtraD2, MissingD2),	
	(retract(monitor(best_individual, _));true),
	assert(monitor(best_individual, D)),
	(retract(monitor(distance, _));true),
	assert(monitor(distance, Dist)),
	(retract(monitor(missing, _));true),
	assert(monitor(missing, Missing)),
	(retract(monitor(errorD1, _));true),
	assert(monitor(errorD1, DistD1)),
	(retract(monitor(extraD1, _));true),
	assert(monitor(extraD1, ExtraD1)),
	(retract(monitor(missingD1, _));true),
	assert(monitor(missingD1, MissingD1)),
	(retract(monitor(errorD2, _));true),
	assert(monitor(errorD2, DistD2)),
	(retract(monitor(extraD2, _));true),
	assert(monitor(extraD2, ExtraD2)),
	(retract(monitor(missingD2, _));true),
	assert(monitor(missingD2, MissingD2)),
	(save_name(SName); SName='output.dt'),
%	(creature_name(Creature); Creature='creature.txt'),
%	(save_creature(Creature, D);true),
	(save_domain(SName, D, V);true).





loop_ga(_, _, _, 0, _).


loop_ga(Initial, Name, Popsize, Number, ID):-
	retract(current_population(L)),
	retractall(class(_)),
	evaluate(L, LValues),  !,
	natural_selection(LValues, ChosenValues), !,
%	write(ChosenValues),
	ChosenValues=[I/Ind|_], 
	elaborate(Ind),
	nl,nl,nl,
	findall(positives/Conds/negatives/Negs, violated_integrity(Ind, Conds, Negs), Violation_List),
	write('VIOLATED INTEGRITY: '), nl, mostra_listanl(Violation_List), nl, nl,
	findall(rel(Ind, XInd, RelInd, YInd), rel(Ind, XInd, RelInd, YInd), DInd), mostra_listanl(DInd), nl,nl,
	findall(FInd/LInd, frame_relations(Ind, FInd, LInd,_), H), mostra_listanl(H),nl,
	(optimality_pressure(Ind, integration, Value1), write('Integration='), write(Value1),
    	optimality_pressure(Ind, pattern_completion, Value2), tab(8), write('PCompletion='), write(Value2),
        optimality_pressure(Ind, topology, Value4),  tab(8), write('Topology='), write(Value4),                 
	optimality_pressure(Ind, maximization_vr, Value5),tab(8), write('Max_VR='), write(Value5),
        optimality_pressure(Ind, intensification_vr, Value6), tab(8), write('Int_VR='), write(Value6), 
        optimality_pressure(Ind, unpacking, Value7),tab(8), write('Unpacking='), write(Value7),
        optimality_pressure(Ind, web, Value8),tab(8), write('Web='), write(Value8),
        optimality_pressure(Ind, relevance, Value3), tab(8), write('Relevance='), write(Value3);true),
	nl,nl,nl,
	write('Best individual:'), write(I), origin(Ind, Origin), write('( from '), write(Origin), write(')'),nl, 
	compare_to_solution(Ind, Dist, _), write('Distance='), write(Dist),nl,
	stats(domain1, D1), stats(domain2, D2),
	compare_to_input(Ind, D1,_,_,_),
	compare_to_input(Ind, D2,_,_,_),
	(monitor(emergentExtra(Ind),EE); EE=[]),
	(monitor(emergentMissing(Ind),EM); EM=[]),
	write('Emergent structure: added -->'), write(EE), nl,
	write('removed -->'), write(EM), nl,	
	nl,(monitor(stable,ST);ST=0),write('Stall='), write(ST),nl,nl,
	write('next generation...'), write(ID),nl, 
	monitor_evolution(Number, I),
	next_generation(Initial, Popsize, Name, ChosenValues),!,
	current_population(NewL),
	elaborate_domains(NewL),!,
	cleanDomains(L),
%	retractall(value(pattern_completion(_),_)),
	NextA is Number - 1,
	NewID is ID + 1,
	config(stall, Stall),
	(ST > Stall, Next=0, assert(monitor(generation, ID)); I>=1, Next=0,assert(monitor(generation, ID)); Next=NextA),
	loop_ga(Initial, Name, Popsize, Next, NewID).

monitor_evolution(Number, Value):-
	(retract(monitor(last_value, LastV)); LastV=0),
	(retract(monitor(growth_rate, GR)); GR=0),
	(retract(monitor(last_growth, LG)); LG=0),
	(retract(monitor(stable, S));S=0),
	assert(monitor(last_value, Value)),
	GR2 is Value/Number, 
	assert(monitor(growth_rate, GR2)),
	(LastV=Value, 
	 S1 is S + 1, 
	assert(monitor(stable, S1)),
	assert(monitor(last_growth, LG));
	assert(monitor(stable, 0)),
	assert(monitor(last_growth, Value))).

next_generation(Initial, Popsize, Name, ChosenValues):-
	value(individual_specification,IS),
	conform_specification(IS, ChosenValues),
	nl,nl,write('Copy...'),nl,
	get_evo_weights(AS, CS, MutationRate, RS),
	NA is round(Popsize * AS),
	NS is round(Popsize * CS),
%	NM is round(Popsize * MS),
	NR is round(Popsize * RS),
	assexual_reproduction(Initial, NA, Name, ChosenValues),
	write('Crossover...'),nl,
	sexual_reproduction(Initial, NS, Name, ChosenValues),
	write('Random Individual       '),nl,
	random_individual(Initial, NR, Name),
	write('Mutation...'),nl,
	current_population([_|NewL]),
	mutation(Initial, MutationRate, Name, NewL),
	write('Finished mutation').


elaborate_domains([]).

elaborate_domains([Individual|Rest]):-
	elaborate(Individual),
	elaborate_domains(Rest).


get_evo_weights(AS, RS, MS, CS):-   %If there was no growth in the last 0.8 x S (from config) populations, swap Crossover with random
	monitor(stable, N), 
	config(stall, S),
	T is 0.6*S,
	N > T,!,
	config(copy, AS),
	config(crossover, CS),
	config(mutation, MS),
	config(random, RS),
	monitor(stable, N).

get_evo_weights(AS, MS, CS, RS):-   %If there was no growth in the last 0.4 x s populations, swap Crossover with Mutation
	monitor(stable, N), 
	config(stall, S),
	T is 0.4*S,
	N > T,!,
	config(copy, AS),
	config(crossover, CS),
	config(mutation, MS),
	config(random, RS),
	monitor(stable, N).


get_evo_weights(AS, CS, MS, RS):-   
	config(copy, AS),
	config(crossover, CS),
	config(mutation, MS),
	config(random, RS).
	


conform_specification(_,[]).


conform_specification(IS, [_/Ind|Rest]):-
	findall(D/N, projection(Ind, D, N, _),IndS),
	correct_individual(IS, Ind, IndS),
	conform_specification(IS, Rest).


correct_individual(IS, Ind, IndS):-
	length(IS, N),
	length(IndS, N1),
	N\=N1,!,
	obtem_ExtraMissing(IS, IndS, IndSMissing, IndSExtra), 
	write('Extra problem='), write(IndSExtra),nl,
	write('Missing problem='), write(IndSMissing),nl,
	retira_extra(Ind, IndSExtra),
	acrescenta_missing(Ind, IndSMissing),
	findall(projection(Ind, D, NA, MB), retract(projection(Ind, D, NA, MB)),ProjList),
	assert_in_IS_order(IS, ProjList).

correct_individual(IS, Ind, _):-
	findall(projection(Ind, D, NA, MB), retract(projection(Ind, D, NA, MB)),ProjList),
	assert_in_IS_order(IS, ProjList).
 


correct_individual(_, _, _).


retira_extra(_, []).

retira_extra(Ind, [D/A|Rest]):-
	retract(projection(Ind, D, A, _)),
	retira_extra(Ind, Rest).


acrescenta_missing(_, []).

acrescenta_missing(Ind, [D/A|Rest]):-
	assert(projection(Ind, D, A, nil)),
	acrescenta_missing(Ind, Rest).

	
obtem_ExtraMissing([], E, [], E).

obtem_ExtraMissing(M, [], M, []).

obtem_ExtraMissing([A|Rest], L, M, E):-
	remove(A, L, L1),
	obtem_ExtraMissing(Rest, L1, M, E).

obtem_ExtraMissing([A|Rest], L, [A|M], E):-
	obtem_ExtraMissing(Rest, L, M, E).



assert_in_IS_order([],_).

assert_in_IS_order([D/N|IS], L):-
	delete(L, projection(Ind, D, N, M), L2),
	assert(projection(Ind, D, N, M)),
	assert_in_IS_order(IS, L2).
assert_in_IS_order(_,_):-
	write('BIG PROBLEM in INDIVIDUAL SPECIFICATION!!!!'), trace.


		


random_individual(_, N, _):- N<1.

random_individual(Initial, N, Name):-
	NextNumber is N - 1,
	retract(counter(V)),
	NextV is V + 1, 
	concat_atom([Name, V], New),
	assert(origin(New, random)),
	generate_random_individual(Initial, New),
	assert(counter(NextV)),
	(retract(current_population(L)), append(L, [New], L2) ; L2=[New]),
	assert(current_population(L2)),
	random_individual(Initial, NextNumber, Name).

 
assexual_reproduction(_,0,_, _).

assexual_reproduction(_,_,_, []).

assexual_reproduction(Initial,Number, Name, [Value/A|ChosenValues]):-
	NextNumber is Number - 1,
	retract(counter(V)),
	NextV is V + 1, 
	concat_atom([Name, V], New),
	assert(origin(New, copy)),
	copy_domain(A, New),
%	assert(cache(evaluate_individual(New, Value))),
	assert(counter(NextV)),
	(retract(current_population(L)), append(L, [New], L2) ; L2=[New]),
	assert(current_population(L2)),
	assexual_reproduction(Initial, NextNumber, Name, ChosenValues).

sexual_reproduction(_, N, _, _):- N < 1.

sexual_reproduction(_, _, _, [_]).

sexual_reproduction(Initial, Number, Name, ChosenValues):-
	NextNumber is Number - 1,
	retract(counter(V1)),
	NextV is V1 + 2, 
	V2 is V1 + 1,
	join_words([Name, V1], New1),
	join_words([Name, V2], New2),
	assert(origin(New1, crossover)),
	assert(origin(New2, crossover)),
	pick_1(ChosenValues, _/Individual1, ChosenValues2),
	pick_1(ChosenValues2, _/Individual2, _),
	crossover(Individual1, Individual2, New1, New2),
	assert(counter(NextV)),
	(retract(current_population(L)), append(L, [New1,New2], L2) ; L2=[New1, New2]),
	assert(current_population(L2)),
	sexual_reproduction(Initial, NextNumber, Name, ChosenValues).

mutation(_, _, _, []).

mutation(Initial, MutationRate, Name, [Individual|ChosenValues]):-
	make_mutation(Individual, MutationRate),
	mutation(Initial, MutationRate, Name, ChosenValues).


make_mutation(Individual, MutationRate):-			
	findall(projection(Individual, D, A, B), retract(projection(Individual, D, A, B)), ProjList),
%	write('Mutating '), write(Individual),
	random_mutate_N(Individual, MutationRate, ProjList, NewProjList),
	assertall(NewProjList),
	readProjection(NewProjList, _).


replace_projections([],L,L).

replace_projections(_,[],[]).

replace_projections([projection(New, D, A, B)|Projs], [projection(New, D, A, _)|L1],[projection(New,D,A,B)|L2]):-
	replace_projections(Projs, L1, L2).

replace_projections(Projs, [P|L1],[P|L2]):-
	replace_projections(Projs, L1, L2).


random_mutate_N(_, _, [], []).


random_mutate_N(New, MutationRate, [Projection|ProjList], [Projection|NewProjList]):-
	Choice is random(100)/100, 
	Choice>MutationRate, !,
%	write('-'),
	random_mutate_N(New, MutationRate, ProjList, NewProjList).

random_mutate_N(New, MutationRate, [projection(New, Domain, A, AN)|ProjList], [projection(New, Domain, A, NewAN)|NewProjList]):-
%	write('+'),
	get_projection_possibilities(Domain, A, Poss),
	delete(Poss, AN, Possibilities),
	pick_1(Possibilities, NewAN,_),!,
	random_mutate_N(New, MutationRate, ProjList, NewProjList).
	
	
	
	

get_projection_possibilities(Domain, A, [A,nil]):-
	current_blendoid(TMP),
	projection(TMP, Domain, A, A),!.
	
get_projection_possibilities(Domain, A, [A, nil| Rest]):-  %%no a|b!!!!
	stats(domain1, Domain),
	findall(C, m(_,A, C),Rest).

get_projection_possibilities(Domain, A, [A, nil| Rest]):-  %%no a|b!!!!
	stats(domain2, Domain),
	findall(C, m(_,C, A),Rest).


get_projection_possibilities(Domain, A, []):-
	write('NO PROJECTION POSSIBILITIES FOR domain '), write(Domain), write('  concept '), write(A),nl.

substitute_element(P, NewP, [P|L], [NewP|L]):-!.

substitute_element(P, NewP, [Element|L], [Element|NewL]):-
	substitute_element(P, NewP, L, NewL).


crossover(Individual1, Individual2, New1, New2):-
	findall(projection(Individual1, D, A, AN), projection(Individual1, D, A, AN), Proj1),
	findall(projection(Individual2, D, A, AN), projection(Individual2, D, A, AN), Proj2),
	pick_1(Proj1, projection(_,Domain,Element,_),_),
	split_by_two(Proj1, projection(_,Domain,Element,_), Left1, Right1),
	split_by_two(Proj2, projection(_,Domain,Element,_), Left2, Right2),
	append(Left1, Right2, ProjNew1),
	append(Left2, Right1, ProjNew2),
	findall(projection(New1, D, A, B), member(projection(_, D, A, B), ProjNew1),LP1),
	findall(projection(New2, D, A, B), member(projection(_, D, A, B), ProjNew2),LP2),
	assertall(LP1),
	assertall(LP2),
	readProjection(LP1, New1),
	readProjection(LP2, New2).

split_by_two([Element|Rest], Element, [Element], Rest):-!.

split_by_two([], _, [], []).	

split_by_two([A|List], Element, [A|Left], Right):-
	split_by_two(List, Element, Left, Right).

copy_domain(Origin, Destination):-
	findall(rel(Destination, A, R, B), rel(Origin, A, R, B), LR),
	findall(projection(Destination, D, A, B), projection(Origin, D, A, B),LP),
	findall(rule(Destination, Name, A, B, C, D), rule(Origin, Name, A, B, C, D),LRL),
	findall(frame(Destination, Name, A, B, C, D), frame(Origin, Name, A, B, C, D),LF),
	(cache(evaluate_individual(_)),
	findall(monitor(emergentExtra(Destination), Emergent), monitor(emergentExtra(Origin), Emergent), LEmergentE),
	findall(monitor(emergentMissing(Destination), Emergent), monitor(emergentMissing(Origin), Emergent), LEmergentM),
	findall(violated_integrity(Destination, VIP, VIN), violated_integrity(Origin, VIP, VIN), VIS),
	findall(integrates(Destination, A), integrates(Origin, A), INTS),
	findall(frame_relations(Destination, A, B, C), frame_relations(Origin, A, B, C), FRS),
	assertall(LEmergentE),
	assertall(LEmergentM),
	assertall(VIS),
	assertall(FRS),
	assertall(INTS); true),
	assertall(LR),
	assertall(LP),
	assertall(LRL),
	assertall(LF).
	

	
natural_selection(LValues, ChosenValues):-
	sort(LValues, LOrd1),
	reverse(LOrd1, LOrdenada),
	length(LValues, Popsize),
	N is Popsize / 2, 
	obtem_n_primeiros(N, LOrdenada, ChosenValues).
%	biodiversity(ChosenValues1, ChosenValues).  %No Biodiversity for now...

biodiversity(LOrdenada, LEscolhas):-
	findall(a, (class(C),member(X, C), member(_/X, LOrdenada), retract(class(C))),_),
	findall(0/Ind, class([Ind|_]), NewChoices),
	append(LOrdenada, NewChoices, LEscolhas).

evaluate_individual(Individual,Value):-
	cache(evaluate_individual(Individual, Val)),
	(satisfies_minimal_constraints(Individual),Value = Val;
	Value is 0).
	
evaluate_individual(Individual, Value):-
%	check_connectivity(Individual),
	verify_pressures(Individual),
	(satisfies_minimal_constraints(Individual),
%	classify_individual(Individual),
	optimality_pressure(Individual, integration, Value1),
	optimality_pressure(Individual, pattern_completion, Value2),
	optimality_pressure(Individual, relevance, Value3),
	optimality_pressure(Individual, topology, Value4),
	optimality_pressure(Individual, maximization_vr, Value5),
	optimality_pressure(Individual, intensification_vr, Value6),
	optimality_pressure(Individual, unpacking, Value7),
	optimality_pressure(Individual, web, Value8),
	config(integration_weight, IW),
	config(pattern_completion_weight, PCW),
	config(relevance_weight, RW),
	config(topology_weight, TW),
	config(maximization_vr, MVRW),
	config(intensification_vr, IVRW),
	config(unpacking, UW),
	config(web, WW),
	TOTAL is (IW+PCW+RW+TW+MVRW+IVRW+UW+WW),
	get_threshold_penalty(Individual, Penalty),
	findall(a, violated_integrity(Individual,_,_),VIO),
	length(VIO,VIOl),
	config(integrity_penalty,IP),
	Penalty2 is 1 - IP,
	exp(Penalty2, VIOl, Discount),		
	(TOTAL = 0, Value=0;
	Value is Discount*(1-Penalty)*(IW*Value1 + PCW*Value2 + RW*Value3 + Value4*TW + Value5*MVRW+ Value6*IVRW + Value7*UW + Value8*WW)/TOTAL);Value is 0).

evaluate_individual(_, 0).


get_threshold_penalty(Individual, Penalty):-
	optimality_pressure(Individual, integration, Value1),
	optimality_pressure(Individual, pattern_completion, Value2),
	optimality_pressure(Individual, relevance, Value3),
	optimality_pressure(Individual, topology, Value4),
	optimality_pressure(Individual, maximization_vr, Value5),
	optimality_pressure(Individual, intensification_vr, Value6),
	optimality_pressure(Individual, unpacking, Value7),
	optimality_pressure(Individual, web, Value8),
	config(minimal_integration_weight, MValue1),
	config(minimal_pattern_completion_weight, MValue2),
	config(minimal_relevance_weight, MValue3),
	config(minimal_topology_weight, MValue4),
	config(minimal_maximization_vr, MValue5),
	config(minimal_intensification_vr, MValue6),
	config(minimal_unpacking, MValue7),
	config(minimal_web, MValue8),
	obtain_penalty_value([MValue1/Value1,MValue2/Value2,MValue3/Value3,MValue4/Value4,MValue5/Value5,MValue6/Value6,MValue7/Value7,MValue8/Value8], Penalty).

get_threshold_penalty(_, 0).


obtain_penalty_value([],0).


obtain_penalty_value([M/V|Rest],VF):-
	Diff is M - V,
	Diff > 0,!,
	P is (1-V/M),
	obtain_penalty_value(Rest,V1),	
	%max(V1, P, VF).
	VF is (P+V1)/2.

obtain_penalty_value([_|Rest],VF):-
	obtain_penalty_value(Rest,VF).



satisfies_minimal_constraints(_).

reevaluate_individual(NewB, Value):-
	(config(integration_weight, 0), assert(optimality_pressure(NewB, integration, 0));integration(NewB)),
	(config(pattern_completion_weight, 0), assert(optimality_pressure(NewB, pattern_completion, 0));pattern_completion(NewB)),
	(config(topology_weight, 0), assert(optimality_pressure(NewB, topology, 0));topology(NewB)),
	(config(maximization_vr, 0), assert(optimality_pressure(NewB, maximization_vr, 0));maximization_VR(NewB)),
	(config(intensification_vr, 0), assert(optimality_pressure(NewB, intensification_vr, 0));intensification_VR(NewB)),	
	(config(unpacking, 0), assert(optimality_pressure(NewB, unpacking, 0));unpacking(NewB)),
	(config(web, 0), assert(optimality_pressure(NewB, web, 0));web(NewB)),
	(config(relevance, 0), assert(optimality_pressure(NewB, relevance, 0));relevance(NewB)),!,
	optimality_pressure(NewB, integration, Value1),
	optimality_pressure(NewB, pattern_completion, Value2),
	optimality_pressure(NewB, relevance, Value3),
	optimality_pressure(NewB, topology, Value4),
	optimality_pressure(NewB, maximization_vr, Value5),
	optimality_pressure(NewB, intensification_vr, Value6),
	optimality_pressure(NewB, unpacking, Value7),
	optimality_pressure(NewB, web, Value8),
	config(integration_weight, IW),
	config(pattern_completion_weight, PCW),
	config(relevance_weight, RW),
	config(topology_weight, TW),
	config(maximization_vr, MVRW),
	config(intensification_vr, IVRW),
	config(unpacking, UW),
	config(web, WW),
	TOTAL is (IW+PCW+RW+TW+MVRW+IVRW+UW+WW),
	(TOTAL = 0, Value=0;
	Value is (IW*Value1 + PCW*Value2 + RW*Value3 + Value4*TW + Value5*MVRW+ Value6*IVRW + Value7*UW + Value8*WW)/TOTAL).


check_connectivity(Individual):-
	findall(C, (rel(Individual, C, R,_), R\=isa),L1),
	findall(C, (rel(Individual, _, R,C), R\=isa),L2),
	append(L1, L2, L),
	elimina_reps(L, [CI|LClean]),!,	
	retractall(marked(_)),
	check_connectivity(Individual, [CI], LClean).
	

check_connectivity(_, _, []).
	
check_connectivity(Individual, [CI|Rest], LClean):-
	assert(marked(CI)),
	findall(CN, (rel(Individual, CI, _, CN), not(marked(CN))), L1),
	findall(CN, (rel(Individual, CN, _, CI), not(marked(CN))), L2),
	append(L1, L2, L),
	remove_sublist(L, LClean, LClean2),
	append(L, Rest, Next),!,
	check_connectivity(Individual, Next, LClean2).
	
	


classify_individual(Individual):-
	findall(class(Inds), class(Inds), L),
	findall(a, projection(Individual,_,_,_),H),
	length(H, K),
	T is K / 2,	%Threshold
	join_to_class(Individual, T, L).

join_to_class(Individual, _, []):-
	assert(class([Individual])),!.

join_to_class(Individual, T, [class(L)|_]):-	
	in_class(Individual, T, L),!,
	retract(class(L)),
	append([Individual], L, L1),
	assert(class(L1)).

join_to_class(Individual, T, [_|Inds]):-	
	join_to_class(Individual, T, Inds),!.


in_class(Individual, T, [Ind|_]):-
	findall(A, projection(Individual, _,_,A), P1),
	findall(A, projection(Ind, _,_, A),P2),
	and(P1, P2, _L, N),!,
	N > T.

and([],[],_, 0).

and([A|Rest], [A|Rest2],[1|Rest3],N1):-
	and(Rest, Rest2, Rest3, N),
	N1 is N + 1.

and([_|Rest], [_|Rest2],[0|Rest3],N):-
	and(Rest, Rest2, Rest3, N).


evaluate([],[]).

evaluate([Individual|RestI], [Value/Individual|RestV]):-
	evaluate_individual(Individual, Value),!,
	evaluate(RestI, RestV).


generate_initial_population(Initial, Popsize, Name):-	 %%forgetting the arcs for now...
	remove_previous_run,
	retractall(counter(_)),
	assert(counter(0)),
	retractall(current_population(_)),
	generate_random_population(Initial, Popsize, Name, Individuals),
	assert(current_population(Individuals)),!.


remove_previous_run:-
	retract(current_population(L)),!,
	cleanDomains(L),
	remove_previous_run.

remove_previous_run:-
	stats(domain1,D1),
	stats(domain2, D2),
	findall(D, (rel(D,_,_,_),not(member(D, [D1, D2, tmp, both, generic]))),L),
	elimina_reps(L, L1),
	cleanDomains(L1).

remove_previous_run.
		

generate_random_population(_, 0, _, []).


generate_random_population(Initial, Popsize, Name, [Current|Rest]):-
	retract(counter(ID)),
	join_words([Name, ID], Current),
	assert(origin(Current, random)),
	V is ID + 1,
	Nextsize is Popsize - 1, 
	assert(counter(V)),
	generate_random_individual(Initial, Current),!,
	generate_random_population(Initial, Nextsize, Name, Rest).

generate_random_individual(Initial, Current):-
	findall(projection(Current, D, A, B), projection(Initial, D, A, B), L),
	generate_random_individual2(L, ProjList),
	assertall(ProjList),
	readProjection(ProjList, Current).
       

generate_random_individual2([], []).

generate_random_individual2([projection(New,Domain,A,A)|L], [projection(New, Domain, A, AN)|ProjList]):-
	pick_1([A,nil],AN,_),
	generate_random_individual2(L, ProjList).


generate_random_individual2([projection(New, Domain, A, _AN)|L], [projection(New, Domain, A, ANNew)|ProjList]):-
	stats(domain1, Domain),
	findall(C, m(_, A, C), Candidates),
	pick_1([nil,A|Candidates], ANNew, _),  				%%does not allow a|b!!!!
	generate_random_individual2(L, ProjList).

generate_random_individual2([projection(New, Domain, A, _AN)|L], [projection(New, Domain, A, ANNew)|ProjList]):-
	stats(domain2, Domain),
	findall(C, m(_, C, A), Candidates),
	pick_1([A, nil|Candidates], ANNew, _),  				%%does not allow a|b!!!!
	generate_random_individual2(L, ProjList).


projectDomain(D):-
	findall(projection(D, A, B, C), projection(D, A, B, C), L),
	readProjection(L,D).








%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




save_creature(File, D):-
	open(File, write, ID, [close_on_abort(true)]),
	rel(D, _, pw, Creature),
	findall(pw/Part, rel(D, Part, pw, Creature), L),
	write(ID, 'name:\t'), write(ID, D), nl(ID),
	write(ID, 'type:\t'), write(ID, creature), nl(ID),
	(rel(D, Creature, color, _), write(ID, 'color:\t'), write(ID, silver), nl(ID); nl(ID)),
	save_parts(L, ID),
	close(ID).

save_parts([],_).

save_parts([pw/Part|Rest], ID):-
	write(ID, 'pw:\t'), write(ID, Part), nl(ID),
/*	write(ID, '\tsize:\t2'), nl(ID),*/
	save_parts(Rest, ID).
	
	

save_domain(File, D, Value):-
	open(File, write, ID, [close_on_abort(true)]),
%	stats(domain1, D1),
%	stats(domain2, D2),
	findall(a, (rel(D, A, B, C), write(ID, rel(D,A,B,C)), nl(ID)),_),nl(ID),nl(ID),write(ID, '%%%%%%%%%%%%%%%%%% Mappings %%%%%%%%%%%%%%%%%'),nl(ID),
	findall(a, (m(_, A, B), write(ID, A), write(ID, '<--->'), write(ID, B), nl(ID)), _),nl(ID),nl(ID),
	write(ID, '%%%%%%%%%%%%%%%%%%  Frames  %%%%%%%%%%%%%%%%%'), nl(ID),
	findall(a, (frame_relations(D, Name, Conds, Negs), write(ID, Name),write(ID, ':'),nl(ID), tab(ID, 10), write(ID,'Conds:'), write(ID, Conds), nl(ID), tab(ID, 10), write(ID,'Negs:'), write(ID, Negs), nl(ID),nl(ID)),_), nl(ID), nl(ID),
	(value(pattern_completion(D),List);List=[]),
	write(ID, '%%%%  Integrity Constraint Violations %%%%%%%%'),nl(ID),
	findall(a, (violated_integrity(D, VP, VN), write(ID, '+ '), write(ID, VP), write(ID, '  - '), write(ID, VN)),_), nl(ID),
	write(ID, '%%%%%%%%%%%%%%%%  Completion %%%%%%%%%%%%%%%%%'),nl(ID),
	findall(a, (member(A, List), write(ID, A), nl(ID)),_),nl(ID),nl(ID),
	write(ID, '%%%%%%%%%%%%%%%%  Comparison to Target %%%%%%%%%%%%%%%%%'),nl(ID),
	(monitor(extraTarget,ExtraTarget); ExtraTarget=void), 
	write(ID, 'Extra knowledge: '), write(ID, ExtraTarget),nl(ID),
	(monitor(missingTarget,MissingTarget);MissingTarget=void), 
	write(ID, 'Missing knowledge: '), write(ID, MissingTarget),nl(ID),nl(ID),
/*
	write(ID, '%%%%%%%%%%%%%%%%  Comparison to Input domains %%%%%%%%%%%%%%%%%'),nl(ID),
	write(ID, D1), write(ID, ':'), nl(ID),
	(monitor(extraD1,ExtraD1); ExtraD1=void), 
	write(ID, 'Extra knowledge: '), write(ID, ExtraD1),nl(ID),
	(monitor(missingD1,MissingD1);MissingD1=void), 
	write(ID, 'Missing knowledge: '), write(ID, MissingD1),nl(ID),nl(ID),
	write(ID, D2), write(ID, ':'), nl(ID),
	(monitor(extraD2,ExtraD2);ExtraD2=void), 
	write(ID, 'Extra knowledge: '), write(ID, ExtraD2),nl(ID),
	(monitor(missingD2,MissingD2);MissingD2=void), 
	write(ID, 'Missing knowledge: '), write(ID, MissingD2),nl(ID),nl(ID),
*/
	write(ID, '%%%%%%%%%%%%%%%%%%  Emergent data %%%%%%%%%%%%%%%%%%'),nl(ID),
	(monitor(emergentExtra(D), Added); Added=void), 
	write(ID, 'Added: '), write(ID, Added),nl(ID),
	(monitor(emergentMissing(D), Removed);Removed=void), 
	write(ID, 'Removed: '), write(ID, Removed),nl(ID),nl(ID),	
	write(ID, '%%%%%%%%%%%%%%%%%%  Weights %%%%%%%%%%%%%%%%%%'),nl(ID),
	config(integration_weight,IW),write(ID, 'integration = '), write(ID, IW),nl(ID),
	config(pattern_completion_weight, PCW) ,write(ID, 'pattern completion = '), write(ID, PCW),nl(ID),
	config(topology_weight, TW), write(ID, 'topology = '), write(ID, TW),nl(ID),
	config(maximization_vr , MVW) ,write(ID, 'maximization VR  = '), write(ID, MVW),nl(ID),
	config(intensification_vr, IVW) ,write(ID, 'intensification VR = '), write(ID, IVW),nl(ID),
	config(unpacking , UW) ,write(ID, 'unpacking  = '), write(ID, UW),nl(ID),
	config(web, WW) ,write(ID, 'web = '), write(ID, WW),nl(ID),
	config(relevance_weight, RW) ,write(ID, 'relevance = '), write(ID, RW),nl(ID),nl(ID),nl(ID),
	write(ID, '%%%%%%%%%%%%%%%%%%  Values %%%%%%%%%%%%%%%%%%'),nl(ID),
	(optimality_pressure(D, integration,Integration); Integration=0),write(ID, 'integration = '), write(ID, Integration),nl(ID),
	(optimality_pressure(D, pattern_completion, PatComp); PatComp=0) ,write(ID, 'pattern completion = '), write(ID, PatComp),nl(ID),
	(optimality_pressure(D, topology, Top); Top=0), write(ID, 'topology = '), write(ID, Top),nl(ID),
	(optimality_pressure(D, maximization_vr , MVR); MVR=0) ,write(ID, 'maximization VR  = '), write(ID, MVR),nl(ID),
	(optimality_pressure(D, intensification_vr, IVR); IVR=0) ,write(ID, 'intensification VR = '), write(ID, IVR),nl(ID),
	(optimality_pressure(D, unpacking , Unp); Unp=0) ,write(ID, 'unpacking  = '), write(ID, Unp),nl(ID),
	(optimality_pressure(D, web, Web); Web=0) ,write(ID, 'web = '), write(ID, Web),nl(ID),
	(optimality_pressure(D, relevance, Relevance); Relevance=0) ,write(ID, 'relevance = '), write(ID, Relevance),nl(ID),nl(ID),nl(ID),
	write(ID, '%%%%%%%%%%%%%%%%  Query Data %%%%%%%%%%%%%%%%'),nl(ID),
	(retract(query(frames, [query|Rest])), (Rest=[]; assert(query(frames, Rest)));true),
	(query(frames,QF), write(ID, query(frames,QF)); write(ID, 'no query frames')),nl(ID),
	(query(facts,QB), write(ID, query(facts,QB)); write(ID, 'no query facts')),nl(ID),
	write(ID, 'Value ='), write(ID, Value),nl(ID),
	close(ID).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%







escreve_medias(ID,[],[N/TValue/TDist/TMissing/TDistD1/TDistD2/TSize/TSizeE/TSizeM/TGeneration]):-
	AValue is TValue / N,
	ADist is TDist / N,
	AMissing is TMissing / N, 
	ADistD1 is TDistD1 / N,
	ADistD2 is TDistD2 / N,
	ASize is TSize / N,
	ASizeE is TSizeE / N,
	ASizeM is TSizeM / N,
	AGeneration is TGeneration / N,
	nl(ID),
	write(ID, '--------------------------------------  MEANS  ------------------------------'),
	nl(ID),
	write(ID, 'value='), write(ID, AValue), nl(ID),
	write(ID, 'total error='), write(ID, ADist), nl(ID),
	write(ID, 'missing error='), write(ID, AMissing), nl(ID),
	write(ID, 'distance to domain 1='), write(ID, ADistD1), nl(ID),
	write(ID, 'distance to domain 2='), write(ID, ADistD2), nl(ID),
	write(ID, 'size='), write(ID, ASize), nl(ID),
	write(ID, 'emergent added='), write(ID, ASizeE), nl(ID),
	write(ID, 'emergent removed='), write(ID, ASizeM), nl(ID),
	write(ID, 'number of generation='), write(ID, AGeneration), nl(ID).



escreve_medias(ID, [_/Value/Dist/Missing/DistD1/DistD2/Size/SizeE/SizeM/Generation|Values], [N/TValue/TDist/TMissing/TDistD1/TDistD2/TSize/TSizeE/TSizeM/TGeneration]):-
		N1 is N + 1,
		TValue1 is TValue + Value,
		TDist1 is TDist + Dist,
		TMissing1 is TMissing + Missing, 
		TDistD11 is TDistD1 + DistD1,
		TDistD21 is TDistD2 + DistD2,
		TSize1 is TSize + Size,
		TSizeE1 is TSizeE + SizeE,
		TSizeM1 is TSizeM + SizeM,
		TGeneration1 is TGeneration + Generation,
		escreve_medias(ID, Values, [N1/TValue1/TDist1/TMissing1/TDistD11/TDistD21/TSize1/TSizeE1/TSizeM1/TGeneration1]).


escreve_medianas(ID, Values):-
	nl(ID), nl(ID),
	write(ID, '--------------------------------------  MEDIANS  ------------------------------'),
	nl(ID),
	obtem_mediana_indice(Values, 2, Value),
	write(ID, 'Value='), write(ID, Value), nl(ID),
	obtem_mediana_indice(Values, 3, Dist),
	write(ID, 'total error='), write(ID, Dist), nl(ID),
	obtem_mediana_indice(Values, 4, Missing),
	write(ID, 'missing error='), write(ID, Missing), nl(ID),
	obtem_mediana_indice(Values, 5, DistD1),		
	write(ID, 'difference to domain 1='), write(ID, DistD1), nl(ID),
	obtem_mediana_indice(Values, 6, DistD2),
	write(ID, 'difference to domain 2='), write(ID, DistD2), nl(ID),
	obtem_mediana_indice(Values, 7, Size),
	write(ID, 'size='), write(ID, Size), nl(ID),
	obtem_mediana_indice(Values, 8, SizeE),
	write(ID, 'emergent added='), write(ID, SizeE), nl(ID),
	obtem_mediana_indice(Values, 9, SizeM),
	write(ID, 'emergent removed='), write(ID, SizeM), nl(ID),
	obtem_mediana_indice(Values, 10, Generation),
	write(ID, 'generations='), write(ID, Generation), nl(ID).

escreve_mode(ID, Runs, Values):-
	nl(ID), nl(ID),
	write(ID, '--------------------------------------  MODES  ------------------------------'),
	nl(ID),
	obtem_mode_indice(Values, 2,  Runs, Value, RatioV),
	write(ID, 'Value='), write(ID, Value), 
	write(ID, ' ratio='), write(ID, RatioV), nl(ID),
	obtem_mode_indice(Values, 3,  Runs, Dist, RatioD),
	write(ID, 'total error='), write(ID, Dist), 
	write(ID, ' ratio='), write(ID, RatioD), nl(ID),
	obtem_mode_indice(Values, 4,  Runs, Missing, RatioM),
	write(ID, 'missing error='), write(ID, Missing), 
	write(ID, ' ratio='), write(ID, RatioM), nl(ID),
	obtem_mode_indice(Values, 5,  Runs, DistD1, RatioD1),
	write(ID, 'difference to domain 1='), write(ID, DistD1), 
	write(ID, ' ratio='), write(ID, RatioD1), nl(ID),
	obtem_mode_indice(Values, 6,  Runs, DistD2, RatioD2),
	write(ID, 'difference to domain 2='), write(ID, DistD2), 
	write(ID, ' ratio='), write(ID, RatioD2), nl(ID),
	obtem_mode_indice(Values, 7,  Runs, Size, RatioS),
	write(ID, 'size='), write(ID, Size), 
	write(ID, ' ratio='), write(ID, RatioS), nl(ID),
	obtem_mode_indice(Values, 8,  Runs, SizeE, RatioSE),
	write(ID, 'emergent added='), write(ID, SizeE), 
	write(ID, ' ratio='), write(ID, RatioSE), nl(ID),
	obtem_mode_indice(Values, 9,  Runs, SizeM, RatioSM),
	write(ID, 'emergent removed='), write(ID, SizeM), 
	write(ID, ' ratio='), write(ID, RatioSM), nl(ID),
	obtem_mode_indice(Values, 10,  Runs, Generation, RatioG),
	write(ID, 'generations='), write(ID, Generation), 
	write(ID, ' ratio='), write(ID, RatioG), nl(ID).

obtem_mode_indice(Values, Ind,  Runs, Result, Ratio):-
	obtem_lista_indice(Values, Ind, List),
	find_mode(List, Result,  Runs, Ratio).

find_mode(List, Result, Runs, Ratio):-
	find_mode(List, [], Result, Runs, Ratio).

find_mode([], L, Result,  Runs, Ratio):-
	msort(L, SortedList),
	reverse(SortedList, [N/Result|_]),
	Ratio is N/Runs.

find_mode([A|Rest], L, Result,  Runs, Ratio):-
	remove(N/A, L, L2),
	N1 is N + 1,
	find_mode(Rest, [N1/A|L2], Result,  Runs, Ratio).

find_mode([A|Rest], L, Result,  Runs, Ratio):-
	find_mode(Rest, [1/A|L], Result,  Runs, Ratio).



obtem_mediana_indice(Values, Ind, Result):-
	obtem_lista_indice(Values, Ind, List),
	length(List, N),
	msort(List, SortedList),
	obtem_mediana(SortedList, N, Result).

obtem_mediana(L, N, Result):-
	Middle is N / 2,
	Middle_frac is float_fractional_part(Middle),
	Middle_frac=0,
	Middle_int is float_integer_part(Middle),
	nth1(Middle_int, L, V1),
	Second is Middle_int + 1,
	nth1(Second, L, V2),
	Result is (V1 + V2 )/2.

obtem_mediana(L, N, Result):-
	Middle is float_integer_part((N + 1)/ 2),
	nth1(Middle, L, Result).

obtem_lista_indice([],_, []).

obtem_lista_indice([Compound|Values], Ind, [V|Rest]):-
	Dni is 10 - Ind,
	obtem_compound_indice(Compound, Dni, V),
	obtem_lista_indice(Values, Ind, Rest).

obtem_compound_indice(_/X, 0, X):-
	atomic(X).

obtem_compound_indice(X, 0, X):-
	atomic(X).


obtem_compound_indice(Rest/Y, Ind, X):-
	atomic(Y),
	Ind1 is Ind - 1,
	obtem_compound_indice(Rest, Ind1, X).

