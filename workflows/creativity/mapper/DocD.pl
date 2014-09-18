:-[utilities], [elaboration], [mapper], [blender], [config], [constraints], [factory].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%º
%	This is the "project" file of Divago. It should be enough to consult only this one (DocD.pl) in order to get all the system in memory.
%	In this file, we find some "external" predicates that are commonly used to make Divago work. It is highly recommended the reading of all the other documents before starting to work with the system.
%	There are two kinds of predicates:
%		- Direct predicates 
%			. make_blend/5 calls the whole process of blending generation. The user is expected to select two input domains, an optional "target" file, which will contain the blend understood as being the correct output (used for assessing the success of the process) and two lists (that may be left empty). The first must contain the set goal frames the blend is expected to instantiate while the second serves to specify exact relations that should appear in the final blend.
%			. mapping/2 finds a structure mapping between the two input domains. The current version only allows the "metaphor mapping function", as described in our papers. This predicate will output a file (mappings.tmp) which will contain the corresponding m/3 predicates.
%			
%		- Handcoded configured predicates (a kind of Macros) - these predicates serve normally for intensive testing. Instead of writing big commands in the command line (e.g. make_blend(horse, bird, 'horse_bird.tgt', [], [ability(_, fly)])) for many times (e.g. 30 runs), we then use short predicates (e.g. x(0,1,30)). In these predicates, we also added some monitorization in order to get statistical feedback.
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%% Direct predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%make_blend(Input1, Input2, <Target_file,> [Goal frames], [Goal predicates])

make_blend(Input1, Input2, Frames, Predicates):-
	blend(Input1, Input2),
	ga(Frames,Predicates).


make_blend(Input1, Input2, Target, Frames, Predicates):-
	[Target],
	blend(Input1, Input2),
	ga(Frames,Predicates).

%mapping(Input1, Input2)

mapping(Input1, Input2):-
	metaphor(Input1, Input2).




%%%%%%%%%%%%%%%%% Handcoded configured predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

x(Mapping, Experiment, Number):-
%	['ashes_crawler & ash_elemental (1).tgt'],
	x(Mapping, Experiment, Number, 0).

x(Mapping, Experiment, Begin, End):- 
	A=horse, B=dragon,
	blend(A, B),
 	experiments(A, B, Mapping, Experiment, Begin, End, []).

experiments(D1, D2, Mapping,Experiment, A, A, Values):-
	join_words([D1,'_',D2, Mapping, 'E', Experiment, '.out'], Name),	
	open(Name, write, ID),
	findall(a, (member(N/V/D/Missing/DD1/DD2/Size/SizeE/SizeM/Generation, Values), 
		write(ID, N), write(ID, ' -->'), write(ID, V), 
		write(ID, '  Size='), write(ID, Size), 
		write(ID, '       Generations='), write(ID, Generation), 
		(D\= -1, write(ID,'  total error='), write(ID, D);D= -1),
		(Missing\= -1, write(ID,'  missing error='), write(ID, Missing);Missing= -1),
		(SizeE\=0, write(ID, '  emergent added='), write(ID, SizeE);SizeE=0),
		(SizeM\=0, write(ID, '  emergent missing='), write(ID, SizeM);SizeM=0),
		write(ID, '   diff. to '), write(ID, D1), write(ID, '='), 
		write(ID, DD1), write(ID, '   diff. to '), write(ID, D2), write(ID, '='),  
		write(ID, DD2), nl(ID)),All),
		length(All, Runs),
		escreve_medias(ID, Values, [0/0/0/0/0/0/0/0/0/0]),
		escreve_medianas(ID, Values),
		escreve_mode(ID, Runs, Values),
	close(ID).




experiments(D1, D2, Mapping, Experiment, Begin, End, Values):- 
	retractall(save_name(_)),
	retractall(creature_name(_)),
	retractall(monitor(_,_)),
	join_words([D1,'_',D2, Mapping, 'E', Experiment, r, Begin, '.dt'], Name),
	concat_atom([D1,'_',D2, Mapping, 'E', Experiment, r, Begin, '.txt'], Creature),	
	assert(save_name(Name)),
	assert(creature_name(Creature)),
	NewBeg is Begin - 1,
	ga([creature(_), frame(_), shape_transfer(E1, _), shape_transfer(E2, _), {E1\=E2}],[]),
	monitor(distance, Dist),
	monitor(missing, Missing),
	monitor(errorD1, DistD1),
	monitor(errorD2, DistD2),
	monitor(last_value,Value),
	monitor(best_individual,Ind),
	monitor(generation, Generation),
	(monitor(emergentExtra(Ind),EL); EL=[]),
	(monitor(emergentMissing(Ind),ML); ML = []),
	findall(a, rel(Ind, _,_,_),AL),
	length(AL, Size),
	length(EL, SizeE),
	length(ML, SizeM),
	experiments(D1, D2, Mapping, Experiment, NewBeg, End, [Name/Value/Dist/Missing/DistD1/DistD2/Size/SizeE/SizeM/Generation|Values]).

compare_to_solution(_Ind, -1, -1):-
	findall(_, s(_,_,_,_), []).

compare_to_solution(Ind, N, Missing):-
	findall(A/R/B, rel(Ind, A, R, B), L1),
	findall(A/R/B, s(solution, A, R, B), L2),
	intersection(L1, L2, L3),
	remove_sublist(L3, L1, L1a), write('Extra:'), write(L1a),nl,
	remove_sublist(L3, L2, L2a), write('Missing:'), write(L2a),nl,
	append(L1a, L2a, R),
	length(R, N),
	length(L2a, Missing),
	(retract(monitor(extraTarget,_));true),
	assert(monitor(extraTarget, L1a)),
	(retract(monitor(missingTarget,_));true),
	assert(monitor(missingTarget, L2a)).


compare_to_input(_Ind, D, -1,[],[]):-
	findall(_, rel(D,_,_,_), []).

compare_to_input(Ind, Domain, N, Extra, Missing):-
	findall(A/R/B, rel(Ind, A, R, B), L1),
	findall(A/R/B, rel(Domain, A, R, B), L2),
	intersection(L1, L2, L3),
	write('<-------------------------------------------------------------------------------------------->'),nl,
	write('Comparison to '), write(Domain), write(' domain:'),nl, 
	remove_sublist(L3, L1, Extra), length(Extra,E), write('Extra:'), write(E),nl,
	remove_sublist(L3, L2, Missing), length(Missing, M), write('Missing:'), write(M),nl,
	append(Extra, Missing, R),
	length(R, N).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




