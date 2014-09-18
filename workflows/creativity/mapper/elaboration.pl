
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	This module is the most recent development of Divago. It consists of the generation of emergent structure
%	within the resulting blend. It essentially applies the rules of the generic space ("generic.dt" file), 
%	executes the conclusion part of the frames and completes frames that were sufficiently instantiated by the 
%	"pattern completion" principle. These are the only three ways of elaboration used so far. 
%	It is also important to say that this module applies the elaboration repeatedly until the output is 
%	stabilized (allowing the propagation of rules, although not avoiding cycles). Thus, a conclusion of a frame 
%	can be used as a permise for triggering a rule. The user must only beware of avoiding cycles.
%
%	The external usage of this module, if necessary, can be made by elaborate(Individual). Individual corresponds 
%	to the "name" of the blend (e.g. elaborate(horse_dragon)). As a result the domain (in the form of rel/4 %	predicates) will be changed in the memory.
%	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


elaborate(Individual):-
%	write('Elaborating'),nl,
	retractall(current_blend(_)), 
	assert(current_blend(Individual)),
	findall(R, (rel(Individual, C1, Rel, C2), R=..[Rel, C1, C2]),PreElaboratedList),
	stats(domain1, D1),
	stats(domain2, D2),
	findall(PermissesP/PermissesN/Add/Del, rule(generic, Name, PermissesP, PermissesN, Add, Del), L1),
	findall(PermissesP/PermissesN/Add/Del, rule(D1, Name, PermissesP, PermissesN, Add, Del), L2),
	findall(PermissesP/PermissesN/Add/Del, rule(D2, Name, PermissesP, PermissesN, Add, Del), L3),
	append(L1, L2, L12),
	append(L12, L3, Rules),
	findall(Add/Del, (frame_elaboration(Individual, _, Add, Del)), Fs),
/*	findall(Frame/NewRels, (value(pattern_completion(Individual, Frame, E), NewRels), E>0.9, Frame\=query), Completion),
	apply_completion(Individual, Completion),*/
	fix_point(Individual, Rules, Fs), 
	findall(R, (rel(Individual, C1, Rel, C2), R=..[Rel, C1, C2]),ElaboratedList),
	evaluate_emergent_structure(Individual, PreElaboratedList, ElaboratedList).


apply_completion(_,[]).

apply_completion(Individual, [Frame/NewRels|Rest]):-
	apply_completion_list(Individual, NewRels, Flag),
	(Flag=1, assert(integrates(Individual, Frame));true),
	apply_completion(Individual, Rest).

apply_completion_list(_, [], 1).

apply_completion_list(Individual, [Cond|Rest], Flag):-
	Cond=..[_, X, Y],
	atomic(X), atomic(Y),
	X\=nil, Y\=nil,
	add_rels(Individual, [Cond]),
	apply_completion_list(Individual, Rest, Flag).
	
apply_completion_list(Individual, [_|Rest], 0):-
	apply_completion_list(Individual, Rest, _).



fix_point(Individual, Rules, Fs):-	
	elaborate_rules(Individual, Rules, Change1),
	elaborate_frames(Individual, Fs, Change2),
	check_cycle(Individual, Rules, Fs, Change1, Change2).


check_cycle(_, _, _, 0, 0).

check_cycle(Individual, Rules, Fs, _, _):-
	fix_point(Individual, Rules, Fs).


elaborate_rules(_, [], 0).

elaborate_rules(Individual, [PP/PN/Add/Del|Rules], 1):-	
	satisfies_conds(Individual, PP, PN, _, _),
	transforming_conclusions(Individual, Add, Del),
	apply_addlist(Individual, Add),
	apply_dellist(Individual, Del),
	elaborate_rules(Individual, Rules, _).

elaborate_rules(Individual, [_|Rules], Change):-
	elaborate_rules(Individual, Rules, Change).	
	

transforming_conclusions(_, [], []):-!, fail.

transforming_conclusions(Individual, [{_}|AddRest], Del):-
	transforming_conclusions(Individual, AddRest, Del).	

transforming_conclusions(Individual, [A|_], _):-
	A=..[R, C1, C2],	
	not(rel(Individual, C1, R, C2)).

	
transforming_conclusions(Individual, _, [D|_]):-
	test_condition(Individual, D).

transforming_conclusions(Individual, [_|AddRest], Del):-
	transforming_conclusions(Individual, AddRest, Del).

transforming_conclusions(Individual, Add, [_|DelRest]):-
	transforming_conclusions(Individual, Add, DelRest).
	

apply_addlist(_, []).
		

apply_addlist(Individual, [Add|Rest]):-
	expand_conds(Individual, [Add], Conds_expanded),
	add_rels(Individual, Conds_expanded),
	apply_addlist(Individual, Rest).

apply_addlist(Individual, [Add|Rest]):-
	add_rels(Individual, [Add]),
	apply_addlist(Individual, Rest).



add_rels(_,  []).

add_rels(Individual, [C|Conds_expanded]):-
	C=..[R, A, B],
	Rel=..[rel, Individual, A, R, B],
	assertrel(Rel),
	add_rels(Individual, Conds_expanded).

apply_dellist(_, []).
		

apply_dellist(Individual, [Del|Rest]):-
	expand_conds(Individual, [Del], Conds_expanded),
	remove_rels(Individual, Conds_expanded),
	apply_dellist(Individual, Rest).

apply_dellist(Individual, [Del|Rest]):-
	remove_rels(Individual, [Del]),
	apply_dellist(Individual, Rest).


remove_rels(_, []).

remove_rels(Individual, [C|Conds_expanded]):-
	C=..[R, A, B],
	Rel=..[rel, Individual, A, R, B],
	retractall(Rel),
	remove_rels(Individual, Conds_expanded).
	

elaborate_frames(_, [], 0).

elaborate_frames(Individual, [Add/Del|FsRest], 1):-
	transforming_conclusions(Individual, Add, Del),
	apply_addlist(Individual, Add),
	apply_dellist(Individual, Del),
	elaborate_frames(Individual, FsRest, _).

elaborate_frames(Individual, [_|FsRest], Change):-
	elaborate_frames(Individual, FsRest, Change).

	
	
evaluate_emergent_structure(Ind, PreElaboratedList, ElaboratedList):-
	intersection(PreElaboratedList, ElaboratedList, Intersection),
	remove_sublist(Intersection, PreElaboratedList, L2), 
	remove_sublist(Intersection, ElaboratedList, L1),
	(retract(monitor(emergentExtra(Ind),LInd1));LInd1=[]),
	append(LInd1, L1, LInd1New),
	assert(monitor(emergentExtra(Ind), LInd1New)),
	(retract(monitor(emergentMissing(Ind),LInd2));LInd2=[]),
	append(LInd2, L2, LInd2New),
	assert(monitor(emergentMissing(Ind), LInd2New)).



