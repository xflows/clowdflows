:- dynamic db/2, af/4, used/2, m/3, rel/4, stats/2, cycle/1, r/4.
:- ensure_loaded(utilities).
:- ensure_loaded(config).
%%:- ensure_loaded(blender).
%:- write(cheguei),nl.

%%Leitura dos dois ficheiros
readDomains([]).

readDomains([D|Rest]):-
	readDomain(D),
	readDomains(Rest).

readDomain(D1):-
	write(D1),nl,
	str_cat(D1, '',F),
	load_dyn(F),	
	convert_to_rel(D1).

convert_to_rel(Domain):-
	retract(r(A,B,C,D)),
	(rel(A,B,C,D);
	assertrel(rel(A,B,C,D))),
	convert_to_rel(Domain).
	

convert_to_rel(_).

%% Listagem de relações lidas dos domínios
mostraDomains:-
    nl,nl,write(' %%%%%%%%%%% Concept Map   %%%%%%%%%%%%%%%%% '),nl,
    findall(rel(Domain, A, R, B), rel(Domain, A, R, B), L1),
    mostra_listanl(L1).


%% Gravar relações em ficheiro
saveRel(File):-
	%%fopen(ID2,  File, w),
	str_cat(File, '.out',File1),
	open(File1,write,ID2),
	saveRel2(ID2),
	close(ID2).

saveRel2(ID2):-
	retract(rel(Domain, A, R, B)),
	write(ID2,relation(A, R, B)),write(ID2,'.'),nl(ID2),
	saveRel2(ID2).

saveRel2(_).



run :-
	readDomains(['/var/folders/2f/m6y6cmzj2w1fwdt6lwps6ds00000gn/T/tmpB6DRPv.dt','/var/folders/2f/m6y6cmzj2w1fwdt6lwps6ds00000gn/T/tmpYULkOF.dt']),
	mostraDomains.
	%saveRel(output).

:- run.


