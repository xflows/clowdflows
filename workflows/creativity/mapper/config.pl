:-dynamic config/2.

config(desintegration_factor, 0.8). %% alpha factor of the Integration formula
config(integrity_penalty, 0.1). %% Penalty for each integrity constraint violation. It has exponential effect.
config(evidence_threshold, 0.4). %% Minimal evidence value in order to be a "pattern to be completed". Only used in the Pattern Completion formula


%% The user can say exactly which are "vital relations", instead of the original ones (a la Fauconnier and Turner). These relations are preferred to the others in the Maximization of Vital Relations formula. So far, this is the only of effect of this specification.

%These are the Fauconnier and Turner's Vital Relations:
%config(vital_relations, [change, identity, spatio_temporal, spatial_temporal, cause_effect, part_whole, symbolization, %role_playing, analogy, disanalogy, property_ascription, similarity, class_ascription, taxonomicq, intention, %logical_uniqueness]).

%For us, in the "creatures domains", these are the vital relations
config(vital_relations, [isa, analogy, pw, purpose, quantity]).  


%%%Optimality pressures weights
config(web_alpha, 0.5).	%%Alpha parameter of the Web measure
config(web_beta, 0.5).	%%Beta parameter of the Web measure


% Weights for the fitness function, which essentially consists of the weighted sum of each of the optimality constraints value. Below, we see an example of a typical configuration: very much emphasis on Relevance (if there are goal frames), medium low emphasis on Integration (sometimes, it has bigger values), Topology and Unpacking have only "preference" roles while the others are "switched off". 
 
config(integration_weight, 4).
config(pattern_completion_weight, 0).
config(topology_weight, 0.5).
config(maximization_vr, 0).
config(intensification_vr, 0).
config(unpacking, 0.5).
config(web, 0).
config(relevance_weight, 10).


% It is also possible to ask for "minimal" values. I.e. if a minimal value is not achieved a penalty is given to the fitness. Below, we see no minimal values required, which means there are never penalties to the fitness of an individual. Only in last case should these values be changed (e.g. if the user finds the results tend to get lost in a local maximum that has unnacceptable values for, for example, Integration). We normally used these to check the search space.

config(minimal_integration_weight, 0).
config(minimal_pattern_completion_weight, 0).
config(minimal_topology_weight, 0).
config(minimal_maximization_vr, 0).
config(minimal_intensification_vr, 0).
config(minimal_unpacking, 0).
config(minimal_web, 0).
config(minimal_relevance_weight, 0).


%% GA configuration
config(popsize, 100).		%Population size
config(generations, 200).	%Maximum number of generations


%% GA operator weights
config(copy, 0.03).		%Assexual reproduction
config(crossover, 0.96).	%Crossover
config(mutation, 0.1).		%Mutation	
config(random, 0.01).		%Random pick of individuals from the search space

%% Maximum sequence of stalled generations
config(stall, 15).		%Stopping condition for the GA. If it doesn't get better results for more than N subsequent 					%generations, stop (in this case, N=15).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% We advise to leave this variables unchanged %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

config(mapping_type, alignment).
config(type,structured).   %%tipo de casos structured/linear
config(compound, yes).	%%junta ou nao os conceitos (e.g. snake<->cache & Yes => snake_cache;No => snake)
config(metablends, no).   %%considera ou nao os "blended_with" anteriores no blend actual
config(both, yes).   %%gera ou nao o espaco generico, composto por todos as relacoes comuns a ambos os dominios
config(mapping_mode, extensional).  %%extensional -> utiliza apenas os factos sem derivar as regras (contrario de intensional).
config(abductive_pattern_completion, 0).  %%% weight used in abductive pattern completion. Not totally implemented
config(unpacking_effort, 0.2). %%Penalidade que estima o esforco de fazer unpacking do "tipo 2" -> atraves de evidencias em vez de associacao directa.
constant(maxcasesize,9).    %%maximo numero de campos de um case/N












