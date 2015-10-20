
extern int the_expected_value;
extern int use_sampling;
extern double the_sampling_ratio;
extern int query_prolog;
extern int counting_bottom_up;
extern int incremental_loading;
extern double the_num_evaluated_literals;
extern int average;

long random_long(long a_max);
int eval_cube(tcube *a_cube,int a_fold,int *a_fold_table);
int optimistic_value(tlist* an_elt,tlist *another);
int optimistic_then_observed_value(tlist* an_elt,tlist *another);
int negated_optimistic_then_observed_value(tlist* an_elt,tlist *another);
double confirmation_cube(tcube *a_cube);
int confirmation_value(tlist* an_elt,tlist *another);
int confirmation_then_observed_value(tlist* an_elt,tlist *another);
int negated_confirmation_then_observed_value(tlist* an_elt,tlist *another);
double confirmation_cube_n(tcube *a_cube);
int confirmation_value_n(tlist* an_elt,tlist *another);
int evaluate_literal(tliteral *a_literal,targ *a_cube_args,int a_part,int *a_substitution,int *a_result);
int subsumes(tcube *a_first, tcube *a_second, int *a_result);
int all_substitutions_cube(tcube *a_cube,long *a_nb_substitutions,int **a_table_substitutions);
int write_substitutions(FILE *an_output,tcube *a_cube,long a_nb_substitutions,int *a_table_substitutions);
int reeval_cubes(tlist *a_list);
int can_prune(tlist *a_list, tlist *a_node);
int can_refine(tlist *a_list, tlist *a_node);
int can_store(tlist *a_list, tlist *a_node);
int prune_tautology(tcube *a_cube, int *a_result);
int equivalent_clauses(tcube *a_first_clause, tcube *a_second_clause, int *a_result);
int count_substitutions_for_all_instances(tcube *a_cube,int *a_substitution,int *a_lits_table,
					  int *a_nb_consts,int a_part,int a_literal,
					  tinstance *instances,int a_num_inst,long *a_number);
int recursive_count_substitutions(tcube *a_cube,int *a_lits_table,int *the_substitution,
			      int *the_nb_consts,long *a_number,int a_part);
void evaluate_confirmation_and_optimistic(long nHB, long not_head, long body, long population, long *blocks, int num_blocks,
				     double *confirmation,double *optimistic);
