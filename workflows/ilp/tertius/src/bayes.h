
extern int the_num_fold;
extern int the_num_properties;
extern int bayesian_approach;
extern double the_discriminative_threshold;
extern int the_individual_predicate;

int create_individual_and_their_constants(int **an_individual,int **individual_constants,long **individual_dividers);
int initialise_individual_constants(int the_part,int *individual_constants,
				    long *individual_dividers,long *a_num_individuals);
int initialise_individual(long a_num_ind,int *an_individual,long *individual_dividers,int *individual_constants);
int generate_bayesian_properties(tlist *a_list_nodes, tlist **a_list_results,char *a_target_predicate);
int separate_target_properties(char *a_name,tlist *a_list,tlist **a_target_list,tlist **a_remaining_list);
int bayesian_counting(tlist *a_target_list,tlist *a_property_list,
		      long *a_num_individuals,long **a_target_table,
		      long *a_num_target,long ***a_property_table,int a_fold,int *a_fold_table);
int eval_classifier(int a_num_target,long *a_target_table,long a_num_individuals,long **a_property_table,
		    tlist *a_target_list,tlist *a_property_list,FILE *a_result_file,
		    long *a_num_success,long *a_num_test,int a_fold,int *a_fold_table,double *a_roc_weights,
		    tlist **a_roc_list,int a_debug_flag);
int write_one_dim_table(FILE *a_result_file,long *a_table,int a_num_column);
int write_two_dim_table(FILE *a_result_file,long **a_table,int a_num_column,int a_num_line);
int free_bayesian_tables(int a_num_target,long *a_target_table,long a_num_properties,long **a_property_table);
int set_individual_var(tliteral *a_cube_lits,int a_cube_num_args,int **a_table);
int instantiate_cube(int *an_individual,tcube *a_cube);
int invert_sign_literals(tcube *a_cube);
int filter_target_property(char *a_name,tlist **a_target_list);
int select_discriminative_features(tlist *a_list, float a_threshold, long *a_target_table, int a_num_target,
				   long ***a_property_table,tlist **an_output);
