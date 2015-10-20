
extern int recursive_1BC;
extern int ROC;
extern int roc_optimisation;
extern FILE *the_roc_file;
extern FILE *the_roc_file1;
extern FILE *the_roc_file2;
extern FILE *the_roc_file3;
extern int the_num_internal_fold;
extern int the_bayesian_approach2;

int initialise_recursive_1BC(char *a_target_name,int *a_num_classes);
void reset_recursive_1BC();
void estimate_probabilities();
void print_probabilities(FILE *a_file);
void export_differences(FILE *a_file);
void clean_probabilities();
int compare_contexts(tcontext *first_context, tcontext *second_context);
int get_set_probabilities(int an_individual,int a_type,int *occurences, double **probabilities,int a_class,int status,tcontext *a_context);
int call_recursive_1BC(long *a_num_success,long *a_num_test,int status,int a_fold,int *a_fold_table,double *a_roc_weights,
		       tlist **a_roc_list,int a_debug_flag);
int tune_roc_weights(tlist *a_target_list,tlist *a_remaining_list,int a_discriminative_threshold_tag,double a_discriminative_threshold,
		   int a_fold,int *a_fold_table,double **a_roc_weights,double *a_best_value);

