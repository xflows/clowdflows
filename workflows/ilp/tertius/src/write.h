
extern int first_order_features;

int initialise_current_value(FILE *an_output);
int display_current_value(FILE *an_output, tlist *a_list);
int write_subsets(FILE *an_output,int n, tlist *a_list_subsets);
int write_clauses_appropriate(FILE *an_output,tlist *a_list,int a_part);
int write_clauses_normal(FILE *an_output,tlist *a_list,int a_part);
int write_class(FILE *a_result_file,int *an_individual,tcube *a_cube,int a_part);
int write_first_order_features(FILE *an_output,tlist *a_list,int a_part);
