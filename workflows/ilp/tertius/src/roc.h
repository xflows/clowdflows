
extern int the_seed;

void delete_roc_list(tlist *a_list);
int append(tlist *a_list, tlist *another, tlist **a_result);
int quicksort(tlist *a_list, int (a_function)(tlist *,tlist *),tlist **a_sorted_list);
int compare_roc(tlist *a_first,tlist *_second);
double generate_roc(FILE *a_file,tlist *a_list,double *a_roc_weights,FILE *a_roc_file,FILE *a_roc_file1,FILE *a_roc_file2,FILE *a_roc_file3);
int find_good_roc_weights(tlist **a_list,double *a_roc_weights,int a_num_classes,double *a_best_value);
int initialise_roc_weights(double **a_roc_weights,int a_num_classes);
void print_roc_weights(FILE *a_file,double *a_roc_weights,int a_num_classes);
int find_best_roc_weights(tlist **a_list,double *a_roc_weights,int a_num_classes);
