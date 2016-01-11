#define oneBC 0


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <math.h>
#include <signal.h>
#include "types.h"
#include "search.h"
#include "eval.h"
#include "input_output.h"
#include "gestion.h"
#include "bayes.h"
#include "error.h"
#include "write.h"
#if PROLOG
#include "background.h"
#endif
#include "database.h"
#include "memory.h"
#include "bayes2.h"
#include "roc.h"

long the_memory;
long the_max_memory;
int the_global_stop;
int the_ask_more_memory_option;
int eval_clauses;
int the_discriminative_threshold_tag;
int debug;
long the_random_seed;

int the_reevaluate;

char *the_target_predicate;

/* global variables for that file only */

void store_discretised_type(int an_argc,char **an_argv,int *an_index)
{
tdiscretised_type *the_new_discretised_types_table;
int i;

the_num_discretised_types++;
*an_index = *an_index + 1;
the_new_discretised_types_table = Calloc(the_num_discretised_types,tdiscretised_type)
if (the_new_discretised_types_table == NULL)
  error_deal_with(1,2);
the_new_discretised_types_table[the_num_discretised_types-1].name = an_argv[*an_index];
*an_index = *an_index + 1;
the_new_discretised_types_table[the_num_discretised_types-1].num_inter = atoi(an_argv[*an_index]);
if (the_new_discretised_types_table[the_num_discretised_types-1].num_inter == 0)
  error_deal_with(1,33);
*an_index = *an_index + 1;
if (strcmp(an_argv[*an_index],"sdm") == 0)
  the_new_discretised_types_table[the_num_discretised_types-1].kind = SDM;
else
  if (strcmp(an_argv[*an_index],"ffd") == 0) {
    the_new_discretised_types_table[the_num_discretised_types-1].kind = FFD;
    the_new_discretised_types_table[the_num_discretised_types-1].size = the_new_discretised_types_table[the_num_discretised_types-1].num_inter;
  }
  else
    the_new_discretised_types_table[the_num_discretised_types-1].kind = EQB;
for (i=0;i<the_num_discretised_types-1;i++) {
  the_new_discretised_types_table[i].name = the_discretised_types_table[i].name;
  the_new_discretised_types_table[i].num_inter = the_discretised_types_table[i].num_inter;
  the_new_discretised_types_table[i].kind = the_discretised_types_table[i].kind;
  the_new_discretised_types_table[i].size = the_discretised_types_table[i].size;
}
Free_calloc(the_num_discretised_types-1,the_discretised_types_table)
the_discretised_types_table = the_new_discretised_types_table;
}

void configure(int an_argc,char **an_argv,int *an_index)
{
int i;
char the_string[10];

for (i=*an_index; (i<an_argc) && (an_argv[i][0] == '-'); i++) {
    if (strcmp(an_argv[i],"-ask") == 0)
      the_ask_more_memory_option = TRUE;
    else if (strcmp(an_argv[i],"-breadth") == 0)
      search_approach = BREADTH;
    else if (strcmp(an_argv[i],"-nosubs") == 0)
      test_subsumption = FALSE;
    else if (strcmp(an_argv[i],"-sr") == 0)
      {use_sampling = 1; the_reevaluate = TRUE; i++; the_sampling_ratio = atof(an_argv[i]);}
    else if (strcmp(an_argv[i],"-SR") == 0)
      {use_sampling = 2; i++; the_reevaluate = TRUE; the_sampling_ratio = atof(an_argv[i]);}
    else if (strcmp(an_argv[i],"-sat") == 0)
      consistency = TRUE;
    else if (strcmp(an_argv[i],"-struct") == 0) {
      structure_based = TRUE;
      i++;
      if (sscanf(an_argv[i],"%[inf]",the_string) == 1)
	the_num_properties = INT_MAX;
      else
	the_num_properties = atoi(an_argv[i]);
    }
    else if (strcmp(an_argv[i],"-prolog") == 0)
      query_prolog = TRUE;
    else if (strcmp(an_argv[i],"-bg") == 0)
      the_expected_value = BACKGROUND;
    else if (strcmp(an_argv[i],"-pttp") == 0)
      the_expected_value = LINEAR_RESOLUTION;
    else if (strcmp(an_argv[i],"-ic") == 0)
      the_expected_value = INTEGRITY_CONSTRAINTS;
    else if (strcmp(an_argv[i],"-sc") == 0)
      starting_clause = TRUE;
    else if (strcmp(an_argv[i],"-cbu") == 0)
      counting_bottom_up = TRUE;
    else if (strcmp(an_argv[i],"-bayes") == 0) {
      bayes = TRUE;
      structure_based = TRUE;
      i++;
      if (strcmp(an_argv[i],"language") == 0)
	bayesian_approach = LANGUAGE;
      else if (strcmp(an_argv[i],"individual") == 0)
	bayesian_approach = INDIVIDUAL;
      else if (strcmp(an_argv[i],"first") == 0)
	bayesian_approach = FIRST;
      else error_deal_with(1,29);
      i++; the_target_predicate = an_argv[i];
    }
    else if (strcmp(an_argv[i],"-cross") == 0) {
      i++; the_num_fold = atoi(an_argv[i]);
    }
    else if (strcmp(an_argv[i],"-srand") == 0) {
      i++; the_random_seed = atoi(an_argv[i]);
    }
    else if (strcmp(an_argv[i],"-eval") == 0) {
      eval_clauses = TRUE;
    }
    else if (strcmp(an_argv[i],"-avg") == 0) {
      average = TRUE;
    }
    else if (strcmp(an_argv[i],"-db") == 0) {
      database = TRUE;
    }
    else if (strcmp(an_argv[i],"-roc") == 0) {
      ROC = TRUE;
    }
    else
      switch (an_argv[i][1]) {
      case 'm': i++; the_max_memory = 1000000 * atoi(an_argv[i]); break;
      case 'e': the_expected_value = NAIVE; break;
      case 'i': incremental_loading = TRUE; break;
      case 'n': i++; the_consistency_threshold = atof(an_argv[i]); break;
      case 'u': unification_form = EXPLICIT; break;
      case 's': use_sampling = 1; i++; the_sampling_ratio = atof(an_argv[i]); break;
      case 'S': use_sampling = 2; i++; the_sampling_ratio = atof(an_argv[i]); break;
      case 'v': verbose = TRUE; break;
      case 'b': i++; if (strcmp(an_argv[i], "class") == 0)
	the_search_bias = CLASSIFICATION;
      else if (strcmp(an_argv[i], "pos_class") == 0)
	the_search_bias = POSITIVE_CLASSIFICATION;
      else if (strcmp(an_argv[i], "horn") == 0)
	the_search_bias = HORN;
      else if (strcmp(an_argv[i], "horn_pos_class") == 0)
	the_search_bias = HORN_POSITIVE_CLASSIFICATION;
      else
	error_deal_with(1,9);
      break;
      case 'k': i++; k_best = atoi(an_argv[i]); break;
      case 'c': i++; k_best = -1; confirmation_level = atof(an_argv[i]); break;
      case 'f': i++; the_frequency_threshold = atof(an_argv[i]); break;
      case 't': i++; the_discriminative_threshold = atof(an_argv[i]); the_discriminative_threshold_tag = TRUE; break;
      case 'h': error_deal_with(1,9); break;
      case 'd': store_discretised_type(an_argc,an_argv,&i); break;	
      case 'w': first_order_features = TRUE; break;	
      case 'r': i++; recursive_1BC = TRUE; if (strcmp(an_argv[i], "INDIVIDUAL") == 0)
	the_bayesian_approach2 = INDIVIDUAL;
      else
	the_bayesian_approach2 = LANGUAGE;
      break;	
      case 'o': i++; roc_optimisation = TRUE; the_num_internal_fold = atoi(an_argv[i]);
	/*	i++; the_seed = atoi(an_argv[i]);*/
	break;	
      }
  }
*an_index = i;
}

int main(int argc, char *argv[])
{
tlist *the_list_nodes;
FILE *the_predicates_file = NULL;
FILE *the_facts_file = NULL;
FILE *the_configuration_file;
tlist *the_list_results;
FILE *the_result_file = NULL;
int the_error = 0;
int i;
char the_stem_name[STEMLENGTH];
char the_file_name[STEMLENGTH];
FILE *the_clause_file = NULL;
#if DEBUG
tlist the_debug_node;
#endif
int the_argc;
char **the_argv;
/* for Bayesian classification */
tlist *the_target_list,*the_remaining_list;
long the_num_individuals,*the_target_table,the_num_target,**the_property_table;
FILE *the_test_file = NULL;
long the_total_success,the_num_success;
long the_total_individuals,the_num_test;
int read_bayes_properties;
FILE *the_first_order_feature_file = NULL;
double the_auc;
double *the_roc_weights = NULL;
int *the_fold_table = NULL;
int the_fold;
tlist *the_roc_list;
int the_num_external_fold;
double the_best_accuracy;

signal(SIGINT, signal_deal_with);

/* initialise default values */
the_max_memory = 10000000;
the_expected_value = NORMAL;
the_consistency_threshold = 0;
the_ask_more_memory_option = FALSE;
unification_form = IMPLICIT;
use_sampling = 0;
the_sampling_ratio = 0;
search_approach = BEST;
verbose = FALSE;
test_subsumption = TRUE;
consistency = FALSE;
the_search_bias = NONE;
k_best = 10;
query_prolog = FALSE;
the_reevaluate = FALSE;
the_global_stop = FALSE;
the_frequency_threshold = 0;
individual_based = FALSE;
structure_based = FALSE;
starting_clause = FALSE;
counting_bottom_up = FALSE;
bayes = FALSE;
the_num_discretised_types = 0;
the_num_fold = 1;
the_num_properties = 1;
incremental_loading = FALSE;
average = FALSE;
first_order_features = FALSE;
the_discriminative_threshold_tag = FALSE;
the_discriminative_threshold = 0;
database = FALSE;
recursive_1BC = FALSE;
ROC = FALSE;
roc_optimisation = FALSE;
the_random_seed = 0;

if (argc < 2)
  error_deal_with(1,9);

if ((the_configuration_file = fopen(".tertiusrc","r")) != NULL) {
  error_deal_with(1,read_configuration_file(the_configuration_file,&the_argc,&the_argv));
  i = 0;
  configure(the_argc,the_argv,&i);
}

i = 1;
configure(argc,argv,&i);

if (k_best == 0)
  error_deal_with(1,14);

if (argc <= i)
  error_deal_with(1,9);

#if oneBC
bayes = TRUE;
structure_based = TRUE;
bayesian_approach = LANGUAGE;
the_target_predicate = argv[i];
i++;
#endif

if (sscanf(argv[i], "%d/%d",&the_max_lit, &the_max_var) != 2)
  error_deal_with(1,9);
i++;
if (argc > i+1)
  error_deal_with(1,9);
else {
  if (argc == i+1) {
    if (strlen(argv[i]) > STEMLENGTH + 4)
      error_deal_with(1, 15);
    strcpy(the_stem_name, argv[i]);
  }
  else
    strcpy(the_stem_name, "default");
  strcpy(the_file_name,the_stem_name);
  strcat(the_file_name, ".prd");
  if ((the_predicates_file = fopen(the_file_name,"r")) == NULL)
    error_deal_with(1, 1);
  strcpy(the_file_name,the_stem_name);
  if (database == FALSE) {
    strcat(the_file_name, ".fct");
    if ((the_facts_file = fopen(the_file_name,"r")) == NULL)
      error_deal_with(1, 1);
  }
  strcpy(the_file_name,the_stem_name);
  strcat(the_file_name, ".res");
  if ((the_result_file = fopen(the_file_name,"w")) == NULL)
    error_deal_with(1, 1);
  if (verbose == TRUE) {
    strcpy(the_file_name,the_stem_name);
    strcat(the_file_name, ".sta");
    if ((the_state_file = fopen(the_file_name,"w")) == NULL)
      error_deal_with(1, 1);
  }
  if ((starting_clause == TRUE)||(eval_clauses == TRUE)) {
    strcpy(the_file_name,the_stem_name);
    strcat(the_file_name, ".cla");
    if ((the_clause_file = fopen(the_file_name,"r")) == NULL)
      error_deal_with(1, 1);
  }
  if ((bayes == TRUE)&&(the_num_fold == 1)) {
    strcpy(the_file_name,the_stem_name);
    strcat(the_file_name, ".test");
    if ((the_test_file = fopen(the_file_name,"r")) == NULL)
      error_deal_with(1, 1);
  }
  if (first_order_features == TRUE) {
    strcpy(the_file_name,the_stem_name);
    strcat(the_file_name, ".fof");
    if ((the_first_order_feature_file = fopen(the_file_name,"w")) == NULL)
      error_deal_with(1, 1);
  }
  if (ROC == TRUE) {
    strcpy(the_file_name,the_stem_name);
    strcat(the_file_name, ".roc");
    if ((the_roc_file = fopen(the_file_name,"w")) == NULL)
      error_deal_with(1, 1);
    strcpy(the_file_name,the_stem_name);
    strcat(the_file_name, ".roc1");
    if ((the_roc_file1 = fopen(the_file_name,"w")) == NULL)
      error_deal_with(1, 1);
    strcpy(the_file_name,the_stem_name);
    strcat(the_file_name, ".roc2");
    if ((the_roc_file2 = fopen(the_file_name,"w")) == NULL)
      error_deal_with(1, 1);
    strcpy(the_file_name,the_stem_name);
    strcat(the_file_name, ".roc3");
    if ((the_roc_file3 = fopen(the_file_name,"w")) == NULL)
      error_deal_with(1, 1);
  }
}

if ((unification_form == EXPLICIT)&&(counting_bottom_up == TRUE)) {
  warning_deal_with(1,30);
  unification_form = IMPLICIT;
}

error_deal_with(1,read_predicates(the_predicates_file));
initialise_global_types();
if (database == FALSE) {
  initialise_discretised_types();
  if (the_num_discretised_types > 0) {
    error_deal_with(1,read_facts(the_facts_file,COLLECT_STATS));
    rewind(the_facts_file);
    statistics_discretised_types();
    display_discretised_types(the_result_file);
    display_discretised_types(stdout);
  }
  the_input_file = the_facts_file;
  if (incremental_loading == TRUE) {
    error_deal_with(1,read_facts(the_facts_file,PREPARE));
    rewind(the_facts_file);
  }
  else
    error_deal_with(1,read_facts(the_facts_file,STORE));
}
else {
  error_deal_with(1,collect_parameters());
  }

if (verbose == TRUE)
  {
    write_predicates(the_result_file);
    write_types(the_result_file);
    write_facts(the_result_file);
    fflush(the_result_file);
  }
if ((the_num_fold < 1) || (the_num_fold > the_nb_parts))
  error_deal_with(1,26);

if (eval_clauses == TRUE) {
  error_deal_with(1,read_clauses(the_clause_file,&the_list_results));
  write_clauses_appropriate(stdout,the_list_results,0);
  write_clauses_appropriate(the_result_file,the_list_results,0);
  exit(0);
}


#if DEBUG
starting_clause = FALSE;
#endif
if ((bayes == TRUE)&&(starting_clause==TRUE)) {
  starting_clause = FALSE;
  read_bayes_properties = TRUE;
}
else
  read_bayes_properties = FALSE;
  

#if PROLOG
if ((query_prolog == TRUE) || (the_expected_value == BACKGROUND)
    || (the_expected_value == LINEAR_RESOLUTION)|| (the_expected_value == INTEGRITY_CONSTRAINTS))
  error_deal_with(1,initialise_prolog(the_stem_name));
#endif

#if DEBUG
starting_clause = FALSE;
#endif
the_node_counter = 0;
the_num_evaluated_literals = 0;
error_deal_with(1,initialise_search(&the_list_nodes,the_clause_file));

#if DEBUG
error_deal_with(1,read_clause(the_clause_file,&the_global_clause));
the_error = eval_cube(the_global_clause,0,NULL);
the_debug_node.elt = (void*)the_global_clause;
the_debug_node.next = NULL;
write_clauses_appropriate(stdout,&the_debug_node,0);
#endif

if (bayes == FALSE) {
  error_deal_with(1,initialise_current_value(stderr));
  the_error = search(the_list_nodes,&the_list_results);
  if (the_error == 2)
    warning_deal_with(1,2);
  else
    error_deal_with(1,the_error);
reverse(the_list_results,&the_list_results);
if (the_reevaluate == TRUE)
  reeval_cubes(the_list_results);
/* Probably useless now that the list of results is constantly ordered to display current values
if (k_best < 0)
  quicksort(the_list_results,confirmation_value,&the_list_results);
*/
fprintf(stderr,"\n");
if (verbose == TRUE)
  fprintf(the_result_file,"\nThe result (%ld nodes):\n",the_node_counter);
else
  write_clauses_appropriate(stdout,the_list_results,0);
write_clauses_appropriate(the_result_file,the_list_results,0);
delete_cube_list(the_list_results);
} /* (bayes == FALSE) */
else /* (bayes == TRUE) */ {
  if (recursive_1BC == FALSE) {
    if (read_bayes_properties == TRUE)
      the_error = read_clauses(the_clause_file,&the_list_results);
    else
      the_error = generate_bayesian_properties(the_list_nodes,&the_list_results,the_target_predicate);
    if (the_list_results == NULL)
      error_deal_with(1,25);
    if (the_error != 0) 
      error_deal_with(1,the_error);
    separate_target_properties(the_target_predicate,the_list_results,&the_target_list,&the_remaining_list);
    reverse(the_target_list,&the_target_list);
    if (the_predicate_table[0].arity > 1)
      filter_target_property(the_target_predicate,&the_target_list);
    write_clauses_appropriate(the_result_file,the_target_list,0);
    write_clauses_appropriate(the_result_file,the_remaining_list,0);
    write_clauses_appropriate(stdout,the_target_list,0);
    write_clauses_appropriate(stdout,the_remaining_list,0);
  } /* (recursive_1BC == FALSE) */
  else /* (recursive_1BC == TRUE) */ {
    the_error = initialise_recursive_1BC(the_target_predicate,(int*)(&the_num_target));
    error_deal_with(1,the_error);
  } /* (recursive_1BC == TRUE) */
  if (the_num_fold > 1)
    the_error = create_k_fold(the_num_fold,the_nb_parts,&the_fold_table,0,NULL);
  else
    the_input_file = the_test_file;
  if ((ROC == TRUE)||(roc_optimisation == TRUE)) {
    the_roc_list = NULL;
  }
  the_total_success = 0; the_total_individuals = 0;
  for (the_fold=0;(the_fold<the_num_fold)&&(the_error==0);the_fold++) {
    /* COUNT */
    if (roc_optimisation == TRUE) {
      the_num_external_fold = the_num_fold;
      the_num_fold = the_num_internal_fold;
      debug = FALSE;
      error_deal_with(1,tune_roc_weights(the_target_list,the_remaining_list,the_discriminative_threshold_tag,the_discriminative_threshold,
					 the_fold,the_fold_table,&the_roc_weights,&the_best_accuracy));
      /*      debug = TRUE;*/
      the_num_fold = the_num_external_fold;
    }
    if (recursive_1BC == FALSE) {
      the_error = bayesian_counting(the_target_list,the_remaining_list,&the_num_individuals,&the_target_table,
				    &the_num_target,&the_property_table,the_fold,the_fold_table);
      if (the_discriminative_threshold_tag == TRUE)
	select_discriminative_features(the_remaining_list,the_discriminative_threshold,the_target_table,the_num_target,
				       &the_property_table,&the_remaining_list);
      if (first_order_features == TRUE)
	write_first_order_features(the_first_order_feature_file,
				   the_remaining_list,0);
      if (verbose == TRUE) {
	write_one_dim_table(the_result_file,the_target_table,the_num_target);
	fprintf(the_result_file,"\n");
	write_two_dim_table(the_result_file,the_property_table,the_num_target+2,length_list(the_remaining_list));
      } /* (verbose == TRUE) */
      /*      write_one_dim_table(stdout,the_target_table,the_num_target);
      fprintf(stdout,"\n");
      write_two_dim_table(stdout,the_property_table,the_num_target+2,length_list(the_remaining_list));*/
    } /* (recursive_1BC == FALSE) */
    else /* (recursive_1BC == TRUE) */ {
      reset_recursive_1BC();
      the_error = call_recursive_1BC(NULL,&the_num_test,COUNT,the_fold,the_fold_table,0,NULL,FALSE);
      error_deal_with(1,the_error);
      /*      print_probabilities(stdout);
	      print_probabilities(the_result_file);*/
      estimate_probabilities();
      export_differences(the_result_file);
    } /* (recursive_1BC == TRUE) */
    if (the_roc_weights == NULL)
      error_deal_with(1,initialise_roc_weights(&the_roc_weights,the_num_target));
    /* CLASSIFY */
    if (the_error == 0) {
      if (the_num_fold == 1) {
	the_input_file = the_test_file;
	if (incremental_loading == FALSE) {
	  free_instances();
	  free_non_global_types();
	}
	if (incremental_loading == TRUE) {
	  error_deal_with(1,read_facts(the_test_file,PREPARE));
	  rewind(the_test_file);
	}
	else
	  error_deal_with(1,read_facts(the_test_file,STORE));
	
	if (verbose == TRUE) {
	  write_predicates(the_result_file);
	  write_types(the_result_file);
	  write_facts(the_result_file);
	  fflush(the_result_file);
	}
      }
      if (recursive_1BC == FALSE)
	the_error = eval_classifier(the_num_target,the_target_table,the_num_individuals,the_property_table,
				    the_target_list,the_remaining_list,the_result_file,
				    &the_num_success,&the_num_test,the_fold,the_fold_table,the_roc_weights,&the_roc_list,FALSE);
      else /* (recursive_1BC == TRUE) */ {
	the_error = call_recursive_1BC(&the_num_success,&the_num_test,CLASSIFY,the_fold,the_fold_table,the_roc_weights,
				       &the_roc_list,FALSE);
	error_deal_with(1,the_error);
      }
      error_deal_with(1,the_error);
      the_total_success += the_num_success;
      the_total_individuals += the_num_test;
      if (roc_optimisation == TRUE) {
	fprintf(the_result_file,"%f:",the_best_accuracy);
	fprintf(stdout,"%f:",the_best_accuracy);
	print_roc_weights(the_result_file,the_roc_weights,the_num_target);
	print_roc_weights(stdout,the_roc_weights,the_num_target);
      }
      fprintf(the_result_file,"Accuracy: %ld/%ld = %f\n",the_num_success,the_num_test,
	      the_num_success/(double)the_num_test);
      /*      fprintf(stdout,"%ld,",the_num_success);*/
      fprintf(stdout,"Accuracy: %ld/%ld = %f\n",the_num_success,the_num_test,
	      the_num_success/(double)the_num_test);
      if (recursive_1BC == FALSE)
	free_bayesian_tables(the_num_target,the_target_table,length_list(the_remaining_list),the_property_table);
    }
  } /* for the_fold */
  fprintf(the_result_file,"Accuracy: %ld/%ld = %f\n",the_total_success,the_total_individuals,
	  the_total_success/(double)the_total_individuals);
  fprintf(stdout,"Accuracy: %ld/%ld = %f\n",the_total_success,the_total_individuals,
	  the_total_success/(double)the_total_individuals);
  if (the_error == 0) {
    if (recursive_1BC == FALSE) {
      delete_cube_list(the_target_list);
      delete_cube_list(the_remaining_list);
    } /* (recursive_1BC == FALSE) */
    else /* (recursive_1BC == TRUE) */
      reset_recursive_1BC();
    /*    if (roc_optimisation == TRUE) {
	  find_good_roc_weights(&the_roc_list,the_roc_weights,the_num_target,&the_best_accuracy);*/
      /*      find_best_roc_weights(&the_roc_list,the_roc_weights,the_num_target);*/
    /*      fprintf(the_result_file,"%f:",the_best_accuracy);
      fprintf(stdout,"%f:",the_best_accuracy);
      print_roc_weights(the_result_file,the_roc_weights,the_num_target);
      print_roc_weights(stdout,the_roc_weights,the_num_target);
      }*/
    if (ROC == TRUE) {
      quicksort(the_roc_list,compare_roc,&the_roc_list);
      the_auc = generate_roc(the_result_file,the_roc_list,the_roc_weights,the_roc_file,the_roc_file1,the_roc_file2,the_roc_file3);
      fprintf(stdout,"AUC = %f\n",the_auc);
      fprintf(the_result_file,"AUC = %f\n",the_auc);
    } /* (ROC == TRUE) */
    if ((ROC == TRUE)||(roc_optimisation == TRUE))
      delete_roc_list(the_roc_list);
    if (roc_optimisation == TRUE)
      Free_calloc(the_num_target,the_roc_weights)
  } /* (the_error == 0) */
} /* bayes == TRUE */

if (incremental_loading == FALSE) {
  free_instances();
  free_types();
}
fclose(the_result_file);
if (verbose == TRUE)
  fclose(the_state_file);
 if (ROC == TRUE) {
  fclose(the_roc_file);
  fclose(the_roc_file1);
  fclose(the_roc_file2);
  fclose(the_roc_file3);
 }
 if ((bayes == FALSE)||(recursive_1BC == FALSE))
   fprintf(stderr,"Number of hypotheses explored: %ld\nAverage number of ground queries to the database: %ld\n",
	   the_node_counter, (long)(the_num_evaluated_literals / the_node_counter));

return 0;

}
