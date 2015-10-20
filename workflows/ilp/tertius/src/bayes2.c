
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "types.h"
#include "search.h"
#include "bayes.h"
#include "error.h"
#include "input_output.h"
#include "gestion.h"
#include "bayes2.h"
#include "roc.h"

int recursive_1BC;
int the_target_predicate;
int the_part;
int the_num_classes;
double *the_class_probabilities; /* to store P(c), probabilities of each class */
int ROC;
int roc_optimisation;
FILE *the_roc_file;
FILE *the_roc_file1;
FILE *the_roc_file2;
FILE *the_roc_file3;
long *the_num_main_individuals;
tlist *the_contexts_list;
int the_num_internal_fold;
int the_bayesian_approach2; /* INDIVIDUAL: 1BC2, counting at the level of each (sub-)individual ; LANGUAGE: 1BC, counting based on the main individual, plus propositionalisation */

void print_parameter(FILE *a_file,int a_predicate,int a_subpredicate)
{
  int i;
  tpredicate *the_predicate;
  tsubpredicate *the_subpredicate;
  ttype *the_type;

  the_predicate = &(the_predicate_table[a_predicate]);
  the_subpredicate = the_predicate->subpredicates[a_subpredicate];
  for (i=0;i<the_predicate->num_param;i++) {
    the_type = &(the_type_table[the_predicate->types[i]]);
    if (the_type->global == TRUE)
      fprintf(a_file,"\t%s",the_type->constants[0][the_subpredicate->val_params[i]]);
    else
      fprintf(a_file,"\t%s",the_type->constants[the_part][the_subpredicate->val_params[i]]);
  } /* for each component of the parameter */
}

int initialise_recursive_1BC(char *a_target_name,int *a_num_classes)
{
  int the_error = 0;
  int i;
  
  the_num_main_individuals = 0;
  the_contexts_list = NULL;
  the_target_predicate = the_nb_predicates;
  for (i=0;(i<the_nb_predicates)&&(the_error==0);i++)
    if (strcmp(a_target_name,the_predicate_table[i].name) == 0)
      the_target_predicate = i;
  if (the_target_predicate == the_nb_predicates)
    return 44;
  the_num_classes = the_predicate_table[the_target_predicate].num_subpredicate;
  *a_num_classes = the_num_classes;
  for (i=0;(i<the_nb_predicates)&&(the_error==0);i++) {
    switch (the_predicate_table[i].kind) {
    case STRUCTURAL:
      if (the_predicate_table[i].arity != 2)
	return 28;
      if ((the_predicate_table[i].num_occurence != 0) && (the_predicate_table[i].num_subpredicate != 1))
	return 41;
      if (the_predicate_table[i].structural_kind == BV)
	return 42;
      break;
    case PROPERTY:
      if (the_predicate_table[i].arity != 1)
	the_error = 38;
      break;
    } /* switch (the_predicate_table[i].kind) */
  } /* for each predicate i */
  for (i=0;(i<the_nb_types)&&(the_error==0);i++) {
    the_type_table[i].num_contexts = 0;
  }
  the_class_probabilities = Calloc(the_num_classes,double)
  if (the_class_probabilities == NULL) the_error = 2;

  return the_error;
}

void estimate_probabilities()
{
  int the_predicate,the_subpredicate,the_class,the_context;
  long the_sum;
  double the_num_individuals;

  for (the_predicate=0;the_predicate<the_nb_predicates;the_predicate++)
    switch (the_predicate_table[the_predicate].kind) {
    case STRUCTURAL:
      if ((the_predicate_table[the_predicate].num_occurence != 0)
	  && ((the_predicate_table[the_predicate].dimensions[0] != 1)||(the_predicate_table[the_predicate].dimensions[1] != 1))) {
	for (the_context=0;the_context<the_type_table[the_predicate_table[the_predicate].types[the_predicate_table[the_predicate].num_param]].num_contexts;
	     the_context++)
	  for (the_class=0;the_class<the_num_classes;the_class++)
	    if (the_predicate_table[the_predicate].subpredicates[0]->num_individuals[the_context][the_class] == 0)
	      the_predicate_table[the_predicate].subpredicates[0]->probabilities[the_context][the_class] = -HUGE_VAL;
	    else {
	      the_predicate_table[the_predicate].subpredicates[0]->probabilities[the_context][the_class]
		/= the_predicate_table[the_predicate].subpredicates[0]->num_individuals[the_context][the_class];
	      the_predicate_table[the_predicate].subpredicates[0]->probabilities[the_context][the_class]
		= log(1 / (1 + the_predicate_table[the_predicate].subpredicates[0]->probabilities[the_context][the_class]));
	    } /* for each class, for each context, if not empty */
      } /* if non-determinate structural predicate */
      break;
    case PROPERTY:
      if (the_predicate != the_target_predicate)
	for (the_context=0;the_context<the_type_table[the_predicate_table[the_predicate].types[the_predicate_table[the_predicate].num_param]].num_contexts;
	     the_context++)
	  for (the_subpredicate=0;the_subpredicate<the_predicate_table[the_predicate].num_subpredicate;the_subpredicate++) {
	    the_sum = 0;
	    for (the_class=0;the_class<the_num_classes;the_class++)
	      the_sum += the_predicate_table[the_predicate].subpredicates[the_subpredicate]->probabilities[the_context][the_class];
	    for (the_class=0;the_class<the_num_classes;the_class++) {
	      if (the_sum > 0) {
		/* only if the subpredicate occurs in the current training set, because it is not necessarily the case during cross-validation */
		if (the_bayesian_approach2 == INDIVIDUAL)
		  the_num_individuals
		    = the_type_table[the_predicate_table[the_predicate].types[the_predicate_table[the_predicate].num_param]].num_individuals[the_context][the_class];
		else /* (the_bayesian_approach2 == INDIVIDUAL) */
		  the_num_individuals = the_class_probabilities[the_class];
		/* debug: display countings
		   fprintf(stderr,"%d %d ", (int)(the_predicate_table[the_predicate].subpredicates[the_subpredicate]->probabilities[the_context][the_class]),(int)the_num_individuals);*/
		if ((the_bayesian_approach2 == INDIVIDUAL)
		    || (the_type_table[the_predicate_table[the_predicate].types[the_predicate_table[the_predicate].num_param]].contexts[the_context] == NULL)
		    || (the_type_table[the_predicate_table[the_predicate].types[the_predicate_table[the_predicate].num_param]].contexts[the_context]->functional
			== TRUE))
		  the_predicate_table[the_predicate].subpredicates[the_subpredicate]->probabilities[the_context][the_class]
		    = log((the_predicate_table[the_predicate].subpredicates[the_subpredicate]->probabilities[the_context][the_class] + 1)
			  / (the_num_individuals + the_predicate_table[the_predicate].num_subpredicate));
		else
		  the_predicate_table[the_predicate].subpredicates[the_subpredicate]->probabilities[the_context][the_class]
		    = log((the_predicate_table[the_predicate].subpredicates[the_subpredicate]->probabilities[the_context][the_class] + 1)
			  / (the_num_individuals + 2));
	      } /* if (the_sum > 0) */
	      else
		/* else keep a log(P) = 0 for each class */
		the_predicate_table[the_predicate].subpredicates[the_subpredicate]->probabilities[the_context][the_class] = 0;
	    } /* for each class and each context */
	    /* debug: display subpredicate
	       print_parameter(stderr,the_predicate,the_subpredicate);fprintf(stderr,"\n");*/
	  } /* for each subpredicate */
      break;
    } /* switch (the_predicate_table[the_predicate].kind) */
  /* initialise a-priori probabilities of each class */
  the_num_individuals = 0;
  for (the_class=0;the_class<the_num_classes;the_class++) {
    the_num_individuals += the_class_probabilities[the_class];
  } /* for the class */
  for (the_class=0;the_class<the_num_classes;the_class++)
    the_class_probabilities[the_class] = log((the_class_probabilities[the_class] + 1) / (the_num_individuals + the_num_classes));
}

void reset_recursive_1BC()
{
  int i,the_subpredicate,the_class,j;
  tcontext *the_context;
  int the_num_contexts;
  
  for (i=0;i<the_nb_predicates;i++) {
    the_num_contexts = the_type_table[the_predicate_table[i].types[the_predicate_table[i].num_param]].num_contexts;
    switch (the_predicate_table[i].kind) {
    case STRUCTURAL:
      if ((the_predicate_table[i].num_occurence != 0)
	  && ((the_predicate_table[i].dimensions[0] != 1)||(the_predicate_table[i].dimensions[1] != 1))) {
	for (j=0;j<the_num_contexts;j++)
	  Free_calloc(the_num_classes,the_predicate_table[i].subpredicates[0]->num_individuals[j])
	Free_calloc(the_num_contexts,the_predicate_table[i].subpredicates[0]->num_individuals)
      } /* if non-determinate structural predicate */
      break;
    } /* switch (the_predicate_table[i].kind) */
    for (the_subpredicate=0;the_subpredicate<the_predicate_table[i].num_subpredicate;the_subpredicate++) {
      for (j=0;j<the_num_contexts;j++)
	Free_calloc(the_num_classes,the_predicate_table[i].subpredicates[the_subpredicate]->probabilities[j])
      Free_calloc(the_num_contexts,the_predicate_table[i].subpredicates[the_subpredicate]->probabilities)
      Free_calloc(the_num_contexts,the_predicate_table[i].subpredicates[the_subpredicate]->already_counted)
    } /* for the_subpredicate */
  } /* for each predicate i */
  for (i=0;i<the_nb_types;i++) {
    the_num_contexts = the_type_table[i].num_contexts;
    for (j=0;j<the_num_contexts;j++)
      Free_calloc(the_num_classes,the_type_table[i].num_individuals[j])
    Free_calloc(the_num_contexts,the_type_table[i].num_individuals)
    Free_calloc(the_num_contexts,the_type_table[i].contexts)
    the_type_table[i].num_contexts = 0;
  } /* for each type i */
  /*  fprintf(stderr,"Number of contexts %ld\n",length_list(the_contexts_list));*/
  while (the_contexts_list != NULL) {
    the_context = (tcontext*)(the_contexts_list->elt);
    Free_calloc(2,the_context->new_lit->args)
    Free(the_context->new_lit)
    Free(the_context)
    the_contexts_list = the_contexts_list->next;
  } /* while (the_contexts_list != NULL) */
  for (the_class=0;the_class<the_num_classes;the_class++)
    the_class_probabilities[the_class] = 0;
}

int initialise_occurences(int **occurences)
{
  int the_error = 0;
  int *the_occurences;
  int i;

  the_occurences = Calloc(the_nb_predicates,int)
  if (the_occurences == NULL) return 2;
  for (i=0;i<the_nb_predicates;i++)
    the_occurences[i] = the_predicate_table[i].num_occurence;

  *occurences = the_occurences;
  return the_error;
}

int get_other_individual(int a_predicate,int an_individual,int an_index,int *another_individual)
{
  int the_error = 0;
  int i;
  int stop = FALSE;
  
  for (i=0;(i<the_predicate_table[a_predicate].subpredicates[0]->nb_inst[the_part]) && (stop==FALSE);i++) {
    if (the_predicate_table[a_predicate].subpredicates[0]->instances[the_part][i][an_index]
	== an_individual) {
      *another_individual =
	the_predicate_table[a_predicate].subpredicates[0]->instances[the_part][i][1-an_index];
      stop = TRUE;
    } /* if match */
  } /* for each instance */

  if (stop == FALSE)
    the_error = 40;

  return the_error;
}

int add_to_int_table(int an_element,int **a_table,int *a_number)
{
  int the_error = 0;
  int *the_table;
  int i;

  *a_number += 1;
  the_table = Calloc(*a_number,int)
  if (the_table == NULL) return 2;
  for (i=0;i<*a_number-1;i++)
    the_table[i] = (*a_table)[i];
  the_table[*a_number-1] = an_element;
  Free_calloc(*a_number-1,*a_table)
  *a_table = the_table;

  return the_error;
}

int get_all_other_individuals(int a_predicate,int an_individual,int an_index,
			      int **other_individuals,int *a_num_individuals)
{
  int the_error = 0;
  int i;
  
  *a_num_individuals = 0;
  for (i=0;(i<the_predicate_table[a_predicate].subpredicates[0]->nb_inst[the_part])&&(the_error==0);i++) {
    if (the_predicate_table[a_predicate].subpredicates[0]->instances[the_part][i][an_index]
	== an_individual) {
      the_error = add_to_int_table(the_predicate_table[a_predicate].subpredicates[0]->instances[the_part][i][1-an_index],
				   other_individuals,a_num_individuals);
    }
  } /* for each instance */

  return the_error;
}

void print_context(FILE *a_file, tcontext *a_context)
{
  if (a_context != NULL) {
    print_context(a_file,a_context->ancestors);
    if (a_context->new_lit->sign == NEG)
      fprintf(a_file,"not ");
    fprintf(a_file,"%s(",the_predicate_table[a_context->new_lit->predicate].name);
    fprintf(a_file,"%c,",a_context->new_lit->args[0] + 'A');
    fprintf(a_file,"%c) ",a_context->new_lit->args[1] + 'A');
  }
}

void print_probabilities(FILE *a_file)
{
  int the_class, the_predicate_index, the_subpredicate_index, the_context_index;
  tpredicate *the_predicate;
  tsubpredicate *the_subpredicate;

  fprintf(a_file,"\t\t\t");
  for (the_class=0;the_class<the_num_classes;the_class++)
    print_parameter(a_file,the_target_predicate,the_class);
  fprintf(a_file,"\n");
  fprintf(a_file,"\t\t\t");
  for (the_class=0;the_class<the_num_classes;the_class++)
    fprintf(a_file,"\t%d",(int)the_class_probabilities[the_class]);
  fprintf(a_file,"\n");
  for (the_predicate_index=0;the_predicate_index<the_nb_predicates;the_predicate_index++) {
    the_predicate = &(the_predicate_table[the_predicate_index]);
    if ((the_predicate->kind == PROPERTY)&&(the_predicate_index != the_target_predicate))
      for (the_context_index=0;the_context_index<the_type_table[the_predicate->types[the_predicate->num_param]].num_contexts;the_context_index++)
	for (the_subpredicate_index=0;the_subpredicate_index<the_predicate->num_subpredicate;the_subpredicate_index++) {
	  fprintf(a_file,"\t%s",the_predicate->name);
	  print_parameter(a_file,the_predicate_index,the_subpredicate_index);
	  /*fprintf(a_file,"\t%d",the_context_index);*/
	  fprintf(a_file,"\t");
	  print_context(a_file,the_type_table[the_predicate->types[the_predicate->num_param]].contexts[the_context_index]);
	  the_subpredicate = the_predicate->subpredicates[the_subpredicate_index];
	  for (the_class=0;the_class<the_num_classes;the_class++) {
	    fprintf(a_file,"\t%d",(int)the_subpredicate->probabilities[the_context_index][the_class]);
	  }
	  fprintf(a_file,"\n");
	} /* for each subpredicate and each context */
    if ((the_predicate->kind == STRUCTURAL) && (the_predicate->num_occurence != 0) && ((the_predicate->dimensions[0] != 1)||(the_predicate->dimensions[1] != 1)))
      for (the_context_index=0;the_context_index<the_type_table[the_predicate->types[the_predicate->num_param]].num_contexts;the_context_index++) {
	  fprintf(a_file,"\t%s\t",the_predicate->name);
	  /*fprintf(a_file,"\t%d",the_context_index);*/
	  print_context(a_file,the_type_table[the_predicate->types[the_predicate->num_param]].contexts[the_context_index]);
	  the_subpredicate = the_predicate->subpredicates[0];
	  for (the_class=0;the_class<the_num_classes;the_class++)
	    if (the_subpredicate->num_individuals[the_context_index][the_class] == 0) 
	      fprintf(a_file,"\t0");
	    else {
	    fprintf(a_file,"\t%.3f",(the_subpredicate->probabilities[the_context_index][the_class]
				   / (double)the_subpredicate->num_individuals[the_context_index][the_class]));
	    }
	  fprintf(a_file,"\n");
      } /* for each context */

  } /* for each predicate */
  fprintf(a_file,"\n");
}

void export_differences(FILE *a_file)
{
  int the_predicate_index, the_subpredicate_index, the_context_index;
  tpredicate *the_predicate;
  tsubpredicate *the_subpredicate;

  for (the_predicate_index=0;the_predicate_index<the_nb_predicates;the_predicate_index++) {
    the_predicate = &(the_predicate_table[the_predicate_index]);
    if ((the_predicate->kind == PROPERTY)&&(the_predicate_index != the_target_predicate))
      for (the_context_index=0;the_context_index<the_type_table[the_predicate->types[the_predicate->num_param]].num_contexts;the_context_index++)
	for (the_subpredicate_index=0;the_subpredicate_index<the_predicate->num_subpredicate;the_subpredicate_index++) {
	  the_subpredicate = the_predicate->subpredicates[the_subpredicate_index];
	  fprintf(a_file,"%f %f",the_subpredicate->probabilities[the_context_index][0], the_subpredicate->probabilities[the_context_index][1]);
	  fprintf(a_file,"\t%s",the_predicate->name);
	  print_parameter(a_file,the_predicate_index,the_subpredicate_index);
	  /*fprintf(a_file,"\t%d",the_context_index);*/
	  fprintf(a_file,"\t");
	  print_context(a_file,the_type_table[the_predicate->types[the_predicate->num_param]].contexts[the_context_index]);
	  fprintf(a_file,"\n");
	} /* for each subpredicate and each context */
  } /* for each predicate */
  fprintf(a_file,"\n");
}

void print_individual_parameter_class(FILE *a_file,int an_individual,int a_type,int a_predicate,int a_subpredicate,int a_class)
{
  fprintf(a_file,"Individual\t%s\tproperty\t%s\tvalue",
	  the_type_table[a_type].constants[the_part][an_individual],
	  the_predicate_table[a_predicate].name);
  print_parameter(a_file,a_predicate,a_subpredicate);
  fprintf(a_file,"\tclass");
  print_parameter(a_file,the_target_predicate,a_class);
  fprintf(a_file,"\n");
}

int deal_with_single_parent(int a_predicate,int an_index,int an_individual,int *occurences,
			    double *probabilities,int a_class,int status,tcontext *a_context)
{
  int the_error = 0;
  int the_other_individual;
  double *the_other_probabilities;
  int *the_occurences;
  int i;

  the_error = get_other_individual(a_predicate,an_individual,an_index,&the_other_individual);
  if (the_error == 40) return 0;
  if (the_error != 0) return the_error;
  the_occurences = Calloc(the_nb_predicates,int)
  if (the_occurences == NULL) return 2;
  for (i=0;i<the_nb_predicates;i++)
    if (i == a_predicate)
      the_occurences[i] = occurences[i] - 1;
    else
      the_occurences[i] = occurences[i];
  if (status == CLASSIFY)
    the_error = get_set_probabilities(the_other_individual,the_predicate_table[a_predicate].types[1-an_index],
				      the_occurences,&the_other_probabilities,-1,CLASSIFY,a_context);
  else
    the_error = get_set_probabilities(the_other_individual,the_predicate_table[a_predicate].types[1-an_index],
				      the_occurences,NULL,a_class,COUNT,a_context);
  if (the_error != 0) return the_error;
  if ((status == CLASSIFY) && (the_other_probabilities != NULL)) {
    for (i=0;i<the_num_classes;i++) {
      probabilities[i] += the_other_probabilities[i];
    }
    Free_calloc(the_num_classes,the_other_probabilities)
  }
  Free_calloc(the_nb_predicates,the_occurences)
  return the_error;
}

double add_log(double a,double b,int add_sub)
{
  if (a > b)
    return a + log(1 + add_sub * exp(b - a));
  else
    return b + log(1 + add_sub * exp(a - b));
}

int evaluate_pss(double **a_pac,int a_num_pac,double *tau,int a_num_target,double *the_probabilities_table)
{
  int the_error = 0;
  long the_num_subsets;
  int the_subset_length;
  int the_subset_index;
  double *the_pac_sum;
  double *f;
  double *the_pss;
  long the_quotient;
  int the_reste;
  int i,the_class,first;
  int *the_sign;

  the_pac_sum = Calloc(a_num_target,double)
  if (the_pac_sum == NULL) return 2;
  f = Calloc(a_num_target,double)
  if (f == NULL) return 2;
  the_pss = Calloc(a_num_target,double)
  if (the_pss == NULL) return 2;
  the_sign = Calloc(a_num_target,int)
  if (the_sign == NULL) return 2;
  the_num_subsets = (long)power(2,a_num_pac);
  for (the_subset_index=1;the_subset_index<the_num_subsets;the_subset_index++) {
    the_subset_length = 0;
    first = TRUE;
    the_quotient = the_subset_index;
    for (i=0;i<a_num_pac;i++) {
      the_reste = the_quotient % 2;
      the_quotient = the_quotient / 2;
      if (the_reste == 1) {
	the_subset_length++;
	for (the_class=0;the_class<a_num_target;the_class++) {
	  if (first == TRUE)
	    the_pac_sum[the_class] = a_pac[i][the_class] + log(1 - exp(tau[the_class]));
	  else
	    the_pac_sum[the_class] = add_log(the_pac_sum[the_class],a_pac[i][the_class] + log(1 - exp(tau[the_class])),1);
	}
	if (first == TRUE)
	  first = FALSE;
      } /* the_reste == 1 */
    } /* for i */
    for (the_class=0;the_class<a_num_target;the_class++) {
      f[the_class] = tau[the_class] + the_pac_sum[the_class] * the_subset_length - log(1 - exp(the_pac_sum[the_class]));
    }
    for (the_class=0;the_class<a_num_target;the_class++) {
      if (the_subset_index == 1) {
	the_sign[the_class] = (a_num_pac - the_subset_length) % 2; /* 0: positive if even, 1: negative if odd */
	the_pss[the_class] = (a_num_pac - the_subset_length) * the_pac_sum[the_class] + f[the_class];
      }
      else
	if ((a_num_pac - the_subset_length) % 2 == the_sign[the_class])
	  the_pss[the_class] = add_log(the_pss[the_class],(a_num_pac - the_subset_length) * the_pac_sum[the_class] + f[the_class],1);
	else
	  if (the_pss[the_class] > ((a_num_pac - the_subset_length) * the_pac_sum[the_class] + f[the_class]))
	    the_pss[the_class] = add_log(the_pss[the_class],(a_num_pac - the_subset_length) * the_pac_sum[the_class] + f[the_class],-1);
	  else {
	    the_sign[the_class] = 1 - the_sign[the_class];
	    the_pss[the_class] = add_log((a_num_pac - the_subset_length) * the_pac_sum[the_class] + f[the_class],the_pss[the_class],-1);
	  }
    } /* for each class */
  } /* for the_subset_index */
  for (the_class=0;the_class<a_num_target;the_class++) {
    the_probabilities_table[the_class] += the_pss[the_class];
    if (the_sign[the_class] == 1) {
      fprintf(stderr,"Negative Pss!\n");
      exit(-1);
    }
  }
  Free_calloc(a_num_target,the_pac_sum)
  Free_calloc(a_num_target,f)
  Free_calloc(a_num_target,the_pss)
  Free_calloc(a_num_target,the_sign)
  return the_error;
}

int compare_contexts(tcontext *first_context, tcontext *second_context)
{
  int the_result = 1;
  int k;

  if (first_context != second_context) {
    if ( (first_context == NULL)
	 || (second_context == NULL)
	 || (first_context->num_lits != second_context->num_lits) ) {
      the_result = 0;
    }
    else {
      if (first_context->new_lit->sign != second_context->new_lit->sign)
	the_result = 0;
      else {
	if (first_context->new_lit->predicate != second_context->new_lit->predicate)
	  the_result = 0;
	else {
	  for (k=0;(k<the_predicate_table[first_context->new_lit->predicate].arity)&&(the_result==1);k++)
	    if (first_context->new_lit->args[k] != second_context->new_lit->args[k])
	      the_result = 0;
	  if (the_result == 1) /* same new lit */
	    the_result = compare_contexts(first_context->ancestors,second_context->ancestors);
	}
      }
    }
  }
  return the_result;
}

int find_context(int a_type, tcontext *a_context, int status, int *a_context_index)
{
  int the_error = 0;
  int i,j,match;
  tcontext **the_contexts;
  double **the_probabilities;
  int *the_already_counted;
  double **the_num_individuals;
  ttype *the_individual_type;
  int the_num_contexts,the_predicate_index;
  tpredicate *the_predicate;
  long **the_type_num_individuals;
  
  the_individual_type = &(the_type_table[a_type]);
  the_num_contexts = the_individual_type->num_contexts;
  match = 0;
  for (i=0;(i<the_individual_type->num_contexts)&&(match==0);i++) {
    match = compare_contexts(a_context,the_individual_type->contexts[i]);
  } /* for all contexts */
  if (match == 0) {
    if (status == COUNT) {
      /* store the new context */
      the_num_contexts += 1;
      the_individual_type->num_contexts = the_num_contexts;
      the_contexts = Calloc(the_num_contexts,tcontext*)
      if (the_contexts == NULL) return 2;
      for (i=0;i<the_num_contexts-1;i++)
	the_contexts[i] = the_individual_type->contexts[i];
      Free_calloc(the_num_contexts-1,the_individual_type->contexts)
      the_individual_type->contexts = the_contexts;
      the_individual_type->contexts[the_num_contexts-1] = a_context;
      the_type_num_individuals = Calloc(the_num_contexts,long*)
      if (the_type_num_individuals == NULL) return 2;
      for (i=0;i<the_num_contexts-1;i++)
	the_type_num_individuals[i] = the_individual_type->num_individuals[i];
      the_type_num_individuals[the_num_contexts-1] = Calloc(the_num_classes,long)
      if (the_type_num_individuals[the_num_contexts-1] == NULL) return 2;
      for (j=0;j<the_num_classes;j++)
	the_type_num_individuals[the_num_contexts-1][j] = 0;
      Free_calloc(the_num_contexts-1,the_individual_type->num_individuals)
      the_individual_type->num_individuals = the_type_num_individuals;
      /* add one more cell to all tables of all subpredicates of all predicates of that type */
      for (the_predicate_index=0;the_predicate_index<the_nb_predicates;the_predicate_index++) {
	the_predicate = &(the_predicate_table[the_predicate_index]);
	if (the_predicate->types[the_predicate->num_param] == a_type) {
	  for (i=0;i<the_predicate->num_subpredicate;i++) {
	    the_probabilities = Calloc(the_num_contexts,double*)
	    if (the_probabilities == NULL) return 2;
	    the_already_counted = Calloc(the_num_contexts,int)
	    if (the_already_counted == NULL) return 2;
	    the_probabilities[the_num_contexts-1] = Calloc(the_num_classes,double)
	    if (the_probabilities[the_num_contexts-1] == NULL) return 2;
	    for (j=0;j<the_num_classes;j++)
	      the_probabilities[the_num_contexts-1][j] = 0;
	    the_already_counted[the_num_contexts-1] = 0;
	    for (j=0;j<the_num_contexts-1;j++) {
	      the_probabilities[j] = the_predicate->subpredicates[i]->probabilities[j];
	      the_already_counted[j] = the_predicate->subpredicates[i]->already_counted[j];
	    } /* for all contexts j */
	    Free_calloc(the_num_contexts-1,the_predicate->subpredicates[i]->probabilities)
	    the_predicate->subpredicates[i]->probabilities = the_probabilities;
	    Free_calloc(the_num_contexts-1,the_predicate->subpredicates[i]->already_counted)
	    the_predicate->subpredicates[i]->already_counted = the_already_counted;
	    if ((the_predicate->kind == STRUCTURAL) && (the_predicate->num_occurence != 0)
		&& ((the_predicate->dimensions[0] != 1)||(the_predicate->dimensions[1] != 1))) {
	      the_num_individuals = Calloc(the_num_contexts,double*)
	      if (the_num_individuals == NULL) return 2;
	      the_num_individuals[the_num_contexts-1] = Calloc(the_num_classes,double)
	      if (the_num_individuals[the_num_contexts-1] == NULL) return 2;
	      for (j=0;j<the_num_classes;j++)
		the_num_individuals[the_num_contexts-1][j] = 0;
	      for (j=0;j<the_num_contexts-1;j++) {
		the_num_individuals[j] = the_predicate->subpredicates[i]->num_individuals[j];
	      } /* for all contexts j */
	      Free_calloc(the_num_contexts-1,the_predicate->subpredicates[i]->num_individuals)
	      the_predicate->subpredicates[i]->num_individuals = the_num_individuals;
	    } /* non-determinate structural predicate */
	  } /* for all subpredicates i */
	} /* if it is the right type */
      } /* for each predicate */
      *a_context_index = the_num_contexts-1;
    } /* COUNT*/
  else
    the_error = 43;
  } /* match == 0 */
  else {
    *a_context_index = i - 1;
  } /* match == 1 */
  
  return the_error;
}

int deal_with_multiple_parents(int a_predicate,int an_index,int an_individual,int *occurences,
			       double *probabilities,int a_class,int status,tcontext *a_context)
{
  int the_error = 0;
  int *the_other_individuals;
  int the_num_individuals;
  double **the_other_probabilities;
  int *the_occurences;
  int i,the_class,the_context_index;

  the_error = get_all_other_individuals(a_predicate,an_individual,an_index,
					&the_other_individuals,&the_num_individuals);
  if (the_error != 0) return the_error;
  the_occurences = Calloc(the_nb_predicates,int)
  if (the_occurences == NULL) return 2;
  if (status == CLASSIFY) {
    the_other_probabilities = Calloc(the_num_individuals,double*)
    if (the_other_probabilities == NULL) return 2;
  }
  for (i=0;i<the_nb_predicates;i++)
    if (i == a_predicate)
      the_occurences[i] = occurences[i] - 1;
    else
      the_occurences[i] = occurences[i];
  for (i=0;(i<the_num_individuals)&&(the_error==0);i++)
    if (status == CLASSIFY)
      the_error = get_set_probabilities(the_other_individuals[i],
					the_predicate_table[a_predicate].types[1-an_index],
					the_occurences,&(the_other_probabilities[i]),-1,CLASSIFY,a_context);
    else /* COUNT */ {
      the_error = get_set_probabilities(the_other_individuals[i],
					the_predicate_table[a_predicate].types[1-an_index],
					the_occurences,NULL,a_class,COUNT,a_context);
    } /* COUNT */
  if (the_error != 0) return the_error;
  the_error = find_context(the_predicate_table[a_predicate].types[1-an_index],a_context,status,&the_context_index);
  if (status == CLASSIFY) {
    if (the_error == 43)
      the_error = 0;
    else
      if (the_error == 0) {
	switch (the_predicate_table[a_predicate].structural_kind) {
	case MS:
	case LI:
	  for (the_class=0;the_class<the_num_classes;the_class++) {
	    if (the_bayesian_approach2 == INDIVIDUAL)
	      probabilities[the_class] += the_predicate_table[a_predicate].subpredicates[0]->probabilities[the_context_index][the_class]; /* tau */
	    /* debug: print tau 
	       fprintf(stderr,"%f %f tau\n",the_predicate_table[a_predicate].subpredicates[0]->probabilities[the_context_index][0],the_predicate_table[a_predicate].subpredicates[0]->probabilities[the_context_index][1]);*/
	    for (i=0;i<the_num_individuals;i++) {
	      if (the_bayesian_approach2 == INDIVIDUAL)
		probabilities[the_class] += log(1 - exp(the_predicate_table[a_predicate].subpredicates[0]->probabilities[the_context_index][the_class]));
	      if (the_other_probabilities[i] != NULL)
		probabilities[the_class] += the_other_probabilities[i][the_class];
	    } /* for each individual i */
	  } /* for each class */
	  break;
	case SS:
	  evaluate_pss(the_other_probabilities,the_num_individuals,the_predicate_table[a_predicate].subpredicates[0]->probabilities[the_context_index],
		       the_num_classes,probabilities);
	  break;
	} /* switch */
      } /* context found */
    for (i=0;i<the_num_individuals;i++) {
      Free_calloc(the_num_classes,the_other_probabilities[i])
    }
    Free_calloc(the_num_individuals,the_other_probabilities)
  } /* CLASSIFY */
  else  /* COUNT */ 
    if (the_error == 0) {
      the_predicate_table[a_predicate].subpredicates[0]->probabilities[the_context_index][a_class] += the_num_individuals;
      the_predicate_table[a_predicate].subpredicates[0]->num_individuals[the_context_index][a_class] += 1;
    } /* COUNT */
  Free_calloc(the_nb_predicates,the_occurences)
  return the_error;
}

int deal_with_parents(int a_predicate,int an_index,int an_individual,int *occurences,
		      double *probabilities,int a_class,int status, tcontext *a_context)
{
  int the_error = 0;
  tcontext *the_context;
  tlist *the_elt;
  int found;
  
  if ( ((a_context == NULL)&&(the_max_lit > 2)) || ((a_context != NULL)&&(a_context->num_lits < the_max_lit - 2)) )
    /* the_max_lit is the number of structural predicates + 1 for the property (given on the command line)
       + 1 for the individual predicate added automatically by the initialise_search procedure called in 1BC.c */
    /* look for existing context */
    if ( (a_context == NULL)
	 || (a_context->new_lit->predicate != a_predicate)
	 || (a_context->new_lit->args[an_index] != a_context->num_lits)
	 || (the_predicate_table[a_predicate].dimensions[1-an_index] != 1)) {
      /* avoid a loop coming back to the same individual */
      found = FALSE;
      the_elt = the_contexts_list;
      while ((found == FALSE) && (the_elt != NULL)) {
	the_context = the_elt->elt;
	if ((the_context->ancestors == a_context)
	    && (the_context->new_lit->predicate == a_predicate)
	    && (the_context->new_lit->args[an_index] == the_context->num_lits - 1))
	  found = TRUE;
	the_elt = the_elt->next;
      } /* ((found == FALSE) && (the_elt != NULL)) */
      if (found == FALSE) {
	/* new context */
	the_context = Malloc(tcontext)
	if (the_context == NULL) return 2;
	the_elt = Malloc(tlist)
	if (the_elt == NULL) return 2;
	the_elt->elt = the_context;
	the_elt->next = the_contexts_list;
	the_contexts_list = the_elt;
	if (a_context == NULL)
	  the_context->num_lits = 1;
	else
	  the_context->num_lits = a_context->num_lits + 1;
	the_context->ancestors = a_context;
	the_context->new_lit = Malloc(tliteral)
	if (the_context->new_lit == NULL) return 2;
	the_context->new_lit->sign = POS;
	the_context->new_lit->predicate = a_predicate;
	the_context->new_lit->subpredicate = 0;
	the_context->new_lit->args = Calloc(the_predicate_table[a_predicate].arity,int) /* arity MUST be 2 for structural predicates! */
	if(the_context->new_lit->args == NULL) return 2;
	the_context->new_lit->args[an_index] = the_context->num_lits - 1;
	the_context->new_lit->args[1-an_index] = the_context->num_lits;
	if (the_predicate_table[a_predicate].dimensions[1-an_index] == 1) {
	  if (a_context == NULL)
	    the_context->functional = TRUE;
	  else
	    the_context->functional = a_context->functional;
	} /* functional */
	else {
	  the_context->functional = FALSE;
	} /* non-determinate */
      } /* (found == FALSE) */
      if (the_predicate_table[a_predicate].dimensions[1-an_index] == 1) {
	the_error = deal_with_single_parent(a_predicate,an_index,an_individual,occurences,probabilities,a_class,status,the_context);
      } /* functional */
      else {
	the_error = deal_with_multiple_parents(a_predicate,an_index,an_individual,occurences,probabilities,a_class,status,the_context);
      } /* non-determinate */
    } /* no loop */
  return the_error;
}

int get_parameter_value(int an_individual, int a_predicate, int *a_parameter_value)
{
  int the_error = 0;
  int the_subpredicate, the_instance;
  int stop = FALSE;
  
  for (the_subpredicate=0;(the_subpredicate<the_predicate_table[a_predicate].num_subpredicate) && (stop==FALSE);the_subpredicate++) {
    for (the_instance=0;(the_instance<the_predicate_table[a_predicate].subpredicates[the_subpredicate]->nb_inst[the_part]) && (stop==FALSE);the_instance++) {
      if (the_predicate_table[a_predicate].subpredicates[the_subpredicate]->instances[the_part][the_instance][0] == an_individual) {
	*a_parameter_value = the_subpredicate;
	stop = TRUE;
      } /* if match */
    } /* for each instance */
  } /* for each subpredicate */
	 
  if (stop == FALSE)
    the_error = 39;

  return the_error;
}

/* if status == CLASSIFY, get the probabilities of an_individual
   if status == COUNT, increase the probability of a_class for an_individual */
int get_set_probabilities(int an_individual,int a_type,int *occurences, double **probabilities,int a_class,int status,tcontext *a_context)
{
  int the_error = 0;
  double *the_probabilities = NULL;
  double *the_other_probabilities;
  int the_class,the_predicate;
  int the_parameter_value, the_context_index;

  if (status == CLASSIFY) {
    the_probabilities = Calloc(the_num_classes,double)
    if (the_probabilities == 0) return 2;
    for (the_class=0;the_class<the_num_classes;the_class++)
      the_probabilities[the_class] = 0;
  } /* (status == CLASSIFY) */
  else { /* COUNT */
    the_error = find_context(a_type,a_context,status,&the_context_index);
    if (the_error == 43)
      the_error = 0;
    else
      if (the_error == 0)
	the_type_table[a_type].num_individuals[the_context_index][a_class] += 1;
  } /* COUNT */
  for (the_predicate=0;(the_predicate<the_nb_predicates)&&(the_error==0);the_predicate++) {
    if ((the_predicate != the_target_predicate) && (occurences[the_predicate] > 0)) {
      switch (the_predicate_table[the_predicate].kind) {
      case INDIVIDUAL: break;
      case STRUCTURAL:
	if (the_predicate_table[the_predicate].types[0] == a_type)
	  the_error = deal_with_parents(the_predicate,0,an_individual,occurences,the_probabilities,a_class,status,a_context);
	if (the_predicate_table[the_predicate].types[1] == a_type)
	  the_error = deal_with_parents(the_predicate,1,an_individual,occurences,the_probabilities,a_class,status,a_context);
	break;
      case PROPERTY:
	if (the_predicate_table[the_predicate].types[the_predicate_table[the_target_predicate].num_param] == a_type) { /* if it is a property of a_type of an_individual */
	  the_error = get_parameter_value(an_individual,the_predicate,&the_parameter_value);
	  if (the_error == 39)
	    the_error = 0;
	  else {
	    if (status == CLASSIFY) {
	      the_error = find_context(a_type,a_context,status,&the_context_index);
	      if (the_error == 43)
		the_error = 0;
	      else
		if (the_error == 0) {
		  if (the_bayesian_approach2 == INDIVIDUAL)
		    the_other_probabilities = the_predicate_table[the_predicate].subpredicates[the_parameter_value]->probabilities[the_context_index];
		  /* the_other_probabilities == NULL if the subpredicate occurs in the test set but not in the training set! */
		  if (the_bayesian_approach2 == LANGUAGE) {
		    if (the_predicate_table[the_predicate].subpredicates[the_parameter_value]->already_counted[the_context_index] == FALSE) {
		      the_other_probabilities = the_predicate_table[the_predicate].subpredicates[the_parameter_value]->probabilities[the_context_index];
		      the_predicate_table[the_predicate].subpredicates[the_parameter_value]->already_counted[the_context_index] = TRUE;
		    } /* not already counted */
		    else
		      the_other_probabilities = NULL;
		  } /* the_bayesian_approach2 == LANGUAGE */
		  for (the_class=0;(the_other_probabilities!=NULL)&&(the_class<the_num_classes);the_class++)
		    the_probabilities[the_class] += the_other_probabilities[the_class];
		  /* debug: print two classes probabilities and feature
		  fprintf(stderr,"%f\t%f",the_probabilities[0],the_probabilities[1]);
		  fprintf(stderr,"\t%s",the_predicate_table[the_predicate].name);
		  print_parameter(stderr,the_predicate,the_parameter_value);
		 fprintf(stderr,"\t");
		  print_context(stderr,the_type_table[the_predicate_table[the_predicate].types[the_predicate_table[the_predicate].num_param]].contexts[the_context_index]);
		  fprintf(stderr,"\n");*/
		} /* context found */
	    } /* (status == CLASSIFY) */
	    else /* (status = COUNT) */ {
	      if (the_bayesian_approach2 == INDIVIDUAL)
		the_predicate_table[the_predicate].subpredicates[the_parameter_value]->probabilities[the_context_index][a_class] += 1;
	      if ((the_bayesian_approach2 == LANGUAGE)
		  && (the_predicate_table[the_predicate].subpredicates[the_parameter_value]->already_counted[the_context_index] == FALSE)) {
		the_predicate_table[the_predicate].subpredicates[the_parameter_value]->probabilities[the_context_index][a_class] += 1;
		the_predicate_table[the_predicate].subpredicates[the_parameter_value]->already_counted[the_context_index] = TRUE;
	      } /* LANGUAGE and not already counted */
	    } /* (status = COUNT) */
	  } /* parameter found */
	} /* the_predicate_table[the_predicate].types[0] == a_type) */
	break;
      } /* switch (the_table_predicate[i].kind) */
    } /* occurences[i] > 0 */
  } /* for each predicate i */
  
  if (status == CLASSIFY)
    *probabilities = the_probabilities;
  return the_error;
}

/******************************************************************************
  Give the index of the maximum value in a table of mpf
int index_max_mpf(mpf_t *a_table,int a_number)
{
int the_index,the_index_max;
mpf_t the_max;

 mpf_init(the_max);
the_index_max = 0;
mpf_set(the_max,a_table[0]);
for (the_index=1;the_index<a_number;the_index++) {
  if (mpf_cmp(a_table[the_index],the_max) > 0) {
    the_index_max = the_index;
    mpf_set(the_max,a_table[the_index]);
  }
}
return the_index_max;
}
******************************************************************************/


int count_classify(int an_individual, int *a_class, double **probabilities, int status,double *a_roc_weights,tcontext *a_context)
{
  int the_error = 0;
  int the_class;
  int *the_occurences;
  int the_individual_type;
  int the_predicate, the_subpredicate;
  long the_context;
  double the_sum;

  the_error = initialise_occurences(&the_occurences);
  if (the_error == 2) return 2;
  the_individual_type = the_predicate_table[the_target_predicate].types[the_predicate_table[the_target_predicate].num_param];
  if (the_bayesian_approach2 == LANGUAGE)
    for (the_predicate=0;the_predicate<the_nb_predicates;the_predicate++)
      for (the_subpredicate=0;the_subpredicate<the_predicate_table[the_predicate].num_subpredicate;the_subpredicate++)
	for (the_context=0;the_context<the_type_table[the_predicate_table[the_predicate].types[the_predicate_table[the_predicate].num_param]].num_contexts;
	     the_context++)
	  the_predicate_table[the_predicate].subpredicates[the_subpredicate]->already_counted[the_context] = FALSE;
  if (status == CLASSIFY) {
    the_error = get_set_probabilities(an_individual,the_individual_type,the_occurences,probabilities,-1,CLASSIFY,a_context);
    if ((the_error == 0) && (*probabilities != NULL)) {
      if (the_bayesian_approach2 == LANGUAGE)
	for (the_predicate=0;the_predicate<the_nb_predicates;the_predicate++)
	  if ((the_predicate_table[the_predicate].kind == PROPERTY) && (the_predicate_table[the_predicate].num_occurence > 0)
	      && (the_predicate != the_target_predicate))
	    for (the_context=0;the_context<the_type_table[the_predicate_table[the_predicate].types[the_predicate_table[the_predicate].num_param]].num_contexts;
		 the_context++) {
	      if ((the_type_table[the_predicate_table[the_predicate].types[the_predicate_table[the_predicate].num_param]].contexts[the_context] != NULL)
		  && (the_type_table[the_predicate_table[the_predicate].types[the_predicate_table[the_predicate].num_param]].contexts[the_context]->functional
		      == FALSE))
		for (the_subpredicate=0;the_subpredicate<the_predicate_table[the_predicate].num_subpredicate;the_subpredicate++)
		  if (the_predicate_table[the_predicate].subpredicates[the_subpredicate]->already_counted[the_context] == FALSE) {
		    the_predicate_table[the_predicate].subpredicates[the_subpredicate]->already_counted[the_context] = TRUE;
		    the_sum = 0;
		    if (the_predicate_table[the_predicate].subpredicates[the_subpredicate]->probabilities[the_context] != NULL)
		      for (the_class=0;the_class<the_num_classes;the_class++)
			the_sum += the_predicate_table[the_predicate].subpredicates[the_subpredicate]->probabilities[the_context][the_class];
		    if (the_sum < 0) /* the parameter occurs in the training set */
		      for (the_class=0;the_class<the_num_classes;the_class++)
			(*probabilities)[the_class] += 
			  log(1 - exp(the_predicate_table[the_predicate].subpredicates[the_subpredicate]->probabilities[the_context][the_class]));
		  } /* if for each subpredicate */
	    } /* for each context */
      for (the_class=0;the_class<the_num_classes;the_class++)
	(*probabilities)[the_class] += the_class_probabilities[the_class];
      /* debug: print the class a priori probabilities
	 fprintf(stderr,"%f %f the_class_probabilities\n", the_class_probabilities[0], the_class_probabilities[1]);*/
      the_class = index_max_weighted_double(*probabilities,a_roc_weights,the_num_classes);
      /*fprintf(stdout,"%f\t%f\n",(*probabilities)[0],(*probabilities)[1]);*/
    } /* ((the_error == 0) && (*probabilities != NULL)) */
    *a_class = the_class;
  }
  else /* (status == COUNT) */ {
    the_error = get_set_probabilities(an_individual,the_individual_type,the_occurences,NULL,*a_class,COUNT,a_context);
    the_class_probabilities[*a_class] += 1;
  }
  return the_error;
}

/* Evaluate 1BC2 accuracy (status == CLASSIFY) or initialise probabilities (status == COUNT).
When classifying, it returns the number of successful classification and the total number of classifications. */
int call_recursive_1BC(long *a_num_success,long *a_num_test,int status,int a_fold,int *a_fold_table,double *a_roc_weights,tlist **a_roc_list,int a_debug_flag)
{
  int the_error = 0;
  int the_individual;
  int the_individual_type;
  int i;
  long the_num_individuals = 0;
  long the_num_success = 0;
  long the_individual_total = 0;
  long the_success_total = 0;
  int the_predicted_class, the_actual_class;
  long the_useless_individuals;
  double *the_probabilities;
  troc *the_roc;
  tlist *the_list;
  
  the_part = first_partition();
  while (! last_partition(the_part)) {
    if ( ((the_num_fold == 1) && (a_fold_table == NULL))
	 || ((the_num_fold == 1) && (a_fold_table[the_part] != -1))
	 || ((status == CLASSIFY) && (a_fold_table[the_part] == a_fold))
	 || ((status == COUNT) && (a_fold_table[the_part] != -1) && (a_fold_table[the_part] != a_fold)) ) {
      the_individual_type = the_predicate_table[the_target_predicate].types[the_predicate_table[the_target_predicate].num_param];
      if (the_type_table[the_individual_type].global == TRUE)
	the_num_individuals = the_type_table[the_individual_type].nb_const[0];
      else
	the_num_individuals = the_type_table[the_individual_type].nb_const[the_part];
      the_num_success = 0;
      the_useless_individuals = 0;
      for (the_individual=0;(the_individual<the_num_individuals)&&(the_error==0);the_individual++) {
	the_error = get_parameter_value(the_individual,the_target_predicate,&the_actual_class);
	if (the_error == 39) { /* no class found */
	  the_error = 0;
	  the_useless_individuals++;
	}
	else {
	  if (status == CLASSIFY) {
	    if (a_debug_flag == TRUE)
	      fprintf(stdout,"%s\t",the_type_table[the_individual_type].constants[the_part][the_individual]);
	    the_error = count_classify(the_individual,&the_predicted_class,&the_probabilities,CLASSIFY,a_roc_weights,NULL);
	    if (a_debug_flag == TRUE) {
	      fprintf(stdout,"%d\t",the_actual_class);
	      for (i=0;i<the_num_classes;i++)
		fprintf(stdout,"%f\t",the_probabilities[i]);
	      fprintf(stdout,"\n");
	    } /* a_debug_flag == TRUE */
	    if (the_error == 0) {
	      if ((ROC == TRUE)||(roc_optimisation == TRUE)) {
		the_list = Malloc(tlist)
		if (the_list == NULL) the_error = 2;
		if (the_error == 0) {
		  the_roc = Malloc(troc)
		  if (the_roc == NULL) the_error = 2;
		  if (the_error == 0) {
		    the_roc->probabilities = Calloc(the_num_classes,double)
		    if (the_roc->probabilities == NULL) the_error = 2;
		    if (the_error == 0) {
		      for (i=0;i<the_num_classes;i++)
			the_roc->probabilities[i] = the_probabilities[i];
		      the_roc->individual = the_type_table[the_individual_type].constants[the_part][the_individual];
		      the_roc->class = the_actual_class;
		      the_list->elt = (void *)the_roc;
		      the_list->next = *a_roc_list;
		      *a_roc_list = the_list;
		      /* display the id, the actual and the predicted classes, and the "ratio" of probabilities */
		      fprintf(stderr,"%s, %d, %d, %lf\n",the_roc->individual,the_actual_class,the_predicted_class,the_probabilities[0]-the_probabilities[1]);
		    } /* (the_error == 0) */
		  } /* (the_error == 0) */
		} /* (the_error == 0) */
	      } /* ((ROC == TRUE)||(roc_optimisation == TRUE)) */
	      if (the_actual_class == the_predicted_class)
		the_num_success++;
	    }
	  } /* (status == CLASSIFY) */
	  else /* (status == COUNT) */ {
	    the_error = count_classify(the_individual,&the_actual_class,NULL,COUNT,0,NULL);
	    /*	    print_individual_parameter_class(stderr,the_individual,the_individual_type,the_target_predicate,the_actual_class,the_actual_class);
		    print_probabilities(stderr);*/
	  } /* (status == COUNT) */
	} /* if (the_error == 39) */
      } /* for each individual */
      if (the_error == 0) {
	if (status == CLASSIFY)
	  the_success_total += the_num_success;
	the_individual_total += the_num_individuals - the_useless_individuals;;
      } /* the_error == 0 */
      else if ((the_error==2)&&(ask_more_memory(NULL,NULL) == TRUE)){
	the_error = 0;
	the_part--;
      } /* the_error == 2 */
    } /* if ((the_num_fold == 1)... */
    the_part = next_partition(the_part);
  } /* while (! last_partition(the_part)) */
  if (status == CLASSIFY)
    *a_num_success = the_success_total;
  *a_num_test = the_individual_total;

  return the_error;
}

int tune_roc_weights(tlist *a_target_list,tlist *a_remaining_list,int a_discriminative_threshold_tag,double a_discriminative_threshold,
		   int a_fold,int *a_fold_table,double **a_roc_weights,double *a_best_value)
{
int the_error = 0;
int the_fold, *the_fold_table;
long the_num_individuals,*the_target_table,the_num_target,**the_property_table,the_num_test,the_num_success;
tlist *the_roc_list;

the_roc_list = NULL;
the_error = create_k_fold(the_num_fold,the_nb_parts,&the_fold_table,a_fold,a_fold_table);
for (the_fold=0;(the_fold<the_num_fold)&&(the_error==0);the_fold++) {
  /* COUNT */
  if (recursive_1BC == FALSE) {
    the_error = bayesian_counting(a_target_list,a_remaining_list,&the_num_individuals,&the_target_table,
				  &the_num_target,&the_property_table,the_fold,the_fold_table);
    the_num_classes = the_num_target;
    if (a_discriminative_threshold_tag == TRUE)
      select_discriminative_features(a_remaining_list,a_discriminative_threshold,the_target_table,the_num_target,
				     &the_property_table,&a_remaining_list);
  } /* (recursive_1BC == FALSE) */
  else /* (recursive_1BC == TRUE) */ {
    reset_recursive_1BC();
    the_error = call_recursive_1BC(NULL,&the_num_test,COUNT,the_fold,the_fold_table,NULL,NULL,FALSE);
    error_deal_with(1,the_error);
    estimate_probabilities();
  } /* (recursive_1BC == TRUE) */
  /* CLASSIFY */
  if (the_error == 0) {
    if (*a_roc_weights == NULL)
      error_deal_with(1,initialise_roc_weights(a_roc_weights,the_num_classes));
    if (recursive_1BC == FALSE)
      the_error = eval_classifier(the_num_target,the_target_table,the_num_individuals,the_property_table,
				  a_target_list,a_remaining_list,NULL,
				  &the_num_success,&the_num_test,the_fold,the_fold_table,*a_roc_weights,&the_roc_list,FALSE);
    else /* (recursive_1BC == TRUE) */ {
      the_error = call_recursive_1BC(&the_num_success,&the_num_test,CLASSIFY,the_fold,the_fold_table,*a_roc_weights,&the_roc_list,FALSE);
    }
    error_deal_with(1,the_error);
    if (recursive_1BC == FALSE)
      free_bayesian_tables(the_num_target,the_target_table,length_list(a_remaining_list),the_property_table);
  }
} /* for the_fold */
if (the_error == 0) {
  find_good_roc_weights(&the_roc_list,*a_roc_weights,the_num_classes,a_best_value);
    /*find_best_roc_weights(&the_roc_list,*a_roc_weights,the_num_classes);*/
  delete_roc_list(the_roc_list);
  Free_calloc(the_nb_parts,the_fold_table)
} /* (the_error == 0) */
return the_error;
}
