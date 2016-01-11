
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <math.h>
#include "types.h"
#include "input_output.h"
#include "search.h"
#include "gestion.h"
#include "eval.h"
#include "error.h"
#include "write.h"
#include "bayes2.h" /* for ROC curves */

int the_num_fold;
int the_num_properties;
int bayesian_approach;
double the_discriminative_threshold;
int the_individual_predicate = 0;

/******************************************************************************
  Detect clause with unnecessary structural predicates
******************************************************************************/
int detect_opened_clause(tcube *a_cube,int *a_result)
{
int the_error = 0;
int the_num_flag = 0;
int *the_arg_flags;
int *the_lit_flags = NULL;
int the_lit,the_arg,the_result;

the_result = FALSE;
the_arg_flags = Calloc(a_cube->nb_arg,int)
if (the_arg_flags == NULL) the_error = 2; else
  the_lit_flags = Calloc(a_cube->nb_lit,int)
  if (the_lit_flags == NULL) the_error = 2; else {
    for (the_lit=1;the_lit<a_cube->nb_lit;the_lit++)
      the_lit_flags[the_lit] = -1;
    for (the_arg=0;the_arg<a_cube->nb_arg;the_arg++)
      the_arg_flags[the_arg] = -1;
    for (the_lit=1;the_lit<a_cube->nb_lit;the_lit++)
      if ((the_lit_flags[the_lit] == -1)&&(the_predicate_table[a_cube->lits[the_lit].predicate].kind == PROPERTY)) {
	the_num_flag++;
	the_lit_flags[the_lit] = 0;
	for (the_arg=0;the_arg<the_predicate_table[a_cube->lits[the_lit].predicate].arity;the_arg++)
	  if (the_arg_flags[a_cube->lits[the_lit].args[the_arg]] == -1)
	    the_arg_flags[a_cube->lits[the_lit].args[the_arg]] = 0;
      }
    while ((the_num_flag < a_cube->nb_lit-1)&&(the_result == FALSE))
      for (the_lit=1;the_lit<a_cube->nb_lit;the_lit++)
	if ((the_lit_flags[the_lit] == -1)&&(the_predicate_table[a_cube->lits[the_lit].predicate].kind == STRUCTURAL)) {
	  if (the_arg_flags[a_cube->lits[the_lit].args[0]] == -1)
	    {
	      if (the_arg_flags[a_cube->lits[the_lit].args[1]] != -1) {
		the_num_flag++;
		the_lit_flags[the_lit] = 0;
		the_arg_flags[a_cube->lits[the_lit].args[0]] = 0;
		the_arg_flags[a_cube->lits[the_lit].args[1]] = 1;
	      }
	    }
	  else {
	    if (the_arg_flags[a_cube->lits[the_lit].args[0]] == 0) {
	      if (the_arg_flags[a_cube->lits[the_lit].args[1]] == -1) {
		the_num_flag++;
		the_lit_flags[the_lit] = 0;
		the_arg_flags[a_cube->lits[the_lit].args[1]] = 0;
		the_arg_flags[a_cube->lits[the_lit].args[0]] = 1;
	      }
	      else {
		the_num_flag++;
		the_lit_flags[the_lit] = 0;
		the_arg_flags[a_cube->lits[the_lit].args[1]] = 1;
		the_arg_flags[a_cube->lits[the_lit].args[0]] = 1;
	      }
	    }
	    else /* the_arg_flags[a_cube->lits[the_lit].args[0]] == 1 */
	      if (the_arg_flags[a_cube->lits[the_lit].args[1]] == -1) {
		the_num_flag++;
		the_lit_flags[the_lit] = 0;
		the_arg_flags[a_cube->lits[the_lit].args[0]] = 1;
		the_arg_flags[a_cube->lits[the_lit].args[1]] = 0;
	      }
	      else if (the_arg_flags[a_cube->lits[the_lit].args[1]] == 0) {
		the_num_flag++;
		the_lit_flags[the_lit] = 0;
		the_arg_flags[a_cube->lits[the_lit].args[0]] = 1;
		the_arg_flags[a_cube->lits[the_lit].args[1]] = 1;
	      }
	      else
		the_result = TRUE;
	  }
	}
    for (the_arg=0;the_arg<the_predicate_table[a_cube->lits[0].predicate].arity;the_arg++)
      the_arg_flags[a_cube->lits[0].args[the_arg]] = 1;
    for (the_arg=0;the_arg<a_cube->nb_arg;the_arg++)
      if (the_arg_flags[the_arg] != 1)
	the_result = TRUE;
    Free_calloc(a_cube->nb_lit,the_lit_flags)
    Free_calloc(a_cube->nb_arg,the_arg_flags)
  }

*a_result = the_result;
return the_error;
}

/******************************************************************************
  Filter clauses with unnecessary structural predicates
******************************************************************************/
int filter_opened_clauses(tlist **a_list)
{
int the_error = 0;
tlist *the_list,*the_node,*the_next_node;
int the_result;

the_list = *a_list;
the_node = *a_list;
while ((the_node != NULL)&&(the_error == 0)) {
  the_next_node = the_node->next;
  the_error = detect_opened_clause((tcube*)(the_node->elt),&the_result);
  if (the_result == TRUE)
    remove_elt_list(the_node,the_list,&the_list);
  the_node = the_next_node;
}

*a_list = the_list;
return the_error;
}


/*******************************************************************************
  Generate all clauses containing exactly one property and the necessary structural predicates
  ******************************************************************************/
int generate_bayesian_properties(tlist *a_list_nodes, tlist **a_list_results,char *a_target_predicate)
{
int the_error = 0;
tlist *the_node,*the_next;
int the_result;
tcube *the_cube;
int i,j,k;
int the_previous_num_fold;
int spy;
int the_local_num_properties;

the_error = test_refine(a_list_nodes,a_list_results);

if (structure_based == TRUE) {
/* Remove clauses without properties, or containing a property and the target predicate */
the_node = *a_list_results;
while ((the_node != NULL)&&(the_error == 0)) {
  the_next = the_node->next;
  the_result = FALSE;
  the_local_num_properties = 0;
  the_cube = (tcube*)the_node->elt;
  for (i=0;(i<the_cube->nb_lit);i++)
    if (the_predicate_table[the_cube->lits[i].predicate].kind == PROPERTY) {
      the_local_num_properties += 1;
      if (strcmp(the_predicate_table[the_cube->lits[i].predicate].name,a_target_predicate) == 0)
	the_result = TRUE;
    }
  switch (the_local_num_properties) {
  case 0: the_result = FALSE; break;
  case 1: the_result = TRUE; break;
  default: if (the_result == TRUE) the_result = FALSE; else the_result = TRUE; break;
  }
  if (the_result == TRUE)
    if ((strcmp(the_predicate_table[the_cube->lits[the_cube->nb_lit-1].predicate].name,a_target_predicate) == 0)
	&& (the_cube->nb_arg != the_predicate_table[the_cube->lits[0].predicate].arity))
      the_result = FALSE;
  if (the_result == FALSE)
    remove_elt_list(the_node,*a_list_results,a_list_results);
  the_node = the_next;
}
}

/* Remove clauses with redondant structural predicates */
the_node = *a_list_results;
while ((the_node != NULL)&&(the_error == 0)) {
  the_next = the_node->next;
  the_result = FALSE;
  the_cube = (tcube*)the_node->elt;
  for (i=0;(i<the_cube->nb_lit)&&(the_result==FALSE);i++)
    if (the_predicate_table[the_cube->lits[i].predicate].kind == STRUCTURAL) {
      for (j=0;(j<the_cube->nb_lit)&&(the_result==FALSE);j++)
	if ((i != j) && (the_cube->lits[j].predicate == the_cube->lits[i].predicate)) {
	  the_result = TRUE;
	  for (k=0;(k<the_predicate_table[the_cube->lits[j].predicate].arity)&&(the_result==TRUE);k++)
	    if (the_cube->lits[j].args[k] != the_cube->lits[i].args[k])
	      the_result = FALSE;
	}      
    }
  if (the_result == TRUE)
    remove_elt_list(the_node,*a_list_results,a_list_results);
  the_node = the_next;
}

if (the_error == 0)
  the_error = filter_opened_clauses(a_list_results);

/*write_clauses_appropriate(stdout,*a_list_results,0);*/
/* Eval clauses */
the_previous_num_fold = the_num_fold;
the_num_fold = 1;
the_node = *a_list_results;
spy = 0;
while ((the_node != NULL)&&(the_error == 0)) {
  the_next = the_node->next;
  the_error = eval_cube((tcube*)(the_node->elt),0,NULL);
  if ((((tcube*)(the_node->elt))->nb_inst == 0) /* irrelevant features */
      || (((tcube*)(the_node->elt))->num_blocks > 1 + the_num_properties))
    remove_elt_list(the_node,*a_list_results,a_list_results);
  the_node = the_next;
  spy++;
}
the_num_fold = the_previous_num_fold;

return the_error;
}

/******************************************************************************
  Find whether a feature is functional
******************************************************************************/
int test_functional_feature(tlist *a_property_node,int *a_result)
{
int the_error = 0;
int the_num_flag = 0;
int *the_arg_flags;
int the_lit,the_arg;
int the_result = 0;
tcube *the_cube;

the_cube = (tcube*)a_property_node->elt;
the_arg_flags = Calloc(the_cube->nb_arg,int)
if (the_arg_flags == NULL) the_error = 2; else {
  /* orientate structural predicates */
  for (the_arg=0;the_arg<the_cube->nb_arg;the_arg++)
    the_arg_flags[the_arg] = -1;
  for (the_arg=0;the_arg<the_predicate_table[the_cube->lits[0].predicate].arity;the_arg++) {
    the_arg_flags[the_cube->lits[0].args[the_arg]] = 0;
    the_num_flag++;
  }
  while (the_num_flag < the_cube->nb_arg)
    for (the_lit=1;the_lit<the_cube->nb_lit;the_lit++)
      if (the_predicate_table[the_cube->lits[the_lit].predicate].kind == STRUCTURAL) {
	if ((the_arg_flags[the_cube->lits[the_lit].args[0]] != -1)
	    && (the_arg_flags[the_cube->lits[the_lit].args[1]] == -1)) {
	  the_arg_flags[the_cube->lits[the_lit].args[1]] = the_arg_flags[the_cube->lits[the_lit].args[0]] + 1;
	  the_num_flag++;
	}
	else if ((the_arg_flags[the_cube->lits[the_lit].args[0]] == -1)
		 && (the_arg_flags[the_cube->lits[the_lit].args[1]] != -1)) {
	  the_arg_flags[the_cube->lits[the_lit].args[0]] = the_arg_flags[the_cube->lits[the_lit].args[1]] + 1;
	  the_num_flag++;
	}
      }

  /* test whether the feature is functional */
  the_result = TRUE;
  for (the_lit=1;(the_lit<the_cube->nb_lit)&&(the_result==TRUE);the_lit++)
    if (the_predicate_table[the_cube->lits[the_lit].predicate].kind == STRUCTURAL) {
      if (the_arg_flags[the_cube->lits[the_lit].args[0]] > the_arg_flags[the_cube->lits[the_lit].args[1]]) {
	if (the_predicate_table[the_cube->lits[the_lit].predicate].dimensions[0] != 1) {
	  the_result = FALSE;
	}
      }
      else if (the_predicate_table[the_cube->lits[the_lit].predicate].dimensions[1] != 1) {
	the_result = FALSE;
      }
    }

  Free_calloc(the_cube->nb_arg,the_arg_flags)
}

*a_result = the_result;
return the_error;
}


/******************************************************************************
  Find whether a feature is multivalued and its number of values
******************************************************************************/
int get_number_values_feature(tlist *a_property_node,long *a_num_values)
{
int the_error = 0;
int the_lit;
int the_num_values = 0;
tcube *the_cube;

the_cube = (tcube*)a_property_node->elt;
the_num_values = 1;
for (the_lit=1;the_lit<the_cube->nb_lit;the_lit++)
  if (the_predicate_table[the_cube->lits[the_lit].predicate].kind == PROPERTY) {
    /*    if (the_predicate_table[the_cube->lits[the_lit].predicate].num_subpredicate == 1) changed on 3/Jan/2003 */
    if (the_predicate_table[the_cube->lits[the_lit].predicate].num_param == 0)
      the_num_values *= 2;
    else
      the_num_values *= the_predicate_table[the_cube->lits[the_lit].predicate].num_subpredicate;
  }

*a_num_values = the_num_values;
return the_error;
}


/******************************************************************************
 Invert the sign of the target property
******************************************************************************/
int invert_target_sign(tcube *a_cube)
{
int the_error = 0;
int i;

for (i=0;i<a_cube->nb_lit;i++)
  if (the_predicate_table[a_cube->lits[i].predicate].kind == PROPERTY) {
    if (a_cube->lits[i].sign == POS)
      a_cube->lits[i].sign = NEG;
    else
      a_cube->lits[i].sign = POS;
  }

return the_error;
}


/******************************************************************************
 Set the number of occurrences of the named predicate
******************************************************************************/
int set_predicate_occurrences(char *a_name,int a_number)
{
int the_error = 0;
int i;

for (i=0;i<the_nb_predicates;i++) {
  if (strcmp(the_predicate_table[i].name,a_name) == 0)
    the_predicate_table[i].num_occurence = a_number;
}

return the_error;
}


/******************************************************************************
Separate properties containing the target predicate
******************************************************************************/
int separate_target_properties(char *a_name,tlist *a_list,tlist **a_target_list,tlist **a_remaining_list)
{
int the_error = 0;
tlist *the_target_list,*the_remaining_list;
int i;
tcube *the_cube;
tlist *the_node,*the_next;
int the_result;

the_target_list = NULL; the_remaining_list = NULL;
the_node = a_list;
while (the_node != NULL) {
  the_next = the_node->next;
  the_result = FALSE;
  the_cube = (tcube*)the_node->elt;
  for (i=0;(i<the_cube->nb_lit)&&(the_result==FALSE);i++)
    if (strcmp(the_predicate_table[the_cube->lits[i].predicate].name,a_name) == 0)
      the_result = TRUE;
  if (the_result == TRUE) {
    the_node->next = the_target_list;
    the_target_list = the_node;
  }
  else {
    the_node->next = the_remaining_list;
    the_remaining_list = the_node;
  }
  the_node = the_next;
}

*a_target_list = the_target_list;
*a_remaining_list = the_remaining_list;
return the_error;
}

/******************************************************************************
Keep only the property matching the individual
******************************************************************************/
int filter_target_property(char *a_name,tlist **a_target_list)
{
int the_error = 0;
tlist *the_target_list,*the_remaining_list;
int i,j;
tcube *the_cube;
tlist *the_node,*the_next;
int the_result;

the_target_list = NULL; the_remaining_list = NULL;
the_node = *a_target_list;
while (the_node != NULL) {
  the_next = the_node->next;
  the_result = FALSE;
  the_cube = (tcube*)the_node->elt;
  for (i=0;(i<the_cube->nb_lit)&&(the_result==FALSE);i++)
    if (strcmp(the_predicate_table[the_cube->lits[i].predicate].name,a_name) == 0)
      the_result = TRUE;
  i--;
  if (the_predicate_table[the_cube->lits[0].predicate].arity
      != the_predicate_table[the_cube->lits[i].predicate].arity)
    error_deal_with(1,27);
  for (j=0;(j<the_predicate_table[the_cube->lits[i].predicate].arity)&&(the_result==TRUE);j++)
    if (the_cube->lits[i].args[j] != the_cube->lits[0].args[j])
      the_result = FALSE;
  if (the_result == TRUE){
    the_node->next = the_target_list;
    the_target_list = the_node;
  }
  else {
    the_node->next = the_remaining_list;
    the_remaining_list = the_node;
  }
  the_node = the_next;
}

delete_cube_list(the_remaining_list);
*a_target_list = the_target_list;
return the_error;
}

/******************************************************************************
  free literals and arguments of additional literals
******************************************************************************/
int free_literals_and_additional_args(tliteral *lits,int a_num_lit,int a_num_additional_lits)
{
int the_error = 0;
int i;

for (i=a_num_lit;i<a_num_lit+a_num_additional_lits;i++)
  Free_calloc(the_predicate_table[lits[i].predicate].arity,lits[i].args)
Free_calloc(a_num_lit,lits)

return the_error;
}

/******************************************************************************
  Add target literals to a property literals
******************************************************************************/
int add_target_literals_to_property(tliteral *target_lits,int a_target_nb_lit,
				    tliteral **property_lits,int a_property_nb_lit,int a_num_args)
{
int the_error = 0;
tliteral *the_new_lits;
int i,j;
int *the_corresponding_arg;

if (a_num_args != the_predicate_table[0].arity)
  error_deal_with(1,23);
else {
  the_corresponding_arg = Calloc(a_num_args,int)
  if (the_corresponding_arg == NULL) the_error = 2; else {
    for (j=0;j<a_num_args;j++)
      the_corresponding_arg[target_lits[0].args[j]] = (*property_lits)[0].args[j];
    the_new_lits = Calloc(a_property_nb_lit+a_target_nb_lit-1,tliteral)
    if (the_new_lits == NULL) the_error = 2; else {
      i=0;
      while (i<a_property_nb_lit) {
	the_new_lits[i] = (*property_lits)[i];
	i++;
      }
      while ((i<a_property_nb_lit+a_target_nb_lit-1)&&(the_error == 0)) {
	the_new_lits[i].sign = target_lits[i-a_property_nb_lit+1].sign;
	the_new_lits[i].predicate = target_lits[i-a_property_nb_lit+1].predicate;
	the_new_lits[i].subpredicate = target_lits[i-a_property_nb_lit+1].subpredicate;
	the_new_lits[i].args
	  = Calloc(the_predicate_table[target_lits[i-a_property_nb_lit+1].predicate].arity,int)
	if (the_new_lits[i].args == NULL) the_error = 2; else {
	  for (j=0;j<the_predicate_table[target_lits[i-a_property_nb_lit+1].predicate].arity;j++)
	    the_new_lits[i].args[j] = the_corresponding_arg[target_lits[i-a_property_nb_lit+1].args[j]];
	  i++;
	}
      }
      free_literals_and_additional_args(*property_lits,a_property_nb_lit,0);
      *property_lits = the_new_lits;
    }
    Free_calloc(a_num_args,the_corresponding_arg)
  }
}

return the_error;
}

/******************************************************************************
  Merge literals of the property and the target predicate
******************************************************************************/
int add_target_to_property(tcube *a_property_cube,tcube *a_target_cube)
{
int the_error = 0;

the_error = add_target_literals_to_property(a_target_cube->lits,a_target_cube->nb_lit,
					    &(a_property_cube->lits),a_property_cube->nb_lit,
					    a_target_cube->nb_arg);
if (the_error == 0) {
  the_error = add_target_literals_to_property(a_target_cube->exp_lits,a_target_cube->nb_lit,
					      &(a_property_cube->exp_lits),a_property_cube->nb_lit,
					      a_target_cube->nb_exp_args);
  a_property_cube->nb_lit += a_target_cube->nb_lit - 1;
}

return the_error;
}


/******************************************************************************
  Remove literals of the target from the property
******************************************************************************/
int remove_target_literals_from_property(tliteral **property_lits,int a_property_num_lits,
					 int a_target_num_lits)
{
int the_error = 0;
tliteral *the_new_lits;
int i;

the_new_lits = Calloc(a_property_num_lits,tliteral)
if (the_new_lits == NULL) the_error = 2; else {
  i=0;
  while (i<a_property_num_lits) {
    the_new_lits[i] = (*property_lits)[i];
    i++;
  }
  free_literals_and_additional_args(*property_lits,a_property_num_lits,a_target_num_lits-1);
  *property_lits = the_new_lits;
}

return the_error;
}

/******************************************************************************
  Remove implicit and explicit literals of the target from the property
******************************************************************************/
int remove_target_from_property(tcube *a_property_cube,tcube *a_target_cube)
{
int the_error = 0;

a_property_cube->nb_lit -= a_target_cube->nb_lit - 1;
remove_target_literals_from_property(&(a_property_cube->lits),a_property_cube->nb_lit,
				     a_target_cube->nb_lit);
remove_target_literals_from_property(&(a_property_cube->exp_lits),a_property_cube->nb_lit,
				     a_target_cube->nb_lit);

return the_error;
}


/******************************************************************************
  Initialise all Bayesian tables
******************************************************************************/
int bayesian_counting(tlist *a_target_list,tlist *a_property_list,
		      long *a_num_individuals,long **a_target_table,
		      long *a_num_target,long ***a_property_table,int a_fold,int *a_fold_table)
{
int the_error = 0;
long the_num_target,the_length_properties,the_target_index,the_property_index;
tlist *the_target_node,*the_property_node;
long **the_property_table,*the_target_table;
long N = 0;

the_num_target = length_list(a_target_list);
the_length_properties = length_list(a_property_list);
if (the_num_target == 1)
  the_property_table = Calloc(2+2,long*)
else
  the_property_table = Calloc(the_num_target+2,long*)
if (the_property_table == NULL) the_error = 2; else {
  if (the_num_target == 1)
    the_target_table = Calloc(2,long)
  else
    the_target_table = Calloc(the_num_target,long)
  if (the_target_table == NULL) the_error = 2; else {
    if (the_num_target == 1)
      *a_num_target = 2;
    else
      *a_num_target = the_num_target;
   /* initialise whether each property is functional or the kind of structure,
      and its number of values */
    the_property_table[*a_num_target] = Calloc(the_length_properties,long)
    if (the_property_table[*a_num_target] == NULL) the_error = 2; else {
      the_property_table[*a_num_target+1] = Calloc(the_length_properties,long)
      if (the_property_table[*a_num_target+1] == NULL) the_error = 2; else {
	the_property_index = 0;
	the_property_node = a_property_list;
	while ((the_property_node != NULL)&&(the_error == 0)) {
	  the_error = test_functional_feature(the_property_node,
					      (int*)&(the_property_table[*a_num_target][the_property_index]));
	  if (the_error == 0) {
	    if (the_property_table[*a_num_target][the_property_index] == TRUE) /*functional*/
	      the_error = get_number_values_feature(the_property_node,
						    &(the_property_table[*a_num_target+1][the_property_index]));
	    else/*non-determinate*/
	      the_property_table[*a_num_target+1][the_property_index] = 2;
	    if (the_error == 0) {
	      the_property_node = the_property_node->next;
	      the_property_index++;
	    }
	  }
	}
      }
    }
    the_target_index = 0;
    the_target_node = a_target_list;
    while ((the_target_node != NULL)&&(the_error == 0)) {
      the_error = eval_cube((tcube*)the_target_node->elt,a_fold,a_fold_table);
      Free_calloc(((tcube*)the_target_node->elt)->nb_lit,((tcube*)the_target_node->elt)->blocks_table)
      ((tcube*)the_target_node->elt)->blocks_table = NULL;
      Free_calloc(((tcube*)the_target_node->elt)->num_blocks,((tcube*)the_target_node->elt)->block_nb_inst)
      ((tcube*)the_target_node->elt)->block_nb_inst = NULL;
      the_target_table[the_target_index] = ((tcube*)the_target_node->elt)->nb_inst;
      N = ((tcube*)the_target_node->elt)->capital_n;
      the_property_table[the_target_index] = Calloc(the_length_properties,long)
      if (the_property_table[the_target_index] == NULL) the_error = 2; else {
	the_property_index = 0;
	the_property_node = a_property_list;
	while ((the_property_node != NULL)&&(the_error == 0)) {
	  the_error = add_target_to_property((tcube*)the_property_node->elt,(tcube*)the_target_node->elt);
	  if (the_error == 0) {
	    the_error = eval_cube((tcube*)the_property_node->elt,a_fold,a_fold_table);
	    Free_calloc(((tcube*)the_property_node->elt)->nb_lit,((tcube*)the_property_node->elt)->blocks_table)
	    ((tcube*)the_property_node->elt)->blocks_table = NULL;
	    Free_calloc(((tcube*)the_property_node->elt)->num_blocks,((tcube*)the_property_node->elt)->block_nb_inst)
	    ((tcube*)the_property_node->elt)->block_nb_inst = NULL;
	    the_property_table[the_target_index][the_property_index] = ((tcube*)the_property_node->elt)->nb_inst;
	    if (the_error == 0) {
	      the_error = remove_target_from_property((tcube*)the_property_node->elt,(tcube*)the_target_node->elt);
	      if (the_error == 0) {
		the_property_node = the_property_node->next;
		the_property_index++;
	      }
	    }
	  }
	}
	/* next target */
	if (the_num_target == 1) {
	  if (the_target_index == 1) {
	    invert_target_sign((tcube*)the_target_node->elt);
	    the_target_node = NULL;
	  }
	  else {
	    invert_target_sign((tcube*)the_target_node->elt);
	    the_target_index++;
	  }
	}
	else {
	  the_target_node = the_target_node->next;
	  the_target_index++;
	}
      }
    }
    
    *a_num_individuals = N;
    *a_target_table = the_target_table;
    *a_property_table = the_property_table;
  }
}

return the_error;
}


/*****************************************************************************
 *****************************************************************************/
int set_individual_var(tliteral *a_cube_lits,int a_cube_num_args,int **a_table)
{
int the_error = 0;
int the_individual_arity; /* number of variables in an individual */
int *the_individual_var = NULL; /* the indices of first the variables of individual, then the remaining variables */
int i,k,l;
int belong_to_individual; /* used to test whether a variable belongs to the individual description */

the_individual_var = Calloc(the_max_var,int)
if (the_individual_var == NULL) the_error = 2; else {
  if (individual_based == TRUE) {
    the_individual_arity = the_predicate_table[a_cube_lits[0].predicate].arity;
    for (i=0;i<the_individual_arity;i++)
      the_individual_var[i] = a_cube_lits[0].args[i];
    i=the_individual_arity;
    l=0;
    while (i<a_cube_num_args) {
      belong_to_individual = FALSE;
      for (k=0;(k<the_individual_arity)&&(belong_to_individual==FALSE);k++)
	if (the_individual_var[k] == l) belong_to_individual = TRUE;
      if (belong_to_individual == TRUE)
	l++;
      else {
	the_individual_var[i] = l;
	i++;
	l++;
      }
    }
  }
  else {
    the_individual_arity = a_cube_num_args;
    for (i=0;i<the_individual_arity;i++)
      the_individual_var[i] = i;
  }
}

*a_table = the_individual_var;
return the_error;
}

/*****************************************************************************
 *****************************************************************************/
int create_individual_and_their_constants(int **an_individual,int **individual_constants,long **individual_dividers)
{
int the_error = 0;
int *the_individual;
int *the_individual_constants = NULL;
long *the_individual_dividers = NULL;

the_individual = Calloc(the_predicate_table[the_individual_predicate].arity,int)
if (the_individual == NULL) the_error = 2; else {
  the_individual_constants = Calloc(the_predicate_table[the_individual_predicate].arity,int)
  if (the_individual_constants == NULL) the_error = 2; else {
    the_individual_dividers = Calloc(the_predicate_table[the_individual_predicate].arity,long)
    if (the_individual_dividers == NULL) the_error = 2;
  }
}

*an_individual = the_individual;
*individual_constants = the_individual_constants;
*individual_dividers = the_individual_dividers;
return the_error;
}


/*****************************************************************************
 *****************************************************************************/
int initialise_individual_constants(int the_part,int *individual_constants,
				    long *individual_dividers,long *a_num_individuals)
{
int the_error = 0;
int i;

for (i=0;i<the_predicate_table[the_individual_predicate].arity;i++)
  if (the_type_table[the_predicate_table[the_individual_predicate].types[i]].global == TRUE)
    individual_constants[i] = the_type_table[the_predicate_table[the_individual_predicate].types[i]].nb_const[0];
  else
    individual_constants[i] = the_type_table[the_predicate_table[the_individual_predicate].types[i]].nb_const[the_part];

individual_dividers[the_predicate_table[the_individual_predicate].arity-1] = 1;
for (i=the_predicate_table[the_individual_predicate].arity-2;(i>=0) && (the_error == 0);i--)
  if ((individual_constants[i+1] != 0)
      && (individual_dividers[i+1] >= LONG_MAX / individual_constants[i+1])) the_error = 11; else
    individual_dividers[i] = individual_dividers[i+1] * individual_constants[i+1];
if (the_error == 0) {
  if ((individual_constants[0] != 0)
      && (individual_dividers[0] >= LONG_MAX / individual_constants[0])) the_error = 11; else
    *a_num_individuals = individual_dividers[0] * individual_constants[0];
}

return the_error;
}

/*****************************************************************************
 *****************************************************************************/
int initialise_individual(long a_num_ind,int *an_individual,long *individual_dividers,int *individual_constants)
{
int the_error = 0;
int i;

for (i=0;i<the_predicate_table[the_individual_predicate].arity;i++)
  an_individual[i] = (a_num_ind / individual_dividers[i]) % individual_constants[i];

return the_error;
}

/*****************************************************************************
 *****************************************************************************/
int create_and_initialise_complement(int a_part,tcube *a_cube,int **a_substitution,int **complement_constants,
				     long **complement_dividers,long *a_num_complements,
				     int **corresponding_args)
{
int the_error = 0;
int *the_substitution;
int *the_corresponding_args;
tliteral *the_cube_lits;
int the_cube_num_args;
targ *the_cube_args;
int i;
int *the_complement_constants = NULL;
long *the_complement_dividers = NULL;

if (unification_form == EXPLICIT){
  the_cube_num_args = a_cube->nb_exp_args;
  the_cube_lits = a_cube->exp_lits;
  the_cube_args = a_cube->exp_args;
}
else{
  the_cube_num_args = a_cube->nb_arg;
  the_cube_lits = a_cube->lits;
  the_cube_args = a_cube->args;
}

the_substitution = Calloc(the_cube_num_args,int)
if (the_substitution == NULL) the_error = 2; else {
  the_complement_constants = Calloc(the_cube_num_args-the_predicate_table[0].arity,int)
  if (the_complement_constants == NULL) the_error = 2; else {
    the_complement_dividers = Calloc(the_cube_num_args-the_predicate_table[0].arity,long)
    if (the_complement_dividers == NULL) the_error = 2; else {
      the_error =  set_individual_var(the_cube_lits,the_cube_num_args,&the_corresponding_args);
      if (the_error == 0) {
	for (i=0;i<the_cube_num_args-the_predicate_table[0].arity;i++)
	  if (the_type_table[the_cube_args[the_corresponding_args[i+the_predicate_table[0].arity]].type].global == TRUE)
	    the_complement_constants[i]
	      = the_type_table[the_cube_args[the_corresponding_args[i+the_predicate_table[0].arity]].type].nb_const[0];
	  else
	    the_complement_constants[i]
	      = the_type_table[the_cube_args[the_corresponding_args[i+the_predicate_table[0].arity]].type]
	      .nb_const[a_part];
	if (the_cube_num_args-the_predicate_table[0].arity > 0) {
	  the_complement_dividers[the_cube_num_args-the_predicate_table[0].arity-1] = 1;
	  for (i=the_cube_num_args-the_predicate_table[0].arity-2;(i>=0) && (the_error == 0);i--)
	    if ((the_complement_constants[i+1] != 0)
		&& (the_complement_dividers[i+1] >= LONG_MAX / the_complement_constants[i+1])) the_error = 11; else
	      the_complement_dividers[i] = the_complement_dividers[i+1] * the_complement_constants[i+1];
	  if (the_error == 0) {
	    if ((the_complement_constants[0] != 0)
		&& (the_complement_dividers[0] >= LONG_MAX / the_complement_constants[0])) the_error = 11;
	    else
	      *a_num_complements = the_complement_dividers[0] * the_complement_constants[0];
	  }
	}
	else
	  *a_num_complements = 1;
      }
    }
  }
}
*a_substitution = the_substitution;
*complement_constants = the_complement_constants;
*complement_dividers = the_complement_dividers;
*corresponding_args= the_corresponding_args;
return the_error;
}

/*****************************************************************************
 *****************************************************************************/
int generate_substitution(int *an_individual,long a_complement_index,int *complement_constants,
			  long *complement_dividers,tcube *a_cube,int *a_substitution,int *corresponding_args)
{
int the_error = 0;
int i;
int the_cube_num_args;

if (unification_form == EXPLICIT){
  the_cube_num_args = a_cube->nb_exp_args;
}
else{
  the_cube_num_args = a_cube->nb_arg;
}
for (i=0;i<the_predicate_table[0].arity;i++)
  a_substitution[corresponding_args[i]] = an_individual[i];
for (i=0;i<the_cube_num_args-the_predicate_table[0].arity;i++)
  a_substitution[corresponding_args[i+the_predicate_table[0].arity]]
    = (a_complement_index / complement_dividers[i]) % complement_constants[i];

return the_error;
}

/*****************************************************************************
 *****************************************************************************/
int invert_sign_literals(tcube *a_cube)
{
tliteral *the_cube_lits;
int i;

if (unification_form == EXPLICIT){
  the_cube_lits = a_cube->exp_lits;
}
else{
  the_cube_lits = a_cube->lits;
}
for (i=0;i<a_cube->nb_lit;i++)
  if (the_cube_lits[i].sign == POS)
    the_cube_lits[i].sign = NEG;
  else
    the_cube_lits[i].sign = POS;
return 0;
}

/*****************************************************************************
 *****************************************************************************/
int satisfies(int a_part,int *an_individual,tcube *a_cube,long *a_result)
{
int the_error = 0;
int *the_complement_constants;
long *the_complement_dividers;
long the_num_complements;
int *the_corresponding_args;
int *the_substitution;
long the_complement_index;
tliteral *the_cube_lits;
int the_lit;
targ *the_cube_args;
int the_result;
long the_num_result;
int the_cube_num_args;

if (unification_form == EXPLICIT){
  the_cube_lits = a_cube->exp_lits;
  the_cube_args = a_cube->exp_args;
  the_cube_num_args = a_cube->nb_exp_args;
}
else{
  the_cube_lits = a_cube->lits;
  the_cube_args = a_cube->args;
  the_cube_num_args = a_cube->nb_arg;
}
the_error = create_and_initialise_complement(a_part,a_cube,&the_substitution,&the_complement_constants,
					     &the_complement_dividers,&the_num_complements,
					     &the_corresponding_args);
the_num_result = 0;
for (the_complement_index=0;
     (the_error==0)&&(the_complement_index<the_num_complements)&&((bayesian_approach==FIRST)||(the_num_result==0));
     the_complement_index++) {
  the_error = generate_substitution(an_individual,the_complement_index,the_complement_constants,
				    the_complement_dividers,a_cube,the_substitution,the_corresponding_args);
  invert_sign_literals(a_cube);
  the_result = 1;
  for(the_lit=1;(the_error==0)&&(the_lit<a_cube->nb_lit)&&(the_result==1);the_lit++)
    the_error = evaluate_literal(&(the_cube_lits[the_lit]),the_cube_args,a_part,the_substitution, &the_result);
  the_num_result += the_result;
  invert_sign_literals(a_cube);
}
if (the_error == 0) {
  Free_calloc(the_cube_num_args-the_predicate_table[0].arity,the_complement_constants)
  Free_calloc(the_cube_num_args-the_predicate_table[0].arity,the_complement_dividers)
  Free_calloc(the_cube_num_args,the_corresponding_args)
  Free_calloc(the_cube_num_args,the_substitution)
}

*a_result = the_num_result;
return the_error;
}

/*****************************************************************************
 *****************************************************************************/
int multi_valued(tcube *a_cube)
{
int the_result = FALSE;
int the_lit;

for (the_lit=1;the_lit<a_cube->nb_lit;the_lit++)
  if ((the_predicate_table[a_cube->lits[the_lit].predicate].kind == PROPERTY)
      &&(the_predicate_table[a_cube->lits[the_lit].predicate].num_subpredicate > 1))
    the_result = TRUE;

return the_result;
}

/*****************************************************************************
 *****************************************************************************/
int eval_classifier(int a_num_target,long *a_target_table,long a_num_individuals,long **a_property_table,
		    tlist *a_target_list,tlist *a_property_list,FILE *a_result_file,
		    long *a_num_success,long *a_num_test,int a_fold,int *a_fold_table,double *a_roc_weights,
		    tlist **a_roc_list,int a_debug_flag)
{
int the_error = 0;
int *the_individual;
int *the_individual_constants;
long *the_individual_dividers;
double *the_probabilities_table;
int the_part,the_class;
long the_length_properties;
long the_property_index;
tlist *the_property_node;
long the_result;
long the_num_individuals = 0;
long the_num_ind;
long the_num_success = 0;
tlist *the_class_node;
long the_individual_total = 0;
long the_success_total = 0;
int the_best_class;
tlist the_debug_node;
long the_list_length;
tlist *the_list;
troc *the_roc;
long the_useless_individuals;
int the_actual_class;
long the_sum;

the_probabilities_table = Calloc(a_num_target,double)
if (the_probabilities_table == NULL) the_error = 2; else {
  the_error = create_individual_and_their_constants(&the_individual,&the_individual_constants,&the_individual_dividers);
  the_part = first_partition();
  while (! last_partition(the_part)) {
    /*for (the_part=0;(the_part<the_nb_parts)&&(the_error==0);the_part++)*/
    if ( ((the_num_fold == 1) && (a_fold_table == NULL))
	 || ((the_num_fold == 1) && (a_fold_table[the_part] != -1))
	 || (a_fold_table[the_part] == a_fold)) {
      the_error = initialise_individual_constants(the_part,the_individual_constants,the_individual_dividers,
						  &the_num_individuals);
      the_num_success = 0;
      the_useless_individuals = 0;
      for (the_num_ind=0;(the_num_ind<the_num_individuals)&&(the_error==0);the_num_ind++) {
	initialise_individual(the_num_ind,the_individual,the_individual_dividers,the_individual_constants);
	/* look for actual class */
	the_result = 0;
	for (the_class=0;(the_class<a_num_target)&&(the_error==0)&&(the_result==0);the_class++) {
	  get_nth_node(a_target_list,the_class,&the_class_node);
	  the_error = satisfies(the_part,the_individual,(tcube*)the_class_node->elt,&the_result);
	  if (the_error == 0)
	    if (the_result != 0)
	      the_actual_class = the_class;
	} /* for all classes */
	if (the_result == 0) /* No class found! */
	  the_useless_individuals += 1;
	else /* class found */ {
	  for (the_class=0;the_class<a_num_target;the_class++)
	    the_probabilities_table[the_class] = (a_target_table[the_class] + 1) / ((double)a_num_individuals + a_num_target);
	  the_property_node = a_property_list;
	  the_length_properties = length_list(a_property_list);
	  the_list_length = 0;
	  for (the_property_index=0;(the_property_index<the_length_properties)&&(the_error==0);the_property_index++) {
	    the_sum = 0;
	    for (the_class=0;the_class<a_num_target;the_class++)
	      the_sum += a_property_table[the_class][the_property_index];
	    if (the_sum > 0) {
	      /* only if the subpredicate occurs in the current training set, because it is not necessarily the case during cross-validation */
	      /* else nothing, i.e. keep P=1 for each class */
	      the_error = satisfies(the_part,the_individual,(tcube*)the_property_node->elt,&the_result);
	      if (the_error == 0) {
		if (verbose == TRUE) {
		  the_debug_node.elt = (void *)the_property_node->elt;
		  the_debug_node.next = NULL;
		  write_clauses_appropriate(stderr,&the_debug_node,the_part);
		}
		if (the_result >= 1) {
		  /*	      if (verbose == TRUE)
			      fprintf(stderr,"satisfied\n");*/
		  for (the_class=0;the_class<a_num_target;the_class++) {
		    if (bayesian_approach == FIRST)
		      the_probabilities_table[the_class] *= power((a_property_table[the_class][the_property_index] + 1)
								  / ((double)a_target_table[the_class] + a_property_table[a_num_target+1][the_property_index]),the_result);
		    else
		      the_probabilities_table[the_class] *= (a_property_table[the_class][the_property_index] + 1)
			/ ((double)a_target_table[the_class] + a_property_table[a_num_target+1][the_property_index]);
		  } /* for the_class */
		} /* if (the_result >= 1) */
		else {
		  /*	      if (verbose == TRUE)
			      fprintf(stderr,"not satisfied\n");*/
		  /* non-determinate feature */
		  if ((bayesian_approach == LANGUAGE)
		      && (a_property_table[a_num_target][the_property_index] == FALSE))
		    for (the_class=0;the_class<a_num_target;the_class++)
		      the_probabilities_table[the_class]
			*= (a_target_table[the_class] - a_property_table[the_class][the_property_index] + 1)
			/ ((double)a_target_table[the_class] + a_property_table[a_num_target+1][the_property_index]);
		} /* else (the_result >= 1) */
	      } /* if (the_error == 0), i.e. satisfy */
	    } /* if (the_sum > 0) */
	    the_property_node = the_property_node->next;
	  } /* for the property index */
	  if (the_error == 0) {
	    for (the_class=0;the_class<a_num_target;the_class++)
	      the_probabilities_table[the_class] = log(the_probabilities_table[the_class]);
	    the_best_class = index_max_weighted_double(the_probabilities_table,a_roc_weights,a_num_target);
	    if (verbose == TRUE) {
	      for (the_class=0;the_class<a_num_target;the_class++) {
		fprintf(a_result_file,"%f ",the_probabilities_table[the_class]);
		/*		get_nth_node(a_target_list,the_class,&the_class_node);
				write_class(a_result_file,the_individual,(tcube*)the_class_node->elt,the_part);*/
	      }
	      fprintf(a_result_file,"\n");
	    } /* verbose */
	    if (a_debug_flag == TRUE) {
	      fprintf(stdout,"%s\t",the_type_table[0].constants[the_part][the_individual[0]]);
	      fprintf(stdout,"%d\t",the_actual_class);
	      for (the_class=0;the_class<a_num_target;the_class++)
		fprintf(stdout,"%f\t",the_probabilities_table[the_class]);
	      fprintf(stdout,"\n");
	    } /* a_debug_flag == TRUE */
	    if (the_best_class == the_actual_class)
	      the_num_success++;
	    if ((ROC == TRUE) || (roc_optimisation == TRUE)) {
	      the_list = Malloc(tlist)
	      if (the_list == NULL) the_error = 2;
	      if (the_error == 0) {
		the_roc = Malloc(troc)
		if (the_roc == NULL) the_error = 2;
		if (the_error == 0) {
		  the_roc->probabilities = Calloc(a_num_target,double)
		  if (the_roc->probabilities == NULL) the_error = 2;
		  if (the_error == 0) {
		    for (the_class=0;(the_class<a_num_target)&&(the_error==0);the_class++) {
		      the_roc->probabilities[the_class] = the_probabilities_table[the_class];
		    } /* for all classes */
		    the_roc->class = the_actual_class;
		    the_roc->individual = the_type_table[0].constants[the_part][the_individual[0]];
		    the_list->elt = (void *)the_roc;
		    the_list->next = *a_roc_list;
		    *a_roc_list = the_list;
		  } /* (the_error == 0) */
		} /* (the_error == 0) */
	      } /* (the_error == 0) */
	    } /* ((ROC == TRUE) || (roc_optimisation == TRUE)) */
	  } /* the_error == 0 */
	} /* class found */
      } /* for all individuals */
      if (the_error == 0) {
	the_success_total += the_num_success;
	the_individual_total += the_num_individuals - the_useless_individuals;
      }
      else if ((the_error==2)&&(ask_more_memory(NULL,NULL) == TRUE)){
	the_error = 0;
	the_part--;
      }
    } /* this part should be considered */
    the_part = next_partition(the_part);
  } /* for each part */
  if (the_error == 0) {
    Free_calloc(the_predicate_table[0].arity,the_individual)
    Free_calloc(the_predicate_table[0].arity,the_individual_constants)
    Free_calloc(the_predicate_table[0].arity,the_individual_dividers)
    Free_calloc(a_num_target,the_probabilities_table)
    *a_num_success = the_success_total;
    *a_num_test = the_individual_total;
  }
}

return the_error;
}

/*****************************************************************************
 *****************************************************************************/
int instantiate_cube(int *an_individual,tcube *a_cube)
{
int the_error = 0;
int i;
targ *the_cube_args;
tliteral *the_cube_lits;

if (unification_form == EXPLICIT) {
  the_cube_args = a_cube->exp_args;
  the_cube_lits = a_cube->exp_lits;
}
else {
  the_cube_args = a_cube->args;
  the_cube_lits = a_cube->lits;
}


if (the_cube_lits[0].predicate != 0)
  error_deal_with(1,24);
for (i=0;i<the_predicate_table[0].arity;i++) {
  the_cube_args[the_cube_lits[0].args[i]].value = an_individual[i];
  the_cube_args[the_cube_lits[0].args[i]].flip = CONST;
}

return the_error;
}

/******************************************************************************
  Write one dimensional table of long
******************************************************************************/
int write_one_dim_table(FILE *a_result_file,long *a_table,int a_num_column)
{
int i;

for (i=0;i<a_num_column;i++)
  fprintf(a_result_file,"%ld\t",a_table[i]);
fprintf(a_result_file,"\n");

return 0;
}

/******************************************************************************
  Write two dimensional table of long
******************************************************************************/
int write_two_dim_table(FILE *a_result_file,long **a_table,int a_num_column,int a_num_line)
{
int i,j;

for (i=0;i<a_num_line;i++) {
  for (j=0;j<a_num_column;j++)
    fprintf(a_result_file,"%ld\t",a_table[j][i]);
  fprintf(a_result_file,"\n");
}
return 0;
}

/******************************************************************************
  Free bayesian tables									       
******************************************************************************/
int free_bayesian_tables(int a_num_target,long *a_target_table,long a_num_properties,long **a_property_table)
{
int i;

for (i=0;i<a_num_target+2;i++)
  Free_calloc(a_num_properties,a_property_table[i])
Free_calloc(a_num_target,a_property_table)
Free_calloc(a_num_target,a_target_table)

return 0;
}

/******************************************************************************
select first-order features above the threshold		       
******************************************************************************/
int select_discriminative_features(tlist *a_list, float a_threshold, long *a_target_table, int a_num_target,
				   long ***a_property_table,tlist **an_output)
{
tlist *the_output = NULL;
tlist *the_trash_bin = NULL;
tlist *the_node;
tlist *the_next_node;
long the_length;
long i,j;
long the_index = 0;
long the_new_index = 0;
long **the_new_property_table;
long **the_last_property_table;
long the_population;
long the_num_sat_property;
int the_test;
double the_discriminative_power;

 the_population = 0;
 for (i=0;i<a_num_target;i++)
   the_population += a_target_table[i];
 the_length = length_list(a_list);
 the_new_property_table = Calloc(a_num_target+2,long*)
 if (the_new_property_table == NULL)
   error_deal_with(1,2);
 for (i=0;i<a_num_target+2;i++) {
   the_new_property_table[i] = Calloc(the_length,long)
   if (the_new_property_table[i] == NULL)
     error_deal_with(1,2);
 }
 the_node = a_list;
 while (the_node != NULL) {
   the_next_node = the_node->next;
   the_num_sat_property = 0;
   for (i=0;i<a_num_target;i++)
     the_num_sat_property += (*a_property_table)[i][the_index];
   the_test = FALSE;
   for (i=0;(i<a_num_target)&&(the_test==FALSE);i++) {
     the_discriminative_power = (*a_property_table)[i][the_index] / (double)the_population
       - a_target_table[i] * the_num_sat_property / (double)the_population / (double)the_population;
     if (the_discriminative_power < 0)
       the_discriminative_power = - the_discriminative_power;
     if (the_discriminative_power > a_threshold)
       the_test = TRUE;
   }
   if (the_test == TRUE) {
     for (i=0;i<a_num_target+2;i++)
       the_new_property_table[i][the_new_index] = (*a_property_table)[i][the_index];
     the_new_index++;
     the_node->next = the_output;
     the_output = the_node;
   }
   else {
     the_node->next = the_trash_bin;
     the_trash_bin = the_node;
   }
   the_node = the_next_node;
   the_index++;
 }

 the_last_property_table = Calloc(a_num_target+2,long*)
 if (the_last_property_table == NULL)
   error_deal_with(1,2);
 for (i=0;i<a_num_target+2;i++) {
   the_last_property_table[i] = Calloc(the_new_index,long)
   if (the_last_property_table[i] == NULL)
     error_deal_with(1,2);
 }
 for (i=0;i<a_num_target+2;i++)
   for (j=0;j<the_new_index;j++)
     the_last_property_table[i][j] = the_new_property_table[i][j];
 for (i=0;i<a_num_target+2;i++) {
   Free_calloc(the_length,the_new_property_table[i])
   Free_calloc(the_length,(*a_property_table)[i])
 }
 Free_calloc(a_num_target+2,the_new_property_table)
 Free_calloc(a_num_target+2,(*a_property_table))
 delete_cube_list(the_trash_bin);
 *a_property_table = the_last_property_table;
 reverse(the_output,an_output);
 fprintf(stdout,"%ld features above the threshold %f\n",the_new_index,a_threshold);
return 0;
}
