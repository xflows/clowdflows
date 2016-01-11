
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "types.h"
#include "input_output.h"
#include "eval.h"
#include "search.h"
#include "bayes.h"
#include "write.h"
#include "error.h"
#include "bayes2.h"
#include "roc.h"

int copy_literals(int a_nb_lit, tliteral *a_source_lits, tliteral *a_destination_lits)
{
int the_error = 0;
int i,j;

for (i=0;(i<a_nb_lit) && (the_error==0);i++)
  {
    a_destination_lits[i].sign = a_source_lits[i].sign;
    a_destination_lits[i].predicate = a_source_lits[i].predicate;
    a_destination_lits[i].subpredicate = a_source_lits[i].subpredicate;
    a_destination_lits[i].args = Calloc(the_predicate_table[a_source_lits[i].predicate].arity,int)
    if (a_destination_lits[i].args != NULL)
      for (j=0;j<the_predicate_table[a_source_lits[i].predicate].arity;j++)
	a_destination_lits[i].args[j] = a_source_lits[i].args[j];
    else
      {
	the_error = 2;
	for (j=0;j<i;j++)
	  Free_calloc(the_predicate_table[a_source_lits[j].predicate].arity,a_destination_lits[j].args)
      }
  }

return the_error;
}

int copy_and_unify_literals(int a_nb_lit, int a_first_var, int a_second_var, tliteral *a_source_lits, tliteral *a_destination_lits)
{
int the_error = 0;
int i,j;

for (i=0;(i<a_nb_lit) && (the_error==0);i++)
  {
    a_destination_lits[i].sign = a_source_lits[i].sign;
    a_destination_lits[i].predicate = a_source_lits[i].predicate;
    a_destination_lits[i].subpredicate = a_source_lits[i].subpredicate;
    a_destination_lits[i].args = Calloc(the_predicate_table[a_source_lits[i].predicate].arity,int)
    if (a_destination_lits[i].args != NULL) {
      for (j=0;j<the_predicate_table[a_source_lits[i].predicate].arity;j++)
	if (a_source_lits[i].args[j]<a_second_var)
	  a_destination_lits[i].args[j] = a_source_lits[i].args[j];
	else {
	  if (a_source_lits[i].args[j]==a_second_var)
	    a_destination_lits[i].args[j] = a_first_var;
	  else
	    a_destination_lits[i].args[j] = a_source_lits[i].args[j]-1;
	}
    }
    else
      {
	the_error = 2;
	for (j=0;j<i;j++)
	  Free_calloc(the_predicate_table[a_source_lits[j].predicate].arity,a_destination_lits[j].args)
      }
  }

return the_error;
}


int create_cube(tcube **a_cube)/*to gather initialisation in only one file !*/
{
int the_error = 0;

(*a_cube)->block_nb_inst = NULL;
(*a_cube)->blocks_table = NULL;

return the_error;
}


int delete_cube(tcube *a_cube)
{
int the_error = 0;
int i;

if (a_cube->blocks_table != NULL) {
  if (unification_form == EXPLICIT)
    Free_calloc(a_cube->nb_lit+a_cube->nb_substs,a_cube->blocks_table)
  else
    Free_calloc(a_cube->nb_lit,a_cube->blocks_table)
}
if (a_cube->block_nb_inst != NULL)
  Free_calloc(a_cube->num_blocks,a_cube->block_nb_inst)
for (i=0;i<a_cube->nb_lit;i++)
  Free_calloc(the_predicate_table[a_cube->lits[i].predicate].arity,a_cube->lits[i].args)
Free_calloc(a_cube->nb_lit,a_cube->lits)
if (a_cube->exp_lits != NULL)
  {
    for (i=0;i<a_cube->nb_lit;i++)
      if (a_cube->exp_lits[i].args != NULL)
	Free_calloc(the_predicate_table[a_cube->exp_lits[i].predicate].arity,a_cube->exp_lits[i].args)
    Free_calloc(a_cube->nb_lit,a_cube->exp_lits)
  }
if (a_cube->exp_args != NULL)
  Free_calloc(a_cube->nb_arg,a_cube->exp_args)
if (a_cube->corresp_args != NULL)
  Free_calloc(a_cube->nb_arg,a_cube->corresp_args)
if (a_cube->substs != NULL)
  Free_calloc(a_cube->nb_substs,a_cube->substs)
Free_calloc(a_cube->nb_arg,a_cube->args)
Free(a_cube)

return the_error;
}

int reverse(tlist *a_list, tlist **a_result)
{
tlist *the_first;
tlist *the_second = NULL;
tlist *the_next;

the_first = a_list;
while (the_first != NULL)
  {
    the_next = the_first->next;
    the_first->next = the_second;
    the_second = the_first;
    the_first = the_next;
  }
*a_result = the_second;
return 0;
}

int delete_list(tlist *a_list)
{
tlist *the_next;
tlist *the_pointer = a_list;

while (the_pointer != NULL)
  {
    the_next = the_pointer->next;
    Free(the_pointer)
    the_pointer = the_next;
  }
return 0;
}

int delete_cube_list(tlist *a_list)
{
tlist *the_next;
tlist *the_pointer = a_list;

while (the_pointer != NULL)
  {
    delete_cube((tcube*)the_pointer->elt);
    the_next = the_pointer->next;
    Free(the_pointer)
    the_pointer = the_next;
  }
return 0;
}

int indice(int a_value, int a_max, int *a_table)
{
int i=0;

while (i<a_max)
  if (a_table[i] == a_value)
    return i;
  else
    i++;
return i;
}

int merge_sorted_lists(tlist *a_first,tlist *a_second,int (a_function)(tlist *,tlist *),tlist **a_list)
{
int the_error = 0;
tlist *the_first,*the_second,*the_current;

the_current = NULL;
the_first = a_first;
the_second = a_second;
while ((the_first != NULL) && (the_second != NULL))
  {
    if (a_function(the_second,the_first))
      {
	if (the_current != NULL)
	  the_current->next = the_second;
	else
	  *a_list = the_second;
	the_current = the_second;
	the_second = the_second->next;
      }
    else
      {
	if (the_current != NULL)
	  the_current->next = the_first;
	else
	  *a_list = the_first;
	the_current = the_first;
	the_first = the_first->next;
      }
  }
if (the_first == NULL) {
  if (the_current != NULL)
    the_current->next = the_second;
  else
    *a_list = the_second;
}
else
  if (the_current != NULL)
    the_current->next = the_first;
  else
    *a_list = the_first;

return the_error;
}


int duplicate_and_reverse_list(tlist *a_source,tlist **a_copy)
{
int the_error = 0;
tlist *the_pointer;
tlist *the_new_elt;
tlist *the_copy_pointer;

the_pointer = a_source;
the_copy_pointer = NULL;
while ((the_pointer != NULL) && (the_error == 0))
  {
    the_new_elt = Malloc(tlist)
    if (the_new_elt == NULL)
      {
	the_error = 2;
	delete_list(the_copy_pointer);
      }
    else
      {
	the_new_elt->elt = the_pointer->elt;
	the_new_elt->next = the_copy_pointer;
	the_copy_pointer = the_new_elt;
	the_pointer = the_pointer->next;
      }
  }

*a_copy = the_copy_pointer;
return the_error;
}


int check_list(tlist *a_list, long a_max_length)
{
int the_error = 0;
long the_length = 0;
tlist *the_pointer;

the_pointer = a_list;
while ((the_pointer != NULL) && (the_length <= a_max_length))
  {
    the_pointer = the_pointer->next;
    the_length++;
  }
if (the_length > a_max_length)
  the_error = 1000;
return the_error;
}

long length_list(tlist *a_list)
{
long the_length = 0;
tlist *the_pointer;

the_pointer = a_list;
while (the_pointer != NULL)
  {
    the_pointer = the_pointer->next;
    the_length++;
  }
return the_length;
}

int ask_more_memory(tlist *a_result,tlist *a_remaining_nodes)
{
long the_new_memory;
int the_test;
char the_string[30];

if ((the_global_stop == FALSE) && (the_ask_more_memory_option == TRUE))
  {
    the_test = 0;
    while (the_test != 1)
      {
	fprintf(stderr,"\nThe max memory %ld Mb is reached.\n",the_max_memory/1000000);
	if ((a_result != NULL)||(a_remaining_nodes != NULL))
	  fprintf(stderr,"%ld clauses have been found while considering %ld nodes and at least %ld nodes are remaining.\n",length_list(a_result),the_node_counter,length_list(a_remaining_nodes));
	fprintf(stderr,"Please give a new max memory: ");
	fgets(the_string,30,stdin);
	the_test = sscanf(the_string,"%ld",&the_new_memory);
	the_new_memory *= 1000000;
      }
    if (the_new_memory > the_max_memory)
      {
	the_max_memory = the_new_memory;
	return TRUE;
      }
    else
      return FALSE;
  }
else
  return FALSE;
}


int check_sorted_list(tlist *a_list,int (a_function)(tlist *,tlist *))
{
int the_result = 1;
tlist *the_elt;

if (a_list != NULL)
  {
    the_elt = a_list;
    while (the_elt->next != NULL)
      {
	if (a_function(the_elt->next,the_elt))
	  the_result = 0;
	the_elt = the_elt->next;
      }
  }

return the_result;
}

int check_optimisticity(tlist *a_node, tlist *its_children)
{
int the_result = 1;
tlist *the_elt;
tlist *the_next;

the_elt = its_children;
while (the_elt != NULL)
  {
    /*    if ((((tcube*)a_node->elt)->optimistic < ((tcube*)the_elt->elt)->optimistic) && (((tcube*)the_elt->elt)->observed == 0))*/
    if (((tcube*)a_node->elt)->optimistic < ((tcube*)the_elt->elt)->optimistic)
      {
	the_result = 0;
	fprintf(stdout,"The heuristic is not optimistic !\n");
	the_next = a_node->next;
	a_node->next = NULL;
	write_clauses_appropriate(stdout,a_node,0);
	a_node->next = the_next;
	the_next = the_elt->next;
	the_elt->next = NULL;
	write_clauses_appropriate(stdout,the_elt,0);
	the_elt->next = the_next;
	eval_cube((tcube*)a_node->elt,0,NULL);
	eval_cube((tcube*)the_elt->elt,0,NULL);
      }
    the_elt = the_elt->next;
  }

return the_result;
}

int num_values_list(tlist *a_list, int (a_function)(tlist *,tlist *))
{
int the_counter;
tlist *the_current;

the_current = a_list;
the_counter = 1;
while(the_current != NULL) {
  if ((the_current->next != NULL) && (a_function(the_current->next,the_current)))
    the_counter += 1;
  the_current = the_current->next;
}

return the_counter;
}


int add_k_best(int *k, int (a_function)(tlist *,tlist *), tlist *a_node, tlist *a_list, tlist **a_result)
{
int the_error = 0;
tlist *the_result,*the_previous,*the_next,*the_current;
int stop;

the_current = a_list;
the_previous = NULL;
the_result = a_list;
stop = FALSE;
while ((the_current != NULL) && (stop == FALSE))
  {
    if (a_function(a_node,the_current))
      {
	the_previous = the_current;
	the_current = the_current->next;
      }
    else
      stop = TRUE;
  }
if (the_previous != NULL)
  {
    the_previous->next = a_node;
    a_node->next = the_current;
  }
else
  {
    the_result = a_node;
    a_node->next = the_current;
  }    
while (num_values_list(the_result,a_function) > *k) {
  the_next = the_result->next;
  the_result->next = NULL;
  delete_cube_list(the_result);
  the_result = the_next;
}


*a_result = the_result;
return the_error;
}


int remove_elt_list(tlist *an_elt,tlist *a_list,tlist **a_result)
{
int the_error = 0;
tlist *the_previous,*the_current,*the_result;

the_previous = NULL;
the_current = a_list;
the_result = a_list;
while (the_current != NULL)
    if (the_current == an_elt)
	{
	    if (the_previous == NULL)
		the_result = the_current->next;
	    else
		the_previous->next = the_current->next;
	    the_current->next = NULL;
	    delete_cube_list(the_current);
	    the_current = NULL;
	}
    else
	{
	    the_previous = the_current;
	    the_current = the_current->next;
	}
*a_result = the_result;
return the_error;
}

int argument_of_last_literal_only(int a_var,tcube *a_cube)
{
int the_result = FALSE;
int i,j;

for (i=0; (i<the_predicate_table[a_cube->lits[a_cube->nb_lit-1].predicate].arity) && (the_result == FALSE); i++)
  if (a_cube->lits[a_cube->nb_lit-1].args[i] == a_var)
    the_result = TRUE;
for (j=0; (j<a_cube->nb_lit-1) && (the_result == TRUE); j++)
  for (i=0; (i<the_predicate_table[a_cube->lits[j].predicate].arity) && (the_result == TRUE); i++)
    if (a_cube->lits[j].args[i] == a_var)
      the_result = FALSE;

return the_result;
}


int copy_predicate(tpredicate *a_predicate, tpredicate *a_new_predicate)
{
  a_new_predicate->name = a_predicate->name;
  a_new_predicate->kind = a_predicate->kind;
  a_new_predicate->structural_kind = a_predicate->structural_kind;
  a_new_predicate->arity = a_predicate->arity;
  a_new_predicate->num_occurence = a_predicate->num_occurence;
  a_new_predicate->types = a_predicate->types;
  a_new_predicate->num_param = a_predicate->num_param;
  a_new_predicate->params = a_predicate->params;
  a_new_predicate->paramIndices = a_predicate->paramIndices;
  a_new_predicate->varIndices = a_predicate->varIndices;
  a_new_predicate->dimensions = a_predicate->dimensions;
  a_new_predicate->cwa = a_predicate->cwa;
  a_new_predicate->num_subpredicate = a_predicate->num_subpredicate;
  a_new_predicate->subpredicates = a_predicate->subpredicates;
#if PROLOG
  a_new_predicate->term_ref = a_predicate->term_ref;
#endif
  return 0;
}

int copy_predicates(int a_kind, tpredicate *a_predicate_table, tpredicate *a_new_predicate_table, int *an_index)
{
int i;

for (i=0;i<the_nb_predicates;i++)
  if (a_predicate_table[i].kind == a_kind) {
    copy_predicate(&(a_predicate_table[i]),&(a_new_predicate_table[*an_index]));
    (*an_index) += 1;
  }
return 0;
}


int sort_predicates()
{
int the_error = 0;
int the_index, the_kind, i, found;
tpredicate *the_new_predicate_table;

the_new_predicate_table = Calloc(the_nb_predicates,tpredicate)
if (the_new_predicate_table == NULL) the_error = 2; else {
  the_index = 0;
  copy_predicates(INDIVIDUAL,the_predicate_table,the_new_predicate_table,&the_index);
  if ((the_search_bias == CLASSIFICATION)
      || (the_search_bias == POSITIVE_CLASSIFICATION)
      || (the_search_bias == HORN_POSITIVE_CLASSIFICATION)) {
    the_kind = PROPERTY;
    found = 0;
    for (i=0;(found==0)&&(i<the_nb_predicates);i++)
      if (the_predicate_table[i].kind == the_kind) {
	found = 1;
	copy_predicate(&(the_predicate_table[i]),&(the_new_predicate_table[the_index]));
	the_index++;
      }
  }
  copy_predicates(STRUCTURAL,the_predicate_table,the_new_predicate_table,&the_index);
  the_kind = PROPERTY;
  found = 0;
  for (i=0;i<the_nb_predicates;i++)
    if (the_predicate_table[i].kind == the_kind) {
      if (((the_search_bias == CLASSIFICATION)
	   || (the_search_bias == POSITIVE_CLASSIFICATION)
	   || (the_search_bias == HORN_POSITIVE_CLASSIFICATION))
	  &&(found == 0))
	found = 1;
      else {
	copy_predicate(&(the_predicate_table[i]),&(the_new_predicate_table[the_index]));
	the_index++;
      }
    }

  Free_calloc(NBPREDS,the_predicate_table)
  the_predicate_table = the_new_predicate_table;
}

return the_error;
}

int can_repeat_literal(tcube *a_cube)
{
int the_last_predicate, the_last_subpredicate;
int the_num_occurence,i;

if (a_cube->nb_lit > 0) {
  the_last_predicate = a_cube->lits[a_cube->nb_lit-1].predicate;
  the_last_subpredicate = a_cube->lits[a_cube->nb_lit-1].subpredicate;
  the_num_occurence = 0;
  for (i=0;i<a_cube->nb_lit;i++)
    if ((a_cube->lits[i].predicate == the_last_predicate)
	&& (a_cube->lits[i].subpredicate == the_last_subpredicate))
      the_num_occurence += 1;
  if (the_num_occurence < the_predicate_table[the_last_predicate].num_occurence)
    return TRUE;
  else
    return FALSE;
}

return FALSE;
}

int make_literal_blocks(tcube *a_cube)
{
int the_error = 0;
int *the_blocks_table;
int the_cube_nb_args;
tliteral *the_cube_lits;
int the_num_blocks = 0;
int finished,found,stop;
int the_num_remaining_var,the_starting_variable;
int the_literal_num,the_subst_num,the_arg_num;
int *already_considered;
int *the_remaining_vars;

if (unification_form == EXPLICIT) {
  the_cube_nb_args = a_cube->nb_exp_args;
  the_cube_lits = a_cube->exp_lits;
  the_blocks_table = Calloc(a_cube->nb_lit+a_cube->nb_substs,int)
  }
else {
  the_cube_nb_args = a_cube->nb_arg;
  the_cube_lits = a_cube->lits;
  the_blocks_table = Calloc(a_cube->nb_lit,int)
}
if (the_blocks_table == NULL) the_error = 2; else {
  already_considered = Calloc(the_cube_nb_args,int)
  if (already_considered == NULL) the_error = 2; else {
    the_remaining_vars = Calloc(the_cube_nb_args,int)
    if (the_remaining_vars == NULL) {the_error = 2; Free_calloc(the_cube_nb_args,already_considered)} else {
      if (unification_form == EXPLICIT)
	for (the_literal_num=0;the_literal_num<a_cube->nb_lit+a_cube->nb_substs;the_literal_num++)
	  the_blocks_table[the_literal_num] = -1;
      else
	for (the_literal_num=0;the_literal_num<a_cube->nb_lit;the_literal_num++)
	  the_blocks_table[the_literal_num] = -1;
      for (the_arg_num = 0; the_arg_num < the_cube_nb_args; the_arg_num++)
	already_considered[the_arg_num] = FALSE;
      if (individual_based == TRUE)
	for (the_arg_num=0;the_arg_num<the_predicate_table[the_cube_lits[0].predicate].arity;the_arg_num++)
	  already_considered[the_cube_lits[0].args[the_arg_num]] = TRUE;
      finished = FALSE;
      while (finished == FALSE) {
	the_starting_variable = 0;
	found = FALSE;
	while ((the_starting_variable < the_cube_nb_args) && (found == FALSE))
	  if (already_considered[the_starting_variable] == FALSE)
	    found = TRUE;
	  else
	    the_starting_variable++;
	if ((individual_based == TRUE) && (found == TRUE)) {
	  the_num_remaining_var = 1;
	  the_remaining_vars[0] = the_starting_variable;
	  already_considered[the_starting_variable] = TRUE;
	  while (found == TRUE) {
	    /* search constraints belonging to the block */
	    if (unification_form == EXPLICIT)
	      for (the_subst_num=0;the_subst_num<a_cube->nb_substs;the_subst_num++) 
		if (a_cube->substs[the_subst_num].flip == VAR) {
		  if (a_cube->substs[the_subst_num].arg == the_starting_variable) {
		    if (already_considered[a_cube->substs[the_subst_num].value] == FALSE) {
		      already_considered[a_cube->substs[the_subst_num].value] = TRUE;
		      the_num_remaining_var++;
		      the_remaining_vars[the_num_remaining_var-1] = a_cube->substs[the_subst_num].value;
		    }
		    the_blocks_table[a_cube->nb_lit+the_subst_num] = the_num_blocks;
		  }
		  else if (a_cube->substs[the_subst_num].value == the_starting_variable) {
		    if (already_considered[a_cube->substs[the_subst_num].arg] == FALSE) {
		      already_considered[a_cube->substs[the_subst_num].arg] = TRUE;
		      the_num_remaining_var++;
		      the_remaining_vars[the_num_remaining_var-1] = a_cube->substs[the_subst_num].arg;
		    }
		    the_blocks_table[a_cube->nb_lit+the_subst_num] = the_num_blocks;
		  }
		}
	    /* search constraints belonging to the block */
	    for (the_literal_num=0;the_literal_num<a_cube->nb_lit;the_literal_num++) {
	      stop = FALSE;
	      for (the_arg_num=0;(stop == FALSE)
		     && (the_arg_num<the_predicate_table[the_cube_lits[the_literal_num].predicate].arity);the_arg_num++)
		if (the_cube_lits[the_literal_num].args[the_arg_num] == the_starting_variable)
		  stop = TRUE;
	      if (stop == TRUE) {
		for (the_arg_num=0;the_arg_num<the_predicate_table[the_cube_lits[the_literal_num].predicate].arity;the_arg_num++)
		  if (already_considered[the_cube_lits[the_literal_num].args[the_arg_num]] == FALSE) {
		    already_considered[the_cube_lits[the_literal_num].args[the_arg_num]] = TRUE;
		    the_num_remaining_var++;
		    the_remaining_vars[the_num_remaining_var-1] = the_cube_lits[the_literal_num].args[the_arg_num];
		  }
		the_blocks_table[the_literal_num] = the_num_blocks;
	      }
	    }
	    /* next variable of the block or new block */
	    found = FALSE;
	    for (the_arg_num=0;(the_arg_num<the_num_remaining_var)&&(found == FALSE);the_arg_num++)
	      if ((the_remaining_vars[the_arg_num] == the_starting_variable)
		  && (the_arg_num+1<the_num_remaining_var)) {
		found = TRUE;
		the_starting_variable = the_remaining_vars[the_arg_num+1];
	      }
	    if (found == FALSE)
	      the_num_blocks++;
	  }
	}
	else {
	  finished = TRUE;
	  /*each remaining literal defines a new block*/
	  if (unification_form == EXPLICIT)
	    for (the_literal_num=0;the_literal_num<a_cube->nb_lit+a_cube->nb_substs;the_literal_num++) {
	      if (the_blocks_table[the_literal_num] == -1) {
		the_blocks_table[the_literal_num] = the_num_blocks;
		the_num_blocks++;
	      }
	    }
	  else
	    for (the_literal_num=0;the_literal_num<a_cube->nb_lit;the_literal_num++)
	      if (the_blocks_table[the_literal_num] == -1) {
		the_blocks_table[the_literal_num] = the_num_blocks;
		the_num_blocks++;
	      }
	}
      }
      Free_calloc(the_cube_nb_args,already_considered)
      Free_calloc(the_cube_nb_args,the_remaining_vars)
      a_cube->blocks_table = the_blocks_table;
      a_cube->num_blocks = the_num_blocks;
      a_cube->block_nb_inst = Calloc(the_num_blocks,long)
      if (a_cube->block_nb_inst == NULL) {
	the_error = 2;
	if (unification_form == EXPLICIT)
	  Free_calloc(a_cube->nb_lit+a_cube->nb_substs,the_blocks_table)
	else
	  Free_calloc(a_cube->nb_lit,the_blocks_table)
	}
    }
  }
}



return the_error;
}


int clause_location(tlist *a_list)
{
int the_location = 0;
int the_index = 1;
tlist* the_current;
int the_result;

the_current = a_list;
while ((the_current != NULL)&&(the_location == 0)) {
  error_deal_with(2,equivalent_clauses((tcube*)the_current->elt,the_global_clause,&the_result));
  if (the_result)
    the_location = the_index;
  the_index++;
  the_current = the_current->next;
}

return the_location;
}

int current_node_subsumption_test(tlist *a_node,tlist *a_list,int *do_not_store,int *a_result)
{
int the_error = 0;
tlist *the_node;
int the_result = 0;
int stop_this_test;

the_node = a_list;
while ((the_node != NULL) && (the_error == 0) && (*do_not_store == FALSE) && (the_result == 0)){
  stop_this_test = FALSE;
  /*  if (k_best != 0){
    if ((((tcube*)the_node->elt)->observed <= the_consistency_threshold)
	&& (((tcube*)a_node->elt)->observed <= the_consistency_threshold)){
      if (confirmation_value(a_node,the_node))
	stop_this_test = TRUE;
    }
    else*/
  if (confirmation_value(a_node,the_node)
      || (((tcube*)a_node->elt)->observed < ((tcube*)the_node->elt)->observed))
    stop_this_test = TRUE;
      /*  }*/
  if (stop_this_test == FALSE){
    the_error = subsumes((tcube*)the_node->elt,((tcube*)a_node->elt),&the_result);
    if (the_error == 0) {
      if (the_result)
	if (((tcube*)a_node->elt)->nb_lit < ((tcube*)the_node->elt)->nb_lit) {
	  the_error = subsumes(((tcube*)a_node->elt),(tcube*)the_node->elt,&the_result);
	  if (the_error == 0) {
	    if (the_result == 0)
	      the_result = 1;
	    else
	      the_result = 0;
	  }
	}
      if (the_result) {
	if (verbose == TRUE) {
	  fprintf(stderr,"s");
	  fflush(stderr);
	}
	*do_not_store = TRUE;
	the_result = 0;
      }
      else
	the_node = the_node->next;
    }
  }
  else
    the_node = the_node->next;
}

*a_result = the_result;
return the_error;
}


/*****************************************************************************
  Removes all elements of a_list subsumed by a_node
 *****************************************************************************/
int results_subsumption_test(tlist *a_node,tlist **a_list)
{
int the_error = 0;
tlist *the_node,*the_next;
int stop_this_test,the_result;
/*tlist *the_debug_next;*/

the_node = *a_list;
while ((the_node != NULL) && (the_error == 0)) {
  stop_this_test = FALSE;
  /*  if ((((tcube*)the_node->elt)->observed <= the_consistency_threshold)
      && (((tcube*)a_node->elt)->observed <= the_consistency_threshold)) {
    if (confirmation_value(the_node,a_node))
      stop_this_test = TRUE;
  }
  else*/
  if (confirmation_value(the_node,a_node)
      || (((tcube*)the_node->elt)->observed < ((tcube*)a_node->elt)->observed))
    stop_this_test = TRUE;
  if (stop_this_test == FALSE) {
    the_error = subsumes(((tcube*)a_node->elt),(tcube*)the_node->elt,&the_result);
    if (the_error == 0) {
      if (the_result) {
	the_next = the_node->next;
	remove_elt_list(the_node,*a_list,a_list);
	the_node = the_next;
      }
      else
	the_node = the_node->next;
    }
  }
  else
    the_node = the_node->next;
}

return the_error;
}

/********************************************************************************
********************************************************************************/
int refine_and_add_children(tlist **a_node,tlist *a_list_results,tlist **next_nodes,int *a_debug_flag)
{
int the_error;
tlist *the_children,*the_child,*the_next_child,*the_sorted_children,*the_next_nodes;
#if DEBUG
int the_debug_result;
tlist *the_debug_next;
#endif
/*int the_location;*/
tcube *the_cube;

the_cube = (tcube*)((*a_node)->elt);
the_next_nodes = *next_nodes;
the_error = refine_cube(the_cube,&the_children);
if (the_error == 0)/*refine_cube*/{
  the_child = the_children;
  while ((the_child != NULL) && (the_error == 0)) {
    the_error = eval_cube((tcube*)the_child->elt,0,NULL);
#if DEBUG
    if (the_error == 0) {
      the_error = equivalent_clauses((tcube*)the_child->elt,the_global_clause,&the_debug_result);
      if ((the_error == 0) && (the_debug_result)) {
	the_debug_next = the_child->next;
	the_child->next = NULL;
	write_clauses_appropriate(stdout,the_child,0);
	the_child->next = the_debug_next;
      }
    }
#endif
    if (the_error != 0)
      delete_cube_list(the_children);
    else {
      if (((the_expected_value != NAIVE)&&(the_expected_value != BACKGROUND)
	   && ((((tcube*)the_child->elt)->nb_inst_positive / (double)((tcube*)the_child->elt)->capital_n < the_frequency_threshold)
	       || (((tcube*)the_child->elt)->nb_inst_negative / (double)((tcube*)the_child->elt)->capital_n < the_frequency_threshold)))
	  ||((k_best != 0) && (can_prune(a_list_results,the_child) == 1))) {
	if (verbose == TRUE) {
	  fprintf(stderr,"p");
	  fflush(stderr);
	}
	the_next_child = the_child->next;
	remove_elt_list(the_child,the_children,&the_children);
	the_child = the_next_child;
      }
      else
	the_child = the_child->next;
    }
  }
  if (*a_debug_flag) {
    write_clauses_appropriate(stdout,the_children,0);
    *a_debug_flag = 0;
  }
  /*	      check_optimisticity(*a_node,the_children);*/
  if (the_error == 0) {
    if (search_approach == BEST) {
      quicksort(the_children,optimistic_then_observed_value,&the_sorted_children);
      merge_sorted_lists(the_sorted_children,the_next_nodes,optimistic_then_observed_value,&the_next_nodes);
      /*      the_location = clause_location(the_next_nodes);
      fprintf(stderr,"%d\n",the_location);
      if (the_location == 0)
	fprintf(stderr,"%d\n",the_cube->num);*/
      if (check_sorted_list(the_next_nodes,optimistic_then_observed_value) == 0)
	fprintf(stderr,"Not sorted list !!!\n");
    }
    else
      append(the_next_nodes,the_children,&the_next_nodes);
    (*a_node)->next = NULL;
    if (DO_NOT_DELETE_CURRENT_CUBE == FALSE)
      delete_cube_list(*a_node);
    else {
      delete_list(*a_node);
      DO_NOT_DELETE_CURRENT_CUBE =  FALSE;
    }
    *a_node = the_next_nodes;
  }
}

return the_error;
}

/********************************************************************************
********************************************************************************/
int bayes_can_add_predicates(tcube* a_cube)
{
int the_result=0;
int the_lit;

for (the_lit=0;the_lit<a_cube->nb_lit;the_lit++)
  if (the_predicate_table[a_cube->lits[the_lit].predicate].kind == PROPERTY)
    the_result++;
if (the_result<the_num_properties)
  the_result = TRUE;
else
  the_result = FALSE;

return the_result;
}

/********************************************************************************
********************************************************************************/
int initialise_discretised_types()
{
int the_error = 0;
int i,j,stop;

for (j=0;j<the_nb_types;j++)
  the_type_table[j].discretised = -1;
for (i=0;i<the_num_discretised_types;i++) {
  the_discretised_types_table[i].num_values = 0;
  if (the_discretised_types_table[i].kind == SDM) {
    the_discretised_types_table[i].sum = 0;
    the_discretised_types_table[i].sum2 = 0;
    the_discretised_types_table[i].thresholds = Calloc(2*the_discretised_types_table[i].num_inter,double)
    if (the_discretised_types_table[i].thresholds == NULL)
      return 2;
  }
  else {
    the_discretised_types_table[i].maxnumvalues = 1000;
    the_discretised_types_table[i].values = Calloc(the_discretised_types_table[i].maxnumvalues,double)
    if (the_discretised_types_table[i].values == NULL)
      return 2;
    if (the_discretised_types_table[i].kind == FFD) {
      the_discretised_types_table[i].maxnuminter = (long)ceil(the_discretised_types_table[i].maxnumvalues / the_discretised_types_table[i].size);
      the_discretised_types_table[i].thresholds = Calloc(the_discretised_types_table[i].maxnuminter-1,double)
    }
    else
      the_discretised_types_table[i].thresholds = Calloc(the_discretised_types_table[i].num_inter-1,double)
    if (the_discretised_types_table[i].thresholds == NULL)
      return 2;
  }
  stop = FALSE;
  for (j=0;(j<the_nb_types)&&(stop==FALSE);j++)
    if (strcmp(the_type_table[j].name,the_discretised_types_table[i].name)==0) {
      the_type_table[j].discretised = i;
      the_type_table[j].global = TRUE;
      stop = TRUE;
    }
}

return the_error;
}

/********************************************************************************
********************************************************************************/
int display_discretised_types(FILE *a_result_file)
{
int the_type,i;
int the_error = 0;
tdiscretised_type *the_dt;

for (the_type=0;the_type<the_nb_types;the_type++)
  if (the_type_table[the_type].discretised > -1) {
    the_dt = &(the_discretised_types_table[the_type_table[the_type].discretised]);
    fprintf(a_result_file,"%s\t",the_dt->name);
    for (i=0;i<the_dt->num_inter-1;i++)
      fprintf(a_result_file,"%d\t%f\t", i, the_dt->thresholds[i]);
    fprintf(a_result_file,"%d\n", i);
  }

return the_error;
}

/********************************************************************************
********************************************************************************/

int double_compare(const void *a, const void *b)
{
if (*((double*)a) > *((double*)b))
  return 1;
else if (*((double*)a) < *((double*)b))
  return -1;
else
  return 0;
}

int statistics_discretised_types()
{
int the_error = 0;
int the_type;
tdiscretised_type *the_dt;
int i;

for (the_type=0;the_type<the_nb_types;the_type++)
  if (the_type_table[the_type].discretised > -1) {
    the_dt = &(the_discretised_types_table[the_type_table[the_type].discretised]);
    if (the_dt->kind == SDM) {
      the_dt->mean = the_dt->sum / the_dt->num_values;
      the_dt->sigma = sqrt(the_dt->sum2 / the_dt->num_values - the_dt->mean * the_dt->mean);
      for (i=0;i<the_dt->num_inter*2;i++)
	the_dt->thresholds[i] = (0.5 - the_dt->num_inter + i) * the_dt->sigma + the_dt->mean;      
      the_dt->num_inter = the_dt->num_inter * 2 + 1;
    }
    else {
      qsort(the_dt->values,the_dt->num_values,sizeof(the_dt->values[0]),&double_compare);
      if (the_dt->kind == FFD) {
	the_dt->num_inter=0;
	for (i=0;i<the_dt->num_values-1;i++) {
	  if (i % the_dt->size  == 0) {
	    the_dt->thresholds[the_dt->num_inter] = (the_dt->values[i] + the_dt->values[i+1])/2;
	    the_dt->num_inter += 1;
	  }
	}
      }
      else {
	for (i=0;i<the_dt->num_inter-1;i++)
	  the_dt->thresholds[i] = (the_dt->values[(long)floor((double)the_dt->num_values*(i+1)/the_dt->num_inter)-1]
				   + the_dt->values[(long)floor((double)the_dt->num_values*(i+1)/the_dt->num_inter)])/2;
	Free_calloc(the_dt->num_values,the_dt->values)
      }
    }
  }

return the_error;
}

/********************************************************************************
********************************************************************************/
char *discrete_value(int a_discrete_type,double a_value,char *a_string)
{
int the_discrete_value = -1;
int i;

for (i=0;(i<the_discretised_types_table[a_discrete_type].num_inter-1)&&(the_discrete_value==-1);i++)
  if (a_value < the_discretised_types_table[a_discrete_type].thresholds[i])
    the_discrete_value = i;
if (the_discrete_value==-1)
  the_discrete_value = i;
sprintf(a_string,"%d",the_discrete_value);

return a_string;
}

#if 0
no longer used 14/7/99
char *get_discrete_name(ttype a_type,int a_part,int an_index,char *a_buffer)
{
char *the_name;

if (a_type.discretised == -1)
  the_name = a_type.constants[a_part][an_index];
else
  the_name = discrete_value(a_type.discretised,a_type.values[an_index],a_buffer);

return the_name;
}


int discretise_instance(tpredicate *a_predicate,int a_predicate_index,int a_sub_predicate,int a_part,
			int a_sign,int *an_instance)
{
int the_error = 0;
int *the_new_instance;
int the_num_param,the_num_arg;
char *the_name;
char the_buffer[SIZEPRED];

the_new_instance = Calloc(a_predicate->arity+a_predicate->num_param,int)
if (the_new_instance == NULL) the_error = 2; else {
  the_num_param = 0; the_num_arg = 0;
  while ((the_num_param + the_num_arg < a_predicate->arity+a_predicate->num_param)&&(the_error==0)) {
    if (a_predicate->params[the_num_param + the_num_arg] == TRUE)
      the_name = get_discrete_name(the_type_table[a_predicate->types[the_num_param]],0,
				   the_predicate_table[a_predicate_index].subpredicates[a_sub_predicate]
				   ->val_params[the_num_param],the_buffer);
    else
      if (the_type_table[a_predicate->types[a_predicate->num_param+the_num_arg]].global == TRUE)
	the_name = get_discrete_name(the_type_table[a_predicate->types[a_predicate->num_param + the_num_arg]],0,
				     an_instance[the_num_arg],the_buffer);
      else
	the_name = get_discrete_name(the_type_table[a_predicate->types[a_predicate->num_param + the_num_arg]],a_part,
				     an_instance[the_num_arg],the_buffer);
    the_error = meta_fact_name_store(a_predicate,&the_num_param,&the_num_arg,a_part+1,the_name,
				     TRUE,STORE,the_new_instance);
  }
  the_error = store_instance(the_new_instance,a_sign,a_predicate,the_nb_parts,a_part,TRUE,STORE);
  Free_calloc(a_predicate->arity+a_predicate->num_param,the_new_instance)
}

return the_error;
}
#endif

/********************************************************************************
********************************************************************************/
int free_predicate_instances(int a_predicate_index)
{
int the_sub_predicate,the_part,the_index;

for (the_part=0;the_part<the_nb_parts;the_part++) {
  for (the_sub_predicate=0;the_sub_predicate<the_predicate_table[a_predicate_index].num_subpredicate;the_sub_predicate++) {
    for (the_index=0;the_index<the_predicate_table[a_predicate_index].subpredicates[the_sub_predicate]
	   ->nb_inst[the_part];the_index++)
      Free_calloc(the_predicate_table[a_predicate_index].arity,
		  the_predicate_table[a_predicate_index].subpredicates[the_sub_predicate]
		  ->instances[the_part][the_index])
    Free_calloc(the_predicate_table[a_predicate_index].subpredicates[the_sub_predicate]->nb_inst[the_part],
		the_predicate_table[a_predicate_index].subpredicates[the_sub_predicate]->instances[the_part])
    for (the_index=0;the_index<the_predicate_table[a_predicate_index].subpredicates[the_sub_predicate]
	   ->nb_cinst[the_part];the_index++)
      Free_calloc(the_predicate_table[a_predicate_index].arity,
		  the_predicate_table[a_predicate_index].subpredicates[the_sub_predicate]
		  ->cinstances[the_part][the_index])
    Free_calloc(the_predicate_table[a_predicate_index].subpredicates[the_sub_predicate]->nb_cinst[the_part],
		the_predicate_table[a_predicate_index].subpredicates[the_sub_predicate]->cinstances[the_part])
  }
}
for (the_sub_predicate=0;the_sub_predicate<the_predicate_table[a_predicate_index].num_subpredicate;the_sub_predicate++) {
  Free_calloc(the_nb_parts,the_predicate_table[a_predicate_index].subpredicates[the_sub_predicate]->nb_inst)
  Free_calloc(the_nb_parts,the_predicate_table[a_predicate_index].subpredicates[the_sub_predicate]->nb_cinst)
  Free_calloc(the_nb_parts,the_predicate_table[a_predicate_index].subpredicates[the_sub_predicate]->instances)
  Free_calloc(the_nb_parts,the_predicate_table[a_predicate_index].subpredicates[the_sub_predicate]->cinstances)
}
return 0;
}

/********************************************************************************
********************************************************************************/
int free_instances()
{
int the_predicate_index;

for (the_predicate_index=0;the_predicate_index<the_nb_predicates;the_predicate_index++)
  free_predicate_instances(the_predicate_index);

return 0;
}

/********************************************************************************
  Free constants in all but global types
********************************************************************************/
int free_non_global_types()
{
int the_error = 0;
int i,j,p;

for (i=0; i<the_nb_types; i++) {
  if (the_type_table[i].global == FALSE) {
    for (p=0; p<the_nb_parts; p++) {
      for (j=0;j<the_type_table[i].nb_const[p];j++)
	Free_calloc(SIZECONST,the_type_table[i].constants[p][j])
      Free_calloc(the_type_table[i].nb_const[p],the_type_table[i].constants[p])
    }
    Free_calloc(the_nb_parts,the_type_table[i].nb_const)
    Free_calloc(the_nb_parts,the_type_table[i].constants)
    the_type_table[i].nb_const = NULL;
    the_type_table[i].constants = NULL;
  }
}

return(the_error);
}


/********************************************************************************
  Free constants in all types
********************************************************************************/
int free_types()
{
int the_error = 0;
int i,j,p;

for (i=0; i<the_nb_types; i++) {
  for (p=0; p<the_nb_parts; p++)
    if ((the_type_table[i].global == FALSE) || (p == 0)) {
      for (j=0;j<the_type_table[i].nb_const[p];j++)
	Free_calloc(SIZECONST,the_type_table[i].constants[p][j])
      Free_calloc(the_type_table[i].nb_const[p],the_type_table[i].constants[p])
    }
  Free_calloc(the_nb_parts,the_type_table[i].nb_const)
  Free_calloc(the_nb_parts,the_type_table[i].constants)
  the_type_table[i].nb_const = NULL;
  the_type_table[i].constants = NULL;
}

return(the_error);
}

#if 0
no longer used 14/7/99
/********************************************************************************
********************************************************************************/
int initialise_discrete_values()
{
int the_error = 0;
int the_type,the_predicate_index,the_sub_predicate,i,the_part,the_index;
tpredicate *the_predicate;
int *the_num_values;

the_num_values = Calloc(the_nb_types,int)
if (the_num_values == NULL) the_error = 2; else {
  for (the_type=0;the_type<the_nb_types;the_type++)
    if (the_type_table[the_type].discretised > -1) {
      the_num_values[the_type] = the_type_table[the_type].nb_values;
    }
  the_predicate = Malloc(tpredicate)
  if (the_predicate == NULL) the_error = 2; else {
    for (the_predicate_index=0;(the_error == 0)&&(the_predicate_index<the_nb_predicates);the_predicate_index++) {
      the_error = copy_predicate(&(the_predicate_table[the_predicate_index]),the_predicate);
      the_predicate->num_subpredicate = the_predicate_table[the_predicate_index].num_existing_subpredicate;
      the_predicate->subpredicates = Calloc(the_predicate->num_subpredicate,tsubpredicate*)
      if (the_predicate->subpredicates == NULL)
	the_error = 2;
      /* save existing subpredicates */
      for (the_sub_predicate=0;(the_error == 0)
	     &&(the_sub_predicate<the_predicate_table[the_predicate_index].num_existing_subpredicate);
	   the_sub_predicate++) {
	the_error = create_subpredicate(&(the_predicate->subpredicates[the_sub_predicate]),
					the_predicate,the_nb_parts);
	for (i=0;i<the_predicate->num_param;i++)
	  the_predicate->subpredicates[the_sub_predicate]->val_params[i] =
	    the_predicate_table[the_predicate_index].subpredicates[the_sub_predicate]->val_params[i];
      }
      for (the_part=0;(the_error == 0)&&(the_part<the_nb_parts);the_part++)
	for (the_sub_predicate=the_predicate_table[the_predicate_index].num_existing_subpredicate;(the_error == 0)
	       &&(the_sub_predicate<the_predicate_table[the_predicate_index].num_subpredicate);the_sub_predicate++) {
	  for (the_index=0;(the_error == 0)
		 &&(the_index<the_predicate_table[the_predicate_index].subpredicates[the_sub_predicate]
		    ->nb_inst[the_part]);the_index++)
	    the_error = discretise_instance(the_predicate,the_predicate_index,the_sub_predicate,the_part,POS,
					    the_predicate_table[the_predicate_index].subpredicates[the_sub_predicate]
					    ->instances[the_part][the_index]);
	  for (the_index=0;(the_error == 0)
		 &&(the_index<the_predicate_table[the_predicate_index].subpredicates[the_sub_predicate]
		    ->nb_cinst[the_part]);the_index++)
	    the_error = discretise_instance(the_predicate,the_predicate_index,the_sub_predicate,the_part,NEG,
					    the_predicate_table[the_predicate_index].subpredicates[the_sub_predicate]
					    ->cinstances[the_part][the_index]);
	}
      free_predicate_instances(the_predicate_index);
      the_predicate->num_existing_subpredicate = the_predicate->num_subpredicate;
      the_error = copy_predicate(the_predicate,&(the_predicate_table[the_predicate_index]));     
    }
    Free(the_predicate)
  }
  for (the_type=0;the_type<the_nb_types;the_type++)
    if (the_type_table[the_type].discretised > -1)
      Free_calloc(the_num_values[the_type],the_type_table[the_type].values)
  Free_calloc(the_nb_types,the_num_values)
}
return the_error;
}
#endif

/******************************************************************************
  Give the index of the maximum value in a table of double
******************************************************************************/
int index_max_weighted_double(double *a_table,double *weights,int a_number)
{
  int the_index,the_index_max;
  double the_max;
  
  the_index_max = 0;
  the_max = a_table[0] + weights[0];
  for (the_index=1;the_index<a_number;the_index++) {
    /*   fprintf(stdout,"%f,",a_table[the_index]);*/
    if ( (a_table[the_index] + weights[the_index]) > the_max) {
      the_index_max = the_index;
      the_max = a_table[the_index] + weights[the_index];
    }
  } /* for the_index */
  
  return the_index_max;
}


/******************************************************************************
  Return the nth node of a list
******************************************************************************/
int get_nth_node(tlist *a_list,int an_index,tlist **a_node)
{
int the_error = 0;
int the_index = 0;
tlist *the_result = NULL;
tlist *the_node = a_list;

while ((the_result == NULL)&&(the_node!=NULL)) {
  if (the_index == an_index)
    the_result = the_node;
  the_node = the_node->next;
  the_index++;
}

*a_node  = the_result;
return the_error;
}

/******************************************************************************
  Partition a set into k subsets
******************************************************************************/
int create_k_fold(int k, int n, int **a_new_fold_table,int a_fold,int *a_previous_fold_table)
{
int the_error = 0;
int i,the_index,the_fold;
int *the_internal_table;
int *the_result_table = NULL;

if (a_previous_fold_table == NULL) {
  /* shuffle */
  the_internal_table = Calloc(n,int)
  if (the_internal_table == NULL) the_error = 2; else {
    for (i=0;i<n;i++)
      the_internal_table[i] = -1;
    for (i=0;i<n;i++) {
      the_index = (int)random_long((long)(n-1));
      while (the_internal_table[the_index] != -1)
	if (the_index < n-1) the_index++; else the_index = 0;
      the_internal_table[the_index] = i;
      }
    /* partition */
    the_result_table = Calloc(n,int)
    if (the_result_table == NULL) the_error = 2; else {
      for (the_fold=0;the_fold<k;the_fold++)
	for(i=the_fold*n/k;i<(the_fold+1)*n/k;i++)
	  the_result_table[the_internal_table[i]] = the_fold;
      for (i=k*n/k;i<n;i++)
	the_result_table[the_internal_table[i]] = i-k*n/k;
    }
  }
} /* (a_previous_fold_table == NULL) */
else {
  the_result_table = Calloc(n,int)
  if (the_result_table == NULL) the_error = 2; else {
    the_fold = 0;
    for (i=0;i<n;i++)
      if (a_previous_fold_table[i] != a_fold) {
	the_result_table[i] = the_fold;
	the_fold++;
	if (the_fold == k)
	  the_fold = 0;
      } /* (a_previous_fold_table[i] == a_fold) */
      else
	the_result_table[i] = -1;
  }
}

*a_new_fold_table = the_result_table;
return the_error;
}

/******************************************************************************
  Remove clauses where structural predicates loop
******************************************************************************/
int filter_structural_predicates(tlist **a_list)
{
int the_error = 0;
tlist *the_node,*the_next,*the_result;
int byebye,the_lit1,the_lit2;
int the_predicate;
tcube *the_cube;

the_result = *a_list;
the_node = *a_list;
while (the_node != NULL) {
  the_next = the_node->next;
  the_lit1 = 0;
  byebye = FALSE;
  the_cube = (tcube*)the_node->elt;
  while ((byebye==FALSE)&&(the_lit1<the_cube->nb_lit-1)) {
    if (the_predicate_table[the_cube->lits[the_lit1].predicate].kind == STRUCTURAL) {
      the_predicate = the_cube->lits[the_lit1].predicate;
      the_lit2 = the_lit1 + 1;
      while ((byebye==FALSE)&&(the_lit2<the_cube->nb_lit))
	if (((tcube*)the_node->elt)->lits[the_lit2].predicate != the_predicate)
	  the_lit2++;
	else if (((the_cube->lits[the_lit2].args[0]
		   == the_cube->lits[the_lit1].args[0])
		  && (the_predicate_table[the_predicate].dimensions[1] == 1))
		 || ((the_cube->lits[the_lit2].args[1]
		      == the_cube->lits[the_lit1].args[1])
		     && (the_predicate_table[the_predicate].dimensions[0] == 1))) {
	  byebye = TRUE;
	  remove_elt_list(the_node,the_result,&the_result);
	}
	else
	  the_lit2++;
    }
    the_lit1++;
  }
  the_node = the_next;
}

*a_list = the_result;
return the_error;
}


double power(double x,long n)
{
long i;
double the_result;

the_result = 1;
for (i=0;i<n;i++)
  the_result *= x;

return the_result;
}

int find_first_non_individual_predicate()
{
  int i=0;

  while (the_predicate_table[i].kind == INDIVIDUAL) {
    i++;
  }

  return i;
}

double factoriel(long n)
{
  int i;
  int r = 1;

  for (i=2;i<=n;i++)
    r *= i;

  return r;
}

