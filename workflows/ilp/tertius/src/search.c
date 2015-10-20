
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include "types.h"
#include "input_output.h"
#include "gestion.h"
#include "eval.h"
#include "search.h"
#include "write.h"
#include "error.h"
#include "roc.h"

int the_max_var;
int the_max_lit;
int the_search_bias;
double the_consistency_threshold;
long the_node_counter;
FILE *the_state_file;
int test_subsumption;
int k_best;
int consistency;
int DO_NOT_DELETE_CURRENT_CUBE;
int unification_form;
int search_approach;
double confirmation_level;
double khi_level;
int verbose;
tcube *the_global_clause;
double the_frequency_threshold;
int individual_based;
int structure_based;
int starting_clause;
int bayes;

/*****************************************************************************
 Deal with interruption outside the search
 *****************************************************************************/
void signal_deal_with(int sig)
{
/*  char the_string[10];
  
  signal(SIGINT, signal_deal_with);
  fprintf(stderr, "\nInterruption is not effective yet.\nHit any key to continue.\n");
  fgets(the_string, 10, stdin);
  fprintf(stderr, "Go on then !\n");*/
  exit( 0);
}

/*****************************************************************************
 Deal with interruption during the search
 *****************************************************************************/
void search_signal_deal_with(int sig)
{
  int the_loop = 1;
  char the_string[10];

  signal(SIGINT, search_signal_deal_with);
  fprintf(stderr, "\n");
  while (the_loop)
    {
      fprintf(stderr, "Do you really want to stop the search ? (y/n)\n");
      fgets(the_string, 10, stdin);
      switch (the_string[0])
	{
	case 'y':
	case 'Y': the_global_stop = TRUE;
	  fprintf(stderr, "The search is stopped. Current results will be given. Do not care about the following message on memory limitation.\n");
	  the_loop = 0;
	  break;
	case 'n': 
	case 'N': the_loop = 0;
	  fprintf(stderr, "Go on then !\n");
	  break;
	default: break;
	}
    }
}


int search(tlist *a_list_nodes, tlist **a_list_results)
{
int the_error = 0;
tlist *the_next_nodes;
tcube *the_cube;
int the_result;
tlist *the_current_node,*the_list_results;
tlist *the_copy;
int do_not_store;
tlist *the_new_result;
tlist *the_previous_sorted_results;
int the_debug_flag = 0;
#if DEBUG
tlist *the_debug_next;
int the_debug_result;
#endif

signal(SIGINT, search_signal_deal_with);
the_current_node = a_list_nodes;
the_list_results = NULL;
DO_NOT_DELETE_CURRENT_CUBE = FALSE;
while ((the_current_node != NULL) && (the_error == 0)) {
  the_node_counter++;
  display_current_value(stderr,the_list_results);
  fflush(stderr);
  the_cube = (tcube*)(the_current_node->elt);
  the_cube->num = the_node_counter;
#if DEBUG
  the_error = equivalent_clauses((tcube*)the_current_node->elt,the_global_clause,&the_debug_result);
  if ((the_error == 0) && (the_debug_result)) {
    the_debug_next = the_current_node->next;
    the_current_node->next = NULL;
    write_clauses_appropriate(stdout,the_current_node,0);
    write_clauses_appropriate(stdout,the_list_results,0);
    the_current_node->next = the_debug_next;
    the_debug_flag = 1;
  }
#endif
  do_not_store = FALSE;
  the_next_nodes = the_current_node->next;
  prune_tautology(the_cube,&the_result);
  if (the_result == 1)/*it's a tautology*/{
    if (verbose == TRUE){
      fprintf(stderr,"t");
      fflush(stderr);
    }
    the_current_node->next = NULL;
    if (DO_NOT_DELETE_CURRENT_CUBE == FALSE)
      delete_cube_list(the_current_node);
    else {
      delete_list(the_current_node);
      DO_NOT_DELETE_CURRENT_CUBE =  FALSE;
    }
    the_current_node = the_next_nodes;	  
  }
  if ((the_result == 0) && (test_subsumption == TRUE)) {
    /* If the_cube is subsumed by a known "true" clause, it must not be refined */
    the_error = current_node_subsumption_test(the_current_node,the_list_results,&do_not_store,&the_result);
    if ((the_error == 0) && (the_result == 1))/*the current node is subsumed by a previous result*/{
      the_current_node = the_next_nodes;	  
    }
  }
  if ((the_error == 0) && (the_result == 0)) {
    if ((do_not_store == FALSE)	&& (can_store(the_list_results,the_current_node))) {
      /* the_cube is a "true" clause */
      the_next_nodes = the_current_node->next;
      the_current_node->next = NULL;
      the_error = duplicate_and_reverse_list(the_current_node,&the_copy);
      DO_NOT_DELETE_CURRENT_CUBE = TRUE;
      if (verbose == TRUE){
	write_clauses_appropriate(the_state_file,the_current_node,0);
	fflush(the_state_file);
      }
      the_copy->next = the_list_results;
      the_list_results = the_copy;
      if (test_subsumption == TRUE) {
	/* Test whether a previous "true" clause is subsumed by it */
	the_error = results_subsumption_test(the_current_node,&(the_list_results->next));
      }
      if (k_best > 0) {
	add_k_best(&k_best,confirmation_then_observed_value,the_copy,the_list_results->next,&the_list_results);
      }
      else{
	the_new_result = the_list_results;
	the_previous_sorted_results = the_list_results->next;
	the_list_results->next = NULL;
	merge_sorted_lists(the_new_result,the_previous_sorted_results,
			   negated_confirmation_then_observed_value,&the_list_results);
      }
    }
  }
  if ((the_error == 0) && (the_result == 0))/*start refining*/{
    if (can_refine(the_list_results,the_current_node)) {
      if (verbose == TRUE) {
	fprintf(stderr,"r");
	fflush(stderr);
      }
      the_error = refine_and_add_children(&the_current_node,the_list_results,&the_next_nodes,&the_debug_flag);
    }
    else{
      /* do not refine the current node */
      the_current_node->next = NULL;
      if (DO_NOT_DELETE_CURRENT_CUBE == FALSE)
	delete_cube_list(the_current_node);
      else {
	delete_list(the_current_node);
	DO_NOT_DELETE_CURRENT_CUBE =  FALSE;
      }
      the_current_node = the_next_nodes;
    }
  }

  if (the_error == 2) {
    if(ask_more_memory(the_list_results,the_next_nodes) == TRUE)
      the_error = 0;
    else
      /* do not delete the current node since it can be used in the result list !*/
      delete_cube_list(the_next_nodes);
  }
}

*a_list_results = the_list_results;
signal(SIGINT, signal_deal_with);
return the_error;
}

int test_refine(tlist *a_list_nodes, tlist **a_list_results)
{
int the_error = 0;
tlist *the_children;
tlist *the_next_nodes;
tlist *the_list_results = NULL;
tlist *the_children_double;
tlist *the_current_node;
int the_debug_flag = 0;
#if DEBUG
int the_debug_result;
tlist *the_debug_next;
tlist the_debug_node;
#endif

the_current_node = a_list_nodes;
while ((the_current_node != NULL) && (the_error == 0)) {
  the_node_counter++;
  ((tcube*)the_current_node->elt)->num = the_node_counter;
#if DEBUG
  the_error = equivalent_clauses((tcube*)the_current_node->elt,the_global_clause,&the_debug_result);
  if ((the_error == 0) && (the_debug_result))
    {
      the_debug_node.elt = (void*)the_global_clause;
      the_debug_node.next = NULL;
      write_clauses_appropriate(stdout,&the_debug_node,0);
      the_debug_next = the_current_node->next;
      the_current_node->next = NULL;
      write_clauses_appropriate(stdout,the_current_node,0);
      the_current_node->next = the_debug_next;
      the_debug_flag = 1;
    }
#endif
  /*  the_debug_next = the_current_node->next;
  the_current_node->next = NULL;
  fprintf(stdout,"\t");
  write_clauses_appropriate(stdout,the_current_node,0);
  the_current_node->next = the_debug_next;*/
  the_error = refine_cube((tcube*)the_current_node->elt,&the_children);
  if (the_error == 0) {
    if (the_debug_flag == 1) {
      write_clauses_appropriate(stdout,the_children,0);
      the_debug_flag = 0;
    }
    the_error = duplicate_and_reverse_list(the_children, &the_children_double);
    if (the_error == 0) {
      append(the_current_node->next,the_children,&the_next_nodes);
      append(the_children_double,the_list_results,&the_list_results);
      the_current_node->next = NULL;
      delete_list(the_current_node);
      the_current_node = the_next_nodes;
    }
    else
      if(ask_more_memory(the_list_results,the_current_node->next) == TRUE)
	the_error = 0;
      else {
	delete_list(the_children);
	delete_list(the_current_node);
      }
  }
  else
    if(ask_more_memory(the_list_results,the_current_node->next) == TRUE)
      the_error = 0;
    else
      delete_list(the_current_node);
}
*a_list_results = the_list_results;
return the_error;
}

int build_children_instanciate(tcube *a_cube,int a_var,tlist **a_list_children)
{
int the_error = 0;
tcube *the_child;
int i,j;
tlist *the_list_p;
tlist *the_list_children = NULL;

if ((a_cube->args[a_var].flip == VAR) && (the_type_table[a_cube->args[a_var].type].global == TRUE))
  for (j=0;j<the_type_table[a_cube->args[a_var].type].nb_const[0];j++)
    {
      the_child = Malloc(tcube)
      if (the_child == NULL)
	the_error = 2;
      else
	{
	  the_child->lits = Calloc(the_max_lit,tliteral)
	    if (the_child->lits == NULL)
	      {
		the_error = 2;
		Free(the_child)
	      }
	    else
	      {
		the_error = create_cube(&the_child);
		the_child->flag_instanciation = a_var+1;
		the_child->num = 0;
		the_child->first_var = a_cube->first_var;
		the_child->second_var = a_cube->second_var;
		the_child->nb_positive = a_cube->nb_positive;
		the_child->observed = 1;
		the_child->optimistic = 0;
		the_child->nb_lit = a_cube->nb_lit;
		the_error = copy_literals(a_cube->nb_lit,a_cube->lits,the_child->lits);
		if (the_error != 0)
		  {
		    the_error = 2;
		    Free_calloc(the_max_lit,the_child->lits)
		    Free(the_child)
		  }
		else
		  {
		    the_child->nb_arg = a_cube->nb_arg;
		    the_child->args = Calloc(the_child->nb_arg,targ)
		    if (the_child->args == NULL)
		      {
			the_error = 2;
			for (i=0;i<a_cube->nb_lit;i++)
			  Free_calloc(the_predicate_table[the_child->lits[i].predicate].arity,the_child->lits[i].args)
			Free_calloc(the_max_lit,the_child->lits)
			Free(the_child)
		      }
		    else
		      {
		    for (i=0;i<the_child->nb_arg;i++)
		      {
			the_child->args[i].type = a_cube->args[i].type;
			if (/*(unification_form == IMPLICIT) &&*/ (i == a_var))
			  {
			    the_child->args[i].flip = CONST;
			    the_child->args[i].value = j;
			  }
			else
			  {
			    the_child->args[i].flip = a_cube->args[i].flip;
			    the_child->args[i].value = a_cube->args[i].value;
			  }
		      }
		    the_list_p = Malloc(tlist)
		    if (the_list_p == NULL)
		      {
			the_error = 2;
			Free_calloc(the_child->nb_arg,the_child->args)
			for (i=0;i<a_cube->nb_lit;i++)
			  Free_calloc(the_predicate_table[the_child->lits[i].predicate].arity,the_child->lits[i].args)
			Free_calloc(the_max_lit,the_child->lits)
			Free(the_child)
		      }
		    else
		      {
			the_list_p->elt = (void*)(the_child);
			the_list_p->next = the_list_children;
			the_list_children = the_list_p;
			the_child->nb_substs = a_cube->nb_substs + 1;
			the_child->substs = Calloc(the_child->nb_substs,tsubst)
			if (the_child->substs == NULL)
			  {
			    the_error = 2;
			    Free_calloc(the_child->nb_arg,the_child->args)
			    for (i=0;i<a_cube->nb_lit;i++)
			      Free_calloc(the_predicate_table[the_child->lits[i].predicate].arity,the_child->lits[i].args)
			    Free_calloc(the_max_lit,the_child->lits)
			    Free(the_child)
			    Free(the_list_p)
			  }
			else
			  {
			    for (i=0; i<the_child->nb_substs-1; i++)
			      {
				the_child->substs[i].arg = a_cube->substs[i].arg;
				the_child->substs[i].flip = a_cube->substs[i].flip;
				the_child->substs[i].value = a_cube->substs[i].value;
			      }
			    the_child->substs[the_child->nb_substs-1].arg = a_cube->corresp_args[a_var];
			    the_child->substs[the_child->nb_substs-1].flip = CONST;
			    the_child->substs[the_child->nb_substs-1].value = j;
			    the_child->corresp_args = Calloc(the_child->nb_arg,int)
			    if (the_child->corresp_args == NULL)
			      {
				the_error = 2;
				Free_calloc(the_child->nb_substs,the_child->substs)
				Free_calloc(the_child->nb_arg,the_child->args)
				for (i=0;i<a_cube->nb_lit;i++)
				  Free_calloc(the_predicate_table[the_child->lits[i].predicate].arity,the_child->lits[i].args)
				Free_calloc(the_max_lit,the_child->lits)
				Free(the_child)
				Free(the_list_p)
			      }
			    else
			      {
				for (i=0; i<the_child->nb_arg; i++)
				  the_child->corresp_args[i] = a_cube->corresp_args[i];
				the_child->exp_lits = Calloc(the_max_lit,tliteral)
				if (the_child->exp_lits == NULL)
				  {
				    the_error = 2;
				    Free_calloc(the_child->nb_arg,the_child->corresp_args)
				    Free_calloc(the_child->nb_substs,the_child->substs)
				    Free_calloc(the_child->nb_arg,the_child->args)
				    for (i=0;i<a_cube->nb_lit;i++)
				      Free_calloc(the_predicate_table[the_child->lits[i].predicate].arity,the_child->lits[i].args)
				    Free_calloc(the_max_lit,the_child->lits)
				    Free(the_child)
				    Free(the_list_p)
				  }
				else
				    {
					the_error = copy_literals(a_cube->nb_lit,a_cube->exp_lits,the_child->exp_lits);
					the_child->nb_exp_args = a_cube->nb_exp_args;
					the_child->exp_args = Calloc(a_cube->nb_exp_args,targ)
					if (the_child->exp_args == NULL)
					  {
					    the_error = 2;
					    Free_calloc(the_child->nb_arg,the_child->corresp_args)
					    Free_calloc(the_child->nb_substs,the_child->substs)
					    Free_calloc(the_child->nb_arg,the_child->args)
					    for (i=0;i<a_cube->nb_lit;i++)
					      Free_calloc(the_predicate_table[the_child->lits[i].predicate].arity,the_child->lits[i].args)
					    Free_calloc(the_max_lit,the_child->lits)
					    Free(the_child)
					    Free(the_list_p)
					  }
					else
					  {
					    for (i=0;i<a_cube->nb_exp_args;i++)
					      {
						the_child->exp_args[i].type = a_cube->exp_args[i].type;
					        the_child->exp_args[i].flip = a_cube->exp_args[i].flip;
					        the_child->exp_args[i].value = a_cube->exp_args[i].value;
					      }
					  }
				    }
			      }
			  }
		      }
		  }
	      }
	  }
      }
  }
else
  *a_list_children = NULL;

*a_list_children = the_list_children;
return the_error;
}


int refine_cube(tcube *a_cube, tlist **its_children)
{
int the_error = 0;
int the_nb_new_arg;
tlist *the_subsets;
int i,j,the_min_j;
tlist *the_children;
tlist *the_subset,*the_next_subset;
int the_first_pred,the_last_pred;
tlist *the_resulting_children;
tlist *the_child;

the_resulting_children = NULL;
if ((a_cube->flag_instanciation == 0)&&(a_cube->nb_lit < the_max_lit)
    && ((bayes == FALSE)||(bayes_can_add_predicates(a_cube)==TRUE)))
  /* add predicates */
  {
    if (a_cube->nb_lit == 0)
      {
	the_first_pred = 0;
	if (individual_based == TRUE)
	  the_last_pred = find_first_non_individual_predicate();
	else if ((the_search_bias == CLASSIFICATION)
		 || (the_search_bias == POSITIVE_CLASSIFICATION)
		 || (the_search_bias == HORN_POSITIVE_CLASSIFICATION))
	  the_last_pred = 1;
	else
	  the_last_pred = the_nb_predicates;
      }
    else if ((individual_based == TRUE) && (a_cube->nb_lit == 1))
      {
	the_first_pred = find_first_non_individual_predicate();
	if ((the_search_bias == CLASSIFICATION)
	    || (the_search_bias == POSITIVE_CLASSIFICATION)
	    || (the_search_bias == HORN_POSITIVE_CLASSIFICATION))
	  the_last_pred = 2;
	else
	  the_last_pred = the_nb_predicates;
      }
    else
      {
	if (can_repeat_literal(a_cube) == TRUE)
	  the_first_pred = a_cube->lits[a_cube->nb_lit-1].predicate;
	else
	  if (the_predicate_table[a_cube->lits[a_cube->nb_lit-1].predicate].num_param != 0)
	    the_first_pred = a_cube->lits[a_cube->nb_lit-1].predicate;
	  else
	    the_first_pred = a_cube->lits[a_cube->nb_lit-1].predicate+1;
	the_last_pred = the_nb_predicates;
      }
    for (i = the_first_pred; (i<the_last_pred) && (the_error==0); i++)
      if (the_predicate_table[i].num_occurence != 0) {
	if ((a_cube->nb_arg + the_predicate_table[i].arity)  <= the_max_var)
	  the_nb_new_arg = the_predicate_table[i].arity;
	else
	  the_nb_new_arg = the_max_var - a_cube->nb_arg;
	/*	if ((individual_based == TRUE) && (a_cube->nb_lit == 1)
	    && (the_nb_new_arg > the_predicate_table[i].arity - a_cube->nb_arg)) {
	  the_nb_new_arg = the_predicate_table[i].arity - a_cube->nb_arg;
	  if (the_nb_new_arg < 0) the_nb_new_arg = 0;
	}*/
	if ((structure_based == TRUE)
	    && (((individual_based == TRUE) && (a_cube->nb_lit >0)) || (individual_based == FALSE))) {
	  if (the_predicate_table[i].kind != STRUCTURAL)
	    the_nb_new_arg = 0;
	  else if (the_nb_new_arg == the_predicate_table[i].arity)
	    the_nb_new_arg = the_predicate_table[i].arity - 1;
	}
	the_error = build_subsets(the_nb_new_arg,the_nb_new_arg,the_predicate_table[i].arity,
				  the_predicate_table[i].arity,&the_subsets);
	the_subset = the_subsets;
	while ((the_subset != NULL) && (the_error == 0))
	  {
	    the_children = NULL;
	    if (((the_search_bias == NONE)
		 || (((the_search_bias == HORN)||(the_search_bias == HORN_POSITIVE_CLASSIFICATION))
		     && (a_cube->nb_positive == 0))
		 || (the_search_bias == POSITIVE_CLASSIFICATION)
		 || (the_search_bias == CLASSIFICATION))
		&& (((individual_based == FALSE) && (a_cube->nb_lit == 0))
		    || ((individual_based == TRUE) && (a_cube->nb_lit <= 1))
		    || (i != a_cube->lits[a_cube->nb_lit-1].predicate)
		    || (a_cube->lits[a_cube->nb_lit-1].sign != NEG)
		    || (the_predicate_table[i].num_param != 0))
		&& ((structure_based == FALSE) || (the_predicate_table[i].kind != STRUCTURAL)))

		if ((bayes == FALSE)
		    ||((individual_based == TRUE) && (a_cube->nb_lit == 0))
		    ||(the_predicate_table[i].num_subpredicate==1))
		  the_error = build_children_predicate(a_cube,i,POS,0,the_nb_new_arg,(int*)(the_subset->elt),&the_children);
	    if (the_error == 0)
	      {
		append(the_resulting_children,the_children,&the_resulting_children);
		the_children = NULL;
		if (((individual_based == FALSE)
		     && (((the_search_bias != POSITIVE_CLASSIFICATION)&&(the_search_bias != HORN_POSITIVE_CLASSIFICATION))
			 || (a_cube->nb_lit != 0)))
		     || ((individual_based == TRUE)
			 && ((a_cube->nb_lit > 0)
			     && (((the_search_bias != POSITIVE_CLASSIFICATION)
				  &&(the_search_bias != HORN_POSITIVE_CLASSIFICATION))
				 || (a_cube->nb_lit > 1)))))
		  the_error = build_children_predicate(a_cube,i,NEG,0,the_nb_new_arg,(int*)(the_subset->elt),&the_children);
		if (the_error == 0)
		  {
		    if (the_predicate_table[i].kind == STRUCTURAL) {
		      filter_structural_predicates(&the_children);
		      /*		      write_clauses_appropriate(stdout,the_children,0);*/
		    }
		    append(the_resulting_children,the_children,&the_resulting_children);
		    the_subset = the_subset->next;
		  }
		else
		  delete_cube_list(the_resulting_children);
	      }
	    else
	      delete_cube_list(the_resulting_children);
	  }
	the_subset = the_subsets;
	while (the_subset != NULL)
	  {
	    Free_calloc(the_nb_new_arg,(int*)the_subset->elt)
	    the_next_subset = the_subset->next;
	    the_subset->next = NULL;
	    Free(the_subset)
	    the_subset = the_next_subset;
	  }
      }
    if (the_error == 0)
      {
	the_child = the_resulting_children;
	while ((the_child != NULL) && (the_error == 0))
	  {
	    the_error = initialise_expansed_form((tcube*)the_child->elt,a_cube);
	    the_child = the_child->next;
	  }
	if (the_error != 0)
	  delete_cube_list(the_resulting_children);
      }
  }
/* unification of variables */
if ((a_cube->flag_instanciation == 0)&&((individual_based == FALSE) || (a_cube->nb_lit > 1)))
  {
    for (i=a_cube->first_var;(i<a_cube->nb_arg-1)&&(the_error == 0);i++)
      if ((a_cube->args[i].flip == VAR) && (argument_of_last_literal_only(i,a_cube) == TRUE))
	{
	  if (i==a_cube->first_var)
	    the_min_j = a_cube->second_var;
	  else
	    the_min_j = i;
	  for (j=the_min_j+1;(j<a_cube->nb_arg)&&(the_error == 0);j++)
	    if (a_cube->args[j].flip == VAR)
	      {
	      the_error = build_children_unify(a_cube,i,j,&the_children);
	      if (the_error == 0)
		{
		  append(the_resulting_children,the_children,&the_resulting_children);
		}
	      else
		delete_cube_list(the_resulting_children);
	      }
	}
  }
    
the_child = the_resulting_children;
while ((the_child != NULL) && (the_error == 0)) {
  the_error = rename_variables((tcube*)(the_child->elt));
  if (the_error != 0)
    delete_cube_list(the_resulting_children);
  else {
    initialise_unified_variables((tcube*)(the_child->elt));
    the_child = the_child->next;
  }
}
 if (the_error == 0) {
   /* remove clauses where two variables refer to the same sub-term */
   filter_structural_predicates(&the_resulting_children);
 }

/* instanciate variables */
for (i=a_cube->flag_instanciation; (i<a_cube->nb_arg)&&(the_error == 0);i++)
  if (a_cube->args[i].flip == VAR)
    {
      the_error = build_children_instanciate(a_cube,i,&the_children);
      if (the_error == 0)
	{
	  append(the_resulting_children,the_children,&the_resulting_children);
	}
      else
	delete_cube_list(the_resulting_children);
    }

*its_children = the_resulting_children;
return the_error;
}

int initialise_unified_variables(tcube *a_cube)
{
int the_error = 0;
int v,i,j;
int the_current_second = 0;
int beginning;

a_cube->second_var = 0;
a_cube->first_var = 0;
for (v=0;v<a_cube->nb_arg;v++)
  if ((a_cube->args[v].flip == VAR) && (argument_of_last_literal_only(v,a_cube) == TRUE))
    {
      beginning = TRUE;
      for (i=a_cube->nb_lit-1;i>=0;i--)
	for (j=the_predicate_table[a_cube->lits[i].predicate].arity-1;j>=0;j--)
	  if (beginning == TRUE)
	    {
	      if (a_cube->lits[i].args[j] == v)
		{
		  beginning = FALSE;
		  the_current_second = v;
		}
	    }
	  else
	    {
	      if (a_cube->lits[i].args[j] != v)
		{
		  if (a_cube->lits[i].args[j] > the_current_second)
		    the_current_second = a_cube->lits[i].args[j];
		}
	      else
		{
		  a_cube->second_var = the_current_second;
		  a_cube->first_var = v;
		}
	    }
    }

return the_error;
}

int rename_variables(tcube* a_cube)
{
int the_error = 0;
int the_last_new = 0;
targ *the_new_args;
int *the_redirections;
int *the_new_corresp;
int i,j;

the_new_args = Calloc(a_cube->nb_arg,targ)
if (the_new_args == NULL)
  the_error = 2;
else
  {
    the_redirections = Calloc(a_cube->nb_arg,int)
    if (the_redirections == NULL)
      {
	the_error = 2;
	Free_calloc(a_cube->nb_arg,the_new_args)
      }
    else
      {
	the_new_corresp = Calloc(a_cube->nb_arg,int)
	if (the_new_corresp == NULL)
	  {
	    the_error = 2;
	    Free_calloc(a_cube->nb_arg,the_new_args)
	    Free_calloc(a_cube->nb_arg,the_redirections)
	  }
	else
	  {
	    for (i=0;i<a_cube->nb_arg;i++)
	      the_redirections[i] = -1;
	    for (i=a_cube->nb_lit-1;i>=0;i--)
	      for (j=the_predicate_table[a_cube->lits[i].predicate].arity-1;j>=0;j--)
		if (the_redirections[a_cube->lits[i].args[j]] == -1)
		  {
		    the_redirections[a_cube->lits[i].args[j]] = the_last_new;
		    a_cube->lits[i].args[j] = the_last_new;
		    the_last_new++;
		  }
		else
		  a_cube->lits[i].args[j] = the_redirections[a_cube->lits[i].args[j]];
	    for (i=0;i<a_cube->nb_arg;i++)
	      {
		the_new_corresp[the_redirections[i]] = a_cube->corresp_args[i];
		the_new_args[the_redirections[i]].type = a_cube->args[i].type;
		the_new_args[the_redirections[i]].flip = a_cube->args[i].flip;
		the_new_args[the_redirections[i]].value = a_cube->args[i].value;
	      }
	    Free_calloc(a_cube->nb_arg,the_redirections)
	    Free_calloc(a_cube->nb_arg,a_cube->args)
	    Free_calloc(a_cube->nb_arg,a_cube->corresp_args)
	    a_cube->args = the_new_args;
	    a_cube->corresp_args = the_new_corresp;
	  }
      }
  }
return the_error;
}

int build_children_predicate(tcube *a_cube,int a_pred,int a_sign,int an_indice,int a_nb_new_args,int *a_subset,tlist **a_children_list)
{
int the_error = 0;
tcube *the_child;
tlist *the_child_p;
int i;
tlist *the_next_child;
tlist *the_new_children;
tlist *the_result_list = NULL;
int the_indice;
tlist *the_brothers;
int the_subpredicate,the_first_subpredicate;

if (an_indice == the_predicate_table[a_pred].arity){
  if (the_predicate_table[a_pred].num_param == 0)
    the_first_subpredicate = 0;
  else if ((a_cube->nb_lit < 1) || (a_cube->lits[a_cube->nb_lit-1].predicate != a_pred))
    the_first_subpredicate = 0;
  else if ((can_repeat_literal(a_cube) == TRUE) && (a_cube->lits[a_cube->nb_lit-1].sign == POS))
    the_first_subpredicate = a_cube->lits[a_cube->nb_lit-1].subpredicate;
  else
    the_first_subpredicate = a_cube->lits[a_cube->nb_lit-1].subpredicate + 1;
  for (the_subpredicate=the_first_subpredicate;
       (the_subpredicate<the_predicate_table[a_pred].num_subpredicate)&&(the_error==0);
       the_subpredicate++) {
    the_child = Malloc(tcube)
    if (the_child == NULL) the_error = 2; else{
      the_child->lits = Calloc(the_max_lit,tliteral)
      if (the_child->lits == NULL){
	the_error = 2;
	Free(the_child)
	  }
      else/*(an_indice != the_predicate_table[a_pred].arity) */{
	the_error = create_cube(&the_child);
	the_child->flag_instanciation = 0;
	the_child->first_var = a_cube->first_var;
	the_child->second_var = a_cube->second_var;
	the_child->optimistic = 1;
	the_child->observed = 1;
	the_child->num = 0;
	the_child->nb_lit = a_cube->nb_lit+1;
	if ((a_sign == POS) && ((individual_based == FALSE) || (a_cube->nb_lit > 0)))
	  the_child->nb_positive = a_cube->nb_positive + 1;
	else
	  the_child->nb_positive = a_cube->nb_positive;
	the_error = copy_literals(a_cube->nb_lit,a_cube->lits,the_child->lits);
	if (the_error != 0){
	  Free_calloc(the_max_lit,the_child->lits)
	  Free(the_child)
	}
	else/*copy_literals*/{
	  the_child->nb_arg = a_cube->nb_arg + a_nb_new_args;
	  the_child->args = Calloc(the_child->nb_arg,targ)
	  if (the_child->args == NULL){
	    the_error = 2;
	    for (i=0;i<a_cube->nb_lit;i++)
	      Free_calloc(the_predicate_table[the_child->lits[i].predicate].arity,the_child->lits[i].args)
	    Free_calloc(the_max_lit,the_child->lits)
	    Free(the_child)
	  }
	  else/*the_child->args*/{
	    for (i=0;i<a_cube->nb_arg;i++){
	      the_child->args[i].type = a_cube->args[i].type;
	      the_child->args[i].flip = a_cube->args[i].flip;
	      the_child->args[i].value = a_cube->args[i].value;
	    }
	    for (i=0;i<the_predicate_table[a_pred].arity;i++) {
	      the_indice = indice(i,a_nb_new_args,a_subset);
	      if (the_indice < a_nb_new_args) {
		the_child->args[a_cube->nb_arg+the_indice].type
		  = the_predicate_table[a_pred].types[the_predicate_table[a_pred].num_param+i];
		the_child->args[a_cube->nb_arg+the_indice].flip = VAR;
		the_child->args[a_cube->nb_arg+the_indice].value = a_cube->nb_exp_args + the_indice;
	      }
	    }
	    the_child->lits[the_child->nb_lit-1].predicate = a_pred;
	    the_child->lits[the_child->nb_lit-1].subpredicate = the_subpredicate;
	    the_child->lits[the_child->nb_lit-1].sign = a_sign;
	    the_child->lits[the_child->nb_lit-1].args = Calloc(the_predicate_table[a_pred].arity,int)
	    if (the_child->lits[the_child->nb_lit-1].args == NULL){
	      the_error = 2;
	      Free_calloc(the_child->nb_arg,the_child->args)
	      for (i=0;i<the_child->nb_lit-1;i++)
		Free_calloc(the_predicate_table[the_child->lits[i].predicate].arity,the_child->lits[i].args)
	      Free_calloc(the_max_lit,the_child->lits)
	      Free(the_child)
	    }
	    else/*the_child->lits[the_child->nb_lit-1].args*/{
	      the_new_children = Malloc(tlist)
	      if (the_new_children == NULL){
		the_error = 2;
		Free_calloc(the_child->nb_arg,the_child->args)
		for (i=0;i<the_child->nb_lit;i++)
		  Free_calloc(the_predicate_table[the_child->lits[i].predicate].arity,the_child->lits[i].args)
		Free_calloc(the_max_lit,the_child->lits)
		Free(the_child)
	      }
	      else{/*the_result_list*/
		the_new_children->elt = (void*)the_child;
		the_new_children->next = the_result_list;
		the_result_list = the_new_children;
		the_child->exp_lits = NULL;
		the_child->exp_args = NULL;
		the_child->corresp_args = NULL;
		the_child->substs = NULL;
	      }
	    }
	  }
	}
      }
    }
  }
}
else/*(an_indice == the_predicate_table[a_pred].arity)*/{
  the_error = build_children_predicate(a_cube,a_pred,a_sign,an_indice+1,a_nb_new_args,a_subset,&the_result_list);
  if (the_error == 0){
    the_new_children = NULL;
    the_child_p = the_result_list;
    while ((the_child_p != NULL) && (the_error == 0)) {
      the_brothers = NULL;
      the_next_child = the_child_p->next;
      the_indice = indice(an_indice,a_nb_new_args,a_subset);
      if (the_indice < a_nb_new_args)
	the_error = build_brother(((tcube*)(the_child_p->elt)),an_indice,VAR,
				  a_cube->nb_arg + the_indice,0,&the_brothers);
      else
	for (i=0;(i<a_cube->nb_arg) && (the_error==0);i++)
	  the_error = build_brother(((tcube*)(the_child_p->elt)),an_indice,VAR,i,0,&the_brothers);
      the_child_p->next = NULL;
      delete_cube_list(the_child_p);
      if (the_error != 0){
	delete_cube_list(the_new_children);
	delete_cube_list(the_brothers);
      }
      else{
	reverse(the_brothers,&the_brothers);
	append(the_new_children,the_brothers,&the_new_children);
      }
      the_child_p = the_next_child;
    }
    the_result_list = the_new_children;
  }
}
*a_children_list = the_result_list;
return the_error;
}

int build_subsets(int global_n,int new_n,int global_p,int new_p,tlist **a_list)
{
tlist *the_first_list;
tlist *the_second_list;
tlist *the_subset_p;
int *the_subset;
int i;
int the_error=0;
tlist *the_result,*the_next;

if (new_p == new_n)
  {
    the_subset = Calloc(global_n,int)
    if (the_subset != NULL)
      {
	for (i=1;i<=new_n;i++)
	  the_subset[global_n-i] = global_p-i;
	the_result = Malloc(tlist)
	if (the_result == NULL)
	  {
	    the_error = 2;
	    Free_calloc(global_n,the_subset)
	  }
	else
	  {
	    the_result->elt = (void*)the_subset;
	    the_result->next = NULL;
	  }
      }
    else
      {
	the_error = 2;
	the_result = NULL;
      }
  }
else
  if (new_n == 0)
    {
      the_subset = Calloc(global_n,int)
      if (the_subset == NULL)
	{
	  the_error = 2;
	  the_result = NULL;
	}
      else
	{
	  the_result = Malloc(tlist)
	  if (the_result == NULL)
	    {
	      Free_calloc(global_n,the_subset)
	      the_error = 2;
	    }
	  else
	    {
	      the_result->elt = (void*)the_subset;
	      the_result->next = NULL;
	    }
	}
    }
  else
    {
      the_error = build_subsets(global_n,new_n-1,global_p,new_p-1,&the_first_list);
      if (the_error == 0)
	{
	  the_subset_p = the_first_list;
	  while (the_subset_p != NULL)
	    {
	      ((int*)(the_subset_p->elt))[global_n-new_n] = global_p-new_p;
	      the_subset_p = the_subset_p->next;
	    }
	  the_error = build_subsets(global_n,new_n,global_p,new_p-1,&the_second_list);
	  if (the_error == 0)
	    append(the_first_list,the_second_list,&the_result);
	  else
	    {
	      the_subset_p = the_first_list;
	      while (the_subset_p != NULL)
		{
		  Free_calloc(global_n,(int*)the_subset_p->elt)
		  the_next = the_subset_p->next;
		  the_subset_p->next = NULL;
		  Free(the_subset_p)
		  the_subset_p = the_next;
		}
	      the_result = NULL;
	    }
	}
      else
	the_result = NULL;
    }
*a_list = the_result;
return(the_error);
}

int build_brother(tcube *a_cube,int an_indice,int a_flip,int an_arg,int a_value, tlist **a_list_brothers)
{
int the_error = 0;
tcube *the_brother;
int i;
tlist *the_list_p;

if ((a_flip == CONST)
    || (a_cube->args[an_arg].type == the_predicate_table[a_cube->lits[a_cube->nb_lit-1].predicate]
	.types[the_predicate_table[a_cube->lits[a_cube->nb_lit-1].predicate].num_param+an_indice])){
  the_brother = Malloc(tcube)
  if (the_brother == NULL) the_error = 2; else{
    the_brother->lits = Calloc(the_max_lit,tliteral)
    if (the_brother->lits == NULL){
      the_error = 2;
      Free(the_brother)
    }
    else/*the_brother->lits*/{
      the_error = create_cube(&the_brother);
      the_brother->flag_instanciation = a_cube->flag_instanciation;
      the_brother->num = 0;
      the_brother->first_var = a_cube->first_var;
      the_brother->second_var = a_cube->second_var;
      the_brother->nb_positive = a_cube->nb_positive;
      the_brother->optimistic = a_cube->optimistic;
      the_brother->observed = a_cube->observed;
      the_brother->nb_lit = a_cube->nb_lit;
      the_error = copy_literals(a_cube->nb_lit,a_cube->lits,the_brother->lits);
      if (the_error != 0){
	Free_calloc(the_max_lit,the_brother->lits)
	Free(the_brother)
      }
      else/*copy_literals*/{
	the_brother->nb_arg = a_cube->nb_arg;
	the_brother->args = Calloc(the_brother->nb_arg,targ)
	if (the_brother->args == NULL){
	  the_error = 2;
	  for (i=0;i<a_cube->nb_lit;i++)
	    Free_calloc(the_predicate_table[the_brother->lits[i].predicate].arity,the_brother->lits[i].args)
	  Free_calloc(the_max_lit,the_brother->lits)
	  Free(the_brother)
	}
	else/*the_brother->args*/{
	  for (i=0;i<a_cube->nb_arg;i++){
	    the_brother->args[i].type = a_cube->args[i].type;
	    the_brother->args[i].flip = a_cube->args[i].flip;
	    the_brother->args[i].value = a_cube->args[i].value;
	  }
	  the_brother->lits[the_brother->nb_lit-1].args[an_indice] = an_arg;
	  if (a_flip == CONST)
	    the_brother->args[an_arg].value = a_value;
	  the_list_p = Malloc(tlist)
	  if (the_list_p == NULL){
	    the_error = 2;
	    Free_calloc(the_brother->nb_arg,the_brother->args)
	    for (i=0;i<a_cube->nb_lit;i++)
	      Free_calloc(the_predicate_table[the_brother->lits[i].predicate].arity,the_brother->lits[i].args)
	    Free_calloc(the_max_lit,the_brother->lits)
	    Free(the_brother)
	  }
	  else/*the_list_p*/{
	    the_list_p->elt = (void*)(the_brother);
	    the_list_p->next = *a_list_brothers;
	    *a_list_brothers = the_list_p;
	    the_brother->nb_substs = 0;
	    the_brother->substs = NULL;
	    the_brother->corresp_args = NULL;
	    the_brother->exp_lits = NULL;
	    the_brother->exp_args = NULL;
	  }
	}
      }
    }
  }
}

return the_error;
}

int initialise_search(tlist **a_node,FILE *a_clause_file)
{
int the_error = 0;
tcube *the_cube;
tlist *the_list = NULL;
int i;
int the_predicate;

srand48(the_random_seed);
the_error = sort_predicates();
if ((the_error == 0)&&(individual_based == TRUE)) {
  the_max_lit += 1;
  for (the_predicate=0; the_predicate_table[the_predicate].kind == INDIVIDUAL; the_predicate++) {
    if (the_max_var < the_predicate_table[0].arity) {
      warning_deal_with(1,7);
      the_max_var = the_predicate_table[0].arity;
    }
    if (the_predicate_table[the_predicate].num_subpredicate != 0)
      warning_deal_with(1,8);
    the_predicate_table[the_predicate].num_subpredicate = 1;
    the_predicate_table[the_predicate].subpredicates = Calloc(1,tsubpredicate*)
      if (the_predicate_table[the_predicate].subpredicates == NULL) the_error = 2; else {
	the_predicate_table[the_predicate].subpredicates[0] = Malloc(tsubpredicate)
	if (the_predicate_table[the_predicate].subpredicates[0] == NULL) the_error = 2; else {
	  the_predicate_table[the_predicate].subpredicates[0]->val_params = NULL;
	  /* INITIALISE NB_INST AND NB_CINST */
	  the_predicate_table[the_predicate].subpredicates[0]->nb_inst = Calloc(the_nb_parts,int)
	  if (the_predicate_table[the_predicate].subpredicates[0]->nb_inst == NULL) the_error = 2; else {
	    for (i=0;i<the_nb_parts;i++)
	      the_predicate_table[the_predicate].subpredicates[0]->nb_inst[i] = 0;
	    the_predicate_table[the_predicate].subpredicates[0]->nb_cinst = Calloc(the_nb_parts,int)
	    if (the_predicate_table[the_predicate].subpredicates[0]->nb_cinst == NULL) the_error = 2; else {
	      for (i=0;i<the_nb_parts;i++)
		the_predicate_table[the_predicate].subpredicates[0]->nb_cinst[i] = 0;
	      the_predicate_table[the_predicate].subpredicates[0]->instances = Calloc(the_nb_parts,tinstance*)
	      if (the_predicate_table[the_predicate].subpredicates[0]->instances == NULL) the_error = 2; else {
		for (i=0;i<the_nb_parts;i++)
		  the_predicate_table[the_predicate].subpredicates[0]->instances[i] = NULL;
		the_predicate_table[the_predicate].subpredicates[0]->cinstances = Calloc(the_nb_parts,tinstance*)
		if (the_predicate_table[the_predicate].subpredicates[0]->cinstances == NULL) the_error = 2; else
		  for (i=0;i<the_nb_parts;i++)
		    the_predicate_table[the_predicate].subpredicates[0]->cinstances[i] = NULL;
	      }
	    }
	  }
	}
      }
  }
}
if (the_error == 0) {
  if (starting_clause == TRUE) {
    the_error = read_clause(a_clause_file,&the_cube);
    if (the_error == 0) {
      /*      the_max_lit += the_cube->nb_lit;
      the_max_var += the_cube->nb_arg;*/
      the_error = eval_cube(the_cube,0,NULL);
    }
  }
  else {
    the_cube = Malloc(tcube)
    if (the_cube == NULL) the_error = 2; else {
      the_cube->lits = Calloc(the_max_lit,tliteral)
      if (the_cube->lits == NULL) {
	the_error = 2;
	Free(the_cube)
      }
      else {
	the_error = create_cube(&the_cube);
	the_cube->flag_instanciation = 0;
	the_cube->num = 0;
	the_cube->first_var = 0;
	the_cube->second_var = 0;
	the_cube->nb_lit = 0;
	the_cube->nb_positive = 0;
	the_cube->nb_arg = 0;
	the_cube->nb_inst = 0;
	the_cube->observed = 1;
	the_cube->optimistic = 1;
	the_cube->nb_inst_positive = 1;
	the_cube->nb_inst_negative = 0;
	the_cube->capital_n = 1;
	the_cube->args = Calloc(the_cube->nb_arg,targ)
	if (the_cube->args == NULL) {
	  the_error = 2;
	  Free_calloc(the_max_lit,the_cube->lits)
	  Free(the_cube)
	}
	else {
	  the_error = initialise_expansed_form(the_cube,NULL);
	  if (the_error != 0) {		  
	    Free_calloc(the_cube->nb_arg,the_cube->args)
	    Free_calloc(the_max_lit,the_cube->lits)
	    Free(the_cube)
	    Free(the_list)
	  }
	}
      }
    }
  }
}
if (the_error == 0) {
  the_list = Malloc(tlist)
    if (the_list == NULL) {
      the_error = 2;
      if (starting_clause == FALSE) {
	Free_calloc(the_cube->nb_arg,the_cube->args)
	Free_calloc(the_max_lit,the_cube->lits)
	Free(the_cube)
      }
    }
    else {
      the_list->elt = (void*)(the_cube);
      the_list->next = NULL;
      *a_node = the_list;
    }
}

return the_error;
}




int build_children_unify(tcube *a_cube,int a_first_var,int a_second_var, tlist **a_list_children)
{
int the_error = 0;
tcube *the_child;
int i;
tlist *the_list_p;

if (a_cube->args[a_first_var].type == a_cube->args[a_second_var].type)
  {
    the_child = Malloc(tcube)
    if (the_child == NULL)
      the_error = 2;
    else
      {
	the_child->lits = Calloc(the_max_lit,tliteral)
	if (the_child->lits == NULL)
	  {
	    the_error = 2;
	    Free(the_child)
	  }
	else
	  {
	    the_error = create_cube(&the_child);
	    the_child->flag_instanciation = 0;
	    the_child->num = 0;
	    the_child->first_var = a_first_var;
	    the_child->second_var = a_second_var-1;
	    the_child->nb_positive = a_cube->nb_positive;
	    the_child->observed = 1;
	    the_child->optimistic = 1;
	    the_child->nb_lit = a_cube->nb_lit;
	    the_error = copy_and_unify_literals(a_cube->nb_lit,a_first_var,a_second_var,a_cube->lits,the_child->lits);
	    if (the_error != 0)
	      {
		the_error = 2;
		Free_calloc(the_max_lit,the_child->lits)
		Free(the_child)
	      }
	    else
	      {
		the_child->nb_arg = a_cube->nb_arg-1;
		the_child->args = Calloc(the_child->nb_arg,targ)
		if (the_child->args == NULL)
		  {
		    the_error = 2;
		    for (i=0;i<a_cube->nb_lit;i++)
		      Free_calloc(the_predicate_table[the_child->lits[i].predicate].arity,the_child->lits[i].args)
		    Free_calloc(the_max_lit,the_child->lits)
		    Free(the_child)
		  }
		else
		  {
		    for (i=0;i<the_child->nb_arg;i++)
		      {
			if (i<a_second_var)
			  {
			    the_child->args[i].type = a_cube->args[i].type;
			    the_child->args[i].flip = a_cube->args[i].flip;
			    the_child->args[i].value = a_cube->args[i].value;
			  }
			else
			  {
			    the_child->args[i].type = a_cube->args[i+1].type;
			    the_child->args[i].flip = a_cube->args[i+1].flip;
			    the_child->args[i].value = a_cube->args[i+1].value;
			  }
		      }
		    the_list_p = Malloc(tlist)
		    if (the_list_p == NULL)
		      {
			the_error = 2;
			Free_calloc(the_child->nb_arg,the_child->args)
			for (i=0;i<a_cube->nb_lit;i++)
			  Free_calloc(the_predicate_table[the_child->lits[i].predicate].arity,the_child->lits[i].args)
			Free_calloc(the_max_lit,the_child->lits)
			Free(the_child)
		      }
		    else
		      {
			the_list_p->elt = (void*)(the_child);
			the_list_p->next = NULL;
			*a_list_children = the_list_p;
			the_child->nb_substs = a_cube->nb_substs + 1;
			the_child->substs = Calloc(the_child->nb_substs,tsubst)
			if (the_child->substs == NULL)
			  {
			    the_error = 2;
			    Free_calloc(the_child->nb_arg,the_child->args)
			    for (i=0;i<a_cube->nb_lit;i++)
			      Free_calloc(the_predicate_table[the_child->lits[i].predicate].arity,the_child->lits[i].args)
			    Free_calloc(the_max_lit,the_child->lits)
			    Free(the_child)
			    Free(the_list_p)
			  }
			else
			  {
			    for (i=0; i<the_child->nb_substs-1; i++)
			      {
				the_child->substs[i].arg = a_cube->substs[i].arg;
				the_child->substs[i].flip = a_cube->substs[i].flip;
				the_child->substs[i].value = a_cube->substs[i].value;
			      }
			    the_child->substs[the_child->nb_substs-1].arg = a_cube->args[a_first_var].value;
			    the_child->substs[the_child->nb_substs-1].flip = VAR;
			    the_child->substs[the_child->nb_substs-1].value = a_cube->args[a_second_var].value;
			    the_child->corresp_args = Calloc(the_child->nb_arg,int)
			    if (the_child->corresp_args == NULL)
			      {
				the_error = 2;
				Free_calloc(the_child->nb_substs,the_child->substs)
				Free_calloc(the_child->nb_arg,the_child->args)
				for (i=0;i<a_cube->nb_lit;i++)
				  Free_calloc(the_predicate_table[the_child->lits[i].predicate].arity,the_child->lits[i].args)
				Free_calloc(the_max_lit,the_child->lits)
				Free(the_child)
				Free(the_list_p)
			      }
			    else
			      {
				for (i=0; i<the_child->nb_arg; i++)
				  if (i<a_second_var)
				    the_child->corresp_args[i] = a_cube->corresp_args[i];
				  else
				    the_child->corresp_args[i] = a_cube->corresp_args[i+1];
				the_child->exp_lits = Calloc(the_max_lit,tliteral)
				if (the_child->exp_lits == NULL)
				  {
				    the_error = 2;
				    Free_calloc(the_child->nb_arg,the_child->corresp_args)
				    Free_calloc(the_child->nb_substs,the_child->substs)
				    Free_calloc(the_child->nb_arg,the_child->args)
				    for (i=0;i<a_cube->nb_lit;i++)
				      Free_calloc(the_predicate_table[the_child->lits[i].predicate].arity,the_child->lits[i].args)
				    Free_calloc(the_max_lit,the_child->lits)
				    Free(the_child)
				    Free(the_list_p)
				  }
				else
				    {
					the_error = copy_literals(a_cube->nb_lit,a_cube->exp_lits,the_child->exp_lits);
					the_child->nb_exp_args = a_cube->nb_exp_args;
					the_child->exp_args = Calloc(a_cube->nb_exp_args,targ)
					if (the_child->exp_args == NULL)
					  {
					    the_error = 2;
					    Free_calloc(the_child->nb_arg,the_child->corresp_args)
					    Free_calloc(the_child->nb_substs,the_child->substs)
					    Free_calloc(the_child->nb_arg,the_child->args)
					    for (i=0;i<a_cube->nb_lit;i++)
					      Free_calloc(the_predicate_table[the_child->lits[i].predicate].arity,the_child->lits[i].args)
					    Free_calloc(the_max_lit,the_child->lits)
					    Free(the_child)
					    Free(the_list_p)
					  }
					else
					  {
					    for (i=0;i<a_cube->nb_exp_args;i++)
					      {
						the_child->exp_args[i].type = a_cube->exp_args[i].type;
					        the_child->exp_args[i].flip = a_cube->exp_args[i].flip;
					        the_child->exp_args[i].value = a_cube->exp_args[i].value;
					      }
					  }
				    }
			      }
			  }
		      }
		  }
	      }
	  }
      }
  }
else
  *a_list_children = NULL;

return the_error;
}


int initialise_expansed_form(tcube *a_cube,tcube *a_parent)
{
int the_error = 0;
int i;

if (individual_based == TRUE)
  a_cube->exp_lits = Calloc(the_max_lit+1,tliteral)
else
  a_cube->exp_lits = Calloc(the_max_lit,tliteral)
if (a_cube->exp_lits == NULL)
  the_error = 2;
else
  {
    if (a_parent != NULL)
      {
	the_error = copy_literals(a_parent->nb_lit,a_parent->exp_lits,a_cube->exp_lits);
	if (the_error == 0)
	  {
	    a_cube->exp_lits[a_cube->nb_lit-1].sign = a_cube->lits[a_cube->nb_lit-1].sign;
	    a_cube->exp_lits[a_cube->nb_lit-1].predicate = a_cube->lits[a_cube->nb_lit-1].predicate;
	    a_cube->exp_lits[a_cube->nb_lit-1].subpredicate = a_cube->lits[a_cube->nb_lit-1].subpredicate;
	    a_cube->exp_lits[a_cube->nb_lit-1].args = Calloc(the_predicate_table[a_cube->lits[a_cube->nb_lit-1].predicate].arity,int)
	    if (a_cube->exp_lits[a_cube->nb_lit-1].args != NULL) {
	      for (i=0;i<the_predicate_table[a_cube->lits[a_cube->nb_lit-1].predicate].arity;i++)
		if (a_cube->args[a_cube->lits[a_cube->nb_lit-1].args[i]].flip == VAR)
		  a_cube->exp_lits[a_cube->nb_lit-1].args[i] = a_cube->args[a_cube->lits[a_cube->nb_lit-1].args[i]].value;
		else
		  a_cube->exp_lits[a_cube->nb_lit-1].args[i] = a_cube->lits[a_cube->nb_lit-1].args[i] + a_parent->nb_exp_args - a_parent->nb_arg;
	    }
	    else
	      the_error = 2;
	  }
      }
    else
      the_error = copy_literals(a_cube->nb_lit,a_cube->lits,a_cube->exp_lits);
    if (the_error != 0)
      {
	if (individual_based == TRUE)
	  Free_calloc(the_max_lit+1,a_cube->exp_lits)
	else
	  Free_calloc(the_max_lit,a_cube->exp_lits)
	a_cube->exp_lits = NULL;
      }
    else
      {
	if (a_parent != NULL)
	  a_cube->nb_exp_args = a_parent->nb_exp_args + a_cube->nb_arg - a_parent->nb_arg;
	else
	  a_cube->nb_exp_args = a_cube->nb_arg;
	a_cube->exp_args = Calloc(a_cube->nb_exp_args,targ)
	if (a_cube->exp_args == NULL)
	  {
	    the_error = 2;
	    for (i=0; i<a_cube->nb_lit; i++)
	      Free_calloc(the_predicate_table[a_cube->lits[i].predicate].arity,a_cube->lits[i].args)
	    Free_calloc(the_max_lit,a_cube->exp_lits)
	    a_cube->exp_lits = NULL;
	  }
	else
	  {
	    if (a_parent != NULL)
	      {
		for (i=0; i<a_parent->nb_exp_args; i++)
		  {
		    a_cube->exp_args[i].type = a_parent->exp_args[i].type;
		    a_cube->exp_args[i].flip = a_parent->exp_args[i].flip;
		    a_cube->exp_args[i].value = a_parent->exp_args[i].value;
		  }		
		for (i=0; i<a_cube->nb_exp_args-a_parent->nb_exp_args; i++)
		  {
		    a_cube->exp_args[i+a_parent->nb_exp_args].type = a_cube->args[i+a_parent->nb_arg].type;
		    a_cube->exp_args[i+a_parent->nb_exp_args].flip = a_cube->args[i+a_parent->nb_arg].flip;
		    a_cube->exp_args[i+a_parent->nb_exp_args].value = a_cube->args[i+a_parent->nb_arg].value;
		  }
	      }
	    else
	      for (i=0; i<a_cube->nb_exp_args; i++)
		{
		  a_cube->exp_args[i].type = a_cube->args[i].type;
		  a_cube->exp_args[i].flip = a_cube->args[i].flip;
		  a_cube->exp_args[i].value = a_cube->args[i].value;
		}
	    a_cube->corresp_args = Calloc(a_cube->nb_arg,int)
	    if (a_cube->corresp_args == NULL)
	      {
		the_error = 2;
		for (i=0; i<a_cube->nb_lit; i++)
		  Free_calloc(the_predicate_table[a_cube->lits[i].predicate].arity,a_cube->exp_lits[i].args)
		Free_calloc(the_max_lit,a_cube->exp_lits)
		a_cube->exp_lits = NULL;
		Free_calloc(a_cube->nb_arg,a_cube->exp_args)
		  a_cube->exp_args = NULL;
	      }
	    else
	      {
		if (a_parent != NULL)
		  {
		    for (i=0; i<a_parent->nb_arg; i++)
		      a_cube->corresp_args[i] = a_parent->corresp_args[i];
		    for (i=a_parent->nb_arg; i<a_cube->nb_arg; i++)
		      a_cube->corresp_args[i] = a_cube->args[i].value;
		  }
		else
		  for (i=0; i<a_cube->nb_arg; i++)
		    a_cube->corresp_args[i] = a_cube->args[i].value;
		if (a_parent != NULL)
		  {
		    a_cube->nb_substs = a_parent->nb_substs;
		    a_cube->substs = Calloc(a_cube->nb_substs,tsubst)
		    if (a_cube->substs == NULL)
		      {
			the_error = 2;
			for (i=0; i<a_cube->nb_lit; i++)
			  Free_calloc(the_predicate_table[a_cube->lits[i].predicate].arity,a_cube->exp_lits[i].args)
			Free_calloc(the_max_lit,a_cube->exp_lits)
			a_cube->exp_lits = NULL;
			Free_calloc(a_cube->nb_arg,a_cube->exp_args)
			a_cube->exp_args = NULL;
			Free_calloc(a_cube->nb_arg,a_cube->corresp_args)
			a_cube->corresp_args = NULL;
		      }
		    else
		      for (i=0; i<a_cube->nb_substs; i++)
			{
			  a_cube->substs[i].arg = a_parent->substs[i].arg;
			  a_cube->substs[i].flip = a_parent->substs[i].flip;
			  a_cube->substs[i].value = a_parent->substs[i].value;
			}
		  }
		else
		  {
		    a_cube->nb_substs = 0;
		    a_cube->substs = NULL;
		  }
	      }
	  }
      }
  }
return the_error;
}
