#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "types.h"
#include "background.h"
#include "table.h"
#include "sicstusio.h"
#include "input_output.h"
#include "search.h"
#include "error.h"

SP_term_ref substituted_term_ref(int a_value,int a_no_arguments,targ *some_clause_arguments)
{
  int the_argument_count;
  
  for(the_argument_count=0;the_argument_count<a_no_arguments;the_argument_count++){
    if(some_clause_arguments[the_argument_count].value==a_value)
      return(some_clause_arguments[the_argument_count].term_ref);
  }
error_deal_with(1,12);
return (SP_term_ref)NULL;
}


int make_prolog_literal_list(tcube *a_clause,SP_term_ref* a_list_ref)
{
  /* Declarations */
  int the_call_result,the_no_literals,the_literal_count,the_no_clause_arguments,the_clause_argument_count;
  int the_no_literal_arguments,the_literal_argument_count,the_parameter_count,the_proper_argument_count;
  int the_substitution_count;
  int *the_initiated_arguments;
  tliteral *the_literal;
  targ *the_clause_arguments;
  SP_term_ref the_list_ref,the_term_ref,the_literal_ref;
  the_no_literals = a_clause->nb_lit;

  the_term_ref = SP_new_term_ref();
  the_list_ref = SP_new_term_ref();
  /* Check unification form */
  if(unification_form == IMPLICIT){
    /* The unification form is implicit */
    the_no_clause_arguments = a_clause->nb_arg;
    the_clause_arguments = a_clause->args;
  }else{
    /* The unification form is explicit */
    the_no_clause_arguments = a_clause->nb_exp_args;
    the_clause_arguments = a_clause->exp_args;
  }
  the_initiated_arguments = Calloc(the_no_clause_arguments,int)
  if (the_initiated_arguments == NULL) 
    return 2;

  /* Set all arguments in clause to uninitiated */
    for(the_clause_argument_count=0;the_clause_argument_count<the_no_clause_arguments;
	the_clause_argument_count++){
      the_initiated_arguments[the_clause_argument_count] = 0;
    }
  /* Add all literals to the list of literal lists */
  for(the_literal_count=the_no_literals-1;the_literal_count>=0;the_literal_count--){
    the_literal_ref = SP_new_term_ref();
    if(unification_form == IMPLICIT){
      /* The unification form is implicit */
      the_literal = &a_clause->lits[the_literal_count];
    }else{
      /* The unification form is explicit */
      the_literal = &a_clause->exp_lits[the_literal_count];      
    }
    the_no_literal_arguments = the_predicate_table[the_literal->predicate].arity;
    the_parameter_count = the_predicate_table[the_literal->predicate].num_param-1;
    the_proper_argument_count = the_no_literal_arguments-1;
    /* Add all parameters and arguments to a literal list */ 
    for(the_literal_argument_count=the_no_literal_arguments+
	the_predicate_table[the_literal->predicate].num_param-1;the_literal_argument_count>=0;
	the_literal_argument_count--){
      /* Check if the argument is a parameter */
      if(the_predicate_table[the_literal->predicate].params[the_literal_argument_count]==TRUE){
	/* The argument is a parameter */
	/* Create the Prolog constant */
	the_call_result = SP_cons_list(the_literal_ref,
				       the_type_table[the_predicate_table[the_literal->predicate].
						      types[the_parameter_count]]
				       .term_ref[0][the_predicate_table[the_literal->predicate]
						    .subpredicates[the_literal->subpredicate]->
						    val_params[the_parameter_count]],
				       the_literal_ref);
	if(the_call_result==0){
	  Free_calloc(the_no_clause_arguments,the_initiated_arguments)
	  return ERR_PROLOG;
	}
	the_parameter_count--;
      }else{
	/* The argument is a proper argument */
	/* Check if argument is instantiated */
	if(the_initiated_arguments[the_literal->args[the_proper_argument_count]]==0){
	  /* The argument is uninstantiated */
	  /* Check if argument is a variable */
	  if(the_clause_arguments[the_literal->args[the_proper_argument_count]].flip==VAR){
	    /* Argument is a variable */
	    /* Create Prolog variable */
	    the_term_ref = SP_new_term_ref();
	    the_call_result = SP_put_variable(the_term_ref);
	    if(the_call_result==0){
	      Free_calloc(the_no_clause_arguments,the_initiated_arguments)
	      return ERR_PROLOG;
	    }
	    /* Initiate argument */
	    the_clause_arguments[the_literal->args[the_proper_argument_count]].term_ref = 
	      the_term_ref;
	    the_initiated_arguments[the_literal->args[the_proper_argument_count]]=1;
	    /* Add argument to literal_list */
	    the_call_result = SP_cons_list(the_literal_ref,the_term_ref,the_literal_ref);
	    if(the_call_result==0){
	      Free_calloc(the_no_clause_arguments,the_initiated_arguments)
	      return ERR_PROLOG;
	    }	    
	  }else{
	    /* Argument is a constant */
	    /* Create prolog constant */
	    the_call_result = SP_cons_list(the_literal_ref,
					   the_type_table[the_clause_arguments
							  [the_literal->args[the_proper_argument_count]].
							  type].term_ref[0]
					   [the_clause_arguments[the_literal->
								 args[the_proper_argument_count]].value],
					   the_literal_ref);
	    if(the_call_result==0){
	      Free_calloc(the_no_clause_arguments,the_initiated_arguments)
	      return ERR_PROLOG;
	    }
	    /* Instantiate argument */
	    the_initiated_arguments[the_literal->args[the_proper_argument_count]]=1;
	    the_clause_arguments[the_literal->args[the_proper_argument_count]].term_ref =
	      the_type_table[the_clause_arguments[the_literal->args[the_proper_argument_count]].type].
		term_ref[0][the_clause_arguments[the_literal->args[the_proper_argument_count]].value];
	  }
	}else{
	  /* The argument is instantiated */
	  the_call_result = SP_cons_list(the_literal_ref,
					 the_clause_arguments[the_literal->args[the_proper_argument_count]].
					 term_ref,the_literal_ref);
	  if(the_call_result==0){
	    Free_calloc(the_no_clause_arguments,the_initiated_arguments)
	    return ERR_PROLOG;
	  }	
	}
	the_proper_argument_count--;
      } /* Ends proper argument */
      /* Add the predicate to a literal list */
    } /* Ends all literal arguments loop */
    the_call_result = SP_cons_list(the_literal_ref,the_predicate_table[the_literal->predicate].term_ref,
				   the_literal_ref);
    if(the_call_result==0){
      Free_calloc(the_no_clause_arguments,the_initiated_arguments)
      return ERR_PROLOG;
    }	
    /* Add the literal list to the list of literal lists */
    the_call_result = SP_cons_list(the_list_ref,the_literal_ref,the_list_ref);
    if(the_call_result==0){
      Free_calloc(the_no_clause_arguments,the_initiated_arguments)
      return ERR_PROLOG;
    }
  } /* Ends all literals loop */
  
  /* Check if unification form is explicit */
  if(unification_form==EXPLICIT){
    /* Unification form is explicit */
    for(the_substitution_count=0;the_substitution_count<a_clause->nb_substs;the_substitution_count++){
      /* Initiate literal list to empty list */
      the_literal_ref = SP_new_term_ref();
      /* Check if the object of the substitution is a variable */
      if(a_clause->substs[the_substitution_count].flip == VAR){
	/* The object of the substitution is a variable */
	/* Add prolog variable as equation object in the literal list */
	if(the_initiated_arguments[a_clause->substs[the_substitution_count].value])
	  the_term_ref = substituted_term_ref(a_clause->substs[the_substitution_count].value,
					      the_no_clause_arguments,the_clause_arguments);
	else
	  error_deal_with(1,10);
	the_call_result = SP_cons_list(the_literal_ref,the_term_ref,the_literal_ref);
	if(the_call_result==0){
	  Free_calloc(the_no_clause_arguments,the_initiated_arguments)
	  return ERR_PROLOG;
	}	
      }else{
	/* The object of the substitution is a constant*/
	/* Add the prolog constant to the literal list */
	the_call_result = SP_cons_list(the_literal_ref,the_type_table[the_clause_arguments
								      [a_clause->
								       substs[the_substitution_count].
								       arg].type].term_ref
				       [0][a_clause->substs[the_substitution_count].value],the_literal_ref);
	if(the_call_result==0){
	  Free_calloc(the_no_clause_arguments,the_initiated_arguments)
	  return ERR_PROLOG;
	}	
      }
      /* Add equation subject to the literal list */
      if(the_initiated_arguments[a_clause->substs[the_substitution_count].arg])
	the_term_ref = substituted_term_ref(a_clause->substs[the_substitution_count].arg,
					    the_no_clause_arguments,the_clause_arguments);
      else
	error_deal_with(1,10);
      the_call_result = SP_cons_list(the_literal_ref,the_term_ref,the_literal_ref);
      if(the_call_result==0){
	Free_calloc(the_no_clause_arguments,the_initiated_arguments)
	return ERR_PROLOG;
      }	
      /* Add the predicate 'equals' to the literal list */
      the_call_result = SP_put_string(the_term_ref,"equals");
      if(the_call_result==0){
	Free_calloc(the_no_clause_arguments,the_initiated_arguments)
	return ERR_PROLOG;
      }	
      the_call_result = SP_cons_list(the_literal_ref,the_term_ref,the_literal_ref);
      if(the_call_result==0){
	Free_calloc(the_no_clause_arguments,the_initiated_arguments)
	return ERR_PROLOG;
      }	
      /* Add the equation list to the list of literal lists */
      the_call_result = SP_cons_list(the_list_ref,the_literal_ref,the_list_ref);
      if(the_call_result==0){
	Free_calloc(the_no_clause_arguments,the_initiated_arguments)
	return ERR_PROLOG;
      }	
    } /* Ends all substitutions */
  } /* Ends unification explicit */
  
  /* Return success */
  *a_list_ref = the_list_ref;
  return 0;
}

int get_expected_value(tcube *a_clause,table a_table,double *an_expected_value)
{
  /* Declarations */
  int the_call_result,the_no_literals,the_no_equations,the_literal_count,the_index_count;
  int the_base,the_observed_cell_index;
  int the_no_zero_indecies,the_zero_index_count;
  int *the_zero_indecies;
  float the_fitting_threshold;
  long the_population;
  double the_local_expected_value;
  SP_term_ref the_list_ref,the_zero_indecies_ref;
  d1_marginal_set the_marginals;
  table the_base_table;

  the_fitting_threshold = 0.00001;

  the_population = a_clause->capital_n;
  the_no_literals = a_clause->nb_lit;
  if(unification_form==EXPLICIT)
    the_no_equations = a_clause->nb_substs;
  else
    the_no_equations = 0;

  /* Find the implied cell index */
  the_observed_cell_index = 0;
  the_base = 0;
  for(the_literal_count=0;the_literal_count<the_no_literals;the_literal_count++){
    /* uses negated clause */
    if(a_clause->lits[the_literal_count].sign==NEG){
      the_base = 1;
      for(the_index_count=0;the_index_count<((the_no_literals+the_no_equations-the_literal_count)-1);
	  the_index_count++){
	the_base *= 2;
      }
    }
    the_observed_cell_index += the_base;
    the_base = 0;
  }
  /* Equations will have no effect of the cell index */
  /*printf("The observed cell index %i\n",the_observed_cell_index);*/

  /* Translate clause to Prolog list */
  the_call_result = make_prolog_literal_list(a_clause,&the_list_ref);
  if(the_call_result!=0)
    return the_call_result;  

  /* Query Prolog */
  the_zero_indecies_ref = SP_new_term_ref();
  the_call_result = SP_put_variable(the_zero_indecies_ref);
  if(the_call_result==0)
    return ERR_PROLOG;
  the_call_result = SP_query(the_follows_reference,the_list_ref,the_zero_indecies_ref);
  /*SP_print_term(the_zero_indecies_ref);*/
  if(the_call_result!=SP_SUCCESS){
    printf("Query 'follows' failed\n");
    SP_print_term(the_list_ref);
    return ERR_PROLOG;
  }
  the_prolog_query_count++;

  /* Check for garbage collection */
  if(the_prolog_query_count>GARBAGE_COLLECT_LIMIT){
    /*printf("Doing garbage collection\n");*/
    the_prolog_query_count = 0;
    the_call_result = SP_query(the_garbage_collect_reference,0,NULL);
    if(the_call_result!=SP_SUCCESS){
      printf("Garbage collection failed\n");
      return ERR_PROLOG;
    }
  }

  the_call_result = make_c_table(the_zero_indecies_ref,the_no_literals+the_no_equations,&the_base_table);
  if(the_call_result!=0)
    return the_call_result;

  /*For test puposes calculate the naive expected value */
  /*the_naive_expected_value = 1.0;
    for(the_literal_count=0;the_literal_count<the_no_literals;the_literal_count++){
     if(a_clause->lits[the_literal_count].sign==NEG)
    the_naive_expected_value *= (double)(the_marginals->marginal_values[the_literal_count*2+1])/
    (double)the_population;
    else
    the_naive_expected_value *= (double)the_marginals->marginal_values[the_literal_count*2]/
    (double)the_population;
    }
    for(the_equation_count=0;the_equation_count<the_no_equations;the_equation_count++){
    the_naive_expected_value *= 
    (double)the_marginals->marginal_values[(the_no_literals+the_equation_count)*2]/(double)the_population;
    }*/

  /* Clean the table of noise */
  /*print_table(a_table);
  print_table(the_base_table);*/
  the_no_zero_indecies = get_no_zero_indecies(the_base_table);
  the_call_result = get_zero_indecies(the_base_table,&the_zero_indecies);
  if(the_call_result!=0)
    return the_call_result;
  for(the_zero_index_count=0;the_zero_index_count<the_no_zero_indecies;the_zero_index_count++)
    set_cell_value(0.0,the_zero_indecies[the_zero_index_count],a_table);
  Free_calloc(the_no_zero_indecies,the_zero_indecies)
  /*print_table(a_table);*/
  the_call_result = get_d1_marginal_set(a_table,&the_marginals);
  if(the_call_result!=0)
    return the_call_result;
  /*print_d1_marginal_set(the_marginals);*/

  /* Fit the marginals */
  fit_marginals(the_marginals,the_base_table);

  /*printf("the fitted table:\n");
  print_table(the_base_table);*/

  the_local_expected_value = get_cell_value(the_observed_cell_index,the_base_table);

  /* Return success */
  /*printf("The local expected value: %f\n",the_local_expected_value);*/
  if(the_local_expected_value/(double)the_population<10*FITTING_THRESHOLD)
    *an_expected_value = 0.0;
  else
    *an_expected_value = the_local_expected_value/(double)the_population;

  /*if((the_naive_expected_value-the_local_expected_value/(double)the_population>0.001)||
     (the_naive_expected_value-the_local_expected_value/(double)the_population<-0.001)){
     the_test_node = Malloc(tlist)
     the_test_node->elt = (void*)a_clause;
     the_test_node->next = NULL;
     write_clauses_appropriate(stdout,the_test_node,0);
     Free(the_test_node);
     print_d1_marginal_set(the_marginals);
     free_d1_marginal_set(the_marginals);
     print_table(the_table);
     printf("%i ",the_observed_cell_index);
     printf("%f ",the_naive_expected_value);
     printf("%f\n",the_local_expected_value/(double)the_population);
     }*/

  
  return 0;
} 











