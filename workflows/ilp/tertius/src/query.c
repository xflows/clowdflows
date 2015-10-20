#include </usr/local/sicstus/include/sicstus/sicstus.h>
#include <stdio.h>
#include <string.h>
#include "types.h"
#include "input_output.h"
#include "background.h"

/*========================================================================
  query_literal
  =========================================================================
  */

int query_literal(tliteral *a_literal, targ *a_cube_args, int a_part,int *a_substitution, int *a_result)
{
  /* Tertius Related Variables */
  register int the_result;
  register int the_arity;
  
  int the_error = 0 ;
  
  /* Prolog Query Related Variables */
  int arg_no,param_no,call_result;
  SP_term_ref arg,arg_list;
  
  /* Initiating 'the_arity' */
  the_arity = the_predicate_table[a_literal->predicate].num_param + the_predicate_table[a_literal->predicate].arity;
  
  /* Initialise the variable 'X' */
  call_result = SP_put_variable(the_variable);
  if(call_result!=1){
    /*    printf("Variable not constructed\n");*/
    return ERR_PROLOG;
  }
  
  /*Add all arguments to 'arg_list' */
  arg = SP_new_term_ref();
  if(arg==0){
    /*    printf("Term reference for 'arg' not constructed\n");*/
    return ERR_PROLOG;
  }
  arg_list = SP_new_term_ref();
  if(arg_list==0){
    /*    printf("Term reference for 'arg_list' not constructed\n");*/
    return ERR_PROLOG;
  }
  arg_no = the_predicate_table[a_literal->predicate].arity;
  param_no = the_predicate_table[a_literal->predicate].num_param;
  while (param_no + arg_no > 0) {
    /* Check whether the argument is a parameter */
    if (the_predicate_table[a_literal->predicate].params[param_no + arg_no - 1] == TRUE) {
      call_result = SP_cons_list(arg_list,the_type_table[the_predicate_table[a_literal->predicate].types[param_no-1]]
				 .term_ref[0] [the_predicate_table[a_literal->predicate]
					     .subpredicates[a_literal->subpredicate]->val_params[param_no-1]],arg_list);
      if(call_result!=1) {
	/*	printf("'arg' not added to list\n");*/
	return ERR_PROLOG;
      }
      param_no -= 1;
    }
    /* Check whether the argument is a variable */
    else if (a_cube_args[a_literal->args[arg_no-1]].flip == VAR) {
      /* The argument is a variable */
      call_result = SP_cons_list(arg_list,the_type_table[the_predicate_table[a_literal->predicate].types[the_predicate_table[a_literal->predicate].num_param + arg_no-1]].term_ref[a_part][a_substitution[a_literal->args[arg_no-1]]],arg_list);
      if(call_result!=1){
	/*	printf("'arg' not added to list\n");*/
	return ERR_PROLOG;
      }
      arg_no -= 1;
    }
    else {
      /* The argument is a constant */
      call_result = SP_cons_list(arg_list,the_type_table[the_predicate_table[a_literal->predicate].types[the_predicate_table[a_literal->predicate].num_param + arg_no-1]].term_ref[0][a_cube_args[arg_no-1].value],arg_list);
      if(call_result!=1){
	/*	printf("'arg' not added to list\n");*/
	return ERR_PROLOG;
      }
      arg_no -= 1;
    }
  }
  
  /* Add Predicate to List */
  call_result = SP_cons_list(arg_list,the_predicate_table[a_literal->predicate].term_ref,arg_list);
  if(call_result!=1){
    /*    printf("'the_arg' not added to list\n");*/
    return ERR_PROLOG;
  }  
  
  /* Create literal 'X=..[arg1,arg2,...]' */
  call_result = SP_cons_functor(arg,SP_atom_from_string("=.."),2,the_variable,arg_list);
  if(call_result!=1){
    /*    printf("Term 'X =.. [arg1,arg2,...]' not created");*/
    return ERR_PROLOG;
  }
  
  /* Query 'X=..[arg1,arg2,...],X' */
  the_result = SP_query(the_conjunction_reference,arg,the_variable);
  if(the_result==-1){
    /*    printf("Query not posed\n");*/
    return ERR_PROLOG;
  }
  the_prolog_query_count++;
  
  /* Count prolog query and garbage collect */
  if(the_prolog_query_count>=GARBAGE_COLLECT_LIMIT){
    call_result = SP_query(the_garbage_collect_reference,0);
    if(call_result!=1){
      /*      printf("Did not garbage collect\n");*/
      return ERR_PROLOG;
    }
    the_prolog_query_count=0;
  }
  
  /* Return result */
  *a_result = the_result;
  return the_error;
}

 


