#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "types.h"
#include "eval.h"
#include "input_output.h"
#include "search.h"
#include "sicstusio.h"

SP_pred_ref the_follows_reference,the_conjunction_reference,the_garbage_collect_reference;
SP_term_ref the_variable;

int the_prolog_query_count;

/*==================================================================
 * initialise_prolog 
 *==================================================================
 * filename argument must be
 * unindexed
 *-----------
*/

initialise_prolog(void)
{
  int the_call_result,const_no,const_count,type_count,the_predicate_count,part_count,the_local_nb_part;
  char *the_indexed_temporary_filename,*the_indexed_background_filename,*the_debug_string;
  FILE *the_temporary_file;
  SP_term_ref the_arity_reference,the_predicate_list_reference,the_arity_list_reference;
  SP_term_ref the_filename_reference;
  SP_term_ref the_predicates_list_term,the_predicate_list_term,the_predicate_term,the_variable_term;
  SP_term_ref the_indecies_term;
  SP_pred_ref the_initiate_equality_reference,the_initiate_background_knowledge_reference;
  SP_pred_ref the_fcompile_reference;

  /* Initiating Prolog */
  printf("Initiating Prolog\n");
  the_call_result = SP_initialize(0,NULL,"/usr/local/sicstus/bin/");
  if(the_call_result!=SP_SUCCESS)
    return ERR_PROLOG;
  

  /* Create temporary file */
  printf("Creating temporary file 'tertius_background_tmp.pl'\n");
  the_temporary_file = fopen("tertius_background_tmp.pl","w");
  if(the_temporary_file==NULL){
    printf("Cannot open 'tertius_background_tmp.pl'\n");
    exit(0);
  }
  fprintf(the_temporary_file,":- dynamic fly/1.\n\n");
  
  /* Closing the temporary file 'tertius_background_tmp.pl' */
  printf("Closing the temporary file 'tertius_background_tmp.pl'\n");
  the_call_result = fclose(the_temporary_file);
  if(the_call_result!=0){
    printf("Could not close the temporary file 'tertius_background_tmp.pl'\n");
    return 1;
  }

  /* Fcompile files */
  printf("Fcompiling the files 'follows.pl' and 'tertius_background_tmp.pl'\n");
  the_call_result = system("sicstus -f -l test_compile.pl > compile.log 2> compile.log");
  if(the_call_result!=0){
    printf("Failed in fcompiling files 'follows.pl' and 'tertius_background_tmp.pl'\n");
    exit(0);
  }
  
  /* Load file 'tertius_background_tmp.ql' */
  printf("Loading file 'tertius_background_tmp.ql'\n");
  the_call_result = SP_load("tertius_background_tmp.ql");
  if(the_call_result!=SP_SUCCESS){
    printf("Cannot load file 'tertius_background_tmp.ql'\n");
    exit(0);
  } 

  /* Load file 'follows.ql' */
  printf("Loading file  'follows.ql\n"); 
  the_call_result = SP_load("follows.ql");
  if(the_call_result!=SP_SUCCESS){
    printf("Cannot load file 'follows.ql'\n");
    exit(0);
  }

  /* Remove the follows.ql file */
  printf("Removing 'follows.ql\n"); 
  the_call_result = remove("follows.ql");
  if(the_call_result!=0){
    printf("Cannot remove 'follows.ql'\n");
    exit(0);
  }

  /* Remove the tertius_background_tmp.pl file */
  printf("Removing 'tertius_background_tmp.pl\n"); 
  the_call_result = remove("tertius_background_tmp.pl");
  if(the_call_result!=0){
    printf("Cannot remove 'tertius_background_tmp.pl'\n");
    exit(0);
  }

  /* Remove the tertius_background_tmp.ql file */
  printf("Removing 'tertius_background_tmp.ql\n"); 
  the_call_result = remove("tertius_background_tmp.ql");
  if(the_call_result!=0){
    printf("Cannot remove 'tertius_background_tmp.ql'\n");
    exit(0);
  }

  /* Remove the compile.log file */
  printf("Removing 'compile.log\n"); 
  the_call_result = remove("compile.log");
  if(the_call_result!=0){
    printf("Cannot remove 'compile.log'\n");
    exit(0);
  }

  /* Find 'initiate_background_knowledge/1' reference */
  printf("Finding the 'initiate_background_knowledge/1' reference\n");
  the_initiate_background_knowledge_reference = SP_predicate("initiate_background_knowledge",1,NULL);
  if(the_initiate_background_knowledge_reference==NULL){
    printf("Could not find the 'initiate_background_knowledge/1' reference\n");
    exit(0);
  }

  /* Create background filename reference */
  the_filename_reference = SP_new_term_ref();
  SP_put_string(the_filename_reference,"birds.bg");

  /* Query Prolog 'initiate_background_knowledge/1' */
  printf("Querying Prolog 'initiate_background_knowledge/1'\n");
  the_call_result = SP_query(the_initiate_background_knowledge_reference,the_filename_reference);
  if(the_call_result!=SP_SUCCESS){
    printf("Failed in querying Prolog 'initiate_background_knowledge/1'\n");
    exit(0);
  }
  
  /* Find follows/2 reference */
  printf("Finding the 'follows/2' reference\n");
  the_follows_reference = SP_predicate("follows",2,NULL);
  if(the_follows_reference==NULL){
    printf("Cannot find predicate 'follows/2'\n");
    exit(0);
  }

  /* Create term 'fly(A)' */
  the_predicates_list_term = SP_new_term_ref();
  the_predicate_list_term = SP_new_term_ref();
  the_predicate_term = SP_new_term_ref();
  SP_put_string(the_predicate_term,"fly");
  the_variable_term = SP_new_term_ref();
  SP_put_variable(the_variable_term);
  SP_cons_list(the_predicate_list_term,the_variable_term,the_predicate_list_term);
  SP_cons_list(the_predicate_list_term,the_predicate_term,the_predicate_list_term);
  SP_cons_list(the_predicates_list_term,the_predicate_list_term,the_predicates_list_term);

  /* Create the indecies variable */
  the_indecies_term = SP_new_term_ref();
  SP_put_variable(the_indecies_term);

  /* Query Prolog */
  printf("Querying Prolog\n");
  the_call_result = SP_query(the_follows_reference,the_predicates_list_term,the_indecies_term);
  if(the_call_result!=SP_SUCCESS){
    printf("Failed querying Prolog\n");
    exit(0);
  }
  
  printf("Succeeded in querying Prolog\nIndecies: ");
  SP_print_term(the_indecies_term);
}

main (void)
{
  int the_call_result;

  initialise_prolog();
}

 




















