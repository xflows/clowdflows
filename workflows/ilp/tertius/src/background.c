#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include "types.h"
#include "eval.h"
#include "input_output.h"
#include "search.h"

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

int initialise_prolog(char *data_filename)
{
  int the_call_result,const_count,type_count,the_predicate_count,part_count,the_local_nb_part;
  char *the_indexed_background_filename,*the_debug_string;
  FILE *the_temporary_file;
  SP_term_ref the_arity_reference,the_predicate_list_reference,the_arity_list_reference;
  SP_term_ref the_filename_reference;
  SP_pred_ref the_initiate_equality_reference,the_initiate_background_knowledge_reference;
  struct stat the_stat_buf;
  
  the_prolog_query_count = 0;
  
  /* Initiating Prolog */
  printf("Initiating Prolog\n");
  the_call_result = SP_initialize(0,NULL,"/usr/local/sicstus/lib/sicstus-3.7.1/bin/");
  if(the_call_result!=SP_SUCCESS){
    printf("Failed initiating Prolog\n");
    return ERR_PROLOG;
  }
  
  /* Initiate type table */
  /* printf("Initiating type table\n");*/
  for(type_count=0;type_count<the_nb_types;type_count++){
    if (the_type_table[type_count].global == TRUE)
      the_local_nb_part = 1;
    else
      the_local_nb_part = the_nb_parts;
    for (part_count=0;part_count<the_local_nb_part;part_count++){
      for(const_count=0;const_count<the_type_table[type_count].nb_const[part_count];const_count++){
	the_type_table[type_count].term_ref[part_count][const_count] = SP_new_term_ref();
	the_call_result = SP_put_string(the_type_table[type_count].term_ref[part_count][const_count],
				    the_type_table[type_count].constants[part_count][const_count]);
	if(the_call_result!=1){
	  printf("Failed initiating the type table\n");
	  return ERR_PROLOG;
	}
      the_call_result = SP_get_string(the_type_table[type_count].term_ref[part_count][const_count],
				  &the_debug_string);
	if(the_call_result==0){
	  printf("Failed initiating the type table\n");
	  return ERR_PROLOG;
	}
      }
    }
  }

  /* Initiate predicate table */
  /*printf("Initiating predicate table\n");*/
  for(the_predicate_count=0;the_predicate_count<the_nb_predicates;the_predicate_count++){
    the_predicate_table[the_predicate_count].term_ref = SP_new_term_ref();
    if(the_predicate_table[the_predicate_count].term_ref==0){
      printf("Failed initiation predicate table\n");
      return ERR_PROLOG;
    }
    the_call_result = SP_put_string(the_predicate_table[the_predicate_count].term_ref,
				the_predicate_table[the_predicate_count].name);
    if(the_call_result!=1){
      printf("Failed initiation predicate table\n");
      return ERR_PROLOG;
    }
  }
  
  /* Check the basis of the expected value */
  if((the_expected_value==BACKGROUND)
     ||(the_expected_value==LINEAR_RESOLUTION)
     ||(the_expected_value==INTEGRITY_CONSTRAINTS)){
    /* The expected value is based on background knowledge */
    
    /* Create temporary file 'tertius_background_tmp.pl' */
    /*printf("Creating temporary file 'tertius_background_tmp.pl'\n");*/
    the_temporary_file = fopen("tertius_background_tmp.pl","w");
    if(the_temporary_file==NULL){
      printf("Could not create temporary file 'tertius_background_tmp.pl'\n");
      return 1;
    }

    /* Add dynamic predicate declaration to file */
    fprintf(the_temporary_file,":- dynamic ");
    for(the_predicate_count=0;the_predicate_count<the_nb_predicates-1;the_predicate_count++){
      fprintf(the_temporary_file," %s/%i,",the_predicate_table[the_predicate_count].name,
	      the_predicate_table[the_predicate_count].arity
	      +the_predicate_table[the_predicate_count].num_param);
    }
    fprintf(the_temporary_file," %s/%i.\n\n",the_predicate_table[the_predicate_count].name,
	    the_predicate_table[the_predicate_count].arity
	    +the_predicate_table[the_predicate_count].num_param);
    /*fprintf(the_temporary_file,":- print('loaded temporary file').\n\n");*/
    
    /* Closing the temporary file 'tertius_background_tmp.pl' */
    /* printf("Closing the temporary file 'tertius_background_tmp.pl'\n");*/
    the_call_result = fclose(the_temporary_file);
    if(the_call_result!=0){
      printf("Could not close the temporary file 'tertius_background_tmp.pl'\n");
      return 1;
    }

    /* Check if the expected value is based on linear resolution */
    if(the_expected_value==LINEAR_RESOLUTION){
      /* The expected value is based on linear resolution */
      /* Compile pttp related prolog files */
      /*printf("Fcompiling files 'pttp.pl', 'follows.pl' and 'tertius_background_tmp.pl'\n");*/
      the_call_result = 
	system("/usr/local/sicstus/bin/sicstus -l pttp_compile.pl > compile.log 2> compile.log");
      if(the_call_result!=0){
	printf("Failed in fcompiling files 'pttp.pl', 'follows.pl' and 'tertius_background_tmp.pl'\n");
	return ERR_PROLOG;
      }
      
      /* Load file 'pttp.ql' */
      /*printf("Loading file 'pttp.ql'\n");*/
      the_call_result = SP_load("pttp.ql");
      if(the_call_result!=SP_SUCCESS){
	printf("Could not load file 'pttp.ql'\n");
	return ERR_PROLOG;
      }
      
      /* Remove the pttp.ql file */
      /*printf("Removing file 'pttp.ql'\n");*/
      the_call_result = remove("pttp.ql");
      if(the_call_result!=0){
	printf("Could not remove file 'pttp.pl'\n");
	return 1;
      }
      
    }else{
      /* The expected value is based on normal resolution or integrity constraints */
      /* Compile prolog files */
      /*printf("Fcompiling files 'follows.pl' and 'tertius_background_tmp.pl'\n");*/
      the_call_result = 
	system("/usr/local/sicstus/bin/sicstus -l compile.pl > compile.log 2> compile.log");
      if(the_call_result!=0){
	printf("Failed in fcompiling files 'follows.pl' and 'tertius_background_tmp.pl'\n");
	return ERR_PROLOG;
      }
    }
    
    /* Load file 'tertius_background_tmp.ql' */
    /*printf("Loading file 'tertius_backgroud_tmp.ql'\n");*/
    the_call_result = SP_load("tertius_background_tmp.ql");
    if(the_call_result!=SP_SUCCESS){
      printf("Could not load file 'tertius_background_tmp.ql'\n");
      return ERR_PROLOG;
    }

    /* Load file 'follows.ql' */
    /*printf("Loading file 'follows.ql'\n");*/
    the_call_result = SP_load("follows.ql");
    if(the_call_result!=SP_SUCCESS){
      printf("Could not load file 'follows.ql'\n");
      return ERR_PROLOG;
    }
    
    /* Remove the temporary files */
    /*printf("Removing file 'follows.ql'\n");*/
    /*the_call_result = remove("follows.ql");
    if(the_call_result!=0){
      printf("Could not remove file 'follows.ql'\n");
      return 1;
    }*/

    /*printf("Removing file 'tertius_backgound_tmp.pl'\n");*/
    the_call_result = remove("tertius_background_tmp.pl");
    if(the_call_result!=0){
      printf("Could not remove file 'tertius_backgroud_tmp.pl'\n");
      return 1;
    }

    /*printf("Removing file 'tertius_background_tmp.ql'\n");*/
    the_call_result = remove("tertius_background_tmp.ql");
    if(the_call_result!=0){
      printf("Could not remove file 'tertius_backgroud_tmp.ql'\n");
      return 1;
    } 

    /* Check if the basis of the expected value is linear resolution */
    if(the_expected_value==LINEAR_RESOLUTION){
      /*printf("Finding initiate_pttp_background_knowledge reference\n");*/
      /* The expected value is based on linear resolution */
      the_initiate_background_knowledge_reference = 
	SP_predicate("initiate_pttp_background_knowledge",1,NULL);
      if(the_initiate_background_knowledge_reference==NULL){
	printf("Could not find initiate_pttp_background_knowledge reference\n");
	return ERR_PROLOG;
      }
    }else{
      /* The expected value is based on normal resolution or integrity constraints */
      /*printf("Finding initiate_background_knowledge reference\n");*/
      the_initiate_background_knowledge_reference = 
	SP_predicate("initiate_background_knowledge",1,NULL);
      if(the_initiate_background_knowledge_reference==NULL){
	printf("Could not find initiate_background_knowledge reference\n");
	return ERR_PROLOG;
      }
    }
    
    /* Create background filename */
    the_indexed_background_filename = Calloc(strlen(data_filename)+3,char)
      if (the_indexed_background_filename == NULL)
	return 2;
    the_indexed_background_filename = strcpy(the_indexed_background_filename,data_filename);
    the_indexed_background_filename = strcat(the_indexed_background_filename,".bg");
    the_filename_reference = SP_new_term_ref();
    the_call_result = SP_put_string(the_filename_reference,the_indexed_background_filename);
    
    /* Query initiate background */
    /* Check if a background file exists */
    if(stat(the_indexed_background_filename,&the_stat_buf)==-1){
      printf("No background knowledge file (*.bg) was found\n");
    }else{
      the_call_result = SP_query(the_initiate_background_knowledge_reference,the_filename_reference);
      if(the_call_result!=SP_SUCCESS){
	printf("Failed in querying Prolog 'initiate_(pttp_)background_knowledge\n");
	return ERR_PROLOG;
      }
      the_prolog_query_count++;
    }
    
    /*printf("Removing file 'compile.log'\n");*/
    the_call_result = remove("compile.log");
    if(the_call_result!=0){
      printf("Could not remove file compile.log\n");
      return 1;
    }
    
    /* Check if unification form is explicit */
    if(unification_form==EXPLICIT){
      /* Unification is explicit */
      
      /* Initiate prolog equality theory */      
      /* Create prolog lists of predicates and arities */
      the_arity_reference = SP_new_term_ref();
      the_arity_list_reference = SP_new_term_ref();
      the_predicate_list_reference = SP_new_term_ref();
      for(the_predicate_count=0;the_predicate_count<the_nb_predicates;the_predicate_count++){
	/* Add predicate to predicate list */
	the_call_result = SP_cons_list(the_predicate_list_reference,
				       the_predicate_table[the_predicate_count].term_ref,
				       the_predicate_list_reference);
	if(the_call_result==0)
	  return ERR_PROLOG;
	/* Add arity to arity list */
	SP_put_integer(the_arity_reference,the_predicate_table[the_predicate_count].arity+
		       the_predicate_table[the_predicate_count].num_param);
	if(the_call_result==0)
	  return ERR_PROLOG;
	the_call_result = SP_cons_list(the_arity_list_reference,the_arity_reference,
				       the_arity_list_reference);
	if(the_call_result==0)
	  return ERR_PROLOG;    
      }
      /*SP_print_term(the_predicate_list_reference);
	SP_print_term(the_arity_list_reference);*/
      
      /* Call initiate_equality in Prolog */
      /*printf("Finding 'initiate_equality/2' reference\n");*/
      the_initiate_equality_reference = SP_predicate("initiate_equality",2,NULL);
      if(the_initiate_equality_reference==NULL){
	printf("Could not find 'initiate_equality/2' reference\n");
	return ERR_PROLOG;
      }
      /*printf("Querying Prolog 'initiate_equality/2'\n");*/
      the_call_result = SP_query(the_initiate_equality_reference,
				 the_predicate_list_reference,the_arity_list_reference);
      if(the_call_result!=SP_SUCCESS){
	printf("Failed in querying Prolog 'initiate_equality/2\n");
	return ERR_PROLOG;
      }
      the_prolog_query_count++;
      
    } /* Ends explicit unification form */
    
    
    /* Find the follows reference */
    /*printf("Finding the '(pttp_)(ic_)follows/2' reference\n");*/
    /* Check the basis for the expected value */
    switch(the_expected_value){
    case BACKGROUND:
      the_follows_reference = SP_predicate("follows",2,NULL);
      break;
    case LINEAR_RESOLUTION:
      the_follows_reference = SP_predicate("pttp_follows",2,NULL);
      break;
    case INTEGRITY_CONSTRAINTS:
      the_follows_reference = SP_predicate("ic_follows",2,NULL);
      break;
    }
    if(the_follows_reference==NULL){
      printf("Could not find the '(pttp_)(ic_)follows/2' reference\n");
      return ERR_PROLOG;
    }
    
  } /* Ends expected value based on background knowledge */
  
  /* Check query prolog mode */
  if(query_prolog==TRUE){
    /* Prolog query mode is on */
    /* Create the variable 'X' */
    the_variable = SP_new_term_ref();
    
    /* Find the conjunction reference */
    the_conjunction_reference = SP_predicate(",",2,NULL);
    if(the_conjunction_reference==0){
      return ERR_PROLOG;
    }
  } /* Ends query prolog mode on */

  /* Find the 'garbage_collect' reference */
  the_garbage_collect_reference = SP_predicate("garbage_collect",0,NULL);
  if(the_garbage_collect_reference==0){
    return ERR_PROLOG;  
  }
  
  /* Return Success */
  return 0;
}
























