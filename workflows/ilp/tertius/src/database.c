#if JAVA
#include <jni.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#if JAVA
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#endif
#include "types.h"
#include "input_output.h"
#include "search.h"
#include "eval.h"
#include "gestion.h"
#include "error.h"
#include "write.h"

char views [100][20];
int the_num_views = 0;
int database;

#if JAVA
JNIEnv *the_java_env;
JavaVM *the_java_VM;
jclass the_database_class;
jobject the_database_connection;
#endif

void addAND (char * query, int nbAnd);
void itostr(char c, int integer, char * the_query);
void line2query(char * the_string, char ** query);
int counting(tcube *a_cube,int *a_lit_indices, int a_lit_num, long *a_count);
#if JAVA
jarray cStrArray2j(char ** cStrArray, int size);
void jStrArray2c(jarray arr, char *** cStrArray, int * size);
void jStrArr_of_Array2c(jarray arr, char **** cStrArrArray, int *size_2);
#endif

int myStrCat(char **a_destination_string,char *a_string,long *a_length)
{
int the_error = 0;
char *the_new_string;
long the_new_length;

if (strlen(*a_destination_string) + strlen(a_string) > *a_length) {
  the_new_length = *a_length + strlen(a_string) + 1000;
  the_new_string = Calloc(the_new_length,char)
  if (the_new_string == NULL) return 2;
  strcat(the_new_string,*a_destination_string);
  Free_calloc(*a_length,*a_destination_string)
  *a_destination_string = the_new_string;
  *a_length = the_new_length;
}
strcat(*a_destination_string,a_string);
return the_error;
}

/*********************************************************************************/
int collect_parameters()
{
int the_error = 0;
int i;
int *the_instance;
tpredicate *the_predicate;
int the_num_param,the_num_arg;
char *the_query;/*where the query is stored in c*/
char *** the_results;
int the_num_results=0;
int the_result_line;
long the_length;
#if JAVA
jstring the_jquery;/*the query in java*/
jarray the_jresults;
jmethodID the_method_ID;
fprintf (stderr,"In collect_parameters\n");
the_method_ID = (*the_java_env)->GetStaticMethodID(the_java_env, the_database_class, "process_the_params_query", "(Ljava/sql/Connection;Ljava/lang/String;)[[Ljava/lang/string");
#endif
		

for (i=0; i<the_nb_predicates; i++) {
  the_predicate = &(the_predicate_table[i]);
  if ((the_predicate->num_occurence > 0)&&(the_predicate->num_param > 0)) {
    the_length = (17 + the_predicate->num_param * 7 + 7 + strlen(the_predicate->name) + 10);
    the_query = Calloc(the_length, char)
    if (the_query == NULL) return 2;
    the_query [0] = '\0';
    if (myStrCat(&the_query, "SELECT DISTINCT (",&the_length) == 2) return 2;
    the_instance = Calloc(the_predicate->arity+the_predicate->num_param,int)
    if (the_instance == NULL) the_error = 2; else {
      the_num_param = 0; the_num_arg = 0;
      while ((the_num_param + the_num_arg < the_predicate->arity+the_predicate->num_param)&&(the_error==0)) {
	if (the_predicate->params[the_num_param+the_num_arg] == TRUE) {
	  /* collect the column the_type_table[the_predicate->types[the_num_param]].name of the_predicate */
	  itostr('C',the_num_param+the_num_arg, the_query);
	  if (myStrCat(&the_query, ", ",&the_length) == 2) return 2;
	  the_num_param += 1;
	}
	else
	  the_num_arg += 1;
      }
      the_query[strlen(the_query) - 2] = '\0';
      if (myStrCat(&the_query, ") FROM ",&the_length) == 2) return 2;
      if (myStrCat(&the_query, the_predicate->name,&the_length) == 2) return 2;
#if JAVA
      the_jquery = (*the_java_env)->NewStringUTF(the_java_env, the_query);
      the_jresults = (*the_java_env)->CallStaticObjectMethod(the_java_env, the_database_class, the_method_ID, the_database_connection, the_jquery);
      jStrArr_of_Array2c(the_jresults, &the_results, &the_num_results);
#endif
	  
      /* for each line ! */
      for (the_result_line=0;the_result_line<the_num_results;the_result_line++) {
	for (the_num_param = 0;(the_num_param < the_predicate->num_param)&&(the_error==0);the_num_param++) {
	  the_error = fact_name_store(the_predicate->types[the_num_param],0,the_results[the_num_param][the_result_line],
				      STORE,&(the_instance[the_num_param]));
	}
	for (the_num_arg = 0; (the_num_arg < the_predicate->arity)&&(the_error==0);the_num_arg++)
	  the_instance[the_predicate->num_param+the_num_arg] = 0;
	the_error = store_instance(the_instance,POS,the_predicate,1,0,STORE);
      } /* end for each line */
      Free_calloc(the_predicate->arity+the_predicate->num_param,the_instance)
      Free_calloc(the_length, the_query)
    }
  }
}

return the_error;
}

/*****************************************************************************/

int eval_cube_in_database(tcube *a_cube)
{
int the_error = 0;
int the_first_lit, the_individual_arity, i, the_other_lit;
int the_block_sign;
int *the_body,*the_head,*the_whole_conjunction;
int the_body_count=0;
int the_head_count=0;
int the_whole_conjunction_count=0;
tlist the_debug_node;

the_body = Calloc(a_cube->nb_lit,int)
if (the_body == NULL)
  return 2;
the_head = Calloc(a_cube->nb_lit,int)
if (the_head == NULL)
  return 2;
the_whole_conjunction = Calloc(a_cube->nb_lit,int)
if (the_whole_conjunction == NULL)
  return 2;
if (a_cube->nb_arg != 0) {
  if (individual_based == TRUE) {
    the_first_lit = 1;
    the_individual_arity = the_predicate_table[a_cube->lits[0].predicate].arity;
    the_head[the_head_count] = 0;
    the_head_count++;
    the_body[the_body_count] = 0;
    the_body_count++;
    the_whole_conjunction[the_whole_conjunction_count] = 0;
    the_whole_conjunction_count++;
  }
  else {
    the_first_lit = 0;
    the_individual_arity = a_cube->nb_arg;
    error_deal_with(1,32);
  }
 if ((the_search_bias == CLASSIFICATION)
      || (the_search_bias == POSITIVE_CLASSIFICATION)
      || (the_search_bias == HORN_POSITIVE_CLASSIFICATION)) {
    the_head[the_head_count] = the_first_lit;
    the_head_count++;
    the_whole_conjunction[the_whole_conjunction_count] = the_first_lit;
    the_whole_conjunction_count++;
    for (the_other_lit=the_first_lit+1;the_other_lit<a_cube->nb_lit;the_other_lit++) {
      the_body[the_body_count] = the_other_lit;
      the_body_count++;
      the_whole_conjunction[the_whole_conjunction_count] = the_other_lit;
      the_whole_conjunction_count++;
    }
  }
  else /* not classification */{
    the_error = make_literal_blocks(a_cube);
    if (the_error == 0) {
      for (i=the_first_lit;i<a_cube->nb_lit;i++) {
	the_block_sign = POS;
	for (the_other_lit=the_first_lit;
	     (the_block_sign == POS)&&(the_other_lit<a_cube->nb_lit);
	     the_other_lit++)
	  if ((a_cube->blocks_table[the_other_lit]
	     == a_cube->blocks_table[i])
	      && (a_cube->lits[the_other_lit].sign == NEG))
	    the_block_sign = NEG;
	/* then the sign of the literal is known... */
	if (the_block_sign == POS) {
	  the_head[the_head_count] = i;
	  the_head_count++;
	  the_whole_conjunction[the_whole_conjunction_count] = i;
	  the_whole_conjunction_count++;
	}
	else {
	  the_body[the_body_count] = i;
	  the_body_count++;
	  the_whole_conjunction[the_whole_conjunction_count] = i;
	  the_whole_conjunction_count++;
	}
      }
    }
  }

 the_debug_node.elt = a_cube;
 the_debug_node.next = NULL;
 write_clauses_appropriate(stderr,&the_debug_node,0);
  
  the_error = counting(a_cube, the_whole_conjunction, the_whole_conjunction_count, &(a_cube->nb_inst));
  		/* of the conjunction of the negation of the head and the body */

  the_error = counting(a_cube, the_body, the_body_count, &(a_cube->nb_inst_positive));
		/* body */
  
  the_error = counting(a_cube, the_head, the_head_count, &(a_cube->nb_inst_negative));
  		/* negation of the head */

  the_error = counting(a_cube, the_whole_conjunction, 1, &(a_cube->capital_n));
  		/* number of individuals since the individual is the first literal */
  
  
    a_cube->observed = (double)a_cube->nb_inst / (double)a_cube->capital_n;
    if (average == FALSE) {
      if ((the_expected_value != NORMAL)
	  || (unification_form == EXPLICIT))
	error_deal_with(1,31);
      evaluate_confirmation_and_optimistic(a_cube->nb_inst,a_cube->nb_inst_negative,
					   a_cube->nb_inst_positive,
					   a_cube->capital_n,a_cube->block_nb_inst,a_cube->num_blocks,
					   &(a_cube->confirmation),&(a_cube->optimistic));
    }
  }

Free_calloc(a_cube->nb_lit,the_body)
Free_calloc(a_cube->nb_lit,the_head)
Free_calloc(a_cube->nb_lit,the_whole_conjunction)
return the_error;
}


/*******************************************************************/

int counting(tcube *a_cube,int *a_lit_indices, int a_lit_num, long *a_count)
{
int the_error=0;
char *the_query;
int nbAnd = 0;
int nbAnd2;
int the_arg,the_param;
int the_lit;
int the_first_lit;
int the_lit_arg;
int stop;
int the_first_lit_arg;
tpredicate *the_predicate;
long the_length;
#if JAVA
jstring the_jquery;	
jmethodID the_method_ID;
jint the_jresult;
fprintf (stderr,"In counting(\n");
the_method_ID = (*the_java_env)->GetStaticMethodID(the_java_env, the_database_class, "process_the_count_query", "(Ljava/sql/Connection;Ljava/lang/String;)I");
#endif

the_length = 14 + NBPREDS * 7 + 7 + NBPREDS * (SIZEPRED + 6) + 7 + 26 * NBPREDS * NBARGS + NBPREDS * (18 +  NBPREDS * 7 + 7 + NBPREDS * (SIZEPRED + 6));
the_query = Calloc(the_length, char)
if (the_query == NULL) return 2;
the_predicate = &(the_predicate_table[a_cube->lits[0].predicate]);
if (myStrCat(&the_query,"SELECT COUNT (DISTINCT ",&the_length) == 2) return 2;
for (the_arg=0;the_arg<the_predicate->arity;the_arg++) {
  if (myStrCat(&the_query,"L0.",&the_length) == 2) return 2;
  itostr('C',the_arg,the_query);
  if (myStrCat(&the_query,", ",&the_length) == 2) return 2;
}
the_query[strlen(the_query)-2] = '\0';
if (myStrCat(&the_query,") FROM ",&the_length) == 2) return 2;
/* the_individual */
if (myStrCat(&the_query,the_predicate->name,&the_length) == 2) return 2;
if (myStrCat(&the_query," L0, ",&the_length) == 2) return 2;
/* list the negative literals */
for (the_lit=1;the_lit<a_lit_num;the_lit++)
  if (a_cube->lits[a_lit_indices[the_lit]].sign == NEG) {
    if (myStrCat(&the_query,the_predicate_table[a_cube->lits[a_lit_indices[the_lit]].predicate].name,&the_length) == 2) return 2;
    if (myStrCat(&the_query," ",&the_length) == 2) return 2;
    itostr('L',a_lit_indices[the_lit], the_query);
    if (myStrCat(&the_query,", ",&the_length) == 2) return 2;
  }
/* end of the from part */
the_query[strlen(the_query)-2] = '\0';
/* set of join conditions */
for (the_arg=0;the_arg<a_cube->nb_arg;the_arg++) {
  /* search the first occurrence */
  stop = FALSE;
  for (the_first_lit=0;(stop==FALSE) && (the_first_lit<a_lit_num);the_first_lit++)
    for (the_first_lit_arg=0;
	 (stop==FALSE)
	   && (the_first_lit_arg<the_predicate_table[a_cube->lits[a_lit_indices[the_first_lit]].predicate].arity);
	 the_first_lit_arg++)
      if (a_cube->lits[a_lit_indices[the_first_lit]].args[the_first_lit_arg]==the_arg)
	stop = TRUE;
  the_first_lit--;
  the_first_lit_arg--;
  /* search the other occurrences in negative literals*/
  for (the_lit=the_first_lit+1;the_lit<a_lit_num;the_lit++)
    if (a_cube->lits[a_lit_indices[the_lit]].sign == NEG)
      for (the_lit_arg=0;
	   (the_lit_arg<the_predicate_table[a_cube->lits[a_lit_indices[the_lit]].predicate].arity);
	   the_lit_arg++)
	if (a_cube->lits[a_lit_indices[the_lit]].args[the_lit_arg]==the_arg) {
	  addAND(the_query, nbAnd++);
	  itostr('L',a_lit_indices[the_first_lit], the_query);
	  if (myStrCat(&the_query,".",&the_length) == 2) return 2;
	  itostr('C',
		 the_predicate_table[a_cube->lits[a_lit_indices[the_first_lit]].predicate].varIndices[the_first_lit_arg],
		 the_query);
	  if (myStrCat(&the_query," = ",&the_length) == 2) return 2;
	  itostr('L',a_lit_indices[the_lit], the_query);
	  if (myStrCat(&the_query,".",&the_length) == 2) return 2;
	  itostr('C',
		 the_predicate_table[a_cube->lits[a_lit_indices[the_lit]].predicate].varIndices[the_lit_arg],
		 the_query);
	 }
 } /* end of the join conditions */
 /* set of conditions on the properties */
 for (the_lit=the_first_lit;the_lit<a_lit_num;the_lit++)
   if (a_cube->lits[a_lit_indices[the_lit]].sign == NEG)
     for (the_param=0;the_param<the_predicate_table[a_cube->lits[a_lit_indices[the_lit]].predicate].num_param;the_param++) {
       addAND(the_query, nbAnd++);
       itostr('L',a_lit_indices[the_lit], the_query);
       if (myStrCat(&the_query,".",&the_length) == 2) return 2;
       itostr('C',the_predicate_table[a_cube->lits[a_lit_indices[the_lit]].predicate].paramIndices[the_param],the_query);
       if (myStrCat(&the_query," = '",&the_length) == 2) return 2;
       if (myStrCat(&the_query,the_type_table[the_predicate_table[a_cube->lits[a_lit_indices[the_lit]].predicate].types[the_param]]
	      .constants[0][the_predicate_table[a_cube->lits[a_lit_indices[the_lit]].predicate]
			   .subpredicates[a_cube->lits[a_lit_indices[the_lit]].subpredicate]->val_params[the_param]],&the_length) == 2) return 2;
       if (myStrCat(&the_query,"'",&the_length) == 2) return 2;
     }
 /* positive literals of the clause, i.e. negative in the conjunction, i.e. not in */
 for (the_lit=1;the_lit<a_lit_num;the_lit++)
   if (a_cube->lits[a_lit_indices[the_lit]].sign == POS) {
     addAND(the_query, nbAnd++);
     if (myStrCat(&the_query,"(",&the_length) == 2) return 2;
     for (the_lit_arg=0;
	  (the_lit_arg<the_predicate_table[a_cube->lits[a_lit_indices[the_lit]].predicate].arity);
	  the_lit_arg++) {
       stop = FALSE;
       for (the_first_lit=0;(stop==FALSE) && (the_first_lit<the_lit);the_first_lit++)
	 for (the_first_lit_arg=0;
	      (stop==FALSE)
		&& (the_first_lit_arg<the_predicate_table[a_cube->lits[a_lit_indices[the_first_lit]].predicate].arity);
	      the_first_lit_arg++)
	   if (a_cube->lits[a_lit_indices[the_first_lit]].args[the_first_lit_arg]
	       ==a_cube->lits[a_lit_indices[the_lit]].args[the_lit_arg])
	     stop = TRUE;
       the_first_lit--;
       the_first_lit_arg--;
       itostr('L',a_lit_indices[the_first_lit],the_query);
       if (myStrCat(&the_query,".",&the_length) == 2) return 2;
       itostr('C',the_first_lit_arg,the_query);
       if (myStrCat(&the_query,", ",&the_length) == 2) return 2;
     }
     the_query[strlen(the_query)-2]='\0';
     if (myStrCat(&the_query,") not in (select ",&the_length) == 2) return 2;
     for (the_lit_arg=0;
	  (the_lit_arg<the_predicate_table[a_cube->lits[a_lit_indices[the_lit]].predicate].arity);
	  the_lit_arg++) {
       itostr('L',a_lit_indices[the_lit], the_query);
       if (myStrCat(&the_query,".",&the_length) == 2) return 2;
       itostr('C',the_lit_arg,the_query);
       if (myStrCat(&the_query,", ",&the_length) == 2) return 2;
     }
     the_query[strlen(the_query)-2]='\0';
     if (myStrCat(&the_query," from ",&the_length) == 2) return 2;
     if (myStrCat(&the_query,the_predicate_table[a_cube->lits[a_lit_indices[the_lit]].predicate].name,&the_length) == 2) return 2;
     if (myStrCat(&the_query," ",&the_length) == 2) return 2;
     itostr('L',a_lit_indices[the_lit], the_query);
     nbAnd2 = 0;
     /* set of conditions on the properties */
     for (the_param=0;the_param<the_predicate_table[a_cube->lits[a_lit_indices[the_lit]].predicate].num_param;the_param++) {
       addAND(the_query, nbAnd2++);
       itostr('L',a_lit_indices[the_lit], the_query);
       if (myStrCat(&the_query,".",&the_length) == 2) return 2;
       itostr('C',the_predicate_table[a_cube->lits[a_lit_indices[the_lit]].predicate].paramIndices[the_param],the_query);
       if (myStrCat(&the_query," = '",&the_length) == 2) return 2;
       if (myStrCat(&the_query,the_type_table[the_predicate_table[a_cube->lits[a_lit_indices[the_lit]].predicate].types[the_param]]
	      .constants[0][the_predicate_table[a_cube->lits[a_lit_indices[the_lit]].predicate]
			   .subpredicates[a_cube->lits[a_lit_indices[the_lit]].subpredicate]->val_params[the_param]],&the_length) == 2) return 2;
       if (myStrCat(&the_query,"'",&the_length) == 2) return 2;
     }
     if (myStrCat(&the_query,")",&the_length) == 2) return 2;
   }
	
#if JAVA
the_jquery = (*the_java_env)->NewStringUTF(the_java_env,the_query);
the_jresult = (*the_java_env)->CallStaticIntMethod(the_java_env, the_database_class, the_method_ID, the_database_connection, the_jquery);

(* a_count) = the_jresult;
#endif

Free_calloc(the_length,the_query)
return the_error;
}


/********************************************************/

int create_view(char * the_stem_name)
{
#if JAVA
	int the_num_query = 0;
	int fd = 0,  the_string_length = 0, char_count = 0, BN_count = 0;/*the_new_line = FALSE,*/
	char the_db_file[80];
	char the_string [80];
	char * query [NBPREDS];
	char c;
	jarray the_query_array;
	jmethodID the_method_ID;
fprintf (stderr,"In create_view\n");
	
	the_method_ID = (*the_java_env)->GetStaticMethodID(the_java_env, the_database_class, "process_the_create_view_query", "(Ljava/sql/Connection;[Ljava/lang/String;)I");

	strcpy(the_db_file, the_stem_name);
	if (myStrCat(&the_db_file, ".db",&the_length) == 2) return 2;
		
	if((fd = open(the_db_file, O_RDONLY)) == -1);
		/*erreur*/
 		
	if((char_count = read(fd,&c,1)) == -1);
		/*erreur*/
	
	while(char_count == 1 && BN_count < 7)
	{
		if(c == '\n')
			BN_count++;
		if((char_count = read(fd,&c,1)) == -1);
			/*erreur	*/
	}
	
	if(BN_count == 7)
	{
		while(char_count == 1)/* && the_new_line == FALSE)*/
		{
			if(c == '\n' && the_string_length > 0)
			{
				the_string[the_string_length] = '\0';
				line2query (the_string, &query[the_num_query++]);
				/*the_new_line = TRUE;*/
				the_string[0] = '\0';
				the_string_length = 0;
			}	
			else
				if(c != ' ')
					the_string[the_string_length++] = c;
		
			if((char_count = read(fd,&c,1)) == -1);
				/*erreur*/
		}
        query[the_num_query][0] = '\0';
	}
	
	the_query_array = cStrArray2j(query, the_num_query) ;
	
	(*the_java_env)->CallStaticIntMethod(the_java_env, the_database_class, the_method_ID, the_database_connection, the_query_array);
#endif

	return 1;
}			

/*******************************************************/

void drop_all_views()
{
#if JAVA
	jmethodID the_method_ID;
	jarray arr;
fprintf (stderr,"In drop_all_views\n");
	
	the_method_ID = (*the_java_env)->GetStaticMethodID(the_java_env, the_database_class, "drop_views", "(Ljava/sql/Connection;[Ljava/lang/String;)I");
	
	arr = cStrArray2j ((char **)views, the_num_views);
	
	(*the_java_env)->CallStaticIntMethod(the_java_env, the_database_class, the_method_ID, the_database_connection, arr);
#endif
}	

/*******************************************************/

void line2query(char * the_string, char ** query) {
	int i = 0, j =0;
	int column_index = 1;
	char table_name [20];
fprintf (stderr,"In line2query\n");
	
	(* query) = Calloc ( 200, char)
	
	(*query)[0] = '\0';
	strcat((*query), "CREATE VIEW ");
					
	for(i = 0; the_string[i] != '('; i++);
	strncpy(views[the_num_views++], the_string, i);
	strncat((*query), the_string, i + 1);

	for(j = i; the_string[j] !=	')'; j++)
	{
		if(the_string[j] == ',')
		{
			itostr('c', column_index, (*query));
			strcat((*query),", ");
			column_index++;
		}
	}
	if(the_string[j] == ')')
	{
		itostr('c', column_index, (*query));		
		strcat((*query), ")");
	}		
							
	strcat((*query), " AS SELECT ");	
		
	for(i = j; the_string[i - 1] != ':' && the_string[i] != '-' ; i++); 	
	for(j = i; the_string[j] != '('; j++);	
			
	table_name[0] = '\0';
	strncat (table_name, the_string + i + 1, j - i - 1);
		
	for(i = j; the_string[i] != ')'; i++)
	{
		if(the_string[i] == ',')
		{
			if(the_string[i - 1] != '-')
			{
				strncat((*query), the_string + j + 1, i - j - 1);
				strcat((*query), ", ");
			}
			j = i;
		}
	}
	if(the_string[i] == ')')
	{
		if(the_string[i - 1] != '-')
			strncat((*query), the_string + j + 1, i - j - 1);
		else
			(*query)[strlen((*query)) - 2] = '\0';	
	}		
								
	strcat((*query), " FROM ");
	strcat((*query), table_name);		
}

/* Create a new Java Virtual Machine and get connection to database */

void getConnection (char * stem) {
#if JAVA
	jmethodID the_method_ID;
	JavaVMInitArgs vm_args;
	JavaVMOption option [1];
	jint res;
	jstring the_stem;
fprintf (stderr,"In getConnection\n");
	
	option [0].optionString = "-Djava.class.path=.:/opt/jdk1.2.2/src.jar";
	vm_args.version = JNI_VERSION_1_2;
	vm_args.options = option;
	vm_args.nOptions = 1;
	vm_args.ignoreUnrecognized = TRUE;
	
		/* Create the Java VM */
	res = JNI_CreateJavaVM(&the_java_VM,(void **)&the_java_env,&vm_args);
	if (res < 0)
		fprintf(stderr,"Can create Java Virtual Machine !\n");
fprintf (stderr, "%d\n", * the_java_env);
	the_database_class = (*the_java_env)->FindClass(the_java_env, "JDBC_functions");
	if (the_database_class == 0 )
		fprintf (stderr, "Can't find JDBC_functions class !\n");
			
	the_method_ID = (*the_java_env)->GetStaticMethodID(the_java_env, the_database_class, "connect_to_database", "(Ljava/lang/String;)Ljava/sql/Connection;");
	if (the_method_ID == 0)
		fprintf (stderr, "Can't find JDBC_functions.connect_to_database ! \n");
		
	the_stem = (*the_java_env)->NewStringUTF(the_java_env, stem);
	if (the_stem == 0)
		fprintf (stderr,"Out of memory !\n");
	
	the_database_connection = (*the_java_env)->CallStaticObjectMethod(the_java_env, the_database_class, the_method_ID, the_stem);
#endif
}


/* Close connection to database and destroy the current Java Virtual Machine */

void deConnection () {
#if JAVA
	jmethodID the_method_ID;
fprintf (stderr,"In deConnection\n");
	
	the_method_ID = (*the_java_env)->GetStaticMethodID(the_java_env, the_database_class, "deconnect_from_database", "(Ljava/sql/Connection;)V");
		/* mid == 0 : Can't find JDBC_functions.deconnect_from_database */
	
	(*the_java_env)->CallStaticVoidMethod(the_java_env, the_database_class, the_method_ID);
	
    (*the_java_VM)->DestroyJavaVM(the_java_VM);
	
	return;
#endif
}

/*****************************************************************/

/* USEFUL FUNCTIONS */

#if JAVA
/*"Translate" a java string array to a c char * array*/

void jStrArray2c(jarray arr, char *** cStrArray, int * size) {
	int i = 0 ;
	jsize lenArr ;
	jstring jStrTmp ;
	char * cStrTmp ;
fprintf (stderr,"In jStrArray2c\n");

	lenArr = (*the_java_env)->GetArrayLength(the_java_env, arr);
	
	(*size) = lenArr ; /* lenArr */
	
	(*cStrArray) = (char **)Calloc ((*size), char *);
	
				/* copy from java string array to a c array */
	while ((*size) > i) {
		
		jStrTmp = (*the_java_env)->GetObjectArrayElement (the_java_env, (jobjectArray) arr, i);
		
		cStrTmp =(char *) (*the_java_env)->GetStringUTFChars(the_java_env, jStrTmp, 0);
		
		(*cStrArray) [i] = (char *) Calloc ((strlen (cStrTmp) + 1), char);
		
		strcpy ((*cStrArray) [i], cStrTmp);

		(*the_java_env)->ReleaseStringUTFChars(the_java_env, jStrTmp, cStrTmp);
		
		i++;
	}
}

/*"Translate" a java array of string array to the c eq*/

void jStrArr_of_Array2c(jarray arr, char **** cStrArrArray, int *a_size) {
	jsize lenArr;
	jarray current_array;
	int size;
	int i = 0;
fprintf (stderr,"In jStrArr_of_Array2c\n");
	
	lenArr = (*the_java_env)->GetArrayLength(the_java_env, arr);
	
	(*a_size) = lenArr;
	
	(*cStrArrArray) = (char ***)Calloc ((*a_size), char **);
	
	while ((*a_size) > i) {
		
		current_array = (*the_java_env)->GetObjectArrayElement (the_java_env, (jobjectArray) arr, i);
		
		jStrArray2c(current_array,&((*cStrArrArray)[i]), &size);
		
		i++;
	}
}

/*"Translate" a c string array to a java array*/

jarray cStrArray2j(char ** cStrArray, int size) {
	int i = 0 ;
	jsize lenArr = size;
	jsize i_bis;
	jobjectArray the_res;
	jclass cls;
	jstring jStrTmp;
	
fprintf (stderr,"In cStrArray2j\n");
for (i = 0; i < size; i++)
	fprintf (stderr,"cStrArray [%d] : %s\n", i, cStrArray [i]);
fprintf (stderr, "All element printed\n");

fprintf (stderr, "%d\n", * the_java_env);
	cls = (*the_java_env)->FindClass(the_java_env, "Tertius");
	if (cls == NULL)
		fprintf (stderr,"Can't find class java.lang.String\n");
	fprintf (stderr,"In cStrArray2j\n");

	jStrTmp = (*the_java_env)->NewStringUTF(the_java_env, "");
	if (jStrTmp == NULL)
		fprintf (stderr,"Can't construct string\n");
	fprintf (stderr,"In cStrArray2j\n");
	
	the_res = (*the_java_env)->NewObjectArray(the_java_env, lenArr, cls, jStrTmp);
	if (the_res == NULL)
		fprintf (stderr,"Can't create Array\n");
	fprintf (stderr,"In cStrArray2j\n");
	
	
	for (i = 0; i < size; i++) {
		i_bis = i;
		jStrTmp = (*the_java_env)->NewStringUTF(the_java_env, cStrArray [i]);
		(*the_java_env)->SetObjectArrayElement (the_java_env, the_res, i_bis, jStrTmp);
	}
	
	return the_res;
}
#endif

void itostr(char c, int integer, char * the_query) /* a transformer en srtcati */
{	/*?sprintf*/
	char str [5];
	str[0] = c;
	if ((integer/100)>=1){
		str[1] = '0' + (integer/100);
		str[2] = '0' + ((integer%100)/10);
	    str[3] = '0' + (integer%10);
	    str[4] = '\0';
	}
	else{
		if ((integer/10)>=1){
			str[1] = '0' + (integer/10);
	    	str[2] = '0' + (integer%10);
	    	str[3] = '\0';
		}
		else {
			str[1] = '0' + integer;
			str[2] = '\0';
		}
	}
	strcat(the_query, str);
}

void addAND (char * the_query, int nbAnd) {
if (nbAnd == 0)
	strcat (the_query, " WHERE ");
else
	strcat (the_query, " AND ");
}
