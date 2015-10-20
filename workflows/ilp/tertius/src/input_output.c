
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <math.h>
#include "types.h"
/* to declare the_max_lit and the_max_var */
#include "gestion.h"
#include "search.h"
#include "bayes.h"
#include "eval.h"
#include "input_output.h"

int the_nb_predicates;
tpredicate *the_predicate_table;
int the_nb_types;
ttype *the_type_table;
int the_nb_parts;
int the_num_discretised_types;
tdiscretised_type *the_discretised_types_table;
FILE *the_input_file;

/********************************************************************************
initialise first partition of facts
********************************************************************************/
int first_partition()
{

return 0;

}

/********************************************************************************
get next  partition of facts
********************************************************************************/
int next_partition(int a_part)
{

if (incremental_loading == TRUE) {
  free_instances();
  free_non_global_types();  
  return 0;
}
else
  return(a_part + 1);

}

/********************************************************************************
test whether it was the last partition of facts
********************************************************************************/
int last_partition(int a_part)
{

if (incremental_loading == TRUE) {
  if (feof(the_input_file)) {
    rewind(the_input_file);
    return 1;
  }
  else {
    read_facts(the_input_file,STORE);
    return 0;
  }
}
else
  return (a_part>=the_nb_parts);
}

/********************************************************************************
Read a list of predicate definitions, one per line, of the form
<name> <arity> <type1> ... <type_arity>
********************************************************************************/

int read_predicates(FILE *a_predicates_file)
{
int the_error = 0;
char *the_name_p;
int the_arity;
int the_kind = PROPERTY;
int *the_types;
int i,j;
int not_found;
int the_global_type;
int the_num_param,the_num_arg;
char the_char;
int the_num_occurence;

the_nb_predicates = 0;
the_nb_types = 0;
the_type_table = Calloc(NBTYPES,ttype)
if (the_type_table == NULL) return 2;
the_predicate_table = Calloc(NBPREDS,tpredicate)
if (the_predicate_table == NULL) return 2;
while((the_error==0)&&(! feof(a_predicates_file))) {
  the_name_p = Calloc(SIZEPRED,char)
  if (the_name_p == NULL) the_error = 2; else/*the_name_p*/{
    fscanf(a_predicates_file, "%[ ]", the_name_p);
    if (fscanf(a_predicates_file, "%[%]", the_name_p) == 1) {
      Free_calloc(SIZEPRED,the_name_p)
      the_char = getc(a_predicates_file);
      while (the_char != '\n')
	the_char = getc(a_predicates_file);
    }
    else if (fscanf(a_predicates_file, "%[-]", the_name_p) == 1) {
      if (fscanf(a_predicates_file, "%s", the_name_p) != 1) the_error = 3; else {
	if (strcmp(the_name_p,"INDIVIDUAL") == 0) {
	  individual_based = TRUE;
	  the_kind = INDIVIDUAL;
	}
	else if (strcmp(the_name_p,"STRUCTURAL") == 0) {
	  structure_based = TRUE;
	  the_kind = STRUCTURAL;
	}
	else if (strcmp(the_name_p,"PROPERTIES") == 0) {
	  the_kind = PROPERTY;
	}
	else the_error = 3;
	fscanf(a_predicates_file, "\n");
      }
    }
    else if (fscanf(a_predicates_file, "%s %d", the_name_p, &the_arity) != 2) the_error = 3; else
      if ((the_kind == STRUCTURAL)&&(the_arity != 2))
	the_error = 28;
	else {
	the_nb_predicates++;
	if (the_nb_predicates > NBPREDS) the_error = 4; else {
	  the_predicate_table[the_nb_predicates-1].name = the_name_p;
	  the_predicate_table[the_nb_predicates-1].arity = the_arity;
	  the_predicate_table[the_nb_predicates-1].kind = the_kind;
	  the_predicate_table[the_nb_predicates-1].num_subpredicate = 0;
	  the_predicate_table[the_nb_predicates-1].subpredicates = NULL;
	  the_predicate_table[the_nb_predicates-1].num_param = 0;
	  the_predicate_table[the_nb_predicates-1].paramIndices = Calloc(the_arity,int)
	  if (the_predicate_table[the_nb_predicates-1].paramIndices == NULL) return 2;
	  the_predicate_table[the_nb_predicates-1].varIndices = Calloc(the_arity,int)
	  if (the_predicate_table[the_nb_predicates-1].varIndices == NULL) return 2;
	  the_predicate_table[the_nb_predicates-1].params = Calloc(the_arity,int)
	  if (the_predicate_table[the_nb_predicates-1].params == NULL) the_error = 2; else {
	    the_predicate_table[the_nb_predicates-1].dimensions = Calloc(the_arity,int)
	    if (the_predicate_table[the_nb_predicates-1].dimensions == NULL) the_error = 2; else {
	      the_predicate_table[the_nb_predicates-1].types = Calloc(the_arity,int)
	      if (the_predicate_table[the_nb_predicates-1].types == NULL) the_error = 2; else {
		for (i=0; i<the_arity; i++) {
		  the_global_type = FALSE;
		  the_name_p = Calloc(SIZETYPE,char)
		  if (the_name_p == NULL) the_error = 2; else {
		    the_predicate_table[the_nb_predicates-1].params[i] = FALSE;
		    fscanf(a_predicates_file, "%[ ]", the_name_p);
		    the_predicate_table[the_nb_predicates-1].dimensions[i] = the_max_lit;
		    if (the_kind == STRUCTURAL) {
		      if (fscanf(a_predicates_file, "%[*]:", the_name_p) == 1)
			the_predicate_table[the_nb_predicates-1].dimensions[i] = the_max_lit;
		      else
			fscanf(a_predicates_file, "%d:", &(the_predicate_table[the_nb_predicates-1].dimensions[i]));
		    }
		    if (fscanf(a_predicates_file, "#%s", the_name_p) == 1) {
		      the_global_type = TRUE;
		      the_predicate_table[the_nb_predicates-1].params[i] = TRUE;
		      the_predicate_table[the_nb_predicates-1]
			.paramIndices[the_predicate_table[the_nb_predicates-1].num_param] = i;
		      the_predicate_table[the_nb_predicates-1].num_param += 1;
		    }
		    else if (fscanf(a_predicates_file, "?%s", the_name_p) == 1) {
		      the_global_type = TRUE;
		    }
		    else if (fscanf(a_predicates_file, "%s", the_name_p) == 1)
		      the_predicate_table[the_nb_predicates-1]
			.varIndices[i-the_predicate_table[the_nb_predicates-1].num_param] = i;
		    else
		      the_error = 3;
		    if (the_error == 0) {
		  not_found = 1;
		  for (j=0; (j<the_nb_types) && not_found; j++)
		    if (strcmp(the_name_p,the_type_table[j].name) == 0)
		      not_found = 0;
		  if (not_found) {
		    the_nb_types++;
		    if (the_nb_types > NBTYPES) the_error = 5; else {
		      the_predicate_table[the_nb_predicates-1].types[i] = the_nb_types-1;
		      the_type_table[the_nb_types-1].name = the_name_p;
		      the_type_table[the_nb_types-1].global = the_global_type;
		      the_type_table[the_nb_types-1].constants = NULL;
		      the_type_table[the_nb_types-1].nb_const = NULL;
#if PROLOG
		      the_type_table[the_nb_types-1].term_ref = NULL;
#endif
		      the_type_table[the_nb_types-1].num_contexts = 0;
		      the_type_table[the_nb_types-1].contexts = NULL;
		      the_type_table[the_nb_types-1].num_individuals = 0;
		    }
		  }
		  else
		    if (the_global_type != the_type_table[j-1].global)
		      the_error = 21;
		    else {
		      the_predicate_table[the_nb_predicates-1].types[i] = j-1;
		      Free_calloc(SIZETYPE,the_name_p)
		    }
		    }
		  }
		}
		the_name_p = Calloc(SIZETYPE,char)
		if (the_name_p == NULL) the_error = 2; else {
		  if (fscanf(a_predicates_file, "%d", &the_num_occurence) == 1)
		    the_predicate_table[the_nb_predicates-1].num_occurence = the_num_occurence;
		  else the_predicate_table[the_nb_predicates-1].num_occurence = 1;
		  if (fscanf(a_predicates_file, "* %s", the_name_p) == 1)
		    the_predicate_table[the_nb_predicates-1].num_occurence = the_max_lit;
		  else if (fscanf(a_predicates_file, "%s", the_name_p) != 1) the_error = 3;
		  if (the_error == 0) {
		    if (strcmp(the_name_p,"explicit") == 0)
		      the_predicate_table[the_nb_predicates-1].cwa = FALSE;
		    else {
		      if (strcmp(the_name_p,"cwa") == 0)
			the_predicate_table[the_nb_predicates-1].cwa = TRUE;
		      else
			the_error = 13;
		    }
		  }
		  if (the_kind == STRUCTURAL) {
		    fscanf(a_predicates_file, "%s", the_name_p);
		    if (strcmp(the_name_p,"li") == 0)
		      the_predicate_table[the_nb_predicates-1].structural_kind = LI;
		    else if (strcmp(the_name_p,"bv") == 0)
		      the_predicate_table[the_nb_predicates-1].structural_kind = BV;
		    else if (strcmp(the_name_p,"ms") == 0)
		      the_predicate_table[the_nb_predicates-1].structural_kind = MS;
		    else if (strcmp(the_name_p,"ss") == 0)
		      the_predicate_table[the_nb_predicates-1].structural_kind = SS;
		    else
		      return 34;
		  }
		  Free_calloc(SIZETYPE,the_name_p)
		}
		fscanf(a_predicates_file, "\n");
		the_predicate_table[the_nb_predicates-1].arity = the_predicate_table[the_nb_predicates-1].arity
		  - the_predicate_table[the_nb_predicates-1].num_param;
		the_types = Calloc(the_arity,int)
		if (the_types == NULL) the_error = 2; else {
		  the_num_arg = 0; the_num_param = 0;
		  while (the_num_arg+the_num_param < the_arity)
		    if (the_predicate_table[the_nb_predicates-1].params[the_num_arg+the_num_param] == TRUE) {
		      the_types[the_num_param] =
			the_predicate_table[the_nb_predicates-1].types[the_num_arg+the_num_param];
		      the_num_param += 1;
		    }
		    else {
		      the_types[the_predicate_table[the_nb_predicates-1].num_param+the_num_arg] =
			the_predicate_table[the_nb_predicates-1].types[the_num_arg+the_num_param];
		      the_num_arg += 1;
		    }
		  Free_calloc(the_arity,the_predicate_table[the_nb_predicates-1].types)
		  the_predicate_table[the_nb_predicates-1].types = the_types;
		}
	      }
	    }
	  }
	}
      }
  }
}

return(the_error);
}


/********************************************************************************
********************************************************************************/
char *get_constant_name(int a_predicate,int a_sub_predicate,int a_part,tinstance an_instance,
			int *a_num_param,int *a_num_arg)
{
char *the_name;

if (the_predicate_table[a_predicate].params[*a_num_param + *a_num_arg] == TRUE) {
  the_name = the_type_table[the_predicate_table[a_predicate].types[*a_num_param]].constants[0]
    [the_predicate_table[a_predicate].subpredicates[a_sub_predicate]->val_params[*a_num_param]];
  (*a_num_param) += 1;
}
else {
  if (the_type_table[the_predicate_table[a_predicate]
		    .types[the_predicate_table[a_predicate].num_param + *a_num_arg]].global == TRUE)
    the_name = the_type_table[the_predicate_table[a_predicate]
			     .types[the_predicate_table[a_predicate].num_param + *a_num_arg]]
      .constants[0][an_instance[*a_num_arg]];
  else
    the_name = the_type_table[the_predicate_table[a_predicate]
			     .types[the_predicate_table[a_predicate].num_param + *a_num_arg]]
      .constants[a_part][an_instance[*a_num_arg]];
  (*a_num_arg) += 1;
}

return the_name;
}

/********************************************************************************
********************************************************************************/
int write_instances(FILE *an_output_file,int a_predicate,int a_sub_predicate,int a_part,tinstance *instances,int a_num_inst, char a_separator, int a_sign)
{
int the_error = 0;
int the_num_param,the_num_arg,j;

for (j=0; j<a_num_inst; j++) {
  if ((a_separator == ',')&&(a_sign == NEG))
    fprintf(an_output_file, "- ");
  if (a_separator == ',')
    fprintf(an_output_file, "%s(",the_predicate_table[a_predicate].name);
  else
    fprintf(an_output_file, "\t");
  the_num_param = 0; the_num_arg = 0;
  while (the_num_param + the_num_arg
	 < the_predicate_table[a_predicate].arity + the_predicate_table[a_predicate].num_param) {
    fprintf(an_output_file, "%s", get_constant_name(a_predicate,a_sub_predicate,a_part,instances[j],
						    &the_num_param,&the_num_arg));
    if (the_num_param + the_num_arg
	< the_predicate_table[a_predicate].arity + the_predicate_table[a_predicate].num_param)
      fprintf(an_output_file, "%c", a_separator);
  }
  if (a_separator == ',')
    fprintf(an_output_file, ").");
  fprintf(an_output_file, "\n");
}

return the_error;
}

				      
/********************************************************************************
Write the content of the predicate table
********************************************************************************/
int write_predicates(FILE *an_output_file)
{
int the_error = 0;
int i,k,p;
int the_num_param,the_num_arg;

for (i=0; i<the_nb_predicates; i++)
  {
    fprintf(an_output_file, "%s %d", the_predicate_table[i].name,
	    the_predicate_table[i].arity+the_predicate_table[i].num_param);
    the_num_param = 0; the_num_arg = 0;
    while (the_num_param + the_num_arg < the_predicate_table[i].arity + the_predicate_table[i].num_param)
      if (the_predicate_table[i].params[the_num_param + the_num_arg] == TRUE)
	fprintf(an_output_file, " %s", the_type_table[the_predicate_table[i].types[the_num_param++]].name);
      else {
	fprintf(an_output_file, " %s",
		the_type_table[the_predicate_table[i].types[the_predicate_table[i].num_param + the_num_arg]].name);
	the_num_arg += 1;
      }
    if (the_predicate_table[i].cwa == TRUE)
      fprintf(an_output_file, " cwa");
    else
      fprintf(an_output_file, " explicit");
    switch (the_predicate_table[i].kind) {
    case INDIVIDUAL: fprintf(an_output_file, " INDIVIDUAL"); break;
    case STRUCTURAL: fprintf(an_output_file, " STRUCTURAL"); break;
    case PROPERTY: fprintf(an_output_file, " PROPERTY"); break;
    }
    fprintf(an_output_file, "\n");
    for (k=0;k<the_predicate_table[i].num_subpredicate;k++)
      for (p=0; p<the_nb_parts; p++){
	fprintf(an_output_file, "Instances:\n");
	the_error = write_instances(an_output_file,i,k,p,the_predicate_table[i].subpredicates[k]->instances[p],
				    the_predicate_table[i].subpredicates[k]->nb_inst[p],' ',POS);
	fprintf(an_output_file, "Counter-instances:\n");
	the_error = write_instances(an_output_file,i,k,p,the_predicate_table[i].subpredicates[k]->cinstances[p],
				    the_predicate_table[i].subpredicates[k]->nb_cinst[p],' ',NEG);
      }
  }
return(the_error);
}

/********************************************************************************
Write the facts
********************************************************************************/
int write_facts(FILE *an_output_file)
{
int the_error = 0;
int i,k,p;

for (p=0; p<the_nb_parts; p++) {
  fprintf(an_output_file, "%c!\n",'%');
  for (i=0; i<the_nb_predicates; i++) {
    for (k=0;k<the_predicate_table[i].num_subpredicate;k++) {
      the_error = write_instances(an_output_file,i,k,p,the_predicate_table[i].subpredicates[k]->instances[p],
				  the_predicate_table[i].subpredicates[k]->nb_inst[p],',',POS);
      the_error = write_instances(an_output_file,i,k,p,the_predicate_table[i].subpredicates[k]->cinstances[p],
				  the_predicate_table[i].subpredicates[k]->nb_cinst[p],',',NEG);
    }
  }
}
return(the_error);
}

/********************************************************************************
Write the content of the type table
********************************************************************************/
int write_types(FILE *an_output_file)
{
int the_error = 0;
int i,j,p;

for (i=0; i<the_nb_types; i++)
  {
    fprintf(an_output_file, "%s", the_type_table[i].name);
    fprintf(an_output_file, "\n");
    for (p=0; p<the_nb_parts; p++)
      if ((the_type_table[i].global == FALSE) || (p == 0))
	{
	  fprintf(an_output_file, "\t");
	  for (j=0; j<the_type_table[i].nb_const[p]; j++)
	    fprintf(an_output_file, "%s ", the_type_table[i].constants[p][j]);
	  fprintf(an_output_file, "\n");
	}
    fprintf(an_output_file, "\n");
  }
return(the_error);
}


/********************************************************************************
Store a constant in the given type
********************************************************************************/
int fact_name_store(int a_type_nb, int a_part, char *a_name, int status, int *a_const_nb)
{
int i=0;
int j;
int not_found;
int the_error = 0;
char *the_name;
char **the_constants = NULL;
tdiscretised_type *the_dt;
#if PROLOG
SP_term_ref *the_term_ref;
#endif

if ((status == COLLECT_STATS) && (the_type_table[a_type_nb].discretised != -1)) {
  the_dt = &(the_discretised_types_table[the_type_table[a_type_nb].discretised]);
  if (the_dt->kind == SDM) {
    the_dt->num_values += 1;
    the_dt->sum +=  atof(a_name);
    the_dt->sum2 +=  atof(a_name) * atof(a_name);
  }
  else {
    the_dt->values[the_dt->num_values] = atof(a_name);
    the_dt->num_values += 1;
    if (the_dt->num_values >= the_dt->maxnumvalues) {
      the_error = my_double_recalloc(&(the_dt->values), the_dt->maxnumvalues, the_dt->maxnumvalues + 1000);
      the_dt->maxnumvalues += 1000;
      if ((the_error == 0) && (the_dt->kind == FFD)) {
	the_error = my_double_recalloc(&(the_dt->thresholds), the_dt->maxnuminter, (long)ceil(the_dt->maxnumvalues / the_dt->size));
	the_dt->maxnuminter = (long)ceil(the_dt->maxnumvalues / the_dt->size);
      }
    }
  }
  return the_error;
}
the_name = Calloc(SIZECONST,char)
if (the_name == NULL)
  return 2;
if (the_type_table[a_type_nb].discretised != -1)
  the_name = discrete_value(the_type_table[a_type_nb].discretised,atof(a_name),the_name);
else
  strcpy(the_name,a_name);
not_found = 1;
for (i=0; (i<the_type_table[a_type_nb].nb_const[a_part]) && not_found; i++)
  if (strcmp(the_name,the_type_table[a_type_nb].constants[a_part][i]) == 0)
    not_found = 0;
if (not_found) {
  the_type_table[a_type_nb].nb_const[a_part] += 1;
  the_constants = Calloc(the_type_table[a_type_nb].nb_const[a_part],char *)
  if (the_constants == NULL) the_error = 2; else {
#if PROLOG
    the_term_ref = Calloc(the_type_table[a_type_nb].nb_const[a_part], SP_term_ref)
      if (the_term_ref == NULL) the_error = 2; else
#endif
	{
	  for (j=0;j<the_type_table[a_type_nb].nb_const[a_part]-1;j++) {
	    the_constants[j] = the_type_table[a_type_nb].constants[a_part][j];
#if PROLOG
	    the_term_ref[j] = the_type_table[a_type_nb].term_ref[a_part][j];
#endif
	  }
	  Free_calloc(the_type_table[a_type_nb].nb_const[a_part]-1,the_type_table[a_type_nb].constants[a_part])
	  the_type_table[a_type_nb].constants[a_part] = the_constants;
#if PROLOG
	  Free_calloc(the_type_table[a_type_nb].nb_const[a_part]-1,the_type_table[a_type_nb].term_ref[a_part])
	  the_type_table[a_type_nb].term_ref[a_part] = the_term_ref;
#endif
	}
  }
  the_type_table[a_type_nb].constants[a_part][the_type_table[a_type_nb].nb_const[a_part]-1] = the_name;
  *a_const_nb = the_type_table[a_type_nb].nb_const[a_part]-1;
}
else {
  Free_calloc(SIZECONST,the_name)
  *a_const_nb = i-1;
}

return(the_error);
}


/********************************************************************************
 Add a new partition to the data
********************************************************************************/
int add_a_partition()
{
int the_error = 0;
char ***the_constants;
int *the_nb_const;
#if PROLOG
SP_term_ref **the_term_ref;
#endif
tinstance **the_instances;
int *the_nb_inst;
tinstance **the_cinstances;
int *the_nb_cinst;
int i,the_type,the_predicate,the_subpredicate;

for (the_type=0;(the_type<the_nb_types)&&(the_error==0);the_type++)
  if (the_type_table[the_type].global == FALSE) {
    the_constants = Calloc(the_nb_parts,char**)
    if (the_constants == NULL) the_error = 2; else {
      the_nb_const = Calloc(the_nb_parts,int)
      if (the_nb_const == NULL) the_error = 2; else {
#if PROLOG
	the_term_ref = Calloc(the_nb_parts,SP_term_ref*)
	if (the_term_ref == NULL) the_error = 2; else
#endif
	  {
	    for (i=0;i<the_nb_parts-1;i++) {
	      the_constants[i] = the_type_table[the_type].constants[i];
	      the_nb_const[i] = the_type_table[the_type].nb_const[i];
#if PROLOG
	      the_term_ref[i] = the_type_table[the_type].term_ref[i];
#endif
	    }
	    Free_calloc(the_nb_parts-1,the_type_table[the_type].constants)
	    Free_calloc(the_nb_parts-1,the_type_table[the_type].nb_const)
	    the_type_table[the_type].constants = the_constants;
	    the_type_table[the_type].nb_const = the_nb_const;
#if PROLOG
	    Free_calloc(the_nb_parts-1,the_type_table[the_type].term_ref)
	    the_type_table[the_type].term_ref = the_term_ref;
#endif
	  }
      }
    }
  }

for (the_predicate=0;(the_predicate<the_nb_predicates)&&(the_error==0);the_predicate++)
  for (the_subpredicate=0;(the_subpredicate<the_predicate_table[the_predicate].num_subpredicate)&&(the_error==0);
       the_subpredicate++) {
    the_instances = Calloc(the_nb_parts,tinstance*)
    if (the_instances == NULL) the_error = 2; else {
      the_nb_inst = Calloc(the_nb_parts,int)
      if (the_nb_inst == NULL) the_error = 2; else {
	the_cinstances = Calloc(the_nb_parts,tinstance*)
	if (the_cinstances == NULL) the_error = 2; else {
	  the_nb_cinst = Calloc(the_nb_parts,int)
	  if (the_nb_cinst == NULL) the_error = 2; else {
	    for (i=0;i<the_nb_parts-1;i++) {
	      the_instances[i] = the_predicate_table[the_predicate].subpredicates[the_subpredicate]->instances[i];
	      the_nb_inst[i] = the_predicate_table[the_predicate].subpredicates[the_subpredicate]->nb_inst[i];
	      the_cinstances[i] = the_predicate_table[the_predicate].subpredicates[the_subpredicate]->cinstances[i];
	      the_nb_cinst[i] = the_predicate_table[the_predicate].subpredicates[the_subpredicate]->nb_cinst[i];
	    }
	    Free_calloc(the_nb_parts-1,the_predicate_table[the_predicate].subpredicates[the_subpredicate]->instances)
	    Free_calloc(the_nb_parts-1,the_predicate_table[the_predicate].subpredicates[the_subpredicate]->nb_inst)
	    Free_calloc(the_nb_parts-1,the_predicate_table[the_predicate].subpredicates[the_subpredicate]->cinstances)
	    Free_calloc(the_nb_parts-1,the_predicate_table[the_predicate].subpredicates[the_subpredicate]->nb_cinst)
	    the_predicate_table[the_predicate].subpredicates[the_subpredicate]->instances = the_instances;
	    the_predicate_table[the_predicate].subpredicates[the_subpredicate]->nb_inst = the_nb_inst;
	    the_predicate_table[the_predicate].subpredicates[the_subpredicate]->cinstances = the_cinstances;
	    the_predicate_table[the_predicate].subpredicates[the_subpredicate]->nb_cinst = the_nb_cinst;
	  }
	}
      }
    }
  }

return the_error;
}

/********************************************************************************
********************************************************************************/
int meta_fact_name_store(tpredicate *a_predicate,int *a_num_param,int *a_num_arg,int a_nb_parts,char *a_name_p,
			 int status,int *an_instance)
{
int the_error = 0;

if (a_predicate->params[*a_num_param+*a_num_arg] == TRUE) {
  the_error = fact_name_store(a_predicate->types[*a_num_param],0,a_name_p,status,&(an_instance[*a_num_param]));
  (*a_num_param) += 1;
}
else {
  if (the_type_table[a_predicate->types[a_predicate->num_param+*a_num_arg]].global == TRUE)
    the_error = fact_name_store(a_predicate->types[a_predicate->num_param+*a_num_arg],0,a_name_p,status,
				&(an_instance[a_predicate->num_param+*a_num_arg]));
  else
    if (status == STORE)
      the_error = fact_name_store(a_predicate->types[a_predicate->num_param+*a_num_arg],a_nb_parts-1,a_name_p,status,
				  &(an_instance[a_predicate->num_param+*a_num_arg]));
  (*a_num_arg) += 1;
}

return the_error;
}

/********************************************************************************
********************************************************************************/
int add_instance(tinstance **instances,int *a_num_inst,tpredicate *a_predicate,int *an_instance)
{
int the_error = 0;
tinstance *the_instances;
int j;

*a_num_inst += 1;
the_instances = Calloc(*a_num_inst,tinstance)
if (the_instances == NULL) the_error = 2; else {
  for (j=0;j<*a_num_inst-1;j++)
    the_instances[j] = (*instances)[j];
  Free_calloc(*a_num_inst-1,*instances)
  *instances = the_instances;
  (*instances)[*a_num_inst-1] = Calloc(a_predicate->arity,int)
  if ((*instances)[*a_num_inst-1] == NULL) the_error = 2; else
    for (j=0;j<a_predicate->arity;j++)
      (*instances)[*a_num_inst-1][j] = an_instance[a_predicate->num_param+j];
}

return the_error;
}

/********************************************************************************
********************************************************************************/
int create_subpredicate(tsubpredicate **a_subpredicate,tpredicate *a_predicate,int a_nb_parts)
{
int the_error = 0;
int i,the_num_contexts;
tsubpredicate *the_subpredicate;

the_subpredicate = Malloc(tsubpredicate)
if (the_subpredicate == NULL) the_error = 2; else {
  the_subpredicate->instances = Calloc(a_nb_parts,tinstance*)
  if (the_subpredicate->instances == NULL) the_error = 2; else{
    the_subpredicate->nb_inst = Calloc(a_nb_parts,int)
    if (the_subpredicate->nb_inst == NULL) the_error = 2; else{
      the_subpredicate->cinstances = Calloc(a_nb_parts,tinstance*)
      if (the_subpredicate->cinstances == NULL) the_error = 2; else{
	the_subpredicate->nb_cinst = Calloc(a_nb_parts,int)
	if (the_subpredicate->nb_cinst == NULL) the_error = 2; else {
	  for (i=0;i<a_nb_parts;i++) {
	    the_subpredicate->instances[i] = NULL;
	    the_subpredicate->nb_inst[i] = 0;
	    the_subpredicate->cinstances[i] = NULL;
	    the_subpredicate->nb_cinst[i] = 0;
	  }
	  the_num_contexts = the_type_table[a_predicate->types[0]].num_contexts;
	  the_subpredicate->probabilities = Calloc(the_num_contexts,double*)
	  if (the_subpredicate == NULL) return 2;
	  the_subpredicate->already_counted = Calloc(the_num_contexts,int)
	  if (the_subpredicate == NULL) return 2;
	  for (i=0;i<the_num_contexts;i++)
	    the_subpredicate->probabilities[i] = NULL; /* for 1BC2, in particular when a subpredicate occurs in the test set but not in the training set! */
	  the_subpredicate->val_params = Calloc(a_predicate->num_param,int)
	  if (the_subpredicate->val_params == NULL)
	    the_error = 2;	      }
      }
    }
  }
}

*a_subpredicate = the_subpredicate;
return the_error;
}

/********************************************************************************
********************************************************************************/
int store_instance(int *an_instance,int a_sign,tpredicate *a_predicate,int a_nb_parts,int a_part,int status)
{
int the_error = 0;
int not_found;
int i,j;
tsubpredicate **the_subpredicates;
int the_first,the_last;

the_first = 0;
the_last = a_predicate->num_subpredicate;
not_found = 1;
for (i=the_first;(i<the_last)&&(not_found==1);i++) {
  for (j=0; (j<a_predicate->num_param)&&(not_found==1); j++)
    not_found = not_found && (an_instance[j] == a_predicate->subpredicates[i]->val_params[j]);
  if (not_found == 1)
    not_found = 0;
  else
    not_found = 1;
}
if (not_found == 1) /* new set of parameters */{
  a_predicate->num_subpredicate += 1;
  the_subpredicates = Calloc(a_predicate->num_subpredicate,tsubpredicate*)
    if (the_subpredicates == NULL) the_error = 2; else {
      for (i=0;i<a_predicate->num_subpredicate-1;i++)
	the_subpredicates[i] = a_predicate->subpredicates[i];
      Free_calloc(a_predicate->num_subpredicate-1,a_predicate->subpredicates)
      a_predicate->subpredicates = the_subpredicates;
      the_error = create_subpredicate(&(a_predicate->subpredicates[a_predicate->num_subpredicate-1]),
				      a_predicate,a_nb_parts);
      if (the_error == 0) {
	for (i=0;i<a_predicate->num_param;i++)
	  a_predicate->subpredicates[a_predicate->num_subpredicate-1]->val_params[i] = an_instance[i];
	a_predicate->subpredicates[a_predicate->num_subpredicate-1]->nb_inst[a_part] = 0;
	a_predicate->subpredicates[a_predicate->num_subpredicate-1]->nb_cinst[a_part] = 0;
	/* store the instance */
	if (status == STORE) {
	  if (a_sign == POS)
	    the_error = add_instance(&(a_predicate->subpredicates[a_predicate->num_subpredicate-1]
				       ->instances[a_part]),
				     &(a_predicate->subpredicates[a_predicate->num_subpredicate-1]
				       ->nb_inst[a_part]),a_predicate,an_instance);
	  else
	    the_error = add_instance(&(a_predicate->subpredicates[a_predicate->num_subpredicate-1]
				       ->cinstances[a_part]),
				     &(a_predicate->subpredicates[a_predicate->num_subpredicate-1]
				       ->nb_cinst[a_part]),a_predicate,an_instance);
	}
      }
    }
}
else /* existing sub_predicate: i-1 */
  if (status == STORE) {
    if (a_sign == POS)
      the_error = add_instance(&(a_predicate->subpredicates[i-1]->instances[a_part]),
			       &(a_predicate->subpredicates[i-1]->nb_inst[a_part]),a_predicate,an_instance);
    else
      the_error = add_instance(&(a_predicate->subpredicates[i-1]->cinstances[a_part]),
			       &(a_predicate->subpredicates[i-1]->nb_cinst[a_part]),a_predicate,an_instance);
  }
return the_error;
}

/********************************************************************************
********************************************************************************/
int initialise_global_types()
{
int the_error = 0;
int the_type;

for (the_type=0;(the_type<the_nb_types) && (the_error == 0);the_type++)
  if (the_type_table[the_type].global == TRUE) {
    the_type_table[the_type].constants = Calloc(1,char**)
    if (the_type_table[the_type].constants == NULL) the_error = 2;
    else {
      the_type_table[the_type].constants[0] = NULL;
      the_type_table[the_type].nb_const = Calloc(1,int)
      if (the_type_table[the_type].nb_const == NULL) {
	the_error = 2;
	Free_calloc(1,the_type_table[the_type].constants)
      }
      else {
	the_type_table[the_type].nb_const[0] = 0;
      }
    }
  }

return the_error;
}

/********************************************************************************
********************************************************************************/
int add_a_part()
{
int the_error = 0;
int i,j;

the_nb_parts++;
the_error = add_a_partition();
for (i=0;(i<the_nb_predicates) && (the_error == 0);i++)
  for (j=0; (j<the_predicate_table[i].num_subpredicate)&&(the_error==0);j++) {
    the_predicate_table[i].subpredicates[j]->nb_inst[the_nb_parts-1] = 0;
    the_predicate_table[i].subpredicates[j]->nb_cinst[the_nb_parts-1] = 0;
    the_predicate_table[i].subpredicates[j]->instances[the_nb_parts-1] = NULL;
    the_predicate_table[i].subpredicates[j]->cinstances[the_nb_parts-1] = NULL;
  }
for (i=0;(i<the_nb_types) && (the_error == 0);i++)
  if (the_type_table[i].global == FALSE) {
    the_type_table[i].nb_const[the_nb_parts-1] = 0;
    the_type_table[i].constants[the_nb_parts-1] = NULL;
#if PROLOG
    the_type_table[i].term_ref[the_nb_parts-1] = NULL;
#endif
  }

return the_error;
}

/********************************************************************************
Read a list of ground facts
<name>(<const1>,...,<const_arity>).
if status==STORE then store the facts
if status==PREPARE then store only global constants and sub_predicates
********************************************************************************/
int read_facts(FILE *a_facts_file,int status)
{
int the_error = 0;
char *the_name_p;
int *the_instance;
tpredicate *the_predicate;
int i;
int not_found;
int the_sign = 0;
int the_num_param,the_num_arg;

the_nb_parts = 0;
the_name_p = Calloc(SIZEPRED,char)
if (the_name_p == NULL) the_error = 2; else {
  while ((! feof(a_facts_file)) && (the_error == 0)) {
    if (fscanf(a_facts_file, "%[!% \t]\n", the_name_p) == 1) {
      if ((status != STORE)||(incremental_loading == FALSE)||(the_nb_parts==0))
	the_error = add_a_part();
      else
	break;
    }
    if (fscanf(a_facts_file, "- %[a-zA-Z0-9_-](", the_name_p) == 1)
      the_sign = NEG;
    else if (fscanf(a_facts_file, "%[a-zA-Z0-9_-](", the_name_p) == 1)
      the_sign = POS;
    else
      the_error = 3;
    if (the_error == 0) {
      not_found = 1;
      for (i=0; (i<the_nb_predicates) && not_found; i++)
	if (strcmp(the_name_p,the_predicate_table[i].name) == 0)
	  not_found = 0;
      if (not_found) the_error = 6; else {
	if (the_nb_parts == 0)
	  the_error = add_a_part();
	the_predicate = &(the_predicate_table[i-1]);
	if (the_predicate->num_occurence > 0) {
	  the_instance = Calloc(the_predicate->arity+the_predicate->num_param,int)
	    if (the_instance == NULL) the_error = 2; else {
	      the_num_param = 0; the_num_arg = 0;
	      while ((the_num_param + the_num_arg < the_predicate->arity+the_predicate->num_param-1)&&(the_error==0)) {
		if (fscanf(a_facts_file, "%[a-zA-Z0-9_./+-],", the_name_p) != 1) the_error = 3;
		else the_error = meta_fact_name_store(the_predicate,&the_num_param,&the_num_arg,the_nb_parts,the_name_p,
						      status,the_instance);
	      }
	      if (the_error == 0) {
		if (fscanf(a_facts_file, "%[a-zA-Z0-9_./+-]).", the_name_p) != 1) the_error = 3; else
		  the_error = meta_fact_name_store(the_predicate,&the_num_param,&the_num_arg,the_nb_parts,the_name_p,
						   status,the_instance);
		fscanf(a_facts_file, "\n");
	      }
	      the_error = store_instance(the_instance,the_sign,the_predicate,the_nb_parts,the_nb_parts-1,status);
	      Free_calloc(the_predicate->arity+the_predicate->num_param,the_instance)
	    }
	}
	else {
	  for (the_num_arg=0;(the_num_arg<the_predicate->arity+the_predicate->num_param-1)&&(the_error==0);the_num_arg++)
	    if (fscanf(a_facts_file, "%[a-zA-Z0-9_./+-],", the_name_p) != 1) the_error = 3;
	  if (the_error == 0) {
	    if (fscanf(a_facts_file, "%[a-zA-Z0-9_./+-]).", the_name_p) != 1) the_error = 3;
	    fscanf(a_facts_file, "\n");
	  }
	}
      }
    }
  }
  Free_calloc(SIZEPRED,the_name_p)
}
return(the_error);
}


/********************************************************************************
********************************************************************************/
int store_argument(char *a_name_p,int an_index,tcube *a_cube,tpredicate *a_predicate,char **a_var_table)
{
int the_error = 0;
int not_found;
int j,k;

not_found = 1;
for (j=0; (j<a_cube->nb_arg) && not_found; j++)
  if (strcmp(a_name_p,a_var_table[j]) == 0)
    {
      not_found = 0;
      if (a_cube->args[j].type != a_predicate->types[a_predicate->num_param + an_index])
	the_error = 17;
    }
if ((the_error == 0) && (not_found))
  {
    a_cube->nb_arg += 1;
    if (a_cube->nb_arg > NBARGS)
      the_error = 19;
    else
      {
	a_cube->args[a_cube->nb_arg-1].type = a_predicate->types[a_predicate->num_param + an_index];
	if (the_type_table[a_predicate->types[a_predicate->num_param + an_index]].global == TRUE)
	  {
	    not_found = 1;
	    for (k=0; (k<the_type_table[a_predicate->types[a_predicate->num_param + an_index]].nb_const[0])
		   && not_found; k++)
	      if (strcmp(a_name_p,the_type_table[a_predicate->types[a_predicate->num_param + an_index]].constants[0][k])
		  == 0)
		{
		  not_found = 0;
		  a_cube->args[a_cube->nb_arg-1].flip = CONST;
		  a_cube->args[a_cube->nb_arg-1].value = k;
		}
	  }
	if (not_found == 1)
	  {
	    a_cube->args[a_cube->nb_arg-1].flip = VAR;
	    a_cube->args[a_cube->nb_arg-1].value = a_cube->nb_arg-1;
	  }
	a_cube->lits[a_cube->nb_lit-1].args[an_index] = a_cube->nb_arg-1;
	a_var_table[a_cube->nb_arg-1] = Calloc(SIZEPRED,char)
	if (a_var_table[a_cube->nb_arg-1] == NULL)
	  the_error = 2;
	else
	  strcpy(a_var_table[a_cube->nb_arg-1],a_name_p);
      }
  }
else
  a_cube->lits[a_cube->nb_lit-1].args[an_index] = j-1;

return the_error;
}


/********************************************************************************
Read a clause

********************************************************************************/
int read_clause(FILE *a_clause_file,tcube **a_clause)
{
int the_error = 0;
char *the_name_p;
tcube *the_cube = NULL;
tpredicate *the_predicate;
int i,j;
int not_found;
int the_sign;
int stop;
char **the_var_table = NULL;
tliteral *the_ordered_lits = NULL;
int the_indice;
int *the_params = NULL;
int the_num_param,the_num_arg;

the_name_p = Calloc(SIZEPRED,char)
if (the_name_p == NULL)
  the_error = 2;
else
    {
      the_cube = Malloc(tcube)
      if (the_cube == NULL)
	the_error = 2;
      else
	{
	  the_cube->args = Calloc(NBARGS,targ)
	  if (the_cube->args == NULL)
	    the_error = 2;
	  else
	    {
	      if (individual_based == TRUE)
		the_cube->lits = Calloc(the_max_lit+1,tliteral)
	      else
		the_cube->lits = Calloc(the_max_lit,tliteral)
	      if (the_cube->lits == NULL)
		the_error = 2;
	      else
		{
		  the_cube->nb_lit = 0;
		  the_cube->nb_arg = 0;
		  the_var_table = Calloc(the_max_var,char*)
		  if (the_var_table == NULL)
		    the_error = 2;
		  else
		    {
		      if (individual_based == TRUE)
			the_ordered_lits = Calloc(the_max_lit+1,tliteral)
		      else
			the_ordered_lits = Calloc(the_max_lit,tliteral)
		      if (the_ordered_lits == NULL)
			the_error = 2;
		    }
		}
	    }
	}
    }
the_sign = POS;
stop = FALSE;
while ((! feof(a_clause_file)) && (the_error == 0) && (stop == FALSE))
  {
    if (fscanf(a_clause_file, "%[.]\n", the_name_p) == 1)
      stop = TRUE;
    else
      {
	if (the_sign == POS)
	  {
	    fscanf(a_clause_file, "%[ ]", the_name_p);
	    if ((fscanf(a_clause_file, "%[:]", the_name_p) == 1)
		&& (fscanf(a_clause_file, "%[-]", the_name_p) == 1))
	      the_sign = NEG;
	    else
	      if (the_cube->nb_lit != 0) {
		if (fscanf(a_clause_file, "%[;]", the_name_p) == 1)
		  the_error = 0;
		else
		  the_error = 3;
	      }
	    fscanf(a_clause_file, "%[ ]", the_name_p);
	  }
	else
	  {
	    fscanf(a_clause_file, "%[ ]", the_name_p);
	    if (fscanf(a_clause_file, "%[,]", the_name_p) == 1)
	      the_error = 0;
	    else
	      the_error = 3;
	    fscanf(a_clause_file, "%[ ]", the_name_p);
	  }
	  
	if (fscanf(a_clause_file, "%[a-zA-Z0-9_](", the_name_p) == 1)
	  {
	    the_cube->nb_lit += 1;
	    if (individual_based == TRUE) {
	      if (the_cube->nb_lit > the_max_lit+1)
		the_error = 20;
	    }
	    else
	      if (the_cube->nb_lit > the_max_lit)
		the_error = 20;
	  }
	else
	  the_error = 3;
	if (the_error == 0)
	  {
	    not_found = 1;
	    for (i=0; (i<the_nb_predicates) && not_found; i++)
	      if (strcmp(the_name_p,the_predicate_table[i].name) == 0)
		not_found = 0;
	    if (not_found)
	      the_error = 18;
	    else
	      {
		the_predicate = &(the_predicate_table[i-1]);
		the_cube->lits[the_cube->nb_lit-1].sign = the_sign;
		the_cube->lits[the_cube->nb_lit-1].predicate = i-1;
		the_cube->lits[the_cube->nb_lit-1].args = Calloc(the_predicate->arity,int)
		if (the_cube->lits[the_cube->nb_lit-1].args == NULL)
		  the_error = 2;
		if (the_error == 0){
		  the_params = Calloc(the_predicate->num_param,int)
		  if (the_params == NULL)
		    the_error = 2;
		}
		the_num_param = 0; the_num_arg = 0;
		while ((the_error == 0)
		       && (the_num_param + the_num_arg < the_predicate->num_param + the_predicate->arity-1)) {
		  fscanf(a_clause_file, "%[ ]", the_name_p);
		  if (fscanf(a_clause_file, "%[a-zA-Z0-9_.-],", the_name_p) != 1)
		    the_error = 3;
		  else if (the_predicate->params[the_num_param + the_num_arg] == TRUE) {
		    not_found = 1;
		    for (i=0; (i<the_type_table[the_predicate->types[the_num_param]].nb_const[0]) && not_found; i++)
		      if (strcmp(the_name_p,the_type_table[the_predicate->types[the_num_param]].constants[0][i]) == 0)
			not_found = 0;
		    if (not_found)
		      the_error = 22;
		    else
		      the_params[the_num_param] = i-1;
		    the_num_param += 1;
		  }
		  else
		    the_error = store_argument(the_name_p,the_num_arg++,the_cube,the_predicate,the_var_table);
		}
		if (the_error == 0) {
		  fscanf(a_clause_file, "%[ ]", the_name_p);
		  if (fscanf(a_clause_file, "%[a-zA-Z0-9_.-])", the_name_p) != 1)
		    the_error = 3;
		  else if (the_predicate->params[the_num_param + the_num_arg] == TRUE) {
		    not_found = 1;
		    for (i=0; (i<the_type_table[the_predicate->types[the_num_param]].nb_const[0]) && not_found; i++)
		      if (strcmp(the_name_p,the_type_table[the_predicate->types[the_num_param]].constants[0][i]) == 0)
			not_found = 0;
		    if (not_found)
		      the_error = 22;
		    else
		      the_params[the_num_param] = i-1;
		  }
		  else
		    the_error = store_argument(the_name_p,the_num_arg++,the_cube,the_predicate,the_var_table);
		  not_found = 1;
		  if ((individual_based == TRUE)
		      && (the_predicate_table[the_cube->lits[the_cube->nb_lit-1].predicate].num_subpredicate == 0)) {
		    not_found = 0;
		    i = 1;
		  }
		  else
		    for (i=0; (i<the_predicate_table[the_cube->lits[the_cube->nb_lit-1].predicate].num_subpredicate)
			   && not_found && (the_error == 0); i++){
		      not_found = 0;
		      for (j=0;(j<the_predicate->num_param)&&(not_found == 0);j++)
			if (the_params[j] != the_predicate_table[the_cube->lits[the_cube->nb_lit-1].predicate]
			    .subpredicates[i]->val_params[j])
			  not_found = 1;
		    }
		  if (not_found)
		    the_error = 22;
		  else
		    the_cube->lits[the_cube->nb_lit-1].subpredicate = i-1;
		  Free_calloc(the_predicate->num_param,the_params)
		}
	      }
	  }
      }
  }

if (the_error == 0)
  {
    the_cube->nb_positive = 0;
    for (i=0; i<the_cube->nb_lit; i++)
      {
	if (the_cube->lits[i].sign == POS)
	  the_cube->nb_positive += 1;
	the_indice = 0;
	for (j=0; j<the_cube->nb_lit; j++)
	  if (((j < i) && (the_cube->lits[j].predicate <= the_cube->lits[i].predicate))
	      || ((j > i) && (the_cube->lits[j].predicate < the_cube->lits[i].predicate)))
	    the_indice++;
	the_ordered_lits[the_indice].sign = the_cube->lits[i].sign;
	the_ordered_lits[the_indice].predicate = the_cube->lits[i].predicate;
	the_ordered_lits[the_indice].subpredicate = the_cube->lits[i].subpredicate;
	the_ordered_lits[the_indice].args = the_cube->lits[i].args;
      }
    if (individual_based == TRUE)
      Free_calloc(the_max_lit+1,the_cube->lits)
    else
      Free_calloc(the_max_lit,the_cube->lits)
    the_cube->lits = the_ordered_lits;
    for (j=0; j<the_cube->nb_arg; j++)
      Free_calloc(SIZEPRED,the_var_table[j])
    Free_calloc(the_max_var,the_var_table)
    the_error = initialise_expansed_form(the_cube,NULL);
    if (the_error == 0)
      the_error = rename_variables(the_cube);
    if (the_error == 0)
      the_error = initialise_unified_variables(the_cube);
    if (the_error == 0)
      {
	the_cube->flag_instanciation = 0;
	for (i=0;i<the_cube->nb_lit;i++)
	  for (j=0;j<the_predicate_table[the_cube->lits[i].predicate].arity;j++)
	    if ((the_cube->args[the_cube->lits[i].args[j]].flip == CONST)
		&& (the_predicate_table[the_cube->lits[i].predicate].params[j] == FALSE))
	      if (the_cube->flag_instanciation < the_cube->lits[i].args[j]+1)
		the_cube->flag_instanciation = the_cube->lits[i].args[j] + 1;
      }
  }
*a_clause = the_cube;
return(the_error);
}

/********************************************************************************
Read several clauses
********************************************************************************/
int read_clauses(FILE *a_clause_file,tlist **a_list_clauses)
{
int the_error = 0;
tlist *the_list;
tlist *the_node;
tcube *the_cube;
int the_previous_num_fold;

the_previous_num_fold = the_num_fold;
the_num_fold = 1;
the_list = NULL;
while ((! feof(a_clause_file)) && (the_error == 0)) {
  the_error = read_clause(a_clause_file, &the_cube);
  if (the_error == 0) {
    the_error = eval_cube(the_cube,0,NULL);
    if (the_error == 0) {
      the_node = Malloc(tlist)
      if (the_node == NULL) the_error = 2; else {
	the_node->elt = (void*)the_cube;
	the_node->next = the_list;
	the_list = the_node;
      }
    }
  }
}

the_num_fold = the_previous_num_fold;
*a_list_clauses = the_list;
return(the_error);
}

/********************************************************************************
********************************************************************************/
int read_configuration_file(FILE *a_file,int *an_argc,char ***an_argv)
{
int the_error = 0;
char *the_name_p;
char the_char;
char **the_new_argv;
int i;

*an_argc = 0;
*an_argv = NULL;
while ((! feof(a_file)) && (the_error == 0)) {
  the_name_p = Calloc(SIZEPRED,char)
  if (the_name_p == NULL) the_error = 2; else/*the_name_p*/{
    fscanf(a_file, "%[ ]", the_name_p);
    if (fscanf(a_file, "%[%]", the_name_p) == 1) {
      Free_calloc(SIZEPRED,the_name_p)
      the_char = getc(a_file);
      while (the_char != '\n')
	the_char = getc(a_file);
    }
    else if (fscanf(a_file, "%s", the_name_p) == 1) {
      *an_argc += 1;
      the_new_argv = Calloc(*an_argc,char*)
      if (the_new_argv == NULL) the_error = 2; else {
	for (i=0;i<*an_argc-1;i++)
	  the_new_argv[i] = (*an_argv)[i];
	the_new_argv[*an_argc-1] = the_name_p;
      }
      Free_calloc(*an_argc-1,*an_argv)
      *an_argv = the_new_argv;
    }
  }
}

return the_error;
}
