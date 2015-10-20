
#include <stdio.h>
#include <stdlib.h>
#include "types.h"
#include "input_output.h"
#include "eval.h"
#include "search.h"
#include "bayes.h"
int first_order_features;

/* global variables local to the file */
int new_head = 0;

double the_first_confirmation,the_last_confirmation,the_first_observed,the_last_observed;

int initialise_current_value(FILE *an_output)
{
the_first_confirmation = 0;
the_last_confirmation = 0;
the_first_observed = 0;
the_last_observed = 0;

fprintf(an_output,"Best and worst current values:\n");
fprintf(an_output,"%.6f %.6f   %.6f %.6f",the_last_confirmation,the_last_observed,the_first_confirmation,the_first_observed);

return 0;
}

int display_current_value(FILE *an_output, tlist *a_list)
{
tlist *the_current, *the_next;
double the_new_first_conf,the_new_first_obs,the_new_last_conf,the_new_last_obs;

the_current = a_list;
if (the_current != NULL)
  {
    the_next = the_current->next;
    while (the_next != NULL)
      {
	the_current = the_next;
	the_next = the_current->next;
      }
    the_new_first_conf = ((tcube*)a_list->elt)->confirmation;
    the_new_first_obs = ((tcube*)a_list->elt)->observed;
    the_new_last_conf = ((tcube*)the_current->elt)->confirmation;
    the_new_last_obs = ((tcube*)the_current->elt)->observed;
    if ((the_new_first_conf != the_first_confirmation)
	|| (the_new_first_obs != the_first_observed)
	|| (the_new_last_conf != the_last_confirmation)
	|| (the_new_last_obs != the_last_observed))
      {
	the_first_confirmation = the_new_first_conf;
	the_first_observed = the_new_first_obs;
	the_last_confirmation = the_new_last_conf;
	the_last_observed = the_new_last_obs;
	if (verbose == FALSE)
	  fprintf(an_output, "\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b");
	fprintf(an_output,"%.6f %.6f - %.6f %.6f",the_last_confirmation,the_last_observed,the_first_confirmation,the_first_observed);
      }
  }

return 0;
}

int write_subsets(FILE *an_output,int n, tlist *a_list_subsets)
{
int i;

if (a_list_subsets != NULL)
  {
    for (i=0;i<n;i++)
      fprintf(an_output, "%d ",((int *)(a_list_subsets->elt))[i]);
    fprintf(an_output, "\n");
    write_subsets(an_output,n,a_list_subsets->next);
  }
return 0;
}

void write_literal(FILE *an_output,tcube *a_cube,int a_lit,int a_part)
{
int the_num_arg,the_num_param;

fprintf(an_output, "%s",the_predicate_table[a_cube->lits[a_lit].predicate].name);
if ((a_lit == 0)&&(new_head > 0))
  fprintf(an_output, "%d", new_head);
if (the_predicate_table[a_cube->lits[a_lit].predicate].arity + the_predicate_table[a_cube->lits[a_lit].predicate].num_param> 0){
  fprintf(an_output, "(");
  the_num_arg = 0; the_num_param = 0;
  while (the_num_param + the_num_arg < the_predicate_table[a_cube->lits[a_lit].predicate].arity
	 + the_predicate_table[a_cube->lits[a_lit].predicate].num_param){
    if (the_predicate_table[a_cube->lits[a_lit].predicate].params[the_num_param + the_num_arg] == TRUE){
      fprintf(an_output, "%s", the_type_table[the_predicate_table[a_cube->lits[a_lit].predicate].types[the_num_param]]
	      .constants[0][the_predicate_table[a_cube->lits[a_lit].predicate]
			   .subpredicates[a_cube->lits[a_lit].subpredicate]->val_params[the_num_param]]);
      the_num_param += 1;
    }
    else {
      if (a_cube->args[a_cube->lits[a_lit].args[the_num_arg]].flip == VAR)
	if (unification_form == IMPLICIT)
	  fprintf(an_output, "%c", a_cube->args[a_cube->lits[a_lit].args[the_num_arg]].value + 'A');
	else
	  fprintf(an_output, "%c", a_cube->exp_args[a_cube->exp_lits[a_lit].args[the_num_arg]].value + 'A');
      else
	if (unification_form == IMPLICIT)
	  fprintf(an_output, "%s", the_type_table[a_cube->args[a_cube->lits[a_lit].args[the_num_arg]].type].constants[a_part][a_cube->args[a_cube->lits[a_lit].args[the_num_arg]].value]);
	else
	  fprintf(an_output, "%s", the_type_table[a_cube->exp_args[a_cube->exp_lits[a_lit].args[the_num_arg]].type].constants[a_part][a_cube->exp_args[a_cube->exp_lits[a_lit].args[the_num_arg]].value]);
      the_num_arg += 1;
    }
    if (the_num_param + the_num_arg < the_predicate_table[a_cube->lits[a_lit].predicate].arity
	+ the_predicate_table[a_cube->lits[a_lit].predicate].num_param)
      fprintf(an_output, ",");
  }
  fprintf(an_output, ")");
}
}

int write_clause_values(FILE *an_output,tcube *a_cube,int a_part)
{
int the_error = 0;
 
fprintf(an_output, "/* %f %f ",a_cube->confirmation,a_cube->observed);
    if (verbose == TRUE)
      fprintf(an_output, " %ld nHB:%ld B:%ld nH:%ld N:%ld ",a_cube->num,a_cube->nb_inst,a_cube->nb_inst_positive,a_cube->nb_inst_negative,a_cube->capital_n);
    if (individual_based == TRUE) {
      write_literal(an_output,a_cube,0,a_part);
      fprintf(an_output, " */\t");
    }
    else
      fprintf(an_output, "*/\t");

return the_error;
}

int write_clauses(FILE *an_output,tlist *a_list,int a_part)
{
tlist *a_node;
int i;
tcube *the_cube;
int the_first_lit;

if (individual_based == TRUE)
  the_first_lit = 1;
else
  the_first_lit = 0;
a_node = a_list;
while (a_node != NULL)
  {
    the_cube = (tcube*)(a_node->elt);
    write_clause_values(an_output,the_cube,a_part);
    for (i=the_first_lit;i<the_cube->nb_lit;i++)
      {
	if (the_cube->lits[i].sign == NEG)
	  fprintf(an_output,"- ");
	write_literal(an_output,the_cube,i,a_part);
	if (i<the_cube->nb_lit-1)
	  fprintf(an_output, " v ");
      }
    if (unification_form == EXPLICIT) {
      for (i=0;i<the_cube->nb_substs;i++) {
	if (the_cube->substs[i].flip == VAR)
	  fprintf(an_output,"v %c <> %c", the_cube->substs[i].arg + 'A', the_cube->substs[i].value + 'A');
	else
	  fprintf(an_output,"v %c <> %s", the_cube->substs[i].arg + 'A', the_type_table[the_cube->exp_args[the_cube->substs[i].arg].type].constants[0][the_cube->substs[i].value]);
      }
    }
    fprintf(an_output, "\n");
    a_node = a_node->next;
  }

return 0;
}

int write_clauses_classification(FILE *an_output,tlist *a_list,int a_part)
{
tlist *a_node;
int i;
tcube *the_cube;
int the_first_lit,the_other_lit;
int the_block_sign;

if (individual_based == TRUE)
  the_first_lit = 1;
else
  the_first_lit = 0;
a_node = a_list;
while (a_node != NULL) {
  the_cube = (tcube*)(a_node->elt);
  if ((individual_based == FALSE) || (the_cube->nb_lit > 1)) {
    write_clause_values(an_output,the_cube,a_part);
    the_block_sign = POS;
    for (the_other_lit=the_first_lit+1;(the_block_sign == POS)&&(the_other_lit<the_cube->nb_lit);the_other_lit++)
      if ((the_cube->blocks_table[the_other_lit] == the_cube->blocks_table[the_first_lit]) && (the_cube->lits[the_other_lit].sign == NEG))
	the_block_sign = NEG;
    if (the_block_sign == POS) {
      if (the_cube->lits[the_first_lit].sign == NEG)
	fprintf(an_output,"not ");
      write_literal(an_output,the_cube,the_first_lit,a_part);
      if (unification_form == EXPLICIT) {
	for (i=0;i<the_cube->nb_substs;i++) {
	  if (the_cube->substs[i].flip == VAR)
	    fprintf(an_output,", %c =/= %c", the_cube->substs[i].arg + 'A', the_cube->substs[i].value + 'A');
	  else
	    fprintf(an_output,", %c =/= %s", the_cube->substs[i].arg + 'A', the_type_table[the_cube->exp_args[the_cube->substs[i].arg].type].constants[0][the_cube->substs[i].value]);
	}
      }
      if ((the_cube->nb_lit > the_first_lit+1) || ((unification_form == EXPLICIT)&&(the_cube->nb_substs>0)))
	fprintf(an_output, " :- ");
    }
    else {
      fprintf(an_output, " :- ");
      the_first_lit -= 1;
    }
    for (i=the_first_lit+1;i<the_cube->nb_lit;i++)
      {
	if (the_cube->lits[i].sign == POS)
	  fprintf(an_output,"not ");
	write_literal(an_output,the_cube,i,a_part);
	if (i<the_cube->nb_lit-1)
	  fprintf(an_output, ", ");
      }
    if (the_block_sign == NEG)
      the_first_lit += 1;
    if (unification_form == EXPLICIT)
      for (i=0;i<the_cube->nb_substs;i++) {
	if ((the_cube->nb_lit - 1 > the_first_lit) || (i > 0))
	  fprintf(an_output,", ");
	if (the_cube->substs[i].flip == VAR)
	  fprintf(an_output,"%c = %c", the_cube->substs[i].arg + 'A', the_cube->substs[i].value + 'A');
	else
	  fprintf(an_output,"%c = %s", the_cube->substs[i].arg + 'A', the_type_table[the_cube->exp_args[the_cube->substs[i].arg].type].constants[0][the_cube->substs[i].value]);
      }
    fprintf(an_output, ".\n");
  }
  a_node = a_node->next;
}
return 0;
}


int write_clauses_normal_internal(FILE *an_output, tcube *a_cube, int a_part)
{
int i,j,k;
int the_interest;
int the_beginning;
int the_num_pos,the_num_neg;
int the_first_lit,the_other_lit;
int the_block_sign;

if ((individual_based == TRUE)&&(new_head == 0))
  the_first_lit = 1;
else
  the_first_lit = 0;
if ((individual_based == FALSE) || (a_cube->nb_lit > 1)) {
  /*    for (i=the_first_lit;i<a_cube->nb_lit;i++){
	write_literal(an_output,a_cube,i,a_part);
	fprintf(an_output,"(b %d)",a_cube->blocks_table[i]);
	}*/
  the_num_pos = 0;
  the_num_neg = 0;
  for (k=0;k<2;k++) {
    if (k == 0)
      the_interest = POS;
    else
      the_interest = NEG;
    the_beginning = TRUE;
    if (the_interest == NEG) {
      if (unification_form == EXPLICIT)
	for (j=0;j<a_cube->nb_substs;j++) {
	  if ((the_beginning == TRUE) && (the_num_pos == 0))
	    the_beginning = FALSE;
	  else {
	    the_beginning = FALSE;
	    fprintf(an_output,"; ");
	  }
	  if (a_cube->substs[j].flip == VAR)
	    fprintf(an_output,"%c =/= %c", a_cube->substs[j].arg + 'A',
		    a_cube->substs[j].value + 'A');
	  else
	    fprintf(an_output,"%c =/= %s", a_cube->substs[j].arg + 'A',
		    the_type_table[a_cube->exp_args[a_cube->substs[j].arg].type].constants[0][a_cube->substs[j].value]);
	}
      if ((the_num_neg > 0)
	  || ((unification_form == EXPLICIT) && (a_cube->nb_substs > 0))) {
	fprintf(an_output," :- ");
	the_beginning = TRUE;
      }
    }
    for (i=the_first_lit;i<a_cube->nb_lit;i++) {
      if (a_cube->blocks_table != NULL) {
	the_block_sign = POS;
	for (the_other_lit=the_first_lit;
	     (the_block_sign == POS)&&(the_other_lit<a_cube->nb_lit);
	     the_other_lit++)
	  if ((a_cube->blocks_table[the_other_lit]
	       == a_cube->blocks_table[i])
	      && (a_cube->lits[the_other_lit].sign == NEG))
	    the_block_sign = NEG;
      }
      else
	the_block_sign = a_cube->lits[i].sign;
      if (the_block_sign == the_interest) {
	if (the_beginning == FALSE)
	  if (the_interest == POS)
	    fprintf(an_output,"; ");
	  else
	    fprintf(an_output,", ");
	else
	  the_beginning = FALSE;
	if (a_cube->lits[i].sign != the_interest)
	  fprintf(an_output,"not ");
	write_literal(an_output,a_cube,i,a_part);
      }
      if (the_interest == POS) {
	if (the_block_sign == POS)
	  the_num_pos += 1;
	else
	  the_num_neg += 1;
      }
    }
  }
  if (unification_form == EXPLICIT)
    for (i=0;i<a_cube->nb_substs;i++) {
      if ((the_beginning == TRUE) && (the_num_neg == 0))
	the_beginning = FALSE;
      else {
	the_beginning = FALSE;
	fprintf(an_output,", ");
      }
      if (a_cube->substs[i].flip == VAR)
	fprintf(an_output,"%c = %c", a_cube->substs[i].arg + 'A',
		a_cube->substs[i].value + 'A');
      else
	fprintf(an_output,"%c = %s", a_cube->substs[i].arg + 'A',
		the_type_table[a_cube->exp_args[a_cube->substs[i].arg].type].constants[0][a_cube->substs[i].value]);
    }
  fprintf(an_output, ".\n");
}
return 0;
}

int write_clauses_normal(FILE *an_output,tlist *a_list,int a_part)
{
tlist *a_node;
tcube *the_cube;

a_node = a_list;
while (a_node != NULL) {
  the_cube = (tcube*)(a_node->elt);
  write_clause_values(an_output,the_cube,a_part);
  write_clauses_normal_internal(an_output,the_cube,a_part);
  a_node = a_node->next;
}

return 0;
}

int write_first_order_features(FILE *an_output,tlist *a_list,int a_part)
{
tlist *a_node;
tcube *the_cube;

new_head = 1;
a_node = a_list;
while (a_node != NULL) {
  the_cube = (tcube*)(a_node->elt);
  write_clauses_normal_internal(an_output,the_cube,a_part);
  a_node = a_node->next;
  new_head += 1;
}

return 0;
}

int write_clauses_appropriate(FILE *an_output,tlist *a_list,int a_part)
{

if ((the_search_bias == CLASSIFICATION)
    || (the_search_bias == POSITIVE_CLASSIFICATION)
    || (the_search_bias == HORN_POSITIVE_CLASSIFICATION))
  write_clauses_classification(an_output,a_list,a_part);
else
  if ((the_expected_value == NAIVE) || (the_expected_value == BACKGROUND)
      || (the_expected_value == LINEAR_RESOLUTION)|| (the_expected_value == INTEGRITY_CONSTRAINTS))
    write_clauses(an_output,a_list,a_part);
  else
    if (the_expected_value == NORMAL)
      write_clauses_normal(an_output,a_list,a_part);

return 0;
}

/******************************************************************************
  Write the class of an individual
******************************************************************************/
int write_class(FILE *a_result_file,int *an_individual,tcube *a_cube,int a_part)
{
int the_error = 0;
int *the_arg_values;
int *the_arg_flips;
int the_cube_num_args;
targ *the_cube_args;
int i;
tlist the_node;

if (unification_form == EXPLICIT) {
  the_cube_args = a_cube->exp_args;
  the_cube_num_args = a_cube->nb_exp_args;
}
else {
  the_cube_args = a_cube->args;
  the_cube_num_args = a_cube->nb_arg;
}
the_arg_values = Calloc(the_cube_num_args,int)
if (the_arg_values == 0) the_error = 2; else {
  the_arg_flips = Calloc(the_cube_num_args,int)
  if (the_arg_flips == 0) the_error = 2; else {
    for (i=0;i<the_cube_num_args;i++) {
      the_arg_values[i] = the_cube_args[i].value;
      the_arg_flips[i] = the_cube_args[i].flip;
    }
    instantiate_cube(an_individual,a_cube);
    invert_sign_literals(a_cube);
    the_node.elt = a_cube;
    the_node.next = NULL;
    write_clauses_appropriate(a_result_file,&the_node,a_part);
    invert_sign_literals(a_cube);
    for (i=0;i<the_cube_num_args;i++) {
      the_cube_args[i].value = the_arg_values[i];
      the_cube_args[i].flip = the_arg_flips[i];
    }
    Free_calloc(the_cube_num_args,the_arg_flips)
  }
  Free_calloc(the_cube_num_args,the_arg_values)
}

return the_error;
}
