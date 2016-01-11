
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <math.h>
#include "types.h"
#include "input_output.h"
#include "search.h"
#include "query.h"
#include "bayes.h"
#include "gestion.h"
#include "eval.h"
#if PROLOG
#include "table.h"
#include "expected.h"
#endif
#include "database.h"

int the_expected_value;
/*int the_heuristic;*/
int use_sampling;
double the_sampling_ratio;
int query_prolog;
int counting_bottom_up;
int incremental_loading;
double the_num_evaluated_literals;
int average;

/*****************************************************************************
  Give a random number between 0 and a_max (included)
 *****************************************************************************/
long random_long(long a_max)
{
long the_value;
the_value = lrand48() % (a_max + 1);
if (the_value >= 0 && the_value <= a_max)
    return the_value;
else
    {
	fprintf(stderr, "Error in random long int !\n");
	exit(0);
    }
}

double oates_cohen_function(double d1,double d2)
{
double the_result;

if ((d1 == 0) && (d2 == 0))
  the_result = 0;
else
  the_result = d1 *log(d2);
return the_result;
}


int get_block_sign(tcube* a_cube,int a_block,tliteral *a_cube_lits)
{
int the_other_lit;
int the_block_sign;
int the_first_lit;

if (individual_based == TRUE) the_first_lit = 1; else the_first_lit = 0;
the_block_sign = NEG;
for (the_other_lit=the_first_lit;(the_block_sign == NEG)&&(the_other_lit<a_cube->nb_lit);the_other_lit++)
  if ((a_cube->blocks_table[the_other_lit] == a_block) && (a_cube_lits[the_other_lit].sign == POS)
      && ((the_search_bias != CLASSIFICATION) || (the_other_lit != the_first_lit)))
    the_block_sign = POS;
return the_block_sign;
}

long num_substitutions_satisfying_constraints(int *a_substitution,int *a_nb_consts,int a_cube_nb_args)
{
long n=1;
int i;

for (i=0;i<a_cube_nb_args;i++)
  if (a_substitution[i] == -1)
    n *= a_nb_consts[i];

return n;
}

int count_substitutions_for_all_instances(tcube *a_cube,int *a_substitution,int *a_lits_table,
					  int *a_nb_consts,int a_part,int a_literal,
					  tinstance *instances,int a_num_inst,long *a_number)
{
int the_error = 0;
int i,j;
int the_result,functional,stop=FALSE;
long the_number;
int *the_substitution;

*a_number = 0;
the_substitution = Calloc(a_cube->nb_arg,int)
if (the_substitution == NULL) the_error = 2; else {
  if ((the_predicate_table[a_cube->lits[a_literal].predicate].kind == STRUCTURAL)
      && (((a_substitution[a_cube->lits[a_literal].args[0]] != -1)
	   && (the_predicate_table[a_cube->lits[a_literal].predicate].dimensions[1] == 1))
	  || ((a_substitution[a_cube->lits[a_literal].args[1]] != -1)
	   && (the_predicate_table[a_cube->lits[a_literal].predicate].dimensions[0] == 1))))
    functional = TRUE;
  else
    functional = FALSE;
  for (i=0;(the_error == 0)&&(i<a_num_inst)&&(stop==FALSE);i++) {
    for (j=0;j<a_cube->nb_arg;j++)
      the_substitution[j] = a_substitution[j];
    the_result = 1;
    for (j=0;the_result && (j<the_predicate_table[a_cube->lits[a_literal].predicate].arity);j++)
      if (the_substitution[a_cube->lits[a_literal].args[j]] != -1)
	the_result = (the_substitution[a_cube->lits[a_literal].args[j]] 
		      == instances[i][j]);
      else
	the_substitution[a_cube->lits[a_literal].args[j]] = instances[i][j];
    if (the_result) {
      the_error = recursive_count_substitutions(a_cube,a_lits_table,the_substitution,
						a_nb_consts,&the_number,a_part);
      *a_number += the_number;
      if (functional == TRUE)
	stop = TRUE;
    }
  }
  Free_calloc(a_cube->nb_arg,the_substitution)
}

return the_error;
}

/******************************************************************************
******************************************************************************/
int recursive_count_substitutions(tcube *a_cube,int *a_lits_table,int *the_substitution,
			      int *the_nb_consts,long *a_number,int a_part)
{
int the_error = 0;
int the_next_literal = -1;
int i,the_first_lit;
long the_total,the_complement;
int *the_lits_table;

if (individual_based == TRUE)
  the_first_lit = 1;
else
  the_first_lit = 0;

for (i=the_first_lit;(i<a_cube->nb_lit)&&(the_next_literal == -1);i++)
  if ((a_lits_table[i] == 0)&&(a_cube->lits[i].sign == POS))
    the_next_literal = i;
for (i=the_first_lit;(i<a_cube->nb_lit)&&(the_next_literal == -1);i++)
  if ((a_lits_table[i] == 0)&&(a_cube->lits[i].sign == NEG))
    the_next_literal = i;

if (the_next_literal == -1) {
  *a_number = num_substitutions_satisfying_constraints(the_substitution,the_nb_consts,a_cube->nb_arg);
  return the_error;
}

the_lits_table = Calloc(a_cube->nb_lit,int)
if (the_lits_table == NULL) the_error = 2; else {
  for (i=the_first_lit;i<a_cube->nb_lit;i++)
    the_lits_table[i] = a_lits_table[i];
  the_lits_table[the_next_literal] = 1;

  if (a_cube->lits[the_next_literal].sign == POS) {
    the_error = count_substitutions_for_all_instances(a_cube,the_substitution,the_lits_table,
						      the_nb_consts,a_part,the_next_literal,
						      the_predicate_table[a_cube->lits[the_next_literal].predicate].
						      subpredicates[a_cube->lits[the_next_literal].subpredicate]
						      ->instances[a_part],
						      the_predicate_table[a_cube->lits[the_next_literal].predicate]
						      .subpredicates[a_cube->lits[the_next_literal].subpredicate]
						      ->nb_inst[a_part],
						      a_number);
  }
  else
    if (the_predicate_table[a_cube->lits[the_next_literal].predicate].cwa == TRUE){
      the_error = recursive_count_substitutions(a_cube,the_lits_table,the_substitution,
						the_nb_consts,&the_total,a_part);
      if (the_error == 0) {
	the_error = count_substitutions_for_all_instances(a_cube,the_substitution,the_lits_table,
							  the_nb_consts,a_part,the_next_literal,
							  the_predicate_table[a_cube->lits[the_next_literal].predicate].
							  subpredicates[a_cube->lits[the_next_literal].subpredicate]
							  ->instances[a_part],
							  the_predicate_table[a_cube->lits[the_next_literal].predicate]
							  .subpredicates[a_cube->lits[the_next_literal].subpredicate]
							  ->nb_inst[a_part],
							  &the_complement);
	*a_number = the_total - the_complement;
      }
    }
    else {
      the_error = count_substitutions_for_all_instances(a_cube,the_substitution,the_lits_table,
							the_nb_consts,a_part,the_next_literal,
							the_predicate_table[a_cube->lits[the_next_literal].predicate].
							subpredicates[a_cube->lits[the_next_literal].subpredicate]
							->cinstances[a_part],
							the_predicate_table[a_cube->lits[the_next_literal].predicate]
							.subpredicates[a_cube->lits[the_next_literal].subpredicate]
							->nb_cinst[a_part],
							a_number);
    }
  Free_calloc(a_cube->nb_lit,the_lits_table)
}

if ((individual_based == TRUE)&&(*a_number > 0))
  *a_number = 1;
return the_error;
}

/******************************************************************************
******************************************************************************/
int count_building_substitutions_from_instances(tcube *a_cube,int *the_substitution,
						int *the_nb_consts, int the_cube_nb_args,int a_part)
{
int the_error = 0;
int i;
int the_block,the_block_sign,the_first_lit;
int *the_lits_table;
long the_number;

if (individual_based == TRUE)
  the_first_lit = 1;
else {
  the_first_lit = 0;
  for (i=0;i<the_cube_nb_args;i++)
    the_substitution[i] = -1;
  a_cube->capital_n = num_substitutions_satisfying_constraints(the_substitution,the_nb_consts,the_cube_nb_args);
}

the_lits_table = Calloc(a_cube->nb_lit,int)
if (the_lits_table == NULL) return 2;

if (the_expected_value == NORMAL){
  /* count negative */
  for (i=the_first_lit;i<a_cube->nb_lit;i++)
    the_lits_table[i] = -1;
  for (the_block=0;(the_block<a_cube->num_blocks)&&(the_error==0);the_block++) {
    the_block_sign = get_block_sign(a_cube,the_block,a_cube->lits);
    if ((the_search_bias == CLASSIFICATION)
	|| (the_search_bias == POSITIVE_CLASSIFICATION)
	|| (the_search_bias == HORN_POSITIVE_CLASSIFICATION)) {
      if ((the_block == a_cube->blocks_table[the_first_lit])&&(the_block_sign == NEG))
	for (i=the_first_lit;i<a_cube->nb_lit;i++)
	  if (a_cube->blocks_table[i] == the_block)
	    the_lits_table[i] = 0;
    }
    else
      if (the_block_sign == NEG)
	for (i=the_first_lit;i<a_cube->nb_lit;i++)
	  if (a_cube->blocks_table[i] == the_block)
	    the_lits_table[i] = 0;
  }
  the_error = recursive_count_substitutions(a_cube,the_lits_table,the_substitution,the_nb_consts,&the_number,a_part);
  if (individual_based == TRUE) {
    if (the_number > 0)
      a_cube->nb_inst_negative += 1;
  }
  else
    a_cube->nb_inst_negative = the_number;

  /* count positive */
  for (i=the_first_lit;i<a_cube->nb_lit;i++)
    the_lits_table[i] = -1;
  for (the_block=0;(the_block<a_cube->num_blocks)&&(the_error==0);the_block++) {
    the_block_sign = get_block_sign(a_cube,the_block,a_cube->lits);
    if ((the_search_bias == CLASSIFICATION)
	|| (the_search_bias == POSITIVE_CLASSIFICATION)
	|| (the_search_bias == HORN_POSITIVE_CLASSIFICATION)) {
      if ((the_block != a_cube->blocks_table[the_first_lit])||(the_block_sign == POS))
	for (i=the_first_lit;i<a_cube->nb_lit;i++)
	  if (a_cube->blocks_table[i] == the_block)
	    the_lits_table[i] = 0;
    }
    else
      if (the_block_sign == POS)
	for (i=the_first_lit;i<a_cube->nb_lit;i++)
	  if (a_cube->blocks_table[i] == the_block)
	    the_lits_table[i] = 0;
  }
  the_error = recursive_count_substitutions(a_cube,the_lits_table,the_substitution,the_nb_consts,&the_number,a_part);
  if (individual_based == TRUE) {
    if (the_number > 0)
      a_cube->nb_inst_positive += 1;
  }
  else
    a_cube->nb_inst_positive = the_number;
}
else
  /* count for each block */
  for (the_block=0;(the_block<a_cube->num_blocks)&&(the_error==0);the_block++) {
    for (i=the_first_lit;i<a_cube->nb_lit;i++)
      if (a_cube->blocks_table[i] == the_block)
	the_lits_table[i] = 0;
      else
	the_lits_table[i] = -1;
    the_error = recursive_count_substitutions(a_cube,the_lits_table,the_substitution,the_nb_consts,&the_number,a_part);
    if (individual_based == TRUE) {
      if (the_number > 0)
	a_cube->block_nb_inst[the_block] += 1;
    }
    else
      a_cube->block_nb_inst[the_block] = the_number;
  }

/* count for the clause */
for (i=the_first_lit;(i<a_cube->nb_lit)&&(the_error == 0);i++)
  the_lits_table[i] = 0;
the_error = recursive_count_substitutions(a_cube,the_lits_table,the_substitution,the_nb_consts,&the_number,a_part);
if (individual_based == TRUE) {
  if (the_number > 0)
    a_cube->nb_inst += 1;
}
else
  a_cube->nb_inst = the_number;

return the_error;
}

int count_substitutions(tcube *a_cube, int the_part, int *the_individual_var, long *the_dividers,
			int *the_nb_consts, long *the_block_nb_inst, int *the_substitution,
			int *the_block_results, table the_table, int the_individual_arity)
{
int the_error = 0;
tliteral *the_cube_lits = NULL;
int the_cube_nb_args;
targ *the_cube_args = NULL;
int i;
long j,the_long;
long N = 0;
long the_internal_N = 0;
int the_result;
int the_clause_value;
int the_positive_value;
int the_negative_value;
int the_internal_clause_value;
int the_internal_positive_value;
int the_internal_negative_value;
int the_first_lit;
int the_block,the_block_sign,the_block_value;
long the_sampling_size;
long the_internal_long;

if (unification_form == EXPLICIT){
  the_cube_nb_args = a_cube->nb_exp_args;
  the_cube_lits = a_cube->exp_lits;
  the_cube_args = a_cube->exp_args;
}
else{
  the_cube_nb_args = a_cube->nb_arg;
  the_cube_lits = a_cube->lits;
  the_cube_args = a_cube->args;
}

for (i=0;i<the_cube_nb_args;i++)
  if (the_cube_args[i].flip == VAR) {
    if (the_type_table[the_cube_args[i].type].global == TRUE)
      the_nb_consts[i] = the_type_table[the_cube_args[i].type].nb_const[0];
    else
      the_nb_consts[i] = the_type_table[the_cube_args[i].type].nb_const[the_part];
  }
  else
    the_nb_consts[i] = 1;
the_dividers[the_individual_var[the_individual_arity-1]] = 1;
for (i=the_individual_arity-2;(i>=0) && (the_error == 0);i--)
  if ((the_nb_consts[the_individual_var[i+1]] != 0)
      && (the_dividers[the_individual_var[i+1]]
      >= LONG_MAX / the_nb_consts[the_individual_var[i+1]])) the_error = 11; else
	the_dividers[the_individual_var[i]] = the_dividers[the_individual_var[i+1]]
	  * the_nb_consts[the_individual_var[i+1]];
if (the_cube_nb_args > the_individual_arity) {
  the_dividers[the_individual_var[the_cube_nb_args-1]] = 1;
  for (i=the_cube_nb_args-2;(i>=the_individual_arity) && (the_error == 0);i--)
    if ((the_nb_consts[the_individual_var[i+1]] != 0)
	&& (the_dividers[the_individual_var[i+1]]
	>= LONG_MAX / the_nb_consts[the_individual_var[i+1]])) the_error = 11; else
	  the_dividers[the_individual_var[i]] = the_dividers[the_individual_var[i+1]]
	    * the_nb_consts[the_individual_var[i+1]];
}
if (the_error == 0){
  if ((the_nb_consts[the_individual_var[0]] != 0)
    && (the_dividers[the_individual_var[0]]
      >= LONG_MAX / the_nb_consts[the_individual_var[0]])) the_error = 11; else{
	N = the_dividers[the_individual_var[0]] * the_nb_consts[the_individual_var[0]];
	if (the_cube_nb_args > the_individual_arity) {
	  if ((the_nb_consts[the_individual_var[the_individual_arity]] != 0)
	      && (the_dividers[the_individual_var[the_individual_arity]]
		  >= LONG_MAX / the_nb_consts[the_individual_var[the_individual_arity]])) the_error = 11; else
		    the_internal_N = the_dividers[the_individual_var[the_individual_arity]]
		      * the_nb_consts[the_individual_var[the_individual_arity]];
	}
	else
	  the_internal_N = 1;
      }
}
if (the_error == 0) {
  if ((counting_bottom_up == TRUE) && (individual_based == FALSE)
      && ((the_expected_value == NORMAL)||(the_expected_value == NAIVE))
      && (use_sampling == FALSE) && (unification_form == IMPLICIT)) {
    the_error = count_building_substitutions_from_instances(a_cube,the_substitution,the_nb_consts,the_cube_nb_args,the_part);
  }
  else {
    a_cube->nb_inst = 0;
    a_cube->nb_inst_positive = 0;
    a_cube->nb_inst_negative = 0;
    for (i=0;i<a_cube->num_blocks;i++)
      a_cube->block_nb_inst[i] = 0;
    if (use_sampling == 1)
      the_sampling_size = N * the_sampling_ratio;
    else
      the_sampling_size = N;
    a_cube->capital_n = 0;
    for (j=0;j<the_sampling_size;j++) {
      if (use_sampling == 1)
	the_long = random_long(N-1);
      else
	the_long = j;
      if ((use_sampling != 2) || (random_long(N) <= (long)(the_sampling_ratio * (double)N))) {
	a_cube->capital_n += 1;
	for (i=0;i<the_individual_arity;i++)
	  the_substitution[the_individual_var[i]] = (the_long / the_dividers[the_individual_var[i]])
	    %  the_nb_consts[the_individual_var[i]];
	if ((counting_bottom_up == TRUE) && ((the_expected_value == NORMAL)||(the_expected_value == NAIVE))
	    && (unification_form == IMPLICIT)) {
	  the_error = count_building_substitutions_from_instances(a_cube,the_substitution,the_nb_consts,the_cube_nb_args,the_part);
	}
	else {
	  the_clause_value = 0;
	  the_positive_value = 0;
	  the_negative_value = 0;
	  for (i=0;i<a_cube->num_blocks;i++)
	    the_block_results[i] = 0;
	  for (the_internal_long=0;
	       (the_internal_long<the_internal_N)
		 &&(the_clause_value==0);
	       the_internal_long++) {
	    for (i=the_individual_arity;i<the_cube_nb_args;i++)
	      the_substitution[the_individual_var[i]] = (the_internal_long / the_dividers[the_individual_var[i]])
		%  the_nb_consts[the_individual_var[i]];
	    the_internal_clause_value = 1;
	    the_internal_positive_value = 1;
	    the_internal_negative_value = 1;
	    for (the_block=0;(the_block<a_cube->num_blocks)&&(the_error==0)
		   &&((the_expected_value==NAIVE)||(the_expected_value==BACKGROUND)
		      ||(the_expected_value == LINEAR_RESOLUTION)
		      ||(the_expected_value == INTEGRITY_CONSTRAINTS)
		      ||(the_internal_positive_value==1)||(the_internal_negative_value==1));the_block++) {
	      the_block_value = 1;
	      if (unification_form == EXPLICIT) {
		for (i=0;(i<a_cube->nb_substs)&&(the_block_value==1);i++)
		  if (a_cube->blocks_table[a_cube->nb_lit+i] == the_block) {
		    if (a_cube->substs[i].flip == VAR)
		      the_block_value = (the_substitution[a_cube->substs[i].arg]
					 == the_substitution[a_cube->substs[i].value]);
		    else
		      the_block_value = (the_substitution[a_cube->substs[i].arg]
					 == a_cube->substs[i].value);
		  }
		the_internal_clause_value = the_internal_clause_value && the_block_value;
		the_internal_positive_value = the_internal_positive_value && the_block_value;
		the_internal_negative_value = the_internal_negative_value && the_block_value;
	      }
	      if (individual_based == TRUE) the_first_lit = 1; else the_first_lit = 0;
	      the_block_sign = get_block_sign(a_cube,the_block,the_cube_lits);
	      for (i=the_first_lit;(i<a_cube->nb_lit)&&(the_error==0)&&(the_block_value==1);i++)
		if (a_cube->blocks_table[i] == the_block)
		  if ((the_expected_value==NAIVE) || (the_expected_value==BACKGROUND)
		      || (the_expected_value == LINEAR_RESOLUTION)
		      || (the_expected_value == INTEGRITY_CONSTRAINTS)
		      || (((the_search_bias == CLASSIFICATION)
			   || (the_search_bias == POSITIVE_CLASSIFICATION)
			   || (the_search_bias == HORN_POSITIVE_CLASSIFICATION))
			  && (the_internal_positive_value==1))
		      || (((the_block_sign==POS) && (the_internal_positive_value==1))
			  || ((the_block_sign==NEG) && (the_internal_negative_value==1)))) {
		    the_error
		      = evaluate_literal(&(the_cube_lits[i]),the_cube_args,the_part,the_substitution, &the_result);
		    if (the_error == 0)
		      the_block_value = the_block_value && the_result;
		  }
	      if (the_error == 0) {
		the_block_results[the_block] = the_block_results[the_block] || the_block_value;
		a_cube->block_nb_inst[the_block] += the_block_value;
		the_internal_clause_value = the_internal_clause_value && the_block_value;
		if ((the_search_bias == CLASSIFICATION)
		    || (the_search_bias == POSITIVE_CLASSIFICATION)
		    || (the_search_bias == HORN_POSITIVE_CLASSIFICATION)) {
		  if ((a_cube->num_blocks > the_first_lit)&&(the_block == a_cube->blocks_table[the_first_lit])
		      &&(the_block_sign == NEG))
		    the_internal_negative_value = the_internal_negative_value && the_block_value;
		  else
		    the_internal_positive_value = the_internal_positive_value && the_block_value;
		}
		else
		  if (the_block_sign == POS)
		    the_internal_positive_value = the_internal_positive_value && the_block_value;
		  else
		    the_internal_negative_value = the_internal_negative_value && the_block_value;
	      }
	    }
	    the_clause_value = the_clause_value || the_internal_clause_value;
	    the_positive_value = the_positive_value || the_internal_positive_value;
	    the_negative_value = the_negative_value || the_internal_negative_value;
	  }
	  if (the_clause_value>=1) {
	    a_cube->nb_inst += the_clause_value;
	    a_cube->nb_inst_positive += the_positive_value;
	    a_cube->nb_inst_negative += the_negative_value;
	  }
	  else {
	    if (the_positive_value==1)
	      a_cube->nb_inst_positive += the_positive_value;
	    if (the_negative_value==1)
	      a_cube->nb_inst_negative += the_negative_value;
	  }
#if PROLOG
	  increment_literal_cells(a_cube->num_blocks,the_block_results,a_cube,the_table);
#endif
	}
      }
    }
  }
}

return the_error;
}

int eval_cube_in_file(tcube *a_cube,int a_fold,int * a_fold_table)
{
int the_error = 0;
int *the_substitution = NULL;
int i;
long *the_dividers = NULL;
int *the_nb_consts = NULL;
int the_part;
long the_nb_inst_positive = 0;
long the_nb_inst_negative = 0;
long the_nb_inst = 0;
long the_nb_substitution = 0;
long *the_block_nb_inst = NULL;
tliteral *the_cube_lits = NULL;
int the_cube_nb_args;
targ *the_cube_args = NULL;
int the_individual_arity; /* number of variables in an individual */
int *the_individual_var = NULL; /* the indices of first the variables of individual, then the remaining variables */
int *the_block_results = NULL; /* stores whether an individual satisfies each block of literals */
table the_table = NULL; /* contingency table */
double the_confirmation;
double the_optimistic;
int the_number_parts;

if (unification_form == EXPLICIT){
  the_cube_nb_args = a_cube->nb_exp_args;
  the_cube_lits = a_cube->exp_lits;
  the_cube_args = a_cube->exp_args;
}
else{
  the_cube_nb_args = a_cube->nb_arg;
  the_cube_lits = a_cube->lits;
  the_cube_args = a_cube->args;
}
/* negate the clause */
for (i=0;i<a_cube->nb_lit;i++)
  if (the_cube_lits[i].sign == POS)
    the_cube_lits[i].sign = NEG;
  else
    the_cube_lits[i].sign = POS;

if (the_cube_nb_args != 0) {
  if (individual_based == TRUE)
    the_individual_arity = the_predicate_table[the_cube_lits[0].predicate].arity;
  else
    the_individual_arity = the_cube_nb_args;    
  the_error = set_individual_var(the_cube_lits,the_cube_nb_args,&the_individual_var);
  if (the_error == 0) {
    the_error = make_literal_blocks(a_cube);
    if (the_error == 0)/*make_literal_blocks*/{
      the_dividers = Calloc(the_cube_nb_args,long)
      if (the_dividers == NULL) {
	the_error = 2;
	Free_calloc(the_max_var,the_individual_var)
      }
      else/*the_dividers*/{
	the_nb_consts = Calloc(the_cube_nb_args,int)
	if (the_nb_consts == NULL){
	  the_error = 2;
	  Free_calloc(the_max_var,the_individual_var)
	  Free_calloc(the_cube_nb_args,the_dividers)
	}
	else/*the_nb_consts*/{
	  if ((the_expected_value == NAIVE)||(the_expected_value == BACKGROUND)
	    ||(the_expected_value == LINEAR_RESOLUTION)||(the_expected_value == INTEGRITY_CONSTRAINTS)) {
	    the_block_nb_inst = Calloc(a_cube->num_blocks,long)
	    if (the_block_nb_inst == NULL) {
	      the_error = 2;
	      Free_calloc(the_cube_nb_args,the_nb_consts)
	      Free_calloc(the_max_var,the_individual_var)
	      Free_calloc(the_cube_nb_args,the_dividers)
	    }
	    else/*the_block_nb_inst*/{
	      for (i=0;i<a_cube->num_blocks;i++)
		the_block_nb_inst[i] = 0;
	    }
	  }
	  else/*(the_expected_value != NAIVE)*/{
	    the_nb_inst_positive = 0;
	    the_nb_inst_negative = 0;
	  }
	  if (the_error == 0) {
	    the_substitution = Calloc(the_cube_nb_args,int)
	    if (the_substitution == NULL){
	      the_error = 2;
	      if ((the_expected_value == NAIVE)||(the_expected_value == BACKGROUND)
		 ||(the_expected_value == LINEAR_RESOLUTION)||(the_expected_value == INTEGRITY_CONSTRAINTS))
		Free_calloc(a_cube->num_blocks,the_block_nb_inst)
	      Free_calloc(the_cube_nb_args,the_nb_consts)
	      Free_calloc(the_max_var,the_individual_var)
	      Free_calloc(the_cube_nb_args,the_dividers)
	    }
	    else/*the_substitution*/{
	      for (i=0;i<a_cube->nb_arg;i++)
		if (a_cube->args[i].flip == VAR)
		  the_substitution[i] = -1;
		else
		  the_substitution[i] = a_cube->args[i].value;
	      the_block_results = Calloc(a_cube->num_blocks,int)
	      if (the_block_results == NULL) {
		the_error = 2;
		if ((the_expected_value == NAIVE)||(the_expected_value == BACKGROUND)
		    ||(the_expected_value == LINEAR_RESOLUTION)||(the_expected_value == INTEGRITY_CONSTRAINTS))
		  Free_calloc(a_cube->num_blocks,the_block_nb_inst)
		Free_calloc(the_cube_nb_args,the_nb_consts)
		Free_calloc(the_max_var,the_individual_var)
		Free_calloc(the_cube_nb_args,the_dividers)
	      }
	      else/*the_block_results*/{
#if PROLOG
		if (unification_form == EXPLICIT)
		  the_error = new_table_0(a_cube->nb_lit+a_cube->nb_substs,&the_table);
		else
		  the_error = new_table_0(a_cube->nb_lit,&the_table);
#endif
		if (the_error == 2) {
		  if ((the_expected_value == NAIVE)||(the_expected_value == BACKGROUND)
		      ||(the_expected_value == LINEAR_RESOLUTION)||(the_expected_value == INTEGRITY_CONSTRAINTS))
		    Free_calloc(a_cube->num_blocks,the_block_nb_inst)
		  Free_calloc(the_cube_nb_args,the_nb_consts)
		  Free_calloc(the_max_var,the_individual_var)
		  Free_calloc(the_cube_nb_args,the_dividers)
		  Free_calloc(a_cube->num_blocks,the_block_results)
		}
	      }	    
	    }
	  }
	}
      }
    }
  }/*end of allocations*/
  if (the_error == 0)/*let's count!*/{
    the_nb_inst = 0;
    the_nb_substitution = 0;
    a_cube->confirmation = 0;
    the_part = first_partition();
    the_number_parts = 1;
    while (! last_partition(the_part)) {
      /*for (the_part=0; (the_part<the_nb_parts) && (the_error == 0); the_part++)*/
      if (((the_num_fold == 1) && (a_fold_table == NULL))
	  || ((the_num_fold == 1) && (a_fold_table[the_part] != -1))
	  || ((a_fold_table[the_part] != -1) && (a_fold_table[the_part] != a_fold))) {
	the_error = count_substitutions(a_cube,the_part,the_individual_var,the_dividers,
					the_nb_consts,the_block_nb_inst,the_substitution,
					the_block_results,the_table,the_individual_arity);
	if (the_error == 0){
	  if (the_expected_value == NORMAL){
	    the_nb_inst_positive += a_cube->nb_inst_positive;
	    the_nb_inst_negative += a_cube->nb_inst_negative;
	  }
	  else
	    for (i=0;i<a_cube->num_blocks;i++)
	      the_block_nb_inst[i] += a_cube->block_nb_inst[i];
	  the_nb_inst += a_cube->nb_inst;
	  the_nb_substitution += a_cube->capital_n;
	  evaluate_confirmation_and_optimistic(a_cube->nb_inst,a_cube->nb_inst_negative,a_cube->nb_inst_positive,
					       a_cube->capital_n,a_cube->block_nb_inst,a_cube->num_blocks,
					       &the_confirmation,&the_optimistic);
	  a_cube->confirmation += the_confirmation;
	  a_cube->optimistic += the_optimistic;
	}
      }
      the_part = next_partition(the_part);
      the_number_parts += 1;
    }
  }
  if (the_error == 0)/*calculation of expected value and confirmation*/{
    a_cube->observed = (double)the_nb_inst / (double)the_nb_substitution;
    a_cube->nb_inst = the_nb_inst;
    a_cube->nb_inst_positive = the_nb_inst_positive;
    a_cube->nb_inst_negative = the_nb_inst_negative;
    a_cube->capital_n = the_nb_substitution;
    a_cube->confirmation = a_cube->confirmation / (double)the_number_parts;
    a_cube->optimistic = a_cube->optimistic / (double)the_number_parts;
    if (average == FALSE) {
      evaluate_confirmation_and_optimistic(a_cube->nb_inst,a_cube->nb_inst_negative,
					   a_cube->nb_inst_positive,
					   a_cube->capital_n,a_cube->block_nb_inst,a_cube->num_blocks,
					   &the_confirmation,&the_optimistic);
      a_cube->confirmation = the_confirmation;
      a_cube->optimistic = the_optimistic;
    }
    if ((the_expected_value == NAIVE)||(the_expected_value == BACKGROUND)
       ||(the_expected_value == LINEAR_RESOLUTION)||(the_expected_value == INTEGRITY_CONSTRAINTS))
      Free_calloc(a_cube->num_blocks,the_block_nb_inst)
    Free_calloc(the_cube_nb_args,the_substitution)
    Free_calloc(the_cube_nb_args,the_nb_consts)
    Free_calloc(the_max_var,the_individual_var)
    Free_calloc(the_cube_nb_args,the_dividers)
    Free_calloc(a_cube->num_blocks,the_block_results)
#if PROLOG
    if (unification_form == EXPLICIT)
      free_table(a_cube->nb_lit+a_cube->nb_substs,the_table);
    else
      free_table(a_cube->nb_lit,the_table);
#endif
  }
}
else{
  a_cube->observed = 1;
}

/* negate the cube */
for (i=0;i<a_cube->nb_lit;i++)
  if (the_cube_lits[i].sign == POS)
    the_cube_lits[i].sign = NEG;
  else
    the_cube_lits[i].sign = POS;

return the_error;
}

/*****************************************************************************
 *****************************************************************************/
int eval_cube(tcube *a_cube,int a_fold,int *a_fold_table)
{
int the_error = 0;

if (database == TRUE)
  the_error = eval_cube_in_database(a_cube);
else
  the_error = eval_cube_in_file(a_cube,a_fold,a_fold_table);

return the_error;
}


int evaluate_literal(tliteral *a_literal,targ *a_cube_args,int a_part,int *a_substitution,int *a_result)
{
int the_result;
int i,j;
int the_arity;
tinstance *the_instances;
tinstance *the_cinstances;

int the_error = 0;

the_num_evaluated_literals += 1;

#if PROLOG
if (query_prolog == TRUE){
  if (the_predicate_table[a_literal->predicate].cwa == TRUE){
    the_error = query_literal(a_literal, a_cube_args, a_part, a_substitution, &the_result);
    if (a_literal->sign == NEG)
      if (the_result == 1)
	the_result = 0;
      else
	the_result = 1;
  }
  else/*(the_predicate_table[a_literal->predicate].cwa == FALSE)*/{
    if (a_literal->sign == NEG)
      the_result = 0;
    else
      the_error = query_literal(a_literal, a_cube_args, a_part, a_substitution, &the_result);	  
  }
}
else/*(query_prolog == FALSE)*/
#endif
{
  the_arity = the_predicate_table[a_literal->predicate].arity;
  
  the_instances = the_predicate_table[a_literal->predicate].subpredicates[a_literal->subpredicate]->instances[a_part];
  the_cinstances = the_predicate_table[a_literal->predicate].subpredicates[a_literal->subpredicate]->cinstances[a_part];
  the_result = 0;
  if ((the_predicate_table[a_literal->predicate].cwa == TRUE)||(a_literal->sign == POS)){
    for (i=0;(the_result == 0)
	   && (i<the_predicate_table[a_literal->predicate].subpredicates[a_literal->subpredicate]->nb_inst[a_part]);i++){
      the_result = 1;
      for (j=0;the_result && (j<the_arity);j++)
	if (a_cube_args[a_literal->args[j]].flip == VAR)
	  the_result = the_result && (a_substitution[a_literal->args[j]] == the_instances[i][j]);
	else
	  the_result = the_result && (a_cube_args[a_literal->args[j]].value == the_instances[i][j]);
    }
    if (a_literal->sign == NEG) {
      if (the_result == 1)
	the_result = 0;
      else
	the_result = 1;
    }
  }
  else/*(the_predicate_table[a_literal->predicate].cwa == FALSE)&&(a_literal->sign == NEG)*/{
    for (i=0;(the_result == 0)
	   && (i<the_predicate_table[a_literal->predicate].subpredicates[a_literal->subpredicate]->nb_cinst[a_part]);i++){
      the_result = 1;
      for (j=0;the_result && (j<the_arity);j++)
	if (a_cube_args[a_literal->args[j]].flip == VAR)
	  the_result = the_result && (a_substitution[a_literal->args[j]] == the_cinstances[i][j]);
	else
	  the_result = the_result && (a_cube_args[a_literal->args[j]].value == the_cinstances[i][j]);
    }
  }
}

*a_result = the_result;
return the_error;
}

int subsumes(tcube *a_first, tcube *a_second, int *a_result)
{
int the_error = 0;
int i,j,k,j0;
long num_substitution;
int the_result;
int the_function_result = 0;
long *the_dividers;
int *the_substitution;
long N = 0;
int the_match,the_stop;

the_result = 1;
i = 0; j = 0; j0 = 0;
while ((i<a_first->nb_lit) && (j<a_second->nb_lit) && the_result)
  {
    if ((a_first->lits[i].predicate > a_second->lits[j].predicate)
	|| ((a_first->lits[i].predicate == a_second->lits[j].predicate)
	    && (a_first->lits[i].subpredicate > a_second->lits[j].subpredicate)))
      {
	if ((a_second->lits[j].predicate != a_second->lits[j0].predicate)
	    || (a_second->lits[j].subpredicate != a_second->lits[j0].subpredicate))
	  j0 = j;
	j++;
      }
    else
      if ((a_first->lits[i].predicate == a_second->lits[j].predicate)
	  && (a_first->lits[i].subpredicate == a_second->lits[j].subpredicate)) {
	if (a_first->lits[i].sign == a_second->lits[j].sign)
	  {
	    i++;
	    j = j0;
	  }
	else
	  j++;
      }
      else
	the_result = 0;
  }

if ((i==a_first->nb_lit) && the_result)
  {
    the_dividers = Calloc(a_first->nb_arg,long)
    if (the_dividers == NULL)
      the_error = 2;
    else
      {
	the_dividers[a_first->nb_arg-1] = 1;
	for (i=a_first->nb_arg-2;i>=0;i--)
	  if (the_dividers[i+1] < LONG_MAX / a_second->nb_arg)
	    the_dividers[i] = the_dividers[i+1] * a_second->nb_arg;
	  else
	    the_error = 11;
	if (the_error == 0)
	  {
	    if (the_dividers[0] < LONG_MAX / a_second->nb_arg)
	      N = the_dividers[0] * a_second->nb_arg;
	    else
	      the_error = 11;
	    if (the_error == 0)
	      {
		the_substitution = Calloc(a_first->nb_arg,int)
		if (the_substitution == NULL)
		  {
		    the_error = 2;
		    Free_calloc(a_first->nb_arg,the_dividers)
		  }
		else
		  {
		    the_function_result = 0;
		    for (num_substitution=0;(num_substitution<N) && (the_function_result == 0);num_substitution++)
		      {
			the_stop = 0;
			for (i=0;(i<a_first->nb_arg) && (the_stop == 0);i++)
			  {
			    the_substitution[i] = (num_substitution / the_dividers[i]) % a_second->nb_arg;
			    if (a_second->args[the_substitution[i]].type != a_first->args[i].type)
			      the_stop = 1;
			    else
			      if (a_first->args[i].flip == CONST) {
				if (a_second->args[the_substitution[i]].flip == VAR)
				  the_stop = 1;
				else
				  if (a_second->args[the_substitution[i]].value != a_first->args[i].value)
				    the_stop = 1;
			      }
			  }
			if (the_stop == 0)
			  {
			    the_result = 1;
			    i = 0; j = 0; j0 = 0;
			    while ((i<a_first->nb_lit) && (j<a_second->nb_lit) && the_result)
			      {
				if ((a_first->lits[i].predicate > a_second->lits[j].predicate)
				    || ((a_first->lits[i].predicate == a_second->lits[j].predicate)
					&& (a_first->lits[i].subpredicate > a_second->lits[j].subpredicate)))
				  {
				    if ((a_second->lits[j].predicate != a_second->lits[j0].predicate)
					|| (a_second->lits[j].subpredicate != a_second->lits[j0].subpredicate))
				      j0 = j;
				    j++;
				  }
				else
				  if ((a_first->lits[i].predicate == a_second->lits[j].predicate)
				      && (a_first->lits[i].subpredicate == a_second->lits[j].subpredicate))
				    {
				      the_match = (a_first->lits[i].sign == a_second->lits[j].sign);
				      for (k=0;(k<the_predicate_table[a_first->lits[i].predicate].arity) && the_match;k++)
					the_match = (the_substitution[a_first->lits[i].args[k]] == a_second->lits[j].args[k]);
				      if (the_match)
					{
					  i++;
					  j = j0;
					}
				      else
					j++;
				    }
				  else
				    the_result = 0;
			      }
			    if ((i==a_first->nb_lit) && the_result)
			      the_function_result = 1;
			  }
		      }
		    Free_calloc(a_first->nb_arg,the_substitution)
		  }
	      }
	  }
	Free_calloc(a_first->nb_arg,the_dividers)
      }
  }
else
  the_function_result = 0;

*a_result = the_function_result;
return the_error;
}

int reeval_cubes(tlist *a_list)
{
int the_error = 0;
tlist *the_child;
int the_sampling_value;

the_sampling_value = use_sampling;
use_sampling = 0;
the_child = a_list;
while ((the_child != NULL) && (the_error == 0))
  {
    the_error = eval_cube((tcube*)the_child->elt,0,NULL);
    the_child = the_child->next;
  }
use_sampling = the_sampling_value;

return the_error;
}

int optimistic_value(tlist* an_elt,tlist *another)
{
int the_result;

if (((tcube*)an_elt->elt)->optimistic > ((tcube*)another->elt)->optimistic)
  the_result = 1;
else
  /*  if (((tcube*)an_elt->elt)->optimistic < ((tcube*)another->elt)->optimistic)
    the_result = 0;
  else
    if (((tcube*)an_elt->elt)->nb_lit < ((tcube*)another->elt)->nb_lit)
      the_result = 1;
    else*/
      the_result = 0;

return the_result;
}

int optimistic_then_observed_value(tlist* an_elt,tlist *another)
{
int the_result;

if (((tcube*)an_elt->elt)->optimistic > ((tcube*)another->elt)->optimistic)
  the_result = 1;
else if (((tcube*)an_elt->elt)->optimistic < ((tcube*)another->elt)->optimistic)
  the_result = 0;
else if (((tcube*)an_elt->elt)->observed < ((tcube*)another->elt)->observed)
  the_result = 1;
else
  the_result = 0;

return the_result;
}

int negated_optimistic_then_observed_value(tlist* an_elt,tlist *another)
{
return (! optimistic_then_observed_value(an_elt,another));
}


int confirmation_then_observed_value(tlist* an_elt,tlist *another)
{
int the_result;
double the_value1,the_value2;

the_value1 = ((tcube*)(an_elt->elt))->confirmation;
the_value2 = ((tcube*)(another->elt))->confirmation;
if (the_value1 > the_value2)
  the_result = 1;
else if (the_value1 < the_value2)
  the_result = 0;
else if (((tcube*)(an_elt->elt))->observed < ((tcube*)(another->elt))->observed)
  the_result = 1;
else
    the_result = 0;

return the_result;
}

int confirmation_value(tlist* an_elt,tlist *another)
{
int the_result;
double the_value1,the_value2;

the_value1 = ((tcube*)(an_elt->elt))->confirmation;
the_value2 = ((tcube*)(another->elt))->confirmation;
if (the_value1 > the_value2)
  the_result = 1;
else
  the_result = 0;

return the_result;
}

int negated_confirmation_then_observed_value(tlist* an_elt,tlist *another)
{
return (! confirmation_then_observed_value(an_elt,another));
}


int can_prune(tlist *a_list, tlist *a_node)
{
int the_result = 0;
int the_num_values = 0;

if (k_best > 0)
  {
    if (the_num_values == k_best)
      if (((tcube*)(a_list->elt))->confirmation > ((tcube*)a_node->elt)->optimistic)
	the_result = 1;
  }
else
  if (((tcube*)a_node->elt)->optimistic < confirmation_level)
    the_result = 1;

return the_result;
}

int can_store(tlist *a_list, tlist *a_node)
{
int the_result = 0;
int the_num_values;

if ((((tcube*)a_node->elt)->nb_lit == 0)
    || ((individual_based == TRUE) && (((tcube*)a_node->elt)->nb_lit == 1)))
  the_result = 0;
else {
  if (k_best > 0)
    {
      the_num_values = num_values_list(a_list,confirmation_then_observed_value);
      if ((the_num_values < k_best) ||
	  (! confirmation_then_observed_value(a_list,a_node)))
	the_result = 1;
    }
  else
    if (((tcube*)(a_node->elt))->confirmation >= confirmation_level)
      the_result = 1;
  if ((the_result == 1) && (consistency == TRUE))
    the_result = (((tcube*)a_node->elt)->observed <= the_consistency_threshold);
}

return the_result;
}


int can_refine(tlist *a_list, tlist *a_node)
{
int the_result = 0;

if ((((tcube*)a_node->elt)->nb_lit == 0)
    || ((individual_based == TRUE) && (((tcube*)a_node->elt)->nb_lit == 1)))
    the_result = 1;
else
    /*    if (((tcube*)a_node->elt)->observed > the_consistency_threshold)*/
  if (k_best > 0) {
    if ((num_values_list(a_list,confirmation_value) < k_best) ||
	(((tcube*)a_node->elt)->optimistic >= ((tcube*)(a_list)->elt)->confirmation))
      the_result = 1;
  }
  else
    if (((tcube*)a_node->elt)->optimistic >= confirmation_level)
      the_result = 1;

return the_result;
}

int prune_tautology(tcube *a_cube, int *a_result)
{
int the_error = 0;
int the_result = 0;
int i,j,k;

for (i=0; (i<a_cube->nb_lit-1)&&(the_result == 0);i++)
  for (j=i+1; (j<a_cube->nb_lit)&&(the_result == 0);j++)
    if ((a_cube->lits[i].predicate == a_cube->lits[j].predicate)
	&& (a_cube->lits[i].subpredicate == a_cube->lits[j].subpredicate)
	&& (a_cube->lits[i].sign != a_cube->lits[j].sign))
      {
	the_result = 1;
	for (k=0; (k<the_predicate_table[a_cube->lits[i].predicate].arity) && (the_result == 1); k++)
	  if (a_cube->lits[i].args[k] != a_cube->lits[j].args[k])
	    the_result = 0;
	  else
	    the_result = 1;
      }

*a_result = the_result;
return the_error;
}

int equivalent_clauses(tcube *a_first_clause, tcube *a_second_clause, int *a_result)
{
int the_error = 0;
int the_result = 0;

if ((a_first_clause->nb_lit == a_second_clause->nb_lit)
    && (a_first_clause->nb_arg == a_second_clause->nb_arg))
  {
    the_error = subsumes(a_first_clause,a_second_clause,&the_result);
    if ((the_error == 0) && (the_result))
      the_error = subsumes(a_second_clause,a_first_clause,&the_result);
  }
else
  the_result = 0;

*a_result = the_result;
return the_error;
}

void evaluate_confirmation_and_optimistic(long nHB, long not_head, long body, long population, long *blocks, int num_blocks,
				     double *confirmation,double *optimistic)
{
double the_expected;
double the_observed;
int i;

the_observed = nHB / (double)population;
the_expected = 1;
if ((the_expected_value==BACKGROUND)
    ||(the_expected_value == LINEAR_RESOLUTION)
    ||(the_expected_value == INTEGRITY_CONSTRAINTS)){
#if PROLOG
  the_error = get_expected_value(a_cube,the_table,&the_expected);
#endif
  *optimistic = the_expected;
  *optimistic =*optimistic/(1-sqrt(*optimistic))/(1-sqrt(*optimistic));
}
else
  if (the_expected_value == NORMAL) {
    the_expected = body * not_head / (double)population / (double)population;
    if (nHB <= body - not_head)
      *optimistic = not_head * (body - nHB) / (double)population / (double)population;
    else
      if (nHB <= not_head - body)
	*optimistic = body * (not_head - nHB) / (double)population / (double)population;
      else
	*optimistic = (not_head + body - nHB) * (not_head + body - nHB)
	  / (double)4 / (double)population / (double)population;
    *optimistic = sqrt(*optimistic/(1-sqrt(*optimistic))/(1-sqrt(*optimistic))); /* 24/08/2001 Amelie added global sqrt */
  }
  else/*(the_expected_value == NAIVE)*/{
    for (i=0;i<num_blocks;i++)
      the_expected *= (double)blocks[i] / (double)population;
    *optimistic = the_expected;
    *optimistic = sqrt(*optimistic/(1-sqrt(*optimistic))/(1-sqrt(*optimistic)));
  }
 if ((the_expected == 0)||(the_expected == 1))
  *confirmation = 0;
else
  *confirmation = (the_expected - the_observed)/(sqrt(the_expected) - the_expected);
}
