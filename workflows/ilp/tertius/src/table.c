#include <stdlib.h>
#include <math.h>
#include "types.h"
#include "table.h"


/* Allocate space for a table 
 *============================
 */

int talloc(table *a_table)
{
  table the_table = NULL;
  int the_error = 0;

  the_table = Malloc(tnode)

  if(the_table==NULL)
    the_error = 2;
  else
    the_error = 0;
  *a_table = the_table;
  return the_error;
}


/*
 * Free all the memory used by a table 
 *=====================================
 */

void free_table(int a_no_dimensions,table a_table)
{
  if(a_no_dimensions>0){
    free_table(a_no_dimensions-1,a_table->table_part.true_table);
    free_table(a_no_dimensions-1,a_table->table_part.false_table);
  }

  Free(a_table);
}


/* Create a table with 'size' dimensions and
 * all cells initiated to '1'
 *============================
 */

int new_table(int the_no_dimensions,table* a_table)
{
  int the_call_result;
  table the_table;

  the_call_result = talloc(&the_table);
  if(the_call_result!=0)
    return the_call_result;

  if(the_no_dimensions>0){
    the_table->table_part.type = TABLE;
    the_call_result = new_table(the_no_dimensions-1,&the_table->table_part.true_table);
    if(the_call_result!=0){
      Free(&the_table);
      return the_call_result;
    }
    the_call_result = new_table(the_no_dimensions-1,&the_table->table_part.false_table);
    if(the_call_result!=0){
      free_table(the_no_dimensions,the_table->table_part.true_table);
      Free(&the_table);
      return the_call_result;
    }
  }else{
    the_table->cell_part.type = CELL;
    the_table->cell_part.value = 1.0;
  }

  /* Return success */
  *a_table = the_table;
  return 0;
}


/* Create a table with 'size' dimensions and
 * all cells initiated to '0'
 *============================
 */

int new_table_0(int the_no_dimensions,table* a_table)
{
  int the_call_result;
  table the_table;

  the_call_result = talloc(&the_table);
  if(the_call_result!=0)
    return the_call_result;

  if(the_no_dimensions>0){
    the_table->table_part.type = TABLE;
    the_call_result = new_table_0(the_no_dimensions-1,&(the_table->table_part.true_table));
    if(the_call_result!=0){
      Free(&the_table);
      return the_call_result;
    }
    the_call_result = new_table_0(the_no_dimensions-1,&(the_table->table_part.false_table));
    if(the_call_result!=0){
      free_table(the_no_dimensions-1,the_table->table_part.true_table);
      Free(&the_table);
      return the_call_result;
    }
  }else{
    the_table->cell_part.type = CELL;
    the_table->cell_part.value = 0.0;
  }

  /* Return success */
  *a_table = the_table;
  return 0;
}


/* Print all the values in a table
 *=================================
 */

void print_table2(table a_table)
{
  if(a_table->table_part.type == TABLE){
    print_table2(a_table->table_part.true_table);
    if(get_no_dimensions(a_table->table_part.true_table)==3)
      printf("\n");
    print_table2(a_table->table_part.false_table);
    if(get_no_dimensions(a_table->table_part.false_table)==3)
      printf("\n");
  }else{
    printf("%f ",a_table->cell_part.value);
  }
}

void print_table(table a_table)
{
  if(a_table->table_part.type == TABLE){
    print_table2(a_table->table_part.true_table);
    print_table2(a_table->table_part.false_table);
    printf("\n");
  }else{
    printf("%f\n",a_table->cell_part.value);
  }
}


/* Get the number of dimensions in a table
 *=========================================
 */

int get_no_dimensions(table a_table)
{
  if(a_table->cell_part.type==CELL)
    return 0;
  else
    return(get_no_dimensions(a_table->table_part.true_table)+1);
}


/* Set the value of a cell
 *=========================
 */

void set_cell_value(float a_value,int an_index,table a_table)
{
  int the_no_dimensions,the_no_cells,the_dimension_count;

  the_no_dimensions = get_no_dimensions(a_table);
  the_no_cells = 1;
  for(the_dimension_count=0;the_dimension_count<the_no_dimensions;the_dimension_count++){
    the_no_cells *= 2;
  }
  the_no_cells--;

  for(the_dimension_count=0;the_dimension_count<the_no_dimensions;the_dimension_count++){
    if(an_index>the_no_cells/2){
      a_table = a_table->table_part.false_table;
      an_index -= 2*((the_no_dimensions-1)-the_dimension_count); 
      the_no_cells /= 2;
    }else{
      a_table = a_table->table_part.true_table;
      the_no_cells = the_no_cells/2;
    }
  }
  
  a_table->cell_part.value=a_value;
}


/* Increment the value of a cell by 1
 *====================================
 */

void increment_cell_value_1(int an_index,table a_table)
{
  int the_no_dimensions,the_no_cells,the_dimension_count;

  the_no_dimensions = get_no_dimensions(a_table);
  the_no_cells = 1;
  for(the_dimension_count=0;the_dimension_count<the_no_dimensions;the_dimension_count++){
    the_no_cells *= 2;
  }
  the_no_cells--;

  for(the_dimension_count=0;the_dimension_count<the_no_dimensions;the_dimension_count++){
    if(an_index>the_no_cells/2){
      a_table = a_table->table_part.false_table;
      an_index -= 2*((the_no_dimensions-1)-the_dimension_count); 
      the_no_cells /= 2;
    }else{
      a_table = a_table->table_part.true_table;
      the_no_cells = the_no_cells/2;
    }
  }
  
  a_table->cell_part.value++;
}


/* Get the value of a cell
 *=========================
 */

float get_cell_value(int an_index,table a_table)
{
  int the_no_dimensions,the_no_cells,the_dimension_count;

  the_no_dimensions = get_no_dimensions(a_table);
  the_no_cells = pow(2,the_no_dimensions)-1;

  for(the_dimension_count=0;the_dimension_count<the_no_dimensions;the_dimension_count++){
    if(an_index>the_no_cells/2){
      a_table = a_table->table_part.false_table;
      an_index = an_index-2*((the_no_dimensions-1)-the_dimension_count); 
      the_no_cells = the_no_cells/2;
    }else{
      a_table = a_table->table_part.true_table;
      the_no_cells = the_no_cells/2;
    }
  }
  
  return a_table->cell_part.value;
}


/* Adds up all the cell values in a table
 *========================================
 */

float get_population(table a_table)
{
  if(a_table->cell_part.type==CELL){
    return a_table->cell_part.value;
  }else{
    return(get_population(a_table->table_part.true_table)+
	   get_population(a_table->table_part.false_table));
  }
}


/* Returns a 1d marginal according to a table
 *============================================
 */

float get_d1_marginal(int an_index,table a_table)
{
  if(an_index==0)
    return(get_population(a_table->table_part.true_table));
  else{
    if(an_index==1)
      return(get_population(a_table->table_part.false_table));
    else{
      return(get_d1_marginal(an_index-2,a_table->table_part.true_table)+
	     get_d1_marginal(an_index-2,a_table->table_part.false_table));
    }
  }
}
   

/* Allocates space for a d1_marginal set
 *====================================
 */
  
int d1alloc(d1_marginal_set* a_d1_marginal_set)
{
  d1_marginal_set the_d1_marginal_set;
  
  the_d1_marginal_set = Malloc(d1node);
  if(the_d1_marginal_set==NULL)
    return 2;

  /* Return success */
  *a_d1_marginal_set = the_d1_marginal_set;
  return 0;
}
 

/* Creates a new marginal set of the given dimension
 *===================================================
 */

int new_d1_marginal_set(int a_no_dimensions,d1_marginal_set* a_d1_marginal_set)
{
  int the_call_result,the_value_count;
  d1_marginal_set the_d1_marginal_set;

  the_call_result = d1alloc(&the_d1_marginal_set);
  if(the_call_result!=0)
    return the_call_result;
  
  the_d1_marginal_set->no_marginals = a_no_dimensions*2;
  the_d1_marginal_set->marginal_values = Calloc(a_no_dimensions*2,float);
  if(the_d1_marginal_set->marginal_values==NULL){
    Free(&the_d1_marginal_set);
    return 2;
  }

  for(the_value_count=0;the_value_count<a_no_dimensions*2;the_value_count++){
    the_d1_marginal_set->marginal_values[the_value_count]=0.0;
  }

  /* Return success */
  *a_d1_marginal_set = the_d1_marginal_set;
  return 0;
}

/* Frees the memory used by a d1 marginal set 
 *============================================
 */

void free_d1_marginal_set(d1_marginal_set a_d1_marginal_set)
{
  Free_calloc(a_d1_marginal_set->no_marginals,a_d1_marginal_set->marginal_values);
  Free(a_d1_marginal_set);
}


/* Get number of dimensions from a d1 marginal set
 * ================================================
 */

int get_no_dimensions_d1_marginal_set(d1_marginal_set a_d1_marginal_set)
{
  return a_d1_marginal_set->no_marginals/2;
}

/* Sets a marginal value
 *=======================
 */

void set_d1_marginal_value(float a_value,int a_d1_marginal_index,
			  d1_marginal_set a_d1_marginal_set)
{
  a_d1_marginal_set->marginal_values[a_d1_marginal_index]=a_value;
}

/* Prints out a set of 1d marginals
 *==================================
 */

void print_d1_marginal_set(d1_marginal_set a_d1_marginal_set)
{
  int the_value_count;

  printf("%i ",a_d1_marginal_set->no_marginals);
  for(the_value_count=0;the_value_count<a_d1_marginal_set->no_marginals;
      the_value_count++){
    printf("%f ",a_d1_marginal_set->marginal_values[the_value_count]);
  }

  printf("\n");
}


/* Return the population according to a 1d set of marginals
 *==========================================================
 */

float get_population_d1_marginal_set(d1_marginal_set a_d1_marginal_set)
{
  return(a_d1_marginal_set->marginal_values[0]+
	 a_d1_marginal_set->marginal_values[1]);
}

/* Returns a set of 1d marginals according to a table
 *====================================================
 */

int get_d1_marginal_set(table a_table,d1_marginal_set* a_d1_marginal_set)
{
  int the_call_result,the_no_dimensions,the_index_count;
  d1_marginal_set the_d1_marginal_set;

  the_no_dimensions = get_no_dimensions(a_table);
  the_call_result = new_d1_marginal_set(the_no_dimensions,&the_d1_marginal_set);
  if(the_call_result!=0)
    return the_call_result;
  
  for(the_index_count=0;the_index_count<the_no_dimensions*2;the_index_count++){
    set_d1_marginal_value(get_d1_marginal(the_index_count,a_table),the_index_count,the_d1_marginal_set);
  }
  
  /* Return success */
  *a_d1_marginal_set = the_d1_marginal_set;
  return 0;
} 


/* Returns a table of expected values according to the given marginals
 *=====================================================================
 */

int get_expected_values(d1_marginal_set a_d1_marginal_set,table* a_table){
  int the_call_result,the_no_dimensions,the_no_cells,the_no_cells_copy;
  int the_cell_count,the_cell_count_copy,the_dimension_count;
  float the_population,the_numerator, the_denominator;
  table the_table;

  the_population = get_population_d1_marginal_set(a_d1_marginal_set);
  the_no_dimensions = a_d1_marginal_set->no_marginals/2;
  the_no_cells = pow(2,the_no_dimensions);
  the_call_result = new_table(the_no_dimensions,&the_table);
  if(the_call_result!=0)
    return the_call_result;
  
  /* Celculate estimated values for all cells */
  for(the_cell_count=0;the_cell_count<the_no_cells;the_cell_count++){
    the_numerator = 1.0;
    the_denominator = 1.0;
    the_cell_count_copy = the_cell_count;
    the_no_cells_copy = the_no_cells-1;
    for(the_dimension_count=0;the_dimension_count<the_no_dimensions;
	the_dimension_count++){
      /* Check for true or false table of dimension */
      if(the_cell_count_copy<=the_no_cells_copy/2){
	/* Use the index for the false part of the dimension */
	the_numerator=the_numerator*
	  a_d1_marginal_set->marginal_values[2*the_dimension_count];
	the_no_cells_copy = the_no_cells_copy/2;
      }else{
	/* Use the index for the true part of the dimension */
	the_numerator=the_numerator*
	  a_d1_marginal_set->marginal_values[2*the_dimension_count+1];
	the_cell_count_copy = 
	  the_cell_count_copy-2*((the_no_dimensions-1)-the_dimension_count);
	the_no_cells_copy = the_no_cells_copy/2;
      }
      /* Multiply denominator */
    }
    
    the_denominator = pow(the_population,the_no_dimensions-1);
    /* Set table value */
    set_cell_value(the_numerator/the_denominator,the_cell_count,the_table);
  }

  /* Return success */
  *a_table = the_table;
  return 0;
}


/* Adjust all the values in a table by the given adjustment
 *==========================================================
 */

void adjust_table(float an_adjustment,table a_table)
{
  if(a_table->cell_part.type==CELL){
    a_table->cell_part.value = a_table->cell_part.value*an_adjustment;
  }else{
    adjust_table(an_adjustment,a_table->table_part.true_table);
    adjust_table(an_adjustment,a_table->table_part.false_table);
  }
}

/* Adjusts the cell values in the given table 
 *that make up the given marginal to fit the given value
 *=======================================================
 */

void adjust_cell_values(float an_adjustment,int a_d1_marginal_index,table a_table)
{
  if(a_d1_marginal_index==0)
    adjust_table(an_adjustment,a_table->table_part.true_table);
  else{
    if(a_d1_marginal_index==1)
      adjust_table(an_adjustment,a_table->table_part.false_table);
    else{
      adjust_cell_values(an_adjustment,a_d1_marginal_index-2,a_table->table_part.true_table);
      adjust_cell_values(an_adjustment,a_d1_marginal_index-2,a_table->table_part.false_table);
    }
  }
}


/* Adjusts the values of a given table to fit the given marginals
 *================================================================
 */

void fit_marginals(d1_marginal_set a_d1_marginal_set,table a_table)
{
  /* Declarations */
  int the_no_d1_marginals;
  float the_delta;

  the_no_d1_marginals = get_no_dimensions(a_table)*2;

  the_delta = 1.0;
  while(the_delta>FITTING_THRESHOLD)
    fit_cycle(the_no_d1_marginals,a_d1_marginal_set,a_table,&the_delta);
}


/* Perform one cycle of the fitting algorithm 
 * ===========================================
 */

void fit_cycle(int a_no_marginals,d1_marginal_set a_d1_marginal_set,table a_table,float* a_delta)
{
  int the_marginal_count;
  float the_greatest_adjustment,the_fitted_marginal,the_adjustment,the_delta;

  the_greatest_adjustment = 0;
  for(the_marginal_count=0;the_marginal_count<a_no_marginals;the_marginal_count++){
    the_fitted_marginal = get_d1_marginal(the_marginal_count,a_table);
    /* Check if the observed marginal is zero */
    if(a_d1_marginal_set->marginal_values[the_marginal_count]==0){
      /* The observed marginal is zero */
      /* Check if the fitted marginal is zero */
      if(the_fitted_marginal==0){
	/* The fitted marginal is zero */
	/* Leave the fitted marginal at zero */
	the_adjustment = 1;
      }else{
	/* The fitted marginal is not zero */
	/* Set the fitted marginal to zero */
	the_adjustment = 0;
      }
    }else{
      /* The observed marginal is not zero */
      /* Check if the fitted marginal is zero */
      if(the_fitted_marginal==0){
	/* The fitted marginal is zero */
	/* Set the observed marginal to zero */
	/* Check if the observed marginal's literal is positive */
	if(the_marginal_count%2==0){
	  /* The observed marginal's literal is positive */
	  /* Add positive literal observed marginal value to negative literal observed marginal value */
	  a_d1_marginal_set->marginal_values[the_marginal_count+1] = 
	    a_d1_marginal_set->marginal_values[the_marginal_count+1]+ 
	      a_d1_marginal_set->marginal_values[the_marginal_count];
	  /* Set positive literal observed merginal to zero */ 
	  a_d1_marginal_set->marginal_values[the_marginal_count]=0;

	  /* Leave the fitted marginal at zero */
	  the_adjustment = 1;
	}else{
	  /* The observed marginal's literal is negative */
	  /* Add negative literal observed marginal value to positive literal observed marginal value */
	  a_d1_marginal_set->marginal_values[the_marginal_count-1] = 
	    a_d1_marginal_set->marginal_values[the_marginal_count-1]+ 
	      a_d1_marginal_set->marginal_values[the_marginal_count];
	  /* Set negative literal observed merginal to zero */ 
	  a_d1_marginal_set->marginal_values[the_marginal_count]=0;
	  /* Leave the fitted marginal at zero */
	  the_adjustment = 1;
	}
      } /* Ends zero fitting marginal */
      /* The fitted marginal is not zero */
      the_adjustment = a_d1_marginal_set->marginal_values[the_marginal_count]/the_fitted_marginal;
    } /* Ends non zero observed marginal */
    adjust_cell_values(the_adjustment,the_marginal_count,a_table);
    /*printf("adjusted table ");*/
    /*print_table(a_table);*/
    if(the_adjustment>the_greatest_adjustment)
      the_greatest_adjustment = the_adjustment;
    the_delta = fabs(the_greatest_adjustment)-1;
  } /* Ends all marginals loop */
  /*printf("the greatest adjustment and delta %f %f\n",the_greatest_adjustment,the_delta);*/
  *a_delta = the_delta;
}

/* Translate a Prolog table into a C table
 *=========================================
 */

int make_c_table(SP_term_ref a_zero_indecies_ref,int a_no_dimensions,table* a_table)
{
  int the_call_result;
  long the_index;
  SP_term_ref the_head_ref;
  table the_table;

  the_call_result = new_table(a_no_dimensions,&the_table);
  if(the_call_result!=0)
    return the_call_result;
  
  the_head_ref = SP_new_term_ref();
  
  while(SP_is_compound(a_zero_indecies_ref)){
    the_call_result = SP_get_list(a_zero_indecies_ref,the_head_ref,a_zero_indecies_ref);
    if(the_call_result==0)
      return ERR_PROLOG;
    the_call_result = SP_get_integer(the_head_ref,&the_index);
    if(the_call_result==0)
      return ERR_PROLOG;
    set_cell_value(0.0,(int)the_index,the_table);
  }

  /* Return success */
  *a_table = the_table;
  return 0;
}


/* Return the number of cells with value zero 
 * ===========================================
 */

int get_no_zero_indecies(table a_table)
{
  int the_true_no_zeroes,the_false_no_zeroes;

  if(a_table->cell_part.type==CELL){
    if(a_table->cell_part.value==0.0)
      return 1;
    else
      return 0;
  }else{
    the_true_no_zeroes = get_no_zero_indecies(a_table->table_part.true_table);
    the_false_no_zeroes = get_no_zero_indecies(a_table->table_part.false_table);
    return(the_true_no_zeroes+the_false_no_zeroes);
  }
}


/* Set indecies for cells of value zero 
 * for the given index number.
 * Set the last index number used.
 *====================================
 */ 

void set_zero_indecies_from_to(int a_next_available_index, int *a_last_used_index,int a_dimension_base,
			       int a_no_dimensions,table a_table,int *some_indecies)
{
  int the_no_dimensions,the_dimension_count,the_next_available_index,the_last_used_index,the_dimension_base;
  the_no_dimensions = get_no_dimensions(a_table);

  if(a_table->cell_part.type==CELL){
    if(a_table->cell_part.value==0.0){
      some_indecies[a_next_available_index] = a_dimension_base;
      a_next_available_index++;
      *a_last_used_index = a_next_available_index;
    }else{
      *a_last_used_index = a_next_available_index;
    }
  }else{
    the_dimension_base = 1;
    for(the_dimension_count=0;the_dimension_count<a_no_dimensions-1;the_dimension_count++){
      the_dimension_base *= 2;
    }
    set_zero_indecies_from_to(a_next_available_index,&the_next_available_index,a_dimension_base,
			      a_no_dimensions-1,a_table->table_part.true_table,some_indecies);
    the_dimension_base += a_dimension_base;
    set_zero_indecies_from_to(the_next_available_index,&the_last_used_index,the_dimension_base,
			      a_no_dimensions-1,a_table->table_part.false_table,some_indecies);    
    *a_last_used_index = the_last_used_index;
  } 
}

/* Set the indecies for all cells of value zero 
 * =============================================
 */

void set_zero_indecies(table a_table,int *some_indecies)
{
  int the_no_dimensions,the_dimension_count,the_next_available_index,the_last_used_index,the_dimension_base;

  the_no_dimensions = get_no_dimensions(a_table);
  the_dimension_base = 1;
  for(the_dimension_count=0;the_dimension_count<the_no_dimensions-1;the_dimension_count++){
    the_dimension_base *= 2;
  }
  set_zero_indecies_from_to(0,&the_next_available_index,0,the_no_dimensions-1,
			    a_table->table_part.true_table,some_indecies);
  set_zero_indecies_from_to(the_next_available_index,&the_last_used_index,the_dimension_base,
			    the_no_dimensions-1,a_table->table_part.false_table,some_indecies);
}


/* Find the indecies of the cells with value zero
 * ===============================================
 */
 
int get_zero_indecies(table a_table,int **some_zero_indecies)
{
  int the_no_zero_indecies;
  int *the_zero_indecies;

  the_no_zero_indecies = get_no_zero_indecies(a_table);

  the_zero_indecies = Calloc(the_no_zero_indecies,int)
  if(the_zero_indecies == NULL)
    return 2;
  
  set_zero_indecies(a_table,the_zero_indecies);

  /* Return success */
  *some_zero_indecies = the_zero_indecies;
  return 0;
}


/* Increment the cells in a table according to the satisfaction of clause literals
 * ================================================================================
 */

void increment_literal_cells(int a_no_literals,int *some_literals_satisfaction,tcube *a_clause,
			      table a_table)
{
  int the_literal_count,the_base_count,the_literal_base,the_cell_index;

  the_cell_index=0;
  for(the_literal_count=0;the_literal_count<a_no_literals;the_literal_count++){
    the_literal_base = 0;
    if (some_literals_satisfaction[the_literal_count]==0) {
	the_literal_base = 1;
	for(the_base_count=0;the_base_count<(a_no_literals-1)-the_literal_count;the_base_count++)
	  the_literal_base *= 2;
      }
    the_cell_index += the_literal_base;
  }
  increment_cell_value_1(the_cell_index,a_table);
}

/* Set all values in the table less than the threshold value to zero
 * ==================================================================
 */

void  lt_threshold_to_zero(table a_table)
{
    if(a_table->cell_part.type==CELL){
      if(a_table->cell_part.value<FITTING_THRESHOLD){
         a_table->cell_part.value=0.0; 
      }
    }else{
      lt_threshold_to_zero(a_table->table_part.true_table);
      lt_threshold_to_zero(a_table->table_part.false_table);
    } 
}











