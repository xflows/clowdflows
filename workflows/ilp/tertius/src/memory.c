#include <stdio.h>
#include <stdlib.h>
#include "types.h"

void *my_malloc(size_t size)
{
  void *the_result;

  the_memory += size;
  if ((the_memory <= the_max_memory) && (! the_global_stop))
    the_result = malloc(size);
  else {
    the_memory -= size;
    the_result = NULL;
  }
  return the_result;
}

void *my_calloc(size_t n, size_t size)
{
  void *the_result;

  the_memory += n * size;
  if ((the_memory <= the_max_memory) && (! the_global_stop))
    the_result = calloc(n,size);
  else {
    the_memory -= n * size;
    the_result = NULL;
  }
  return the_result;
}


int my_double_recalloc(double **ptr, size_t oldn, size_t newn)
{
  int the_error = 0;
  double *the_new_ptr;
  size_t i;

  the_new_ptr = my_calloc(newn, sizeof(double));
  if (the_new_ptr != NULL) {
    for (i=0;i<oldn;i++)
      the_new_ptr[i] = (*ptr)[i];
    Free_calloc(oldn,*ptr)
    *ptr = the_new_ptr;
  }
  else
    the_error = 2;
  return the_error;
}
