#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <math.h>
#include <signal.h>
#include "types.h"
#include "roc.h"

long the_memory;
long the_max_memory;
int the_global_stop;


int main (void)
{
  troc *roc1,*roc2,*roc3;
  tlist *elt1,*elt2,*elt3;
  int the_error,i;
  double *the_roc_weights;

  the_memory = 0;
  the_max_memory = 10000000;
  the_global_stop = 0;
  
  roc1 = Malloc(troc)
  roc2 = Malloc(troc)
  roc3 = Malloc(troc)
  roc1->probabilities = Calloc(3,double)
  roc2->probabilities = Calloc(3,double)
  roc3->probabilities = Calloc(3,double)
  roc1->probabilities[0] = -221; roc1->probabilities[1] = -224; roc1->probabilities[2] = -219;
  roc2->probabilities[0] = -214; roc2->probabilities[1] = -206; roc2->probabilities[2] = -185;
  roc3->probabilities[0] = -204; roc3->probabilities[1] = -195; roc3->probabilities[2] = -207;
  roc1->class = 0;
  roc2->class = 2;
  roc3->class = 1;
  elt1 = Malloc(tlist)
  elt2 = Malloc(tlist)
  elt3 = Malloc(tlist)
  elt1->elt = (void*)roc1;
  elt2->elt = (void*)roc2;
  elt3->elt = (void*)roc3;
  elt1->next = elt2;
  elt2->next = elt3;
  elt3->next = NULL;
  the_error = initialise_roc_weights(&the_roc_weights,3);
  find_good_roc_weights(&elt1,the_roc_weights,3);
  for (i=0;i<3;i++)
    fprintf(stdout,"%f ",the_roc_weights[i]);

  return 0;
}
  




















