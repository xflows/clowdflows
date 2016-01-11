
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "types.h"
#include "memory.h"
#include "gestion.h"
#include "eval.h"

int the_seed;

double get_balanced_accuracy(double FPR, double TPR, long P, long N) {
  return  (1  - FPR + TPR) / 2;
}

double get_recall(double FPR, double TPR, long P, long N) {
  /* !!! We assume the first class, denoted as positive in 1BCs, is actually the negative !!! */
  return  1  - FPR;
}

double get_precision(double FPR, double TPR, long P, long N) {
  /* !!! We assume the first class, denoted as positive in 1BCs, is actually the negative !!! */
  if (2-FPR-TPR > 0)
    return (1-FPR)*N / ((1-FPR)*N + (1-TPR)*P);
  else
    return 1;
}

double get_F_measure(double FPR, double TPR, long P, long N) {
  /* !!! We assume the first class, denoted as positive in 1BCs, is actually the negative !!! */
  double precision = get_precision(FPR,TPR,P,N);
  double recall = get_recall(FPR,TPR,P,N);
  if ( precision+recall > 0)
    return 2*precision*recall / (precision + recall);
  else
    return 0;
}

double get_value(long TP, long TN, long P, long N) {
  double accuracy, recall,precision,F_measure,balanced;
  long FP,FN;
  /* accuracy */
  accuracy = ( TP + TN ) / ( (double)N + (double)P );
  /* balanced accuracy */
  balanced = ( TP / (double)P + TN / (double)N) / 2;
  /* f-measure */
  FN = P - TP;
  FP = N - TN;
  /* !!! We assume the first class, denoted as positive in 1BCs, is actually the negative !!! */
  recall = TN / (double)N;
  if (TN+FN > 0) {
    precision = TN / ((double)TN + (double)FN);
    if (precision + recall > 0)
      F_measure = 2*precision*recall / (precision + recall);
    else
      F_measure = 0;
  } /*if (TN+FN > 0)*/
  else
    F_measure = 0;
  return balanced;
}

int append(tlist *a_list, tlist *another, tlist **a_result)
{
tlist *the_pointer;
tlist *the_result;

if (a_list == NULL)
  the_result = another;
else
  {
    the_result = a_list;
    the_pointer = a_list;
    while (the_pointer->next != NULL)
      the_pointer = the_pointer->next;
    the_pointer->next = another;
  }
*a_result = the_result;
return 0;
}

int quicksort(tlist *a_list, int (a_function)(tlist *,tlist *),tlist **a_sorted_list)
{
int the_error = 0;
tlist *the_elt,*the_next,*the_less,*the_greater;
tlist *the_sorted_less,*the_sorted_greater;

if (a_list == NULL)
  *a_sorted_list = NULL;
else
  {
    the_less = NULL;
    the_greater = NULL;
    the_elt = a_list->next;
    while (the_elt != NULL)
      {
	the_next = the_elt->next;
	if (a_function(the_elt,a_list))
	  {
	    the_elt->next = the_greater;
	    the_greater = the_elt;
	  }
	else
	  {
	    the_elt->next = the_less;
	    the_less = the_elt;
	  }
	the_elt = the_next;
      }
    a_list->next = NULL;
    quicksort(the_less,a_function,&the_sorted_less);
    append(a_list,the_sorted_less,&the_less);
    quicksort(the_greater,a_function,&the_sorted_greater);
    append(the_sorted_greater,the_less,a_sorted_list);
  }

return the_error;
}

int compare_roc(tlist *a_first,tlist *a_second)
{
  if (  (((troc *)(a_first->elt))->probabilities[0] - ((troc *)(a_first->elt))->probabilities[1])
      > (((troc *)(a_second->elt))->probabilities[0] - ((troc *)(a_second->elt))->probabilities[1]))
    return 1;
  else
    return 0;
}

int compare_roc_value(tlist *a_first,tlist *a_second)
{
  if (  ((troc *)(a_first->elt))->value < ((troc *)(a_second->elt))->value)
    return 1;
  else
    return 0;
}

double generate_roc(FILE *a_file,tlist *a_list,double *a_roc_weights,FILE *a_roc_file,FILE *a_roc_file1,FILE *a_roc_file2,FILE *a_roc_file3)
{
  double auc,x,y;
  troc *the_roc;
  tlist *the_elt;
  int first,last;
  double previous,a0,x1,y1,y0,p,n,xp,yp,xpp,ypp,current;
  long N,the_roc_positives,the_roc_negatives;

  the_roc_positives = 0;
  the_roc_negatives = 0;
  the_elt = a_list;
  while (the_elt != NULL) {
    if (((troc*)(the_elt->elt))->class == 0)
      the_roc_positives++;
    else
      the_roc_negatives++;
    the_elt = the_elt->next;
  }
  previous = -HUGE_VAL; /* 4/9/2007 : negation */
  x = 0;
  xp = 0;
  yp = 0;
  y = 0;
  xpp = 0;
  ypp = 0;
  auc = 0;
  first = TRUE;
  last = FALSE;
  the_elt = a_list;
  while ((the_elt != NULL)||(last == FALSE)) {
    if (the_elt != NULL) {
      the_roc = (troc *)(the_elt->elt);
      if (the_roc->class == 0)
	y += 1 / (double)the_roc_positives;
      else {
	x += 1 / (double)the_roc_negatives;
      } /* else */
      current = the_roc->probabilities[0] - the_roc->probabilities[1];
    } /* the_elt != NULL */
    else {
      current = -HUGE_VAL;
    }
    if (current != previous) { /* new point */
      if ((first==TRUE) && (current <= a_roc_weights[1])) { /*   changed < into <= on 02/06/08 to print if threshold == -Inf */
	first = FALSE;
	fprintf(a_roc_file1,"%f %f\n",(double)0,yp);
	fprintf(a_roc_file1,"%f %f\n",(double)1,yp);
	fprintf(a_roc_file2,"%f %f\n",xp,(double)0);
	fprintf(a_roc_file2,"%f %f\n",xp,(double)1);
	a0 = (yp*the_roc_positives+(1-xp)*the_roc_negatives)/(the_roc_positives+the_roc_negatives);
	N = the_roc_positives + the_roc_negatives;
	p = the_roc_positives / (double)N;
	n = the_roc_negatives / (double)N;
	x1 = 1 - (a0 - p) / n;
	y0 = (a0 - n) / p;
	y1 = a0 / p;
	if (x1 <= 1)
	  fprintf(a_roc_file3,"%f %f\n",x1,(double)1);
	else
	  fprintf(a_roc_file3,"%f %f\n",(double)1,y1);
	fprintf(a_roc_file3,"%f %f\n",(double)0,y0);
	fprintf(stdout,"x=%f y=%f P=%ld N=%ld (y P + (1 -x) N) / (P + N) = %f balanced=%f recall=%f precision=%f F-measure=%f\n",xp,yp,the_roc_positives,the_roc_negatives,a0,
		get_balanced_accuracy(xp,yp,the_roc_positives,the_roc_negatives),
		get_recall(xp,yp,the_roc_positives,the_roc_negatives),
		get_precision(xp,yp,the_roc_positives,the_roc_negatives),
		get_F_measure(xp,yp,the_roc_positives,the_roc_negatives));
	if (a_file != NULL)
	  fprintf(a_file,"x=%f y=%f P=%ld N=%ld (y P + (1 -x) N) / (P + N) = %f balanced=%f recall=%f precision=%f F-measure=%f\n",xp,yp,the_roc_positives,the_roc_negatives,a0,
		  get_balanced_accuracy(xp,yp,the_roc_positives,the_roc_negatives),
		  get_recall(xp,yp,the_roc_positives,the_roc_negatives),
		  get_precision(xp,yp,the_roc_positives,the_roc_negatives),
		  get_F_measure(xp,yp,the_roc_positives,the_roc_negatives));
      }
      fprintf(a_roc_file,"%f %f %f\n",xp,yp,current);
      /*fprintf(a_roc_file,"%s\t%d %f %f %f %d\n",the_roc->individual,the_roc->fold,x,y,the_roc->probability,the_roc->class);*/
      auc += (xp - xpp) * (yp + ypp) / 2;
      xpp = xp;
      ypp = yp;
    }
    xp = x;
    yp = y;
    previous = current;
    if (the_elt != NULL)
      the_elt = the_elt->next;
    else
      last = TRUE;
    /*    Free(the_roc)*/
  } /* (the_elt != NULL) */
  return auc;
}

double find_best_roc_weight(tlist *a_list,long a_num_positives,long a_num_negatives,int a_class,double *a_best_value)
{
  long xpos,xneg/*,the_best_accuracy*/;
  troc *the_roc;
  tlist *the_elt;
  double previous,best,current;
  double the_best_value;

  previous = -HUGE_VAL;
  xpos = 0;
  xneg = a_num_negatives;
  /*  the_best_accuracy = a_num_negatives;*/
  the_best_value = get_value(xpos,xneg,a_num_positives,a_num_negatives);
  best = previous;
  the_elt = a_list;
  while (the_elt != NULL) {
    the_roc = (troc *)(the_elt->elt);
    /*    	fprintf(stdout,"%f\t%d\t%s\n",the_roc->value,the_roc->class,the_roc->individual);*/
    current = the_roc->value;
    if (current != previous) { /* new point */
      /*      fprintf(stdout,"xpos: %ld, xneg: %ld, %lf\n",xpos,xneg,the_roc->value);*/
      /*      if ( (xpos+xneg) > the_best_accuracy) { */
      if (get_value(xpos,xneg,a_num_positives,a_num_negatives) > the_best_value) { /* better point */
	/*	the_best_accuracy = xpos + xneg;*/
	the_best_value = get_value(xpos,xneg,a_num_positives,a_num_negatives);
	best = (previous + current) / 2;
	/*	fprintf(stdout,"best: %f\n",best);*/
      } /* better point */
    } /* new point */
    if (the_roc->class == a_class)
      xpos += 1;
    else
      xneg -= 1;
    previous = current;
    the_elt = the_elt->next;
  } /* (the_elt != NULL) */
  /* last point */
  /*      if ( (xpos+xneg) > the_best_accuracy) { */
  if (get_value(xpos,xneg,a_num_positives,a_num_negatives) > the_best_value) { /* better point */
    /*	the_best_accuracy = xpos + xneg;*/
    the_best_value = get_value(xpos,xneg,a_num_positives,a_num_negatives);
    best = (current + HUGE_VAL) / 2;
  } /* better point */
  
  *a_best_value = the_best_value;
  fprintf(stdout,"Best: %f\n",best);
  return best;
}

int compare_count_class(tlist *a_first,tlist *a_second)
{
  if (  ((tcount_class *)(a_first->elt))->count > ((tcount_class *)(a_second->elt))->count)
    return 1;
  else
    return 0;
}

void delete_roc_list(tlist *a_list)
{
  tlist *the_elt;

  the_elt = a_list;
  while (the_elt != NULL) {
    Free((troc*)(the_elt->elt))
    the_elt = the_elt->next;
  }
}

int shuffle_classes(int a_seed, int a_num_classes, int **a_table, int **inverted_table)
{
  int the_error = 0;
  int *t,*inv_t;
  int i,j;

  t = Calloc(a_num_classes,int)
  if (t == NULL) return 2;
  inv_t = Calloc(a_num_classes,int)
  if (inv_t == NULL) return 2;
  for (i=0;i<a_seed;i++)
    j = random_long(a_num_classes-1);
  for (i=0;i<a_num_classes;i++)
    t[i] = -1;
  for (i=0;i<a_num_classes;i++) {
    j = random_long(a_num_classes-1);
    while (t[j] != -1)
      if (j+1 < a_num_classes)
	j += 1;
      else
	j = 0;
    t[j] = i;
  }
  for (i=0;i<a_num_classes;i++)
    inv_t[t[i]] = i;
  *a_table = t;
  *inverted_table = inv_t;
  return the_error;
}

int sort_classes(tlist *a_list, int a_num_classes, int **a_table, int **inverted_table)
{
  int the_error = 0;
  int *t,*inv_t;
  long *the_counts;
  tlist *the_elt,*the_classes;
  int the_class;
  tcount_class *the_pt;

  t = Calloc(a_num_classes,int)
  if (t == NULL) return 2;
  inv_t = Calloc(a_num_classes,int)
  if (inv_t == NULL) return 2;
  the_counts = Calloc(a_num_classes,long)
  if (the_counts == NULL) return 2;
  the_elt = a_list;
  while (the_elt != NULL) {
    the_counts[((troc*)(the_elt->elt))->class] += 1;
    the_elt = the_elt->next;
  }
  the_classes = NULL;
  for (the_class=0;the_class<a_num_classes;the_class++) {
    the_pt = Malloc(tcount_class)
    if (the_pt == NULL) return 2;
    the_elt = Malloc(tlist)
    if (the_elt == NULL) return 2;
    the_pt->class = the_class;
    the_pt->count = the_counts[the_class];
    the_elt->elt = (void*)the_pt;
    the_elt->next = the_classes;
    the_classes = the_elt;
  } /* for each class */
  quicksort(the_classes,compare_count_class,&the_classes);
  the_elt = the_classes;
  the_class = 0;
  while (the_elt != NULL) {
    t[the_class] = ((tcount_class*)(the_elt->elt))->class;
    Free(((tcount_class*)(the_elt->elt)))
    the_class++;
    the_elt = the_elt->next;
  }
  delete_list(the_classes);
  Free_calloc(a_num_classes,the_counts)
  for (the_class=0;the_class<a_num_classes;the_class++)
    inv_t[t[the_class]] = the_class;
  *a_table = t;
  *inverted_table = inv_t;
  return the_error;
}

int find_good_roc_weights(tlist **a_list,double *a_roc_weights,int a_num_classes,double *a_best_value)
{
  int the_error = 0;
  tlist *the_elt,*remaining,*interesting,*uninteresting,*misclassified,*next;
  int the_current_class;
  long the_num_pos, the_num_neg;
  double the_best_value;
  troc *the_roc;
  int *t,*inv_t;

  the_error = sort_classes(*a_list,a_num_classes,&t,&inv_t);
    /*shuffle_classes(the_seed,a_num_classes,&t,&inv_t);*/
  if (the_error == 0) {
    a_roc_weights[t[0]] = 0;
    the_elt = *a_list;
    while (the_elt != NULL) {
      ((troc *)(the_elt->elt))->predicted = t[0];
      the_elt = the_elt->next;
    } /* (the_elt != NULL) */
    misclassified = NULL;
    remaining = *a_list;
    for (the_current_class=1;the_current_class<a_num_classes;the_current_class++) {
      uninteresting = NULL;
      interesting = NULL;
      the_num_pos = 0;
      the_num_neg = 0;
      the_elt = remaining;
      while (the_elt != NULL) {
	the_roc = (troc*)(the_elt->elt);
	next = the_elt->next;
	if (the_roc->class == t[the_current_class]) {
	  the_roc->value = a_roc_weights[the_roc->predicted] + the_roc->probabilities[the_roc->predicted] - the_roc->probabilities[t[the_current_class]];
	  /*	fprintf(stdout,"%f\t%d\t%s\n",the_roc->value,the_roc->class,the_roc->individual);*/
	  the_elt->next = interesting;
	  interesting = the_elt;
	  the_num_pos += 1;
	} /* (the_roc->class == t[the_current_class]) */
	else if (inv_t[the_roc->class] < the_current_class) {
	  the_roc->value = a_roc_weights[the_roc->class] + the_roc->probabilities[the_roc->class] - the_roc->probabilities[t[the_current_class]];
	  /*	fprintf(stdout,"%f\t%d\t%s\n",the_roc->value,the_roc->class,the_roc->individual);*/
		/*if (strcmp(the_roc->individual,"e64") == 0)
		the_roc->value = the_roc->value;*/
	  the_elt->next = interesting;
	  interesting = the_elt;
	  the_num_neg += 1;
	} /* (inv_t[the_roc->class] < the_current_class) */
	else { /* (inv_t[the_roc->class] > the_current_class) */
	  the_elt->next = uninteresting;
	  uninteresting = the_elt;
	} /* (inv_t[the_roc->class] > the_current_class) */
	the_elt = next;
      } /* for each elt of remaining */
      quicksort(interesting,compare_roc_value,&interesting);
      a_roc_weights[t[the_current_class]] = find_best_roc_weight(interesting,the_num_pos,the_num_neg,t[the_current_class],&the_best_value);
      the_elt = uninteresting;
      while (the_elt != NULL) {
	the_roc = (troc*)(the_elt->elt);      
	if ((a_roc_weights[t[the_current_class]] + the_roc->probabilities[t[the_current_class]])
	    > (a_roc_weights[the_roc->predicted] + the_roc->probabilities[the_roc->predicted]))
	the_roc->predicted = t[the_current_class];
	the_elt = the_elt->next;
      } /* for each elt of uninteresting */
      the_elt = interesting;
      while (the_elt != NULL) {
	next = the_elt->next;
	the_roc = (troc*)(the_elt->elt);      
	if (the_roc->class == t[the_current_class]) {
	  if (the_roc->value < a_roc_weights[t[the_current_class]]) {
	    the_roc->predicted = t[the_current_class];
	    the_elt->next = uninteresting;
	    uninteresting = the_elt;
	  } /* correctly classified in current class */
	  else { /* misclassified in predicted */
	    the_elt->next = misclassified;
	    misclassified = the_elt;
	  }
	} /* (the_roc->class == t[the_current_class]) */
	else { /* (the_roc->class != t[the_current_class]) */
	  if (the_roc->value > a_roc_weights[t[the_current_class]]) {
	    the_roc->predicted = the_roc->class;
	    the_elt->next = uninteresting;
	    uninteresting = the_elt;
	  } /* correctly classified in its class */
	  else { /* misclassified in current class */
	    the_roc->predicted = t[the_current_class];
	    the_elt->next = misclassified;
	    misclassified = the_elt;
	  }
	} /* (the_roc->class != t[the_current_class]) */
	the_elt = next;
      } /* for each elt of interesting */
      remaining = uninteresting;
    } /* for each class, i.e. each weight */
    append(remaining,misclassified,a_list);
    /*    *a_best_accuracy = the_best_accuracy / ((double)the_num_pos + (double)the_num_neg);*/
    *a_best_value = the_best_value;
    Free_calloc(a_num_classes,t)
    Free_calloc(a_num_classes,inv_t)
  } /* (the_error == 0) */
  return the_error;
}

int initialise_roc_weights(double **a_roc_weights,int a_num_classes)
{
  int the_error = 0;
  int i;

  *a_roc_weights = Calloc(a_num_classes,double)
  if (*a_roc_weights == NULL) the_error = 2;
  for (i=0;i<a_num_classes;i++)
      (*a_roc_weights)[i] = 0;

  return the_error;
}

void print_roc_weights(FILE *a_file,double *a_roc_weights,int a_num_classes)
{
  int i;

  for (i=0;i<a_num_classes;i++)
    fprintf(a_file,"%f ",a_roc_weights[i]);
  fprintf(a_file,"\n");
}

int eval_roc_weights(tlist *a_list,double *a_roc_weights,int a_num_classes,double *best_score,double *best_roc_weights)
{
  int the_error = 0;
  double the_score,the_max;
  int i,the_predicted_class;
  tlist *the_elt;
  troc *the_roc;

  the_score = 0;
  the_elt = a_list;
  while (the_elt != NULL) {
    the_roc = (troc*)(the_elt->elt);
    the_max = the_roc->probabilities[0];
    the_predicted_class = 0;
    for (i= 1;i<a_num_classes;i++) {
      if (the_max < a_roc_weights[i] + the_roc->probabilities[i]) {
	the_max = a_roc_weights[i] + the_roc->probabilities[i];
	the_predicted_class = i;
      }
    } /* for each class i */
    if (the_roc->class == the_predicted_class)
      the_score += 1;
    the_elt = the_elt->next;
  } /* while (the_elt != NULL) */

  if (the_score > *best_score) {
    *best_score = the_score;
    for (i=1;i<a_num_classes;i++)
      best_roc_weights[i] = a_roc_weights[i];
  }
  return the_error;
}
int compare_weights(tlist *a_first,tlist *a_second)
{
  if (  *((double *)(a_first->elt)) < *((double *)(a_second->elt)))
    return 1;
  else
    return 0;
}

int explore_roc_weights(tlist *a_list,double *a_roc_weights,int a_num_classes,int a_class,double *best_score,double *best_roc_weights,double low,double high)
{
  int the_error = 0;
  double the_initial_weight;
  troc *the_roc;
  tlist *the_instance,*the_roc_list,*the_roc_elt,*the_next;
  troc *the_new_roc;
  double the_max,the_current_value;
  int i;
  
  the_initial_weight = a_roc_weights[a_class];
  if (a_class < a_num_classes - 1)
    explore_roc_weights(a_list,a_roc_weights,a_num_classes,a_class+1,best_score,best_roc_weights,the_initial_weight,the_initial_weight);
  else
    eval_roc_weights(a_list,a_roc_weights,a_num_classes,best_score,best_roc_weights);

  /* create roc list */
  the_roc_list = NULL;
  the_instance = a_list;
  while (the_instance != NULL) {
    the_roc_elt = Malloc(tlist)
    if (the_roc_elt == NULL) return 2;
    the_new_roc = Malloc(troc)
    if (the_new_roc == NULL) return 2;
    the_new_roc->probabilities = Calloc(a_num_classes,double)
    if (the_new_roc->probabilities == NULL) return 2;
    the_roc = (troc*)(the_instance->elt);
    the_max = the_roc->probabilities[0];
    the_new_roc->predicted = 0;
    for (i= 0;i<a_num_classes;i++)
       the_new_roc->probabilities[i] = the_roc->probabilities[i];
    for (i= 1;i<a_class;i++) {
      if (the_max < a_roc_weights[i] + the_roc->probabilities[i]) {
	the_max = a_roc_weights[i] + the_roc->probabilities[i];
	the_new_roc->predicted = i;
      }
    } /* for each class i */
    the_new_roc->value = the_max - the_roc->probabilities[a_class];
    the_new_roc->class = the_roc->class;
    the_new_roc->individual = the_roc->individual;
    the_roc_elt->elt = (void*)the_new_roc;
    the_roc_elt->next = the_roc_list;
    the_roc_list = the_roc_elt;
    the_instance = the_instance->next;
  } /* while (the_instance != NULL) */

  quicksort(the_roc_list,compare_roc_value,&the_roc_list);
  
  the_roc_elt = the_roc_list;
  while (the_roc_elt != NULL) {
    if (a_class == 2) {
      the_roc = (troc*)(the_roc_elt->elt);
      if (the_roc->probabilities[0] > a_roc_weights[1] + the_roc->probabilities[1]) {
	the_instance = the_roc_list;
	while (the_instance != NULL) {
	  the_new_roc = (troc*)(the_instance->elt);
	  if (the_new_roc->probabilities[0] < a_roc_weights[1] + the_new_roc->probabilities[1]) {
	    if ((the_roc->probabilities[0] - the_roc->probabilities[2] > low + the_new_roc->probabilities[1] - the_new_roc->probabilities[2])
		&& (the_roc->probabilities[0] - the_roc->probabilities[2] < high + the_new_roc->probabilities[1] - the_new_roc->probabilities[2]))
	      fprintf(stdout,"1\t%f\t%f\t%f\n%s\t%f\t%f\t%f\n%s\t%f\t%f\t%f\n\n",a_roc_weights[1],low,high,
		      the_roc->individual,the_roc->probabilities[0],the_roc->probabilities[1],the_roc->probabilities[2],
		      the_new_roc->individual,the_new_roc->probabilities[0],the_new_roc->probabilities[1],the_new_roc->probabilities[2]);
	  } /* (the_new_roc->probabilities[0] < a_roc_weights[1] + the_new_roc->probabilities[1]) */
	  the_instance = the_instance->next;
	} /* (the_instance != NULL) */
      } /* (the_roc->probabilities[0] > a_roc_weights[1] + the_roc->probabilities[1]) */
    } /* (a_class == 2) */
    the_current_value = ((troc*)(the_roc_elt->elt))->value;
    while ((the_roc_elt != NULL) && (the_current_value == ((troc*)(the_roc_elt->elt))->value))
      the_roc_elt = the_roc_elt->next;
    the_next = the_roc_elt;
    if (the_next != NULL) {
      a_roc_weights[i] = (the_current_value + ((troc*)(the_roc_elt->elt))->value) / 2;
      if (a_class < a_num_classes - 1)
	explore_roc_weights(a_list,a_roc_weights,a_num_classes,a_class+1,best_score,best_roc_weights,the_current_value,((troc*)(the_roc_elt->elt))->value);
      else
	eval_roc_weights(a_list,a_roc_weights,a_num_classes,best_score,best_roc_weights);
    }
    else {
      a_roc_weights[i] = the_current_value + 1;
      if (a_class < a_num_classes - 1)
	explore_roc_weights(a_list,a_roc_weights,a_num_classes,a_class+1,best_score,best_roc_weights,the_current_value + 1,the_current_value + 1);
      else
	eval_roc_weights(a_list,a_roc_weights,a_num_classes,best_score,best_roc_weights);
    }
    the_roc_elt = the_next;
  } /* while (the_roc_elt != NULL) */



  /* free roc list */
  the_roc_elt = the_roc_list;
  while (the_roc_elt != NULL) {
    Free_calloc(a_num_classes,((troc*)(the_roc_elt->elt))->probabilities)
    Free((troc*)the_roc_elt->elt)
    the_roc_elt = the_roc_elt->next;
  }
  delete_list(the_roc_list);

  a_roc_weights[a_class] = the_initial_weight;
  return the_error;
}

int find_best_roc_weights(tlist **a_list,double *a_roc_weights,int a_num_classes)
{
  int the_error = 0;
  tlist *the_elt;
  troc *the_roc;
  int i;
  double the_best_score,the_min;
  double *the_best_roc_weights;

  for (i=1;i<a_num_classes;i++) {
    the_elt = *a_list;
    the_roc = (troc*)(the_elt->elt);
    the_min = the_roc->probabilities[0] - the_roc->probabilities[i];
    the_elt = the_elt->next;
    while (the_elt != NULL) {
      the_roc = (troc*)(the_elt->elt);
      if (the_min > the_roc->probabilities[0] - the_roc->probabilities[i])
	the_min = the_roc->probabilities[0] - the_roc->probabilities[i];
      the_elt = the_elt->next;
    } /* while (the_elt != NULL) */
    a_roc_weights[i] = the_min - 1;
  } /* for each class i */
  the_error = initialise_roc_weights(&the_best_roc_weights,a_num_classes);
  if (the_error == 0) {
    the_best_score = -1;
    for (i=1;i<a_num_classes;i++)
      the_best_roc_weights[i] = a_roc_weights[i];
    the_error = explore_roc_weights(*a_list,a_roc_weights,a_num_classes,1,&the_best_score,the_best_roc_weights,1,1);
    for (i=1;i<a_num_classes;i++)
      a_roc_weights[i] = the_best_roc_weights[i];
    Free_calloc(a_num_classes,the_best_roc_weights)
  }
  return the_error;
}
