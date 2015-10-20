
#include <stdio.h>
#include <stdlib.h>

/*****************************************************************************
Display errors from tertius
 *****************************************************************************/
void tertius_error_display(int a_number)
{
char *the_message;

switch (a_number) {
  case 1: the_message = "Cannot open file.";
      break;
  case 2: the_message = "Unable to allocate memory.";
      break;
  case 3: the_message = "Error in parsing.";
      break;
  case 4: the_message = "More predicates than NBPREDS.";
      break;
  case 5: the_message = "More types than NBTYPES.";
      break;
  case 6: the_message = "A fact makes use of an undefined predicate.";
      break;
  case 7: the_message = "The maximum number of variables is lower than the arity of individuals: the arity is used instead.";
      break;
  case 8: the_message = "No fact is supposed to be associated to individuals. They are disregarded.";
      break;
  case 9: the_message = "The arguments are at least max_literals/max_variables, plus an optional stem for file names (otherwise the stem \"default\" is used) or the names of files containing predicate, facts, results, debugging information if verbose is used, and the starting clause if used.\nThe following options can be used before the arguments:\n-m and a number to change the max memory (default is 10 Mb)\n-e for a multi-dimensional estimate of the number of counter-examples (bi-dimensional by default)\n-n and a threshold for the percent of noise (0 by default)\n-ask to ask more memory to the user if needed (by default, tertius stops when the maximum memory is reached and give the current results if any)\n-u for explicit unification (unified variables are simply replaced by one of them by default)\n-[s|sr|S|SR] and the sampling ratio (no sampling by default)\n-breadth to use a breadth first search (best first search by default)\n-v verbose (the default is off)\n-nosubs to keep subsumed clauses (subsumed clauses are removed by default)\n-sat to consider satisfied clauses only (By default, clauses are given according to their confirmation only.)\n-b language bias: horn, class for rules predicting the first predicate only, pos_class for predicting only it as positive literal, horn_pos_class for both horn and pos_class options (no bias by default)\n-k and the number of results wanted (10 by default)\n-c and the confirmation threshold (10 best results are considered by default instead of setting a confirmation threshold.)\n-f and a threshold on the frequency (0 by default)\n-prolog use prolog, only available with appropriate files (the default is off)\n-[bg|pttp|ic] to use background knowledge to influence the expected value (bg: regular-resolution (prolog),pttp: linear-resolution (prolog technology theorem prover),ic: integrity-constraints)\n-i load partitions incrementaly, useful when the training set is too to be loaded in one go\n-struct to use structural properties\n-sc to start the search from a clause given in the appropriate file (usually stem.cla)\n-cbu to count instances in a bottom-up manner, generating substitutions incrementaly from examples instead of the default generate-then-test approach\n-avg with partionning, when each partition represents a separate dataset, the confirmations of partitions are averaged. Off by default: the cells of the contingency tables for each partition are added up into on global contingency table.-d an_attribute name, a number of intervals the attribute has to be discretised in, and a kind of discretisation (sdm: standard deviation centered on the mean, eqb: equal bins).\n-bayes naive/optimal language/individual/first and the name of the predicate used as the class for the naive or optimal Bayes classifier\n-cross and the number of partition to apply a cross-validation on the dataset\n-srand and an integer for initialising the random seed\n-eval to evaluate the clause{s} in the .cla file\n-w in the Bayesian classifier to write first-order features with new predicates in the head to a .fof file\n-t and a threshold to select the features modifying the default distribution of the classes more than the threshold\n-r INDIVIDUAL/LANGUAGE for a recursive approach based on the first-order (list, multiset and set) distributions (i.e. pure 1BC2) / on-the-fly propositionalisation (i.e. emulated 1BC) in Bayesian classification\n-roc to generate roc curve\n-o a_number_of_folds to find the best threshold using an internal cross-validation according to roc curve for Bayesian classification\n-h to see this message";
      break;
  case 10: the_message = "A variable term ref has not been initialised.";
      break;
  case 11: the_message = "More substitutions than LONG_MAX.";
      break;
  case 12: the_message = "Error in substituted_term_ref.";
      break;
  case 13: the_message = "For each predicate, you must defined whether the closed world assumption is used (cwa) or negative facts are explicitly given (explicit).";
      break;
  case 14: the_message = "k_best should not be zero!";
      break;
  case 15: the_message = "Stem longer than STEMLENGTH.";
      break;
  case 16: the_message = "Error in Prolog interface.";
    break;
  case 17: the_message = "The clause makes use of the same variable for several types.";
      break;
  case 18: the_message = "The clause makes use of an undefined predicate.";
      break;
  case 19: the_message = "The clause makes use of more variables than the maximum number of variables allowed in the search.";
  case 20: the_message = "The clause makes use of more literals than the maximum number of literals allowed in the search.";
      break;
  case 21: the_message = "A type is sometimes groundable, sometimes not !";
      break;
  case 22: the_message = "The clause makes use of a new set of parameters.";
      break;
  case 23: the_message = "The target predicate makes use of more variables than the individual.";
      break;
  case 24: the_message = "The first literal of a property should be the individual!";
      break;
  case 25: the_message = "No properties can be generated from the current settings!";
      break;
  case 26: the_message = "The number used in the cross-validation must be between 1 and the number of partitions!";
      break;
  case 27: the_message = "The target predicate doesn't match the individual!";
      break;
  case 28: the_message = "Structural predicates must be of arity 2!";
      break;
  case 29: the_message = "-bayes option must be followed by 'optimal' or 'naive', whether the decomposition is based on the 'individual', the 'language', or 'first'-order, then the name of the target predicate!";
      break;
  case 30: the_message = "-cbu option cannot be used yet with -u option: switching to an implicit representation of unification.";
      break;
 case 31: the_message = "Counting in a database only works with the normal expected value and an implicit representation of unification.";
      break;
 case 32: the_message = "Counting in a database only works with an individual predicate.";
      break;
 case 33: the_message = "The number of intervals of a discrete type must be greater than 0.";
      break;
 case 34: the_message = "The kind of structural predicate (bv,li,...) must be specified.";
      break;
 case 38: the_message = "This classifier can only deal with individuals defined by a single type.";
      break;
 case 39: the_message = "No value found for property of an individual.";
      break;
 case 40: the_message = "No related individual found for a structural predicate given an individual.";
      break;
 case 41: the_message = "A structural predicate cannot have parameters.";
      break;
 case 42: the_message = "Bit-Vector (bv) distribution is not available.";
      break;
 case 43: the_message = "New context during classification.";
      break;
 case 44: the_message = "Wrong name for the target predicate.";
      break;
  default: the_message = "Error is not defined.";
      break;
  }
fprintf(stderr, the_message);
}


/*****************************************************************************
Deals with errors
   1: from tertius
 *****************************************************************************/
void error_deal_with(int origin, int a_number)
{
if (a_number != 0)
    {
	switch (origin) {
	case 1:	tertius_error_display(a_number);
	    break;
	default: fprintf(stderr, "The origin %d of errors is not defined.\n",origin);
	    break;
	}
	fprintf(stderr, "\n");
	exit(0);
    }
}

/*****************************************************************************
Deals with warning
   1: from tertius
 *****************************************************************************/
void warning_deal_with(int origin, int a_number)
{
if (a_number != 0)
    {
	switch (origin) {
	case 1:	tertius_error_display(a_number);
	    break;
	default: fprintf(stderr, "The origin %d of errors is not defined.",origin);
	    break;
	}
	fprintf(stderr, "\n");
    }
}

