#define PROLOG 0
#define JAVA 0
#define DEBUG 0

#include "memory.h"
#if PROLOG
#include "/usr/local/sicstus/include/sicstus/sicstus.h"
#endif

extern long the_memory;
extern long the_max_memory;
extern int the_global_stop;
extern int the_ask_more_memory_option;
extern int debug;
extern long the_random_seed;

#define Malloc(type) (type *)my_malloc(sizeof(type));

#define Calloc(n,type) (type *)my_calloc((n),sizeof(type));

#define Free(object) {if (object != NULL) {free(object); the_memory -= sizeof(*(object));}}

#define Free_calloc(n,object) {if ((n!=0)&&(object != NULL)) {free(object); the_memory -= (n) * sizeof(*(object));}}

#define NBPREDS 6000
#define SIZEPRED 40
#define NBTYPES 200
#define SIZETYPE 40
#define NBCONSTS 100
#define SIZECONST 40
#define NBVARS 10
#define STEMLENGTH 200
#define GARBAGE_COLLECT_LIMIT 50
#define NBARGS 50

#define CONST 0
#define VAR 1
#define POS 1
#define NEG 0
#define NONE 0
#define HORN 1
#define CLASSIFICATION 2
#define POSITIVE_CLASSIFICATION 3
#define HORN_POSITIVE_CLASSIFICATION 4
#define FALSE 0
#define TRUE 1
#define NAIVE 0
#define NORMAL 1
#define BACKGROUND 2
#define LINEAR_RESOLUTION 3
#define INTEGRITY_CONSTRAINTS 4
#define EXPECTED 0
#define DIFFERENCE 1
#define EXPLICIT 0
#define IMPLICIT 1
#define BEST 0
#define BREADTH 1
#define FIRST 2
#define LANGUAGE 1
#define INDIVIDUAL 0
#define STRUCTURAL 1
#define PROPERTY 2
#define NAIVE 0
#define OPTIMAL 1
#define STORE 0
#define PREPARE 1
#define COLLECT_STATS 2
#define SDM 0
#define EQB 1
#define FFD 2
#define BV 0
#define LI 1
#define MS 2
#define SS 3
#define CLASSIFY 0
#define COUNT 1

#define ERR_PROLOG 16

#define TABLE 0
#define CELL 1

#define FITTING_THRESHOLD 0.00001

typedef union table_node *table;

typedef union table_node{
  struct{
    int type;
    float value;
  }cell_part;
  struct{
    int type;
    table true_table;
    table false_table;
  }table_part;
}tnode;

/* ADJUST READ_PREDICATE TO ANY CHANGE OF TTYPE */
typedef struct type {
  char *name;
  int global;
  int discretised;
  int *nb_const;
  char ***constants;
  int num_contexts; /* number of contexts */
  struct context **contexts; /* each contexts */
  long **num_individuals; /* number of individuals for each class and each context */
#if PROLOG
  SP_term_ref **term_ref;
#endif
} ttype;

typedef struct discretised_type {
  char *name;
  int num_inter;
  int kind; /*standard deviations around the mean (SDM) or equal bins (EQB)*/
  double mean;
  double sigma;
  double sum;
  double sum2;
  long num_values;
  long maxnumvalues; /*size of values array*/
  double *values; /*array of all values for equal bins*/
  double *thresholds; /*array of thresholds between intervals*/
  long maxnuminter; /* max number of intervals for FFD */
  int size; /* fixed number of values in each interval for FFD */
} tdiscretised_type;

typedef int *tinstance;

typedef struct subpredicate {
  int *val_params;
  int *nb_inst;
  tinstance **instances;
  int *nb_cinst;
  tinstance **cinstances;
  double **probabilities; /* for each context and each class */
  int *already_counted; /* for each context */
  double **num_individuals; /* for each context and each class, structural predicates only, in order to kow how many individuals are linked to another */
} tsubpredicate;

/* ADJUST COPY_PREDICATE AND READ_PREDICATE TO ANY CHANGE OF TPREDICATE */
typedef struct predicate {
  char *name;
  int kind;
  int structural_kind; /* kind of structure to use for Bayesian counting : BV, LI, ... */
#if PROLOG
  SP_term_ref term_ref;
#endif
  int arity;
  int num_occurence;
  int *types; /* params first then variables */
  int num_param;
  int *params;
  int *paramIndices;
  int *varIndices;
  int *dimensions;
  int cwa;
  int num_subpredicate; /*Number of different sets of parameters*/
  struct subpredicate **subpredicates; /*Subpredicate associated to each set of parameters*/
} tpredicate;

typedef struct arg {
  int type;
  int flip;
  int value;
#if PROLOG
  SP_term_ref term_ref;
#endif
} targ;

/* ADJUST add_target_literals_to_property TO ANY CHANGE OF TLITERAL */
typedef struct literal {
  int sign;
  int predicate;
  int subpredicate;
  int *args;
} tliteral;

typedef struct subst {
  int arg; /* the instantiated or unified variable */
  int flip; /* VAR or CONST */
  int value; /* the value to which it is instantiated or the variable to which it is unified */
  long nb_inst;
} tsubst;

typedef struct context {
  int num_lits;
  struct context *ancestors;
  tliteral *new_lit;
  int functional;
} tcontext;

typedef struct cube {
  int flag_instanciation;
  int first_var;
  int second_var;
  int nb_positive;
  int nb_lit;
  tliteral *lits;
  int nb_arg;
  targ *args;
  tliteral *exp_lits;
  int nb_exp_args;
  targ *exp_args;
  int *corresp_args;
  tsubst *substs;
  int nb_substs;
  long nb_inst;
  long nb_inst_positive;
  long nb_inst_negative;
  long capital_n;
  double observed;
  double optimistic;
  double confirmation;
  long num;
  int *blocks_table;
  int num_blocks;
  long *block_nb_inst;
/*   int structural_kind; for Bayesian computation */
/*   int property_predicate; for Bayesian computation too */
} tcube;

typedef struct list {
  void *elt;
  struct list *next;
} tlist;

typedef struct roc {
  double *probabilities;
  int class;
  char *individual;
  int predicted;
  double value;
} troc;

typedef struct count_class {
  int class;
  long count;
} tcount_class;





