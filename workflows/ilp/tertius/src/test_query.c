
#include <stdio.h>
#include "types.h"
#include "input_output.h"
#include "search.h"
#include "background.h"

long the_memory;
long the_max_memory = 100000;
int the_max_var;
int the_max_lit;
int the_global_stop;
int the_ask_more_memory_option;


main(int argc, char *argv[])
{
FILE *the_predicates_file;
FILE *the_facts_file;
FILE *the_clause_file;
tcube *the_clause;
tlist *the_node;

the_max_var = 10;
the_max_lit = 10;
unification_form = IMPLICIT;
if ((the_predicates_file = fopen("default.prd","r")) == NULL)
    error_deal_with(1, 1);
if ((the_facts_file = fopen("default.fct","r")) == NULL)
    error_deal_with(1, 1);
error_deal_with(1,read_predicates(the_predicates_file));
error_deal_with(1,read_facts(the_facts_file));
write_predicates(stdout);
write_types(stdout);

error_deal_with(1,initialise_prolog("default"));

if ((the_clause_file = fopen("default.cla","r")) == NULL)
    error_deal_with(1, 1);
error_deal_with(1,read_clause(the_clause_file,&the_clause));
the_node = Malloc(tlist)
if (the_node != NULL)
  {
    the_node->elt = (void*)the_clause;
    the_node->next = NULL;
    write_clauses_normal(stdout,the_node);
  }

eval_cube(the_clause,0,NULL);
write_clauses_normal(stdout,the_node);

}
