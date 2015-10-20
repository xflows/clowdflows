
extern int the_nb_predicates;
extern tpredicate *the_predicate_table;
extern int the_nb_types;
extern ttype *the_type_table;
extern int the_nb_parts;
extern int the_num_discretised_types;
extern tdiscretised_type *the_discretised_types_table;
extern FILE *the_input_file;

int first_partition();
int next_partition(int a_part);
int last_partition(int a_part);
int read_predicates(FILE *a_predicates_file);
int write_predicates(FILE *an_output_file);
int write_types(FILE *an_output_file);
int read_facts(FILE *a_facts_file,int status);
int read_clause(FILE *a_clause_file,tcube **a_clause);
int meta_fact_name_store(tpredicate *a_predicate,int *a_num_param,int *a_num_arg,int a_nb_parts,char *a_name_p,
			 int status,int *an_instance);
int fact_name_store(int a_type_nb, int a_part, char *a_name, int status, int *a_const_nb);
int store_instance(int *an_instance,int a_sign,tpredicate *a_predicate,int a_nb_parts,int a_part,int status);
int create_subpredicate(tsubpredicate **a_subpredicate,tpredicate *a_predicate,int a_nb_parts);
int read_configuration_file(FILE *a_file,int *an_argc,char ***an_argv);
int read_clauses(FILE *a_clause_file,tlist **a_list_clauses);
int initialise_global_types();
int write_facts(FILE *an_output_file);
