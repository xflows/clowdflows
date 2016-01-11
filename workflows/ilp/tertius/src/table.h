
typedef struct marginal_node *marginal_reference;

typedef struct marginal_node{
  int no_dimensions;
  int *marginal_indecies;
}mnode;

typedef struct d1_marginal_set_node *d1_marginal_set;

typedef struct d1_marginal_set_node{
  int no_marginals;
  float *marginal_values;
}d1node;

int new_table(int a_no_dimensions,table* a_table);

int new_table_0(int a_no_dimensions,table* a_table);

void free_table(int a_no_dimensions,table a_table);

void print_table(table a_table);

int get_no_dimensions(table a_table);

void set_cell_value(float value,int an_index,table a_table);

void increment_cell_value_1(int an_index,table a_table);

float get_cell_value(int an_index,table a_table);

float get_d1_marginal(int a_marginal_index,table a_table);

float get_population(table a_table);

int new_d1_marginal_set(int a_no_dimensions,d1_marginal_set* a_d1_marginal_set);

void free_d1_marginal_set(d1_marginal_set a_d1_marginal_set);

int get_no_dimensions_d1_marginal_set(d1_marginal_set a_d1_marginal_set);

void set_d1_marginal_value(float a_value,int a_d1_marginal_index,d1_marginal_set a_d1_marginal_set);

void print_d1_marginal_set(d1_marginal_set a_d1_marginal_set);

float get_population_d1_marginal_set(d1_marginal_set a_d1_marginal_set);

int get_d1_marginal_set(table a_table,d1_marginal_set* a_d1_marginal_set);

int get_expected_values(d1_marginal_set a_d1_marginal_set,table* a_table);

void adjust_cell_values(float an_adjustment,int a_d1_marginal_index,table a_table);

void fit_marginals(d1_marginal_set a_d1_marginal_set,table a_table);

void fit_cycle(int a_no_marginals,d1_marginal_set some_marginals,table a_table,float* a_delta);

int make_c_table(SP_term_ref a_table_ref,int a_no_dimensions,table* a_table);

int get_no_zero_indecies(table a_table);

int get_zero_indecies(table a_table,int **some_zero_indecies);

void increment_literal_cells(int a_no_literals,int *some_literals_satisfaction,tcube *a_clause,
			     table a_table);

void  lt_threshold_to_zero(table a_table);
