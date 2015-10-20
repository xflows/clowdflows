typedef struct elem_s *var_list;

typedef struct elem_s {
  SP_term_ref term;
  char name;
  var_list next;
} element;

int SP_print_term(SP_term_ref term);

var_list new_var_list(void);

int on_var_list(SP_term_ref term,var_list list);

int add_to_var_list(SP_term_ref *term,var_list list);

char get_var_name(SP_term_ref term,var_list list);




