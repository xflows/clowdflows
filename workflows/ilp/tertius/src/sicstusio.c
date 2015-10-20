#include </usr/local/sicstus/include/sicstus/sicstus.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct elem_s *var_list;

typedef struct elem_s {
  SP_term_ref term;
  char name;
  var_list next;
} element;

var_list new_var_list(void)
{
  var_list new_list = NULL; 
  return new_list;
}

int on_var_list(SP_term_ref term,var_list list)
{
  if(list == NULL){
    return 0;
  } else {
    if(!SP_compare(term,list->term)){
      return 1;
    } else {
      return on_var_list(term,list->next);
    }
  }
}

char next_var_name(var_list list)
{
  if(list==NULL){
    return 'A';
  } else {
    return list->name+1;
  }
}

var_list add_to_var_list(SP_term_ref term,var_list list)
{
  var_list new_list;

  new_list = (var_list)malloc(sizeof(element));
  new_list->term = SP_new_term_ref();

  SP_put_term(new_list->term,term);
  
  new_list->name = next_var_name(list);
  new_list->next = list;

  return new_list;
}
 
char get_var_name(SP_term_ref term,var_list list)
{
  if(list == NULL){
    return NULL;
  } else {
    if(!SP_compare(list->term,term)){
      return list->name;
    } else {
      return get_var_name(term,list->next);
    }
  }
}


/*=======================================================
 * SP_print_term
 *=======================================================
 */

var_list SP_print_arg(SP_term_ref term,var_list list)
{
  int arity,arg_count;
  long the_integer;
  unsigned long name;
  char *term_name;
  SP_term_ref functor,arg;

  functor = SP_new_term_ref();
  arg = SP_new_term_ref();

  if(SP_is_atom(term)){ 
    SP_get_string(term,&term_name);
    printf(term_name); 
  }else{ 
    if(SP_is_number(term)){
      SP_get_integer(term,&the_integer); printf("%i",(int)the_integer);
    } else { 
      if(SP_is_variable(term)){ 
	if(on_var_list(term,list)){
	  printf("%c",get_var_name(term,list)); 
	} else { 
	  list = add_to_var_list(term,list);
	  printf("%c",get_var_name(term,list)); 
	} 
      } else {
	SP_get_functor(term,&name,&arity);
	printf("%s(",SP_string_from_atom(name));
	for(arg_count=1;arg_count<=arity;arg_count++){ 
	  if(arg_count>1){
	    printf(","); 
	  } SP_get_arg(arg_count,term,arg); 
	  list = SP_print_arg(arg,list); 
	} 
	printf(")"); 
      } 
    } 
  } 
  return list; 
}

int SP_print_term(SP_term_ref term)
{
  int arity,arg_count;
  long the_integer;
  unsigned long name;
  char *term_name;
  SP_term_ref functor,arg;
  var_list list = new_var_list();

  functor = SP_new_term_ref();
  arg = SP_new_term_ref();

  if(SP_is_atom(term)){
    SP_get_string(term,&term_name);
    printf("%s\n",term_name);
  }else{
    if(SP_is_number(term)){
      SP_get_integer(term,&the_integer);
      printf("%i",(int)the_integer);
    } else {
      if(SP_is_variable(term)){
	printf("A\n");
      } else {
	SP_get_functor(term,&name,&arity);
	printf("%s(",SP_string_from_atom(name));
	for(arg_count=1;arg_count<=arity;arg_count++){
	  if(arg_count>1){
	    printf(",");
	  }
	  SP_get_arg(arg_count,term,arg);
	  list = SP_print_arg(arg,list);
	}
	printf(")\n");
      }
    }
  }
  return 1;
}















