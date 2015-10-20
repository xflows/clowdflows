#include "types.h"

int the_global_stop;
long the_memory,the_max_memory;

main (void)
{
  int the_call_result;
  FILE *the_temporary_file;

  /* Initiating Prolog */
  the_call_result = SP_initialize(0,NULL,"/usr/local/sicstus/bin/");
  if(the_call_result!=SP_SUCCESS)
    return ERR_PROLOG;
  
  /* Create temporary file */
  the_temporary_file = fopen("tertius_background_tmp.pl","w");
  if(the_temporary_file==NULL){
    return 1;
  }
  fprintf(":- dynamic colour/2.\n\n");
  fprintf("test:-colour(red,blue).\n");
  fprintf("test.");

  /* Fcompile prolog files */
  system("sicstus -f -l test_compile.pl > compile.log 2> compile.log");

  /* Load file 'test.ql' */
  the_call_result = SP_load("test.ql");
  if(the_call_result!=SP_SUCCESS)
    return ERR_PROLOG;
  
  /* Remove the test.ql file */
  the_call_result = remove("test.ql");
  if(the_call_result!=0){
    return 1;
  }

  /* Remove the test.pl file */
  the_call_result = remove("test.pl");
  if(the_call_result!=0){
    return 1;
  }

}
  




















