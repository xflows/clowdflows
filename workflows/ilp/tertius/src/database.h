extern char views [100][20];
extern int the_num_views;
extern int database;

#if JAVA
extern JNIEnv *the_java_env;
extern JavaVM *the_java_VM;
extern jclass the_database_class;
extern jobject the_database_connection;
#endif

void addAND (char * query, int nbAnd);
void itostr(char c, int integer, char * the_query);
void line2query(char * the_string, char ** query);
int counting(tcube *a_cube,int *a_lit_indices, int a_lit_num, long *a_count);
#if JAVA
jarray cStrArray2j(char ** cStrArray, int size);
void jStrArray2c(jarray arr, char *** cStrArray, int * size);
void jStrArr_of_Array2c(jarray arr, char **** cStrArrArray, int *size_2);
#endif

int myStrCat(char **a_destination_string,char *a_string,long *a_length);
int collect_parameters();
int eval_cube_in_database(tcube *a_cube);
int counting(tcube *a_cube,int *a_lit_indices, int a_lit_num, long *a_count);
int create_view(char * the_stem_name);
void drop_all_views();
void line2query(char * the_string, char ** query);
void getConnection (char * stem);
void deConnection ();
