PC = spmkrs
CC = gcc -ggdb -Wall

tertius: error.o input_output.o gestion.o eval.o search.o write.o bayes.o memory.o database.o bayes2.o roc.o tertius.o
	$(CC) -o tertius error.o input_output.o gestion.o eval.o search.o write.o bayes.o memory.o database.o bayes2.o roc.o tertius.o -lm

tertius_prolog: error.o input_output.o gestion.o eval.o search.o write.o bayes.o background.o query.o expected.o table.o sicstusio.o memory.o database.o tertius.o
	$(PC) tertius +o error.o input_output.o gestion.o eval.o search.o write.o bayes.o background.o query.o expected.o table.o sicstusio.o memory.o database.o tertius.o -lm

1BC: error.o input_output.o gestion.o eval.o search.o write.o bayes.o memory.o database.o bayes2.o roc.o 1bc.o
	$(CC) -o 1BC error.o input_output.o gestion.o eval.o search.o write.o bayes.o memory.o database.o bayes2.o roc.o 1bc.o -lm

1BC_prolog: error.o input_output.o gestion.o eval.o search.o write.o bayes.o background.o query.o expected.o table.o sicstusio.o memory.o database.o 1BC.o
	$(PC) 1BC +o error.o input_output.o gestion.o eval.o search.o write.o bayes.o background.o query.o expected.o table.o sicstusio.o memory.o database.o 1BC.o -lm

query: 	query.o
	$(CC) -c query.c -I/usr/local/sicstus/include 
roc:	roc.o
	$(CC) -o roc roc.o

test_query: error.o input_output.o gestion.o eval.o search.o query.o write.o background.o test_query.o
	$(PC) test_query +o error.o input_output.o gestion.o eval.o search.o query.o write.o background.o test_query.o

test_expected: table.o sicstusio.o expected.o input_output.o error.o search.o gestion.o eval.o background.o write.o query.o test_expected.o
	$(PC) test_expected +o table.o sicstusio.o expected.o input_output.o error.o search.o gestion.o eval.o background.o write.o query.o test_expected.o

test_sicstusio:	sicstusio.o test_sicstusio.o
	$(PC) test_sicstusio +o sicstusio.o test_sicstusio.o

test_table: table.o test_table.o
	$(PC) test_table +o table.o test_table.o

test_prolog: test_prolog.o sicstusio.o
	$(PC) test_prolog +o test_prolog.o sicstusio.o

testgmp:	testgmp.o
	$(CC) -o testgmp testgmp.o -lm -lgmp

test_roc:	memory.c roc.o test_roc.o
	$(CC) -o test_roc memory.c roc.o test_roc.o

dylibTertius: tertius
	cc -v -dynamiclib -install_name libtertius.dylib -o ../../../libtertius.dylib error.o input_output.o gestion.o eval.o search.o write.o bayes.o memory.o database.o bayes2.o roc.o tertius.o -lm

dylibOneBc: 1BC
	cc -v -dynamiclib -install_name libonebc.dylib -o ../../../libonebc.dylib error.o input_output.o gestion.o eval.o search.o write.o bayes.o memory.o database.o bayes2.o roc.o 1bc.o -lm

clean: 
	rm *.o


