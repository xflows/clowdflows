CFLAGS = -O3 -fPIC -Wno-deprecated
CPP = g++
INCLUDES = -I/usr/include/python2.6 -I/usr/include/python2.7
OBJS = main.o maxent.o tokenize.o bidir.o morph.o chunking.o namedentity.o


geniatagger: clean $(OBJS)
	g++ -shared $(OBJS) -o ../geniatagger.so
	rm -r -f geniatagger.py* *.o

clean:
	rm -r -f ../geniatagger.so geniatagger.py* *.o

.cpp.o:
	$(CPP) -c $(CFLAGS) $(INCLUDES) $<


