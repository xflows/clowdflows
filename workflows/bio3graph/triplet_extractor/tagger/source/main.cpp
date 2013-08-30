/*
 * $Id: main.cpp,v 1.8 2005/02/16 10:45:39 tsuruoka Exp $
 */

#include <stdio.h>
#include <fstream>
#include <map>
#include <list>
#include <iostream>
#include <sstream>
#include "maxent.h"
#include "common.h"
#include <Python.h>



using namespace std;

PyObject * bidir_postag(const string & s, const vector<ME_Model> & vme, const vector<ME_Model> & cvme, bool dont_tokenize);
void bidir_chunking(vector<Sentence> & vs, const vector<ME_Model> & vme);
void init_morphdic();

extern void load_ne_models();

/*
int main(int argc, char** argv)
{
  bool dont_tokenize = false;
  
  istream *is(&std::cin);

  string ifilename, ofilename;
  for (int i = 1; i < argc; i++) {
    string v = argv[i];
    if (v == "-nt") { dont_tokenize = true; continue; }
    if (v == "--help") { help(); exit(0); }
    ifilename = argv[i];
  }
  ifstream ifile;
  if (ifilename != "" && ifilename != "-") {
    ifile.open(ifilename.c_str());
    if (!ifile) { cerr << "error: cannot open " << ifilename << endl; exit(1); }
    is = &ifile;
  }
                                                                      
  init_morphdic();

  vector<ME_Model> vme(16);

  cerr << "loading pos_models";
  for (int i = 0; i < 16; i++) {
    char buf[1000];
    sprintf(buf, "./models_medline/model.bidir.%d", i);
    vme[i].load_from_file(buf);
    cerr << ".";
  }
  cerr << "done." << endl;

  cerr << "loading chunk_models";
  vector<ME_Model> vme_chunking(16);
  for (int i = 0; i < 8; i +=2 ) {
    char buf[1000];
    sprintf(buf, "./models_chunking/model.bidir.%d", i);
    vme_chunking[i].load_from_file(buf);
    cerr << ".";
  }
  cerr << "done." << endl;

  load_ne_models();
  
  string line;
  int n = 1;
  while (getline(*is, line)) {
    if (line.size() > 1024) {
      cerr << "warning: the sentence seems to be too long at line " << n;
      cerr << " (please note that the input should be one-sentence-per-line)." << endl;
    }
    string postagged = bidir_postag(line, vme, vme_chunking, dont_tokenize);
    cout << postagged << endl;
    n++;
  }
  
}

*/


bool dont_tokenize = false;
int dont_tokenizePrev = 0;
vector<ME_Model> vme(16);
vector<ME_Model> vme_chunking(16);
bool modelsLoaded = false;

/*
void load_models(bool disableTokenization) {
	  if (disableTokenization)
		  dont_tokenize = true;

	  init_morphdic();

	  cerr << "loading pos_models";
	  for (int i = 0; i < 16; i++) {
	    char buf[1000];
	    sprintf(buf, "./models_medline/model.bidir.%d", i);
	    vme[i].load_from_file(buf);
	    cerr << ".";
	  }
	  cerr << "done." << endl;

	  cerr << "loading chunk_models";
	  for (int i = 0; i < 8; i +=2 ) {
	    char buf[1000];
	    sprintf(buf, "./models_chunking/model.bidir.%d", i);
	    vme_chunking[i].load_from_file(buf);
	    cerr << ".";
	  }
	  cerr << "done." << endl;

	  modelsLoaded = true;
	  load_ne_models();
}

const char* tag_sentence(char *line) {
	if (!modelsLoaded)
		return NULL;
    string postagged = bidir_postag(line, vme, vme_chunking, dont_tokenize);
	return postagged.c_str();
}

*/



static PyObject * load_models(PyObject *self, PyObject *args, PyObject *keywds) {
	int disableTok = 0;
	static char *kwlist[] = { (char *)"dontTokenize", NULL };

	if (!PyArg_ParseTupleAndKeywords(args, keywds, "|i", kwlist, &disableTok))
		return NULL;

	if (modelsLoaded && (disableTok==dont_tokenizePrev)) {
		//cout << "GENIA models already loaded";
		return Py_None;
	}

	dont_tokenizePrev = disableTok;

	if (disableTok)
		dont_tokenize = true;

	  init_morphdic();

	  cerr << "loading pos_models";
	  for (int i = 0; i < 16; i++) {
	    char buf[1000];
	    sprintf(buf, "./models_medline/model.bidir.%d", i);
	    vme[i].load_from_file(buf);
	    cerr << ".";
	  }
	  cerr << "done." << endl;

	  cerr << "loading chunk_models";
	  for (int i = 0; i < 8; i +=2 ) {
	    char buf[1000];
	    sprintf(buf, "./models_chunking/model.bidir.%d", i);
	    vme_chunking[i].load_from_file(buf);
	    cerr << ".";
	  }
	  cerr << "done." << endl;

	  load_ne_models();

	  modelsLoaded = true;
	  return Py_None;
}


static PyObject * tag_sentence(PyObject * self, PyObject * args) {
	PyObject* sentence;
	const char *s, *line;

	s = PyString_AsString(args);
	if (s==NULL)
		return NULL;
//	if (not PyArg_ParseTuple(args, "s", &s))
//		return NULL;
	if (!modelsLoaded) {
		PyErr_SetString(PyExc_RuntimeError, "Tagger models not loaded!");
		return NULL;
	}

	line = strdup(s);

    PyObject * postagged = bidir_postag(line, vme, vme_chunking, dont_tokenize);
    return postagged;
	//return PyString_FromString(postagged.c_str());
}



// doc strings
static char module_doc[] = "wrapper for the GENIA tagger";
static char load_models_doc[] = "load_models(dontTokenize=False) loads tagger models. Must be called first!";
static char tag_sentence_doc[] = "tag_sentence(sentence) tags a sentence.";

// The method table must always be present; it lists the functions that should be callable from Python:
static PyMethodDef geniatagger_methods[] =
{
	//structure:
	// name of function when called from Python
	// corresponding C function
	// METH_VARARGS / METH_NOARGS / METH_O / METH_KEYWORDS  (type of arguments)
	// doc string

    { "load_models", (PyCFunction)load_models, METH_KEYWORDS, load_models_doc},
    { "tag_sentence", tag_sentence, METH_O, tag_sentence_doc},
    { NULL, NULL } // required ending of the method table
};


PyMODINIT_FUNC initgeniatagger(void)
{
  Py_InitModule3("geniatagger", geniatagger_methods, module_doc);
}



