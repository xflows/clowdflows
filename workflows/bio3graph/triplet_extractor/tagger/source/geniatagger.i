%module geniatagger

%{
	extern void load_models(bool disableTokenization=false);
	extern const char* tag_sentence(char *);
%}

extern void load_models(bool disableTokenization=false);
extern const char* tag_sentence(char *);
