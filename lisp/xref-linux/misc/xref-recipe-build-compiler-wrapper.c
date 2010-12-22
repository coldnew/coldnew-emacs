/*

  This file is Copylefted by Marian Vittek and Xref-Tech.

*/


#include <unistd.h>
#include <sys/stat.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <assert.h>
#include <limits.h>
#include <stdlib.h>

#define DEBUG 0

#if defined(__APPLE__)
#include <sys/malloc.h>
#else
#if (! defined(__FreeBSD__)) || (__FreeBSD__ < 5)
#include <malloc.h>
#endif
#endif


#ifdef __WIN32__
#define SLASH '\\'
#define PATH_SEPARATOR ';'
#else
#define PATH_SEPARATOR ':'
#define SLASH '/'
#endif


#define MAP_ON_PATH(path, cp, command) {\
    int ii;\
	char *currentPath, *pathEnd;\
	currentPath = path;\
	pathEnd = currentPath + strlen(currentPath);\
	while (currentPath < pathEnd) {\
		for(ii=0; currentPath[ii]!=0 && currentPath[ii]!=PATH_SEPARATOR; ii++) ;\
		strncpy(cp, currentPath, ii); cp[ii]=0; \
        { command; }\
		currentPath += ii;\
		currentPath++;\
	}\
}

typedef struct {
	char *compilerName;
	char *xrefEnvVariable;
	char *xrefRecipeOptions;
} S_recognizedCompilers;


S_recognizedCompilers s_recognizedCompilers[] = {
	{"c++", "XREFACTORY_WHICH_CPP", "-cpp--g++ -rcwd"},
	{"g++", "XREFACTORY_WHICH_GPP", "-cpp--g++ -rcwd"},
	{"gcc", "XREFACTORY_WHICH_GCC", "-cpp--gcc -rcwd"},
	{"cc", "XREFACTORY_WHICH_CC", " -rcwd"},
	{"CC", "XREFACTORY_WHICH__CC", "-cpp--sun -rcwd"},
	{NULL, NULL, NULL},
};

extern char **environ;


static void fprintOption(FILE *ff, char *option) {
	char *s;
	for(s=option; *s; s++) {
		if (*s == '\"') fprintf(ff, "${dq}");
		else if (*s == ' ') fprintf(ff, "${sp}");
		else fprintf(ff, "%c", *s);
	}
}

static int hasPrefix(char *v, char *p) {
	int n;
	n = strlen(p);
	return(strncmp(v, p, n)==0);
}

static S_recognizedCompilers *getCompilerItemForCompiler(char *compiler) {
	int 					j;
	S_recognizedCompilers 	*result;
	result = NULL;
	for(j=0; s_recognizedCompilers[j].compilerName!=NULL; j++) {
		if (strcmp(compiler, s_recognizedCompilers[j].compilerName)==0) {
			result = &s_recognizedCompilers[j];
			break;
		}
	}
	return(result);
}

static char *getRecipeOptionsForTask(char *task) {
	S_recognizedCompilers 	*ci;
	char					*compiler, *ccc;
	compiler = task;
	while ((ccc=strchr(compiler, SLASH))!=NULL) compiler = ccc+1;
	ci = getCompilerItemForCompiler(compiler);
	if (ci==NULL) return(NULL);
	return(ci->xrefRecipeOptions);
}

int main(int argc, char **argv) {
	char					pwd[PATH_MAX];
	char					cp[PATH_MAX];
	char 					*recipefile, *compiler, *roptions;
	char					*s, **eargv, *pp, *d, *ds, *path, *pathe, *cc, *ccc;
	char					*variable, *xref_compiler_wrappers;
	FILE					*ff;
	int						i, j, edim, eedim, pdim, argshift, pathfound, roptionslen;
	int						cclen, cplen, optionsloop, xref_compiler_wrappers_len;
	int 					compilerIndex;
	char					**eenv;
	struct stat				st;
	S_recognizedCompilers 	*ci;


	compilerIndex = -1;

	xref_compiler_wrappers = getenv("XREFACTORY_COMPILER_WRAPPERS");
	if (xref_compiler_wrappers == NULL) xref_compiler_wrappers="///"; //impossible path
	xref_compiler_wrappers_len = strlen(xref_compiler_wrappers);

	// get command line options (if any)
	recipefile = NULL;
	compiler = NULL;
	roptions = NULL;

	argshift = 0;
	if (argc > argshift+2 && strcmp(argv[argshift+1], "--xcall")==0) {
		compiler = argv[argshift+2];
		argshift += 2;
		optionsloop = 1;
		while (optionsloop) {
			optionsloop = 0;
			if (argc > argshift+2 && strcmp(argv[argshift+1], "--recipe")==0) {
				recipefile = argv[argshift+2];
				argshift += 2;
				optionsloop = (argshift < 6);
			}
			if (argc > argshift+2 && strcmp(argv[argshift+1], "--recipe-options")==0) {
				roptions = argv[argshift+2];
				argshift += 2;
				optionsloop = (argshift < 6);
			}
		}
	}
	

	// Prepare compiler environment, its task and parameters

	pathe = NULL;
	// prepare environment, it will be the original one
	for(eedim=0; environ[eedim]!=NULL; eedim++) ;
	eenv = malloc((eedim+1) * sizeof(char*));
	for(i=0, j=0; environ[i]!=NULL; i++) {
		if (hasPrefix(environ[i], "PATH=")) {
			// remove the path added by xref-recipe-build, it will be the first one
			pdim = strlen(environ[i]);
			pathe = malloc(pdim+1);
			s = environ[i]; d = pathe;
			while (*s && *s!='=') *d++ = *s++;
			if (*s) *d++ = *s++;

			ds = s;
			while (*s && *s!=PATH_SEPARATOR) s++ ;
			if ((s-ds == xref_compiler_wrappers_len-1 && strncmp(ds, xref_compiler_wrappers, s-ds)==0)
				) {
				// it is recipe-build-wrappers path, delete it
				if (*s) s++;
			} else {
				// not recipe-build-wrappers path, keep it
				strncpy(d, ds, s-ds);
				d += (s-ds);
				if (*s) *d++ = *s++;
			}

			while (*s) *d++ = *s++;
			*d++ = 0;
			eenv[j++] = pathe;
		} else if (! (
					   hasPrefix(environ[i], "XREFACTORY_RECIPE_FILE=")
					   || hasPrefix(environ[i], "XREFACTORY_COMPILER_WRAPPERS=")
					   || hasPrefix(environ[i], "XREFACTORY_WHICH_CPP=")
					   || hasPrefix(environ[i], "XREFACTORY_WHICH_GPP=")
					   || hasPrefix(environ[i], "XREFACTORY_WHICH_GCC=")
					   || hasPrefix(environ[i], "XREFACTORY_WHICH_CC=")
					   || hasPrefix(environ[i], "XREFACTORY_WHICH__CC=")
					   )) {
			eenv[j++]=environ[i];
		} 
	}
	eenv[j] = NULL;



	// if not given by option, identify the compiler to call

	if (compiler == NULL) {
		cc = argv[0];
		while ((ccc=strchr(cc, SLASH))!=NULL) cc = ccc+1;
		ci = getCompilerItemForCompiler(cc);
		if (ci==NULL) variable = "";
		else variable = ci->xrefEnvVariable;
		compiler = getenv(variable);
		if (compiler == NULL) compiler = cc;
	}


	// if compiler is given by a relative name, find it in new PATH

	if (strchr(compiler, SLASH) == NULL && pathe != NULL) {
		path = pathe;
		pathfound = 0;
		cclen = strlen(compiler);
		MAP_ON_PATH(path, cp, {
			cplen = strlen(cp);
			while (cplen>0 && cp[cplen-1]==SLASH) cplen--;
			cp[cplen++]=SLASH;
			cp[cplen]=0;
			// do not call yourself
			if (strcmp(cp, xref_compiler_wrappers) != 0) {
				if (cclen + cplen < PATH_MAX) {
					strcat(cp, compiler);
					// fprintf(stdout, "checking %s\n", cp);
					if (stat(cp, &st) == 0 && (st.st_mode & S_IFDIR) == 0) {
						pathfound = 1;
						break;
					}
				}
			}
		});
		if (pathfound) {
			compiler = cp;
		} else {
			fprintf(stderr, "[xref-compiler-wrapper] Can't find %s in $PATH==%s\n", compiler, path);
			exit(1);

		}
	}

	// prepare compiler parameters
	edim = argc-argshift+1;
	eargv = malloc(edim * sizeof(char *));
	eargv[0] = compiler;
	for(i=1; i<argc; i++) {
		eargv[i] = argv[i+argshift];
	}
	eargv[i] = NULL;


	/* **************************************************************** */

	// identify recipe file (if not given by option)
	if (recipefile == NULL) {
		recipefile = getenv("XREFACTORY_RECIPE_FILE");
		if (recipefile == NULL) {
			if (argshift == 0) {
				// make an error message only when no option is given,
				// otherwise, it is probable, that user is passing through
				// compiler wrappers via a regular make call
				fprintf(stderr, "[xref-compiler-wrapper] Can't get $XREFACTORY_RECIPE_FILE\n");
			}
			goto execute;
		}
	}

	// compute the options line identifying compiler in the recipe file
	// first try it from the wrapper name
	if (roptions==NULL) roptions = getRecipeOptionsForTask(argv[0]);
	// then from the compiler name
	if (roptions==NULL) roptions = getRecipeOptionsForTask(compiler);
	// no chance, get default without compiler identification
	if (roptions==NULL) roptions = " -rcwd";



	// Write item to the recipe file

#if DEBUG
	fprintf(stdout, "Adding item to %s\n", recipefile);
#endif

	ff = fopen(recipefile, "a");
	if (ff == NULL) {
		fprintf(stderr, "[xref-compiler-wrapper] Can't open %s\n", recipefile);
		goto close;
	}
	pp = getcwd(pwd, PATH_MAX);
	if (pp == NULL) {
		fprintf(stderr, "[xref-compiler-wrapper] Can't get cwd\n");
		goto close;
	}
	fprintf(ff, "%s ", roptions);
	// if roptions finishes by -rcwd, then add actual pwd
	roptionslen = strlen(roptions);
	if (roptionslen > 5 && strcmp(&roptions[roptionslen-5], "-rcwd")==0) {
		fprintOption(ff, pwd);
		fprintf(ff, " ");
	}
	for(i=1; eargv[i]!=NULL; i++) {
		fprintOption(ff, eargv[i]);
		fprintf(ff, " ");
	}
	fprintf(ff, " end-of-options\n\n");
 close:
	fclose(ff);

 execute:

#if DEBUG
	fprintf(stdout, "Executing %s\n", compiler);
	for(i=0; eargv[i]!=NULL; i++) printf("argv[%d] == '%s'\n", i, eargv[i]);
	fprintf(stdout, "\n");
	for(i=0; eenv[i]!=NULL; i++) printf("env[%d] == '%s'\n", i, eenv[i]);
	fprintf(stdout, "\n\n");
#endif

	// execute the compiler

	execve(compiler, eargv, eenv);

	fprintf(stderr, "[xref-compiler-wrapper] Can't exec %s", compiler);
	exit(1);
}


