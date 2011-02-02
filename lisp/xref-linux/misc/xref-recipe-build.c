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
#include <ctype.h>

#ifndef __WIN32__
#include <alloca.h>
#endif

#if defined(__APPLE__)
#include <sys/malloc.h>
#else
#if (! defined(__FreeBSD__)) || (__FreeBSD__ < 5)
#include <malloc.h>
#endif
#endif


#define DEBUG 0


#ifdef __WIN32__
#define SLASH '\\'
#define PATH_SEPARATOR ';'
#else
#define PATH_SEPARATOR ':'
#define SLASH '/'
#endif

#define MAX_XREF_ENVIRONMENT_ITEMS		50

#define EDITOR_ALLOCATION_RESERVE 		0
#define MAX_BUFFER_SIZE 				(((unsigned)-1)>>1)
#define EDITOR_USUAL_SPRINTF_LEN		5000

// this is my implementation of (editable) strings

extern char **environ;

struct editorBuffer {
	int 			bufferSize;
	char			*text;
	int				allocatedSize;
};


static char 				*xrefEnv[MAX_XREF_ENVIRONMENT_ITEMS];
static int  				xrefEnvi;



static void editorResizeBuffer(struct editorBuffer *ff, int size) {
	int 	minSize, allocSize;
	char 	*space;
	minSize = size + EDITOR_ALLOCATION_RESERVE;
	allocSize = 2048;
	for(; allocSize<minSize; ) allocSize = allocSize << 1;
	if (allocSize > MAX_BUFFER_SIZE) {
		fprintf(stderr, "buffer too long");
		exit(1);
	}
	if (allocSize > ff->allocatedSize) {
		space = malloc(allocSize+1);
		if (space == NULL) {
			fprintf(stderr, "editor out of memory");
			exit(1);
		}
		if (ff->text != NULL) {
			memcpy(space, ff->text, ff->bufferSize);
			free(ff->text);
		}
		ff->text = space;
		ff->allocatedSize = allocSize;
	}
}

static struct editorBuffer *editorCrNewBuffer() {
	struct editorBuffer *ff;
	ff = malloc(sizeof(struct editorBuffer));
	if (ff==NULL) {
		fprintf(stderr, "editor out of memory");
		exit(1);
	}
	ff->bufferSize = ff->allocatedSize = 0;
	ff->text = NULL;
	return(ff);
}

static void editorFree(struct editorBuffer **b) {
	free((*b)->text);
	free(*b);
	*b = NULL;
}

static void editorReplaceStringWithLen(struct editorBuffer *buff, int position, int delsize, char *str, int strlength) {
	int 					nsize, oldsize, index, undosize, pattractor;
	char					*text, *space, *undotext;
	assert(position >=0 && position <= buff->bufferSize);
	assert(delsize >= 0);
	assert(strlength >= 0);
	oldsize = buff->bufferSize;
	if (delsize+position > oldsize) {
		// deleting over end of buffer, 
		// delete only until end of buffer
		delsize = oldsize - position;
	}
	nsize = oldsize + strlength - delsize;
	// prepare operation
	if (nsize >= buff->allocatedSize) {
		editorResizeBuffer(buff, nsize);
	}
	assert(nsize < buff->allocatedSize);
	// edit text
	if (delsize+position < oldsize) {
		memmove(buff->text+position+strlength, buff->text+position+delsize,
				buff->bufferSize - position - delsize);
	}
	if (strlength > 0) {
		memcpy(buff->text+position, str, strlength);
	}
	buff->bufferSize = buff->bufferSize - delsize + strlength;
}

static void editorReplaceString(struct editorBuffer *buff, int position, int delsize, char *str) {
	editorReplaceStringWithLen(buff, position, delsize, str, strlen(str));
}

static void editorAppendChar(struct editorBuffer *buff, int c) {
	if (buff->bufferSize + 1 > buff->allocatedSize) {
		editorResizeBuffer(buff, buff->bufferSize + 1);
	}
	buff->text[buff->bufferSize] = c;
	buff->bufferSize ++;
	buff->text[buff->bufferSize]=0;
}

static void editorDump(struct editorBuffer *buff, FILE *ff) {
	int i;
	fprintf(ff, "[dump] start\n"); fflush(ff);
	for(i=0; i<buff->bufferSize; i++) putc(buff->text[i], ff);
	fprintf(ff, "[dump] end\n\n"); fflush(ff);
}

static void editorAppendString(struct editorBuffer *b, char *str) {
	editorReplaceString(b, b->bufferSize, 0, str);
	b->text[b->bufferSize]=0;
}

#define _EDITOR_INTERNAL_PRINTF(b, position, delsize, f) {\
	char 	*t, tt[EDITOR_USUAL_SPRINTF_LEN];\
	int 	len;\
	va_list ap;\
	va_start(ap, f);\
	len = vsnprintf(tt, EDITOR_USUAL_SPRINTF_LEN, f, ap);\
	if (len < EDITOR_USUAL_SPRINTF_LEN) {\
		editorReplaceString(b, position, delsize, tt);\
	} else {\
		t = malloc(len+5);\
		vsnprintf(t, len+5, f, ap);\
		editorReplaceString(b, position, delsize, t);\
		free(t);\
	}\
	b->text[b->bufferSize]=0;\
	va_end(ap);\
}

static void editorPrintf(struct editorBuffer *b, int position, int delsize, char *f, ...) {
	_EDITOR_INTERNAL_PRINTF(b, position, delsize, f);
}

static struct editorBuffer *editorCrBufferPrintf(char *f, ...) {
	struct editorBuffer *b;
	b = editorCrNewBuffer();
	_EDITOR_INTERNAL_PRINTF(b, 0, 0, f);
	return(b);
}

/* ********************************************************************** */


#define EDITOR_BUFFER_LAST_CHAR(b) ((b->bufferSize>0)?b->text[b->bufferSize-1]:0)

static void createDir(char *dirname) {
#ifdef __WIN32__
	mkdir(dirname);
#else
	mkdir(dirname,0777);
#endif
}

void recursivelyCreateFileDirIfNotExists(char *fpath) {
	char 			*p;
	int 			ch,len,loopFlag;
	struct stat		st;
	len = strlen(fpath);
	loopFlag = 1;
	for (p=fpath+len; p>fpath && loopFlag; p--) {
		if (*p!=SLASH) continue;
		ch = *p; *p = 0;
		if (stat(fpath, &st)==0 && (st.st_mode & S_IFMT) == S_IFDIR) {
			 loopFlag=0;
		}
		*p = ch;
	}
	for(p+=2; *p; p++) {
		if (*p!=SLASH) continue;
		ch = *p; *p = 0;
		createDir(fpath);
		*p = ch;
	}
}

static int fileExists(char *p) {
	struct stat 	st;
	if (stat(p, &st) == 0 && (st.st_mode & S_IFDIR) == 0) return(1);
	return(0);
}

static char *getAbsolutePath(char *path, char *result) {
	static char 	cwd[PATH_MAX] = {0,};
	char			*s, *d;
	int				i;
	if (path[0] == SLASH || (isalpha(path[0]) && path[1]==':' && (path[2]=='/' || path[2]=='\\'))) {
		// absolute path, just copy it
		strcpy(result, path);
	} else {
		if (cwd[0] == 0) {
			getcwd(cwd, PATH_MAX);
		}
		strcpy(result, cwd);
		i = strlen(result);
		while (i>0 && result[i-1] == SLASH) i--;
		result[i++] = SLASH;
		strcpy(result+i, path);
	}
	return(result);
}

static int checkItemExistence(struct editorBuffer *b, char *file) {
	struct editorBuffer *b2;
	if (EDITOR_BUFFER_LAST_CHAR(b) != SLASH) {
		editorAppendChar(b, SLASH);
	}
	editorAppendString(b, file);
	// for Windows port, you shall check existence of .exe too!!
	if (fileExists(b->text)) return(1);
#ifdef __WIN32__
	editorAppendString(b, ".exe");
	if (fileExists(b->text)) return(1);	
#endif
	return(0);
}

static int checkItem(struct editorBuffer *b, char *var, char *file) {
	struct editorBuffer *b2;
	char				*p, *pp;
	int					ilen, res;
	char				cp[PATH_MAX];
	res = 0;
	ilen = b->bufferSize;
	if (checkItemExistence(b, file)) {
		pp = getAbsolutePath(b->text, cp);
		if (pp != NULL) {
			res = 1;
			b2 = editorCrBufferPrintf("%s=%s", var, cp);
			p = malloc(b2->bufferSize+1);
			strcpy(p, b2->text);
			xrefEnv[xrefEnvi ++] = p;
//fprintf(stdout,"adding %s\n", p);
			editorFree(&b2);
		}
	}
	b->bufferSize = ilen;
	return(res);
}

#define MAP_ON_PATH(path, command) {\
    int ii;\
	char *currentPath, *pathEnd;\
	currentPath = path;\
	pathEnd = currentPath + strlen(currentPath);\
	while (currentPath < pathEnd) {\
		for(ii=0; currentPath[ii]!=0 && currentPath[ii]!=PATH_SEPARATOR; ii++) ;\
		editorReplaceStringWithLen(cp, 0, cp->bufferSize, currentPath, ii);\
        { command; }\
		currentPath += ii;\
		currentPath++;\
	}\
}

static void addEnvItem(char *name, char *val) {
	struct editorBuffer *b2;
	char				*p;
	b2 = editorCrBufferPrintf("%s=%s", name, val);
	p = malloc(b2->bufferSize+1);
	strcpy(p, b2->text);
	xrefEnv[xrefEnvi++] = p;
	editorFree(&b2);
}

static void envDump() {
	int i;
	for(i=0; i<xrefEnvi; i++) {
		fprintf(stdout, "%d: %s\n", i, xrefEnv[i]);
	}
}

static int hasPrefix(char *v, char *p) {
	int n;
	n = strlen(p);
	return(strncmp(v, p, n)==0);
}

int main(int argc, char **argv) {
	int  				i, j, ii, ilen, ergc, edim, adim, cwdlen, scriptFileLen;
	int					argshift;
	char 				*path, *pp, *p, *tt;
	char				scriptFile[PATH_MAX];
	char				recipeFile[PATH_MAX];
	char				wrappersDir[PATH_MAX];
	char				cwd[PATH_MAX];
	char				runFile[PATH_MAX];
	struct editorBuffer	*cp, *b;
	FILE				*ff;
	char				**eenv;
	char				**eargv;

	if (argc < 2) {
		fprintf(stderr, "usage: xref-recipe-build [-wd wrappers_dir] recipe-file command command_parameters ...\n");
		exit(1);
	}

	wrappersDir[0] = 0;

	argshift = 0;
	if (argc > 2 && strcmp(argv[1], "-wd")==0) {
		if (strlen(argv[2]) < PATH_MAX-1) strcpy(wrappersDir, argv[2]);
		argshift += 2;
	}

	xrefEnvi = 0;
	path = getenv("PATH");
	if (path==NULL) {
		fprintf(stderr, "Can't get PATH, sorry.\n");
		exit(1);
	}
	cp = editorCrNewBuffer();
	MAP_ON_PATH(path, {
		if (checkItem(cp, "XREFACTORY_WHICH_CPP", "c++")) break;
	});
	MAP_ON_PATH(path, {
		if (checkItem(cp, "XREFACTORY_WHICH_GPP", "g++")) break;
	});
	MAP_ON_PATH(path, {
		if (checkItem(cp, "XREFACTORY_WHICH_GCC", "gcc")) break;
	});
	MAP_ON_PATH(path, {
		if (checkItem(cp, "XREFACTORY_WHICH_CC", "cc")) break;
	});
	MAP_ON_PATH(path, {
		if (checkItem(cp, "XREFACTORY_WHICH__CC", "CC")) break;
	});
	runFile[0] = 0;
	if (strchr(argv[argshift+2], SLASH) != NULL) {
		strcpy(runFile, argv[argshift+2]);
	} else {
		MAP_ON_PATH(path, {
			ilen = cp->bufferSize;
			if (checkItemExistence(cp, argv[argshift+2])) {
				pp = getAbsolutePath(cp->text, runFile);
				if (pp!=NULL) break;
			}
			cp->bufferSize = ilen;
		});
	}
	if (runFile[0]==0) {
		fprintf(stderr, "Can't find %s in PATH\n", argv[argshift+2]);
		exit(1);
	}

	// find the wrappers directory
	if (wrappersDir[0] == 0) {
		scriptFile[0] = 0;
		if (strchr(argv[argshift+0], SLASH) != NULL) {
			pp = getAbsolutePath(argv[argshift+0], scriptFile);
			if (pp == NULL) {
				tt = getcwd(cwd, PATH_MAX);
				if (tt) {
					cwdlen = strlen(cwd);
					if (cwdlen>0 && cwd[cwdlen-1] == SLASH) cwd[--cwdlen] = 0;
					sprintf(scriptFile, "%s%c%s",  cwd, SLASH, argv[argshift+0]);
				} else {
					scriptFile[0] = 0;
				}
			} 
		} else {
			MAP_ON_PATH(path, {
				ilen = cp->bufferSize;
				if (checkItemExistence(cp, argv[argshift+0])) {
					pp = getAbsolutePath(cp->text, scriptFile);
					if (pp!=NULL) break;
				}
				cp->bufferSize = ilen;
			});
		}
		if (scriptFile[0]==0) {
			fprintf(stderr, "Can not infer absolute path to the file xref-recipe-build.\nPut its directory into PATH, or invoke it with absolute prefix.\nSorry.");
			exit(1);
		}

		// following code is for cygwin
		scriptFileLen = strlen(scriptFile);
		if (scriptFileLen > 4 && strcmp(&scriptFile[scriptFileLen-4], ".exe")==0) {
			scriptFileLen -= 4;
			scriptFile[scriptFileLen] = 0;
		}
		sprintf(wrappersDir, "%s%s", scriptFile, "-compiler-wrappers");
	}

	// adjust PATH
	b = editorCrBufferPrintf("%s=%s%c%s", "PATH", wrappersDir, PATH_SEPARATOR, path);
	p = malloc(b->bufferSize+1);
	strcpy(p, b->text);
	xrefEnv[xrefEnvi++] = p;
	editorFree(&b);
	
	// prepare XREFACTORY_COMPILER_WRAPPERS
	b = editorCrBufferPrintf("%s=%s%c", "XREFACTORY_COMPILER_WRAPPERS", wrappersDir, SLASH);
	p = malloc(b->bufferSize+1);
	strcpy(p, b->text);
	xrefEnv[xrefEnvi++] = p;
	editorFree(&b);

	//
	
	recursivelyCreateFileDirIfNotExists(argv[argshift+1]);
	// create empty file
	ff = fopen(argv[argshift+1], "w");
	fclose(ff);
	// get its absolute path
	pp = getAbsolutePath(argv[argshift+1], recipeFile);
	if (pp == NULL) {
		fprintf(stderr, "Can't get recipe file\n");
	}
	addEnvItem("XREFACTORY_RECIPE_FILE", recipeFile);

	for(i=0; environ[i]!=NULL; i++) ;
	edim = i+xrefEnvi;
	eenv = malloc((edim+1) * sizeof(char*));
	for(i=0,j=0; environ[i]!=NULL; i++) {
		if (! (hasPrefix(environ[i], "PATH=")
			   || hasPrefix(environ[i], "XREFACTORY_RECIPE_FILE=")
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
	for(i=0; i<xrefEnvi; i++) eenv[j++]=xrefEnv[i];
	eenv[j] = NULL;

#if DEBUG
	fprintf(stdout, "xref-recipe-build environment: \n");
	for(i=0; i<xrefEnvi; i++) fprintf(stdout, "%s\n", xrefEnv[i]);
#endif

	adim = argc-argshift-2;
	eargv = malloc((adim+1)*sizeof(char *));
	for(i=0; i<adim; i++) eargv[i]=argv[argshift+i+2];
	eargv[i] = NULL;

#if DEBUG
	fprintf(stdout, "xref-recipe-build calls %s as: ", runFile);
	for(i=0; eargv[i]!=NULL; i++) fprintf(stdout, "%s ", eargv[i]);
	fprintf(stdout, "\n");
	fflush(stdout);
#endif			

	editorFree(&cp);

	execve(runFile, eargv, eenv);

	// shall not return
	fprintf(stderr, "Can't execve %s\n", eargv[0]);
	return(1);
}
