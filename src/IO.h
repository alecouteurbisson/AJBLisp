////////////////////////////////////////////////////////////////////////////
//
//  AJBLisp - A Lisp Interpreter
//
//  (c) A J Le Couteur Bisson 1997-2007
//
////////////////////////////////////////////////////////////////////////////
// io.h
////////////////////////////////////////////////////////////////////////////
#ifndef ioH
#define ioH

#include <stdio.h>
#include <stdarg.h>
#include "terminal.h"
//---------------------------------------------------------------------------
#define maxfmt 16

// User modifiable print formats
extern char  ifmt[];
extern char  ffmt[];

// Fixed write formats
extern char *wifmt;
extern char *wffmt;

// Session log
extern FILE *logfile;

// Low level output and logging
void fwritec(FILE* file, char c);
void writec(char c);
void fwrites(FILE* file, char* c, int len = -1);
void writes(char* c, int len = -1);
void fwritef(FILE* file, char* fmt, ...);
void writef(char* fmt, ...);

// Low level input
int inch(FILE* in);
void uninch(int c, FILE* in);
const char *readln(FILE* in);

// Lisp I/O
LISP ratom(FILE* in);
LISP rchar(FILE* in);
LISP rstring(FILE* in);
int  skipspace(FILE* in);
LISP replread();
LISP read(FILE* in);
LISP read1(FILE* in);
LISP flush(FILE* in);
LISP readlist(FILE* in, bool dot_ok = false);
LISP readline(FILE* in);
LISP patom(LISP c, FILE* out, bool cflag);
LISP print(LISP c, FILE* out, bool cflag);
void printlist(LISP c, FILE* out, bool cflag);
//---------------------------------------------------------------------------
#endif


