////////////////////////////////////////////////////////////////////////////
//
//  AJBLisp - A Lisp Interpreter
//
//  (c) A J Le Couteur Bisson 1997-2002
//
////////////////////////////////////////////////////////////////////////////
// ajblisp.h
////////////////////////////////////////////////////////////////////////////
#ifndef ajblispH
#define ajblispH


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

//---------------------------------------------------------------------------
#include "lisperr.h"
#include "store.h"
#include "eval.h"
#include "io.h"
#include "func.h"
#include "repl.h"
#include "terminal.h"
//---------------------------------------------------------------------------
// Default memory sizes
extern int LispHeapSize;
extern int StackSize;

// Set when interactive REPL entered
extern bool Interactive;

// Window handle for MessageBox
extern void *ListenerHandle;

// Version string
extern void SetVersion(void);
extern char *Version;
//---------------------------------------------------------------------------
#endif


