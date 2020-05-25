////////////////////////////////////////////////////////////////////////////
//
//  AJBLisp - A Lisp Interpreter
//
//  (c) A J Le Couteur Bisson 1997-2007
//
////////////////////////////////////////////////////////////////////////////
// repl.h
////////////////////////////////////////////////////////////////////////////
#ifndef replH
#define replH
#include <excpt.h>
#include <math.h>
#include "lisperr.h"
#include "store.h"

extern bool  UserBreak;
extern bool  Prompt;
extern bool  PromptPending;
extern int   DebugLevel;
extern bool  DebugLoop;

//---------------------------------------------------------------------------
// Errors always have an error number (-ve if fatal)
// Errors may have a message or can rely upon the handler to provide a
// generic message.  Explicit messages always override handler messages.
struct Err
{
  LERROR err;           // The error number
  const char *msg;      // The error message, if not the default
  LISP obj;             // The object (if any) that caused the error
};

void  Initialise();
void  REPL(void);
void  SetHistoryVars(LISP output);
LISP  DebugREPL(Err e);
bool  LoadFile(char *filename);
LISP  LispError(LERROR err, LISP obj = NONE, char *msg = NULL);
LISP  WindowsError();
bool  ReportError(Err e);
int   FilterException(int code);
Err   OSException(int code);

#endif
