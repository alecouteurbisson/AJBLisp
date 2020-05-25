////////////////////////////////////////////////////////////////////////////
//
//  AJBLisp - A Lisp Interpreter
//
//  (c) A J Le Couteur Bisson 1997-2007
//
////////////////////////////////////////////////////////////////////////////
// terminal.h
////////////////////////////////////////////////////////////////////////////
#ifndef TerminalH
#define TerminalH
//---------------------------------------------------------------------------
#include <stdio.h>
#include <stdarg.h>

extern void  tprompt(bool active);
extern void  getline();
extern char *treadln();
extern int   tgetch();
extern void  tungetc(int c);
extern void  tflushin();
extern void  twritec(char c);
extern void  tfwritec(FILE *f, char c);
extern void  twrites(char *s, int len = -1);
extern void  twritesvb(char *s, int len = -1);
extern void  tfwrites(FILE *f, char *s, int len = -1);
extern void  twritef(const char *format, ...);
extern void  tfwritef(FILE *f, const char *format, ...);
extern void  tvwritef(const char *format, va_list arglist);
extern void  tvfwritef(FILE *f, const char *format, va_list arglist);
extern void  tvwritef(const char *format, va_list arglist);
extern void  tflushout();
extern bool  prompt;

#endif

