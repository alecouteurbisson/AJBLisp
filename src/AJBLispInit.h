////////////////////////////////////////////////////////////////////////////
//
//  AJBLisp - A Lisp Interpreter
//
//  (c) A J Le Couteur Bisson 1997-2007
//
////////////////////////////////////////////////////////////////////////////
// AJBLispInit.h
//
// Main thread - Initialisation
////////////////////////////////////////////////////////////////////////////
#ifndef ajblispinitH
#define ajblispinitH

//---------------------------------------------------------------------------
// Default memory sizes
extern int LispHeapSize;
extern int StackSize;

// Set when interactive REPL entered
extern bool Interactive;

// Main interpreter thread entry point
extern void AJBLisp(int argc, char *argv[]);
//---------------------------------------------------------------------------
#endif

