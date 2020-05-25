////////////////////////////////////////////////////////////////////////////
//
//  AJBLisp - A Lisp Interpreter
//
//  (c) A J Le Couteur Bisson 1997-2007
//
////////////////////////////////////////////////////////////////////////////
// eval.h
////////////////////////////////////////////////////////////////////////////
#ifndef OldEvalH
#define OldEvalH

//---------------------------------------------------------------------------
extern int EvalCount;

extern LISP iEval(LISP c);
extern LISP iApply(LISP f, LISP args);
LISP EvalSubr(LISP f);
LISP EvalFSubr(LISP f);
LISP EvalExpr(LISP f);
LISP EvalMacr(LISP f);
LISP EvalSubrl(LISP f);
LISP EvalFSubrl(LISP f);
LISP EvalExprl(LISP f);
LISP EvalMacrl(LISP f);

//---------------------------------------------------------------------------
#endif
