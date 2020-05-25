////////////////////////////////////////////////////////////////////////////
//
//  AJBLisp - A Lisp Interpreter
//
//  (c) A J Le Couteur Bisson 1997-2007
//
////////////////////////////////////////////////////////////////////////////
// eval.cpp
//
// The evaluator
////////////////////////////////////////////////////////////////////////////
#include "ajblisp.h"
#include "listenerif.h"

// How many times to evaluate the car of a function call
// looking for a function before we give up with an error.
const int EVALLIMIT = 3;

int EvalCount = 0;    // This now counts function applications and not
                      // trivial value lookups

//---------------------------------------------------------------------------
// The heart of the interpreter
// Evaluate a lisp object and return the result
LISP iEval(LISP c)
{
  // Check for exit from interpreter
  if(ThreadCheckTerminate())
    LispError(FATAL_EXIT);

  if(UserBreak)
  {
    UserBreak = false;
    return LispError(ERR_USER);
  }

  // Keys evaluate to themselves
  if(IsKey(c))
    return c;

  // Other symbols evaluate to their value
  if(IsSymbol(c))
    return value(c);

  // These evaluate to themselves
  if(!c || IsAtom(c) || IsVector(c))
    return c;

  // Then we must have a list with the function in the car and
  // args (if any) in the cdr
  protect(func, car(c));

  // Handle this common case immediately (don't recurse)
  if(IsSymbol(func))
    func = value(func);

  // Maintain statistics
  ++EvalCount;

  NewFrame(c);

  // Try to find a function
  for (int i = 0; i < EVALLIMIT; i++)
  {
    if(!func)
      return LispError(ERR_BAD_FUNC);

    if(IsSubr(func))
      return EvalSubr(func);

    if(IsFSubr(func))
      return EvalFSubr(func);

    if(IsSubrl(func))
      return EvalSubrl(func);

    if(IsFSubrl(func))
      return EvalFSubrl(func);

    if(car(func) == LAMBDA)
    {
      if(!cdr(func))
        return LispError(ERR_BAD_FUNC);

      if(!cadr(func) || IsAtom(cadr(func)))
        return EvalExprl(func);
      else
        return EvalExpr(func);
    }

    if(car(func) == MACRO)
    {
      if(!cdr(func))
        return LispError(ERR_BAD_MAC);

      if(!cadr(func) || IsAtom(cadr(func)))
        return EvalMacrl(func);
      else
        return EvalMacr(func);
    }
    // Evaluate to see if we can find a function
    func = iEval(func);
  }
  return LispError(ERR_BAD_FUNC, func);
}

// Apply a function to its arguments but do not evaluate
// the arguments
LISP iApply(LISP f, LISP args)
{
  EvalCount++;
  protect(func, f);
  LISP funcall = NewCons(func, args);
  NewFrame(funcall);

  // Try to find a function
  for(int i = 0; i < EVALLIMIT; i++)
  {
    if(!func)
      return LispError(ERR_BAD_FUNC);

    // Primitive?
    if(IsSubr(func) || IsFSubr(func))
      return EvalFSubr(func);

    if(IsSubrl(func) || IsFSubrl(func))
      return EvalFSubrl(func);

    // Expr
    if(car(func) == LAMBDA)
    {
      if(!cdr(func))
        return LispError(ERR_BAD_FUNC);

      if(!cadr(func) || IsAtom(cadr(func)))
        return EvalMacrl(func);
      else
        return EvalMacr(func);
    }

    // Macro
    if(car(func) == MACRO)
    {
      if(!cdr(func))
        return LispError(ERR_BAD_MAC);

      if(!cadr(func) || IsAtom(cadr(func)))
        return EvalMacrl(func);
      else
        return EvalMacr(func);
    }
    func = iEval(func);
  }
  return LispError(ERR_BAD_FUNC);
  // return NIL;
}

// Evaluate a SUBR
LISP EvalSubr(LISP f)
{
  LISP result;

  // Get the execution address before everything moves
  SUBR exec = (SUBR) (code(f));

  // Evaluate list of args
  EvArgs(cdr(base->fun));
  result = exec(base->nargs, base->args);
  PopFrame();
  return result;
}

// Evaluate a SUBRL
LISP EvalSubrl(LISP f)
{
  LISP result;

  // Get the execution address before everything moves
  SUBRL exec = (SUBRL) (code(f));

  // Push evaluated list of args
  PushF(EvList(cdr(base->fun)));
  base->nargs++;
  result = exec(base->args[0]);
  PopFrame();
  return result;
}

// Evaluate an FSUBR
LISP EvalFSubr(LISP f)
{
  LISP result;

  // Get the execution address before everything moves
  FSUBR exec = (SUBR) (code(f));

  // Push list of args
  PushArgs(cdr(base->fun));
  result = exec(base->nargs, base->args);
  PopFrame();
  return result;
}

// Evaluate an FSUBRL
LISP EvalFSubrl(LISP f)
{
  LISP result;
  FSUBRL exec = (FSUBRL) (code(f));

  // Push list of args
  PushF(cdr(base->fun));
  base->nargs++;
  result = exec(base->args[0]);
  PopFrame();
  return result;
}

// Evaluate the body of a EXPR
LISP EvalExpr(LISP f)
{
  protect(func, f);

  base->save = true;                   // This is a save frame
  EvArgs(cdr(base->fun));              // Evaluate args into frame
  Bind(cadr(func));                    // Bind formal args to values in frame
  LISP result = Progn(cddr(func));
  PopFrame();                          // Restore environment
  return result;
}

// Evaluate the body of a EXPRL
LISP EvalExprl(LISP f)
{
  protect(body, cddr(f));
  LISP farg = cadr(f);

  if(farg) // Functions of no argument come here
  {
    if(!IsSymbol(farg))               // Formal arg must be a symbol
      throw LispError(ERR_BAD_FORMAL);

    base->save = true;                // This is a save frame
    base->nargs = 1;                  // ...with 1 argument

    PushF(farg);                      // Save argument
    PushF(value(farg));
    value(farg) =                     // Initialise formal argument
      EvList(cdr(base->fun));
  }
  else
  {
    // Check that there are no arguments
    if(cdr(base->fun))
      LispError(ERR_NUM_ARGS);
  }
  LISP result = Progn(body);
  PopFrame();                         // Restore environment
  return result;
}

// Evaluate the body of a MACRO
LISP EvalMacr(LISP f)
{
  protect(func, f);

  base->save = true;                   // This is a save frame
  PushArgs(cdr(base->fun));            // Evaluate args into frame
  Bind(cadr(func));                    // Bind formal args to values in frame
  LISP result = Progn(cddr(func));
  PopFrame();                          // Restore environment

  return result;
}

// Evaluate the body of a MACRL
LISP EvalMacrl(LISP f)
{
  LISP farg = cadr(f);

  if(farg) // Functions of no argument come here
  {
    if(!IsSymbol(farg))               // Formal arg must be a symbol
      LispError(ERR_BAD_FORMAL);

    base->save = true;                 // This is a save frame
    base->nargs = 1;                   // ...with 1 argument

    PushF(farg);                       // Save argument
    PushF(value(farg));
    value(farg) = cdr(base->fun);      // Initialise formal argument
  }
  else
  {
    // Check that there are no arguments
    if(cdr(base->fun))
      LispError(ERR_NUM_ARGS);
  }
  LISP result = Progn(cddr(f));
  PopFrame();                          // Restore environment
  return result;
}

//---------------------------------------------------------------------------


