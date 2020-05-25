#pragma inline
////////////////////////////////////////////////////////////////////////////
//
//  AJBLisp - A Lisp Interpreter
//
//  (c) A J Le Couteur Bisson 1997-2007
//
////////////////////////////////////////////////////////////////////////////
// funcasm.cpp
//
// Lisp function implementation (SUBRs & FSUBRS)
// Functions containing assembler are compiled separately to avoid errors
// in Tasm.
////////////////////////////////////////////////////////////////////////////
#include "ajblisp.h"

//#############################################################################
// Arithmetic operators
//#############################################################################

//f (+ [<num> ...])
//$ Calculate the sum of the numeric arguments.
//$ If any of the arguments are real then the result will be
//$ real otherwise the result is integer.
//x > (+)                  // Degenerate case
//x = 0
//x > (+ 2 2 2)
//x = 6
LISP Add(LISP& args)
{
  int result = 0;
  double fresult = 0.0;
  bool intf = true;

  LISP a = args;
  while(a)
  {
    EXPECT_NUM(car(a));
    if(intf)
    {
      if(IsReal(car(a)))
      {
        intf = false;
        fresult = result;
        continue;
      };
      result += ivalue(car(a));
      asm into;              // Overflow test
    }
    else
    {
      if(IsReal(car(a)))
        fresult += rvalue(car(a));
      else
        fresult += ivalue(car(a));
    }
    a = cdr(a);
  }
  if(intf)
    return NewInteger(result);
  else
    return NewReal(fresult);
}

//f (- <num> [<num>])
//$ Calculate the difference of two numeric arguments or the
//$ negative of a single numeric argument.
//$ If either of the arguments are real then the result will be
//$ real otherwise the result is integer.
//x > (- 1)
//x = -1
//x > (- 10 6)
//x = 4
LISP Subtract(byte nargs, LISP args[])
{
  if(nargs == 1)                            // Negate
  {
    EXPECT_NUM(ARG1);
    if(IsInteger(ARG1))
      return NewInteger(-ivalue(ARG1));
    else
      return NewReal(-rvalue(ARG1));
  }
  if(nargs == 2)                            // Subtract
  {
    EXPECT_NUM2(ARG1, ARG2);
    if(IsReal(ARG1))
    {
      if(IsReal(ARG2))
        return NewReal(rvalue(ARG1) - rvalue(ARG2));
      else
        return NewReal(rvalue(ARG1) - ivalue(ARG2));
    }
    else
    {
      if(IsReal(ARG2))
        return NewReal(ivalue(ARG1) - rvalue(ARG2));
      else
      {
        int i = ivalue(ARG1) - ivalue(ARG2);
        asm into;              // Overflow test
        return NewInteger(i);
      }
    }
  }
  LispError(ERR_NUM_ARGS);
  return(NIL);                              // Keep compiler happy
}

//f (* [<num> ...])
//$ Calculate the product of the numeric arguments.
//$ If any of the arguments are real then the result will be
//$ real otherwise the result is integer.
//x > (*)                  ; Degenerate case
//x = 1
//x > (* 2 2 2)
//x = 8
LISP Multiply(LISP& args)
{
  int result = 1;
  double fresult = 1.0;
  bool intf = true;

  LISP a = args;
  while(a)
  {
    EXPECT_NUM(car(a));
    if(intf)
    {
      if(IsReal(car(a)))
      {
        intf = false;
        fresult = result;
        continue;
      };
      result *= ivalue(car(a));
      asm into;              // Overflow test
    }
    else
    {
      if(IsReal(car(a)))
        fresult *= rvalue(car(a));
      else
        fresult *= ivalue(car(a));
    }
    a = cdr(a);
  }
  if(intf)
    return NewInteger(result);
  else
    return NewReal(fresult);
}

//m (inc <symbol> [<integer>])
//$ Increment the integer value of <symbol> by <integer>
//$ or 1 (default) and return the result.
//x > (setq a 3)
//x = 3
//x > (inc a)
//x = 4
//x > a
//x = 4
LISP Inc(byte nargs, LISP args[])
{
  CHECK_ARITY_IN(1, 2);
  int i = 1;
  if(nargs == 2)
  {
    LISP delta = iEval(ARG2);
    EXPECT_INT(delta);
    i = ivalue(delta);
  }
  EXPECT_SYM(ARG1);
  if(!IsInteger(value(ARG1)))
    LispError(ERR_INT_EXP, ARG1, "Symbol value is not an integer");
  i = ivalue(value(ARG1)) + i;
  asm into;

  LISP res = NewInteger(i);
  return iSet(ARG1, res);
}

//m (dec <symbol> [<integer>])
//$ Decrement the integer value of <symbol> by <integer>
//$ or 1 (default) and return the result.
//x > (setq a 3)
//x = 3
//x > (dec a)
//x = 2
//x > a
//x = 2
LISP Dec(byte nargs, LISP args[])
{
  CHECK_ARITY_IN(1, 2);
  int i = 1;
  if(nargs == 2)
  {
    LISP delta = iEval(ARG2);
    EXPECT_INT(delta);
    i = ivalue(delta);
  }
  EXPECT_SYM(ARG1);
  if(!IsInteger(value(ARG1)))
    LispError(ERR_INT_EXP, ARG1, "Symbol value is not an integer");
  i = ivalue(value(ARG1)) - i;
  asm into;

  LISP res = NewInteger(i);
  return iSet(ARG1, res);
}

