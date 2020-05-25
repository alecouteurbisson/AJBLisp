////////////////////////////////////////////////////////////////////////////
//                                     
//  AJBLisp - A Lisp Interpreter
//
//  (c) A J Le Couteur Bisson 1997-2007
//
////////////////////////////////////////////////////////////////////////////
// func.cc
//
// Lisp function implementation (SUBRs & FSUBRS)
////////////////////////////////////////////////////////////////////////////
#include <windows.h>
#include <locale.h>
#include <errno.h>
#include <dir.h>
#include <io.h>
#include <sys\stat.h>
#include <float.h>
#include <math.h>
#include "ajblisp.h"
#include "random.h"
#include "RunCommand.h"
// ---------------------------------------------------------------------------
//
// NOTE: Using dotted lists as arguments to any control structure
// may crash the interpreter.
// This is not a bug but a concious decision to avoid numerous
// tests for improper lists.
// There is no valid interpretation of such forms in any case.
// e.g.(cond((eq x 3)(dothis) . (dothat))(t . nil)) = BOOM!
//
// All other syntax errors are trapped and reported.
//
// Functions names beginning with i indicate internal forms of Lisp
// functions e.g. iEq is the internal form of Eq
// ---------------------------------------------------------------------------
// Embedded help
// This file contains embedded help for the Lisp functions which may be
// extracted and formatted by HtmlHelp.lsp.
//
// Lines beginning '//f ' indicates the name of a function.
// Lines beginning '//m ' indicates the name of a macro.
// Lines beginning '//$ ' indicates a line of descriptive text.
// Lines beginning '//x ' indicates an example.
// Line content that starts with . is preformatted - HTML <pre> </pre>
// Line content that starts with ~ introduces a new paragraph - HTML <p> </p>
//
// To create the AJBLisp-Help.htm file run AJBLisp from the build directory
// and enter:
// (load "HtmlHelp")
// Or from a command shell type:
// ajblisp -lHtmlHelp.lsp
//
// This will destroy and replace an existing directory named 'help' in the
// current directory
// ----------------------------------------------------------------------------
bool endloop;                   // loop control variable

char *ErrorMessage = 0L;        // Message associated with last Lisp error

static bool utcflag = true;     // Set if time values are UTC
static bool dstflag = true;     // Set if time values take DST into account

const int BUFSZ = 256;          // General purpose text buffer
char txtbuf[BUFSZ];

//#############################################################################
// Basic primitives
//#############################################################################

//m (quote <literal>) or '<literal>
//$ Protects its argument from evaluation.
//$ ~'x is a shorthand for (quote x) and is converted to the
//$ latter form immediately upon input.
//x > ''a
//x = (quote a)            ; One quote evaluated away
//x > (setq a '(x y z))    ; Set a to a literal list
//x = (x y z)
//x > (atom a)
//x = nil                  ; The value of a is not an atom
//x > (atom 'a)
//x = T                    ; ...but a (the symbol) is itself an atom
LISP Quote(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  return ARG1;
}

//f (symbol-value <sym>)
//$ Returns the value of <symbol>.  This is identical in
//$ operation to (eval <sym>) but the argument must be a
//$ symbol.
LISP SymbolValue(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  EXPECT_SYM(ARG1);
  return value(ARG1);
}

//f (cons <car> <cdr>)
//$ Construct a dotted pair. <cdr> is usually a list or nil.
//x > (cons 'a '(b c))
//x = (a b c)
//x > (cons 'a nil)
//x = (a)
//x > (cons 'a 'b)
//x = (a . b)
LISP Cons(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(2);
  return NewCons(ARG1, ARG2);
}

//f (set <symbol> <value>)
//$ Sets <symbol> to <value>. Both arguments are evaluated so
//$ the first argument must evaluate to a symbol. Returns <value>.
//x > (set 'a 3)
//x = 3
//x > a
//x = 3
LISP Set(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(2);
  return iSet(ARG1, ARG2);
}

// Helper function for Set
LISP iSet(LISP c, LISP v)
{
  if(IsConstant(c))
    return LispError(ERR_IMMUTABLE, c);
  EXPECT_SYM(c);
  value(c) = v;
  return(v);
}

//m (setq <symbol> <value> [<symbol> <value>] ...)
//$ Sets the <symbol>s to the <value>s sequentially so that
//$ the values may refer to the value of previously bound
//$ symbols. The <symbol> arguments are not evaluated and should
//$ not be quoted (setq means 'set quote'). Returns the last
//$ value bound.
//x > (setq a 3 b (- a)) ; Value of b refers to new value of a
//x = -3
//x > (println a :space b)
//x 3 -3
//x = -3
LISP Setq(LISP& args)
{
  LISP result;
  if(!args)
    return LispError(ERR_NUM_ARGS);
  protect(pargs, args);
  while(pargs)
  {
    if(!IsCons(cdr(pargs)))
      return LispError(ERR_NUM_ARGS, NONE, "Args must occur in pairs");
    result = iEval(cadr(pargs));
    iSet(car(pargs), result);
    pargs = cddr(pargs);
  }
  return(result);
}

//m (psetq <symbol> <value> [<symbol> <value>] ...)
//$ Sets the <symbol>s to the <value>s in parallel so that all
//$ values are evaluated in the enclosing scope. The symbol
//$ arguments are not evaluated and should not be quoted.
//$ Returns the last value bound.
//x > (setq x 1 y 2)
//x = 2
//x > (psetq x y y x)    ; Value of y set to previous value of x
//x = 1
//x (println x :space y)
//x 2 1                  ; Values swapped
//x = 1
LISP Psetq(LISP& args)
{
  if(!args)
    return LispError(ERR_NUM_ARGS);

  protect(pargs, args);
  // Evaluate values
  while(pargs)
  {
    if(!IsCons(cdr(pargs)))
      return LispError(ERR_NUM_ARGS, NONE, "Args must occur in pairs");
    cadr(pargs) = iEval(cadr(pargs));
    pargs = cddr(pargs);
  }
  // Bind to symbols
  LISP result;
  pargs = args;
  while(pargs)
  {
    result = iSet(car(pargs), cadr(pargs));
    pargs = cddr(pargs);
  }
  return(result);
}

//f (car <list>)
//$ Return the first element of <list>.
//$ nil, the empty list, is an invalid argument.
//x > (car '(a b c))
//x = a
LISP Car(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  EXPECT_CONS(ARG1);
  return car(ARG1);
}

//f (cdr <list>)
//$ Returns <list> with the first element removed.
//$ nil, the empty list, is an invalid argument.
//x > (cdr '(a b c))
//x = (b c)
//x > (cdr '(a))
//x = nil
LISP Cdr(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  EXPECT_CONS(ARG1);
  return cdr(ARG1);
}

//f (caar <list>) ... (cddr <list>), (caaar <list>) ... (cdddr <list>)
//$ These functions return the result of successive
//$ applications of car or cdr. Each a or d in the name
//$ represents the application of car or cdr respectively.
//$ ~The nth and nthcdr functions are usually clearer when
//$ they suffice.
//x > (cadar '((1 2 3) 4 (5 6)))
//x = 2   ; (car (cdr (car '((1 2 3) 4 (5 6)))))
//x > (caddr '((1 2 3) 4 (5 6)))
//x = 5   ; (car (cdr (cdr '((1 2 3) 4 (5 6)))))
LISP Caar(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  if(IsCons(ARG1) && IsCons(car(ARG1)))
    return caar(ARG1);
  return LispError(ERR_LIST_EXP, ARG1);
}

LISP Cadr(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  if(IsCons(ARG1) && IsCons(cdr(ARG1)))
    return cadr(ARG1);
  return LispError(ERR_LIST_EXP, ARG1);
}

LISP Cdar(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  if(IsCons(ARG1) && IsCons(car(ARG1)))
    return cdar(ARG1);
  return LispError(ERR_LIST_EXP, ARG1);
}

LISP Cddr(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  if(IsCons(ARG1) && IsCons(cdr(ARG1)))
    return cddr(ARG1);
  return LispError(ERR_LIST_EXP, ARG1);
}

LISP Caaar(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  if(IsCons(ARG1) && IsCons(cdr(ARG1)) && IsCons(caar(ARG1)))
    return car(caar(ARG1));
  return LispError(ERR_LIST_EXP, ARG1);
}

LISP Caadr(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  if(IsCons(ARG1) && IsCons(cdr(ARG1)) && IsCons(cadr(ARG1)))
    return car(cadr(ARG1));
  return LispError(ERR_LIST_EXP, ARG1);
}

LISP Cadar(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  if(IsCons(ARG1) && IsCons(cdr(ARG1)) && IsCons(cdar(ARG1)))
    return car(cdar(ARG1));
  return LispError(ERR_LIST_EXP, ARG1);
}

LISP Caddr(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  if(IsCons(ARG1) && IsCons(cdr(ARG1)) && IsCons(cddr(ARG1)))
    return car(cddr(ARG1));
  return LispError(ERR_LIST_EXP, ARG1);
}

LISP Cdaar(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  if(IsCons(ARG1) && IsCons(cdr(ARG1)) && IsCons(caar(ARG1)))
    return cdr(caar(ARG1));
  return LispError(ERR_LIST_EXP, ARG1);
}

LISP Cdadr(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  if(IsCons(ARG1) && IsCons(cdr(ARG1)) && IsCons(cadr(ARG1)))
    return cdr(cadr(ARG1));
  return LispError(ERR_LIST_EXP, ARG1);
}

LISP Cddar(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  if(IsCons(ARG1) && IsCons(cdr(ARG1)) && IsCons(cdar(ARG1)))
    return cdr(cdar(ARG1));
  return LispError(ERR_LIST_EXP, ARG1);
}

LISP Cdddr(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  if(IsCons(ARG1) && IsCons(cdr(ARG1)) && IsCons(cddr(ARG1)))
    return cdr(cddr(ARG1));
  return LispError(ERR_LIST_EXP, ARG1);
}

//f (rplaca <list> <value>)
//$ Replace the first element of <list> with <value>.
//$ ~This is a destructive operation and will affect all
//$ references to <list>.  It is possible, for example,
//$ to set the car of a list to the list itself. 
//$ This is OK until the resulting list is scanned
//$ (e.g. by print.) An endless loop will result which
//$ can be broken by pressing the [Esc] key.
//x > (setq x '(1 2 3) y x)   ; x = y = (1 2 3)
//x = (1 2 3)
//x > (rplaca x 'z)           ; Modify x
//x = (z 2 3)
//x > y
//x = (z 2 3)                 ; y affected similarly
LISP Rplaca(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(2);
  EXPECT_CONS(ARG1);
  car(ARG1) = ARG2;
  return ARG1;
}

//f (rplacd <list> <value>)
//$ Replace the cdr of <list> with <value>.
//$ ~This is a destructive operation and will affect all
//$ references to the list.  It is possible to set the
//$ cdr of a list or sub-list to the list itself.  This
//$ is OK until the resulting list is scanned (e.g. by print.)
//$ An endless loop will result which can be broken by
//$ pressing the [Esc] key.
//x > (setq x '(1 2 3) y x)   ; x = y = (1 2 3)
//x = (1 2 3)
//x > (rplacd x 'z)           ; Modify x
//x = (1 . z)
//x > y
//x = (1 . z)                 ; Y affected similarly

LISP Rplacd(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(2);
  EXPECT_CONS(ARG1);
  cdr(ARG1) = ARG2;
  return ARG1;
}

//f (eval <any>)
//$ Evaluates its argument.
//$ ~Note that all lambda functions (i.e. not macros)
//$ automatically evaluate their arguments so eval actually
//$ evaluates its argument twice.  List arguments are treated
//$ as the application of a function and the expression at the
//$ car of the list will be evaluated up to three times
//$ in an attempt to yield a valid function or macro.
//x > (eval 1)
//x = 1
//x > (eval (list 'car (list 'quote (list 2 3))))
//x = 2             ; Value of (car '(2 3))
LISP Eval(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  return iEval(ARG1);
}

//f (apply <exec> <args>)
//$ Apply <exec> to a list of arguments, <args>.
//$ The argument list is not evaluated further even if
//$ <exec> is a function, rather than a macro.
//x > (apply + '(1 2 3))
//x = 6             ; Value of (+ 1 2 3)
//x > (apply car '((2 3)))
//x = 2             ; Value of (car '(2 3))
LISP Apply(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(2);
  EXPECT_LIST(ARG2);
  return iApply(ARG1, ARG2);
}

//f (typeof <any>)
//$ Return the type of any lisp object.
//$ ~The type will always be one of the following,
//$ . nil
//$ . :cons       :symbol
//$ . :constant   :key
//$ . :character  :string
//$ . :integer    :float
//$ . :vector     :cvector
//$ . :subr       :subrl
//$ . :fsubr      :fsubrl
//$ . :file       :window
//x > (mapc typeof '(a 1 3.0))
//x = (:symbol :integer :float)
LISP Typeof(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);

  if(!ARG1) return NIL;

  switch(flags(ARG1))
  {
    case   tCONS: return lCONS;
    case    tSYM: return lSYMBOL;
    case  tCONST: return lCONSTANT;
    case    tKEY: return lKEY;
    case    tCHR: return lCHARACTER;
    case    tSTR: return lSTRING;
    case    tINT: return lINTEGER;
    case   tREAL: return lFLOAT;
    case   tVECT: return lVECTOR;
    case  tCVECT: return lCVECTOR;
    case   tSUBR: return lSUBR;
    case  tSUBRL: return lSUBRL;
    case  tFSUBR: return lFSUBR;
    case tFSUBRL: return lFSUBRL;
    case   tFILE: return lFILE;
    case   tWIND: return lWINDOW;

    // Don't attempt to display the damaged object as this will have
    // repercussions! Just report the error
    default:      return LispError(ERR_BAD_TYP, NONE, "The heap is very sick");
  }
  // return NIL;
}

//f (collect <flag>)
//$ Force a garbage collection.  If <flag> is present and not null
//$ then print the heap status.
//x > (collect t)
//x 25362 used - 19490 used = 5872 bytes freed
//x = nil
LISP Collect(byte nargs, LISP args[])
{
  CHECK_ARITY_LE(1);
  int oldused = hfree - heap;
  GC();
  int newused = hfree - heap;
  if(nargs && (args[0] != NIL))
    fwritef(stderr, "%d used - %d used = %d bytes freed\n",
            oldused, newused, oldused - newused);
  return NIL;
}
//#############################################################################
// Symbol functions
//#############################################################################

//f (symbol <name>)
//$ Return a symbol with the given <name> string.  The symbol is created
//$ if it does not already exist. (symbol "nil") will return nil even though
//$ nil is not a symbol since this behaviour is useful and probably expected.
//x > (symbol "t")        ; Just returned
//x = t
//x > (symbol "f*&==@##")
//x = *&==@##!            ; Probably created   :)
LISP Symbol(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  EXPECT_STR(ARG1);
  LISP result = FindSym(text(ARG1));
  if(result)
    return result;
  else
    return(NewSym(text(ARG1)));
}

//f (free <sym>)
//$ Create an uninterned symbol with the same name as <sym>.
//$ ~Uninterned symbols are not unique and cannot be identified
//$ by name, only by reference.
//x > (free 'a)
//x = a
//x > (eq 'x (free 'x))
//x = nil
//x > (eq (free 'x) (free 'x))
//x = nil
LISP Free(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  EXPECT_SYM(ARG1);
  int len = size(ARG1) - SYM_SIZE;
  strncpy(txtbuf, name(ARG1), len);
  txtbuf[len] = '\0';
  return NewSym(txtbuf, false);
}

//f (plist <symbol>)
//$ Returns the entire property list of <symbol> as an
//$ association list.
//x > (plist 'apple)
//x = ((variety . "Granny Smith") (colour . green))
LISP Plist(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  EXPECT_SYM(ARG1);
  return prop(ARG1);
}

//f (put <symbol> <name> <value>)
//$ Put <value> as a property named <name> on the
//$ property list of <symbol> and set it to <value>.
//x > (put 'bill 'friends 0)   ; bill has no friends!
//x = 0
LISP Put(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(3);
  EXPECT_SYM2(ARG1, ARG2);
  LISP entry = iAssoc(ARG2, prop(ARG1), ERR_BAD_PLIST);
  if(entry)
    cdr(entry) = ARG3;
  else
    prop(ARG1) = NewCons(NewCons(ARG2, ARG3), prop(ARG1));
  return ARG3;
}

//f (get <symbol> <name>)
//$ Get the value of the property of <symbol> named <name>.
//$ Return :undefined if <name> is not found.
//x > (get 'apple 'colour)
//x = "green"                ; for example
LISP Get(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(2);
  EXPECT_SYM2(ARG1, ARG2);
  LISP entry = iAssoc(ARG2, prop(ARG1), ERR_BAD_PLIST);
  if(entry)
    return cdr(entry);
  else
    return UNDEFINED;
}

//f (remprop <symbol> [<name>])
//$ Remove the named property from the symbol.  If <name> is omitted then
//$ remove all properties and return the original property list. Otherwise
//$ returns the value of the property removed or :undefined if the named
//$ property was not present.
//x > (remprop 'apple 'colour)
//x = "green"                ; for example
//x > (remprop 'apple 'colour)
//x = :undefined
LISP Remprop(byte nargs, LISP args[])
{
  if(nargs == 1)
  {
    EXPECT_SYM(ARG1);
    LISP result = prop(ARG1);
    prop(ARG1) = NIL;
    return result;
  }

  CHECK_ARITY_EQ(2);
  EXPECT_SYM2(ARG1, ARG2);
  LISP *last = &(prop(ARG1));
  LISP a = *last;
  if(!a) return NIL;

  while(a)
  {
    LISP e = car(a);
    if(IsAtom(e) || !IsSymbol(car(e)))
      return LispError(ERR_BAD_PLIST, a);

    if(car(e) == ARG2)
    {
      *last = cdr(a);
      return cdr(e);
    }
    last = &(cdr(a));
    a = *last;
  }
  return UNDEFINED;
}

//m (defconstant [<name> <value>]...)
//$ Define constants. If a symbol exists with the same name
//$ then it will be converted and references to it will become
//$ references to the constant.  Therefore, you may reference
//$ a constant before it is defined.  You must, of course, ensure
//$ that the constant is defined before it is used (dereferenced.)
//$ ~Keys cannot be converted to constants and attempting to do so
//$ will cause an error.  Converting a constant is legal and
//$ re-assigns the constant value.  This is the only way to modify
//$ a constant.
//x > (defconstant :red   0xFF0000
//x                :green 0x00FF00
//x                :blue  0x0000FF)
//x = :blue
LISP DefConstant(LISP& args)
{
  LISP c;
  while(args)
  {
    c = Pop(args);
    if(!args)
      return LispError(ERR_NUM_ARGS, NONE, "Args must occur in pairs");
    if(!IsSymbol(c) || IsKey(c))
      return LispError(ERR_SYM_EXP, c, "Arg must be a symbol or constant");
    // Convert the symbol passed as the first argument
    // or if a constant just refresh it
    flags(c) = tCONST;
    value(c) = iEval(Pop(args));
  }
  return c;
}

//m (defkey <name>...)
//$ Defines keys.  If a variable exists with the same name as any
//$ argument then it will be destroyed and references to it will
//$ become references to the key.
//$ Therefore, you may reference a key before it is defined. You
//$ must, of course, ensure that the key is defined before it is
//$ used (dereferenced.)
//$ ~Arguments may be keys, these are ignored. defkey returns its last
//$ argument as a key. Constants may not be converted to keys.
//x > (defkey :weekly :monthly :annual)
//x = :annual
//x > :monthly
//x = :monthly
LISP DefKey(LISP& args)
{
  LISP arg = NIL;
  while(args)
  {
    arg = Pop(args);
    if((!IsSymbol(arg)) || (flags(arg) == tCONST))
      return LispError(ERR_SYM_EXP, arg);
    // Convert the symbol passed as the first argument
    flags(arg) = tKEY;
    value(arg) = NIL;
  }
  return arg;
}

//f (destroy <sym> ...)
//$ Destroys symbols.  Removes them from the oblist so they will
//$ be reclaimed at the next GC.  Existing references to them
//$ will return :undefined.  You can destroy important system
//$ objects and this will break the interpreter.
//$ ~Don't use this function indiscriminately.  This is the only
//$ way to remove a key.
//x > (destroy 'x 'a)
//x = :undefined
LISP Destroy(LISP& args)
{
  while(args)
  {
    if(!IsSymbol(car(args)))
      return LispError(ERR_SYM_EXP, car(args));
    flags(car(args)) = tSYM;
    value(car(args)) = UNDEFINED;
    prop(Pop(args)) = NIL;
  }
  return UNDEFINED;
}

//f (oblist [<all>])
//$ Returns the list of all symbols that have a defined value or
//$ property.  Omits all symbols/constants/keys with names
//$ starting with ':' unless <all> is present and non-nil.
//x > (length (oblist))
//x = 275
//x > (length (oblist t))
//x = 411
LISP Oblist(byte nargs, LISP args[])
{
  CHECK_ARITY_LE(1);
  bool all = ((nargs == 1) && (ARG1 != NIL));
  bool again = false;

  // I am fairly sure that restarting on GC is unnecessary but this way
  // avoids pernicious bugs if the GC code is ever revised.

retry:  // restart here if GC occurs

  // Note: result is not protected and will be collected on GC
  LISP result = NIL;
  int gcs = GCCount;

  // List the names found in the symbol hashtable
  for(int i = 0; i < HASHSIZE; i++)
  {
    // Chase each hashchain
    for(LISP next = hashtable[i]; next; next = hash(next))
    {
      if((all || (name(next)[0] != ':')) &&
         ((value(next) != UNDEFINED) || (prop(next) != NIL)))
      {
        LISP cons = NewCons(next, NIL);
        // If GC occurs, abandon and try again
        // A GC on the second attempt is an error
        if(GCCount > gcs)
        {
          if(again)
            return LispError(ERR_HEAP);

          again = true;
          goto retry;
        }
        // result is still valid, link it and continue
        cdr(cons) = result;
        result = cons;
      }
    }
  }
  return result;
}

//m (let ((<symbol> <value>) ...) <body> ...)
//$ Creates local variables.  The expressions in body are
//$ evaluated in turn and the value of the last is returned.
//$ The first argument is a list of variable bindings that exist
//$ only during the evaluation of <body> ...
//$ ~Each variable binding is itself a list of two items, a
//$ symbol and an expression that is evaluated and initially
//$ bound to the symbol. All of the <value> expressions are
//$ evaluated in the environment surrounding the let.
//x > (let ((a 1) (b 2))
//x        (+ a b) )
//x = 3
LISP Let(LISP& args)
{
  protect(pargs, args);
  protect(bindings, car(pargs));

  LISP *locals = (LISP*)stop;
  // Evaluate the values into the stack frame
  while(bindings)
  {
    if(!IsCons(car(bindings)) || !IsSymbol(caar(bindings)))
      return LispError(ERR_LET_BIND, car(bindings));

    LISP val = cdar(bindings);

    if(val)
      val = iEval(car(val));
    PushF(NIL);                             // Space for symbol pointer
    PushF(val);

    bindings = cdr(bindings);
  }
  // Bind the variables and save the old environment
  bindings = car(pargs);

  while(bindings)
  {
    LISP variable = caar(bindings);

    *locals++ = variable;
    LISP oldval = value(variable);

    value(variable) = *locals;
    *locals++ = oldval;
    bindings = cdr(bindings);
  }
  // Evaluate the body
  return Progn(cdr(pargs));
}

//m (slet ((<symbol> <value>) ...) <body> ...)
//$ Creates local variables.  The expressions in body are
//$ evaluated in turn and the value of the last is returned.
//$ The first argument is a list of variable bindings that exist
//$ from their definition to the end of <body> ...
//$ ~Each variable binding is itself a list of two items, a
//$ symbol and an expression that is evaluated and initially
//$ bound to the symbol. The bindings take place sequentially
//$ and the <value> of any binding may refer to the local value
//$ of any previously bound symbol.
//x > (slet ((a 2) (b (* a 3)))
//x         (+ a b))
//x = 8
LISP Slet(LISP& args)
{
  protect(pargs, args);
  protect(bindings, car(pargs));

  // Evaluate the bindings into the stack frame
  while(bindings)
  {
    if(!IsCons(car(bindings)) || !IsSymbol(caar(bindings)))
      return LispError(ERR_LET_BIND, car(bindings));

    LISP val = cdar(bindings);
    if(val)
      val = iEval(car(val));

    LISP variable = caar(bindings);
    LISP oldval = value(variable);

    value(variable) = val;
    PushF(variable);
    PushF(oldval);
    bindings = cdr(bindings);
  }
  return Progn(cdr(pargs));
}

//f (gensym [<string>])
//$ Generate a new symbol using the prefix <string> and appending
//$ a number to create its name.  The prefix defaults to G.
//$ ~The numbers are sequential regardless of the prefix used
//$ but already defined symbols are skipped so that the result
//$ is always new and undefined.
//x > (gensym)
//x = G0
//x > (defkey (gensym ":key"))
//x = :key1
LISP Gensym(byte nargs, LISP args[])
{
  CHECK_ARITY_LE(1);
  char* prefix = "G";

  if(nargs == 1)
  {
    EXPECT_STR(ARG1);
    prefix = text(ARG1);
  }

  static unsigned int count = 0;
  do
  {
    sprintf(txtbuf, "%.8s%u", prefix, count++);
  } while(FindSym(txtbuf));
  return NewSym(txtbuf);
}

//#############################################################################
// List functions
//#############################################################################

//f (list [<arg> ...])
//$ Returns a list of the given arguments.
//x > (setq shopping (list 'tea 'sugar 'eggs))
//x = (tea sugar eggs)
LISP List(LISP& args)
{
  return args;
}

//f (length <list>), (length <vector>), (length <string>)
//$ Return the length of a vector, list or string.
//$ It counts only the top level elements; elements which
//$ are themselves vectors or lists are counted once.
//$ For a string argument it returns the length of the string
//$ in characters.
//x > (length '())
//x = 0
//x > (length '(a b c))
//x = 3
//x > (length '(a b . c))
//x = 3
//x > (length #(1 2 3 4 5))
//x = 5
//x > (length "Hello")
//x = 5
LISP Length(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  if(!ARG1)
    return NewInteger(0);

  if(IsAtom(ARG1) && !IsString(ARG1))
    return LispError(ERR_BAD_ARG, ARG1);

  return NewInteger(iLength(ARG1));
}

// Internal form of Length
// l must be a list, a vector or a string.
int iLength(LISP l)
{
  if(IsVector(l))
    return dim(l);

  if(IsString(l))
    return(strlen(text(l)));

  // Must be a list
  int i;
  for(i = 0; IsCons(l); i++)
    l = cdr(l);

  return l == NIL ? i : i + 1;
}

//f (nth <index> <list>)
//$ Returns the indexed top level item of the list.
//$ Element indices begin at zero.
//x > (nth 5 '(a b c d e f g))
//x = f
LISP Nth(byte nargs, LISP args[])
{
  return iNth(nargs, args, false);
}

//f (nthcdr <index> <list>)
//$ Returns the indexed top level cdr of the list.
//$ Element indices begin at zero.
//x > (nthcdr 0 '(a b c))
//x = (a b c)
//x > (nthcdr 2 '(0 1 (2 a) 3 (4 b c)))
//x = ((2 a) 3 (4 b c))
LISP NthCdr(byte nargs, LISP args[])
{
  return iNth(nargs, args, true);
}

// Helper function for nth and nthcdr
LISP iNth(byte nargs, LISP args[], bool rtncdr)
{
  CHECK_ARITY_EQ(2);
  EXPECT_INT(ARG1);
  EXPECT_LIST(ARG2);
  LISP iter = ARG2;

  int index = ivalue(ARG1);
  if((index < 0) || !ARG2)
    return LispError(ERR_INDEX, ARG1);

  while(index--)
  {
    if(IsAtom(iter))
      return LispError(ERR_INDEX, ARG1);
    iter = cdr(iter);
  }
  if(rtncdr)
    return iter;

  if(IsAtom(iter))
      return LispError(ERR_INDEX, ARG1);

  return car(iter);
}

//f (last <list>)
//$ Returns the last top level item of the list.
//$ Returns nil for an empty list.
//$ Improper lists are handled correctly.
//x > (last '((a b) (c d) (e f)))
//x = (e f)
//x > (last '(a b . c))
//x = c
LISP Last(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  if(!ARG1) return NIL;
  EXPECT_LIST(ARG1);
  LISP iter = ARG1;
  LISP result;
  while(IsCons(iter))
    result = Pop(iter);

  return cdr(result) ? cdr(result) : car(result);
}

//f (append [<list>]...)
//$ Create a single list containing the top-level items of each
//$ list concatenated. The rightmost argument may be an improper
//$ (dotted) list. The other arguments must be proper lists.
//x > (append)
//x = nil
//x > (append '(a b c) '(1 2 3))
//x = (a b c 1 2 3)
//x > (append '(a b c) '(1 2 . 3))
//x = (a b c 1 2 . 3)
//x > (append '(a (b c)) '((1 2) 3))
//x = (a (b c) (1 2) 3)
//x > (append '(a b c) '(1 2 3 4) '(+ - * /))
//x = (a b c 1 2 3 4 + - * /)
LISP Append(LISP& args)
{
  if(!args)
    return NIL;
  protect(pargs, args);
  protect(result, NewCons(NIL, NIL));
  protect(tail, result);
  protect(scan, NIL);

  while(pargs)
  {
    if(cdr(tail))
      LispError(ERR_APPEND_DOT, cdr(tail));

    scan = Pop(pargs);
    if(!scan)
      continue;
    EXPECT_CONS(scan);
    while(scan)
    {
      cdr(tail) = NewCons(NIL, NIL);
      tail = cdr(tail);
      car(tail) = car(scan);
      if(IsAtom(cdr(scan)))
        cdr(tail) = cdr(scan), scan = NIL;
      else
        scan = cdr(scan);
    }
  }
  return cdr(result);
}

//f (delete <atom> <list>)
//$ Returns the list with any top-level atom eql to <atom>
//$ deleted.
//$ ~The filter function provides a more general facility
//$ to filter list items based on any predicate.
//x > (string (delete #\s (chars "Mississippi")))
//x = "Miiippi"
LISP Delete(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(2);
  EXPECT_ATOM(ARG1);
  EXPECT_LIST(ARG2);

  protect(parg, ARG2);
  while(parg && iEql(ARG1, car(parg)))
    parg = cdr(parg);

  if(!parg) return NIL;

  protect(result, NewCons(Pop(parg), NIL));
  protect(tail, result);

  while(true)
  {
    while(parg && iEql(ARG1, car(parg)))
      parg = cdr(parg);

    if(!parg) return result;

    LISP t = NewCons(Pop(parg), NIL);
    cdr(tail) = t;
    tail = t;
  }
}

//f (reverse <list>) (reverse <vector> [<len>])
//$ Returns a list of the top level items of <list> reversed.
//$ ~With a vector argument it returns the first <len> items
//$ reversed into a new vector of size <len>.  <len> defaults
//$ to the length of the input vector.
//x > (string (reverse (chars "Madam I'm Adam")))
//x = "madA m'I madaM"
LISP Reverse(byte nargs, LISP args[])
{
  CHECK_ARITY_LE(2);
  if(!ARG1) return NIL;
  if(IsVector(ARG1))
  {
    int size = dim(ARG1);
    if(nargs == 2)
    {
      EXPECT_INT(ARG2);
      int s = ivalue(ARG2);
      if(s < 1)
        return LispError(ERR_VEC_DIM, ARG2);
      if(s > size)
        return LispError(ERR_INDEX, ARG2);
      size = s;
    }
    LISP result = NewVector(size, NIL);
    for(int i = 0, j = size - 1; i < size; i++, j--)
      data(result)[i] = data(ARG1)[j];
    return result;
  }
  CHECK_ARITY_EQ(1);
  EXPECT_CONS(ARG1);
  protect(parg, ARG1);
  LISP result = NIL;
  while(IsCons(parg))
    Push(Pop(parg), result);

  return(result);
}

//f (assoc <key> <alist>)
//$ Lookup a key-value pair in an association list.  Returns nil
//$ if the key is not found.
//x > (assoc "x" '(("w" . 3) ("x" . 7) ("y" . 4) ("z" . 1)))
//x = ("x" . 7)
LISP Assoc(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(2);
  EXPECT_LIST(ARG2);
  return iAssoc(ARG1, ARG2, ERR_BAD_ASSOC);
}

//f (cdrassoc <key> <alist>)
//$ Lookup a value in an association list.  Returns :undefined
//$ if the key is not found.
//x > (cdrassoc "x" '(("w" . 3) ("x" . 7) ("y" . 4) ("z" . 1)))
//x = 7
LISP CdrAssoc(byte nargs, LISP args[])
{
  LISP result = Assoc(nargs, args);
  return result ? cdr(result) : UNDEFINED;
}

// Helper function for Assoc
// err is the error to report upon failure
LISP iAssoc(LISP x, LISP a, LERROR err)
{
  EXPECT_ATOM(x);
  if(!a)
    return NIL;
  while(a)
  {
    if(IsAtom(a))
      return LispError(err, a);

    LISP e = car(a);

    if(IsAtom(e) || !IsAtom(car(e)))
      return LispError(err, e);

    if(iEql(car(e), x))
      return e;
    a = cdr(a);
  }
  return NIL;
}

//f (member <atom> <list>)
//$ Return a true value if <atom> is a top-level member of <list>
//$ else return nil.  The true value returned is the sublist
//$ of <list> starting with <atom> rather than the symbol t.
//x > (member #\p (chars "open"))
//x = (#\p #\e #\n)
//x > (member #\q (chars "open"))
//x = nil
LISP Member(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(2);
  EXPECT_ATOM(ARG1);
  EXPECT_LIST(ARG2);
  return iMember(ARG1, ARG2);
}

// Internal version of Member
LISP iMember(LISP a, LISP b)
{
  while(b)
  {
    if(iEql(a, car(b)))
      return b;
    b = cdr(b);
  }
  return NIL;
}

//f (filter <pred> <list> [<keep>])
//$ Returns <list> with its top-level items filtered.  Each item
//$ is passed to function <pred> and those which produce a result
//$ equal to the boolean <keep> are kept.  <keep> is t by default,
//$ (i.e. the predicate must return true to keep an item.)
//x > (filter numberp '(a 1 b c 3 5 d))
//x = (1 3 5)
//x > (filter numberp '(a 1 b c 3 5 d) nil)
//x = (a b c d)
LISP Filter(byte nargs, LISP args[])
{
  CHECK_ARITY_IN(2, 3);
  EXPECT_LIST(ARG2);

  bool keep = true;

  if(nargs == 3)
    keep = ARG3 != NIL;

  protect(arg, NewCons(NIL, NIL));

  protect(parg, ARG2);
  while(parg)
  {
    car(arg) = car(parg);
    if((iApply(ARG1, arg) != NIL) == keep)
      break;
    parg = cdr(parg);
  }
  if(!parg) return NIL;

  protect(result, NewCons(Pop(parg), NIL));
  protect(tail, result);

  while(true)
  {
    while(parg)
    {
      car(arg) = car(parg);
      if((iApply(ARG1, arg) != NIL) == keep)
        break;
      parg = cdr(parg);
    }
    if(!parg) return result;

    LISP tl = NewCons(Pop(parg), NIL);
    cdr(tail) = tl;
    tail = tl;
  }
}

//f (mapc <func> <list>)
//$ Applies the function to each element of the list and returns
//$ a list of the results.
//x > (mapc - (1 2 3))
//x = (-1 -2 -3)
//x > (mapc '(lambda (x) (* x 2)) '(1 2 3))
//x = (2 4 6)
LISP Mapc(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(2);
  EXPECT_LIST(ARG2);

  if(!ARG2)
    return NIL;

  protect(result, NewCons(NIL, NIL));
  protect(tail, result);
  protect(parg, ARG2);

  while(true)
  {
    LISP arglist = NewCons(Pop(parg), NIL);
    car(tail) = iApply(ARG1, arglist);
    if(!parg) return result;
    cdr(tail) = NewCons(NIL, NIL);
    tail = cdr(tail);
  }
}

//m (push <item> <sym>)
//$ Push any item onto the front of the list. <sym> must be a
//$ symbol and its value will be modified.  The value of sym need
//$ not be a list and the result will be a dotted pair in this case.
//$ ~Returns the modified list. The item can be removed with pop.
//x > (setq stack nil)
//x = nil
//x > (push stack 'a)
//x = (a)
//x > (push stack 'b)
//x = (b a)
//x > (pop stack)
//x = b
//x > (pop stack)
//x = a
//x > stack
//x = nil
LISP Push(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(2);
  EXPECT_SYM(ARG2);
  LISP list = NewCons(iEval(ARG1), value(ARG2));
  iSet(ARG2, list);
  return list;
}

//m (pop <sym>)
//$ Pop an item from the front of the list. <sym> must be a
//$ symbol that points at a list.
//x > (setq x '(a b c))
//x = (a b c)
//x > (pop x)
//x = a
//x > x
//x = (b c)
LISP Pop(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  EXPECT_SYM(ARG1);
  LISP list = value(ARG1);
  if(!list)
    return NIL;
  if(!IsCons(list))
    return LispError(ERR_BAD_ARG, list);
  iSet(ARG1, cdr(list));
  return car(list);
}

//#############################################################################
// Vector functions
//#############################################################################

//f (make-vector <dim> [<init>])
//$ Create a vector of <dim> values optionally initialised with
//$ init.  If <init> is omitted, initialise with nil.
//x > (make-vector 3)
//x = #(nil nil nil)
LISP MakeVector(byte nargs, LISP args[])
{
  CHECK_ARITY_IN(1, 2);
  if(!IsInteger(ARG1) || (ivalue(ARG1) < 1))
    return LispError(ERR_VEC_DIM, ARG1);

  LISP init = (nargs == 2) ? ARG2 : NIL;
  return NewVector(ivalue(ARG1), init);
}

//f (vector <list-vector-or-string>)
//$ Shallow copy a list or vector to a vector.
//$ Convert a string to a vector of characters.
//$ Use this to make writeable copies of literal vectors
//x > (vector "hello")
//x = #(#\h #\e #\l #\l #\o)
LISP Vector(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  if(IsString(ARG1))
  {
    int len = strlen(text(ARG1));
    protect(result, NewVector(len, NIL));

    for(int i = 0; i < len; i++)
      data(result)[i] = NewChar(text(ARG1)[i]);

    return result;
  }
  if(IsVector(ARG1))  // Shallow copy
  {
    LISP result = NewVector(dim(ARG1), NIL);
    for(int i = 0; i < dim(ARG1); i++)
      data(result)[i] = data(ARG1)[i];

    return result;
  }
  EXPECT_CONS(ARG1);

  LISP result = iVector(ARG1);
  return result;
}

// Convert a list to a vector by shallow copying.
LISP iVector(LISP l)
{
  protect(lst, l)
  int len = iLength(lst);
  LISP result = NewVector(len, NIL);

  for(int i = 0; i < len; i++)
    data(result)[i] = Pop(lst);

  return result;
}

//f (vlist <vector>)
//$ Convert a vector to a list.
//x > (vlist (vector "abc"))
//x = (#\a #\b #\c)
LISP Vlist(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  EXPECT_VEC(ARG1);
  int d = dim(ARG1);
    if(!d) return NIL;

  protect(result, NewCons(data(ARG1)[0], NIL));
  protect(tail, result)

  for(int i = 1; i < d; i++)
  {
    LISP t = NewCons(data(ARG1)[i], NIL);
    cdr(tail) = t;
    tail = t;
  }
  return result;
}

//f (vref <vector> <index>)
//$ Return the value at the given index of the vector.
//x > (vref #(x y z) 2)
//x = z
LISP Vref(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(2);
  EXPECT_VEC(ARG1);
  EXPECT_INT(ARG2);

  int i = ivalue(ARG2);
  if((i < 0) || (i >= dim(ARG1)))
    return LispError(ERR_INDEX, ARG2);
  return data(ARG1)[i];
}

//f (vset <vector> <index> <value>)
//$ Set the value at the given index of the vector and return
//$ the new value.
//$ This function will not modify literal vectors (e.g.
//x > (setq v #(x y z))
//x = #(x y z)
//x > (vset v 1 'q)
//x = q
//x > v
//x = #(x q z)
LISP Vset(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(3);
  EXPECT_VEC(ARG1);
  EXPECT_INT(ARG2);
  if(IsCVector(ARG1))
    LispError(ERR_IMMUTABLE, NONE, "Cannot modify literal vector");

  int i = ivalue(ARG2);
  if((i < 0) || (i >= dim(ARG1)))
    return LispError(ERR_INDEX, ARG2);
  return data(ARG1)[i] = ARG3;
}

// This function compares values for vsort
// a, b are the values compared
// args is a ready-made arg list for fun
// fun is the comparer function or NIL (to use the dafault lexcmp)
int cmp(LISP a, LISP b, LISP args, LISP fun)
{
  if(fun == NIL)
  return iLexCmp(a, b);

  car(args) = a;
  cadr(args) = b;
  LISP result = iApply(fun, args);
  if(!IsInteger(result))
    LispError(ERR_VSORT_CMP);

  return ivalue(result);
}

#define  MAX_LEVELS  1000

//f (vsort <vector> [<cmp>]>
//$ Sort a vector. (<cmp> a b) should return an integer,
//$ <0 if a < b, 0 if a = b and >0 if a > b assuming an ascending sort.
//$ Negate these results for a descending sort (or reverse the result.)
//$ The default value for <cmp> is lexcmp which does the right thing
//$ for ascending sorts of homogeneous vectors and canonicalisation
//$ of inhomogeneous vectors.
LISP Vsort(byte nargs, LISP args[])
{
  CHECK_ARITY_IN(1, 2);
  EXPECT_VEC(ARG1);
  if(IsCVector(ARG1))
    LispError(ERR_IMMUTABLE, NONE, "Cannot modify literal vector");
  protect(piv, NIL);

  LISP cmpargs = NIL;


  protect(cmpfun, NIL)
  if(nargs == 2)
  {
    cmpfun = ARG2;
    cmpargs = NewCons(NIL, NIL);
    cmpargs = NewCons(NIL, cmpargs);
  }
  protect(pcmpargs, cmpargs);

  int beg[MAX_LEVELS];
  int end[MAX_LEVELS];
  int i=0;
  int L, R;

  beg[0] = 0;
  end[0] = dim(ARG1);
  while(i >= 0)
  {
    L = beg[i];
    R = end[i] - 1;
    if(L < R)
    {
      piv = data(ARG1)[L];

      if(i == MAX_LEVELS - 1)
        LispError(ERR_DEBUG);
      while(L < R)
      {
        while((cmp(data(ARG1)[R], piv,  pcmpargs, cmpfun) >= 0) && (L < R))
          R--;

        if(L < R)
          data(ARG1)[L++] = data(ARG1)[R];

        while((cmp(data(ARG1)[L], piv, pcmpargs, cmpfun) <= 0) && (L < R))
          L++;

        if(L < R)
          data(ARG1)[R--] = data(ARG1)[L];
      }
      data(ARG1)[L] = piv;
      beg[i + 1] = L + 1;
      end[i + 1] = end[i];
      end[i++] = L;
    }
    else
    {
      i--;
    }
  }

  return ARG1;
}

//#############################################################################
// Character functions
//#############################################################################

//f (char <string> <index>) (char <integer>)
//$ Return the indexed character from the string or, with a
//$ single integer argument, returns the character with the
//$ given ASCII code. The first character of a string has
//$ index 0.
//x > (char "frobozz" 3)
//x = #\b
//x > (char 0x41)
//x = #\A
LISP Char(byte nargs, LISP args[])
{
  if(nargs == 1)
  {
    EXPECT_INT(ARG1);
    int c = ivalue(ARG1);
    if((c < 0) || (c > 255))
      return LispError(ERR_BAD_ARG, ARG1);
    return NewChar((char)c);
  }
  CHECK_ARITY_EQ(2);
  EXPECT_STR(ARG1);
  EXPECT_INT(ARG2);
  int i = ivalue(ARG2);
  if((i < 0) || (i >= (int)strlen(text(ARG1))))
    return LispError(ERR_INDEX, ARG2);
  return NewChar(text(ARG1)[i]);
}

//f (char= <char> <char> [<mode>])
//$ Character comparison predicate. Return t if the chars are
//$ equal.  Mode can be :case (default) or :nocase.
//x > (char= #\A #\a :nocase)
//x = t
LISP CharEqual(byte nargs, LISP args[])
{
  int r = iCharCompare(nargs, args);
  return Boolean(r == 0);
}

//f (char<> <char> <char> [<mode>])
//$ Character comparison predicate. Return t if the chars are
//$ unequal.  Mode can be :case (default) or :nocase.
//x > (char<> #\A #\a)
//x = t
LISP CharNEqual(byte nargs, LISP args[])
{
  return Boolean(iCharCompare(nargs, args) != 0);
}

//f (char> <char> <char> [<mode>])
//$ Character comparison predicate. Return t if the first char
//$ follows the second.  Mode can be :case (default) or :nocase.
LISP CharGreater(byte nargs, LISP args[])
{
  return Boolean(iCharCompare(nargs, args) > 0);
}

//f (char< <char> <char> [<mode>])
//$ Character comparison predicate. Return t if the first char
//$ preceeds the second.  Mode can be :case (default) or :nocase.
LISP CharLess(byte nargs, LISP args[])
{
  return Boolean(iCharCompare(nargs, args) < 0);
}

//f (char<= <char> <char> [<mode>])
//$ Character comparison predicate. Return nil if the first char
//$ follows the second.  Mode can be :case (default) or :nocase.
LISP CharNGreater(byte nargs, LISP args[])
{
  return Boolean(iCharCompare(nargs, args) <= 0);
}

//f (char>= <char> <char> [<mode>])
//$ Character comparison predicate. Return nil if the first char
//$ preceeds the second.  Mode can be :case (default) or :nocase.
LISP CharNLess(byte nargs, LISP args[])
{
  return Boolean(iCharCompare(nargs, args) >= 0);
}

// Compare two characters and return the integer difference
int iCharCompare(byte nargs, LISP args[])
{
  if((nargs < 2) || (nargs > 3))
  {
    LispError(ERR_NUM_ARGS);
    return 0;
  }
  if(!IsChar(ARG1))
  {
    LispError(ERR_CHR_EXP, ARG1);
    return 0;
  }
  if(!IsChar(ARG2))
  {
    LispError(ERR_CHR_EXP, ARG2);
    return 0;
  }

  LISP mode = CASE;
  if(nargs == 3)
    mode = ARG3;

  if(mode == CASE)    return chr(ARG1) - chr(ARG2);
  if(mode == NOCASE)  return (toupper(chr(ARG1)) - toupper(chr(ARG2))) & 0xFF;

  LispError(ERR_INV_KEY, ARG3, "Invalid char compare mode");

  return 0;
}

//f (upper-casep <char>)
//$ Predicate that returns t if <char> is an upper case letter.
LISP UpperCasep(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  EXPECT_CHR(ARG1);
  return Boolean(isupper(chr(ARG1)));
}

//f (lower-casep <char>)
//$ Predicate that returns t if <char> is a lower case letter.
LISP LowerCasep(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  EXPECT_CHR(ARG1);
  return Boolean(islower(chr(ARG1)));
}

//#############################################################################
// String functions
//#############################################################################

//f (string <any>)
//$ Convert numbers and symbols to strings.  Will also convert a
//$ character or a list or vector of characters to a string.
//$ ~A list argument must be a flat list of characters only.
//$ ~Vectors of characters will be converted up to the first nil.
//$ The vectors content after the first nil is irrelevant
//$ but all prior items must be characters.  If there is no nil
//$ marker then the entire vector will be converted.
//x > (string 123)
//x = "123"          ; depends on integer-format setting
//x > (string #(#\a #\b #\c nil #\d wibble))
//x = "abc"
LISP String(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  if(!ARG1)
#ifdef UPPER_CASE_SYMBOLS
    return NewString("NIL");
#else
    return NewString("nil");
#endif
  if(IsSymbol(ARG1))
  {
    memcpy(txtbuf, name(ARG1), ARG1->size - SYM_SIZE);
    txtbuf[ARG1->size - SYM_SIZE] = '\0';
    return NewString(txtbuf);
  }

  if(IsNumber(ARG1))
  {
    if(IsReal(ARG1))
      sprintf(txtbuf, ffmt, rvalue(ARG1));
    else
      sprintf(txtbuf, ifmt, ivalue(ARG1));
    return NewString(txtbuf);
  }

  if(IsChar(ARG1))
  {
    txtbuf[0] = chr(ARG1);
    txtbuf[1] = '\0';
    return NewString(txtbuf);
  }

  if(IsString(ARG1))
    return ARG1;

  if(IsCons(ARG1))
  {
    LISP c = ARG1;
    int i;

    for(i = 0; (i < BUFSZ) && c; i++)
    {
      if(IsChar(car(c)))
        txtbuf[i] = chr(Pop(c));
      else
        return LispError(ERR_CHR_EXP, ARG1);
    }
    if(i == BUFSZ)
      return LispError(ERR_STR_LEN, ARG1, "String buffer overflow");

    txtbuf[i] = '\0';
    return NewString(txtbuf);
  }

  if(IsVector(ARG1))
  {
    int i;
    int d = dim(ARG1);

    // Check and count characters stopping on nil
    for(i = 0; i < d; i++)
    {
      LISP elem = data(ARG1)[i];
      if(!elem) break;
      if(!IsChar(elem))
        return LispError(ERR_CHR_EXP, elem, "Not a vector of characters");
    }

    d = i;

    // Get a string of the correct size
    LISP result = NewString(d + 1);
    char* str = text(result);

    // Transfer characters to string
    for(i = 0; i < d; i++)
      *str++ = chr(data(ARG1)[i]);

    *str = '\0';
    return result;
  }
  return LispError(ERR_BAD_ARG, ARG1, "Can't convert to string");

  // return NIL;
}

//f (chars <string>)
//$ Convert a string to a list of characters.
//$ This conversion can be reversed by string.
//x > (chars "Hello")
//x = (#\H #\e #\l #\l #\o)
LISP Chars(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  EXPECT_STR(ARG1);
  protect(s, ARG1);
  protect(result, NIL);
  for(int i = strlen(text(s)) - 1; i >= 0; i--)
  {
    LISP c = NewChar(text(s)[i]);
    result = NewCons(c, result);
  }
  return result;
}

//f (schar <string> <index> <char>)
//$ Returns <string> with the character at <index> set to <char>.
//$ The first character of a string has index 0.
//x > (schar "abcde" 3 #\z)
//x = "abcze"
LISP Schar(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(3);
  EXPECT_STR(ARG1);
  EXPECT_INT(ARG2);
  EXPECT_CHR(ARG3);

  int i = ivalue(ARG2);
  if((i < 0) || (i >= (int)strlen(text(ARG1))))
    return LispError(ERR_INDEX, ARG2);

  LISP result = NewString(text(ARG1));
  text(result)[i] = chr(ARG3);
  return result;
}


//f (string= <string> <string> [<mode>])
//$ String comparison predicate. Return t if the strings are
//$ equal.  Mode can be :case (default), :nocase or :collate.
//$ ~The operation of :collate depends on the locale setting.
//x > (string= "Hello" "HeLlO" :nocase)
//x = t
LISP StringEqual(byte nargs, LISP args[])
{
  return Boolean(iStringCompare(nargs, args) == 0);
}

//f (string<> <string> <string> [<mode>])
//$ String comparison predicate. Return t if the strings are
//$ unequal.  Mode can be :case (default), :nocase or :collate.
//$ ~The operation of :collate depends on the locale setting.
//x > (string<> "0ne" (string 1))
//x = t
LISP StringNEqual(byte nargs, LISP args[])
{
  return Boolean(iStringCompare(nargs, args) != 0);
}

//f (string> <string> <string> [<mode>])
//$ String comparison predicate. Return t if the first string
//$ follows the second.  Mode can be :case (default), :nocase
//$ or :collate.
//$ ~The operation of :collate depends on the
//$ locale setting.

LISP StringGreater(byte nargs, LISP args[])
{
  return Boolean(iStringCompare(nargs, args) > 0);
}

//f (string< <string> <string> [<mode>])
//$ String comparison predicate. Return t if the first string
//$ preceeds the second.  Mode can be :case (default), :nocase
//$ or :collate.
//$ ~The operation of :collate depends on the
//$ locale setting.
LISP StringLess(byte nargs, LISP args[])
{
  return Boolean(iStringCompare(nargs, args) < 0);
}

//f (string<= <string> <string> [<mode>])
//$ String comparison predicate. Return nil if the first string
//$ follows the second.  Mode can be :case (default), :nocase
//$ or :collate.
//$ ~The operation of :collate depends on the
//$ locale setting.
LISP StringNGreater(byte nargs, LISP args[])
{
  return Boolean(iStringCompare(nargs, args) <= 0);
}

//f (string>= <string> <string> [<mode>])
//$ String comparison predicate. Return nil if the first string
//$ preceeds the second.  Mode can be :case (default), :nocase
//$ or :collate.
//$ ~The operation of :collate depends on the
//$ locale setting.
LISP StringNLess(byte nargs, LISP args[])
{
  return Boolean(iStringCompare(nargs, args) >= 0);
}

// Compare two strings and return a result of -1/0/1 accordingly
int iStringCompare(byte nargs, LISP args[])
{
  if((nargs < 2) || (nargs > 3))
  {
    LispError(ERR_NUM_ARGS);
    return 0;
  }
  if(!IsString(ARG1))
  {
    LispError(ERR_STR_EXP, ARG1);
    return 0;
  }
  if(!IsString(ARG2))
  {
    LispError(ERR_STR_EXP, ARG2);
    return 0;
  }
  LISP mode = CASE;
  if(nargs == 3)
    mode = ARG3;

  if(mode == CASE)    return strcmp(text(ARG1), text(ARG2));
  if(mode == NOCASE)  return stricmp(text(ARG1), text(ARG2));
  if(mode == COLLATE) return strcoll(text(ARG1), text(ARG2));

  LispError(ERR_INV_KEY, ARG3, "Invalid string compare mode");

  return 0;
}

//f (upper-case <string>) (upper-case <char>)
//$ Returns the argument converted to upper case.
//x > (upper-case "Hello")
//x = "HELLO"
//x > (upper-case #\f)
//x = #\F
LISP UpperCase(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  if(IsChar(ARG1))
    return NewChar((char)toupper(chr(ARG1)));
  EXPECT_STR(ARG1);
  LISP result = NewString(text(ARG1));
  strupr(text(result));
  return result;
}

//f (lower-case <string>) (lower-case <char>)
//$ Returns the argument converted to lower case.
//x > (lower-case "Hello")
//x = "hello"
//x > (lower-case #\e)
//x = #\e
LISP LowerCase(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  if(IsChar(ARG1))
    return NewChar((char)tolower(chr(ARG1)));
  EXPECT_STR(ARG1);
  LISP result = NewString(text(ARG1));
  strlwr(text(result));
  return result;
}

//f (string-search <string> <string>) (string-search <string> <char>)
//$ Searches the first argument string for the first ocurrence of any
//$ of the characters in the second argument.  If found, then the
//$ matching character's index is returned, otherwise return nil.
//x > (string-search "1,2,3" #\,)
//x = 1
//x > (string-search "abc efg;mno" "; ")
//x = 3
LISP StringSearch(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(2);
  EXPECT_STR(ARG1);

  char *target = " ";
  if(IsChar(ARG2))
    *target = chr(ARG2);
  else if (IsString(ARG2))
    target = text(ARG2);
  else
    return LispError(ERR_BAD_ARG, ARG2, "String or character expected");

  unsigned int r = strcspn(text(ARG1), target);
  if(r == strlen(text(ARG1)))
    return NIL;
  else
    return NewInteger(r);
}

//f (concat [<string>]...)
//$ Concatenate strings. (concat) returns "".
//x > (concat "Hello " "World " "from " "AJBLisp")
//x = "Hello World from AJBLisp"
LISP Concat(LISP& args)
{
  if(!args)
    return NewString("");

  if(!cdr(args))
  {
    EXPECT_STR(car(args));
    return car(args);
  }

  // Get total length
  int length = 1;
  for(LISP l = args; IsCons(l);)
  {
    EXPECT_STR(car(l));
    length += strlen(text(Pop(l)));
  }

  LISP result = NewString(length);
  text(result)[0] = '\0';
  for(LISP l = args; IsCons(l); )
    strcat(text(result), text(Pop(l)));

  return result;
}

//f (substring <string> <start> [<for>])
//$ Extract a substring of a <string> starting at <start>
//$ for <for> characters.  If <for> is omitted then return all
//$ of the characters until the end of <string>.
//x > (substring "Hello" 0 4)
//x = "Hell"
//x > (substring "Hello World" 6)
//x = "World"
LISP Substring(byte nargs, LISP args[])
{
  int from;
  int size = -1;

  CHECK_ARITY_IN(2, 3);
  EXPECT_STR(ARG1);
  EXPECT_INT(ARG2);
  from = ivalue(ARG2);
  if(from < 0)
    from = 0;
  if(nargs == 3)
  {
    EXPECT_INT(ARG3);
    size = ivalue(ARG3);
    if(size < 0)
      size = -1;
  }

  int len = strlen(text(ARG1)) - from;
  if(len < 0)
    return NewString("");

  if((size > 0) && (len > size))
    len = size;
  LISP result = NewString(len + 1);

  memcpy(text(result), text(ARG1) + from, len);
  text(result)[len] = '\0';
  return result;
}

// Internal function to read a token from a string from character
// index i
LISP iToken(LISP arg, int& i)
{
  char *sp = &text(arg)[i];
  char *tp = txtbuf;
  LISP result;

  while(isspace(*sp))
    sp++, i++;

  if(!*sp)
    return NIL;

  else if(isalpha(*sp))
  {
    // It's a symbol
    while(isalnum(*sp) && (i < (BUFSZ - 1)))
    {
      i++;
      *tp++ = *sp++;
    };
    *tp = '\0';
    // Does this symbol exist?
#ifdef UPPER_CASE_SYMBOLS
    if(!stricmp(txtbuf, "NIL"))
#else
    if(!strcmp(txtbuf, "nil"))
#endif
      result = NIL;
    else
    {
      result = FindSym(txtbuf);
      if(!result)
        result = NewSym(txtbuf);
    }
  }

  else if(isdigit(*sp))
  {
    // It's a number
    double d;
    char *end;

    d = strtod(sp, &end);
    i += (end - sp);
    result = NewReal(d);
  }

  else // It's punctuation
  {
    result = NewChar(*sp);
    i ++;
  }
  return result;
}

//f (tokenise <string>)
//$ Create a list of tokens from a string. Tokens can be:
//$ alphanumeric symbols that start with a letter, returned
//$ as symbols; numbers, returned as floats or anything else
//$ returned as a character.
//x > (tokenise "fred+2*a")
//x = (fred #\+ 2.00000000 #\* a)
LISP Tokenise(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  EXPECT_STR(ARG1);
  int i = 0;
  LISP tok = iToken(ARG1, i);
  if(!tok)
    return NIL;
  protect(result, NewCons(tok, NIL));
  protect(tail, result);

  while(true)
  {
    tok = iToken(ARG1, i);
    if(!tok) break;
    cdr(tail) = NewCons(tok, NIL);
    tail = cdr(tail);
  }
  return result;
}

//#############################################################################
// Date/Time functions
//#############################################################################

//f (tzset [<zone>])
//$ Sets the timezone according to the TZ environment variable.
//$ If a timezone string is provided then TZ will be set to this
//$ value before setting the timezone itself.  Therefore (tzset <string>)
//$ is identical in effect to (environ "TZ" <string>) (tzset).
//$ ~The TZ variable can be read by evaluating (environ "TZ") but the
//$ returned value is not necessarily the current timezone unless tzset
//$ has been evaluated with or without an argument.  To avoid confusion
//$ it is best to explicitly set the timezone in init.lsp.
//$ ~Neither the argument or the value of TZ are checked for validity.
//$ If an argument is provided it will be truncated after the 16th character.
//x > (tzset "GMT0BST")
//x = nil
LISP Tzset(byte nargs, LISP args[])
{
  CHECK_ARITY_LE(1);
  static char tz[20] = "TZ=";
  if(nargs == 1)
  {
    EXPECT_STR(ARG1);
    strncpy(&tz[3], text(ARG1), 16);
    putenv(tz);
  }
  _tzset();
  return NIL;
}

//f (localtime [<bool>])
//$ Sets time format to localtime if <bool> is true and to UTC if false.
//$ ~Local time automatically takes account of daylight savings time
//$ unless this has been disabled with (dst nil). (localtime) returns the
//$ current setting without changing it.
LISP LocalTime(byte nargs, LISP args[])
{
  CHECK_ARITY_LE(1);
  if(nargs == 1)
    utcflag = (ARG1 == NIL);

  return utcflag ? NIL : T;
}

//f (dst [<on>])
//$ Causes local times to be input/output with daylight savings time taken
//$ into account if <on> is true.  This setting has no effect if UTC mode
//$ has been set with (local-time nil). With no argument dst returns the
//$ current setting without changing it.  Dst dates are provided by Windows
//$ and are therefore unaffected by tzset.
LISP Dst(byte nargs, LISP args[])
{
  CHECK_ARITY_LE(1);
  if(nargs >= 1)
    dstflag = (ARG1 != NIL);
  return Boolean(dstflag);
}

// Get the current date/time as an integer number of seconds
// since 1-JAN-1970 00:00:00 UTC
int iNow()
{
  //return (int)time(NULL);
  SYSTEMTIME st;
  GetSystemTime(&st);
  return iEncodeTime(st.wYear, st.wMonth, st.wDay,
                     st.wHour, st.wMinute, st.wSecond);
}

//f (now)
//$ Get the current date/time as an integer number of seconds
//$ since 1-JAN-1970 00:00:00 UTC
LISP Now(LISP& args)
{
  if(args != NIL)
    return LispError(ERR_NUM_ARGS);

  return NewInteger(iNow());
}

// Decode an integer time
tm* iDecodeTime(time_t itime)
{
  tm *dectime;

  _daylight = 0;

  if(utcflag)
  {
    dectime = gmtime(&itime);
  }
  else
  {
    //itime -= _timezone;
    dectime = gmtime(&itime);

    // Local summertime rules
    if(dstflag)
      iSummerTime(dectime, itime);
  }
  if(!dectime)
    LispError(ERR_BAD_ARG, NONE, "Invalid integer time");

  return dectime;
}

// Get a decoded time from the integer argument or the system time
tm* iTimeArgs(byte nargs, LISP args[])
{
  time_t itime;

  if(nargs == 0)
  {
    itime = iNow();
  }
  else
  {
    if(nargs != 1)
    {
      LispError(ERR_NUM_ARGS);
      return iDecodeTime(0);
    }
    if(!IsInteger(ARG1))
    {
      LispError(ERR_INT_EXP, ARG1);
      return iDecodeTime(0);
    }
    if(ivalue(ARG1) < 0)
    {
      LispError(ERR_BAD_ARG, ARG1, "Invalid integer time");
      itime = (time_t)ivalue(ARG1);
    }
    else
      itime = (time_t)ivalue(ARG1);
  }

  return iDecodeTime((int)itime);
}

//f (date [<itime>])
//$ Returns a list of (year month day) as integers for the
//$ integer time or for the current time if the argument is
//$ omitted.
//$ ~The allowable range of calendar times is
//$ Jan 1 1970 00:00:00 to Jan 19 2038 03:14:07 UTC.
//x > (date 0)
//x = (1970 1 1)
LISP Date(byte nargs, LISP args[])
{
  tm* dectime = iTimeArgs(nargs, args);
  protect(result, NIL);
  result = NewCons(NewInteger(dectime->tm_mday), NIL);
  result = NewCons(NewInteger(dectime->tm_mon + 1), result);
  result = NewCons(NewInteger(dectime->tm_year + 1900), result);
  return result;
}

//f (time [<itime>])
//$ Returns a list of (hours minutes seconds) as integers for
//$ the integer time or for the current time if the argument
//$ is omitted.
//$ ~The allowable range of calendar times is
//$ Jan 1 1970 00:00:00 to Jan 19 2038 03:14:07 UTC.
//x > (time)
//x = (3 45 32)
LISP Time(byte nargs, LISP args[])
{
  tm* dectime = iTimeArgs(nargs, args);
  protect(result, NIL);
  result = NewCons(NewInteger(dectime->tm_sec), NIL);
  result = NewCons(NewInteger(dectime->tm_min), result);
  result = NewCons(NewInteger(dectime->tm_hour), result);
  return result;
}

//f (encode-time <y> <m> <d> [<H> <M> <S>])
//$ Encodes a date or date and time as an integer time.
//$ The time is always given as a standard time rather than as DST
//$ because DST can be ambiguous. The time may be omitted
//$ and will default to 00:00:00.
//$ ~The allowable range of calendar times is
//$ Jan 1 1970 00:00:00 to Jan 19 2038 03:14:07 UTC.
LISP EncodeTime(byte nargs, LISP args[])
{
  if((nargs != 6) && (nargs != 3))
    return LispError(ERR_NUM_ARGS);

  EXPECT_INT2(ARG1, ARG2);
  EXPECT_INT(ARG3);

  if(nargs == 3)
    return NewInteger(iEncodeTime(ivalue(ARG1), ivalue(ARG2), ivalue(ARG3)));

  EXPECT_INT(ARG4);
  EXPECT_INT2(ARG5, ARG6);
//  return NewInteger(iEncodeTime(ivalue(ARG1), ivalue(ARG2), ivalue(ARG3),
//                                ivalue(ARG4), ivalue(ARG5), ivalue(ARG6) ));

  time_t itime = iEncodeTime(ivalue(ARG1), ivalue(ARG2), ivalue(ARG3),
                             ivalue(ARG4), ivalue(ARG5), ivalue(ARG6));
  if(!utcflag)
    itime -= _timezone;

  return NewInteger(itime);
}

// Internal implementation of EncodeTime
time_t iEncodeTime(int y, int m, int d, int H, int M, int S)
{
  // Offset from 1st Jan of months
  static unsigned int month_day_one[] =
    {0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365};

  // Adjust date to zero-base each part
  y -= 1970; m--; d--;
  if( (y > 68) || (m > 11) || (d > 30) ||
      (y < 0)  || (m < 0)  || (d < 0)     )
  {
    LispError(ERR_BAD_ARG, NONE, "Invalid date" );
    return 0;
  }

  if( (H > 23) || (M > 59) || (S > 59) ||
      (H < 0)  || (M < 0)  || (S < 0)     )
  {
    LispError(ERR_BAD_ARG, NONE, "Invalid time" );
    return 0;
  }

  // Accumlate years and leap days
  unsigned int itime = y * 365 + (y + 1) / 4;
  // Add days of current year
  unsigned int days = month_day_one[m] + d;
  itime += days;
  //Adjust if after 29 Feb
  if(((y + 2) % 4) == 0)
  {
    if(m > 1)
      itime ++;
    if(days > 58)
      days--;
  }
  // Check month length
  if(days >= month_day_one[m + 1])
  {
    LispError(ERR_BAD_ARG, NONE, "Invalid day of month");
    return 0;
  }
  // Days to seconds
  itime *= (24 * 60 * 60);

  // Add time in seconds
  itime += H * 3600 + M * 60 + S;
  if(itime > 0x7fffffff)
  {
    LispError(ERR_BAD_ARG, NONE, "Date/Time outside valid range");
    return 0;
  }

//  if(!utcflag)
//    itime -= _timezone;
  return (time_t)itime;
}

//f (date-time [<itime>])
//$ Returns a list of (year month day hours minutes seconds wday
//$ yday dst) for the integer time or for the current time if
//$ the argument is omitted.  wday is the weekday (0-6, 0=>Sun),
//$ yday is the year day (0-355) and dst is a boolean which is
//$ T if daylight savings time is in effect.
//$ ~The allowable range of calendar times is
//$ Jan 1 1970 00:00:00 to Jan 19 2038 03:14:07 UTC.
//x > (date-time 333333333)
//x = (1980 7 25 1 35 33 5 206 t)
LISP DateTime(byte nargs, LISP args[])
{
  tm* dectime = iTimeArgs(nargs, args);
  protect(result, NIL);
  result = NewCons(Boolean(dectime->tm_isdst), NIL);
  result = NewCons(NewInteger(dectime->tm_yday), result);
  result = NewCons(NewInteger(dectime->tm_wday), result);
  result = NewCons(NewInteger(dectime->tm_sec), result);
  result = NewCons(NewInteger(dectime->tm_min), result);
  result = NewCons(NewInteger(dectime->tm_hour), result);
  result = NewCons(NewInteger(dectime->tm_mday), result);
  result = NewCons(NewInteger(dectime->tm_mon + 1), result);
  result = NewCons(NewInteger(dectime->tm_year + 1900), result);
  return result;
}

//f (time-string <fmt> [<itime>])
//$ Convert an integer time to a formatted string using the
//$ format string supplied.  If itime is omitted then the
//$ current time is used.  The format string is passed directly
//$ to the C-Library function strftime.  The output of this
//$ function is affected by the locale setting.
//$ ~Some of the more useful format strings are:
//$ ."%c"       Date & time
//$ ."%#c"      Long date & time
//$ ."%x"       Date
//$ ."%#x"      Long date
//$ ."%X"       Time
//$ ."%A"       Day of week
//$ ."%#c %Z"   Long date, time and timezone
//x > (time-string "%#c %Z" 333333333)
//x = "25 July 1980 01:35:33 BST"
LISP TimeString(byte nargs, LISP args[])
{
  time_t itime;

  CHECK_ARITY_IN(1, 2);
  EXPECT_STR(ARG1);

  // Get the time to be converted
  if(nargs == 2)
  {
    EXPECT_INT(ARG2);
    itime = (time_t)ivalue(ARG2);
  }
  else
  {
    itime = iNow();
  }

  tm* dectime = iDecodeTime(itime);

  unsigned int n = strlen(text(ARG1)) + 16;
  char* fmt = new char[n + 1];
  strncpy(fmt, text(ARG1), n);

  if(utcflag)
  {
    // Convert %z or %Z to UTC
    for(char* c = fmt; *c; c++)
    {
      if((c[0] == '%') && ((c[1] == 'z') || (c[1] == 'Z')))
      {
        if(strlen(fmt) == n)
          return LispError(ERR_PRT_BUF, NONE, "Format buffer overflow");
        for(char* d = c + strlen(c); d > (c + 2); d--)
          d[0] = d[-1];

        c[0] = 'U';
        c[1] = 'T';
        c[2] = 'C';
      }
    }
  }
  int len = strftime(txtbuf, BUFSZ, fmt, dectime);

  delete [] fmt;

  if(!len)
    return NIL;

  return NewString(txtbuf);
}

// Apply the local summertime correction as provided by Windows
bool iSummerTime(tm*& dectime, time_t itime)
{
  // Keep a copy of dectime
  static tm save;
  save = *dectime;

  TIME_ZONE_INFORMATION tzi;
  if(GetTimeZoneInformation(&tzi) == TIME_ZONE_ID_UNKNOWN)
    return false;

  // Get the changeover times for the year in question
  time_t start = iDecodeChangeOverTime(tzi.DaylightDate, save.tm_year + 1900);
  time_t end = iDecodeChangeOverTime(tzi.StandardDate, save.tm_year + 1900);

  if((itime >= start) && (itime < end))
  {
    // It's summertime
    itime -= tzi.DaylightBias * 60;
    dectime = gmtime(&itime);
    dectime->tm_isdst = 1;
  }
  else
  {
    // Not summertime, restore the original time
    *dectime = save;
  }
  return false;
}

// Determine the local summertime changeover times for the specified year
time_t iDecodeChangeOverTime(SYSTEMTIME& st, int year)
{
  static char mdays[12] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
  tm* dectime;
  time_t cotime;

  // If given an absolute date then use that with the year adjusted to
  // that required
  if(st.wYear != 0)
  {
    cotime = iEncodeTime(year, st.wMonth, st.wDay,
                         st.wHour, st.wMinute, st.wSecond);
  }
  else
  {
    // The date is encoded as the nth (or last) occurrence of a particular
    // weekday in a particular month.  Use this rule to derive an absolute time

    // Get the 1st of the changeover month
    cotime = iEncodeTime(year, st.wMonth, 1);
    dectime = gmtime(&cotime);

    // st.DayOfWeek is changeover day (0 = Sunday)
    // Get first changeover day of month
    int firstday = st.wDayOfWeek - dectime->tm_wday + 1;
    if(firstday < 1)
      firstday += 7;

    // st.wDay selects the correct changeover day of the month
    // where 1-4 -> 1st-4th and 5 -> last
    int day = firstday + 7 * (st.wDay - 1);

    // Keep the day in the correct month preserving day of week
    if(day > mdays[st.wMonth - 1])
      day -= 7;

    cotime = iEncodeTime(dectime->tm_year + 1900, dectime->tm_mon + 1, day,
                          st.wHour, st.wMinute, st.wSecond);
  }
  return cotime;
}

//m (timer <body>...)
//$ Evaluate body expressions like progn and return the elapsed time
//$ in milliseconds.
//x > (timer (sleep 1000))
//x = 1000
LISP Timer(LISP& args)
{
  int start = timeGetTime();
  Progn(args);
  int end = timeGetTime();
  return NewInteger(end - start);
}

//#############################################################################
// Stream functions
//#############################################################################

//f (open-file <filename> [<mode> [<text>]])
//$ Opens the named file with the specified mode.
//$ <mode> may be :read (readonly, the default),
//$ :write (read/write) or :create (create, read and write).
//$ ~If <text> is present and true then open as a text file.
//$ This function is the only way to create a File atom.
//x > (setq f (open-file "c:\\temp\\scratch" :create))
//x = #<File:0x0004d6c7>
LISP OpenFile(byte nargs, LISP args[])
{
  CHECK_ARITY_IN(1, 3);
  EXPECT_STR(ARG1);

  char mode[4];
  if((nargs == 1) || (ARG2 == READ))
    strcpy(mode, "r");
  else if(ARG2 == WRITE)
    strcpy(mode, "r+");
  else if(ARG2 == CREATE)
    strcpy(mode, "w+");
  else
    return LispError(ERR_INV_KEY, ARG2, "Invalid file mode");

  if((nargs <= 2) || (ARG3 == NIL))
    strcat(mode, "b");
  else
    strcat(mode, "t");

  FILE *f = fopen(text(ARG1), mode);

  if(!f)
    return NIL;

  return NewFile(f);
}

//f (close-file <file>)
//$ Close a file.  Returns a boolean which is T if the file was
//$ open and is now closed.
//x > (setq file (open-file "fred.txt"))
//X = #<File:0x0004df5e>
//x > (print file "Hello")
//x = "Hello"
//x > (close-file file)
//x = t
LISP CloseFile(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  if(!ARG1)                                 // Close one
    return NIL;                             // File wasn't open
  EXPECT_FILE(ARG1);
  int result = fclose(file(ARG1));
  return result ? NIL : T;
}

//f (file-position <file> [<integer> [<key>]])
//$ Get or set the position of a file pointer.  The first
//$ argument is always a file.  If this is the only argument
//$ then the file pointer is returned as an integer.
//$ ~If the file is followed by an integer then the file pointer
//$ is set accordingly.  An optional third argument is a key
//$ which may be :seek-relative, :seek-start or :seek-end which
//$ treats the integer value as an offset from the current
//$ location, the start (default) or the end of the file
//$ respectively.
LISP FilePosition(byte nargs, LISP args[])
{
  CHECK_ARITY_IN(1, 3);
  EXPECT_FILE(ARG1);
  if(nargs == 1)
    return NewInteger(ftell(file(ARG1)));
  EXPECT_INT(ARG2);

  int whence = SEEK_SET;
  if(nargs == 3)
  {
    if     (ARG3 == SK_START) whence = SEEK_SET;
    else if(ARG3 == SK_END)   whence = SEEK_END;
    else if(ARG3 == SK_REL)   whence = SEEK_CUR;
    else return LispError(ERR_BAD_ARG, ARG3, "Invalid seek mode");
  }

  return NewInteger(fseek(file(ARG1), ivalue(ARG2), whence));
}

//f (float-format [<string>])
//$ Read or set the format for floating output. The format string
//$ should be a C language format as used by printf.  This format
//$ is used by string, print and println.  The formats used by
//$ write and writeln are fixed to provide re-readable input with
//$ full accuracy. Remember that Lisp floats are C doubles and the
//$ format should be of type lg, le, lE or lf.
//$ print-format is a more flexible (and modeless) option.
//x > (float-format "%10.4lf")
//x = "%10.4lf"
//x > (println (sqrt 3))
//x       1.7321
//x = 1.732050807568877
LISP FloatFormat(byte nargs, LISP args[])
{
  if(nargs == 0)
    return NewString(ffmt);
  CHECK_ARITY_EQ(1);
  EXPECT_STR(ARG1);
  if(strlen(text(ARG1)) > 15)
    ERR_BAD_ARG;
  strcpy(ffmt, text(ARG1));
  return ARG1;
}

//f (integer-format [<string>])
//$ Read or set the format for integer output. The format string
//$ should be a C language format as used by printf.  This format
//$ is used by string, print and println.  The formats used by
//$ write and writeln are fixed to provide re-readable input with
//$ full accuracy. Remember that Lisp integers are C longs and the
//$ format should be of type ld, lu, lx, lX or lo.
//x > (integer-format "%#010X")
//x = "%#010X"
//x > (println 123)
//x 0X0000007B
//x = 123
LISP IntegerFormat(byte nargs, LISP args[])
{
  if(nargs == 0)
    return NewString(ifmt);
  CHECK_ARITY_EQ(1);
  EXPECT_STR(ARG1);
  if(strlen(text(ARG1)) > 15)
    ERR_BAD_ARG;
  strcpy(ifmt, text(ARG1));
  return ARG1;
}

// Function to handle all output
// Checks for a file at the car of args.  If it finds
// one it sets out to the file (rather than stdin.) It then
// prints the remainder of the arguments
LISP iPrint(LISP args, bool cflag, bool cr)
{
  FILE *out = stdout;

  if(args)
  {
    if(IsFile(car(args)))
      out = file(Pop(args));
  }

  LISP result = NIL;
  while(args)
  {
    result = Pop(args);
    print(result, out, cflag);
  }
  if(cr)
    fwritec(out, '\n');

  return result;
}

//f (write [<file>] [<arg> ...])
//$ Print each argument in a quoted form so that it may be read
//$ back in correctly.
//$ ~If the first argument is a file then the output is directed
//$ there instead of to the console.
LISP Write(LISP& args)
{
  return iPrint(args, false, false);
}

//f (print [<file>] [<arg> ...])
//$ Print each argument in an unquoted form.
//$ ~If the first argument is a file then the output is directed
//$ there instead of to the console.
LISP Print(LISP& args)
{
  return iPrint(args, true, false);
}

//f (writeln [<file>] [<arg> ...])
//$ Print each argument in a quoted form so that it may be read
//$ back in correctly. Then print a newline.
//$ ~If the first argument is a file then the output is directed
//$ there instead of to the console.
LISP Writeln(LISP& args)
{
  return iPrint(args, false, true);
}

//f (println [<file>] [<arg> ...])
//$ Print each argument in an unquoted form. Then print a
//$ newline. If the first argument is a file then the output is
//$ directed there instead of to the console.
LISP Println(LISP& args)
{
  return iPrint(args, true, true);
}

//f (print-format [<file>] <format> [<arg> ...])
//$ Print formatted output. If the first argument is a file then
//$ the output is directed there instead of to the console.
//$ The following format placeholders may be used.
//$ .%d       Decimal integer
//$ .%o       Octal integer
//$ .%x       Hexadecimal integer
//$ .%X       Hexadecimal integer
//$ .%f       Fixed point float
//$ .%e       Exponential notation float
//$ .%E       Exponential notation float
//$ .%g       General format float
//$ .%G       General format float
//$ .%c       Character
//$ .%s       String
//$ .%a       Any value printed with (print <arg>)
//$ .%w       Any value printed with (write <arg>)
//$ ~All formats up to and including %s may be extended as per the C
//$ printf format specifiers (e.g. %#04X).  %a and %s will act upon
//$ the field width and the left justify flag, '-' but all other
//$ format options are ignored. Note that the field width for %a and
//$ %w is the minimum field width and output will not be truncated.
//$ Use %s with (string <arg>) to truncate.
//$ ~The l flag normally required in C to indicate long/double arguments
//$ must not be used with print-format.  Long integers and doubles are
//$ the only numeric values that exist in AJBLisp so they do not require
//$ to be flagged as such.
//$ ~print-format always returns nil.
//x > (print-format "%04x*%-4a*%c\n" 10 t (char 66))
//x 000A*t   *B
//x = nil
LISP PrintFormat(LISP& args)
{
  FILE* out = stdout;
  if(IsFile(car(args)))
    out = file(Pop(args));

  EXPECT_STR(car(args));
  char* fmt = text(Pop(args));
  iFormat(out, fmt, args);
  return NIL;
}

// Print formatted arguments to a stream
void iFormat(FILE* f, char* fmt, LISP args)
{
  // char  buf[80];
  char  txt[20];

  bool  ljust;
  int   width;
  char  spec;

  int pad;

  char* pc = fmt - 1;
  char* oldpc = fmt;
  while((pc = strchr(pc + 1, '%')) != 0)
  {
    ljust = false;
    width = 0;

    if(!pc)
    {
      fwrites(f, oldpc);
      break;
    }

    *pc = '\0';
    fwrites(f, oldpc);
    *pc = '%';

    oldpc = pc++;

    // %% translates to %
    if(*pc == '%')
    {
      fwritec(f, '%');
      oldpc = pc++;
      continue;
    }

    LISP arg;
    if(!args)
      arg = LispError(ERR_NUM_ARGS, NONE, "Missing argument for format placeholder");
    else
      arg = Pop(args);

    // read flag characters
    bool flag = true;
    while(flag)
    {
      switch(*pc)
      {
        case '-': ljust = true;
        case '+':
        case '#':
        case ' ':
        case '0': pc++; break;
        default: flag = false;
      }
    }
    // read width
    if(isdigit(*pc))
    {
      width = 0;
      while(isdigit(*pc))
      {
        width *= 10;
        width += (*pc++ - '0');
      }
    }

    // skip .
    if(*pc == '.')
    {
      pc++;

      // skip precision
      while(isdigit(*pc)) pc++;
    }

    spec = *pc++;

    int len = pc - oldpc;
    if(len > 18)
    {
       LispError(ERR_PRT_BUF, NONE, "Format placeholder buffer overflow");
       return;
    }

    // Copy format inserting 'l' for a long/double argument
    memcpy(txt, oldpc, len);
    oldpc = pc;

    if((spec != 'c') && (spec != 's'))
    {
      txt[len - 1] = 'l';
      txt[len] = spec;
      txt[len + 1] = '\0';
    }
    else
    {
      txt[len] = '\0';
    }

    switch(spec)
    {
      // Standard printf integer conversions
      case 'd':
      case 'u':
      case 'o':
      case 'x':
      case 'X':
        if(!IsInteger(arg))
        {
          LispError(ERR_INT_EXP, arg);
          return;
        }
        fwritef(f, txt, ivalue(arg));
      break;

      // Standard printf floating conversions
      case 'f':
      case 'e':
      case 'E':
      case 'g':
      case 'G':
        if(!IsReal(arg))
        {
          LispError(ERR_REAL_EXP, arg);
          return;
        }
        fwritef(f, txt, rvalue(arg));
      break;

      // Call lisp print with any argument
      case 'a':
        if(width)
          pad = width - iPrintSize1(arg, 0, true);
        else
          pad = 0;

        if(!ljust && (pad > 0))
          Spaces(f, pad);

        print(arg, f, true);

        if(ljust && (pad > 0))
          Spaces(f, pad);
      break;

      // Call lisp write with any argument
      case 'w':
        if(width)
          pad = width - iPrintSize1(arg, 0, false);
        else
          pad = 0;

        if(!ljust && (pad > 0))
          Spaces(f, pad);

        print(arg, f, false);

        if(ljust && (pad > 0))
          Spaces(f, pad);
      break;

      // Standard printf string conversion
      case 's':
        if(!IsString(arg))
        {
          LispError(ERR_STR_EXP, arg);
          return;
        }
        fwritef(f, txt, text(arg));
      break;

      // Standard printf char conversion
      case 'c':
        if(!IsChar(arg))
        {
          LispError(ERR_CHR_EXP, arg);
          return;
        }
        fwritef(f, txt, chr(arg));
      break;

      default:
        LispError(ERR_BAD_ARG, NewChar(spec), "Bad format placeholder");
        fwrites(f, "?FMT?");
    }
  }
  if(*oldpc) fwrites(f, oldpc);
}

//f (read [<file>]) (read <path>)
//$ Read an expression from an open file. With no argument it reads from
//$ the console.  With one string argument it will open the file with the
//$ given path, read its content and then close it again.  One complete
//$ s-expression will be read or an error will occur.  It is not possible
//$ to read beyond the first s-expression when a file path is given.
//x > (read) (a b c)
//x = (a b c)
LISP Read(byte nargs, LISP args[])
{
  if(nargs == 0)
    return read(stdin);

  CHECK_ARITY_EQ(1);
  if(IsFile(ARG1))
    return read(file(ARG1));

  if(!IsString(ARG1))
    LispError(ERR_BAD_ARG, ARG1, "Bad argument to read");

  FILE* f = fopen(text(ARG1), "r");
  if(f == 0)
    LispError(ERR_NOT_FOUND, ARG1, "Invalid file path in read");
  LISP input;
  try
  {
    input = read(f);
  }
  __finally
  {
    fclose(f);
  }
  return input;
}

//f (readline [<file>])
//$ Read one line from the console or a file as a string.
//x > (readline) There's an echo
//x = "There's an echo"
LISP Readline(byte nargs, LISP args[])
{
  if(nargs == 0)
    return readline(stdin);
  CHECK_ARITY_EQ(1);
  if(IsFile(ARG1))
    return readline(file(ARG1));
  return LispError(ERR_FILE_EXP, ARG1);
}

//f (readchar [<file>])
//$ Read one character from the console or a file. Returns the
//$ character or :eof
LISP Readchar(byte nargs, LISP args[])
{
  int c;
  FILE *f = stdin;

  CHECK_ARITY_LE(1);

  if(nargs == 1)
  {
    EXPECT_FILE(ARG1);
    f = file(ARG1);
  }
  c = inch(f);
  if(c == EOF)
    return EOF_;
  return NewChar((char)c);
}

//f (writechar [<file>] <char>)
//$ Write one character to the console or a file. Returns the
//$ character written.
//x > (writechar #\g)
//x g= #\g
LISP Writechar(byte nargs, LISP args[])
{

  if(nargs == 1)
  {
    EXPECT_CHR(ARG1);
    writec(chr(ARG1));
    return(ARG1);
  }
  CHECK_ARITY_EQ(2);
  EXPECT_FILE(ARG1);
  EXPECT_CHR(ARG2);
  fwritec(file(ARG1), chr(ARG2));
  return(ARG1);
}

//f (readbyte <file>)
//$ Read an 8-bit word from a file returning an integer or :eof.
LISP Readbyte(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  EXPECT_FILE(ARG1);
  unsigned char c;
  int n = fread(&c, 1, 1, file(ARG1));

  if(n == 0) return EOF_;
  else       return NewInteger((unsigned int)c);
}

//f (writebyte <file> <integer>)
//$ Write an 8-bit word to a file returning a boolean which is
//$ t upon success.
LISP Writebyte(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(2);
  EXPECT_FILE(ARG1);
  EXPECT_INT(ARG2);
  unsigned char c = (unsigned char)(ivalue(ARG2) & 0xFF);
  int n = fwrite(&c, 1, 1, file(ARG1));
  return Boolean(n == 1);
}

//f (readword <file>)
//$ Read a 16-bit word from a file returning an integer or :eof.
LISP Readword(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  EXPECT_FILE(ARG1);
  unsigned short c;
  int n = fread(&c, 2, 1, file(ARG1));

  if(n == 0) return EOF_;
  else       return NewInteger((unsigned int)c);
}

//f (writeword <file> <integer>)
//$ Write a 16-bit word to a file returning a boolean which is
//$ t upon success.
LISP Writeword(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(2);
  EXPECT_FILE(ARG1);
  EXPECT_INT(ARG2);
  unsigned short c = (unsigned short)(ivalue(ARG2) & 0xFFFF);
  int n = fwrite(&c, 2, 1, file(ARG1));
  return Boolean(n == 1);
}

//f (readlong <file>)
//$ Read a 32-bit word from a file returning an integer or :eof.
LISP Readlong(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  EXPECT_FILE(ARG1);
  unsigned long c;
  int n = fread(&c, 4, 1, file(ARG1));

  if(n == 0) return EOF_;
  else       return NewInteger(c);
}

//f (writelong <file> <integer>)
//$ Write a 32-bit word to a file returning a boolean which is
//$ t upon success.
LISP Writelong(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(2);
  EXPECT_FILE(ARG1);
  EXPECT_INT(ARG2);
  unsigned long c = ivalue(ARG2);
  int n = fwrite(&c, 4, 1, file(ARG1));
  return Boolean(n == 1);
}

//f (unread [<file>] <byte-or-char>)
//$ Unread one character or byte from the console or a file.
//$ Returns nil.  <file> defaults to :stdin.  Unreading :eof
//$ has no effect. At most, only one item may be unread at any
//$ time. Unreading more than one item has an undefined
//$ effect (beware, it may be successful some of the time.)
LISP Unread(byte nargs, LISP args[])
{
  int c;
  FILE* f = stdin;
  LISP val;

  CHECK_ARITY_IN(1, 2);

  if(nargs == 1)
  {
    val = ARG1;
  }
  else
  {
    EXPECT_FILE(ARG1);
    f = file(ARG1);
    val = ARG2;
  }
  if(val == EOF_) return NIL;
  if(IsInteger(val))
    c = ivalue(val) & 0x0F;
  else if(IsChar(val))
    c = chr(val);
  else
    return LispError(ERR_BAD_ARG, val);
  uninch(c, f);
  return NIL;
}

//f (eof <file>)
//$ Return t if at the end of the file.
LISP Eof(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  EXPECT_FILE(ARG1);
  return Boolean(feof(file(ARG1)));
}

//f (flush [<file>])
//$ Flush the file output buffer. Flushes both console buffers
//$ if no argument provided.  Returns nil.
//x > (flush) This text would cause an error if not flushed
//x = nil
LISP Flush(byte nargs, LISP args[])
{
  CHECK_ARITY_LE(1);
  if(nargs == 0)
  {
    tflushout();
    tflushin();
  }
  else
  {
    EXPECT_FILE(ARG1);
    flush(file(ARG1));
  }
  return NIL;
}

//f (logfile [<string> [<createflag>]])
//$ Open/close the session log file.  The file is specified
//$ by the path string argument. The optional <createflag>
//$ argument forces the log-file to be flushed if non-nil. If
//$ absent or nil then any existing log-file will be appended
//$ to.  If no arguments are passed then the logfile will be
//$ closed if it was open.
//$ ~Whenever a log-file is opened or
//$ closed then a comment is inserted in the log so that
//$ multiple sessions can be kept distinct.  logfile returns
//$ a boolean which is t on success.
//$ ~Note that the log-file
//$ will fail to open if it is already open but it may be
//$ closed without error whether it is open or not.
LISP LogFile(byte nargs, LISP args[])
{
  time_t itime = time(NULL);
  tm *dectime = localtime(&itime);
  strftime(txtbuf, BUFSZ, "%#c %Z", dectime);

  if(nargs == 0)
  {
    if(!logfile) return NIL;
    fwritef(logfile, ";;;;;; SESSION LOG CLOSED - %s\n", txtbuf);
    int failed = fclose(logfile);
    logfile = 0;
    return failed ? NIL : T;
  }

  if(logfile) return NIL;
  CHECK_ARITY_IN(1, 2);
  EXPECT_STR(ARG1);
  char* mode = "a";
  if((nargs == 2) && ARG2)
    mode = "w";
  logfile = fopen(text(ARG1), mode);

  if(logfile)
    fwritef(logfile, ";;;;;; SESSION LOG OPENED - %s\n", txtbuf);
  return Boolean(logfile);
}

//f (load <file> [<verbose>])
//$ Load and evaluate expressions from a file.  If the verbose
//$ option is present and not nil then print the value of each
//$ expression read.
//$ ~The file name can be given as a string or a file atom can be
//$ used.  If a file name is given then the file is always closed
//$ after use. A boolean is returned which is t if the file opened
//$ correctly.  An open file, given as a file atom, will be read
//$ from the current file pointer on entry.  Use file-position to
//$ adjust this if necessary. Errors ocurring when evaluating the
//$ expressions in the file are handled in the normal way.
//$ ~If a named file cannot be found then the following variations
//$ will be tried in sequence and the first that resolves to a real
//$ file will be used.
//$ . <file>.lsp
//$ . %LISPPATH%<file>
//$ . %LISPPATH%<file>.lsp
//$ ~Note: No special consideration is taken to avoid invalid
//$ filenames since these will always fail-safe anyway.
//$ ~E.g. "%LISPPATH%C:\fred.lsp" might be tried but will just
//$ fail without incident
//x > (load "answer to life the universe and everything")
//x 42   ; Assumes file exists and can be located
//x = t  ; File opened OK
LISP Load(byte nargs, LISP args[])
{
  FILE* f;
  int winexcept;

  CHECK_ARITY_IN(1, 2);

  // Open the filename given in a string
  if(IsString(ARG1))
  {
    char* fname = text(ARG1);
    f = iOpenLispSource(fname);
  }
  // or just use an open file
  else if(IsFile(ARG1))
    f = file(ARG1);

  else
    return LispError(ERR_BAD_ARG, ARG1, "File or filename expected");

  // Return NIL if open fails
  if(!f)
    return NIL;

  // Check verbose flag
  bool verbose =((nargs == 2) && (value(ARG2)));

  // Read and evaluate expressions making sure that we close the file
  // whatever happens
  try    // __finally
  {
    try  // __except (Windows SEH)
    {
      while(!feof(f))
      {
        LISP result = iEval(read(f));

        if(verbose)
        {
          print(result, stdout, false);
          writes("\n");
        }
      }
    }
    __except(FilterException(winexcept = GetExceptionCode()))
    {
      OSException(winexcept);  // Throw Lisp Error
    }
  }
  __finally
  {
    // Ensure the file is closed
    // if we opened it
    if(!IsFile(ARG1)) fclose(f);
  }
  return T;
}

// Try to open a lisp source file
// First try the path supplied
// Then try appending .lsp and trying again
// Then prepend %LISPPATH% to the path supplied
// Finally try appending .lsp to this
// The first combination that opens is used, otherwise
// return an invalid handle
FILE* iOpenLispSource(char* fname)
{
  FILE* f;

  f = fopen(fname, "r");
  if(!f)
  {
    char* lpath = getenv("LISPPATH");
    int pathlen = lpath ? strlen(lpath) : 0;
    char* pathbuf = (char*)malloc(strlen(fname) + pathlen + 5);
    strcpy(pathbuf, fname);
    strcat(pathbuf, ".lsp");
    f = fopen(pathbuf, "r");
    if(!f && lpath)
    {
      strcpy(pathbuf, lpath);
      strcat(pathbuf, fname);
      f = fopen(pathbuf, "r");

      if(!f)
      {
        strcat(pathbuf, ".lsp");
        f = fopen(pathbuf, "r");
      }
    }
    free(pathbuf);
  }
  return f;
}

// Efficiently print n spaces to a stream
void Spaces(FILE* f, int n)
{
  const int SWIDTH = 20;
  static const char* s = "                    "; // SWIDTH spaces

  if(n <= 0)
    return;

  while((n -= SWIDTH) > 0)
    fwrites(f, (char *)s);
  fwrites(f, (char *)(s - n));
}

//f (write-size <any>...)
//$ Determine the printed width of the arguments as they would
//$ be rendered by (write <args>...)
//x > (write-size '(a #\b c))
//x = 9
LISP WriteSize(LISP& args)
{
  return NewInteger(iPrintSize(args, 0, false));
}

//f (print-size <any>...)
//$ Determine the printed width of the arguments as they would
//$ be rendered by (print <args>...)  Tabs are counted correctly
//$ for 8 character tab stops (same as the listener.)
//x > (print-size '(a #\b c))
//x = 7
LISP PrintSize(LISP& args)
{
  return NewInteger(iPrintSize(args, 0, true));
}

// Determine the width of the arguments when printed
// (cflag true) or written (cflag false)
int iPrintSize(LISP args, int sz, bool cflag)
{
  while(args)
  {
    LISP arg = car(args);
    args = cdr(args);
    sz = iPrintSize1(arg, sz, cflag);
  }
  return sz;
}

// Determine the width of the single argument when
// printed (cflag true) or written (cflag false)
// Note that the return value is the width plus the sz argument
// which should be set to the current output position.
// Knowing the absolute position in the output string allows
// tab characters to be rendered correctly at every 8th character
// position
int iPrintSize1(LISP arg, int sz, bool cflag)
{
  if(!arg) return 3;  // nil

  int width;
  switch (flags(arg))
  {
    case tCHR:     {
                     char c = chr(arg);

                     if(cflag)
                     {
                       if(c == '\t')
                         // Eight character tabs
                         sz = (sz & ~7) + 8;
                       else
                         // Other characters count as 1
                         sz++;
                       break;
                     }

                     if(c == '^')
                     {
                       sz += 4;    // #\^^
                       break;
                     }

                     if(!iscntrl(c) && (c != ' '))
                     {
                       sz += 3;    // #\A
                       break;
                     }
                     // Control characters are always output as ASCII values
                     // eg #\^A is actually output as #\^1
                     if(c < '\xA')
                       sz += 4;    // #\^1
                     else if (c < '\x64')
                       sz += 5;    // #\^11
                     else
                       sz += 6;    // #\^111
                   }
    break;

    case tSYM:
    case tCONST:
    case tKEY:
                   width =  size(arg) - SYM_SIZE;
                   sz += width;

                   if(!cflag)
                   {
                     if((*name(arg) == '#') || (*name(arg) == '\''))
                       sz++;
                     for(int i = width - 1; i >= 0; i--)
                       if(strchr("\"();", name(arg)[i]))
                         sz++;
                   }
    break;

    case tSTR:     width = strlen(text(arg));
                   sz += width;

                   if(!cflag)
                   {
                     sz += 2;   // Quotes
                     for(int i = width - 1; i >= 0; i--)
                       if(strchr("\n\r\t\"\\", text(arg)[i]))
                         sz++;
                   }
    break;

    case tINT:     if(cflag)
                     sz += sprintf(txtbuf, ifmt, ivalue(arg));
                   else
                     sz += sprintf(txtbuf, wifmt, ivalue(arg));
    break;

    case tREAL:    if(cflag)
                     sz += sprintf(txtbuf, ffmt, rvalue(arg));
                   else
                     sz += sprintf(txtbuf, wffmt, rvalue(arg));
    break;

    // These all have the printed form #<type:id>
    case tFSUBRL:  sz++;
    case tFSUBR:
    case tSUBRL:   sz++;
    case tSUBR:
    case tFILE:    sz += 16;
    break;

    case tCONS:    sz += 1;   // +2 for parens
                              // -1 because too many spaces counted
                   while(IsCons(arg))
                   {
                     sz = iPrintSize1(car(arg), sz, cflag) + 1;
                     arg = cdr(arg);
                   }
                   if(arg)
                     sz = iPrintSize1(arg, sz, cflag) + 3;  // +3 for ' . '
    break;

    case tVECT:    sz += 2 + dim(arg);
                   for(int i = 0; i < dim(arg); i++)
                     sz = iPrintSize1(data(arg)[i], sz, cflag);
  }
  return sz;
}

//#############################################################################
// Predicates
//#############################################################################

//f (eq <this> <that>)
//$ Compares two objects for identity (i.e. the SAME object)
//$ ~Beware! It normally only makes sense to compare symbols
//$ with eq as these are always unique (unless explicitly
//$ declared free.)
//$ .
//$ ~Note that characters and the integers -128..127 are
//$ also unique but this should not be relied upon.  The only
//$ valid use for eq with arguments other than symbols is to
//$ determine if two objects occupy the same memory.
//$ ~Use =, char= and string= to compare values.
//x > (setq symbol 'value)
//x = value
//x > (eq symbol 'value)
//x = t
//x > (eq 1.0 1.0)
//x = nil                  ; Use = for numbers
LISP Eq(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(2);
  return iEq(ARG1, ARG2);
}

// Internal form of Eq
LISP iEq(LISP a, LISP b)
{
  return Boolean(a == b);
}


//f (eql <this> <that>)
//$ Compares two lists or vectors for identity or any other
//$ objects for equality of type and value.
//x > (eql (concat "Hi " "World!") "Hi World!")
//x = t
//x > (eql  '(a b c) '(a b c))
//x = nil                  ; Equal but not identical
//x > (eql 1 1)
//x = t                    ; Equal integers
//x > (eql 1 1.0)
//x = nil                  ; Different types
LISP Eql(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(2);
  return iEql(ARG1, ARG2);
}

// Internal form of Eql
LISP iEql(LISP a, LISP b)
{
  if(a == b)
    return T;
  if(!a || !b)
    return NIL;
  if(a->flags != b->flags)
    return NIL;
  if(IsInteger(a))
    return Boolean(ivalue(a) == ivalue(b));
  if(IsReal(a))
    return Boolean(rvalue(a) == rvalue(b));
  if(IsChar(a))
    return Boolean(chr(a) == chr(b));
  if(IsString(a))
    return Boolean(!strcmp(text(a), text(b)));
  return NIL;
}

//f (equal <this> <that>)
//$ Compares any two objects for equality of type and value.
//$ The equality of lists and vectors is determined element by
//$ element.
//x > (equal  '(a b c) '(a b c))
//x = t                    ; Equal (but not eq) lists
//x > (equal 1 1)
//x = t                    ; Equal integers
//x > (equal 1 1.0)
//x = nil                  ; Different types
LISP Equal(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(2);
  return iEqual(ARG1, ARG2);
}

// Internal form of Equal
LISP iEqual(LISP a, LISP b)
{
  // Loop to remove tail-end recursion for lists
  while(true)
  {
    if(iEql(a, b))
      return T;

    if(IsAtom(a) || IsAtom(b))
      return NIL;

    if(IsVector(a) || IsVector(b))
    {
      if(!(IsVector(a) && IsVector(b)))
        return NIL;
      if(dim(a) != dim(b))
        return NIL;
      // Compare all elements of vectors
      for(int i = 0; i < dim(a); i++)
      {
        if(!iEqual(data(a)[i], data(b)[i]))
          return NIL;
      }
      // Equal vectors
      return T;
    }
    // Only lists remain
    if(!iEqual(Pop(a), Pop(b))) return NIL;
  }
}

//f (zerop <any>) (zerop <num> <fuzz>)
//$ Predicate; true if the single argument is precisely 0 or 0.0.
//$ ~With two numeric arguments it checks that <num> is zero with
//$ a tolerance of +/-<fuzz>.
//x > (zerop (- :pi (/ 22.0 7.0)) 0.05)
//x = t         ; pi is close to 22/7
LISP Zerop(byte nargs, LISP args[])
{
  CHECK_ARITY_LE(2);
  if(nargs == 2)
  {
    EXPECT_NUM2(ARG1, ARG2);
    double fuzz = IsInteger(ARG2) ? (double) ivalue(ARG2) : rvalue(ARG2);
    double x = IsInteger(ARG1) ? (double) ivalue(ARG1) : rvalue(ARG1);
    if(fabs(x) < fuzz) return T;
    return NIL;
  }
  if(IsInteger(ARG1) && (ivalue(ARG1) == 0))
    return T;
  if(IsReal(ARG1) && (rvalue(ARG1) == 0.0) && !(_isnan(rvalue(ARG1))))
    return T;
  return NIL;
}

//f (onep <any>) (onep <num> <fuzz>)
//$ Predicate; true if the single argument is precisely 1 or 1.0.
//$ ~With two numeric arguments it checks that <num> is one with
//$ a tolerance of +/-<fuzz>.
//x > (onep 'fred)
//x = nil          ; 'fred can never be 1
LISP Onep(byte nargs, LISP args[])
{
  CHECK_ARITY_LE(2);
  if(nargs == 2)
  {
    EXPECT_NUM2(ARG1, ARG2);
    double fuzz = IsInteger(ARG2) ? (double) ivalue(ARG2) : rvalue(ARG2);
    double x = IsInteger(ARG1) ? (double) ivalue(ARG1) : rvalue(ARG1);
    if(fabs(1.0 - x) < fuzz) return T;
    return NIL;
  }
  if(IsInteger(ARG1) && (ivalue(ARG1) == 1))
    return T;
  if(IsReal(ARG1) && (rvalue(ARG1) == 1.0))
    return T;
  return NIL;
}

//f (plusp <any>)
//$ Predicate; true if the argument is numeric and positive.
//x > (plusp bank_balance)
//x = nil     ; oops!
LISP Plusp(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  if(IsInteger(ARG1) && (ivalue(ARG1) >= 0))
    return T;
  if(IsReal(ARG1) && (rvalue(ARG1) >= 0.0))
    return T;
  return NIL;
}

//f (minusp <any>)
//$ Predicate; true if the argument is numeric and negative.
//x > (minusp k)
//x = t       ; k is a negative number
LISP Minusp(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  if(IsInteger(ARG1) && (ivalue(ARG1) < 0))
    return T;
  if(IsReal(ARG1) && (rvalue(ARG1) < 0.0))
    return T;
  return NIL;
}

//f (atom <arg>)
//$ Predicate; true if the argument is an atom
//$ i.e. not a list or a vector.  Note that strings are atoms.
//$ nil is an atom and a list.
//x (atom t)
//x = t
//x > (atom '(a b c))
//x = nil
LISP Atom(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  return Boolean(IsAtom(ARG1) && !IsVector(ARG1));
}

//f (symbolp <arg>)
//$ Predicate; true if the argument is a variable, a constant
//$ symbol or a key.
//x > (symbolp 'a)
//x = t
LISP Symbolp(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  return Boolean(IsSymbol(ARG1));
}

//f (variablep <arg>)
//$ Predicate; true if the argument is a variable symbol.
//x > (variablep ':pi)
//x = nil
LISP Variablep(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  return Boolean(IsVariable(ARG1));
}

//f (constantp <arg>)
//$ Predicate; true if the argument is a constant symbol
//$ or a key.
//x > (constantp ':pi)
//x = t
LISP Constantp(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  return Boolean(IsConstant(ARG1));
}

//f (keyp <arg>)
//$ Predicate; true if the argument is a key.
//x > (keyp ':pi)
//x = nil         ; :pi is not a key
LISP Keyp(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  return Boolean(IsKey(ARG1));
}

//f (numberp <arg>)
//$ Predicate; true if the argument is a number.
//x > (numberp :pi)
//x = t
LISP Numberp(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  return Boolean(IsNumber(ARG1));
}

//f (integerp <arg>)
//$ Predicate; true if the argument is an integer.
//x > (integerp :pi)
//x = nil
LISP Integerp(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  return Boolean(IsInteger(ARG1));
}

//f (floatp <arg>)
//$ Predicate; true if the argument is a float.
//x > (floatp :pi)
//x = t
LISP Floatp(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  return Boolean(IsReal(ARG1));
}

//f (listp <arg>)
//$ Predicate; true if the argument is a list
//$ nil is treated as a list. Use consp to exclude the nil case.
//x > (listp (oblist))
//x = t
LISP Listp(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  return Boolean(!ARG1 || IsCons(ARG1));
}

//f (consp <arg>)
//$ Predicate; true if the argument is a cons node.
//$ All lists, excluding nil, are cons nodes.
//x > (consp '(nil))
//x = t
//x > (consp nil)
//x = nil
LISP Consp(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  return Boolean(IsCons(ARG1));
}

//f (charp <arg>)
//$ Predicate; true if the argument is a character.
//x > (charp 'a)
//x = nil
//x > (charp #\a)
//x = t
LISP Charp(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  return Boolean(IsChar(ARG1));
}

//f (stringp <arg>)
//$ Predicate; true if the argument is a string.
//x > (stringp "a")
//x = t
//x > (stringp #\a)
//x = nil
LISP Stringp(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  return Boolean(IsString(ARG1));
}

//f (execp <arg>)
//$ Predicate; true if the argument is a primitive operation.
//$ I.e. a built-in function or macro.
//x > (execp execp)
//x = t
//x > (execp 'execp)
//x = nil
LISP Execp(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  return Boolean(IsSubr(ARG1)     ||
                 IsSubrl(ARG1)    ||
                 IsFSubr(ARG1)    ||
                 IsFSubrl(ARG1));
}

//f (subrp <arg>)
//$ Predicate; true if the argument is a subr.
//$ I.e. a primitive function that always evaluates its
//$ arguments.
LISP Subrp(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  return Boolean(IsSubr(ARG1) || IsSubrl(ARG1));
}

//f (fsubrp <arg>)
//$ Predicate; true if the argument is an fsubr.
//$ I.e. a primitive function that does not automatically
//$ evaluate its arguments.
LISP FSubrp(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  return Boolean(IsFSubr(ARG1) || IsFSubrl(ARG1));
}

//f (vectorp <arg>)
//$ Predicate; true if the argument is a vector.
LISP Vectorp(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  return Boolean(IsVector(ARG1));
}

//f (filep <arg>)
//$ Predicate; true if the argument is a file.
LISP Filep(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  return Boolean(IsFile(ARG1));
}

//f (windowp <arg>)
//$ Predicate; true if the argument is a graphics window.
LISP Windowp(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  return Boolean(IsWindow(ARG1));
}

//f (freep <arg>)
//$ Predicate; true if the argument is a free (uninterned) symbol
//x > (setq x 'a y (free 'a))
//x = a
//x > (freep x)
//x = nil
//x > (freep y)
//x = t
LISP Freep(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  // Free symbols are not hashed (by definition)
  return Boolean(IsSymbol(ARG1) && !hash(ARG1));
}

//f (typep <any> <type>)
//$ Predicate; true if the first arg has the type
//$ specified by the second.  If the second arg is not
//$ a valid type key then the result is always nil.
//$ ~Valid type keys are:
//$ .nil
//$ .:cons       :symbol
//$ .:constant   :key
//$ .:character  :string
//$ .:integer    :float
//$ .:vector
//$ .:subr       :subrl
//$ .:fsubr      :fsubrl
//$ .:file       :window
LISP Typep(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(2);
  
  if(!ARG1) return Boolean(ARG2 == NIL);

  switch(flags(ARG1))
  {
    case   tCONS: return Boolean(ARG2 == lCONS);
    case    tSYM: return Boolean(ARG2 == lSYMBOL);
    case  tCONST: return Boolean(ARG2 == lCONSTANT);
    case    tKEY: return Boolean(ARG2 == lKEY);
    case    tCHR: return Boolean(ARG2 == lCHARACTER);
    case    tSTR: return Boolean(ARG2 == lSTRING);
    case    tINT: return Boolean(ARG2 == lINTEGER);
    case   tREAL: return Boolean(ARG2 == lFLOAT);
    case   tVECT: return Boolean(ARG2 == lVECTOR);
    case   tSUBR: return Boolean(ARG2 == lSUBR);
    case  tSUBRL: return Boolean(ARG2 == lSUBRL);
    case  tFSUBR: return Boolean(ARG2 == lFSUBR);
    case tFSUBRL: return Boolean(ARG2 == lFSUBRL);
    case   tFILE: return Boolean(ARG2 == lFILE);
    case   tWIND: return Boolean(ARG2 == lWINDOW);

    default: return LispError(ERR_BAD_TYP, NewInteger(flags(ARG1)));
  }
}

//f (lexcmp <x> <y>)
//$ Compare <x> and <y> in a consistent way. Types are ordered in an
//$ arbitrary way and within each type the values are ordered
//$ arbitrarily (although, if a meaningful order exists then it is used.)
//$ The result is -1, 0 or 1 for <x> < <y>, <x> = <y> or <x> > <y>.
//$ This function is used to impose a canonical order on lists and vectors
//$ so that they may be more easily compared and analysed.
LISP LexCmp(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(2);
  return NewInteger(iLexCmp(ARG1, ARG2));
}

int iLexCmp(LISP arg1, LISP arg2)
{
  if(!arg1 || !arg2)
  {
    if(arg1)
      return 1;
    if(arg2)
      return -1;
    return 0;
  }
  if(flags(arg1) != flags(arg2))
  {
    if(flags(arg1) < flags(arg2))
      return -1;
    return 1;
  }

  int c, l;
  double d;

  switch(flags(arg1))
  {
    case tCONS:
      c = iLexCmp(car(arg1), car(arg2));
      if(c)
        return c;
      else
        return iLexCmp(cdr(arg1), cdr(arg2));

    case tSYM:
    case tCONST:
    case tKEY:
      return strcmp(name(arg1), name(arg2));

    case tCHR:
      c = chr(arg1) - chr(arg2);
      if(c > 0) return 1;
      if(c < 0) return -1;
      return 0;

    case tSTR:
      return strcmp(text(arg1), text(arg2));

    case tINT:
      c = ivalue(arg1) - ivalue(arg2);
      if(c > 0) return 1;
      if(c < 0) return -1;
      return 0;

    case tREAL:
      d = rvalue(arg1) - rvalue(arg2);
      if(d > 0.0) return 1;
      if(d < 0.0) return -1;
      return 0;

    case tVECT:
      l = dim(arg1);
      if(dim(arg2) < l)
        l = dim(arg2);
      for(int i = 0; i < l; i++)
      {
        c = iLexCmp(data(arg1)[i], data(arg2)[i]);
        if(c)
          return c;
      }
      c = dim(arg1) - dim(arg2);
            if(c > 0) return 1;
      if(c < 0) return -1;
      return 0;

    case tSUBR:
    case tSUBRL:
    case tFSUBR:
    case tFSUBRL:
      c = code(arg1) - code(arg2);
      if(c > 0) return 1;
      if(c < 0) return -1;
      return 0;

    case tFILE:
      c = file(arg1) - file(arg2);
      if(c > 0) return 1;
      if(c < 0) return -1;
      return 0;

    case tWIND:
      c = window(arg1) - window(arg2);
      if(c > 0) return 1;
      if(c < 0) return -1;
      return 0;
  }
  // Can't get here
  return 0;
}

//#############################################################################
// Arithmetic operators
//#############################################################################
//f (+ [<num> ...])
//$ Calculate the sum of the numeric arguments.
//$ ~If any of the arguments are real then the result will be
//$ real otherwise the result is integer.
//x > (+)                  ; Degenerate case
//x = 0
//x > (+ 2 2 2)
//x = 6

// Function Add defined in funcasm.cpp

//f (- <num> [<num>])
//$ Calculate the difference of two numeric arguments or the
//$ negative of a single numeric argument.
//$ ~If either of the arguments are real then the result will be
//$ real otherwise the result is integer.
//x > (- 1)
//x = -1
//x > (- 10 6)
//x = 4

// Function Subtract defined in funcasm.cpp

//f (* [<num> ...])
//$ Calculate the product of the numeric arguments.
//$ ~If any of the arguments are real then the result will be
//$ real otherwise the result is integer.
//x > (*)                  ; Degenerate case
//x = 1
//x > (* 2 2 2)
//x = 8

// Function Multiply defined in funcasm.cpp

//f (/ <num> [<num>])
//$ Calculate the quotient of two numeric arguments.
//$ If either of the arguments are real then the result will be
//$ real otherwise the result is integer and integer division is
//$ performed.  By analogy with unary -, unary / is defined to
//$ be the reciprocal function which always returns a real result.
//x > (/ 5.0)
//x = 0.2
//x > (/ 10 3)
//x = 3
//x > (/ 10 3.0)
//x = 3.3333333
LISP Divide(byte nargs, LISP args[])
{
  if(nargs == 1)                            // Reciprocal
  {
    EXPECT_NUM(ARG1);
    if(IsInteger(ARG1))
    {
      if(ivalue(ARG1) == 0)
        return LispError(ERR_M_DIVZ, ARG1);
      return NewReal(1.0 / ivalue(ARG1));
    }
    else
    {
      if(rvalue(ARG1) == 0.0)
        return LispError(ERR_M_DIVZ, ARG1);
      return NewReal(1.0 / rvalue(ARG1));
    }
  }
  CHECK_ARITY_EQ(2);
  EXPECT_NUM2(ARG1, ARG2);
  if(IsReal(ARG2))
  {
    if(rvalue(ARG2) == 0.0)
      return LispError(ERR_M_DIVZ, ARG2);

    if(IsReal(ARG1))
      return NewReal(rvalue(ARG1) / rvalue(ARG2));
    else
      return NewReal(ivalue(ARG1) / rvalue(ARG2));
  }
  else
  {
    if(ivalue(ARG2) == 0)
      return LispError(ERR_M_DIVZ, ARG2);

    if(IsReal(ARG1))
      return NewReal(rvalue(ARG1) / ivalue(ARG2));
    else
      return NewInteger(ivalue(ARG1) / ivalue(ARG2));
  }
}


//f (rem <integer> <integer>)
//$ Calculate the remainder after division of two integer
//$ arguments.
//x > (rem 10 3)
//x = 1
LISP Rem(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(2);
  EXPECT_INT2(ARG1, ARG2);
  if(ivalue(ARG2) == 0)
    return LispError(ERR_M_DIVZ, ARG2);

  return NewInteger(ivalue(ARG1) % ivalue(ARG2));
}

//f (inc <symbol> [<integer>])
//$ Increment the integer value of <symbol> by <integer>
//$ or 1 (default) and return the result.
//x > (setq a 3)
//x = 3
//x > (inc a)
//x = 4
//x > a
//x = 4

// Function Inc defined in funcasm.cpp

//f (dec <symbol> [<integer>])
//$ Decrement the integer value of <symbol> by <integer>
//$ or 1 (default) and return the result.
//x > (setq a 3)
//x = 3
//x > (dec a)
//x = 2
//x > a
//x = 2

// Function Dec defined in funcasm.cpp

//#############################################################################
// Arithmetic predicates
//#############################################################################

//f (> <num> <num>)
//$ Predicate: True if the first argument is numerically
//$ greater than the second.
LISP Greaterp(byte nargs, LISP args[])
{
  LISP err;
  LISP result = Boolean(iCompare(nargs, args, err) > 0.0);
  return (err == NONE) ? result : err;
}

//f (< <num> <num>)
//$ Predicate: True if the first argument is numerically
//$ less than the second.
LISP Lessp(byte nargs, LISP args[])
{
  LISP err;
  LISP result = Boolean(iCompare(nargs, args, err) < 0.0);
  return (err == NONE) ? result : err;
}

//f (<= <num> <num>)
//$ Predicate: True if the first argument is numerically
//$ less than or equal to the second.
LISP NGreaterp(byte nargs, LISP args[])
{
  LISP err;
  LISP result = Boolean(iCompare(nargs, args, err) <= 0.0);
  return (err == NONE) ? result : err;
}

//f (>= <num> <num>)
//$ Predicate: True if the first argument is numerically
//$ greater than or equal to the second.
LISP NLessp(byte nargs, LISP args[])
{
  LISP err;
  LISP result = Boolean(iCompare(nargs, args, err) >= 0.0);
  return (err == NONE) ? result : err;
}

//f (= <num> <num> [<fuzz>])
//$ Predicate: True if the first argument is numerically
//$ equal to the second. The optional <fuzz> argument is the
//$ largest difference allowed between arguments considered
//$ equal and must be a float. <fuzz> defaults to 0.0.
LISP Equalp(byte nargs, LISP args[])
{
  double fuzz = 0.0;
  if(nargs == 3)
  {
    EXPECT_REAL(ARG3);
    fuzz = rvalue(ARG3);
    nargs = 2;
  }
  LISP err;
  LISP result = Boolean(fabs(iCompare(nargs, args, err)) <= fuzz);
  return (err == NONE) ? result : err;
}

//f (<> <num> <num> [<fuzz>])
//$ Predicate: True if the first argument is numerically
//$ unequal to the second. The optional <fuzz> argument is the
//$ largest difference allowed between arguments considered
//$ equal and must be a float. <fuzz> defaults to 0.0.
LISP NEqualp(byte nargs, LISP args[])
{
  double fuzz = 0.0;
  if(nargs == 3)
  {
    EXPECT_REAL(ARG3);
    fuzz = rvalue(ARG3);
    nargs = 2;
  }
  LISP err;
  LISP result = Boolean(fabs(iCompare(nargs, args, err)) > fuzz);
  return (err == NONE) ? result : err;
}

// Help function for comparisons
double iCompare(byte nargs, LISP args[], LISP& err)
{
  err = NONE;
  
  if(nargs != 2)
  {
    err = LispError(ERR_NUM_ARGS);
    return 0.0;
  }

  if(!IsNumber(ARG1))
  {
    err = LispError(ERR_NUM_EXP, ARG1);
    return 0.0;
  }

  if(!IsNumber(ARG2))
  {
    err = LispError(ERR_NUM_EXP, ARG2);
    return 0.0;
  }


  if(IsInteger(ARG1))
  {
    if(IsInteger(ARG2))
      return (double)(ivalue(ARG1) - ivalue(ARG2));
    else
      return (double)ivalue(ARG1) - rvalue(ARG2);
  }
  else
  {
    if(IsInteger(ARG2))
      return rvalue(ARG1) - (double)ivalue(ARG2);
    else
      return rvalue(ARG1) - rvalue(ARG2);
  }
}


//#############################################################################
// Mathematical functions
//#############################################################################

// Fetch a single numerical arg as a double
double MathsArg(byte nargs, LISP args[])
{
  if(nargs != 1)
  {
    LispError(ERR_NUM_ARGS);
    return 0.0;
  }
  if(!IsNumber(ARG1))
  {
    LispError(ERR_NUM_EXP, ARG1);
    return 0.0;
  }
  return IsInteger(ARG1) ? (double)ivalue(ARG1)
                         : rvalue(ARG1);
}

//f (abs <num>)
//$ Returns the absolute value of a number.  The result retains
//$ the type of the argument.
//x > (abs -1)
//x = 1
//x > (abs 1.0)
//x = 1.000000000000000
LISP Abs(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  EXPECT_NUM(ARG1);
  return(IsInteger(ARG1)) ? NewInteger(abs(ivalue(ARG1)))
                          : NewReal(fabs(rvalue(ARG1)));
}

//f (sign <num>)
//$ Returns the sign of a number as +/- 1 or 0
//x > (sign 37)
//x = 1
//x > (sign -1.5)
//x = -1
//x > (sign 0.0)
//x = 0
LISP Sign(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  EXPECT_NUM(ARG1);
  if(IsInteger(ARG1))
    return (ivalue(ARG1) < 0) ?
              NewInteger(-1) :
              ((ivalue(ARG1) > 0) ? NewInteger(1) : NewInteger(0));

  return (rvalue(ARG1) < 0.0) ?
            NewInteger(-1) :
            ((rvalue(ARG1) > 0.0) ? NewInteger(1) : NewInteger(0));
}
//f (float <num>)
//$ Convert a string, an integer or a float to a float.
//x > (float "1e3")
//x = 1000.000000000000
LISP Float(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  if(IsString(ARG1))
  {
    double r;
    char *end;                              // The first character not converted

    r = strtod(text(ARG1), &end);

    if(*end == '\0')
      return(NewReal(r));
    else
      return LispError(ERR_BAD_REAL, ARG1);

  }
  return NewReal(MathsArg(nargs, args));
}

//f (integer <x>)
//$ Convert a string, character, integer or a float to an integer.
//$ Float arguments are rounded to the nearest integer.
//$ String arguments must represent a valid integer (no decimal
//$ points) and may be Octal (if a leading zero is supplied,
//$ beware) or Hexadecimal (if prefixed by 0x.)
//x > (integer 3.6)
//x = 4
//x > (integer "0xff")
//x = 255
LISP Integer(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  if(IsString(ARG1))
  {
    int i;
    char *end;                              // The first character not converted
    bool neg = false;
    char* b = text(ARG1);
    while(*b == '-')
    {
      neg = !neg;
      b++;
    }
    while(isspace(*b)) b++;
    if(!*b)
      return LispError(ERR_BAD_INT, ARG1);
    i = strtoul(b, &end, 0);
    if(!*end)
      return(NewInteger(neg ? -i : i));
    else
      return LispError(ERR_BAD_INT, ARG1);
  }
  else if(IsChar(ARG1))
    return NewInteger((unsigned char)chr(ARG1));

  double r = MathsArg(nargs, args);

  return NewInteger((int)(r +((r < 0) ? -0.5 : 0.5)));
}

//f (floor <num>)
//$ Convert a number to the next more negative or equal integer.
LISP Floor(byte nargs, LISP args[])
{
  double arg = MathsArg(nargs, args);

  return NewInteger((int)floor(arg));
}

//f (ceil <num>)
//$ Convert a number to the next more positive or equal integer.
LISP Ceil(byte nargs, LISP args[])
{
  double arg = MathsArg(nargs, args);

  return NewInteger((int)ceil(arg));
}

//f (sqrt <num>)
//$ Returns the square root of its numeric argument as a float.
LISP Sqrt(byte nargs, LISP args[])
{
  double arg = MathsArg(nargs, args);

  if(arg < 0.0)
    return LispError(ERR_M_DOM, ARG1, "Negative argument to sqrt");
  return NewReal(sqrt(arg));
}

//f (sin <num>)
//$ Returns the sine of its numeric argument as a float.
LISP Sin(byte nargs, LISP args[])
{
  double arg = MathsArg(nargs, args);

  if(fabs(arg) > 1e8)
    return LispError(ERR_M_DOM, ARG1, "Domain error in sin");
  return NewReal(sin(arg));
}

//f (cos <num>)
//$ Returns the cosine of its numeric argument as a float.
LISP Cos(byte nargs, LISP args[])
{
  double arg = MathsArg(nargs, args);

  if(fabs(arg) > 1e8)
    return LispError(ERR_M_DOM, ARG1, "Domain error in cos");
  return NewReal(cos(arg));
}

//f (tan <num>)
//$ Returns the tangent of its numeric argument as a float.
LISP Tan(byte nargs, LISP args[])
{
  double arg = MathsArg(nargs, args);

  if(fabs(arg) > 1e8)
    return LispError(ERR_M_DOM, ARG1, "Domain error in tan");
  return NewReal(tan(arg));
}

//f (sinh <num>)
//$ Returns the hyperbolic sine of its numeric argument as a
//$ float.
LISP Sinh(byte nargs, LISP args[])
{
  double arg = MathsArg(nargs, args);

  return NewReal(sinh(arg));
}

//f (cosh <num>)
//$ Returns the hyperbolic cosine of its numeric argument as a
//$ float.
LISP Cosh(byte nargs, LISP args[])
{
  double arg = MathsArg(nargs, args);

  return NewReal(cosh(arg));
}

//f (tanh <num>)
//$ Returns the hyperbolic tangent of its numeric argument as a
//$ float.
LISP Tanh(byte nargs, LISP args[])
{
  double arg = MathsArg(nargs, args);

  return NewReal(tanh(arg));
}

//f (asin <num>)
//$ Returns the arc sine of its numeric argument as a float.
LISP Asin(byte nargs, LISP args[])
{
  double arg = MathsArg(nargs, args);

  if(fabs(arg) > 1.0)
    return LispError(ERR_M_DOM, ARG1, "Domain error in asin");
  return NewReal(asin(arg));
}

//f (acos <num>)
//$ Returns the arc cosine of its numeric argument as a float.
LISP Acos(byte nargs, LISP args[])
{
  double arg = MathsArg(nargs, args);

  if(fabs(arg) > 1.0)
    return LispError(ERR_M_DOM, ARG1, "Domain error in acos");
  return NewReal(acos(arg));
}

//f (atan <num> [<num>])
//$ Returns the arc-tangent of its numeric argument as a float.
//$ If two arguments are given then it calculates
//$ atan(ARG1 / ARG2) with consistent accuracy at high values
//$ of the ratio.
LISP Atan(byte nargs, LISP args[])
{
  if(nargs == 1)
  {
    EXPECT_NUM(ARG1);
    if(IsInteger(ARG1))
      return NewReal(atan(ivalue(ARG1)));
    else
      return NewReal(atan(rvalue(ARG1)));
  }

  CHECK_ARITY_EQ(2);
  EXPECT_NUM2(ARG1, ARG2);
  double x =
    (IsInteger(ARG1)) ? (double) ivalue(ARG1) : rvalue(ARG1);
  double y =
    (IsInteger(ARG2)) ? (double) ivalue(ARG2) : rvalue(ARG2);

  return NewReal(atan2(y, x));
}

//f (asinh <num>)
//$ Returns the arc hyperbolic sine of its numeric argument as a
//$ float.
LISP Asinh(byte nargs, LISP args[])
{
  double arg = MathsArg(nargs, args);

  return NewReal(log(arg + sqrt(arg * arg + 1.0)));
}

//f (acosh <num>)
//$ Returns the arc hyperbolic cosine of its numeric argument as
//$ a float.
LISP Acosh(byte nargs, LISP args[])
{
  double arg = MathsArg(nargs, args);

  if(arg < 1.0)
    return LispError(ERR_M_DOM, ARG1, "Domain error in acosh");
  return NewReal(log(arg + sqrt(arg - 1.0) * sqrt(arg + 1.0)));
}

//f (atanh <num>)
//$ Returns the arc hyperbolic tangent of its numeric argument
//$ as a float.
LISP Atanh(byte nargs, LISP args[])
{
  double arg = MathsArg(nargs, args);

  if((arg < -1.0) || (arg > 1.0))
    return LispError(ERR_M_DOM, ARG1, "Domain error in atanh");
  return NewReal(0.5 * (log(1.0 + arg) - log(1.0 - arg)));
}

//f (log <num> [<base>])
//$ Returns the logarithm to base <base> of <num> as
//$ a float. <base> defaults to 10. Use ln for natural log.
LISP Log(byte nargs, LISP args[])
{
  CHECK_ARITY_LE(2);
  double arg = MathsArg(1, args);

  double base = nargs == 2 ? MathsArg(1, args + 1) : 10.0;

  if((arg <= 0.0) || (base <= 0.0))
    return LispError(ERR_M_DOM, ARG1, "Domain error in log");
  return NewReal(log(arg)/log(base));
}

//f (ln <num>)
//$ Returns the natural logarithm of its numeric argument as a
//$ float.
LISP Ln(byte nargs, LISP args[])
{
  double arg = MathsArg(nargs, args);

  if(arg <= 0.0)
    return LispError(ERR_M_DOM, ARG1, "Domain error in ln");
  return NewReal(log(arg));
}

//f (exp <num>)
//$ Returns the natural anti-logarithm of its numeric argument as a
//$ float.
LISP Exp(byte nargs, LISP args[])
{
  double arg = MathsArg(nargs, args);

  if(arg > 705.0)
    return LispError(ERR_M_DOM, ARG1, "Domain error in exp");
  return NewReal(exp(arg));
}

//f (power <num> <num>)
//$ Raises its first numeric argument to the power of the
//$ second.
LISP Power(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(2);
  EXPECT_NUM2(ARG1, ARG2);

  double arg1 =
   (IsReal(ARG1)) ? rvalue(ARG1) :(double) ivalue(ARG1);
  double arg2 =
   (IsReal(ARG2)) ? rvalue(ARG2) :(double) ivalue(ARG2);

  return NewReal(pow(arg1, arg2));
}

//f (random [<limit>]) (random <mean> <deviation>)
//$ Return a uniformly distributed random number less than the
//$ optional limit value.  If not given then the limit is 1.0.
//$ The result retains the type of the argument.
//$ ~With two arguments, return a floating gaussian distributed
//$ value with the given mean and deviation.
//$ ~These functions use a reasonably high quality generator that
//$ passes most statistical tests for randomness.
//x > (for (x 1 10) (print (random 10) :space))
//x 9 1 5 6 9 7 9 1 6 7 = 10
//x > (for (x 1 8) (print (random 0 1) :space))
//x -1.5949  0.0025  1.0016  0.0692 -0.4862  0.6586 -1.4428 -0.0631 = 8
LISP Random(byte nargs, LISP args[])
{
  double mean, dev;

  switch(nargs)
  {
    case 0:  return NewReal(ran1());

    case 1:  EXPECT_NUM(ARG1);
             if(IsReal(ARG1))
               return NewReal(ran1() * rvalue(ARG1));
             else
               return NewInteger((int) floor(ran1() * ivalue(ARG1)));

    case 2:  EXPECT_NUM2(ARG1, ARG2);
             mean =
               (IsReal(ARG1)) ? rvalue(ARG1) :(double) ivalue(ARG1);
             dev =
               (IsReal(ARG2)) ? rvalue(ARG2) :(double) ivalue(ARG2);
             return NewReal(gasdev() * dev + mean);

    default: return LispError(ERR_NUM_ARGS);
  }
}

//#############################################################################
// Boolean operators
//#############################################################################

//f (not <boolean>)
//$ Return t, given nil else return nil. Used to invert
//$ boolean values.
//$ ~Identical to (null <val>) but
//$ different semantics to show the intent more clearly.
//x > (not 0)
//x = nil                  ; 0 isn't nil!
//x > (not nil)
//x = t

//f (null <val>)
//$ Tests <val> to check if it is nil.  Used to check
//$ for an empty list.
//$ ~Identical to (not <val>) but
//$ different semantics to show the intent more clearly.
//x > (null '())
//x = t           ; Yup!
LISP Not(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  return(ARG1) ? NIL : T;
}

//m (and [<boolean> ...])
//$ Evaluates its arguments from left to right until
//$ one of them is nil whereupon it returns nil.
//$ If all arguments are not nil or if there are no arguments
//$ then it returns t.
//$ ~In addition to its use as a boolean operator it can also
//$ be used to check pre-conditions (although when/unless are
//$ better suited for this purpose.)
//x > (and)                  ; Degenerate case
//x = t
//x > (and t nil)
//x = nil
//x > (and (consp x) (pop x))
//x = nil                     ; x was not a cons so (pop x)
//x                           ; was never evaluated
LISP And(LISP& args)
{
  protect(pargs, args);

  while(pargs)
  {
    if(!iEval(Pop(pargs)))
      return NIL;
  }
  return T;
}

//m (or [<boolean> ...])
//$ Evaluates its arguments from left to right until one of
//$ them is not nil whereupon it returns t.
//$ If all arguments are nil or if there are no arguments then
//$ it returns nil.
//$ ~In addition to its use as a boolean operator it can also
//$ be used to check pre-conditions (although when/unless are
//$ better suited for this purpose.).
//x > (or)                 ; Degenerate case
//x = nil
//x > (or t nil)
//x = nil
//x > (or x (do_it))       ; Only do_it if x is NIL
//x = t
LISP Or(LISP& args)
{
  protect(pargs, args);

  while(pargs)
  {
    if(iEval(Pop(pargs)))
      return T;
  }
  return NIL;
}

//f (xor <boolean> [<boolean> ...])
//$ Generates the odd parity function of its boolean arguments.
//$ I.e. if the argument list has odd parity the result will be
//$ t, else nil.
//$ All the arguments are evaluated
//x > (xor)                 ; Degenerate case
//x = nil
//x > (xor t nil)
//x = t
//x > (xor t t)
//x = nil
LISP Xor(LISP& args)
{
  protect(pargs, args);
  bool result = false;
  while(pargs)
  {
    if(iEval(Pop(pargs)))
      result = !result;
  }
  return Boolean(result);
}

//#############################################################################
// Bitwise logical operators
//#############################################################################

//f (bnot <integer>)
//$ Binary not. Flips all the bits of <integer>.
//x > (bnot 0)
//x = -1
LISP Bnot(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  EXPECT_INT(ARG1);
  return NewInteger(~ivalue(ARG1));
}

//f (band [<integer> ...])
//$ Binary and. Ands together coresponding bits of each integer.
//x > (band)
//x = -1                   ; Degenerate case
//x > (band 81 82 83)
//x = 80
LISP Band(LISP& args)
{
  int result = ~0;
  while(args)
    result &= ivalue(Pop(args));

  return NewInteger(result);
}

//f (bor [<integer> ...])
//$ Binary or. Ors together coresponding bits of each integer.
//x > (bor)
//x = 0                    ; Degenerate case
//x > (bor 81 82 83)
//x = 83
LISP Bor(LISP& args)
{
  int result = 0;
  while(args)
    result |= ivalue(Pop(args));

  return NewInteger(result);
}
//f (bxor [<integer> ...])
//$ Binary exclusive or. Exclusive-ors together coresponding
//$ bits of each integer.
//x > (bxor)
//x = 0                    ; Degenerate case
//x > (bxor 81 82 83)
//x = 80
LISP Bxor(LISP& args)
{
  int result = 0;
  while(args)
    result ^= ivalue(Pop(args));

  return NewInteger(result);
}

//f (bset <bit> <integer>)
//$ Returns the result of setting the specified bit in
//$ <integer>.
//x > (bset 3 0)
//x = 8
LISP Bset(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(2);
  EXPECT_INT2(ARG1, ARG2);
  if(ivalue(ARG1) & ~0x1F)
    return LispError(ERR_BIT_NUM, ARG1);
  return NewInteger(ivalue(ARG2) | (1 << ivalue(ARG1)));
}

//f (bclr <bit> <integer>)
//$ Returns the result of clearing the specified bit in
//$ <integer>.
//x > (bclr 0 255)
//x = 254
LISP Bclr(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(2);
  EXPECT_INT2(ARG1, ARG2);
  if(ivalue(ARG1) & ~0x1F)
    return LispError(ERR_BIT_NUM, ARG1);
  return NewInteger(ivalue(ARG2) & ~(1 << ivalue(ARG1)));
}

//f (btgl <bit> <integer>)
//$ Returns the result of toggling the specified bit in
//$ <integer>.
//x > (btgl 0 255)
//x = 254
LISP Btgl(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(2);
  EXPECT_INT2(ARG1, ARG2);
  if(ivalue(ARG1) & ~0x1F)
    return LispError(ERR_BIT_NUM, ARG1);
  return NewInteger(ivalue(ARG2) ^ (1 << ivalue(ARG1)));
}

//f (btst <bit> <integer>)
//$ Returns the value of the specified bit of <integer> as a
//$ boolean (0 => nil, 1 => t).
//x > (btst 0 3)
//x = t                    ; 3 is odd
//x > (btst 13 0)
//x = nil
LISP Btst(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(2);
  EXPECT_INT2(ARG1, ARG2);
  if(ivalue(ARG1) & ~0x1F)
    return LispError(ERR_BIT_NUM, ARG1);
  if(ivalue(ARG2) & (1 << ivalue(ARG1)))
    return T;
  else
    return NIL;
}

//f (bcnt <integer>)
//$ Counts the number of set (= 1) bits in <integer>.
//x > (bcnt 3)
//x = 2
//x > (bcnt -1)
//x = 32
LISP Bcnt(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  EXPECT_INT(ARG1);
  unsigned int a = ivalue(ARG1);
  int cnt = 0;
  while(a)
  {
    if(a & 1)
      cnt++;
    a >>= 1;
  }
  return NewInteger(cnt);
}

//f (bshift <integer> <shift>)
//$ Shifts the bits of <integer> by <shift> places.
//$ Positive values of <shift> shift to the left.
//$ Shifts of greater than 31 bits are handled correctly.
//x > (bshift 1 4)
//x = 16
//x > (bshift 12 -1)
//x = 6
LISP Bshift(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(2);
  EXPECT_INT2(ARG1, ARG2);
  unsigned int a = ivalue(ARG1);
  int s = ivalue(ARG2);
  if(abs(s) > 31)
    return NewInteger(0);
  if(s <= 0) return NewInteger(a >> -s);
  else       return NewInteger(a << s);
}

//f (ashift <integer> <shift>)
//$ Shifts the bits of <integer> arithmetically by <shift>
//$ places. Positive values of <shift> shift to the left.
//$ ~Shifts of greater than 31 bits cannot be handled in
//$ a consistently meaningful way and will cause an error.
//x > (ashift 1 4)
//x = 16
//x > (ashift -2 -1)
//x = -1
LISP Ashift(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(2);
  EXPECT_INT2(ARG1, ARG2);
  int a = ivalue(ARG1);
  int s = ivalue(ARG2);
  if(abs(s) > 31)
    return LispError(ERR_BAD_ARG, ARG2, "Invalid arithmetic shift");

  if(s <= 0) return NewInteger(a >> -s);
  else       return NewInteger(a << s);
}

//#############################################################################
// Control flow
//#############################################################################

//m (if <pred> <then> [<else>])
//$ If <pred> evaluates to nil then evaluate and return
//$ <else> otherwise evaluate and return <then>.
//$ ~If <else> is omitted and <pred> evaluates to nil then
//$ return nil.
//x > (if (= 3 3) 'Equal 'Unequal)
//x = Equal
//x > (if (= x 3) "X is 3" "X is not 3")
//x = "X is not 3"
LISP If(byte nargs, LISP args[])
{
  CHECK_ARITY_IN(2, 3);
  LISP result = iEval(ARG1) ?
                iEval(ARG2) :
                (nargs == 3 ? iEval(ARG3) : NIL);

  return result;
}

//m (cond (<pred>  [<action> ...]) ...)
//$ Takes any number of arguments, each of which is a list.
//$ The car of each list is evaluated, in turn, until one
//$ evaluates non-nil.  The actions which form the cdr of
//$ that list are then evaluated in order and the value of
//$ the last action returned.
//$ ~If no list is selected, the selected list has no actions
//$ or the entire cond has no arguments, then nil is returned.
//$ The last condition in a cond is often t as a catch-all
//$ for everything that has fallen through the prior cases.
//$ ~No arguments are evaluated unless it is expressly stated
//$ so above.
//x > (cond)                         ; Degenerate case
//x = nil
//x > (cond ((atom x) 'atom)
//x         ((listp x) 'list)
//x         (t 'not_atom_or_list) )
//x = atom                           ; So x was an atom
LISP Cond(LISP& args)
{
  protect(pargs, args);
  protect(clause, NIL);
  protect(action, NIL);

  while(pargs)                      // While there are clauses left to test
  {
    clause = Pop(pargs);
    EXPECT_CONS(clause);
    LISP test = iEval(car(clause));

    if(test)
    {
      action = cdr(clause);
      if(!action)                   // No action part, return value of test part
        return test;
      else
        return Progn(action);
    }
  }
  return NIL;
}

//m (for (<var> <init> <to> [<step>]) <body>...)
//$ Evaluate the expressions in <body>... with symbol <var>
//$ taking values from <init> to <to> in steps of <step>.
//$ If step is omitted then 1 is used.  <init>, <to> and <step>
//$ must all be integers.  Counting backwards is OK if step
//$ is negative.  The loop ends when the next value would
//$ exceed <to> or when an enclosed (break) is executed.
//$ <var> is local to the loop.  For returns the counter value.
//x > (for (k 1 10) (print k " "))
//x 1 2 3 4 5 6 7 8 9 10 = 10
//x > (for (k 10 1 -2) (print k " "))
//x 10 8 6 4 2 = 2
LISP For(LISP& args)
{
  protect(var, NIL);
  protect(pargs, args);
  protect(body, cdr(pargs));
  protect(expr, NIL);

  LISP tmp;

  // Extract and check loop variable
  EXPECT_CONS(car(pargs));
  var = caar(pargs);
  pargs = cdar(pargs);
  EXPECT_SYM(var);
  Local(var);

  // Extract and check loop limits
  if(!pargs)
    return LispError(ERR_NUM_ARGS);
  tmp = iEval(Pop(pargs));
  EXPECT_INT(tmp);
  int counter = ivalue(tmp);

  tmp = iEval(Pop(pargs));
  EXPECT_INT(tmp);
  int end = ivalue(tmp);

  // Extract and check loop step
  int step;
  if(!pargs)
  {
    step = 1;
  }
  else
  {
    tmp = iEval(car(pargs));
    if(!IsInteger(tmp))
      step = 1;
    else
      step = ivalue(tmp);
  }

  if(!step)
    return LispError(ERR_NULL_LOOP);

  // Execute loop like progn but allow break to terminate early
  endloop = false;
  while(((step > 0) && (counter <= end)) || ((step < 0) && (counter >= end)))
  {
    LISP val = NewInteger(counter);
    iSet(var, val);
    expr = body;
    while(expr)                             // Implicit progn
    {
      (void) iEval(Pop(expr));
      if(endloop) break;                    // Break inner while
    }
    if(endloop) break;                      // Break outer while
    counter += step;
  }
  endloop = false;

  return NewInteger(counter - step);
}


//m (for-each (<var> <vect-or-list> [<index>] [<retn>]) <body>...)
//$ Evaluate the expressions in <body>... with symbol <var>
//$ taking its values from the top-level elements of <vect-or-list>
//$ <var> is local to the loop. If the symbolic argument <index> is
//$ present then it also becomes a local variable that counts from 0
//$ on each loop. for-each returns the value of the expression <retn>
//$ or the value of the last body expression evaluated if omitted.
//$ <index> may be null if it is not required but a return expression is.
//$ ~Both <var> and <index> are valid <retn> values.
//$ break, while and until may be used to end the loop prematurely.
//x > (for-each (k #(a b c d)) (print k " "))
//x a b c d = nil
//x > (for-each (k '(a b c 1 2 3)) (until (numberp k) (string k)))
//x = "1"
//x > (for-each (k '(a b c 1 2 3) i i) (when (numberp k) (println i :space k)))
//x 3 1
//x 4 2
//x 5 3
//x = 5
LISP ForEach(LISP& args)
{
  protect(var, NIL);
  protect(idx, NIL);
  protect(pargs, args);
  protect(values, NIL);
  protect(body, cdr(pargs));
  protect(expr, NIL);
  LISP result = NIL;

  // Get iteration variable
  EXPECT_CONS(car(pargs));
  var = caar(pargs);
  pargs = cdar(pargs);
  EXPECT_SYM(var);
 
  // Get a list/vector
  if(!pargs)
    return LispError(ERR_NUM_ARGS);
  values = iEval(Pop(pargs));

  if(pargs)
  {
    // Get index variable (if present)
    if(car(pargs))
    {
      idx = Pop(pargs);
      EXPECT_SYM(idx);

      Local(idx);
      iSet(idx, NewInteger(0));
    }
  }

  // Iterate over a vector...
  if(IsVector(values))
  {
    for(int i = 0; i < dim(values); i++)
    {
      iSet(var, data(values)[i]);
      if(idx)
        iSet(idx, NewInteger(i));

      expr = body;
      while(expr)                             // Implicit progn
      {
        result = iEval(car(expr));
        expr = cdr(expr);
        if(endloop) break;                    // Break inner while
      }
      if(endloop) break;                      // Break outer while
    }
  }
  else  // ...or a list
  {
    EXPECT_LIST(values);

    endloop = false;
    int i = 0;
    while(IsCons(values) && !endloop)
    {
      iSet(var, Pop(values));

      expr = body;
      while(expr)                             // Implicit progn
      {
        result = iEval(Pop(expr));
        if(endloop) break;                    // Break inner while
      }
      if(endloop) break;                      // Break outer while

      if(idx)
        iSet(idx, NewInteger(i++));
    }
  }
  endloop = false;

  // Check for an explicit return expression
  if(pargs)
    return iEval(car(pargs));

  return result;
}

//m (loop <body> ...)
//$ Repeatedly evaluates <body>...
//$ It is assumed that at least one of the expressions in
//$ <body>... is, or contains, a while, until or break expression.
//$ If not then then the loop is infinite and can only be escaped
//$ from by an error, throw or user break [Esc].
LISP Loop(LISP& args)
{
  protect(pargs, args);
  protect(start, pargs);


  if(!start)
    return LispError(ERR_NULL_LOOP);


  endloop = false;
  LISP result = NIL;

  while(!endloop)
  {
    if(!pargs)
      pargs = start;                 // Loop

    result = iEval(Pop(pargs));
  }
  endloop = false;
  return result;
}

//m (until <cond> [<body> ...])
//$ If <cond> is non-nil then evaluate the expressions in <body>
//$ in turn, returning the last.  If <body>... is omitted then
//$ return the value of <cond>.
//$ In addition, the next time that control returns to the top
//$ level of a loop function then the loop will exit with the
//$ value of the top-level expression that halted it as the result.
//$ ~It is intended that until only be used at the top-
//$ level of a loop and in this case the operation is obvious.
//$ ~For/foreach loops will also exit but the return value is
//$ discarded.
LISP Until(LISP& args)
{
  LISP result = iEval(car(args));

  if(!result)
    return result;

  if(cdr(args))
    result = Progn(cdr(args));
  endloop = true;
  return result;
}

//m (while <cond> <body> ...)
//$ If <cond> is nil then evaluate the expressions in <body>
//$ in turn, returning the last.  If <body>... is omitted then
//$ return the value of <cond>, always nil.
//$ In addition, the next time that control returns to the top
//$ level of a loop function then the loop will exit with the
//$ entire value of the expression that halted it as the result.
//$ ~It is intended that until only be used at the top-level
//$ of a loop and in this case the operation is obvious.
//$ ~For/foreach loops will also exit but the return value is
//$ discarded
LISP While(LISP& args)
{
  LISP result = iEval(car(args));

  if(result)
    return result;

  if(cdr(args))
    result = Progn(cdr(args));
  endloop = true;
  return result;
}

//f (break <body> ...)
//$ Evaluates its arguments like progn, returning the last.
//$ In addition, the next time that control returns to the top
//$ level of a loop function then the loop will exit with the
//$ entire value of the expression that halted it as the result.
//$ ~For/foreach loops will also exit but the return value is
//$ discarded
//$ ~Equivalent to (while nil <body>...) inside a loop.
//$ ~Note that a for/foreach loop will discard the return
//$ value of break.
LISP Break(LISP& args)
{
  LISP result = Progn(args);
  endloop = true;
  return result;
}

//m (progn <body> ...)
//$ Evaluate the expressions in <body>... in turn,
//$ returning the value of the last.
LISP Progn(LISP& args)
{
  protect(pargs, args);
  LISP result = NIL;

  while(pargs)
  {
    result = iEval(car(pargs));
    pargs = cdr(pargs);
  }
  return result;
}

//m (prog1 <body> ...)
//$ Evaluate the expressions in <body>... in turn, returning
//$ the value of the first. This function avoids creating
//$ temporary storage for the required result.
LISP Prog1(LISP& args)
{
  protect(pargs, args);
  protect(result, NIL);

  if(!pargs)
    return result;

  result = iEval(car(pargs));
  Progn(cdr(pargs));

  return result;
}

//m (when <cond> <body>...)
//$ When <cond> evaluates true evaluate <body> like progn.
//$ Otherwise returns nil and does not evaluate the body.
//x > (when x (println x))
//x hello
//x = "hello"
LISP When(LISP& args)
{
  if(!iEval(car(args)))
    return NIL;

  return Progn(cdr(args));
}

//m (unless <cond> <body>...)
//$ When <cond> evaluates nil evaluate <body> like progn.
//$ Otherwise returns nil and does not evaluate the body.
//x > (unless (< j 0) (println (sqrt j)) t)
//x 3.00000000
//x = t         ; j was 9
LISP Unless(LISP& args)
{
  if(iEval(car(args)))
    return NIL;

  return Progn(cdr(args));
}

//m (case <expr> (<match> <body>...) ... [(:else <body>...)])
//$ Evaluate <expr> and search for an eql <match> in the clauses
//$ that follow.  <match> may be an atom or a list of atoms, any
//$ of which may match with <expr>.  The <body>... of the first
//$ matching clause is evaluated (like progn) and returned.  If the
//$ matching process reaches a clause beginning with :else then this
//$ automatically matches. There may be clauses following the :else
//$ clause but these will always be ignored.
//$ ~Failure to locate a match results in an error.
LISP Case(LISP& args)
{
  protect(pargs, args);

  if(!pargs)
    return LispError(ERR_NUM_ARGS);

  LISP expr = iEval(Pop(pargs));
  if(!IsCons(pargs))
    return LispError(ERR_BAD_CASE, pargs);
  while(pargs)
  {
    LISP clause = Pop(pargs);
    if(!IsCons(clause) || !(IsCons(cdr(clause))))
      return LispError(ERR_BAD_CASE, clause);

    if(IsAtom(car(clause)))
    {
      if(iEql(car(clause), expr) || (car(clause) == ELSE))
        return Progn(cdr(clause));  // Got it (single case key)
      continue;
    }
    if(IsCons(car(clause)))
    {
      if(iMember(expr, car(clause)))
        return Progn(cdr(clause));  // Got it (list of case keys)
      continue;
    }
    return LispError(ERR_BAD_CASE, clause);
  }
  return LispError(ERR_NO_CASE, expr);
  //return NIL;
}

//#############################################################################
// Errors and exceptions
//#############################################################################

//f (error <num> [<obj>] [<string>])
//$ Raise an error with the given message string and error
//$ number.
//$ The default error message depends upon the error number.
//$ ~Negative error numbers are fatal errors and cannot be
//$ trapped with errorset.
//$ ~All messages are prefixed with "Error [<num>]:" or
//$ "Fatal [<num>]:" except for errors 0 and 1.
//$ The error object (if any) will be printed on the next line.
//$ To specify no error object use :none
//x > (error 1 'foo)
//x
//x Programmed error
//x Error object: foo
LISP Error(byte nargs, LISP args[])
{
  CHECK_ARITY_IN(1, 3);
  EXPECT_INT(ARG1);

  if(nargs == 1)
    LispError((LERROR)ivalue(ARG1));

  LISP eobj = NONE;
  if(nargs > 1)
    eobj = ARG2;

  if(nargs == 3)
  EXPECT_STR(ARG3);

  LispError((LERROR)(ivalue(ARG1)), eobj, (nargs == 3) ? text(ARG3) : 0L);

  return NIL;
}

//m (errorset <body>)
//$ Evaluate <body> and return the result wrapped in
//$ a list.  If a trappable error should occur during the
//$ evaluation of <body> then the error number is returned
//$ instead.
//x > (errorset (+ 1 2))
//x = (3)
//x > (errorset (- 3 'a))
//x = 5                   ; = :err_num_exp
LISP Errorset(LISP& args)
{
  PFrame savebase = base;
  LISP result;
  int winexcept;

  try      // catch
  {
    try    // __except (Windows SEH)
    {
      while(args)
        result = iEval(Pop(args));

      result = NewCons(result, NIL);
    }
    __except(FilterException(winexcept = GetExceptionCode()))
    {
      OSException(winexcept);   // Throw Lisp Error
    }
  }
  catch(Err e)
  {
    // Don't trap fatal errors
    if(e.err <= 0)
      throw;
    // If no user-supplied message then supply the default
    if(!e.msg)
      e.msg = err_msg(e.err);

    // Save the error message
    ErrorMessage = (char *)e.msg;

    // Restore our stack frame
    while(base != savebase)
      PopFrame();
    result = NewInteger(e.err);
  }
  return result;
}

//f (errormsg [<integer>])
//$ Returns the default error message associated with an error
//$ number or the last Lisp error trapped by errorset if no
//$ argument is given. The message is returned as a string.
//x > (errormsg 3)
//x = "Incorrect number of arguments to function"
LISP ErrorMsg(byte nargs, LISP args[])
{
  CHECK_ARITY_LE(1);
  if(nargs == 1)
  {
    EXPECT_INT(ARG1);
    return NewString((char *)err_msg((LERROR)ivalue(ARG1)));
  }

  if(!ErrorMessage) return NIL;
  return NewString(ErrorMessage);
}

//m (guard <body>... <cleanup>)
//$ Guard functions just like progn except that the last
//$ expression in a guard is always executed.
//$ If any form of non-local exit from <body>
//$ occurs then the final <cleanup> form will be executed
//$ before control leaves the guard.
//$ ~Exceptions occurring in the cleanup expression are not
//$ affected.  Errors will still be reported and guard will
//$ not return if an error occurs in any of the expressions.
//x > (guard (print "before ") (+ 3 "q") (print "after"))
//x before after
//x Error [5] : Numeric argument expected
//x Error object: "q"
//x Failed in: (+ 3 "q")
LISP Guard(LISP& args)
{
  LISP result;
  protect(pargs, args);

  try
  {
    while(cdr(pargs))
      iEval(Pop(pargs));

  }
  __finally
  {
    while(cdr(pargs))
      pargs = cdr(pargs);
    result = iEval(car(pargs));
  }
  return result;
}


//f (catch <tag> <body>...)
//$ Evaluates its arguments, after the first, and returns the
//$ value of the last.  If any of the body code or any code
//$ called from the body throws with the specified tag then the
//$ value of the thrown expression is returned instead.
//$ If the throw does not specify a value then :none is returned.
//$ <tag> can be any symbol, usually a key.
//x > (defkey :ball)
//x = :ball
//x > (catch :ball (throw :ball 123))
//x = 123
LISP Catch(LISP& args)
{
  PFrame savebase = base;
  LISP result = NIL;

  if(!args)
    return LispError(ERR_NUM_ARGS);

  protect(tag, car(args));
  protect(pargs, cdr(args));

  tag = iEval(tag);
  EXPECT_SYM(tag);

  try
  {
    while(pargs)
      result = iEval(Pop(pargs));

    return result;
  }
  catch(Ball b)
  {
    if(iEq(tag, b.tag))
    {
      // Restore our stack frame
      while(base != savebase)
        PopFrame();
      return b.value;
    }
    else
      throw;
  }
}

//f (throw <tag> [<value>])
//$ Throws a value to a catch expression with the same tag.
//$ ~The transfer of control to the catch expression is immediate
//$ and throw never returns (or rather it returns from the catch)
//$ ~If throw is used without a matching enclosing catch then it
//$ propogates right up to the read-eval-print loop where an error
//$ is reported.  <tag> can be any symbol, usually a key.
//x > (defkey :ball)
//x = :ball
//x > (defun jack-in-the-box  nil
//x     (when (< (random 1000000) 2)
//x           (throw :ball "Escaped!") ))
//x = jack-in-the-box
//x > (catch :ball
//x     (loop (jack-in-the-box)))  ; Loop until the ball is thrown
//x = "Escaped!"                   ; after some indeterminate time
LISP Throw(byte nargs, LISP args[])
{
  CHECK_ARITY_IN(1,2);
  EXPECT_SYM(ARG1);
  Ball b(ARG1, (nargs == 2) ? ARG2 : NONE);
  throw b;
}

//f (backtrace [<integer>])
//$ Sets the number of stack frames to dump on an error. With
//$ no argument it reports the current value.
//$ ~In debug mode it immediately dumps <integer> stack levels
//$ of context.
//x > (backtrace 5)
//x = 5
LISP Backtrace(byte nargs, LISP args[])
{
  if(nargs == 0)
    return NewInteger(backtrace);
  CHECK_ARITY_EQ(1);
  EXPECT_INT(ARG1);
  if(DebugLevel > 0)
    BacktraceDump(ivalue(ARG1));
  else
    backtrace = ivalue(ARG1);
  return ARG1;
}

//m (exit)
//$ Exit the interpreter silently.
LISP Exit(LISP& args)
{
  if(args)
    return LispError(ERR_NUM_ARGS);
  LispError(FATAL_EXIT);        // Silent exit from interpreter
  return NIL;                   // Never executed
}

//#############################################################################
// System functions
//#############################################################################

//f (locale [<string>])
//$ Gets or sets all locale strings.  (locale "") restores the default
//$ locale and (locale) returns the currents settings as a string.
//x > (locale "English_United Kingdom.850")
//x = t
LISP Locale(byte nargs, LISP args[])
{
  if(nargs == 0)
  {
    char* loc = setlocale(LC_ALL, 0L);
    if(loc)
      return NewString(loc);
    else
      return NIL;
  }
  CHECK_ARITY_EQ(1);
  EXPECT_STR(ARG1);
  return setlocale(LC_ALL, (text(ARG1))) == 0 ? NIL : T;
}

//f (delete-file <string>)
//$ Removes the named file from a filesystem. Returns nil.
//x > (delete-file "C:/Program Files/AJBLisp/tmpdata.dat")
//x = nil
LISP lDeleteFile(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  EXPECT_STR(ARG1);
  if(!DeleteFile(text(ARG1)))
    return WindowsError();
  return NIL;
}

//f (move-file <string> <string>)
//$ Move or rename a file from the first path string to the
//$ second. Returns nil.
//x > (move-file "C:/Program Files/AJBLisp/file.dat" "C:/f.dat")
//x = nil
LISP lMoveFile(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(2);
  EXPECT_STR2(ARG1, ARG2);
  if(!MoveFile(text(ARG1), text(ARG2)))
    return WindowsError();
  return NIL;
}

//f (copy-file <string> <string>)
//$ Copy a file from the first path string to the second.
//$ Returns nil.
//x > (copy-file "C:/Program Files/AJBLisp/file.dat" "C:/f.dat")
//x = nil
LISP lCopyFile(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(2);
  EXPECT_STR2(ARG1, ARG2);
  if(!CopyFile(text(ARG1), text(ARG2), TRUE))
    return WindowsError();
  return NIL;
}

//f (protect-file <string> <ro>)
//$ Alter the read-only flag of a file. Return t on success.
//$ <ro> is a boolean that stored to the read-only flag.
//x > (protect-file "fred.dat" nil)
//x = t
LISP ProtectFile(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(2);
  EXPECT_STR(ARG1);
  if(chmod(text(ARG1), ARG2 ? S_IREAD : S_IREAD | S_IWRITE))
    return NIL;
  return T;
}

//f (create-directory <string>)
//$ Create a new directory.  Always returns nil.
//x > (create-directory "./data")
//x = nil
LISP lCreateDirectory(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  EXPECT_STR(ARG1);
  if(!CreateDirectory(text(ARG1), 0L))
    return WindowsError();
  return NIL;
}

//f (delete-directory <string>)
//$ Delete a directory, failing if the directory is not empty  Returns nil.
//x > (delete-directory "./data")
//x = nil
LISP lDeleteDirectory(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  EXPECT_STR(ARG1);
  if(!RemoveDirectory(text(ARG1)))
    return WindowsError();
  return NIL;
}

//f (dir <string> [t])
//$ List a directory.  Returns a list of filenames matching the
//$ given path string which may have wildcard characters.
//$ ~With a single argument, subdirectories are returned as sublists,
//$ each containing a single directory name. If the second
//$ boolean argument is true then all items are returned as
//$ sublists with the following structure:
//$ (name attrib created accessed modified).
//$ ~Attrib is a string of the form "drhsa" with active attributes
//$ capitalised.  E.g. a readonly directory would return "DRhsa".
//$ created, accessed and modified are the file timestamps returned
//$ as integer times.
//$ ~Note that the supplied path should always be complete as in
//$ the example below.
//x > (dir "c:/*")
//x = ("AUTOEXEC.BAT" "boot.ini" "BOOTLOG.PRV" ...
//x   ... "ntldr" ("Program Files") ("Temp") ("WINDOWS"))
//x > (dir "c:/")        ; Path specifies no files
//x = nil
LISP Dir(byte nargs, LISP args[])
{
  WIN32_FIND_DATA filedata;
  HANDLE ff;
  bool full = false;
  CHECK_ARITY_LE(2);
  if((nargs == 2) && ARG2)
    full = true;
  EXPECT_STR(ARG1);
  ff = FindFirstFile(text(ARG1), &filedata);
  if(ff == INVALID_HANDLE_VALUE)
    return NIL;

  protect(result, NewCons(NIL, NIL));
  protect(tail, result);
  while(true)
  {
    LISP fi;
    if(full)
    {
      fi = iFileInfo(filedata);
    }
    else
    {
      if(filedata.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
        fi = NewCons(NewString(filedata.cFileName), NIL);
      else
        fi = NewString(filedata.cFileName);
    }
    car(tail) = fi;

    if(FindNextFile(ff, &filedata))
    {
      cdr(tail) = NewCons(NIL, NIL);
      tail = cdr(tail);
      continue;
    }
    FindClose(ff);

    if(GetLastError() ==  ERROR_NO_MORE_FILES)
      return result;

    return WindowsError();
  }
}

// Construct a list of the directory information for a file or directory
LISP iFileInfo(WIN32_FIND_DATA& filedata)
{
  int createtime, accesstime, writetime;
  char attrib[6] = "drhsa";
  if(filedata.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
    attrib[0] = 'D';
  if(filedata.dwFileAttributes & FILE_ATTRIBUTE_READONLY)
    attrib[1] = 'R';
  if(filedata.dwFileAttributes & FILE_ATTRIBUTE_HIDDEN)
    attrib[2] = 'H';
  if(filedata.dwFileAttributes & FILE_ATTRIBUTE_SYSTEM)
    attrib[3] = 'S';
  if(filedata.dwFileAttributes & FILE_ATTRIBUTE_ARCHIVE)
    attrib[4] = 'A';

  createtime = iFileTimeConvert(filedata.ftCreationTime);
  accesstime = iFileTimeConvert(filedata.ftLastAccessTime);
  writetime  = iFileTimeConvert(filedata.ftLastWriteTime);

  protect(result, NIL);
  LISP l;
  l = NewInteger(writetime);
  result = NewCons(l, NIL);
  l = NewInteger(accesstime);
  result = NewCons(l, result);
  l = NewInteger(createtime);
  result = NewCons(l, result);
  l = NewString(attrib);
  result = NewCons(l, result);
  l = NewString(filedata.cFileName);
  return NewCons(l, result);
}

// Convert a file time to a Lisp integer time
int iFileTimeConvert(FILETIME& ft)
{
  SYSTEMTIME st;

  // These times are invalid. Return -1;
  if((ft.dwHighDateTime > 0x80000000UL) ||
     !(ft.dwHighDateTime | ft.dwLowDateTime))
    return -1;

  FileTimeToSystemTime(&ft, &st);
  return (int)iEncodeTime(st.wYear, st.wMonth, st.wDay,
                          st.wHour, st.wMinute, st.wSecond);
}

//f (cd [<string>])
//$ Change the current directory and drive. This is the only
//$ way to change the current directory.
//$ (system "cd <path>") will not work as it only changes the
//$ directory for the newly created process.
//$ ~Beware that (cd "c:\") and the like will apparently lock
//$ up the listener because the correct command is (cd "c:\\")
//$ or (cd "c:/").  If you get stuck like this then type ") to
//$ close the string and the list and re-type the command
//$ correctly. cd returns nil, unless no argument is given, where
//$ it returns the current directory as a string.
//x > (cd)
//x = "E:\Work\Prog\CPP\AJBLisp\Distribution"
//x > (cd "..")
//x = nil
//x > (cd)
//x = "E:\Work\Prog\CPP\AJBLisp"
LISP Cd(byte nargs, LISP args[])
{
  CHECK_ARITY_LE(1);
  if(nargs == 0)
  {
    // Get
    LISP result = NIL;

    int r = GetCurrentDirectory(BUFSZ, txtbuf);
    if(!r)
      return WindowsError();
    if(r < BUFSZ)
      return NewString(txtbuf);

    char *path = (char *)malloc(r);
    if(path)
    {
      if(GetCurrentDirectory(r, path))
        result = NewString(path);
      free(path);
    }
    return result;
  }
  // Set
  EXPECT_STR(ARG1);
  if(!SetCurrentDirectory(text(ARG1)))
    return WindowsError();
  return NIL;
}

//f (system <command> [<dir> [<outfile> [<infile>]]])
//$ Pass the <command> string to a new shell process and return the
//$ exit code as an integer.  <dir> is the directory in which to run
//$ the new process and it defaults to the current directory.
//$ ~If <outfile> is present then this becomes the stdout/stderr
//$ for the new process.  Likewise for <infile> which replaces stdin.
//$ ~Note that these must be file atoms, not file names.
//$ ~The defaults are for output to go to the terminal and for input
//$ to be at EOF.  Any of the optional arguments may be explicitly set
//$ to nil to retain the default setting.
//$ ~Input/output redirection and pipes may be used in the command
//$ string and this approach is easiest if you simply want a commands
//$ input and/or output in a named file.
//$ ~Note that using / instead of \\ in path names will not work
//$ reliably because many shell commands interpret / as the prefix of
//$ a switch.
//x > (system "cd")                ; Where are we?
//x C:\Program Files\AJBLisp
//x = 0
//x > (system "cd" "e:\\Work")     ; Now run cd in e drive
//x e:\Work
//x = 0
//x > (system "dir >dir.txt c:\\") ; Command redirection
//x = 0
//x > (system "ipconfig" nil file) ; Append output to an open file
//x = 0                            ; but let the directory default
//x > (system "start c:\\")        ; Run a GUI process
//x = 0
LISP lSystem(byte nargs, LISP args[])
{
  FILE *out = 0L;
  FILE *in  = 0L;
  char *dir = 0L;

  CHECK_ARITY_IN(1, 4);
  EXPECT_STR(ARG1);

  if(nargs == 2 && ARG2)
  {
    EXPECT_STR(ARG2);
    dir = text(ARG2);
  }

  if(nargs == 3 && ARG3)
  {
    EXPECT_FILE(ARG3);
    out = file(ARG3);
  }

  if(nargs == 4 && ARG4)
  {
    EXPECT_FILE(ARG4);
    in = file(ARG4);
  }

  return NewInteger(RunCommand(text(ARG1), out, in, dir));
}

//f (sleep <integer>)
//$ Delays for an integer numer of milliseconds.  Always return nil.
//x > (sleep (random 10000))
//x = nil
LISP lSleep(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  EXPECT_INT(ARG1);

  int ms = ivalue(ARG1);
  if((ms < 0) || (ms > 0xFFFF))
    return LispError(ERR_BAD_ARG, ARG1, "Invalid sleep duration");

  Sleep((unsigned short)ivalue(ARG1));
  return NIL;
}

//f (scripted)
//$ Returns t if running from a command line script and nil if
//$ running interactively.
//$ ~Note that loading a .lsp file directly
//$ or indirectly from the listener is not considered to be
//$ 'script' mode. In fact script mode ends permanantly, for any
//$ session, just before the first prompt appears.
LISP Scripted(LISP& args)
{
  if(args)
    return LispError(ERR_NUM_ARGS);
  return Boolean(!Interactive);
}

//f (environ [<var> [<value>]])
//$ Read/write environment variables.  With no arguments it
//$ returns the whole environment as a list of strings.
//$ ~With one string argument it returns the value of the named
//$ variable as a string or nil if not found.  With two string
//$ arguments it sets <var> to <value> and returns a boolean
//$ which is true if successful.
//x > (environ "windir")
//x = "C:\WINDOWS"
LISP Environ(byte nargs, LISP args[])
{
  if(nargs == 0)
  {
    protect(res, NIL);
    protect(str, NIL);
    int i = 0;
    while (_environ[i])
    {
      str = NewString(_environ[i++]);
      res = NewCons(str, res);
    }
    return res;
  }

  EXPECT_STR(ARG1);
  if(strchr(text(ARG1), '='))
    return LispError(ERR_BAD_ARG, ARG1,
              "Environment variable name cannot contain '='");

  if(nargs == 1)
  {
    char* r = getenv(text(ARG1));
    if(r) return NewString(r);
    return NIL;
  }

  CHECK_ARITY_EQ(2);
  EXPECT_STR(ARG2);

  strcpy(txtbuf, text(ARG1));
  strcat(txtbuf, "=");
  strcat(txtbuf, text(ARG2));
  return putenv(strdup(txtbuf)) ? NIL : T;
}

//#############################################################################
// Debug and instrumentation
//#############################################################################

//f (debug t)
//$ Enables the debug loop.  Debug loop prompts are prefixed with the
//$ debug level.  Additional debug loop commands are:
//$ .(r: <x>)  ; Return <x> from faulted function
//$ .e:        ; Throw the error that entered this loop
//$ .t:        ; Return to top level
LISP Debug(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  DebugLoop = (ARG1 != 0);
  return ARG1;
}

//f (addr <any>)
//$ Return the heap address of <any> as an integer.
//$ This will be valid only until the next GC.
//$ ~Note that the example will fail if the heap is
//$ nearly full.
//x > (addr t)
//x = 10354720
//x > (addr 't)
//x = 10354720       ;; i.e. (eq t 't) -> t
LISP Addr(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  return NewInteger((int)args[0]);
}

//m (dump [<more>])
//$ Debug dump of heap. Probably best sent to a logfile for
//$ detailed examination because the output will be very long.
//$ ~Every heap object is dumped in a linear scan.
//$ Any object reference not properly resolved in the heap or
//$ object of zero length will be flagged with "### Error -"
//$ ~Zero length objects will abort the dump since the offset to
//$ the next real object is unknown.  dump returns the total
//$ number of errors found.
//$ ~If <more> is present and not null then add the addresses of
//$ the car/cdr of cons nodes and the value/property of symbols to
//$ the dump.  The dump cannot be fully understood without this
//$ info but the shorter version is easier to scan for errors.
LISP Dump(LISP& arg)
{
  return NewInteger(DumpHeap((bool)arg));
}

//m (dumphash)
//$ Debug dump of symbol hashtable including occupancy
//$ histogram figures.
LISP DumpHash(LISP&)
{
  DumpHashtable();
  return NIL;
}

//m (statistics [0])
//$ Fetch system statistics, (EvalCount GCCount ConsCount).
//$ ~EvalCount does not include simple value lookups but only
//$ real function applications. With any argument (it makes
//$ sense to use 0) it resets the counters to zero and returns
//$ nil.
//x > (statistics)
//x = (2268 1 14735)
LISP Statistics(LISP& arg)
{
  if(arg)
  {
    EvalCount = 0;
    GCCount   = 0;
    ConsCount = 0;

    return NIL;
  }
  protect(result, NIL);
  result = NewCons(NewInteger(ConsCount), result);
  result = NewCons(NewInteger(GCCount), result);
  result = NewCons(NewInteger(EvalCount), result);

  return result;
}


