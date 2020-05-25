////////////////////////////////////////////////////////////////////////////
//
//  AJBLisp - A Lisp Interpreter
//
//  (c) A J Le Couteur Bisson 1997-2007
//
////////////////////////////////////////////////////////////////////////////
// Repl.cpp
//
// Read eval print loop and error handling
////////////////////////////////////////////////////////////////////////////
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <float.h>
#include "AJBLisp.h"
#include "listenerif.h"
//---------------------------------------------------------------------------

bool UserBreak;              // If true then iEval will be interrupted
int  DebugLevel = 0;
bool DebugLoop = false;

static char *winmsg = NULL;  // Windows error message

// The main Read Evaluate Print Loop
void REPL()
{
  int winexcept;
  // Main interpreter loop
  while(true)
  {
    try      // catch
    {
      try    // __except (Windows SEH)
      {
        tprompt(true);
        StackReset();
        LISP cell = replread();
        if(cell == EOF_)
          LispError(FATAL_EXIT);
        tprompt(false);

        UserBreak = false;
        cell = iEval(cell);
        writes("= ");
        print(cell, stdout, false);
        writec('\n');
        SetHistoryVars(cell);
      }
      __except(FilterException(winexcept = GetExceptionCode()))
      {
        OSException(winexcept);  // Throw Lisp Error
      }
    }
    catch(Err e)
    {
      if(ReportError(e))
        break;                   // Break out of REPL loop on fatal error
    }

    // Trap Lisp throw with no catch target
    catch(Ball b)
    {
      Err e;
      e.err = ERR_CATCH;
      e.msg = NULL;
      e.obj = b.tag;
      ReportError(e);
    }

    // Silent return to top level or die if -ve
    catch(int i)
    {
      if(i < 1)
        break;
    }

    // Return value from debug-loop
    catch (LISP result)
    {
      writes("= ");
      print(result, stdout, false);
      writec('\n');
    }

    // Oops! Die!
    catch(...)
    {
      break;
    }
    tprompt(false);
  }
}

void SetHistoryVars(LISP output)
{
  value(PERCENT6) = value(PERCENT5);
  value(PERCENT5) = value(PERCENT4);
  value(PERCENT4) = value(PERCENT3);
  value(PERCENT3) = value(PERCENT2);
  value(PERCENT2) = value(PERCENT);
  value(PERCENT)  = output;
  value(PERCENT1) = output;
}

// The debug Read Evaluate Print Loop
LISP DebugREPL(Err e)
{
  int winexcept;
  pointer stacklvl = stop;

  DebugLevel++;
  // Debug interpreter loop
  while(true)
  {
    try      // catch
    {
      try    // __except (Windows SEH)
      {
        tprompt(true);
        LISP cell = replread();

        if((cell == EOF_) || ThreadCheckTerminate())
           throw 0;   // Exit interpreter

        if(IsCons(cell) && IsSymbol(car(cell))&& !strcmp(name(car(cell)), "r:"))
        {
          DebugLevel--;
          while(stop > stacklvl)
            PopFrame();

          LISP ret = IsCons(cdr(cell)) ? cadr(cell) : NIL;

          // Check for top-level
          if(base->last == 0)
            throw ret;
          else
            return ret;
        }

        if(IsSymbol(cell))
        {
          // Throw original error
          if(!strcmp(name(cell), "e:"))
          {
            DebugLevel = 0;
            throw &e;
          }

          // Return to top level
          if(!strcmp(name(cell), "t:"))
          {
            DebugLevel = 0;
            throw 1;
          }

          // Up one level
          if(!strcmp(name(cell), "u:"))
          {
            // Top level?
            if(base->last == 0)
            {
              DebugLevel = 0;
              throw 1;
            }

            PopFrame();
            writes("function: ");
            print(base->fun, stdout, false);
            writec('\n');
            tprompt(false);
            continue;
          }
        }
        tprompt(false);
        UserBreak = false;
        cell = iEval(cell);
        if(ThreadCheckTerminate())
           throw 0;   // Exit interpreter
        twritef("%d", DebugLevel);
        writes("= ");
        print(cell, stdout, false);
        writec('\n');
      }
      __except(FilterException(winexcept = GetExceptionCode()))
      {
        OSException(winexcept);  // Throw Lisp Error
      }
    }

    catch(Err e)
    {
      if(ReportError(e))
        throw 0;                   // Break out of REPL loop on fatal error
    }

    catch(Err *e)
    {
      throw *e;
    }

    catch(LISP)
    {
      throw;
    }

    catch(...)
    {
      break;
    }
    tprompt(false);
  }

  // Never reached
  return NIL;
}

//---------------------------------------------------------------------------
bool LoadFile(char *filename)
{
  // Load a lisp source file
  if(!(filename && *filename))
    return false;
  
  FILE* f = iOpenLispSource(filename);
  if(f)
  {
    try
    {
      while(!feof(f))
      {
        LISP in = read(f);
        UserBreak = false;
        iEval(in);
      }
    }
    __finally
    {
      fclose(f);
    }
    return true;
  }
  return false;
}

//---------------------------------------------------------------------------
// Throw an error to be caught by ERRORSET or the top level loop, REPL()
// If the debug loop is enabled, enter it on non-fatal errors
LISP LispError(LERROR err, LISP obj, char *msg)
{
  static Err e;

  e.err = err;
  e.msg = msg;
  e.obj = obj;
  if(DebugLoop && (err > 0))
  {
    if(ReportError(e))
      throw 0;        // Exit to top level REPL on fatal error

    // Otherwise enter debug repl
    writes("(r: result) return, e: throw error, t: top level, u: up one level\n");
    return DebugREPL(e);
  }
  else
    throw e;
}

LISP WindowsError()
{
  static Err  e;
  integer     winerr = GetLastError();

  // The memory allocated here is freed in ReportError below
  FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
                NULL, winerr, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                (LPTSTR) &winmsg, 0, NULL);
  e.err = ERR_OS;
  e.msg = winmsg;
  e.obj = NewInteger(winerr);
  ReportError(e);
  return DebugREPL(e);
}

//---------------------------------------------------------------------------
// Process an error
// Print the message and error object
// Display a stack trace
// Return to the top level
// Return true on fatal errors
bool ReportError(Err e)
{
  if(!e.msg) // If no user-supplied message then supply the default
    e.msg = err_msg(e.err);

  if(e.err > 0)  // Non fatal errors
  {
    if(e.err == 1)
      fwritef(stderr, "\n%s\n", e.msg);
    else
      fwritef(stderr, "\nError [%ld] : %s\n", e.err, e.msg);
    if(e.obj != NONE)
    {
      fwrites(stderr, "Error object: ");
      print(e.obj, stderr, false);
      pointer save = (pointer)base;

      // If we can do so without error, print the value of the error object.
      // The following code ignores non-fatal exceptions and restores the
      // stack frame for backtrace afterwards.
      // Nothing is printed for objects that evaluate to themselves to avoid
      // pointless clutter.
      try
      {
        try
        {
          try
          {
            LISP val = iEval(e.obj);
            if(val != e.obj)  // Don't state the obvious...
            {
              fwrites(stderr, " = ");
              print(val, stderr, false);
            }
          }
          catch(...)
          {
            // Do Nothing
          }
        }
        __except(FilterException(GetExceptionCode()))
        {
          // Do Nothing
        }
      }
      __finally
      {
        // Unwind the stack to the original error
        while((pointer)base > save)
          PopFrame();
      }
      fwritec(stderr, '\n');
    }

    // Print extra diagnostics if not a user break
    bool extra = e.err != ERR_USER;

    // Don't access dead stack
    if((pointer)stop > stack)
    {
      if(extra)
      {
        // Print the failed expression
        fwrites(stderr, "Failed in: ");
        print(base->fun, stderr, false);
        fwritec(stderr, '\n');
      }
      BacktraceDump(backtrace);
    }
    flush(stdin);

    // Free messages provided by windows
    if(winmsg)
    {
      LocalFree(winmsg);
      winmsg = 0L;
    }
    return false;
  }

  // Here on fatal errors
  if(e.err != 0)
  {
    sprintf((char*)heap, "\nFatal [%ld] : %s\n", e.err, e.msg);
    MessageBox(ListenerHandle, (char*)heap, "AJBLisp Fatal Error", MB_ICONERROR);
  }
  else if(e.msg && *e.msg)  // Error 0 with message
  {
    sprintf((char*)heap, "%s\n", e.msg);
    MessageBox(ListenerHandle, (char*)heap, "AJBLisp Closing", MB_ICONINFORMATION);
  }
  // Otherwise we have error 0 with no message which is a silent exit

  // Free messages allocated by windows
  if(winmsg)
    LocalFree(winmsg);

  return true;
}

#if(__BCPLUSPLUS__ <= 0x540)
#define _exception math_exception
#endif

// A maths error has been signalled.
// Build a Lisp Error and throw it
int __cdecl _matherr(struct _exception *e)
{
  static char  msgbuf[40];
  char* msg;
  static Err le;
  le.msg = msgbuf;
  le.obj = NONE;

  switch(e->type)
  {
    case DOMAIN:
      msg = "Domain error in ";
      le.err = ERR_M_DOM;
    break;
    case SING:
      msg = "Singular result in ";
      le.err = ERR_M_SING;
    break;
    case OVERFLOW:
      msg = "Overflow in ";
      le.err = ERR_M_OVRF;
    break;
    case UNDERFLOW:
      msg = "Underflow in ";
      le.err = ERR_M_UNDF;
    break;
    case TLOSS:
      msg = "Total loss of accuracy in ";
      le.err = ERR_M_INAC;
    break;
  }

  // C++ uses different names for some functions
  char* name = e->name;
  if(!strcmp(name, "log"))   name = "ln";
  if(!strcmp(name, "log10")) name = "log";
  if(!strcmp(name, "atan2")) name = "atan";
  if(!strcmp(name, "pow"))   name = "power";
  strcpy(msgbuf, msg);
  strcat(msgbuf, name);
  throw le;
}


// Indicate which execeptions are handled
int FilterException(int code)
{
  switch(code)
  {
    case EXCEPTION_FLT_DIVIDE_BY_ZERO:
    case EXCEPTION_FLT_INEXACT_RESULT:
    case EXCEPTION_FLT_INVALID_OPERATION:
    case EXCEPTION_FLT_OVERFLOW:
    case EXCEPTION_FLT_UNDERFLOW:
    case EXCEPTION_INT_DIVIDE_BY_ZERO:
    case EXCEPTION_INT_OVERFLOW:
      return EXCEPTION_EXECUTE_HANDLER;    // We handle these
    default:
      return EXCEPTION_CONTINUE_SEARCH;    // ...but no others
  }
}

// A structured exception has been delivered by Windows
// Translate it into a lisp error and throw it
Err OSException(int code)
{
  Err aritherror;
  aritherror.obj = NONE; 

  switch(code)
  {
    case EXCEPTION_FLT_DIVIDE_BY_ZERO:
      aritherror.msg = "Floating point divide by zero";
      aritherror.err = ERR_M_DIVZ;
    break;
    case EXCEPTION_FLT_INEXACT_RESULT:
      aritherror.msg = "Floating point total loss of accuracy";
      aritherror.err = ERR_M_INAC;
    break;
    case EXCEPTION_FLT_INVALID_OPERATION:
      aritherror.msg = "Invalid floating point operation";
      aritherror.err = ERR_M_INV;
    break;
    case EXCEPTION_FLT_OVERFLOW:
      aritherror.msg = "Floating point overflow";
      aritherror.err = ERR_M_OVRF;
    break;
    case EXCEPTION_FLT_UNDERFLOW:
      aritherror.msg = "Floating point underflow";
      aritherror.err = ERR_M_UNDF;
    break;
    case EXCEPTION_INT_DIVIDE_BY_ZERO:
      aritherror.msg = "Integer divide by zero";
      aritherror.err = ERR_M_DIVZ;
    break;
    case EXCEPTION_INT_OVERFLOW:
      aritherror.msg = "Integer overflow";
      aritherror.err = ERR_M_OVRF;
    break;
  }
  _fpreset();
  throw aritherror;
}


