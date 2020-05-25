////////////////////////////////////////////////////////////////////////////
//
//  AJBLisp - A Lisp Interpreter
//
//  (c) A J Le Couteur Bisson 1997-2007
//
////////////////////////////////////////////////////////////////////////////
// AJBLispInit.cpp
//
// Main thread - Initialisation
////////////////////////////////////////////////////////////////////////////
#include <condefs.h>
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#pragma hdrstop
#include "AJBLisp.h"

//---------------------------------------------------------------------------
// Default sizes
int LispHeapSize = 128 * 1024;
int StackSize = 16 * 1024;


// Set when interactive REPL entered
bool Interactive = false;

void AJBLisp(int argc, char *argv[])
{
  // Deal with the arguments
  int arg, argl;

  for(arg = 1; arg < argc; arg++)
  {
    if(argv[arg][0] != '-') break;
    switch(argv[arg][1])
    {
      case 'h':     // heap
        LispHeapSize = (atoi(&argv[arg][2]));
        if(LispHeapSize < 32768)
        {
          MessageBox(ListenerHandle, "Heap too small or error in -h switch\n",
                     "AJBLisp Fatal Error", MB_ICONERROR);
          exit(0);
        }
      break;

      case 's':     // stack
        StackSize = (atoi(&argv[arg][2]));
        if(StackSize < 1024)
        {
          MessageBox(ListenerHandle, "Stack too small or error in -s switch\n",
                     "AJBLisp Fatal Error", MB_ICONERROR);
          exit(0);
        }
      break;

      case 'l':     // load source
      break;

      default:
        MessageBox(ListenerHandle, "Unknown command line switch\n",
                   "AJBLisp Fatal Error", MB_ICONERROR);
        exit(0);
      break;
    }
  }

  // Startup banner
  writes(";;\n");
  writef(";; AJBLisp - %s\n", Version);
  writes(";;\n");
  writes(";; (c) A J Le Couteur Bisson 1997-2007\n");
  writes(";;\n\n");
#ifdef _DEBUG
  writes("**** DEBUGGING ENABLED ****\n\n");
#endif
  // Allocate memory
  Stack(StackSize);                     // Allocate a stack
  Heap(LispHeapSize);                   // ...and some heap

  try
  {
    try
    {
      // Load the initialisation file, if any, before any
      // command line args are read
      LoadFile("init.lsp");

      // All arguments from the first without a leading '-' are passed into
      // the interpreter as strings in a list named cmdline
      protect(cmd, NewSym("cmdline", true));
      value(cmd) = NIL;

      // Go from the end back and collect trailing arguments
      for(argl = argc - 1; argl >= arg; argl--)
        value(cmd) = NewCons(NewString(argv[argl]), value(cmd));

      char* file;

      // Load user supplied source file(s) if any
      for(arg = 1; arg < argc; arg++)
      {
        if(argv[arg][0] != '-') break;
        if(argv[arg][1] == 'l')
        {
          file = &argv[arg][2];
          if(*file == '\0')
          {
            twrites("Use -lfilename to load source files");
          }
          else
          {
            // Strip double quotes
            if(file[0] == '"')
            {
              int closeq = strlen(file) - 1;
              if(closeq && (file[closeq] == '"'))
              {
                file[closeq] = '\0';
                ++file;
              }
              else
              {
                twrites("Invalid quoting of source file name");
                continue;
              }
            }
            if(!LoadFile(file))
               twritef("%s not found\n", file);
          }
        }
      }
    }
    catch(Err e)
    {
      if(ReportError(e))
        return;                   // Die on fatal error
    }
    // Not running from a script any more
    Interactive = true;

    // The main interpreter loop
    REPL();
  }
  __finally
  {
    FreeHeap();
    FreeStack();

    if(logfile)
      fclose(logfile);
  }
  // Return from LispThread::Execute(), terminate this thread
  // and close the application
}



