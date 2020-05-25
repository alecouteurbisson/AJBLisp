////////////////////////////////////////////////////////////////////////////
//
//  AJBLisp - A Lisp Interpreter
//
//  (c) A J Le Couteur Bisson 1997-2007
//
////////////////////////////////////////////////////////////////////////////
// AJBLisp.cpp
//
// Application start up
////////////////////////////////////////////////////////////////////////////
#include <vcl.h>
#pragma hdrstop
#include "ajblispinit.h"

USEFORM("Listener.cpp", formListener);
USEFORM("Window.cpp", formGWin);
USEUNIT("AJBLispInit.cpp");
USEUNIT("REPL.cpp");
USEUNIT("Eval.cpp");
USEUNIT("Store.cpp");
USEUNIT("Func.cpp");
USEUNIT("FuncAsm.cpp");
USEUNIT("GFunc.cpp");
USEUNIT("IO.cpp");
USEUNIT("Terminal.cpp");
USEUNIT("Random.cpp");
USEUNIT("RunCommand.cpp");
USEUNIT("lisperr.cpp");
USERES("AJBLisp.res");
USE("errors.lsp", File);
USE("Init.lsp", File);
//---------------------------------------------------------------------------
char *Version = "V3.7";

//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->Title = "AJBLisp";
     Application->CreateForm(__classid(TformListener), &formListener);
                 Application->Run();
  }
  catch (Exception &exception)
  {
     Application->ShowException(&exception);
  }
  return 0;
}

//---------------------------------------------------------------------------

